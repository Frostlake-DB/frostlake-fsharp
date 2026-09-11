namespace Frostlake.FSharp

open System
open System.Collections.Concurrent
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Threading
open System.Threading.Tasks

module internal Sync =
    /// The result of a task the blocking API started. On that path every await is on a task that
    /// has already finished — the transport answers a synchronous send with a completed task — so
    /// this returns at once instead of blocking a thread on a continuation.
    let wait (task: Task<'T>) : 'T = task.GetAwaiter().GetResult()

type internal Reply = { Status: int; Body: string }

/// How a connection reaches the engine. HTTP is the only implementation that ships; the tests
/// substitute a scripted one.
type internal ITransport =
    abstract BaseUri: Uri

    /// Send one request. With `sync` the work happens on the calling thread and the returned task
    /// has already completed, so the blocking API does real blocking I/O rather than waiting on
    /// asynchronous I/O.
    abstract Send:
        verb: string *
        path: string *
        body: byte[] *
        timeout: TimeSpan option *
        sync: bool *
        cancellationToken: CancellationToken ->
            Task<Reply>

[<Sealed>]
type internal HttpTransport(baseUri: Uri, connectTimeout: TimeSpan) =
    /// One client per connect timeout, shared by every connection: HttpClient pools its sockets,
    /// and a client per connection would leave a socket behind for each one.
    static let clients = ConcurrentDictionary<TimeSpan, HttpClient>()

    static let clientFor (limit: TimeSpan) =
        clients.GetOrAdd(
            limit,
            Func<TimeSpan, HttpClient>(fun connectLimit ->
                let handler =
                    new SocketsHttpHandler(
                        ConnectTimeout = (if connectLimit > Limits.longest then Timeout.InfiniteTimeSpan else connectLimit),
                        PooledConnectionIdleTimeout = TimeSpan.FromSeconds 30.0,
                        UseCookies = false
                    )
                // Per-request deadlines come from the caller's timeout, so the client imposes none.
                new HttpClient(handler, true, Timeout = Timeout.InfiniteTimeSpan))
        )

    let client = clientFor connectTimeout

    let origin = baseUri.ToString().TrimEnd('/')

    let describe (limit: TimeSpan option) =
        match limit with
        | Some span when span.TotalSeconds >= 1.0 -> sprintf "%gs" span.TotalSeconds
        | Some span -> sprintf "%gms" span.TotalMilliseconds
        | None -> "its timeout"

    /// Turn what HttpClient throws into the driver's own failures. A cancellation the caller asked
    /// for stays an OperationCanceledException.
    let translate (error: exn) (deadline: CancellationTokenSource) (caller: CancellationToken) (limit: TimeSpan option) : exn =
        match error with
        | :? OperationCanceledException when caller.IsCancellationRequested -> error
        | :? OperationCanceledException when deadline.IsCancellationRequested ->
            FrostlakeException(
                ErrorKind.Timeout,
                sprintf "%s did not answer within %s; whether the statement ran is unknown" origin (describe limit),
                None,
                error
            )
            :> exn
        | :? HttpRequestException as e ->
            FrostlakeException(ErrorKind.Transport, sprintf "cannot reach %s: %s" origin e.Message, None, e) :> exn
        | :? IOException as e ->
            FrostlakeException(ErrorKind.Transport, sprintf "the connection to %s broke: %s" origin e.Message, None, e)
            :> exn
        | other -> other

    interface ITransport with
        member _.BaseUri = baseUri

        member _.Send(verb, path, body, timeout, sync, cancellationToken) =
            let request = new HttpRequestMessage(HttpMethod(verb), Uri(baseUri, path))
            if not (isNull body) then
                let content = new ByteArrayContent(body)
                content.Headers.ContentType <- MediaTypeHeaderValue("application/json", "utf-8")
                request.Content <- content
            let deadline = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken)
            match timeout with
            // A limit past the longest timer .NET can arm is no limit at all.
            | Some limit when limit > TimeSpan.Zero && limit <= Limits.longest -> deadline.CancelAfter limit
            | _ -> ()
            if sync then
                try
                    try
                        use response = client.Send(request, HttpCompletionOption.ResponseContentRead, deadline.Token)
                        use stream = response.Content.ReadAsStream(deadline.Token)
                        use reader = new StreamReader(stream, Encoding.UTF8)
                        Task.FromResult
                            { Status = int response.StatusCode
                              Body = reader.ReadToEnd() }
                    with error ->
                        Task.FromException<Reply>(translate error deadline cancellationToken timeout)
                finally
                    request.Dispose()
                    deadline.Dispose()
            else
                task {
                    try
                        try
                            let! response = client.SendAsync(request, HttpCompletionOption.ResponseContentRead, deadline.Token)
                            use response = response
                            let! text = response.Content.ReadAsStringAsync(deadline.Token)
                            return
                                { Status = int response.StatusCode
                                  Body = text }
                        with error ->
                            return raise (translate error deadline cancellationToken timeout)
                    finally
                        request.Dispose()
                        deadline.Dispose()
                }
