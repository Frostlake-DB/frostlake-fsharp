module Frostlake.FSharp.Tests.TestSupport

open System
open System.Diagnostics
open System.IO
open System.Net
open System.Net.Http
open System.Net.Sockets
open System.Text
open System.Threading
open Frostlake.FSharp
open Xunit

// The engine-backed tests share one server and create databases on it, so they run one at a time.
[<assembly: CollectionBehavior(DisableTestParallelization = true)>]
do ()

/// Where the engine-backed tests find a server: FROSTLAKE_URL attaches to a running one;
/// otherwise FROSTLAKE_CLASSPATH (the engine jar plus its dependencies) boots one on a free port.
let private url = Environment.GetEnvironmentVariable "FROSTLAKE_URL"

let private classpath = Environment.GetEnvironmentVariable "FROSTLAKE_CLASSPATH"

let engineConfigured = not (String.IsNullOrWhiteSpace url) || not (String.IsNullOrWhiteSpace classpath)

let private freePort () =
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    listener.Stop()
    port

let private boot () : string =
    if not (String.IsNullOrWhiteSpace url) then
        url.TrimEnd('/')
    else
        let javaHome = Environment.GetEnvironmentVariable "JAVA_HOME"
        let java =
            if String.IsNullOrWhiteSpace javaHome then
                "java"
            else
                Path.Combine(javaHome, "bin", "java")
        let port = freePort ()
        // The engine writes its log and data into its working directory; keep them out of the tree.
        let home = Path.Combine(Path.GetTempPath(), sprintf "frostlake-fsharp-%d" port)
        Directory.CreateDirectory home |> ignore
        let start = ProcessStartInfo(java, UseShellExecute = false, RedirectStandardOutput = true, RedirectStandardError = true, WorkingDirectory = home)
        // Without no-delay a kept-alive connection waits on a delayed ACK for every response.
        start.ArgumentList.Add "-Dsun.net.httpserver.nodelay=true"
        start.ArgumentList.Add "-cp"
        start.ArgumentList.Add classpath
        start.ArgumentList.Add "dev.frostlake.http.DatabaseHttpServer"
        start.ArgumentList.Add(string port)
        let output = StringBuilder()
        let server = Process.Start start
        let capture (line: string) =
            if not (isNull line) then
                lock output (fun () -> output.AppendLine line |> ignore)
        server.OutputDataReceived.Add(fun e -> capture e.Data)
        server.ErrorDataReceived.Add(fun e -> capture e.Data)
        server.BeginOutputReadLine()
        server.BeginErrorReadLine()
        AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
            try
                if not server.HasExited then
                    server.Kill true
            with _ ->
                ())
        use http = new HttpClient()
        let deadline = DateTime.UtcNow.AddSeconds 60.0
        let mutable ready = false
        while not ready do
            try
                use response = http.Send(new HttpRequestMessage(HttpMethod.Get, sprintf "http://127.0.0.1:%d/api/health" port))
                ready <- response.IsSuccessStatusCode
            with _ ->
                ()
            if not ready then
                if server.HasExited || DateTime.UtcNow > deadline then
                    let captured = lock output (fun () -> output.ToString())
                    failwithf
                        "the Frostlake server did not become healthy (java: %s, classpath entries: %d, exited: %b)\n%s"
                        java
                        (classpath.Split(Path.PathSeparator).Length)
                        server.HasExited
                        captured
                Thread.Sleep 200
        sprintf "frostlake://127.0.0.1:%d" port

let private server = lazy (boot ())

/// The DSN of the engine under test, booting it on first use.
let engineDsn () : string = server.Value

/// The engine's base HTTP address.
let engineHttp () : string =
    (Dsn.parse (engineDsn ())).BaseUri.ToString().TrimEnd('/')

/// A [<Fact>] that reports as skipped — never as passed — when no engine is configured.
type EngineFactAttribute() as this =
    inherit FactAttribute()

    do
        if not engineConfigured then
            this.Skip <- "set FROSTLAKE_CLASSPATH (the engine jar and its dependencies) or FROSTLAKE_URL to run the engine-backed tests"

let mutable private counter = 0

/// A fresh, empty database with a PUBLIC schema, and a DSN scoped to it.
let freshDatabase (prefix: string) : string * string =
    let name = sprintf "FS_%s_%d_%d" prefix (Environment.ProcessId) (Interlocked.Increment(&counter))
    use connection = Connection.Open(engineDsn ())
    connection.Execute(sprintf "CREATE OR REPLACE DATABASE %s" name) |> ignore
    name, sprintf "%s/%s/PUBLIC" (engineDsn ()) name

/// A raw request to the engine, bypassing the driver.
let rawRequest (verb: string) (path: string) (body: string) : int * string =
    use http = new HttpClient()
    use request = new HttpRequestMessage(HttpMethod(verb), engineHttp () + path)
    if not (isNull body) then
        request.Content <- new StringContent(body, Encoding.UTF8, "application/json")
    use response = http.Send request
    use reader = new StreamReader(response.Content.ReadAsStream())
    int response.StatusCode, reader.ReadToEnd()

/// Whether the engine has DELETE /api/sessions/{id} (0.1.0 and later): 404 for an unknown id,
/// where older engines answer 405 for the method.
let engineReleasesSessions () : bool =
    fst (rawRequest "DELETE" "/api/sessions/no-such-session" null) = 404

let activeSessions () : int =
    let _, body = rawRequest "GET" "/api/sessions" null
    use document = System.Text.Json.JsonDocument.Parse body
    document.RootElement.GetProperty("activeSessions").GetInt32()

/// Assert that `action` raises a FrostlakeException of the given kind, and return it.
let raisesKind (kind: ErrorKind) (action: unit -> 'T) : FrostlakeException =
    let error = Assert.Throws<FrostlakeException>(fun () -> action () |> ignore)
    Assert.True((error.Kind = kind), sprintf "expected %A, got %A: %s" kind error.Kind error.Message)
    error

/// A SynchronizationContext like a UI thread that is blocked waiting on a task: work posted to it
/// never runs, so an await that captured it never resumes.
type NeverPumped() =
    inherit SynchronizationContext()
    override _.Post(_, _) = ()

/// Run `action` with a NeverPumped context installed on the current thread.
let underBlockedUiThread (action: unit -> unit) =
    let previous = SynchronizationContext.Current
    SynchronizationContext.SetSynchronizationContext(NeverPumped())
    try
        action ()
    finally
        SynchronizationContext.SetSynchronizationContext previous
