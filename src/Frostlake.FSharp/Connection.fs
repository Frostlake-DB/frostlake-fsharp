namespace Frostlake.FSharp

open System
open System.Diagnostics
open System.Runtime.ExceptionServices
open System.Threading
open System.Threading.Tasks

[<RequireQualifiedAccess>]
type internal Lifecycle =
    | Open
    | Retired of reason: string
    | Closed

type internal Outcome =
    | Answered of Wire.Decoded
    /// The engine refused the session id as unknown (requireSession): nothing ran.
    | SessionGone

/// A connection to a Frostlake engine: one engine session, spoken to over HTTP.
///
/// Open one with Connection.Open, run statements with Execute, and dispose it to release the
/// session. Statements on one connection share its session — the current database and schema,
/// session variables, an open transaction — so give each thread its own connection; calls on one
/// connection are serialised rather than interleaved.
[<Sealed>]
type Connection internal (config: ConnectionConfig, transport: ITransport) =
    /// How long closing may spend on its courtesy requests: rolling back and releasing the session.
    static let closeBudget = Some(TimeSpan.FromSeconds 5.0)

    let gate = new SemaphoreSlim(1, 1)
    let mutable state = Lifecycle.Open
    let mutable sessionId: string option = None
    /// Whether the engine reports `newSession`, which arrived together with requireSession and
    /// DELETE /api/sessions. None until the first answer that names a session.
    let mutable tracksSessions: bool option = None
    let mutable scopeApplied = false
    /// Set once a statement left state behind that a fresh session would not have.
    let mutable dirty = false
    let mutable inTransaction = false
    let mutable lastActivity = Stopwatch.GetTimestamp()
    let mutable serverVersion: string option = None

    /// The DSN's scope as one request, in dependency order, together with the number of statements
    /// it holds — a request carrying more than one has to say so, and the scope stays a single
    /// request so that a session lost while it is applied has one place to recover from, never a
    /// half-moved session.
    let scopeRequest =
        [ config.Role |> Option.map (fun name -> "USE ROLE " + SqlText.identifier name)
          config.Warehouse |> Option.map (fun name -> "USE WAREHOUSE " + SqlText.identifier name)
          config.Database |> Option.map (fun name -> "USE DATABASE " + SqlText.identifier name)
          config.Schema |> Option.map (fun name -> "USE SCHEMA " + SqlText.identifier name) ]
        |> List.choose id
        |> function
            | [] -> None
            | statements -> Some(String.Join("; ", statements), List.length statements)

    let origin = transport.BaseUri.ToString().TrimEnd('/')

    let acquire (sync: bool) (cancellationToken: CancellationToken) : Task =
        if sync then
            gate.Wait(cancellationToken)
            Task.CompletedTask
        else
            gate.WaitAsync(cancellationToken)

    member private _.Retire(reason: string) =
        if state = Lifecycle.Open then
            state <- Lifecycle.Retired reason

    member private _.EnsureUsable() =
        match state with
        | Lifecycle.Open -> ()
        | Lifecycle.Closed -> raise (FrostlakeException(ErrorKind.ConnectionClosed, "the connection is closed"))
        | Lifecycle.Retired reason ->
            raise (
                FrostlakeException(
                    ErrorKind.ConnectionClosed,
                    "the connection was retired after " + reason + " and is not used again; open a new one"
                )
            )

    /// Send, retiring the connection when the request never became an answer: after a broken socket
    /// or a timeout the statement's fate is unknown, and it is never re-sent — re-running it could
    /// duplicate an INSERT.
    member private this.SendGuarded
        (verb: string, path: string, body: byte[], timeout: TimeSpan option, sync: bool, cancellationToken: CancellationToken)
        : Task<Reply> =
        task {
            try
                return! transport.Send(verb, path, body, timeout, sync, cancellationToken)
            with error ->
                this.Retire(
                    match error with
                    | :? FrostlakeException as e when e.Kind = ErrorKind.Timeout -> "a request that timed out"
                    | :? OperationCanceledException -> "a cancelled request"
                    | _ -> "a failed request (" + error.Message + ")"
                )
                ExceptionDispatchInfo.Throw error
                return Unchecked.defaultof<Reply>
        }

    member private _.Absorb(decoded: Wire.Decoded, sentId: bool) =
        match decoded.SessionId with
        | None -> ()
        | Some id ->
            sessionId <- Some id
            match decoded.NewSession with
            | Some started ->
                tracksSessions <- Some true
                if started && sentId then
                    // The engine ran the statement in a fresh session in place of ours: whatever the
                    // old one held is gone, and the scope goes back on before the next statement.
                    scopeApplied <- false
                    dirty <- false
                    inTransaction <- false
            | None ->
                if tracksSessions.IsNone then
                    tracksSessions <- Some false

    /// One POST /api/execute, without any recovery. `statements` declares how many the request
    /// carries — the driver says it for the scope it packs itself, and a caller may say it for a
    /// pack of their own; None leaves the session to answer for the request.
    member private this.Post
        (sql: string, statements: int option, timeout: TimeSpan option, sync: bool, cancellationToken: CancellationToken)
        : Task<Outcome> =
        task {
            let sentId = sessionId.IsSome
            let body = Wire.encodeExecute sql sessionId (not inTransaction) statements
            let! reply = this.SendGuarded("POST", "/api/execute", body, timeout, sync, cancellationToken)
            match Wire.decode reply.Body with
            | Some decoded when reply.Status = 404 && sentId && not decoded.Success && decoded.SessionId.IsNone ->
                return SessionGone
            | Some decoded ->
                this.Absorb(decoded, sentId)
                return Answered decoded
            | None when Wire.carriesBareUndefined reply.Body ->
                return
                    raise (
                        FrostlakeException(
                            ErrorKind.Protocol,
                            "the engine's answer is not valid JSON: it renders a VARIANT undefined as a bare token (engines before 0.1.0 do so for FILTER and TRANSFORM results). The statement ran, but its result cannot be read.",
                            Some sql
                        )
                    )
            | None ->
                this.Retire(sprintf "an answer that was not a Frostlake response (HTTP %d)" reply.Status)
                return
                    raise (
                        FrostlakeException(
                            ErrorKind.Protocol,
                            sprintf "%s/api/execute answered HTTP %d with a body that is not a Frostlake response" origin reply.Status,
                            Some sql
                        )
                    )
        }

    /// Put the DSN's scope on the session, starting one when there is none.
    member private this.ApplyScope(timeout: TimeSpan option, sync: bool, cancellationToken: CancellationToken) : Task<unit> =
        task {
            match scopeRequest with
            | None -> scopeApplied <- true
            | Some(sql, statements) ->
                let! first = this.Post(sql, Some statements, timeout, sync, cancellationToken)
                let! outcome =
                    match first with
                    | SessionGone ->
                        sessionId <- None
                        this.Post(sql, Some statements, timeout, sync, cancellationToken)
                    | answered -> Task.FromResult answered
                match outcome with
                | Answered decoded when decoded.Success ->
                    scopeApplied <- true
                    dirty <- false
                    lastActivity <- Stopwatch.GetTimestamp()
                | Answered decoded ->
                    let message = defaultArg decoded.ErrorMessage "the engine refused it without a message"
                    raise (
                        FrostlakeException(
                            ErrorKind.Refused,
                            "the connection's scope could not be applied: " + message,
                            Some sql
                        )
                    )
                | SessionGone ->
                    raise (
                        FrostlakeException(ErrorKind.SessionLost, "the engine refused a session it had just started", Some sql)
                    )
        }

    /// The engine no longer knows the session (it expired, was released, or the server restarted),
    /// and nothing ran. With a transaction or a moved context gone with it, re-running would put
    /// the statement somewhere its author did not intend, so that is refused; otherwise a fresh
    /// session on the DSN's scope takes over and the statement is sent once more.
    member private this.Recover
        (sql: string, statements: int option, timeout: TimeSpan option, sync: bool, cancellationToken: CancellationToken)
        : Task<Wire.Decoded> =
        task {
            let hadTransaction = inTransaction
            let hadContext = dirty
            sessionId <- None
            scopeApplied <- false
            inTransaction <- false
            dirty <- false
            if hadTransaction then
                raise (
                    FrostlakeException(
                        ErrorKind.SessionLost,
                        "the engine no longer holds this connection's session (it expired, was released, or the server restarted), so its open transaction is gone; the statement did not run",
                        Some sql
                    )
                )
            if hadContext then
                raise (
                    FrostlakeException(
                        ErrorKind.SessionLost,
                        "the engine no longer holds this connection's session (it expired, was released, or the server restarted), and the context set up on it (USE, SET, ALTER SESSION or a temporary object) went with it, so the statement was not re-run; the next statement starts a fresh session on the connection's scope",
                        Some sql
                    )
                )
            do! this.ApplyScope(timeout, sync, cancellationToken)
            match! this.Post(sql, statements, timeout, sync, cancellationToken) with
            | Answered decoded -> return decoded
            | SessionGone ->
                return
                    raise (
                        FrostlakeException(ErrorKind.SessionLost, "the engine refused a session it had just started", Some sql)
                    )
        }

    member private _.Finish(sql: string, decoded: Wire.Decoded) : QueryResult =
        lastActivity <- Stopwatch.GetTimestamp()
        if not decoded.Success then
            let message =
                match decoded.ErrorMessage with
                | Some text when text.Length > 0 -> text
                | _ -> "the engine reported the statement as failed without a message"
            raise (FrostlakeException(ErrorKind.Refused, message, Some sql))
        for statement in SqlText.statements sql do
            if SqlText.touchesSession statement then
                dirty <- true
            match SqlText.transactionEffect statement with
            | TransactionEffect.Begins -> inTransaction <- true
            | TransactionEffect.Ends -> inTransaction <- false
            | TransactionEffect.NoEffect -> ()
        QueryResult(decoded.ResultSets, defaultArg sessionId "", TimeSpan.FromMilliseconds(float decoded.ExecutionTimeMs))

    /// Run already-rendered SQL. `statements` declares how many the one request carries, for a
    /// caller that packs several; None leaves the session's MULTI_STATEMENT_COUNT to answer for it.
    member internal this.RunCore
        (sql: string, statements: int option, timeout: TimeSpan option, sync: bool, cancellationToken: CancellationToken)
        : Task<QueryResult> =
        task {
            this.EnsureUsable()
            do! acquire sync cancellationToken
            try
                this.EnsureUsable()
                if
                    tracksSessions = Some false
                    && scopeApplied
                    && scopeRequest.IsSome
                    && not inTransaction
                    && not dirty
                    && Stopwatch.GetElapsedTime(lastActivity) > Connection.IdleRescopeAfter
                then
                    scopeApplied <- false
                if not scopeApplied then
                    do! this.ApplyScope(timeout, sync, cancellationToken)
                let! outcome = this.Post(sql, statements, timeout, sync, cancellationToken)
                let! decoded =
                    match outcome with
                    | Answered decoded -> Task.FromResult decoded
                    | SessionGone -> this.Recover(sql, statements, timeout, sync, cancellationToken)
                return this.Finish(sql, decoded)
            finally
                gate.Release() |> ignore
        }

    member private this.CheckHealth(retire: bool, sync: bool, cancellationToken: CancellationToken) : Task<unit> =
        task {
            let! reply =
                if retire then
                    this.SendGuarded("GET", "/api/health", null, config.Timeout, sync, cancellationToken)
                else
                    transport.Send("GET", "/api/health", null, config.Timeout, sync, cancellationToken)
            if reply.Status <> 200 || not (Wire.looksLikeHealth reply.Body) then
                if retire then
                    this.Retire(sprintf "a health check that was not answered by a Frostlake engine (HTTP %d)" reply.Status)
                raise (
                    FrostlakeException(
                        ErrorKind.Protocol,
                        sprintf
                            "%s/api/health answered HTTP %d with a body that is not a Frostlake health response; is this the engine's address?"
                            origin
                            reply.Status
                    )
                )
        }

    /// An engine before 0.1.0 re-creates an expired session under the same id at the server's
    /// default scope, and nothing in its answer says so. A connection to such an engine idle this
    /// long re-applies its DSN scope rather than trusting that it survived. Its expiry is 30 minutes.
    static member val internal IdleRescopeAfter = TimeSpan.FromMinutes 5.0 with get, set

    static member internal OpenCore
        (config: ConnectionConfig, transport: ITransport, sync: bool, cancellationToken: CancellationToken)
        : Task<Connection> =
        task {
            let connection = new Connection(config, transport)
            let mutable failure: exn = null
            try
                do! connection.CheckHealth(true, sync, cancellationToken)
                do! connection.ApplyScope(config.Timeout, sync, cancellationToken)
            with error ->
                failure <- error
            if not (isNull failure) then
                do! connection.CloseCore(sync)
                ExceptionDispatchInfo.Throw failure
            return connection
        }

    /// Close, releasing the session with one courtesy request bounded by the close budget. Releasing
    /// a session rolls back whatever it left open, so a retired connection — whose last statement
    /// may still be running — is released too, without anything being re-run. An engine before
    /// 0.1.0 has no release endpoint: an open transaction gets a ROLLBACK instead, and the session
    /// lingers until the engine's own idle expiry.
    member internal _.CloseCore(sync: bool) : Task<unit> =
        task {
            if state <> Lifecycle.Closed then
                do! acquire sync CancellationToken.None
                try
                    if state <> Lifecycle.Closed then
                        state <- Lifecycle.Closed
                        match sessionId with
                        | None -> ()
                        | Some id ->
                            try
                                if tracksSessions <> Some false then
                                    let! _ =
                                        transport.Send(
                                            "DELETE",
                                            "/api/sessions/" + Uri.EscapeDataString id,
                                            null,
                                            closeBudget,
                                            sync,
                                            CancellationToken.None
                                        )
                                    ()
                                elif inTransaction then
                                    let! _ =
                                        transport.Send(
                                            "POST",
                                            "/api/execute",
                                            Wire.encodeExecute "ROLLBACK" sessionId false None,
                                            closeBudget,
                                            sync,
                                            CancellationToken.None
                                        )
                                    ()
                            with _ ->
                                ()
                        sessionId <- None
                        inTransaction <- false
                finally
                    gate.Release() |> ignore
        }

    member internal this.BeginCore(sync: bool, cancellationToken: CancellationToken) : Task<unit> =
        task {
            if inTransaction then
                Fail.usage "a transaction is already open on this connection"
            let! _ = this.RunCore("BEGIN", None, config.Timeout, sync, cancellationToken)
            ()
        }

    member internal this.CommitCore(sync: bool, cancellationToken: CancellationToken) : Task<unit> =
        task {
            if not inTransaction then
                Fail.usage "Commit was called with no transaction open"
            let mutable failure: exn = null
            try
                let! _ = this.RunCore("COMMIT", None, config.Timeout, sync, cancellationToken)
                ()
            with error ->
                failure <- error
            if not (isNull failure) then
                match failure with
                // A refused COMMIT leaves a transaction nobody chose; end it rather than let the
                // next statement inherit it.
                | :? FrostlakeException as e when e.Kind = ErrorKind.Refused && inTransaction ->
                    try
                        let! _ = this.RunCore("ROLLBACK", None, config.Timeout, sync, CancellationToken.None)
                        ()
                    with _ ->
                        ()
                    inTransaction <- false
                | _ -> ()
                ExceptionDispatchInfo.Throw failure
        }

    member internal this.RollbackCore(sync: bool, cancellationToken: CancellationToken) : Task<unit> =
        task {
            if not inTransaction then
                Fail.usage "Rollback was called with no transaction open"
            try
                let! _ = this.RunCore("ROLLBACK", None, config.Timeout, sync, cancellationToken)
                ()
            finally
                inTransaction <- false
        }

    member internal this.ResetCore(sync: bool) : Task<unit> =
        task {
            this.EnsureUsable()
            if inTransaction then
                do! this.RollbackCore(sync, CancellationToken.None)
            if dirty then
                do! acquire sync CancellationToken.None
                try
                    match sessionId with
                    | Some id when tracksSessions <> Some false ->
                        try
                            let! _ =
                                transport.Send(
                                    "DELETE",
                                    "/api/sessions/" + Uri.EscapeDataString id,
                                    null,
                                    closeBudget,
                                    sync,
                                    CancellationToken.None
                                )
                            ()
                        with _ ->
                            ()
                    | _ -> ()
                    sessionId <- None
                    scopeApplied <- false
                    dirty <- false
                finally
                    gate.Release() |> ignore
        }

    member private this.ExecuteAsyncCore
        (sql: string, positional: SqlValue[], named: (string * SqlValue)[], statements: int option,
         cancellationToken: CancellationToken)
        : Task<QueryResult> =
        backgroundTask {
            let rendered = Binding.bind sql positional named true
            return! this.RunCore(rendered, statements, config.Timeout, false, cancellationToken)
        }

    /// Open a connection: check that the address answers as a Frostlake engine, then start a
    /// session on the configuration's scope (USE ROLE, WAREHOUSE, DATABASE, SCHEMA).
    static member Open(config: ConnectionConfig) : Connection =
        Sync.wait (
            Connection.OpenCore(config, HttpTransport(config.BaseUri, config.ConnectTimeout), true, CancellationToken.None)
        )

    /// Open a connection from a DSN (frostlake://host:port/DATABASE/SCHEMA?…) or a connection string.
    static member Open(dsn: string) : Connection = Connection.Open(Dsn.parse dsn)

    static member OpenAsync(config: ConnectionConfig, ?cancellationToken: CancellationToken) : Task<Connection> =
        let token = defaultArg cancellationToken CancellationToken.None
        // backgroundTask leaves the caller's SynchronizationContext behind, so a caller that blocks
        // on the result from a UI thread cannot deadlock the driver's own continuations.
        backgroundTask {
            return! Connection.OpenCore(config, HttpTransport(config.BaseUri, config.ConnectTimeout), false, token)
        }

    static member OpenAsync(dsn: string, ?cancellationToken: CancellationToken) : Task<Connection> =
        backgroundTask {
            let config = Dsn.parse dsn
            return! Connection.OpenAsync(config, defaultArg cancellationToken CancellationToken.None)
        }

    /// The configuration the connection was opened with.
    member _.Config = config

    /// The engine's id for the session, once it has named one.
    member _.SessionId = sessionId

    /// Whether the connection can still run statements: false once closed, or once retired after a
    /// failure whose outcome the driver could not know.
    member _.IsOpen = (state = Lifecycle.Open)

    /// Whether a transaction is open on the session.
    member _.InTransaction = inTransaction

    /// The engine's CURRENT_VERSION(), read once.
    member this.ServerVersion: string =
        match serverVersion with
        | Some version -> version
        | None ->
            let version =
                match (this.Execute "SELECT CURRENT_VERSION()").TryScalar with
                | Some(SqlValue.Text text) -> text
                | _ -> ""
            serverVersion <- Some version
            version

    /// Run a statement — or several separated by semicolons — with positional `?` arguments. With no
    /// arguments the text is sent as written, so a scripting cursor's own `?` marks reach the engine.
    member this.Execute(sql: string, [<ParamArray>] args: SqlValue[]) : QueryResult =
        let rendered = Binding.bind sql args [||] true
        Sync.wait (this.RunCore(rendered, None, config.Timeout, true, CancellationToken.None))

    /// Run a statement with named `:name` (or `@name`) arguments.
    member this.Execute(sql: string, parameters: (string * SqlValue) list) : QueryResult =
        let rendered = Binding.bind sql [||] (Array.ofList parameters) true
        Sync.wait (this.RunCore(rendered, None, config.Timeout, true, CancellationToken.None))

    /// Run a pack of statements, saying how many the one request carries — best written as
    /// `connection.Execute(sql, multiStatementCount = 2)`.
    ///
    /// The count applies to this request alone and outranks the session's MULTI_STATEMENT_COUNT
    /// without touching it, so nothing has to be saved and put back. `0` accepts any number. Leave
    /// it off, as the overloads above do, and the session's value decides as it always has.
    member this.Execute(sql: string, multiStatementCount: int, [<ParamArray>] args: SqlValue[]) : QueryResult =
        let rendered = Binding.bind sql args [||] true
        Sync.wait (this.RunCore(rendered, Some multiStatementCount, config.Timeout, true, CancellationToken.None))

    /// ditto, with named `:name` (or `@name`) arguments.
    member this.Execute
        (sql: string, multiStatementCount: int, parameters: (string * SqlValue) list)
        : QueryResult =
        let rendered = Binding.bind sql [||] (Array.ofList parameters) true
        Sync.wait (this.RunCore(rendered, Some multiStatementCount, config.Timeout, true, CancellationToken.None))

    member this.ExecuteAsync(sql: string, [<ParamArray>] args: SqlValue[]) : Task<QueryResult> =
        this.ExecuteAsyncCore(sql, args, [||], None, CancellationToken.None)

    member this.ExecuteAsync(sql: string, parameters: (string * SqlValue) list) : Task<QueryResult> =
        this.ExecuteAsyncCore(sql, [||], Array.ofList parameters, None, CancellationToken.None)

    member this.ExecuteAsync(sql: string, cancellationToken: CancellationToken) : Task<QueryResult> =
        this.ExecuteAsyncCore(sql, [||], [||], None, cancellationToken)

    member this.ExecuteAsync(sql: string, args: SqlValue list, cancellationToken: CancellationToken) : Task<QueryResult> =
        this.ExecuteAsyncCore(sql, Array.ofList args, [||], None, cancellationToken)

    member this.ExecuteAsync
        (sql: string, parameters: (string * SqlValue) list, cancellationToken: CancellationToken)
        : Task<QueryResult> =
        this.ExecuteAsyncCore(sql, [||], Array.ofList parameters, None, cancellationToken)

    /// ditto, awaited: `connection.ExecuteAsync(sql, multiStatementCount = 2)`.
    member this.ExecuteAsync
        (sql: string, multiStatementCount: int, [<ParamArray>] args: SqlValue[])
        : Task<QueryResult> =
        this.ExecuteAsyncCore(sql, args, [||], Some multiStatementCount, CancellationToken.None)

    member this.ExecuteAsync
        (sql: string, multiStatementCount: int, args: SqlValue list, cancellationToken: CancellationToken)
        : Task<QueryResult> =
        this.ExecuteAsyncCore(sql, Array.ofList args, [||], Some multiStatementCount, cancellationToken)

    member this.ExecuteAsync
        (sql: string, multiStatementCount: int, parameters: (string * SqlValue) list,
         cancellationToken: CancellationToken)
        : Task<QueryResult> =
        this.ExecuteAsyncCore(sql, [||], Array.ofList parameters, Some multiStatementCount, cancellationToken)

    /// Open a transaction: BEGIN. The engine offers read committed and nothing else.
    member this.BeginTransaction() = Sync.wait (this.BeginCore(true, CancellationToken.None))

    member this.BeginTransactionAsync(?cancellationToken: CancellationToken) : Task =
        backgroundTask { do! this.BeginCore(false, defaultArg cancellationToken CancellationToken.None) } :> Task

    /// COMMIT. When the engine refuses it, the transaction is rolled back before the error is raised.
    member this.Commit() = Sync.wait (this.CommitCore(true, CancellationToken.None))

    member this.CommitAsync(?cancellationToken: CancellationToken) : Task =
        backgroundTask { do! this.CommitCore(false, defaultArg cancellationToken CancellationToken.None) } :> Task

    member this.Rollback() = Sync.wait (this.RollbackCore(true, CancellationToken.None))

    member this.RollbackAsync(?cancellationToken: CancellationToken) : Task =
        backgroundTask { do! this.RollbackCore(false, defaultArg cancellationToken CancellationToken.None) } :> Task

    /// Run `body` inside a transaction: committed when it returns, rolled back when it raises.
    member this.Transaction(body: unit -> 'T) : 'T =
        this.BeginTransaction()
        let result =
            try
                body ()
            with _ ->
                if inTransaction && state = Lifecycle.Open then
                    try
                        this.Rollback()
                    with _ ->
                        ()
                reraise ()
        this.Commit()
        result

    /// The asynchronous Transaction. The body runs without the caller's SynchronizationContext,
    /// like the rest of the driver's asynchronous work.
    member this.TransactionAsync(body: unit -> Task<'T>, ?cancellationToken: CancellationToken) : Task<'T> =
        let token = defaultArg cancellationToken CancellationToken.None
        backgroundTask {
            do! this.BeginCore(false, token)
            let mutable failure: exn = null
            let mutable result = Unchecked.defaultof<'T>
            try
                let! value = body ()
                result <- value
            with error ->
                failure <- error
            if isNull failure then
                do! this.CommitCore(false, token)
            else
                if inTransaction && state = Lifecycle.Open then
                    try
                        do! this.RollbackCore(false, CancellationToken.None)
                    with _ ->
                        ()
                ExceptionDispatchInfo.Throw failure
            return result
        }

    /// Ask the health endpoint whether the engine is still there, without touching the session.
    member this.Ping() =
        this.EnsureUsable()
        Sync.wait (this.CheckHealth(false, true, CancellationToken.None))

    member this.PingAsync(?cancellationToken: CancellationToken) : Task =
        this.EnsureUsable()
        backgroundTask { do! this.CheckHealth(false, false, defaultArg cancellationToken CancellationToken.None) } :> Task

    /// Put the connection back where it started, for reuse across unrelated work: an open
    /// transaction is rolled back, and a session whose scope, variables or settings moved is
    /// released and replaced by a fresh one on the connection's scope.
    member this.Reset() = Sync.wait (this.ResetCore true)

    member this.ResetAsync() : Task = backgroundTask { do! this.ResetCore false } :> Task

    /// Close the connection: roll back an open transaction and release the engine session.
    /// Idempotent.
    member this.Close() = Sync.wait (this.CloseCore true)

    member this.CloseAsync() : Task = backgroundTask { do! this.CloseCore false } :> Task

    interface IDisposable with
        member this.Dispose() = this.Close()

    interface IAsyncDisposable with
        member this.DisposeAsync() = ValueTask(this.CloseAsync())
