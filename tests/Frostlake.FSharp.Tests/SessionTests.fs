/// How a connection keeps its idea of the engine session in step with the engine's, over a
/// scripted transport: every request a scenario makes is one it scripted.
module Frostlake.FSharp.Tests.SessionTests

open System
open Frostlake.FSharp
open Frostlake.FSharp.Tests.TestSupport
open Frostlake.FSharp.Tests.ScriptedTransport
open Xunit

let private dsn = "frostlake://scripted:18082/APP/PUBLIC"
let private scope = "USE DATABASE APP; USE SCHEMA PUBLIC"
let private ok = statusSet "Statement executed successfully."

/// A connection opened on a scripted engine that reports newSession.
let private opened () =
    let transport = Transport()
    transport.Reply health
    transport.Reply(answer "s1" true [ ok; ok ])
    let connection = openWith transport dsn
    transport, connection

[<Fact>]
let ``opening checks health and applies the scope in one request`` () =
    let transport, connection = opened ()
    let sent = transport.Sent
    Assert.Equal("GET", sent.[0].Verb)
    Assert.Equal("/api/health", sent.[0].Path)
    Assert.Equal<string list>([ scope ], statements transport)
    Assert.True(sent.[1].SessionId.IsNone)
    Assert.True(sent.[1].RequireSession.IsNone)
    Assert.Equal(Some "s1", connection.SessionId)

[<Fact>]
let ``statements resume the session and require it`` () =
    let transport, connection = opened ()
    transport.Reply(answer "s1" false [ numberSet "N" 1L ])
    let result = connection.Execute "SELECT 1 AS N"
    Assert.Equal(SqlValue.Int 1L, result.Scalar)
    let last = List.last transport.Sent
    Assert.Equal(Some "s1", last.SessionId)
    Assert.Equal(Some true, last.RequireSession)
    Assert.Equal(Some true, last.AutoCommit)

[<Fact>]
let ``a lost session is replaced and the statement sent once more`` () =
    let transport, connection = opened ()
    transport.Reply(404, sessionGone "s1")
    transport.Reply(answer "s2" true [ ok; ok ])
    transport.Reply(answer "s2" false [ numberSet "N" 1L ])
    let result = connection.Execute "SELECT 1 AS N"
    Assert.Equal(SqlValue.Int 1L, result.Scalar)
    Assert.Equal<string list>([ scope; "SELECT 1 AS N"; scope; "SELECT 1 AS N" ], statements transport)
    let executes = transport.Sent |> List.filter (fun s -> s.Path = "/api/execute")
    Assert.True(executes.[2].SessionId.IsNone)
    Assert.Equal(Some "s2", executes.[3].SessionId)
    Assert.Equal(Some "s2", connection.SessionId)
    Assert.Equal(0, transport.Pending)

[<Fact>]
let ``a lost session with an open transaction is reported, not replaced`` () =
    let transport, connection = opened ()
    transport.Reply(answer "s1" false [ ok ])
    connection.BeginTransaction()
    Assert.True(connection.InTransaction)
    transport.Reply(404, sessionGone "s1")
    let error = raisesKind ErrorKind.SessionLost (fun () -> connection.Execute "INSERT INTO T VALUES (1)")
    Assert.Contains("transaction", error.Message)
    Assert.False(connection.InTransaction)
    Assert.True(connection.IsOpen)
    // The next statement starts over on the DSN's scope.
    transport.Reply(answer "s2" true [ ok; ok ])
    transport.Reply(answer "s2" false [ numberSet "N" 1L ])
    connection.Execute "SELECT 1 AS N" |> ignore
    Assert.Equal<string list>(
        [ scope; "BEGIN"; "INSERT INTO T VALUES (1)"; scope; "SELECT 1 AS N" ],
        statements transport
    )

[<Fact>]
let ``a lost session whose context moved is reported, not replaced`` () =
    let transport, connection = opened ()
    transport.Reply(answer "s1" false [ ok ])
    connection.Execute "USE SCHEMA OTHER" |> ignore
    transport.Reply(404, sessionGone "s1")
    let error = raisesKind ErrorKind.SessionLost (fun () -> connection.Execute "SELECT * FROM T")
    Assert.Contains("context", error.Message)
    Assert.Equal<string list>([ scope; "USE SCHEMA OTHER"; "SELECT * FROM T" ], statements transport)

[<Fact>]
let ``statements that move the session are noticed anywhere in a request`` () =
    let transport, connection = opened ()
    transport.Reply(answer "s1" false [ numberSet "N" 1L; ok ])
    connection.Execute "SELECT 1 AS N; USE SCHEMA OTHER" |> ignore
    transport.Reply(404, sessionGone "s1")
    raisesKind ErrorKind.SessionLost (fun () -> connection.Execute "SELECT 2") |> ignore

[<Fact>]
let ``a transport failure retires the connection without re-sending`` () =
    let transport, connection = opened ()
    transport.Fail(FrostlakeException(ErrorKind.Transport, "socket reset"))
    raisesKind ErrorKind.Transport (fun () -> connection.Execute "INSERT INTO T VALUES (1)") |> ignore
    Assert.False(connection.IsOpen)
    raisesKind ErrorKind.ConnectionClosed (fun () -> connection.Execute "SELECT 1") |> ignore
    Assert.Equal<string list>([ scope; "INSERT INTO T VALUES (1)" ], statements transport)
    // Nothing is re-sent, but closing still releases the session.
    transport.Reply released
    connection.Close()
    Assert.Equal("DELETE", (List.last transport.Sent).Verb)
    Assert.Equal(0, transport.Pending)

[<Fact>]
let ``an answer that is not Frostlake's retires the connection`` () =
    let transport, connection = opened ()
    transport.Reply(502, "<html>Bad Gateway</html>")
    let error = raisesKind ErrorKind.Protocol (fun () -> connection.Execute "SELECT 1")
    Assert.Contains("HTTP 502", error.Message)
    Assert.False(connection.IsOpen)

[<Fact>]
let ``a bare undefined is reported as the engine's defect and the connection stays`` () =
    let transport, connection = opened ()
    transport.Reply(
        """{"success":true,"sessionId":"s1","resultSets":[{"columns":[{"name":"F","dataType":"VARCHAR"}],"rows":[[[1,undefined]]]}]}"""
    )
    let error = raisesKind ErrorKind.Protocol (fun () -> connection.Execute "SELECT TRANSFORM(a, x -> NULL)")
    Assert.Contains("undefined", error.Message)
    Assert.True(connection.IsOpen)

[<Fact>]
let ``a refused statement leaves the session usable`` () =
    let transport, connection = opened ()
    transport.Reply(refused "s1" "SQL compilation error:\nObject 'NOPE' does not exist or not authorized.")
    let error = raisesKind ErrorKind.Refused (fun () -> connection.Execute "SELECT * FROM NOPE")
    Assert.StartsWith("SQL compilation error", error.Message)
    Assert.Equal(Some "SELECT * FROM NOPE", error.Statement)
    Assert.True(connection.IsOpen)
    Assert.Equal(Some "s1", connection.SessionId)

[<Fact>]
let ``an older engine's HTTP 500 refusal keeps the session, and closing sends nothing`` () =
    let transport = Transport()
    transport.Reply health
    transport.Reply(legacyAnswer "old1" [])
    let connection = openWith transport dsn
    transport.Reply(500, legacyRefused "SQL compilation error:\nsyntax error line 1 at position 0 unexpected 'SELEC'.")
    raisesKind ErrorKind.Refused (fun () -> connection.Execute "SELEC 1") |> ignore
    Assert.Equal(Some "old1", connection.SessionId)
    Assert.True(connection.IsOpen)
    // An engine without newSession has no DELETE /api/sessions.
    connection.Close()
    Assert.Equal(3, transport.Sent.Length)

[<Fact>]
let ``closing releases the session, which rolls back its open transaction`` () =
    let transport, connection = opened ()
    transport.Reply(answer "s1" false [ ok ])
    connection.BeginTransaction()
    transport.Reply released
    connection.Close()
    let last = List.last transport.Sent
    Assert.Equal("DELETE", last.Verb)
    Assert.Equal("/api/sessions/s1", last.Path)
    Assert.Equal(0, transport.Pending)
    let count = transport.Sent.Length
    connection.Close()
    Assert.Equal(count, transport.Sent.Length)
    raisesKind ErrorKind.ConnectionClosed (fun () -> connection.Execute "SELECT 1") |> ignore

[<Fact>]
let ``closing on an older engine rolls back an open transaction instead`` () =
    let transport = Transport()
    transport.Reply health
    transport.Reply(legacyAnswer "old1" [])
    let connection = openWith transport dsn
    transport.Reply(legacyAnswer "old1" [])
    connection.BeginTransaction()
    transport.Reply(legacyAnswer "old1" [])
    connection.Close()
    Assert.Equal(Some "ROLLBACK", (List.last transport.Sent).Sql)
    Assert.Equal(0, transport.Pending)

[<Fact>]
let ``a retired connection still releases its session when closed`` () =
    let transport, connection = opened ()
    transport.Reply(answer "s1" false [ ok ])
    connection.BeginTransaction()
    transport.Reply(502, "<html>Bad Gateway</html>")
    raisesKind ErrorKind.Protocol (fun () -> connection.Execute "INSERT INTO T VALUES (1)") |> ignore
    Assert.False(connection.IsOpen)
    transport.Reply released
    connection.Close()
    let last = List.last transport.Sent
    Assert.Equal("DELETE", last.Verb)
    Assert.Equal("/api/sessions/s1", last.Path)

[<Fact>]
let ``a scope the engine refuses fails the open and releases the session`` () =
    let transport = Transport()
    transport.Reply health
    transport.Reply(refused "s9" "Database 'APP' does not exist or not authorized.")
    transport.Reply released
    let error = raisesKind ErrorKind.Refused (fun () -> openWith transport dsn)
    Assert.Contains("APP", error.Message)
    Assert.Equal("DELETE", (List.last transport.Sent).Verb)

[<Fact>]
let ``transactions switch autoCommit off and track BEGIN, COMMIT and ROLLBACK`` () =
    let transport, connection = opened ()
    for _ in 1..4 do
        transport.Reply(answer "s1" false [ ok ])
    connection.BeginTransaction()
    connection.Execute "INSERT INTO T VALUES (1)" |> ignore
    Assert.Equal(Some false, (List.last transport.Sent).AutoCommit)
    connection.Commit()
    Assert.False(connection.InTransaction)
    connection.Execute "SELECT 1" |> ignore
    Assert.Equal(Some true, (List.last transport.Sent).AutoCommit)
    raisesKind ErrorKind.Usage (fun () -> connection.Commit()) |> ignore
    raisesKind ErrorKind.Usage (fun () -> connection.Rollback()) |> ignore

[<Fact>]
let ``a transaction opened by a statement is tracked too`` () =
    let transport, connection = opened ()
    transport.Reply(answer "s1" false [ ok ])
    connection.Execute "BEGIN TRANSACTION" |> ignore
    Assert.True(connection.InTransaction)
    raisesKind ErrorKind.Usage (fun () -> connection.BeginTransaction()) |> ignore
    transport.Reply(answer "s1" false [ ok ])
    connection.Execute "ROLLBACK" |> ignore
    Assert.False(connection.InTransaction)

[<Fact>]
let ``a refused commit rolls back before reporting`` () =
    let transport, connection = opened ()
    transport.Reply(answer "s1" false [ ok ])
    connection.BeginTransaction()
    transport.Reply(refused "s1" "commit failed")
    transport.Reply(answer "s1" false [ ok ])
    raisesKind ErrorKind.Refused (fun () -> connection.Commit()) |> ignore
    Assert.False(connection.InTransaction)
    Assert.Equal(Some "ROLLBACK", (List.last transport.Sent).Sql)

[<Fact>]
let ``Transaction commits when the body returns and rolls back when it raises`` () =
    let transport, connection = opened ()
    for _ in 1..5 do
        transport.Reply(answer "s1" false [ ok ])
    let value =
        connection.Transaction(fun () ->
            connection.Execute "INSERT INTO T VALUES (1)" |> ignore
            42)
    Assert.Equal(42, value)
    Assert.Throws<InvalidOperationException>(fun () ->
        connection.Transaction(fun () -> raise (InvalidOperationException "boom")) |> ignore)
    |> ignore
    Assert.Equal<string list>(
        [ scope; "BEGIN"; "INSERT INTO T VALUES (1)"; "COMMIT"; "BEGIN"; "ROLLBACK" ],
        statements transport
    )

[<Fact>]
let ``resetting a moved session releases it and starts over on the scope`` () =
    let transport, connection = opened ()
    transport.Reply(answer "s1" false [ ok ])
    connection.Execute "ALTER SESSION SET TIMEZONE = 'Europe/Warsaw'" |> ignore
    transport.Reply released
    connection.Reset()
    Assert.True(connection.SessionId.IsNone)
    transport.Reply(answer "s2" true [ ok; ok ])
    transport.Reply(answer "s2" false [ numberSet "N" 1L ])
    connection.Execute "SELECT 1 AS N" |> ignore
    Assert.Equal("DELETE", transport.Sent.[3].Verb)
    Assert.Equal<string list>(
        [ scope; "ALTER SESSION SET TIMEZONE = 'Europe/Warsaw'"; scope; "SELECT 1 AS N" ],
        statements transport
    )

[<Fact>]
let ``resetting an untouched session sends nothing`` () =
    let transport, connection = opened ()
    connection.Reset()
    Assert.Equal(2, transport.Sent.Length)

[<Fact>]
let ``the endpoint's own refusal does not hide the engine's session support`` () =
    let transport, connection = opened ()
    transport.Reply(400, """{"error":"SQL is required"}""")
    let error = raisesKind ErrorKind.Refused (fun () -> connection.Execute "   ")
    Assert.Equal("SQL is required", error.Message)
    transport.Reply released
    connection.Close()
    Assert.Equal("DELETE", (List.last transport.Sent).Verb)

[<Fact>]
let ``an older engine's idle session gets its scope back`` () =
    let transport = Transport()
    transport.Reply health
    transport.Reply(legacyAnswer "old1" [])
    let connection = openWith transport dsn
    let saved = Connection.IdleRescopeAfter
    Connection.IdleRescopeAfter <- TimeSpan.Zero
    try
        transport.Reply(legacyAnswer "old1" [])
        transport.Reply(legacyAnswer "old1" [ numberSet "N" 1L ])
        connection.Execute "SELECT 1 AS N" |> ignore
        Assert.Equal<string list>([ scope; scope; "SELECT 1 AS N" ], statements transport)
    finally
        Connection.IdleRescopeAfter <- saved

[<Fact>]
let ``a server that replaced the session gets the scope back before the next statement`` () =
    let transport, connection = opened ()
    transport.Reply(answer "s1" true [ numberSet "N" 1L ])
    connection.Execute "SELECT 1 AS N" |> ignore
    transport.Reply(answer "s1" false [ ok; ok ])
    transport.Reply(answer "s1" false [ numberSet "N" 2L ])
    connection.Execute "SELECT 2 AS N" |> ignore
    Assert.Equal<string list>([ scope; "SELECT 1 AS N"; scope; "SELECT 2 AS N" ], statements transport)

[<Fact>]
let ``with no scope in the DSN the session starts with the first statement`` () =
    let transport = Transport()
    transport.Reply health
    let connection = openWith transport "frostlake://scripted:18082"
    Assert.True(connection.SessionId.IsNone)
    transport.Reply(answer "n1" true [ numberSet "N" 1L ])
    connection.Execute "SELECT 1 AS N" |> ignore
    Assert.Equal(Some "n1", connection.SessionId)
    Assert.True((List.last transport.Sent).RequireSession.IsNone)

[<Fact>]
let ``a health answer that is not Frostlake's fails the open`` () =
    let transport = Transport()
    transport.Reply(200, "<html>It works!</html>")
    raisesKind ErrorKind.Protocol (fun () -> openWith transport "frostlake://scripted:18082") |> ignore

[<Fact>]
let ``binding mistakes are caught before anything is sent`` () =
    let transport, connection = opened ()
    let before = transport.Sent.Length
    raisesKind ErrorKind.Binding (fun () -> connection.Execute("SELECT ?", SqlValue.Int 1L, SqlValue.Int 2L))
    |> ignore
    raisesKind ErrorKind.Binding (fun () -> connection.Execute("SELECT :a", [ "b", SqlValue.Int 1L ])) |> ignore
    Assert.Equal(before, transport.Sent.Length)

[<Fact>]
let ``the asynchronous path follows the same session rules`` () =
    let transport, connection = opened ()
    transport.Reply(404, sessionGone "s1")
    transport.Reply(answer "s2" true [ ok; ok ])
    transport.Reply(answer "s2" false [ numberSet "N" 3L ])
    let result = (connection.ExecuteAsync("SELECT ? AS N", SqlValue.Int 3L)).GetAwaiter().GetResult()
    Assert.Equal(SqlValue.Int 3L, result.Scalar)
    Assert.Equal<string list>([ scope; "SELECT 3 AS N"; scope; "SELECT 3 AS N" ], statements transport)

[<Fact>]
let ``a request carries a statement count only when the caller asks for one`` () =
    let transport, connection = opened ()
    transport.Reply(answer "s1" false [ numberSet "N" 1L ])
    connection.Execute "SELECT 1 AS N" |> ignore
    // Nothing asked for a count, so the body carries no such field and the session answers for the
    // request exactly as it did before the argument existed.
    Assert.True((List.last transport.Sent).MultiStatementCount.IsNone)
    transport.Reply(answer "s1" false [ numberSet "A" 1L; numberSet "B" 2L ])
    connection.Execute("SELECT 1 AS A; SELECT 2 AS B", multiStatementCount = 2) |> ignore
    Assert.Equal(Some 2, (List.last transport.Sent).MultiStatementCount)
    // Zero is a count like any other — "any number of statements" — not an absent one.
    transport.Reply(answer "s1" false [ numberSet "A" 1L; numberSet "B" 2L ])
    connection.Execute("SELECT 1 AS A; SELECT 2 AS B", multiStatementCount = 0) |> ignore
    Assert.Equal(Some 0, (List.last transport.Sent).MultiStatementCount)

[<Fact>]
let ``a count travels with its own request and nothing else`` () =
    let transport, connection = opened ()
    transport.Reply(answer "s1" false [ numberSet "A" 1L; numberSet "B" 2L ])
    connection.Execute("SELECT 1 AS A; SELECT 2 AS B", multiStatementCount = 2) |> ignore
    transport.Reply(answer "s1" false [ numberSet "N" 1L ])
    connection.Execute "SELECT 1 AS N" |> ignore
    // No ALTER SESSION was sent for it, and the next statement is back to carrying no count: the
    // count was the request's, never the session's.
    Assert.Equal<string list>(
        [ scope; "SELECT 1 AS A; SELECT 2 AS B"; "SELECT 1 AS N" ],
        statements transport
    )
    Assert.True((List.last transport.Sent).MultiStatementCount.IsNone)

[<Fact>]
let ``the asynchronous path carries the count too`` () =
    let transport, connection = opened ()
    transport.Reply(answer "s1" false [ numberSet "A" 1L; numberSet "B" 2L ])
    (connection.ExecuteAsync("SELECT 1 AS A; SELECT 2 AS B", multiStatementCount = 2))
        .GetAwaiter()
        .GetResult()
    |> ignore
    Assert.Equal(Some 2, (List.last transport.Sent).MultiStatementCount)

[<Fact>]
let ``blocking on an asynchronous call from a UI-style thread does not deadlock`` () =
    let transport, connection = opened ()
    transport.Delay <- true
    transport.Reply(answer "s1" false [ numberSet "N" 1L ])
    transport.Reply(answer "s1" false [ ok ])
    underBlockedUiThread (fun () ->
        let pending = connection.ExecuteAsync "SELECT 1 AS N"
        Assert.True(pending.Wait(TimeSpan.FromSeconds 10.0), "ExecuteAsync never completed")
        Assert.Equal(SqlValue.Int 1L, pending.Result.Scalar)
        Assert.True(connection.BeginTransactionAsync().Wait(TimeSpan.FromSeconds 10.0), "BeginTransactionAsync never completed"))
