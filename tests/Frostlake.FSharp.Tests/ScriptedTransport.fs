module Frostlake.FSharp.Tests.ScriptedTransport

open System
open System.Collections.Generic
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Frostlake.FSharp

/// A request the driver sent, with its JSON body read back.
type Sent =
    { Verb: string
      Path: string
      Sql: string option
      SessionId: string option
      RequireSession: bool option
      AutoCommit: bool option
      /// None when the body carried no multiStatementCount field at all.
      MultiStatementCount: int option }

type private Scripted =
    | Answer of Reply
    | Throw of exn

/// A transport that answers from a script and records what it was sent. An unscripted request
/// fails the test, so every round trip a scenario makes is one it expects.
type Transport() =
    let script = Queue<Scripted>()
    let sent = List<Sent>()

    let read (body: byte[]) =
        if isNull body then
            None, None, None, None, None
        else
            use document = JsonDocument.Parse(Encoding.UTF8.GetString body)
            let root = document.RootElement
            let text (name: string) =
                match root.TryGetProperty(name) with
                | true, value when value.ValueKind = JsonValueKind.String -> Some(value.GetString())
                | _ -> None
            let flag (name: string) =
                match root.TryGetProperty(name) with
                | true, value when value.ValueKind = JsonValueKind.True -> Some true
                | true, value when value.ValueKind = JsonValueKind.False -> Some false
                | _ -> None
            let number (name: string) =
                match root.TryGetProperty(name) with
                | true, value when value.ValueKind = JsonValueKind.Number ->
                    match value.TryGetInt32() with
                    | true, count -> Some count
                    | _ -> None
                | _ -> None
            text "sql", text "sessionId", flag "requireSession", flag "autoCommit", number "multiStatementCount"

    member _.Reply(status: int, body: string) = script.Enqueue(Answer { Status = status; Body = body })
    member this.Reply(body: string) = this.Reply(200, body)
    member _.Fail(error: exn) = script.Enqueue(Throw error)
    member _.Sent = List.ofSeq sent
    member _.Pending = script.Count
    /// Answer on another thread a little later, the way a real server does, instead of at once.
    member val Delay = false with get, set

    interface ITransport with
        member _.BaseUri = Uri("http://scripted:18082")

        member this.Send(verb, path, body, _timeout, _sync, _cancellationToken) =
            let sql, sessionId, requireSession, autoCommit, multiStatementCount = read body
            sent.Add
                { Verb = verb
                  Path = path
                  Sql = sql
                  SessionId = sessionId
                  RequireSession = requireSession
                  AutoCommit = autoCommit
                  MultiStatementCount = multiStatementCount }
            if script.Count = 0 then
                Task.FromException<Reply>(InvalidOperationException(sprintf "unscripted request: %s %s %A" verb path sql))
            else
                match script.Dequeue() with
                | Answer reply when this.Delay ->
                    Task.Run<Reply>(fun () ->
                        Thread.Sleep 20
                        reply)
                | Answer reply -> Task.FromResult reply
                | Throw error -> Task.FromException<Reply> error

let health = """{"status":"healthy","activeSessions":0}"""

let statusSet (text: string) =
    sprintf
        """{"columns":[{"dataType":"VARCHAR","name":"status","nullable":false,"precision":0,"scale":0}],"rowCount":1,"rows":[["%s"]],"updateCount":-1}"""
        text

let numberSet (name: string) (value: int64) =
    sprintf
        """{"columns":[{"dataType":"NUMBER","name":"%s","nullable":false,"precision":38,"scale":0}],"rowCount":1,"rows":[[%d]],"updateCount":-1}"""
        name
        value

let insertedSet (count: int64) =
    sprintf
        """{"columns":[{"dataType":"NUMBER","name":"number of rows inserted","nullable":false,"precision":38,"scale":0}],"rowCount":1,"rows":[[%d]],"updateCount":%d}"""
        count
        count

/// An answer from an engine that reports newSession (0.1.0 and later).
let answer (sessionId: string) (started: bool) (sets: string list) =
    sprintf
        """{"errorMessage":null,"executionTimeMs":1,"newSession":%s,"resultSets":[%s],"sessionId":"%s","success":true}"""
        (if started then "true" else "false")
        (String.Join(",", sets))
        sessionId

/// An answer from an engine that predates newSession (0.0.7).
let legacyAnswer (sessionId: string) (sets: string list) =
    sprintf
        """{"errorMessage":null,"executionTimeMs":1,"resultSets":[%s],"sessionId":"%s","success":true}"""
        (String.Join(",", sets))
        sessionId

let refused (sessionId: string) (message: string) =
    sprintf
        """{"errorMessage":%s,"executionTimeMs":0,"newSession":false,"resultSets":[],"sessionId":"%s","success":false}"""
        (JsonSerializer.Serialize message)
        sessionId

/// How engines before 0.1.0 answer a failed statement: HTTP 500 and no session id.
let legacyRefused (message: string) =
    sprintf
        """{"errorMessage":%s,"executionTimeMs":0,"resultSets":[],"sessionId":null,"success":false}"""
        (JsonSerializer.Serialize message)

/// The 404 a requireSession request gets when its session is gone.
let sessionGone (sessionId: string) =
    sprintf
        """{"errorMessage":"Session '%s' does not exist or has expired.","executionTimeMs":0,"newSession":false,"resultSets":[],"sessionId":null,"success":false}"""
        sessionId

let released = """{"errorMessage":null,"executionTimeMs":0,"newSession":false,"resultSets":[],"sessionId":null,"success":true}"""

let config (dsn: string) = Dsn.parse dsn

/// Open a connection over a scripted transport.
let openWith (transport: Transport) (dsn: string) : Connection =
    Sync.wait (Connection.OpenCore(config dsn, transport, true, CancellationToken.None))

/// The SQL of every POST /api/execute the transport saw.
let statements (transport: Transport) =
    transport.Sent
    |> List.filter (fun s -> s.Path = "/api/execute")
    |> List.choose (fun s -> s.Sql)
