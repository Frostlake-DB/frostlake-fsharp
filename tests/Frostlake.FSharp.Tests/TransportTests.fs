/// The HTTP transport's failures, against local sockets rather than an engine.
module Frostlake.FSharp.Tests.TransportTests

open System
open System.Net
open System.Net.Sockets
open System.Text
open System.Threading
open System.Threading.Tasks
open Frostlake.FSharp
open Frostlake.FSharp.Tests.TestSupport
open Xunit

/// A listener that accepts connections and answers every request with `body` over plain HTTP, or
/// holds each connection open and says nothing when `body` is None.
let private listen (body: string option) : TcpListener * int =
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let held = ResizeArray<TcpClient>()
    let serve () =
        try
            while true do
                let client = listener.AcceptTcpClient()
                let stream = client.GetStream()
                let buffer = Array.zeroCreate<byte> 8192
                stream.Read(buffer, 0, buffer.Length) |> ignore
                match body with
                | Some text ->
                    let reply =
                        sprintf
                            "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                            (Encoding.UTF8.GetByteCount text)
                            text
                    let bytes = Encoding.UTF8.GetBytes reply
                    stream.Write(bytes, 0, bytes.Length)
                    client.Close()
                | None -> held.Add client
        with _ ->
            ()
    Thread(serve, IsBackground = true).Start()
    listener, port

/// A listener that answers the health check like an engine and never answers anything else, so a
/// statement hangs in flight.
let private listenStalling () : TcpListener * int =
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let held = ResizeArray<TcpClient>()
    let serve () =
        try
            while true do
                let client = listener.AcceptTcpClient()
                let stream = client.GetStream()
                let buffer = Array.zeroCreate<byte> 8192
                let read = stream.Read(buffer, 0, buffer.Length)
                if Encoding.ASCII.GetString(buffer, 0, read).StartsWith("GET /api/health", StringComparison.Ordinal) then
                    let body = """{"status":"healthy","activeSessions":0}"""
                    let reply =
                        sprintf
                            "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                            body.Length
                            body
                    let bytes = Encoding.ASCII.GetBytes reply
                    stream.Write(bytes, 0, bytes.Length)
                    client.Close()
                else
                    held.Add client
        with _ ->
            ()
    Thread(serve, IsBackground = true).Start()
    listener, port

let private closedPort () =
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    listener.Stop()
    port

[<Fact>]
let ``a refused connection is a transport failure`` () =
    let error = raisesKind ErrorKind.Transport (fun () -> Connection.Open(sprintf "frostlake://127.0.0.1:%d" (closedPort ())))
    Assert.Contains("cannot reach", error.Message)

[<Fact>]
let ``something that is not an engine is reported as such`` () =
    let listener, port = listen (Some "<html>It works!</html>")
    try
        let error = raisesKind ErrorKind.Protocol (fun () -> Connection.Open(sprintf "frostlake://127.0.0.1:%d" port))
        Assert.Contains("/api/health", error.Message)
    finally
        listener.Stop()

[<Fact>]
let ``a request that outlives its timeout is a timeout`` () =
    let listener, port = listen None
    try
        raisesKind ErrorKind.Timeout (fun () -> Connection.Open(sprintf "frostlake://127.0.0.1:%d?timeout=300ms" port))
        |> ignore
    finally
        listener.Stop()

[<Fact>]
let ``a statement that outlives its timeout retires the connection`` () =
    let listener, port = listenStalling ()
    try
        use connection = Connection.Open(sprintf "frostlake://127.0.0.1:%d?timeout=300ms" port)
        let error = raisesKind ErrorKind.Timeout (fun () -> connection.Execute "SELECT 1")
        Assert.Contains("unknown", error.Message)
        Assert.False(connection.IsOpen)
        raisesKind ErrorKind.ConnectionClosed (fun () -> connection.Execute "SELECT 1") |> ignore
    finally
        listener.Stop()

[<Fact>]
let ``cancelling an ADO.NET command in flight retires its connection`` () =
    let listener, port = listenStalling ()
    try
        use connection = new FrostlakeConnection(sprintf "frostlake://127.0.0.1:%d?timeout=0" port)
        connection.Open()
        use command = connection.CreateCommand()
        command.CommandText <- "SELECT 1"
        use _timer = new Timer((fun _ -> command.Cancel()), null, 300, Timeout.Infinite)
        Assert.ThrowsAny<OperationCanceledException>(fun () -> command.ExecuteNonQuery() |> ignore) |> ignore
        Assert.Equal(Data.ConnectionState.Broken, connection.State)
        Assert.Throws<InvalidOperationException>(fun () -> connection.Open()) |> ignore
    finally
        listener.Stop()

[<Fact>]
let ``the asynchronous path reports the same failures`` () : Task =
    task {
        let listener, port = listen None
        try
            let! error =
                Assert.ThrowsAsync<FrostlakeException>(fun () ->
                    Connection.OpenAsync(sprintf "frostlake://127.0.0.1:%d?timeout=300ms" port) :> Task)
            Assert.Equal(ErrorKind.Timeout, error.Kind)
        finally
            listener.Stop()
    }

[<Fact>]
let ``a cancellation the caller asked for stays a cancellation`` () : Task =
    task {
        let listener, port = listen None
        try
            use source = new CancellationTokenSource(TimeSpan.FromMilliseconds 200.0)
            let! _ =
                Assert.ThrowsAnyAsync<OperationCanceledException>(fun () ->
                    Connection.OpenAsync(sprintf "frostlake://127.0.0.1:%d?timeout=0" port, source.Token) :> Task)
            ()
        finally
            listener.Stop()
    }

/// A listener that reads each whole request, the way a server does, and answers it with the body
/// `route` picks from its request line.
let private listenRouted (route: string -> string option) : TcpListener * int =
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    let handle (client: TcpClient) =
        try
            let stream = client.GetStream()
            let received = ResizeArray<byte>()
            let buffer = Array.zeroCreate<byte> 8192
            let mutable headerEnd = -1
            let mutable closed = false
            while headerEnd < 0 && not closed do
                let read = stream.Read(buffer, 0, buffer.Length)
                if read = 0 then
                    closed <- true
                else
                    received.AddRange(buffer.[.. read - 1])
                    headerEnd <- Encoding.ASCII.GetString(received.ToArray()).IndexOf("\r\n\r\n")
            if not closed then
                let head = Encoding.ASCII.GetString(received.ToArray(), 0, headerEnd)
                let lines = head.Split("\r\n")
                let length =
                    lines
                    |> Array.tryPick (fun line ->
                        if line.StartsWith("Content-Length:", StringComparison.OrdinalIgnoreCase) then
                            Some(int (line.Substring(15).Trim()))
                        else
                            None)
                    |> Option.defaultValue 0
                while received.Count < headerEnd + 4 + length do
                    let read = stream.Read(buffer, 0, buffer.Length)
                    received.AddRange(buffer.[.. read - 1])
                match route lines.[0] with
                | Some body ->
                    let reply =
                        sprintf
                            "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                            (Encoding.UTF8.GetByteCount body)
                            body
                    let bytes = Encoding.UTF8.GetBytes reply
                    stream.Write(bytes, 0, bytes.Length)
                | None -> ()
        with _ ->
            ()
        client.Dispose()
    let serve () =
        try
            while true do
                handle (listener.AcceptTcpClient())
        with _ ->
            ()
    Thread(serve, IsBackground = true).Start()
    listener, port

[<Fact>]
let ``the ADO.NET asynchronous surface does not deadlock a blocked UI-style thread`` () =
    let statement =
        """{"success":true,"sessionId":"a1","newSession":true,"resultSets":[{"columns":[{"name":"N","dataType":"NUMBER","precision":38,"scale":0}],"rows":[[7]],"updateCount":-1}]}"""
    let listener, port =
        listenRouted (fun line ->
            if line.StartsWith("GET /api/health", StringComparison.Ordinal) then
                Some """{"status":"healthy","activeSessions":1}"""
            elif line.StartsWith("POST /api/execute", StringComparison.Ordinal) then
                Some statement
            else
                Some """{"success":true,"sessionId":null,"newSession":false,"resultSets":[]}""")
    try
        use connection = new FrostlakeConnection(sprintf "frostlake://127.0.0.1:%d" port)
        underBlockedUiThread (fun () ->
            Assert.True(connection.OpenAsync().Wait(TimeSpan.FromSeconds 10.0), "OpenAsync never completed")
            use command = connection.CreateCommand()
            command.CommandText <- "SELECT 7 AS N"
            let scalar = command.ExecuteScalarAsync()
            Assert.True(scalar.Wait(TimeSpan.FromSeconds 10.0), "ExecuteScalarAsync never completed")
            Assert.Equal(box 7L, scalar.Result))
    finally
        listener.Stop()
