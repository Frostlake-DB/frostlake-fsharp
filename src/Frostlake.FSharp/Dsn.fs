namespace Frostlake.FSharp

open System
open System.Data.Common
open System.Globalization
open System.Threading

/// Where a connection goes and what its session starts with.
type ConnectionConfig =
    {
        /// http://host:port or https://host:port; every endpoint hangs off it.
        BaseUri: Uri
        /// Applied with USE DATABASE when a session starts.
        Database: string option
        /// Applied with USE SCHEMA when a session starts.
        Schema: string option
        /// Applied with USE ROLE when a session starts.
        Role: string option
        /// Applied with USE WAREHOUSE when a session starts.
        Warehouse: string option
        /// How long one request may take; None waits indefinitely.
        Timeout: TimeSpan option
        /// How long opening a TCP connection may take.
        ConnectTimeout: TimeSpan
    }

    /// localhost:18082, no scope, a five-minute request timeout and a ten-second connect timeout.
    static member Default =
        { BaseUri = Uri("http://localhost:18082")
          Database = None
          Schema = None
          Role = None
          Warehouse = None
          Timeout = Some(TimeSpan.FromMinutes 5.0)
          ConnectTimeout = TimeSpan.FromSeconds 10.0 }

module internal Limits =
    /// The longest span .NET can arm a timer or a connect timeout for: Int32.MaxValue
    /// milliseconds, about 24.8 days.
    let longest = TimeSpan.FromMilliseconds(float Int32.MaxValue)

/// Reading a DSN or an ADO.NET connection string into a ConnectionConfig.
///
/// ```
/// frostlake://host:port[/DATABASE[/SCHEMA]][?schema=S&role=R&warehouse=W&timeout=30s]
/// Host=localhost;Port=18082;Database=MY_DB;Schema=PUBLIC
/// ```
[<RequireQualifiedAccess>]
module Dsn =
    /// The port a frostlake:// DSN without one connects to.
    [<Literal>]
    let DefaultPort = 18082

    let private invariant = CultureInfo.InvariantCulture

    /// A duration written the way Go writes one — 30s, 5m, 1500ms, 1h, 2m30s — or bare digits,
    /// which are seconds.
    let internal parseDuration (key: string) (text: string) : TimeSpan =
        let refuse () : 'T =
            Fail.dsn (sprintf "%s \"%s\" is not a duration; write it as 30s, 5m, 1500ms or 0" key text)
        let body = text.Trim()
        if body.Length = 0 then
            refuse ()
        let mutable milliseconds = 0.0
        let mutable i = 0
        while i < body.Length do
            let start = i
            while i < body.Length && Char.IsAsciiDigit body.[i] do
                i <- i + 1
            if i = start then
                refuse ()
            let magnitude =
                match Int64.TryParse(body.Substring(start, i - start), NumberStyles.None, invariant) with
                | true, value -> value
                | _ -> refuse ()
            let unitStart = i
            while i < body.Length && Char.IsAsciiLetter body.[i] do
                i <- i + 1
            let factor =
                match body.Substring(unitStart, i - unitStart).ToLowerInvariant() with
                | ""
                | "s" -> 1000.0
                | "ms" -> 1.0
                | "m" -> 60000.0
                | "h" -> 3600000.0
                | unitText -> Fail.dsn (sprintf "%s unit \"%s\" is not one of ms, s, m or h" key unitText)
            milliseconds <- milliseconds + float magnitude * factor
        if milliseconds > Limits.longest.TotalMilliseconds then
            Fail.dsn (
                sprintf "%s \"%s\" is longer than the longest timeout .NET can keep (about 24 days); write 0 for no limit" key text
            )
        TimeSpan.FromMilliseconds milliseconds

    let private parseBool (key: string) (text: string) =
        match text.Trim().ToLowerInvariant() with
        | "true"
        | "1"
        | "yes"
        | "on" -> true
        | "false"
        | "0"
        | "no"
        | "off" -> false
        | _ -> Fail.dsn (sprintf "%s \"%s\" is not a boolean" key text)

    let private name (value: string) =
        if String.IsNullOrWhiteSpace value then None else Some(value.Trim())

    /// Apply one setting shared by both spellings. Credentials are refused rather than dropped: the
    /// engine's HTTP API has no authentication, and quietly discarding a password would suggest
    /// otherwise.
    let private apply (config: ConnectionConfig) (key: string) (value: string) : ConnectionConfig =
        match key.Trim().ToLowerInvariant() with
        | "database"
        | "db" -> { config with Database = name value }
        | "schema" -> { config with Schema = name value }
        | "role" -> { config with Role = name value }
        | "warehouse" -> { config with Warehouse = name value }
        | "timeout"
        | "command timeout"
        | "commandtimeout" ->
            let limit = parseDuration key value
            { config with Timeout = (if limit = TimeSpan.Zero then None else Some limit) }
        | "connecttimeout"
        | "connect_timeout"
        | "connect timeout"
        | "connection timeout" ->
            let limit = parseDuration key value
            { config with ConnectTimeout = (if limit = TimeSpan.Zero then Timeout.InfiniteTimeSpan else limit) }
        | "user"
        | "username"
        | "user id"
        | "userid"
        | "uid"
        | "password"
        | "pwd" ->
            Fail.dsn
                "the engine's HTTP API has no authentication, so a user or password would be silently discarded; leave them out"
        | _ -> Fail.dsn (sprintf "unknown connection setting \"%s\"" key)

    let private isTls (key: string) =
        match key.Trim().ToLowerInvariant() with
        | "tls"
        | "ssl"
        | "use tls" -> true
        | _ -> false

    let private baseUri (scheme: string) (host: string) (port: int) =
        let bracketed =
            if host.Contains(':') && not (host.StartsWith("[", StringComparison.Ordinal)) then
                "[" + host + "]"
            else
                host
        match Uri.TryCreate(sprintf "%s://%s:%d" scheme bracketed port, UriKind.Absolute) with
        | true, uri -> uri
        | _ -> Fail.dsn (sprintf "\"%s\" is not a host name or address" host)

    let private parseUrl (text: string) : ConnectionConfig =
        let separator = text.IndexOf("://", StringComparison.Ordinal)
        let schemeText = text.Substring(0, separator).ToLowerInvariant()
        let mutable scheme =
            match schemeText with
            | "frostlake"
            | "http" -> "http"
            | "https" -> "https"
            | _ -> Fail.dsn (sprintf "DSN scheme \"%s\" is not one of frostlake, http or https" schemeText)
        let uri =
            match Uri.TryCreate(text, UriKind.Absolute) with
            | true, parsed -> parsed
            | _ -> Fail.dsn (sprintf "\"%s\" is not a valid DSN" text)
        if uri.UserInfo.Length > 0 then
            Fail.dsn
                "the DSN carries a user or password, which the engine's HTTP API has no way to use; leave them out"
        if String.IsNullOrEmpty uri.Host then
            Fail.dsn (sprintf "DSN \"%s\" names no host" text)
        let port = if uri.Port < 0 then DefaultPort else uri.Port
        let segments =
            uri.AbsolutePath.Split('/', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map Uri.UnescapeDataString
        if segments.Length > 2 then
            Fail.dsn (sprintf "the DSN path names at most a database and a schema, got \"%s\"" uri.AbsolutePath)
        let mutable config =
            { ConnectionConfig.Default with
                Database = (if segments.Length > 0 then name segments.[0] else None)
                Schema = (if segments.Length > 1 then name segments.[1] else None) }
        for pair in uri.Query.TrimStart('?').Split('&', StringSplitOptions.RemoveEmptyEntries) do
            let equals = pair.IndexOf('=')
            let key = Uri.UnescapeDataString(if equals < 0 then pair else pair.Substring(0, equals))
            let value = if equals < 0 then "" else Uri.UnescapeDataString(pair.Substring(equals + 1))
            if isTls key then
                scheme <- if parseBool key value then "https" else "http"
            else
                let lowered = key.Trim().ToLowerInvariant()
                if (lowered = "database" || lowered = "db") && segments.Length > 0 then
                    Fail.dsn "the DSN names its database twice, in the path and as a parameter"
                if lowered = "schema" && segments.Length > 1 then
                    Fail.dsn "the DSN names its schema twice, in the path and as a parameter"
                config <- apply config key value
        { config with BaseUri = baseUri scheme uri.Host port }

    let private portNumber (text: string) : int =
        match Int32.TryParse(text.Trim(), NumberStyles.None, invariant) with
        | true, parsed when parsed > 0 && parsed <= 65535 -> parsed
        | _ -> Fail.dsn (sprintf "Port \"%s\" is not a port number" text)

    let private parseKeyValue (text: string) : ConnectionConfig =
        let builder =
            try
                DbConnectionStringBuilder(ConnectionString = text)
            with :? ArgumentException as e ->
                Fail.dsn (sprintf "the connection string cannot be read: %s" e.Message)
        let mutable host = "localhost"
        let mutable hostPort: int option = None
        let mutable port: int option = None
        let mutable scheme = "http"
        let mutable config = ConnectionConfig.Default
        for key in Seq.cast<string> builder.Keys do
            let value = string builder.[key]
            match key.Trim().ToLowerInvariant() with
            | "host"
            | "server"
            | "data source" ->
                // host:port the way some tools write it; a bare IPv6 address has more than one colon.
                let written = value.Trim()
                let colon = written.LastIndexOf(':')
                if written.StartsWith("[", StringComparison.Ordinal) && written.Contains("]:") then
                    let close = written.IndexOf("]:", StringComparison.Ordinal)
                    host <- written.Substring(0, close + 1)
                    hostPort <- Some(portNumber (written.Substring(close + 2)))
                elif colon > 0 && written.IndexOf(':') = colon then
                    host <- written.Substring(0, colon)
                    hostPort <- Some(portNumber (written.Substring(colon + 1)))
                else
                    host <- written
            | "port" -> port <- Some(portNumber value)
            | _ when isTls key -> scheme <- if parseBool key value then "https" else "http"
            | _ -> config <- apply config key value
        if host.Length = 0 then
            Fail.dsn "the connection string names no host"
        let chosen =
            match hostPort, port with
            | Some written, Some given when written <> given ->
                Fail.dsn (sprintf "the connection string names two ports, %d and %d" written given)
            | Some written, _ -> written
            | None, Some given -> given
            | None, None -> DefaultPort
        { config with BaseUri = baseUri scheme host chosen }

    /// Read a DSN (frostlake://, http:// or https://) or an ADO.NET connection string.
    ///
    /// DSN parameters and connection-string keys: database (or db), schema, role, warehouse,
    /// timeout (a duration, or bare seconds; 0 waits indefinitely), connectTimeout, and tls. An
    /// unknown setting is an error rather than a silent no-op, and so is a user or password.
    let parse (text: string) : ConnectionConfig =
        try
            if String.IsNullOrWhiteSpace text then
                Fail.dsn "the DSN is empty"
            elif text.Contains("://") then
                parseUrl (text.Trim())
            else
                parseKeyValue text
        with
        | :? FrostlakeException -> reraise ()
        // Whatever else the framework's parsers raise is still a DSN that cannot be honoured.
        | error -> Fail.dsn (sprintf "the DSN cannot be read: %s" error.Message)

    /// Like parse, answering with the reason instead of raising.
    let tryParse (text: string) : Result<ConnectionConfig, string> =
        try
            Ok(parse text)
        with :? FrostlakeException as e ->
            Error e.Message
