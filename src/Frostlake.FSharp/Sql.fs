namespace Frostlake.FSharp

open System
open System.Numerics
open System.Runtime.ExceptionServices
open System.Threading
open System.Threading.Tasks

/// The pipeline API:
///
/// ```fsharp
/// dsn
/// |> Sql.connect
/// |> Sql.query "SELECT ID, NAME FROM PEOPLE WHERE ID > :minimum"
/// |> Sql.parameters [ "minimum", Sql.int 1 ]
/// |> Sql.execute (fun read -> read.int "ID", read.string "NAME")
/// ```
///
/// `Sql.connect` opens a connection for each execution and closes it afterwards, so session state
/// does not carry from one to the next; `Sql.existingConnection` runs on a connection you hold.
[<RequireQualifiedAccess>]
module Sql =
    type internal Target =
        | FromDsn of string
        | FromConfig of ConnectionConfig
        | FromConnection of Connection

    /// A query being put together: where it runs, its text, and its arguments.
    type SqlProps =
        internal
            { Target: Target
              Query: string option
              Positional: SqlValue list
              Named: (string * SqlValue) list
              Timeout: TimeSpan option option
              MultiStatementCount: int option
              CancellationToken: CancellationToken }

    let private start (target: Target) : SqlProps =
        { Target = target
          Query = None
          Positional = []
          Named = []
          Timeout = None
          MultiStatementCount = None
          CancellationToken = CancellationToken.None }

    /// Run on a connection opened from a DSN or connection string for each execution.
    let connect (dsn: string) : SqlProps = start (FromDsn dsn)

    /// Run on a connection opened from a configuration for each execution.
    let connectWith (config: ConnectionConfig) : SqlProps = start (FromConfig config)

    /// Run on a connection you hold, in its session.
    let existingConnection (connection: Connection) : SqlProps = start (FromConnection connection)

    /// The SQL to run: one statement, or several separated by semicolons.
    let query (sql: string) (props: SqlProps) : SqlProps = { props with Query = Some sql }

    /// Named arguments for `:name` (or `@name`) markers. Each must be used by the statement.
    let parameters (named: (string * SqlValue) list) (props: SqlProps) : SqlProps = { props with Named = named }

    /// Positional arguments for `?` markers, in order.
    let args (values: SqlValue list) (props: SqlProps) : SqlProps = { props with Positional = values }

    /// How long the request may take, overriding the connection's timeout; zero waits indefinitely.
    let timeout (limit: TimeSpan) (props: SqlProps) : SqlProps =
        { props with Timeout = Some(if limit <= TimeSpan.Zero then None else Some limit) }

    /// How many statements the one request carries, for a query that packs several.
    ///
    /// The count travels with this request alone and outranks the session's MULTI_STATEMENT_COUNT
    /// without touching it, so there is nothing to save and put back; `0` accepts any number. A
    /// query that leaves this step out behaves as it always has — the session's value decides.
    ///
    /// It belongs to the query `Sql.query` names: `executeTransaction` sends each of its own
    /// statements as a request of its own, so a count set here does not reach them.
    let multiStatementCount (count: int) (props: SqlProps) : SqlProps =
        { props with MultiStatementCount = Some count }

    let cancellationToken (token: CancellationToken) (props: SqlProps) : SqlProps =
        { props with CancellationToken = token }

    let private render (props: SqlProps) : string =
        match props.Query with
        | None -> Fail.usage "no query to run; add one with Sql.query"
        | Some sql -> Binding.bind sql (Array.ofList props.Positional) (Array.ofList props.Named) true

    let private limitFor (props: SqlProps) (connection: Connection) =
        match props.Timeout with
        | Some limit -> limit
        | None -> connection.Config.Timeout

    let private withConnection (props: SqlProps) (sync: bool) (work: Connection -> Task<'T>) : Task<'T> =
        task {
            match props.Target with
            | FromConnection connection -> return! work connection
            | target ->
                let config =
                    match target with
                    | FromDsn dsn -> Dsn.parse dsn
                    | FromConfig config -> config
                    | FromConnection connection -> connection.Config
                let! connection =
                    Connection.OpenCore(
                        config,
                        HttpTransport(config.BaseUri, config.ConnectTimeout),
                        sync,
                        props.CancellationToken
                    )
                let mutable failure: exn = null
                let mutable result = Unchecked.defaultof<'T>
                try
                    let! value = work connection
                    result <- value
                with error ->
                    failure <- error
                do! connection.CloseCore(sync)
                if not (isNull failure) then
                    ExceptionDispatchInfo.Throw failure
                return result
        }

    let private run (props: SqlProps) (sync: bool) : Task<QueryResult> =
        task {
            let sql = render props
            return!
                withConnection props sync (fun connection ->
                    connection.RunCore(
                        sql,
                        props.MultiStatementCount,
                        limitFor props connection,
                        sync,
                        props.CancellationToken
                    ))
        }

    let private firstRow (result: QueryResult) : Row =
        let rows = result.Rows
        if rows.Length = 0 then
            Fail.usage "the query returned no rows"
        else
            rows.[0]

    /// Everything the request answered: a result set per statement.
    let executeResult (props: SqlProps) : QueryResult = Sync.wait (run props true)

    let executeResultAsync (props: SqlProps) : Task<QueryResult> = backgroundTask { return! run props false }

    /// Map every row of the final result set.
    let execute (read: Row -> 'T) (props: SqlProps) : 'T list =
        [ for row in (executeResult props).Rows -> read row ]

    let executeAsync (read: Row -> 'T) (props: SqlProps) : Task<'T list> =
        backgroundTask {
            let! result = run props false
            return [ for row in result.Rows -> read row ]
        }

    /// Map the first row of the final result set; raises when there is none.
    let executeRow (read: Row -> 'T) (props: SqlProps) : 'T = read (firstRow (executeResult props))

    let executeRowAsync (read: Row -> 'T) (props: SqlProps) : Task<'T> =
        backgroundTask {
            let! result = run props false
            return read (firstRow result)
        }

    /// The rows the request's DML statements changed, added up.
    let executeNonQuery (props: SqlProps) : int64 = (executeResult props).RowsAffected

    let executeNonQueryAsync (props: SqlProps) : Task<int64> =
        backgroundTask {
            let! result = run props false
            return result.RowsAffected
        }

    let private transaction
        (queries: (string * (string * SqlValue) list list) list)
        (props: SqlProps)
        (sync: bool)
        : Task<int64 list> =
        task {
            // Everything is bound before anything runs, so a binding mistake cannot leave a
            // transaction half done.
            let batches =
                [ for sql, sets in queries ->
                      [ for set in (if List.isEmpty sets then [ [] ] else sets) ->
                            Binding.bind sql [||] (Array.ofList set) true ] ]
            return!
                withConnection props sync (fun connection ->
                    task {
                        let limit = limitFor props connection
                        do! connection.BeginCore(sync, props.CancellationToken)
                        let counts = ResizeArray<int64>()
                        let mutable failure: exn = null
                        try
                            for statements in batches do
                                let mutable total = 0L
                                for sql in statements do
                                    let! result = connection.RunCore(sql, None, limit, sync, props.CancellationToken)
                                    total <- total + result.RowsAffected
                                counts.Add total
                        with error ->
                            failure <- error
                        if isNull failure then
                            do! connection.CommitCore(sync, props.CancellationToken)
                        else
                            if connection.InTransaction && connection.IsOpen then
                                try
                                    do! connection.RollbackCore(sync, CancellationToken.None)
                                with _ ->
                                    ()
                            ExceptionDispatchInfo.Throw failure
                        return List.ofSeq counts
                    })
        }

    /// Run each query once per parameter set (or once, given no sets) inside one transaction,
    /// committed at the end and rolled back on the first failure. Answers the rows each query changed.
    let executeTransaction (queries: (string * (string * SqlValue) list list) list) (props: SqlProps) : int64 list =
        Sync.wait (transaction queries props true)

    let executeTransactionAsync
        (queries: (string * (string * SqlValue) list list) list)
        (props: SqlProps)
        : Task<int64 list> =
        backgroundTask { return! transaction queries props false }

    // Values. These are defined last because they shadow the conversion functions of the same
    // names (int, string, float, …) for the rest of the module.

    let dbnull: SqlValue = SqlValue.Null

    let int (value: int) : SqlValue = SqlValue.Int(Operators.int64 value)

    let intOrNone (value: int option) : SqlValue =
        match value with
        | Some v -> SqlValue.Int(Operators.int64 v)
        | None -> SqlValue.Null

    let int64 (value: int64) : SqlValue = SqlValue.Int value

    let int64OrNone (value: int64 option) : SqlValue =
        match value with
        | Some v -> SqlValue.Int v
        | None -> SqlValue.Null

    let bigint (value: BigInteger) : SqlValue = SqlValue.BigInt value

    let bigintOrNone (value: BigInteger option) : SqlValue =
        match value with
        | Some v -> SqlValue.BigInt v
        | None -> SqlValue.Null

    let decimal (value: decimal) : SqlValue = SqlValue.Decimal value

    let decimalOrNone (value: decimal option) : SqlValue =
        match value with
        | Some v -> SqlValue.Decimal v
        | None -> SqlValue.Null

    let double (value: float) : SqlValue = SqlValue.Float value

    let doubleOrNone (value: float option) : SqlValue =
        match value with
        | Some v -> SqlValue.Float v
        | None -> SqlValue.Null

    let float (value: float) : SqlValue = SqlValue.Float value

    let floatOrNone (value: float option) : SqlValue = doubleOrNone value

    let text (value: string) : SqlValue =
        if isNull value then SqlValue.Null else SqlValue.Text value

    let textOrNone (value: string option) : SqlValue =
        match value with
        | Some v -> text v
        | None -> SqlValue.Null

    let string (value: string) : SqlValue = text value

    let stringOrNone (value: string option) : SqlValue = textOrNone value

    let bool (value: bool) : SqlValue = SqlValue.Bool value

    let boolOrNone (value: bool option) : SqlValue =
        match value with
        | Some v -> SqlValue.Bool v
        | None -> SqlValue.Null

    let bytes (value: byte[]) : SqlValue =
        if isNull value then SqlValue.Null else SqlValue.Binary value

    let bytesOrNone (value: byte[] option) : SqlValue =
        match value with
        | Some v -> bytes v
        | None -> SqlValue.Null

    let date (value: DateOnly) : SqlValue = SqlValue.Date value

    let dateOrNone (value: DateOnly option) : SqlValue =
        match value with
        | Some v -> SqlValue.Date v
        | None -> SqlValue.Null

    let time (value: TimeOnly) : SqlValue = SqlValue.Time value

    let timeOrNone (value: TimeOnly option) : SqlValue =
        match value with
        | Some v -> SqlValue.Time v
        | None -> SqlValue.Null

    /// A TIMESTAMP_NTZ, from the DateTime's wall-clock digits whatever its Kind.
    let timestamp (value: DateTime) : SqlValue = SqlValue.Timestamp value

    let timestampOrNone (value: DateTime option) : SqlValue =
        match value with
        | Some v -> SqlValue.Timestamp v
        | None -> SqlValue.Null

    /// A TIMESTAMP_TZ, keeping the offset.
    let timestamptz (value: DateTimeOffset) : SqlValue = SqlValue.TimestampTz value

    let timestamptzOrNone (value: DateTimeOffset option) : SqlValue =
        match value with
        | Some v -> SqlValue.TimestampTz v
        | None -> SqlValue.Null

    /// A Guid, as its text.
    let uuid (value: Guid) : SqlValue = SqlValue.Text(value.ToString())

    let uuidOrNone (value: Guid option) : SqlValue =
        match value with
        | Some v -> uuid v
        | None -> SqlValue.Null

    /// JSON text bound as a VARIANT: PARSE_JSON('…').
    let variant (json: string) : SqlValue =
        if isNull json then SqlValue.Null else SqlValue.Variant json

    let variantOrNone (json: string option) : SqlValue =
        match json with
        | Some v -> variant v
        | None -> SqlValue.Null

    /// Digits bound verbatim as a numeric literal, for a NUMBER wider than decimal holds.
    let decimalText (digits: string) : SqlValue = SqlValue.DecimalText digits

    /// SQL inlined exactly as written — `CURRENT_TIMESTAMP()`, a column name — and never escaped,
    /// so never pass it untrusted text.
    let raw (sql: string) : SqlValue = SqlValue.Raw sql

    /// Any .NET value, converted by its runtime type (see SqlValue.ofObj).
    let value (value: obj) : SqlValue = SqlValue.ofObj value
