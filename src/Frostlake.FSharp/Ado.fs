namespace Frostlake.FSharp

open System
open System.Collections
open System.Collections.Generic
open System.Data
open System.Data.Common
open System.Globalization
open System.Numerics
open System.Threading
open System.Threading.Tasks

/// How the ADO.NET reader types a column. The type is settled once per result set from the values
/// the column actually holds, so GetFieldType agrees with GetValue for every row — what
/// DataTable.Load, data adapters and Dapper rely on. An integral column reads as int64 and widens
/// to decimal, then to its exact text, only when a value in it does not fit; the engine reports a
/// plain INTEGER as NUMBER(38,0), so the declared precision alone cannot decide.
module internal AdoTypes =
    let resolve (set: ResultSet) (ordinal: int) : Type =
        let all (accept: SqlValue -> bool) =
            set.Rows
            |> Array.forall (fun row ->
                match row.Values.[ordinal] with
                | SqlValue.Null -> true
                | value -> accept value)
        match ColumnKinds.classify set.Columns.[ordinal] with
        | ColumnKind.Integral ->
            if all (function
                    | SqlValue.Int _ -> true
                    | _ -> false)
            then
                typeof<int64>
            elif all (function
                      | SqlValue.Int _
                      | SqlValue.Decimal _ -> true
                      | SqlValue.BigInt v -> Cells.fitsDecimal v
                      | _ -> false)
            then
                typeof<decimal>
            else
                typeof<string>
        | ColumnKind.Decimal ->
            if all (function
                    | SqlValue.Decimal _
                    | SqlValue.Int _ -> true
                    | _ -> false)
            then
                typeof<decimal>
            else
                typeof<string>
        | ColumnKind.Floating ->
            if all (function
                    | SqlValue.Float _ -> true
                    | _ -> false)
            then
                typeof<float>
            else
                typeof<string>
        | ColumnKind.Boolean ->
            if all (function
                    | SqlValue.Bool _ -> true
                    | _ -> false)
            then
                typeof<bool>
            else
                typeof<string>
        | ColumnKind.Binary ->
            if all (function
                    | SqlValue.Binary _ -> true
                    | _ -> false)
            then
                typeof<byte[]>
            else
                typeof<string>
        | ColumnKind.Date
        | ColumnKind.Timestamp ->
            if all (function
                    | SqlValue.Date _
                    | SqlValue.Timestamp _ -> true
                    | _ -> false)
            then
                typeof<DateTime>
            else
                typeof<string>
        | ColumnKind.Time ->
            if all (function
                    | SqlValue.Time _ -> true
                    | _ -> false)
            then
                typeof<TimeSpan>
            else
                typeof<string>
        | ColumnKind.TimestampTz ->
            if all (function
                    | SqlValue.TimestampTz _ -> true
                    | _ -> false)
            then
                typeof<DateTimeOffset>
            else
                typeof<string>
        | ColumnKind.Variant
        | ColumnKind.Text -> typeof<string>

    let toClr (column: string) (target: Type) (value: SqlValue) : obj =
        match value with
        | SqlValue.Null -> box DBNull.Value
        | _ when target = typeof<int64> -> box (Cells.toInt64 column value)
        | _ when target = typeof<decimal> -> box (Cells.toDecimal column value)
        | _ when target = typeof<float> -> box (Cells.toDouble column value)
        | _ when target = typeof<bool> -> box (Cells.toBool column value)
        | _ when target = typeof<DateTime> -> box (Cells.toTimestamp column value)
        | _ when target = typeof<DateTimeOffset> -> box (Cells.toTimestampTz column value)
        | _ when target = typeof<TimeSpan> -> box ((Cells.toTime column value).ToTimeSpan())
        | _ when target = typeof<byte[]> -> box (Cells.toBytes column value)
        | _ -> box (Cells.toText column value)

    /// A command's time budget. Int32.MaxValue seconds is the usual way to say "no limit", and any
    /// budget past the longest timer .NET can arm means the same.
    let commandLimit (seconds: int option) (fallback: TimeSpan option) : TimeSpan option =
        match seconds with
        | Some 0 -> None
        | Some given when float given * 1000.0 > Limits.longest.TotalMilliseconds -> None
        | Some given -> Some(TimeSpan.FromSeconds(float given))
        | None -> fallback

    /// The update counts of a request added up, or -1 when no DML statement ran (the ADO.NET
    /// convention for ExecuteNonQuery and RecordsAffected).
    let recordsAffected (result: QueryResult) : int =
        let mutable total = -1L
        for set in result.ResultSets do
            match set.UpdateCount with
            | Some count -> total <- (if total < 0L then count else total + count)
            | None -> ()
        if total > int64 Int32.MaxValue then Int32.MaxValue else int total

/// An ADO.NET parameter. One without a name fills the next `?` placeholder; a named one fills its
/// `:name` and `@name` markers (a `:` or `@` on the parameter's own name is ignored). The value is
/// converted by its runtime type (see SqlValue.ofObj); DbType.Date and DbType.Time narrow a
/// DateTime to a DATE or a TIME.
[<AllowNullLiteral>]
type FrostlakeParameter(parameterName: string, value: obj) =
    inherit DbParameter()
    let mutable name = if isNull parameterName then "" else parameterName
    let mutable current = value
    let mutable dbType = DbType.Object
    let mutable dbTypeSet = false
    let mutable size = 0
    let mutable sourceColumn = ""
    let mutable sourceColumnNullMapping = false
    let mutable isNullable = false

    new() = FrostlakeParameter("", null)

    override _.DbType
        with get () = dbType
        and set (v: DbType) =
            dbType <- v
            dbTypeSet <- true

    override _.Direction
        with get () = ParameterDirection.Input
        and set (v: ParameterDirection) =
            if v <> ParameterDirection.Input then
                raise (NotSupportedException "parameters are bound client-side, so only input parameters exist")

    override _.IsNullable
        with get () = isNullable
        and set (v: bool) = isNullable <- v

    override _.ParameterName
        with get () = name
        and set (v: string) = name <- (if isNull v then "" else v)

    override _.Size
        with get () = size
        and set (v: int) = size <- v

    override _.SourceColumn
        with get () = sourceColumn
        and set (v: string) = sourceColumn <- (if isNull v then "" else v)

    override _.SourceColumnNullMapping
        with get () = sourceColumnNullMapping
        and set (v: bool) = sourceColumnNullMapping <- v

    override _.Value
        with get () = current
        and set (v: obj) = current <- v

    override _.ResetDbType() =
        dbType <- DbType.Object
        dbTypeSet <- false

    member internal _.ToSqlValue() : SqlValue =
        match current with
        | :? DateTime as v when dbTypeSet && dbType = DbType.Date -> SqlValue.Date(DateOnly.FromDateTime v)
        | :? DateTime as v when dbTypeSet && dbType = DbType.Time -> SqlValue.Time(TimeOnly.FromDateTime v)
        | other -> SqlValue.ofObj other

/// The parameters of a FrostlakeCommand. Names compare ignoring case and any `:` or `@` prefix.
[<Sealed>]
type FrostlakeParameterCollection() =
    inherit DbParameterCollection()
    let items = List<FrostlakeParameter>()

    let indexOfName (parameterName: string) =
        let wanted = Binding.bare parameterName
        items.FindIndex(fun p -> String.Equals(Binding.bare p.ParameterName, wanted, StringComparison.OrdinalIgnoreCase))

    let requireName (parameterName: string) =
        match indexOfName parameterName with
        | -1 -> raise (IndexOutOfRangeException(sprintf "no parameter named %s" parameterName))
        | index -> index

    let cast (value: obj) : FrostlakeParameter =
        match value with
        | :? FrostlakeParameter as parameter -> parameter
        | null -> raise (ArgumentNullException "value")
        | other ->
            raise (InvalidCastException(sprintf "a FrostlakeParameterCollection holds FrostlakeParameter objects, not %s" (other.GetType().Name)))

    override _.Count = items.Count
    override _.SyncRoot = (items :> ICollection).SyncRoot

    override _.Add(value: obj) =
        items.Add(cast value)
        items.Count - 1

    override _.AddRange(values: Array) =
        for value in values do
            items.Add(cast value)

    override _.Clear() = items.Clear()

    override _.Contains(value: obj) =
        match value with
        | :? FrostlakeParameter as parameter -> items.Contains parameter
        | _ -> false

    override _.Contains(value: string) = indexOfName value >= 0
    override _.CopyTo(array: Array, index: int) = (items :> ICollection).CopyTo(array, index)
    override _.GetEnumerator() = (items :> IEnumerable).GetEnumerator()
    override _.GetParameter(index: int) = items.[index] :> DbParameter
    override _.GetParameter(parameterName: string) = items.[requireName parameterName] :> DbParameter

    override _.IndexOf(value: obj) =
        match value with
        | :? FrostlakeParameter as parameter -> items.IndexOf parameter
        | _ -> -1

    override _.IndexOf(parameterName: string) = indexOfName parameterName
    override _.Insert(index: int, value: obj) = items.Insert(index, cast value)
    override _.Remove(value: obj) = items.Remove(cast value) |> ignore
    override _.RemoveAt(index: int) = items.RemoveAt index
    override _.RemoveAt(parameterName: string) = items.RemoveAt(requireName parameterName)
    override _.SetParameter(index: int, value: DbParameter) = items.[index] <- cast value
    override _.SetParameter(parameterName: string, value: DbParameter) = items.[requireName parameterName] <- cast value

    /// Add a parameter and hand it back.
    member _.Add(parameter: FrostlakeParameter) : FrostlakeParameter =
        items.Add parameter
        parameter

    /// Add a parameter by name and value. An empty name makes it positional.
    member this.AddWithValue(parameterName: string, value: obj) : FrostlakeParameter =
        this.Add(FrostlakeParameter(parameterName, value))

    member internal _.Binds() : SqlValue[] * (string * SqlValue)[] =
        let positional =
            [| for p in items do
                   if (Binding.bare p.ParameterName).Length = 0 then
                       p.ToSqlValue() |]
        let named =
            [| for p in items do
                   if (Binding.bare p.ParameterName).Length > 0 then
                       p.ParameterName, p.ToSqlValue() |]
        positional, named

/// An ADO.NET connection to a Frostlake engine, over the same session machinery as Connection.
/// The connection string is a DSN (frostlake://host:port/DATABASE?schema=S) or key=value pairs
/// (Host=…;Port=…;Database=…;Schema=…).
[<AllowNullLiteral>]
type FrostlakeConnection(connectionString: string) =
    inherit DbConnection()
    let mutable text = if isNull connectionString then "" else connectionString
    let mutable session: Connection option = None
    let mutable database = ""
    let mutable transaction: FrostlakeTransaction = null

    new() = new FrostlakeConnection("")

    member private this.Changed(before: ConnectionState, after: ConnectionState) =
        this.OnStateChange(StateChangeEventArgs(before, after))

    /// The session behind the connection, for the F# API: `Sql.existingConnection conn.Session`.
    member _.Session: Connection =
        match session with
        | Some connection -> connection
        | None -> raise (InvalidOperationException "the connection is not open")

    member internal _.Transaction
        with get () = transaction
        and set (value: FrostlakeTransaction) = transaction <- value

    override _.ConnectionString
        with get () = text
        and set (value: string) =
            if session.IsSome then
                raise (InvalidOperationException "cannot change the connection string of an open connection")
            text <- if isNull value then "" else value

    override _.Database =
        match session with
        | Some _ -> database
        | None ->
            match Dsn.tryParse text with
            | Ok config -> defaultArg config.Database ""
            | Error _ -> ""

    override _.DataSource =
        match Dsn.tryParse text with
        | Ok config -> config.BaseUri.ToString().TrimEnd('/')
        | Error _ -> ""

    override _.ConnectionTimeout =
        match Dsn.tryParse text with
        | Ok config when config.ConnectTimeout > TimeSpan.Zero -> int config.ConnectTimeout.TotalSeconds
        | _ -> 0

    override this.ServerVersion = this.Session.ServerVersion

    override _.State =
        match session with
        | Some connection when connection.IsOpen -> ConnectionState.Open
        | Some _ -> ConnectionState.Broken
        | None -> ConnectionState.Closed

    override _.DbProviderFactory = FrostlakeProviderFactory.Instance :> DbProviderFactory

    member private _.EnsureNotBroken() =
        match session with
        | Some connection when not connection.IsOpen ->
            raise (
                InvalidOperationException
                    "the connection is broken — a statement's outcome became unknown — so Close it before opening it again"
            )
        | _ -> ()

    override this.Open() =
        this.EnsureNotBroken()
        if session.IsNone then
            let config = Dsn.parse text
            session <- Some(Connection.Open config)
            database <- defaultArg config.Database ""
            this.Changed(ConnectionState.Closed, ConnectionState.Open)

    override this.OpenAsync(cancellationToken: CancellationToken) : Task =
        backgroundTask {
            this.EnsureNotBroken()
            if session.IsNone then
                let config = Dsn.parse text
                let! opened = Connection.OpenAsync(config, cancellationToken)
                session <- Some opened
                database <- defaultArg config.Database ""
                this.Changed(ConnectionState.Closed, ConnectionState.Open)
        }
        :> Task

    override this.Close() =
        match session with
        | Some connection ->
            transaction <- null
            session <- None
            connection.Close()
            this.Changed(ConnectionState.Open, ConnectionState.Closed)
        | None -> ()

    override this.CloseAsync() : Task =
        backgroundTask {
            match session with
            | Some connection ->
                transaction <- null
                session <- None
                do! connection.CloseAsync()
                this.Changed(ConnectionState.Open, ConnectionState.Closed)
            | None -> ()
        }
        :> Task

    override this.ChangeDatabase(databaseName: string) =
        if String.IsNullOrWhiteSpace databaseName then
            raise (ArgumentException "a database name is required")
        this.Session.Execute("USE DATABASE " + SqlText.identifier databaseName) |> ignore
        database <- databaseName

    override this.BeginDbTransaction(isolationLevel: IsolationLevel) : DbTransaction =
        if not (isNull transaction) then
            raise (InvalidOperationException "a transaction is already open on this connection")
        match isolationLevel with
        | IsolationLevel.Unspecified
        | IsolationLevel.ReadCommitted -> ()
        | other -> raise (NotSupportedException(sprintf "%O is not available; the engine offers read committed only" other))
        this.Session.BeginTransaction()
        transaction <- new FrostlakeTransaction(this)
        transaction :> DbTransaction

    override this.CreateDbCommand() = new FrostlakeCommand("", this) :> DbCommand

    override this.Dispose(disposing: bool) =
        if disposing then
            this.Close()
        base.Dispose(disposing)

/// An ADO.NET command. `?` placeholders take the unnamed parameters in order; `:name` and `@name`
/// markers take the named ones. A named parameter the text does not use is allowed — Dapper hands
/// over every property of its parameter object — but an unused positional one is an error.
and [<AllowNullLiteral>] FrostlakeCommand(commandText: string, connection: FrostlakeConnection) =
    inherit DbCommand()
    let mutable text = if isNull commandText then "" else commandText
    let mutable owner = connection
    let mutable transaction: FrostlakeTransaction = null
    let mutable timeoutSeconds: int option = None
    let mutable designTimeVisible = false
    let mutable updatedRowSource = UpdateRowSource.None
    let mutable cancellation: CancellationTokenSource = null
    let parameters = FrostlakeParameterCollection()

    new() = new FrostlakeCommand("", null)

    new(commandText: string) = new FrostlakeCommand(commandText, null)

    /// The command's parameters, typed.
    member _.Parameters = parameters

    override _.CommandText
        with get () = text
        and set (value: string) = text <- (if isNull value then "" else value)

    /// Seconds a statement may take; 0 waits indefinitely. Defaults to the connection string's timeout.
    override _.CommandTimeout
        with get () =
            match timeoutSeconds with
            | Some seconds -> seconds
            | None ->
                match (if isNull owner then None else Some owner) with
                | Some c when c.State = ConnectionState.Open ->
                    match c.Session.Config.Timeout with
                    | Some limit -> int (Math.Ceiling limit.TotalSeconds)
                    | None -> 0
                | _ -> 0
        and set (value: int) =
            if value < 0 then
                raise (ArgumentException "CommandTimeout cannot be negative")
            timeoutSeconds <- Some value

    override _.CommandType
        with get () = CommandType.Text
        and set (value: CommandType) =
            if value <> CommandType.Text then
                raise (NotSupportedException "only CommandType.Text is supported")

    override _.DesignTimeVisible
        with get () = designTimeVisible
        and set (value: bool) = designTimeVisible <- value

    override _.UpdatedRowSource
        with get () = updatedRowSource
        and set (value: UpdateRowSource) = updatedRowSource <- value

    override _.DbConnection
        with get () = owner :> DbConnection
        and set (value: DbConnection) =
            owner <-
                match value with
                | null -> null
                | :? FrostlakeConnection as c -> c
                | other -> raise (InvalidCastException(sprintf "a FrostlakeCommand runs on a FrostlakeConnection, not %s" (other.GetType().Name)))

    override _.DbParameterCollection = parameters :> DbParameterCollection

    override _.DbTransaction
        with get () = transaction :> DbTransaction
        and set (value: DbTransaction) =
            transaction <-
                match value with
                | null -> null
                | :? FrostlakeTransaction as t -> t
                | other -> raise (InvalidCastException(sprintf "a FrostlakeCommand takes a FrostlakeTransaction, not %s" (other.GetType().Name)))

    member private _.Prepared() : Connection * string * TimeSpan option =
        if isNull owner then
            raise (InvalidOperationException "the command has no connection")
        if owner.State <> ConnectionState.Open then
            raise (InvalidOperationException "the connection is not open")
        if String.IsNullOrWhiteSpace text then
            raise (InvalidOperationException "CommandText is empty")
        if not (isNull transaction) && not (Object.ReferenceEquals(transaction.Connection, owner)) then
            raise (InvalidOperationException "the command's transaction belongs to another connection")
        let session = owner.Session
        let positional, named = parameters.Binds()
        let sql = Binding.bind text positional named false
        session, sql, AdoTypes.commandLimit timeoutSeconds session.Config.Timeout

    member private _.Token(cancellationToken: CancellationToken) : CancellationTokenSource =
        let source = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken)
        cancellation <- source
        source

    member private this.Run(sync: bool, cancellationToken: CancellationToken) : Task<QueryResult> =
        task {
            let session, sql, limit = this.Prepared()
            use source = this.Token(cancellationToken)
            try
                return! session.RunCore(sql, None, limit, sync, source.Token)
            finally
                cancellation <- null
        }

    member private _.Reader(result: QueryResult, behavior: CommandBehavior) : DbDataReader =
        let closer = if behavior.HasFlag CommandBehavior.CloseConnection then owner else null
        new FrostlakeDataReader(result, closer) :> DbDataReader

    /// Cancels a statement in flight. Its outcome is then unknown, so the connection is retired.
    override _.Cancel() =
        match cancellation with
        | null -> ()
        | source -> source.Cancel()

    override _.Prepare() = ()

    override _.CreateDbParameter() = FrostlakeParameter() :> DbParameter

    override this.ExecuteDbDataReader(behavior: CommandBehavior) : DbDataReader =
        this.Reader(Sync.wait (this.Run(true, CancellationToken.None)), behavior)

    override this.ExecuteDbDataReaderAsync(behavior: CommandBehavior, cancellationToken: CancellationToken) =
        backgroundTask {
            let! result = this.Run(false, cancellationToken)
            return this.Reader(result, behavior)
        }

    /// The rows the statements changed, added up, or -1 when none was DML.
    override this.ExecuteNonQuery() =
        AdoTypes.recordsAffected (Sync.wait (this.Run(true, CancellationToken.None)))

    override this.ExecuteNonQueryAsync(cancellationToken: CancellationToken) =
        backgroundTask {
            let! result = this.Run(false, cancellationToken)
            return AdoTypes.recordsAffected result
        }

    /// The first cell of the first result set: DBNull for a NULL, null when there is no row.
    override this.ExecuteScalar() : obj =
        FrostlakeCommand.FirstCell(Sync.wait (this.Run(true, CancellationToken.None)))

    override this.ExecuteScalarAsync(cancellationToken: CancellationToken) =
        backgroundTask {
            let! result = this.Run(false, cancellationToken)
            return FrostlakeCommand.FirstCell result
        }

    static member private FirstCell(result: QueryResult) : obj =
        use reader = new FrostlakeDataReader(result, null)
        if reader.FieldCount > 0 && reader.Read() then reader.GetValue(0) else null

/// An ADO.NET transaction: BEGIN, then COMMIT or ROLLBACK. Disposing one that was not finished
/// rolls it back. The engine offers read committed only.
and [<AllowNullLiteral>] FrostlakeTransaction internal (connection: FrostlakeConnection) =
    inherit DbTransaction()
    let mutable finished = false

    member private _.Finish() =
        if finished then
            raise (InvalidOperationException "the transaction has already been committed or rolled back")
        finished <- true
        connection.Transaction <- null

    override _.DbConnection = (if finished then null else connection) :> DbConnection

    override _.IsolationLevel = IsolationLevel.ReadCommitted

    override this.Commit() =
        this.Finish()
        connection.Session.Commit()

    override this.Rollback() =
        this.Finish()
        connection.Session.Rollback()

    override this.CommitAsync(cancellationToken: CancellationToken) : Task =
        this.Finish()
        connection.Session.CommitAsync(cancellationToken)

    override this.RollbackAsync(cancellationToken: CancellationToken) : Task =
        this.Finish()
        connection.Session.RollbackAsync(cancellationToken)

    override _.Dispose(disposing: bool) =
        if disposing && not finished then
            finished <- true
            connection.Transaction <- null
            try
                if connection.State = ConnectionState.Open && connection.Session.InTransaction then
                    connection.Session.Rollback()
            with _ ->
                ()
        base.Dispose(disposing)

/// A forward-only reader over a request's result sets; NextResult walks one per statement.
///
/// Types: integral NUMBER reads as int64 (widening per column as described on AdoTypes), scaled
/// NUMBER as decimal, FLOAT as double, BOOLEAN as bool, DATE and TIMESTAMP_NTZ as DateTime,
/// TIMESTAMP_TZ and TIMESTAMP_LTZ as DateTimeOffset, TIME as TimeSpan, BINARY as byte[], and
/// everything else — VARIANT, OBJECT and ARRAY as their JSON text — as string. GetFieldValue also
/// answers DateOnly, TimeOnly, BigInteger, Guid and SqlValue.
and [<Sealed; AllowNullLiteral>] FrostlakeDataReader internal (result: QueryResult, owner: FrostlakeConnection) =
    inherit DbDataReader()
    let sets = result.ResultSets
    let mutable setIndex = 0
    let mutable rowIndex = -1
    let mutable closed = false
    let mutable types: Type[] = null

    let ensureOpen () =
        if closed then
            raise (InvalidOperationException "the reader is closed")

    let currentSet () : ResultSet option =
        if setIndex < sets.Length then Some sets.[setIndex] else None

    let requireSet () : ResultSet =
        ensureOpen ()
        match currentSet () with
        | Some set -> set
        | None -> raise (InvalidOperationException "no result set: the reader is past the last one")

    let column (ordinal: int) : Column =
        let set = requireSet ()
        if ordinal < 0 || ordinal >= set.Columns.Length then
            raise (IndexOutOfRangeException(sprintf "no column at ordinal %d" ordinal))
        set.Columns.[ordinal]

    let cell (ordinal: int) : SqlValue =
        let set = requireSet ()
        if ordinal < 0 || ordinal >= set.Columns.Length then
            raise (IndexOutOfRangeException(sprintf "no column at ordinal %d" ordinal))
        if rowIndex < 0 || rowIndex >= set.Rows.Length then
            raise (InvalidOperationException "no current row; call Read first")
        set.Rows.[rowIndex].Values.[ordinal]

    let columnType (ordinal: int) : Type =
        let set = requireSet ()
        if isNull types then
            types <- Array.zeroCreate set.Columns.Length
        match types.[ordinal] with
        | null ->
            let resolved = AdoTypes.resolve set ordinal
            types.[ordinal] <- resolved
            resolved
        | known -> known

    let chunk (available: int) (dataOffset: int64) (length: int) (bufferLength: int) (bufferOffset: int) : int =
        if dataOffset < 0L || dataOffset >= int64 available || length <= 0 then
            0
        else
            if bufferOffset < 0 || bufferOffset > bufferLength then
                raise (ArgumentOutOfRangeException("bufferOffset", "the offset is outside the buffer"))
            max 0 (min (min length (available - int dataOffset)) (bufferLength - bufferOffset))

    override _.Depth = 0

    override _.FieldCount =
        match currentSet () with
        | Some set -> set.Columns.Length
        | None -> 0

    override _.HasRows =
        match currentSet () with
        | Some set -> set.Rows.Length > 0
        | None -> false

    override _.IsClosed = closed

    override _.RecordsAffected = AdoTypes.recordsAffected result

    override this.Item
        with get (ordinal: int): obj = this.GetValue ordinal

    override this.Item
        with get (name: string): obj = this.GetValue(this.GetOrdinal name)

    override _.Read() =
        ensureOpen ()
        match currentSet () with
        | Some set when rowIndex < set.Rows.Length ->
            rowIndex <- rowIndex + 1
            rowIndex < set.Rows.Length
        | _ -> false

    override _.NextResult() =
        ensureOpen ()
        if setIndex >= sets.Length then
            false
        else
            setIndex <- setIndex + 1
            rowIndex <- -1
            types <- null
            setIndex < sets.Length

    override _.Close() =
        if not closed then
            closed <- true
            if not (isNull owner) then
                owner.Close()

    override this.GetEnumerator() = new DbEnumerator(this, false) :> IEnumerator

    override _.GetName(ordinal: int) = (column ordinal).Name

    override _.GetDataTypeName(ordinal: int) = (column ordinal).DataType

    override _.GetFieldType(ordinal: int) =
        column ordinal |> ignore
        columnType ordinal

    override _.GetOrdinal(name: string) =
        let columns = (requireSet ()).Columns
        match Array.tryFindIndex (fun (c: Column) -> String.Equals(c.Name, name, StringComparison.Ordinal)) columns with
        | Some index -> index
        | None ->
            match Array.tryFindIndex (fun (c: Column) -> String.Equals(c.Name, name, StringComparison.OrdinalIgnoreCase)) columns with
            | Some index -> index
            | None -> raise (IndexOutOfRangeException(sprintf "no column named %s" name))

    override _.IsDBNull(ordinal: int) =
        match cell ordinal with
        | SqlValue.Null -> true
        | _ -> false

    override _.GetValue(ordinal: int) : obj =
        let value = cell ordinal
        AdoTypes.toClr (column ordinal).Name (columnType ordinal) value

    override this.GetValues(values: obj[]) =
        if isNull values then
            raise (ArgumentNullException "values")
        let count = min values.Length this.FieldCount
        for i in 0 .. count - 1 do
            values.[i] <- this.GetValue i
        count

    override this.GetBoolean(ordinal: int) = Convert.ToBoolean(this.GetValue ordinal, CultureInfo.InvariantCulture)
    override this.GetByte(ordinal: int) = Convert.ToByte(this.GetValue ordinal, CultureInfo.InvariantCulture)
    override this.GetInt16(ordinal: int) = Convert.ToInt16(this.GetValue ordinal, CultureInfo.InvariantCulture)
    override this.GetInt32(ordinal: int) = Convert.ToInt32(this.GetValue ordinal, CultureInfo.InvariantCulture)
    override this.GetInt64(ordinal: int) = Convert.ToInt64(this.GetValue ordinal, CultureInfo.InvariantCulture)
    override this.GetDecimal(ordinal: int) = Convert.ToDecimal(this.GetValue ordinal, CultureInfo.InvariantCulture)
    override this.GetDouble(ordinal: int) = Convert.ToDouble(this.GetValue ordinal, CultureInfo.InvariantCulture)
    override this.GetFloat(ordinal: int) = Convert.ToSingle(this.GetValue ordinal, CultureInfo.InvariantCulture)

    /// The engine's text for the value — a DATE as 2024-01-02, BINARY as hex. NULL raises, as it
    /// does in other providers, rather than reading as an empty string.
    override _.GetString(ordinal: int) =
        match cell ordinal with
        | SqlValue.Null -> raise (InvalidCastException(sprintf "column %s is NULL" (column ordinal).Name))
        | value -> Cells.toText (column ordinal).Name value

    /// A zoned timestamp reads as its UTC instant; a DATE as its midnight.
    override this.GetDateTime(ordinal: int) =
        match this.GetValue ordinal with
        | :? DateTime as value -> value
        | :? DateTimeOffset as value -> value.UtcDateTime
        | _ -> Cells.toTimestamp (column ordinal).Name (cell ordinal)

    override _.GetGuid(ordinal: int) = Cells.toGuid (column ordinal).Name (cell ordinal)

    override this.GetChar(ordinal: int) =
        let text = this.GetString ordinal
        if text.Length > 0 then
            text.[0]
        else
            raise (InvalidCastException(sprintf "column %s is empty, so there is no character to read" (this.GetName ordinal)))

    override this.GetBytes(ordinal: int, dataOffset: int64, buffer: byte[], bufferOffset: int, length: int) =
        let bytes = Cells.toBytes (column ordinal).Name (cell ordinal)
        if isNull buffer then
            int64 bytes.Length
        else
            let count = chunk bytes.Length dataOffset length buffer.Length bufferOffset
            if count > 0 then
                Array.blit bytes (int dataOffset) buffer bufferOffset count
            int64 count

    override this.GetChars(ordinal: int, dataOffset: int64, buffer: char[], bufferOffset: int, length: int) =
        let text = this.GetString ordinal
        if isNull buffer then
            int64 text.Length
        else
            let count = chunk text.Length dataOffset length buffer.Length bufferOffset
            if count > 0 then
                text.CopyTo(int dataOffset, buffer, bufferOffset, count)
            int64 count

    override this.GetFieldValue<'T>(ordinal: int) : 'T =
        let value = cell ordinal
        let target = typeof<'T>
        if target = typeof<SqlValue> then
            unbox<'T> (box value)
        else
            match value with
            | SqlValue.Null when target = typeof<obj> || target = typeof<DBNull> -> unbox<'T> (box DBNull.Value)
            | SqlValue.Null ->
                if not target.IsValueType || not (isNull (Nullable.GetUnderlyingType target)) then
                    Unchecked.defaultof<'T>
                else
                    raise (InvalidCastException(sprintf "column %s is NULL" (column ordinal).Name))
            | _ ->
                let name = (column ordinal).Name
                let underlying =
                    match Nullable.GetUnderlyingType target with
                    | null -> target
                    | inner -> inner
                let converted: obj =
                    if underlying = typeof<DateOnly> then box (Cells.toDate name value)
                    elif underlying = typeof<TimeOnly> then box (Cells.toTime name value)
                    elif underlying = typeof<DateTimeOffset> then box (Cells.toTimestampTz name value)
                    elif underlying = typeof<BigInteger> then box (Cells.toBigInt name value)
                    elif underlying = typeof<Guid> then box (Cells.toGuid name value)
                    elif underlying = typeof<obj> then this.GetValue ordinal
                    else
                        match this.GetValue ordinal with
                        | clr when underlying.IsInstanceOfType clr -> clr
                        | clr -> Convert.ChangeType(clr, underlying, CultureInfo.InvariantCulture)
                unbox<'T> converted

    /// Describes the current result set the way DataTable.Load and data adapters expect.
    override this.GetSchemaTable() : DataTable =
        let schema = new DataTable("SchemaTable")
        let add (name: string) (t: Type) = schema.Columns.Add(name, t) |> ignore
        add SchemaTableColumn.ColumnName typeof<string>
        add SchemaTableColumn.ColumnOrdinal typeof<int>
        add SchemaTableColumn.ColumnSize typeof<int>
        add SchemaTableColumn.NumericPrecision typeof<int16>
        add SchemaTableColumn.NumericScale typeof<int16>
        add SchemaTableColumn.DataType typeof<Type>
        add SchemaTableColumn.ProviderType typeof<string>
        add SchemaTableColumn.IsLong typeof<bool>
        add SchemaTableColumn.AllowDBNull typeof<bool>
        add SchemaTableColumn.IsUnique typeof<bool>
        add SchemaTableColumn.IsKey typeof<bool>
        add SchemaTableColumn.IsAliased typeof<bool>
        add SchemaTableColumn.IsExpression typeof<bool>
        match currentSet () with
        | None -> ()
        | Some set ->
            for i in 0 .. set.Columns.Length - 1 do
                let c = set.Columns.[i]
                let row = schema.NewRow()
                row.[SchemaTableColumn.ColumnName] <- box c.Name
                row.[SchemaTableColumn.ColumnOrdinal] <- box i
                // The wire carries no length for text or binary, and a NUMBER's precision is not a
                // size: a wide NUMBER read as text would otherwise become a MaxLength it breaks.
                row.[SchemaTableColumn.ColumnSize] <- box -1
                row.[SchemaTableColumn.NumericPrecision] <- box (int16 c.Precision)
                row.[SchemaTableColumn.NumericScale] <- box (int16 c.Scale)
                row.[SchemaTableColumn.DataType] <- box (columnType i)
                row.[SchemaTableColumn.ProviderType] <- box c.DataType
                row.[SchemaTableColumn.IsLong] <- box false
                row.[SchemaTableColumn.AllowDBNull] <- box (defaultArg c.Nullable true)
                row.[SchemaTableColumn.IsUnique] <- box false
                row.[SchemaTableColumn.IsKey] <- box false
                row.[SchemaTableColumn.IsAliased] <- box false
                row.[SchemaTableColumn.IsExpression] <- box false
                schema.Rows.Add row
        schema

/// The ADO.NET provider factory, for code that goes through DbProviderFactories. Register the
/// instance — `DbProviderFactories.RegisterFactory("Frostlake.FSharp", FrostlakeProviderFactory.Instance)`
/// — because F# cannot declare the public static field the by-type overload looks for.
and [<Sealed; AllowNullLiteral>] FrostlakeProviderFactory private () =
    inherit DbProviderFactory()

    /// The singleton.
    static member val Instance = FrostlakeProviderFactory()

    override _.CreateConnection() = new FrostlakeConnection() :> DbConnection
    override _.CreateCommand() = new FrostlakeCommand() :> DbCommand
    override _.CreateParameter() = FrostlakeParameter() :> DbParameter
    override _.CreateConnectionStringBuilder() = DbConnectionStringBuilder()
