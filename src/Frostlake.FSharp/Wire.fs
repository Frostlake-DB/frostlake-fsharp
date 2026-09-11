namespace Frostlake.FSharp

open System
open System.Globalization
open System.IO
open System.Text.Json

/// The JSON on the wire. `POST /api/execute` takes `{"sql", "sessionId", "requireSession",
/// "autoCommit", "multiStatementCount"}` and answers `{"success", "sessionId", "newSession", "errorMessage",
/// "executionTimeMs", "resultSets": [{"columns", "rows", "rowCount", "updateCount"}]}`.
///
/// Numbers are read from their raw text, never through a double first: a NUMBER(38,0) or a
/// NUMBER(20,10) holds values a double cannot name, and a decoder that parses before it knows the
/// column's type has already rounded them away.
module internal Wire =
    let private invariant = CultureInfo.InvariantCulture

    type Decoded =
        { Success: bool
          SessionId: string option
          /// None from a server that predates the field — which is also a server that ignores
          /// requireSession and has no DELETE /api/sessions.
          NewSession: bool option
          ErrorMessage: string option
          ExecutionTimeMs: int64
          ResultSets: ResultSet[] }

    let encodeExecute
        (sql: string)
        (sessionId: string option)
        (autoCommit: bool)
        (statements: int option)
        : byte[] =
        use stream = new MemoryStream()
        use writer = new Utf8JsonWriter(stream)
        writer.WriteStartObject()
        writer.WriteString("sql", sql)
        match sessionId with
        | Some id ->
            writer.WriteString("sessionId", id)
            // Resume this session or refuse: without it the server silently starts a fresh session
            // under the same id when the old one has expired, and the statement runs in the wrong
            // context. Servers that predate the field ignore it.
            writer.WriteBoolean("requireSession", true)
        | None -> ()
        writer.WriteBoolean("autoCommit", autoCommit)
        match statements with
        | Some count ->
            // A request carries one statement unless it says how many it holds; a request that sends
            // more without asking is refused, as it is on an account whose MULTI_STATEMENT_COUNT is
            // still 1. Servers that predate the field ignore it.
            writer.WriteNumber("multiStatementCount", count)
        | None -> ()
        writer.WriteEndObject()
        writer.Flush()
        stream.ToArray()

    let private integral (raw: string) : SqlValue =
        match Int64.TryParse(raw, NumberStyles.AllowLeadingSign, invariant) with
        | true, value -> SqlValue.Int value
        | _ ->
            match Cells.parseExact raw with
            | None -> SqlValue.Text raw
            | Some parts ->
                match Cells.integralOf parts with
                | Some value when Cells.fitsInt64 value -> SqlValue.Int(int64 value)
                | Some value -> SqlValue.BigInt value
                // A scale-0 column that answered with a fraction anyway keeps it rather than
                // being rounded to fit the declared type.
                | None ->
                    match Cells.decimalOf parts with
                    | Some value -> SqlValue.Decimal value
                    | None -> SqlValue.DecimalText raw

    let private scaled (raw: string) : SqlValue =
        match Cells.parseExact raw with
        | None -> SqlValue.Text raw
        | Some parts ->
            match Cells.decimalOf parts with
            | Some value -> SqlValue.Decimal value
            | None -> SqlValue.DecimalText raw

    let private floating (raw: string) : SqlValue =
        match raw with
        | "NaN" -> SqlValue.Float Double.NaN
        | "Infinity"
        | "inf" -> SqlValue.Float Double.PositiveInfinity
        | "-Infinity"
        | "-inf" -> SqlValue.Float Double.NegativeInfinity
        | _ ->
            match Double.TryParse(raw, NumberStyles.Float, invariant) with
            | true, value -> SqlValue.Float value
            | _ -> SqlValue.Text raw

    let private temporal (raw: string) : SqlValue =
        match Temporal.parseTimestamp raw with
        | Some(wall, Some minutes) ->
            match Temporal.tryOffset wall minutes with
            | Some instant -> SqlValue.TimestampTz instant
            // A DateTimeOffset holds instants in years 1 to 9999; past them the text survives.
            | None -> SqlValue.Text raw
        | Some(wall, None) -> SqlValue.Timestamp wall
        | None -> SqlValue.Text raw

    /// Decode one cell against its column. A cell whose text does not parse as its column's type
    /// is kept as Text rather than dropped.
    let decodeCell (kind: ColumnKind) (cell: JsonElement) : SqlValue =
        match cell.ValueKind with
        | JsonValueKind.Null
        | JsonValueKind.Undefined -> SqlValue.Null
        // A container that crossed as real JSON rather than as text (engines before 0.1.0 send
        // FILTER and TRANSFORM results this way) is still structured data, whatever the column says.
        | JsonValueKind.Object
        | JsonValueKind.Array -> SqlValue.Variant(cell.GetRawText())
        | valueKind ->
            let raw =
                if valueKind = JsonValueKind.String then
                    cell.GetString()
                else
                    cell.GetRawText()
            match kind with
            | ColumnKind.Integral when valueKind <> JsonValueKind.True && valueKind <> JsonValueKind.False -> integral raw
            | ColumnKind.Decimal when valueKind <> JsonValueKind.True && valueKind <> JsonValueKind.False -> scaled raw
            | ColumnKind.Floating -> floating raw
            | ColumnKind.Boolean ->
                match valueKind with
                | JsonValueKind.True -> SqlValue.Bool true
                | JsonValueKind.False -> SqlValue.Bool false
                | _ when String.Equals(raw, "true", StringComparison.OrdinalIgnoreCase) -> SqlValue.Bool true
                | _ when String.Equals(raw, "false", StringComparison.OrdinalIgnoreCase) -> SqlValue.Bool false
                | _ -> SqlValue.Text raw
            | ColumnKind.Binary when valueKind = JsonValueKind.String ->
                match Cells.tryHex raw with
                | Some bytes -> SqlValue.Binary bytes
                | None -> SqlValue.Text raw
            | ColumnKind.Variant -> SqlValue.Variant raw
            | ColumnKind.Date when valueKind = JsonValueKind.String ->
                match Temporal.parseDate raw with
                | Some date -> SqlValue.Date date
                | None -> SqlValue.Text raw
            | ColumnKind.Time when valueKind = JsonValueKind.String ->
                match Temporal.parseTime raw with
                | Some time -> SqlValue.Time time
                | None -> SqlValue.Text raw
            | ColumnKind.Timestamp
            | ColumnKind.TimestampTz when valueKind = JsonValueKind.String -> temporal raw
            | _ -> SqlValue.Text raw

    let private stringProperty (element: JsonElement) (name: string) : string =
        match element.TryGetProperty(name) with
        | true, value when value.ValueKind = JsonValueKind.String -> value.GetString()
        | _ -> ""

    let private intProperty (element: JsonElement) (name: string) : int =
        match element.TryGetProperty(name) with
        | true, value when value.ValueKind = JsonValueKind.Number ->
            match value.TryGetInt32() with
            | true, number -> number
            | _ -> 0
        | _ -> 0

    let private decodeColumn (element: JsonElement) : Column =
        { Name = stringProperty element "name"
          DataType = stringProperty element "dataType"
          Precision = intProperty element "precision"
          Scale = intProperty element "scale"
          Nullable =
            match element.TryGetProperty("nullable") with
            | true, value when value.ValueKind = JsonValueKind.True -> Some true
            | true, value when value.ValueKind = JsonValueKind.False -> Some false
            | _ -> None }

    /// A server that predates `updateCount` marks nothing, so a DML statement is recognised by its
    /// grid: one row whose columns are all named "number of …". The count adds up every "number of
    /// rows …" column — UPDATE carries a second "number of multi-joined rows updated" column, and
    /// MERGE one per action — the rule the engine's own JDBC driver applies.
    let private derivedUpdateCount (columns: Column[]) (rows: Row[]) : int64 option =
        let allCounts =
            columns.Length > 0
            && columns
               |> Array.forall (fun c ->
                   not (isNull c.Name)
                   && c.Name.StartsWith("number of ", StringComparison.OrdinalIgnoreCase))
        if rows.Length <> 1 || not allCounts then
            None
        else
            let mutable total = 0L
            let mutable counted = false
            for i in 0 .. columns.Length - 1 do
                if columns.[i].Name.StartsWith("number of rows", StringComparison.OrdinalIgnoreCase) then
                    match rows.[0].Values.[i] with
                    | SqlValue.Int count ->
                        total <- total + count
                        counted <- true
                    | _ -> ()
            if counted then Some total else None

    let private decodeResultSet (element: JsonElement) : ResultSet =
        let columns =
            match element.TryGetProperty("columns") with
            | true, value when value.ValueKind = JsonValueKind.Array ->
                [| for column in value.EnumerateArray() -> decodeColumn column |]
            | _ -> [||]
        let kinds = columns |> Array.map ColumnKinds.classify
        let index = ColumnIndex(columns)
        let rows =
            match element.TryGetProperty("rows") with
            | true, value when value.ValueKind = JsonValueKind.Array ->
                [| for row in value.EnumerateArray() ->
                       let cells =
                           if row.ValueKind = JsonValueKind.Array then
                               [| for cell in row.EnumerateArray() -> cell |]
                           else
                               [||]
                       let width = max cells.Length columns.Length
                       let values =
                           Array.init width (fun i ->
                               if i >= cells.Length then SqlValue.Null
                               elif i < kinds.Length then decodeCell kinds.[i] cells.[i]
                               else decodeCell ColumnKind.Text cells.[i])
                       Row(index, values) |]
            | _ -> [||]
        let updateCount =
            match element.TryGetProperty("updateCount") with
            | true, value when value.ValueKind = JsonValueKind.Number ->
                match value.TryGetInt64() with
                | true, count when count >= 0L -> Some count
                | _ -> None
            | true, value when value.ValueKind <> JsonValueKind.Null -> None
            | _ -> derivedUpdateCount columns rows
        ResultSet(columns, rows, updateCount)

    let private decodeBody (body: string) : Decoded option =
        let document =
            try
                Some(JsonDocument.Parse(body))
            with :? JsonException ->
                None
        match document with
        | None -> None
        | Some document ->
            use document = document
            let root = document.RootElement
            if root.ValueKind <> JsonValueKind.Object then
                None
            else
                let mutable success: bool option = None
                let mutable sessionId: string option = None
                let mutable newSession: bool option = None
                let mutable errorMessage: string option = None
                let mutable endpointError: string option = None
                let mutable executionTimeMs = 0L
                let mutable resultSets: ResultSet[] = [||]
                for property in root.EnumerateObject() do
                    let value = property.Value
                    match property.Name with
                    | "success" when value.ValueKind = JsonValueKind.True -> success <- Some true
                    | "success" when value.ValueKind = JsonValueKind.False -> success <- Some false
                    | "sessionId" when value.ValueKind = JsonValueKind.String -> sessionId <- Some(value.GetString())
                    | "newSession" when value.ValueKind = JsonValueKind.True -> newSession <- Some true
                    | "newSession" when value.ValueKind = JsonValueKind.False -> newSession <- Some false
                    | "errorMessage" when value.ValueKind = JsonValueKind.String ->
                        errorMessage <- Some(value.GetString())
                    // How the endpoint rejects a request rather than running it, e.g. a blank sql.
                    | "error" when value.ValueKind = JsonValueKind.String -> endpointError <- Some(value.GetString())
                    | "executionTimeMs" when value.ValueKind = JsonValueKind.Number ->
                        match value.TryGetInt64() with
                        | true, ms -> executionTimeMs <- ms
                        | _ -> ()
                    | "resultSets" when value.ValueKind = JsonValueKind.Array ->
                        resultSets <- [| for set in value.EnumerateArray() -> decodeResultSet set |]
                    | _ -> ()
                match success, endpointError with
                | None, None -> None
                | _ ->
                    Some
                        { Success = defaultArg success false
                          SessionId = sessionId
                          NewSession = newSession
                          ErrorMessage =
                            (match errorMessage with
                             | Some message -> Some message
                             | None -> endpointError)
                          ExecutionTimeMs = executionTimeMs
                          ResultSets = resultSets }

    /// Decode an answer from /api/execute, or None when the body is not a Frostlake answer at all.
    let decode (body: string) : Decoded option =
        try
            decodeBody body
        // JSON of the wrong shape — an array where an object belongs — is not one of our answers.
        with :? InvalidOperationException ->
            None

    /// Whether the health endpoint's body is a Frostlake health answer. A 200 on its own only says
    /// something is listening; the `status` field is what says it is an engine.
    let looksLikeHealth (body: string) : bool =
        try
            use document = JsonDocument.Parse(body)
            document.RootElement.ValueKind = JsonValueKind.Object
            && (match document.RootElement.TryGetProperty("status") with
                | true, _ -> true
                | _ -> false)
        with :? JsonException ->
            false

    /// Whether a body that failed to parse carries a bare `undefined` outside any string. Engines
    /// before 0.1.0 render a VARIANT undefined — what FILTER or TRANSFORM leave where an element
    /// was SQL NULL — as that bare token, which no JSON parser accepts. Telling it apart from a
    /// wrong address matters: one is a server defect on one statement, the other a misconfiguration.
    let carriesBareUndefined (body: string) : bool =
        let mutable i = 0
        let mutable found = false
        while not found && i < body.Length do
            match body.[i] with
            | '"' ->
                i <- i + 1
                while i < body.Length && body.[i] <> '"' do
                    i <- i + (if body.[i] = '\\' then 2 else 1)
                i <- i + 1
            | 'u' when String.CompareOrdinal(body, i, "undefined", 0, 9) = 0 -> found <- true
            | _ -> i <- i + 1
        found
