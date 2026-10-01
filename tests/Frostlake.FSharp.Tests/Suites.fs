/// Runs the engine's language-neutral testkit corpus through this driver, with the execution
/// contract, normalisation and capability rules the corpus's SCHEMA.md lays down and the Java
/// reference runner applies. The report is TSV in the reference runner's shape, so the two can be
/// compared case for case.
///
/// It runs only when FL_CORPUS names frostlake's engine/src/test/resources/testkit, whose suites/*.json
/// it replays — the corpus tracks the engine it lives with, so an older engine is expected to fail
/// cases of a newer corpus. FROSTLAKE_TESTKIT_REPORT names the report file.
module Frostlake.FSharp.Tests.Suites

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Numerics
open System.Text
open System.Text.Json
open Frostlake.FSharp
open Frostlake.FSharp.Tests.TestSupport
open Xunit
open Xunit.Abstractions

let private backend = "http"

let private corpusDirectory () : string option =
    match Environment.GetEnvironmentVariable "FL_CORPUS" with
    | path when not (String.IsNullOrWhiteSpace path) -> Some path
    | _ -> None

/// The testkit directory's suites/*.json in name order; empty when it has none, and so is not the corpus.
let private suiteFiles (corpus: string) : string array =
    let suites = Path.Combine(corpus, "suites")
    if Directory.Exists suites then Directory.GetFiles(suites, "*.json") |> Array.sort else [||]

/// Skipped without FL_CORPUS, and without an engine — but an FL_CORPUS that names no corpus is never
/// skipped: the test fails on it, naming the value.
type CorpusFactAttribute() as this =
    inherit FactAttribute()

    do
        match corpusDirectory () with
        | None -> this.Skip <- "set FL_CORPUS to frostlake's engine/src/test/resources/testkit to replay the testkit corpus"
        | Some corpus when not engineConfigured && (suiteFiles corpus).Length > 0 ->
            this.Skip <- "set FROSTLAKE_CLASSPATH or FROSTLAKE_URL to run the testkit corpus"
        | Some _ -> ()

/// SCHEMA.md's value normalisation: NULL, case-insensitive booleans, a number rounded to ten
/// significant digits (half up, trailing zeros stripped, written plain), else the trimmed text.
let norm (value: string) : string =
    if isNull value then
        "NULL"
    else
        let v = value.Trim()
        if v.Length = 0 || v.Equals("null", StringComparison.OrdinalIgnoreCase) then "NULL"
        elif v.Equals("true", StringComparison.OrdinalIgnoreCase) then "TRUE"
        elif v.Equals("false", StringComparison.OrdinalIgnoreCase) then "FALSE"
        else
            match Cells.parseExact v with
            | None -> v
            | Some(unscaled, _) when unscaled.IsZero -> "0"
            | Some(unscaled, scale) ->
                let ten = BigInteger(10)
                let negative = unscaled.Sign < 0
                let mutable u = BigInteger.Abs unscaled
                let mutable s = scale
                let digits = u.ToString(CultureInfo.InvariantCulture).Length
                if digits > 10 then
                    let drop = digits - 10
                    let divisor = BigInteger.Pow(ten, drop)
                    let quotient, remainder = BigInteger.DivRem(u, divisor)
                    u <- if remainder * BigInteger(2) >= divisor then quotient + BigInteger.One else quotient
                    s <- s - drop
                while not u.IsZero && (BigInteger.Remainder(u, ten)).IsZero do
                    u <- BigInteger.Divide(u, ten)
                    s <- s - 1
                let text = u.ToString(CultureInfo.InvariantCulture)
                let plain =
                    if s <= 0 then text + String('0', -s)
                    elif text.Length > s then text.Substring(0, text.Length - s) + "." + text.Substring(text.Length - s)
                    else "0." + String('0', s - text.Length) + text
                (if negative then "-" else "") + plain

let private fraction (ticks: int64) (minimum: int) : string =
    // The wire writes as many fractional digits as the value needs — 3, 6 or 9 — and a timestamp
    // never fewer than three.
    let digits = (ticks % TimeSpan.TicksPerSecond).ToString("0000000", CultureInfo.InvariantCulture) + "00"
    let needed =
        if digits.Substring(3) = "000000" then 3
        elif digits.Substring(6) = "000" then 6
        else 9
    let width = max minimum needed
    if width = 0 || (minimum = 0 && digits = "000000000") then "" else "." + digits.Substring(0, width)

/// A decoded cell written back the way the engine sends it, so the corpus's expectations (which
/// are the wire's text) compare against what the driver actually decoded.
let wireText (value: SqlValue) : string =
    let invariant = CultureInfo.InvariantCulture
    match value with
    | SqlValue.Null -> null
    | SqlValue.Bool v -> if v then "true" else "false"
    | SqlValue.Int v -> v.ToString(invariant)
    | SqlValue.BigInt v -> v.ToString(invariant)
    | SqlValue.Decimal v -> v.ToString(invariant)
    | SqlValue.DecimalText v
    | SqlValue.Text v
    | SqlValue.Variant v
    | SqlValue.Raw v -> v
    | SqlValue.Float v when Double.IsNaN v -> "NaN"
    | SqlValue.Float v when Double.IsPositiveInfinity v -> "Infinity"
    | SqlValue.Float v when Double.IsNegativeInfinity v -> "-Infinity"
    | SqlValue.Float v -> v.ToString("R", invariant)
    | SqlValue.Binary v -> Convert.ToHexString v
    | SqlValue.Date v -> v.ToString("yyyy-MM-dd", invariant)
    | SqlValue.Time v -> v.ToString("HH:mm:ss", invariant) + fraction v.Ticks 0
    | SqlValue.Timestamp v -> v.ToString("yyyy-MM-dd HH:mm:ss", invariant) + fraction v.Ticks 3
    | SqlValue.TimestampTz v ->
        let offset = v.Offset
        let sign = if offset < TimeSpan.Zero then "-" else "+"
        v.ToString("yyyy-MM-dd HH:mm:ss", invariant)
        + fraction v.Ticks 3
        + sprintf " %s%02d%02d" sign (abs offset.Hours) (abs offset.Minutes)

/// Whether a column carries semi-structured values, read from the type the engine reported. The
/// gate is the column's type and never the cell's shape: a VARCHAR whose content happens to look
/// like "quoted" is that text, and stays it.
let private semiStructured (dataType: string) : bool =
    if isNull dataType then
        false
    else
        let trimmed = dataType.Trim()
        let paren = trimmed.IndexOf '('
        let name = (if paren >= 0 then trimmed.Substring(0, paren) else trimmed).Trim()
        match name.ToUpperInvariant() with
        | "VARIANT"
        | "OBJECT"
        | "ARRAY" -> true
        | _ -> false

/// The value a semi-structured cell carries, as the corpus records it.
///
/// A client is handed a VARIANT, OBJECT or ARRAY cell as its JSON text — a string's own quotes
/// included — which is what the account's own drivers do. The corpus records the value instead: a
/// rather than "a", and an object as its own text rather than as a string holding that text. One
/// level of decoding covers both: a cell that is a JSON string becomes that string's contents, and
/// anything else — a number, a boolean, text that is not JSON at all — is left exactly as it came.
let private semiStructuredText (cell: string) : string =
    if isNull cell then
        null
    else
        try
            use document = JsonDocument.Parse cell
            if document.RootElement.ValueKind = JsonValueKind.String then
                document.RootElement.GetString()
            else
                cell
        with :? JsonException ->
            // Text that is not JSON at all is a value in its own right.
            cell

type private Outcome =
    { Columns: string list
      Rows: string list list option
      UpdateCount: int64
      Error: string option }

let private run (connection: Connection) (sql: string) : Outcome =
    try
        let result = connection.Execute sql
        if result.ResultSets.Length = 0 then
            { Columns = []; Rows = None; UpdateCount = -1L; Error = None }
        else
            // The reference runner reads the first result set, and derives an update count from its
            // grid: one row whose columns are all named "number of …" counts its first cell.
            let first = result.ResultSets.[0]
            let columns = [ for c in first.Columns -> c.Name ]
            let semi = [| for c in first.Columns -> semiStructured c.DataType |]
            let cell (index: int) (value: SqlValue) =
                let text = wireText value
                if index < semi.Length && semi.[index] then semiStructuredText text else text
            let rows = [ for row in first.Rows -> [ for i in 0 .. row.Values.Length - 1 -> cell i row.Values.[i] ] ]
            let updateCount =
                if not columns.IsEmpty
                   && rows.Length = 1
                   && columns |> List.forall (fun c -> not (isNull c) && c.ToLowerInvariant().StartsWith "number of") then
                    match Int64.TryParse((List.head rows.[0]).Trim(), NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
                    | true, count -> count
                    | _ -> -1L
                else
                    -1L
            { Columns = columns; Rows = Some rows; UpdateCount = updateCount; Error = None }
    with :? FrostlakeException as e when e.Kind = ErrorKind.Refused ->
        { Columns = []; Rows = None; UpdateCount = -1L; Error = Some e.Message }

let private text (element: JsonElement) : string =
    match element.ValueKind with
    | JsonValueKind.Null -> null
    | JsonValueKind.String -> element.GetString()
    | JsonValueKind.True -> "true"
    | JsonValueKind.False -> "false"
    | _ -> element.GetRawText()

let private property (element: JsonElement) (name: string) : JsonElement option =
    if element.ValueKind <> JsonValueKind.Object then
        None
    else
        match element.TryGetProperty(name) with
        | true, value -> Some value
        | _ -> None

let private canonical (grid: string list list) (ordered: bool) : string list =
    let rows = [ for row in grid -> String.Join("", [ for cell in row -> norm cell + "\u001f" ]) ]
    if ordered then rows else List.sortWith (fun a b -> String.CompareOrdinal(a, b)) rows

/// Check one step's expectations; None when they hold, Some detail when one fails.
let private check (expect: JsonElement option) (outcome: Outcome) : string option =
    match expect |> Option.bind (fun e -> property e "error") with
    | Some error ->
        match outcome.Error with
        | None -> Some "expected an error, statement succeeded"
        | Some message ->
            match property error "messageContains" |> Option.map text with
            | Some wanted when not (message.ToLowerInvariant().Contains(wanted.ToLowerInvariant())) ->
                Some(sprintf "error message [%s] does not contain [%s]" message wanted)
            | _ -> None
    | None ->
        match outcome.Error, expect with
        | Some message, _ -> Some("unexpected error: " + message)
        | None, None -> None
        | None, Some expect ->
            let firstRow = outcome.Rows |> Option.bind List.tryHead
            let failures =
                [ match property expect "value" with
                  | Some wanted ->
                      let actual = firstRow |> Option.bind List.tryHead |> Option.defaultValue null
                      if norm (text wanted) <> norm actual then
                          yield sprintf "value [%s] != expected [%s]" actual (text wanted)
                  | None -> ()
                  match property expect "rows" with
                  | Some wanted ->
                      let expected = [ for row in wanted.EnumerateArray() -> [ for cell in row.EnumerateArray() -> text cell ] ]
                      let ordered =
                          match property expect "ordered" with
                          | Some flag -> flag.ValueKind = JsonValueKind.True
                          | None -> false
                      let w = canonical expected ordered
                      let g = canonical (defaultArg outcome.Rows []) ordered
                      if w <> g then
                          yield sprintf "rows differ: expected %A got %A" w g
                  | None -> ()
                  match property expect "rowCount" with
                  | Some wanted ->
                      let got = outcome.Rows |> Option.map List.length |> Option.defaultValue 0
                      if got <> wanted.GetInt32() then
                          yield sprintf "rowCount %d != expected %d" got (wanted.GetInt32())
                  | None -> ()
                  match property expect "columns" with
                  | Some wanted ->
                      let expected = [ for c in wanted.EnumerateArray() -> text c ]
                      if expected.Length <> outcome.Columns.Length then
                          yield sprintf "column count %d != expected %d %A" outcome.Columns.Length expected.Length outcome.Columns
                      else
                          for i in 0 .. expected.Length - 1 do
                              if not (expected.[i].Equals(outcome.Columns.[i], StringComparison.OrdinalIgnoreCase)) then
                                  yield sprintf "column[%d] [%s] != expected [%s]" i outcome.Columns.[i] expected.[i]
                  | None -> ()
                  match property expect "updateCount" with
                  | Some wanted when outcome.UpdateCount <> wanted.GetInt64() ->
                      yield sprintf "updateCount %d != expected %d" outcome.UpdateCount (wanted.GetInt64())
                  | _ -> () ]
            List.tryHead failures

let private skipReason (test: JsonElement) : string option =
    match property test "skip" with
    | Some skip ->
        match property skip "backends" with
        | Some backends when
            backends.EnumerateArray()
            |> Seq.exists (fun b -> String.Equals(text b, backend, StringComparison.OrdinalIgnoreCase))
            ->
            Some(
                match property skip "reason" with
                | Some reason -> text reason
                | None -> "skipped for " + backend
            )
        | _ -> None
    | None -> None

type Corpus(output: ITestOutputHelper) =
    [<CorpusFact>]
    member _.``the testkit corpus runs over the driver``() =
        let corpus = (corpusDirectory ()).Value
        let files = suiteFiles corpus
        if files.Length = 0 then
            failwithf
                "FL_CORPUS is %s, but there are no *.json suites in %s (set it to frostlake's engine/src/test/resources/testkit)"
                corpus
                (Path.GetFullPath(Path.Combine(corpus, "suites")))
        let report = StringBuilder("suite\ttest\tstatus\tfailedStep\tdetail\tms\n")
        let counts = Dictionary<string, int>()
        let failures = List<string>()
        let record (suite: string) (test: string) (status: string) (step: int) (detail: string) (ms: int64) =
            counts.[status] <- (match counts.TryGetValue status with | true, n -> n + 1 | _ -> 1)
            report
                .Append(suite).Append('\t').Append(test).Append('\t').Append(status).Append('\t')
                .Append(if step < 0 then "" else string step).Append('\t')
                .Append((if isNull detail then "" else detail).Replace('\t', ' ').Replace('\n', ' ')).Append('\t')
                .Append(ms).Append('\n')
            |> ignore
            if status = "FAIL" || status = "ERROR" then
                failures.Add(sprintf "%s/%s %s: %s" suite test status detail)
        let started = Diagnostics.Stopwatch.StartNew()
        let mutable connection = Connection.Open(engineDsn ())
        for file in files do
            use document = JsonDocument.Parse(File.ReadAllText file)
            let root = document.RootElement
            let suite =
                match property root "suite" with
                | Some name -> text name
                | None -> Path.GetFileNameWithoutExtension file
            match property root "tests" with
            | None -> ()
            | Some tests ->
                for test in tests.EnumerateArray() do
                    let name = property test "name" |> Option.map text |> Option.defaultValue ""
                    match skipReason test with
                    | Some reason -> record suite name "SKIP" -1 reason 0L
                    | None ->
                        let clock = Diagnostics.Stopwatch.StartNew()
                        try
                            if not connection.IsOpen then
                                connection <- Connection.Open(engineDsn ())
                            for sql in
                                // A session runs one statement per request until it asks for more, so
                                // a case whose step sends several would be refused on the count rather
                                // than answered; 0 means any number of them.
                                [ "ALTER SESSION SET MULTI_STATEMENT_COUNT = 0"
                                  "CREATE OR REPLACE DATABASE test_db"
                                  "USE DATABASE test_db"
                                  "CREATE OR REPLACE SCHEMA test_schema"
                                  "USE SCHEMA test_schema" ] do
                                match (run connection sql).Error with
                                | Some message -> failwithf "resetContext failed on '%s': %s" sql message
                                | None -> ()
                            let steps =
                                match property test "steps" with
                                | Some steps -> [ for step in steps.EnumerateArray() -> step ]
                                | None -> []
                            let mutable failed = None
                            let mutable index = 0
                            while failed.IsNone && index < steps.Length do
                                let step = steps.[index]
                                let sql = property step "sql" |> Option.map text |> Option.defaultValue ""
                                match check (property step "expect") (run connection sql) with
                                | Some detail -> failed <- Some(index + 1, detail + "  [sql: " + sql + "]")
                                | None -> ()
                                index <- index + 1
                            match failed with
                            | Some(step, detail) -> record suite name "FAIL" step detail clock.ElapsedMilliseconds
                            | None -> record suite name "PASS" -1 "" clock.ElapsedMilliseconds
                        with error ->
                            record suite name "ERROR" -1 error.Message clock.ElapsedMilliseconds
        connection.Close()
        let reportPath =
            match Environment.GetEnvironmentVariable "FROSTLAKE_TESTKIT_REPORT" with
            | path when not (String.IsNullOrWhiteSpace path) -> path
            | _ -> Path.Combine(Path.GetTempPath(), "frostlake-fsharp-testkit.tsv")
        File.WriteAllText(reportPath, report.ToString())
        let summary =
            String.Join(", ", [ for KeyValue(status, n) in counts |> Seq.sortBy (fun p -> p.Key) -> sprintf "%s=%d" status n ])
        output.WriteLine(sprintf "%d suites in %.1fs: %s; report %s" files.Length started.Elapsed.TotalSeconds summary reportPath)
        if failures.Count > 0 then
            let shown = failures |> Seq.truncate 25 |> String.concat "\n"
            failwithf "%d case(s) failed or errored (%s); report %s\n%s" failures.Count summary reportPath shown
