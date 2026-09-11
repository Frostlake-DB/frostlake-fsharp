module Frostlake.FSharp.Tests.WireTests

open System
open System.Globalization
open System.Numerics
open System.Text
open System.Text.Json
open Frostlake.FSharp
open Xunit

let private decode (body: string) =
    match Wire.decode body with
    | Some decoded -> decoded
    | None -> failwith "expected a Frostlake answer"

/// One column, one row, one cell.
let private cell (dataType: string) (precision: int) (scale: int) (json: string) : SqlValue =
    let body =
        sprintf
            """{"success":true,"sessionId":"s","resultSets":[{"columns":[{"name":"C","dataType":"%s","precision":%d,"scale":%d,"nullable":true}],"rows":[[%s]],"rowCount":1}]}"""
            dataType
            precision
            scale
            json
    (decode body).ResultSets.[0].Rows.[0].Values.[0]

[<Fact>]
let ``integral numbers read as int64 and wider ones as BigInteger`` () =
    Assert.Equal(SqlValue.Int 42L, cell "NUMBER" 38 0 "42")
    Assert.Equal(SqlValue.Int(-42L), cell "NUMBER" 38 0 "-42")
    Assert.Equal(
        SqlValue.BigInt(BigInteger.Parse "12345678901234567890123456789012345678"),
        cell "NUMBER" 38 0 "12345678901234567890123456789012345678"
    )
    Assert.Equal(SqlValue.Int 1000L, cell "NUMBER" 38 0 "1E+3")

[<Fact>]
let ``scaled numbers stay exact`` () =
    match cell "NUMBER" 10 2 "1.50" with
    | SqlValue.Decimal value -> Assert.Equal("1.50", value.ToString(CultureInfo.InvariantCulture))
    | other -> failwithf "expected a decimal, got %A" other
    Assert.Equal(
        SqlValue.DecimalText "12345678901234567890.123456789012345678",
        cell "NUMBER" 38 18 "12345678901234567890.123456789012345678"
    )
    Assert.Equal(SqlValue.Decimal 0.333333M, cell "NUMBER" 7 6 "0.333333")

[<Fact>]
let ``floats read their special spellings`` () =
    match cell "FLOAT" 0 0 "\"NaN\"" with
    | SqlValue.Float value -> Assert.True(Double.IsNaN value)
    | other -> failwithf "expected NaN, got %A" other
    Assert.Equal(SqlValue.Float Double.PositiveInfinity, cell "FLOAT" 0 0 "\"Infinity\"")
    Assert.Equal(SqlValue.Float Double.NegativeInfinity, cell "FLOAT" 0 0 "\"-Infinity\"")
    Assert.Equal(SqlValue.Float 1e300, cell "FLOAT" 0 0 "1.0E300")
    match cell "DOUBLE" 0 0 "-0.0" with
    | SqlValue.Float value -> Assert.True(Double.IsNegative value && value = 0.0)
    | other -> failwithf "expected negative zero, got %A" other

[<Fact>]
let ``temporal text reads as dotnet temporal values`` () =
    Assert.Equal(SqlValue.Date(DateOnly(2024, 1, 2)), cell "DATE" 0 0 "\"2024-01-02\"")
    Assert.Equal(
        SqlValue.Time(TimeOnly(12, 34, 56).Add(TimeSpan.FromTicks 1234567L)),
        cell "TIME" 0 0 "\"12:34:56.123456789\""
    )
    Assert.Equal(SqlValue.Time(TimeOnly(12, 0)), cell "TIME" 0 0 "\"12:00:00\"")
    Assert.Equal(
        SqlValue.Timestamp(DateTime(2024, 1, 2, 3, 4, 5).AddTicks 1234567L),
        cell "TIMESTAMP_NTZ" 0 0 "\"2024-01-02 03:04:05.123456789\""
    )
    match cell "TIMESTAMP_TZ" 0 0 "\"2024-01-02 03:04:05.123 +0200\"" with
    | SqlValue.TimestampTz value ->
        Assert.Equal(TimeSpan.FromHours 2.0, value.Offset)
        Assert.Equal(DateTime(2024, 1, 2, 3, 4, 5, 123), value.DateTime)
    | other -> failwithf "expected a zoned timestamp, got %A" other
    match cell "TIMESTAMP_LTZ" 0 0 "\"2024-07-01 12:00:00.000 -0530\"" with
    | SqlValue.TimestampTz value -> Assert.Equal(TimeSpan.FromHours(-5.5), value.Offset)
    | other -> failwithf "expected a zoned timestamp, got %A" other

[<Fact>]
let ``text that does not parse as its column's type is kept as text`` () =
    Assert.Equal(SqlValue.Text "not a date", cell "DATE" 0 0 "\"not a date\"")
    Assert.Equal(SqlValue.Text "zz", cell "BINARY" 0 0 "\"zz\"")
    Assert.Equal(SqlValue.Text "-0001-01-01", cell "DATE" 0 0 "\"-0001-01-01\"")
    Assert.Equal(SqlValue.Text "24:00:00", cell "TIME" 0 0 "\"24:00:00\"")

[<Fact>]
let ``binary crosses as hex`` () =
    Assert.Equal(SqlValue.Binary [| 0x0Auy; 0xFFuy |], cell "BINARY" 0 0 "\"0AFF\"")

[<Fact>]
let ``semi-structured values keep the engine's text`` () =
    Assert.Equal(SqlValue.Variant "{\"a\":1}", cell "VARIANT" 0 0 "\"{\\\"a\\\":1}\"")
    Assert.Equal(SqlValue.Variant "s", cell "VARIANT" 0 0 "\"s\"")
    Assert.Equal(SqlValue.Variant "1", cell "VARIANT" 0 0 "1")
    Assert.Equal(SqlValue.Variant "[1,2,3]", cell "VECTOR(INT, 3)" 0 0 "\"[1,2,3]\"")

[<Fact>]
let ``a container that crossed as JSON is a variant whatever the column says`` () =
    Assert.Equal(SqlValue.Variant "[1,2]", cell "VARCHAR" 0 0 "[1,2]")

[<Fact>]
let ``a number in a text column reads as its text`` () =
    Assert.Equal(SqlValue.Text "42", cell "VARCHAR" 0 0 "42")

[<Fact>]
let ``booleans read from JSON and from text`` () =
    Assert.Equal(SqlValue.Bool true, cell "BOOLEAN" 0 0 "true")
    Assert.Equal(SqlValue.Bool false, cell "BOOLEAN" 0 0 "\"FALSE\"")

[<Fact>]
let ``null is null in every column`` () =
    for dataType in [ "NUMBER"; "VARCHAR"; "DATE"; "VARIANT"; "BINARY"; "FLOAT" ] do
        Assert.Equal(SqlValue.Null, cell dataType 0 0 "null")

[<Fact>]
let ``an answer from 0.1.0 carries newSession, update counts and nullability`` () =
    let decoded =
        decode
            """{"errorMessage":null,"executionTimeMs":4,"newSession":false,"resultSets":[{"columns":[{"dataType":"NUMBER","name":"number of rows inserted","nullable":false,"precision":38,"scale":0}],"rowCount":1,"rows":[[3]],"updateCount":3},{"columns":[{"dataType":"VARCHAR","name":"status","nullable":false,"precision":0,"scale":0}],"rowCount":1,"rows":[["Table T successfully created."]],"updateCount":-1}],"sessionId":"abc","success":true}"""
    Assert.True(decoded.Success)
    Assert.Equal(Some "abc", decoded.SessionId)
    Assert.Equal(Some false, decoded.NewSession)
    Assert.Equal(4L, decoded.ExecutionTimeMs)
    Assert.Equal(Some 3L, decoded.ResultSets.[0].UpdateCount)
    Assert.True(decoded.ResultSets.[1].UpdateCount.IsNone)
    Assert.Equal(Some false, decoded.ResultSets.[0].Columns.[0].Nullable)

[<Fact>]
let ``an answer from 0.0.7 has its update count derived from the grid`` () =
    let update =
        decode
            """{"errorMessage":null,"executionTimeMs":7,"resultSets":[{"columns":[{"dataType":"NUMBER","name":"number of rows updated","nullable":false,"precision":38,"scale":0},{"dataType":"NUMBER","name":"number of multi-joined rows updated","nullable":false,"precision":38,"scale":0}],"rowCount":1,"rows":[[2,0]]}],"sessionId":"s","success":true}"""
    Assert.True(update.NewSession.IsNone)
    Assert.Equal(Some 2L, update.ResultSets.[0].UpdateCount)
    let merge =
        decode
            """{"success":true,"sessionId":"s","resultSets":[{"columns":[{"dataType":"NUMBER","name":"number of rows inserted","precision":38,"scale":0},{"dataType":"NUMBER","name":"number of rows updated","precision":38,"scale":0}],"rows":[[1,1]]}]}"""
    Assert.Equal(Some 2L, merge.ResultSets.[0].UpdateCount)
    Assert.True(merge.ResultSets.[0].Columns.[0].Nullable.IsNone)

[<Fact>]
let ``a query is never taken for DML when the server marks its results`` () =
    let decoded =
        decode
            """{"success":true,"sessionId":"s","newSession":false,"resultSets":[{"columns":[{"dataType":"NUMBER","name":"number of rows inserted","precision":38,"scale":0}],"rows":[[5]],"updateCount":-1}]}"""
    Assert.True(decoded.ResultSets.[0].UpdateCount.IsNone)

[<Fact>]
let ``the endpoint's own refusal reads as a failure with its message`` () =
    let decoded = decode """{"error":"SQL is required"}"""
    Assert.False(decoded.Success)
    Assert.Equal(Some "SQL is required", decoded.ErrorMessage)

[<Theory>]
[<InlineData("<html>502 Bad Gateway</html>")>]
[<InlineData("{\"status\":\"ok\"}")>]
[<InlineData("[1,2]")>]
[<InlineData("")>]
let ``a body that is not a Frostlake answer decodes to nothing`` (body: string) =
    Assert.True((Wire.decode body).IsNone)

[<Fact>]
let ``a short row is padded with NULL and a long one keeps its extra cells`` () =
    let decoded =
        decode
            """{"success":true,"sessionId":"s","resultSets":[{"columns":[{"name":"A","dataType":"NUMBER","scale":0},{"name":"B","dataType":"NUMBER","scale":0}],"rows":[[1],[1,2,"x"]]}]}"""
    let rows = decoded.ResultSets.[0].Rows
    Assert.Equal(SqlValue.Null, rows.[0].Values.[1])
    Assert.Equal(SqlValue.Text "x", rows.[1].Values.[2])

[<Fact>]
let ``a bare undefined is recognised outside strings only`` () =
    Assert.True(Wire.carriesBareUndefined """{"rows":[[[1,undefined,2]]]}""")
    Assert.False(Wire.carriesBareUndefined """{"rows":[["undefined"]]}""")
    Assert.False(Wire.carriesBareUndefined """{"rows":[["say \"undefined\""]]}""")

[<Fact>]
let ``the health answer is recognised`` () =
    Assert.True(Wire.looksLikeHealth """{"status":"healthy","activeSessions":0}""")
    Assert.False(Wire.looksLikeHealth "OK")
    Assert.False(Wire.looksLikeHealth """{"healthy":true}""")

[<Fact>]
let ``requests carry requireSession only with a session id, and a count only for a pack`` () =
    let read (bytes: byte[]) =
        JsonDocument.Parse(Encoding.UTF8.GetString bytes).RootElement
    let first = read (Wire.encodeExecute "SELECT 'é\"'" None true None)
    Assert.Equal("SELECT 'é\"'", first.GetProperty("sql").GetString())
    Assert.False(first.TryGetProperty("sessionId") |> fst)
    Assert.False(first.TryGetProperty("requireSession") |> fst)
    Assert.True(first.GetProperty("autoCommit").GetBoolean())
    // Without a count the session answers for the request, so a caller who asked its session for a
    // pack keeps it.
    Assert.False(first.TryGetProperty("multiStatementCount") |> fst)
    let later = read (Wire.encodeExecute "SELECT 1" (Some "abc") false None)
    Assert.Equal("abc", later.GetProperty("sessionId").GetString())
    Assert.True(later.GetProperty("requireSession").GetBoolean())
    Assert.False(later.GetProperty("autoCommit").GetBoolean())
    let packed = read (Wire.encodeExecute "USE ROLE R; USE DATABASE D" (Some "abc") true (Some 2))
    Assert.Equal(2, packed.GetProperty("multiStatementCount").GetInt32())

[<Fact>]
let ``a zoned timestamp whose instant dotnet cannot hold stays text`` () =
    Assert.Equal(SqlValue.Text "0001-01-01 00:00:00.000 +0100", cell "TIMESTAMP_TZ" 0 0 "\"0001-01-01 00:00:00.000 +0100\"")
    Assert.Equal(SqlValue.Text "9999-12-31 23:59:59.000 -0100", cell "TIMESTAMP_LTZ" 0 0 "\"9999-12-31 23:59:59.000 -0100\"")
    match cell "TIMESTAMP_TZ" 0 0 "\"0001-01-01 02:00:00.000 +0100\"" with
    | SqlValue.TimestampTz value -> Assert.Equal(TimeSpan.FromHours 1.0, value.Offset)
    | other -> failwithf "expected a zoned timestamp, got %A" other

[<Fact>]
let ``a number with an absurd exponent is kept as text, not computed`` () =
    Assert.Equal(SqlValue.Text "1e999999999", cell "NUMBER" 38 0 "1e999999999")

[<Theory>]
[<InlineData("{\"success\":true,\"resultSets\":[1]}")>]
[<InlineData("{\"success\":true,\"resultSets\":[{\"columns\":[1],\"rows\":[]}]}")>]
let ``an answer of the wrong shape is not a Frostlake answer`` (body: string) =
    Assert.True((Wire.decode body).IsNone)
