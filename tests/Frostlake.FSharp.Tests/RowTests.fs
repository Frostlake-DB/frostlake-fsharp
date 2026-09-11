module Frostlake.FSharp.Tests.RowTests

open System
open System.Numerics
open Frostlake.FSharp
open Frostlake.FSharp.Tests.TestSupport
open Xunit

let private result (body: string) : QueryResult =
    match Wire.decode body with
    | Some decoded -> QueryResult(decoded.ResultSets, "s", TimeSpan.Zero)
    | None -> failwith "not an answer"

let private sample () =
    result
        """{"success":true,"sessionId":"s","resultSets":[{"columns":[
            {"name":"ID","dataType":"NUMBER","precision":38,"scale":0},
            {"name":"NAME","dataType":"VARCHAR","precision":0,"scale":0},
            {"name":"PRICE","dataType":"NUMBER","precision":10,"scale":2},
            {"name":"RATIO","dataType":"FLOAT","precision":0,"scale":0},
            {"name":"OK","dataType":"BOOLEAN","precision":0,"scale":0},
            {"name":"BORN","dataType":"DATE","precision":0,"scale":0},
            {"name":"AT","dataType":"TIME","precision":0,"scale":0},
            {"name":"SEEN","dataType":"TIMESTAMP_NTZ","precision":0,"scale":0},
            {"name":"SEEN_TZ","dataType":"TIMESTAMP_TZ","precision":0,"scale":0},
            {"name":"DATA","dataType":"BINARY","precision":0,"scale":0},
            {"name":"DOC","dataType":"VARIANT","precision":0,"scale":0},
            {"name":"NOTHING","dataType":"VARCHAR","precision":0,"scale":0},
            {"name":"WIDE","dataType":"NUMBER","precision":38,"scale":0},
            {"name":"lower","dataType":"VARCHAR","precision":0,"scale":0},
            {"name":"TEXTNUM","dataType":"VARCHAR","precision":0,"scale":0},
            {"name":"GUID","dataType":"VARCHAR","precision":0,"scale":0}
          ],"rows":[[7,"Ada",12.50,0.25,true,"1815-12-10","09:30:00","2024-01-02 03:04:05.678","2024-01-02 03:04:05.000 +0100","CAFE","{\"k\":[1,2]}",null,99999999999999999999,"quiet","42","7f5fd0d6-5a21-4b8a-9a7e-2f33b9b8ff10"]]}]}"""

[<Fact>]
let ``typed readers convert each column`` () =
    let row = (sample ()).Rows.[0]
    Assert.Equal(7, row.int "ID")
    Assert.Equal(7L, row.int64 "ID")
    Assert.Equal("Ada", row.string "NAME")
    Assert.Equal("Ada", row.text "NAME")
    Assert.Equal(12.50M, row.decimal "PRICE")
    Assert.Equal(0.25, row.double "RATIO")
    Assert.Equal(0.25, row.float "RATIO")
    Assert.True(row.bool "OK")
    Assert.Equal(DateOnly(1815, 12, 10), row.date "BORN")
    Assert.Equal(TimeOnly(9, 30), row.time "AT")
    Assert.Equal(DateTime(2024, 1, 2, 3, 4, 5, 678), row.timestamp "SEEN")
    Assert.Equal(TimeSpan.FromHours 1.0, (row.timestamptz "SEEN_TZ").Offset)
    Assert.Equal<byte[]>([| 0xCAuy; 0xFEuy |], row.bytes "DATA")
    Assert.Equal("{\"k\":[1,2]}", row.variant "DOC")
    Assert.Equal(BigInteger.Parse "99999999999999999999", row.bigint "WIDE")
    Assert.Equal(Guid.Parse "7f5fd0d6-5a21-4b8a-9a7e-2f33b9b8ff10", row.uuid "GUID")
    Assert.Equal(SqlValue.Text "Ada", row.value "NAME")
    Assert.Equal(SqlValue.Text "Ada", row.["NAME"])
    Assert.Equal(SqlValue.Int 7L, row.[0])

[<Fact>]
let ``OrNone readers answer None for NULL where typed readers refuse it`` () =
    let row = (sample ()).Rows.[0]
    Assert.True((row.stringOrNone "NOTHING").IsNone)
    Assert.Equal(Some "Ada", row.stringOrNone "NAME")
    Assert.Equal(Some 7, row.intOrNone "ID")
    let error = raisesKind ErrorKind.TypeMismatch (fun () -> row.string "NOTHING")
    Assert.Contains("stringOrNone", error.Message)
    Assert.True(row.isNull "NOTHING")
    Assert.False(row.isNull "NAME")

[<Fact>]
let ``conversions that would lose information are refused`` () =
    let row = (sample ()).Rows.[0]
    raisesKind ErrorKind.TypeMismatch (fun () -> row.int64 "WIDE") |> ignore
    raisesKind ErrorKind.TypeMismatch (fun () -> row.int "PRICE") |> ignore
    raisesKind ErrorKind.TypeMismatch (fun () -> row.date "NAME") |> ignore
    raisesKind ErrorKind.TypeMismatch (fun () -> row.timestamptz "SEEN") |> ignore
    raisesKind ErrorKind.TypeMismatch (fun () -> row.timestamp "SEEN_TZ") |> ignore
    raisesKind ErrorKind.TypeMismatch (fun () -> row.bytes "NAME") |> ignore
    raisesKind ErrorKind.TypeMismatch (fun () -> row.bool "NAME") |> ignore

[<Fact>]
let ``exact conversions are allowed`` () =
    let row = (sample ()).Rows.[0]
    Assert.Equal(42L, row.int64 "TEXTNUM")
    Assert.Equal(7.0, row.double "ID")
    Assert.Equal(7M, row.decimal "ID")
    Assert.Equal(99999999999999999999M, row.decimal "WIDE")
    Assert.Equal("12.50", row.string "PRICE")
    Assert.Equal("true", row.string "OK")
    Assert.Equal("1815-12-10", row.string "BORN")
    Assert.Equal(DateOnly(2024, 1, 2), row.date "SEEN")
    Assert.Equal("CAFE", row.string "DATA")

[<Fact>]
let ``names match exactly first, then ignoring case`` () =
    let row = (sample ()).Rows.[0]
    Assert.Equal("quiet", row.string "lower")
    Assert.Equal("quiet", row.string "LOWER")
    Assert.Equal("Ada", row.string "name")
    let error = raisesKind ErrorKind.Usage (fun () -> row.string "MISSING")
    Assert.Contains("NAME", error.Message)
    Assert.True((row.TryGet "MISSING").IsNone)
    raisesKind ErrorKind.Usage (fun () -> row.[99]) |> ignore

[<Fact>]
let ``duplicate names resolve to the first column`` () =
    let r =
        result
            """{"success":true,"sessionId":"s","resultSets":[{"columns":[{"name":"ID","dataType":"NUMBER","scale":0},{"name":"ID","dataType":"NUMBER","scale":0}],"rows":[[1,2]]}]}"""
    Assert.Equal(1, r.Rows.[0].int "ID")
    Assert.Equal(SqlValue.Int 2L, r.Rows.[0].[1])

[<Fact>]
let ``a request's rows are those of its final result set`` () =
    let r =
        result
            """{"success":true,"sessionId":"s","resultSets":[
                {"columns":[{"name":"number of rows inserted","dataType":"NUMBER","scale":0}],"rows":[[2]],"updateCount":2},
                {"columns":[{"name":"number of rows deleted","dataType":"NUMBER","scale":0}],"rows":[[1]],"updateCount":1},
                {"columns":[{"name":"N","dataType":"NUMBER","scale":0}],"rows":[[5],[6]],"updateCount":-1}]}"""
    Assert.Equal(3, r.ResultSets.Length)
    Assert.Equal(2, r.Rows.Length)
    Assert.Equal("N", r.Columns.[0].Name)
    Assert.Equal(3L, r.RowsAffected)
    Assert.Equal(SqlValue.Int 5L, r.Scalar)

[<Fact>]
let ``a scalar needs a row`` () =
    let r = result """{"success":true,"sessionId":"s","resultSets":[]}"""
    Assert.True(r.TryScalar.IsNone)
    raisesKind ErrorKind.Usage (fun () -> r.Scalar) |> ignore
    Assert.Empty(r.Rows)
    Assert.Equal(0L, r.RowsAffected)

[<Fact>]
let ``an exponent past a thousand places is refused rather than computed`` () =
    Assert.True((Cells.parseExact "1e10000000").IsNone)
    Assert.True((Cells.parseExact "1e-10000000").IsNone)
    Assert.True((Cells.parseExact "1.7976931348623157E308").IsSome)
    let r =
        result """{"success":true,"sessionId":"s","resultSets":[{"columns":[{"name":"S","dataType":"VARCHAR"}],"rows":[["1e10000000"]]}]}"""
    raisesKind ErrorKind.TypeMismatch (fun () -> r.Rows.[0].int64 "S") |> ignore

[<Fact>]
let ``a zoned timestamp outside dotnet's range is refused as a DateTimeOffset`` () =
    let r =
        result """{"success":true,"sessionId":"s","resultSets":[{"columns":[{"name":"T","dataType":"VARCHAR"}],"rows":[["0001-01-01 00:00:00.000 +0100"]]}]}"""
    raisesKind ErrorKind.TypeMismatch (fun () -> r.Rows.[0].timestamptz "T") |> ignore
