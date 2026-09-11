/// The ADO.NET surface over canned answers: how the reader types and walks results, and how
/// parameters bind.
module Frostlake.FSharp.Tests.AdoTests

open System
open System.Data
open System.Data.Common
open System.Numerics
open Frostlake.FSharp
open Xunit

let private result (body: string) =
    match Wire.decode body with
    | Some decoded -> QueryResult(decoded.ResultSets, "s", TimeSpan.Zero)
    | None -> failwith "not an answer"

let private reader (body: string) = new FrostlakeDataReader(result body, null)

let private oneColumn (dataType: string) (scale: int) (cells: string) =
    reader (
        sprintf
            """{"success":true,"sessionId":"s","resultSets":[{"columns":[{"name":"C","dataType":"%s","precision":38,"scale":%d,"nullable":true}],"rows":[%s]}]}"""
            dataType
            scale
            cells
    )

[<Fact>]
let ``an integral column of ordinary numbers reads as int64`` () =
    use r = oneColumn "NUMBER" 0 "[1],[2],[null]"
    Assert.Equal(typeof<int64>, r.GetFieldType 0)
    Assert.True(r.Read())
    Assert.Equal(box 1L, r.GetValue 0)
    Assert.Equal(1, r.GetInt32 0)
    Assert.True(r.Read())
    Assert.True(r.Read())
    Assert.True(r.IsDBNull 0)
    Assert.Equal(box DBNull.Value, r.GetValue 0)
    Assert.False(r.Read())

[<Fact>]
let ``an integral column widens only as far as its values need`` () =
    use wide = oneColumn "NUMBER" 0 "[1],[12345678901234567890]"
    Assert.Equal(typeof<decimal>, wide.GetFieldType 0)
    Assert.True(wide.Read())
    Assert.Equal(box 1M, wide.GetValue 0)
    use wider = oneColumn "NUMBER" 0 "[1],[12345678901234567890123456789012345678]"
    Assert.Equal(typeof<string>, wider.GetFieldType 0)
    wider.Read() |> ignore
    wider.Read() |> ignore
    Assert.Equal(box "12345678901234567890123456789012345678", wider.GetValue 0)
    Assert.Equal(BigInteger.Parse "12345678901234567890123456789012345678", wider.GetFieldValue<BigInteger> 0)

[<Fact>]
let ``each type family has its CLR type`` () =
    let typed (dataType: string) (scale: int) (cell: string) =
        use r = oneColumn dataType scale ("[" + cell + "]")
        r.Read() |> ignore
        r.GetFieldType 0, r.GetValue 0
    Assert.Equal((typeof<decimal>, box 1.50M), typed "NUMBER" 2 "1.50")
    Assert.Equal((typeof<float>, box 0.5), typed "FLOAT" 0 "0.5")
    Assert.Equal((typeof<bool>, box true), typed "BOOLEAN" 0 "true")
    Assert.Equal((typeof<DateTime>, box (DateTime(2024, 1, 2))), typed "DATE" 0 "\"2024-01-02\"")
    Assert.Equal((typeof<TimeSpan>, box (TimeSpan(1, 2, 3))), typed "TIME" 0 "\"01:02:03\"")
    Assert.Equal((typeof<DateTime>, box (DateTime(2024, 1, 2, 3, 4, 5))), typed "TIMESTAMP_NTZ" 0 "\"2024-01-02 03:04:05.000\"")
    let zonedType, zoned = typed "TIMESTAMP_TZ" 0 "\"2024-01-02 03:04:05.000 +0200\""
    Assert.Equal(typeof<DateTimeOffset>, zonedType)
    Assert.Equal(TimeSpan.FromHours 2.0, (unbox<DateTimeOffset> zoned).Offset)
    Assert.Equal((typeof<byte[]>, box [| 0xABuy |]), typed "BINARY" 0 "\"AB\"")
    Assert.Equal((typeof<string>, box "{\"a\":1}"), typed "VARIANT" 0 "\"{\\\"a\\\":1}\"")

[<Fact>]
let ``GetFieldValue answers the modern types and SqlValue`` () =
    use r =
        reader
            """{"success":true,"sessionId":"s","resultSets":[{"columns":[{"name":"D","dataType":"DATE"},{"name":"T","dataType":"TIME"},{"name":"N","dataType":"NUMBER","scale":0},{"name":"S","dataType":"VARCHAR"}],"rows":[["2024-01-02","01:02:03",null,null]]}]}"""
    r.Read() |> ignore
    Assert.Equal(DateOnly(2024, 1, 2), r.GetFieldValue<DateOnly> 0)
    Assert.Equal(TimeOnly(1, 2, 3), r.GetFieldValue<TimeOnly> 1)
    Assert.Equal(SqlValue.Date(DateOnly(2024, 1, 2)), r.GetFieldValue<SqlValue> 0)
    Assert.Equal(SqlValue.Null, r.GetFieldValue<SqlValue> 2)
    Assert.Equal(Nullable<int64>(), r.GetFieldValue<Nullable<int64>> 2)
    Assert.Null(r.GetFieldValue<string> 3)
    Assert.Throws<InvalidCastException>(fun () -> r.GetFieldValue<int64> 2 |> ignore) |> ignore

[<Fact>]
let ``NextResult walks one result set per statement`` () =
    use r =
        reader
            """{"success":true,"sessionId":"s","resultSets":[
                {"columns":[{"name":"number of rows inserted","dataType":"NUMBER","scale":0}],"rows":[[2]],"updateCount":2},
                {"columns":[{"name":"N","dataType":"NUMBER","scale":0}],"rows":[[5]],"updateCount":-1}]}"""
    Assert.Equal(2, r.RecordsAffected)
    Assert.Equal("number of rows inserted", r.GetName 0)
    Assert.True(r.NextResult())
    Assert.Equal("N", r.GetName 0)
    Assert.True(r.HasRows)
    Assert.True(r.Read())
    Assert.Equal(5L, r.GetInt64 0)
    Assert.False(r.NextResult())
    Assert.Equal(0, r.FieldCount)

[<Fact>]
let ``a request without DML reports -1 records affected`` () =
    use r = oneColumn "NUMBER" 0 "[1]"
    Assert.Equal(-1, r.RecordsAffected)

[<Fact>]
let ``columns are found by name ignoring case`` () =
    use r =
        reader
            """{"success":true,"sessionId":"s","resultSets":[{"columns":[{"name":"ID","dataType":"NUMBER","scale":0},{"name":"lower","dataType":"VARCHAR"}],"rows":[[1,"x"]]}]}"""
    Assert.Equal(0, r.GetOrdinal "id")
    Assert.Equal(1, r.GetOrdinal "LOWER")
    Assert.Throws<IndexOutOfRangeException>(fun () -> r.GetOrdinal "nope" |> ignore) |> ignore
    r.Read() |> ignore
    Assert.Equal(box "x", r.["lower"])

[<Fact>]
let ``the schema table describes the columns`` () =
    use r =
        reader
            """{"success":true,"sessionId":"s","resultSets":[{"columns":[{"name":"PRICE","dataType":"NUMBER","precision":10,"scale":2,"nullable":false}],"rows":[[1.25]]}]}"""
    let schema = r.GetSchemaTable()
    let row = schema.Rows.[0]
    Assert.Equal(box "PRICE", row.[SchemaTableColumn.ColumnName])
    Assert.Equal(box typeof<decimal>, row.[SchemaTableColumn.DataType])
    Assert.Equal(box 10s, row.[SchemaTableColumn.NumericPrecision])
    Assert.Equal(box 2s, row.[SchemaTableColumn.NumericScale])
    Assert.Equal(box false, row.[SchemaTableColumn.AllowDBNull])

[<Fact>]
let ``GetBytes copies in chunks`` () =
    use r = oneColumn "BINARY" 0 "[\"00010203\"]"
    r.Read() |> ignore
    Assert.Equal(4L, r.GetBytes(0, 0L, null, 0, 0))
    let buffer = Array.zeroCreate<byte> 3
    Assert.Equal(3L, r.GetBytes(0, 1L, buffer, 0, 3))
    Assert.Equal<byte[]>([| 1uy; 2uy; 3uy |], buffer)
    Assert.Equal(0L, r.GetBytes(0, 9L, buffer, 0, 3))

[<Fact>]
let ``parameters split into positional and named`` () =
    let parameters = FrostlakeParameterCollection()
    parameters.AddWithValue("", 1) |> ignore
    parameters.AddWithValue("@name", "Ada") |> ignore
    parameters.AddWithValue(":when", DateTime(2024, 1, 2)) |> ignore
    parameters.Add(FrostlakeParameter("day", DateTime(2024, 1, 2, 5, 0, 0), DbType = DbType.Date)) |> ignore
    let positional, named = parameters.Binds()
    Assert.Equal<SqlValue[]>([| SqlValue.Int 1L |], positional)
    Assert.Equal(3, named.Length)
    Assert.Equal(SqlValue.Date(DateOnly(2024, 1, 2)), snd named.[2])
    Assert.Equal(1, parameters.IndexOf "name")
    Assert.Equal(1, parameters.IndexOf ":NAME")
    Assert.True(parameters.Contains "when")
    Assert.Throws<IndexOutOfRangeException>(fun () -> parameters.["nope"] |> ignore) |> ignore

[<Fact>]
let ``only input parameters exist`` () =
    let parameter = FrostlakeParameter()
    parameter.Direction <- ParameterDirection.Input
    Assert.Throws<NotSupportedException>(fun () -> parameter.Direction <- ParameterDirection.Output) |> ignore

[<Fact>]
let ``the provider factory registers with DbProviderFactories`` () =
    DbProviderFactories.RegisterFactory("Frostlake.FSharp.Tests", FrostlakeProviderFactory.Instance)
    let factory = DbProviderFactories.GetFactory "Frostlake.FSharp.Tests"
    Assert.Same(FrostlakeProviderFactory.Instance, factory)
    Assert.IsType<FrostlakeConnection>(factory.CreateConnection()) |> ignore
    Assert.IsType<FrostlakeCommand>(factory.CreateCommand()) |> ignore

[<Fact>]
let ``a command needs an open connection`` () =
    use command = new FrostlakeCommand("SELECT 1")
    Assert.Throws<InvalidOperationException>(fun () -> command.ExecuteScalar() |> ignore) |> ignore
    use connection = new FrostlakeConnection("frostlake://localhost:1/DB")
    Assert.Equal(ConnectionState.Closed, connection.State)
    Assert.Equal("DB", connection.Database)
    Assert.Equal("http://localhost:1", connection.DataSource)
    command.Connection <- connection
    Assert.Throws<InvalidOperationException>(fun () -> command.ExecuteNonQuery() |> ignore) |> ignore

[<Fact>]
let ``a data table loads a column of numbers too wide for decimal`` () =
    use r = oneColumn "NUMBER" 0 "[1],[-99999999999999999999999999999999999999]"
    let table = new DataTable()
    table.Load r
    Assert.Equal(2, table.Rows.Count)
    Assert.Equal(typeof<string>, table.Columns.[0].DataType)
    Assert.Equal(box "-99999999999999999999999999999999999999", table.Rows.[1].[0])

[<Fact>]
let ``GetString answers the engine's text and refuses NULL`` () =
    use r =
        reader
            """{"success":true,"sessionId":"s","resultSets":[{"columns":[{"name":"B","dataType":"BINARY"},{"name":"D","dataType":"DATE"},{"name":"N","dataType":"VARCHAR"}],"rows":[["0AFF","2024-01-02",null]]}]}"""
    r.Read() |> ignore
    Assert.Equal("0AFF", r.GetString 0)
    Assert.Equal("2024-01-02", r.GetString 1)
    Assert.Throws<InvalidCastException>(fun () -> r.GetString 2 |> ignore) |> ignore
    Assert.Equal(box DBNull.Value, r.GetFieldValue<obj> 2)

[<Fact>]
let ``a command timeout past the longest timer means no limit`` () =
    Assert.True((AdoTypes.commandLimit (Some Int32.MaxValue) (Some(TimeSpan.FromMinutes 5.0))).IsNone)
    Assert.True((AdoTypes.commandLimit (Some 0) (Some(TimeSpan.FromMinutes 5.0))).IsNone)
    Assert.Equal(Some(TimeSpan.FromSeconds 30.0), AdoTypes.commandLimit (Some 30) None)
    Assert.Equal(Some(TimeSpan.FromMinutes 5.0), AdoTypes.commandLimit None (Some(TimeSpan.FromMinutes 5.0)))
