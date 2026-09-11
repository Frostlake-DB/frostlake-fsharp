/// The Connection API against a real engine.
module Frostlake.FSharp.Tests.EngineTests

open System
open System.Numerics
open System.Threading
open Frostlake.FSharp
open Frostlake.FSharp.Tests.TestSupport
open Xunit

/// Engines from 0.1.0 keep a TIMESTAMP_TZ's offset and a TIME's fraction on the wire; 0.0.7 does not.
let private modern (connection: Connection) =
    not (connection.ServerVersion.StartsWith("0.0.", StringComparison.Ordinal))

[<EngineFact>]
let ``values round-trip through a table`` () =
    let _, dsn = freshDatabase "ROUNDTRIP"
    use connection = Connection.Open dsn
    connection.Execute
        "CREATE TABLE T (I INTEGER, W NUMBER(38,0), D NUMBER(12,2), F FLOAT, B BOOLEAN, S VARCHAR, X BINARY, DT DATE, TM TIME, TS TIMESTAMP_NTZ, TZ TIMESTAMP_TZ, V VARIANT)"
    |> ignore
    // PARSE_JSON is not allowed in a VALUES clause, so the row goes in through a SELECT.
    let inserted =
        connection.Execute(
            "INSERT INTO T SELECT ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?",
            SqlValue.Int 42L,
            SqlValue.BigInt(BigInteger.Parse "12345678901234567890123456789012345678"),
            SqlValue.Decimal 1234.50M,
            SqlValue.Float 0.1,
            SqlValue.Bool true,
            SqlValue.Text "it's a \\ ☃",
            SqlValue.Binary [| 0uy; 255uy |],
            SqlValue.Date(DateOnly(2024, 2, 29)),
            SqlValue.Time(TimeOnly(23, 59, 59, 500)),
            SqlValue.Timestamp(DateTime(2024, 1, 2, 3, 4, 5, 678)),
            SqlValue.TimestampTz(DateTimeOffset(2024, 1, 2, 3, 4, 5, TimeSpan.FromHours 2.0)),
            SqlValue.Variant """{"k":[1,"two",null]}"""
        )
    Assert.Equal(1L, inserted.RowsAffected)
    let row = (connection.Execute "SELECT * FROM T").Rows.[0]
    Assert.Equal(42L, row.int64 "I")
    Assert.Equal(BigInteger.Parse "12345678901234567890123456789012345678", row.bigint "W")
    Assert.Equal("1234.50", row.string "D")
    Assert.Equal(0.1, row.double "F")
    Assert.True(row.bool "B")
    Assert.Equal("it's a \\ ☃", row.string "S")
    Assert.Equal<byte[]>([| 0uy; 255uy |], row.bytes "X")
    Assert.Equal(DateOnly(2024, 2, 29), row.date "DT")
    Assert.Equal(DateTime(2024, 1, 2, 3, 4, 5, 678), row.timestamp "TS")
    Assert.Equal(DateTime(2024, 1, 2, 3, 4, 5), (row.timestamptz "TZ").DateTime)
    Assert.Contains("\"two\"", row.variant "V")
    if modern connection then
        Assert.Equal(TimeOnly(23, 59, 59, 500), row.time "TM")
        Assert.Equal(TimeSpan.FromHours 2.0, (row.timestamptz "TZ").Offset)
    else
        Assert.Equal(TimeOnly(23, 59, 59), row.time "TM")

[<EngineFact>]
let ``a negative bound after a minus stays a negative number`` () =
    use connection = Connection.Open(engineDsn ())
    for value in
        [ SqlValue.Int(-5L)
          SqlValue.Decimal(-5M)
          SqlValue.Float(-5.0)
          SqlValue.BigInt(BigInteger(-5))
          SqlValue.DecimalText "-5" ] do
        let result = connection.Execute("SELECT 3-? AS N", value)
        Assert.Equal(8.0, result.Rows.[0].double "N")

[<EngineFact>]
let ``positional, named and at-sign arguments bind`` () =
    use connection = Connection.Open(engineDsn ())
    Assert.Equal(SqlValue.Int 3L, (connection.Execute("SELECT ? + ? AS N", SqlValue.Int 1L, SqlValue.Int 2L)).Scalar)
    Assert.Equal(
        SqlValue.Int 7L,
        (connection.Execute("SELECT :a + @b AS N", [ "a", SqlValue.Int 3L; "b", SqlValue.Int 4L ])).Scalar
    )
    Assert.Equal(
        SqlValue.Text "O'Brien \\ ✓",
        (connection.Execute("SELECT ? AS S", SqlValue.Text "O'Brien \\ ✓")).Scalar
    )

[<EngineFact>]
let ``special floats bind in the engine's own spellings`` () =
    use connection = Connection.Open(engineDsn ())
    let result =
        connection.Execute(
            "SELECT ? AS A, ? AS B, ? AS C",
            SqlValue.Float nan,
            SqlValue.Float infinity,
            SqlValue.Float(-infinity)
        )
    let row = result.Rows.[0]
    Assert.True(Double.IsNaN(row.double "A"))
    Assert.Equal(infinity, row.double "B")
    Assert.Equal(-infinity, row.double "C")

[<EngineFact>]
let ``a bound float is a FLOAT and a bound decimal stays exact`` () =
    use connection = Connection.Open(engineDsn ())
    let result = connection.Execute("SELECT ? + ? AS F, ? + ? AS D", SqlValue.Float 0.1, SqlValue.Float 0.2, SqlValue.Decimal 0.1M, SqlValue.Decimal 0.2M)
    Assert.Equal(0.1 + 0.2, result.Rows.[0].double "F")
    Assert.Equal(0.3M, result.Rows.[0].decimal "D")

[<EngineFact>]
let ``transactions commit and roll back`` () =
    let _, dsn = freshDatabase "TXN"
    use connection = Connection.Open dsn
    connection.Execute "CREATE TABLE T (ID INTEGER)" |> ignore
    connection.BeginTransaction()
    connection.Execute("INSERT INTO T VALUES (?)", SqlValue.Int 1L) |> ignore
    connection.Rollback()
    Assert.Equal(SqlValue.Int 0L, (connection.Execute "SELECT COUNT(*) FROM T").Scalar)
    connection.Transaction(fun () -> connection.Execute("INSERT INTO T VALUES (?)", SqlValue.Int 2L) |> ignore)
    Assert.Throws<InvalidOperationException>(fun () ->
        connection.Transaction(fun () ->
            connection.Execute("INSERT INTO T VALUES (?)", SqlValue.Int 3L) |> ignore
            raise (InvalidOperationException "boom"))
        |> ignore)
    |> ignore
    Assert.False(connection.InTransaction)
    Assert.Equal(SqlValue.Int 2L, (connection.Execute "SELECT SUM(ID) FROM T").Scalar)

[<EngineFact>]
let ``a script answers its final result set and every count`` () =
    let _, dsn = freshDatabase "SCRIPT"
    use connection = Connection.Open dsn
    // A request carrying more than one statement has to be asked for. 0 means any number, so the
    // single statements further down keep working on the same session.
    connection.Execute "ALTER SESSION SET MULTI_STATEMENT_COUNT = 0" |> ignore
    let result =
        connection.Execute
            "CREATE TABLE T (ID INTEGER, V VARCHAR); INSERT INTO T VALUES (1,'a'),(2,'b'),(3,'c'); UPDATE T SET V = 'z' WHERE ID > 1; SELECT ID, V FROM T ORDER BY ID"
    Assert.Equal(3, result.Rows.Length)
    Assert.Equal("z", result.Rows.[2].string "V")
    Assert.Equal(5L, result.RowsAffected)
    let merged =
        connection.Execute
            "MERGE INTO T USING (SELECT 1 AS ID, 'q' AS V UNION ALL SELECT 9, 'n') S ON T.ID = S.ID WHEN MATCHED THEN UPDATE SET V = S.V WHEN NOT MATCHED THEN INSERT VALUES (S.ID, S.V)"
    Assert.Equal(2L, merged.RowsAffected)
    Assert.Equal(1L, (connection.Execute "DELETE FROM T WHERE ID = 9").RowsAffected)

[<EngineFact>]
let ``a refused statement leaves the connection usable`` () =
    use connection = Connection.Open(engineDsn ())
    let error = raisesKind ErrorKind.Refused (fun () -> connection.Execute "SELECT * FROM NO_SUCH_TABLE_ANYWHERE")
    Assert.Contains("does not exist", error.Message)
    Assert.Equal(SqlValue.Int 1L, (connection.Execute "SELECT 1").Scalar)
    // Engines from 0.1.0 fail a blank statement as the account does; older ones refuse it before it runs.
    let empty = raisesKind ErrorKind.Refused (fun () -> connection.Execute "   ")
    Assert.True(empty.Message.Contains "Empty SQL statement." || empty.Message.Contains "SQL is required", empty.Message)

[<EngineFact>]
let ``the DSN's scope is applied, folding plain names as SQL does`` () =
    let name, _ = freshDatabase "SCOPE"
    use connection = Connection.Open(sprintf "%s/%s/public" (engineDsn ()) (name.ToLowerInvariant()))
    let row = (connection.Execute "SELECT CURRENT_DATABASE() AS D, CURRENT_SCHEMA() AS S").Rows.[0]
    Assert.Equal(name, row.string "D")
    Assert.Equal("PUBLIC", row.string "S")

[<EngineFact>]
let ``a quoted name keeps its case`` () =
    use admin = Connection.Open(engineDsn ())
    admin.Execute "CREATE OR REPLACE DATABASE \"Mixed Case Db\"" |> ignore
    use connection = Connection.Open(engineDsn () + "/%22Mixed%20Case%20Db%22")
    let current = (connection.Execute "SELECT CURRENT_DATABASE() AS D").Rows.[0].string "D"
    // Engine 0.0.7 selects the right database but reports its name upper-cased.
    if modern connection then
        Assert.Equal("Mixed Case Db", current)
    else
        Assert.Equal("MIXED CASE DB", current)

[<EngineFact>]
let ``role and warehouse come from the DSN`` () =
    use connection = Connection.Open(engineDsn () + "?role=SYSADMIN&warehouse=COMPUTE_WH")
    let row = (connection.Execute "SELECT CURRENT_ROLE() AS R, CURRENT_WAREHOUSE() AS W").Rows.[0]
    Assert.Equal("SYSADMIN", row.string "R")
    Assert.Equal("COMPUTE_WH", row.string "W")

[<EngineFact>]
let ``a DSN naming a missing database fails to open`` () =
    let error = raisesKind ErrorKind.Refused (fun () -> Connection.Open(engineDsn () + "/NO_SUCH_DATABASE_ANYWHERE"))
    Assert.Contains("could not be applied", error.Message)

[<EngineFact>]
let ``session state carries from one statement to the next`` () =
    use connection = Connection.Open(engineDsn ())
    connection.Execute "SET MY_VAR = 5" |> ignore
    Assert.Equal(5L, (connection.Execute "SELECT $MY_VAR AS V").Rows.[0].int64 "V")

[<EngineFact>]
let ``scripting keeps its own markers when no arguments are given`` () =
    use connection = Connection.Open(engineDsn ())
    // A block answers one column, named `anonymous block` as live names it; engines before 0.1.0 named it RESULT.
    let blockValue (row: Row) =
        let name = row.Columns.[0].Name
        Assert.True((name = "anonymous block" || name = "RESULT"), name)
        row.int64 name
    let cursor =
        connection.Execute
            "EXECUTE IMMEDIATE $$ DECLARE c CURSOR FOR SELECT ? + 1 AS x; r INT; BEGIN OPEN c USING (41); FETCH c INTO r; CLOSE c; RETURN r; END; $$"
    Assert.Equal(42L, blockValue cursor.Rows.[0])
    let block = connection.Execute "BEGIN LET x := 1; RETURN :x + 1; END"
    Assert.Equal(2L, blockValue block.Rows.[0])

[<EngineFact>]
let ``closing releases the engine session`` () =
    if engineReleasesSessions () then
        let before = activeSessions ()
        let connection = Connection.Open(engineDsn () + "/SNOWFLAKE")
        Assert.Equal(before + 1, activeSessions ())
        connection.Close()
        Assert.Equal(before, activeSessions ())

[<EngineFact>]
let ``a released session is replaced on the DSN's scope`` () =
    if engineReleasesSessions () then
        let name, dsn = freshDatabase "RECOVER"
        use connection = Connection.Open dsn
        let first = connection.SessionId.Value
        rawRequest "DELETE" ("/api/sessions/" + first) null |> ignore
        let row = (connection.Execute "SELECT CURRENT_DATABASE() AS D").Rows.[0]
        Assert.Equal(name, row.string "D")
        Assert.NotEqual<string>(first, connection.SessionId.Value)

[<EngineFact>]
let ``a released session that had moved is reported`` () =
    if engineReleasesSessions () then
        let _, dsn = freshDatabase "LOST"
        use connection = Connection.Open dsn
        connection.Execute "CREATE SCHEMA OTHER" |> ignore
        rawRequest "DELETE" ("/api/sessions/" + connection.SessionId.Value) null |> ignore
        raisesKind ErrorKind.SessionLost (fun () -> connection.Execute "SELECT 1") |> ignore
        Assert.Equal("PUBLIC", (connection.Execute "SELECT CURRENT_SCHEMA() AS S").Rows.[0].string "S")

[<EngineFact>]
let ``reset puts the session back on its scope`` () =
    let _, dsn = freshDatabase "RESET"
    use connection = Connection.Open dsn
    connection.Execute "CREATE SCHEMA OTHER" |> ignore
    Assert.Equal("OTHER", (connection.Execute "SELECT CURRENT_SCHEMA() AS S").Rows.[0].string "S")
    connection.Reset()
    Assert.Equal("PUBLIC", (connection.Execute "SELECT CURRENT_SCHEMA() AS S").Rows.[0].string "S")

[<EngineFact>]
let ``the asynchronous API runs the same statements`` () =
    task {
        let _, dsn = freshDatabase "ASYNC"
        use! connection = Connection.OpenAsync dsn
        let! _ = connection.ExecuteAsync "CREATE TABLE T (ID INTEGER)"
        let! inserted = connection.ExecuteAsync("INSERT INTO T VALUES (?), (?)", [ SqlValue.Int 1L; SqlValue.Int 2L ], CancellationToken.None)
        Assert.Equal(2L, inserted.RowsAffected)
        do! connection.TransactionAsync(fun () ->
            task {
                let! _ = connection.ExecuteAsync("INSERT INTO T VALUES (:id)", [ "id", SqlValue.Int 3L ])
                return ()
            })
        let! total = connection.ExecuteAsync "SELECT SUM(ID) AS S FROM T"
        Assert.Equal(6L, total.Rows.[0].int64 "S")
        do! connection.PingAsync()
    }
    |> fun t -> t.GetAwaiter().GetResult()

[<EngineFact>]
let ``the server version and a ping answer`` () =
    use connection = Connection.Open(engineDsn ())
    Assert.False(String.IsNullOrEmpty connection.ServerVersion)
    connection.Ping()

[<EngineFact>]
let ``a pack declaring its own count runs without touching the session`` () =
    let _, dsn = freshDatabase "MULTI"
    use connection = Connection.Open dsn
    // The session still runs one statement per request, so the pack is refused on its count alone.
    let refused =
        raisesKind ErrorKind.Refused (fun () ->
            connection.Execute "CREATE TABLE T (ID INTEGER); INSERT INTO T VALUES (1), (2)")
    Assert.Contains("statement count", refused.Message)
    // The same pack, saying how many statements it holds, runs — and no ALTER SESSION was needed.
    let result =
        connection.Execute(
            "CREATE TABLE T (ID INTEGER); INSERT INTO T VALUES (1), (2)",
            multiStatementCount = 2
        )
    Assert.Equal(2, result.ResultSets.Length)
    Assert.Equal(2L, (connection.Execute "SELECT COUNT(*) AS N FROM T").Rows.[0].int64 "N")
    // The count was the request's alone: the session is where it was, still one statement a request.
    raisesKind ErrorKind.Refused (fun () -> connection.Execute "SELECT 1; SELECT 2") |> ignore
    // Zero accepts any number.
    let three = connection.Execute("SELECT 1 AS A; SELECT 2 AS B; SELECT 3 AS C", multiStatementCount = 0)
    Assert.Equal(3, three.ResultSets.Length)
