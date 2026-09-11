/// The ADO.NET provider against a real engine, including Dapper, which proves the surface is the
/// standard one.
module Frostlake.FSharp.Tests.AdoEngineTests

open System
open System.Data
open System.Data.Common
open Dapper
open Frostlake.FSharp
open Frostlake.FSharp.Tests.TestSupport
open Xunit

[<CLIMutable>]
type CrewMember = { Id: int64; Name: string; Joined: DateTime }

let private connect (dsn: string) =
    let connection = new FrostlakeConnection(dsn)
    connection.Open()
    connection

let private crew (connection: DbConnection) =
    connection.Execute("CREATE OR REPLACE TABLE CREW (ID INTEGER, NAME VARCHAR, JOINED DATE)") |> ignore
    connection.Execute(
        "INSERT INTO CREW VALUES (@Id, @Name, @Joined)",
        [| {| Id = 1; Name = "Ada"; Joined = DateTime(1843, 7, 1) |}
           {| Id = 2; Name = "Grace"; Joined = DateTime(1944, 8, 7) |} |]
    )

[<EngineFact>]
let ``Dapper runs on the provider`` () =
    let _, dsn = freshDatabase "DAPPER"
    use connection = connect dsn
    Assert.Equal(2, crew connection)
    let everyone = connection.Query<CrewMember>("SELECT ID, NAME, JOINED FROM CREW ORDER BY ID") |> List.ofSeq
    Assert.Equal(2, everyone.Length)
    Assert.Equal("Grace", everyone.[1].Name)
    Assert.Equal(DateTime(1843, 7, 1), everyone.[0].Joined)
    let one = connection.QuerySingle<CrewMember>("SELECT ID, NAME, JOINED FROM CREW WHERE ID = @id", {| id = 2 |})
    Assert.Equal("Grace", one.Name)
    Assert.Equal(2L, connection.ExecuteScalar<int64>("SELECT COUNT(*) FROM CREW"))

[<EngineFact>]
let ``commands bind positional and named parameters`` () =
    let _, dsn = freshDatabase "ADOCMD"
    use connection = connect dsn
    use create = connection.CreateCommand()
    create.CommandText <- "CREATE TABLE T (ID INTEGER, NAME VARCHAR)"
    Assert.Equal(-1, create.ExecuteNonQuery())
    use insert = new FrostlakeCommand("INSERT INTO T VALUES (?, ?), (:id, :name)", connection)
    insert.Parameters.AddWithValue("", 1) |> ignore
    insert.Parameters.AddWithValue("", "Ada") |> ignore
    insert.Parameters.AddWithValue("id", 2) |> ignore
    insert.Parameters.AddWithValue("@name", "Grace") |> ignore
    Assert.Equal(2, insert.ExecuteNonQuery())
    use count = new FrostlakeCommand("SELECT COUNT(*) FROM T", connection)
    Assert.Equal(box 2L, count.ExecuteScalar())
    use nothing = new FrostlakeCommand("SELECT ID FROM T WHERE ID = 99", connection)
    Assert.Null(nothing.ExecuteScalar())

[<EngineFact>]
let ``a reader walks a script's result sets`` () =
    let _, dsn = freshDatabase "ADOREAD"
    use connection = connect dsn
    // A request carrying more than one statement has to be asked for; 0 means any number.
    use declare = connection.CreateCommand()
    declare.CommandText <- "ALTER SESSION SET MULTI_STATEMENT_COUNT = 0"
    declare.ExecuteNonQuery() |> ignore
    use command = connection.CreateCommand()
    command.CommandText <- "CREATE TABLE T (ID INTEGER); INSERT INTO T VALUES (1), (2); SELECT ID FROM T ORDER BY ID"
    use reader = command.ExecuteReader()
    let mutable ids = []
    let mutable more = true
    while more do
        if reader.FieldCount > 0 && reader.GetName(0) = "ID" then
            while reader.Read() do
                ids <- ids @ [ reader.GetInt64 0 ]
        more <- reader.NextResult()
    Assert.Equal<int64 list>([ 1L; 2L ], ids)
    Assert.Equal(2, reader.RecordsAffected)

[<EngineFact>]
let ``transactions commit, roll back, and roll back on dispose`` () =
    let _, dsn = freshDatabase "ADOTXN"
    use connection = connect dsn
    connection.Execute("CREATE TABLE T (ID INTEGER)") |> ignore
    do
        use transaction = connection.BeginTransaction()
        connection.Execute("INSERT INTO T VALUES (1)", transaction = transaction) |> ignore
        transaction.Commit()
    do
        use transaction = connection.BeginTransaction()
        connection.Execute("INSERT INTO T VALUES (2)", transaction = transaction) |> ignore
        transaction.Rollback()
    do
        use _transaction = connection.BeginTransaction()
        connection.Execute("INSERT INTO T VALUES (3)") |> ignore
    Assert.Equal(1L, connection.ExecuteScalar<int64>("SELECT COUNT(*) FROM T"))
    Assert.Throws<NotSupportedException>(fun () -> connection.BeginTransaction(IsolationLevel.Serializable) |> ignore)
    |> ignore

[<EngineFact>]
let ``a connection string opens like a DSN`` () =
    let name, _ = freshDatabase "ADOKV"
    let config = Dsn.parse (engineDsn ())
    use connection =
        connect (sprintf "Host=%s;Port=%d;Database=%s;Schema=PUBLIC" config.BaseUri.Host config.BaseUri.Port name)
    Assert.Equal(ConnectionState.Open, connection.State)
    Assert.Equal(name, connection.Database)
    Assert.Equal(name, connection.ExecuteScalar<string>("SELECT CURRENT_DATABASE()"))
    connection.ChangeDatabase "SNOWFLAKE"
    Assert.Equal("SNOWFLAKE", connection.ExecuteScalar<string>("SELECT CURRENT_DATABASE()"))
    Assert.False(String.IsNullOrEmpty connection.ServerVersion)

[<EngineFact>]
let ``a data table loads from a reader`` () =
    let _, dsn = freshDatabase "ADOTABLE"
    use connection = connect dsn
    crew connection |> ignore
    use reader = connection.ExecuteReader("SELECT ID, NAME, JOINED FROM CREW ORDER BY ID")
    let table = new DataTable()
    table.Load reader
    Assert.Equal(2, table.Rows.Count)
    Assert.Equal(typeof<int64>, table.Columns.["ID"].DataType)
    Assert.Equal(box "Ada", table.Rows.[0].["NAME"])

[<EngineFact>]
let ``the F# API runs on an ADO connection's session`` () =
    let _, dsn = freshDatabase "ADOBRIDGE"
    use connection = connect dsn
    connection.Execute("CREATE TEMPORARY TABLE SCRATCH (N INTEGER)") |> ignore
    connection.Session |> Sql.existingConnection |> Sql.query "INSERT INTO SCRATCH VALUES (5)" |> Sql.executeNonQuery |> ignore
    Assert.Equal(5L, connection.ExecuteScalar<int64>("SELECT N FROM SCRATCH"))
