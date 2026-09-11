/// The Sql pipeline against a real engine.
module Frostlake.FSharp.Tests.SqlModuleTests

open System
open Frostlake.FSharp
open Frostlake.FSharp.Tests.TestSupport
open Xunit

type private Person = { Id: int; Name: string; Nickname: string option }

let private people (dsn: string) =
    dsn
    |> Sql.connect
    |> Sql.query "CREATE OR REPLACE TABLE PEOPLE (ID INTEGER, NAME VARCHAR, NICKNAME VARCHAR)"
    |> Sql.executeNonQuery
    |> ignore
    dsn
    |> Sql.connect
    |> Sql.executeTransaction
        [ "INSERT INTO PEOPLE VALUES (:id, :name, :nickname)",
          [ [ "id", Sql.int 1; "name", Sql.text "Ada"; "nickname", Sql.textOrNone None ]
            [ "id", Sql.int 2; "name", Sql.text "Grace"; "nickname", Sql.textOrNone (Some "Amazing") ] ] ]

[<EngineFact>]
let ``rows map to records`` () =
    let _, dsn = freshDatabase "SQLMAP"
    Assert.Equal<int64 list>([ 2L ], people dsn)
    let found =
        dsn
        |> Sql.connect
        |> Sql.query "SELECT ID, NAME, NICKNAME FROM PEOPLE WHERE ID >= :minimum ORDER BY ID"
        |> Sql.parameters [ "minimum", Sql.int 1 ]
        |> Sql.execute (fun read ->
            { Id = read.int "ID"
              Name = read.string "NAME"
              Nickname = read.stringOrNone "NICKNAME" })
    Assert.Equal<Person list>(
        [ { Id = 1; Name = "Ada"; Nickname = None }
          { Id = 2; Name = "Grace"; Nickname = Some "Amazing" } ],
        found
    )

[<EngineFact>]
let ``positional arguments, a single row and a count`` () =
    let _, dsn = freshDatabase "SQLROW"
    people dsn |> ignore
    let name =
        dsn
        |> Sql.connect
        |> Sql.query "SELECT NAME FROM PEOPLE WHERE ID = ?"
        |> Sql.args [ Sql.int 2 ]
        |> Sql.executeRow (fun read -> read.string "NAME")
    Assert.Equal("Grace", name)
    let changed =
        dsn
        |> Sql.connect
        |> Sql.query "UPDATE PEOPLE SET NICKNAME = ? WHERE NICKNAME IS NULL"
        |> Sql.args [ Sql.text "Countess" ]
        |> Sql.executeNonQuery
    Assert.Equal(1L, changed)
    raisesKind ErrorKind.Usage (fun () ->
        dsn |> Sql.connect |> Sql.query "SELECT * FROM PEOPLE WHERE ID = 99" |> Sql.executeRow ignore)
    |> ignore

[<EngineFact>]
let ``a failed transaction leaves nothing behind`` () =
    let _, dsn = freshDatabase "SQLTXN"
    people dsn |> ignore
    raisesKind ErrorKind.Refused (fun () ->
        dsn
        |> Sql.connect
        |> Sql.executeTransaction
            [ "INSERT INTO PEOPLE VALUES (:id, :name, NULL)", [ [ "id", Sql.int 3; "name", Sql.text "Linus" ] ]
              "INSERT INTO NO_SUCH_TABLE VALUES (1)", [] ])
    |> ignore
    let count =
        dsn |> Sql.connect |> Sql.query "SELECT COUNT(*) AS N FROM PEOPLE" |> Sql.executeRow (fun r -> r.int "N")
    Assert.Equal(2, count)

[<EngineFact>]
let ``an existing connection keeps its session`` () =
    let _, dsn = freshDatabase "SQLSESSION"
    use connection = Connection.Open dsn
    connection |> Sql.existingConnection |> Sql.query "CREATE TEMPORARY TABLE SCRATCH (N INTEGER)" |> Sql.executeNonQuery |> ignore
    connection |> Sql.existingConnection |> Sql.query "INSERT INTO SCRATCH VALUES (1), (2)" |> Sql.executeNonQuery |> ignore
    let total =
        connection
        |> Sql.existingConnection
        |> Sql.query "SELECT SUM(N) AS S FROM SCRATCH"
        |> Sql.executeRow (fun r -> r.int "S")
    Assert.Equal(3, total)

[<EngineFact>]
let ``executions on a DSN release their sessions`` () =
    if engineReleasesSessions () then
        let before = activeSessions ()
        for _ in 1..5 do
            engineDsn () |> Sql.connect |> Sql.query "SELECT 1" |> Sql.executeNonQuery |> ignore
        Assert.Equal(before, activeSessions ())

[<EngineFact>]
let ``the asynchronous pipeline`` () =
    let _, dsn = freshDatabase "SQLASYNC"
    people dsn |> ignore
    let names =
        (dsn
         |> Sql.connect
         |> Sql.query "SELECT NAME FROM PEOPLE ORDER BY ID"
         |> Sql.executeAsync (fun read -> read.string "NAME"))
            .GetAwaiter()
            .GetResult()
    Assert.Equal<string list>([ "Ada"; "Grace" ], names)
    let count =
        (dsn |> Sql.connect |> Sql.query "DELETE FROM PEOPLE" |> Sql.executeNonQueryAsync).GetAwaiter().GetResult()
    Assert.Equal(2L, count)

[<EngineFact>]
let ``the value helpers bind what they say`` () =
    let row =
        engineDsn ()
        |> Sql.connect
        |> Sql.query "SELECT :a AS A, :b AS B, :c AS C, :d AS D, :e AS E, :f AS F, :g AS G"
        |> Sql.parameters
            [ "a", Sql.intOrNone None
              "b", Sql.decimal 12.50M
              "c", Sql.bool false
              "d", Sql.date (DateOnly(2024, 3, 1))
              "e", Sql.uuid (Guid.Parse "7f5fd0d6-5a21-4b8a-9a7e-2f33b9b8ff10")
              "f", Sql.variant "[1,2]"
              "g", Sql.raw "1 + 1" ]
        |> Sql.executeRow id
    Assert.True(row.isNull "A")
    Assert.Equal(12.50M, row.decimal "B")
    Assert.False(row.bool "C")
    Assert.Equal(DateOnly(2024, 3, 1), row.date "D")
    Assert.Equal(Guid.Parse "7f5fd0d6-5a21-4b8a-9a7e-2f33b9b8ff10", row.uuid "E")
    Assert.Equal("[1,2]", (row.variant "F").Replace(" ", "").Replace("\n", ""))
    Assert.Equal(2, row.int "G")

[<EngineFact>]
let ``a pipeline query may declare how many statements it packs`` () =
    let _, dsn = freshDatabase "SQLMULTI"
    let packed =
        dsn
        |> Sql.connect
        |> Sql.query "CREATE OR REPLACE TABLE T (ID INTEGER); INSERT INTO T VALUES (1), (2), (3)"
        |> Sql.multiStatementCount 2
        |> Sql.executeResult
    Assert.Equal(2, packed.ResultSets.Length)
    let counted =
        dsn
        |> Sql.connect
        |> Sql.query "SELECT COUNT(*) AS N FROM T"
        |> Sql.executeRow (fun read -> read.int64 "N")
    Assert.Equal(3L, counted)
