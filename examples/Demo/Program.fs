// A tour of the driver against a running engine:
//
//     dotnet run --project examples/Demo -- frostlake://localhost:18082

open System
open Frostlake.FSharp

type Person =
    { Id: int
      Name: string
      Nickname: string option
      Born: DateOnly }

[<EntryPoint>]
let main argv =
    let server = if argv.Length > 0 then argv.[0] else "frostlake://localhost:18082"

    // The scope a connection opens on is applied with USE, so the database has to exist first.
    server
    |> Sql.connect
    |> Sql.query "CREATE DATABASE IF NOT EXISTS DEMO"
    |> Sql.executeNonQuery
    |> ignore

    let config =
        { Dsn.parse server with
            Database = Some "DEMO"
            Schema = Some "PUBLIC" }

    use connection = Connection.Open config
    printfn "Connected to Frostlake %s, session %s" connection.ServerVersion (defaultArg connection.SessionId "-")

    connection.Execute "CREATE OR REPLACE TABLE PEOPLE (ID INTEGER, NAME VARCHAR, NICKNAME VARCHAR, BORN DATE)"
    |> ignore

    // One statement per parameter set, all inside one transaction.
    let inserted =
        connection
        |> Sql.existingConnection
        |> Sql.executeTransaction
            [ "INSERT INTO PEOPLE VALUES (:id, :name, :nickname, :born)",
              [ [ "id", Sql.int 1
                  "name", Sql.text "Ada Lovelace"
                  "nickname", Sql.textOrNone None
                  "born", Sql.date (DateOnly(1815, 12, 10)) ]
                [ "id", Sql.int 2
                  "name", Sql.text "Grace Hopper"
                  "nickname", Sql.text "Amazing Grace"
                  "born", Sql.date (DateOnly(1906, 12, 9)) ] ] ]
    printfn "Inserted %d rows" (List.sum inserted)

    let people =
        connection
        |> Sql.existingConnection
        |> Sql.query "SELECT ID, NAME, NICKNAME, BORN FROM PEOPLE WHERE BORN < ? ORDER BY ID"
        |> Sql.args [ Sql.date (DateOnly(2000, 1, 1)) ]
        |> Sql.execute (fun read ->
            { Id = read.int "ID"
              Name = read.string "NAME"
              Nickname = read.stringOrNone "NICKNAME"
              Born = read.date "BORN" })
    for person in people do
        let nickname = person.Nickname |> Option.map (sprintf " (%s)") |> Option.defaultValue ""
        printfn "  %d %-14s born %s%s" person.Id person.Name (person.Born.ToString("yyyy-MM-dd")) nickname

    // A transaction that raises is rolled back.
    try
        connection.Transaction(fun () ->
            connection.Execute("DELETE FROM PEOPLE WHERE ID = ?", Sql.int 1) |> ignore
            failwith "changed my mind")
    with error ->
        printfn "Rolled back: %s" error.Message

    let count = (connection.Execute "SELECT COUNT(*) AS N FROM PEOPLE").Rows.[0].int64 "N"
    printfn "Still %d people" count

    // A refused statement is an answer, not a broken connection.
    try
        connection.Execute "SELECT * FROM NO_SUCH_TABLE" |> ignore
    with :? FrostlakeException as error when error.Kind = ErrorKind.Refused ->
        printfn "Refused: %s" (error.Message.Replace("\n", " "))

    printfn "Connection still open: %b" connection.IsOpen
    0
