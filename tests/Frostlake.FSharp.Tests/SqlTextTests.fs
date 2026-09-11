module Frostlake.FSharp.Tests.SqlTextTests

open Frostlake.FSharp
open Xunit

let private names (sql: string) =
    SqlText.markers sql |> List.map (fun m -> if m.Name = "" then "?" else m.Name)

[<Fact>]
let ``question marks are positional markers`` () =
    Assert.Equal<string list>([ "?"; "?" ], names "SELECT ?, ? FROM t")

[<Fact>]
let ``markers inside literals, identifiers, bodies and comments are text`` () =
    Assert.Empty(names "SELECT 'a?b', 'it''s ?', 'back\\'?' FROM \"we?ird\"")
    Assert.Empty(names "CREATE FUNCTION f() RETURNS INT AS $$ SELECT ? $$")
    Assert.Empty(names "SELECT 1 -- ? :a\n")
    Assert.Empty(names "SELECT 1 // ? @b\n")
    Assert.Empty(names "SELECT /* ? :x @y */ 1")

[<Fact>]
let ``a dollar pair inside an identifier does not open a body`` () =
    Assert.Equal<string list>([ "?" ], names "SELECT A$$B, ? FROM t")

[<Fact>]
let ``casts, assignments and server-side positional references are not markers`` () =
    Assert.Empty(names "SELECT v::date, x := 1, :1, :2")

[<Fact>]
let ``a colon glued to an expression is variant path access`` () =
    Assert.Empty(names "SELECT v:field, PARSE_JSON('{}'):k, \"V\":k, arr[0]:k, f(x):k, {'a':1}:a")

[<Fact>]
let ``named markers follow an operator, a comma or the start`` () =
    Assert.Equal<string list>([ "a"; "b"; "c" ], names ":a, x = :b AND y IN (@c)")

[<Fact>]
let ``an at-sign glued to a name is not a marker`` () = Assert.Empty(names "SELECT a@b FROM t")

[<Fact>]
let ``requests split on top-level semicolons only`` () =
    Assert.Equal<string list>(
        [ "SELECT 1"; " SELECT ';'"; " SELECT \";\"" ],
        SqlText.statements "SELECT 1; SELECT ';'; SELECT \";\";"
    )

[<Fact>]
let ``a semicolon in a dollar body or a comment does not split`` () =
    Assert.Equal(1, (SqlText.statements "EXECUTE IMMEDIATE $$ SELECT 1; SELECT 2; $$").Length)
    Assert.Equal(2, (SqlText.statements "SELECT 1 -- a; b\n; SELECT 2").Length)

[<Fact>]
let ``leading words skip comments and fold case`` () =
    Assert.Equal<string list>(
        [ "CREATE"; "OR"; "REPLACE" ],
        SqlText.leadingWords "/* c */ -- x\n create or replace table t" 3
    )

[<Theory>]
[<InlineData("USE SCHEMA s", true)>]
[<InlineData("use database d", true)>]
[<InlineData("SET x = 1", true)>]
[<InlineData("UNSET x", true)>]
[<InlineData("ALTER SESSION SET TIMEZONE = 'UTC'", true)>]
[<InlineData("CREATE OR REPLACE DATABASE d", true)>]
[<InlineData("CREATE SCHEMA IF NOT EXISTS s", true)>]
[<InlineData("DROP DATABASE d", true)>]
[<InlineData("CREATE TEMPORARY TABLE t (a INT)", true)>]
[<InlineData("CREATE OR REPLACE TEMP TABLE t (a INT)", true)>]
[<InlineData("CREATE LOCAL TEMPORARY TABLE t (a INT)", true)>]
[<InlineData("CREATE TABLE t (a INT)", false)>]
[<InlineData("CREATE OR REPLACE TRANSIENT TABLE t (a INT)", false)>]
[<InlineData("ALTER TABLE t ADD COLUMN b INT", false)>]
[<InlineData("SELECT 1", false)>]
[<InlineData("INSERT INTO t VALUES (1)", false)>]
let ``which statements leave session state behind`` (sql: string, expected: bool) =
    Assert.Equal(expected, SqlText.touchesSession sql)

[<Theory>]
[<InlineData("BEGIN", "Begins")>]
[<InlineData("begin transaction", "Begins")>]
[<InlineData("BEGIN WORK", "Begins")>]
[<InlineData("BEGIN NAME t1", "Begins")>]
[<InlineData("START TRANSACTION", "Begins")>]
[<InlineData("COMMIT", "Ends")>]
[<InlineData("ROLLBACK WORK", "Ends")>]
[<InlineData("BEGIN LET x := 1; RETURN x; END", "NoEffect")>]
[<InlineData("SELECT 1", "NoEffect")>]
let ``transaction control is recognised and a scripting block is not`` (sql: string, expected: string) =
    let effect = SqlText.transactionEffect (List.head (SqlText.statements sql))
    Assert.Equal(expected, sprintf "%A" effect)

[<Theory>]
[<InlineData("MY_DB", "MY_DB")>]
[<InlineData("my_db", "my_db")>]
[<InlineData("\"My Db\"", "\"My Db\"")>]
[<InlineData("my db", "\"my db\"")>]
[<InlineData("a\"b", "\"a\"\"b\"")>]
[<InlineData("DB.S", "\"DB.S\"")>]
[<InlineData("1abc", "\"1abc\"")>]
let ``DSN names are written as identifiers`` (name: string, expected: string) =
    Assert.Equal(expected, SqlText.identifier name)
