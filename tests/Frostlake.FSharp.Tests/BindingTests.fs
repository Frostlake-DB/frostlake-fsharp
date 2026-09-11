module Frostlake.FSharp.Tests.BindingTests

open System
open System.Numerics
open Frostlake.FSharp
open Frostlake.FSharp.Tests.TestSupport
open Xunit

let private bind (sql: string) (args: SqlValue list) = Binding.bind sql (Array.ofList args) [||] true

let private bindNamed (sql: string) (args: (string * SqlValue) list) =
    Binding.bind sql [||] (Array.ofList args) true

let private literal (value: SqlValue) = SqlValue.toSqlLiteral value

type private Colour =
    | Red = 1
    | Green = 2

[<Fact>]
let ``positional arguments fill question marks in order`` () =
    Assert.Equal(
        "SELECT 1, 'a' WHERE x = TRUE",
        bind "SELECT ?, ? WHERE x = ?" [ SqlValue.Int 1L; SqlValue.Text "a"; SqlValue.Bool true ]
    )

[<Fact>]
let ``with no arguments the text is sent as written`` () =
    let body = "EXECUTE IMMEDIATE $$ DECLARE c CURSOR FOR SELECT ?; BEGIN OPEN c USING (1); END; $$"
    Assert.Equal(body, bind body [])
    Assert.Equal("SELECT :x, ?", bind "SELECT :x, ?" [])

[<Fact>]
let ``a positional count mismatch is refused both ways`` () =
    raisesKind ErrorKind.Binding (fun () -> bind "SELECT ?, ?" [ SqlValue.Int 1L ]) |> ignore
    raisesKind ErrorKind.Binding (fun () -> bind "SELECT ?" [ SqlValue.Int 1L; SqlValue.Int 2L ]) |> ignore

[<Fact>]
let ``negative numbers are parenthesised so they cannot open a comment`` () =
    Assert.Equal("SELECT 3-(-5)", bind "SELECT 3-?" [ SqlValue.Int(-5L) ])
    Assert.Equal("SELECT 3-(-1.50)", bind "SELECT 3-?" [ SqlValue.Decimal(-1.50M) ])
    Assert.Equal("SELECT 3-(-2.5::FLOAT)", bind "SELECT 3-?" [ SqlValue.Float(-2.5) ])
    Assert.Equal(
        "SELECT 3-(-12345678901234567890)",
        bind "SELECT 3-?" [ SqlValue.BigInt(BigInteger.Parse "-12345678901234567890") ]
    )
    Assert.Equal("SELECT 3-(-7.25)", bind "SELECT 3-?" [ SqlValue.DecimalText "-7.25" ])

[<Fact>]
let ``named arguments bind ignoring case and prefix`` () =
    Assert.Equal(
        "SELECT 1, 1, 'x'",
        bindNamed "SELECT :id, @ID, :Name" [ "@id", SqlValue.Int 1L; ":name", SqlValue.Text "x" ]
    )

[<Fact>]
let ``an unused named argument is refused when strict and allowed otherwise`` () =
    raisesKind ErrorKind.Binding (fun () -> bindNamed "SELECT :a" [ "a", SqlValue.Int 1L; "b", SqlValue.Int 2L ])
    |> ignore
    Assert.Equal("SELECT 1", Binding.bind "SELECT :a" [||] [| "a", SqlValue.Int 1L; "b", SqlValue.Int 2L |] false)

[<Fact>]
let ``a named marker with no argument is left for the server`` () =
    Assert.Equal(
        "COPY INTO t FROM @stage WHERE x = 1",
        bindNamed "COPY INTO t FROM @stage WHERE x = :x" [ "x", SqlValue.Int 1L ]
    )

[<Fact>]
let ``question marks with only named arguments are refused`` () =
    raisesKind ErrorKind.Binding (fun () -> bindNamed "SELECT ?, :a" [ "a", SqlValue.Int 1L ]) |> ignore

[<Fact>]
let ``positional arguments leave named markers for the server`` () =
    Assert.Equal("SELECT 1, :x", bind "SELECT ?, :x" [ SqlValue.Int 1L ])

[<Fact>]
let ``a name given twice is refused`` () =
    raisesKind ErrorKind.Binding (fun () -> bindNamed "SELECT :a" [ "a", SqlValue.Int 1L; "@A", SqlValue.Int 2L ])
    |> ignore

[<Fact>]
let ``text escapes backslashes and quotes`` () =
    Assert.Equal("'it''s a \\\\ path'", literal (SqlValue.Text "it's a \\ path"))

[<Fact>]
let ``every value has its literal`` () =
    Assert.Equal("NULL", literal SqlValue.Null)
    Assert.Equal("TRUE", literal (SqlValue.Bool true))
    Assert.Equal("FALSE", literal (SqlValue.Bool false))
    Assert.Equal("42", literal (SqlValue.Int 42L))
    Assert.Equal("1.50", literal (SqlValue.Decimal 1.50M))
    Assert.Equal("1.5::FLOAT", literal (SqlValue.Float 1.5))
    Assert.Equal("1E+300::FLOAT", literal (SqlValue.Float 1e300))
    Assert.Equal("'NaN'::FLOAT", literal (SqlValue.Float nan))
    Assert.Equal("'Infinity'::FLOAT", literal (SqlValue.Float infinity))
    Assert.Equal("'-Infinity'::FLOAT", literal (SqlValue.Float(-infinity)))
    Assert.Equal("X'0AFF'", literal (SqlValue.Binary [| 0x0Auy; 0xFFuy |]))
    Assert.Equal("'2024-01-02'::DATE", literal (SqlValue.Date(DateOnly(2024, 1, 2))))
    Assert.Equal("'12:34:56'::TIME", literal (SqlValue.Time(TimeOnly(12, 34, 56))))
    Assert.Equal("'12:34:56.5'::TIME", literal (SqlValue.Time(TimeOnly(12, 34, 56, 500))))
    Assert.Equal(
        "'2024-01-02 03:04:05.1234567'::TIMESTAMP_NTZ",
        literal (SqlValue.Timestamp(DateTime(2024, 1, 2, 3, 4, 5).AddTicks 1234567L))
    )
    Assert.Equal(
        "'2024-01-02 03:04:05'::TIMESTAMP_NTZ",
        literal (SqlValue.Timestamp(DateTime(2024, 1, 2, 3, 4, 5, DateTimeKind.Utc)))
    )
    Assert.Equal(
        "'2024-01-02 03:04:05 +02:00'::TIMESTAMP_TZ",
        literal (SqlValue.TimestampTz(DateTimeOffset(2024, 1, 2, 3, 4, 5, TimeSpan.FromHours 2.0)))
    )
    Assert.Equal("PARSE_JSON('{\"a\":''x''}')", literal (SqlValue.Variant "{\"a\":'x'}"))
    Assert.Equal("CURRENT_DATE()", literal (SqlValue.Raw "CURRENT_DATE()"))
    Assert.Equal("NULL", literal (SqlValue.Text null))

[<Fact>]
let ``decimal text is inlined only when it is a number`` () =
    Assert.Equal(
        "123456789012345678901234567890.123",
        literal (SqlValue.DecimalText "123456789012345678901234567890.123")
    )
    Assert.Equal("1E+5", literal (SqlValue.DecimalText "+1E+5"))
    raisesKind ErrorKind.Binding (fun () -> literal (SqlValue.DecimalText "1; DROP TABLE t")) |> ignore

[<Fact>]
let ``dotnet values convert by their runtime type`` () =
    Assert.Equal(SqlValue.Int 5L, SqlValue.ofObj (box 5))
    Assert.Equal(SqlValue.Int 5L, SqlValue.ofObj (box 5uy))
    Assert.Equal(SqlValue.BigInt(BigInteger UInt64.MaxValue), SqlValue.ofObj (box UInt64.MaxValue))
    Assert.Equal(SqlValue.Float 1.1, SqlValue.ofObj (box 1.1f))
    Assert.Equal(SqlValue.Decimal 2.50M, SqlValue.ofObj (box 2.50M))
    Assert.Equal(SqlValue.Text "x", SqlValue.ofObj (box 'x'))
    Assert.Equal(SqlValue.Null, SqlValue.ofObj null)
    Assert.Equal(SqlValue.Null, SqlValue.ofObj (box DBNull.Value))
    Assert.Equal(SqlValue.Int 7L, SqlValue.ofObj (box (Some 7)))
    Assert.Equal(SqlValue.Null, SqlValue.ofObj (box (None: int option)))
    Assert.Equal(SqlValue.Int 8L, SqlValue.ofObj (box (ValueSome 8)))
    Assert.Equal(SqlValue.Null, SqlValue.ofObj (box (ValueNone: int voption)))
    Assert.Equal(SqlValue.Int 2L, SqlValue.ofObj (box Colour.Green))
    Assert.Equal(SqlValue.Time(TimeOnly(1, 2, 3)), SqlValue.ofObj (box (TimeSpan(1, 2, 3))))
    Assert.Equal(SqlValue.Text "x", SqlValue.ofObj (box (SqlValue.Text "x")))
    raisesKind ErrorKind.Binding (fun () -> SqlValue.ofObj (box (TimeSpan.FromDays 2.0))) |> ignore
    raisesKind ErrorKind.Binding (fun () -> SqlValue.ofObj (box (Uri "http://x"))) |> ignore
