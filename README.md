# frostlake-fsharp

An F# driver for [Frostlake](https://frostlake.dev), speaking the engine's HTTP protocol against a
running `DatabaseHttpServer`. It offers two surfaces over one session machinery:

- an F# API — `Sql.connect dsn |> Sql.query … |> Sql.execute read`, typed row readers, and a
  `SqlValue` union for parameters and cells;
- an ADO.NET provider (`FrostlakeConnection`, `FrostlakeCommand`, …) that Dapper and other ADO.NET
  libraries run on.

.NET 8, and nothing beyond FSharp.Core: the transport is `HttpClient` and the JSON is
`System.Text.Json`, both in the base library.

## Engine version

Requires a Frostlake engine **0.0.7 or newer**. Ask a running server which one it is with
`SELECT CURRENT_VERSION()`. The driver speaks the HTTP protocol, not the jar, so this is a floor
rather than a lockstep pin. Engines from 0.1.0 add three things the driver uses when they are there:
releasing a session on close, exact detection of a lost session, and full TIME and zoned-timestamp
precision on the wire (see *Sessions* and *Known limitations* below).

## Install

```sh
dotnet add package Frostlake.FSharp
```

## Quick start

```fsharp
open Frostlake.FSharp

// A connection opens on its DSN's database with USE, so the database has to exist first.
"frostlake://localhost:18082"
|> Sql.connect
|> Sql.query "CREATE DATABASE IF NOT EXISTS MY_DB"
|> Sql.executeNonQuery
|> ignore

let dsn = "frostlake://localhost:18082/MY_DB/PUBLIC"

dsn
|> Sql.connect
|> Sql.query "CREATE OR REPLACE TABLE PEOPLE (ID INTEGER, NAME VARCHAR, NICKNAME VARCHAR)"
|> Sql.executeNonQuery
|> ignore

// Each parameter set runs the statement once, all inside one transaction.
dsn
|> Sql.connect
|> Sql.executeTransaction
    [ "INSERT INTO PEOPLE VALUES (:id, :name, :nickname)",
      [ [ "id", Sql.int 1; "name", Sql.text "Ada"; "nickname", Sql.textOrNone None ]
        [ "id", Sql.int 2; "name", Sql.text "Grace"; "nickname", Sql.text "Amazing Grace" ] ] ]
|> ignore

type Person = { Id: int; Name: string; Nickname: string option }

let people =
    dsn
    |> Sql.connect
    |> Sql.query "SELECT ID, NAME, NICKNAME FROM PEOPLE WHERE ID >= :minimum ORDER BY ID"
    |> Sql.parameters [ "minimum", Sql.int 1 ]
    |> Sql.execute (fun read ->
        { Id = read.int "ID"
          Name = read.string "NAME"
          Nickname = read.stringOrNone "NICKNAME" })
```

`Sql.connect` opens a connection for each execution and releases it afterwards, so nothing carries
over from one execution to the next. To keep a session — a temporary table, a `USE`, a transaction
that spans several calls — hold a `Connection` and run on it with `Sql.existingConnection`:

```fsharp
use connection = Connection.Open dsn
connection.Execute "CREATE TEMPORARY TABLE SCRATCH (N INTEGER)" |> ignore
connection.Execute("INSERT INTO SCRATCH VALUES (?), (?)", Sql.int 1, Sql.int 2) |> ignore

let total =
    connection
    |> Sql.existingConnection
    |> Sql.query "SELECT SUM(N) AS TOTAL FROM SCRATCH"
    |> Sql.executeRow (fun read -> read.int64 "TOTAL")
```

A runnable tour is in [examples/Demo](https://github.com/Frostlake-DB/frostlake-fsharp/blob/master/examples/Demo/Program.fs):

```sh
dotnet run --project examples/Demo -- frostlake://localhost:18082
```

## Connecting

```
frostlake://host:port[/DATABASE[/SCHEMA]][?parameter=value&…]
```

| Parameter | Meaning | Default |
| --- | --- | --- |
| `database` (or `db`), `schema` | database and schema the session starts in; the path can name both instead | — |
| `role`, `warehouse` | role and warehouse the session starts with | — |
| `timeout` | how long one request may take: `30s`, `5m`, `1500ms`, bare seconds, or `0` for no limit | `5m` |
| `connectTimeout` | how long opening a TCP connection may take | `10s` |
| `tls` | `true` to speak HTTPS; an `https://` DSN does the same | `false` |

The same settings work as an ADO.NET connection string:
`Host=localhost;Port=18082;Database=MY_DB;Schema=PUBLIC;Timeout=30` (`Host=localhost:18082` works too). A setting the driver does not
know is an error rather than a silent no-op, and so is a user or password: the engine's HTTP API has
no authentication to hand them to.

The scope goes on with `USE ROLE`, `USE WAREHOUSE`, `USE DATABASE` and `USE SCHEMA`, sent together
as one request when the connection opens — a request that declares how many statements it carries,
since the engine runs one per request unless it is asked for more. A plain name folds to upper case the way it does in SQL
(`my_db` selects `MY_DB`); a name in double quotes keeps its case — percent-encode it in a DSN:
`frostlake://localhost:18082/%22My%20Db%22`. A name the engine cannot find fails the open.

## Running statements

```fsharp
use connection = Connection.Open "frostlake://localhost:18082/MY_DB/PUBLIC"

let result = connection.Execute("SELECT ID, NAME FROM PEOPLE WHERE ID = ?", Sql.int 1)
for row in result.Rows do
    printfn "%d %s" (row.int64 "ID") (row.string "NAME")

connection.Execute("UPDATE PEOPLE SET NAME = :name WHERE ID = :id", [ "name", Sql.text "Ada L."; "id", Sql.int 1 ])
|> fun changed -> printfn "%d row(s) updated" changed.RowsAffected
```

`Execute` takes positional arguments, or a list of named ones; `ExecuteAsync` does the same
asynchronously and takes a `CancellationToken`. A request may hold several statements separated by
semicolons once the session has been asked for them — `ALTER SESSION SET MULTI_STATEMENT_COUNT = n`
for exactly `n`, or `= 0` for any number; a session left at the default of one refuses a request that
carries more. Either way the answer is a `QueryResult`:

- `ResultSets` — one per statement, each with `Columns`, `Rows` and, for DML, an `UpdateCount`;
- `Rows` and `Columns` — those of the final result set, so `CREATE …; INSERT …; SELECT …` answers the
  `SELECT`;
- `RowsAffected` — the rows every DML statement changed, added up;
- `Scalar` / `TryScalar` — the first cell of the final result set.

A `Row` is read by column name — an exact match first, then one ignoring case — through typed
readers that convert where the conversion is exact and raise where it is not: `int`, `int64`,
`bigint`, `decimal`, `double`, `string`, `bool`, `bytes`, `date`, `time`, `timestamp`,
`timestamptz`, `uuid` and `variant`, each with an `…OrNone` twin that answers `None` for NULL
(`row.intOrNone "ID"`). `row.["NAME"]` and `row.[0]` give the `SqlValue` itself.

`Sql` has the same shape as a pipeline: `execute` maps every row, `executeRow` the first,
`executeNonQuery` answers the rows changed, `executeResult` the whole `QueryResult`, and each has an
`…Async` twin.

### Saying how many statements one request carries

`ALTER SESSION` is not the only way to send a pack. Both surfaces take an optional
`multiStatementCount` that belongs to the one call:

```fsharp
connection.Execute("CREATE TABLE T (ID INTEGER); INSERT INTO T VALUES (1), (2)", multiStatementCount = 2)

dsn
|> Sql.connect
|> Sql.query "CREATE OR REPLACE TABLE T (ID INTEGER); INSERT INTO T VALUES (1), (2)"
|> Sql.multiStatementCount 2
|> Sql.executeResult
```

The count applies to that request and outranks the session's `MULTI_STATEMENT_COUNT` for it; `0`
accepts any number. It changes no session state, so there is nothing to save and put back, and two
connections — or two calls on one — can pack differently without disturbing each other. Leave it out
and nothing changes: the request carries no such field and the session's value decides, as it always
has.

## Parameters

The protocol carries no bind values, so arguments are inlined into the statement as SQL literals —
the same thing Frostlake's JDBC driver does.

- `?` takes positional arguments in order, and the counts must match exactly.
- `:name` and `@name` take named arguments, matched ignoring case (a `:` or `@` on the argument's
  own name is ignored). A named argument the statement does not use is an error, since it is almost
  always a misspelling; the ADO.NET provider allows it, because Dapper hands over every property of
  its parameter object.
- A named marker with no argument of that name is left alone for the engine — a stage
  (`COPY INTO t FROM @my_stage`) or a scripting variable is written the same way.
- With no arguments at all, the text is sent exactly as written, so a Snowflake Scripting cursor's
  own `?` (`OPEN c USING (…)`) and variables (`:v`) reach the engine untouched.
- Nothing inside a string literal, a quoted identifier, a `$$…$$` body or a comment is a marker, and
  neither are `::` (a cast), `:=` (an assignment), `:1`, or VARIANT path access — a colon glued to
  the end of an expression (`v:field`, `PARSE_JSON(…):k`, `"V":k`) reads a field, so a marker has to
  follow an operator, a comma, a parenthesis or the start of the text.
- A negative number is written in parentheses, so `3-?` bound to `-5` stays `3-(-5)` rather than
  opening a `--` comment.

| `SqlValue` | Helper | Literal |
| --- | --- | --- |
| `Null` | `Sql.dbnull`, any `…OrNone None` | `NULL` |
| `Bool` | `Sql.bool` | `TRUE` / `FALSE` |
| `Int`, `BigInt`, `Decimal` | `Sql.int`, `Sql.int64`, `Sql.bigint`, `Sql.decimal` | as written, scale kept |
| `Float` | `Sql.double` | `1.5::FLOAT`, and `'NaN'::FLOAT`, `'Infinity'::FLOAT` |
| `Text` | `Sql.text`, `Sql.uuid` | `'…'`, backslashes and quotes doubled |
| `Binary` | `Sql.bytes` | `X'0AFF'` |
| `Date`, `Time` | `Sql.date`, `Sql.time` | `'2024-01-02'::DATE`, `'12:34:56.5'::TIME` |
| `Timestamp` | `Sql.timestamp` | `'2024-01-02 03:04:05.678'::TIMESTAMP_NTZ` |
| `TimestampTz` | `Sql.timestamptz` | `'2024-01-02 03:04:05 +02:00'::TIMESTAMP_TZ` |
| `Variant` | `Sql.variant` | `PARSE_JSON('…')` |
| `DecimalText` | `Sql.decimalText` | the digits, checked to be a number |
| `Raw` | `Sql.raw` | the text verbatim — never pass it anything untrusted |

`Sql.value` (and `SqlValue.ofObj`) converts any .NET value by its runtime type, options included.
A float binds with a `FLOAT` cast because a bare `1.5` is a `NUMBER(2,1)` literal: without it a bound
double would take part in exact rather than floating-point arithmetic. `PARSE_JSON` is not allowed in
a `VALUES` clause, so a VARIANT goes in with `INSERT … SELECT ?`.

## Types

| Engine type | `SqlValue` | Reader |
| --- | --- | --- |
| `NUMBER(p,0)`, `INTEGER`, … | `Int`, or `BigInt` past 64 bits | `int64`, `int`, `bigint` |
| `NUMBER(p,s)` with a scale | `Decimal`, exact with its scale; `DecimalText` past 28 digits | `decimal`, `string` |
| `FLOAT`, `DOUBLE`, `REAL` | `Float`, NaN and ±Infinity included | `double` |
| `BOOLEAN` | `Bool` | `bool` |
| `VARCHAR` and the other text types | `Text` | `string` |
| `BINARY` | `Binary` | `bytes` |
| `DATE` | `Date` (`DateOnly`) | `date` |
| `TIME` | `Time` (`TimeOnly`) | `time` |
| `TIMESTAMP_NTZ` | `Timestamp` (`DateTime`, kind Unspecified) | `timestamp` |
| `TIMESTAMP_TZ`, `TIMESTAMP_LTZ` | `TimestampTz` (`DateTimeOffset`) | `timestamptz` |
| `VARIANT`, `OBJECT`, `ARRAY`, `VECTOR` | `Variant`, the engine's text | `variant` |

Exact numbers stay exact: a `NUMBER(12,2)` reads as a `decimal`, never through a float, and an
integer too wide for 64 bits — `NUMBER(38,0)` holds 38 digits — as a `BigInteger`. A cell whose text
does not parse as its column's type is kept as `Text` rather than dropped.

## Transactions

```fsharp
connection.Transaction(fun () ->
    connection.Execute("INSERT INTO PEOPLE VALUES (?, ?, NULL)", Sql.int 3, Sql.text "Edsger") |> ignore
    connection.Execute("DELETE FROM PEOPLE WHERE ID = ?", Sql.int 1) |> ignore)
```

`Transaction` commits when its body returns and rolls back when it raises; `TransactionAsync` is the
same for a task, and runs its body without the caller's `SynchronizationContext`, as all of the
driver's asynchronous work does, so blocking on it from a UI thread cannot deadlock. `BeginTransaction`, `Commit` and `Rollback` are there for the manual form, and a
`BEGIN` or `COMMIT` sent as a statement is tracked too. When the engine refuses a `COMMIT`, the driver
rolls back before raising rather than leave a transaction open for the next statement to inherit.
The engine offers read committed and nothing else.

## Sessions

One connection is one engine session, which holds the current database and schema, session
variables, `ALTER SESSION` settings and an open transaction.

- Opening a connection checks `/api/health` — anything can answer HTTP 200; its body is what says it
  is a Frostlake engine — and then puts the DSN's scope on a new session. Without a scope, the
  session starts with the first statement.
- Every later request names its session and asks the engine to refuse it if the session is gone
  (idle past the engine's 30 minutes, released, or from before a restart) rather than quietly start a
  fresh one in its place. Nothing has run when that refusal comes back. If the session held nothing
  a fresh one would lack, the driver starts one on the DSN's scope and sends the statement again.
  If it held something — an open transaction, or state set with `USE`, `SET`, `ALTER SESSION`, a
  temporary object, `CREATE` or `DROP` of a database or schema — re-running the statement elsewhere
  would be wrong, so the driver raises `SessionLost` instead, and the next statement starts over on
  the DSN's scope.
- `Close` (or `Dispose`) rolls back an open transaction and releases the session.
- `Reset` puts a connection back where it started, for reuse across unrelated work: it rolls back an
  open transaction and replaces a session whose scope or settings moved.
- A broken connection is retired, never retried. After a transport failure or a timeout the
  statement's fate is unknown — re-running it could duplicate an `INSERT` — so the connection raises
  `ConnectionClosed` from then on; open a new one. Closing it still releases its session.
- Calls on one connection are serialised, but they share one session: give each thread its own.

Engines before 0.1.0 cannot refuse an unknown session or release one. They re-create an expired
session under the same id at the server's default scope and say nothing, so against them the
driver re-applies the DSN's scope to a connection idle for more than five minutes, and closing
leaves the session to the engine's own idle expiry.

## Errors

Every failure is a `FrostlakeException`, which derives from `DbException`. Its `Kind` says what you
can do about it:

| `ErrorKind` | Meaning |
| --- | --- |
| `Refused` | The engine refused the statement — a SQL error. The connection stays usable. |
| `Binding` | The arguments do not fit the placeholders. Nothing was sent. |
| `TypeMismatch` | A cell was read as a type it cannot convert to, or was NULL. |
| `InvalidDsn` | The DSN or connection string cannot be honoured. |
| `Transport` | The request never became an answer. The connection is retired. |
| `Timeout` | The request outlived its timeout. The connection is retired. |
| `Protocol` | What answered is not a Frostlake engine, or its answer cannot be read. |
| `SessionLost` | The session went away with state the statement depended on. |
| `ConnectionClosed` | The connection is closed or retired. |
| `Usage` | A call made in the wrong state, such as `Commit` with no transaction. |

`Statement` holds the SQL the engine was sent. Because binding is client-side it has every argument
inlined — a bound password appears in it verbatim — so log `Message` freely and treat `Statement`
as sensitive.

## ADO.NET and Dapper

```fsharp
open Dapper

// Dapper maps columns onto settable properties, hence [<CLIMutable>].
[<CLIMutable>]
type PersonRow = { Id: int64; Name: string }

use connection = new FrostlakeConnection("frostlake://localhost:18082/MY_DB/PUBLIC")
let everyone = connection.Query<PersonRow>("SELECT ID, NAME FROM PEOPLE WHERE ID > @id", {| id = 0 |})
```

- `?` takes the unnamed parameters in order; `@name` and `:name` take the named ones.
- The reader settles each column's CLR type once per result set from the values it holds, so
  `GetFieldType` agrees with `GetValue` on every row, which `DataTable.Load` and Dapper rely on. An
  integral column reads as `long`, and widens to `decimal`, then to its exact text, only when a value
  in it does not fit. `DATE` and `TIMESTAMP_NTZ` read as `DateTime`, `TIME` as `TimeSpan`, the zoned
  timestamps as `DateTimeOffset`, and `GetFieldValue` also answers `DateOnly`, `TimeOnly`,
  `BigInteger`, `Guid` and `SqlValue`.
- `ExecuteNonQuery` answers the rows changed, or -1 when no DML ran; `ExecuteScalar` answers the first
  cell of the first result set — `DBNull` for a NULL, null when there is no row; `NextResult` walks a
  script's result sets.
- `CommandTimeout` defaults to the connection string's timeout, and `Int32.MaxValue` means no limit.
  `Cancel` stops a statement in flight and, since its outcome is then unknown, retires the
  connection: its state becomes `Broken`, and it has to be closed before it can open again.
- `connection.Session` is the `Connection` underneath, for the F# API.
- For `DbProviderFactories`, register the instance:
  `DbProviderFactories.RegisterFactory("Frostlake.FSharp", FrostlakeProviderFactory.Instance)`.

## Known limitations

- **Temporal values keep 100 ns.** The wire carries up to nine fractional digits, but .NET's
  `DateTime`, `DateTimeOffset` and `TimeOnly` stop at seven, so the last two are dropped on read.
- **Engine 0.0.7 sends less than the types hold.** It sends `TIME` in whole seconds and timestamps
  to the millisecond, and a `TIMESTAMP_TZ` arrives with its wall clock but its offset replaced by
  `+0000`. A `FILTER` or `TRANSFORM` result holding an undefined element makes its whole answer
  invalid JSON; the driver reports that as the engine's defect (`Protocol`) rather than a wrong
  address, but cannot read the value.
- **Keep-alive stalls against a server without TCP no-delay.** A kept-alive connection then waits on
  a delayed ACK for every response: against a stock 0.0.7 server a statement takes about 45 ms,
  against one started with `-Dsun.net.httpserver.nodelay=true` about 0.3 ms. Engines from 0.1.0 turn
  no-delay on themselves.
- **Two routes move a session unseen**: `EXECUTE IMMEDIATE` of a `USE`, and a procedure that changes
  scope when `CALL`ed. The driver cannot know such a session moved, so name the scope in the DSN when
  it matters.
- **DSN names are single identifiers.** `schema=DB.S` names one schema called `DB.S`, not a qualified
  path.
- **An empty statement is refused by the endpoint itself on engines before 0.1.0**, with `SQL is
  required`. From 0.1.0 it runs and fails as the account fails it, `Empty SQL statement.`
- **A request carries one statement unless the session asked for more.** `ALTER SESSION SET
  MULTI_STATEMENT_COUNT = n` allows exactly `n`, `= 0` any number; a session at the default of one
  refuses a request that carries more, and a session at `n` equally refuses one that carries fewer.
  The driver's own scoping request declares its count, so a DSN scope is unaffected.
- **A multi-statement request that fails part-way** reports the error only, with no results from the
  statements before it.

## Tests

```sh
dotnet test
```

The unit tests need nothing installed — no engine, no JVM, no network. They cover what the driver
decides on its own: DSN parsing, which characters are markers, what each value renders as, how a
cell is typed, the ADO.NET reader, and — over a scripted transport — how a connection keeps its
session, recovers from losing it, retires itself, and runs a transaction.

The engine-backed tests need a server and report as **skipped**, never as passed, without one.
`FROSTLAKE_CLASSPATH` (the engine jar and its dependencies, with `JAVA_HOME` choosing the JDK)
boots one on a free port; `FROSTLAKE_URL` uses one that is already running:

```sh
FROSTLAKE_CLASSPATH="/path/to/frostlake-db-<version>.jar:<its dependencies>" dotnet test
FROSTLAKE_URL=frostlake://localhost:18082 dotnet test
```

## License

Apache-2.0 — see [LICENSE](https://github.com/Frostlake-DB/frostlake-fsharp/blob/master/LICENSE).
