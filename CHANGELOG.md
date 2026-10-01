# Changelog

## 0.2.0

- A text or binary column reports the length it was declared with: `Column.Length`, in characters
  for text and in bytes for binary, and `ColumnSize` in the ADO.NET reader's schema table. Every
  other type reports none.
- Requires a Frostlake engine 0.2.0 or newer.

## 0.1.0

First release.

- The F# API: `Connection`, the `Sql` pipeline, the `SqlValue` union for parameters and cells, and
  typed row readers.
- An ADO.NET provider — `FrostlakeConnection`, `FrostlakeCommand`, `FrostlakeParameter`,
  `FrostlakeDataReader`, `FrostlakeTransaction`, `FrostlakeProviderFactory` — that Dapper runs on.
- An optional per-call `multiStatementCount` — `connection.Execute(sql, multiStatementCount = 2)`
  and `Sql.multiStatementCount` — says how many statements one request carries, without moving the
  session's `MULTI_STATEMENT_COUNT`.
- Sessions are resumed with `requireSession`, replaced when one is lost with nothing depending on
  it, and released when the connection closes.
