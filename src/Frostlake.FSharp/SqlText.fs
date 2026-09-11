namespace Frostlake.FSharp

open System
open System.Collections.Generic

/// One bind site in a statement: the characters [Start, End), and the name of a :name or @name
/// marker, or "" for a positional ?.
[<Struct>]
type internal Marker = { Start: int; End: int; Name: string }

/// What a statement does to the session's transaction.
[<RequireQualifiedAccess>]
type internal TransactionEffect =
    | Begins
    | Ends
    | NoEffect

/// Lexical analysis of SQL text: where the literals, comments and bind markers are, and where one
/// statement ends and the next begins. Binding and session tracking both read this one scanner, so
/// they cannot disagree about what is code and what is quoted.
module internal SqlText =
    /// A character that may appear in an unquoted identifier. `$` is one, which is why `A$$B` is a
    /// name rather than the start of a dollar-quoted body.
    let isWordChar (c: char) = c = '_' || c = '$' || Char.IsLetterOrDigit c

    /// Index just past the single-quoted literal starting at `start`. A doubled quote and a
    /// backslash escape both stay inside it: backslash always escapes in the engine's dialect.
    let skipString (sql: string) (start: int) : int =
        let mutable j = start + 1
        let mutable past = -1
        while past < 0 && j < sql.Length do
            match sql.[j] with
            | '\\' -> j <- j + 2
            | '\'' when j + 1 < sql.Length && sql.[j + 1] = '\'' -> j <- j + 2
            | '\'' -> past <- j + 1
            | _ -> j <- j + 1
        if past < 0 then sql.Length else past

    /// Index just past a run delimited by `quote`, where a doubled delimiter escapes itself — the
    /// way a quoted identifier ("a""b") is written.
    let skipQuoted (sql: string) (start: int) (quote: char) : int =
        let mutable j = start + 1
        let mutable past = -1
        while past < 0 && j < sql.Length do
            if sql.[j] = quote then
                if j + 1 < sql.Length && sql.[j + 1] = quote then j <- j + 2 else past <- j + 1
            else
                j <- j + 1
        if past < 0 then sql.Length else past

    /// Whether the `$` at `i` opens a $$…$$ body rather than sitting inside an identifier.
    let opensDollarQuote (sql: string) (i: int) =
        i + 1 < sql.Length
        && sql.[i] = '$'
        && sql.[i + 1] = '$'
        && (i = 0 || not (isWordChar sql.[i - 1]))

    let skipDollarQuoted (sql: string) (start: int) =
        let j = sql.IndexOf("$$", start + 2, StringComparison.Ordinal)
        if j < 0 then sql.Length else j + 2

    let skipLine (sql: string) (start: int) =
        let j = sql.IndexOf('\n', start)
        if j < 0 then sql.Length else j + 1

    /// An unterminated comment swallows the rest of the input, as it does on the server.
    let skipBlockComment (sql: string) (start: int) =
        let j = sql.IndexOf("*/", start + 2, StringComparison.Ordinal)
        if j < 0 then sql.Length else j + 2

    /// Index just past the non-code construct starting at `i` — a literal, a quoted identifier, a
    /// $$ body, a comment — or -1 when `i` is code. Every scanner below defers to this, so a
    /// construct added here teaches all of them at once.
    let skipNonCode (sql: string) (i: int) : int =
        let next = if i + 1 < sql.Length then sql.[i + 1] else '\000'
        match sql.[i] with
        | '\'' -> skipString sql i
        | '"' -> skipQuoted sql i '"'
        | '$' when opensDollarQuote sql i -> skipDollarQuoted sql i
        | '-' when next = '-' -> skipLine sql i
        | '/' when next = '/' -> skipLine sql i
        | '/' when next = '*' -> skipBlockComment sql i
        | _ -> -1

    /// A character that ends an expression. A colon glued to one is VARIANT path access
    /// (`v:field`, `PARSE_JSON('…'):k`, `"V":k`), and an at-sign glued to one is not a marker either.
    let private endsExpression (c: char) =
        isWordChar c || c = ')' || c = ']' || c = '}' || c = '"' || c = '\''

    /// The bind sites of a statement, in order.
    ///
    /// `?` is positional. `:name` and `@name` are named markers when they follow an operator, a
    /// comma, a keyword boundary or the start of the text; `::` (a cast), `:=` (an assignment) and
    /// `:1` (a server-side positional reference) never are.
    let markers (sql: string) : Marker list =
        let found = List<Marker>()
        let mutable i = 0
        while i < sql.Length do
            let past = skipNonCode sql i
            if past >= 0 then
                i <- past
            else
                match sql.[i] with
                | '?' ->
                    found.Add { Start = i; End = i + 1; Name = "" }
                    i <- i + 1
                | ':' when i + 1 < sql.Length && (sql.[i + 1] = ':' || sql.[i + 1] = '=') -> i <- i + 2
                | ':'
                | '@' when i > 0 && endsExpression sql.[i - 1] -> i <- i + 1
                | ':'
                | '@' ->
                    let mutable j = i + 1
                    while j < sql.Length && isWordChar sql.[j] do
                        j <- j + 1
                    if j > i + 1 && not (Char.IsDigit sql.[i + 1]) && sql.[i + 1] <> '$' then
                        found.Add { Start = i; End = j; Name = sql.Substring(i + 1, j - i - 1) }
                    i <- max j (i + 1)
                | _ -> i <- i + 1
        List.ofSeq found

    /// The request split on its top-level semicolons; one inside a literal, a quoted identifier, a
    /// $$ body or a comment does not split. Blank pieces are dropped.
    ///
    /// A scripting block is split along with everything else, which only makes the session checks
    /// below more willing to flag a request — the safe direction to be wrong in.
    let statements (sql: string) : string list =
        let pieces = List<string>()
        let mutable start = 0
        let mutable i = 0
        while i < sql.Length do
            let past = skipNonCode sql i
            if past >= 0 then
                i <- past
            elif sql.[i] = ';' then
                pieces.Add(sql.Substring(start, i - start))
                start <- i + 1
                i <- i + 1
            else
                i <- i + 1
        pieces.Add(sql.Substring(start))
        [ for piece in pieces do
              if not (String.IsNullOrWhiteSpace piece) then
                  piece ]

    /// Up to `limit` leading words of a statement, upper-cased, skipping whitespace and comments and
    /// stopping at the first thing that is not a word.
    let leadingWords (statement: string) (limit: int) : string list =
        let words = List<string>()
        let mutable i = 0
        let mutable stop = false
        while not stop && words.Count < limit && i < statement.Length do
            let c = statement.[i]
            let next = if i + 1 < statement.Length then statement.[i + 1] else '\000'
            if Char.IsWhiteSpace c then
                i <- i + 1
            elif (c = '-' && next = '-') || (c = '/' && next = '/') then
                i <- skipLine statement i
            elif c = '/' && next = '*' then
                i <- skipBlockComment statement i
            elif isWordChar c then
                let start = i
                while i < statement.Length && isWordChar statement.[i] do
                    i <- i + 1
                words.Add(statement.Substring(start, i - start).ToUpperInvariant())
            else
                stop <- true
        List.ofSeq words

    /// The words that may sit between CREATE/DROP/ALTER and the kind of object being named.
    let private modifiers =
        HashSet<string>(
            [ "OR"
              "REPLACE"
              "TRANSIENT"
              "TEMPORARY"
              "TEMP"
              "VOLATILE"
              "LOCAL"
              "GLOBAL"
              "SECURE"
              "IF"
              "NOT"
              "EXISTS"
              "PUBLIC"
              "PRIVATE"
              "ICEBERG"
              "DYNAMIC"
              "HYBRID"
              "EVENT"
              "RECURSIVE"
              "MATERIALIZED"
              "EXTERNAL" ]
        )

    let private temporary = HashSet<string>([ "TEMPORARY"; "TEMP"; "VOLATILE" ])

    /// Whether a statement leaves behind state a fresh session would not have: a moved scope (USE,
    /// CREATE or DROP of a DATABASE or SCHEMA), a session variable or setting (SET, UNSET, ALTER
    /// SESSION), or a temporary object. CREATE TABLE and its kind leave the session as it was.
    let touchesSession (statement: string) : bool =
        match leadingWords statement 16 with
        | ("USE" | "SET" | "UNSET") :: _ -> true
        | "ALTER" :: rest ->
            match List.skipWhile modifiers.Contains rest with
            | "SESSION" :: _ -> true
            | _ -> false
        | verb :: rest when verb = "CREATE" || verb = "DROP" ->
            match List.skipWhile modifiers.Contains rest with
            | ("DATABASE" | "SCHEMA") :: _ -> true
            | _ -> verb = "CREATE" && List.exists temporary.Contains (List.takeWhile modifiers.Contains rest)
        | _ -> false

    /// Whether a statement opens or ends a transaction. `BEGIN` on its own (or with TRANSACTION,
    /// WORK or NAME) opens one; `BEGIN` followed by a statement opens a scripting block instead.
    let transactionEffect (statement: string) : TransactionEffect =
        match leadingWords statement 2 with
        | [ "BEGIN" ]
        | [ "BEGIN"; ("TRANSACTION" | "WORK" | "NAME") ]
        | [ "START"; "TRANSACTION" ] -> TransactionEffect.Begins
        | ("COMMIT" | "ROLLBACK") :: _ -> TransactionEffect.Ends
        | _ -> TransactionEffect.NoEffect

    let private isPlainIdentifier (name: string) =
        name.Length > 0
        && (Char.IsAsciiLetter name.[0] || name.[0] = '_')
        && Seq.forall (fun c -> Char.IsAsciiLetterOrDigit c || c = '_' || c = '$') name

    let private isQuotedIdentifier (name: string) =
        name.Length >= 2
        && name.[0] = '"'
        && name.[name.Length - 1] = '"'
        && name.Substring(1, name.Length - 2).Replace("\"\"", "").IndexOf('"') < 0

    /// A name from a DSN written as an identifier. A plain name goes bare, so it folds to upper case
    /// the way it would in SQL (`my_db` selects MY_DB); a name already in double quotes goes as
    /// given; anything else is quoted with its own quotes doubled, which also keeps a name from
    /// breaking out of the statement.
    let identifier (name: string) : string =
        if isPlainIdentifier name || isQuotedIdentifier name then
            name
        else
            "\"" + name.Replace("\"", "\"\"") + "\""
