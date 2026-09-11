namespace Frostlake.FSharp

open System
open System.Globalization
open System.Numerics
open System.Text
open System.Text.RegularExpressions

/// A SQL value: what a parameter binds and what a result cell holds.
///
/// A result never produces Raw; a bind accepts every case.
[<RequireQualifiedAccess>]
type SqlValue =
    /// SQL NULL.
    | Null
    /// BOOLEAN.
    | Bool of bool
    /// An integral NUMBER that fits in 64 bits.
    | Int of int64
    /// An integral NUMBER too wide for 64 bits: NUMBER(38,0) holds 38 digits.
    | BigInt of BigInteger
    /// A NUMBER with a scale. Exact: it never passes through a float.
    | Decimal of decimal
    /// FLOAT / DOUBLE / REAL: a binary float, which is what those columns hold.
    | Float of float
    /// Text.
    | Text of string
    /// BINARY.
    | Binary of byte[]
    /// DATE.
    | Date of DateOnly
    /// TIME.
    | Time of TimeOnly
    /// TIMESTAMP_NTZ: a wall clock with no zone of its own. Read back with DateTimeKind.Unspecified;
    /// bound by its wall-clock digits whatever its Kind.
    | Timestamp of DateTime
    /// TIMESTAMP_TZ / TIMESTAMP_LTZ: a wall clock with its offset from UTC.
    | TimestampTz of DateTimeOffset
    /// VARIANT, OBJECT, ARRAY, VECTOR, GEOGRAPHY: the value's text as the engine sent it, which is
    /// JSON for a container. Binds as PARSE_JSON(text).
    | Variant of string
    /// A number as its exact digits: a scaled NUMBER too wide for decimal when read, or digits to
    /// bind verbatim.
    | DecimalText of string
    /// Binds only: SQL inlined exactly as written and never escaped, so never build one from
    /// untrusted text.
    | Raw of string

/// Rendering a value as the SQL literal that carries it to the engine.
module internal Literal =
    let private invariant = CultureInfo.InvariantCulture

    let private numeral =
        Regex(@"^[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?$", RegexOptions.CultureInvariant)

    /// A single-quoted literal. Backslashes are doubled because a backslash always escapes in the
    /// engine's string dialect, and quotes are doubled.
    let quote (text: string) : string =
        let builder = StringBuilder(text.Length + 2)
        builder.Append('\'') |> ignore
        for c in text do
            match c with
            | '\\' -> builder.Append("\\\\") |> ignore
            | '\'' -> builder.Append("''") |> ignore
            | other -> builder.Append(other) |> ignore
        builder.Append('\'').ToString()

    /// A negative numeral goes in parentheses: spliced after a minus it would otherwise open a
    /// comment, so `SELECT 3-?` bound -5 would become `SELECT 3--5`, which reads as `SELECT 3`.
    let private signed (text: string) =
        if text.StartsWith("-", StringComparison.Ordinal) then "(" + text + ")" else text

    let formatDate (value: DateOnly) = value.ToString("yyyy-MM-dd", invariant)

    let formatTime (value: TimeOnly) = value.ToString("HH:mm:ss.FFFFFFF", invariant)

    let formatTimestamp (value: DateTime) =
        value.ToString("yyyy-MM-dd HH:mm:ss.FFFFFFF", invariant)

    let formatTimestampTz (value: DateTimeOffset) =
        value.ToString("yyyy-MM-dd HH:mm:ss.FFFFFFF zzz", invariant)

    let formatFloat (value: float) = value.ToString("R", invariant)

    /// A float binds with a FLOAT cast: a bare 1.5 is a NUMBER(2,1) literal, so without it a bound
    /// double would take part in exact rather than floating-point arithmetic. The special values use
    /// the engine's own spellings; written bare they would read as identifiers.
    let private floatLiteral (value: float) =
        if Double.IsNaN value then "'NaN'::FLOAT"
        elif Double.IsPositiveInfinity value then "'Infinity'::FLOAT"
        elif Double.IsNegativeInfinity value then "'-Infinity'::FLOAT"
        else
            let text = formatFloat value
            if text.StartsWith("-", StringComparison.Ordinal) then "(" + text + "::FLOAT)" else text + "::FLOAT"

    let render (value: SqlValue) : string =
        match value with
        | SqlValue.Null -> "NULL"
        | SqlValue.Bool true -> "TRUE"
        | SqlValue.Bool false -> "FALSE"
        | SqlValue.Int v -> signed (v.ToString(invariant))
        | SqlValue.BigInt v -> signed (v.ToString(invariant))
        | SqlValue.Decimal v -> signed (v.ToString(invariant))
        | SqlValue.Float v -> floatLiteral v
        | SqlValue.Text null
        | SqlValue.Binary null
        | SqlValue.Variant null
        | SqlValue.DecimalText null
        | SqlValue.Raw null -> "NULL"
        | SqlValue.Text v -> quote v
        | SqlValue.Binary v -> "X'" + Convert.ToHexString(v) + "'"
        | SqlValue.Date v -> "'" + formatDate v + "'::DATE"
        | SqlValue.Time v -> "'" + formatTime v + "'::TIME"
        | SqlValue.Timestamp v -> "'" + formatTimestamp v + "'::TIMESTAMP_NTZ"
        | SqlValue.TimestampTz v -> "'" + formatTimestampTz v + "'::TIMESTAMP_TZ"
        | SqlValue.Variant v -> "PARSE_JSON(" + quote v + ")"
        | SqlValue.DecimalText v ->
            let digits = v.Trim()
            if not (numeral.IsMatch digits) then
                Fail.binding (
                    sprintf
                        "DecimalText %s is not a number; it is inlined verbatim, so only digits, a sign, a point and an exponent are accepted"
                        (quote v)
                )
            signed (digits.TrimStart('+'))
        | SqlValue.Raw v -> v

/// Functions over SqlValue.
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SqlValue =
    let private invariant = CultureInfo.InvariantCulture

    /// The SQL literal a value binds as: exactly the text inlined into a statement.
    let toSqlLiteral (value: SqlValue) : string = Literal.render value

    /// Convert a .NET value by its runtime type: integers to Int (or BigInt past 64 bits), decimal
    /// to Decimal, float32/float to Float, string/char/Guid to Text, DateTime to Timestamp,
    /// DateTimeOffset to TimestampTz, DateOnly/TimeOnly/TimeSpan to Date/Time, byte[] to Binary,
    /// an enum to its number, an option or voption to its content or Null, and null or DBNull to
    /// Null. A SqlValue passes through unchanged.
    let rec ofObj (value: obj) : SqlValue =
        match value with
        | null -> SqlValue.Null
        | :? SqlValue as v -> v
        | :? DBNull -> SqlValue.Null
        | :? bool as v -> SqlValue.Bool v
        | :? sbyte as v -> SqlValue.Int(int64 v)
        | :? byte as v -> SqlValue.Int(int64 v)
        | :? int16 as v -> SqlValue.Int(int64 v)
        | :? uint16 as v -> SqlValue.Int(int64 v)
        | :? int as v -> SqlValue.Int(int64 v)
        | :? uint32 as v -> SqlValue.Int(int64 v)
        | :? int64 as v -> SqlValue.Int v
        | :? uint64 as v ->
            if v <= uint64 Int64.MaxValue then
                SqlValue.Int(int64 v)
            else
                SqlValue.BigInt(BigInteger(v))
        | :? BigInteger as v -> SqlValue.BigInt v
        | :? decimal as v -> SqlValue.Decimal v
        // The shortest text a float32 prints as is the value its writer meant; widening the bits
        // instead would bind 1.1f as 1.100000023841858.
        | :? float32 as v -> SqlValue.Float(Double.Parse(v.ToString("R", invariant), invariant))
        | :? float as v -> SqlValue.Float v
        | :? string as v -> SqlValue.Text v
        | :? char as v -> SqlValue.Text(string v)
        | :? Guid as v -> SqlValue.Text(v.ToString())
        | :? DateTime as v -> SqlValue.Timestamp v
        | :? DateTimeOffset as v -> SqlValue.TimestampTz v
        | :? DateOnly as v -> SqlValue.Date v
        | :? TimeOnly as v -> SqlValue.Time v
        | :? TimeSpan as v ->
            if v >= TimeSpan.Zero && v < TimeSpan.FromDays 1.0 then
                SqlValue.Time(TimeOnly.FromTimeSpan v)
            else
                Fail.binding (
                    sprintf "a TimeSpan binds as TIME, which holds 00:00:00 to 23:59:59.9999999; %O is outside it" v
                )
        | :? (byte[]) as v -> SqlValue.Binary v
        | :? Enum as v -> ofObj (Convert.ChangeType(v, Enum.GetUnderlyingType(v.GetType()), invariant))
        | _ ->
            let t = value.GetType()
            let generic = if t.IsGenericType then t.GetGenericTypeDefinition() else null
            if generic = typedefof<option<_>> then
                ofObj (t.GetProperty("Value").GetValue(value))
            elif generic = typedefof<voption<_>> then
                if t.GetProperty("IsSome").GetValue(value) :?> bool then
                    ofObj (t.GetProperty("Value").GetValue(value))
                else
                    SqlValue.Null
            else
                Fail.binding (sprintf "a %s has no SQL literal form; convert it to one of the SqlValue cases" t.FullName)
