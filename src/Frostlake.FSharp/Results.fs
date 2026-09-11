namespace Frostlake.FSharp

open System
open System.Collections.Generic
open System.Globalization
open System.Numerics
open System.Text

/// A result column, as the engine describes it.
type Column =
    {
        /// The engine's spelling: an unquoted name upper-case, a quoted one as written.
        Name: string
        /// The engine's type name: NUMBER, VARCHAR, TIMESTAMP_TZ, VECTOR(INT, 3), …
        DataType: string
        /// Precision of a NUMBER column; 0 for every other type.
        Precision: int
        /// Scale of a NUMBER column; 0 for every other type.
        Scale: int
        /// Some true when the column is known to accept NULL, Some false when it is known not to,
        /// None from a server that predates the field.
        Nullable: bool option
    }

/// How a column's cells are read, decided by its declared type.
[<RequireQualifiedAccess>]
type internal ColumnKind =
    | Text
    | Integral
    | Decimal
    | Floating
    | Boolean
    | Binary
    | Variant
    | Date
    | Time
    | Timestamp
    | TimestampTz

module internal ColumnKinds =
    /// The type name without its (p,s) suffix, upper-cased.
    let baseName (dataType: string) : string =
        if String.IsNullOrEmpty dataType then
            ""
        else
            let trimmed = dataType.Trim()
            let paren = trimmed.IndexOf('(')
            (if paren >= 0 then trimmed.Substring(0, paren) else trimmed).Trim().ToUpperInvariant()

    /// The scale of an inline NUMBER(p,s) spelling, for a server that puts the pair in the name.
    let private inlineScale (dataType: string) : int =
        if String.IsNullOrEmpty dataType then
            0
        else
            let openAt = dataType.IndexOf('(')
            let comma = dataType.IndexOf(',')
            let closeAt = dataType.IndexOf(')')
            if openAt >= 0 && comma > openAt && closeAt > comma then
                match
                    Int32.TryParse(
                        dataType.Substring(comma + 1, closeAt - comma - 1).Trim(),
                        NumberStyles.Integer,
                        CultureInfo.InvariantCulture
                    )
                with
                | true, scale -> scale
                | _ -> 0
            else
                0

    let classify (column: Column) : ColumnKind =
        match baseName column.DataType with
        | "NUMBER"
        | "NUMERIC"
        | "DECIMAL"
        | "DEC"
        | "FIXED" ->
            let scale = if column.Scale <> 0 then column.Scale else inlineScale column.DataType
            if scale = 0 then ColumnKind.Integral else ColumnKind.Decimal
        | "INT"
        | "INTEGER"
        | "BIGINT"
        | "SMALLINT"
        | "TINYINT"
        | "BYTEINT" -> ColumnKind.Integral
        | "FLOAT"
        | "FLOAT4"
        | "FLOAT8"
        | "DOUBLE"
        | "DOUBLE PRECISION"
        | "REAL" -> ColumnKind.Floating
        | "BOOLEAN"
        | "BOOL" -> ColumnKind.Boolean
        | "BINARY"
        | "VARBINARY" -> ColumnKind.Binary
        | "VARIANT"
        | "OBJECT"
        | "ARRAY"
        | "MAP"
        | "GEOGRAPHY"
        | "GEOMETRY"
        | "VECTOR" -> ColumnKind.Variant
        | "DATE" -> ColumnKind.Date
        | "TIME" -> ColumnKind.Time
        | "TIMESTAMP"
        | "TIMESTAMP_NTZ"
        | "TIMESTAMPNTZ"
        | "DATETIME" -> ColumnKind.Timestamp
        | "TIMESTAMP_LTZ"
        | "TIMESTAMPLTZ"
        | "TIMESTAMP_TZ"
        | "TIMESTAMPTZ" -> ColumnKind.TimestampTz
        | _ -> ColumnKind.Text

/// The engine's temporal text: DATE `yyyy-MM-dd`, TIME `HH:mm:ss[.f…]`, a timestamp
/// `yyyy-MM-dd HH:mm:ss[.f…]` followed by ` ±HHMM` when it carries an offset. A `T` separator, a
/// `±HH:MM` or `Z` offset and a missing seconds field are accepted too. The wire carries up to nine
/// fractional digits; .NET's clock stops at seven (100 ns), so the last two are dropped.
module internal Temporal =
    /// The value of `count` digits at `i`, or -1.
    let private digitsAt (s: string) (i: int) (count: int) : int =
        if i < 0 || i + count > s.Length then
            -1
        else
            let mutable value = 0
            let mutable ok = true
            for k in i .. i + count - 1 do
                let c = s.[k]
                if c >= '0' && c <= '9' then
                    value <- value * 10 + (int c - int '0')
                else
                    ok <- false
            if ok then value else -1

    let private dateAt (s: string) (i: int) : (int * int * int) option =
        if i + 10 > s.Length || s.[i + 4] <> '-' || s.[i + 7] <> '-' then
            None
        else
            let year = digitsAt s i 4
            let month = digitsAt s (i + 5) 2
            let day = digitsAt s (i + 8) 2
            if year < 1 || month < 1 || month > 12 || day < 1 || day > DateTime.DaysInMonth(year, month) then
                None
            else
                Some(year, month, day)

    /// HH:mm[:ss[.fraction]] at `i`: the time of day in ticks and the index just past it.
    let private timeAt (s: string) (i: int) : (int64 * int) option =
        let hour = digitsAt s i 2
        let minute = digitsAt s (i + 3) 2
        if hour < 0 || minute < 0 || s.[i + 2] <> ':' then
            None
        else
            let mutable j = i + 5
            let mutable second = 0
            let mutable fraction = 0L
            let mutable ok = true
            if j < s.Length && s.[j] = ':' then
                second <- digitsAt s (j + 1) 2
                j <- j + 3
                if second < 0 then
                    ok <- false
                elif j < s.Length && s.[j] = '.' then
                    j <- j + 1
                    let start = j
                    let mutable kept = 0
                    while j < s.Length && Char.IsAsciiDigit s.[j] do
                        if kept < 7 then
                            fraction <- fraction * 10L + int64 (int s.[j] - int '0')
                            kept <- kept + 1
                        j <- j + 1
                    if j = start then
                        ok <- false
                    else
                        for _ in kept .. 6 do
                            fraction <- fraction * 10L
            if not ok || hour > 23 || minute > 59 || second > 59 then
                None
            else
                let ticks =
                    int64 hour * TimeSpan.TicksPerHour
                    + int64 minute * TimeSpan.TicksPerMinute
                    + int64 second * TimeSpan.TicksPerSecond
                    + fraction
                Some(ticks, j)

    /// A trailing offset — ±HHMM, ±HH:MM, ±HH or Z — in minutes east of UTC.
    let private offsetOf (text: string) : int option =
        let rest = text.Trim()
        if rest = "Z" || rest = "z" then
            Some 0
        elif rest.Length < 3 || (rest.[0] <> '+' && rest.[0] <> '-') then
            None
        else
            let sign = if rest.[0] = '-' then -1 else 1
            let body = rest.Substring(1).Replace(":", "")
            let hours, minutes =
                match body.Length with
                | 2 -> digitsAt body 0 2, 0
                | 4 -> digitsAt body 0 2, digitsAt body 2 2
                | _ -> -1, -1
            if hours < 0 || minutes < 0 || minutes > 59 then
                None
            else
                Some(sign * (hours * 60 + minutes))

    let parseDate (text: string) : DateOnly option =
        let s = text.Trim()
        if s.Length <> 10 then
            None
        else
            dateAt s 0 |> Option.map DateOnly

    let parseTime (text: string) : TimeOnly option =
        let s = text.Trim()
        if s.Length < 5 then
            None
        else
            match timeAt s 0 with
            | Some(ticks, past) when past = s.Length -> Some(TimeOnly(ticks))
            | _ -> None

    /// A timestamp's wall clock, and its offset in minutes when the text carries one.
    let parseTimestamp (text: string) : (DateTime * int option) option =
        let s = text.Trim()
        match (if s.Length >= 10 then dateAt s 0 else None) with
        | None -> None
        | Some(year, month, day) ->
            let date = DateTime(year, month, day, 0, 0, 0, DateTimeKind.Unspecified)
            if s.Length = 10 then
                Some(date, None)
            elif (s.[10] = ' ' || s.[10] = 'T') && s.Length >= 16 then
                match timeAt s 11 with
                | None -> None
                | Some(ticks, past) ->
                    let wall = date.AddTicks ticks
                    if past = s.Length then
                        Some(wall, None)
                    else
                        match offsetOf (s.Substring past) with
                        // DateTimeOffset holds ±14:00; anything wider stays text rather than wrong.
                        | Some minutes when abs minutes <= 14 * 60 -> Some(wall, Some minutes)
                        | _ -> None
            else
                None

    /// The instant a wall clock and an offset name, when DateTimeOffset can hold it: its UTC
    /// instant has to fall in years 1 to 9999.
    let tryOffset (wall: DateTime) (minutes: int) : DateTimeOffset option =
        let utcTicks = wall.Ticks - int64 minutes * TimeSpan.TicksPerMinute
        if utcTicks < DateTime.MinValue.Ticks || utcTicks > DateTime.MaxValue.Ticks then
            None
        else
            Some(DateTimeOffset(wall, TimeSpan.FromMinutes(float minutes)))

/// Conversions between cells and .NET values, shared by the typed row readers and the ADO.NET reader.
module internal Cells =
    let private invariant = CultureInfo.InvariantCulture
    let private ten = BigInteger(10)
    let private int64Min = BigInteger(Int64.MinValue)
    let private int64Max = BigInteger(Int64.MaxValue)
    let private decimalMax = BigInteger(Decimal.MaxValue)

    let fitsInt64 (value: BigInteger) = value >= int64Min && value <= int64Max

    let fitsDecimal (value: BigInteger) = BigInteger.Abs value <= decimalMax

    /// A number's text as (unscaled, scale), exactly: "1.50" is (150, 2) and "1E+3" is (1, -3).
    let parseExact (text: string) : (BigInteger * int) option =
        let s = if isNull text then "" else text.Trim()
        let digits = StringBuilder(s.Length)
        let mutable i = if s.Length > 0 && (s.[0] = '-' || s.[0] = '+') then 1 else 0
        let negative = s.Length > 0 && s.[0] = '-'
        let mutable scale = 0
        let mutable seenPoint = false
        let mutable seenDigit = false
        let mutable ok = true
        while ok && i < s.Length && s.[i] <> 'e' && s.[i] <> 'E' do
            let c = s.[i]
            if Char.IsAsciiDigit c then
                digits.Append(c) |> ignore
                seenDigit <- true
                if seenPoint then
                    scale <- scale + 1
            elif c = '.' && not seenPoint then
                seenPoint <- true
            else
                ok <- false
            i <- i + 1
        if ok && seenDigit && i < s.Length then
            match Int32.TryParse(s.Substring(i + 1), NumberStyles.AllowLeadingSign, invariant) with
            // No column holds a value a thousand places either side of the point, and powers of ten
            // that large take seconds to compute.
            | true, exponent when exponent >= -1000 && exponent <= 1000 -> scale <- scale - exponent
            | _ -> ok <- false
        if not ok || not seenDigit || scale > 1000 || scale < -1000 then
            None
        else
            let unscaled = BigInteger.Parse(digits.ToString(), invariant)
            Some((if negative then -unscaled else unscaled), scale)

    /// The integer a (unscaled, scale) pair names, when it has no fractional part.
    let integralOf (unscaled: BigInteger, scale: int) : BigInteger option =
        if scale <= 0 then
            Some(unscaled * BigInteger.Pow(ten, -scale))
        else
            let quotient, remainder = BigInteger.DivRem(unscaled, BigInteger.Pow(ten, scale))
            if remainder.IsZero then Some quotient else None

    /// The decimal a (unscaled, scale) pair names, keeping its scale, when decimal can hold it.
    let decimalOf (unscaled: BigInteger, scale: int) : decimal option =
        let mutable u = unscaled
        let mutable s = scale
        if s < 0 then
            u <- u * BigInteger.Pow(ten, -s)
            s <- 0
        while s > 28 && (BigInteger.Remainder(u, ten)).IsZero do
            u <- BigInteger.Divide(u, ten)
            s <- s - 1
        if s > 28 || BigInteger.Abs u > decimalMax then
            None
        else
            let magnitude = (BigInteger.Abs u).ToByteArray(true, false)
            let bits = Array.zeroCreate<byte> 12
            Array.blit magnitude 0 bits 0 (min magnitude.Length 12)
            Some(
                Decimal(
                    BitConverter.ToInt32(bits, 0),
                    BitConverter.ToInt32(bits, 4),
                    BitConverter.ToInt32(bits, 8),
                    u.Sign < 0,
                    byte s
                )
            )

    /// A decimal from text, exactly or not at all. Short plain numerals take the fast path.
    let tryDecimal (text: string) : decimal option =
        if not (isNull text) && text.Length <= 20 && text.IndexOfAny([| 'e'; 'E' |]) < 0 then
            match Decimal.TryParse(text, NumberStyles.AllowLeadingSign ||| NumberStyles.AllowDecimalPoint, invariant) with
            | true, value -> Some value
            | _ -> None
        else
            parseExact text |> Option.bind decimalOf

    let tryHex (text: string) : byte[] option =
        if text.Length % 2 <> 0 then
            None
        else
            try
                Some(Convert.FromHexString text)
            with :? FormatException ->
                None

    let describe (value: SqlValue) : string =
        match value with
        | SqlValue.Null -> "NULL"
        | SqlValue.Bool _ -> "a BOOLEAN"
        | SqlValue.Int _
        | SqlValue.BigInt _ -> "an integer"
        | SqlValue.Decimal _
        | SqlValue.DecimalText _ -> "a decimal number"
        | SqlValue.Float _ -> "a float"
        | SqlValue.Text _ -> "text"
        | SqlValue.Binary _ -> "BINARY"
        | SqlValue.Date _ -> "a DATE"
        | SqlValue.Time _ -> "a TIME"
        | SqlValue.Timestamp _ -> "a TIMESTAMP_NTZ"
        | SqlValue.TimestampTz _ -> "a zoned timestamp"
        | SqlValue.Variant _ -> "a VARIANT"
        | SqlValue.Raw _ -> "raw SQL"

    let private mismatch (column: string) (value: SqlValue) (wanted: string) : 'T =
        Fail.mismatch (sprintf "column %s holds %s, which cannot be read as %s" column (describe value) wanted)

    /// A value's text: text as itself, everything else in the invariant spelling the engine uses.
    let toText (_: string) (value: SqlValue) : string =
        match value with
        | SqlValue.Null -> null
        | SqlValue.Bool v -> if v then "true" else "false"
        | SqlValue.Int v -> v.ToString(invariant)
        | SqlValue.BigInt v -> v.ToString(invariant)
        | SqlValue.Decimal v -> v.ToString(invariant)
        | SqlValue.Float v -> Literal.formatFloat v
        | SqlValue.Text v
        | SqlValue.Variant v
        | SqlValue.DecimalText v
        | SqlValue.Raw v -> v
        | SqlValue.Binary v -> Convert.ToHexString v
        | SqlValue.Date v -> Literal.formatDate v
        | SqlValue.Time v -> Literal.formatTime v
        | SqlValue.Timestamp v -> Literal.formatTimestamp v
        | SqlValue.TimestampTz v -> Literal.formatTimestampTz v

    let toBigInt (column: string) (value: SqlValue) : BigInteger =
        match value with
        | SqlValue.Int v -> BigInteger(v)
        | SqlValue.BigInt v -> v
        | SqlValue.Decimal v when Decimal.Truncate v = v -> BigInteger(v)
        | SqlValue.Float v when Math.Truncate v = v && not (Double.IsInfinity v) -> BigInteger(v)
        | SqlValue.DecimalText t
        | SqlValue.Text t ->
            match parseExact t |> Option.bind integralOf with
            | Some v -> v
            | None -> mismatch column value "an integer"
        | _ -> mismatch column value "an integer"

    let toInt64 (column: string) (value: SqlValue) : int64 =
        match value with
        | SqlValue.Int v -> v
        | _ ->
            let wide = toBigInt column value
            if fitsInt64 wide then int64 wide else mismatch column value "an int64 (it is out of range)"

    let toInt32 (column: string) (value: SqlValue) : int =
        let wide = toInt64 column value
        if wide >= int64 Int32.MinValue && wide <= int64 Int32.MaxValue then
            int wide
        else
            mismatch column value "an int (it is out of range)"

    let toDecimal (column: string) (value: SqlValue) : decimal =
        match value with
        | SqlValue.Decimal v -> v
        | SqlValue.Int v -> decimal v
        | SqlValue.BigInt v when fitsDecimal v -> decimal v
        | SqlValue.Float v when not (Double.IsNaN v || Double.IsInfinity v) && abs v < 7.9e28 -> decimal v
        | SqlValue.DecimalText t
        | SqlValue.Text t ->
            match tryDecimal t with
            | Some v -> v
            | None -> mismatch column value "a decimal (too many digits, or not a number; read it as a string)"
        | _ -> mismatch column value "a decimal"

    let toDouble (column: string) (value: SqlValue) : float =
        match value with
        | SqlValue.Float v -> v
        | SqlValue.Int v -> float v
        | SqlValue.BigInt v -> float v
        | SqlValue.Decimal v -> float v
        | SqlValue.DecimalText t
        | SqlValue.Text t ->
            match Double.TryParse(t, NumberStyles.Float, invariant) with
            | true, v -> v
            | _ -> mismatch column value "a float"
        | _ -> mismatch column value "a float"

    let toBool (column: string) (value: SqlValue) : bool =
        match value with
        | SqlValue.Bool v -> v
        | SqlValue.Text t when String.Equals(t, "true", StringComparison.OrdinalIgnoreCase) -> true
        | SqlValue.Text t when String.Equals(t, "false", StringComparison.OrdinalIgnoreCase) -> false
        | _ -> mismatch column value "a bool"

    let toBytes (column: string) (value: SqlValue) : byte[] =
        match value with
        | SqlValue.Binary v -> v
        | _ -> mismatch column value "bytes"

    let toDate (column: string) (value: SqlValue) : DateOnly =
        match value with
        | SqlValue.Date v -> v
        | SqlValue.Timestamp v -> DateOnly.FromDateTime v
        | SqlValue.Text t ->
            match Temporal.parseDate t with
            | Some v -> v
            | None -> mismatch column value "a DateOnly"
        | _ -> mismatch column value "a DateOnly"

    let toTime (column: string) (value: SqlValue) : TimeOnly =
        match value with
        | SqlValue.Time v -> v
        | SqlValue.Timestamp v -> TimeOnly.FromDateTime v
        | SqlValue.Text t ->
            match Temporal.parseTime t with
            | Some v -> v
            | None -> mismatch column value "a TimeOnly"
        | _ -> mismatch column value "a TimeOnly"

    let toTimestamp (column: string) (value: SqlValue) : DateTime =
        match value with
        | SqlValue.Timestamp v -> v
        | SqlValue.Date v -> v.ToDateTime(TimeOnly.MinValue)
        | SqlValue.Text t ->
            match Temporal.parseTimestamp t with
            | Some(wall, None) -> wall
            | _ -> mismatch column value "a DateTime"
        | _ -> mismatch column value "a DateTime (a zoned timestamp reads as DateTimeOffset)"

    let toTimestampTz (column: string) (value: SqlValue) : DateTimeOffset =
        match value with
        | SqlValue.TimestampTz v -> v
        | SqlValue.Text t ->
            match Temporal.parseTimestamp t with
            | Some(wall, Some minutes) ->
                match Temporal.tryOffset wall minutes with
                | Some instant -> instant
                | None -> mismatch column value "a DateTimeOffset (its instant is outside years 1 to 9999)"
            | _ -> mismatch column value "a DateTimeOffset"
        | _ -> mismatch column value "a DateTimeOffset (a TIMESTAMP_NTZ has no offset to give it)"

    let toGuid (column: string) (value: SqlValue) : Guid =
        match value with
        | SqlValue.Text t ->
            match Guid.TryParse t with
            | true, v -> v
            | _ -> mismatch column value "a Guid"
        | SqlValue.Binary b when b.Length = 16 -> Guid(b)
        | _ -> mismatch column value "a Guid"

/// Column lookup for every row of one result set: an exact name first, then one ignoring case.
/// With duplicate names the first column wins; the rest remain reachable by ordinal.
[<Sealed>]
type internal ColumnIndex(columns: Column[]) =
    let exact = Dictionary<string, int>(StringComparer.Ordinal)
    let folded = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)

    do
        for i in columns.Length - 1 .. -1 .. 0 do
            let name = columns.[i].Name
            if not (isNull name) then
                exact.[name] <- i
                folded.[name] <- i

    member _.Columns = columns

    member _.TryFind(name: string) : int =
        if isNull name then
            -1
        else
            match exact.TryGetValue name with
            | true, i -> i
            | _ ->
                match folded.TryGetValue name with
                | true, i -> i
                | _ -> -1

    member this.Find(name: string) : int =
        let i = this.TryFind name
        if i < 0 then
            let names = columns |> Array.map (fun c -> c.Name) |> String.concat ", "
            Fail.usage (sprintf "no column named %s; the result has %s" name names)
        else
            i

/// One row of a result set. A column is found by name — an exact match first, then one ignoring
/// case — and read either as a SqlValue or through the typed readers, which convert where the
/// conversion is exact and raise a TypeMismatch FrostlakeException where it is not. A typed reader
/// raises on NULL; its OrNone twin answers None instead.
[<Sealed>]
type Row internal (index: ColumnIndex, values: SqlValue[]) =
    member private this.Read(name: string, reader: string, convert: string -> SqlValue -> 'T) : 'T =
        match this.[name] with
        | SqlValue.Null -> Fail.mismatch (sprintf "column %s is NULL; read it with %sOrNone" name reader)
        | value -> convert name value

    member private this.ReadOrNone(name: string, convert: string -> SqlValue -> 'T) : 'T option =
        match this.[name] with
        | SqlValue.Null -> None
        | value -> Some(convert name value)

    /// The columns of the result set this row belongs to.
    member _.Columns: Column[] = index.Columns

    /// The row's values, in column order.
    member _.Values: SqlValue[] = values

    /// The value at an ordinal.
    member _.Item
        with get (ordinal: int): SqlValue =
            if ordinal < 0 || ordinal >= values.Length then
                Fail.usage (sprintf "no column at ordinal %d; the row has %d" ordinal values.Length)
            else
                values.[ordinal]

    /// The value of a named column.
    member _.Item
        with get (name: string): SqlValue = values.[index.Find name]

    /// The value of a named column, or None when the result has no such column.
    member _.TryGet(name: string) : SqlValue option =
        let i = index.TryFind name
        if i < 0 || i >= values.Length then None else Some values.[i]

    member this.value(name: string) : SqlValue = this.[name]

    member this.isNull(name: string) : bool =
        match this.[name] with
        | SqlValue.Null -> true
        | _ -> false

    member this.int(name: string) : int = this.Read(name, "int", Cells.toInt32)
    member this.intOrNone(name: string) : int option = this.ReadOrNone(name, Cells.toInt32)
    member this.int64(name: string) : int64 = this.Read(name, "int64", Cells.toInt64)
    member this.int64OrNone(name: string) : int64 option = this.ReadOrNone(name, Cells.toInt64)
    member this.bigint(name: string) : BigInteger = this.Read(name, "bigint", Cells.toBigInt)
    member this.bigintOrNone(name: string) : BigInteger option = this.ReadOrNone(name, Cells.toBigInt)
    member this.decimal(name: string) : decimal = this.Read(name, "decimal", Cells.toDecimal)
    member this.decimalOrNone(name: string) : decimal option = this.ReadOrNone(name, Cells.toDecimal)
    member this.double(name: string) : float = this.Read(name, "double", Cells.toDouble)
    member this.doubleOrNone(name: string) : float option = this.ReadOrNone(name, Cells.toDouble)
    member this.float(name: string) : float = this.Read(name, "float", Cells.toDouble)
    member this.floatOrNone(name: string) : float option = this.ReadOrNone(name, Cells.toDouble)
    member this.string(name: string) : string = this.Read(name, "string", Cells.toText)
    member this.stringOrNone(name: string) : string option = this.ReadOrNone(name, Cells.toText)
    member this.text(name: string) : string = this.Read(name, "text", Cells.toText)
    member this.textOrNone(name: string) : string option = this.ReadOrNone(name, Cells.toText)
    member this.bool(name: string) : bool = this.Read(name, "bool", Cells.toBool)
    member this.boolOrNone(name: string) : bool option = this.ReadOrNone(name, Cells.toBool)
    member this.bytes(name: string) : byte[] = this.Read(name, "bytes", Cells.toBytes)
    member this.bytesOrNone(name: string) : byte[] option = this.ReadOrNone(name, Cells.toBytes)
    member this.date(name: string) : DateOnly = this.Read(name, "date", Cells.toDate)
    member this.dateOrNone(name: string) : DateOnly option = this.ReadOrNone(name, Cells.toDate)
    member this.time(name: string) : TimeOnly = this.Read(name, "time", Cells.toTime)
    member this.timeOrNone(name: string) : TimeOnly option = this.ReadOrNone(name, Cells.toTime)
    member this.timestamp(name: string) : DateTime = this.Read(name, "timestamp", Cells.toTimestamp)
    member this.timestampOrNone(name: string) : DateTime option = this.ReadOrNone(name, Cells.toTimestamp)
    member this.timestamptz(name: string) : DateTimeOffset = this.Read(name, "timestamptz", Cells.toTimestampTz)

    member this.timestamptzOrNone(name: string) : DateTimeOffset option =
        this.ReadOrNone(name, Cells.toTimestampTz)

    member this.uuid(name: string) : Guid = this.Read(name, "uuid", Cells.toGuid)
    member this.uuidOrNone(name: string) : Guid option = this.ReadOrNone(name, Cells.toGuid)
    /// A VARIANT, OBJECT or ARRAY as the text the engine sent (JSON for a container).
    member this.variant(name: string) : string = this.Read(name, "variant", Cells.toText)
    member this.variantOrNone(name: string) : string option = this.ReadOrNone(name, Cells.toText)

/// One statement's result: a grid of rows, and for a DML statement the number of rows it changed.
[<Sealed>]
type ResultSet internal (columns: Column[], rows: Row[], updateCount: int64 option) =
    member _.Columns: Column[] = columns
    member _.Rows: Row[] = rows
    member _.RowCount = rows.Length

    /// Some n when the result is a DML statement's count grid (INSERT, UPDATE, DELETE, MERGE),
    /// giving the rows it changed; None for a query, a DDL status line or SHOW.
    member _.UpdateCount: int64 option = updateCount

/// Everything one request answered: a result set per statement it ran.
[<Sealed>]
type QueryResult internal (resultSets: ResultSet[], sessionId: string, executionTime: TimeSpan) =
    let final =
        if resultSets.Length = 0 then
            None
        else
            Some resultSets.[resultSets.Length - 1]

    /// One result set per statement, in order. An engine before 0.1.0 answers a DDL statement with
    /// no result set at all, where later ones answer a one-row status line.
    member _.ResultSets: ResultSet[] = resultSets

    /// The rows of the request's final result set — for a single statement, its only one. Final
    /// rather than first, so `CREATE …; INSERT …; SELECT …` answers the SELECT on every engine.
    member _.Rows: Row[] =
        match final with
        | Some set -> set.Rows
        | None -> [||]

    /// The columns of the final result set.
    member _.Columns: Column[] =
        match final with
        | Some set -> set.Columns
        | None -> [||]

    /// The rows every DML statement in the request changed, added up; 0 when none ran.
    member _.RowsAffected: int64 =
        let mutable total = 0L
        for set in resultSets do
            match set.UpdateCount with
            | Some count -> total <- total + count
            | None -> ()
        total

    /// The first cell of the final result set, or None when it has no row.
    member this.TryScalar: SqlValue option =
        let rows = this.Rows
        if rows.Length = 0 || rows.[0].Values.Length = 0 then
            None
        else
            Some rows.[0].Values.[0]

    /// The first cell of the final result set; raises when it has no row.
    member this.Scalar: SqlValue =
        match this.TryScalar with
        | Some value -> value
        | None -> Fail.usage "the statement returned no row to read a scalar from"

    /// The engine session the request ran in.
    member _.SessionId = sessionId

    /// How long the engine spent on the request, as it reports.
    member _.ExecutionTime = executionTime
