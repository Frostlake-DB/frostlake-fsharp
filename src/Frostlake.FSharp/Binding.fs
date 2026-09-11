namespace Frostlake.FSharp

open System
open System.Collections.Generic
open System.Text

/// Client-side parameter binding. The protocol carries no bind values, so arguments are inlined
/// into the statement as SQL literals before it is sent — as Frostlake's JDBC driver does.
module internal Binding =
    /// A parameter's name without the `:` or `@` it may have been written with.
    let bare (name: string) =
        if isNull name then
            ""
        elif name.StartsWith(":", StringComparison.Ordinal) || name.StartsWith("@", StringComparison.Ordinal) then
            name.Substring(1)
        else
            name

    /// Inline positional (`?`) and named (`:name`, `@name`) arguments into `sql`.
    ///
    /// With no arguments at all the text passes through untouched, because its `?` and `:name`
    /// marks then belong to the server: a scripting cursor's `OPEN … USING`, a scripting variable.
    /// Positional arguments must match the `?` count exactly. A named marker binds when an argument
    /// of that name (case-insensitive) was given and is otherwise left for the server — a stage
    /// reference such as `@my_stage` is written the same way. `strict` also refuses a named argument
    /// that no marker used, which is almost always a misspelt name.
    let bind (sql: string) (positional: SqlValue[]) (named: (string * SqlValue)[]) (strict: bool) : string =
        if positional.Length = 0 && named.Length = 0 then
            sql
        else
            let byName = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
            for index in 0 .. named.Length - 1 do
                let key = bare (fst named.[index])
                if key.Length = 0 then
                    Fail.binding "a named argument has an empty name"
                if byName.ContainsKey key then
                    Fail.binding (sprintf "argument :%s is given more than once" key)
                byName.[key] <- index
            let used = Array.zeroCreate<bool> named.Length
            let output = StringBuilder(sql.Length + 16 * (positional.Length + named.Length))
            let mutable cursor = 0
            let mutable next = 0
            for marker in SqlText.markers sql do
                if marker.Name.Length = 0 then
                    if positional.Length = 0 then
                        Fail.binding "the statement has ? placeholders, but only named arguments were given"
                    if next >= positional.Length then
                        Fail.binding (
                            sprintf
                                "the statement has more ? placeholders than the %d positional argument(s) given"
                                positional.Length
                        )
                    output.Append(sql, cursor, marker.Start - cursor).Append(Literal.render positional.[next]) |> ignore
                    next <- next + 1
                    cursor <- marker.End
                else
                    match byName.TryGetValue marker.Name with
                    | true, index ->
                        output.Append(sql, cursor, marker.Start - cursor).Append(Literal.render (snd named.[index]))
                        |> ignore
                        used.[index] <- true
                        cursor <- marker.End
                    | _ -> ()
            output.Append(sql, cursor, sql.Length - cursor) |> ignore
            if next < positional.Length then
                Fail.binding (
                    sprintf
                        "%d positional argument(s) given, but the statement has %d ? placeholder(s)"
                        positional.Length
                        next
                )
            if strict then
                for index in 0 .. named.Length - 1 do
                    if not used.[index] then
                        Fail.binding (sprintf "argument :%s does not appear in the statement" (bare (fst named.[index])))
            output.ToString()
