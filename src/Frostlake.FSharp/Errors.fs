namespace Frostlake.FSharp

open System.Data.Common

/// The family a failure belongs to, which is what decides what a caller can do about it.
[<RequireQualifiedAccess>]
type ErrorKind =
    /// The engine ran the statement and refused it: a SQL error. The connection stays usable.
    | Refused
    /// The DSN or connection string could not be read.
    | InvalidDsn
    /// The arguments do not fit the statement's placeholders, or a value has no SQL literal form.
    | Binding
    /// A cell was read as a type it cannot be converted to, or was NULL where a value was required.
    | TypeMismatch
    /// The request never became an answer: the host refused, the socket broke, TLS failed. The
    /// statement's fate is unknown, so the connection is retired rather than reused.
    | Transport
    /// The request outlived its timeout. Its fate is unknown, so the connection is retired.
    | Timeout
    /// Something answered that is not a Frostlake engine, or its answer could not be read.
    | Protocol
    /// The engine no longer holds the connection's session, and the statement depended on state
    /// that went with it.
    | SessionLost
    /// The connection is closed, or was retired after a failure the driver cannot vouch for.
    | ConnectionClosed
    /// An API call made in a state that does not allow it.
    | Usage

/// Every failure the driver raises. It derives from DbException, so code written against ADO.NET
/// catches it too.
type FrostlakeException(kind: ErrorKind, message: string, statement: string option, innerException: exn) =
    inherit DbException(message, innerException)

    new(kind: ErrorKind, message: string) = FrostlakeException(kind, message, None, null)

    new(kind: ErrorKind, message: string, statement: string option) =
        FrostlakeException(kind, message, statement, null)

    /// Which family the failure belongs to.
    member _.Kind = kind

    /// The SQL the engine was sent, parameters inlined. Binding happens client-side, so a bound
    /// password or card number appears here verbatim: log Message freely, and treat this as
    /// sensitive.
    member _.Statement = statement

module internal Fail =
    let usage (message: string) : 'T =
        raise (FrostlakeException(ErrorKind.Usage, message))

    let binding (message: string) : 'T =
        raise (FrostlakeException(ErrorKind.Binding, message))

    let dsn (message: string) : 'T =
        raise (FrostlakeException(ErrorKind.InvalidDsn, message))

    let mismatch (message: string) : 'T =
        raise (FrostlakeException(ErrorKind.TypeMismatch, message))
