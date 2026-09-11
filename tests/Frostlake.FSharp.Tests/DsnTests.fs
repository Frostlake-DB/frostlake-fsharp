module Frostlake.FSharp.Tests.DsnTests

open System
open Frostlake.FSharp
open Frostlake.FSharp.Tests.TestSupport
open Xunit

[<Fact>]
let ``a frostlake DSN speaks HTTP to its host and port`` () =
    let config = Dsn.parse "frostlake://db.example:19000"
    Assert.Equal(Uri "http://db.example:19000", config.BaseUri)
    Assert.True(config.Database.IsNone)
    Assert.True(config.Schema.IsNone)

[<Fact>]
let ``the port defaults to 18082`` () =
    Assert.Equal(18082, (Dsn.parse "frostlake://localhost").BaseUri.Port)

[<Fact>]
let ``the path names the database and the schema`` () =
    let config = Dsn.parse "frostlake://localhost:18082/MY_DB/PUBLIC"
    Assert.Equal(Some "MY_DB", config.Database)
    Assert.Equal(Some "PUBLIC", config.Schema)

[<Fact>]
let ``parameters set the scope and the timeouts`` () =
    let config =
        Dsn.parse "frostlake://localhost:18082/DB?schema=S&role=R&warehouse=W&timeout=30s&connectTimeout=1500ms"
    Assert.Equal(Some "DB", config.Database)
    Assert.Equal(Some "S", config.Schema)
    Assert.Equal(Some "R", config.Role)
    Assert.Equal(Some "W", config.Warehouse)
    Assert.Equal(Some(TimeSpan.FromSeconds 30.0), config.Timeout)
    Assert.Equal(TimeSpan.FromMilliseconds 1500.0, config.ConnectTimeout)

[<Theory>]
[<InlineData("45", 45000.0)>]
[<InlineData("5m", 300000.0)>]
[<InlineData("2m30s", 150000.0)>]
[<InlineData("1h", 3600000.0)>]
[<InlineData("250ms", 250.0)>]
let ``timeouts read as durations`` (text: string, milliseconds: float) =
    Assert.Equal(Some(TimeSpan.FromMilliseconds milliseconds), (Dsn.parse ("frostlake://h?timeout=" + text)).Timeout)

[<Fact>]
let ``a zero timeout waits indefinitely and the default is five minutes`` () =
    Assert.True((Dsn.parse "frostlake://h?timeout=0").Timeout.IsNone)
    Assert.Equal(Some(TimeSpan.FromMinutes 5.0), (Dsn.parse "frostlake://h").Timeout)

[<Fact>]
let ``tls and the https scheme speak HTTPS`` () =
    Assert.Equal("https", (Dsn.parse "frostlake://h:1?tls=true").BaseUri.Scheme)
    Assert.Equal("https", (Dsn.parse "https://h:1").BaseUri.Scheme)
    Assert.Equal("http", (Dsn.parse "https://h:1?tls=false").BaseUri.Scheme)

[<Fact>]
let ``an IPv6 host keeps its brackets`` () =
    Assert.Equal("[::1]", (Dsn.parse "frostlake://[::1]:18082").BaseUri.Host)

[<Fact>]
let ``percent-escaped names are decoded`` () =
    Assert.Equal(Some "\"My Db\"", (Dsn.parse "frostlake://h/%22My%20Db%22").Database)

[<Theory>]
[<InlineData("frostlake://user:secret@h:1")>]
[<InlineData("frostlake://h:1?password=x")>]
[<InlineData("frostlake://h:1?colour=blue")>]
[<InlineData("frostlake://h:1?timeout=soon")>]
[<InlineData("frostlake://h:1?timeout=5d")>]
[<InlineData("frostlake://h:1/DB?database=OTHER")>]
[<InlineData("frostlake://h:1/DB/S?schema=T")>]
[<InlineData("frostlake://h:1/DB/S/extra")>]
[<InlineData("mysql://h:1")>]
[<InlineData("")>]
[<InlineData("Host=h;Password=x")>]
[<InlineData("Host=h;Port=99999")>]
[<InlineData("Host=h;Flavour=vanilla")>]
[<InlineData("Host=my host")>]
[<InlineData("Server=h:1;Port=2")>]
[<InlineData("frostlake://h:1?timeout=10000000000000")>]
[<InlineData("frostlake://h:1?timeout=1200h")>]
[<InlineData("frostlake://h:1?connectTimeout=1000h")>]
let ``what cannot be honoured is refused`` (text: string) =
    raisesKind ErrorKind.InvalidDsn (fun () -> Dsn.parse text) |> ignore

[<Fact>]
let ``a connection string reads like a DSN`` () =
    let config =
        Dsn.parse "Host=db.example;Port=19000;Database=MY_DB;Schema=PUBLIC;Role=R;Warehouse=W;Timeout=30;Tls=true"
    Assert.Equal(Uri "https://db.example:19000", config.BaseUri)
    Assert.Equal(Some "MY_DB", config.Database)
    Assert.Equal(Some "PUBLIC", config.Schema)
    Assert.Equal(Some "R", config.Role)
    Assert.Equal(Some "W", config.Warehouse)
    Assert.Equal(Some(TimeSpan.FromSeconds 30.0), config.Timeout)

[<Fact>]
let ``a connection string defaults to localhost on 18082`` () =
    Assert.Equal(Uri "http://localhost:18082", (Dsn.parse "Database=D").BaseUri)

[<Fact>]
let ``tryParse answers with the reason`` () =
    match Dsn.tryParse "frostlake://h?nope=1" with
    | Error message -> Assert.Contains("nope", message)
    | Ok _ -> failwith "expected an error"

[<Fact>]
let ``a host written with its port reads as both`` () =
    Assert.Equal(Uri "http://db.example:19000", (Dsn.parse "Server=db.example:19000;Database=D").BaseUri)
    Assert.Equal(Uri "http://[::1]:19000", (Dsn.parse "Host=[::1]:19000").BaseUri)
    Assert.Equal(Uri "http://[::1]:18082", (Dsn.parse "Host=::1").BaseUri)
    Assert.Equal(Uri "http://db.example:19000", (Dsn.parse "Server=db.example:19000;Port=19000").BaseUri)

[<Fact>]
let ``tryParse answers every refusal as an error`` () =
    for text in [ "Host=my host"; "frostlake://h?timeout=10000000000000"; "frostlake://h?timeout=1200h" ] do
        match Dsn.tryParse text with
        | Error _ -> ()
        | Ok _ -> failwithf "expected %s to be refused" text
