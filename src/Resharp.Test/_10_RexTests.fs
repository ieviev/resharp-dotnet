module Resharp.Test._10_RexTests
open Common
open System.Globalization
open Resharp
open Xunit
open System.IO
open System.Text.Json

#if DEBUG

let escapePat (str: string) =
    System.Text.RegularExpressions.Regex.Replace(str, @"(?<!\\)[&~_]|(?<=\\\\)[&~_]", @"\$0")

let assertRexIsMatch (pattern: string) (input: string) : unit =
    let opts =
        ResharpOptions(UseDotnetUnicode = true, MinimizePattern = false)

    let eng = Resharp.Regex(pattern, opts)
    assertTrue (eng.IsMatch(input)) ""


let readGzipFile (path: string) =
    use fileStream = File.OpenRead(path)

    use cstream =
        new Compression.GZipStream(fileStream, Compression.CompressionMode.Decompress)

    use outputStream = new MemoryStream()
    cstream.CopyTo(outputStream)
    cstream.Flush()
    outputStream.ToArray()

let testPositiveMatchesInFile (filePath: string) =
    let bytes = readGzipFile filePath
    let samples = JsonSerializer.Deserialize<(string * string[])[]> bytes
    System.Globalization.CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture
    let failedSamples = ResizeArray()
    for pattern, inputs in samples do
        let escapedPattern = escapePat pattern
        let rsOptions = ResharpOptions.SingleUseDefaults
        let matcher =
            try
                ValueSome(Regex(escapedPattern, rsOptions))
            with e ->
                ValueNone
        match matcher with
        | ValueNone -> () // unsupported pattern; skip
        | ValueSome matcher ->
            for input in inputs do
                let result = matcher.IsMatch input

                if not result then
                    failedSamples.Add $"pat:{escapedPattern}; input:{input}"

    if failedSamples.Count > 0 then
        failwith (failedSamples |> String.concat "\n")


[<Fact(Skip = "takes very long; run for fuzzing")>]
let ``rex runtime 01`` () =
    __SOURCE_DIRECTORY__ + "/data/rexmatch1.json" |> testPositiveMatchesInFile

[<Fact>]
let test1 () =
    let pattern =
        @"[A-z-[dDfFiIoOqQuUwWzZ]]\d[A-z-[dDfFiIoOqQuU]] *\d[A-z-[dDfFiIoOqQuU]]\d\b"

    let input = "x߈x໔G᧔"
    assertIsMatchO ResharpOptions.SingleUseDefaults pattern input


#endif
