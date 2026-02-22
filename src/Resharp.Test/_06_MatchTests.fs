[<Xunit.Collection("Sequential")>]
module Resharp.Test._06_MatchTests

#if DEBUG

open System.IO
open Resharp
open Resharp.Types
open Xunit
open Common

let private parseToml (path: string) =
    use fs = File.OpenRead path
    use sr = new StreamReader(fs)
    let tb = Tommy.TOML.Parse(sr)
    tb["test"].Children |> Seq.toArray

let getTestSample (name: string) =
    (__SOURCE_DIRECTORY__ + $"/../../data/tests/{name}")

let private runTomlTests name =
    for test in parseToml (getTestSample name) do
        let pattern = test["pattern"].AsString.Value
        let input = test["input"].AsString.Value
        let matchArr = test["matches"].AsArray

        let expected = [|
            for m in matchArr do
                let marr = m.AsArray
                if isNull marr then failwith "matches in toml invalid, expected int array"
                int marr[0].AsInteger.Value, int marr[1].AsInteger.Value
        |]

        try
            use matches = Resharp.Regex(pattern).ValueMatches(input)

            if expected.Length <> matches.Length then
                let arr =
                    matches.ToArray()
                    |> Seq.map (fun v -> [ v.Index; v.Index + v.Length ])
                    |> Seq.toList

                failwith $"expected: %A{expected} got:%A{arr}"

            for i = 0 to matches.Length - 1 do
                let item = matches.pool[i]
                Common.assertEqual (expected[i]) (item.Index, item.Index + item.Length)
        with e ->
            failwith $"failed: {pattern}, {e.Message}"

[<Fact>]
let tests01 () = runTomlTests "tests01.toml"

[<Fact>]
let tests02_lookaround () = runTomlTests "tests02_lookaround.toml"

[<Fact>]
let tests03_boolean () = runTomlTests "tests03_boolean.toml"

[<Fact>]
let tests04_anchors () = runTomlTests "tests04_anchors.toml"

[<Fact>]
let tests05_match_end () =
    for test in parseToml (getTestSample "tests05_match_end.toml") do
        let pattern = test["pattern"].AsString.Value
        let input = test["input"].AsString.Value
        let endPositions = [| for v in test["end_positions"].AsArray do int v.AsInteger.Value |]

        try
            let matches = getAllLLmatches pattern input

            for endPos in endPositions do
                if matches |> Seq.exists (fun v -> v.Index + v.Length = endPos) |> not then
                    failwith $"no match ending at {endPos}"
        with e ->
            failwith $"failed: {pattern}, {e.Message}"

[<Fact>]
let tests06_nullable_positions () =
    for test in parseToml (getTestSample "tests06_nullable_positions.toml") do
        let pattern = test["pattern"].AsString.Value
        let input = test["input"].AsString.Value
        let positions = [| for v in test["nullable_positions"].AsArray do int v.AsInteger.Value |]

        try
            assertNullablePositions pattern input (Array.toList positions)
        with e ->
            failwith $"failed: {pattern}, {e.Message}"

[<Fact>]
let tests07_unsupported () =
    for test in parseToml (getTestSample "tests07_unsupported.toml") do
        let pattern = test["pattern"].AsString.Value
        let input = test["input"].AsString.Value

        try
            let regex = Regex(pattern)
            let matcher = regex.TSetMatcher
            use _result = matcher.llmatch_all input
            failwith $"expected unsupported: {pattern}"
        with
        | :? UnsupportedPatternException -> ()
        | e -> failwith $"failed: {pattern}, {e.Message}"

[<Fact>]
let tests08_semantics () = runTomlTests "tests08_semantics.toml"

#endif
