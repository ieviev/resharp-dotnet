module internal Resharp.Test.Common

#if DEBUG

open System
open System.Text.RegularExpressions

open Resharp
open Resharp.Common
open Resharp.RegexNodeConverter
open Resharp.Types
open Resharp.Runtime
open Xunit

[<AutoOpen>]
module DebugExtensions =
    type Resharp.Regex with

        member this.TSetMatcher = this.Matcher :?> RegexMatcher<uint64>

        member this.CreateNonInitialDerivative(mt, v) =
            let m = this.Matcher :?> RegexMatcher<uint64>
            m.CreateDerivative(LocationKind.Center, mt, v)


let inline currCharF (pos: int) (input: string) = input[pos]

let inline currCharRev (pos: int) (input: string) =
    let mutable pos = pos - 1
    input[pos]

let private deriveAt (reg: Regex) (input: string) (raw: bool) (pos: int) =
    let matcher = reg.TSetMatcher
    let cache = matcher.Cache
    let node = if raw then matcher.RawPattern else matcher.TrueStarredPattern
    matcher.CreateDerivative(LocationKind.Begin, cache.Classify(currCharF pos input), node)

let der1 (reg: Regex) (input: string) (raw: bool) =
    deriveAt reg input raw 0 |> reg.TSetMatcher.PrettyPrintNode

let der1RPos (reg: Regex) (input: string) (raw: bool) (pos: int) =
    deriveAt reg input raw pos |> reg.TSetMatcher.PrettyPrintNode

let der1Node (reg: Regex) (input: string) (raw: bool) =
    deriveAt reg input raw 0



let private deriveRev (reg: Regex) (input: string) (node: RegexNodeId) =
    let matcher = reg.TSetMatcher
    let cache = matcher.Cache
    matcher.CreateDerivative(LocationKind.End, cache.Classify(currCharRev input.Length input), node)

let der1Rev (reg: Regex) (input: string) =
    deriveRev reg input reg.TSetMatcher.ReversePattern

let der1RevTS (reg: Regex) (input: string) =
    deriveRev reg input reg.TSetMatcher.ReverseTrueStarredPattern


let inline assertEqual (x1: 't) (x2: 't) = Assert.Equal<'t>(x1, x2)
let inline assertAllEqual (x1: seq<'t>) (x2: seq<'t>) = Assert.Equal<'t>(x1, x2)
let inline assertTrue (x1: _) (msg: string) = Assert.True(x1, msg)
let inline assertFalse (x1: _) (msg: string) = Assert.False(x1, msg)

let inline assertFlag (nf: NodeFlags) (msg: NodeFlags) =
    Assert.True(nf.HasFlag(msg), $"{msg}")

let inline assertNotFlag (nf: NodeFlags) (msg: NodeFlags) =
    Assert.False(nf.HasFlag(msg), $"{msg}")

let inline assertContains (items: 't seq) (data: 't) = Assert.Contains(data, items)


let assertAlternation (matcher: RegexMatcher<TSet>) (node: RegexNodeId) (expectedResults: string list) =
    match matcher.Cache.Builder.Node(node) with
    | Or(nodes = nodes) ->
        let nodestrs = nodes |> Seq.map (fun n -> matcher.PrettyPrintNode n) |> set

        for r in expectedResults do
            Assert.Contains(r, nodestrs)
    | _ ->
        let nodeStr = matcher.PrettyPrintNode node
        for r in expectedResults do
            Assert.Contains(r, nodeStr)


let internal getInitOptimizationsAndMatcher pattern =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let getder = regex.CreateNonInitialDerivative

    let optimizations =
        Optimizations.findInitialOptimizations
            regex.Options
            getder
            (fun node -> matcher.GetOrCreateState(node).Id)
            matcher.Cache
            matcher.ReversePattern
            matcher.ReverseTrueStarredPattern
            true

    optimizations, matcher

let internal getInitOptimizations pattern =
    getInitOptimizationsAndMatcher pattern |> fst

let assertPotentialStart (pattern: string) (expectedPatterns: string seq) =
    let optimizations, matcher = getInitOptimizationsAndMatcher pattern

    match optimizations with
    | Optimizations.InitialAccelerator.SearchValuesPotentialStart(prefix) ->
        let mtarr = prefix.ToArray() |> Array.map (fun v -> v.Minterm)
        let prefixString = Optimizations.printPrefixSets matcher.Cache (mtarr |> Seq.toList)
        Assert.Contains(prefixString, expectedPatterns)
    | _ -> failwith $"invalid optimization result: {optimizations}"

let assertSingleStart (pattern: string) (expectedPatterns: string seq) =
    let optimizations, matcher = getInitOptimizationsAndMatcher pattern

    match optimizations with
    | Optimizations.InitialAccelerator.SingleSearchValuesPrefix(prefix, _)
    | Optimizations.InitialAccelerator.SingleSearchValues(prefix) ->
        let mt = prefix.Minterm
        let prefixString = Optimizations.printPrefixSets matcher.Cache [mt]
        Assert.Contains(prefixString, expectedPatterns)
    | _ -> failwith $"invalid optimization result: {optimizations}"

let assertMinterms pattern expected =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher

    assertAllEqual
        (matcher.Cache.Minterms() |> Seq.map matcher.Cache.PrettyPrintMinterm)
        expected



let assertSetsPrefix pattern expected =
    let optimizations, matcher = getInitOptimizationsAndMatcher pattern

    match optimizations with
    | Optimizations.InitialAccelerator.SearchValuesPrefix(prefix, _) ->
        let prefixString =
            Optimizations.printPrefixSets
                matcher.Cache
                (prefix.ToArray() |> Seq.map (fun v -> v.Minterm) |> Seq.toList)

        Assert.Equal(expected, prefixString)
    | _ -> failwith $"invalid optimization result: {optimizations}"

let assertBvSetsPrefix pattern expected =
    let regex = Regex(pattern)
    let matcher = regex.Matcher :?> RegexMatcher<BitVector>
    let optimizations = matcher.InternalOptimizations

    match optimizations.RightToLeftInitial with
    | Optimizations.InitialAccelerator.SearchValuesPrefix(prefix, _) ->
        let prefixString =
            Optimizations.printPrefixSets2
                matcher.Cache
                (prefix.ToArray() |> Seq.map (fun v -> v.Minterm) |> Seq.toList)

        Assert.Equal(expected, prefixString)
    | Optimizations.InitialAccelerator.SearchValuesPotentialStart(prefix) ->
        let prefixString =
            Optimizations.printPrefixSets2
                matcher.Cache
                (prefix.ToArray() |> Seq.map (fun v -> v.Minterm) |> Seq.toList)

        Assert.Equal(expected, prefixString)
    | _ -> failwith $"invalid optimization result: {optimizations}"



let assertStringPrefix pattern expected =
    let optimizations, _ = getInitOptimizationsAndMatcher pattern

    match optimizations with
    | Optimizations.InitialAccelerator.StringPrefix(prefix, _) ->
        Assert.Equal(expected, prefix.ToString())
    | _ -> failwith $"invalid optimization result: {optimizations}"


let applyPrefix pattern =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let getder = (fun (mt, v) -> matcher.CreateDerivative(LocationKind.Center, mt, v))

    let prefix =
        Optimizations.calcPrefixSets getder matcher.Cache matcher.ReversePattern

    let applied =
        Optimizations.applyPrefixSets
            getder
            matcher.Cache
            matcher.ReverseTrueStarredPattern
            prefix

    matcher.PrettyPrintNode applied

let assertOptimizationPrefixSets pattern expected =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let getder = regex.CreateNonInitialDerivative

    let prefix =
        Optimizations.calcPrefixSets getder matcher.Cache matcher.ReversePattern

    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    assertEqual expected prefixString


let assertNullablePositions (pattern: string) (input: string) expected =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let mutable acc = new ValueList<int>(100)
    let mutable initState = matcher.RevStartStateId
    let flags = matcher.GetStateAndFlagsById(initState).Flags
    let startPos = matcher.HandleInputEnd(flags, &initState, input, &acc)
    let endStateId = matcher.collect_skip (&acc, input, startPos, initState)
    matcher.HandleInputStart(endStateId, &acc)
    Assert.Equal<int seq>(expected, (ValueList.toSpan acc).ToArray())

let getFirstLLmatch (pattern: string) (input: string) =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let llmatches = matcher.llmatch_all(input).ToArray()

    let firstmatch =
        if Array.isEmpty llmatches then
            failwith $"did not match!: {pattern}"

        llmatches[0]

    (firstmatch.Index, firstmatch.Index + firstmatch.Length)

let getFirstLLmatchText (pattern: string) (input: string) =
    let (ms, me) = getFirstLLmatch pattern input
    input.AsSpan().Slice(ms, me - ms).ToString()

let getAllLLmatches (pattern: string) (input: string) =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    let rs = ResizeArray()

    for r in matcher.llmatch_all (input) do
        rs.Add(r)

    rs

let getAllLLmatchesOpts (opts: ResharpOptions) (pattern: string) (input: string) =
    let regex = Regex(pattern, opts)
    let matcher = regex.TSetMatcher
    let rs = ResizeArray()

    for r in matcher.llmatch_all (input) do
        rs.Add(r)

    rs



let assertFirstMatch (pattern: string) (input: string) (expected) =
    let result = getFirstLLmatch pattern input
    Assert.Equal<int * int>(expected, result)

let assertFirstMatchText (pattern: string) (input: string) (expected) =
    let result = getFirstLLmatch pattern input

    Assert.Equal(
        expected,
        input.AsSpan().Slice(fst result, snd result - fst result).ToString()
    )

let assertNoMatch (pattern: string) (input: string) =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher
    use matches = matcher.llmatch_all (input)
    Assert.True(matches.size = 0, $"expected no match for '{pattern}' on '{input}'")


let assertUnsupported (pattern: string) (input: string) =
    try
        let regex = Regex(pattern)
        let matcher = regex.TSetMatcher
        use result = matcher.llmatch_all (input)
        failwith "pattern is supported"
    with
    | :? UnsupportedPatternException as _ -> ()
    | e -> failwith e.Message


let assertIsMatchO (opts: ResharpOptions) (pattern: string) (input: string) =
    let regex = Regex(pattern, opts)
    Assert.True(regex.IsMatch(input))

let assertIsMatch (pattern: string) (input: string) =
    let regex = Regex(pattern)
    Assert.True(regex.IsMatch(input))

let private assertMatchResults (results: ResizeArray<ValueMatch>) expected =
    let result = results |> Seq.map (fun v -> v.Index, v.Length)

    try
        Assert.Equal<int * int>(expected, result)
    with _ ->
        let real = result |> Seq.map string |> String.concat ";"
        failwith $"expected: %A{expected};\nactual:%A{real}"

let assertAllLLmatches (pattern: string) (input: string) expected =
    assertMatchResults (getAllLLmatches pattern input) expected

let assertAllLLmatchesO (o: ResharpOptions) (pattern: string) (input: string) expected =
    assertMatchResults (getAllLLmatchesOpts o pattern input) expected


let assertAllLLmatchTexts (pattern: string) (input: string) (expected) =
    let result =
        getAllLLmatches pattern input |> Seq.map _.GetText(input) |> Seq.toArray

    if result.Length = 0 then
        failwith "did not match!"

    Assert.Equal<string>(expected, result)

let assertMatchEnd (pattern: string) (input: string) (_: int) (expectedEndPos: int) =
    let matches = getAllLLmatches pattern input
    let r = matches |> Seq.tryFind (fun v -> v.Index + v.Length = expectedEndPos)

    if r.IsNone then
        failwith "not found"

let assertNodeOneOf (matcher: RegexMatcher<_>) (node: RegexNodeId) (options: string seq) =
    Assert.Contains(matcher.PrettyPrintNode node, options)


let assertRawDerivative (pattern: string) (input: string) (expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let result = der1 matcher input true
    Assert.Contains(result, expectedDerivatives)

let assertTSDerivative (pattern: string) (input: string) (expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let result = der1 matcher input false
    Assert.Contains(result, expectedDerivatives)



let assertConverted (pattern: string) (expected: string list) =
    let reg = Regex(pattern)
    let asstr = reg.TSetMatcher.PrettyPrintNode reg.TSetMatcher.RawPattern
    Assert.Contains<string>(asstr, expected)


#endif
