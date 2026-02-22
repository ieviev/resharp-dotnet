namespace rec Resharp.Cache

open System
open System.Buffers
open System.Collections.Generic
open System.Runtime.CompilerServices

open Microsoft.FSharp.Core
open Resharp
open Resharp.Types
open Resharp.Common
open Resharp.Runtime

type internal MintermSearchMode =
    | SearchValues = 1
    | InvertedSearchValues = 2

[<RequireQualifiedAccess>]
type internal SearchValuesKind =
    | Small
    | Range
    | Ascii
    | Other
    | Empty

    /// this is a maintenance nightmare, but
    /// only some SearchValues<'t> implementations are faster than RE# by itself.
    /// there is also no correct answer either since it depends on what the input is
    /// so we make some educated guesses
    static member ofSearchValues(sv: SearchValues<'t>) =
        if isNull sv then
            Empty
        else
            match sv.GetType().Name with
            | "EmptySearchValues`1" -> Other
            | "SingleCharSearchValues`1" -> Small
            | "Any1SearchValues`2" -> Small
            | "Any2SearchValues`2" -> Small
            | "Any1CharPackedIgnoreCaseSearchValues" -> Small
            | "Any2CharPackedIgnoreCaseSearchValues" -> Small
            | "Any1CharPackedSearchValues" -> Small
            | "Any2CharPackedSearchValues" -> Small
            | "Any3CharPackedSearchValues" -> Small
            | "Any3SearchValues`2" -> Small
            | "Any2CharSearchValues`1"
            | "Any3CharSearchValues`1" -> Small
            | "RangeCharSearchValues`1" -> Range
            | "AsciiCharSearchValues`1" -> Ascii
            | "AsciiCharSearchValues`2" -> Ascii
            | "Any4SearchValues`2" -> Other
            | "Any5SearchValues`2" -> Other
            | "ProbabilisticCharSearchValues" -> Other
            | "ProbabilisticWithAsciiCharSearchValues`1" -> Other
            | _ ->
                // failwith $"unknown SearchValues type: {sv.GetType().Name}"
                Other

    /// this is a simple way to determine how common a character set is, lower is rarer
    /// would get much better results with real probabilities
    static member CommonalityScoreSimple(charSet: ReadOnlySpan<char>) : float =
        let mutable total = 0.

        for c in charSet do
            total <-
                total
                + if Char.IsWhiteSpace c then 20.0
                  elif Char.IsAsciiLetterLower c then 20.0
                  elif Char.IsAsciiLetterUpper c then 10.0
                  elif Char.IsAsciiDigit c then 10.0
                  elif Char.IsAscii(c) then 10.0
                  else 5.0

        total

/// pre-computed optimizations for a symbolic predicate
[<NoComparison>]
type internal MintermSearchValues<'t> =
    val Mode: MintermSearchMode
    val SearchValues: SearchValues<char>
    val Minterm: 't
    val Commonality: float
#if DEBUG
    val CharactersInMinterm: Memory<char>
    val Classifier: MintermClassifier
    val SearchValuesSize: int
#endif

    static member inline contains(this: MintermSearchValues<'t>, b: char) =
        match this.Mode with
        | MintermSearchMode.SearchValues -> this.SearchValues.Contains(b)
        | MintermSearchMode.InvertedSearchValues -> not (this.SearchValues.Contains(b))
        | _ -> failwith $"invalid MintermSearchMode: {this.Mode}"

    static member inline nextIndexLeftToRight
        (msv: MintermSearchValues<'t>, slice: ReadOnlySpan<char>)
        =
        match msv.Mode with
        | MintermSearchMode.SearchValues -> slice.IndexOfAny(msv.SearchValues)
        | MintermSearchMode.InvertedSearchValues -> slice.IndexOfAnyExcept(msv.SearchValues)
        | _ -> failwith "impossible"

    static member inline nextIndexRightToLeft
        (msv: MintermSearchValues<'t>, slice: ReadOnlySpan<char>)
        =
        match msv.Mode with
        | MintermSearchMode.SearchValues -> slice.LastIndexOfAny(msv.SearchValues)
        | MintermSearchMode.InvertedSearchValues ->
            slice.LastIndexOfAnyExcept(msv.SearchValues)
        | _ -> failwith $"invalid MintermSearchMode: {msv.Mode}"



#if DEBUG
    override this.ToString() =
        let desc =
            match this.Mode with
            | MintermSearchMode.SearchValues -> $"%A{this.CharactersInMinterm}"
            | MintermSearchMode.InvertedSearchValues -> $"%A{this.CharactersInMinterm}"
            | _ -> ArgumentOutOfRangeException() |> raise

        $"{this.Mode.ToString()}: {desc}"
#endif
    new(tset: 't, characters: Memory<char>, invert: bool) =
        let mode =
            if invert then
                MintermSearchMode.InvertedSearchValues
            else
                MintermSearchMode.SearchValues

        let commonality = SearchValuesKind.CommonalityScoreSimple characters.Span

        {
            Mode = mode
            SearchValues = SearchValues.Create(characters.Span)
            Minterm = tset
            Commonality = commonality
#if DEBUG
            CharactersInMinterm = characters
            Classifier = Unchecked.defaultof<_>
            SearchValuesSize = characters.Length
#endif
        }



[<Sealed>]
type internal RegexCache<'t when TSet<'t> and 't: equality>
    (
        _solver: ISolver<'t>,
        _charsetSolver: CharSetSolver,
        _bddMinterms: BDD[],
        _rawPattern: RegexNodeId,
        _builder: RegexBuilder<'t>,
        _bddbuilder: RegexBuilder<BDD>
    ) =
    let classifier =
        if typeof<'t> = typeof<BitVector> then
            (box _solver :?> BitVectorSolver)._classifier
        elif typeof<'t> = typeof<uint64> then
            (box _solver :?> UInt64Solver)._classifier
        else
            failwith "invalid solver"

    let minterms: 't[] = _solver.GetMinterms()
    let mintermBdds = _bddMinterms
    let predStartsets = StartsetHelpers.startsetsFromMintermArray mintermBdds
    let static_merged_chars = Array.zeroCreate<char> (65536)
    let mutable _cachedStartsets: Dictionary<'t, MintermSearchValues<'t>> = Dictionary()

    let _getMintermStartsetChars (tset: 't) =
        match _cachedStartsets.TryGetValue(tset) with
        | true, v -> v
        | _ ->
            let charBuffer = Array.zeroCreate<char> (int StartsetHelpers.CHAR_LIMIT)

            let mintermCharsOpt =
                StartsetHelpers.tryGetMintermChars (
                    charBuffer,
                    _solver,
                    predStartsets,
                    minterms,
                    tset
                )

            let searchValues =
                let isInverted = _solver.elemOfSet (tset) (minterms[0])
                MintermSearchValues(tset, mintermCharsOpt, isInverted)

            _cachedStartsets.Add(tset, searchValues)
            searchValues




    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.Minterms() : 't array = minterms

    member _.NumOfMinterms() = minterms.Length


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.MtsBDD() = mintermBdds

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.MintermStartsets() = predStartsets

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member internal _.MintermSearchValues(startset: 't) = _getMintermStartsetChars startset

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.MintermChars(startset: 't) : Memory<char> =
        StartsetHelpers.tryGetMintermChars (
            static_merged_chars,
            _solver,
            predStartsets,
            minterms,
            startset
        )


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.MintermIsInverted(mt: 't) : _ = _solver.elemOfSet (mt) (minterms[0])

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.CharToMintermId(chr: char) : int = classifier.GetMintermID(int chr)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.MintermById(id: int) = minterms[id]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.MintermToId(tset: 't) : TMinterm =
        minterms |> Array.findIndex (fun v -> _solver.elemOfSet v tset) |> byte

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.Classify(c: char) =
        minterms[classifier.GetMintermID(int c)]

    member val InitialPatternWithoutDotstar = _rawPattern
    member val Solver: ISolver<'t> = _solver
    member val CharsetSolver: CharSetSolver = _charsetSolver
    member val internal Builder = _builder
    member val internal BddBuilder = _bddbuilder


#if DEBUG
    member cache.PrettyPrintMinterm(xs: _) : string =
        (box cache.Solver :?> UInt64Solver).PrettyPrint(xs, _charsetSolver)
#endif
