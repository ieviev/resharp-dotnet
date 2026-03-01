namespace Resharp

open System
open System.Buffers
open System.Diagnostics
open System.Globalization
open System.Runtime.CompilerServices
open System.Collections.Generic
open Microsoft.FSharp.NativeInterop
open Resharp.Runtime

[<AllowNullLiteral>]
[<Sealed>]
type ResharpOptions() =
    /// default: 2048, initial dfa size, can increase initial size to prevent dynamic array growth during match time
    member val InitialDfaCapacity = 2048 with get, set
    /// default: 100_000, maximum dfa size, symbolic DFA-s are generally very small,
    /// but lookarounds may create arbitrarily large state spaces depending on input length.
    /// this sets a hard limit to the number of states created and throws an exception if it is reached
    member val MaxDfaCapacity = 100_000 with get, set
    /// default: true, attempt to make smaller alternations at the cost of initialization time
    member val MinimizePattern = true with get, set
    /// default: 20, maximum string literal prefix length.
    /// generally does not make much difference
    member val MaxPrefixLength = 20 with get, set
    /// default: true, attempt to optimize lookaround prefixes.
    /// can be expensive with unbounded lookarounds
    member val FindLookaroundPrefix = true with get, set
    /// default: 200, increases search-time performance for large regexes at the expense of building the engine
    member val FindPotentialStartSizeLimit = 200 with get, set
    /// default: true, case insensitive k includes Kelvin symbol (K), case insensitive i includes turkish I (ı) etc..
    /// these cost a lot for string literal optimizations and are disabled in HighThroughput defaults
    member val UseDotnetUnicode = true with get, set

    member val IgnoreCase = false with get, set
    /// default:2000
    member val StartsetInferenceLimit = 2000 with get, set
    /// default:100, full dfa compilation state space threshold
    member val DfaThreshold = 100 with get, set

    /// attempt more expensive optimizations for high-throughput
    static member HighThroughputDefaults =
        ResharpOptions(
            FindPotentialStartSizeLimit = 1000,
            InitialDfaCapacity = 256,
            UseDotnetUnicode = false,
            DfaThreshold = 100
        )

    /// skip all expensive optimizations as it's likely going to be used and discarded
    static member SingleUseDefaults =
        ResharpOptions(
            FindPotentialStartSizeLimit = 0,
            MaxPrefixLength = 0,
            FindLookaroundPrefix = false,
            InitialDfaCapacity = 256,
            MaxDfaCapacity = 8192,
            StartsetInferenceLimit = 50,
            DfaThreshold = 0
        )


module internal BDD =

    let rec fromRanges (css: CharSetSolver) (ranges: ResizeArray<struct (char * char)>) =
        let mutable bdd = css.Empty

        for rs, re in ranges do
            bdd <- css.Or(bdd, css.CreateBDDFromRange(rs, re))

        bdd

    let prettyPrintBDD (css: CharSetSolver) (bdd: BDD) =
        let mutable remainingSet = bdd
        let mutable addedSets = ""
        let css = css
        let initial = css.PrettyPrint(remainingSet)
        let isInverted = initial.StartsWith("[^")

        let symbolsToEscape =
            String(
                [|
                    '('
                    ')'
                    '&'
                    '~'
                    '.'
                    '|'
                    '^'
                    '$'
                |]
            )

        match initial with
        | @"[^\n]" -> "."
        | @"." -> @"\."
        | c when symbolsToEscape.Contains(c) -> $@"{c}"
        | _ when initial.Length <= 20 -> initial
        | _ ->

            if isInverted then
                remainingSet <- css.Not(remainingSet)

            let containsSet(p1: BDD) =
                let cond4 = css.IsEmpty(css.And(css.Not(remainingSet), p1))

                cond4

            let removeSet(removedSet: BDD) =
                remainingSet <- css.And(remainingSet, css.Not(removedSet))

            let wordBdd = UnicodeCategoryConditions.WordLetter(css)

            let nonWordBdd = css.Not(wordBdd)

            let spaceBdd = UnicodeCategoryConditions.WhiteSpace

            let nonSpaceBdd = css.Not(spaceBdd)

            let digitBdd =
                UnicodeCategoryConditions.GetCategory(UnicodeCategory.DecimalDigitNumber)

            let nonDigitBdd = css.Not(digitBdd)

            if containsSet wordBdd then
                removeSet wordBdd
                addedSets <- addedSets + @"\w"

            if containsSet nonWordBdd then
                removeSet nonWordBdd
                addedSets <- addedSets + @"\W"

            if containsSet digitBdd then
                removeSet digitBdd
                addedSets <- addedSets + @"\d"

            if containsSet nonDigitBdd then
                removeSet nonDigitBdd
                addedSets <- addedSets + @"\D"

            if containsSet spaceBdd then
                removeSet spaceBdd
                addedSets <- addedSets + @"\s"

            if containsSet nonSpaceBdd then
                removeSet nonSpaceBdd
                addedSets <- addedSets + @"\S"

            let orig = css.PrettyPrint(remainingSet)
            let inv = if isInverted then "^" else ""
            let orig = orig.Replace("~", @"\~")

            let result =
                match orig with
                | "[]" ->
                    match addedSets with
                    | @"\w" when isInverted -> @"\W"
                    | @"\s" when isInverted -> @"\S"
                    | @"\d" when isInverted -> @"\D"
                    | _ when addedSets.Length = 2 -> addedSets
                    | _ -> $"[{inv}{addedSets}]"
                | orig when orig.StartsWith('[') -> $"[{inv}{addedSets}{orig[1..]}"
                | _ ->
                    if addedSets = "" then
                        if isInverted then $"[{inv}{orig}]" else orig
                    else
                        $"[{inv}{addedSets}{orig}]"

            result


module internal Memory =
    let inline forall ([<InlineIfLambda>] f) (mem: Memory<'t>) =
        let span = mem.Span
        let mutable e = span.GetEnumerator()
        let mutable forall = true

        while forall && e.MoveNext() do
            forall <- f e.Current

        forall


type TSet<'t when 't :> IEquatable<'t> and 't :> IComparable<'t> and 't: equality> = 't

module Common =

    /// this constrains the max num of unique minterms to 256 to conserve memory
    type TMinterm = byte
    type TState = int

    [<AutoOpen>]
    module internal Extensions =
        type ISolver<'t> with

            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member inline this.elemOfSet (predicate: 't) (locationMinterm: 't) =
                not (this.IsEmpty(this.And(locationMinterm, predicate)))

            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.notElemOfSet (predicate: 't) (locationMinterm: 't) =
                this.IsEmpty(this.And(locationMinterm, predicate))

            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.contains (larger: 't) (smaller: 't) =
                let overlapped = this.And(smaller, larger)

                match box overlapped, box smaller with
                | :? uint64 as ov, (:? uint64 as sm) -> ov = sm
                | :? BDD as ov, (:? BDD as sm) -> ov = sm
                | :? BitVector as ov, (:? BitVector as sm) -> ov = sm
                | _ -> failwith "invalid set"

            member this.convertToBdd(solver: CharSetSolver, minterms: BDD[], set: 't) =
                match box set with
                | :? uint64 as v ->
                    let partition = minterms
                    let mutable result = BDD.False

                    if v <> 0uL then
                        for i = 0 to partition.Length - 1 do
                            if (v &&& (uint64 1 <<< i)) <> 0uL then
                                result <- solver.Or(result, partition[i])

                    result
                | :? Resharp.Runtime.BitVector as v ->
                    let partition = minterms
                    let mutable result = BDD.False

                    if not (v.Equals(this.Empty)) then
                        for i = 0 to partition.Length - 1 do
                            if v[i] then
                                result <- solver.Or(result, partition[i])

                    result
                | _ -> failwith "invalid set"


    /// disposable memory-pooled collection
    [<Struct; IsByRefLike>]
    [<DebuggerDisplay("{pool}")>]
    type ValueList<'t when 't: equality and 't: struct> =
        val mutable size: int
        val mutable limit: int
        val mutable pool: 't array

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Add(item) =
            if this.size = this.limit then
                this.GrowTo(this.limit * 2)

            this.pool[this.size] <- item
            this.size <- this.size + 1

        static member inline add(vs: byref<ValueList<'t>>, item) =
            if vs.size = vs.limit then
                vs.GrowTo(vs.limit * 2)

            vs.pool[vs.size] <- item
            vs.size <- vs.size + 1

        static member inline addChecked(vs: byref<ValueList<'t>>, item: 't) =
            if vs.size = vs.limit then
                vs.GrowTo(vs.limit * 2)

            let curr = vs.pool[vs.size - 1]

            if not (item = curr) then
                vs.pool[vs.size] <- item
                vs.size <- vs.size + 1

        /// assumes non-empty list
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        static member inline endsWith(vs: ValueList<'t>, item: 't) =
            let curr = vs.pool[vs.size - 1]
            item = curr

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.AddSpan(span: ReadOnlySpan<'t>) =
            if this.size + span.Length > this.limit then
                this.GrowTo(this.size + span.Length)

            let slice = this.pool.AsSpan().Slice(this.size, span.Length)

            span.CopyTo slice
            this.size <- this.size + span.Length

        member this.GrowTo(newLimit) =
            let newArray = ArrayPool.Shared.Rent(newLimit)
            Array.Copy(this.pool, newArray, this.size)
            ArrayPool.Shared.Return(this.pool)
            this.pool <- newArray
            this.limit <- this.pool.Length

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Clear() = this.size <- 0

        member this.Count = this.size

        member this.Contains(item) =
            let mutable e = this.pool.AsSpan(0, this.size).GetEnumerator()

            let mutable found = false

            while not found && e.MoveNext() do
                found <- obj.ReferenceEquals(e.Current, item)

            found

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.GetEnumerator() =
            this.pool.AsSpan(0, this.size).GetEnumerator()

        // this cannot really be used because: F# Compiler (52):
        // The value has been copied to ensure the original
        // is not mutated by this operation or because the copy
        // is implicit when returning a struct from a member and
        // another member is then accessed
        member this.Length = this.size

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member inline this.AsSpan() = this.pool.AsSpan(0, this.size)

        member inline this.ToArray() : 't[] =
            this.pool.AsSpan(0, this.size).ToArray()

        member this.ToHashSet() : HashSet<'t> =
            let hs = HashSet()

            for v in this.pool.AsSpan(0, this.size) do
                hs.Add(v) |> ignore

            hs

        member this.AsMemory() : Memory<'t> = this.pool.AsMemory(0, this.size)

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.Dispose() =
            ArrayPool.Shared.Return(this.pool, false)

        interface IDisposable with
            [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
            member this.Dispose() = this.Dispose()

        // prevent defensive copies
        static member inline toSpan(vs: ValueList<'t>) = vs.pool.AsSpan(0, vs.size)

        new(initialSize: int) =
            {
                size = 0
                limit = initialSize
                pool = ArrayPool.Shared.Rent(initialSize)
            }

    [<Struct; IsReadOnly>]
    type MatchResult = {
        Value: string
        Index: int
        Length: int
    } with

        override this.ToString() = this.Value

    [<Struct; IsReadOnly>]
    type SingleMatchResult = {
        Success: bool
        Value: string
        Index: int
        Length: int
    } with

        static member val Empty =
            {
                Success = false
                Value = ""
                Index = 0
                Length = 0
            }


    // i would want to use the one built into .NET
    // but the problem is this error: 
    // 
    // type instantiation involves a byref type. 
    // This is not permitted by the rules of Common IL. F# Compiler (412)
    [<Struct; IsReadOnly; >]
    type ValueMatch =
        val Index: int
        val Length: int

        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        new(x, y) = { Index = x; Length = y }

        /// gets string from char span
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member this.GetText(input: ReadOnlySpan<char>) =
            let slc = input.Slice(this.Index, this.Length)
            slc.ToString()


    [<AbstractClass>]
    type GenericRegexMatcher<'tchar when 'tchar: struct>() =
        abstract member IsMatch: input: ReadOnlySpan<'tchar> -> bool

        abstract member Replace:
            input: ReadOnlySpan<'tchar> * replacement: ReadOnlySpan<'tchar> -> string

        abstract member Matches: input: ReadOnlySpan<'tchar> -> MatchResult[]
        abstract member ValueMatches: input: ReadOnlySpan<'tchar> -> ValueList<ValueMatch>
        abstract member FirstEnd: input: ReadOnlySpan<'tchar> -> int
        abstract member LongestEnd: input: ReadOnlySpan<'tchar> -> int
        abstract member Count: input: ReadOnlySpan<char> -> int
