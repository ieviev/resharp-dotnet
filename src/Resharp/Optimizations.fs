module internal rec Resharp.Optimizations

open System.Buffers
open Resharp.Common
open System.Collections.Generic
open Resharp.Types
open Resharp.Patterns
open Resharp.Cache
open System
open Resharp.Runtime
open Resharp.Internal

[<Sealed>]
type internal MatchState<'t when TSet<'t> and 't: equality>(node: RegexNodeId) =
    member val Id: TState = zero with get, set
    member val Node = node with get, set
    member val Flags: StateFlags = StateFlags.None with get, set
    // -- optimizations
    member val PendingNullablePositions: struct (rsint * rsint)[] =
        Unchecked.defaultof<_> with get, set

    member val MinPendingNullable: int = Unchecked.defaultof<_> with get, set

    member val MintermSearchValues: MintermSearchValues<'t> =
        Unchecked.defaultof<_> with get, set

/// bypassing the F# type system to make sure there is no conversion magic happening
/// parts of this may be redundant since you never know what is a no-op and what is not
/// but we're going the extra mile to make sure there are no hidden tricks since the
/// whole algorithm is about optimizing one hot loop where any little surprise puts us at 2nd place
module Inline =
    /// *mad scientist*: finally! `return` in F#
    // let inline ret (v: ^t) : ^t = (# "ret" v : ^t #)
    let inline add (a: ^t) (b: ^t2) : ^t3 = (# "add" a b : ^t3 #)
    let inline sub (a: ^t) (b: ^t2) : ^t3 = (# "sub" a b : ^t3 #)
    let inline bor (a: ^t) (b: ^t2) : ^t3 = (# "or" a b : ^t3 #)
    let inline shl (a: ^t) (b: ^t2) : ^t3 = (# "shl" a b : ^t3 #)
    let inline clt_un (a: ^t) (b: ^t2) : bool = (# "clt.un" a b : bool #)
    let inline ldelemu1 (a: ^t) (b: ^t2) : byte = (# "ldelem.u1" a b : byte #)

    let inline isNull (l_nk: ^t) (currentStateId: ^t2) =
        clt_un (ldelemu1 l_nk currentStateId) NullKind.NotNull

    let inline setNullFull
        (_stateArray: MatchState<'t> array)
        (acc: byref<ValueList<int>>)
        (l_nk: NullKind array)
        (currentStateId: int)
        (l_pos: int)
        : unit =
        match (# "ldelem.u1" l_nk currentStateId : NullKind #) with
        | NullKind.CurrentNull
        | NullKind.PrevNull as nk -> ValueList.add(&acc, add l_pos nk)
        | _ ->
            let span = _stateArray[currentStateId].PendingNullablePositions

            for i = span.Length - 1 downto 0 do
                let struct (s, e) = span[i]

                for i = int e downto int s do
                    ValueList.add(&acc, i + l_pos)

    let inline nextStateId
        (l_dfaDelta: TState[])
        (currentStateId: int)
        (l_mtlog: byte)
        (mt: TMinterm[])
        (input: ReadOnlySpan<char>)
        (l_pos: int)
        =
        let shift = currentStateId <<< int l_mtlog
        let mt = mt[int input[l_pos]]
        l_dfaDelta[shift ||| int mt]

    let inline mintermId (p_mt: TMinterm[]) (p_input: ReadOnlySpan<char>) (l_pos: int) =
        p_mt[int p_input[l_pos]]


/// initial prefix optimizations
[<RequireQualifiedAccess>]
[<NoComparison>]
type InitialAccelerator<'t, 'tchar
    when 'tchar: struct
    and 'tchar :> IEquatable<'tchar>
    and 't :> IEquatable<'t>
    and 't: equality> =
    | NoAccelerator
    /// ex. Twain ==> (ε|Twain)
    | StringPrefix of prefix: Memory<'tchar> * transitionNodeId: TState
    | StringPrefixCaseIgnore of
        prefix: Memory<'tchar> *
        isAscii: bool *
        transitionNodeId: TState
    | SearchValuesPrefix of prefix: Memory<MintermSearchValues<'t>> * transitionNodeId: TState
    | SingleSearchValuesPrefix of prefix: MintermSearchValues<'t> * transitionNodeId: TState
    | SingleSearchValues of prefix: MintermSearchValues<'t>
    /// potential start prefix from searchvalues
    | SearchValuesPotentialStart of prefix: Memory<MintermSearchValues<'t>>


/// the default option to find the match end is to match again in reverse
/// often this is overkill and can be replaced with something much simpler
[<NoComparison>]
type LengthLookup<'t> =
    /// skip match end lookup entirely
    | FixedLength of length: int
    /// skip some transitions as we already know where match starts
    /// e.g., hello.\*world starts looking at hello|->.\*end
    | FixedLengthPrefixMatchEnd of prefixLength: int * transitionId: TState
    /// avx lookup a single character
    | SetLookup of
        prefixLength: int *
        mtId: TMinterm *
        skipKind: SkipKind *
        nullKind: NullKind *
        sv: MintermSearchValues<'t>
    | RemainingSets of prefixLength: int * mtId: TMinterm * remaining: byte
    /// default match end lookup
    | MatchEnd

/// override for trivial literal string search
[<RequireQualifiedAccess>]
[<NoComparison>]
type MatchOverride<'tchar when 'tchar: struct and 'tchar :> IEquatable<'tchar>> =
    // removed SearchValues<string> since it could just be used directly instead
    | FixedLengthString of string: Memory<'tchar>
    | FixedLengthStringCaseIgnore of string: Memory<'tchar>
    | NonAsciiFixedLengthStringCaseIgnore of
        head: SearchValues<'tchar> *
        string: Memory<'tchar>
#if DEBUG
let printPrefixSets (cache: RegexCache<_>) (sets: uint64 seq) =
    sets
    |> Seq.map cache.PrettyPrintMinterm
    |> Seq.map (fun v ->
        match v with
        | @"[^\n]" -> "."
        | c when c.Length > 25 -> "φ" // dont expand massive sets
        | c -> c
    )
    |> String.concat ";"

let printPrefixSets2 (cache: RegexCache<_>) (sets: BitVector list) =
    sets
    |> Seq.map (fun v ->
        match (box cache.Solver :?> BitVectorSolver).PrettyPrint(v, cache.CharsetSolver) with
        | @"[^\n]" -> "."
        | c when c.Length > 25 -> "φ" // dont expand massive sets
        | c -> c
    )
    |> String.concat ";"

let printPrettyDerivs (cache: RegexCache<'t>) (derivs: ('t * 'a)[]) =
    derivs
    |> (Array.map (fun (mt, node) ->
        $"{cache.PrettyPrintMinterm(unbox mt), -13} ==> {node.ToString()}"
    ))
    |> String.concat "\n"
    |> (fun v -> "\n" + v)
#endif


let getImmediateDerivativesMerged
    createNonInitialDerivative
    (cache: RegexCache<_>)
    (node: RegexNodeId)
    =
    cache.Minterms()
    |> Seq.map (fun minterm ->
        let der = createNonInitialDerivative (minterm, node)
        minterm, der
    )
    |> Seq.groupBy snd
    |> Seq.map (fun (_, group) ->

        group
        |> Seq.map fst
        |> Seq.fold (fun acc v -> cache.Solver.Or(acc, v)) cache.Solver.Empty,
        group |> Seq.head |> snd
    )

let getNonRedundantDerivatives
    getNonInitialDerivative
    (cache: RegexCache<'t>)
    (redundantNodes: HashSet<RegexNodeId>)
    (node: RegexNodeId)
    =
    getImmediateDerivativesMerged getNonInitialDerivative cache node
    |> Seq.where (fun (_, deriv) -> not (redundantNodes.Contains(deriv)))

/// strip parts irrelevant for prefix.
/// anchorOpt: apply Or-anchor optimization for lookback handling
let rec private getPrefixNodeCore
    (anchorOpt: bool)
    (cache: RegexCache<'t>)
    (node: RegexNodeId)
    : RegexNodeId =
    let b = cache.Builder
    let resolve = b.Resolve
    let recurse = getPrefixNodeCore anchorOpt cache

    match b.Node(node) with
    | Loop(node = body; low = n; up = _) -> b.mkLoop (body, n, n)
    | Concat(head = head; tail = tail) ->
        match b.Node(head) with
        | Loop(low = 0; up = Int32.MaxValue) -> recurse tail
        | Loop(low = 0; up = 1) -> recurse tail
        | Loop(node = body; low = n; up = Int32.MaxValue) ->
            b.mkConcat2 (b.mkLoop (body, n, n), tail)
        | Or(nodes = xs) ->
            let newOr = xs |> Seq.map recurse |> b.mkOrSeq
            b.mkConcat2 (newOr, tail)
        | LookAround(node = lookbody; lookBack = true) ->
            match b.Node(lookbody) with
            | Or(nodes = nodes) when anchorOpt && b.Info(lookbody).NodeFlags.DependsOnAnchor ->
                let remaining =
                    nodes
                    |> Seq.where (fun v ->
                        match b.Node(v) with
                        | End -> false
                        | _ -> true
                    )
                    |> b.mkOrSeq

                match b.Node(remaining), b.Node(tail) with
                | Singleton phead, Concat(head = ch; tail = ctail) ->
                    match (|PredStar|_|) resolve ch with
                    | ValueSome ploop ->
                        let merged = cache.Solver.Or(phead, ploop)

                        if cache.Solver.IsFull(merged) then
                            recurse ctail
                        else
                            recurse (b.mkConcat2 (remaining, tail))
                    | _ -> recurse (b.mkConcat2 (remaining, tail))
                | _ -> recurse (b.mkConcat2 (remaining, tail))
            | Concat(head = ch; tail = lookTail) ->
                match ch = RegexNodeId.TOP_STAR with
                | true -> recurse (b.mkConcat2 (lookTail, tail))
                | _ -> getPrefixNodeCore true cache (b.mkConcat2 (lookbody, tail))
            | _ -> getPrefixNodeCore true cache (b.mkConcat2 (lookbody, tail))
        | _ -> node
    | And _ -> node
    | LookAround(node = inner; lookBack = false) -> inner
    | _ -> node

let getPrefixNode (cache: RegexCache<'t>) (node: RegexNodeId) : RegexNodeId =
    getPrefixNodeCore true cache node

let getPotentialStartNode
    (_: ResharpOptions)
    (cache: RegexCache<'t>)
    (node: RegexNodeId)
    : RegexNodeId =
    getPrefixNodeCore false cache node

let rec calcPrefixSets
    getNonInitialDerivative
    (cache: RegexCache<'t>)
    (startNode: RegexNodeId)
    : ResizeArray<'t> =
    let b = cache.Builder

    let redundant =
        HashSet<RegexNodeId>(
            [
                RegexNodeId.BOT
                startNode
            ]
        )
    // nothing to complement if a match has not started
    let prefixStartNode = getPrefixNode cache startNode
    redundant.Add(prefixStartNode) |> ignore

    let acc = ResizeArray<'t>()

    let mutable node = prefixStartNode
    let mutable cont = true

    while cont do
        if (acc.Count > 0 && redundant.Contains(node)) || b.Info(node).CanBeNullable then
            cont <- false
        else
            let prefix_derivs =
                getNonRedundantDerivatives getNonInitialDerivative cache redundant node
                |> Seq.toArray

            match prefix_derivs with
            | [| (mt, deriv) |] ->
                if deriv = node then
                    acc.Clear()
                    cont <- false
                else
                    acc.Add(mt)
                    node <- deriv
            | _ -> cont <- false

    acc


// todo: get rid of F# list here
let rec calcPotentialMatchStart
    (options: ResharpOptions)
    getNonInitialDerivative
    (cache: RegexCache<_>)
    (startNode: RegexNodeId)
    =
    let b = cache.Builder
    let redundant = HashSet<RegexNodeId>()
    redundant.Add(RegexNodeId.BOT) |> ignore
    let nodes = HashSet<RegexNodeId>()
    let tempList = new PooledArray<_>(128)

    let rec loop(acc: 't list) =
        tempList.Clear()

        if
            nodes.Count > options.FindPotentialStartSizeLimit
            || acc.Length > options.MaxPrefixLength
            || nodes.Count = 0
        then
            acc |> List.rev
        else
            let shouldExit = nodes |> Seq.exists (fun n -> b.Info(n).CanBeNullable)

            if shouldExit then
                acc |> List.rev
            else

                let mutable ss = cache.Solver.Empty

                for n in nodes do
                    let r =
                        getNonRedundantDerivatives getNonInitialDerivative cache redundant n

                    for n in r do
                        ss <- cache.Solver.Or(ss, fst n)

                    tempList.Add(r)

                nodes.Clear()

                for iseq in tempList do
                    iseq |> Seq.iter (fun v -> nodes.Add(snd v) |> ignore)

                loop (ss :: acc)

    let startNode = getPotentialStartNode (options: ResharpOptions) cache startNode
    nodes.Add(startNode) |> ignore
    redundant.Add(startNode) |> ignore
    let sets = loop []
    sets

let rec attemptCompileFullDfa
    (options: ResharpOptions)
    (createTransition: LocationKind -> TState -> TMinterm -> TState)
    (getFlags: TState -> StateFlags)
    (cache: RegexCache<'t>)
    (ts_r_l_node: TState)
    (l_r_node: TState)
    : bool * int =
    let mutable maxId: TState = LanguagePrimitives.GenericZero
    let mutable skippables = 0
    let redundant = HashSet<TState>()
    let pending = HashSet<TState>()
    let tempList: PooledArray<TState> = new PooledArray<_>(128)
    let mids = byte (cache.NumOfMinterms())

    for minId in 0uy .. (mids - 1uy) do
        let endTrans = createTransition LocationKind.End ts_r_l_node minId
        redundant.Add(endTrans) |> ignore
        pending.Add(endTrans) |> ignore
        let transition2 = createTransition LocationKind.Center ts_r_l_node minId
        redundant.Add(transition2) |> ignore
        pending.Add(transition2) |> ignore

    let mutable loopRev = true

    while loopRev do
        loopRev <- false
        tempList.Clear()

        if pending.Count = 0 || int maxId >= options.DfaThreshold then
            ()
        else
            for n in pending do
                redundant.Add(n) |> ignore

                if int maxId >= options.DfaThreshold then
                    ()
                else

                    if int n < 10 && (getFlags n).CanSkip then
                        skippables <- skippables + 1

                    let mutable minId = 0uy

                    while minId < mids do
                        let transition = createTransition LocationKind.Center n minId
                        maxId <- max transition maxId

                        if not (redundant.Contains(transition)) then
                            tempList.Add(transition)

                        minId <- minId + 1uy

            pending.Clear()

            for tempNode in tempList do
                pending.Add(tempNode) |> ignore

            loopRev <- true

    pending.Clear()

    if redundant.Add(l_r_node) then
        pending.Add(l_r_node) |> ignore

    let mutable loopFwd = true

    while loopFwd do
        loopFwd <- false
        tempList.Clear()

        if pending.Count = 0 || int maxId >= options.DfaThreshold then
            ()
        else
            for n in pending do
                redundant.Add(n) |> ignore
                let flags = getFlags n

                if flags.CanSkip then
                    skippables <- skippables + 1

                let mutable minId = 0uy

                while minId < mids do
                    let transition = createTransition LocationKind.Center n minId
                    maxId <- max transition maxId

                    if not (redundant.Contains(transition)) then
                        tempList.Add(transition)

                    minId <- minId + 1uy

            pending.Clear()

            for tempNode in tempList do
                pending.Add(tempNode) |> ignore

            loopFwd <- true

    int maxId < options.DfaThreshold && not (getFlags l_r_node).IsAlwaysNullable, skippables


let applyPrefixSets
    getNonInitialDerivative
    (cache: RegexCache<_>)
    (node: RegexNodeId)
    (sets: ResizeArray<'t>)
    =
    let mts = cache.Minterms()
    let mutable current = node

    for i = 0 to sets.Count - 1 do
        let matchingPred = mts |> Seq.find (fun v -> cache.Solver.elemOfSet v sets[i])
        current <- getNonInitialDerivative (matchingPred, current)

    current

let rec applyPrefixSetsWhileNotNullable
    getNonInitialDerivative
    (cache: RegexCache<_>)
    (node: RegexNodeId)
    (sets: 't list)
    =
    if cache.Builder.Info(node).CanBeNullable then
        node, sets.Length
    else
        match sets with
        | [] -> node, sets.Length
        | head :: tail ->
            let der = getNonInitialDerivative (head, node)
            applyPrefixSetsWhileNotNullable getNonInitialDerivative cache der tail


let caseInsensitivePrefixes prefix (c: RegexCache<_>) (reversed: bool) =
    let mts = c.Minterms()

    prefix
    |> Seq.map (fun v ->
        // negated set
        if c.Solver.elemOfSet v mts[0] then
            ValueNone
        else
            let chrs = c.MintermChars(v)

            chrs
            |> (fun chrs ->
                if chrs.Length = 1 then
                    ValueSome(chrs.Span[0])
                else
                    let up c = Char.IsUpper c
                    let low c = Char.IsLower c

                    if
                        (chrs.Length = 2)
                        && ((up chrs.Span[0] && low chrs.Span[1])
                            || (low chrs.Span[0] && up chrs.Span[1]))
                    then
                        ValueSome(chrs.Span[0])
                    else
                        ValueNone
            )
    )
    |> Seq.takeWhile _.IsSome
    |> Seq.map _.Value
    |> (fun v -> if reversed then Seq.rev v else v)
    |> Seq.toArray
    |> Memory


let isTooCommon (c: RegexCache<'t>) (sv: MintermSearchValues<'t>) =
    match sv.Mode, SearchValuesKind.ofSearchValues (sv.SearchValues) with
    | MintermSearchMode.SearchValues, SearchValuesKind.Small -> false
    | MintermSearchMode.SearchValues, SearchValuesKind.Range ->
        let bdd = c.Solver.convertToBdd (c.CharsetSolver, c.MtsBDD(), sv.Minterm)
        let ranges = BDDRangeConverter.ToRanges(bdd)

        let totalCount =
            let mutable sum = 0u

            for (rs, re) in ranges do
                sum <- sum + (re - rs + 1u)

            sum

        if totalCount < 10u then
            false
        else
            match ranges with
            | [| (97u, 122u) |] -> true // [a-z] classifies as small but is generally too frequent
            | [| (low, up) |] when low >= 97u && up <= 122u -> true
            | [| (48u, 57u) |] -> false // [0-9] usually worth optimizing
            | _ -> false
    | MintermSearchMode.InvertedSearchValues, SearchValuesKind.Small -> sv.Commonality < 100
    | _ -> true

let findInitialOptimizations
    (options: ResharpOptions)
    (getNonInitialDerivative: 't * RegexNodeId -> RegexNodeId)
    (nodeToId: RegexNodeId -> TState)
    (c: RegexCache<'t>)
    (node: RegexNodeId)
    (trueStarredNode: RegexNodeId)
    (reversed: bool)
    : InitialAccelerator<'t, char> =
    if options.FindPotentialStartSizeLimit = 0 then
        InitialAccelerator.NoAccelerator
    else
        let prefix = Optimizations.calcPrefixSets getNonInitialDerivative c node

        if prefix.Count > 1 then
            let singleCharPrefixes =
                prefix
                |> Seq.map (fun v ->
                    if c.MintermIsInverted(v) then
                        ValueNone
                    else
                        let chrs = c.MintermChars(v)

                        if chrs.Length = 1 then
                            ValueSome(chrs.Span[0])
                        else
                            ValueNone
                )
                |> Seq.takeWhile _.IsSome
                |> Seq.map _.Value
                |> (fun v -> if reversed then Seq.rev v else v)
                |> Seq.toArray
                |> Memory

            if singleCharPrefixes.Length > 0 then
                let applied =
                    Optimizations.applyPrefixSets
                        getNonInitialDerivative
                        c
                        trueStarredNode
                        (prefix.GetRange(0, singleCharPrefixes.Length))

                InitialAccelerator.StringPrefix(singleCharPrefixes, nodeToId applied)
            else

                let caseiprefix = caseInsensitivePrefixes prefix c reversed

                if caseiprefix.Length > 0 then
                    let applied =
                        Optimizations.applyPrefixSets
                            getNonInitialDerivative
                            c
                            trueStarredNode
                            (prefix.GetRange(0, caseiprefix.Length))

                    let isAscii = caseiprefix |> Memory.forall (fun v -> Char.IsAscii(v))

                    InitialAccelerator.StringPrefixCaseIgnore(
                        caseiprefix,
                        isAscii,
                        nodeToId applied
                    )
                else

                    let applied =
                        Optimizations.applyPrefixSets
                            getNonInitialDerivative
                            c
                            trueStarredNode
                            prefix

                    let searchPrefix = prefix |> Seq.map c.MintermSearchValues |> Seq.toArray

                    // can not create a fast prefix if all are too common
                    if searchPrefix |> forall (isTooCommon c) then
                        InitialAccelerator.NoAccelerator
                    else
                        InitialAccelerator.SearchValuesPrefix(
                            Memory(searchPrefix),
                            nodeToId applied
                        )

        else
            let found =
                if prefix.Count = 1 then
                    let p1 = c.MintermSearchValues(prefix[0])

                    if not (isTooCommon c p1) then
                        let applied =
                            Optimizations.applyPrefixSets
                                getNonInitialDerivative
                                c
                                trueStarredNode
                                prefix

                        ValueSome(
                            InitialAccelerator.SingleSearchValuesPrefix(p1, nodeToId applied)
                        )
                    else
                        ValueNone
                else
                    ValueNone

            if found.IsSome then
                found.Value
            else
                match
                    Optimizations.calcPotentialMatchStart
                        options
                        getNonInitialDerivative
                        c
                        node
                with
                | potentialStart when potentialStart.Length > 0 ->
                    let searchPrefix =
                        potentialStart |> map c.MintermSearchValues |> Seq.toArray

                    if searchPrefix |> forall (isTooCommon c) then
                        InitialAccelerator.NoAccelerator
                    else

                        let useOnlyHead = searchPrefix[1..] |> forall (isTooCommon c)
                        // avoid some cases when only the head of the prefix is rare
                        if useOnlyHead then
                            if isTooCommon c searchPrefix[0] then
                                InitialAccelerator.NoAccelerator
                            else
                                InitialAccelerator.SingleSearchValues(searchPrefix[0])
                        else
                            InitialAccelerator.SearchValuesPotentialStart(
                                searchPrefix.AsMemory()
                            )

                | _ -> InitialAccelerator.NoAccelerator

let rec mkNodeWithoutLookbackPrefix (b: RegexBuilder<_>) (node: RegexNodeId) : RegexNodeId =
    match b.Node(node) with
    | LookAround(lookBack = true) -> RegexNodeId.EPS
    | Begin
    | End -> RegexNodeId.EPS
    | Concat(head = head; tail = tail) ->
        match b.Node(head) with
        | LookAround(lookBack = true) -> mkNodeWithoutLookbackPrefix b tail
        | _ when b.Info(head).IsAlwaysNullable ->
            let convertedTail = mkNodeWithoutLookbackPrefix b tail
            b.mkConcat2 (head, convertedTail)
        | _ ->
            let convertedHead = mkNodeWithoutLookbackPrefix b head

            match b.Node(convertedHead) with
            | _ when convertedHead = RegexNodeId.EPS -> mkNodeWithoutLookbackPrefix b tail
            | _ -> b.mkConcat2 (convertedHead, tail)
    | Or(nodes = xs) ->
        xs |> Seq.map (mkNodeWithoutLookbackPrefix b) |> Seq.toArray |> b.mkOrSeq
    | And(nodes = xs) -> xs |> Seq.map (mkNodeWithoutLookbackPrefix b) |> b.mkAndSeq
    | Not _ -> node
    | _ -> node

let rec getFixedPrefixLength (c: RegexCache<'t>) (node: RegexNodeId) =
    let b = c.Builder

    let rec loop (acc: int) node : int voption * RegexNodeId voption =
        match b.Node(node) with
        | Concat(head, tail) ->
            let headprf = loop acc head

            match headprf with
            | ValueSome n, ValueNone -> loop n tail
            | ValueSome n, ValueSome remain ->
                ValueSome n, ValueSome(b.mkConcat2 (remain, tail))
            | _ ->
                match acc with
                | 0 -> ValueNone, ValueNone
                | n -> ValueSome n, ValueSome node
        | _ when node = RegexNodeId.EPS -> ValueSome(acc), ValueNone
        | Or(nodes = _) -> ValueNone, ValueSome node
        | And(nodes = _) -> ValueNone, ValueSome node
        | Singleton _ -> ValueSome(1 + acc), ValueNone
        | Loop(node = body; low = low; up = up) ->
            match b.Node(body) with
            | Singleton _ when low = up -> ValueSome(low + acc), ValueNone
            | Singleton _ when low <> 0 ->
                let remainingUp = if up = Int32.MaxValue then up else up - low
                ValueSome(low + acc), ValueSome(b.mkLoop (body, 0, remainingUp))
            | _ -> ValueNone, ValueSome node
        | Not _ -> ValueNone, ValueSome node
        | LookAround _ -> ValueSome(0 + acc), ValueNone
        | Begin
        | End -> ValueSome(0 + acc), ValueNone

    let r = loop 0 node
    r


let inferLengthLookup
    (opts: ResharpOptions)
    (c: RegexCache<'t>)
    getDerivative
    (getNodeId: RegexNodeId -> TState)
    (getInfo: RegexNodeId -> NullKind * SkipKind * MintermSearchValues<'t>)
    (node: RegexNodeId)
    : LengthLookup<'t> =
    let b = c.Builder

    if opts.FindPotentialStartSizeLimit = 0 then
        LengthLookup.MatchEnd
    else
        b.GetFixedLength(node)
        |> ValueOption.map LengthLookup.FixedLength
        |> ValueOption.defaultWith (fun _ ->
            let fixedPrefix = getFixedPrefixLength c node

            match fixedPrefix with
            | ValueSome prefixLen, ValueSome remaining ->
                let stateId = getNodeId remaining

                match b.Node(remaining) with
                | Loop(node = body; low = 0; up = remainUp) ->
                    match b.Node(body) with
                    | Singleton pred when remainUp <= (int Byte.MaxValue) ->
                        LengthLookup.RemainingSets(
                            prefixLen,
                            c.MintermToId(pred),
                            byte remainUp
                        )
                    | _ ->
                        let prefix_derivs =
                            getNonRedundantDerivatives
                                getDerivative
                                c
                                (HashSet(
                                    [
                                        node
                                        remaining
                                    ]
                                ))
                                remaining
                            |> Seq.toArray

                        match prefix_derivs with
                        | [| mt, der |] when b.Info(der).IsAlwaysNullable ->
                            let prefix_derivs_2 =
                                getNonRedundantDerivatives
                                    getDerivative
                                    c
                                    (HashSet(
                                        [
                                            node
                                            remaining
                                            der
                                        ]
                                    ))
                                    der
                                |> Seq.toArray

                            match prefix_derivs_2 with
                            | [| _, der2 |] when der2 = RegexNodeId.BOT ->
                                let nullKind, skipKind, sv = (getInfo der)
                                let mtId = c.MintermToId(mt)

                                LengthLookup.SetLookup(
                                    prefixLen,
                                    mtId,
                                    skipKind,
                                    nullKind,
                                    sv
                                )
                            | _ -> LengthLookup.FixedLengthPrefixMatchEnd(prefixLen, stateId)
                        | _ -> LengthLookup.FixedLengthPrefixMatchEnd(prefixLen, stateId)
                | _ ->
                    let prefix_derivs =
                        getNonRedundantDerivatives
                            getDerivative
                            c
                            (HashSet(
                                [
                                    node
                                    remaining
                                ]
                            ))
                            remaining
                        |> Seq.toArray

                    match prefix_derivs with
                    | [| mt, der |] when b.Info(der).IsAlwaysNullable ->
                        let prefix_derivs_2 =
                            getNonRedundantDerivatives
                                getDerivative
                                c
                                (HashSet(
                                    [
                                        node
                                        remaining
                                        der
                                    ]
                                ))
                                der
                            |> Seq.toArray

                        match prefix_derivs_2 with
                        | [| _, der2 |] when der2 = RegexNodeId.BOT ->
                            let nullKind, skipKind, sv = (getInfo der)
                            let mtId = c.MintermToId(mt)
                            LengthLookup.SetLookup(prefixLen, mtId, skipKind, nullKind, sv)
                        | _ -> LengthLookup.FixedLengthPrefixMatchEnd(prefixLen, stateId)
                    | _ -> LengthLookup.FixedLengthPrefixMatchEnd(prefixLen, stateId)

            | _ -> LengthLookup.MatchEnd
        )

/// heuristics to see if we should use regex at all
let inferOverrideRegex
    (initialOptimizations: InitialAccelerator<'t, char>)
    (lengthLookup: LengthLookup<'t>)
    (c: RegexCache<'t>)
    (node: RegexNodeId)
    (reverseNode: RegexNodeId)
    : MatchOverride<char> voption =
    let flags = c.Builder.Info(node).NodeFlags
    if
        flags.DependsOnAnchor
        || flags.ContainsLookaround
        || c.Builder.Info(reverseNode).NodeFlags.DependsOnAnchor
    then
        ValueNone
    else
        match lengthLookup, initialOptimizations with
        | LengthLookup.FixedLength(fl), InitialAccelerator.StringPrefix(prefix, _) when
            fl = prefix.Length
            ->
            ValueSome(MatchOverride.FixedLengthString(prefix))
        | _ -> ValueNone
