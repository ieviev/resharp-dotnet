namespace Resharp

open System
open System.Buffers
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading

open Microsoft.FSharp.Core
open Resharp.Types
open Resharp.Cache
open Resharp.Optimizations
open Resharp.Common
open Resharp.Runtime
open Resharp.Algorithm
open Resharp.Internal

module I = Optimizations.Inline

[<Sealed>]
type internal RegexOptimizations<'t, 'tchar
    when TSet<'t>
    and 't: equality
    and 'tchar: struct
    and 'tchar :> IEquatable<'tchar>
    and 't: struct>
    (
        _cache: RegexCache<'t>,
        _R_L_Initial: InitialAccelerator<'t, 'tchar>,
        _LengthLookup: LengthLookup<'t>,
        _MatchOverride: MatchOverride<'tchar> voption
    ) =

    let addWeights(unweighted: MintermSearchValues<'t>[]) =
        if unweighted.Length = 0 then
            [||]
        else
            unweighted
            |> Seq.mapi (fun i set ->
                let mintermSV = set

                match mintermSV.Mode with
                | MintermSearchMode.SearchValues ->
                    let weight = mintermSV.Commonality
                    (i, mintermSV, weight)
                | MintermSearchMode.InvertedSearchValues -> (i, mintermSV, 10000.0)
                | _ -> failwith "unreachable!"
            )
            |> Seq.sortBy (fun (_, _, score) -> score)
            |> Seq.map (fun (i, set, _) -> struct (i, set))
            |> Seq.toArray

    member val RightToLeftInitial: InitialAccelerator<'t, 'tchar> = _R_L_Initial
    member val LengthLookup: LengthLookup<'t> = _LengthLookup
    /// if the pattern is simple enough that 
    /// we can use a specialized matching algorithm for it
    member val MatchOverride: MatchOverride<'tchar> voption = _MatchOverride

    member val RightToLeftWeightedSets: Lazy<struct (int * MintermSearchValues<'t>) array> =
        lazy
            let _prefixSets =
                match _R_L_Initial with
                | InitialAccelerator.SearchValuesPotentialStart(prefix) ->
                    prefix.ToArray() |> Array.rev
                | InitialAccelerator.SearchValuesPrefix(prefix, _) ->
                    prefix.ToArray() |> Array.rev
                | _ -> [||]

            addWeights (_prefixSets)


module internal States =
    [<Literal>]
    let DFA_DEAD = 1


[<Sealed>]
type internal RegexMatcher<'t when 't: struct and TSet<'t> and 't: equality>
    (uncanonicalizedNode: RegexNodeId, _cache: RegexCache<'t>, options: ResharpOptions) =
    inherit GenericRegexMatcher<char>()
    let _b = _cache.Builder
    let _stateCache = Dictionary<RegexNodeId, MatchState<'t>>()

    let mutable _stateArray =
        Array.zeroCreate<MatchState<'t>> options.InitialDfaCapacity

    let mutable _flagsArray = Array.zeroCreate<StateFlags> options.InitialDfaCapacity

    let mutable _svArray =
        Array.zeroCreate<MintermSearchValues<'t>> options.InitialDfaCapacity

    let mutable _nullKindArray = Array.zeroCreate<NullKind> options.InitialDfaCapacity
    let mutable _skipKindArray = Array.zeroCreate<SkipKind> options.InitialDfaCapacity
    let _mintermsLog = Algorithm.getMintermsLog (_cache.NumOfMinterms()) |> byte
    let _mtlookup: byte[] = Minterms.createLookupUtf16 (_cache.MtsBDD())

    let mutable _dfaDelta: TState[] =
        Array.zeroCreate ((I.shl options.InitialDfaCapacity _mintermsLog) * 2)

    let mutable _revStartStates: TState[] =
        Array.init
            ((I.shl (_cache.NumOfMinterms() * 2) _mintermsLog) + 1)
            (fun _ -> LanguagePrimitives.GenericZero)

    let _rwlock = new ReaderWriterLockSlim()

    let _createStartset(state: MatchState<'t>, initial: bool) =
        // expensive for a single match
        if _b.Info(state.Node).ContainsLookaround && not options.FindLookaroundPrefix then
            ()
        else
            let minterms = _cache.Minterms()

            let condition =
                if initial then
                    (fun d -> not (d = state.Node || d = RegexNodeId.BOT))
                else
                    (fun d -> not (d = state.Node))

            let startsetPredicate =
                let isTrivial =
                    match _b.Node(state.Node) with
                    | Singleton _ -> true
                    | Concat(head = h; tail = _) ->
                        match _b.Node(h) with
                        | Singleton _ -> true
                        | _ -> false
                    | _ -> false

                if isTrivial then
                    _cache.Solver.Full
                else

                    let mutable ss = _cache.Solver.Empty

                    let transitions = _b.Info(state.Node).Transitions

                    for mt in minterms do

                        let der =
                            match transitions.TryGetValue(mt) with
                            | true, v -> v
                            | _ ->
                                RegexNode.derivative (_b, LocationKind.Center, mt, state.Node)

                        if condition der then
                            ss <- _cache.Solver.Or(ss, mt)

                    ss

            if
                not (
                    _cache.Solver.IsEmpty(startsetPredicate)
                    || _cache.Solver.IsFull(startsetPredicate)
                )
            then
                let setChars = _cache.MintermSearchValues(startsetPredicate)
                state.MintermSearchValues <- setChars
                let searchValuesKind = setChars.SearchValues |> SearchValuesKind.ofSearchValues

                match setChars.Mode, searchValuesKind with
                | MintermSearchMode.InvertedSearchValues, SearchValuesKind.Small -> ()
                | _, SearchValuesKind.Small ->
                    state.Flags <- state.Flags ||| StateFlags.CanSkipFlag
                | MintermSearchMode.SearchValues, SearchValuesKind.Range ->
                    if Optimizations.isTooCommon _cache setChars then
                        ()
                    else
                        state.Flags <- state.Flags ||| StateFlags.CanSkipFlag
                | MintermSearchMode.InvertedSearchValues, SearchValuesKind.Range ->
                    state.Flags <- state.Flags ||| StateFlags.CanSkipFlag
                | _ -> ()

    let replaceWithPooled (oldarr: byref<_[]>) (newsize: int) : unit =
        if oldarr.Length >= newsize then
            ()
        else
            let newPool = ArrayPool<_>.Shared.Rent(newsize)
            Array.Clear(newPool)
            oldarr.AsSpan().CopyTo(newPool.AsSpan())
            ArrayPool.Shared.Return(oldarr)
            oldarr <- newPool

    let rec _getOrCreateState(origNode, isInitial) =
        let node = origNode

        match _stateCache.TryGetValue(node) with
        | true, v -> v // a dfa state already exists for this regex
        | _ ->
            _rwlock.EnterWriteLock()
            try
            match _stateCache.TryGetValue(node) with
            | true, v -> v
            | _ ->
            let state = MatchState(node)
            _stateCache.Add(node, state)
            let stateOrig = _stateCache.Count
            state.Id <- stateOrig
            let nodeFlags = _b.Info(node).NodeFlags

            if _stateArray.Length = stateOrig then
                if _stateArray.Length > options.MaxDfaCapacity then
                    failwith
                        "Maximum allowed state space reached! increase ResharpOptions.MaxDfaCapacity if this is intended"

                let newsize = _stateArray.Length * 2
                // default
                Array.Resize(&_stateArray, newsize)
                Array.Resize(&_flagsArray, newsize)
                Array.Resize(&_svArray, newsize)
                Array.Resize(&_nullKindArray, newsize)
                Array.Resize(&_skipKindArray, newsize)
                Array.Resize(&_dfaDelta, I.shl newsize _mintermsLog)

            _stateArray[state.Id] <- state

            if nodeFlags.IsAlwaysNullable then
                state.Flags <- state.Flags ||| StateFlags.IsAlwaysNullableFlag

            if nodeFlags.CanBeNullable && nodeFlags.DependsOnAnchor then
                state.Flags <- state.Flags ||| StateFlags.IsAnchorNullableFlag

            if isInitial then
                state.Flags <- state.Flags ||| StateFlags.InitialFlag

            if _b.Info(node).CanBeNullable then
                if RegexNode.isNullable (_b, LocationKind.End, node) then
                    state.Flags <- state.Flags ||| StateFlags.IsEndNullableFlag

                if RegexNode.isNullable (_b, LocationKind.Begin, node) then
                    state.Flags <- state.Flags ||| StateFlags.IsBeginNullableFlag

            // generate startset
            if _b.OrCount < options.StartsetInferenceLimit then
                _createStartset (state, isInitial)

            let nodeInfo = _b.Info(node)

            if nodeInfo.ContainsLookaround && nodeInfo.CanBeNullable && not isInitial then
                if not nodeInfo.PendingNullables.IsEmpty then
                    state.Flags <- state.Flags ||| StateFlags.IsPendingNullableFlag

                    let isCurrNullable =
                        if nodeFlags.CanBeNullable then
                            match _b.Node(node) with
                            | Or(nodes = nodes) ->
                                nodes
                                |> exists (fun v ->
                                    let info = _b.Info(v)
                                    info.PendingNullables.IsEmpty && info.CanBeNullable
                                )
                            | Loop(low = 0) -> true
                            | _ -> false
                        else
                            false

                    let pendingNulls = [|
                        let mutable contains0 = false

                        match nodeInfo.PendingNullables.inner with
                        | [||] -> ()
                        | [| (s, e) |] ->
                            if s = LanguagePrimitives.GenericZero then
                                contains0 <- true

                            yield struct (s, e)
                        | hs ->
                            yield! hs

                            if
                                hs
                                |> Seq.exists (fun struct (s, _) ->
                                    s = LanguagePrimitives.GenericZero
                                )
                            then
                                contains0 <- true

                        if isCurrNullable && not (contains0) then
                            yield
                                struct (LanguagePrimitives.GenericZero,
                                        LanguagePrimitives.GenericZero)
                    |]

                    Array.sortInPlace pendingNulls
                    state.PendingNullablePositions <- pendingNulls
                    let struct (s, _) = pendingNulls[0]

                    state.MinPendingNullable <- int s

            _flagsArray[stateOrig] <- state.Flags
            _svArray[stateOrig] <- state.MintermSearchValues

            _nullKindArray[stateOrig] <-
                if not (StateFlags.isAlwaysNullable state.Flags) then
                    NullKind.NotNull
                elif StateFlags.isAlwaysNullableNonPending state.Flags then
                    NullKind.CurrentNull
                elif state.PendingNullablePositions = [| struct (1us, 1us) |] then
                    NullKind.PrevNull
                else
                    NullKind.PendingNull

            _skipKindArray[stateOrig] <-
                if StateFlags.canNotSkip state.Flags then
                    SkipKind.NotSkip
                elif StateFlags.isInitial state.Flags then
                    SkipKind.SkipInitial
                else
                    SkipKind.SkipActive

            state
            finally
                _rwlock.ExitWriteLock()

    let R_canonical = uncanonicalizedNode
    let reverseNode = RegexNode.rev _b R_canonical

    let reverseTrueStarredNode: RegexNodeId =
        _b.mkConcat2 (RegexNodeId.TOP_STAR, reverseNode)

    let trueStarredNode: RegexNodeId = _b.mkConcat2 (RegexNodeId.TOP_STAR, R_canonical)

    let _noprefixRev = mkNodeWithoutLookbackPrefix _b reverseNode
    let _noprefix = mkNodeWithoutLookbackPrefix _b R_canonical

    let _ = _getOrCreateState(RegexNodeId.BOT, false).Id // dfa dead state: 1
    let DFA_TR_REV = _getOrCreateState(reverseTrueStarredNode, true).Id
    let DFA_R_NOPR = _getOrCreateState(_noprefix, false).Id

    let createTransition
        (loc: LocationKind)
        (currStateId: TState)
        (mintermId: TMinterm)
        : TState =
        let node = _stateArray[int currStateId].Node
        let minterm = _cache.MintermById(int mintermId)

        match loc with
        | LocationKind.End when _b.Info(node).NodeFlags.DependsOnAnchor ->
            let dfaOffset: TState = Inline.bor (Inline.shl currStateId _mintermsLog) mintermId
            let cachedStateId = _revStartStates[int dfaOffset]
            // existing transition in dfa
            if cachedStateId > LanguagePrimitives.GenericZero then
                cachedStateId
            else
                let der = RegexNode.derivative (_b, loc, minterm, node)
                let nextState = _getOrCreateState(der, false).Id
                _revStartStates[int dfaOffset] <- nextState
                nextState
        | _ ->
            let dfaOffset = I.bor (I.shl currStateId _mintermsLog) mintermId
            let nextStateId = _dfaDelta[dfaOffset]

            if nextStateId > zero then
                nextStateId
            else
                let targetState = RegexNode.derivative (_b, loc, minterm, node)
                let nextStateId = _getOrCreateState(targetState, false).Id
                _dfaDelta[dfaOffset] <- nextStateId
                nextStateId

    let fullDfa, skippables =
        Optimizations.attemptCompileFullDfa
            options
            createTransition
            (fun v -> _flagsArray[int v])
            _cache
            DFA_TR_REV
            DFA_R_NOPR

    let maxNullKind =
        if fullDfa then
            let totalStates = _stateCache.Count
            let mutable found = NullKind.CurrentNull
            let nk = _nullKindArray
            let mutable i = 0

            while i < totalStates - 1 && found <> NullKind.PendingNull do
                i <- i + 1
                let n = nk[i]

                match n with
                | NullKind.NotNull -> ()
                | NullKind.CurrentNull -> ()
                | NullKind.PrevNull ->
                    match found with
                    | NullKind.CurrentNull
                    | NullKind.NotNull -> found <- n
                    | _ -> ()
                | NullKind.PendingNull -> found <- NullKind.PendingNull
                | _ -> failwith "invalid null"

            found
        else
            NullKind.CurrentNull


    let utf16Optimizations =
        let R_L_Initial: InitialAccelerator<'t, char> =
            (findInitialOptimizations
                options
                (fun (minterm, node) ->
                    RegexNode.derivative (_b, LocationKind.Center, minterm, node)
                )
                (fun node -> _getOrCreateState(node, false).Id)
                _cache
                reverseNode
                reverseTrueStarredNode
                true)

        let _lengthLookup =
            inferLengthLookup
                options
                _cache
                (fun (minterm, node) ->
                    RegexNode.derivative (_b, LocationKind.Center, minterm, node)
                )
                (fun node -> _getOrCreateState(node, false).Id)
                (fun node ->
                    let st = _getOrCreateState(node, false).Id
                    (_nullKindArray[st], _skipKindArray[st], _svArray[st])
                )
                _noprefix

        let _regexOverride: MatchOverride<char> voption =
            inferOverrideRegex R_L_Initial _lengthLookup _cache R_canonical reverseNode

        RegexOptimizations(_cache, R_L_Initial, _lengthLookup, _regexOverride)

    // assertions
    do
        assert
            (if fullDfa then
                 not (_b.Info(_stateArray[DFA_R_NOPR].Node).IsAlwaysNullable)
             else
                 true)


    override this.IsMatch(input) =
        if input.Length = 0 then
            StateFlags.canBeNullable _flagsArray[DFA_TR_REV]
        else
            use mutable acc = new ValueList<int>(16)
            let mutable initState = DFA_TR_REV
            let startPos = this.HandleInputEnd(_flagsArray[initState], &initState, input, &acc)
            let endStateId = this.collect_skip (&acc, input, startPos, initState)
            this.HandleInputStart(endStateId, &acc)
            acc.size > 0

    member this.Match(input) : SingleMatchResult =
        let slices = this.ValueMatches(input)
        let firstMatch = ValueList.toSpan slices

        match firstMatch.Length = 0 with
        | true -> SingleMatchResult.Empty
        | false ->
            let result = firstMatch[0]
            let textslice = input.Slice(result.Index, result.Length)

            {
                Success = true
                Value = textslice.ToString()
                Index = result.Index
                Length = result.Length
            }

    override this.Replace(input: ReadOnlySpan<char>, replacementPattern: ReadOnlySpan<char>) =
        let sb = System.Text.StringBuilder()
        let mutable offset = 0
        // whether to insert the match itself
        let hasReplacement0 = replacementPattern.IndexOf("$0") > -1
        let replacementString = replacementPattern.ToString()
        use results = this.llmatch_all input

        for result in ValueList.toSpan (results) do
            let preceding = input.Slice(offset, result.Index - offset)

            let replacement =
                if hasReplacement0 then
                    // there's an allocation here, could be done better
                    let rawSlice = input.Slice(result.Index, result.Length)
                    let textSlice = rawSlice.ToString()
                    let replacement = replacementString.Replace("$0", textSlice)
                    replacement.AsSpan()
                else
                    replacementPattern

            sb.Append(preceding) |> ignore
            sb.Append(replacement) |> ignore
            let nextStart = offset + preceding.Length + result.Length
            offset <- nextStart

        let remaining = input.Slice(offset)
        sb.Append(remaining) |> ignore
        sb.ToString()


    member this.Replace(input: ReadOnlySpan<char>, replacementPattern: Func<string, string>) =
        let sb = System.Text.StringBuilder()
        let mutable offset = 0
        use results = this.llmatch_all input

        for result in ValueList.toSpan (results) do
            let preceding = input.Slice(offset, result.Index - offset)

            let replacement =
                let textSlice = input.Slice(result.Index, result.Length)
                let replacement = replacementPattern.Invoke(textSlice.ToString())
                replacement.AsSpan()

            sb.Append(preceding) |> ignore
            sb.Append(replacement) |> ignore
            let nextStart = offset + preceding.Length + result.Length
            offset <- nextStart

        let remaining = input.Slice(offset)
        sb.Append(remaining) |> ignore
        sb.ToString()

    /// return all matches on input
    override this.Matches(input) =
        use allResults = this.llmatch_all input
        let arr = Array.zeroCreate allResults.size
        let resultSpan = ValueList.toSpan allResults
        let mutable i = 0
        while i < allResults.size do
            let curr = resultSpan[i]
            let vslice = input.Slice(curr.Index, curr.Length)
            let newResult = {
                Value = vslice.ToString()
                Index = curr.Index
                Length = curr.Length
            }
            arr[i] <- newResult
            i <- i + 1
        arr

    override this.Count(input: ReadOnlySpan<char>) =
        use matches = this.llmatch_all input
        matches.size

    /// initialize regex in DFA
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member internal this.GetOrCreateState(node: RegexNodeId) : MatchState<'t> =
        _getOrCreateState (node, false)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.GetDeltaOffset(stateId: TState, mintermId: TMinterm) : TState =
        I.bor (I.shl stateId _mintermsLog) mintermId

    member this.TryNextDerivative
        (currentState: TState, mintermId: TMinterm, loc: LocationKind)
        =
        let minterm = _cache.MintermById(int mintermId)

        let targetState =
            this.GetOrCreateState(
                this.CreateDerivative(loc, minterm, _stateArray[int currentState].Node)
            )

        targetState.Id


#if DEBUG
    // used in tests
    member internal this.GetStateAndFlagsById(stateId: int) = _stateArray[stateId]
    member internal this.MT_LOOKUP() = _mtlookup
#endif

    member this.TakeAnchorTransitionEnd(currentState: byref<TState>, mtId: TMinterm) =
        let dfaOffset = this.GetDeltaOffset(currentState, mtId)
        let cachedStateId = _revStartStates[int dfaOffset]

        if cachedStateId > zero then
            currentState <- cachedStateId
        else
            let nextState = this.TryNextDerivative(currentState, mtId, LocationKind.End)
            _revStartStates[int dfaOffset] <- nextState
            currentState <- nextState


    /// already confirmed can be nullable
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.StateIsNullable2(flags: StateFlags, loc: LocationKind, stateId: int) : bool =
        (StateFlags.isAlwaysNullable flags
         ||
         // important: prevent unnecessary work if anchor can not be nullable
         (if LocationKind.Center <> loc then
              RegexNode.isNullable (_b, loc, _stateArray[stateId].Node)
          else
              false))

    member this.HandleInputEnd
        (flags: StateFlags, stateId: byref<int>, input: ReadOnlySpan<char>, acc: byref<ValueList<int>>)
        : int =
        let isNullableInEnd =
            (StateFlags.isAlwaysNullable flags
             || RegexNode.isNullable (_b, LocationKind.End, _stateArray[stateId].Node))

        if
            not (
                isNullableInEnd || _b.Info(_stateArray[stateId].Node).NodeFlags.DependsOnAnchor
            )
        then
            input.Length
        else

            let mutable pos = input.Length

            if isNullableInEnd then
                if StateFlags.isPendingNullable flags then
                    this.AddPendingRev(stateId, &acc, pos)
                else
                    ValueList.add(&acc, pos)


            if pos > 0 then
                this.TakeAnchorTransitionEnd(&stateId, _mtlookup[int input[pos - 1]])
                pos <- pos - 1


            if pos <> 0 then
                if I.isNull _nullKindArray stateId then
                    I.setNullFull _stateArray &acc _nullKindArray stateId pos

            else
                let isReallyNull =
                    (StateFlags.isAlwaysNullable flags
                     || RegexNode.isNullable (
                         _b,
                         LocationKind.Begin,
                         _stateArray[stateId].Node
                     ))

                if isReallyNull then
                    match (# "ldelem.u1" _nullKindArray stateId : NullKind #) with
                    | NullKind.CurrentNull
                    | NullKind.PrevNull as nk -> ValueList.add(&acc, int nk)
                    | _ ->
                        let span = _stateArray[stateId].PendingNullablePositions

                        if isNull span || span.Length = 0 then
                            ValueList.add(&acc, 0)
                        else
                            for i = span.Length - 1 downto 0 do
                                let struct (s, e) = span[i]

                                for i = int e downto int s do
                                    ValueList.add(&acc, i + pos)


            pos

    /// zero length strings need a special case for anchors
    member this.HandleZeroLengthString() : ValueList<ValueMatch> =
        let mutable vlist = new ValueList<ValueMatch>(1)

        if StateFlags.canBeNullable _flagsArray[DFA_TR_REV] then
            let ms = ValueMatch(0, 0)
            vlist.Add(ms)

        vlist

    member this.HandleInputStart(stateId: int, acc: byref<ValueList<int>>) : unit =
        let node = _stateArray[stateId].Node

        let isReallyNull =
            StateFlags.isAlwaysNullable _flagsArray[stateId]
            || RegexNode.isNullable (_b, LocationKind.Begin, node)

        if
            isReallyNull
            && (not (
                if acc.size = zero then
                    false
                else
                    acc.pool[acc.size - 1] = 0
            ))
        then
            match (# "ldelem.u1" _nullKindArray stateId : NullKind #) with
            | NullKind.CurrentNull
            | NullKind.PrevNull as nk -> ValueList.addChecked(&acc, int nk)
            | _ ->
                let span = _stateArray[stateId].PendingNullablePositions

                if isNull span || span.Length = 0 then
                    ValueList.add(&acc, 0)
                else
                    let struct (fs, _) = span[0]

                    if acc.size = 0 || not (ValueList.endsWith(acc, int fs)) then
                        for i = span.Length - 1 downto 0 do
                            let struct (s, e) = span[i]

                            for i = int e downto int s do
                                ValueList.add(&acc, i)


    member this.IsNullable(loc: LocationKind, node: RegexNodeId) : bool =
        RegexNode.isNullable (_b, loc, node)

    member this.CreateDerivative<'tchar when 'tchar: struct>
        (loc: LocationKind, loc_pred: 't, node: RegexNodeId)
        : RegexNodeId =
        let canonNode = node
        RegexNode.derivative (_b, loc, loc_pred, canonNode)


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.end_first(mt_log: byte, input: ReadOnlySpan<char>, currentStateId: int) : int =
        let mutable currentStateId = currentStateId
        let mutable currentMax = -2
        let mutable l_pos = 0

        if l_pos = input.Length then
            this.HandleInputEndFwd(&currentMax, l_pos, currentStateId)
            currentStateId <- States.DFA_DEAD

        while currentStateId <> States.DFA_DEAD do
            if
                (# "clt.un" (# "ldelem.u1" _nullKindArray currentStateId : NullKind #) NullKind.NotNull : bool #)
            then
                currentStateId <- States.DFA_DEAD
                currentMax <- l_pos
            else

                let mutable nextStateId =
                    I.nextStateId _dfaDelta currentStateId mt_log _mtlookup input l_pos

                if nextStateId = 0 then
                    if this.deriv_ptr (currentStateId, &nextStateId, input, l_pos) then
                        ()

                currentStateId <- nextStateId

                l_pos <- (# "add" l_pos 1 : int #)

                if l_pos = input.Length then
                    if StateFlags.canBeNullable _flagsArray[currentStateId] then
                        currentMax <- l_pos

                    currentStateId <- States.DFA_DEAD

        currentMax


    member this.end_lazy
        (mt_log: byte, startPos: int, input: ReadOnlySpan<char>, currentStateId: int)
        : int32 =
        let mutable currentStateId = currentStateId
        let mutable currentMax = -2
        let endPos = input.Length
        let mutable l_pos = startPos

        if l_pos = endPos then
            this.HandleInputEndFwd(&currentMax, l_pos, currentStateId)
            currentStateId <- States.DFA_DEAD

        while currentStateId <> States.DFA_DEAD do
            if
                StateFlags.canSkipLeftToRight _flagsArray[currentStateId]
                && match
                    MintermSearchValues.nextIndexLeftToRight (
                        _svArray[currentStateId],
                        input.Slice(l_pos)
                    )
                   with
                   | -1 ->
                       this.HandleInputEndFwd(&currentMax, endPos, currentStateId)
                       currentStateId <- States.DFA_DEAD
                       true
                   | si ->
                       l_pos <- l_pos + si
                       si <> 0
            then
                if l_pos = endPos then
                    this.HandleInputEndFwd(&currentMax, l_pos, currentStateId)
                    currentStateId <- States.DFA_DEAD
            else

                if
                    (# "clt.un" (# "ldelem.u1" _nullKindArray currentStateId : NullKind #) NullKind.NotNull : bool #)
                then
                    match (# "ldelem.u1" _nullKindArray currentStateId : NullKind #) with
                    | NullKind.CurrentNull
                    | NullKind.PrevNull as nk -> currentMax <- I.sub l_pos nk
                    | _ -> this.set_null_fwd_fallback (&currentMax, l_pos, currentStateId)

                let mutable nextStateId =
                    I.nextStateId _dfaDelta currentStateId mt_log _mtlookup input l_pos

                if nextStateId = 0 then
                    if this.deriv_ptr (currentStateId, &nextStateId, input, l_pos) then
                        ()

                currentStateId <- nextStateId

                l_pos <- (# "add" l_pos 1 : int #)

                if l_pos = endPos then
                    this.HandleInputEndFwd(&currentMax, l_pos, currentStateId)
                    currentStateId <- States.DFA_DEAD

        currentMax

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]

    member this.TrySkipInitialRevChar
        (input: ReadOnlySpan<char>, pos: byref<int>, currentStateId: byref<int>)
        : bool =
        match utf16Optimizations.RightToLeftInitial with
        | InitialAccelerator.SearchValuesPrefix(prefix, transitionNodeId) ->
            let skipResult =
                Accelerators.trySkipToWeightedSetCharRev (
                    input,
                    pos,
                    utf16Optimizations.RightToLeftWeightedSets.Value
                )

            match skipResult with
            | ValueSome resultEnd ->
                let suffixStart = resultEnd - prefix.Length
                currentStateId <- transitionNodeId
                pos <- suffixStart
                pos <> resultEnd
            | ValueNone -> // no matches remaining
                pos <- 0
                true
        | InitialAccelerator.SingleSearchValues(prefix) ->
            if MintermSearchValues.contains (prefix, input[pos - 1]) then
                false
            else
                let span = input.Slice(0, pos - 1)
                let skipResult = MintermSearchValues.nextIndexRightToLeft (prefix, span)

                match skipResult with
                | -1 -> // no matches remaining
                    pos <- 0
                    true
                | resultEnd ->
                    let nextPos = resultEnd + 1
                    let cond = pos <> nextPos
                    pos <- nextPos
                    cond
        | InitialAccelerator.SingleSearchValuesPrefix(prefix, transitionNodeId) ->
            let prev = pos - 1

            if MintermSearchValues.contains (prefix, input[prev]) then
                pos <- prev
                currentStateId <- transitionNodeId
                true
            else

                let span = input.Slice(0, prev)
                let skipResult = MintermSearchValues.nextIndexRightToLeft (prefix, span)

                match skipResult with
                | -1 -> // no matches remaining
                    pos <- 0
                    true
                | resultEnd ->
                    let cond = pos <> resultEnd
                    currentStateId <- transitionNodeId
                    pos <- resultEnd
                    cond
        | InitialAccelerator.SearchValuesPotentialStart _ ->
            let skipResult =
                Accelerators.trySkipToWeightedSetCharRev (
                    input,
                    pos,
                    utf16Optimizations.RightToLeftWeightedSets.Value
                )

            match skipResult with
            | ValueSome resultEnd ->
                let cnd = pos <> resultEnd
                pos <- resultEnd
                cnd
            | ValueNone -> // no matches remaining
                pos <- 0
                true
        | InitialAccelerator.StringPrefix(prefix, transitionNodeId) ->
            let slice = input.Slice(0, pos)
            let resultStart = slice.LastIndexOf prefix.Span

            if resultStart = -1 then
                pos <- 0
                true
            else
                currentStateId <- transitionNodeId
                pos <- resultStart
                true
        | InitialAccelerator.StringPrefixCaseIgnore(prefix, _, transitionNodeId) ->
            let mutable resultStart = pos
            let mutable found = false
            let lastChar = prefix.Span[prefix.Length - 1]
            let lowChar = Char.ToLowerInvariant lastChar
            let upChar = Char.ToUpperInvariant lastChar
            let prefixSpan = prefix.Span.Slice(0, prefix.Length - 1)
            let textSpan = input

            while not found do
                let mutable slice = textSpan.Slice(0, resultStart)
                resultStart <- slice.LastIndexOfAny(lowChar, upChar)

                if resultStart = -1 then
                    found <- true
                else
                    slice <- textSpan.Slice(0, resultStart)

                    match slice.EndsWith(prefixSpan, StringComparison.OrdinalIgnoreCase) with
                    | true ->
                        resultStart <- resultStart - prefix.Span.Length + 1
                        found <- true
                    | _ -> ()

            if resultStart = -1 then
                pos <- 0
                true
            else
                currentStateId <- transitionNodeId
                pos <- resultStart
                true
        | _ -> false


    member this.skip_active_rev
        (input: ReadOnlySpan<char>, pos: byref<int>, currentStateId: int, acc: byref<ValueList<int>>)
        : bool =
        if MintermSearchValues.contains (_svArray[currentStateId], input[pos - 1]) then
            false
        else

            let tmp_loc = pos
            let slice = input.Slice(0, pos)

            match
                MintermSearchValues.nextIndexRightToLeft (_svArray[currentStateId], slice)
            with
            | -1 -> pos <- 0
            | n -> pos <- n + 1
            // adding all skipped locations
            if tmp_loc > pos then
                let rstart = pos + 1
                let rend = tmp_loc

                if I.clt_un (I.ldelemu1 _nullKindArray currentStateId) NullKind.NotNull then
                    for i = rend - 1 downto rstart do
                        ValueList.add(&acc, i)

                true
            else
                false


    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.HandleInputEndFwd
        (currentMax: byref<int>, currPosition: int, currentStateId: int)
        =
        if StateFlags.canBeNullable _flagsArray[currentStateId] then
            let flags = _flagsArray[currentStateId]

            if StateFlags.isAnchorNonPending flags then
                currentMax <- currPosition
            else
                match (# "ldelem.u1" _nullKindArray currentStateId : NullKind #) with
                | NullKind.CurrentNull
                | NullKind.PrevNull as nk -> currentMax <- I.sub currPosition nk
                | _ ->
                    currentMax <-
                        max
                            currentMax
                            (currPosition - _stateArray[currentStateId].MinPendingNullable)


    member this.set_null_fwd_fallback
        (currentMax: byref<int>, currPosition: int, currentStateId: TState)
        =
        let flags = _flagsArray[currentStateId]

        if StateFlags.isPendingNullable flags then
            currentMax <-
                max currentMax (currPosition - _stateArray[currentStateId].MinPendingNullable)
        else
            currentMax <- currPosition


    // this is just to get the branch out of the hot loop

    member this.rev_deriv
        (currentStateId: TState, input: ReadOnlySpan<char>, realPos: int)
        : TState =
        let mintermId = _mtlookup[int input[realPos]]
        let nextState = createTransition LocationKind.Center currentStateId mintermId
        nextState

    /// returns true if reference changed
    member this.deriv_ptr
        (
            currentStateId: TState,
            nextStateId: byref<TState>,
            inputptr: ReadOnlySpan<char>,
            realPos: int
        ) : bool =
        // let ch = (# "ldind.u2" (# "add" inputptr (realPos * 2) : int #) : int #)
        let ch = int inputptr[realPos]
        let mintermId = _mtlookup[ch]
        let oldref = _nullKindArray
        nextStateId <- createTransition LocationKind.Center currentStateId mintermId
        not (obj.ReferenceEquals(oldref, _nullKindArray))


    [<MethodImpl(MethodImplOptions.NoInlining)>]

    member this.collect_skip
        (acc: byref<ValueList<int>>, input: ReadOnlySpan<char>, startPos: int, startStateId: int)
        : int =
        let mutable currentStateId = startStateId
        let mutable l_pos = startPos
        let l_mtlog = _mintermsLog

        while l_pos <> 0 do
            let successfulSkip =
                (I.clt_un (I.ldelemu1 _skipKindArray currentStateId) SkipKind.NotSkip)
                && match (# "ldelem.u1" _skipKindArray currentStateId : SkipKind #) with
                   | SkipKind.SkipInitial ->
                       this.TrySkipInitialRevChar(input, &l_pos, &currentStateId)
                   | _ -> this.skip_active_rev (input, &l_pos, currentStateId, &acc)

            if successfulSkip then
                if I.isNull _nullKindArray currentStateId then
                    I.setNullFull _stateArray &acc _nullKindArray currentStateId l_pos
            else

                l_pos <- l_pos - 1

                let mutable nextStateId =
                    I.nextStateId _dfaDelta currentStateId l_mtlog _mtlookup input l_pos

                if nextStateId = 0 then
                    nextStateId <- this.rev_deriv (currentStateId, input, l_pos)

                currentStateId <- nextStateId

                if I.isNull _nullKindArray currentStateId then
                    I.setNullFull _stateArray &acc _nullKindArray currentStateId l_pos


        currentStateId


    member this.AddPendingRev(currentStateId: int, acc: byref<ValueList<int>>, realPos: int) =
        let span = _stateArray[currentStateId].PendingNullablePositions

        for i = span.Length - 1 downto 0 do
            let struct (s, e) = span[i]

            for i = int e downto int s do
                ValueList.add(&acc, i + realPos)


    member this.AddPendingFwd(currentStateId: int, realPos: int, currentMax: byref<int>) =
        let span = _stateArray[currentStateId].PendingNullablePositions
        let struct (s, _) = span[0]
        currentMax <- max currentMax (realPos - int s)


    override this.FirstEnd(input: ReadOnlySpan<char>) =
        let mt_log = _mintermsLog

        match this.end_first (mt_log, input, DFA_R_NOPR) with
        | -2 -> -1
        | n -> n


    override this.LongestEnd(input: ReadOnlySpan<char>) =
        let mt_log = _mintermsLog

        match this.end_lazy (mt_log, 0, input, DFA_R_NOPR) with
        | -2 -> -1
        | n -> n

    
    member internal this.llmatch_all_override
        (
            acc: byref<ValueList<ValueMatch>>,
            input: ReadOnlySpan<char>,
            overridden: MatchOverride<char>
        ) : ValueList<ValueMatch> =
        let tspan = input

        match overridden with
        | MatchOverride.FixedLengthString s ->
            let pspan = s.Span
            let mutable looping = true
            let mutable currPos = 0
            let textLength = s.Length

            while looping do
                // LastIndexOf with ignore case is slow as of net9.0
                // once there is a vectorized version, this should be updated
                match tspan.Slice(currPos).IndexOf(pspan, StringComparison.Ordinal) with
                | -1 -> looping <- false
                | n ->
                    let start = currPos + n

                    acc.Add(ValueMatch(start, textLength))

                    currPos <- start + textLength
        | MatchOverride.FixedLengthStringCaseIgnore(s) ->
            let pspan = s.Span
            let mutable looping = true
            let mutable currPos = 0
            let textLength = s.Length
            // ascii in .net is implicitly vectorized - use it if possible
            while looping do
                match
                    tspan.Slice(currPos).IndexOf(pspan, StringComparison.OrdinalIgnoreCase)
                with
                | -1 -> looping <- false
                | n ->
                    let start = currPos + n

                    acc.Add(ValueMatch(start, textLength))

                    currPos <- start + textLength
        | MatchOverride.NonAsciiFixedLengthStringCaseIgnore(head, s) ->
            let pspan = s.Span
            let mutable looping = true
            let mutable currPos = 0
            let textLength = s.Length

            while looping do
                match tspan.Slice(currPos).IndexOfAny(head) with
                | -1 -> looping <- false
                | n when
                    tspan
                        .Slice(currPos + n)
                        .StartsWith(pspan, StringComparison.OrdinalIgnoreCase)
                    ->
                    let start = currPos + n

                    acc.Add(ValueMatch(start, textLength))

                    currPos <- start + textLength
                | n -> currPos <- currPos + n + 1

        acc


    /// see: `LengthLookup`
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    member this.llmatch_ends_skip
        (matches: byref<ValueList<ValueMatch>>, acc: byref<ValueList<int>>, input: ReadOnlySpan<char>)
        =
        let mutable nextValidStart = 0
        let mutable startSpans = ValueList.toSpan acc
        let mutable pos = 0
        let mt_log = _mintermsLog

        let offset, startState =
            match utf16Optimizations.LengthLookup with
            | LengthLookup.FixedLengthPrefixMatchEnd(fl, stateId) ->
                pos <- pos + fl
                fl, stateId
            | _ -> 0, DFA_R_NOPR

        let mutable i = startSpans.Length

        while i <> 0 do
            i <- i - 1
            let currStart = startSpans[i]

            if currStart >= nextValidStart then
                pos <- currStart + offset

                let matchEnd = this.end_lazy (mt_log, pos, input, startState)

                matches.Add(ValueMatch(currStart, matchEnd - currStart))
                assert (matchEnd <> -2)
                nextValidStart <- matchEnd


    /// see: `LengthLookup`
    member this.llmatch_ends_remaining_set
        (
            matches: byref<ValueList<ValueMatch>>,
            acc: byref<ValueList<int>>,
            input: ReadOnlySpan<char>,
            prefixlen: int,
            mtId: TMinterm,
            remaining: byte
        ) : unit =
        let startSpans = ValueList.toSpan acc
        let mutable i = startSpans.Length
        let mutable longest = input.Length - prefixlen
        let mutable l_pos = 0
        let mutable counter = 0uy
        let l_mtid = mtId

        while i <> 0 do
            i <- i - 1

            if startSpans[i] >= l_pos then
                let currStart = startSpans[i]

                if I.clt_un (longest - currStart) remaining then
                    counter <- byte (longest - currStart)
                else
                    counter <- remaining

                l_pos <- I.add (currStart + prefixlen) counter

                while (counter <> 0uy
                       && I.mintermId _mtlookup input (I.sub l_pos counter) = l_mtid) do
                    counter <- I.sub counter 1uy

                l_pos <- I.sub l_pos counter
                matches.Add(ValueMatch(currStart, l_pos - currStart))


    /// see: `LengthLookup`
    member this.llmatch_ends_setlookup_mt
        (
            matches: byref<ValueList<ValueMatch>>,
            acc: byref<ValueList<int>>,
            input: ReadOnlySpan<char>,
            prefixlen: int,
            mtId: TMinterm,
            nk: NullKind
        ) : unit =
        assert (nk <> NullKind.PendingNull)
        let startSpans = ValueList.toSpan acc
        let mutable i = acc.size
        let mutable l_pos = 0

        while i <> 0 do
            i <- i - 1

            if startSpans[i] >= l_pos then
                let currStart = startSpans[i]
                l_pos <- currStart + prefixlen

                while (I.mintermId _mtlookup input l_pos <> mtId) do
                    l_pos <- l_pos + 1

                l_pos <- (I.sub l_pos nk) + 1
                matches.Add(ValueMatch(currStart, l_pos - currStart))

    /// see: `LengthLookup`
    member this.llmatch_ends_fixlen
        (matches: byref<ValueList<ValueMatch>>, acc: byref<ValueList<int>>, len: int)
        : unit =
        let startSpans = ValueList.toSpan acc
        let mutable i = startSpans.Length
        let mutable l_pos = 0

        while i <> 0 do
            i <- i - 1

            if startSpans[i] >= l_pos then
                let currStart = startSpans[i]
                l_pos <- currStart + len
                matches.Add(ValueMatch(currStart, len))


    [<MethodImpl(MethodImplOptions.NoInlining)>]
    member this.llmatch_collect
        (input: ReadOnlySpan<char>, acc: byref<ValueList<int>>, initState: int, startPosition: int)
        =
        let endStateId = this.collect_skip (&acc, input, startPosition, initState)
        this.HandleInputStart(endStateId, &acc)

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    member this.llmatch_ends
        (matches: byref<ValueList<ValueMatch>>, acc: byref<ValueList<int>>, input: ReadOnlySpan<char>)
        =
        match utf16Optimizations.LengthLookup with
        | LengthLookup.SetLookup(prefixLength, mtId, skipKind, nullKind, mintermSearchValues) when
            skipKind = SkipKind.NotSkip && nullKind <> NullKind.PendingNull
            ->
            this.llmatch_ends_setlookup_mt (&matches, &acc, input, prefixLength, mtId, nullKind)
        | LengthLookup.RemainingSets(prefixLength, mtId, remaining) ->
            this.llmatch_ends_remaining_set (
                &matches,
                &acc,
                input,
                prefixLength,
                mtId,
                remaining
            )
        | LengthLookup.FixedLength(n) -> this.llmatch_ends_fixlen (&matches, &acc, n)
        | _ -> this.llmatch_ends_skip (&matches, &acc, input)


    [<MethodImpl(MethodImplOptions.NoInlining)>]
    member this.llmatch_all(input: ReadOnlySpan<char>) : ValueList<ValueMatch> =
        if input.Length = 0 then
            this.HandleZeroLengthString()

        else
            match utf16Optimizations.MatchOverride with
            | ValueSome regOverride ->
                let mutable matches = new ValueList<ValueMatch>(128)
                this.llmatch_all_override (&matches, input, regOverride)
            | _ ->
                let mutable matches = new ValueList<ValueMatch>(64)
                use mutable acc = new ValueList<int>(16)
                let mutable initState = DFA_TR_REV

                let startPosition =
                    this.HandleInputEnd(_flagsArray[initState], &initState, input, &acc)
                this.llmatch_collect (input, &acc, initState, startPosition)
                this.llmatch_ends (&matches, &acc, input)
                matches


    /// return just the positions of matches without allocating the result
    override this.ValueMatches(input: ReadOnlySpan<char>) : ValueList<ValueMatch> =
        this.llmatch_all input

    // accessors
    member internal _.TrueStarredPattern = trueStarredNode
    member internal _.ReverseTrueStarredPattern = reverseTrueStarredNode
    member internal _.RawPattern = R_canonical

    member _.PrettyPrintNode(node) =
        let bddNode =
            Minterms.transformBack
                (_cache.MtsBDD())
                _cache.Builder
                _cache.BddBuilder
                _cache.Solver
                _cache.CharsetSolver
                node

        Types.Helpers.printNode _cache.CharsetSolver _cache.BddBuilder.Resolve bddNode

    member _.PrettyPrintMinterm(tset: 't) : string =
        let bdd = _cache.Solver.convertToBdd (_cache.CharsetSolver, _cache.MtsBDD(), tset)
        BDD.prettyPrintBDD (_cache.CharsetSolver: CharSetSolver) (bdd)

    member _.GetBddNode(node) =
        let bddNode =
            Minterms.transformBack
                (_cache.MtsBDD())
                _cache.Builder
                _cache.BddBuilder
                _cache.Solver
                _cache.CharsetSolver
                node

        bddNode

    member val RawPatternWithoutLookback = _stateArray[DFA_R_NOPR].Node
    member val ReversePattern = reverseNode
    member val RevStartStateId = DFA_TR_REV
    member val Cache = _cache
    member val IsFullDFA = fullDfa

    member internal this.StateArray = _stateArray |> Array.take (_stateCache.Count + 1)
    member internal this.InternalOptimizations = utf16Optimizations


module internal Helpers =
    let rec createMatcher
        (
            bddBuilder: RegexBuilder<BDD>,
            bddMinterms: BDD array,
            charsetSolver,
            converter,
            symbolicBddnode,
            options
        ) : GenericRegexMatcher<char> =
        match bddMinterms.Length with
        | n when n <= 64 ->
            let solver = UInt64Solver(bddMinterms)

            let uintbuilder = RegexBuilder(converter, solver, charsetSolver, options)

            let rawNode =
                Minterms.transform bddBuilder uintbuilder charsetSolver solver symbolicBddnode

            let cache =
                RegexCache<uint64>(
                    solver,
                    charsetSolver,
                    bddMinterms,
                    _rawPattern = rawNode,
                    _builder = uintbuilder,
                    _bddbuilder = bddBuilder
                )

            let m = RegexMatcher(rawNode, cache, options)

            if m.RawPattern <> rawNode then
                let backToBdd =
                    Minterms.transformBack
                        bddMinterms
                        uintbuilder
                        bddBuilder
                        solver
                        charsetSolver
                        m.RawPattern

                let recomputedMinterms = Minterms.compute charsetSolver bddBuilder backToBdd

                if recomputedMinterms.Length = bddMinterms.Length then
                    m
                else
                    createMatcher (
                        bddBuilder,
                        recomputedMinterms,
                        charsetSolver,
                        converter,
                        backToBdd,
                        options
                    )
            else

                m

        | _ ->
            // ideally subsume the minterms to 64 or below
            // but in case that is not possible, fall back to a bitvector implementation 
            let solver = BitVectorSolver(bddMinterms)
            let tsetbuilder = RegexBuilder(converter, solver, charsetSolver, options)

            let rawNode =
                (Minterms.transform bddBuilder tsetbuilder charsetSolver solver)
                    symbolicBddnode

            let cache =
                RegexCache<BitVector>(
                    solver,
                    charsetSolver,
                    bddMinterms,
                    _rawPattern = rawNode,
                    _builder = tsetbuilder,
                    _bddbuilder = bddBuilder
                )

            let m = RegexMatcher(rawNode, cache, options)

            // only possible if you have more than 255 unique characters in the pattern.
            // there's no real problem to support this, but it's just 
            // exceedingly rare, so it'd feel wrong to 
            // pay the cost of supporting it in the common case
            if m.Cache.NumOfMinterms() > 255 then
                failwith "over 255 unique characters in the pattern not supported! simplify the pattern"

            m
