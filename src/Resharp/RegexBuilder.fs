namespace Resharp

open System
open System.Buffers
open System.Collections.Generic
open System.Globalization
open System.Runtime.CompilerServices
open Resharp.Runtime
open Resharp.Info
open Resharp.Patterns
open Resharp.Common
open Resharp.Types
open System.Linq
open System.Runtime.InteropServices
open System.Threading
open Resharp.Internal

type internal ResharpRegexNodeConverter(solver: CharSetSolver) =
    let _setBddCache = new Dictionary<string, BDD>()

    member this.MapCategoryCodeToCondition(code: UnicodeCategory) =
        if code = enum (int RegexCharClass.SpaceConst - 1) then
            UnicodeCategoryConditions.WhiteSpace
        else
            UnicodeCategoryConditions.GetCategory code

    member this.MapCategoryCodeSetToCondition(catCodes: HashSet<UnicodeCategory>) =
        let mutable result: BDD voption = ValueNone

        if
            (catCodes.Contains(UnicodeCategory.UppercaseLetter)
             && catCodes.Contains(UnicodeCategory.LowercaseLetter)
             && catCodes.Contains(UnicodeCategory.TitlecaseLetter)
             && catCodes.Contains(UnicodeCategory.ModifierLetter)
             && catCodes.Contains(UnicodeCategory.OtherLetter)
             && catCodes.Contains(UnicodeCategory.NonSpacingMark)
             && catCodes.Contains(UnicodeCategory.DecimalDigitNumber)
             && catCodes.Contains(UnicodeCategory.ConnectorPunctuation))
        then
            catCodes.Remove(UnicodeCategory.UppercaseLetter) |> ignore
            catCodes.Remove(UnicodeCategory.LowercaseLetter) |> ignore
            catCodes.Remove(UnicodeCategory.TitlecaseLetter) |> ignore
            catCodes.Remove(UnicodeCategory.ModifierLetter) |> ignore
            catCodes.Remove(UnicodeCategory.OtherLetter) |> ignore
            catCodes.Remove(UnicodeCategory.NonSpacingMark) |> ignore
            catCodes.Remove(UnicodeCategory.DecimalDigitNumber) |> ignore
            catCodes.Remove(UnicodeCategory.ConnectorPunctuation) |> ignore
            result <- ValueSome(UnicodeCategoryConditions.WordLetter(solver))

        for cat in catCodes do
            let cond = this.MapCategoryCodeToCondition(cat)

            result <-
                match result with
                | ValueNone -> ValueSome cond
                | ValueSome oldCond -> ValueSome(solver.Or(oldCond, cond))

        result.Value

    member this.Compute(set: string) =
        let conditions = ResizeArray()
        let negate = RegexCharClass.IsNegated(set)
        let ranges = RegexCharClass.ComputeRanges(set)

        if not (isNull ranges) then
            for first, last in ranges do
                let mutable bdd = solver.CreateBDDFromRange(first, last)

                if negate then
                    bdd <- solver.Not(bdd)

                conditions.Add(bdd)

        let setLength: int = int set[RegexCharClass.SetLengthIndex]

        let catLength: int = int set[RegexCharClass.CategoryLengthIndex]

        let catStart: int = int setLength + RegexCharClass.SetStartIndex

        let mutable i = catStart

        while (i < catStart + catLength) do
            let mutable categoryCode = int16 set[i]
            i <- i + 1

            if (categoryCode <> 0s) then
                let currCode = (Math.Abs(categoryCode) - 1s)

                let mutable cond = this.MapCategoryCodeToCondition(enum (int currCode))

                if (categoryCode < 0s <> negate) then
                    cond <- solver.Not(cond)

                conditions.Add(cond)
            else
                categoryCode <- int16 set[i]
                i <- i + 1

                if categoryCode = 0s then
                    ()
                else
                    let negatedGroup = categoryCode < 0s
                    let categoryCodes = HashSet<UnicodeCategory>()

                    while categoryCode <> 0s do
                        let currCode = (Math.Abs(categoryCode) - 1s)
                        categoryCodes.Add(enum (int currCode)) |> ignore
                        categoryCode <- int16 set[i]
                        i <- i + 1

                    let mutable bdd = this.MapCategoryCodeSetToCondition(categoryCodes)

                    if negate <> negatedGroup then
                        bdd <- solver.Not(bdd)

                    conditions.Add(bdd)

        let mutable subtractorCond: BDD = null

        if set.Length > i then
            subtractorCond <- this.CreateBDDFromSetString(set.Substring(i))

        let mutable result =
            if conditions.Count = 0 then
                if negate then solver.Empty else solver.Full
            else if negate then
                solver.And(CollectionsMarshal.AsSpan(conditions))
            else
                solver.Or(CollectionsMarshal.AsSpan(conditions))

        if not (isNull subtractorCond) then
            result <- solver.And(result, solver.Not(subtractorCond))

        result


    member this.CreateBDDFromSetString(set: string) =
        if not (StackHelper.TryEnsureSufficientExecutionStack()) then
            StackHelper.CallOnEmptyStack(this.CreateBDDFromSetString, set)
        else
            match _setBddCache.TryGetValue(set) with
            | true, v -> v
            | _ ->
                let result = this.Compute(set)
                _setBddCache.Add(set, result)
                result


module internal StartsetHelpers =

    let CHAR_LIMIT = uint32 UInt16.MaxValue

    let bddToStartsetChars(bdd: BDD) : PredStartset =
        let rcc = RegexCharClass()
        let mutable ranges = BDDRangeConverter.ToRanges(bdd)
        let mutable e = ranges.GetEnumerator()
        let mutable i = 0u

        use charArray = new PooledArray<char>(int CHAR_LIMIT)

        while e.MoveNext() do
            let struct (rs, re) = e.Current :?> struct (uint32 * uint32)

            rcc.AddRange(char rs, char re)

            for j = int rs to int re do
                charArray.Add(char j)
                i <- i + 1u

        let trimmed = charArray.AsArray()

        let ranges2 = PredStartset.Of(StartsetFlags.None, trimmed)

        ranges2

    let startsetsFromMintermArray(bdds: BDD array) : PredStartset array =
        let startsets = bdds[1..] |> Array.map bddToStartsetChars
        let searchChars = startsets |> Array.collect (_.Chars)
        let invertedStartset = PredStartset.Of(StartsetFlags.Inverted, searchChars)
        Array.append [| invertedStartset |] startsets

    let tryGetMintermChars
        (
            _char_buffer: char array,
            _solver: ISolver<'t>,
            predStartsetArray: PredStartset array,
            uintMinterms: 't array,
            startset: 't
        ) : Memory<char> =

        let mergedCharSpan = _char_buffer.AsSpan()
        let mutable totalLen = 0

        let shouldInvert = _solver.elemOfSet startset (uintMinterms[0])

        if shouldInvert then
            for i = 1 to predStartsetArray.Length - 1 do
                let pureMt = uintMinterms[i]

                match _solver.elemOfSet startset pureMt with
                | true -> ()
                | false ->
                    let targetSpan = mergedCharSpan.Slice(totalLen)
                    let pspan = predStartsetArray[i].Chars.AsSpan()
                    pspan.CopyTo(targetSpan)
                    totalLen <- totalLen + pspan.Length

            Memory(mergedCharSpan.Slice(0, totalLen).ToArray())

        else
            for i = 1 to predStartsetArray.Length - 1 do
                let pureMt = uintMinterms[i]

                match _solver.elemOfSet startset pureMt with
                | true ->
                    let targetSpan = mergedCharSpan.Slice(totalLen)
                    let pspan = predStartsetArray[i].Chars.AsSpan()
                    pspan.CopyTo(targetSpan)
                    totalLen <- totalLen + pspan.Length
                | false -> ()

            Memory(mergedCharSpan.Slice(0, totalLen).ToArray())


[<AutoOpen>]
module private BuilderHelpers =
    [<Flags>]
    type MkOrFlags =
        | None = 0uy
        | IsTrueStar = 1uy
        | ContainsEpsilon = 2uy

    [<Flags>]
    type MkAndFlags =
        | None = 0uy
        | IsFalse = 1uy
        | ContainsEpsilon = 2uy


[<NoComparison; NoEquality>]
type internal NodeKey<'t when 't :> IEquatable<'t> and 't: equality and 't :> IComparable<'t>>
    =
    | KSingleton of minterm: 't
    | KLoop of body: RegexNodeId * low: int * high: int
    | KConcat of head: RegexNodeId * tail: RegexNodeId
    | KOr of ornodes: Memory<RegexNodeId>
    | KAnd of andnodes: Memory<RegexNodeId>
    | KNot of inner: RegexNodeId
    | KLookaround of labody: RegexNodeId * lookBack: bool * rel: int * pending: RefSet

module internal NodeKey =
    let inline memEquals (xs: Memory<RegexNodeId>) (ys: Memory<RegexNodeId>) =
        xs.Span.SequenceEqual(ys.Span)

    let inline memHash(x: Memory<RegexNodeId>) =
        let mutable hash = 0

        for n in x.Span do
            hash <- hash ^^^ n

        hash

type internal NodeKeyComparer<'t
    when 't :> IEquatable<'t> and 't: equality and 't :> IComparable<'t>>() =
    interface IEqualityComparer<NodeKey<'t>> with
        member _.Equals(x, y) =
            match x, y with
            | KSingleton m1, KSingleton m2 -> EqualityComparer<'t>.Default.Equals(m1, m2)
            | KLoop(b1, l1, h1), KLoop(b2, l2, h2) -> b1 = b2 && l1 = l2 && h1 = h2
            | KConcat(h1, t1), KConcat(h2, t2) -> h1 = h2 && t1 = t2
            | KOr m1, KOr m2 -> NodeKey.memEquals m1 m2
            | KAnd m1, KAnd m2 -> NodeKey.memEquals m1 m2
            | KNot n1, KNot n2 -> n1 = n2
            | KLookaround(b1, lb1, r1, p1), KLookaround(b2, lb2, r2, p2) ->
                lb1 = lb2 && r1 = r2 && b1 = b2 && obj.ReferenceEquals(p1, p2)
            | _ -> false

        member _.GetHashCode(key) =
            match key with
            | KSingleton m -> EqualityComparer<'t>.Default.GetHashCode(m)
            | KLoop(b, l, h) -> HashCode.Combine(1, b, l, h)
            | KConcat(h, t) -> HashCode.Combine(2, h, t)
            | KOr nodes -> HashCode.Combine(3, NodeKey.memHash nodes)
            | KAnd nodes -> HashCode.Combine(4, NodeKey.memHash nodes)
            | KNot n -> HashCode.Combine(5, n)
            | KLookaround(b, _, r, p) ->
                HashCode.Combine(6, b, r, LanguagePrimitives.PhysicalHash p)


/// reuses nodes and ensures structural equality via indexed storage
[<Sealed>]
type internal RegexBuilder<'t
    when 't :> IEquatable<'t> and 't: equality and 't :> IComparable<'t>>
    (
        converter: ResharpRegexNodeConverter,
        solver: ISolver<'t>,
        bcss: CharSetSolver,
        options: ResharpOptions
    ) as b =

    let _nodes = ResizeArray<RegexNode<'t>>()
    let _infos = ResizeArray<RegexNodeInfo<'t>>()

    static let _createInfo
        (flags: NodeFlags)
        (containsMinterms: 't)
        (pendingNullables: RefSet)
        : RegexNodeInfo<'t> =
        RegexNodeInfo<'t>(
            NodeFlags = flags,
            SubsumedByMinterm = containsMinterms,
            PendingNullables = pendingNullables

        )

    let _nodeCache: Dictionary<NodeKey<'t>, RegexNodeId> =
        Dictionary(NodeKeyComparer<'t>())

    let _minLengthCache: Dictionary<RegexNodeId, int voption> = Dictionary()
    let _maxLengthCache: Dictionary<RegexNodeId, int voption> = Dictionary()

    let mutable _orCount = 0

    let _structPairRefEqComparer =
        { new IEqualityComparer<struct (RegexNodeId * RegexNodeId)> with
            member _.Equals(struct (a1, b1), struct (a2, b2)) = a1 = a2 && b1 = b2

            member _.GetHashCode(struct (a, b)) = HashCode.Combine(a, b)
        }

    let _charCache: Dictionary<char, 't> = Dictionary()

    let _canonicalCacheComparer: IEqualityComparer<struct (bool * bool * Memory<RegexNodeId>)> =
        { new IEqualityComparer<struct (bool * bool * Memory<RegexNodeId>)> with
            member this.Equals((dep1, n1, x1), (dep2, n2, x2)) =
                dep1 = dep2 && n1 = n2 && x1.Span.SequenceEqual(x2.Span)

            member this.GetHashCode((_, _, x)) =
                let mutable hash = 0

                for n in x.Span do
                    hash <- hash ^^^ n

                hash
        }

    let _canonicalSingletonCache: Dictionary<'t, RegexNodeId> = Dictionary()

    let _canonicalCache: Dictionary<struct (bool * bool * Memory<RegexNodeId>), RegexNodeId> =
        Dictionary(_canonicalCacheComparer)

    let _setAddCache = Dictionary<struct (rsint * RefSet), RefSet>()

    let _emptyRefSet = RefSet([||])

    let _zeroListRefSet =
        RefSet(
            [|
                (struct (LanguagePrimitives.GenericZero<rsint>,
                         LanguagePrimitives.GenericZero<rsint>))
            |]
        )

    let refSetRelUnionManyMin(min_rel: int, sets: ValueList<struct(int * RefSet)>) : RefSet =
        use mutable newInner = new ValueList<struct (rsint * rsint)>(16)

        for rel, vals in ValueList.toSpan sets do
            let offset = uint16 (rel - min_rel)

            for st, en in vals.inner do
                newInner.Add struct (st + offset, en + offset)


        newInner.AsSpan().Sort()

        if newInner.size = 0 then
            _emptyRefSet
        else

            use mutable mergedInner = new ValueList<struct (rsint * rsint)>(16)

            let mutable pendingEnd: rsint voption = ValueNone
            let mutable currStart = 0us

            for s, e in ValueList.toSpan newInner do
                match pendingEnd with
                // extend range
                | ValueSome pend when pend >= (s - 1us) -> pendingEnd <- ValueSome(max e pend)
                | ValueSome pend ->
                    // end current
                    mergedInner.Add(struct (currStart, pend))
                    // start new pending
                    currStart <- s
                    pendingEnd <- ValueSome e
                | ValueNone ->
                    currStart <- s
                    pendingEnd <- ValueSome e

            mergedInner.Add(struct (currStart, pendingEnd.Value))
            RefSet(mergedInner.ToArray())

    let refSetRelUnionMany(sets: ValueList<struct(int * RefSet)>) : RefSet =
        let mutable min_rel = Int32.MaxValue
        let setspan = sets.pool.AsSpan(0, sets.size)

        for rel, _ in setspan do
            min_rel <- min min_rel rel

        use mutable newInner = new ValueList<struct (rsint * rsint)>(16)

        for rel, vals in ValueList.toSpan sets do
            let offset = uint16 (rel - min_rel)

            for st, en in vals.inner do
                newInner.Add struct (st + offset, en + offset)

        newInner.AsSpan().Sort()

        if newInner.size = 0 then
            _emptyRefSet
        else

            use mutable mergedInner = new ValueList<struct (rsint * rsint)>(16)

            let mutable pendingEnd: rsint voption = ValueNone
            let mutable currStart = zero

            for s, e in ValueList.toSpan newInner do
                match pendingEnd with
                // extend range
                | ValueSome pend when pend >= (s - 1us) -> pendingEnd <- ValueSome(max e pend)
                | ValueSome pend ->
                    // end current
                    mergedInner.Add(struct (currStart, pend))
                    // start new pending
                    currStart <- s
                    pendingEnd <- ValueSome e
                | ValueNone ->
                    currStart <- s
                    pendingEnd <- ValueSome e

            mergedInner.Add(struct (currStart, pendingEnd.Value))
            RefSet(mergedInner.ToArray())

    let refSetUnionMany(sets: RefSet seq) : RefSet =
        use mutable shr = new ValueList<struct(int * RefSet)>(128)

        for s in sets do
            if s.IsEmpty then () else shr.Add(struct (0, s))

        match shr.size with
        | 0 -> _emptyRefSet
        | 1 -> let struct (_, v) = shr.AsSpan()[0] in v
        | _ -> refSetRelUnionMany shr

    let refSetUnion (arg1: RefSet) (arg2: RefSet) : RefSet =
        match arg1.IsEmpty, arg2.IsEmpty with
        | true, true -> _emptyRefSet
        | true, _ -> arg2
        | _, true -> arg1
        | _ ->
            refSetUnionMany (
                seq {
                    arg1
                    arg2
                }
            )

    let refSetAddAll (addBy: rsint) (arg: RefSet) : RefSet =
        if addBy = LanguagePrimitives.GenericZero then
            arg
        else

            let key = struct (addBy, arg)

            match _setAddCache.TryGetValue(key) with
            | true, v -> v
            | _ ->
                if
                    int addBy + (arg.inner |> Array.last |> (fun struct (_, e) -> int e))
                    >= int rsint.MaxValue
                then
                    failwith
                        "Maximum allowed lookaround context length reached! Compile RE# with longer context if this is intended"

                let updated = [|
                    for s, e in arg.inner do
                        yield struct (s + addBy, e + addBy)
                |]

                let updated = RefSet(updated)
                _setAddCache.Add(key, updated)
                updated


    let _registerNode (node: RegexNode<'t>) (info: RegexNodeInfo<'t>) : RegexNodeId =
        let id = _nodes.Count
        _nodes.Add(node)
        _infos.Add(info)
        id

    // register well-known nodes in fixed order matching RegexNodeId constants
    do
        let bot =
            _registerNode
                (RegexNode.Singleton(solver.Empty))
                (_createInfo NodeFlags.None solver.Empty _emptyRefSet)

        let eps =
            _registerNode
                (RegexNode.Loop(bot, 0, Int32.MaxValue))
                (_createInfo
                    (NodeFlags.IsAlwaysNullableFlag ||| NodeFlags.CanBeNullableFlag)
                    solver.Full
                    _emptyRefSet)

        let top =
            _registerNode
                (RegexNode.Singleton(solver.Full))
                (_createInfo NodeFlags.None solver.Full _emptyRefSet)

        let trueStar =
            _registerNode
                (RegexNode.Loop(top, 0, Int32.MaxValue))
                (_createInfo
                    (NodeFlags.IsAlwaysNullableFlag ||| NodeFlags.CanBeNullableFlag)
                    solver.Full
                    _emptyRefSet)

        let truePlus =
            _registerNode
                (RegexNode.Loop(top, 1, Int32.MaxValue))
                (_createInfo NodeFlags.None solver.Full _emptyRefSet)

        let endAnchor =
            _registerNode
                RegexNode<'t>.End
                (_createInfo
                    (NodeFlags.DependsOnAnchorFlag ||| NodeFlags.CanBeNullableFlag)
                    solver.Full
                    _emptyRefSet)

        let beginAnchor =
            _registerNode
                RegexNode<'t>.Begin
                (_createInfo
                    (NodeFlags.DependsOnAnchorFlag ||| NodeFlags.CanBeNullableFlag)
                    solver.Full
                    _emptyRefSet)

        assert (RegexNodeId.BOT = bot)
        assert (RegexNodeId.EPS = eps)
        assert (RegexNodeId.TOP = top)
        assert (RegexNodeId.TOP_STAR = trueStar)
        assert (RegexNodeId.TOP_PLUS = truePlus)
        assert (RegexNodeId.BEGIN_ANCHOR = beginAnchor)
        assert (RegexNodeId.END_ANCHOR = endAnchor)

        _nodeCache.Add(KLoop(RegexNodeId.BOT, 0, Int32.MaxValue), RegexNodeId.EPS)
        _nodeCache.Add(KLoop(RegexNodeId.TOP, 0, Int32.MaxValue), RegexNodeId.TOP_STAR)
        _nodeCache.Add(KLoop(RegexNodeId.TOP, 1, Int32.MaxValue), RegexNodeId.TOP_PLUS)
        _nodeCache.Add(KSingleton solver.Full, RegexNodeId.TOP)
        _nodeCache.Add(KSingleton solver.Empty, RegexNodeId.BOT)

        // pre-seed length caches for leaf nodes
        _minLengthCache[RegexNodeId.EPS] <- ValueSome 0
        _maxLengthCache[RegexNodeId.EPS] <- ValueSome 0
        _minLengthCache[RegexNodeId.TOP] <- ValueSome 1
        _maxLengthCache[RegexNodeId.TOP] <- ValueSome 1
        _minLengthCache[RegexNodeId.BOT] <- ValueSome 1
        _maxLengthCache[RegexNodeId.BOT] <- ValueSome 1
        _minLengthCache[RegexNodeId.BEGIN_ANCHOR] <- ValueSome 0
        _maxLengthCache[RegexNodeId.BEGIN_ANCHOR] <- ValueSome 0
        _minLengthCache[RegexNodeId.END_ANCHOR] <- ValueSome 0
        _maxLengthCache[RegexNodeId.END_ANCHOR] <- ValueSome 0
        _minLengthCache[RegexNodeId.TOP_STAR] <- ValueSome 0
        _maxLengthCache[RegexNodeId.TOP_STAR] <- ValueNone
        _minLengthCache[RegexNodeId.TOP_PLUS] <- ValueSome 1
        _maxLengthCache[RegexNodeId.TOP_PLUS] <- ValueNone

    // instances
    let _uniques = {|
        _wordChar = lazy b.setFromCharClass RegexCharClass.WordClass
        _nonWordChar = lazy b.setFromCharClass RegexCharClass.NotWordClass
    |}


    let _anchors =
        let nonWordLeft =
            lazy
                let body = b.mkOr2 (RegexNodeId.BEGIN_ANCHOR, _uniques._nonWordChar.Value)

                b.mkLookaround (body, lookBack = true, rel = 0, pendingNullable = _emptyRefSet)

        let wordLeft =
            lazy
                let body = b.mkOr2 (RegexNodeId.BEGIN_ANCHOR, _uniques._wordChar.Value)

                b.mkLookaround (body, true, 0, _emptyRefSet)

        let nonWordRight =
            lazy
                let body = b.mkOr2 (RegexNodeId.END_ANCHOR, _uniques._nonWordChar.Value)

                b.mkLookaround (body, false, 0, _emptyRefSet)

        let wordRight =
            lazy
                let body = b.mkOr2 (RegexNodeId.END_ANCHOR, _uniques._wordChar.Value)

                b.mkLookaround (body, false, 0, _emptyRefSet)

        {|
            _endZAnchor =
                lazy
                    let body =
                        b.mkOr2 (
                            RegexNodeId.END_ANCHOR, // either EOF
                            b.mkLookaround ( // or \nEOF
                                b.mkConcat2 (b.one '\n', RegexNodeId.END_ANCHOR),
                                false,
                                0,
                                _emptyRefSet
                            )
                        )

                    body
            _zAnchor = RegexNodeId.END_ANCHOR

            // \A ≡ (?<!_)
            _bigAAnchor = RegexNodeId.BEGIN_ANCHOR
            // __big_a_anchor
            // (?<=\A|\A\n) ≡ \a
            _aAnchor =
                lazy
                    let seqv = [|
                        RegexNodeId.BEGIN_ANCHOR
                        b.mkConcat2 (RegexNodeId.BEGIN_ANCHOR, b.one '\n')
                    |]

                    let node = b.mkOrSeq seqv
                    b.mkLookaround (node, true, 0, _emptyRefSet)
            // proper definition (?<=\A|\W)
            _nonWordLeft = nonWordLeft
            // (?<=\W)
            _wordLeft = wordLeft
            // (?=\W)
            // proper definition (?=\z|\W)
            _nonWordRight = nonWordRight
            _wordRight = wordRight
            // ^ ≡ \A|(?<=\n)
            _caretAnchor =
                lazy
                    let body = b.mkOr2 (RegexNodeId.BEGIN_ANCHOR, b.one '\n')
                    b.mkLookaround (body, true, 0, _emptyRefSet)
            // $ ≡ \z|(?=\n)
            _dollarAnchor =
                lazy
                    let body = b.mkOr2 (RegexNodeId.END_ANCHOR, b.one '\n')
                    b.mkLookaround (body, false, 0, _emptyRefSet)
        |}

    let overriddenForAscii =
        dict [|
            // this is to have better string literal performance in ascii texts
            RegexCharClass.CharsToStringClass("KkK"), RegexCharClass.CharsToStringClass("Kk")
            RegexCharClass.DigitClass, RegexCharClass.ECMADigitClass
            RegexCharClass.WordClass, RegexCharClass.ECMAWordClass
        |]

    member val uniques = _uniques
    member val anchors = _anchors

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.Node(id: RegexNodeId) : RegexNode<'t> = _nodes[id]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.Info(id: RegexNodeId) : RegexNodeInfo<'t> = _infos[id]

    member inline _.Resolve: RegexNodeId -> RegexNode<'t> = fun id -> _nodes[id]

    member inline _.GetFlags: RegexNodeId -> NodeFlags = fun id -> _infos[id].NodeFlags

    member val Solver = solver
    member this.OrCount = _orCount

    member val emptyRefSet = _emptyRefSet
    member val zeroListRefSet = _zeroListRefSet
    member _.RefSetUnion(arg1, arg2) = refSetUnion arg1 arg2
    member _.RefSetUnionMany(sets) = refSetUnionMany sets
    member _.RefSetRelUnionManyMin(min_rel, sets) = refSetRelUnionManyMin (min_rel, sets)
    member _.RefSetAddAll(addBy, arg) = refSetAddAll addBy arg

    // pools for temporary allocations during node construction, to reduce GC pressure.
    // this could be refactored to be more fine-grained and efficient
    
    member val pool_orGroupedHeads =
        ObjectPool<_>(
            (fun _ ->
                Dictionary<RegexNodeId, PooledArray<struct (RegexNodeId * RegexNodeId)>>()
            ),
            1
        )

    member val pool_orGroupedTails =
        ObjectPool<_>(
            (fun _ -> Dictionary<RegexNodeId, PooledArray<struct (RegexNodeId * _)>>()),
            1
        )

    member val pool_orGroupedLoops =
        ObjectPool<_>(
            (fun _ ->
                Dictionary<struct (RegexNodeId * RegexNodeId), struct (int * int)>(
                    _structPairRefEqComparer
                )
            ),
            1
        )

    member val pool_orNonZeroLoops =
        ObjectPool<_>(
            (fun _ ->
                Dictionary<struct (RegexNodeId * RegexNodeId), PooledArray<struct (int * int)>>(
                    _structPairRefEqComparer
                )
            ),
            1
        )

    member val pool_orLookaheads =
        ObjectPool<_>(
            (fun _ ->
                Dictionary<RegexNodeId, PooledArray<struct (RegexNodeId * int * RefSet)>>()
            ),
            1
        )

    member this.setFromNode(node: RegexNode) =
        let adjustedSetString =
            if not options.UseDotnetUnicode then
                match overriddenForAscii.TryGetValue(node.Str) with
                | true, v -> v
                | _ -> node.Str
            else
                node.Str

        let bdd = converter.CreateBDDFromSetString(adjustedSetString)

        let mt = solver.ConvertFromBDD(bdd, bcss)
        this.one mt


    member this.charToMinterm(char: char) : 't =
        match _charCache.TryGetValue(char) with
        | true, v -> v
        | _ ->
            let bdd: BDD = bcss.CreateBDDFromChar char
            let minterm = solver.ConvertFromBDD(bdd, bcss)
            _charCache.Add(char, minterm)
            minterm

    member this.one(char: char) : RegexNodeId = this.one (this.charToMinterm char)

    member this.one(minterm: 't) : RegexNodeId =
        let mt = minterm

        match _nodeCache.TryGetValue(KSingleton mt) with
        | true, v -> v
        | _ ->
            let id =
                _registerNode
                    (RegexNode.Singleton(mt))
                    (_createInfo NodeFlags.None mt _emptyRefSet)

            _nodeCache.Add(KSingleton mt, id)
            _minLengthCache[id] <- ValueSome 1
            _maxLengthCache[id] <- ValueSome 1
            id

    member this.notOne(char: char) =
        let mt = solver.Not(this.charToMinterm char)
        this.one mt


    member this.mkOr2(node1: RegexNodeId, node2: RegexNodeId) : RegexNodeId =
        // basic rules
        if node1 = node2 then
            node1
        else

        match this.Node(node1), this.Node(node2) with
        | Not(node = n1), _ when n1 = node2 -> RegexNodeId.TOP_STAR
        | _, Not(node = n1) when n1 = node1 -> RegexNodeId.TOP_STAR
        | _ when node1 = RegexNodeId.BOT -> node2
        | _ when node2 = RegexNodeId.BOT -> node1
        | _ when node1 = RegexNodeId.TOP_STAR || node2 = RegexNodeId.TOP_STAR ->
            RegexNodeId.TOP_STAR
        | _ ->

            // actually creates it without trying to subsume
            let createCached(nodes: RegexNodeId[]) =

                Array.sortInPlace nodes

                let minterms2 =
                    nodes
                    |> fold
                        solver.Empty
                        (fun acc id -> solver.Or(acc, _infos[id].SubsumedByMinterm))

                let inner =
                    if
                        nodes |> Seq.exists (fun id -> not _infos[id].PendingNullables.IsEmpty)
                    then
                        nodes |> map (fun id -> _infos[id].PendingNullables) |> refSetUnionMany
                    else
                        _emptyRefSet

                let flags = Flags.inferOr this.GetFlags nodes

                let mergedInfo = this.CreateInfo(flags, minterms2, inner)

                let sortedNodes = Array.copy nodes
                let id = _registerNode (RegexNode.Or(sortedNodes)) mergedInfo
                this.CacheLengths(id)
                _nodeCache.Add(KOr(nodes.AsMemory()), id)
                _orCount <- _orCount + 1
                id

            let key = [|
                node1
                node2
            |]

            let createCachedSubsume(n1: RegexNodeId, n2: RegexNodeId) =
                let (SplitTail this.Resolve (h1, t1)) = n1
                let (SplitTail this.Resolve (h2, t2)) = n2

                if t1 = t2 then
                    let newH1 = this.mkConcatResizeArray h1
                    let newH2 = this.mkConcatResizeArray h2
                    let newHead = this.mkOr2 (newH1, newH2)
                    this.mkConcat2 (newHead, t1)
                else
                    createCached (key)

            Array.sortInPlace key

            let addToCache(result: RegexNodeId) =
                if _nodeCache.TryAdd(KOr(key.AsMemory()), result) then
                    _orCount <- _orCount + 1

                result

            let applyRewrite (_: string) (overridden: RegexNodeId) : RegexNodeId =
                _nodeCache.Add(KOr(key.AsMemory()), overridden)
                _orCount <- _orCount + 1
                overridden

            match _nodeCache.TryGetValue(KOr(key.AsMemory())) with
            | true, v -> v
            | _ ->
                match this.Node(node1), this.Node(node2) with
                | Singleton p1, Singleton p2 -> this.one (solver.Or(p1, p2))
                | _ when node1 = RegexNodeId.EPS -> this.mkLoop (node2, 0, 1)
                | _ when node2 = RegexNodeId.EPS -> this.mkLoop (node1, 0, 1)
                // a{0,5}|a{4,7} -> a{0,7}
                | Loop(node = inner1; low = low1; up = up1),
                  Loop(node = inner2; low = low2; up = up2) when inner1 = inner2 ->
                    this.mkLoop (inner1, min low1 low2, max up1 up2)
                // (ab)|(ab){2} -> (ab){1,2}
                | Loop(node = loopBody; low = low; up = up), _ when loopBody = node2 ->
                    match low, up with
                    | 2, _ -> this.mkLoop (loopBody, 1, up)
                    | 0, 1 -> node1
                    | _ -> createCached (key)
                | _, Loop(node = loopBody; low = low; up = up) when loopBody = node1 ->
                    match low, up with
                    | 2, _ -> this.mkLoop (loopBody, 1, up)
                    | 0, 1 -> node2
                    | _ -> createCached (key)
                | Or(nodes = nodes), _ ->
                    if Array.BinarySearch(nodes, node2) >= 0 then
                        node1
                    else
                        let merged =
                            this.mkOrSeq (
                                seq {
                                    yield! nodes
                                    node2
                                }
                            )

                        merged
                | _, Or(nodes = nodes) ->
                    if Array.BinarySearch(nodes, node1) >= 0 then
                        node2
                    else
                        let merged =
                            this.mkOrSeq (
                                seq {
                                    yield! nodes
                                    node1
                                }
                            )

                        merged
                | _ ->

                match (|PredStar|_|) this.Resolve node1, (|PredStar|_|) this.Resolve node2 with
                | ValueSome p1, ValueSome p2 ->
                    if Solver.containsSet solver p1 p2 then node1
                    elif Solver.containsSet solver p2 p1 then node2
                    else createCached (key)
                | _ ->

                match this.Node(node1), this.Node(node2) with
                // merge head
                | Concat(head = chead1; tail = ctail1), Concat(head = chead2; tail = ctail2) when
                    chead1 = chead2
                    ->
                    let newtail = this.mkOr2 (ctail1, ctail2)
                    let v = this.mkConcat2 (chead1, newtail)
                    addToCache v
                // put anchor inside lookaround body
                | (Begin | End),
                  LookAround(
                      node = lookBody; lookBack = lb; relativeTo = rel; pendingNullables = pen)
                | LookAround(
                    node = lookBody; lookBack = lb; relativeTo = rel; pendingNullables = pen),
                  (Begin | End) ->
                    let ancId =
                        if
                            (match this.Node(node1) with
                             | Begin
                             | End -> true
                             | _ -> false)
                        then
                            node1
                        else
                            node2

                    b.mkLookaround (b.mkOr2 (ancId, lookBody), lb, rel, pen)
                | Loop(node = loopBody; low = llow; up = lup), _ ->
                    match this.Node(loopBody) with
                    | Singleton lpred ->
                        let other = node2
                        let subby = _infos[other].SubsumedByMinterm
                        let contained = Solver.containsSet solver lpred subby

                        if contained then
                            match this.MinLength(other), this.MaxLength(other) with
                            | ValueSome omin, ValueSome omax ->
                                let c1 = llow <= omin
                                let c2 = lup >= omax

                                if c1 && c2 then
                                    applyRewrite "sub 013" node1
                                else
                                    createCachedSubsume (node1, node2)
                            | _ -> createCachedSubsume (node1, node2)
                        else
                            createCachedSubsume (node1, node2)
                    | _ -> createCachedSubsume (node1, node2)
                | _, Loop(node = loopBody; low = llow; up = lup) ->
                    match this.Node(loopBody) with
                    | Singleton lpred ->
                        let other = node1
                        let subby = _infos[other].SubsumedByMinterm
                        let contained = Solver.containsSet solver lpred subby

                        if contained then
                            match this.MinLength(other), this.MaxLength(other) with
                            | ValueSome omin, ValueSome omax ->
                                let c1 = llow <= omin
                                let c2 = lup >= omax

                                if c1 && c2 then
                                    applyRewrite "sub 013" node2
                                else
                                    createCachedSubsume (node1, node2)
                            | _ -> createCachedSubsume (node1, node2)
                        else
                            createCachedSubsume (node1, node2)
                    | _ -> createCachedSubsume (node1, node2)
                | _ -> createCachedSubsume (node1, node2)


    member private this.hasPrefixOrSuffix(id: RegexNodeId) =
        let f = _infos[id].NodeFlags

        f &&& (NodeFlags.HasPrefixLookbehindFlag ||| NodeFlags.HasSuffixLookaheadFlag)
        <> NodeFlags.None

    member private this.hasPrefix(id: RegexNodeId) =
        _infos[id].NodeFlags &&& NodeFlags.HasPrefixLookbehindFlag <> NodeFlags.None

    member this.mergeAndPrefixSuffix(nodes: RegexNodeId array) : RegexNodeId =
        let prefixes = ResizeArray()
        let suffixes = ResizeArray()

        let remaining: RegexNodeId array =
            nodes
            |> Seq.map (fun (v) ->
                let p, n, s = this.stripPrefixSuffix v
                prefixes.AddRange(p)
                suffixes.AddRange(s)
                n
            )
            |> Seq.where (fun v -> v <> RegexNodeId.TOP_STAR)
            |> Seq.toArray

        if (prefixes.Count = 0 && suffixes.Count = 0) then
            UnsupportedPatternException
                "this pattern is unsupported because of nested lookarounds"
            |> raise
        else

            let prefs = b.mkConcatResizeArray prefixes
            let sufs = b.mkConcatResizeArray suffixes
            let node = b.mkAndSeq remaining

            let newAnd = b.mkConcat2 (prefs, b.mkConcat2 (node, sufs))

            newAnd

    member this.mkAnd2(node1: RegexNodeId, node2: RegexNodeId) : RegexNodeId =
        let key = [|
            node1
            node2
        |]

        Array.sortInPlace key

        let createCached(key: RegexNodeId[]) =
            Array.sortInPlace key

            match key with
            | _ when key.Length = 0 -> RegexNodeId.TOP_STAR
            | _ when key.Length = 1 -> key[0]
            | twoormore when twoormore |> exists this.hasPrefixOrSuffix ->
                let newAnd = this.mergeAndPrefixSuffix twoormore
                _nodeCache.TryAdd(KAnd(key.AsMemory()), newAnd) |> ignore
                newAnd
            | twoormore ->
                let flags = Flags.inferAnd this.GetFlags twoormore

                let minterms2 =
                    twoormore
                    |> fold
                        solver.Empty
                        (fun acc v -> solver.Or(acc, _infos[v].SubsumedByMinterm))

                let inner =
                    if key |> exists (fun id -> not _infos[id].PendingNullables.IsEmpty) then
                        key |> map (fun id -> _infos[id].PendingNullables) |> refSetUnionMany
                    else
                        _emptyRefSet

                let mergedInfo = this.CreateInfo(flags, minterms2, inner)

                match twoormore with
                | _ when twoormore.Length = 0 -> RegexNodeId.TOP_STAR
                | _ when twoormore.Length = 1 -> twoormore[0]
                | _ ->
                    let sortedNodes = Array.copy twoormore
                    let id = _registerNode (RegexNode.And(sortedNodes)) mergedInfo
                    this.CacheLengths(id)

                    _nodeCache.TryAdd(KAnd(key.AsMemory()), id) |> ignore
                    id

        let rewriteRule(overridden: RegexNodeId) : RegexNodeId =
            _nodeCache.Add(KAnd(key.AsMemory()), overridden)
            overridden

        // basic rules
        if node1 = node2 then
            node1
        else

        match this.Node(node1), this.Node(node2) with
        | Not(node = n1), _ when n1 = node2 -> RegexNodeId.BOT
        | _, Not(node = n1) when n1 = node1 -> RegexNodeId.BOT
        | _ when node1 = RegexNodeId.BOT || node2 = RegexNodeId.BOT -> RegexNodeId.BOT
        | _ when node1 = RegexNodeId.TOP_STAR -> node2
        | _ when node2 = RegexNodeId.TOP_STAR -> node1
        | _ when node1 = RegexNodeId.EPS ->
            if _infos[node2].CanBeNullable then
                RegexNodeId.EPS
            else
                RegexNodeId.BOT
        | _ when node2 = RegexNodeId.EPS ->
            if _infos[node1].CanBeNullable then
                RegexNodeId.EPS
            else
                RegexNodeId.BOT
        | _ ->

            match _nodeCache.TryGetValue(KAnd(key.AsMemory())) with
            | true, v -> v
            | _ ->
                // advanced rules
                match this.Node(node1), this.Node(node2) with
                | Singleton s1, Singleton s2 -> b.one (solver.And(s1, s2))
                | And(nodes = nodes), _ ->
                    if Array.BinarySearch(nodes, node2) >= 0 then
                        node1
                    else
                        let merged =
                            this.mkAndSeq [|
                                yield! nodes
                                node2
                            |]

                        merged
                | _, And(nodes = nodes) ->
                    if Array.BinarySearch(nodes, node1) >= 0 then
                        node2
                    else
                        let merged =
                            this.mkAndSeq [|
                                yield! nodes
                                node1
                            |]

                        merged
                | _ ->

                match (|PredLoop|_|) this.Resolve node1, (|PredLoop|_|) this.Resolve node2 with
                | ValueSome(pred1, low1, up1), ValueSome(pred2, low2, up2) ->
                    let p1large = Solver.containsSet solver pred1 pred2
                    let p2large = Solver.containsSet solver pred2 pred1
                    let p1RangeLarger = low1 <= low2 && up1 >= up2
                    let p2RangeLarger = low2 <= low1 && up2 >= up1

                    let canSubsume = (p1large || p2large) && (p1RangeLarger || p2RangeLarger)

                    if canSubsume then
                        let smpred = if p1large then pred2 else pred1
                        let smlow, smup = if p1RangeLarger then low2, up2 else low1, up1
                        rewriteRule (b.mkLoop (b.one smpred, smlow, smup))
                    else
                        createCached key
                | _ ->

                match (|PredStar|_|) this.Resolve node1, (|PredStar|_|) this.Resolve node2 with
                | ValueSome pred, _ ->
                    if Solver.containsSet solver pred (_infos[node2].SubsumedByMinterm) then
                        node2
                    else
                        createCached key
                | _, ValueSome pred ->
                    if Solver.containsSet solver pred (_infos[node1].SubsumedByMinterm) then
                        node1
                    else
                        createCached key
                | _ -> createCached key


    member this.mkAnd(nodes: inref<Memory<RegexNodeId>>) : RegexNodeId =
        let key = nodes

        match _nodeCache.TryGetValue(KAnd key) with
        | true, v -> v
        | _ ->
            if key.Length = 2 then
                this.mkAnd2 (key.Span[0], key.Span[1])
            else

                let mutable enumerating = true
                let mutable status = MkAndFlags.None
                let derivatives = HashSet()
                let _and_complements = HashSet()

                use mutable singletonStarLoops = new PooledArray<_>(key.Length)

                let mutable e = nodes.Span.GetEnumerator()

                while e.MoveNext() = true && enumerating do
                    let rec handleNode(deriv: RegexNodeId) =
                        match deriv with
                        | _ when deriv = RegexNodeId.TOP_STAR -> ()
                        | _ when deriv = RegexNodeId.BOT ->
                            enumerating <- false
                            status <- MkAndFlags.IsFalse
                        | _ when deriv = RegexNodeId.EPS ->
                            status <- MkAndFlags.ContainsEpsilon
                        | _ ->

                        match this.Node(deriv) with
                        | And(nodes) ->
                            for node in nodes do
                                handleNode node
                        | Not(node = node) -> _and_complements.Add(node) |> ignore
                        | Loop(node = body; low = 0; up = Int32.MaxValue) ->
                            match this.Node(body) with
                            | Singleton _ -> singletonStarLoops.Add(deriv)
                            | _ -> derivatives.Add(deriv) |> ignore
                        | _ -> derivatives.Add(deriv) |> ignore

                    handleNode e.Current

                // add all complements together
                if _and_complements.Count > 0 then
                    let merged = b.mkNot (b.mkOrSeq _and_complements)
                    derivatives.Add(merged) |> ignore

                for starLoop in singletonStarLoops do
                    match (|PredStar|_|) this.Resolve starLoop with
                    | ValueSome pred ->
                        let found =
                            derivatives
                            |> Seq.tryFind (fun deriv ->
                                if starLoop = deriv then
                                    false
                                else
                                    let subby = _infos[deriv].SubsumedByMinterm
                                    let contained = Solver.containsSet solver pred subby
                                    contained
                            )

                        if found.IsNone then
                            derivatives.Add(starLoop) |> ignore
                    | _ -> failwith "bug in regex node constructor"

                if
                    (status &&& MkAndFlags.IsFalse <> MkAndFlags.None)
                    || (status &&& MkAndFlags.ContainsEpsilon <> MkAndFlags.None)
                       && derivatives |> Seq.exists (fun v -> not (_infos[v].CanBeNullable))
                then
                    RegexNodeId.BOT
                else
                    if status.HasFlag(MkAndFlags.ContainsEpsilon) then
                        derivatives.Add(RegexNodeId.EPS) |> ignore

                    let allSingletons =
                        derivatives
                        |> Seq.forall (fun v ->
                            match this.Node(v) with
                            | Singleton _ -> true
                            | _ -> false
                        )

                    if allSingletons && derivatives.Count > 0 then
                        derivatives
                        |> fold
                            solver.Empty
                            (fun acc v ->
                                match this.Node(v) with
                                | Singleton pred -> solver.Or(acc, pred)
                                | _ -> failwith "bug in regex node constructor"
                            )
                        |> b.one
                    else


                        if options.MinimizePattern then
                            let grouped_heads =
                                derivatives
                                |> Seq.groupBy (fun v ->
                                    match this.Node(v) with
                                    | Concat(head = head; tail = tail) ->
                                        let fixlen = this.GetFixedLength(head)

                                        match fixlen with
                                        | ValueSome 0 -> ValueNone
                                        | _ when tail = RegexNodeId.EPS -> ValueNone
                                        | _ -> fixlen
                                    | _ -> ValueNone
                                )
                                |> Seq.toArray

                            derivatives.Clear()

                            grouped_heads
                            |> iter (fun (g, res) ->
                                match g with
                                | ValueNone ->
                                    for n in res do
                                        derivatives.Add(n) |> ignore
                                | ValueSome _ ->

                                    let group = res |> Seq.toArray

                                    match group with
                                    | [| single |] -> derivatives.Add(single) |> ignore
                                    | _ ->

                                        let grouped =
                                            res
                                            |> Seq.map (fun v ->
                                                match this.Node(v) with
                                                | Concat(head = head; tail = tail) ->
                                                    head, tail
                                                | other ->
                                                    failwith
                                                        $"expected Concat node in And-derivative grouping, got: {other}"
                                            )

                                        let headArray = grouped |> Seq.map fst |> Seq.toArray
                                        let tailArray = grouped |> Seq.map snd |> Seq.toArray
                                        let newHead = this.mkAndSeq headArray
                                        let newTail = this.mkAndSeq tailArray
                                        let newNode2 = this.mkConcat2 (newHead, newTail)
                                        derivatives.Add(newNode2) |> ignore
                            )

                            let grouped_tails =
                                derivatives
                                |> Seq.map (fun v ->
                                    let (SplitTail this.Resolve (h, d)) = v
                                    h, d
                                )
                                |> Seq.groupBy (fun (h, t) ->
                                    let fixlen = this.GetFixedLength(t)

                                    match fixlen with
                                    | ValueSome 0 -> ValueNone
                                    | _ when h.Count = 0 -> ValueNone
                                    | _ -> fixlen
                                )
                                |> Seq.toArray

                            derivatives.Clear()

                            grouped_tails
                            |> iter (fun (g, res) ->
                                match g with
                                | ValueNone ->
                                    for n in res do
                                        derivatives.Add(
                                            this.mkConcat2 (
                                                this.mkConcatResizeArray (fst n),
                                                snd n
                                            )
                                        )
                                        |> ignore
                                | ValueSome _ ->

                                    let grouped = res
                                    let tailArray = grouped |> Seq.map snd |> Seq.toArray

                                    let headArray =
                                        grouped
                                        |> Seq.map (fun (fst, _) -> b.mkConcatResizeArray fst)
                                        |> Seq.toArray

                                    let newHead = this.mkAndSeq headArray
                                    let newTail = this.mkAndSeq tailArray
                                    let newNode2 = this.mkConcat2 (newHead, newTail)
                                    derivatives.Add(newNode2) |> ignore
                            )


                        let createNode(key: RegexNodeId[]) =
                            match key with
                            | _ when key.Length = 0 -> RegexNodeId.TOP_STAR
                            | _ when key.Length = 1 -> key[0]
                            | twoormore when
                                twoormore |> Seq.exists (fun id -> this.hasPrefixOrSuffix (id))
                                ->
                                this.mergeAndPrefixSuffix twoormore
                            | twoormore ->
                                let flags = Flags.inferAnd this.GetFlags twoormore

                                let minterms2 =
                                    twoormore
                                    |> Seq.map (fun id -> _infos[id].SubsumedByMinterm)
                                    |> Seq.fold (fun acc v -> solver.Or(acc, v)) solver.Empty

                                let inner =
                                    if
                                        key
                                        |> Seq.exists (fun v ->
                                            not _infos[v].PendingNullables.IsEmpty
                                        )
                                    then
                                        key
                                        |> Seq.map (fun v -> _infos[v].PendingNullables)
                                        |> refSetUnionMany
                                    else
                                        _emptyRefSet

                                let mergedInfo = this.CreateInfo(flags, minterms2, inner)

                                let id =
                                    _registerNode
                                        (RegexNode.And(Array.copy twoormore))
                                        mergedInfo

                                this.CacheLengths(id)
                                _nodeCache.Add(KAnd(key.AsMemory()), id)
                                id

                        let key = derivatives.ToArray()
                        key.AsSpan().Sort()

                        match _nodeCache.TryGetValue(KAnd(key.AsMemory())) with
                        | true, v -> v
                        | _ ->
                            let v = createNode key

                            v

    member this.mergeOrIntersections(derivatives: byref<PooledArray<RegexNodeId>>) =
        let mutable num_intersections = 0

        use mutable temp = new PooledArray<RegexNodeId>(16)

        let mutable pending = HashSet()

        for der in derivatives do
            match this.Node(der) with
            | And(nodes = _) ->
                num_intersections <- num_intersections + 1
                pending.Add(der) |> ignore
            | _ -> temp.Add(der)

        if num_intersections > 0 then
            use mutable e = pending.GetEnumerator()

            while e.MoveNext() do
                let curr = e.Current
                use mutable e2 = pending.GetEnumerator()
                let mutable found = false

                while not found && e2.MoveNext() do
                    let prev = e2.Current

                    if curr = prev then
                        ()
                    else
                        match this.Node(curr), this.Node(prev) with
                        | And(nodes = xs1), And(nodes = xs2) ->
                            // xs1 is superset of xs2 if xs2 is subset of xs1
                            if xs2 |> forall (fun x -> Array.BinarySearch(xs1, x) >= 0) then
                                found <- true
                        | _ -> ()

                if not found then
                    temp.Add(curr)

        derivatives.Clear()

        for t in temp do
            derivatives.Add(t)

    member this.mergeOrLookaheads(derivatives: byref<PooledArray<RegexNodeId>>) =
        let mutable num_lookaheads = 0
        let mutable min_rel = Int32.MaxValue
        let dict = this.pool_orLookaheads.Rent()
        use temp = new PooledArray<RegexNodeId>(16)

        use mutable nullslist = new ValueList<struct(int * RefSet)>(16)

        for der in derivatives do
            match this.Node(der) with
            | LookAround(
                node = node; lookBack = false; relativeTo = rel; pendingNullables = pending) ->
                num_lookaheads <- num_lookaheads + 1
                min_rel <- min min_rel rel
                let nulls = pending

                match dict.TryGetValue(node) with
                | true, v -> v.Add(struct (der, rel, nulls))
                | _ ->
                    let sr = new PooledArray<_>(16)
                    sr.Add(struct (der, rel, nulls))
                    dict.Add(node, sr)
            | _ -> temp.Add(der)

        if num_lookaheads > 0 then
            for entry in dict do
                match entry.Value.Count with
                | 1 ->
                    let struct (d, _, _) = entry.Value[0]
                    temp.Add(d)
                | _ ->
                    for _, rel, nulls in entry.Value do
                        nullslist.Add(rel, nulls)

                    let allNulls = refSetRelUnionManyMin (min_rel, nullslist)

                    let merged = this.mkLookaround (entry.Key, false, min_rel, allNulls)

                    temp.Add(merged)

                entry.Value.Dispose()

        derivatives.OverwriteWith(temp.AsSpan())
        dict.Clear()
        this.pool_orLookaheads.Return(dict)

    member this.decrLoop(x, byv) =
        match x, byv with
        | Int32.MaxValue, _ -> Int32.MaxValue
        | _ -> x - byv

    member this.mergeOrGroupedHeads(derivatives: byref<PooledArray<RegexNodeId>>) =
        let dict = this.pool_orGroupedHeads.Rent()
        use temp = new PooledArray<RegexNodeId>(16)

        use templist = new PooledArray<RegexNodeId>(16)

        for der in derivatives do
            match this.Node(der) with
            | Concat(head = head; tail = tail) ->
                match dict.TryGetValue(head) with
                | true, v -> v.Add(der, tail)
                | _ ->
                    let sr = new PooledArray<_>(16)
                    sr.Add(struct (der, tail))
                    dict.Add(head, sr)
            | Singleton _ ->
                match dict.TryGetValue(der) with
                | true, v -> v.Add(der, RegexNodeId.EPS)
                | _ ->
                    let sr = new PooledArray<_>(16)
                    sr.Add(struct (der, RegexNodeId.EPS))
                    dict.Add(der, sr)
            | _ -> temp.Add(der)

        for entry in dict do
            match entry.Value.Count with
            | 1 ->
                let struct (d, _) = entry.Value[0]
                temp.Add(d)
            | _ ->
                templist.Clear()
                let headNode = entry.Key

                for node in entry.Value do
                    let struct (_, ttail) = node
                    templist.Add(ttail)

                let mem = templist.AsMemory()
                mem.Span.Sort()
                let newNode = this.mkOr (&mem)
                let newNode2 = this.mkConcat2 (headNode, newNode)
                temp.Add(newNode2)

            entry.Value.Dispose()

        derivatives.OverwriteWith(temp.AsSpan())
        dict.Clear()
        this.pool_orGroupedHeads.Return(dict)

    member this.mergeOrGroupedLoops(derivatives: byref<PooledArray<RegexNodeId>>) =
        let _or_loop_dict = this.pool_orGroupedLoops.Rent()

        use _or_values = new PooledArray<_>(derivatives.Length)

        for der in derivatives do
            match this.Node(der) with
            | Concat(head = concatHead; tail = tail) ->
                match this.Node(concatHead) with
                | Loop(low = 0; up = upper; node = body) when upper < Int32.MaxValue ->
                    _or_values.Add(der)
                    let key = struct (body, tail)

                    match _or_loop_dict.TryGetValue(key) with
                    | true, (low, up) -> _or_loop_dict[key] <- struct (min low 0, max up upper)
                    | _ -> _or_loop_dict.Add(struct (body, tail), struct (0, upper))
                | Singleton _ ->
                    _or_values.Add(der)
                    let key = struct (concatHead, tail)

                    match _or_loop_dict.TryGetValue(key) with
                    | true, (low, up) -> _or_loop_dict[key] <- struct (min low 1, max up 1)
                    | _ -> _or_loop_dict.Add(struct (concatHead, tail), struct (1, 1))
                | _ -> ()
            | Loop(low = 0; up = upper; node = body) when upper < Int32.MaxValue ->
                _or_values.Add(der)
                let key = struct (body, RegexNodeId.EPS)

                match _or_loop_dict.TryGetValue(key) with
                | true, (low, up) -> _or_loop_dict[key] <- struct (min low 0, max up upper)
                | _ -> _or_loop_dict.Add(struct (body, RegexNodeId.EPS), struct (0, upper))
            | Singleton _ ->
                _or_values.Add(der)
                let key = struct (der, RegexNodeId.EPS)

                match _or_loop_dict.TryGetValue(key) with
                | true, (low, up) -> _or_loop_dict[key] <- struct (min low 1, max up 1)
                | _ -> _or_loop_dict.Add(struct (der, RegexNodeId.EPS), struct (1, 1))
            | _ -> ()

        for der in _or_values do
            match this.Node(der) with
            | Concat(head = concatHead; tail = tail) ->
                match this.Node(concatHead) with
                | Loop(low = 0; up = upper; node = body) when upper < Int32.MaxValue ->
                    let key = struct (body, tail)
                    let struct (_, maxv) = _or_loop_dict[key]

                    if upper < maxv then
                        derivatives.Remove(der)
                | Singleton _ ->
                    let key = struct (concatHead, tail)
                    let struct (_, maxv) = _or_loop_dict[key]

                    if 1 < maxv then
                        derivatives.Remove(der)
                | _ -> ()
            | Loop(low = 0; up = upper; node = body) when upper < Int32.MaxValue ->
                let key = struct (body, RegexNodeId.EPS)
                let struct (_, maxv) = _or_loop_dict[key]

                if upper < maxv then
                    derivatives.Remove(der)
            | Singleton _ ->
                let key = struct (der, RegexNodeId.EPS)
                let struct (minv, maxv) = _or_loop_dict[key]

                if 1 < maxv then
                    derivatives.Remove(der)
                elif 1 > minv then
                    derivatives.Remove(der)
            | _ -> ()

        _or_loop_dict.Clear()
        this.pool_orGroupedLoops.Return(_or_loop_dict)

    member this.mergeOrNonZeroLoops(derivatives: byref<PooledArray<RegexNodeId>>) =
        let _or_loop_dict = this.pool_orNonZeroLoops.Rent()

        use pending_values = new PooledArray<_>(derivatives.Length)

        use newOr = new PooledArray<_>(derivatives.Length)

        let insertOverlappingRange key range =
            match _or_loop_dict.TryGetValue(key) with
            | true, dictSet -> dictSet.Add(range)
            | _ ->
                let mutable newSet = new PooledArray<struct (int * int)>(16)

                newSet.Add(range)
                pending_values.Add(key)
                _or_loop_dict.Add(key, newSet)

        for der in derivatives do
            match this.Node(der) with
            | Concat(head = concatHead; tail = tail) ->
                match this.Node(concatHead) with
                | Loop(low = low; up = up; node = body) ->
                    insertOverlappingRange struct (body, tail) struct (low, up)
                | _ -> insertOverlappingRange struct (concatHead, tail) struct (1, 1)
            | Loop(low = low; up = up; node = body) when up < int rsint.MaxValue ->
                let range = struct (low, up)
                insertOverlappingRange struct (body, RegexNodeId.EPS) range
            | Singleton _ ->
                let key = struct (der, RegexNodeId.EPS)
                insertOverlappingRange key struct (1, 1)
            | _ -> newOr.Add(der)

        for body, tail in pending_values do
            let key = struct (body, tail)
            use refsets = _or_loop_dict[key]
            use merged = RangeSet.unionMany refsets

            for low, up in merged do
                newOr.Add(b.mkConcat2 (b.mkLoop (body, low, up), tail))

        _or_loop_dict.Clear()
        this.pool_orNonZeroLoops.Return(_or_loop_dict)
        derivatives.OverwriteWith(newOr.AsSpan())


    member this.mergeOrGroupedTails(derivatives: byref<PooledArray<RegexNodeId>>) =
        let dict = this.pool_orGroupedTails.Rent()
        use temp = new PooledArray<RegexNodeId>(16)

        use headsList = new PooledArray<RegexNodeId>(16)

        for der in derivatives do
            let (SplitTail this.Resolve (h, tl)) = der

            match dict.TryGetValue(tl) with
            | true, v -> v.Add(der, h)
            | _ ->
                let sr = new PooledArray<_>(16)
                sr.Add(struct (der, h))
                dict.Add(tl, sr)

        for entry in dict do
            match entry.Value.Count with
            | 1 ->
                let struct (d, _) = entry.Value[0]
                temp.Add(d)
            | _ ->
                headsList.Clear()
                let tailNode = entry.Key

                for node in entry.Value do
                    let struct (_, thead) = node
                    headsList.Add(this.mkConcatResizeArray thead)

                let mem = headsList.AsMemory()
                mem.Span.Sort()
                let newNode = this.mkOr (&mem)
                let newNode2 = this.mkConcat2 (newNode, tailNode)
                temp.Add(newNode2)

            entry.Value.Dispose()

        derivatives.OverwriteWith(temp.AsSpan())
        dict.Clear()
        this.pool_orGroupedTails.Return(dict)


    member this.mkAndSeq(nodes: RegexNodeId seq) : RegexNodeId =
        let mutable pool = ArrayPool<RegexNodeId>.Shared
        let mutable limit = 16
        let mutable rentedArray = pool.Rent(limit)
        use mutable e = nodes.GetEnumerator()
        let mutable i = 0

        while e.MoveNext() do
            if i >= limit then
                let newLimit = limit * 2
                let newRentedArray = pool.Rent(newLimit)
                rentedArray.AsSpan().CopyTo(destination = newRentedArray.AsSpan())
                pool.Return(rentedArray)
                rentedArray <- newRentedArray
                limit <- newLimit

            rentedArray[i] <- e.Current
            i <- i + 1

        let mem = rentedArray.AsMemory(0, i)
        mem.Span.Sort()
        let res = this.mkAnd (&mem)
        pool.Return(rentedArray)
        res

    member this.mkOrSeq(nodes: RegexNodeId seq) : RegexNodeId =
        let mutable pool = ArrayPool<RegexNodeId>.Shared
        let mutable limit = 16
        let mutable rentedArray = pool.Rent(limit)
        use mutable e = nodes.GetEnumerator()
        let mutable i = 0

        while e.MoveNext() do
            if i >= limit then
                let newLimit = limit * 2
                let newRentedArray = pool.Rent(newLimit)
                rentedArray.AsSpan().CopyTo(destination = newRentedArray.AsSpan())
                pool.Return(rentedArray)
                rentedArray <- newRentedArray
                limit <- newLimit

            rentedArray[i] <- e.Current
            i <- i + 1

        let mem = rentedArray.AsMemory(0, i)
        mem.Span.Sort()
        let res = this.mkOr (&mem)
        pool.Return(rentedArray)
        res

    member this.mkOr(nodes: inref<RegexNodeId Memory>) : RegexNodeId =
        let key = nodes

        match _nodeCache.TryGetValue(KOr key) with
        | true, v -> v
        | _ ->
#if SUBSUME
            // if key.Length = 2 then
            //     this.mkOr2 (key.Span[0], key.Span[1])
            // else
#endif

            let mutable enumerating = true
            let mutable status = MkOrFlags.None
            let mutable zeroloops = 0

            use mutable singletonStarLoops = new PooledArray<_>(key.Length)

            let mutable e = nodes.Span.GetEnumerator()

            use mutable derivatives = new PooledArray<_>(key.Length)

            use mutable templist = new PooledArray<RegexNodeId>(key.Length)

            while e.MoveNext() && enumerating do
                let rec handleNode(deriv: RegexNodeId) =
                    match this.Node(deriv) with
                    | _ when deriv = RegexNodeId.BOT -> ()
                    | _ when deriv = RegexNodeId.TOP_STAR ->
                        enumerating <- false
                        status <- MkOrFlags.IsTrueStar
                    | _ when deriv = RegexNodeId.EPS ->
                        status <- status ||| MkOrFlags.ContainsEpsilon
                    | Or(nodes) -> nodes |> iter handleNode
                    | Concat(head = concatHead) ->
                        match this.Node(concatHead) with
                        | Loop(low = 0; up = upper) when upper <> Int32.MaxValue ->
                            zeroloops <- zeroloops + 1
                            derivatives.Add(deriv)
                        | _ -> derivatives.Add(deriv)
                    | Loop(low = 0; up = upper) when upper <> Int32.MaxValue ->
                        // uniqueHeads
                        zeroloops <- zeroloops + 1
                        derivatives.Add(deriv)
                    | Loop(node = loopBody; low = 0; up = Int32.MaxValue) ->
                        match this.Node(loopBody) with
                        | Singleton _ ->
                            singletonStarLoops.Add(deriv)
                            derivatives.Add(deriv)
                        | _ -> derivatives.Add(deriv)
                    | _ -> derivatives.Add(deriv)

                handleNode e.Current

            for starLoop in singletonStarLoops do
                match (|PredStar|_|) this.Resolve starLoop with
                | ValueSome pred ->
                    for deriv in derivatives do
                        if starLoop = deriv then
                            ()
                        else
                            let subby = _infos[deriv].SubsumedByMinterm

                            if Solver.containsSet solver pred subby then
                                derivatives.Remove(deriv)
                | _ -> failwith "bug in regex node constructor"

            match status &&& MkOrFlags.IsTrueStar <> MkOrFlags.None with
            | true -> RegexNodeId.TOP_STAR
            | _ ->

                // add epsilon only if no nullables yet
                if (status &&& MkOrFlags.ContainsEpsilon) <> MkOrFlags.None then
                    if derivatives.Exists(fun d -> _infos[d].IsAlwaysNullable) then
                        ()
                    else
                        derivatives.Add(RegexNodeId.EPS)


                // merge singletons
                do
                    let mutable num_singletons = 0
                    templist.OverwriteWith(derivatives.AsSpan())
                    let mutable ss = solver.Empty

                    for n in templist do
                        match this.Node(n) with
                        | Singleton p ->
                            num_singletons <- num_singletons + 1
                            derivatives.Remove(n)
                            ss <- solver.Or(p, ss)
                        | _ -> ()

                    if num_singletons > 0 then
                        let merged2 = b.one ss
                        derivatives.Add(merged2)

                if zeroloops > 0 then
                    // remove loop duplicates
                    // C{0,9}D  | C{0,8}D = C{0,9}D
                    this.mergeOrGroupedLoops (&derivatives)

                if options.MinimizePattern then
                    this.mergeOrLookaheads (&derivatives)

                    if _orCount < options.StartsetInferenceLimit then
                        this.mergeOrIntersections (&derivatives)
                        this.mergeOrGroupedHeads (&derivatives)
                        this.mergeOrGroupedTails (&derivatives)
                        this.mergeOrNonZeroLoops (&derivatives)

                if derivatives.Count = 0 then
                    RegexNodeId.BOT
                else if derivatives.Count = 1 then
                    derivatives.AsSpan()[0]
                else
                    let createNode(nodes: RegexNodeId Memory) =
                        //  todo: could be simpler
                        match nodes with
                        | _ when nodes.Length = 0 -> RegexNodeId.BOT
                        | _ when nodes.Length = 1 -> nodes.Span[0]
                        | twoormore ->
                            let twoormore = twoormore.ToArray()
                            let flags = Flags.inferOr this.GetFlags twoormore

                            let containsMinterms =
                                twoormore
                                |> fold
                                    solver.Empty
                                    (fun acc v -> solver.Or(acc, _infos[v].SubsumedByMinterm))


                            let inner =
                                if
                                    twoormore
                                    |> exists (fun v ->
                                        not _infos[v].PendingNullables.IsEmpty
                                    )
                                then
                                    twoormore
                                    |> map (fun v -> _infos[v].PendingNullables)
                                    |> refSetUnionMany
                                else
                                    _emptyRefSet

                            let mergedInfo = this.CreateInfo(flags, containsMinterms, inner)

                            let id =
                                _registerNode (RegexNode.Or(Array.copy twoormore)) mergedInfo

                            this.CacheLengths(id)
                            id

                    let mem = derivatives.AsMemory()
                    mem.Span.Sort()

                    match _nodeCache.TryGetValue(KOr mem) with
                    | true, v -> v
                    | _ ->
                        let v = createNode mem
                        let newArr = Array.zeroCreate key.Length

                        for i = 0 to key.Length - 1 do
                            newArr[i] <- key.Span[i]

                        _nodeCache.Add(KOr(newArr.AsMemory()), v)
                        _orCount <- _orCount + 1
                        v

    member this.mkNot(inner: RegexNodeId) =
        let createNode(inner: RegexNodeId) =
            match this.Node(inner) with
            | _ when inner = RegexNodeId.BOT -> RegexNodeId.TOP_STAR // ~(⊥) -> _*
            | _ when inner = RegexNodeId.TOP_STAR -> RegexNodeId.BOT // ~(_*) -> ⊥
            | _ when inner = RegexNodeId.EPS -> RegexNodeId.TOP_PLUS // ~(ε) -> _+
            | _ when _infos[inner].NodeFlags.ContainsLookaround ->
                raise (
                    UnsupportedPatternException(
                        "lookarounds inside complement are not yet allowed"
                    )
                )
            | _ when _infos[inner].NodeFlags.DependsOnAnchor ->
                raise (
                    UnsupportedPatternException(
                        "anchors inside complement are currently unsupported"
                    )
                )
            | _ ->
                let mutable flags = Flags.inferCompl this.GetFlags inner
                // not subsumption is interesting
                let subsumedBy = solver.Full

                let id =
                    _registerNode
                        (Not(inner))
                        (this.CreateInfo(flags, subsumedBy, _infos[inner].PendingNullables))

                this.CacheLengths(id)
                id

        let key = inner

        match _nodeCache.TryGetValue(KNot key) with
        | true, v -> v
        | _ ->
            let v = createNode key
            _nodeCache.Add(KNot key, v)
            v


    member this.setFromStr(setPattern: string) =
        let tree =
            Resharp.Runtime.ExtendedRegexParser.Parse(
                setPattern,
                RegexOptions.ExplicitCapture,
                CultureInfo.InvariantCulture
            )

        let setStr = tree.Root.Child(0).Str
        let bdd = converter.CreateBDDFromSetString(setStr)
        let converted = solver.ConvertFromBDD(bdd, bcss)
        this.one converted

    member this.setFromCharClass(setClass: string) =
        let bdd = converter.CreateBDDFromSetString(setClass)
        let converted = solver.ConvertFromBDD(bdd, bcss)
        this.one converted

    member this.bddFromClass(setClass: string) =
        let bdd = converter.CreateBDDFromSetString(setClass)
        bdd

    member this.bddFromSetString(setPattern: string) =
        converter.CreateBDDFromSetString(setPattern)


    member this.mkConcat2(head: RegexNodeId, tail: RegexNodeId) : RegexNodeId =

        // wont pollute the cache with these
        if head = RegexNodeId.EPS then
            tail // ()R -> R
        elif tail = RegexNodeId.EPS then
            head
        elif head = RegexNodeId.BOT || tail = RegexNodeId.BOT then
            RegexNodeId.BOT // ⊥R -> ⊥
        else

            let createCached(head, tail) =
                let flags = Flags.inferConcat this.GetFlags this.Resolve head tail

                let mergedMinterms =
                    solver.Or(_infos[head].SubsumedByMinterm, _infos[tail].SubsumedByMinterm)


                let info =
                    this.CreateInfo(
                        flags,
                        mergedMinterms,
                        refSetUnion _infos[head].PendingNullables _infos[tail].PendingNullables
                    )

                let id = _registerNode (Concat(head, tail)) info
                this.CacheLengths(id)
                _nodeCache.Add(KConcat(head, tail), id)
                id

            let rewriteRule (rule: string) (overridden: RegexNodeId) : RegexNodeId =
                _nodeCache.Add(KConcat(head, tail), overridden)
                overridden

            match _nodeCache.TryGetValue(KConcat(head, tail)) with
            | true, v -> v
            | _ ->
                let incrLoop x y =
                    match x, y with
                    | Int32.MaxValue, _
                    | _, Int32.MaxValue -> Int32.MaxValue
                    | _ -> x + y

                let resolve = this.Resolve

                match this.Node(head), this.Node(tail) with
                | _, And(nodes = xs) when
                    (head = RegexNodeId.TOP_STAR)
                    && xs
                       |> forall (fun x ->
                           match (|StartsWithTrueStar|_|) resolve solver x with
                           | ValueSome _ -> true
                           | _ -> false
                       )
                    ->
                    tail
                | And(nodes = xs), _ when
                    (tail = RegexNodeId.TOP_STAR)
                    && xs
                       |> forall (fun x ->
                           match (|EndsWithTrueStar|_|) resolve solver x with
                           | ValueSome _ -> true
                           | _ -> false
                       )
                    ->
                    head
                // sub 01: (.*1)?(.*1){2,} -> (.*1){2,}
                | Loop(node = lhead1; low = low1; up = up1),
                  Loop(node = lhead2; low = low2; up = up2) when lhead1 = lhead2 ->
                    let newHead = this.mkLoop (lhead1, incrLoop low1 low2, incrLoop up1 up2)

                    _nodeCache.Add(KConcat(head, tail), newHead)
                    newHead
                // sub 02: (.*1)?.*1 -> .*1
                | Loop(node = lhead1; low = low1; up = up1), _ when lhead1 = tail ->
                    let newHead = this.mkLoop (lhead1, incrLoop low1 1, incrLoop up1 1)

                    _nodeCache.Add(KConcat(head, tail), newHead)
                    newHead
                // merge loops 2
                | _, Concat(head = concatHead; tail = tail2) ->
                    match this.Node(concatHead) with
                    | Loop(node = lhead2; low = low; up = up) when head = lhead2 ->
                        let newHead = this.mkLoop (head, incrLoop low 1, incrLoop up 1)

                        let v = this.mkConcat2 (newHead, tail2)
                        _nodeCache.Add(KConcat(head, tail), v)
                        v
                    | _ ->
                    // merge loops 3
                    match this.Node(head) with
                    | Loop(node = lhead1; low = low1; up = up1) ->
                        match this.Node(concatHead) with
                        | Loop(node = lhead2; low = low2; up = up2) when lhead1 = lhead2 ->
                            let newHead =
                                this.mkLoop (lhead1, incrLoop low1 low2, incrLoop up1 up2)

                            let v = this.mkConcat2 (newHead, tail2)
                            _nodeCache.Add(KConcat(head, tail), v)
                            v
                        | _ ->
                        // fall through to concat normalization below
                        this.mkConcat2_concatTailCase (
                            head,
                            tail,
                            concatHead,
                            tail2,
                            createCached,
                            incrLoop
                        )
                    | _ ->

                    this.mkConcat2_concatTailCase (
                        head,
                        tail,
                        concatHead,
                        tail2,
                        createCached,
                        incrLoop
                    )

                | _ ->

                match (|PredStar|_|) resolve head, (|PredStar|_|) resolve tail with
                | ValueSome p1, ValueSome p2 ->
                    if Solver.containsSet solver p1 p2 then head
                    elif Solver.containsSet solver p2 p1 then tail
                    else createCached (head, tail)
                | _ ->

                match this.Node(head), this.Node(tail) with
                // // (?<=a.*)(?<=\W)aa to (?<=_*a.*&_*\W)aa
                | LookAround(node = node1; lookBack = true), Concat(head = concatHead2) ->
                    match this.Node(concatHead2) with
                    | LookAround(node = node2; lookBack = true) ->
                        let (SplitTail this.Resolve (_, tail2)) = tail

                        let combined =
                            this.mkAndSeq [
                                this.prependTSIfNotAnchor (node1)
                                this.prependTSIfNotAnchor (node2)
                            ]

                        let look = this.mkLookaround (combined, true, 0, _emptyRefSet)

                        let v = this.mkConcat2 (look, tail2)
                        _nodeCache.Add(KConcat(head, tail), v)
                        v
                    | _ -> createCached (head, tail)
                // (?<=a.*)(?<=\W) to (?<=_*a.*&_*\W)
                | LookAround(node = node1; lookBack = true),
                  LookAround(node = node2; lookBack = true) ->
                    let combined =
                        this.mkAndSeq [
                            this.prependTSIfNotAnchor (node1)
                            this.prependTSIfNotAnchor (node2)
                        ]

                    let v = this.mkLookaround (combined, true, 0, _emptyRefSet)
                    _nodeCache.Add(KConcat(head, tail), v)
                    v
                // (?<=.*).* to .* or (?<=.*).*ab to .*ab
                | LookAround(node = lookNode; lookBack = true), _ when
                    (match (|PredStar|_|) resolve lookNode with
                     | ValueSome _ -> true
                     | _ -> false)
                    && (lookNode = tail
                        || (
                            match this.Node(tail) with
                            | Concat(head = chead) -> lookNode = chead
                            | _ -> false
                        ))
                    ->
                    let v = tail
                    _nodeCache.Add(KConcat(head, tail), v)
                    v
                // (?=a.*)(?=\W) to (?=a.*_*&\W_*)
                | LookAround(
                    node = node1
                    lookBack = false
                    relativeTo = rel1
                    pendingNullables = pending1),
                  LookAround(
                      node = node2
                      lookBack = false
                      relativeTo = _
                      pendingNullables = pending2) ->
                    assert pending2.IsEmpty

                    let combined =
                        this.mkAndSeq [
                            this.appendTSIfNotAnchor (node1)
                            this.appendTSIfNotAnchor (node2)
                        ]

                    let v = this.mkLookaround (combined, false, rel1, pending1) // pass pending nullables

                    _nodeCache.Add(KConcat(head, tail), v)
                    v
                | LookAround(node = lookBody; lookBack = false), _ when
                    lookBody = RegexNodeId.EPS && not (_infos[tail].IsAlwaysNullable)
                    ->
                    let v =
                        let flags = Flags.inferConcat this.GetFlags this.Resolve head tail

                        let mergedMinterms =
                            solver.Or(
                                _infos[head].SubsumedByMinterm,
                                _infos[tail].SubsumedByMinterm
                            )

                        let info =
                            this.CreateInfo(
                                flags,
                                mergedMinterms,
                                refSetUnion
                                    _infos[head].PendingNullables
                                    _infos[tail].PendingNullables
                            )

                        let id = _registerNode (Concat(head, tail)) info
                        this.CacheLengths(id)
                        id

                    _nodeCache.Add(KConcat(head, tail), v)
                    v

                // sub 07: ((.*1)?|b).*a -> .*a
                | _, _ when
                    (match (|PredStarHead|_|) resolve tail with
                     | ValueSome pstar ->
                         _infos[head].IsAlwaysNullable
                         && Solver.starSubsumes solver pstar _infos[head].SubsumedByMinterm
                     | _ -> false)
                    ->
                    tail
                // ((.*1)?|b).*a -> .*a
                | Or(nodes = xs), _ ->
                    let pstarOpt =
                        match (|PredStar|_|) this.Resolve tail with
                        | ValueSome p -> ValueSome p
                        | _ ->

                        match this.Node(tail) with
                        | Concat(head = ch) ->
                            match (|PredStar|_|) this.Resolve ch with
                            | ValueSome p -> ValueSome p
                            | _ -> ValueNone
                        | _ -> ValueNone

                    match pstarOpt with
                    | ValueSome pstar ->
                        use mutable vlist = new ValueList<_>(16)
                        let mutable subsumed = false

                        for n in xs do
                            match this.Node(n) with
                            | _ when n = RegexNodeId.EPS -> vlist.Add(n)
                            | _ ->
                                let isSubsumed =
                                    _infos[n].IsAlwaysNullable
                                    && Solver.starSubsumes
                                        solver
                                        pstar
                                        _infos[n].SubsumedByMinterm

                                if isSubsumed then
                                    subsumed <- true
                                    vlist.Add(RegexNodeId.EPS)
                                else
                                    vlist.Add(n)

                        vlist.AsSpan().Sort()

                        if subsumed then
                            let mem = vlist.AsMemory()
                            let res = this.mkOr (&mem)
                            let res2 = this.mkConcat2 (res, tail)
                            _nodeCache.Add(KConcat(head, tail), res2)
                            res2
                        else
                            createCached (head, tail)
                    | _ -> createCached (head, tail)

                // normalize concat
                | Concat(head = h1; tail = h2), _ ->
                    // (ab)(ab){10} -> (ab){11}
                    match this.Node(tail) with
                    | Loop(lbody, low, up) when head = lbody ->
                        this.mkLoop (lbody, incrLoop low 1, incrLoop up 1)
                    | _ ->

                        let merged = this.mkConcat2 (h1, this.mkConcat2 (h2, tail))

                        _nodeCache.Add(KConcat(head, tail), merged)
                        merged
                | _ -> createCached (head, tail)

    member private this.mkConcat2_sub03_06
        (head, tail, concatHead, tail2, createCached, incrLoop)
        =
        let resolve = this.Resolve
        // n1, Concat(...) general case
        let n1 = head
        let n2 = concatHead
        let tailNode = tail2
        let inner = this.mkConcat2 (n1, n2)

        match (|PredStar|_|) resolve inner with
        // sub 06: (.*1)?.*a -> .*a
        | ValueSome _ ->
            let newHead = this.mkConcat2 (inner, tailNode)
            _nodeCache.Add(KConcat(head, tail), newHead)
            newHead
        | _ ->

            match this.Node(tailNode) with
            // sub 03: .*1.*1$ -> (.*1){2,}
            | Concat(head = t1; tail = t2) when n1 = t1 && n2 = t2 ->
                let newHead = this.mkLoop (tailNode, 2, 2)
                _nodeCache.Add(KConcat(head, tail), newHead)
                newHead
            // sub 04 + sub 05
            | Concat(head = t1; tail = tt) ->
                match this.Node(tt) with
                | Concat(head = t2; tail = t3) when n1 = t1 && n2 = t2 ->
                    let inner = this.mkConcat2 (n1, n2)
                    let newHead = this.mkConcat2 (this.mkLoop (inner, 2, 2), t3)
                    _nodeCache.Add(KConcat(head, tail), newHead)
                    newHead
                | _ ->

                match this.Node(t1) with
                | Loop(node = lnode; low = low; up = up) when inner = lnode ->
                    let inner = this.mkLoop (lnode, incrLoop low 1, incrLoop up 1)
                    let newHead = this.mkConcat2 (inner, tt)
                    _nodeCache.Add(KConcat(head, tail), newHead)
                    newHead
                | _ -> createCached (head, tail)
            | _ -> createCached (head, tail)

    member private this.mkConcat2_concatTailCase
        (head, tail, concatHead, tail2, createCached, incrLoop)
        =
        let resolve = this.Resolve

        match (|PredStar|_|) resolve head, (|PredStar|_|) resolve tail with
        | ValueSome p1, ValueSome p2 ->
            if Solver.containsSet solver p1 p2 then head
            elif Solver.containsSet solver p2 p1 then tail
            else createCached (head, tail)
        | ValueSome p1, _ ->
            match (|PredStar|_|) resolve concatHead with
            | ValueSome p2 ->
                if Solver.containsSet solver p1 p2 then
                    this.mkConcat2 (head, tail2)
                elif Solver.containsSet solver p2 p1 then
                    this.mkConcat2 (concatHead, tail2)
                else
                    createCached (head, tail)
            | _ ->
            // .*(t.*)?hat.* -> .*hat
            match this.Node(concatHead) with
            | Loop(loopBody, 0, 1) ->
                let (ConcatSuffix resolve tsuffix) = loopBody
                let theadsubsume = _infos[concatHead].SubsumedByMinterm

                let condition1 = Solver.containsSet solver p1 theadsubsume

                if condition1 && head = tsuffix then
                    this.mkConcat2 (head, tail2)
                else
                    createCached (head, tail)
            | _ ->
                this.mkConcat2_sub03_06 (head, tail, concatHead, tail2, createCached, incrLoop)
        | _ ->

        match this.Node(head) with
        // (.*ab)?.* -> .*
        | Loop(loopBody, 0, 1) ->
            let loopResult =
                match this.Node(loopBody) with
                | Concat(head = ch) ->
                    match (|PredStar|_|) resolve ch, (|PredStar|_|) resolve tail with
                    | ValueSome _, ValueSome p2 ->
                        let subMinterm = _infos[head].SubsumedByMinterm

                        if Solver.containsSet solver p2 subMinterm then
                            if
                                Solver.containsSet
                                    solver
                                    p2
                                    (match (|PredStar|_|) resolve ch with
                                     | ValueSome v -> v
                                     | _ -> solver.Empty)
                            then
                                ValueSome tail
                            elif loopBody = tail then
                                ValueSome tail
                            else
                                ValueNone
                        else
                            ValueNone
                    | _ -> ValueNone
                | _ -> ValueNone

            match loopResult with
            | ValueSome r -> r
            | ValueNone ->
                this.mkConcat2_sub03_06 (head, tail, concatHead, tail2, createCached, incrLoop)
        // .{0,20}_*
        | Loop(_, 0, _) ->
            match (|PredStar|_|) resolve tail with
            | ValueSome p2 ->
                let subMinterm = _infos[head].SubsumedByMinterm

                if Solver.containsSet solver p2 subMinterm then
                    tail
                else
                    createCached (head, tail)
            | _ ->

            this.mkConcat2_sub03_06 (head, tail, concatHead, tail2, createCached, incrLoop)

        | _ -> this.mkConcat2_sub03_06 (head, tail, concatHead, tail2, createCached, incrLoop)


    member this.mkLookaround
        (body: RegexNodeId, lookBack: bool, rel: int, pendingNullable: RefSet)
        : RegexNodeId =

        let createCached
            (body: RegexNodeId, lookBack: bool, rel: int, pendingNullable: RefSet)
            =
            // rewrite body to normal form
            let body =
                if lookBack then
                    body
                else
                    // only for lookaheads
                    match this.Node(body) with
                    | _ when body = RegexNodeId.EPS -> body
                    | _ ->

                    match body = RegexNodeId.TOP_STAR with
                    | true -> body
                    | _ ->

                    match (|EndsWithTrueStar|_|) this.Resolve solver body with
                    | ValueSome _ -> body
                    | _ ->

                    match this.Node(body) with
                    // not always correct but does not make a difference
                    | Concat(head = ch; tail = ct) when
                        (ch = RegexNodeId.TOP_STAR)
                        && (
                            match this.Node(ct) with
                            | End
                            | Begin -> true
                            | _ -> false
                        )
                        && _infos[body].NodeFlags.DependsOnAnchor
                        ->
                        RegexNodeId.EPS
                    | Concat _ when _infos[body].NodeFlags.DependsOnAnchor ->
                        let (SplitTail this.Resolve (_, tail)) = body

                        match _infos[tail].NodeFlags.DependsOnAnchor with
                        | true -> body
                        | _ -> b.mkConcat2 (body, RegexNodeId.TOP_STAR)
                    | _ -> b.mkConcat2 (body, RegexNodeId.TOP_STAR)

            let key2 = KLookaround(body, lookBack, rel, pendingNullable)

            match _nodeCache.TryGetValue(key2) with
            | true, v -> v
            | _ ->
                let flags = Flags.inferLookaround this.GetFlags body lookBack

                let infoNullables =
                    if not flags.CanBeNullable then
                        _emptyRefSet
                    else
                        refSetAddAll (uint16 rel) pendingNullable

                let info = this.CreateInfo(flags, solver.Full, infoNullables)

                let id = _registerNode (LookAround(body, lookBack, rel, pendingNullable)) info
                this.CacheLengths(id)

                _nodeCache.Add(key2, id)
                id

        match _nodeCache.TryGetValue(KLookaround(body, lookBack, rel, pendingNullable)) with
        | true, v -> v
        | _ ->
            let newNode =
                match this.Node(body), lookBack with
                | _, true when body = RegexNodeId.EPS -> body
                // lookaround condition met with _*, match can start at any pos
                | _, true when RegexNodeId.TOP_STAR = body ->
                    createCached (RegexNodeId.TOP_STAR, lookBack, rel, pendingNullable)
                // IMPORTANT: finish pending lookahead
                | _, false when _infos[body].IsAlwaysNullable ->
                    createCached (RegexNodeId.EPS, lookBack, rel, pendingNullable)
                | _ when RegexNodeId.BOT = body -> RegexNodeId.BOT
                | _ -> createCached (body, lookBack, rel, pendingNullable)

            newNode

    /// returns remaining pattern, suffix
    member this.stripSuffixes(node: RegexNodeId) =
        if not (this.hasPrefixOrSuffix (node)) then
            node, []
        else
            match this.Node(node) with
            | Concat(head = chead; tail = ctail) ->
                match this.Node(chead) with
                | LookAround(lookBack = true) -> node, []
                | _ ->

                match this.Node(ctail) with
                | LookAround(lookBack = false) -> chead, [ ctail ]
                | Concat(head = ch; tail = ct) ->
                    match this.Node(ch) with
                    | LookAround(lookBack = false) ->
                        let innerHead, innerSuffixes = this.stripSuffixes ct
                        assert (innerHead = RegexNodeId.EPS)
                        chead, ch :: innerSuffixes
                    | _ ->
                        let hd2, suf = this.stripSuffixes ctail
                        this.mkConcat2 (chead, hd2), suf
                | _ ->
                    let hd2, suf = this.stripSuffixes ctail
                    this.mkConcat2 (chead, hd2), suf
            | LookAround(lookBack = false) -> RegexNodeId.EPS, [ node ]
            | LookAround(lookBack = true) -> node, []
            | Or(nodes = _) ->
                match this.GetFixedLength(node) with
                | ValueSome 0 -> RegexNodeId.EPS, [ node ]
                | len ->
                    failwith
                        $"cannot infer width for lookaround Or node (fixed length: %A{len})"
            | _ -> node, []

    member this.stripPrefixSuffix(node: RegexNodeId) =
        if not (this.hasPrefixOrSuffix (node)) then
            [], node, []
        else
            match this.Node(node) with
            | Concat(head = head; tail = tail) ->
                match this.Node(head) with
                | LookAround(lookBack = true) ->
                    let prf, innerNode, suf = this.stripPrefixSuffix tail
                    head :: prf, innerNode, suf
                | _ ->

                match this.Node(tail) with
                | LookAround(lookBack = false) -> [], head, [ tail ]
                | Concat(head = ch; tail = ct) ->
                    match this.Node(ch) with
                    | LookAround(lookBack = false) ->
                        let innerHead, innerSuffixes = this.stripSuffixes ct
                        assert (innerHead = RegexNodeId.EPS)
                        [], head, ch :: innerSuffixes
                    | _ ->

                    let hp, h =
                        if this.hasPrefix (head) then
                            match this.GetFixedLength(head) with
                            | ValueSome 0 -> [ head ], RegexNodeId.EPS
                            | ValueSome 1 -> [ head ], RegexNodeId.TOP
                            | len ->
                                failwith
                                    $"cannot infer width for lookaround prefix (fixed length: %A{len})"
                        else
                            [], head

                    let hd2, suf = this.stripSuffixes tail
                    hp, this.mkConcat2 (h, hd2), suf
                | _ ->
                    let hp, h =
                        if this.hasPrefix (head) then
                            match this.GetFixedLength(head) with
                            | ValueSome 0 -> [ head ], RegexNodeId.EPS
                            | ValueSome 1 -> [ head ], RegexNodeId.TOP
                            | len ->
                                failwith
                                    $"cannot infer width for lookaround prefix (fixed length: %A{len})"
                        else
                            [], head

                    let hd2, suf = this.stripSuffixes tail
                    hp, this.mkConcat2 (h, hd2), suf
            | LookAround(lookBack = false) -> [], RegexNodeId.EPS, [ node ]
            | LookAround(lookBack = true) -> [ node ], RegexNodeId.EPS, []
            | Or(nodes = _) ->
                // this isnt really supported but will work in zero-width cases
                if this.anchors._dollarAnchor.Value = node then
                    [], RegexNodeId.EPS, [ node ]
                elif this.anchors._caretAnchor.Value = node then
                    raise (
                        UnsupportedPatternException("unsupported lookaround/anchor in pattern")
                    )
                else
                    [], node, []
            | _ -> [], node, []


    member this.collectConcatNodes(node: RegexNodeId) =
        match this.Node(node) with
        | Concat(head = head; tail = tail) -> head :: this.collectConcatNodes tail
        | _ -> [ node ]


    member this.prependTSIfNotAnchor(node: RegexNodeId) =
        this.mkConcat2 (RegexNodeId.TOP_STAR, node)

    member this.appendTSIfNotAnchor(node: RegexNodeId) =
        this.mkConcat2 (node, RegexNodeId.TOP_STAR)

    member this.attemptRewriteCommonLookahead
        (origlookahead: RegexNodeId, remainingTail: RegexNodeId)
        =
        let lookaheadBody =
            match this.Node(origlookahead) with
            | LookAround(node = nodeBody) ->
                let (SplitTail this.Resolve (hd, t)) = nodeBody

                if t = RegexNodeId.TOP_STAR then
                    b.mkConcatResizeArray (hd)
                else
                    nodeBody
            | other ->
                failwith
                    $"expected LookAround node in attemptRewriteCommonLookahead, got: {other}"

        let isNonWordRight = origlookahead = _anchors._nonWordRight.Value

        match this.Node(lookaheadBody), this.Node(remainingTail) with
        // rewriting common word border uses
        | _, _ when
            isNonWordRight
            && (
                match (|PredStar|_|) this.Resolve remainingTail with
                | ValueSome _ -> true
                | _ -> false
            )
            ->
            // a\b.* -> a(?=(\W.*|\z)){...}
            let newNode = b.mkConcat2 (b.uniques._nonWordChar.Value, remainingTail)
            let conds = b.mkOr2 (newNode, RegexNodeId.END_ANCHOR)
            ValueSome(conds)
        // \b.+
        | _, Loop(node = loopBody; low = 1; up = Int32.MaxValue) when
            isNonWordRight
            && (
                match this.Node(loopBody) with
                | Singleton _ -> true
                | _ -> false
            )
            ->
            ValueSome(
                b.mkAndSeq (
                    seq {
                        b.mkConcat2 (lookaheadBody, RegexNodeId.TOP_STAR)
                        remainingTail
                    }
                )
            )
        // \b\s+abc
        | _, Concat _ when isNonWordRight ->
            ValueSome(
                b.mkAndSeq (
                    seq {
                        b.mkConcat2 (b.uniques._nonWordChar.Value, RegexNodeId.TOP_STAR)
                        remainingTail
                    }
                )
            )
        // \b(/[abc]*)
        | _, Loop(node = loopBody; low = 0) when
            isNonWordRight
            && (
                match this.Node(loopBody) with
                | Concat(head = ch) ->
                    (match this.Node(ch) with
                     | Singleton _ -> true
                     | _ -> false)
                | _ -> false
            )
            ->
            let case1 = origlookahead // \b

            let case2 = // /abc
                b.mkAndSeq (
                    seq {
                        b.mkConcat2 (b.uniques._nonWordChar.Value, RegexNodeId.TOP_STAR)
                        remainingTail
                    }
                )

            ValueSome(b.mkOr2 (case1, case2))

        | Singleton _, (Concat _ | Singleton _) ->
            ValueSome(
                b.mkAndSeq (
                    seq {
                        b.mkConcat2 (lookaheadBody, RegexNodeId.TOP_STAR)
                        remainingTail
                    }
                )
            )
        | Singleton _, Loop(low = low) when low > 0 ->
            ValueSome(
                b.mkAndSeq (
                    seq {
                        b.mkConcat2 (lookaheadBody, RegexNodeId.TOP_STAR)
                        remainingTail
                    }
                )
            )
        | _, _ ->
            let isPredStarOrAnd =
                match (|PredStar|_|) this.Resolve remainingTail with
                | ValueSome _ -> true
                | _ ->
                    match this.Node(remainingTail) with
                    | And _ -> true
                    | _ -> false

            if isPredStarOrAnd then
                ValueSome(
                    b.mkAndSeq (
                        seq {
                            b.mkConcat2 (lookaheadBody, RegexNodeId.TOP_STAR)
                            remainingTail
                        }
                    )
                )
            else
                ValueNone

    /// additional rewrites and checks if the pattern is supported
    member this.mkConcatChecked(nodesLeftToRight: RegexNodeId seq) : RegexNodeId =
        let nodesLeftToRight =
            nodesLeftToRight |> Seq.collect this.collectConcatNodes |> Seq.toArray

        let len = nodesLeftToRight.Length

        match nodesLeftToRight.Length with
        | 0 -> RegexNodeId.EPS
        | 1 ->
            let n0 = nodesLeftToRight[0]

            match this.Node(n0) with
            | Or(nodes = _) when _infos[n0].ContainsLookaround ->
                raise (
                    UnsupportedPatternException(
                        $"Lookarounds inside union not supported\nMove lookarounds/anchors outside union ^1$|^2$ -> ^(1|2)$"
                    )
                )
            | _ -> n0

        | _ ->

            match
                this.Node(nodesLeftToRight[len - 2]), this.Node(nodesLeftToRight[len - 1])
            with
            // merge suffixes
            | LookAround(node = node1; lookBack = false),
              LookAround(node = node2; lookBack = false) ->
                let combined =
                    this.mkAndSeq [
                        this.mkConcat2 (node1, RegexNodeId.TOP_STAR)
                        this.mkConcat2 (node2, RegexNodeId.TOP_STAR)
                    ]

                let mergedSuffix = this.mkLookaround (combined, false, 0, _emptyRefSet)

                let leftSide = nodesLeftToRight[.. len - 3]

                this.mkConcatChecked (
                    seq {
                        yield! leftSide
                        mergedSuffix
                    }
                )
            | _ ->

                match this.Node(nodesLeftToRight[0]), this.Node(nodesLeftToRight[1]) with
                // merge prefixes
                | LookAround(node = node1; lookBack = true),
                  LookAround(node = node2; lookBack = true) ->
                    let combined =
                        this.mkAndSeq [
                            this.mkConcat2 (RegexNodeId.TOP_STAR, node1)
                            this.mkConcat2 (RegexNodeId.TOP_STAR, node2)
                        ]

                    let mergedPrefix = this.mkLookaround (combined, true, 0, _emptyRefSet)

                    this.mkConcatChecked (
                        seq {
                            mergedPrefix
                            yield! nodesLeftToRight[2..]
                        }
                    )
                | _ ->

                    let mutable rewrittenNode = ValueNone

                    let throwExn(node: RegexNodeId) : RegexNodeId =
                        raise (
                            UnsupportedPatternException(
                                "this pattern contains unsupported anchors/lookarounds"
                            )
                        )

                    for i = 0 to nodesLeftToRight.Length - 1 do
                        let curr: RegexNodeId = nodesLeftToRight[i]

                        if rewrittenNode.IsSome then
                            ()
                        else
                            match this.Node(curr) with
                            // rewrite lookbacks
                            | LookAround(node = lookBody; lookBack = true) ->
                                // if prefix do nothing
                                if i = 0 then
                                    ()
                                else
                                    let maxLookbackLength = this.MaxLength(lookBody)

                                    match maxLookbackLength with
                                    | ValueSome 1 ->
                                        let leftNodes = nodesLeftToRight[.. i - 1]

                                        let minleft =
                                            leftNodes
                                            |> fold
                                                (ValueSome 0)
                                                (fun acc node ->
                                                    acc
                                                    |> bind (fun n ->
                                                        this.MinLength(node)
                                                        |> map (fun v -> v + n)
                                                    )
                                                )


                                        match minleft with
                                        | ValueSome n when n >= 1 ->
                                            // merge left
                                            let leftSide =
                                                b.mkConcatResizeArray (ResizeArray(leftNodes))

                                            let look =
                                                b.mkConcat2 (RegexNodeId.TOP_STAR, lookBody)

                                            let remainingTails =
                                                b.mkConcatChecked (nodesLeftToRight[i + 1 ..])

                                            let newNode =
                                                b.mkConcat2 (
                                                    b.mkAndSeq (
                                                        seq {
                                                            leftSide
                                                            look
                                                        }
                                                    ),
                                                    remainingTails
                                                )

                                            rewrittenNode <- ValueSome newNode
                                        | ValueSome n when n = 0 ->
                                            // merge left
                                            let leftSide =
                                                b.mkConcatResizeArray (ResizeArray(leftNodes))

                                            let leftSideTS =
                                                b.mkConcat2 (RegexNodeId.TOP_STAR, leftSide)

                                            let look =
                                                b.mkConcat2 (RegexNodeId.TOP_STAR, lookBody)

                                            let remainingTails =
                                                b.mkConcatChecked (nodesLeftToRight[i + 1 ..])

                                            let newNode =
                                                b.mkConcat2 (
                                                    b.mkAndSeq (
                                                        seq {
                                                            leftSideTS
                                                            look
                                                        }
                                                    ),
                                                    remainingTails
                                                )

                                            rewrittenNode <- ValueSome newNode


                                        | _ ->
                                            // experimental
                                            let leftSide =
                                                b.mkConcatResizeArray (ResizeArray(leftNodes))

                                            let look =
                                                b.mkConcat2 (RegexNodeId.TOP_STAR, lookBody)

                                            let remainingTails =
                                                b.mkConcatChecked (nodesLeftToRight[i + 1 ..])

                                            let newNode =
                                                b.mkConcat2 (
                                                    b.mkAndSeq (
                                                        seq {
                                                            leftSide
                                                            look
                                                        }
                                                    ),
                                                    remainingTails
                                                )

                                            rewrittenNode <- ValueSome newNode

                                    | _ -> throwExn (curr) |> ignore
                            // rewrite lookaheads
                            | LookAround(node = lookBody; lookBack = false) ->
                                // if suffix do nothing
                                if i + 1 = nodesLeftToRight.Length then
                                    ()
                                else

                                    let remainingTail =
                                        b.mkConcatChecked (nodesLeftToRight[i + 1 ..])

                                    let lookMaxLength =
                                        let (SplitTail this.Resolve (hd, t)) = lookBody

                                        if t = RegexNodeId.TOP_STAR then
                                            let newhead = b.mkConcatResizeArray (hd)
                                            this.MaxLength(newhead)
                                        else
                                            this.MaxLength(lookBody)

                                    match lookMaxLength with
                                    | ValueNone ->
                                        raise (
                                            UnsupportedPatternException(
                                                "unconstrained lookarounds are only supported as prefixes/suffixes"
                                            )
                                        )
                                    | _ ->

                                        let rewrite =
                                            match
                                                this.attemptRewriteCommonLookahead (
                                                    curr,
                                                    remainingTail
                                                )
                                            with
                                            | ValueSome v -> v
                                            | _ -> throwExn (curr)

                                        let newNode =
                                            b.mkConcatChecked (
                                                seq {
                                                    yield! nodesLeftToRight[.. i - 1]
                                                    rewrite
                                                }
                                            )

                                        rewrittenNode <- ValueSome newNode

                            | _ when _infos[curr].ContainsLookaround ->
                                // must rewrite prefix or suffix
                                if this.hasPrefixOrSuffix (curr) then
                                    match this.Node(curr) with
                                    // attempt combining every or branch
                                    | Or(nodes = nodes) ->
                                        let remainingTails = nodesLeftToRight[i + 1 ..]

                                        let nodesWithTails = [|
                                            for node in nodes do
                                                this.mkConcatChecked (
                                                    seq {
                                                        node
                                                        yield! remainingTails
                                                    }
                                                )
                                        |]

                                        let newOr = this.mkOrSeq nodesWithTails
                                        let remainingHeads = nodesLeftToRight[.. i - 1]

                                        let newNode =
                                            this.mkConcatResizeArray (
                                                ResizeArray(
                                                    seq {
                                                        yield! remainingHeads
                                                        newOr
                                                    }
                                                )
                                            )

                                        rewrittenNode <- ValueSome newNode
                                    // attempt adding more context to the lookaround
                                    | Concat(head = chead; tail = ctail) ->
                                        let remainingHeads = nodesLeftToRight[.. i - 1]
                                        let remainingTails = nodesLeftToRight[i + 1 ..]

                                        let newNode =
                                            this.mkConcatResizeArray (
                                                ResizeArray(
                                                    seq {
                                                        yield! remainingHeads
                                                        chead
                                                        ctail
                                                        yield! remainingTails
                                                    }
                                                )
                                            )

                                        rewrittenNode <- ValueSome newNode
                                    | _ -> throwExn (curr) |> ignore
                                else
                                    throwExn (curr) |> ignore
                            | _ -> ()

                    match rewrittenNode with
                    | ValueSome rewritten -> rewritten
                    | _ ->
                        nodesLeftToRight
                        |> fold_rev RegexNodeId.EPS (fun acc v -> this.mkConcat2 (v, acc))


    member this.mkConcatResizeArray(nodesLeftToRight: RegexNodeId ResizeArray) : RegexNodeId =
        nodesLeftToRight
        |> fold_rev RegexNodeId.EPS (fun acc v -> this.mkConcat2 (v, acc))


    member this.mkLoop(body: RegexNodeId, lower: int, upper: int) : RegexNodeId =
        assert (upper <> Int32.MaxValue + 1)

        let createNode(struct (body: RegexNodeId, lower: int, upper: int)) =

            let fallback(body, lower, upper) =
                let flags = Flags.inferLoop this.GetFlags (body, lower, upper)

                let info =
                    b.CreateInfo(
                        flags,
                        _infos[body].SubsumedByMinterm,
                        _infos[body].PendingNullables
                    )

                let id = _registerNode (RegexNode.Loop(body, lower, upper)) info
                b.CacheLengths(id)
                id

            match this.Node(body), lower, upper with
            | _, 0, 0 -> RegexNodeId.EPS
            | _, 1, 1 -> body
            | Loop(node = _; low = 0; up = Int32.MaxValue), 0, _ -> body
            | Loop(node = inner; low = 0; up = 1), 0, 1 -> this.mkLoop (inner, lower, upper)
            | LookAround(lookBack = true), 0, _ -> RegexNodeId.EPS
            | LookAround(lookBack = true), _, _ -> body
            // (.*a){5} -> (.*a){5,}
            | Concat(head = ch; tail = ct), lower, upper ->
                match (|PredStar|_|) this.Resolve ch with
                | ValueSome pstar ->
                    let tailSubMt = _infos[ct].SubsumedByMinterm

                    let isSubsumed = Solver.containsSet solver pstar tailSubMt

                    if lower > 0 && upper <> Int32.MaxValue then
                        if isSubsumed then
                            this.mkLoop (body, lower, Int32.MaxValue)
                        else
                            fallback (body, lower, upper)
                    elif lower = 1 && upper = Int32.MaxValue then
                        if isSubsumed then body else fallback (body, lower, upper)
                    else
                        fallback (body, lower, upper)
                | _ ->
                // (a.*){5} -> (a.*){5,}
                match (|PredStar|_|) this.Resolve ct with
                | ValueSome pstar ->
                    let headSubMt = _infos[ch].SubsumedByMinterm

                    let isSubsumed = Solver.containsSet solver pstar headSubMt

                    if lower > 0 && upper <> Int32.MaxValue then
                        if isSubsumed then
                            this.mkLoop (body, lower, Int32.MaxValue)
                        else
                            fallback (body, lower, upper)
                    elif lower = 1 && upper = Int32.MaxValue then
                        if isSubsumed then body else fallback (body, lower, upper)
                    else
                        fallback (body, lower, upper)
                | _ -> fallback (body, lower, upper)
            | _ -> fallback (body, lower, upper)

        match _nodeCache.TryGetValue(KLoop(body, lower, upper)) with
        | true, v -> v
        | _ ->
            let v = createNode (struct (body, lower, upper))
            _nodeCache.Add(KLoop(body, lower, upper), v)
            v

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.CreateInfo
        (flags: NodeFlags, containsMinterms: 't, nullables: RefSet)
        : RegexNodeInfo<_> =
        _createInfo flags containsMinterms nullables

    member this.CacheLengths(node: RegexNodeId) =
        _minLengthCache[node] <- computeMinLength _minLengthCache this.Resolve node
        _maxLengthCache[node] <- computeMaxLength _maxLengthCache this.Resolve node

    member this.MinLength(node: RegexNodeId) : voption<int> = _minLengthCache[node]
    member this.MaxLength(node: RegexNodeId) : voption<int> = _maxLengthCache[node]

    member this.GetFixedLength(node: RegexNodeId) : voption<int> =
        match this.MinLength(node), this.MaxLength(node) with
        | ValueSome m, ValueSome x when m = x -> ValueSome m
        | _ -> ValueNone


// for exact .NET semantics we'd need this as well
// https://github.com/dotnet/runtime/blob/1fe9c0bba15e23b65be007ddf38c43d28b2f9dd2/src/libraries/System.Text.RegularExpressions/src/System/Text/RegularExpressions/Symbolic/UnicodeCategoryConditions.cs#L67
// member this.wordCharForWordBorder = this.setFromStr "[\w\u200C\u200D]"
