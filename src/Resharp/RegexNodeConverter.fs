module internal rec Resharp.RegexNodeConverter

open Resharp.Types
open Resharp
open Resharp.Runtime
open Resharp.Internal

let children2Seq(node: Resharp.Runtime.RegexNode) =
    seq {
        for i = 0 to node.ChildCount() - 1 do
            yield node.Child(i)
    }


let rewriteNegativeLookaround
    (b: RegexBuilder<BDD>)
    (lookBack: bool)
    (node: RegexNodeId)
    : RegexNodeId =
    match lookBack, b.Node(node) with
    | false, Singleton pred ->
        // (?!\w) = (?=\z|\W)
        let flipped = b.one (b.Solver.Not(pred))
        let conc = b.mkOr2 (flipped, b.anchors._zAnchor)
        b.mkLookaround (conc, false, 0, b.emptyRefSet)
    | true, Singleton pred ->
        // (?<!\w) = (?<=\A|\W)
        let flipped = b.one (b.Solver.Not(pred))
        let conc = b.mkOr2 (b.anchors._bigAAnchor, flipped)
        b.mkLookaround (conc, true, 0, b.emptyRefSet)
    | false, _ ->
        // (?=~(R·_*)·\z) ≡ (?!R)
        let negpart = b.mkNot (b.mkConcat2 (node, RegexNodeId.TOP_STAR))
        let conc = b.mkConcat2 (negpart, b.anchors._zAnchor)
        b.mkLookaround (conc, false, 0, b.emptyRefSet)
    | true, _ ->
        // (?<=\A·~(_*R)) ≡ (?<!R)
        let negpart = b.mkNot (b.mkConcat2 (RegexNodeId.TOP_STAR, node))
        let conc = b.mkConcat2 (b.anchors._bigAAnchor, negpart)
        b.mkLookaround (conc, true, 0, b.emptyRefSet)

[<Struct>]
type WordCharKind =
    | WordChar
    | NonWordChar
    | WordOption
    | NonWordOption
    | Unknown
    | Edge

let rec determineWordBorderNodeKind
    (b: RegexBuilder<BDD>)
    (css: CharSetSolver)
    (left: bool)
    (node: RegexNode)
    : WordCharKind =
    let rtl = node.Options.HasFlag(RegexOptions.RightToLeft)
    let trueLeft = if rtl then not left else left
    let edgeIdx = if trueLeft then node.ChildCount() - 1 else 0

    let inferSet(setStr: string) =
        match setStr with
        | RegexCharClass.NotSpaceClass
        | RegexCharClass.NumberClass
        | RegexCharClass.AsciiLetterClass
        | RegexCharClass.DigitClass -> WordChar
        | RegexCharClass.WordClass -> WordChar
        | RegexCharClass.SpaceClass -> NonWordChar
        | RegexCharClass.NotDigitClass
        | RegexCharClass.AnyClass -> Unknown
        | _ ->
            let ranges = RegexCharClass.ComputeRanges setStr

            if isNull ranges then
                Unknown
            else
                // let bdd = css.CreateBDDFromRanges(ranges)
                let bdd = BDD.fromRanges css ranges

                let sbdd = b.bddFromClass RegexCharClass.SpaceClass
                // no whitespace
                if css.IsEmpty(css.And(bdd, sbdd)) then
                    WordChar
                else
                    let wbdd = b.bddFromClass RegexCharClass.WordClass
                    // no wordchars
                    if css.IsEmpty(css.And(bdd, wbdd)) then
                        NonWordChar
                    else
                        Unknown

    match node.Kind with
    | RegexNodeKind.One ->
        if RegexCharClass.IsBoundaryWordChar node.Ch then
            WordChar
        else
            NonWordChar
    | RegexNodeKind.Oneloop when node.M > 0 ->
        if RegexCharClass.IsBoundaryWordChar node.Ch then
            WordChar
        else
            NonWordChar
    | RegexNodeKind.Notoneloop -> Unknown

    | RegexNodeKind.Setloop ->
        let setInner = inferSet node.Str

        match setInner, node.M with
        | WordChar, 0 -> WordOption
        | setInner, _ -> setInner

    | RegexNodeKind.Notone -> Unknown //unhandled()
    | RegexNodeKind.Set -> inferSet node.Str

    | RegexNodeKind.Multi ->
        let chr =
            if trueLeft then
                node.Str[node.Str.Length - 1]
            else
                node.Str[0]

        if RegexCharClass.IsBoundaryWordChar chr then
            WordChar
        else
            NonWordChar

    | RegexNodeKind.Conjunction ->
        children2Seq node
        |> Seq.map (determineWordBorderNodeKind b css left)
        |> Seq.find (
            function
            | WordChar -> true
            | NonWordChar -> true
            | _ -> false
        )

    | RegexNodeKind.Capture
    | RegexNodeKind.Concatenate
    | RegexNodeKind.PositiveLookaround ->
        let edgeChild = node.Child(edgeIdx)
        determineWordBorderNodeKind b css left edgeChild
    | RegexNodeKind.Loop -> Unknown
    | _ -> Unknown


let toLeft
    (b: RegexBuilder<BDD>)
    (css: CharSetSolver)
    (outer: RegexNode array)
    (idx: int)
    : WordCharKind =
    match idx with
    | 0 -> Edge
    | n ->
        let temp = outer[n - 1]
        let kind = determineWordBorderNodeKind b css true temp

        match kind with
        | WordOption ->
            match toLeft b css outer (n - 1) with
            | WordChar -> WordChar
            | _ -> Unknown
        | NonWordOption ->
            match toLeft b css outer (n - 1) with
            | NonWordChar -> NonWordChar
            | _ -> Unknown
        | _ -> kind

let toRight (b: RegexBuilder<BDD>) (css: CharSetSolver) (outer: RegexNode array) idx =
    match idx with
    | n when n = outer.Length - 1 -> Edge
    | n ->
        let temp = outer[n + 1]
        let kind = determineWordBorderNodeKind b css false temp

        match kind with
        | WordOption ->
            match toRight b css outer (n + 1) with
            | WordChar -> WordChar
            | _ -> Unknown
        | NonWordOption ->
            match toRight b css outer (n + 1) with
            | NonWordChar -> NonWordChar
            | _ -> Unknown
        | _ -> kind


let rewriteWordBorder
    (b: RegexBuilder<BDD>)
    (css: CharSetSolver)
    (outer: RegexNode array)
    (idx: int)
    (_: RegexNode)
    =
    let left = toLeft b css outer idx
    let right = toRight b css outer idx

    match left, right with
    | NonWordChar, WordChar
    | WordChar, NonWordChar -> RegexNodeId.EPS // redundant word border, remove
    | WordChar, _ -> b.anchors._nonWordRight.Value // wordleft
    | NonWordChar, _ -> b.anchors._wordRight.Value // nonwordleft
    | _, WordChar -> b.anchors._nonWordLeft.Value // wordchar right
    | _, NonWordChar -> b.anchors._wordLeft.Value // nonwordright
    | Edge, Edge ->
        raise (
            UnsupportedPatternException
                @"\b is only supported when next to word or non-word characters"
        )
    | _ ->
        raise (
            UnsupportedPatternException
                @"Resharp does not support unconstrained word borders, rewrite \b.*\b to \b\w+\b or \b\s+\b to show which side the word is on"
        )


let convertToSymbolicRegexNode
    (css: CharSetSolver, builder: RegexBuilder<BDD>, rootNode: RegexNode)
    : RegexNodeId =
    let b = builder
    let mkConcat = b.mkConcatChecked

    let rec loop (acc: ResizeArray<RegexNodeId>) (node: RegexNode) : unit =

        let rec convertAdjacent
            (adjacent: RegexNode[])
            (idx: int)
            (node: RegexNode)
            : ResizeArray<RegexNodeId> =
            match node.Kind with
            | RegexNodeKind.Alternate ->
                let inner = node |> children2Seq

                let allrewritten =
                    inner
                    |> Seq.map (convertAdjacent adjacent idx)
                    |> Seq.map mkConcat
                    |> b.mkOrSeq

                let r = ResizeArray()
                r.Add(allrewritten)
                r
            | RegexNodeKind.Boundary ->
                let rewritten = rewriteWordBorder b css adjacent idx node
                let r = ResizeArray()
                r.Add(rewritten)
                r
            | RegexNodeKind.Loop when
                node.ChildCount() = 0 && node.Child(0).Kind = RegexNodeKind.Boundary
                ->
                let rewritten = rewriteWordBorder b css adjacent idx (node.Child(0))
                let r = ResizeArray()
                r.Add(rewritten)
                r
            | _ ->
                let r = ResizeArray()
                loop r node
                r

        let loopFresh (node: RegexNode) : ResizeArray<RegexNodeId> =
            let r = ResizeArray()
            loop r node
            r

        let convertConcat(outerConcat: RegexNode) : ResizeArray<RegexNodeId> =
            let outerCorrectOrder =
                match outerConcat.Options.HasFlag(RegexOptions.RightToLeft) with
                | true -> node |> children2Seq |> Seq.rev
                | _ -> node |> children2Seq
                |> Seq.toArray

            let r = ResizeArray()
            for i = 0 to outerCorrectOrder.Length - 1 do
                r.AddRange(convertAdjacent outerCorrectOrder i outerCorrectOrder[i])
            r

        let convertChildren(node: RegexNode) : ResizeArray<RegexNodeId> =
            let nodeseq =
                match node.Options.HasFlag(RegexOptions.RightToLeft) with
                | true -> node |> children2Seq |> Seq.rev
                | _ -> node |> children2Seq
                |> Seq.toArray

            let r = ResizeArray()
            for child in nodeseq do
                loop r child
            r

        match node.Kind with
        | RegexNodeKind.One -> acc.Add(b.one node.Ch)
        | RegexNodeKind.Notone -> acc.Add(b.notOne node.Ch)
        | RegexNodeKind.Set -> acc.Add(b.setFromNode node)
        | RegexNodeKind.Multi -> for ch in node.Str do acc.Add(b.one ch)
        | RegexNodeKind.Lazyloop
        | RegexNodeKind.Loop ->
            match node.Kind with
            | RegexNodeKind.Boundary ->
                raise (
                    UnsupportedPatternException(
                        $"turning a word boundary into a loop does not make any sense"
                    )
                )
            | _ ->
                let inner = mkConcat (convertChildren node)
                acc.Add(b.mkLoop (inner, node.M, node.N))
        | RegexNodeKind.Alternate ->
            let adjacent = node.Parent |> children2Seq |> Seq.toArray
            let ownIndex = adjacent |> Seq.findIndex (fun v -> v = node)

            let children2 =
                node
                |> children2Seq
                |> Seq.map (convertAdjacent adjacent ownIndex)
                |> Seq.map mkConcat

            acc.Add(builder.mkOrSeq children2)
        | RegexNodeKind.Conjunction ->
            let children2 = node |> children2Seq |> Seq.map loopFresh |> Seq.map mkConcat
            acc.Add(builder.mkAndSeq children2)
        | RegexNodeKind.Concatenate ->
            acc.AddRange(convertConcat node)
        | RegexNodeKind.Capture ->
            if node.N = -1 then
                acc.Add(convertConcat node |> mkConcat)
            else if node.Options.HasFlag(RegexOptions.Negated) then
                let inner = convertConcat node |> mkConcat
                acc.Add(b.mkNot inner)
            else
                acc.AddRange(convertConcat node)
        // Specialized loops
        | RegexNodeKind.Oneloop
        | RegexNodeKind.Onelazy
        | RegexNodeKind.Notoneloop
        | RegexNodeKind.Notonelazy ->
            let bdd =
                match node.IsNotoneFamily with
                | true -> css.Not(css.CreateBDDFromChar(node.Ch))
                | false -> css.CreateBDDFromChar(node.Ch)

            let single = b.one bdd
            acc.Add(b.mkLoop (single, node.M, node.N))
        // anchors
        | RegexNodeKind.Bol -> acc.Add(b.anchors._caretAnchor.Value)
        | RegexNodeKind.Beginning -> acc.Add(b.anchors._bigAAnchor)
        | RegexNodeKind.Eol -> acc.Add(b.anchors._dollarAnchor.Value)
        | RegexNodeKind.EndZ -> acc.Add(b.anchors._endZAnchor.Value)
        | RegexNodeKind.End -> acc.Add(b.anchors._zAnchor)
        | RegexNodeKind.Boundary ->
            raise (UnsupportedPatternException("Failed to parse word boundary"))
        | RegexNodeKind.NonBoundary ->
            raise (UnsupportedPatternException("Failed to parse word non-boundary"))
        | RegexNodeKind.Setlazy ->
            raise (UnsupportedPatternException("Resharp does not support lazy loops"))
        | RegexNodeKind.Setloop ->
            let set = node.Str
            let bdd = b.bddFromSetString set
            acc.Add(b.mkLoop (b.one bdd, node.M, node.N))
        | RegexNodeKind.Empty -> ()
        | RegexNodeKind.PositiveLookaround ->
            let conc = mkConcat (convertConcat node)

            acc.Add(
                builder.mkLookaround (
                    conc,
                    node.Options.HasFlag RegexOptions.RightToLeft,
                    0,
                    b.emptyRefSet
                )
            )
        | RegexNodeKind.NegativeLookaround ->
            let lookBack = node.Options.HasFlag RegexOptions.RightToLeft
            let lookBody = mkConcat (convertChildren node)
            acc.Add(rewriteNegativeLookaround b lookBack lookBody)
        | other -> failwith $"RegexNodeKind conversion not implemented: {other}, \n{rootNode}"

    let acc = ResizeArray()
    loop acc rootNode
    mkConcat acc
