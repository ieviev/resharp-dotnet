module internal rec Resharp.Algorithm

open System
open System.Numerics
open Resharp.Types
open Resharp.Common
open Resharp.Patterns
open Resharp.Runtime
open Resharp.Internal

module RegexNode =

    /// reverse a regex; asdf -> fdsa
    let rec rev (builder: RegexBuilder<_>) (nodeId: RegexNodeId) =
        let resolve = builder.Resolve

        match builder.Node(nodeId) with
        | Singleton _ -> nodeId
        | Or(nodes = xs) ->
            use mutable vl = new ValueList<RegexNodeId>(xs.Length)
            for x in xs do vl.Add(rev builder x)
            let mem = vl.ToArray()
            builder.mkOr mem
        | Loop nodes ->
            let body = nodes[0]
            let low = nodes[1]
            let up = nodes[2]
            let body' = rev builder body
            builder.mkLoop (body', low, up)
        | And(nodes = xs) ->
            use mutable vl = new ValueList<RegexNodeId>(xs.Length)
            for x in xs do vl.Add(rev builder x)
            let mem = vl.ToArray()
            builder.mkAnd mem
        | Not nodes ->
            let inner = nodes[0]
            let inner' = rev builder inner
            builder.mkNot inner'
        | LookAhead nodes ->
            let node' = nodes[0]
            match builder.Node(node') with
            | _ ->
                let (SplitTail resolve (heads, tail)) = node'

                match tail = RegexNodeId.TOP_STAR with
                | true ->
                    let revBody = rev builder (builder.mkConcatResizeArray (heads))
                    builder.mkLookaround (revBody, true, 0, builder.emptyRefSet)
                | _ ->
                    let revBody = rev builder node'
                    builder.mkLookaround (revBody, true, 0, builder.emptyRefSet)
        | LookBehind nodes ->
            let node' = nodes[0]
            match builder.Node(node') with
            | Concat cnodes ->
                let head = cnodes[0]
                let tail = cnodes[1]
                match head = RegexNodeId.TOP_STAR with
                | true ->
                    let revBody = rev builder tail
                    builder.mkLookaround (revBody, false, 0, builder.emptyRefSet)
                | _ ->
                    let revBody = rev builder node'
                    builder.mkLookaround (revBody, false, 0, builder.emptyRefSet)
            | _ ->
                let revBody = rev builder node'
                builder.mkLookaround (revBody, false, 0, builder.emptyRefSet)
        | Concat _ ->
            let acc = ResizeArray()
            let mutable curr = nodeId

            while (match builder.Node(curr) with
                   | Concat nodes ->
                       acc.Add(rev builder nodes[0])
                       curr <- nodes[1]
                       true
                   | _ ->
                       acc.Add(rev builder curr)
                       false) do
                ()

            acc.Reverse()
            builder.mkConcatResizeArray acc
        | Begin
        | End -> nodeId


    /// whether the regex matches an empty string
    let rec isNullable(b: RegexBuilder<'t>, loc: LocationKind, nodeId: RegexNodeId) : bool =
        let info = b.Info(nodeId)
        // short-circuit
        if not info.CanBeNullable then
            false
        elif info.IsAlwaysNullable then
            true
        else
            match b.Node(nodeId) with
            | Singleton _ -> false
            | Or(xs) ->
                let mutable found = false
                for v in xs do
                    if not found then found <- isNullable (b, loc, v)
                found
            | And(xs) ->
                let mutable all = true
                for v in xs do
                    if all then all <- isNullable (b, loc, v)
                all
            | Loop nodes -> nodes[1] = 0 || (isNullable (b, loc, nodes[0]))
            | Not nodes -> not (isNullable (b, loc, nodes[0]))
            | Concat nodes -> isNullable (b, loc, nodes[0]) && isNullable (b, loc, nodes[1])
            | LookAhead nodes | LookBehind nodes -> isNullable (b, loc, nodes[0])
            | End -> loc = LocationKind.End
            | Begin -> loc = LocationKind.Begin


    let rec derivative
        (b: RegexBuilder<'t>, loc: LocationKind, loc_pred: 't, nodeId: RegexNodeId)
        : RegexNodeId =
        let nodeInfo = b.Info(nodeId)

        let transitions =
            match loc with
            | LocationKind.Begin when nodeInfo.NodeFlags.DependsOnAnchor ->
                b.StartTransitions
            | LocationKind.End when nodeInfo.NodeFlags.DependsOnAnchor ->
                b.EndTransitions
            | _ -> b.Transitions

        let key = struct(nodeId, loc_pred)

        let result =
            match transitions.TryGetValue(key) with
            | true, inf -> inf
            | _ ->

                match b.Node(nodeId) with
                | Singleton nodes ->
                    let pred = b.GetTSet(nodes[0])
                    if b.Solver.elemOfSet pred loc_pred then
                        RegexNodeId.EPS
                    else
                        RegexNodeId.BOT
                | Loop nodes ->
                    let r = nodes[0]
                    let low = nodes[1]
                    let up = nodes[2]
                    let inline decr x =
                        if x = Int32.MaxValue || x = 0 then x else x - 1

                    let R_decr = b.mkLoop (r, decr low, decr up)
                    b.mkConcat2 (derivative (b, loc, loc_pred, r), R_decr)

                | Or(nodes) ->
                    use mutable derivatives = new ValueList<RegexNodeId>(16)

                    for node in nodes do
                        let der = derivative (b, loc, loc_pred, node)

                        if der <> RegexNodeId.BOT then
                            derivatives.Add(der)

                    match derivatives.size with
                    | 0 -> RegexNodeId.BOT
                    | 1 -> derivatives.AsSpan()[0]
                    | _ ->
                        derivatives.AsSpan().Sort()
                        let mem = derivatives.ToArray()
                        b.mkOr (mem)


                | And(nodes) ->
                    use mutable derivatives = new ValueList<RegexNodeId>(16)

                    for node in nodes do
                        let der = derivative (b, loc, loc_pred, node)

                        if der <> RegexNodeId.TOP_STAR then
                            derivatives.Add(der)

                    match derivatives.size with
                    | 0 -> RegexNodeId.TOP_STAR
                    | 1 -> derivatives.AsSpan()[0]
                    | _ ->
                        derivatives.AsSpan().Sort()
                        let mem = derivatives.ToArray()
                        let res = b.mkAnd (mem)
                        res

                | Not nodes -> b.mkNot (derivative (b, loc, loc_pred, nodes[0]))
                | Concat nodes ->
                    let head = nodes[0]
                    let tail = nodes[1]
                    let R' = derivative (b, loc, loc_pred, head)
                    let R'S = b.mkConcat2 (R', tail)

                    if RegexNode.isNullable (b, loc, head) then
                        let S' = derivative (b, loc, loc_pred, tail)

                        if S' = RegexNodeId.BOT then R'S
                        else if R'S = RegexNodeId.BOT then S'
                        else b.mkOr2 (R'S, S')
                    else
                        R'S
                // Lookahead
                | LookAhead nodes ->
                    let r = nodes[0]
                    let rel = nodes[1]
                    let pendingNulls = nodes[2]
                    let der_R = derivative (b, loc, loc_pred, r)

                    match der_R with
                    // start a new pending match
                    | _ when pendingNulls = RefSetId.Empty ->
                        match RegexNode.isNullable (b, loc, der_R) with
                        // initialize the first relative nullable position
                        | true -> b.mkLookaround (der_R, false, rel + 1, b.zeroListRefSet)
                        | false ->
                            match b.Node(der_R) with
                            | Concat cnodes ->
                                let ch = cnodes[0]
                                let ct = cnodes[1]
                                match ch = RegexNodeId.TOP_STAR with
                                | true ->
                                    match b.Node(ct) with
                                    | Concat cnodes2 when
                                        (match b.Node(cnodes2[0]) with
                                         | Begin
                                         | End -> true
                                         | _ -> false)
                                        && (cnodes2[1] = RegexNodeId.TOP_STAR)
                                        ->
                                        b.mkLookaround (
                                            RegexNodeId.EPS,
                                            false,
                                            rel + 1,
                                            b.zeroListRefSet
                                        )
                                    | Begin
                                    | End ->
                                        b.mkLookaround (
                                            RegexNodeId.EPS,
                                            false,
                                            rel + 1,
                                            b.zeroListRefSet
                                        )
                                    | _ ->
                                        b.mkLookaround (
                                            der_R,
                                            false,
                                            rel + 1,
                                            b.zeroListRefSet
                                        )
                                | _ -> b.mkLookaround (der_R, false, rel + 1, b.zeroListRefSet)
                            | _ -> b.mkLookaround (der_R, false, rel + 1, b.zeroListRefSet)

                    | _ -> b.mkLookaround (der_R, false, rel + 1, b.ResolveRefSet(pendingNulls))
                // Lookback
                | LookBehind nodes ->
                    let r = nodes[0]
                    b.mkLookaround (derivative (b, loc, loc_pred, r), true, 0, b.emptyRefSet)

                | Begin
                | End -> RegexNodeId.BOT

        transitions.TryAdd(key, result) |> ignore
        result


let inline getMintermsLog(n: int) =
    if BitOperations.IsPow2(n) then
        (BitOperations.Log2(uint32 n))
    else
        (BitOperations.Log2(uint32 n)) + 1
