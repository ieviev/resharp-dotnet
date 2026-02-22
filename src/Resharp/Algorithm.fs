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
            let xs' = xs |> map (rev builder)
            let mem = xs'.AsMemory()
            builder.mkOr &mem
        | Loop(xs, low, up) ->
            let xs' = rev builder xs
            builder.mkLoop (xs', low, up)
        | And(nodes = xs) ->
            let xs' = xs |> map (rev builder)
            let mem = xs'.AsMemory()
            builder.mkAnd &mem
        | Not(xs) ->
            let xs' = rev builder xs
            builder.mkNot xs'
        | LookAround(node = node'; lookBack = false) ->
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
        | LookAround(node = node'; lookBack = true) ->
            match builder.Node(node') with
            | Concat(head = head; tail = tail) ->
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
                   | Concat(head, tail) ->
                       acc.Add(rev builder head)
                       curr <- tail
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
            | Or(xs) -> xs |> exists (fun v -> isNullable (b, loc, v))
            | And(xs) -> xs |> forall (fun v -> isNullable (b, loc, v))
            | Loop(node = r; low = low) -> low = 0 || (isNullable (b, loc, r))
            | Not(inner) -> not (isNullable (b, loc, inner))
            | Concat(head, tail) -> isNullable (b, loc, head) && isNullable (b, loc, tail)
            | LookAround(node = body) -> isNullable (b, loc, body)
            | End -> loc = LocationKind.End
            | Begin -> loc = LocationKind.Begin


    let rec derivative
        (b: RegexBuilder<'t>, loc: LocationKind, loc_pred: 't, nodeId: RegexNodeId)
        : RegexNodeId =
        let nodeInfo = b.Info(nodeId)

        let transitions =
            match loc with
            | LocationKind.Begin when nodeInfo.NodeFlags.DependsOnAnchor ->
                nodeInfo.StartTransitions
            | LocationKind.End when nodeInfo.NodeFlags.DependsOnAnchor ->
                nodeInfo.EndTransitions
            | _ -> nodeInfo.Transitions

        let result =
            match transitions.TryGetValue(loc_pred) with
            | true, inf -> inf
            | _ ->

                match b.Node(nodeId) with
                | Singleton pred ->
                    if b.Solver.elemOfSet pred loc_pred then
                        RegexNodeId.EPS
                    else
                        RegexNodeId.BOT
                | Loop(r, low, up) ->
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
                        let mem = derivatives.AsMemory()
                        b.mkOr (&mem)


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
                        let mem = derivatives.AsMemory()
                        let res = b.mkAnd (&mem)
                        res

                | Not(inner) -> b.mkNot (derivative (b, loc, loc_pred, inner))
                | Concat(head, tail) ->
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
                | LookAround(
                    node = r
                    lookBack = false
                    relativeTo = rel
                    pendingNullables = pendingNulls) ->
                    let der_R = derivative (b, loc, loc_pred, r)

                    match der_R with
                    // start a new pending match
                    | _ when pendingNulls.IsEmpty ->
                        match RegexNode.isNullable (b, loc, der_R) with
                        // initialize the first relative nullable position
                        | true -> b.mkLookaround (der_R, false, rel + 1, b.zeroListRefSet)
                        | false ->
                            match b.Node(der_R) with
                            | Concat(head = ch; tail = ct) ->
                                match ch = RegexNodeId.TOP_STAR with
                                | true ->
                                    match b.Node(ct) with
                                    | Concat(head = cth; tail = ctt) when
                                        (match b.Node(cth) with
                                         | Begin
                                         | End -> true
                                         | _ -> false)
                                        && (ctt = RegexNodeId.TOP_STAR)
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

                    | _ -> b.mkLookaround (der_R, false, rel + 1, pendingNulls)
                // Lookback
                | LookAround(node = r; lookBack = true) ->
                    match b.Node(r) with
                    | Concat(head = head; tail = tail) ->

                        b.mkLookaround (
                            derivative (
                                b,
                                loc,
                                loc_pred,
                                if head = RegexNodeId.TOP_STAR then tail else r
                            ),
                            true,
                            0,
                            b.emptyRefSet
                        )

                    | _ ->
                        b.mkLookaround (
                            derivative (b, loc, loc_pred, r),
                            true,
                            0,
                            b.emptyRefSet
                        )

                | Begin
                | End -> RegexNodeId.BOT

        transitions.TryAdd(loc_pred, result) |> ignore
        result


let inline getMintermsLog(n: int) =
    if BitOperations.IsPow2(n) then
        (BitOperations.Log2(uint32 n))
    else
        (BitOperations.Log2(uint32 n)) + 1
