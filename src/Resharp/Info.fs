module internal Resharp.Info

open System
open System.Collections.Generic
open Resharp.Types
open Resharp.Internal

[<AutoOpen>]
module Node =


    let inline private getCached (cache: Dictionary<RegexNodeId, int voption>) id = cache[id]

    let computeMinLength
        (cache: Dictionary<RegexNodeId, int voption>)
        (resolve: RegexNodeId -> RegexNode<_>)
        (nodeId: RegexNodeId)
        : int voption =
        match resolve nodeId with
        | Concat(head, tail) ->
            match getCached cache head, getCached cache tail with
            | ValueSome h, ValueSome t -> ValueSome(h + t)
            | _ -> ValueNone
        | Or(nodes) ->
            let lengths = nodes |> map (getCached cache)
            let allSome = lengths |> forall is_some
            if allSome then lengths |> Array.minBy value else ValueNone
        | And(nodes) ->
            let lengths = nodes |> map (getCached cache)
            let allSome = lengths |> forall is_some
            if allSome then lengths |> Array.maxBy value else ValueNone
        | Singleton _ -> ValueSome 1
        | Loop(node = body; low = low) ->
            match resolve body with
            | Singleton _ -> ValueSome low
            | _ -> ValueNone
        | Not _ -> ValueNone
        | LookAround _ -> ValueSome 0
        | Begin
        | End -> ValueSome 0

    let computeMaxLength
        (cache: Dictionary<RegexNodeId, int voption>)
        (resolve: RegexNodeId -> RegexNode<_>)
        (nodeId: RegexNodeId)
        : int voption =
        match resolve nodeId with
        | Concat(head, tail) ->
            match getCached cache head, getCached cache tail with
            | ValueSome h, ValueSome t -> ValueSome(h + t)
            | _ -> ValueNone
        | Or(nodes) ->
            let lengths = nodes |> map (getCached cache)
            let allSome = lengths |> forall is_some
            if allSome then lengths |> Array.maxBy value else ValueNone
        | And(nodes) ->
            let lengths = nodes |> map (getCached cache)
            let allSome = lengths |> forall is_some
            if allSome then lengths |> Array.minBy value else ValueNone
        | Singleton _ -> ValueSome 1
        | Loop(node = body; up = up) ->
            match resolve body with
            | Singleton _ -> if up = Int32.MaxValue then ValueNone else ValueSome up
            | _ -> ValueNone
        | Not _ -> ValueNone
        | LookAround _ -> ValueSome 0
        | Begin
        | End -> ValueSome 0


module rec Flags =
    open System.Runtime.CompilerServices

    let inline private getNullFlags(flags: NodeFlags) =
        flags &&& (NodeFlags.CanBeNullableFlag ||| NodeFlags.IsAlwaysNullableFlag)

    let inline private getAncFlag(flags: NodeFlags) = flags &&& NodeFlags.DependsOnAnchorFlag

    let rec inferLoop (getFlags: RegexNodeId -> NodeFlags) (R, lower, _) =
        let nullableLoopFlag =
            match lower with
            | 0 -> NodeFlags.CanBeNullableFlag ||| NodeFlags.IsAlwaysNullableFlag
            | _ -> NodeFlags.None

        inferNode getFlags R ||| nullableLoopFlag

    let inferAnd (getFlags: RegexNodeId -> NodeFlags) (xs: RegexNodeId array) : NodeFlags =
        xs
        |> fold
            (NodeFlags.CanBeNullableFlag ||| NodeFlags.IsAlwaysNullableFlag)
            (fun acc v ->
                let flags = inferNode getFlags v

                let orflags =
                    (acc ||| flags)
                    &&& (NodeFlags.ContainsLookaroundFlag ||| NodeFlags.DependsOnAnchorFlag)

                let andflags =
                    (acc &&& flags)
                    &&& (NodeFlags.CanBeNullableFlag ||| NodeFlags.IsAlwaysNullableFlag)

                orflags ||| andflags
            )


    [<Literal>]
    let all_orflags =
        (NodeFlags.CanBeNullableFlag
         ||| NodeFlags.IsAlwaysNullableFlag
         ||| NodeFlags.ContainsLookaroundFlag
         ||| NodeFlags.DependsOnAnchorFlag
         ||| NodeFlags.HasSuffixLookaheadFlag
         ||| NodeFlags.HasPrefixLookbehindFlag)


    let rec inferOr (getFlags: RegexNodeId -> NodeFlags) (xs: RegexNodeId[]) : NodeFlags =
        xs
        |> Array.fold
            (fun acc b -> (inferNode getFlags b &&& all_orflags) ||| acc)
            NodeFlags.None

    let inferConcat
        (getFlags: RegexNodeId -> NodeFlags)
        (resolve: RegexNodeId -> RegexNode<_>)
        (head: RegexNodeId)
        (tail: RegexNodeId)
        =
        let h1 = inferNode getFlags head
        let t1 = inferNode getFlags tail
        let orFlags = h1 ||| t1 &&& NodeFlags.ContainsLookaroundFlag

        let andFlags =
            h1 &&& t1 &&& (NodeFlags.IsAlwaysNullableFlag ||| NodeFlags.CanBeNullableFlag)

        let dependsOnAnchor = h1.DependsOnAnchor || (h1.CanBeNullable && t1.DependsOnAnchor)

        let dependsOnFlags =
            if dependsOnAnchor then
                NodeFlags.DependsOnAnchorFlag
            else
                NodeFlags.None

        let lookaroundFlags =
            let suf =
                let tailFlags = getFlags tail

                if tailFlags.HasSuffixLookahead then
                    NodeFlags.HasSuffixLookaheadFlag
                else
                    match resolve tail with
                    | LookAround(lookBack = false) -> NodeFlags.HasSuffixLookaheadFlag
                    | _ -> NodeFlags.None

            let pref =
                let headFlags = getFlags head

                if headFlags.HasPrefixLookbehind then
                    NodeFlags.HasPrefixLookbehindFlag
                else
                    match resolve head with
                    | LookAround(lookBack = true) -> NodeFlags.HasPrefixLookbehindFlag
                    | _ -> NodeFlags.None

            pref ||| suf

        andFlags ||| orFlags ||| dependsOnFlags ||| lookaroundFlags

    let inferLookaround
        (getFlags: RegexNodeId -> NodeFlags)
        (inner: RegexNodeId)
        (lookBack: bool)
        =
        let innerFlags = getFlags inner
        let nullFlags = getNullFlags innerFlags
        let ancFlag = getAncFlag innerFlags

        match lookBack with
        // lookahead
        | false ->
            nullFlags
            ||| NodeFlags.ContainsLookaroundFlag
            ||| NodeFlags.HasSuffixLookaheadFlag
            ||| ancFlag
        // lookback
        | true ->
            ancFlag
            ||| nullFlags
            ||| NodeFlags.ContainsLookaroundFlag
            ||| NodeFlags.HasPrefixLookbehindFlag

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inferNode (getFlags: RegexNodeId -> NodeFlags) (nodeId: RegexNodeId) : NodeFlags =
        getFlags nodeId

    let inferCompl (getFlags: RegexNodeId -> NodeFlags) (inner: RegexNodeId) =
        let innerInfo = inferNode getFlags inner

        let nullableFlags =
            match innerInfo.CanBeNullable, innerInfo.IsAlwaysNullable with
            | false, _ -> NodeFlags.IsAlwaysNullableFlag ||| NodeFlags.CanBeNullableFlag
            | true, true -> NodeFlags.None
            // conditionally nullable
            | true, false -> NodeFlags.CanBeNullableFlag

        let otherFlags =
            innerInfo
            &&& (NodeFlags.ContainsLookaroundFlag ||| NodeFlags.DependsOnAnchorFlag)

        nullableFlags ||| otherFlags
