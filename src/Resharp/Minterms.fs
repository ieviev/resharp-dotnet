module internal Resharp.Minterms

open System
open System.Collections.Generic
open Resharp.Types
open Resharp.Common
open Resharp.Runtime
open Resharp.Internal

let rec transform
    (oldBuilder: RegexBuilder<BDD>)
    (builder: RegexBuilder<'tnewset>)
    (charsetSolver: CharSetSolver)
    (newSolver: ISolver<'tnewset>)
    (nodeId: RegexNodeId)
    : RegexNodeId =

    let inline transformInner v = transform oldBuilder builder charsetSolver newSolver v
    match oldBuilder.Node(nodeId) with
    | Singleton tset -> builder.one(newSolver.ConvertFromBDD(tset, charsetSolver))
    | Not(xs) -> builder.mkNot(transformInner xs)
    | And (xs) ->
        let xs' = xs |> Array.map transformInner
        builder.mkAndSeq(xs')
    | Or (xs) ->
        let xs' = xs |> Array.map transformInner
        builder.mkOrSeq(xs')
    | Loop (xs, lower, upper) ->
        let xs' = transformInner xs
        builder.mkLoop(xs',lower,upper)
    | LookAround (body, back, rel, pendingNullable) ->
        if nodeId = oldBuilder.anchors._nonWordLeft.Value then
            builder.anchors._nonWordLeft.Value
        elif nodeId = oldBuilder.anchors._nonWordRight.Value then
            builder.anchors._nonWordRight.Value
        else
            builder.mkLookaround(transformInner body,back,rel,pendingNullable)
    | Concat(head,tail) ->
        let head' = transformInner head
        let tail' = transformInner tail
        builder.mkConcat2(head',tail')
    | Begin -> RegexNodeId.BEGIN_ANCHOR
    | End -> RegexNodeId.END_ANCHOR



let rec transformBack
    (bdds:BDD[])
    (oldBuilder: RegexBuilder<'t>)
    (builder: RegexBuilder<BDD>)
    (newSolver: ISolver<'t>)
    (charsetSolver: CharSetSolver)

    (nodeId: RegexNodeId)
    : RegexNodeId =

    let inline transformInner v = transformBack bdds oldBuilder builder newSolver charsetSolver v
    match oldBuilder.Node(nodeId) with
    | Singleton tset ->
        let bdd : BDD = newSolver.convertToBdd(charsetSolver, bdds, tset)
        builder.one(bdd)
    | Not(xs) -> builder.mkNot(transformInner xs)
    | And (xs) ->
        let xs' = xs |> Array.map transformInner
        builder.mkAndSeq(xs')
    | Or (xs) ->
        let xs' = xs |> Array.map transformInner
        builder.mkOrSeq(xs')
    | Loop (xs, lower, upper) ->
        let xs' = transformInner xs
        builder.mkLoop(xs',lower,upper)
    | LookAround (body, back, rel, pendingNullable) ->
        builder.mkLookaround(transformInner body,back,rel,pendingNullable)
    | Concat(head,tail) ->
        let head' = transformInner head
        let tail' = transformInner tail
        builder.mkConcat2(head',tail')
    | Begin -> RegexNodeId.BEGIN_ANCHOR
    | End -> RegexNodeId.END_ANCHOR

let collectSets (builder: RegexBuilder<'tset>) (nodeId: RegexNodeId) =
    let hs = HashSet()
    let rec collect (id: RegexNodeId) : unit =
        match builder.Node(id) with
        | Singleton pred -> hs.Add pred |> ignore
        | And (nodes=xs)
        | Or (nodes=xs) -> xs |> iter collect
        | LookAround (node=node)
        | Not (node=node)
        | Loop (node=node) -> collect node
        | Concat(head,tail) ->
            collect head
            collect tail
        | Begin | End -> ()
    collect nodeId
    hs

let compute (solver: ISolver<'tset>) (builder: RegexBuilder<'tset>) (nodeId: RegexNodeId) =
    let hs = collectSets builder nodeId
    let list = MintermGenerator<'tset>.GenerateMinterms (solver, hs)
    list.Sort()
    list.ToArray()

let createLookupUtf16 (bdds:BDD[]) =
    let arr = Array.zeroCreate<byte> (int UInt16.MaxValue + 1)
    if bdds.Length = 1 then arr else
    for mtId = 1 to bdds.Length - 1 do
        let bdd = bdds[mtId]
        let ranges = BDDRangeConverter.ToRanges(bdd)
        for rstart,rend in ranges do
            let slice = arr.AsSpan(int rstart, int (rend - rstart + 1u))
            slice.Fill(byte mtId)
    arr
