namespace Resharp.Internal

// this file contains various utilities that are generally more efficient
// than the ones in the F# standard library

open System.Collections.Generic
open System.Runtime.CompilerServices
open System

#if !FABLE_COMPILER
open System.Threading.Tasks
#endif


[<AbstractClass>]
module Fsil =


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Value =
        static member inline Value(x: Result<'t, _>) : 't =
            match x with
            | Ok(v) -> v
            | _ -> failwith "no value"

        static member inline Value(x: ^t) : ^v = (^t: (member Value: ^v) x)

        static member inline Invoke< ^I, ^v when (^I or Value): (static member Value: ^I -> ^v)>
            (source: _)
            =
            ((^I or Value): (static member Value: ^I -> ^v) (source))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type IsEmpty =
        // if it has length defined then use length
        static member inline IsEmpty(x: ^t) : bool = (^t: (member Length: int) x) = 0
        static member inline IsEmpty(x: option<'t>) : bool = x.IsNone
        static member inline IsEmpty(x: voption<'t>) : bool = x.IsNone

        static member inline IsEmpty(x: Result<'t, _>) : bool =
            match x with
            | Ok(_) -> false
            | _ -> true

        static member inline IsEmpty(x: ResizeArray<'t>) : bool = x.Count = 0
        static member inline IsEmpty(x: Dictionary<'k, 'v>) : bool = x.Count = 0


        static member inline Invoke< ^I
            when (^I or IsEmpty): (static member IsEmpty: ^I -> bool)>
            (source: _)
            : bool =
            ((^I or IsEmpty): (static member IsEmpty: ^I -> bool) (source))


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Length =

        static member inline Length(x: option<'t>) : int =
            match x with
            | Some _ -> 1
            | _ -> 0

        static member inline Length(x: voption<'t>) : int =
            match x with
            | ValueSome _ -> 1
            | _ -> 0

        static member inline Length(x: Result<'t, _>) : int =
            match x with
            | Ok(_) -> 1
            | _ -> 0

        static member inline Length(x: ResizeArray<'t>) : int = x.Count


        static member inline Length(x: ^t) : int = (^t: (member Length: int) x)
        static member inline Length(x: Dictionary<'k, 'v>) : int = x.Count


        static member inline Invoke< ^I when (^I or Length): (static member Length: ^I -> int)>
            (source: _)
            : int =
            ((^I or Length): (static member Length: _ -> _) (source))


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Item =
        static member inline Item(x: Dictionary< ^k, ^v >, key: ^k) : ^v = x[key]
        static member inline Item(x: array< ^t >, key: int) : ^t = x[key]
        static member inline Item(x: list< ^t >, key: int) : ^t = x.Item key

        static member inline Item< ^I, ^k, ^v when ^I: (member Item: ^k -> ^v)>
            (x: ^I, key: ^k)
            : ^v =
            x.Item key

        static member inline Invoke< ^I, ^k, ^v
            when (^I or Item): (static member Item: ^I * ^k -> ^v)>
            (source: _, key: ^k)
            : _ =
            ((^I or Item): (static member Item: ^I * ^k -> ^v) (source, key))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type IterateWhile =

        static member inline IterateWhile(x: 't[], [<InlineIfLambda>] f: 't -> bool) : bool =
            let mutable i = 0
            let length = x.Length
            let mutable cont = true

            while (i < length
                   && (cont <- f x[i]
                       cont)) do
                i <- i + 1

            cont

        static member inline IterateWhile
            (x: Span<'t>, [<InlineIfLambda>] f: 't -> bool)
            : bool =
            let mutable i = 0
            let length = x.Length
            let mutable cont = true

            while (i < length
                   && (cont <- f x[i]
                       cont)) do
                i <- i + 1

            cont

        static member inline IterateWhile
            (x: Dictionary<'k, 'v>, [<InlineIfLambda>] f: KeyValuePair<'k, 'v> -> bool)
            : bool =
            use mutable e = x.GetEnumerator()
            let mutable cont = true

            while (e.MoveNext()
                   && (cont <- f e.Current
                       cont)) do
                ()

            cont


        static member inline IterateWhile
            (x: list<'k>, [<InlineIfLambda>] f: 'k -> bool)
            : bool =
            use mutable e = (x :> seq<_>).GetEnumerator()
            let mutable cont = true

            while (e.MoveNext()
                   && (cont <- f e.Current
                       cont)) do
                ()

            cont

        static member inline IterateWhile
            (x: HashSet<'k>, [<InlineIfLambda>] f: 'k -> bool)
            : bool =
            use mutable e = x.GetEnumerator()
            let mutable cont = true

            while (e.MoveNext()
                   && (cont <- f e.Current
                       cont)) do
                ()

            cont


        static member inline IterateWhile
            (x: option<'t>, [<InlineIfLambda>] f: 't -> bool)
            : bool =
            if x.IsSome then f x.Value else false


        static member inline IterateWhile
            (x: voption<'t>, [<InlineIfLambda>] f: 't -> bool)
            : bool =
            if x.IsSome then f x.Value else false

        static member inline Invoke< ^I, ^t
            when (^I or IterateWhile): (static member IterateWhile: ^I * (^t -> bool) -> bool)>
            (source: _, [<InlineIfLambda>] action: _)
            : bool =
            ((^I or IterateWhile): (static member IterateWhile: ^I * (^t -> bool) -> bool) (source,
                                                                                            action))


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type TryItem =
        static member inline TryItem(x: array<'t>, key: int) : voption<'t> =
            if key < x.Length then ValueSome(x[key]) else ValueNone

        static member inline TryItem(x: Dictionary<'k, 'v>, key: 'k) : voption<'v> =
            match x.TryGetValue(key) with
            | true, v -> ValueSome(v)
            | _ -> ValueNone

        static member inline Invoke< ^I, ^k, ^v
            when (^I or TryItem): (static member TryItem: ^I * ^k -> voption< ^v >)>
            (source: _, key: ^k)
            : _ =
            ((^I or TryItem): (static member TryItem: ^I * ^k -> voption< ^v >) (source, key))


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Iterate =

        static member inline Iterate(x: option<'t>, [<InlineIfLambda>] f: 't -> unit) : unit =
            Option.iter f x

        static member inline Iterate(x: voption<'t>, [<InlineIfLambda>] f: 't -> unit) : unit =
            ValueOption.iter f x

        static member inline Iterate(x: Set<'k>, [<InlineIfLambda>] f: 'k -> unit) : unit =
            Set.iter f x

        static member inline Iterate
            (x: System.Collections.Generic.HashSet<'k>, [<InlineIfLambda>] f: 'k -> unit)
            : unit =
            use mutable e = x.GetEnumerator()

            while e.MoveNext() do
                f e.Current

        static member inline Iterate(x: 't[], [<InlineIfLambda>] f: 't -> unit) : unit =
            let mutable i = 0

            while i < x.Length do
                f x[i]
                i <- i + 1

        static member inline Iterate
            (x: Result<'t, _>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            match x with
            | Ok v -> f v
            | _ -> ()

        static member inline Iterate(x: list<'t>, [<InlineIfLambda>] f: 't -> unit) : unit =
            List.iter f x

        static member inline Iterate
            (x: System.Span<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            let mutable i = 0

            while i < x.Length do
                f x[i]
                i <- i + 1

        static member inline Iterate
            (x: System.ReadOnlySpan<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            let mutable i = 0

            while i < x.Length do
                f x[i]
                i <- i + 1

        static member inline Iterate
            (x: ResizeArray<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            let mutable i = 0

            while i < x.Count do
                f x[i]
                i <- i + 1

        static member inline Iterate
            (x: Dictionary<'k, 'v>, [<InlineIfLambda>] f: KeyValuePair<'k, 'v> -> unit)
            : unit =
            use mutable e = x.GetEnumerator()

            while e.MoveNext() do
                f e.Current


        static member inline Invoke< ^I, ^t
            when (^I or Iterate): (static member Iterate: ^I * (^t -> unit) -> unit)>
            (source: _, [<InlineIfLambda>] action: ^t -> unit)
            : unit =
            ((^I or Iterate): (static member Iterate: ^I * (^t -> unit) -> unit) (source,
                                                                                  action))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type IterateReverse =

        static member inline IterateReverse
            (x: option<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            Iterate.Invoke(x, f)

        static member inline IterateReverse
            (x: voption<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            Iterate.Invoke(x, f)

        static member inline IterateReverse
            (x: Result<'t, _>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            Iterate.Invoke(x, f)

        static member inline IterateReverse
            (x: Set<'k>, [<InlineIfLambda>] f: 'k -> unit)
            : unit =
            for v in x |> Seq.rev do
                f v

        static member inline IterateReverse
            (x: list<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            for v in x |> Seq.rev do
                f v

        static member inline IterateReverse
            (x: System.Collections.Generic.HashSet<'k>, [<InlineIfLambda>] f: 'k -> unit)
            : unit =
            // since it's not deterministic anyway
            // so it's more efficient to iterate as given here
            Iterate.Invoke(x, f)

        static member inline IterateReverse
            (x: Dictionary<'k, 'v>, [<InlineIfLambda>] f: KeyValuePair<'k, 'v> -> unit)
            : unit =
            Iterate.Invoke(x, f)

        static member inline IterateReverse(x: 't[], [<InlineIfLambda>] f: 't -> unit) : unit =
            let mutable i = x.Length

            while i <> 0 do
                i <- i - 1
                f x[i]


        static member inline IterateReverse
            (x: System.Span<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            let mutable i = x.Length

            while i <> 0 do
                i <- i - 1
                f x[i]

        static member inline IterateReverse
            (x: System.ReadOnlySpan<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            let mutable i = x.Length

            while i <> 0 do
                i <- i - 1
                f x[i]

        static member inline IterateReverse
            (x: ResizeArray<'t>, [<InlineIfLambda>] f: 't -> unit)
            : unit =
            let mutable i = x.Count

            while i <> 0 do
                i <- i - 1
                f x[i]

        static member inline Invoke< ^I, ^t
            when (^I or IterateReverse): (static member IterateReverse:
                ^I * (^t -> unit) -> unit)>
            (source: _, [<InlineIfLambda>] action: ^t -> unit)
            : unit =
            ((^I or IterateReverse): (static member IterateReverse: ^I * (^t -> unit) -> unit) (source,
                                                                                                action))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type IterateIndexed =

        static member inline Invoke
            (source: _, [<InlineIfLambda>] action: int -> 't -> unit)
            : unit =
            let mutable index = 0

            Iterate.Invoke<_, ^t>(
                source,
                (fun v ->
                    action index v
                    index <- index + 1
                )
            )

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type IterateIndexedReverse =

        static member inline Invoke
            (source: _, [<InlineIfLambda>] action: int -> 't -> unit)
            : unit =
            let mutable index = 0

            IterateReverse.Invoke<_, ^t>(
                source,
                (fun v ->
                    action index v
                    index <- index + 1
                )
            )


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Find =

        static member inline Invoke
            (source: _, [<InlineIfLambda>] pred: 't -> bool)
            : voption<'t> =
            let mutable result = ValueNone

            let _ =
                IterateWhile.Invoke(
                    source,
                    (fun v ->
                        if pred v then
                            result <- ValueSome v
                            false
                        else
                            true
                    )

                )

            result

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Pick =

        static member inline Invoke
            (source: _, [<InlineIfLambda>] selector: 't -> voption<'v>)
            : voption<'v> =

            let mutable result = ValueNone

            let _ =
                IterateWhile.Invoke(
                    source,
                    (fun v ->
                        result <- selector v
                        not result.IsSome
                    )
                )

            result

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type FindIndex =

        static member inline Invoke
            (source: _, [<InlineIfLambda>] pred: 't -> bool)
            : voption<int> =

            let mutable i = 0
            let mutable result = ValueNone

            let _ =
                IterateWhile.Invoke(
                    source,
                    (fun v ->
                        if pred v then
                            result <- ValueSome i
                            false
                        else
                            i <- i + 1
                            true
                    )
                )

            result

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Forall =

        static member inline Invoke(source: _, [<InlineIfLambda>] pred: _) : bool =

            let mutable notfound = true

            let _ =
                IterateWhile.Invoke(
                    source,
                    (fun v ->
                        notfound <- pred v
                        notfound
                    )
                )

            notfound

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Exists =

        static member inline Invoke(source: ^I, [<InlineIfLambda>] pred: _) : bool =
            let mutable notfound = true

            let _ =
                IterateWhile.Invoke(
                    source,
                    (fun v ->
                        notfound <- not (pred v)
                        notfound
                    )
                )

            not notfound


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Map =
        static member inline Map(x: option<_>, [<InlineIfLambda>] f: 't -> 'u) : option<'u> =
            Option.map f x

        static member inline Map(x: voption<_>, [<InlineIfLambda>] f: 't -> 'u) : voption<'u> =
            ValueOption.map f x

        static member inline Map(x: Set<_>, [<InlineIfLambda>] f: 't -> 'u) : Set<'u> =
            Set.map f x

        static member inline Map
            (x: Result<'t, _>, [<InlineIfLambda>] f: 't -> 'u)
            : Result<'u, _> =
            match x with
            | Ok v -> Ok(f v)
            | Error v -> Error v

        static member inline Map(x: 't[], [<InlineIfLambda>] f: 't -> 'u) : 'u[] =
            let dest = Array.zeroCreate<'u> x.Length
            let mutable i = 0

            while i < x.Length do
                dest[i] <- f x[i]
                i <- i + 1

            dest

        static member inline Map(x: list<'t>, [<InlineIfLambda>] f: 't -> 'u) : list<'u> =
            List.map f x

        static member inline Map
            (x: ResizeArray<'t>, [<InlineIfLambda>] f: 't -> 'u)
            : ResizeArray<'u> =
            let dest = ResizeArray<'u>(x.Count)
            let mutable i = 0

            while i < x.Count do
                dest[i] <- f x[i]
                i <- i + 1

            dest

        static member inline Invoke< ^I, ^t, ^u, ^r
            when (^I or Map): (static member Map: ^I * (^t -> ^u) -> ^r)>
            (source: _, [<InlineIfLambda>] action: ^t -> ^u)
            : _ =
            ((^I or Map): (static member Map: ^I * (^t -> ^u) -> _) (source, action))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type MapIndexed =
        static member inline Invoke(source: _, mapping: int -> 't -> 'u) =

            let mutable index = 0

            Map.Invoke(
                source,
                (fun v ->
                    let v = mapping index v
                    index <- index + 1
                    v
                )
            )


#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Bind =
        static member inline Bind
            (x: option<'t>, [<InlineIfLambda>] f: 't -> option<'u>)
            : option<'u> =
            Option.bind f x

        static member inline Bind
            (x: voption<'t>, [<InlineIfLambda>] f: 't -> voption<'u>)
            : voption<'u> =
            ValueOption.bind f x

        static member inline Bind
            (x: array<'t>, [<InlineIfLambda>] f: 't -> array<'u>)
            : array<'u> =
            let result = ResizeArray()
            Iterate.Invoke(x, (fun v -> result.AddRange(f v)))
            result.ToArray()

        static member inline Bind
            (x: ResizeArray<'t>, [<InlineIfLambda>] f: 't -> ResizeArray<'u>)
            : ResizeArray<'u> =
            let result = ResizeArray()
            Iterate.Invoke(x, (fun v -> result.AddRange(f v)))
            result

        static member inline Bind
            (x: list<'t>, [<InlineIfLambda>] f: 't -> list<'u>)
            : list<'u> =
            List.collect f x

        static member inline Invoke< ^I, ^t, ^u, ^r
            when (^I or Bind): (static member Bind: ^I * (^t -> ^u) -> ^r)>
            ([<InlineIfLambda>] fn: ^t -> ^u, source: _)
            : _ =
            ((^I or Bind): (static member Bind: ^I * (^t -> ^u) -> _) (source, fn))

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type Fold =
        static member inline Invoke
            ((source: _), (s0: _), [<InlineIfLambdaAttribute>] fn: ^acc -> ^t -> ^acc)
            : ^acc =
            let mutable state = s0
            Iterate.Invoke(source, (fun v -> state <- fn state v))
            state

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type FoldRev =
        static member inline Invoke
            ((source: _), (s0: _), [<InlineIfLambdaAttribute>] fn: ^acc -> ^t -> ^acc)
            : ^acc =
            let mutable state = s0
            IterateReverse.Invoke(source, (fun v -> state <- fn state v))
            state

#if FABLE_COMPILER
    [<Fable.Core.Erase>]
#endif
    [<AbstractClass; Sealed>]
    type FoldIndexed =
        static member inline Invoke
            (s0: ^acc)
            (source: _)
            ([<InlineIfLambdaAttribute>] fn: int -> ^acc -> ^t -> ^acc)
            : ^acc =
            let mutable state = s0

            IterateIndexed.Invoke(source, (fun i v -> state <- fn i state v))

            state


#if FABLE_COMPILER
[<Fable.Core.Erase>]
#endif
[<AbstractClass; Sealed; AutoOpen>]
module internal Abstract =

    let inline is_empty(source: _) : bool = Fsil.IsEmpty.Invoke source

    let inline is_null_or_empty(source: _) : bool =
        source = null || Fsil.IsEmpty.Invoke source

    let inline value(source: _) : _ = Fsil.Value.Invoke(source)

    let inline zero<'a when 'a: (static member Zero: 'a)> : ^a = 'a.Zero

    let inline one<'a when 'a: (static member One: 'a)> : ^a = 'a.One

    let inline none<'a when 'a: (static member None: 'a)> : ^a = 'a.None

    // note: perhaps this should just be an alias for ValueSome
    let inline some<'a, 'b when 'a: (static member Some: 'b -> 'a)> : 'b -> ^a = 'a.Some

    let inline is_some<'a when 'a: (member IsSome: bool)>(arg: ^a) : bool = arg.IsSome

    let inline is_none<'a when 'a: (member IsNone: bool)>(arg: ^a) : bool = arg.IsNone

    let inline is_ok<'a when 'a: (member IsOk: bool)>(arg: ^a) : bool = arg.IsOk

    let inline name<'a, ^name when 'a: (member Name: ^name)>(arg: ^a) : ^name = arg.Name

    /// value-to-enum (for enums backed by other than int32)
    let inline enum(value: 'T) : 'Enum when 'Enum: enum<'T> =
        LanguagePrimitives.EnumOfValue value

    /// enum-to-value
    let inline enumv(enum: ^t when ^t: enum<^e>) : ^e = LanguagePrimitives.EnumToValue enum

    let inline forall ([<InlineIfLambdaAttribute>] f: _) (x: _) : bool =
        Fsil.Forall.Invoke(x, f)

    let inline exists ([<InlineIfLambdaAttribute>] f: _) (x: _) : bool =
        Fsil.Exists.Invoke(x, f)

    /// same as a for loop from 0..len-1
    let inline iter_len
        ([<InlineIfLambdaAttribute>] f: int -> unit)
        (limit_exclusive: int)
        : unit =
        let mutable i = 0

        while i < limit_exclusive do
            f i
            i <- i + 1

    let inline iter ([<InlineIfLambdaAttribute>] f) (x: _) : unit = Fsil.Iterate.Invoke(x, f)

    /// equivalent to `Seq.rev >> Seq.iter`
    let inline iter_rev ([<InlineIfLambdaAttribute>] f) (x: _) : unit =
        Fsil.IterateReverse.Invoke(x, f)

    /// iterate while `cond` is true
    let inline iter_while ([<InlineIfLambdaAttribute>] f) (x: _) : bool =
        Fsil.IterateWhile.Invoke(x, f)

    let inline iteri ([<InlineIfLambdaAttribute>] f) (x: _) : unit =
        Fsil.IterateIndexed.Invoke(x, f)

    /// equivalent to `Seq.rev >> Seq.iteri`
    let inline iteri_rev ([<InlineIfLambdaAttribute>] f) (x: _) : unit =
        Fsil.IterateIndexedReverse.Invoke(x, f)

#if !FABLE_COMPILER
    /// parallel iteration for convenience `iter_parallel num_threads f x`
    let inline iter_parallel (num_threads: int) (f: Action<'x>) (x: seq<'x>) : unit =
        let options = ParallelOptions(MaxDegreeOfParallelism = num_threads)
        Parallel.ForEach(x, options, f) |> ignore
#endif

    // this is intentionally defined initial value first for type inference
    let inline fold (initial) ([<InlineIfLambdaAttribute>] f) (x: _) =
        Fsil.Fold.Invoke(x, initial, f)

    // this is intentionally defined initial value first for type inference
    let inline fold_rev (initial) ([<InlineIfLambdaAttribute>] f) (x: _) =
        Fsil.FoldRev.Invoke(x, initial, f)
    let inline foldi (initial) ([<InlineIfLambdaAttribute>] f) (x: _) =
        Fsil.FoldIndexed.Invoke initial x f

    let inline map ([<InlineIfLambdaAttribute>] f) (x: _) = Fsil.Map.Invoke(x, f)

    let inline mapi ([<InlineIfLambdaAttribute>] f) (x: _) = Fsil.MapIndexed.Invoke(x, f)

    let inline map_err ([<InlineIfLambdaAttribute>] f) (x: _) = Result.mapError f x

    let inline bind ([<InlineIfLambdaAttribute>] f) (x: _) = Fsil.Bind.Invoke(f, x)

    let inline len(source: _) : int = Fsil.Length.Invoke source
    /// checked indexer
    let inline try_item k (source: _) = Fsil.TryItem.Invoke(source, k)
    /// alias for try_item
    let inline get k (source: _) = Fsil.TryItem.Invoke(source, k)

    /// wrap function that may throw exception
    let inline catch fn =
        try
            Ok(fn ())
        with e ->
            Error(e)

    /// unchecked indexer/key like `.Item`
    let inline item k (source: _) = Fsil.Item.Invoke(source, k)

    let inline find ([<InlineIfLambdaAttribute>] k) (source: _) =
        Fsil.Find.Invoke(source, k)

    let inline pick ([<InlineIfLambdaAttribute>] k) (source: _) =
        Fsil.Pick.Invoke(source, k)

    let inline position ([<InlineIfLambdaAttribute>] k) (source: _) =
        Fsil.FindIndex.Invoke(source, k)


#if !FABLE_COMPILER
// overloads for byref/struct/other types
// not as general but at least allowed in CIL
[<AbstractClass; Sealed; AutoOpen>]
type Abstract =

    static member inline span(x: ResizeArray<'t>) : System.Span<'t> =
        System.Runtime.InteropServices.CollectionsMarshal.AsSpan(x)

    static member inline span(x: array<'t>) : System.Span<'t> =
        System.MemoryExtensions.AsSpan(x)

    static member inline span(x: string) : System.ReadOnlySpan<char> =
        System.MemoryExtensions.AsSpan(x)

#endif
