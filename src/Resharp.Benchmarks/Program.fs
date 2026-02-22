module Resharp.Benchmarks.Program

open System
open BenchmarkDotNet.Running

let private showMenu () =
    let benches = RebarData.allBenches.Value
    let groups =
        benches
        |> Array.map (fun b -> b.group)
        |> Array.distinct
        |> Array.sort
    let topLevels =
        groups
        |> Array.map (fun g -> g.Split('/')[0])
        |> Array.distinct

    let choices = [|
        yield $"all ({benches.Length} benchmarks)", ValueNone
        for tl in topLevels do
            let n = benches |> Array.filter (fun b -> b.group.StartsWith(tl + "/")) |> Array.length
            yield $"{tl} ({n})", ValueSome (tl + "/")
        for g in groups do
            let n = benches |> Array.filter (fun b -> b.group = g) |> Array.length
            yield $"{g} ({n})", ValueSome (g + "/")
    |]

    printfn ""
    printfn "select benchmarks:"
    choices |> Array.iteri (fun i (label, _) -> printfn $"  [{i}] {label}")
    printfn ""
    printf "> "
    let input = Console.ReadLine().Trim()
    match Int32.TryParse(input) with
    | true, n when n >= 0 && n < choices.Length -> snd choices[n]
    | _ -> ValueSome input

[<EntryPoint>]
let main args =
    if args.Length = 0 then
        RebarData.nameFilter <- showMenu ()
        BenchmarkRunner.Run<RebarBench>() |> ignore
    else
        BenchmarkSwitcher
            .FromAssembly(typeof<RebarBench>.Assembly)
            .Run(args)
        |> ignore
    0
