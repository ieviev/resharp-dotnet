[<Xunit.Collection("ThreadSafety")>]
module Resharp.Test._17_ThreadSafetyTests

open System
open System.Threading
open System.Threading.Tasks
open Xunit
open Resharp

let private hammer (threadCount: int) (iters: int) (op: Regex -> string -> unit) =
    let patterns = [|
        @"\w+@\w+\.\w+"
        @"(foo|bar|baz)[a-z]*[0-9]+"
        @"[a-z]{2,5}\d{1,3}[A-Z]?"
        @"(\d+\.){3}\d+"
        @"[a-f0-9]{8}-[a-f0-9]{4}"
    |]
    let inputs = [|
        "hello@world.com test foo123 bar456"
        "ab12C cd345 ef6 192.168.1.1 aabbccdd-1234"
        "no matches here !!!"
        "foo999 baz42 bar0 hello@x.y"
        "1.2.3.4 5.6.7.8 abc-defg"
        "aaaa1A bb22 ccc333B dddd4444"
        "user@domain.org admin@test.io x@y.z"
    |]

    for pattern in patterns do
        let regex = Regex(pattern)
        let barrier = new Barrier(threadCount)
        let mutable failed: exn = null

        let tasks =
            Array.init threadCount (fun _ ->
                Task.Run(fun () ->
                    barrier.SignalAndWait()
                    for _ in 1 .. iters do
                        for input in inputs do
                            try
                                op regex input
                            with e ->
                                Volatile.Write(&failed, e)
                )
            )

        Task.WaitAll(tasks)
        barrier.Dispose()

        let ex = Volatile.Read(&failed)
        if not (isNull ex) then
            failwith $"thread safety violation on '{pattern}': {ex.GetType().Name}: {ex.Message}\n{ex.StackTrace}"

let private tc = Environment.ProcessorCount * 2 |> max 8

[<Fact>]
let ``concurrent IsMatch on shared instance`` () =
    hammer tc 200 (fun re input -> re.IsMatch(input) |> ignore)

[<Fact>]
let ``concurrent Count on shared instance`` () =
    hammer tc 200 (fun re input -> re.Count(input) |> ignore)

[<Fact>]
let ``concurrent Matches on shared instance`` () =
    hammer tc 200 (fun re input -> re.Matches(input) |> ignore)

[<Fact>]
let ``concurrent Replace on shared instance`` () =
    hammer tc 200 (fun re input -> re.Replace(input, "X") |> ignore)

[<Fact>]
let ``concurrent mixed operations on shared instance`` () =
    let patterns = [| @"\w+@\w+\.\w+"; @"(foo|bar|baz)[a-z]*[0-9]+"; @"[a-z]{2,5}\d{1,3}[A-Z]?" |]
    let inputs = [| "hello@world.com test foo123 bar456"; "ab12C cd345 ef6 192.168.1.1"; "foo999 baz42 bar0" |]
    let ops: (Regex -> string -> unit)[] = [|
        fun re s -> re.IsMatch(s) |> ignore
        fun re s -> re.Count(s) |> ignore
        fun re s -> re.Matches(s) |> ignore
        fun re s -> re.Replace(s, "X") |> ignore
    |]

    for pattern in patterns do
        let regex = Regex(pattern)
        let barrier = new Barrier(tc)
        let mutable failed: exn = null

        let tasks =
            Array.init tc (fun i ->
                Task.Run(fun () ->
                    barrier.SignalAndWait()
                    let op = ops[i % ops.Length]
                    for _ in 1 .. 200 do
                        for input in inputs do
                            try
                                op regex input
                            with e ->
                                Volatile.Write(&failed, e)
                )
            )

        Task.WaitAll(tasks)
        barrier.Dispose()

        let ex = Volatile.Read(&failed)
        if not (isNull ex) then
            failwith $"thread safety violation on '{pattern}': {ex.GetType().Name}: {ex.Message}\n{ex.StackTrace}"
