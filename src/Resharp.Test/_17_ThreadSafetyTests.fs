[<Xunit.Collection("ThreadSafety")>]
module Resharp.Test._17_ThreadSafetyTests

open System
open System.Threading
open System.Threading.Tasks
open Xunit
open Resharp

let private tc = Environment.ProcessorCount * 2 |> max 8

/// runs op on a shared regex instance from many threads simultaneously.
/// any exception (NRE, index OOB, dictionary corruption) fails the test.
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

/// runs op on a shared regex from many threads and collects per-thread results,
/// then verifies all results match the single-threaded baseline.
let private hammerAndVerify
    (threadCount: int) (iters: int)
    (pattern: string) (inputs: string[])
    (op: Regex -> string -> 'a) =
    let regex = Regex(pattern)
    // compute baseline single-threaded
    let baseline = inputs |> Array.map (fun input -> op regex input)
    let barrier = new Barrier(threadCount)
    let mutable failed: exn = null

    let tasks =
        Array.init threadCount (fun _ ->
            Task.Run(fun () ->
                barrier.SignalAndWait()
                for _ in 1 .. iters do
                    for i in 0 .. inputs.Length - 1 do
                        try
                            let result = op regex inputs[i]
                            let expected = baseline[i]
                            if not (obj.Equals(result, expected)) then
                                failwith $"result mismatch: expected {expected}, got {result}"
                        with e ->
                            Volatile.Write(&failed, e)
            )
        )

    Task.WaitAll(tasks)
    barrier.Dispose()

    let ex = Volatile.Read(&failed)
    if not (isNull ex) then
        failwith $"on '{pattern}': {ex.GetType().Name}: {ex.Message}\n{ex.StackTrace}"


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


// correctness under contention

[<Fact>]
let ``concurrent IsMatch results match single-threaded baseline`` () =
    let inputs = [|
        "hello@world.com"; "nope"; "a@b.c"; "user@domain.org test"; ""
        "multi@line.com\nother@place.net"; String('x', 500) + "@y.z"
    |]
    hammerAndVerify tc 100 @"\w+@\w+\.\w+" inputs (fun re s -> re.IsMatch(s))

[<Fact>]
let ``concurrent Count results match single-threaded baseline`` () =
    let inputs = [|
        "1.2.3.4 and 10.20.30.40"; "no match"; "192.168.1.1"; ""
        "0.0.0.0 1.1.1.1 2.2.2.2 3.3.3.3 4.4.4.4"
    |]
    hammerAndVerify tc 100 @"(\d+\.){3}\d+" inputs (fun re s -> re.Count(s))

[<Fact>]
let ``concurrent Matches results match single-threaded baseline`` () =
    let inputs = [| "foo12 bar34 baz56"; "nothing"; "foo1bar2baz3"; "FOO99" |]
    hammerAndVerify tc 100 @"(foo|bar|baz)[a-z]*[0-9]+" inputs
        (fun re s -> re.Matches(s) |> Array.map (fun m -> m.Value) |> String.concat ",")

[<Fact>]
let ``concurrent Replace results match single-threaded baseline`` () =
    let inputs = [| "abc123 def456"; "no digits"; "a1b2c3"; "" |]
    hammerAndVerify tc 100 @"[a-z]+\d+" inputs (fun re s -> re.Replace(s, "X"))


// multiple resizes

[<Fact>]
let ``concurrent access with large state space pattern`` () =
    // alternation of many branches -> many DFA states created lazily
    let pattern = @"(ab|cd|ef|gh|ij|kl|mn|op|qr|st|uv|wx|yz)[a-z]{0,4}\d{1,3}"
    let inputs = [|
        "ab1 cd22 ef333 gh1 ij22 kl333 mn1 op22 qr333 st1 uv22 wx333 yz1"
        "abcde1 cdefg22 efghi333 ghijk1 ijklm22"
        "no match at all !!!"
        String('a', 200) + "1" + String('b', 200) + "2"
        "ab0cd1ef2gh3ij4kl5mn6op7qr8st9uv0wx1yz2"
    |]
    let regex = Regex(pattern)
    let barrier = new Barrier(tc)
    let mutable failed: exn = null

    let tasks =
        Array.init tc (fun _ ->
            Task.Run(fun () ->
                barrier.SignalAndWait()
                for _ in 1 .. 300 do
                    for input in inputs do
                        try
                            regex.Count(input) |> ignore
                            regex.IsMatch(input) |> ignore
                            regex.Matches(input) |> ignore
                        with e ->
                            Volatile.Write(&failed, e)
            )
        )

    Task.WaitAll(tasks)
    barrier.Dispose()

    let ex = Volatile.Read(&failed)
    if not (isNull ex) then
        failwith $"large state space: {ex.GetType().Name}: {ex.Message}\n{ex.StackTrace}"


// anchor transitions

[<Fact>]
let ``concurrent access with anchored patterns`` () =
    let patterns = [|
        @"\A\w+\z"          // full-string anchors
        @"^\d+$"            // line anchors
        @"\b\w{3,5}\b"     // word boundaries
        @"(?<=\s)\w+"       // lookbehind
        @"\w+(?=\s)"        // lookahead
    |]
    let inputs = [|
        "hello"; "12345"; "  abc  def  ghi  "; "one two three four five"
        "a"; ""; "word boundary test here"; "123 456 789"
    |]

    for pattern in patterns do
        let regex = Regex(pattern)
        let barrier = new Barrier(tc)
        let mutable failed: exn = null

        let tasks =
            Array.init tc (fun _ ->
                Task.Run(fun () ->
                    barrier.SignalAndWait()
                    for _ in 1 .. 200 do
                        for input in inputs do
                            try
                                regex.IsMatch(input) |> ignore
                                regex.Count(input) |> ignore
                            with e ->
                                Volatile.Write(&failed, e)
                )
            )

        Task.WaitAll(tasks)
        barrier.Dispose()

        let ex = Volatile.Read(&failed)
        if not (isNull ex) then
            failwith $"anchor pattern '{pattern}': {ex.GetType().Name}: {ex.Message}\n{ex.StackTrace}"


// complement and intersection (RE#-specific operators)

[<Fact>]
let ``concurrent access with complement and intersection`` () =
    let patterns = [|
        @"~(.*foo.*)" 
        @"\w+ & ~(abc)"
        @"[a-z]+ & [^aeiou]+" 
    |]
    let inputs = [|
        "hello"; "foo"; "foobar"; "abc"; "xyz"; "bcdfg"; "aeiou"
        "test string"; "no foo here"; ""; "aaaa"; "zzzz"
    |]

    for pattern in patterns do
        let regexOpt =
            try Some(Regex(pattern))
            with _ -> None
        match regexOpt with
        | None -> ()
        | Some regex ->
            let barrier = new Barrier(tc)
            let mutable failed: exn = null

            let tasks =
                Array.init tc (fun _ ->
                    Task.Run(fun () ->
                        barrier.SignalAndWait()
                        for _ in 1 .. 200 do
                            for input in inputs do
                                try
                                    regex.IsMatch(input) |> ignore
                                    regex.Count(input) |> ignore
                                with e ->
                                    Volatile.Write(&failed, e)
                    )
                )

            Task.WaitAll(tasks)
            barrier.Dispose()

            let ex = Volatile.Read(&failed)
            if not (isNull ex) then
                failwith $"complement/intersection '{pattern}': {ex.GetType().Name}: {ex.Message}\n{ex.StackTrace}"


// FirstEnd and LongestEnd (separate hot loop code paths)

[<Fact>]
let ``concurrent FirstEnd and LongestEnd`` () =
    let patterns = [| @"\d+"; @"[a-z]+"; @"\w+@\w+" |]
    let inputs = [| "abc123def456"; "hello world"; "user@host more@stuff"; "" |]

    for pattern in patterns do
        let regex = Regex(pattern)
        let barrier = new Barrier(tc)
        let mutable failed: exn = null

        let tasks =
            Array.init tc (fun i ->
                Task.Run(fun () ->
                    barrier.SignalAndWait()
                    for _ in 1 .. 300 do
                        for input in inputs do
                            try
                                if i % 2 = 0 then
                                    regex.FirstEnd(input) |> ignore
                                else
                                    regex.LongestEnd(input) |> ignore
                            with e ->
                                Volatile.Write(&failed, e)
                )
            )

        Task.WaitAll(tasks)
        barrier.Dispose()

        let ex = Volatile.Read(&failed)
        if not (isNull ex) then
            failwith $"FirstEnd/LongestEnd '{pattern}': {ex.GetType().Name}: {ex.Message}\n{ex.StackTrace}"


// ValueMatches lifecycle (span-based results under contention)

[<Fact>]
let ``concurrent ValueMatches with disposal`` () =
    let regex = Regex(@"\w+")
    let inputs = [|
        "one two three four five six seven eight nine ten"
        "a b c d e f g h i j k l m n o p q r s t u v w x y z"
        ""; "single"; String('a', 500) + " " + String('b', 500)
    |]
    let barrier = new Barrier(tc)
    let mutable failed: exn = null

    let tasks =
        Array.init tc (fun _ ->
            Task.Run(fun () ->
                barrier.SignalAndWait()
                for _ in 1 .. 300 do
                    for input in inputs do
                        try
                            use matches = regex.ValueMatches(input)
                            // force iteration of all results
                            let mutable total = 0
                            for i in 0 .. matches.size - 1 do
                                total <- total + matches.pool[i].Length
                            total |> ignore
                        with e ->
                            Volatile.Write(&failed, e)
            )
        )

    Task.WaitAll(tasks)
    barrier.Dispose()

    let ex = Volatile.Read(&failed)
    if not (isNull ex) then
        failwith $"ValueMatches: {ex.GetType().Name}: {ex.Message}\n{ex.StackTrace}"

