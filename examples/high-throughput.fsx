#r "nuget: resharp, 1.0.0"
open System

// high-throughput matching on large files
// uses ValueMatches to avoid string allocations

let engine =
    Resharp.Regex(@"\w+", Resharp.ResharpOptions.HighThroughputDefaults)

printfn "fully compiled DFA: %b" engine.IsFullDFA

// simulate a large input
let input = String.replicate 1000 "hello world foo bar baz qux "

// allocation-free counting with ValueMatches
let wordCount =
    let mutable count = 0
    use slices = engine.ValueMatches(input)
    for _ in slices do
        count <- count + 1
    count

printfn "found %d words in %d chars" wordCount input.Length

// only extract text for specific slices
let engine2 =
    Resharp.Regex(@"\d{4}-\d{2}-\d{2}", Resharp.ResharpOptions.HighThroughputDefaults)

let log = "event at 2024-01-15 and another at 2024-03-22 end"
do
    use dateSlices = engine2.ValueMatches(log)
    for s in dateSlices do
        let text = s.GetText(log)  // only allocates here
        printfn "date: %s at position %d" text s.Index
