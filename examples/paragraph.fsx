#r "nuget: resharp, 1.0.0"
open System

let doc =
    __SOURCE_DIRECTORY__ + "/../src/Resharp.Test/data/howto.txt"
    |> IO.File.ReadAllText

// split text into paragraphs (no double newlines) that contain "swap"
// and don't start/end with whitespace

let paragraph = @"~(_*\n\n_*)"       // complement: no double newlines
let containsSwap = "_*swap_*"         // must contain "swap"
let trimmed = @"\S_*\S"              // starts and ends with non-whitespace

let pattern = String.Join("&", [ paragraph; containsSwap; trimmed ])

printfn "pattern: %s\n" pattern

for m in Resharp.Regex(pattern).Matches(doc) do
    printfn "--- match at %d (len %d) ---" m.Index m.Length
    let preview = if m.Value.Length > 80 then m.Value.[..79] + "..." else m.Value
    printfn "%s\n" preview
