#r "nuget: resharp, 1.0.0"
open System

// --- simple replacement ---

let digits = Resharp.Regex(@"\d+")
let result = digits.Replace("order 123 has 4 items", "X")
printfn "simple: %s" result  // "order X has X items"

// --- replacement with function ---

let doubled =
    digits.Replace("price: 10, tax: 3", fun m ->
        string (int m * 2)
    )
printfn "doubled: %s" doubled  // "price: 20, tax: 6"

// --- redacting sensitive data ---

let emailPattern = @"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"
let redactor = Resharp.Regex(emailPattern)
let redacted =
    redactor.Replace(
        "contact alice@example.com or bob@test.org",
        "[REDACTED]"
    )
printfn "redacted: %s" redacted
