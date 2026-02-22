#r "nuget: resharp, 1.0.0"
open System

// --- lookahead ---

// extract numbers followed by "am" or "pm"
let timeRe = Resharp.Regex(@"\d+(?=\s*[aApP]\.?[mM]\.?)")
for m in timeRe.Matches("meeting at 10am, lunch at 12 p.m.") do
    printfn "time number: %s" m.Value

// --- lookbehind ---

// extract text after "author:"
let authorRe = Resharp.Regex(@"(?<=author:\s*).*")
for m in authorRe.Matches("author: Jane Doe") do
    printfn "author: %s" m.Value

// extract content between braces after a key
let valueRe = Resharp.Regex(@"(?<=title\{).*(?=\})")
for m in valueRe.Matches("title{The Great Gatsby}") do
    printfn "title: %s" m.Value

// --- negative lookaround ---

// words NOT preceded by a digit
let noDigitBefore = Resharp.Regex(@"(?<!\d)[a-z]+")
for m in noDigitBefore.Matches("3abc def 7ghi jkl") do
    printfn "word not after digit: '%s'" m.Value

// --- lookaround + intersection ---

// after "author:", extract text containing "and"
let coauthorRe = Resharp.Regex(@"(?<=author).*&.*and.*")
for m in coauthorRe.Matches("author: Alice and Bob") do
    printfn "co-authors: %s" m.Value
