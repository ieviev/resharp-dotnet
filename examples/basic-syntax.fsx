#r "nuget: resharp, 1.0.0"
open System

// --- basic matching ---

let re = Resharp.Regex(@"\w+")
printfn "IsMatch: %b" (re.IsMatch("hello world"))  // true
printfn "Count: %d" (re.Count("one two three"))     // 3

for m in re.Matches("one two three") do
    printfn "  '%s' at %d (len %d)" m.Value m.Index m.Length

// --- underscore wildcard ---
// `_` matches any character including newlines (like [\s\S])
// `.` matches any character except newline

let withNewlines = Resharp.Regex(@"a_*b")
printfn "\n_ wildcard across newlines: %b" (withNewlines.IsMatch("a\n\nb"))  // true

let dotOnly = Resharp.Regex(@"a.*b")
printfn ". does not cross newlines: %b" (dotOnly.IsMatch("a\n\nb"))  // false

// --- intersection (&) ---
// both sides must match simultaneously

// 4-letter word starting with 'c' AND ending with 's'
let inter = Resharp.Regex(@"c...&...s")
for m in inter.Matches("raining cats and dogs") do
    printfn "\nintersection match: '%s'" m.Value  // "cats"

// contains both "cat" and "dog"
let both = Resharp.Regex(@"_*cat_*&_*dog_*")
printfn "contains cat and dog: %b" (both.IsMatch("the cat chased the dog"))  // true
printfn "contains cat but not dog: %b" (both.IsMatch("the cat sat"))  // false

// --- complement ~(...) ---
// matches everything the inner pattern does NOT match

// does not contain two consecutive digits
let noDoubleDigit = Resharp.Regex(@"~(_*\d\d_*)")
for m in noDoubleDigit.Matches("Aa11aBaAA") do
    printfn "\nno-double-digit segment: '%s' at %d" m.Value m.Index

// --- combining intersection and complement ---

// starts with 'F' but does not end with "Finn"
let combined = Resharp.Regex(@"F.*&~(.*Finn)")
for m in combined.Matches("Finn', published in 1885.") do
    printfn "\ncombined match: '%s'" m.Value

// 8+ alphanumeric chars with no consecutive digits (password-like)
let password = Resharp.Regex(@"~(.*\d\d.*)&[a-zA-Z\d]{8,}")
printfn "\npassword-like match: %b" (password.IsMatch("tej55zhA25wXu8bvQxFxt"))
