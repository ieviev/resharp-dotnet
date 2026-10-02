module Resharp.Test._19_SetLookupTests

open System
open Resharp
open Xunit
open Common

let private makeRegex pattern =
    Regex(pattern, ResharpOptions.HighThroughputDefaults)

let private assertMatches
    (pattern: string)
    (input: string)
    (expectedMode: int)
    (expected: struct (int * int) list)
    =
    let regex = makeRegex pattern
    let chars = input.ToCharArray()
    let span = ReadOnlySpan<char>(chars)

    Assert.True(regex.UsesSetLookup)
    Assert.Equal(expectedMode, regex.SetLookupSearchMode)
    Assert.True(regex.ValidateSetLookupSearchValues())

    use actual = regex.ValueMatches(span)

    Assert.Equal(expected.Length, actual.Count)

    for i = 0 to expected.Length - 1 do
        let struct (expectedIndex, expectedLength) = expected[i]
        Assert.Equal(expectedIndex, actual.pool[i].Index)
        Assert.Equal(expectedLength, actual.pool[i].Length)

    Assert.Equal(expected.Length, regex.Count(span))

[<Fact>]
let ``SetLookup SearchValues preserves ASCII match ends`` () =
    assertMatches
        "a[^b]*b"
        "axxxb a123456789b ab a---b"
        1
        [ struct (0, 5); struct (6, 11); struct (18, 2); struct (21, 5) ]

[<Fact>]
let ``SetLookup SearchValues preserves literal prefix match ends`` () =
    assertMatches
        "foo[^;]*;"
        "foo short; xx foo a much longer payload here; foo;"
        1
        [ struct (0, 10); struct (14, 31); struct (46, 4) ]

[<Fact>]
let ``SetLookup SearchValues preserves Unicode match ends`` () =
    assertMatches
        "α[^β]*β"
        "αxyzβ αδεζηθβ αβ"
        1
        [ struct (0, 5); struct (6, 7); struct (14, 2) ]

[<Fact>]
let ``SetLookup SearchValues handles immediate delimiter`` () =
    assertMatches "a[^b]*b" "ab" 1 [ struct (0, 2) ]

[<Fact>]
let ``SetLookup SearchValues handles delimiter at final code unit`` () =
    assertMatches "a[^b]*b" "prefix axxxb" 1 [ struct (7, 5) ]

[<Fact>]
let ``SetLookup SearchValues handles missing delimiter`` () =
    assertMatches "a[^b]*b" "axxx" 1 []

[<Fact>]
let ``SetLookup SearchValues handles multiple candidate starts before delimiter`` () =
    assertMatches "a[^b]*b" "aaab" 1 [ struct (0, 4) ]

[<Fact>]
let ``SetLookup exact minterm handles NUL delimiter`` () =
    assertMatches "a[^\u0000]*\u0000" "ax\u0000" 1 [ struct (0, 3) ]

[<Fact>]
let ``SetLookup exact minterm handles maximum UTF16 code unit`` () =
    assertMatches "a[^\uFFFF]*\uFFFF" "ax\uFFFF" 1 [ struct (0, 3) ]

[<Fact>]
let ``SetLookup inverted representation keeps scalar fallback`` () =
    assertMatches
        "bb*[^b]"
        "bbbbx bbx bx bbbbbby"
        2
        [ struct (0, 5); struct (6, 3); struct (10, 2); struct (13, 7) ]
