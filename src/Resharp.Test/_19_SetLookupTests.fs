module Resharp.Test._19_SetLookupTests

open System
open Resharp
open Xunit
open Common

let private makeRegex pattern vectorized adaptiveThreshold =
    let options = ResharpOptions.HighThroughputDefaults
    options.UseSearchValuesSetLookup <- vectorized
    options.SetLookupAdaptiveThreshold <- adaptiveThreshold
    Regex(pattern, options)

let private assertEquivalent (pattern: string) (input: string) =
    let scalar = makeRegex pattern false 0
    let vectorized = makeRegex pattern true 0
    let adaptive16 = makeRegex pattern true 16
    let chars = input.ToCharArray()
    let span = ReadOnlySpan<char>(chars)

    Assert.True(scalar.UsesSetLookup)
    Assert.True(vectorized.UsesSetLookup)
    Assert.True(vectorized.ValidateSetLookupSearchValues())

    use expected = scalar.ValueMatches(span)
    use actual = vectorized.ValueMatches(span)
    use adaptive = adaptive16.ValueMatches(span)

    Assert.Equal(expected.Count, actual.Count)
    Assert.Equal(expected.Count, adaptive.Count)

    for i = 0 to expected.Count - 1 do
        Assert.Equal(expected.pool[i].Index, actual.pool[i].Index)
        Assert.Equal(expected.pool[i].Length, actual.pool[i].Length)
        Assert.Equal(expected.pool[i].Index, adaptive.pool[i].Index)
        Assert.Equal(expected.pool[i].Length, adaptive.pool[i].Length)

    let expectedCount = scalar.Count(span)
    Assert.Equal(expectedCount, vectorized.Count(span))
    Assert.Equal(expectedCount, adaptive16.Count(span))

[<Fact>]
let ``SetLookup SearchValues preserves ASCII match ends`` () =
    assertEquivalent "a[^b]*b" "axxxb a123456789b ab a---b"

[<Fact>]
let ``SetLookup SearchValues preserves literal prefix match ends`` () =
    assertEquivalent "foo[^;]*;" "foo short; xx foo a much longer payload here; foo;"

[<Fact>]
let ``SetLookup SearchValues preserves Unicode match ends`` () =
    assertEquivalent "α[^β]*β" "αxyzβ αδεζηθβ αβ"

[<Fact>]
let ``SetLookup SearchValues supports inverted mode`` () =
    let regex = makeRegex "bb*[^b]" true 0
    Assert.True(regex.UsesSetLookup)
    Assert.Equal(2, regex.SetLookupSearchMode)
    Assert.True(regex.ValidateSetLookupSearchValues())
    assertEquivalent "bb*[^b]" "bbbbx bbx bx bbbbbby"
