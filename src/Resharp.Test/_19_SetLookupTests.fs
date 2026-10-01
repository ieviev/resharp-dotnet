module Resharp.Test._19_SetLookupTests

open System
open Resharp
open Xunit
open Common

let private makeRegex pattern vectorized =
    let options = ResharpOptions.HighThroughputDefaults
    options.UseSearchValuesSetLookup <- vectorized
    Regex(pattern, options)

let private assertEquivalent pattern input =
    let scalar = makeRegex pattern false
    let vectorized = makeRegex pattern true

    Assert.True(scalar.UsesSetLookup, $"expected SetLookup for pattern: {pattern}")
    Assert.True(vectorized.UsesSetLookup, $"expected SetLookup for pattern: {pattern}")
    Assert.True(vectorized.ValidateSetLookupSearchValues(), $"SearchValues/minterm mismatch: {pattern}")

    use expected = scalar.ValueMatches(input.AsSpan())
    use actual = vectorized.ValueMatches(input.AsSpan())

    Assert.Equal(expected.Count, actual.Count)
    for i = 0 to expected.Count - 1 do
        Assert.Equal(expected.pool[i].Index, actual.pool[i].Index)
        Assert.Equal(expected.pool[i].Length, actual.pool[i].Length)

    Assert.Equal(scalar.Count(input.AsSpan()), vectorized.Count(input.AsSpan()))

[<Fact>]
let ``SetLookup SearchValues preserves ASCII match ends`` () =
    assertEquivalent "a[^b]*b" "axxxb a123456789b ab a---b"

[<Fact>]
let ``SetLookup SearchValues preserves literal prefix match ends`` () =
    assertEquivalent "foo[^;]*;" "foo short; xx foo a much longer payload here; foo;"

[<Fact>]
let ``SetLookup SearchValues preserves Unicode match ends`` () =
    assertEquivalent "α[^β]*β" "αxyzβ αδεζηθβ αβ"
