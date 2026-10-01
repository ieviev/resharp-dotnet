module Resharp.Test._19_SetLookupTests

open System
open Resharp
open Xunit
open Common

let private makeRegex pattern vectorized =
    let options = ResharpOptions.HighThroughputDefaults
    options.UseSearchValuesSetLookup <- vectorized
    Regex(pattern, options)

let private assertEquivalent (pattern: string) (input: string) =
    let scalar = makeRegex pattern false
    let vectorized = makeRegex pattern true
    let chars = input.ToCharArray()
    let span = ReadOnlySpan<char>(chars)

    Assert.True(scalar.UsesSetLookup)
    Assert.True(vectorized.UsesSetLookup)
    Assert.True(vectorized.ValidateSetLookupSearchValues())

    use expected = scalar.ValueMatches(span)
    use actual = vectorized.ValueMatches(span)

    Assert.Equal(expected.Count, actual.Count)
    for i = 0 to expected.Count - 1 do
        Assert.Equal(expected.pool[i].Index, actual.pool[i].Index)
        Assert.Equal(expected.pool[i].Length, actual.pool[i].Length)

    Assert.Equal(scalar.Count(span), vectorized.Count(span))

[<Fact>]
let ``SetLookup SearchValues preserves ASCII match ends`` () =
    assertEquivalent "a[^b]*b" "axxxb a123456789b ab a---b"

[<Fact>]
let ``SetLookup SearchValues preserves literal prefix match ends`` () =
    assertEquivalent "foo[^;]*;" "foo short; xx foo a much longer payload here; foo;"

[<Fact>]
let ``SetLookup SearchValues preserves Unicode match ends`` () =
    assertEquivalent "α[^β]*β" "αxyzβ αδεζηθβ αβ"
