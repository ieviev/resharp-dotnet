module Resharp.Test._12_NegLookaroundTests

open Resharp
open Resharp.Types
open Xunit
open Common

#if DEBUG

[<Fact>]
let ``der neg anchor 1`` () =
    _04_DerivativeTests.testRevDerivative (
        @"(?!b)",
        "b",
        [
            @"⊥"
            @"(?<=~((ε|_*b)))" // TODO: unsure
            @"(?<=~((_*b|ε)))"
            @"(?<=~((_*b)?))"
            @"(?<=~((_*a)?))b"
            @"(?<=(~((_*b)?)|_*\z~(_*b)))"
            @"(?<=(_*\z~(_*b)|~((_*b)?)))"
            // -- no look
            @"(_*\z~(_*b)|~((_*b)?))"
            @"(~((_*b)?)|_*\z~(_*b))"
            @"~((_*b)?)"
        ]
    )

[<Fact>]
let ``der neg anchor 2`` () =
    _04_DerivativeTests.testRevDerivative (
        @"(?!b)",
        "a",
        [
            // @"ε"
            @"(?<=~(_*b))"
            @"((?<=~(_*a))b)?"
            @"(?<=(_*\z)?~(_*b))"
            // no look
            @"(_*\z)?~(_*b)"
            @"~(_*b)"
            // after rewrite
            @"⊥*"

        ]
    )

[<Fact>]
let ``der neg anchor lb 1`` () =
    _04_DerivativeTests.testPartDerivatives (
        @"(?<!a)b",
        "ab",
        [
            "⊥"
            @"(?<=~((_*a)?))b"
            // no look
            @"~((_*a)?)b"
        ]
    )

[<Fact>]
let ``der neg anchor lb 2`` () =
    _04_DerivativeTests.testPartDerivatives (
        @"(?<!a)b",
        "bb",
        [
            @"(~(_*a)b)?"
            @"b?"
        ]
    )

[<Fact>]
let ``c ahead 1.1`` () =
    assertRawDerivative @"(?=1)11" "11" [ "1" ]

[<Fact>]
let ``lookarounds test 2`` () =
    let matcher = Regex("""1(?! Sep)""")
    let ism = matcher.IsMatch("1 Sep")
    Assert.False(ism)


[<Fact>]
let ``lookarounds test 2.1`` () =
    let matcher = Regex("""^(1(?! (Sep)))$""")
    let ism = matcher.IsMatch("1 Sep")
    Assert.False(ism)

[<Fact>]
let ``lookarounds test 2.2`` () =
    let matcher = Regex("""^(a(?!b))$""")
    let ism = matcher.IsMatch("ab")
    Assert.False(ism)


[<Fact>]
let ``lookarounds test 2.4`` () =
    let matcher = Regex("""1\b-""")
    let ism = matcher.IsMatch("1-")
    Assert.True(ism)

[<Fact>]
let ``lookarounds test 2.5`` () =
    let matcher = Regex("""1\b-""")
    let ism = matcher.IsMatch("1-")
    Assert.True(ism)


[<Fact>]
let ``d rewritten test 1.1`` () =
    assertFirstMatchText @".(?<=a)" "aaa" "a"


[<Fact>]
let ``d rewritten test 2.1`` () =
    assertFirstMatchText @"1(?=[012])\d" "11" "11"



let testSameAsRuntime = _07_ComparisonTests.testSameAsRuntime

[<Fact>]
let ``regex with label 1`` () =
    let pattern = """(?<Time>^(?:0?[1-9]:[0-5]|1(?=[012])\d:[0-5])\d(?:[ap]m)?)"""
    let input = "12:00am"
    testSameAsRuntime pattern input


[<Fact>]
let ``regex with label 2`` () =
    let pattern = """^(?:0?[1-9]:[0-5]|1(?=[012])\d:[0-5])\d(?:[ap]m)?"""
    let input = "12:00am"
    testSameAsRuntime pattern input



[<Fact>]
let ``ranges 1`` () =
    assertAllLLmatches @"(?<=\d)a" "1a__a__a" [ 1, 1 ]

[<Fact>]
let ``ranges 2.1`` () =
    assertAllLLmatches @"(?<!\d)a" "1a__a__a" [ 4, 1; 7, 1 ]


[<Fact>]
let ``ranges 2.2`` () =
    assertAllLLmatches @"(?<!\d)a" " a" [ 1, 1 ]



#endif
