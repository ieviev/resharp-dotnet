module Resharp.Test._07_ComparisonTests

open Resharp
open Xunit
open Common
#if DEBUG


let testSameAsRuntime (pattern: string) (input: string) =
    let mymatcher = Regex(pattern)
    let runtime = System.Text.RegularExpressions.Regex(pattern)
    let result = mymatcher.IsMatch(input)
    let result2 = runtime.IsMatch(input)

    Assert.True(
        (result2 = result),
        $"should be the same: \n{pattern}\n{input}\resharp:{result} = runtime:{result2}"
    )


[<Fact>]
let ``same as runtime 1: short`` () =
    testSameAsRuntime """^(31|(0[1-9]))$""" "31 September"


[<Fact>]
let ``same as runtime 3`` () =
    let pattern =
        """^(((0?[1-9]|[12]\d|3[01])[\.\-\/](0?[13578]|1[02])[\.\-\/]((1[6-9]|[2-9]\d)?\d{2}))|((0?[1-9]|[12]\d|30)[\.\-\/](0?[13456789]|1[012])[\.\-\/]((1[6-9]|[2-9]\d)?\d{2}))|((0?[1-9]|1\d|2[0-8])[\.\-\/]0?2[\.\-\/]((1[6-9]|[2-9]\d)?\d{2}))|(29[\.\-\/]0?2[\.\-\/]((1[6-9]|[2-9]\d)?(0[48]|[2468][048]|[13579][26])|((16|[2468][048]|[3579][26])00)|00)))$"""

    let input = "3.4.05"
    testSameAsRuntime pattern input

[<Fact>]
let ``same as runtime 4`` () =
    let pattern = """^((0?[13578]\.)|(0?[13456789]\.))$"""
    let input = "4."
    testSameAsRuntime pattern input


[<Fact>]
let ``same as runtime 5`` () =
    let pattern = """^((\d{5}-\d{4})|(\d{5})|([A-Z]\d[A-Z]\s\d[A-Z]\d))$"""
    let input = "44240"
    testSameAsRuntime pattern input




[<Fact>]
let ``same as runtime 7`` () =
    let pattern = """^((\d{5}-\d{4})|(\d{5})|([A-Z]\d[A-Z]\s\d[A-Z]\d))"""
    let input = "T2P 3C7"
    testSameAsRuntime pattern input


[<Fact>]
let ``same as runtime 8`` () =
    let pattern = """^((\d{5}-\d{4})|(\d{5})|([A-Z]\d[A-Z]\s\d[A-Z]\d))$"""
    let input = "T2P 3C7"
    testSameAsRuntime pattern input


[<Fact>]
let ``same as runtime 9`` () =
    let pattern = """^\d$"""
    let input = "24"
    testSameAsRuntime pattern input

[<Fact>]
let ``same as runtime 10`` () = // fails if or nullability is wrong
    let pattern = """(\s|\n|^)h:"""
    let input = """<a "h:"""
    testSameAsRuntime pattern input




[<Fact>]
let ``regex with label 3`` () =
    let pattern = """(?<Time>^\d)"""
    let input = "12:00am"
    testSameAsRuntime pattern input


//
[<Fact>]
let ``deduplication test`` () =
    let pattern = """(\/\*(\s*|.*)*\*\/)|(\/\/.*)""" // multiline comments
    let input = "/* This is a multi-line comment */"
    let matcher = Regex(pattern)
    let result = matcher.IsMatch(input)
    Assert.True(result)


[<Fact>]
let ``deduplication test 2 `` () =
    assertFirstMatchText
        """^[a-zA-Z]+(([\'\,\.\- ][a-zA-Z ])?[a-zA-Z]*)*$"""
        "T.F. Johnson"
        "T.F. Johnson"
// ? no test

[<Fact>]
let ``deduplication test 3`` () =
    let pattern = """(\/\*(\s*|.*)*\*\/)|(\/\/.*)""" // multiline comments
    let input = "/* This is a multi-line comment */"
    let matcher = Resharp.Regex(pattern)
    let result = matcher.Matches(input)
    let result2 = System.Text.RegularExpressions.Regex(pattern).Matches(input)

    assertAllEqual
        (result2 |> Seq.map (fun v -> v.Index, v.Length))
        (result |> Seq.map (fun v -> v.Index, v.Length))




[<Fact>]
let ``simple 1`` () =
    let input =
        "Lorem Ipsum is simply dummy text of the printing and typesetting industry.Lorem Ipsum has been the Aa11aBaAA standard"

    testSameAsRuntime "types" input


[<Fact>]
let ``multi-nodes ordering comparison`` () =
    let pattern = """((\s*|.*)*q/)"""
    let input = " q/"
    testSameAsRuntime pattern input

[<Fact>]
let ``http address optional`` () =
    let pattern =
        """^(ht|f)tp(s?)\:\/\/[a-zA-Z0-9\-\._]+(\.[a-zA-Z0-9\-\._]+){2,}(\/?)([a-zA-Z0-9\-\.\?\,\'\/\\\+\&%\$#_]*)?$"""

    let input = "http://www.wikipedia.org"
    testSameAsRuntime pattern input


[<Fact>]
let ``massive or pattern`` () =
    let pattern =
        """^((\d{2}((0[13578]|1[02])(0[1-9]|[12]\d|3[01])|(0[13456789]|1[012])(0[1-9]|[12]\d|30)|02(0[1-9]|1\d|2[0-8])))|([02468][048]|[13579][26])0229)$"""

    let input = "751231"
    testSameAsRuntime pattern input



[<Fact>]
let ``semantics test 1`` () =
    assertFirstMatchText @"(a|ab)*" "abab" "abab"



[<Fact>]
let ``top level duplicate test`` () =
    assertFirstMatchText
        @"((\(\d{3}\)?)|(\d{3}))([\s-./]?)(\d{3})([\s-./]?)(\d{4})"
        "1-(212)-123 4567"
        "(212)-123 4567"

#endif
