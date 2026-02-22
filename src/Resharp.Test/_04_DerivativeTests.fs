module Resharp.Test._04_DerivativeTests

open Common

#if DEBUG

open Resharp
open Resharp.Types
open Xunit

let test1stDerivatives (pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let result = Common.der1 matcher input false
    Assert.Contains(result, expectedDerivatives)

let testRawDerivative (pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern)
    let result = der1 matcher input true
    Assert.Equal(expectedDerivative, result)

let testRevDerivative (pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let result = der1Rev matcher input
    Assert.Contains(matcher.TSetMatcher.PrettyPrintNode(result), expectedDerivatives)

let assertRevTSDerivative
    (pattern: string)
    (input: string)
    (expectedDerivatives: string list)
    =
    let matcher = Regex(pattern)
    let result = der1RevTS matcher input
    Assert.Contains(matcher.TSetMatcher.PrettyPrintNode(result), expectedDerivatives)


let assertTSCenterDerivative
    (pattern: string)
    (input: string)
    (expectedDerivatives: string list)
    =
    let regex = Regex(pattern)
    let matcher = regex.TSetMatcher

    let result =
        Algorithm.RegexNode.derivative (
            matcher.Cache.Builder,
            LocationKind.Center,
            matcher.Cache.Classify(input[0]),
            regex.TSetMatcher.ReverseTrueStarredPattern
        )

    Assert.Contains(regex.TSetMatcher.PrettyPrintNode(result), expectedDerivatives)

let testPartDerivative (pattern: string, input: string, expectedDerivative: string) =
    let matcher = Regex(pattern)
    let result = der1 matcher input true


    Assert.Equal(expectedDerivative, result)


let testPartDerivatives (pattern: string, input: string, expectedDerivatives: string list) =
    let matcher = Regex(pattern)
    let prettyResult = Common.der1 matcher input true
    Assert.Contains(prettyResult, expectedDerivatives)




let testPartDerivativeFromLocation
    (pattern: string, input: string, position: int, expectedDerivative: string)
    =
    let matcher = Regex(pattern)
    let result = der1RPos matcher input true position
    Assert.Equal(expectedDerivative, result)


let testPartDerivativeFromLocationMultiple
    (pattern: string, input: string, position: int, expectedDerivatives: string list)
    =
    let matcher = Regex(pattern)
    let prettyResult = der1RPos matcher input true position

    Assert.Contains(prettyResult, expectedDerivatives)



[<Fact>]
let ``raw derivative of ab`` () = testRawDerivative ("ab", "ab", "b")

[<Fact>]
let ``derivative of ab`` () =
    test1stDerivatives (
        "ab",
        "ab",
        [ "(b|_*ab)"; @"(_*ab|b)"; @"(ε|_*a)b"; @"(_*a|ε)b"; "(_*a)?b" ]
    )


[<Fact>]
let ``derivative of true`` () = testPartDerivative ("_", "324", "⊥*")


[<Fact>]
let ``derivative of true ismatch`` () = testPartDerivative ("_", "324", "⊥*")


[<Fact>]
let ``derivative of lookback 1`` () =
    testPartDerivativeFromLocationMultiple (
        @"(?<=-.*).*",
        "-aaaa-",
        5,
        [ @"(.*|(?<=.*).*)"; @"((?<=.*).*|.*)"; @"(?<=.*).*"; @".*" ]
    )

[<Fact>]
let ``derivative lookaround 1`` () =
    testPartDerivatives (@"^\d$", "1", [ @"$" ])



[<Fact>]
let ``derivative lookaround 2`` () = testPartDerivative (@"\b11", "11", "1")

//([]1|1)

[<Fact>]
let ``derivative boundary 1`` () =
    testPartDerivativeFromLocation (@"(?<=\s)22", "1 2", 1, "22")

[<Fact>]
let ``derivative boundary 4`` () =
    testPartDerivativeFromLocation (@"(?<=\d)a", "1a", 0, "a")

[<Fact>]
let ``derivative of plus`` () =
    testPartDerivatives (@"^\d+$", "123", [ @"\d*$" ])


[<Fact>]
let ``derivative concat lookaround`` () =
    testPartDerivatives (@"^\d+$", "123", [ @"\d*$" ])


[<Fact>]
let ``deriv negation 1 `` () =
    testPartDerivatives (
        @"~(.*11.*)",
        "1",
        [ @"~((.*1)?1.*)"; @"~((1|.*11).*)"; @"~((.*11|1).*)" ]
    )


[<Fact>]
let ``derivative eats node from set`` () =
    testPartDerivativeFromLocationMultiple (
        @"^((0?[13578]a)|(0?[13456789]a))$",
        "4a",
        0,
        [ @"a$" ]
    )



[<Fact>]
let ``derivative neg lookaround 1`` () =
    assertRawDerivative @"((?<=B.*).*&~(.*A.*))" "BA" [
        @"(?<=.*)(.*&~(.*A.*))"
        @"(?<=.*)(~(.*A.*)&.*)"
        @"(~(.*A.*)&.*)"
        @".*(.*&~(.*A.*))"
        @".*(~(.*A.*)&.*)"
    ]


[<Fact>]
let ``simple 1`` () =
    testRevDerivative ("..g", "gggg", [ @".{2,2}"; ".." ])



[<Fact>]
let ``inter deriv 1`` () =
    test1stDerivatives (
        "(.*a.*&.*c.*&.*b.*)",
        "ccab",
        [
            @"((.*a.*&.*b.*)|_*(.*a.*&.*c.*&.*b.*))"
            @"((.*a.*&.*b.*)|_*(.*c.*&.*a.*&.*b.*))"
            @"((.*b.*&.*a.*)|_*(.*b.*&.*c.*&.*a.*))"
            @"((.*b.*&.*a.*)|_*(.*c.*&.*b.*&.*a.*))"
            @"(_*(.*a.*&.*b.*&.*c.*)|(.*a.*&.*b.*))"
            @"(_*(.*a.*&.*c.*&.*b.*)|(.*a.*&.*b.*))"
            @"(_*(.*b.*&.*c.*&.*a.*)|(.*b.*&.*a.*))"
            @"(_*(.*c.*&.*a.*&.*b.*)|(.*a.*&.*b.*))"
            @"(_*(.*c.*&.*b.*&.*a.*)|(.*b.*&.*a.*))"
            @"((.*b.*&.*a.*)|_*(.*b.*&.*a.*&.*c.*))"
            @"(_*(.*b.*&.*a.*&.*c.*)|(.*b.*&.*a.*))"
            @"((.*a.*&.*b.*)|_*(.*a.*&.*b.*&.*c.*))"
        ]
    )


[<Fact>]
let ``lookback1`` () =
    assertRawDerivative "(?<!a)b" "bb" [ "(~(_*a)b)?"; "b?" ]


#endif
