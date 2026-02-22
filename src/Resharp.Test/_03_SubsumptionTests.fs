// [<Xunit.Collection("Sequential")>]
module Resharp.Test._03_SubsumptionTests


#if DEBUG

open Resharp
open Resharp.Types
open Xunit
open Common

[<Fact>]
let ``nullable 01`` () =
    let regex = Regex(".{2}c")
    let matcher = regex.TSetMatcher
    let node = matcher.RawPattern
    let nullable = matcher.IsNullable(LocationKind.Begin, node)
    assertFalse nullable


[<Fact>]
let ``derivative neg lookaround 2`` () =
    assertRawDerivative "((?<=.*).*&~(.*A.*))" "A" [ @"⊥" ]



[<Fact>]
let ``subsumption and larger `` () =
    assertRawDerivative @"(.* and .*|and .*)&.*" "aaa" [
        @"(.* and .*|nd .*)"
        "(nd .*|.* and .*)"
        @"(.* a)?nd .*"

        @"((ε|.* a)nd .*&.*)" // subsumed
        @"(.*&(.* a|ε)nd .*)"
        @"(.*&(ε|.* a)nd .*)"
        @"((.* a|ε)nd .*&.*)"
    ]


[<Fact>]
let ``subsumption and loop `` () =
    assertRawDerivative @"(.*&.*s)" "aaa" [ @".*s" ]


[<Fact>]
let ``subsumption or loop `` () =
    assertRawDerivative @"(a*|.*)" "aaa" [ @".*" ]



[<Fact>]
let ``conversion 0.2.5`` () =
    assertConverted """Huck[a-zA-Z]+|Saw[a-zA-Z]+""" [
        @"(Huck|Saw)[A-Za-z]+"
        @"(Saw|Huck)[A-Za-z]+"
    ]


[<Fact>]
let ``b conversion 1 `` () =
    assertRawDerivative @".*t.*hat.*" "ttt" [
        @".*(t.*)?hat.*"
        @".*(hat.*|t.*hat.*)"
        @".*(t.*hat.*|hat.*)"
        @".*hat.*" // <- this is nontrivial to infer
    ]


[<Fact>]
let ``b conversion 2.1 `` () =
    assertTSDerivative @"^a*b" "a" [
        @"(_*^)?a*b"
        @"(a*b|_*^a*b)"
        @"(_*^a*b|a*b)"

        @"a*b|_*(?<=(\n|\A))a*b)"
        @"(a*b|_*(?<=(\A|\n))a*b)"
    ]


[<Fact>]
let ``conversion 0.2.3`` () =
    assertConverted "(.*|(.*11.*|1.*))" [ ".*" ]


[<Fact>]
let ``rewrite suffix 1`` () =
    assertConverted """.*(?=.*def)&.*def""" [ ".*def(?=.*def_*)"; ".*def(?=.*def)" ]

[<Fact>]
let ``rewrite prefix 1`` () =
    assertConverted """(?<=abc).*&.*def""" [ "(?<=abc).*def" ]


[<Fact>]
let ``merge 1`` () = assertConverted """a|s""" [ "[as]" ]


[<Fact>]
let ``merge 2`` () = assertConverted """at|st""" [ "[as]t" ]


[<Fact>]
let ``look nontrivial 1`` () =
    assertConverted @"(.*ereht)?.*" [ @".*" ]

[<Fact>]
let ``loop nontrivial 2 `` () =
    assertRawDerivative @".*t.*hat" "ttt" [ @".*hat" ]

[<Fact>]
let ``sub 001`` () =
    assertConverted @"(.*1)?(.*1){2,}" [ "(.*1){2,}" ]

[<Fact>]
let ``sub 002`` () = assertConverted @"(.*1)?.*1" [ ".*1" ]

[<Fact>]
let ``sub 003`` () =
    assertConverted @".*1.*1" [ "(.*1){2,}" ]

[<Fact>]
let ``sub 004`` () =
    assertConverted @".*1.*1$" [ "(.*1){2,}$" ]

[<Fact>]
let ``sub 005`` () =
    assertConverted @".*1.*1.*1$" [ "(.*1){3,}$" ]

[<Fact>]
let ``sub 006`` () = assertConverted @"(.*1)?.*a" [ ".*a" ]

[<Fact>]
let ``sub 007`` () =
    assertConverted @"((.*1)?|b).*a" [ ".*a" ]

[<Fact>]
let ``sub 008`` () = assertConverted @"(.*|)" [ ".*" ]

#endif
