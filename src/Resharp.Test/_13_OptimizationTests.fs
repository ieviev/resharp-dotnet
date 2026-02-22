[<Xunit.Collection("Sequential")>]
module Resharp.Test._13_OptimizationTests

open Resharp
open Resharp.Types
open Xunit
open Common

#if DEBUG


[<Fact>]
let ``calc reverse prefix 1`` () =
    let regex = Regex("Twain")
    let matcher = regex.TSetMatcher
    let getder = regex.CreateNonInitialDerivative
    let prefix =
        Optimizations.calcPrefixSets getder matcher.Cache matcher.ReversePattern
    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix
    Assert.Equal("n;i;a;w;T", prefixString)


[<Fact>]
let ``calc reverse prefix 2`` () =
    assertOptimizationPrefixSets "_*A_*&_*B" "B"

[<Fact>]
let ``calc reverse prefix 3`` () =
    assertOptimizationPrefixSets @"_*Huck_*" "k;c;u;H"


[<Fact>]
let ``calc reverse prefix 4`` () =
    let optimizations, matcher = getInitOptimizationsAndMatcher @"~(_*\n\n_*)&_*Huck_*"
    match optimizations with
    | Optimizations.InitialAccelerator.SearchValuesPotentialStart(prefix) ->
        let mtarr = prefix.ToArray() |> Array.map (fun v -> v.Minterm)
        let prefixString = Optimizations.printPrefixSets matcher.Cache (mtarr |> Seq.toList)
        Assert.Contains(prefixString, [ @"[\nk];[\nck];[\ncku];[\nHcku]" ])
    | Optimizations.InitialAccelerator.SingleSearchValues _
    | Optimizations.InitialAccelerator.SingleSearchValuesPrefix _ -> () // also valid
    | _ -> failwith $"invalid optimization result: {optimizations}"

[<Fact>]
let ``calc reverse prefix 5`` () =
    assertPotentialStart @"~(.*11.*)&[az1]{8,}" [
        "[1az];[1az];[1az];[1az];[1az];[1az];[1az];[1az]"
    ]


[<Fact>]
let ``calc potential start 1`` () =
    let regex = Regex("Tom|Sawyer|Huckleberry|Finn")
    let matcher = regex.TSetMatcher
    let getder = regex.CreateNonInitialDerivative

    let prefix =
        Optimizations.calcPotentialMatchStart
            regex.Options
            getder
            matcher.Cache
            matcher.ReversePattern

    let prefixString = Optimizations.printPrefixSets matcher.Cache prefix

    Assert.Equal("[mnry];[enor];[Tiry]", prefixString)



[<Fact>]
let ``initialOptimizations 01`` () =
    let optimizations = getInitOptimizations "Twain"

    match optimizations with
    | Optimizations.InitialAccelerator.StringPrefix(prefix, _) ->
        Assert.True(prefix.Length = 5)
    | _ -> failwith "invalid optimization result"


[<Fact>]
let ``initialOptimizations 03`` () = assertStringPrefix "..g" "g"


[<Fact>]
let ``initialOptimizations 04`` () =
    let optimizations = getInitOptimizations "[a-z]shing"

    match optimizations with
    | Optimizations.InitialAccelerator.StringPrefix(prefix, _) ->
        Assert.Equal(5, prefix.Length)
    | _ -> failwith "invalid optimization result"

[<Fact>]
let ``initialOptimizations 06`` () =
    assertPotentialStart "Huck[a-zA-Z]+|Saw[a-zA-Z]+" [ "[A-Za-z];[kw];[ac];[Su]" ]

[<Fact>]
let ``initialOptimizations 08`` () =
    assertPotentialStart @"\s([A-Za-z]awyer|[A-Za-z]inn)\s" [ @"\s;[nr];[en];[iy];[A-Za-z];φ" ]

[<Fact>]
let ``initialOptimizations 09`` () =
    assertSetsPrefix @"\b\w+nn\b" @"\W;n;n;φ"

[<Fact>]
let ``initialOptimizations 10`` () =
    assertSetsPrefix @"(?<=\W)hello(?=\W)" @"\W;o;l;l;e;h;\W"

[<Fact>]
let ``initialOptimizations 11`` () =
    assertPotentialStart @"@( |)G( |)R( |)[a,A,@,(\/\\))]" [ @"[(),/@A\\a];[ R];[ GR];[ @G]" ]


[<Fact>]
let ``initialOptimizations 12`` () =
    assertSingleStart @"a( |)b( |)c( |)d" [ "d" ]

[<Fact>]
let ``initialOptimizations 13`` () =
    assertStringPrefix """.*(?=aaa)""" "aaa"

[<Fact>]
let ``initialOptimizations 16`` () = assertSetsPrefix @"\w+@\w+" @"\w;@;\w"

[<Fact>]
let ``initialOptimizations 18`` () =
    assertPotentialStart
        @"Sherlock Holmes|John Watson|Irene Adler|Inspector Lestrade|Professor Moriarty"
        [
            @"[enrsy];[deot];[almrs];[adlrt];[Aaiot];[ HWrs];[ eo];[LMkn];[ ceh];[or];[IJlo]"
        ]

[<Fact>]
let ``initialOptimizations 19`` () =
    assertPotentialStart
        @"(?i)Sherlock Holmes|John Watson|Irene Adler|Inspector Lestrade|Professor Moriarty"
        [
            @"[ENRSYenrsy];[DEOTdeot];[ALMRSalmrs];[ADLRTadlrt];[AIOTaiot];[ HRSWhrsw];[ EOeo];[K-Nk-n\u212A];[ CEHceh];[ORor];[IJLOijlo]"
        ]


[<Fact>]
let ``initialOptimizations 21`` () =
    assertStringPrefix @"(?<=Context1~(\T*\n\n\T*))(get, set)" @"get, set"

[<Fact>]
let ``initialOptimizations 25`` () =
    assertSingleStart """.*have.*&.*there.*""" [ "e" ]

[<Fact>]
let ``initialOptimizations 28`` () =
    assertSetsPrefix """.*(?=([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/[^ ]*)?)""" @"[^ /];/;/;:"


[<Fact>]
let ``initialOptimizations 29`` () =
    assertStringPrefix ".*(?=author)" @"author"



[<Fact>]
let ``apply prefix 1`` () =
    let applied = Common.applyPrefix """Twain"""
    assertContains [ @"(ε|_*niawT)"; @"(_*niawT|ε)"; @"(_*niawT)?" ] applied


#endif
