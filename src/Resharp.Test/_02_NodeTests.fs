module internal  Resharp.Test._02_NodeTests

open System.Reflection
open Resharp
open Resharp.Info
open Resharp.Types
open Xunit
open Common
open Resharp.Patterns
open Resharp.Runtime

type Flag = NodeFlags

#if DEBUG

module internal Helpers =
    let charSetSolver = CharSetSolver()

    let converter = ResharpRegexNodeConverter(charSetSolver)

    let bddBuilder2 =
        RegexBuilder(converter, charSetSolver, charSetSolver, Resharp.ResharpOptions())



let printNode (reg: RegexMatcher<_>, node: RegexNodeId) =
    try
        let matcher = reg
        matcher.PrettyPrintNode node
    with e ->
        failwith "failed to print node"


let assertSolverContains12 pattern =
    let pat = pattern
    let reg = Regex(pat)
    let b = reg.TSetMatcher.Cache.Builder

    match b.Node(reg.TSetMatcher.RawPattern) with
    | Concat(head = h; tail = t) ->
        match b.Node(h), b.Node(t) with
        | Singleton head, Singleton tail ->
            assertTrue (Solver.containsSet reg.TSetMatcher.Cache.Solver head tail)
        | _ -> failwith "_"
    | _ -> failwith "_"

let assertNotSolverContains12 pattern =
    let pat = pattern
    let reg = Regex(pat)
    let b = reg.TSetMatcher.Cache.Builder

    match b.Node(reg.TSetMatcher.RawPattern) with
    | Concat(head = h; tail = t) ->
        match b.Node(h), b.Node(t) with
        | Singleton head, Singleton tail ->
            assertFalse (Solver.containsSet reg.TSetMatcher.Cache.Solver head tail)
        | _ -> failwith "_"
    | _ -> failwith "_"




[<Fact>]
let ``_ solver 1`` () = assertSolverContains12 "[a-z]a"

[<Fact>]
let ``_ solver 2`` () = assertNotSolverContains12 "a[a-z]"




[<Fact>]
let ``conversion 2.1`` () =
    assertConverted "(_*B_*&_*A_*)" [ "(_*A_*&_*B_*)"; "(_*B_*&_*A_*)" ]


[<Fact>]
let ``conversion 2.2`` () =
    assertConverted "(_*A_*&_*B_*)&_*B_*" [ @"(_*B_*&_*A_*)"; "(_*A_*&_*B_*)" ]




[<Fact>]
let ``conversion 2.6`` () =
    assertConverted """([a-zA-Z]+)Huck|([a-zA-Z]+)Saw""" [
        """[A-Za-z]+(Huck|Saw)"""
        """[A-Za-z]+(Saw|Huck)"""
    ]

[<Fact>]
let ``conversion 2.7`` () =
    assertConverted """t(s?|s)day""" [ "ts?day" ]




[<Fact>]
let ``conversion 2.8`` () =
    assertConverted """1111""" [ "1{4}"; "(11){2}" ]





[<Fact>]
let ``identity true star`` () =

    let matcher = Regex(@"[\s\S]*").TSetMatcher
    let nodes = matcher.RawPattern

    let identity = RegexNodeId.TOP_STAR = nodes

    Assert.True(identity)


[<Fact>]
let ``identity true star reversed`` () =

    let matcher = Regex(@"[\s\S]*").TSetMatcher
    let nodes = matcher.ReversePattern

    let identity = RegexNodeId.TOP_STAR = nodes

    Assert.True(identity)



let equalSeq (xs1: seq<'t>) (xs2: seq<'t>) : unit = Assert.Equal<'t>(xs1, xs2)


[<Fact>]
let ``conversion lookaround 2 `` () =
    assertConverted ".(?=A.*)" [ @".(?=A)"; @".(?=A.*)" ]


[<Fact>]
let ``conversion label`` () =
    assertConverted @"(?<Time>^\d)" [ @"^\d"; @"^φ"; @"(?<=(\n|\A))φ" ]

[<Fact>]
let ``conversion conc `` () = assertConverted "Twain" [ "Twain" ]

[<Fact>]
let ``flags 01`` () =
    let matcher = Regex(@"^\d$").TSetMatcher
    let f = matcher.Cache.Builder.Info(matcher.ReverseTrueStarredPattern).NodeFlags
    assertFlag f Flag.DependsOnAnchorFlag

[<Fact>]
let ``flags 02`` () =
    let matcher = Regex("""(?<=\W)\w+nn(?=\W)""").TSetMatcher
    let f = matcher.Cache.Builder.Info(matcher.ReverseTrueStarredPattern).NodeFlags
    assertNotFlag f Flag.DependsOnAnchorFlag


[<Fact>]
let ``flags 03`` () =
    let matcher = Regex("""(?<=.?)""").TSetMatcher
    let f = matcher.Cache.Builder.Info(matcher.ReverseTrueStarredPattern).NodeFlags
    assertFlag f Flag.CanBeNullableFlag
    assertFlag f Flag.IsAlwaysNullableFlag

[<Fact>]
let ``flags 07`` () =
    let matcher = Regex(@"a(?=b)").TSetMatcher
    let f = matcher.Cache.Builder.Info(matcher.RawPattern).NodeFlags
    assertFlag f Flag.HasSuffixLookaheadFlag

[<Fact>]
let ``flags 08`` () =
    let matcher = Regex(@"(a|b)(?=b)").TSetMatcher
    let f = matcher.Cache.Builder.Info(matcher.RawPattern).NodeFlags
    assertFlag f Flag.HasSuffixLookaheadFlag

[<Fact>]
let ``flags 09`` () =
    let matcher = Regex(@"(?<=b)(a|b)").TSetMatcher
    let f = matcher.Cache.Builder.Info(matcher.RawPattern).NodeFlags
    assertFlag f Flag.HasPrefixLookbehindFlag


[<Fact>]
let ``flags 10`` () =
    let matcher = Regex(""".*$""").TSetMatcher
    let f = matcher.Cache.Builder.Info(matcher.RawPattern).NodeFlags
    assertFlag f Flag.HasSuffixLookaheadFlag


[<Fact>]
let ``identity derivative 2`` () =
    let m = Regex(@"((_*t|)neW_*&_*erohsa_*&_*lirpA_*&_*yadsruhT_*)")
    let deriv = der1Node m "test" true
    let req = m.TSetMatcher.RawPattern = deriv
    Assert.True(req)


[<Fact>]
let ``identity and 1`` () =
    let m = Regex(@"((nglish_*|_*English_*)&~(_*\n\n_*)\n&_*King_*&_*Paris_*)")

    let deriv = der1Node m "English" true

    let req = m.TSetMatcher.RawPattern = deriv
    Assert.True(req)


[<Fact>]
let ``identity rev 1`` () =
    let m = Regex(@"_*Huck_*")

    let deriv = der1Rev m "g"

    let req = m.TSetMatcher.ReversePattern = deriv
    Assert.True(req)

[<Fact>]
let ``identity singleton 1`` () =
    let m = Regex(@".*b|a")
    let b = m.TSetMatcher.Cache.Builder
    let l1 =
        match b.Node(m.TSetMatcher.RawPattern) with
        | Or(nodes) ->
            let conc =
                nodes
                |> Seq.find (fun n ->
                    match b.Node(n) with
                    | Concat(_) -> true
                    | _ -> false
                )

            let loop =
                match b.Node(conc) with
                | Concat(regexNode, _) -> regexNode
                | _ -> failwith "debug"

            loop
        | _ -> failwith "debug"

    let l2 =
        match b.Node(m.TSetMatcher.ReversePattern) with
        | Or(nodes) ->
            let conc =
                nodes
                |> Seq.find (fun n ->
                    match b.Node(n) with
                    | Concat(_) -> true
                    | _ -> false
                )

            let loop =
                match b.Node(conc) with
                | Concat(_, tail) -> tail
                | _ -> failwith "debug"

            loop
        | _ -> failwith "debug"

    let req = l1 = l2
    Assert.True(req)

let assertNodeWithoutPrefix (patt: string) (expected: string list) =
    let m = Resharp.Regex(patt).TSetMatcher
    let n = m.RawPattern
    let n2 = Optimizations.mkNodeWithoutLookbackPrefix m.Cache.Builder n
    assertContains expected (m.PrettyPrintNode n2)


[<Fact>]
let ``withoutprefix 01`` () =
    assertNodeWithoutPrefix "(?<=author).*&.*and.*" [
        "(.*and.*&.*)"
        ".*and.*"
        "(.*&.*and.*)"
    ]

[<Fact>]
let ``withoutprefix 02`` () =
    assertNodeWithoutPrefix @"\b11" [ "11" ]


[<Fact>]
let ``withoutprefix 03`` () =
    assertNodeWithoutPrefix """(?<=aaa).*""" [ ".*" ]


[<Fact>]
let ``minterms 01`` () =
    assertMinterms "(a|b|c)" [ "[^a-c]"; "[a-c]" ]

[<Fact>]
let ``minterms 02`` () =
    assertMinterms "[a-q][^u-z]{13}x" [ "[^a-qu-z]"; "[a-q]"; "[u-wyz]"; "x" ]

[<Fact>]
let ``fixed length 1`` () =
    let regex = Regex("Twain")
    let matcher = regex.TSetMatcher
    let prefixLen = matcher.Cache.Builder.GetFixedLength(matcher.ReversePattern)
    Assert.Equal(ValueSome 5, prefixLen)

[<Fact>]
let ``fixed length 2`` () =
    let regex = Regex("[a-q][^u-z]{13}x")
    let matcher = regex.TSetMatcher
    let prefixLen = matcher.Cache.Builder.GetFixedLength(matcher.ReversePattern)
    Assert.Equal(ValueSome 15, prefixLen)

[<Fact>]
let ``fixed length 3`` () =
    let regex = Regex("""\b1\b""")
    let matcher = regex.TSetMatcher
    let prefixLen = matcher.Cache.Builder.GetFixedLength(matcher.ReversePattern)
    Assert.Equal(ValueSome 1, prefixLen)


[<Fact>]
let ``fixed length 4`` () =
    let regex = Regex("""\b1\b""")
    let matcher = regex.TSetMatcher
    let prefixLen = matcher.Cache.Builder.GetFixedLength(matcher.ReversePattern)
    Assert.Equal(ValueSome 1, prefixLen)







#endif
