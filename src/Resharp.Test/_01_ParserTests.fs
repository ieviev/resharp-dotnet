[<Xunit.Collection("Sequential")>]
module Resharp.Test._01_ParserTests

open System.Globalization
open Xunit
open Resharp.Runtime
#if DEBUG

let parseExtended (pattern: string) : RegexTree =
    ExtendedRegexParser.Parse(
        pattern,
        RegexOptions.ExplicitCapture ||| RegexOptions.NonBacktracking,
        CultureInfo.InvariantCulture
    )


[<Fact>]
let ``conjunction parse test 1: c..&..t`` () =

    let capture = parseExtended "c..&..t" // cat, cut , c.t
    let node = capture.Root.Child(0)

    [
        node.Kind = RegexNodeKind.Conjunction
        node.ChildCount() = 2
        node.Child(0).Kind = RegexNodeKind.Concatenate
        node.Child(1).Kind = RegexNodeKind.Concatenate
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed"))

[<Fact>]
let ``conjunction parse test 2: aa&bb`` () =
    let capture = parseExtended "aa&bb"
    let node = capture.Root.Child(0)

    [
        node.Kind = RegexNodeKind.Conjunction
        node.ChildCount() = 2
        node.Child(0).Kind = RegexNodeKind.Multi
        node.Child(1).Kind = RegexNodeKind.Multi
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed"))

[<Fact>]
let ``conjunction parse test 3`` () =
    // working recursively with other groups
    let capture = parseExtended @"(ca..)&(...s)"
    let node = capture.Root.Child(0)

    [
        node.Kind = RegexNodeKind.Conjunction
        node.ChildCount() = 2
        node.Child(0).Kind = RegexNodeKind.Concatenate
        node.Child(1).Kind = RegexNodeKind.Concatenate
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed"))


[<Fact>]
let ``conjunction parse test 4`` () =

    // infinite recursion test 1
    let capture = parseExtended """(?<=\()c"""
    let node = capture.Root.Child(0)

    [
        node.Kind = RegexNodeKind.Concatenate
        node.ChildCount() = 2
        node.Child(0).Kind = RegexNodeKind.PositiveLookaround
        node.Child(1).Kind = RegexNodeKind.One
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed"))

[<Fact>]
let ``conjunction parse test 5`` () =

    let capture = parseExtended "aa&bb&cc"
    let node = capture.Root.Child(0)
    [ node.Kind = RegexNodeKind.Conjunction; node.ChildCount() = 3 ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed"))


[<Fact>]
let ``conjunction parse test 6`` () =
    let capture = parseExtended """(?<=a)b&c"""
    let node = capture.Root.Child(0)

    [ node.Kind = RegexNodeKind.Conjunction; node.ChildCount() = 2 ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed"))


[<Fact>]
let ``negation parse test 01: ~(ab)`` () =

    let capture = parseExtended "~(ab)"
    let node = capture.Root.Child(0)

    [
        node.Options.HasFlag RegexOptions.Negated
        node.ChildCount() = 1
        node.Kind = RegexNodeKind.Capture
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))


[<Fact>]
let ``negation parse test 02: ~ab`` () =

    let capture = parseExtended "~ab"
    let node = capture.Root.Child(0)

    [
        node.ChildCount() = 2
        node.Kind = RegexNodeKind.Concatenate
        node.Child(0).Options.HasFlag RegexOptions.Negated
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))


[<Fact>]
let ``negation parse test 03: ~(ab|cd)`` () =

    let capture = parseExtended "~(ab|cd)"
    let node = capture.Root.Child(0)
    [
        node.Options.HasFlag RegexOptions.Negated
        node.ChildCount() = 1
        node.Kind = RegexNodeKind.Capture
        not (node.Child(0).Options.HasFlag RegexOptions.Negated)
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))


[<Fact>]
let ``negation parse test 04: (ab|~cd)`` () =

    let capture = parseExtended "(ab|~cd)"
    let node = capture.Root.Child(0)
    [
        node.ChildCount() = 2
        node.Kind = RegexNodeKind.Alternate
        node.Child(1).Child(0).Options.HasFlag RegexOptions.Negated
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))


[<Fact>]
let ``negation parse test 05: (ab|~(cd))`` () =

    let capture = parseExtended "(ab|~(cd))"
    let node = capture.Root.Child(0)

    [
        node.ChildCount() = 2
        node.Kind = RegexNodeKind.Alternate
        node.Child(1).Kind = RegexNodeKind.Capture
        node.Child(1).Options.HasFlag RegexOptions.Negated
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))

[<Fact>]
let ``negation parse test 06: ab~c`` () =

    let capture = parseExtended "ab~c"
    let node = capture.Root.Child(0)

    [
        node.ChildCount() = 2
        node.Kind = RegexNodeKind.Concatenate
        node.Child(0).Kind = RegexNodeKind.Multi // ab
        node.Child(1).Options.HasFlag RegexOptions.Negated // ~c
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))




[<Fact>]
let ``negation parse test 07: ab~cd`` () =

    let capture = parseExtended "ab~cd"
    let node = capture.Root.Child 0

    [
        node.ChildCount() = 3
        node.Kind = RegexNodeKind.Concatenate
        node.Child(0).Kind = RegexNodeKind.Multi // ab
        node.Child(1).Options.HasFlag RegexOptions.Negated // ~c
        not (node.Child(2).Options.HasFlag RegexOptions.Negated) // d
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))


[<Fact>]
let ``negation parse test 08: ab~c.`` () =

    let capture = parseExtended "ab~c."
    let node = capture.Root.Child(0)

    [
        node.ChildCount() = 3
        node.Kind = RegexNodeKind.Concatenate
        node.Child(0).Kind = RegexNodeKind.Multi // ab
        node.Child(1).Options.HasFlag RegexOptions.Negated // ~c
        not (node.Child(2).Options.HasFlag RegexOptions.Negated) // .
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))


[<Fact>]
let ``negation parse test 09: b~(c.)`` () =

    let capture = parseExtended "b~(c.)"
    let node = capture.Root.Child(0)

    [
        node.ChildCount() = 2
        node.Kind = RegexNodeKind.Concatenate
        node.Child(0).Kind = RegexNodeKind.One // b
        node.Child(1).Options.HasFlag(RegexOptions.Negated) // ~c.
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))


[<Fact>]
let ``negation parse test 10: \(~\)`` () =

    let capture = parseExtended @"\(~\)"
    let node = capture.Root.Child(0)

    [
        node.ChildCount() = 2
        node.Kind = RegexNodeKind.Concatenate
        node.Child(0).Kind = RegexNodeKind.One // (
        node.Child(1).Options.HasFlag RegexOptions.Negated // ~)
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))


[<Fact>]
let ``negation parse test 11: ~(d)f`` () =
    let capture = parseExtended @"~(d)f"
    let node = capture.Root.Child(0)
    [
        node.ChildCount() = 2
        node.Kind = RegexNodeKind.Concatenate
        node.Child(0).Options.HasFlag RegexOptions.Negated // ~)
        node.Child(1).Kind = RegexNodeKind.One // (
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))



[<Fact>]
let ``negation parse test 12: b(~d|~e|~f)`` () =

    let capture = parseExtended @"b(~d|~e|~f)"
    let node = capture.Root.Child(0)

    [
        node.ChildCount() = 2
        node.Child(1).ChildCount() = 3
        node.Child(1).Kind = RegexNodeKind.Alternate
        not (node.Child(1).Options.HasFlag RegexOptions.Negated) // outer not negated
        node.Child(1).Child(0).Options.HasFlag RegexOptions.Negated // ~d
        node.Child(1).Child(1).Options.HasFlag RegexOptions.Negated // ~e
        node.Child(1).Child(2).Options.HasFlag RegexOptions.Negated // ~f
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))


[<Fact>]
let ``negation parse test 13: ~(abc)`` () =

    let capture = parseExtended @"~(abc)"
    let node = capture.Root.Child(0)
    [
        node.ChildCount() = 1
        node.Kind = RegexNodeKind.Capture
        node.Options.HasFlag RegexOptions.Negated
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))


[<Fact>]
let ``negation parse test 14: .*\d.*&~(.*01.*)`` () =

    let capture = parseExtended @".*\d.*&~(.*01.*)"
    let node = capture.Root.Child(0)

    [
        node.ChildCount() = 2
        node.Child(1).Options.HasFlag RegexOptions.Negated // ~(.*01.*)
        not (node.Child(1).Child(0).Options.HasFlag RegexOptions.Negated) // .*
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))



[<Fact>]
let ``negation parse test 15: multiset~c`` () =

    let capture = parseExtended @"French~(\n\n)"
    let node = capture.Root.Child(0)

    [
        node.ChildCount() = 2
        node.Kind = RegexNodeKind.Concatenate
        node.Child(0).Kind = RegexNodeKind.Multi // ab
        node.Child(1).Options.HasFlag RegexOptions.Negated // ~c
    ]
    |> Seq.iteri (fun i x -> Assert.True(x, $"condition index {i} failed\n{capture.Root}"))


#endif
