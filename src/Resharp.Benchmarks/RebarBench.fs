namespace Resharp.Benchmarks

open System
open System.IO
open System.Text
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Reports

module RebarData =

    let benchDir =
        let rec find dir =
            let candidate = Path.Combine(dir, "src", "Resharp.Benchmarks", "benchmarks")
            if Directory.Exists(candidate) then candidate
            else
                let parent = Directory.GetParent(dir)
                if isNull parent then failwith "could not find benchmarks dir"
                else find parent.FullName
        find (Directory.GetCurrentDirectory())

    let haystacksDir = Path.Combine(benchDir, "haystacks")
    let regexesDir = Path.Combine(benchDir, "regexes")
    let defsDir = Path.Combine(benchDir, "definitions")

    type BenchDef = {
        group: string
        name: string
        regex: string
        haystack: string
        caseInsensitive: bool
        model: string
    }

    let private readLines (path: string) =
        File.ReadAllLines(path) |> Array.filter (fun l -> l.Length > 0)

    let private loadHaystack (haystackNode: Tommy.TomlNode) =
        match haystackNode with
        | :? Tommy.TomlString as s -> s.Value
        | :? Tommy.TomlTable as t ->
            let mutable text =
                if t.HasKey("path") then
                    let path = Path.Combine(haystacksDir, t["path"].AsString.Value)
                    File.ReadAllText(path, Encoding.UTF8)
                elif t.HasKey("contents") then
                    t["contents"].AsString.Value
                else
                    failwith "haystack must have 'path' or 'contents'"

            if t.HasKey("line-start") || t.HasKey("line-end") then
                let lines = text.Split('\n')
                let start = if t.HasKey("line-start") then int t["line-start"].AsInteger.Value else 0
                let end_ = if t.HasKey("line-end") then int t["line-end"].AsInteger.Value else lines.Length
                text <- String.Join("\n", lines[start .. end_ - 1])

            if t.HasKey("repeat") then
                let n = int t["repeat"].AsInteger.Value
                let sb = StringBuilder(text.Length * n)
                for _ in 1..n do sb.Append(text) |> ignore
                text <- sb.ToString()

            text
        | _ -> failwith "unexpected haystack type"

    let private loadRegex (regexNode: Tommy.TomlNode) =
        match regexNode with
        | :? Tommy.TomlString as s -> s.Value
        | :? Tommy.TomlTable as t ->
            let path = Path.Combine(regexesDir, t["path"].AsString.Value)
            let isLiteral = t.HasKey("literal") && t["literal"].AsBoolean.Value
            let perLine = if t.HasKey("per-line") then t["per-line"].AsString.Value else ""

            if isLiteral && perLine = "alternate" then
                let lines = readLines path
                let escaped = lines |> Array.map System.Text.RegularExpressions.Regex.Escape
                String.Join("|", escaped)
            else
                File.ReadAllText(path).Trim()
        | _ -> failwith "unexpected regex type"

    let private hasEngine (engines: Tommy.TomlArray) (name: string) =
        engines.Children
        |> Seq.exists (fun e ->
            let v = e.AsString.Value
            not (v.StartsWith("#")) && v = name)

    let private parseToml (group: string) (path: string) =
        use fs = File.OpenRead(path)
        use sr = new StreamReader(fs)
        let tb = Tommy.TOML.Parse(sr)

        if not (tb.HasKey("bench")) then Array.empty
        else
            [| for bench in tb["bench"].Children do
                let model = bench["model"].AsString.Value
                // skip compile-only and multi-pattern benchmarks
                if model <> "compile" && bench.HasKey("regex") then
                    let regexNode = bench["regex"]
                    let mkBench () = {
                        group = group
                        name = bench["name"].AsString.Value
                        regex = loadRegex regexNode
                        haystack = loadHaystack bench["haystack"]
                        caseInsensitive = bench.HasKey("case-insensitive") && bench["case-insensitive"].AsBoolean.Value
                        model = model
                    }
                    // skip multi-pattern (per-line = "pattern")
                    match regexNode with
                    | :? Tommy.TomlTable as t ->
                        if not (t.HasKey("per-line") && t["per-line"].AsString.Value = "pattern") then
                            let engines = bench["engines"].AsArray
                            if hasEngine engines "dotnet/compiled" && hasEngine engines "resharp" then
                                mkBench ()
                    | _ ->
                        let engines = bench["engines"].AsArray
                        if hasEngine engines "dotnet/compiled" && hasEngine engines "resharp" then
                            mkBench ()
            |]

    let private loadGroup (subdir: string) =
        let dir = Path.Combine(defsDir, subdir)
        if not (Directory.Exists(dir)) then Array.empty
        else
            Directory.GetFiles(dir, "*.toml")
            |> Array.collect (fun path ->
                let file = Path.GetFileNameWithoutExtension(path)
                let group = $"{subdir}/{file}"
                try parseToml group path
                with ex ->
                    eprintfn $"warning: skipping {group}: {ex.Message}"
                    Array.empty)

    let allBenches =
        lazy (
            [| yield! loadGroup "curated"
               yield! loadGroup "resharp" |]
        )

    let benchMap =
        lazy (
            allBenches.Value
            |> Array.map (fun b -> $"{b.group}/{b.name}", b)
            |> dict
        )

    let mutable nameFilter: string voption = ValueNone

    let benchNames =
        lazy (
            allBenches.Value
            |> Array.map (fun b -> $"{b.group}/{b.name}")
        )


type BenchConfig() =
    inherit ManualConfig()
    do base.SummaryStyle <- SummaryStyle.Default.WithMaxParameterColumnWidth(26)

[<ShortRunJob>]
[<Config(typeof<BenchConfig>)>]
[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
[<CategoriesColumn>]
type RebarBench() =

    let mutable compiled = Unchecked.defaultof<System.Text.RegularExpressions.Regex>
    let mutable resharp = Unchecked.defaultof<Resharp.Regex>
    let mutable haystack = ""

    [<ParamsSource("BenchNames")>]
    member val Name = "" with get, set

    member _.BenchNames =
        let all = RebarData.benchNames.Value
        match RebarData.nameFilter with
        | ValueNone -> all
        | ValueSome f -> all |> Array.filter (fun n -> n.StartsWith(f))

    [<GlobalSetup>]
    member this.Setup() =
        let bench = RebarData.benchMap.Value[this.Name]
        haystack <- bench.haystack

        let dotnetOpts =
            System.Text.RegularExpressions.RegexOptions.Compiled
            ||| (if bench.caseInsensitive then System.Text.RegularExpressions.RegexOptions.IgnoreCase
                 else System.Text.RegularExpressions.RegexOptions.None)

        compiled <- System.Text.RegularExpressions.Regex(bench.regex, dotnetOpts)

        let resharpOpts = Resharp.ResharpOptions.HighThroughputDefaults
        resharpOpts.IgnoreCase <- bench.caseInsensitive
        resharp <- Resharp.Regex(bench.regex, resharpOpts)

    [<Benchmark>]
    member _.Compiled() = compiled.Count(haystack)

    [<Benchmark(Baseline = true)>]
    member _.Resharp() = resharp.Count(haystack)
