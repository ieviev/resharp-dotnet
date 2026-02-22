using System.Text.RegularExpressions;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Jobs;
using BenchmarkDotNet.Reports;

namespace Resharp.Benchmarks;

public class BenchConfig : ManualConfig
{
    public BenchConfig()
    {
        SummaryStyle = SummaryStyle.Default.WithMaxParameterColumnWidth(26);
    }
}

[ShortRunJob]
[Config(typeof(BenchConfig))]
[GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)]
[CategoriesColumn]
public class RebarBench
{
    System.Text.RegularExpressions.Regex compiled = null!;
    System.Text.RegularExpressions.Regex sourceGenerated = null!;
    Resharp.Regex resharp = null!;
    string haystack = "";

    [ParamsSource(nameof(BenchNames))]
    public string Name { get; set; } = "";

    public IEnumerable<string> BenchNames
    {
        get
        {
            var all = RebarData.BenchNames.Value;
            if (RebarData.NameSet is { } set)
                return all.Where(set.Contains);
            return RebarData.NameFilter is { } f
                ? all.Where(n => n.StartsWith(f))
                : all;
        }
    }

    [GlobalSetup]
    public void Setup()
    {
        var bench = RebarData.BenchMap.Value[Name];
        haystack = bench.Haystack;

        var dotnetOpts = RegexOptions.Compiled
            | (bench.CaseInsensitive ? RegexOptions.IgnoreCase : RegexOptions.None);

        compiled = new System.Text.RegularExpressions.Regex(bench.Pattern, dotnetOpts);

        if (!SourceGenRegexes.Lookup.TryGetValue(Name, out sourceGenerated!))
            throw new InvalidOperationException($"no source-generated regex for '{Name}', run: dotnet run -- --generate");

        var resharpOpts = ResharpOptions.HighThroughputDefaults;
        resharpOpts.IgnoreCase = bench.CaseInsensitive;
        resharp = new Resharp.Regex(bench.Pattern, resharpOpts);
    }

    [Benchmark]
    public int Compiled() => compiled.Count(haystack);

    [Benchmark]
    public int SourceGenerated() => sourceGenerated.Count(haystack);

    [Benchmark(Baseline = true)]
    public int Resharp() => resharp.Count(haystack);
}
