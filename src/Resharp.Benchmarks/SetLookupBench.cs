using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Jobs;

namespace Resharp.Benchmarks;

[SimpleJob(launchCount: 5, warmupCount: 12, iterationCount: 20)]
[Config(typeof(BenchConfig))]
[GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)]
[CategoriesColumn]
public class SetLookupSyntheticBench
{
    private Resharp.Regex scalar = null!;
    private Resharp.Regex vectorized = null!;
    private string haystack = "";

    [Params(4, 16, 64, 256, 1024)]
    public int Gap { get; set; }

    [GlobalSetup]
    public void Setup()
    {
        const string pattern = "a[^b]*b";
        const int targetChars = 1 << 20;

        var segment = "a" + new string('x', Gap) + "b ";
        int repeats = Math.Max(1, targetChars / segment.Length);
        haystack = string.Concat(Enumerable.Repeat(segment, repeats));

        scalar = new Resharp.Regex(pattern, CreateOptions(vectorized: false));
        vectorized = new Resharp.Regex(pattern, CreateOptions(vectorized: true));

        if (!scalar.UsesSetLookup || !vectorized.UsesSetLookup)
            throw new InvalidOperationException("synthetic workload must use LengthLookup.SetLookup");
        if (!vectorized.ValidateSetLookupSearchValues())
            throw new InvalidOperationException("SetLookup SearchValues does not exactly match its minterm");

        int expected = scalar.Count(haystack);
        int actual = vectorized.Count(haystack);
        if (expected != actual)
            throw new InvalidOperationException($"count mismatch: scalar={expected}, vectorized={actual}");

        Console.WriteLine(
            $"setlookup-synthetic gap={Gap} chars={haystack.Length} matches={actual}");
    }

    [Benchmark(Baseline = true)]
    [BenchmarkCategory("SetLookupSynthetic")]
    public int Scalar() => scalar.Count(haystack);

    [Benchmark]
    [BenchmarkCategory("SetLookupSynthetic")]
    public int SearchValues() => vectorized.Count(haystack);

    private static ResharpOptions CreateOptions(bool vectorized)
    {
        var options = ResharpOptions.HighThroughputDefaults;
        options.UseSearchValuesSetLookup = vectorized;
        return options;
    }
}

[SimpleJob(launchCount: 3, warmupCount: 8, iterationCount: 12)]
[Config(typeof(BenchConfig))]
[GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)]
[CategoriesColumn]
public class SetLookupRebarBench
{
    private Resharp.Regex scalar = null!;
    private Resharp.Regex vectorized = null!;
    private string haystack = "";

    private static readonly Lazy<string[]> EligibleNames = new(() =>
        RebarData.AllBenches.Value
            .Where(b => b.Model == "count")
            .Select(b => (Bench: b, Name: $"{b.Group}/{b.Name}"))
            .Where(x => UsesSetLookup(x.Bench))
            .Select(x => x.Name)
            .Order()
            .ToArray());

    [ParamsSource(nameof(BenchNames))]
    public string Name { get; set; } = "";

    public IEnumerable<string> BenchNames => EligibleNames.Value;

    [GlobalSetup]
    public void Setup()
    {
        var bench = RebarData.BenchMap.Value[Name];
        haystack = bench.Haystack;

        scalar = new Resharp.Regex(
            bench.Pattern,
            CreateOptions(bench.CaseInsensitive, vectorized: false));
        vectorized = new Resharp.Regex(
            bench.Pattern,
            CreateOptions(bench.CaseInsensitive, vectorized: true));

        if (!scalar.UsesSetLookup || !vectorized.UsesSetLookup)
            throw new InvalidOperationException($"'{Name}' no longer selects SetLookup");
        if (!vectorized.ValidateSetLookupSearchValues())
            throw new InvalidOperationException($"SearchValues/minterm mismatch for '{Name}'");

        int expected = scalar.Count(haystack);
        int actual = vectorized.Count(haystack);
        if (expected != actual)
            throw new InvalidOperationException(
                $"Count mismatch for '{Name}': scalar={expected}, vectorized={actual}");

        Console.WriteLine(
            $"setlookup-rebar name={Name} chars={haystack.Length} matches={actual}");
    }

    [Benchmark(Baseline = true)]
    [BenchmarkCategory("SetLookupRebar")]
    public int Scalar() => scalar.Count(haystack);

    [Benchmark]
    [BenchmarkCategory("SetLookupRebar")]
    public int SearchValues() => vectorized.Count(haystack);

    private static bool UsesSetLookup(BenchDef bench)
    {
        var regex = new Resharp.Regex(
            bench.Pattern,
            CreateOptions(bench.CaseInsensitive, vectorized: true));
        return regex.UsesSetLookup;
    }

    private static ResharpOptions CreateOptions(bool ignoreCase, bool vectorized)
    {
        var options = ResharpOptions.HighThroughputDefaults;
        options.IgnoreCase = ignoreCase;
        options.UseSearchValuesSetLookup = vectorized;
        return options;
    }
}
