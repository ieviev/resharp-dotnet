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
    private Resharp.Regex searchValues = null!;
    private Resharp.Regex adaptive16 = null!;
    private string haystack = "";

    [Params(4, 8, 12, 16, 24, 32)]
    public int Gap { get; set; }

    [GlobalSetup]
    public void Setup()
    {
        const string pattern = "a[^b]*b";
        const int targetChars = 1 << 20;

        var segment = "a" + new string('x', Gap) + "b ";
        int repeats = Math.Max(1, targetChars / segment.Length);
        haystack = string.Concat(Enumerable.Repeat(segment, repeats));

        scalar = new Resharp.Regex(pattern, CreateOptions(searchValues: false, adaptiveThreshold: 0));
        searchValues = new Resharp.Regex(pattern, CreateOptions(searchValues: true, adaptiveThreshold: 0));
        adaptive16 = new Resharp.Regex(pattern, CreateOptions(searchValues: true, adaptiveThreshold: 16));

        ValidateRegex(scalar);
        ValidateRegex(searchValues);
        ValidateRegex(adaptive16);

        int expected = scalar.Count(haystack);
        ValidateCount("SearchValues", expected, searchValues.Count(haystack));
        ValidateCount("Adaptive16", expected, adaptive16.Count(haystack));

        Console.WriteLine(
            $"setlookup-synthetic gap={Gap} chars={haystack.Length} matches={expected}");
    }

    [Benchmark(Baseline = true)]
    [BenchmarkCategory("SetLookupSynthetic")]
    public int Scalar() => scalar.Count(haystack);

    [Benchmark]
    [BenchmarkCategory("SetLookupSynthetic")]
    public int SearchValues() => searchValues.Count(haystack);

    [Benchmark]
    [BenchmarkCategory("SetLookupSynthetic")]
    public int Adaptive16() => adaptive16.Count(haystack);

    private static void ValidateRegex(Resharp.Regex regex)
    {
        if (!regex.UsesSetLookup)
            throw new InvalidOperationException("synthetic workload must use LengthLookup.SetLookup");
        if (regex.SetLookupSearchMode != 1)
            throw new InvalidOperationException(
                $"expected direct SearchValues mode, got {regex.SetLookupSearchMode}");
        if (!regex.ValidateSetLookupSearchValues())
            throw new InvalidOperationException(
                "SetLookup SearchValues does not exactly match its minterm");
    }

    private static void ValidateCount(string strategy, int expected, int actual)
    {
        if (expected != actual)
            throw new InvalidOperationException(
                $"count mismatch for {strategy}: scalar={expected}, candidate={actual}");
    }

    private static ResharpOptions CreateOptions(bool searchValues, int adaptiveThreshold)
    {
        var options = ResharpOptions.HighThroughputDefaults;
        options.UseSearchValuesSetLookup = searchValues;
        options.SetLookupAdaptiveThreshold = adaptiveThreshold;
        return options;
    }
}

[SimpleJob(launchCount: 5, warmupCount: 12, iterationCount: 20)]
[Config(typeof(BenchConfig))]
[GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)]
[CategoriesColumn]
public class SetLookupRealisticBench
{
    private Resharp.Regex scalar = null!;
    private Resharp.Regex searchValues = null!;
    private Resharp.Regex adaptive16 = null!;
    private string haystack = "";

    [Params("user-8", "token-16", "quoted-32", "message-64", "path-256")]
    public string Case { get; set; } = "";

    [GlobalSetup]
    public void Setup()
    {
        const int targetChars = 1 << 20;
        var (pattern, record) = BuildCase(Case);
        int repeats = Math.Max(1, targetChars / record.Length);
        haystack = string.Concat(Enumerable.Repeat(record, repeats));

        scalar = new Resharp.Regex(pattern, CreateOptions(searchValues: false, adaptiveThreshold: 0));
        searchValues = new Resharp.Regex(pattern, CreateOptions(searchValues: true, adaptiveThreshold: 0));
        adaptive16 = new Resharp.Regex(pattern, CreateOptions(searchValues: true, adaptiveThreshold: 16));

        ValidateRegex(scalar);
        ValidateRegex(searchValues);
        ValidateRegex(adaptive16);

        int expected = scalar.Count(haystack);
        ValidateCount("SearchValues", expected, searchValues.Count(haystack));
        ValidateCount("Adaptive16", expected, adaptive16.Count(haystack));

        Console.WriteLine(
            $"setlookup-realistic case={Case} chars={haystack.Length} matches={expected}");
    }

    [Benchmark(Baseline = true)]
    [BenchmarkCategory("SetLookupRealistic")]
    public int Scalar() => scalar.Count(haystack);

    [Benchmark]
    [BenchmarkCategory("SetLookupRealistic")]
    public int SearchValues() => searchValues.Count(haystack);

    [Benchmark]
    [BenchmarkCategory("SetLookupRealistic")]
    public int Adaptive16() => adaptive16.Count(haystack);

    private static void ValidateRegex(Resharp.Regex regex)
    {
        if (!regex.UsesSetLookup)
            throw new InvalidOperationException("realistic workload must use LengthLookup.SetLookup");
        if (regex.SetLookupSearchMode != 1)
            throw new InvalidOperationException(
                $"expected direct SearchValues mode, got {regex.SetLookupSearchMode}");
        if (!regex.ValidateSetLookupSearchValues())
            throw new InvalidOperationException(
                "SetLookup SearchValues does not exactly match its minterm");
    }

    private static void ValidateCount(string strategy, int expected, int actual)
    {
        if (expected != actual)
            throw new InvalidOperationException(
                $"count mismatch for {strategy}: scalar={expected}, candidate={actual}");
    }

    private static (string Pattern, string Record) BuildCase(string name) => name switch
    {
        "user-8" => (
            "user=[^ ]* ",
            $"ts=2026-10-01T17:00:00Z level=info user={new string('u', 8)} host=node01 "),
        "token-16" => (
            "token=[^;]*;",
            $"event=auth;token={new string('t', 16)};result=ok;source=10.0.0.1;"),
        "quoted-32" => (
            "\"[^\"]*\"",
            $"level=info message=\"{new string('q', 32)}\" component=worker "),
        "message-64" => (
            "msg=[^;]*;",
            $"ts=1;level=warning;msg={new string('m', 64)};host=node01;pid=1234;"),
        "path-256" => (
            "path=[^\\n]*\\n",
            $"ts=1 level=info path=/var/log/{new string('p', 247)}\n"),
        _ => throw new ArgumentOutOfRangeException(nameof(name), name, null)
    };

    private static ResharpOptions CreateOptions(bool searchValues, int adaptiveThreshold)
    {
        var options = ResharpOptions.HighThroughputDefaults;
        options.UseSearchValuesSetLookup = searchValues;
        options.SetLookupAdaptiveThreshold = adaptiveThreshold;
        return options;
    }
}
