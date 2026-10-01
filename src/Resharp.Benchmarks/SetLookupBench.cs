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

    [Params("Direct", "Inverted")]
    public string Mode { get; set; } = "";

    [Params(4, 8, 12, 16, 24, 32)]
    public int Gap { get; set; }

    [GlobalSetup]
    public void Setup()
    {
        const int targetChars = 1 << 20;

        string pattern;
        string segment;
        int expectedMode;

        if (Mode == "Direct")
        {
            pattern = "a[^b]*b";
            segment = "a" + new string('x', Gap) + "b ";
            expectedMode = 1;
        }
        else
        {
            pattern = "bb*[^b]";
            segment = "b" + new string('b', Gap) + "x ";
            expectedMode = 2;
        }

        int repeats = Math.Max(1, targetChars / segment.Length);
        haystack = string.Concat(Enumerable.Repeat(segment, repeats));

        scalar = new Resharp.Regex(pattern, CreateOptions(vectorized: false));
        vectorized = new Resharp.Regex(pattern, CreateOptions(vectorized: true));

        ValidateSetup(expectedMode);

        int expected = scalar.Count(haystack);
        int actual = vectorized.Count(haystack);
        if (expected != actual)
            throw new InvalidOperationException(
                $"count mismatch: scalar={expected}, vectorized={actual}");

        Console.WriteLine(
            $"setlookup-synthetic mode={Mode} gap={Gap} chars={haystack.Length} matches={actual}");
    }

    [Benchmark(Baseline = true)]
    [BenchmarkCategory("SetLookupSynthetic")]
    public int Scalar() => scalar.Count(haystack);

    [Benchmark]
    [BenchmarkCategory("SetLookupSynthetic")]
    public int SearchValues() => vectorized.Count(haystack);

    private void ValidateSetup(int expectedMode)
    {
        if (!scalar.UsesSetLookup || !vectorized.UsesSetLookup)
            throw new InvalidOperationException("synthetic workload must use LengthLookup.SetLookup");
        if (vectorized.SetLookupSearchMode != expectedMode)
            throw new InvalidOperationException(
                $"unexpected SetLookup SearchValues mode: {vectorized.SetLookupSearchMode}, expected {expectedMode}");
        if (!vectorized.ValidateSetLookupSearchValues())
            throw new InvalidOperationException(
                "SetLookup SearchValues does not exactly match its minterm");
    }

    private static ResharpOptions CreateOptions(bool vectorized)
    {
        var options = ResharpOptions.HighThroughputDefaults;
        options.UseSearchValuesSetLookup = vectorized;
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
    private Resharp.Regex vectorized = null!;
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

        scalar = new Resharp.Regex(pattern, CreateOptions(vectorized: false));
        vectorized = new Resharp.Regex(pattern, CreateOptions(vectorized: true));

        if (!scalar.UsesSetLookup || !vectorized.UsesSetLookup)
            throw new InvalidOperationException($"realistic case '{Case}' must use LengthLookup.SetLookup");
        if (vectorized.SetLookupSearchMode != 1)
            throw new InvalidOperationException(
                $"realistic case '{Case}' expected direct SearchValues mode, got {vectorized.SetLookupSearchMode}");
        if (!vectorized.ValidateSetLookupSearchValues())
            throw new InvalidOperationException(
                $"SearchValues/minterm mismatch for realistic case '{Case}'");

        int expected = scalar.Count(haystack);
        int actual = vectorized.Count(haystack);
        if (expected != actual)
            throw new InvalidOperationException(
                $"count mismatch for '{Case}': scalar={expected}, vectorized={actual}");

        Console.WriteLine(
            $"setlookup-realistic case={Case} chars={haystack.Length} matches={actual}");
    }

    [Benchmark(Baseline = true)]
    [BenchmarkCategory("SetLookupRealistic")]
    public int Scalar() => scalar.Count(haystack);

    [Benchmark]
    [BenchmarkCategory("SetLookupRealistic")]
    public int SearchValues() => vectorized.Count(haystack);

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

    private static ResharpOptions CreateOptions(bool vectorized)
    {
        var options = ResharpOptions.HighThroughputDefaults;
        options.UseSearchValuesSetLookup = vectorized;
        return options;
    }
}
