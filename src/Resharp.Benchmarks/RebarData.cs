using System.Text;
using Tommy;
using SysRegex = System.Text.RegularExpressions.Regex;

namespace Resharp.Benchmarks;

public record BenchDef(
    string Group,
    string Name,
    string Pattern,
    string Haystack,
    bool CaseInsensitive,
    string Model
);

public static class RebarData
{
    static readonly Lazy<string> BenchDir = new(() =>
    {
        var dir = Directory.GetCurrentDirectory();
        while (true)
        {
            var candidate = Path.Combine(dir, "src", "Resharp.Benchmarks", "benchmarks");
            if (Directory.Exists(candidate)) return candidate;
            var parent = Directory.GetParent(dir);
            if (parent is null) throw new Exception("could not find benchmarks dir");
            dir = parent.FullName;
        }
    });

    static string HaystacksDir => Path.Combine(BenchDir.Value, "haystacks");
    static string RegexesDir => Path.Combine(BenchDir.Value, "regexes");
    static string DefsDir => Path.Combine(BenchDir.Value, "definitions");

    public static string? NameFilter { get; set; }
    public static HashSet<string>? NameSet { get; set; }

    public static readonly Lazy<BenchDef[]> AllBenches = new(() =>
        [.. LoadGroup("curated"), .. LoadGroup("resharp")]
    );

    public static readonly Lazy<Dictionary<string, BenchDef>> BenchMap = new(() =>
        AllBenches.Value.ToDictionary(b => $"{b.Group}/{b.Name}")
    );

    public static readonly Lazy<string[]> BenchNames = new(() =>
        AllBenches.Value.Select(b => $"{b.Group}/{b.Name}").ToArray()
    );

    static string[] ReadNonEmptyLines(string path) =>
        File.ReadAllLines(path).Where(l => l.Length > 0).ToArray();

    static string LoadHaystack(TomlNode node)
    {
        if (node is TomlString s) return s.Value;
        if (node is not TomlTable t) throw new Exception("unexpected haystack type");

        string text;
        if (t.HasKey("path"))
            text = File.ReadAllText(Path.Combine(HaystacksDir, t["path"].AsString.Value), Encoding.UTF8);
        else if (t.HasKey("contents"))
            text = t["contents"].AsString.Value;
        else
            throw new Exception("haystack must have 'path' or 'contents'");

        if (t.HasKey("line-start") || t.HasKey("line-end"))
        {
            var lines = text.Split('\n');
            int start = t.HasKey("line-start") ? (int)t["line-start"].AsInteger.Value : 0;
            int end = t.HasKey("line-end") ? (int)t["line-end"].AsInteger.Value : lines.Length;
            text = string.Join("\n", lines[start..end]);
        }

        if (t.HasKey("repeat"))
        {
            int n = (int)t["repeat"].AsInteger.Value;
            var sb = new StringBuilder(text.Length * n);
            for (int i = 0; i < n; i++) sb.Append(text);
            text = sb.ToString();
        }

        return text;
    }

    static string LoadRegex(TomlNode node)
    {
        if (node is TomlString s) return s.Value;
        if (node is not TomlTable t) throw new Exception("unexpected regex type");

        var path = Path.Combine(RegexesDir, t["path"].AsString.Value);
        bool isLiteral = t.HasKey("literal") && t["literal"].AsBoolean.Value;
        string perLine = t.HasKey("per-line") ? t["per-line"].AsString.Value : "";

        if (isLiteral && perLine == "alternate")
        {
            var lines = ReadNonEmptyLines(path);
            var escaped = lines.Select(SysRegex.Escape);
            return string.Join("|", escaped);
        }

        return File.ReadAllText(path).Trim();
    }

    static bool HasEngine(TomlArray engines, string name) =>
        engines.Children.Any(e =>
        {
            var v = e.AsString.Value;
            return !v.StartsWith('#') && v == name;
        });

    static BenchDef[] ParseToml(string group, string path)
    {
        using var fs = File.OpenRead(path);
        using var sr = new StreamReader(fs);
        var tb = TOML.Parse(sr);

        if (!tb.HasKey("bench")) return [];

        var results = new List<BenchDef>();
        foreach (var bench in tb["bench"].Children)
        {
            var model = bench["model"].AsString.Value;
            if (model == "compile" || !bench.HasKey("regex")) continue;

            var regexNode = bench["regex"];
            if (regexNode is TomlTable rt && rt.HasKey("per-line") && rt["per-line"].AsString.Value == "pattern")
                continue;

            var engines = bench["engines"].AsArray;
            if (HasEngine(engines, "dotnet/compiled") && HasEngine(engines, "resharp"))
            {
                results.Add(new BenchDef(
                    group,
                    bench["name"].AsString.Value,
                    LoadRegex(regexNode),
                    LoadHaystack(bench["haystack"]),
                    bench.HasKey("case-insensitive") && bench["case-insensitive"].AsBoolean.Value,
                    model
                ));
            }
        }

        return [.. results];
    }

    static BenchDef[] LoadGroup(string subdir)
    {
        var dir = Path.Combine(DefsDir, subdir);
        if (!Directory.Exists(dir)) return [];

        return Directory.GetFiles(dir, "*.toml")
            .SelectMany(path =>
            {
                var file = Path.GetFileNameWithoutExtension(path);
                var group = $"{subdir}/{file}";
                try { return ParseToml(group, path); }
                catch (Exception ex)
                {
                    Console.Error.WriteLine($"warning: skipping {group}: {ex.Message}");
                    return [];
                }
            })
            .ToArray();
    }
}
