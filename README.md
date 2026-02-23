# RE#

A high-performance, automata based regex engine with first-class support for **intersection** and **complement** operations.

RE# compiles patterns into deterministic automata. All matching is non-backtracking with guaranteed linear-time execution. RE# extends `System.Text.RegularExpressions` syntax with intersection (`&`), complement (`~`), and a universal wildcard (`_`), enabling patterns that are impossible or impractical to express with standard regex.

[web playground](https://ieviev.github.io/resharp-webapp/) | [paper](https://dl.acm.org/doi/10.1145/3704837) | [blog post](https://iev.ee/blog/resharp-how-we-built-the-fastest-regex-in-fsharp/)

## Install

```
dotnet add package Resharp
```

## Usage

```csharp
// contains "cat", "dog", AND is 8-15 characters long
Resharp.Regex(@".*cat.*&.*dog.*&.{8,15}")
    .Matches("the cat and the dog");
```

## Syntax extensions

RE# supports standard .NET regex syntax plus three extensions:

### `_` -- universal wildcard

Matches any character including newlines (`[\s\S]`).

### `&` -- intersection

Both sides must match. Intersection has higher precedence than alternation.

```
_*cat_*&_*dog_*       contains both "cat" and "dog"
_*cat_*&_*dog_*&_{5,30}  ...and is 5-30 characters long
```

### `~(...)` -- complement

Matches everything the inner pattern does not match.

```
~(_*\d\d_*)     does not contain two consecutive digits
~(.*\n\n.*)     does not contain a double newline
```

### Combining operators

```
F.*&~(.*Finn)                starts with 'F', does not end with "Finn"
~(_*\d\d_*)&[a-zA-Z\d]{8,}  8+ alphanumeric, no consecutive digits
```

## Performance

RE# uses several optimizations: start-set inference, literal prefix scanning, and optional full DFA precompilation. RE# shares many optimizations with .NET's (and `RegexOptions.NonBacktracking` even shares some RE# techniques and strengths that many do not know of!) but RE# is designed from the ground up and returns a different kind of matches (leftmost-longest).

RE# particularly excels with large patterns and will often outperform .NET's regex engine (and all others) for complex patterns, especially those with a large set of alternatives, loops or using context-awareness - [RE# supports lookarounds](./docs/syntax.md#lookarounds), which is unique among automata engines.

To illustrate, here is a little comparison of RE# with .NET's most used compiled and source-generated regex engines on these patterns, you can also find wider comparisons [in the paper](https://dl.acm.org/doi/10.1145/3704837):

On [curated benchmarks from rebar](https://github.com/BurntSushi/rebar) (AMD Ryzen 7 5800X, .NET 10.0):

| Pattern | RE# | .NET Compiled | .NET SourceGenerated | Speedup |
|---------|----:|-----:|-----:|--------:|
| date validation | 1,737 us | 273,822 us | 318,070 us | 158x |
| dictionary search | 105 us | 45,832 us | 26,410 us | 252x |

And on some extensions we added ourselves:

| Pattern | RE# | .NET Compiled | .NET SourceGenerated | Speedup |
|---------|----:|-----:|-----:|--------:|
| dictionary, case-insensitive | 576 us | 29,368 us | 21,146 us | 37x |
| unicode dictionary | 336 us | 62,053 us | 38,613 us | 115x |
| unicode dictionary, case-insensitive | 321 us | 484,135 us | 537,814 us | 1,508x |
| dictionary + context window | 621 us | 48,893 us | 55,383 us | 79x |
| dictionary + context window, unicode | 692 us | 24,105,091 us | 34,706,982 us | 34,833x |

<details>
<summary>Where is RegexOptions.NonBacktracking?</summary>

Conveniently left out for shock effect `:^)`. `NonBacktracking` is actually much closer to RE#, but still behind. See the [paper](https://dl.acm.org/doi/10.1145/3704837) for a fairer comparison.

</details>

For critical paths, you can use `ValueMatches` for memory-pooled matching and `ResharpOptions.HighThroughputDefaults` for more aggressive optimization.

```fsharp
let re = Resharp.Regex("pattern", ResharpOptions.HighThroughputDefaults) 
// note: Regex instances are not thread-safe
use slices = re.ValueMatches(chars) // <- also dispose this after
for s in slices do
    printfn $"match at {s.Index}..{s.Index + s.Length}"
```

## Documentation

- [Syntax reference](docs/syntax.md) -- full pattern syntax including `&`, `~`, `_`
- [API reference](docs/api.md) -- all public types and methods

## Examples

Runnable scripts in [`examples/`](examples/):

| File | Description |
|------|-------------|
| [basic-syntax.fsx](examples/basic-syntax.fsx) | wildcards, intersection, complement |
| [paragraph.fsx](examples/paragraph.fsx) | paragraph extraction with complement and intersection |
| [validation.fsx](examples/validation.fsx) | date, IP, password validation with intersection |
| [replace.fsx](examples/replace.fsx) | string and function-based replacement |
| [lookaround.fsx](examples/lookaround.fsx) | lookahead, lookbehind, combined with intersection |
| [high-throughput.fsx](examples/high-throughput.fsx) | zero-allocation matching for large inputs |
| [Basic.cs](examples/Basic.cs) | C# usage |



Have fun!
