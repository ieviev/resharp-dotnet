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

RE# uses several optimizations: start-set inference, literal prefix scanning, and optional full DFA precompilation. RE# shares many optimizations with .NET's (and `.NonBacktracking` even shares some RE# techniques and strengths) but RE# is designed from the ground up and returns a different kind of matches (leftmost-longest).

RE# particularly excels with large patterns and will often outperform .NET's regex engine (and all others) for complex patterns, especially those with a large set of literals or complex intersection and complement operations

Here is a little comparison of RE# with .NET's compiled regex engine on these patterns, you can also find wider comparisons [in the paper](https://dl.acm.org/doi/10.1145/3704837):

On [curated benchmarks from rebar](https://github.com/BurntSushi/rebar) (AMD Ryzen 7 5800X, .NET 10.0):

| Pattern | RE# | .NET Compiled | Speedup |
|---------|----:|-----:|--------:|
| date validation | 1,698 us | 261,957 us | 154x |
| dictionary search | 412 us | 14,726 us | 36x |


| Pattern | RE# | .NET Compiled | Speedup |
|---------|----:|-----:|--------:|
| dictionary, case-insensitive unicode | 603 us | 29,551 us | 49x |
| dictionary, case-insensitive | 631 us | 49,685 us | 79x |
| unicode dictionary | 326 us | 61,853 us | 190x |
| unicode dictionary, case-insensitive | 331 us | 483,251 us | 1,460x |
| dictionary + context window | 680 us | 24,470,445 us | 35,963x |

For critical paths, you can use `ValueMatches` for memory-pooled matching and `ResharpOptions.HighThroughputDefaults` for more aggressive optimization.

```fsharp
let re = Resharp.Regex("pattern", ResharpOptions.HighThroughputDefaults)
use slices = re.ValueMatches(chars) // <- dispose this after
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
