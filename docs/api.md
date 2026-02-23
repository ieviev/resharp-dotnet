# RE# API Reference

## `Resharp.Regex`

The main entry point. Compiles a pattern into a DFA-based matcher.

```csharp
// C#
var re = new Resharp.Regex(@"\w+");
var re = new Resharp.Regex(@"\w+", new ResharpOptions { IgnoreCase = true });
```

```fsharp
// F#
let re = Resharp.Regex(@"\w+")
let re = Resharp.Regex(@"\w+", ResharpOptions(IgnoreCase = true))
```

> **Building the engine is the expensive part.** Reuse the `Regex` instance across calls.

> **Thread safety:** A `Regex` instance is **not** thread-safe. The lazy DFA mutates internal state during matching. If you need to use the same pattern from multiple threads, create a separate `Regex` instance per thread — or fully compile the DFA to make it safe (see `IsFullDFA`).

### Syntax and supported features

Some features of the default .NET regex syntax are not supported. RE# is an automata engine and does not support many features of the .NET regex engine. Lookahead and lookbehind are supported but with some limitations (see [syntax reference](docs/syntax.md)).

### Methods

#### `IsMatch(input) -> bool`

Returns whether the pattern matches anywhere in the input.

For full-string matching, anchor your pattern: `\A(pattern)\z`.

```csharp
var re = new Resharp.Regex(@"\d+");
re.IsMatch("abc123");  // true
re.IsMatch("abcdef");  // false
```

#### `Count(input) -> int`

Counts non-overlapping matches.

```csharp
var re = new Resharp.Regex(@"\w+");
re.Count("one two three");  // 3
```

#### `Matches(input) -> MatchResult[]`

Returns all non-overlapping matches with text, index, and length.

```csharp
var re = new Resharp.Regex(@"\d+");
foreach (var m in re.Matches("a1b22c333"))
    Console.WriteLine($"{m.Value} at {m.Index}");
// 1 at 1
// 22 at 3
// 333 at 6
```

#### `ValueMatches(input) -> ValueList<ValueMatch>`

Zero-allocation version of `Matches`. Returns index/length pairs without creating strings. **The result must be disposed.**

```fsharp
let re = Resharp.Regex(@"\w+")
use slices = re.ValueMatches("one two three")
for s in slices do
    printfn $"{s.Index}..{s.Index + s.Length}"
```

#### `Replace(input, replacement) -> string`

Replaces all matches with a string.

```csharp
var re = new Resharp.Regex(@"\d+");
re.Replace("a1b2c3", "X");  // "aXbXcX"
```

#### `Replace(input, func) -> string`

Replaces all matches using a transformation function.

```csharp
var re = new Resharp.Regex(@"\d+");
re.Replace("a1b2c3", m => $"[{m}]");  // "a[1]b[2]c[3]"
```

#### `FirstEnd(input) -> int`

Finds the end index of the shortest match from position 0 (anchored at `\A`). Returns `-1` if no match.

```fsharp
let re = Resharp.Regex(@"a+")
re.FirstEnd("aaab")  // 1
```

#### `LongestEnd(input) -> int`

Finds the end index of the longest match from position 0 (anchored at `\A`). Returns `-1` if no match.

```fsharp
let re = Resharp.Regex(@"a+")
re.LongestEnd("aaab")  // 3
```

### Properties

#### `IsFullDFA -> bool`

Whether the pattern was fully compiled to a DFA at construction time. Larger patterns use lazy DFA construction instead. A full DFA is thread safe, a lazy one is not. Raise `ResharpOptions.DfaThreshold` to fully compile larger patterns.

Note that RE# uses infinite automata, so certain patterns can not be fully compiled to a DFA regardless of size.

---

## `ResharpOptions`

Configuration passed to the `Regex` constructor.
Most of these you can ignore and just use the presets below.

| Property                     | Default   | Description                                                    |
|------------------------------|-----------|----------------------------------------------------------------|
| `InitialDfaCapacity`         | 2048      | Initial DFA state array size                                   |
| `MaxDfaCapacity`             | 100,000   | Hard limit on DFA states; throws if exceeded                   |
| `MinimizePattern`            | true      | Minimize alternations at the cost of build time                |
| `MaxPrefixLength`            | 20        | Max string literal prefix length for optimizations             |
| `FindLookaroundPrefix`       | true      | Optimize lookaround prefixes (expensive with unbounded)        |
| `FindPotentialStartSizeLimit`| 200       | Start-set size limit; higher = faster search, slower build     |
| `UseDotnetUnicode`           | true      | Full .NET unicode (Kelvin K, Turkish I, etc.)                  |
| `IgnoreCase`                 | false     | Case-insensitive matching                                      |
| `StartsetInferenceLimit`     | 2000      | Start-set inference limit                                      |
| `DfaThreshold`               | 10        | State count threshold for full DFA compilation                 |

### Presets

#### `ResharpOptions.HighThroughputDefaults`

Spends more time building the engine for faster matching at runtime. Good for patterns that will be used many times on large inputs.

```csharp
var re = new Resharp.Regex(pattern, ResharpOptions.HighThroughputDefaults);
```

#### `ResharpOptions.SingleUseDefaults`

Skips expensive optimizations. Use when the pattern will be used once and discarded like in a search box.

```csharp
var re = new Resharp.Regex(pattern, ResharpOptions.SingleUseDefaults);
```

---

## Result types

### `MatchResult`

Returned by `Matches()`. Readonly struct.

| Field    | Type     | Description          |
|----------|----------|----------------------|
| `Value`  | `string` | The matched text     |
| `Index`  | `int`    | Start position       |
| `Length`  | `int`    | Match length         |

### `ValueMatch`

Returned by `ValueMatches()`. Readonly struct. Does not allocate a string.

| Field    | Type  | Description    |
|----------|-------|----------------|
| `Index`  | `int` | Start position |
| `Length`  | `int` | Match length   |

