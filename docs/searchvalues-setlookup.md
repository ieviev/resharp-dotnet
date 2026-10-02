# SearchValues-backed LengthLookup.SetLookup

## Decision

`LengthLookup.SetLookup` now uses a precomputed `SearchValues<char>` search when
the target exact minterm has a direct `SearchValues` representation. Inverted
representations retain the scalar minterm-table scan.

The experimental scalar-prefix hybrids and Adaptive16 strategy were benchmarked and
rejected. They added dispatch and repeated scalar work without improving the overall
trade-off enough to justify additional runtime policy.

## Correctness invariant

`SetLookup` is created only when the optimizer proves that the terminal predicate is
one exact minterm. Construction stores both its minterm id and:

```fsharp
c.MintermSearchValues(mt)
```

where `mt` has already passed `isExactMinterm`.

The scalar implementation searches for the first UTF-16 code unit whose minterm-table
entry equals `mtId`. The direct implementation searches for the first code unit
contained by the corresponding `SearchValues<char>`. These operations are equivalent
because both representations describe the same exact minterm.

An internal validation helper checks this invariant exhaustively over all 65,536
UTF-16 code units:

```text
SearchValues.contains(c) == (_mtlookup[c] == mtId)
```

The regression tests additionally cover ASCII and Unicode delimiters, literal
prefixes, immediate and final-position delimiters, missing delimiters, multiple
candidate starts, NUL and `U+FFFF`, and the inverted scalar fallback.

## Runtime algorithm

For a direct representation:

```text
offset = SearchValues.nextIndexLeftToRight(input[pos..])
pos = offset < 0 ? input.Length : pos + offset
```

For an inverted representation, the existing scalar lookup remains:

```text
while pos < input.Length && minterm(input[pos]) != mtId:
    pos++
```

`SearchValues<char>` is built while constructing the regex optimization state, not
inside the matching loop.

## Benchmark conclusion

The investigation used same-run BenchmarkDotNet comparisons over synthetic delimiter
distances and representative field-shaped workloads. Direct `IndexOfAny` scaled
well with delimiter distance and produced useful gains from medium fields onward.
The largest gains occurred on long fields. Very short fields were approximately
neutral.

The alternatives were rejected:

- scalar-prefix Hybrid4 and Hybrid8 paid repeated probe cost before vector searches;
- Adaptive16 recovered some large-gap performance but surrendered useful gains at
  8- and 12-code-unit distances;
- inverted `IndexOfAnyExcept` performance was less consistent across measured
  systems.

The final implementation therefore has no adaptive threshold and no runtime feature
switch. Direct exact-minterm representations use `SearchValues`; inverted
representations use the scalar fallback.

The temporary benchmark harness and branch-specific benchmark workflow were removed
after the implementation decision. The benchmark results remain the empirical basis
for this design.
