# SearchValues-backed LengthLookup.SetLookup

## Problem

`LengthLookup.SetLookup` is selected when the optimizer proves that, after a fixed
prefix, the match end is determined by the next character in one exact minterm.

The existing terminal path searches for that minterm one UTF-16 code unit at a time:

```text
char -> _mtlookup[char] -> compare mtId -> branch -> repeat
```

The optimizer already carries a `MintermSearchValues` field for `SetLookup`, but
execution does not use it.

## Correctness prerequisite

The previous `sv` stored in `SetLookup` came from the nullable derivative state's
start set. That set is useful for DFA skipping, but it is not the right invariant for
a direct replacement of the exact `mtId` scan.

This branch changes construction so `SetLookup` retains:

```fsharp
c.MintermSearchValues(mt)
```

where `mt` has already passed `isExactMinterm`.

Therefore the stored `SearchValues` denotes exactly the character set represented by
the `mtId` used by the scalar loop. The representation may be direct
(`IndexOfAny`) or inverted (`IndexOfAnyExcept`), but membership is equivalent.

An internal validation helper exhaustively checks all 65,536 UTF-16 code units:

```text
SearchValues.contains(c) == (_mtlookup[c] == mtId)
```

for every `SetLookup` regex used by tests and benchmarks.

## Algorithm

The scalar implementation remains available as the A/B baseline.

Scalar:

```text
while pos < input.Length && minterm(input[pos]) != mtId:
    pos++
```

Candidate:

```text
offset = SearchValues.nextIndexLeftToRight(input[pos..])
pos = offset < 0 ? input.Length : pos + offset
```

All later match-end arithmetic and non-overlap handling are unchanged.

The strategy switch occurs once in the `LengthLookup` dispatch, not inside the scan.

## Complexity

If the next target minterm is `d` code units away:

- scalar path performs `O(d)` dependent loads/comparisons/branches;
- SearchValues performs one bulk search over the same span using the runtime's
  optimized search implementation.

Both return the first matching position, so the optimization changes execution cost,
not regex semantics.

## Benchmark plan

Two same-run A/B suites are used.

### Synthetic

Pattern:

```regex
a[^b]*b
```

The gap before `b` is varied across 4, 16, 64, 256, and 1024 characters while the
total haystack remains approximately 1 MiB.

Configuration:

- 5 launches;
- 12 warmup iterations;
- 20 measured iterations.

This directly measures how the crossover changes with search distance.

### Rebar

The benchmark discovers all count-model Rebar workloads that actually compile to
`LengthLookup.SetLookup`; non-eligible cases are excluded rather than diluting the
result.

Configuration:

- 3 launches;
- 8 warmup iterations;
- 12 measured iterations.

For every case, setup verifies:

- both variants select `SetLookup`;
- exhaustive SearchValues/minterm equivalence;
- identical match counts.

## Evaluation

Primary metric:

```text
SearchValues / Scalar
```

The candidate should show increasing benefit as scan distance grows and should not
produce a meaningful regression at short distances.

A result is not considered representative unless the corresponding workload is
confirmed to use `LengthLookup.SetLookup`.
