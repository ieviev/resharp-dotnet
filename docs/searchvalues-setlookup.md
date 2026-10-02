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

Pure SearchValues:

```text
offset = SearchValues.nextIndexLeftToRight(input[pos..])
pos = offset < 0 ? input.Length : pos + offset
```

The scalar-prefix hybrid was benchmarked with 4- and 8-character probes. It did not
improve the trade-off: the probe cost is paid for every match, so it was generally
slower than pure SearchValues on medium/long fields and did not reliably beat scalar
on short fields.

The current candidate therefore adapts once per matching operation from observed
input behavior:

```text
useSearchValues = false

for each accepted candidate:
    if useSearchValues:
        SearchValues(input[pos..])
    else:
        start = pos
        scalar minterm scan
        if pos - start >= 16:
            useSearchValues = true
```

The first sufficiently long scan pays the original scalar cost and acts as a sample.
Subsequent SetLookup searches in the same operation use SearchValues. Short-field
inputs therefore remain scalar without paying SearchValues setup on every match.

The threshold of 16 is deliberately conservative: direct SearchValues was clearly
faster at that distance on both measured runners, while shorter-distance behavior was
architecture-dependent.

Inverted `IndexOfAnyExcept` minterms remain on the scalar path because their measured
benefit was smaller and inconsistent.

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

The first run established the broad crossover: SearchValues loses at a 4-character
scan, wins at 16, and becomes dramatically faster at 64+.

The follow-up narrowed the sweep to 4, 8, 12, 16, 24, and 32 characters and tested
both direct and inverted representations.

That run established:

- direct `IndexOfAny` scales strongly with distance and wins clearly from 8+;
- inverted `IndexOfAnyExcept` is much less consistent and is not retained as a
  candidate strategy.

The scalar-prefix follow-up compared scalar, pure SearchValues, Hybrid4, and Hybrid8.
Both hybrids were rejected because they add repeated scalar work before almost every
vector search.

The next benchmark keeps only direct `IndexOfAny` and compares:

- scalar;
- pure SearchValues;
- Adaptive16: start scalar and switch subsequent searches after observing a scan of
  at least 16 UTF-16 code units.

The total haystack remains approximately 1 MiB.

Configuration:

- 5 launches;
- 12 warmup iterations;
- 20 measured iterations.

This directly measures how the crossover changes with search distance.

### Representative delimiter workloads

The existing Rebar count corpus contains no workload that selects
`LengthLookup.SetLookup`, so it cannot provide representative evidence for this
optimization.

The follow-up adds delimiter-oriented cases shaped like common text and log parsing:

- an 8-character `user=` field terminated by space;
- a 16-character `token=` field terminated by semicolon;
- a 32-character quoted value;
- a 64-character `msg=` field terminated by semicolon;
- a 256-character `path=` field terminated by newline.

Each haystack is approximately 1 MiB and each case must select exact-minterm
`SetLookup`.

Configuration:

- 5 launches;
- 12 warmup iterations;
- 20 measured iterations.

For every case, setup verifies:

- both variants select `SetLookup`;
- the expected direct/inverted SearchValues mode;
- exhaustive SearchValues/minterm equivalence;
- identical match counts.

## Evaluation

Primary comparisons:

```text
SearchValues / Scalar
Adaptive16 / Scalar
```

The adaptive candidate should track scalar behavior for consistently short fields and
approach pure SearchValues after the first long field. This uses observed delimiter
distance rather than a CPU-specific assumption.

Because the crossover differed between the Intel AVX-512/AVX10 and AMD AVX2 runners,
the 16-character threshold is chosen as the shortest distance that was clearly in the
SearchValues-favorable region on both measured systems.

A result is not considered representative unless the corresponding workload is
confirmed to use `LengthLookup.SetLookup`.
