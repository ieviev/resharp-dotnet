module internal Resharp.Accelerators

open Resharp.Cache
open System

let trySkipToWeightedSetCharRev
    (
        input: ReadOnlySpan<char>,
        pos: int,
        _weightedSets: struct (int * MintermSearchValues<'t>) array
    ) : int voption =
    let textSpan = input
    let currentPosition = pos
    let charSetsCount = _weightedSets.Length
    let wsetspan = _weightedSets.AsSpan()
    let struct (rarestCharSetIndex, rarestCharSet) = wsetspan[0]
    let mutable searching = true
    let mutable prevMatch = currentPosition
    let rarestOffset = _weightedSets.Length - rarestCharSetIndex
    let mutable resultEnd = ValueNone

    while searching do
        if prevMatch < rarestOffset then
            searching <- false
            resultEnd <- ValueNone
        else
            let slice = textSpan.Slice(0, prevMatch - rarestOffset + 1)
            let sharedIndex = MintermSearchValues.nextIndexRightToLeft (rarestCharSet, slice)
            prevMatch <- sharedIndex

            match sharedIndex with
            | n when n - rarestCharSetIndex < 0 ->
                searching <- false
                resultEnd <- ValueNone
            | curMatch ->
                let absMatchStart = curMatch - rarestCharSetIndex
                let mutable fullMatch = true
                let mutable i = 1

                while fullMatch && i < charSetsCount do
                    let struct (weightedSetIndex, weightedSet) = wsetspan[i]

                    if
                        not (
                            MintermSearchValues.contains (
                                weightedSet,
                                textSpan[absMatchStart + weightedSetIndex]
                            )
                        )
                    then
                        fullMatch <- false

                    i <- i + 1

                if fullMatch && i = charSetsCount then
                    searching <- false
                    resultEnd <- ValueSome(absMatchStart + _weightedSets.Length)
                else
                    prevMatch <- curMatch + rarestOffset - 1

    resultEnd
