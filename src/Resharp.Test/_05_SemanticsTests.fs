module Resharp.Test._05_SemanticsTests

#if DEBUG
open System
open System.Numerics
open Common

[<Xunit.Fact>]
let ``mintermsLog`` () =
    let logceil (n: int) =
        let newv = (int (BitConverter.DoubleToInt64Bits(float n) >>> 52) &&& 0x7ff) - 1022
        if BitOperations.IsPow2(n) then newv - 1 else newv

    let logfloor (n: int) =
        if BitOperations.IsPow2(n) then
            (BitOperations.Log2(uint32 (n)))
        else
            (BitOperations.Log2(uint32 (n))) + 1

    let eq n = assertEqual (logceil n) (logfloor n)
    eq 7
    eq 8
    eq 15
    eq 16
    eq 31
    eq 32
    eq 33

#endif
