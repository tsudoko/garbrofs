namespace Util

module LittleEndian =
    open System

    let wu16(i, b: Span<byte>) =
        b.[0] <- byte (i &&& 0xffus)
        b.[1] <- byte (i >>> 8 &&& 0xffus)
    let wu32(i, b: Span<byte>) =
        b.[0] <- byte (i &&& 0xffu)
        b.[1] <- byte (i >>> 8 &&& 0xffu)
        b.[2] <- byte (i >>> 16 &&& 0xffu)
        b.[3] <- byte (i >>> 24 &&& 0xffu)
    let wu64(i, b: Span<byte>) =
        b.[0] <- byte (i &&& 0xffUL)
        b.[1] <- byte (i >>> 8 &&& 0xffUL)
        b.[2] <- byte (i >>> 16 &&& 0xffUL)
        b.[3] <- byte (i >>> 24 &&& 0xffUL)
        b.[4] <- byte (i >>> 32 &&& 0xffUL)
        b.[5] <- byte (i >>> 40 &&& 0xffUL)
        b.[6] <- byte (i >>> 48 &&& 0xffUL)
        b.[7] <- byte (i >>> 56 &&& 0xffUL)
    let ru16(b: ReadOnlySpan<byte>) =
        uint16 (b.[1] <<< 8 ||| b.[0])
    let ru32(b: ReadOnlySpan<byte>) =
        uint32 (b.[3] <<< 24 ||| b.[2] <<< 16 ||| b.[1] <<< 8 ||| b.[0])
    let ru64(b: ReadOnlySpan<byte>) =
        uint64 (b.[7] <<< 56 ||| b.[6] <<< 48 ||| b.[5] <<< 40 ||| b.[4] <<< 32 ||| b.[3] <<< 24 ||| b.[2] <<< 16 ||| b.[1] <<< 8 ||| b.[0])
