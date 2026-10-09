package kyo

import AllowUnsafe.embrace.danger

/** `unsafeCurrent`, the read without a suspension.
  *
  * It answers only where the read is a field read behind pure projections, and then with the value `current` gives at the same point;
  * everywhere else it answers `Absent` and the caller reads through `current`.
  */
class SignalUnsafeCurrentTest extends kyo.test.Test[Any]:

    "a ref, a constant and pure projections of them answer with the current value" in {
        for
            ref <- Signal.initRef(1)
            chain = ref.readOnly.map(_ * 10).map(_ + 1)
            _          <- ref.set(2)
            fromRef    <- Sync.defer(ref.unsafeCurrent())
            fromChain  <- Sync.defer(chain.unsafeCurrent())
            chainValue <- chain.current
            fromConst  <- Sync.defer(Signal.initConst("c").map(_.toUpperCase).unsafeCurrent())
            fromChange <- Sync.defer(ref.changesTo("handle").unsafeCurrent())
        yield assert(
            fromRef == Present(2) && fromChain == Present(21) && fromChain == Present(chainValue) &&
                fromConst == Present("C") && fromChange == Present("handle")
        )
    }

    "a combinator or a raw signal answers Absent" in {
        for
            a <- Signal.initRef(1)
            b <- Signal.initRef(2)
            combined = a.zip(b)
            raw      = Signal.initRaw[Int](
                currentWith = [C, S] => f => f(3),
                nextWith = [C, S] => f => f(4)
            )
            fromZip <- Sync.defer(combined.unsafeCurrent())
            fromRaw <- Sync.defer(raw.map(_ + 1).unsafeCurrent())
        yield assert(fromZip == Absent && fromRaw == Absent)
    }

end SignalUnsafeCurrentTest
