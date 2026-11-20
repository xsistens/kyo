package kyo

import AllowUnsafe.embrace.danger

/** `unsafeCurrent`, the read without a suspension.
  *
  * It answers only where the read is a field read behind pure projections, and then with the value `current` gives at the same point;
  * everywhere else it answers `Absent` and the caller reads through `current`. It reads with the locals of the fiber the pass runs for.
  */
class SignalUnsafeCurrentTest extends kyo.test.Test[Any]:

    /** The read a pass makes: with the locals of the fiber it runs for. */
    private def now[A](signal: Signal[A])(using Frame): Maybe[A] < Sync =
        Local.snapshotWith(locals => Sync.defer(signal.unsafeCurrent(locals)))

    "a ref, a constant and pure projections of them answer with the current value" in {
        for
            ref <- Signal.initRef(1)
            chain = ref.readOnly.map(_ * 10).map(_ + 1)
            _          <- ref.set(2)
            fromRef    <- now(ref)
            fromChain  <- now(chain)
            chainValue <- chain.current
            fromConst  <- now(Signal.initConst("c").map(_.toUpperCase))
            fromChange <- now(ref.changesTo("handle"))
        yield assert(
            fromRef == Present(2) && fromChain == Present(21) && fromChain == Present(chainValue) &&
                fromConst == Present("C") && fromChange == Present("handle")
        )
    }

    "a signal chosen from a Local answers with the signal the bound Local chooses" in {
        val chooser = Local.init(false)
        for
            on  <- Signal.initRef(1)
            off <- Signal.initRef(2)
            chosen = Signal.withLocal(chooser)(l => if l then on else off)
            read    <- chooser.let(true)(now(chosen.map(_ * 10)))
            current <- chooser.let(true)(chosen.map(_ * 10).current)
        yield assert(read == Present(10) && read == Present(current), s"read $read, current $current")
        end for
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
            fromZip   <- now(combined)
            fromRaw   <- now(raw.map(_ + 1))
            fromLocal <- now(Signal.withLocal(Local.init(0))(_ => raw))
        yield assert(fromZip == Absent && fromRaw == Absent && fromLocal == Absent)
    }

end SignalUnsafeCurrentTest
