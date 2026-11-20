package kyo

import kyo.internal.SignalNow

/** A synchronous pass reads a signal the way the fiber running it would, Locals included. */
class SignalNowTest extends kyo.test.Test[Any]:

    private val label: Local[String] = Local.init("unset")

    private def read[A](signal: Signal[A])(using Frame): A < Sync =
        SignalNow.Locals.take(Sync.Unsafe.defer(SignalNow[A](signal)))

    "a signal over a Local reads the value the running fiber sees" in {
        label
            .let("set")(read(Signal.withLocal(label)(l => Signal.initConst(s"label:$l"))))
            .map(v => assert(v == "label:set"))
    }

    "a signal over a Local without a field read is read in place with the running fiber's locals" in {
        val raw = Signal.initRaw[String](
            currentWith = [B, S] => f => label.use(l => f(s"raw:$l")),
            nextWith = [B, S] => _ => Async.never
        )
        label
            .let("set")(read(Signal.withLocal(label)(_ => raw)))
            .map(v => assert(v == "raw:set"))
    }

end SignalNowTest
