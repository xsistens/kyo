package kyo.internal

import kyo.*

/** A signal's current value, read on the caller's own stack.
  *
  * The tree walk and the HTML render are synchronous passes that read a signal at every bound node:
  * a thousand rows read two each. Through `current` every read is a `Sync` suspension, and each
  * suspension costs the pass a continuation per enclosing frame: the recursion above the read is
  * torn down into objects and re-entered through the kernel loop, for a value that is a field read.
  * So a pass runs under one `Sync.Unsafe.defer` and reads through this instead: the field read where
  * the signal offers one ([[Signal.unsafeCurrent]]: refs, constants, `map` chains over them), and the
  * effectful read evaluated in place for anything else. Both produce the value `current` would have
  * produced at that point of the same pass; the defer around the pass is what keeps the pass a `Sync`
  * effect to its caller.
  *
  * An effect evaluated in place runs outside any fiber, where no `Local` is bound, while a signal such as
  * `Signal.withLocal` chooses its value by one. So a pass takes the locals of the fiber it runs for once,
  * as it starts ([[Locals.take]]), and the in-place read runs with them bound.
  */
private[kyo] object SignalNow:

    /** The locals of the fiber a pass runs for, taken as the pass starts. */
    opaque type Locals = Map[Local[?], AnyRef]

    object Locals:
        /** Runs `f` with the locals of the current fiber at hand for the reads of the pass it starts. */
        def take[B, S](f: Locals ?=> B < S)(using Frame): B < S =
            Local.snapshotWith(locals => f(using locals))

        /** For a pass that reads no signal, so has nothing to read with. */
        val none: Locals = Map.empty
    end Locals

    def apply[A](signal: Signal[A])(using Frame, Locals, AllowUnsafe): A =
        signal.unsafeCurrent() match
            case Present(v) => v
            case Absent     => Sync.Unsafe.evalOrThrow(Local.restore(summon[Locals])(signal.current))

end SignalNow
