package kyo.internal

import kyo.*

/** A signal's current value, read on the caller's own stack.
  *
  * A synchronous pass, such as a UI tree walk and its HTML render, reads a signal at every bound node: a thousand rows read two each.
  * Through `current` every read is a `Sync` suspension, and each suspension costs the pass a continuation per enclosing frame: the
  * recursion above the read is torn down into objects and re-entered through the kernel loop, for a value that is a field read. So a
  * pass runs under one `Sync.Unsafe.defer` and reads through this instead: the field read where the signal offers one
  * ([[Signal.unsafeCurrent]]: refs, constants, `map` chains over them, and a `Signal.withLocal` choosing one of those), and the
  * effectful read evaluated in place for anything else. Both produce the value `current` would have produced at that point of the
  * same pass; the defer around the pass is what keeps the pass a `Sync` effect to its caller.
  *
  * A read on the caller's stack runs outside any fiber, where no `Local` is bound, while a signal such as `Signal.withLocal` chooses
  * its value by one. So a pass takes the locals of the fiber it runs for once, as it starts ([[Locals.take]]), and every read of the
  * pass uses them: the field read reads the `Local` from them, and the in-place read runs with them bound.
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

        extension (locals: Locals)
            /** What `local` holds in these locals. */
            def read[A](local: Local[A]): A = local.getIn(locals)

            /** These locals with `local` bound to `value`. */
            def bound[A](local: Local[A], value: A): Locals =
                locals.updated(local, value.asInstanceOf[AnyRef])
        end extension
    end Locals

    def apply[A](signal: Signal[A])(using Frame, Locals, AllowUnsafe): A =
        signal.unsafeCurrent(summon[Locals]) match
            case Present(v) => v
            case Absent     => eval(signal.current)

    /** Evaluates `v` in place with the locals of the pass bound: what a pass needs of an effect it cannot suspend for. */
    def eval[A](v: A < (Sync & Abort[Throwable]))(using Frame, Locals, AllowUnsafe): A =
        Sync.Unsafe.evalOrThrow(Local.restore(summon[Locals])(v))

end SignalNow
