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
  */
private[kyo] object SignalNow:

    def apply[A](signal: Signal[A])(using AllowUnsafe, Frame): A =
        signal.unsafeCurrent() match
            case Present(v) => v
            case Absent     => Sync.Unsafe.evalOrThrow(signal.current)

end SignalNow
