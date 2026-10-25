package kyo.uic

import kyo.*
import kyo.UI.*

/** What one render of a control reads: a value fixed when the control was built, or one that follows a signal.
  *
  * A control that reads several of them combines them with [[and]] and renders the result ONCE, so it is a single region
  * over everything it reads (see "A control that reads several refs is ONE region" in this module's CLAUDE.md). Nesting a
  * `render` per signal instead leaves each inner region subscribed against the values its outer one held when it built
  * it, and an effect that writes two of them (a pick writes the value and closes the panel) lets the inner one repaint an
  * older state over the finished write.
  *
  * A combination of fixed values stays fixed and renders with no region at all, which is what keeps a control's static
  * projection free of subscriptions where nothing in it can change. A live combination is one `combineLatest` chain, and
  * [[Reading.Fixed]] joins it as a `Signal.initConst`, which sits out the change-driven combinators rather than drive
  * them.
  */
private[uic] enum Reading[A]:
    case Fixed(value: A)
    case Live(signal: Signal[A])

    /** Reads both, as one. */
    def and[B](other: Reading[B])(using CanEqual[A, A], CanEqual[B, B], Frame): Reading[(A, B)] =
        (this, other) match
            case (Fixed(a), Fixed(b)) => Fixed((a, b))
            case _                    => Live(this.signal.combineLatest(other.signal))

    def map[B](f: A => B)(using CanEqual[B, B], Frame): Reading[B] =
        this match
            case Fixed(a)  => Fixed(f(a))
            case Live(sig) => Live(sig.map(f))

    /** The reading as a signal; a fixed value becomes one that never changes. */
    def signal(using CanEqual[A, A], Frame): Signal[A] =
        this match
            case Fixed(a)  => Signal.initConst(a)
            case Live(sig) => sig

    /** Renders `f` with the value: once for a fixed one, as one region for a live one. */
    def render(f: A => UI)(using Frame): UI =
        this match
            case Fixed(a)  => f(a)
            case Live(sig) => sig.render(f)
end Reading

private[uic] object Reading:

    /** A const-or-reactive slot as a reading. */
    def of[A](v: ReactiveValue[A]): Reading[A] =
        v match
            case ReactiveValue.Dyn(sig) => Live(sig)
            case ReactiveValue.Const(a) => Fixed(a)

    /** An optional slot as a reading of its value, `Absent` where it is unset. */
    def optional[A](v: Maybe[ReactiveValue[A]])(using CanEqual[A, A], Frame): Reading[Maybe[A]] =
        v match
            case Present(ReactiveValue.Dyn(sig)) => Live(sig.map(a => Present(a)))
            case Present(ReactiveValue.Const(a)) => Fixed(Present(a))
            case Absent                          => Fixed(Absent)
end Reading
