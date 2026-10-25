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

    /** The reading as a slot value: a fixed one as the value, so the control it binds draws no region for it. */
    def slot: A | Signal[A] =
        this match
            case Fixed(a)  => a
            case Live(sig) => sig

    /** Renders `f` with the value: once for a fixed one, as one region for a live one. */
    def render(f: A => UI)(using Frame): UI =
        this match
            case Fixed(a)  => f(a)
            case Live(sig) => sig.render(f)

    /** Renders the children `f` draws for the value: as they are for a fixed one, inside one region for a live one. All of
      * them share that region, so a rule over siblings (`:first-child`, `:nth-child`, `~`) still sees them as one run.
      */
    def children(f: A => List[UI])(using Frame): List[UI] =
        this match
            case Fixed(a)  => f(a)
            case Live(sig) => List(sig.render(a => fragment(f(a)*)))
end Reading

private[uic] object Reading:

    /** A const-or-reactive slot as a reading. */
    def of[A](v: ReactiveValue[A]): Reading[A] =
        v match
            case ReactiveValue.Dyn(sig) => Live(sig)
            case ReactiveValue.Const(a) => Fixed(a)

    /** Lists, each fixed or a signal, as ONE reading of them end to end, in order: fixed while none of them is a signal, so a
      * component given plain lists draws no region for them. Neighbouring fixed lists are joined first, so two parts or more
      * left over hold a signal.
      */
    def concat[A](parts: Seq[List[A] | Signal[Seq[A]]])(using Frame): Reading[List[A]] =
        given CanEqual[List[A], List[A]]   = CanEqual.derived
        val joined: List[Reading[List[A]]] =
            parts.foldLeft(List.empty[Reading[List[A]]]) { (acc, part) =>
                part match
                    case sig: Signal[?] => Live(sig.asInstanceOf[Signal[Seq[A]]].map(_.toList)) :: acc
                    case fixed          =>
                        val as = fixed.asInstanceOf[List[A]]
                        acc match
                            case Fixed(prev) :: rest => Fixed(prev ++ as) :: rest
                            case _                   => Fixed(as) :: acc
            }.reverse
        joined match
            case Nil        => Fixed(Nil)
            case one :: Nil => one
            case several    => Live(Signal.combineLatestAll(several.map(_.signal)).map(_.toList.flatten))
        end match
    end concat

    /** A signal where one is bound, else `fallback`, fixed. */
    def orElse[A](sig: Maybe[Signal[A]], fallback: A): Reading[A] =
        sig.fold(Fixed(fallback))(Live(_))

    /** Several signals, each under its key, as ONE reading of the map of their values; with none, an empty map, fixed. */
    def keyed[K, V](entries: Seq[(K, Signal[V])])(using CanEqual[V, V], Frame): Reading[Map[K, V]] =
        if entries.isEmpty then Fixed(Map.empty)
        else
            given CanEqual[Map[K, V], Map[K, V]] = CanEqual.derived
            val keys                             = entries.map(_._1)
            Live(Signal.combineLatestAll(entries.map(_._2)).map(values => keys.zip(values).toMap))
    end keyed

    /** An optional slot as a reading of its value, `Absent` where it is unset. */
    def optional[A](v: Maybe[ReactiveValue[A]])(using CanEqual[A, A], Frame): Reading[Maybe[A]] =
        v match
            case Present(ReactiveValue.Dyn(sig)) => Live(sig.map(a => Present(a)))
            case Present(ReactiveValue.Const(a)) => Fixed(Present(a))
            case Absent                          => Fixed(Absent)
end Reading
