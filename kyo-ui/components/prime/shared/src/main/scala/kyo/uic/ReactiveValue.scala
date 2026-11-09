package kyo.uic

import kyo.*
import kyo.UI.*

/** Generic const-or-reactive carrier: either a constant `A` or a `Signal[A]` that a
  * component realizes reactively (a text node, a boolean attribute, a class swap, ...).
  * The shared shape behind the domain aliases: `type SeverityValue = ReactiveValue[Severity]`, and
  * [[TextValue]] and [[BoolValue]], which add their type-specific helpers.
  *
  * It is also what one render of a control reads, combined with [[and]] and drawn with [[render]] or
  * [[children]], whether a caller bound the value or the control holds it in a ref of its own. A
  * value read that way is only read: [[map]] drops the ref, and a write goes through the [[Controlled]].
  *
  * How a `Dyn` value updates is the component's choice (Signal.render for a text node,
  * the boolean-attribute channel for `disabled`, the class channel for a severity accent).
  *
  * The reactive case [[ReactiveValue.Dyn]] is itself sealed: an ordinary one-way signal, or a
  * two-way [[ReactiveVariable]] (a writable `SignalRef`, which, being a `Signal`, is also a
  * `Dyn`, hence a `ReactiveValue`). Nesting `ReactiveVariable` UNDER `Dyn` rather than beside it
  * keeps every read-only `Const | Dyn | Absent` match exhaustive: a variable IS a `Dyn`.
  */
sealed private[uic] trait ReactiveValue[A]:
    /** The constant value if this is a `Const`, else `Absent`. */
    private[uic] def const: Maybe[A]

    /** The backing signal if this is reactive (`Dyn`, incl. a two-way `ReactiveVariable`),
      * else `Absent`.
      */
    private[uic] def dyn: Maybe[Signal[A]]

    /** The value as it is at this moment. */
    private[uic] def current(using Frame): A < Sync =
        this match
            case ReactiveValue.Const(a) => a
            case ReactiveValue.Dyn(sig) => sig.current

    /** The value as a signal, a constant one for a constant. */
    private[uic] def signal(using CanEqual[A, A], Frame): Signal[A] =
        this match
            case ReactiveValue.Const(a) => Signal.initConst(a)
            case ReactiveValue.Dyn(sig) => sig

    /** The binding as the caller gave it, for a host that hands it on to a component it embeds. */
    private[uic] def bound: A | Signal[A] =
        this match
            case ReactiveValue.Const(a) => a
            case ReactiveValue.Dyn(sig) => sig

    /** The binding for a native control whose owner hears its changes: a signal or a ref as it is, and a constant as a signal that stays, so
      * the element puts back a change its owner did not take. A text field's constant stays out of this: it is the starting text the reader
      * goes on editing.
      */
    private[uic] def native(using CanEqual[A, A], Frame): A | Signal[A] =
        this match
            case ReactiveValue.Const(a) => Signal.initConst(a)
            case ReactiveValue.Dyn(sig) => sig

    /** The ref a reader's change is written into, if this binds two way, else `Absent`. */
    private[uic] def writable: Maybe[SignalRef[A]] = Absent

    /** Reads both, as one.
      *
      * A control that reads several values combines them with this and renders the result ONCE, so it is a single region over
      * everything it reads (see "A control that reads several refs is ONE region" in this module's CLAUDE.md). Nesting a `render`
      * per signal instead leaves each inner region subscribed against the values its outer one held when it built it, and an
      * effect that writes two of them (a pick writes the value and closes the panel) lets the inner one repaint an older state
      * over the finished write.
      *
      * A combination of constants stays a constant and renders with no region at all, which is what keeps a control's static
      * projection free of subscriptions where nothing in it can change. A live combination is one `combineLatest` chain, and a
      * constant joins it as a `Signal.initConst`, which sits out the change-driven combinators rather than drive them.
      */
    private[uic] def and[B](other: ReactiveValue[B])(using CanEqual[A, A], CanEqual[B, B], Frame): ReactiveValue[(A, B)] =
        (this, other) match
            case (ReactiveValue.Const(a), ReactiveValue.Const(b)) => ReactiveValue.Const((a, b))
            case _                                                => ReactiveValue.Dyn(signal.combineLatest(other.signal))

    /** The value through `f`. A mapped binding is one way, over a ref too: a reader's change goes through the [[Controlled]], never through
      * what a render read from it.
      */
    private[uic] def map[B](f: A => B)(using CanEqual[B, B], Frame): ReactiveValue[B] =
        this match
            case ReactiveValue.Const(a) => ReactiveValue.Const(f(a))
            case ReactiveValue.Dyn(sig) => ReactiveValue.Dyn(sig.map(f))

    /** Renders `f` with the value: once for a constant, as one region for a signal. */
    private[uic] def render(f: A => UI)(using Frame): UI =
        this match
            case ReactiveValue.Const(a) => f(a)
            case ReactiveValue.Dyn(sig) => sig.render(f)

    /** Renders the children `f` draws for the value: as they are for a constant, inside one region for a signal. All of them share
      * that region, so a rule over siblings (`:first-child`, `:nth-child`, `~`) still sees them as one run.
      */
    private[uic] def children(f: A => List[UI])(using Frame): List[UI] =
        this match
            case ReactiveValue.Const(a) => f(a)
            case ReactiveValue.Dyn(sig) => List(sig.render(a => fragment(f(a)*)))
end ReactiveValue

private[uic] object ReactiveValue:
    final case class Const[A](v: A) extends ReactiveValue[A]:
        private[uic] def const: Maybe[A]       = Present(v)
        private[uic] def dyn: Maybe[Signal[A]] = Absent

    /** A reactive (signal-backed) value. Sealed: either the one-way `OneWay` over an arbitrary
      * `Signal`, or a two-way [[ReactiveVariable]] over a writable `SignalRef`. Match `case Dyn(sig)`
      * to read either uniformly; match `case ReactiveVariable(ref)` FIRST to single out the two-way
      * case.
      */
    sealed trait Dyn[A] extends ReactiveValue[A]:
        def v: Signal[A]

        /** With [[get]], what makes this its own name-based match result, so `case Dyn(sig)` allocates nothing. `false` as a type: a
          * `Boolean` here, or a `Maybe.Ops` from `unapply`, makes the extractor refutable and every `Const | Dyn` match inexhaustive.
          */
        def isEmpty: false = false
        def get: Signal[A] = v

        private[uic] def const: Maybe[A]       = Absent
        private[uic] def dyn: Maybe[Signal[A]] = Present(v)
    end Dyn

    object Dyn:
        /** One-way reactive value over any signal. */
        def apply[A](v: Signal[A]): Dyn[A] = OneWay(v)

        /** The backing signal of any reactive value (one- or two-way). */
        def unapply[A](d: Dyn[A]): Dyn[A] = d

        final private case class OneWay[A](v: Signal[A]) extends Dyn[A]
    end Dyn

    /** Smart constructor from a union: a writable `SignalRef[A]` becomes the two-way
      * [[ReactiveVariable]], any other `Signal[A]` a one-way `Dyn`, any other value `Const`. Lets a
      * single union setter (`x: A | Signal[A]`) carry all three bindings a value slot can hold: the
      * value-level analogue of kyo-ui's union in-place setters. Sound where `A` is never itself a
      * `Signal` (all UI value slots: Boolean/String/enum/Int/Double).
      *
      * The `SignalRef` case comes FIRST and is what makes one setter enough for the editable slots
      * (Slider, Knob, Rating): handing a component something writable means "write back
      * to me". `Signal` is sealed and `SignalRef` is its only named subclass, so the two reactive cases
      * are exact and total, not a heuristic.
      *
      * The choice is made on the RUNTIME class, so ascribing `ref: Signal[A]` does NOT opt out of
      * two-way binding. `ref.readOnly` is the opt-out, and read-only hosts need no change either way:
      * `ReactiveVariable` IS-A `Dyn`, so every `case Dyn(sig)` still matches.
      */
    def apply[A](v: A | Signal[A]): ReactiveValue[A] = v match
        case r: SignalRef[A] @unchecked => ReactiveVariable(r)
        case s: Signal[A] @unchecked    => Dyn(s)
        case a: A @unchecked            => Const(a)

    /** Lists, each fixed or a signal, as ONE value of them end to end, in order: a constant while none of them is a signal, so a
      * component given plain lists draws no region for them. Neighbouring fixed lists are joined first, so two parts or more left
      * over hold a signal.
      */
    def concat[A](parts: Seq[List[A] | Signal[Seq[A]]])(using Frame): ReactiveValue[List[A]] =
        given CanEqual[List[A], List[A]]         = CanEqual.derived
        val joined: List[ReactiveValue[List[A]]] =
            parts.foldLeft(List.empty[ReactiveValue[List[A]]]) { (acc, part) =>
                part match
                    case sig: Signal[?] => Dyn(sig.asInstanceOf[Signal[Seq[A]]].map(_.toList)) :: acc
                    case fixed          =>
                        val as = fixed.asInstanceOf[List[A]]
                        acc match
                            case Const(prev) :: rest => Const(prev ++ as) :: rest
                            case _                   => Const(as) :: acc
            }.reverse
        joined match
            case Nil        => Const(Nil)
            case one :: Nil => one
            case several    => Dyn(Signal.combineLatestAll(several.map(_.signal)).map(_.toList.flatten))
        end match
    end concat

    /** A signal where one is bound, else `fallback`, a constant. */
    def orElse[A](sig: Maybe[Signal[A]], fallback: A): ReactiveValue[A] =
        sig.fold[ReactiveValue[A]](Const(fallback))(Dyn(_))

    /** Several signals, each under its key, as ONE value of the map of their values; with none, an empty map, a constant. */
    def keyed[K, V](entries: Seq[(K, Signal[V])])(using CanEqual[V, V], Frame): ReactiveValue[Map[K, V]] =
        if entries.isEmpty then Const(Map.empty)
        else
            given CanEqual[Map[K, V], Map[K, V]] = CanEqual.derived
            val keys                             = entries.map(_._1)
            Dyn(Signal.combineLatestAll(entries.map(_._2)).map(values => keys.zip(values).toMap))
    end keyed

    /** An optional slot as a value of its value, `Absent` where it is unset. */
    def optional[A](v: Maybe[ReactiveValue[A]])(using CanEqual[A, A], Frame): ReactiveValue[Maybe[A]] =
        v match
            case Present(Dyn(sig)) => Dyn(sig.map(a => Present(a)))
            case Present(Const(a)) => Const(Present(a))
            case Absent            => Const(Absent)

    /** A reader's change to a state slot, as every slot takes it: a two-way binding is written first, then `report` hears
      * the new value, whatever the binding is. A slot bound one way (a signal, a constant, or nothing at all) is not
      * written, so `report` is how a caller that keeps the state elsewhere learns what the reader asked for.
      */
    def change[A](slot: Maybe[ReactiveValue[A]], next: A, report: Maybe[A => UI.Action])(using Frame): UI.Action =
        val write: UI.Action = slot.flatMap(_.writable).fold[UI.Action](())(_.set(next))
        write.andThen(report.fold[UI.Action](())(_(next)))
    end change

    /** A reader's change that is a step from the value the slot holds (a sort press, a toggle in a set): a two-way binding
      * steps its ref atomically, a one-way one steps the value it shows, and `report` hears the result either way.
      */
    def update[A](slot: Maybe[ReactiveValue[A]], step: A => A, report: Maybe[A => UI.Action])(using Frame): UI.Action =
        slot match
            case Absent         => ()
            case Present(bound) =>
                val next: A < Sync = bound match
                    case ReactiveVariable(ref)  => ref.updateAndGet(step)
                    case ReactiveValue.Dyn(sig) => sig.current.map(step)
                    case ReactiveValue.Const(a) => step(a)
                next.map(v => report.fold[UI.Action](())(_(v)))
    end update

    /** Whether a reader's change to `slot` reaches anybody: a two-way binding, or a `report` to hand it to. A slot bound one
      * way with nobody to report to paints the change away on the next render, which is what the cards say.
      */
    def heard[A, R](slot: Maybe[ReactiveValue[A]], report: Maybe[R]): Boolean =
        slot.exists(_.writable.isDefined) || report.isDefined

    /** A constant nobody hears: the slot is fixed, so the component draws it read-only, reachable and readable with nothing
      * to change, and says nothing about it.
      */
    def fixed[A, R](slot: Maybe[ReactiveValue[A]], report: Maybe[R]): Boolean =
        slot.exists(_.const.isDefined) && !heard(slot, report)

    /** A signal bound one way that nobody hears: the state lives somewhere else and the reader's change reaches nobody,
      * which is the one binding a card names.
      */
    def deaf[A, R](slot: Maybe[ReactiveValue[A]], report: Maybe[R]): Boolean =
        slot.exists(s => s.dyn.isDefined && s.writable.isEmpty) && !heard(slot, report)

end ReactiveValue

/** A two-way reactive binding: a writable `SignalRef`. IS-A [[ReactiveValue.Dyn]] (and hence a
  * [[ReactiveValue]]) because a `SignalRef[A]` is a `Signal[A]` (so it reads exactly like a one-way
  * reactive value), while additionally exposing [[ref]] for write-back. Editable value slots
  * (Slider/Rating/Knob) match `case ReactiveVariable(ref)` to enable two-way binding;
  * read-only slots never construct one, and match it transparently as a `Dyn`.
  */
final private[uic] case class ReactiveVariable[A](ref: SignalRef[A]) extends ReactiveValue.Dyn[A]:
    def v: Signal[A]                                        = ref
    override private[uic] def writable: Maybe[SignalRef[A]] = Present(ref)
