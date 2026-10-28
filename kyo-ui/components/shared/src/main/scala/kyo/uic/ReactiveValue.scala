package kyo.uic

import kyo.*

/** Generic const-or-reactive carrier: either a constant `A` or a `Signal[A]` that a
  * component realizes reactively (a text node, a boolean attribute, a class swap, ...).
  * The shared shape behind the domain aliases, e.g. `type SeverityValue =
  * ReactiveValue[Severity]`. [[TextValue]] and [[BoolValue]] predate this and keep their
  * own type-specific helpers, but are the same `Const | Dyn` shape.
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

    /** The ref a reader's change is written into, if this binds two way, else `Absent`. */
    private[uic] def writable: Maybe[SignalRef[A]] = Absent
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
        private[uic] def const: Maybe[A]       = Absent
        private[uic] def dyn: Maybe[Signal[A]] = Present(v)
    end Dyn

    object Dyn:
        /** One-way reactive value over any signal. */
        def apply[A](v: Signal[A]): Dyn[A] = OneWay(v)

        /** The backing signal of any reactive value (one- or two-way). */
        def unapply[A](d: Dyn[A]): Some[Signal[A]] = Some(d.v)

        final private case class OneWay[A](v: Signal[A]) extends Dyn[A]
    end Dyn

    /** Smart constructor from a union: a writable `SignalRef[A]` becomes the two-way
      * [[ReactiveVariable]], any other `Signal[A]` a one-way `Dyn`, any other value `Const`. Lets a
      * single union setter (`x: A | Signal[A]`) carry all three bindings a value slot can hold: the
      * value-level analogue of kyo-ui's union in-place setters. Sound where `A` is never itself a
      * `Signal` (all UI value slots: Boolean/String/enum/Int/Double).
      *
      * The `SignalRef` case comes FIRST and is what makes one setter enough for the editable slots
      * (Slider, Knob, Rating, ProgressBar): handing a component something writable means "write back
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

    /** Draws a slot nobody bound over state the component keeps itself, for as long as it is shown: the reader's change is
      * written there and the field follows, the way an HTML control nobody binds keeps what the reader did. `initial` is
      * where it starts, `draw` draws the component with that state bound two-way, and `placeholder` is what a static
      * projection shows before the mount runs.
      */
    def owned[A](initial: A)(draw: ReactiveValue[A] => UI)(placeholder: UI)(using CanEqual[A, A], Frame): UI =
        UI.mounted(Signal.initRef(initial).map(own => draw(ReactiveVariable(own)))).placeholder(placeholder)

    /** Draws a field the reader edits over a ONE-WAY signal, so that an edit its owner does not take is painted away.
      *
      * The browser changes a native control (a tick, a caret's text, a slider's thumb) before the report reaches the owner,
      * and an owner that declines leaves the signal where it was, so nothing renders and the control keeps what the owner
      * never accepted. `body` gets the value and a `repaint` to run after each report: it bumps a count this mount owns,
      * the region reads the signal beside it, and the field is drawn again from what the owner holds. The morph then puts
      * the control back (a bound box through its `data-kyo-bound` marker, a field through its value). An owner that took
      * the edit draws the same value again, which changes nothing on screen. A static projection draws the placeholder,
      * which has no reader to repaint for.
      */
    def repainting[A](sig: Signal[A])(body: (A, UI.Action) => UI)(using CanEqual[A, A], Frame): UI =
        UI.mounted {
            Signal.initRef(0).map(paints =>
                Reading.Live(sig).and(Reading.Live(paints)).render((a, _) => body(a, paints.updateAndGet(_ + 1).unit))
            )
        }.placeholder(Reading.Live(sig).render(a => body(a, ())))
end ReactiveValue

/** A two-way reactive binding: a writable `SignalRef`. IS-A [[ReactiveValue.Dyn]] (and hence a
  * [[ReactiveValue]]) because a `SignalRef[A]` is a `Signal[A]` (so it reads exactly like a one-way
  * reactive value), while additionally exposing [[ref]] for write-back. Editable value slots
  * (Slider/Rating/Knob/ProgressBar) match `case ReactiveVariable(ref)` to enable two-way binding;
  * read-only slots never construct one, and match it transparently as a `Dyn`.
  */
final private[uic] case class ReactiveVariable[A](ref: SignalRef[A]) extends ReactiveValue.Dyn[A]:
    def v: Signal[A]                                        = ref
    override private[uic] def writable: Maybe[SignalRef[A]] = Present(ref)
