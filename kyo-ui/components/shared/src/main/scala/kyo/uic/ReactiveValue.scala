package kyo.uic

import kyo.*

/** Generic const-or-reactive carrier: either a constant `A` or a `Signal[A]` that a
  * component realizes reactively (a text node, a boolean attribute, a class swap, ...).
  * The shared shape behind the domain aliases — e.g. `type SeverityValue =
  * ReactiveValue[Severity]`. [[TextValue]] and [[BoolValue]] predate this and keep their
  * own type-specific helpers, but are the same `Const | Dyn` shape.
  *
  * How a `Dyn` value updates is the component's choice (Signal.render for a text node,
  * the boolean-attribute channel for `disabled`, the class channel for a severity accent).
  *
  * The reactive case [[ReactiveValue.Dyn]] is itself sealed: an ordinary one-way signal, or a
  * two-way [[ReactiveVariable]] (a writable `SignalRef`, which — being a `Signal` — is also a
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

    /** Smart constructor from a union: a `Signal[A]` becomes `Dyn`, any other value `Const`. Lets a
      * single union setter (`x: A | Signal[A]`) store the correct case with one call — the value-level
      * analogue of kyo-ui's union in-place setters. Sound where `A` is never itself a `Signal` (all UI
      * value slots: Boolean/String/enum/Int).
      */
    def apply[A](v: A | Signal[A]): ReactiveValue[A] = v match
        case s: Signal[A] @unchecked => Dyn(s)
        case a: A @unchecked         => Const(a)
end ReactiveValue

/** A two-way reactive binding: a writable `SignalRef`. IS-A [[ReactiveValue.Dyn]] (and hence a
  * [[ReactiveValue]]) because a `SignalRef[A]` is a `Signal[A]` — so it reads exactly like a one-way
  * reactive value — while additionally exposing [[ref]] for write-back. Editable value slots
  * (Slider/Rating/Knob/ProgressBar) match `case ReactiveVariable(ref)` to enable two-way binding;
  * read-only slots never construct one, and match it transparently as a `Dyn`.
  */
final private[uic] case class ReactiveVariable[A](ref: SignalRef[A]) extends ReactiveValue.Dyn[A]:
    def v: Signal[A] = ref
