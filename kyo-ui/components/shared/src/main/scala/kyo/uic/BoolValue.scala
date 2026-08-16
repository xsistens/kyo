package kyo.uic

import kyo.*
import kyo.UI.*

/** Const-or-reactive carrier for a component's boolean slot — either a constant `Boolean`
  * or a `Signal[Boolean]` (a mutation-in-flight `disabled`, a reactive `invalid`). The
  * `Boolean` specialization of the generic [[ReactiveValue]]; sibling of [[TextValue]] and
  * [[Input.Value]].
  *
  * A `Dyn` slot backing a plain boolean ATTRIBUTE (`disabled`) is patched in place through
  * kyo-ui's boolean-attribute channel; a `Dyn` slot fused with other build-time logic
  * (`invalid`'s class + message row) is resolved through [[reactive]] (`Signal.render`,
  * re-rendering the element with the current boolean).
  */
private[uic] type BoolValue = ReactiveValue[Boolean]

private[uic] object BoolValue:
    export ReactiveValue.Const
    export ReactiveValue.Dyn

    /** The constant value, if this slot is not reactive. After a host resolves its `Dyn`
      * slots at the top of `render` (via [[reactive]]), every remaining slot is `Const`, so the
      * element-building code reads the plain boolean through this.
      */
    def const(bv: Maybe[BoolValue]): Maybe[Boolean] = bv match
        case Present(Const(b)) => Present(b)
        case _                 => Absent

    /** Resolve a possibly-reactive boolean slot before the host builds its element: if `bv` is
      * `Dyn`, re-render `rebuild` with the slot pinned to the current `Const(b)` on every
      * emission; otherwise build once. The host passes a `rebuild` that `copy`s the resolved
      * slot back in and re-invokes its render.
      */
    def reactive(bv: Maybe[BoolValue])(rebuild: Maybe[BoolValue] => UI)(using Frame): UI =
        bv match
            case Present(Dyn(sig)) => sig.render(b => rebuild(Present(Const(b))))
            case other             => rebuild(other)
end BoolValue

/** Call-site projections/folds over an optional boolean slot. Named so the intent reads at the
  * use site instead of spelling out the `Const`/`Dyn`/`Absent` match each time.
  */
extension (bv: Maybe[BoolValue])
    /** Statically-known truth — `Const(true)` ⇒ true; `Const(false)`/`Dyn`/`Absent` ⇒ false. The
      * plain-boolean read every non-reactive builder site needs (invalid class, aria-invalid, ...).
      */
    private[uic] def constTrue: Boolean = bv.flatMap(_.const).getOrElse(false)

    /** The signal iff the slot is `Dyn`, else `Absent` — for feeding a `Signal[Boolean]` seam
      * (e.g. `FieldInvalid.reactive`) or deciding whether a slot needs the reactive render path.
      */
    private[uic] def dynSig: Maybe[Signal[Boolean]] = bv.flatMap(_.dyn)

    /** Fold the slot onto a builder through kyo-ui's single union setter: a constant applies the
      * boolean statically (`set(v)`), any reactive slot (`Dyn`, incl. a two-way `ReactiveVariable`)
      * reactively (`set(sig)`), `Absent` leaves `self` untouched. `set` is one of the
      * `Boolean | Signal[Boolean]` in-place setters (disabled, readOnly, cssClass(name, _)), so both
      * branches share the same call. Routed through `.const`/`.dyn` so it is total over the whole
      * `ReactiveValue` hierarchy. `inline` splices the setter in — no lambda allocation.
      */
    private[uic] inline def foldFlag[S](self: S)(set: (Boolean | Signal[Boolean]) => S): S =
        // Wildcard rather than `case Absent`: `Maybe` is opaque, so the exhaustivity checker cannot
        // see that Present/Absent covers it, and -Werror turns the false warning into a failure.
        bv.flatMap(_.const) match
            case Present(v) => set(v)
            case _ =>
                bv.flatMap(_.dyn) match
                    case Present(s) => set(s)
                    case _          => self
end extension
