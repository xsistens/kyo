package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** Rating — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Rating anatomy: `div.p-rating.p-component[.p-readonly][.p-disabled]` > N
  * `div.p-rating-option[.p-rating-option-active]`, each holding a hidden native
  * radio (`span.p-hidden-accessible` > `input[type=radio]`) plus the star glyph
  * `.p-rating-icon` — filled `.p-rating-on-icon` up to the current value,
  * outline `.p-rating-off-icon` beyond it), so the extracted `@primeuix`
  * rating CSS applies verbatim.
  *
  * Clicking option i sets the value to i+1; clicking the CURRENT value clears
  * to 0 — Prime's cancel-on-same-value semantics. Prime v10's separate cancel
  * icon was retired in the PrimeOne sheet (no `.p-rating-cancel` classes ship),
  * so it is intentionally not ported. `value(SignalRef)` binds two-way: clicks
  * write back into the ref before firing the user `onChange`.
  *
  * The hidden radios are PrimeVue's anatomy: they carry the checked state,
  * the per-star `aria-label`, and native keyboard semantics; `name(...)` groups
  * them and submits the value with HTML forms. Prime derives a default name
  * from a per-instance attr selector — a pure server render has no such
  * uniqueness source, so the `name` attribute is only emitted when set.
  * `onIcon`/`offIcon` swap the star glyphs for any [[IconGlyph]].
  *
  * A [[NumberFormControl]], so `uic.Rating().bind(field)` wires value, validity,
  * message and the blur trigger in one call. The family speaks `Double`; a Rating
  * rounds on the way in and writes whole stars on the way out, which is exactly what
  * `NumberFormControl.integer` asks for and why that setter has nothing left to do
  * here.
  */
final case class Rating private (
    valueBinding: Maybe[ReactiveValue[Int]] = Absent,
    doubleBinding: Maybe[ReactiveValue[Double]] = Absent,
    starsV: Int = 5,
    readonlyFlag: Boolean = false,
    disabledFlag: Maybe[BoolValue] = Absent,
    nameV: Maybe[String] = Absent,
    onIconV: Maybe[IconGlyph] = Absent,
    offIconV: Maybe[IconGlyph] = Absent,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    onChangeF: Maybe[Int => Any < Async] = Absent,
    onBlurDoubleF: Maybe[Double => Any < Async] = Absent,
    idV: Maybe[String] = Absent
) extends Node, NumberFormControl, HasAccessibleName:
    type Self = Rating

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): Rating = copy(idV = v)

    /** The star count, in any of the three bindings a value slot holds (0 = nothing selected). A
      * constant renders the stars statically. A writable `SignalRef[Int]` binds TWO-WAY: clicks
      * write the new value back, and ref changes re-render. Any other `Signal[Int]` binds one-way,
      * so the stars track it and clicks write nowhere.
      *
      * The two-way choice is made on the runtime class, so ascribing a ref as `Signal[Int]` does
      * not opt out of write-back; pass `ref.readOnly` for that.
      */
    def value(v: Int | Signal[Int]): Rating = copy(valueBinding = Present(ReactiveValue(v)))

    /** The `Double`-shaped binding of [[NumberFormControl]], so a Rating can carry a
      * `Form.numberField`. The stars remain whole: the value is read rounded and written back as a
      * whole number, which is what [[integer]] asks of this family and what a star count is anyway.
      *
      * `@targetName` because this and the `Int` binding above both erase to `value(Object)`; the
      * JVM name is the only thing that differs, and the call site still reads `value(...)`.
      */
    @targetName("valueNumber")
    def value(v: Double | Signal[Double]): Rating = copy(doubleBinding = Present(ReactiveValue(v)))

    /** Number of stars (Prime default 5). */
    def stars(n: Int): Rating = copy(starsV = math.max(1, n))

    /** `readonly` — the stars keep their look, keep their place in the tab order, and stop
      * reacting (`.p-readonly`, `aria-readonly` on the group). Only `disabled` takes them out of
      * reach.
      */
    def readonly(v: Boolean): Rating = copy(readonlyFlag = v)

    /** Disables the control (`.p-disabled` dimming + no interaction); a `Signal[Boolean]`
      * toggles it reactively (re-renders this small control on change).
      */
    def disabled(v: Boolean | Signal[Boolean]): Rating = copy(disabledFlag = Present(ReactiveValue(v)))

    /** Native `name` on the hidden per-option radios, which makes the value participate in HTML
      * form submission.
      *
      * Set it for the FORM, not for the keyboard: a mount mints one when you do not, because the
      * name is also what groups the radios, and an ungrouped set of them is five tab stops with no
      * arrows between them rather than the one tab stop this control is.
      */
    def name(v: String): Rating = copy(nameV = Present(v))

    /** Glyph for the ACTIVE options (Prime default: the filled star). */
    def onIcon(glyph: IconGlyph): Rating = copy(onIconV = Present(glyph))

    /** Glyph for the options beyond the value (Prime default: the outline star). */
    def offIcon(glyph: IconGlyph): Rating = copy(offIconV = Present(glyph))

    private[uic] def withAccessibleName(v: Maybe[TextValue]): Rating = copy(accNameV = v)

    private[uic] def withInvalid(v: Maybe[BoolValue]): Rating                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): Rating                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): Rating = copy(invalidMsgDynV = v)

    /** Accepted for the [[NumberFormControl]] contract and satisfied by construction: a
      * star count is always whole, so the constraint has nothing left to enforce here.
      * Returns this unchanged rather than storing a flag that could never be false in
      * effect.
      */
    def integer(v: Boolean): Rating = this

    /** Fired with the NEW value (0 on cancel) after the ref write-back. */
    def onChange(f: Int => Any < Async): Rating = copy(onChangeF = Present(f))

    /** Fires on focus loss with the current star count — the validation layer's Blur
      * trigger, in the `Double` shape the number family speaks.
      */
    def onBlur(f: Double => Any < Async): Rating = copy(onBlurDoubleF = Present(f))

    private def interactive: Boolean = !readonlyFlag && !disabledFlag.constTrue

    private[uic] def render(using Frame): UI =
        // The radios' `name` is what the browser groups them by, and the grouping is what gives
        // this control its single tab stop and its arrows. Without one every star was its own
        // group: five tab stops, no arrows, and nothing stopping two of them from being checked.
        // A caller's own name wins; otherwise the mount mints one, and the static projection
        // keeps the shape it had (the Select precedent).
        if nameV.isDefined then renderNamed
        else
            UI.mounted {
                UI.commands.map(cmds => cmds.freshId.map(n => copy(nameV = Present(n)).renderNamed))
            }.placeholder(renderNamed)

    /** The tree the mount publishes once it has a name to group the radios by — the seam golden
      * tests render directly, since a mount shows only its placeholder there.
      */
    private[uic] def wired(name: String)(using Frame): UI = copy(nameV = Present(name)).renderNamed

    private def renderNamed(using Frame): UI =
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => renderDisabledResolved
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent).renderDisabledResolved
                )

    private def renderDisabledResolved(using Frame): UI =
        BoolValue.reactive(disabledFlag): d =>
            copy(disabledFlag = d).renderResolved

    /** The `Double` ref to write back to, i.e. only when the form-layer binding is two-way. */
    private def doubleWriteRef: Maybe[SignalRef[Double]] = doubleBinding match
        case Present(ReactiveVariable(r)) => Present(r)
        case _                            => Absent

    private def renderResolved(using Frame): UI =
        doubleBinding match
            // The Double binding wins when present: it is the form layer's, and a control
            // bound to a field has no second source of truth. ReactiveVariable before Dyn,
            // since a two-way binding IS a Dyn.
            case Present(ReactiveVariable(dref)) => dref.render(d => body(math.rint(d).toInt, Absent))
            case Present(ReactiveValue.Dyn(sig)) => sig.render(d => body(math.rint(d).toInt, Absent))
            case Present(ReactiveValue.Const(d)) => body(math.rint(d).toInt, Absent)
            case Absent =>
                valueBinding match
                    case Present(ReactiveVariable(ref))  => ref.render(v => body(v, Present(ref)))
                    case Present(ReactiveValue.Dyn(sig)) => sig.render(v => body(v, Absent))
                    case Present(ReactiveValue.Const(v)) => body(v, Absent)
                    case Absent                          => body(0, Absent)

    private def body(value: Int, ref: Maybe[SignalRef[Int]])(using Frame): UI =
        val options: List[UI] = (1 to starsV).toList.map { i =>
            val active = i <= value
            var opt    = div.cssClass("p-rating-option")
            if active then opt = opt.cssClass("p-rating-option-active")
            if interactive then opt = opt.onClick(activate(i, value, ref))

            // PrimeVue's hidden radio: real checked state, per-star aria-label, and native
            // keyboard semantics — the arrows move within the name group and select as they go,
            // which is the whole reason the group has a name.
            //
            // It carries NO handler of its own. Selecting a radio from the keyboard is an
            // activation: the browser checks it, fires `change`, and dispatches a `click` that
            // bubbles out of the input. A handler on each meant the change wrote the new star, the
            // region re-rendered around it, and the click then reached an option rebuilt with that
            // value in hand, read it as a second pick of the same star and cleared it — every
            // arrow press ended at zero. The click is the one activation both inputs produce (a
            // press on the visible star reaches only the option, since the radio is clipped away),
            // so it is the one that acts.
            var hidden = radio
                .checked(i == value)
                .jsProp("value", i.toString)
                .aria("label", if i == 1 then "1 star" else s"$i stars")
            nameV.foreach(n => hidden = hidden.name(n))
            // readonly keeps the stars FOCUSABLE and refuses to change: only `disabled` takes them
            // out of the tab order. `aria-readonly` belongs on the GROUP, since a radio has no such
            // state of its own; the group below carries it.
            if disabledFlag.constTrue then hidden = hidden.disabled(true)
            // The one exception to the rule above, and it is the browser's: a radio that is
            // ALREADY checked has no activation behaviour, so Space on it dispatches no click and
            // Prime's cancel-on-same-value would be a thing only a pointer could reach. Wired on
            // that star alone, which is exactly the star no click can come from.
            if interactive && i == value then
                hidden = hidden.onKeyDown { e =>
                    val eff: Any < Async = if e.key == Keyboard.Space then activate(i, value, ref) else ()
                    eff
                }
            end if
            val hiddenSlot: UI = span.cssClass("p-hidden-accessible")(toChild(hidden: UI))

            val icon: UI =
                if active then GlyphSvg(onIconV.getOrElse(Icons.starFill), "p-rating-icon", "p-rating-on-icon")
                else GlyphSvg(offIconV.getOrElse(Icons.star), "p-rating-icon", "p-rating-off-icon")
            opt(toChild(hiddenSlot), toChild(icon))
        }

        // The radios inside form one group, so the box around them says so: an `aria-label` on a
        // roleless div labels nothing in particular.
        var el = div.cssClass("p-rating").cssClass("p-component").role("radiogroup")
        idV.foreach(v => el = el.id(v))
        if readonlyFlag then el = el.cssClass("p-readonly").aria("readonly", "true").preventActivation
        if disabledFlag.constTrue then el = el.cssClass("p-disabled")
        if invalidV.constTrue then el = el.cssClass("p-invalid").aria("invalid", "true")
        accNameV match
            case Present(TextValue.Const(v)) => el = el.aria("label", v)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        onBlurDoubleF.foreach { f =>
            el = el.onBlur(doubleWriteRef match
                case Present(r) => r.use(d => f(math.rint(d)))
                case Absent     => f(value.toDouble))
        }
        FieldInvalid.withMessage(el(options.map(toChild)*), invalidV.constTrue, invalidMsgV)
    end body

    /** Prime's select semantics: picking the current value clears to 0. */
    private def activate(i: Int, current: Int, ref: Maybe[SignalRef[Int]])(using Frame): Any < Async =
        val next = if i == current then 0 else i
        val write: Any < Async = (ref, doubleWriteRef) match
            case (Present(r), _)       => r.set(next)
            case (Absent, Present(dr)) => dr.set(next.toDouble)
            case _                     => ()
        val fire: Any < Async = onChangeF match
            case Present(f) => f(next)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end activate
end Rating

object Rating:
    def apply(): Rating = new Rating()
