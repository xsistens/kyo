package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** Rating: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Rating anatomy: `div.p-rating.p-component[.p-readonly][.p-disabled]` > N
  * `div.p-rating-option[.p-rating-option-active]`, each holding a hidden native
  * radio (`span.p-hidden-accessible` > `input[type=radio]`) plus the star glyph
  * `.p-rating-icon`: filled `.p-rating-on-icon` up to the current value,
  * outline `.p-rating-off-icon` beyond it), so the extracted `@primeuix`
  * rating CSS applies verbatim.
  *
  * Clicking option i sets the value to i+1; clicking the CURRENT value clears
  * to 0 (Prime's cancel-on-same-value semantics). Prime v10's separate cancel
  * icon was retired in the PrimeOne sheet (no `.p-rating-cancel` classes ship),
  * so it is intentionally not ported. `value(SignalRef)` binds two-way: clicks
  * write back into the ref before firing the user `onChange`.
  *
  * The hidden radios are PrimeVue's anatomy: they carry the checked state,
  * the per-star `aria-label`, and native keyboard semantics; `name(...)` groups
  * them and submits the value with HTML forms. Prime derives a default name
  * from a per-instance attr selector; a pure server render has no such
  * uniqueness source, so the `name` attribute is only emitted when set.
  * `onIcon`/`offIcon` swap the star glyphs for any [[IconGlyph]].
  *
  * A `FormControl[Double]`, so `uic.Rating().bind(field)` wires value, validity,
  * message and the blur trigger in one call. The family speaks `Double`; a Rating
  * rounds on the way in and writes whole stars on the way out, which is exactly what
  * the number binding's `integer` asks for and why that setter has nothing left to do
  * here.
  *
  * `id` lands on the hidden radio a Tab reaches, since the group itself takes no focus: the checked star's,
  * or the first star's while none is checked.
  */
final class Rating private[uic] (private[uic] val p: Rating.Props) extends Node, FormControl[Double], HasAccessibleName, HasProps:
    import p.*

    type Self = Rating

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new Rating(p.copy(access = v))

    /** The star count, in any of the three bindings a value slot holds (0 = nothing selected). A
      * constant nobody hears is fixed, drawn read-only; one [[onChange]] hears is reported and the
      * stars painted back to the constant. A writable `SignalRef[Int]` binds TWO-WAY: clicks
      * write the new value back, and ref changes re-render. Any other `Signal[Int]` binds one-way,
      * so the stars track it and clicks write nowhere.
      *
      * The two-way choice is made on the runtime class, so ascribing a ref as `Signal[Int]` does
      * not opt out of write-back; pass `ref.readOnly` for that.
      */
    def value(v: Int | Signal[Int]): Rating = new Rating(p.copy(valueSlot = valueSlot.bind(v)))

    /** The `Double`-shaped binding of a `FormControl[Double]`, so a Rating can carry a
      * `Form.numberField`. The stars remain whole: the value is read rounded and written back as a
      * whole number, which is what [[integer]] asks of this family and what a star count is anyway.
      *
      * `@targetName` because this and the `Int` binding above both erase to `value(Object)`; the
      * JVM name is the only thing that differs, and the call site still reads `value(...)`.
      */
    @targetName("valueNumber")
    def value(v: Double | Signal[Double]): Rating =
        // The setter is the trait's, which takes no Frame. Nothing here runs: a Frame only names where a signal was built.
        given Frame = Frame.internal
        new Rating(p.copy(valueSlot = valueSlot.bind(v match
            case r: SignalRef[Double] @unchecked => r.zoom(Rating.stars)((_, i) => i.toDouble)
            case s: Signal[Double] @unchecked    => s.map(Rating.stars)
            case d: Double                       => Rating.stars(d))))
    end value

    /** Number of stars (Prime default 5). */
    def stars(n: Int): Rating = new Rating(p.copy(starsV = math.max(1, n)))

    /** `readonly`: the stars keep their look, keep their place in the tab order, and stop
      * reacting (`.p-readonly`, `aria-readonly` on the group). Only `disabled` takes them out of
      * reach.
      */
    def readonly(v: Boolean): Rating = new Rating(p.copy(readonlyFlag = v))

    /** Disables the control (`.p-disabled` dimming + no interaction); a `Signal[Boolean]`
      * toggles it reactively (re-renders this small control on change).
      */
    def disabled(v: Boolean | Signal[Boolean]): Rating = new Rating(p.copy(disabledFlag = Present(ReactiveValue(v))))

    /** Native `name` on the hidden per-option radios, which makes the value participate in HTML
      * form submission.
      *
      * Set it for the FORM, not for the keyboard: a mount mints one when you do not, because the
      * name is also what groups the radios, and an ungrouped set of them is five tab stops with no
      * arrows between them rather than the one tab stop this control is.
      */
    def name(v: String): Rating = new Rating(p.copy(nameV = Present(v)))

    /** Glyph for the ACTIVE options (Prime default: the filled star). */
    def onIcon(glyph: IconGlyph): Rating = new Rating(p.copy(onIconV = Present(glyph)))

    /** Glyph for the options beyond the value (Prime default: the outline star). */
    def offIcon(glyph: IconGlyph): Rating = new Rating(p.copy(offIconV = Present(glyph)))

    private[uic] def validitySlots: FieldValidity.Slots                    = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self            = new Rating(p.copy(validityV = v))
    private[uic] def bindRef(ref: SignalRef[Double])(using Frame): Rating  = value(ref)
    private[uic] def hearBlur(f: Double => UI.Action)(using Frame): Rating = onBlur(f)

    /** Accepted for the number binding's whole-number constraint and satisfied by construction: a
      * star count is always whole, so the constraint has nothing left to enforce here.
      * Returns this unchanged rather than storing a flag that could never be false in
      * effect.
      */
    def integer(v: Boolean): Rating = this

    /** Fired with the NEW value (0 on cancel) after the ref write-back. */
    def onChange(f: Int => UI.Action)(using Frame): Rating = new Rating(p.copy(valueSlot = valueSlot.hear(f)))

    /** Fires on focus loss with the current star count: the validation layer's Blur
      * trigger, in the `Double` shape the number family speaks.
      */
    def onBlur(f: Double => UI.Action)(using Frame): Rating = new Rating(p.copy(onBlurDoubleF = Present(Callback.of(f))))

    private def interactive: Boolean = !readonlyFlag && !disabledFlag.constTrue

    private[uic] def render(using Frame): UI =
        // A constant nobody hears is fixed: drawn read-only.
        if valueSlot.fixed && !readonlyFlag then new Rating(p.copy(readonlyFlag = true)).render
        else renderLive

    private def renderLive(using Frame): UI =
        // The radios' `name` is what the browser groups them by, and the grouping is what gives
        // this control its single tab stop and its arrows. Without one every star was its own
        // group: five tab stops, no arrows, and nothing stopping two of them from being checked.
        // A caller's own name wins; otherwise the mount draws one. Unbound, the same mount keeps the stars the reader picks.
        val field =
            if nameV.isDefined && valueSlot.binding.isDefined then renderNamed
            else
                UI.mounted {
                    for
                        value <- valueSlot.owned(0)
                        name  <- nameV.fold(UI.id)(Kyo.lift)
                    yield new Rating(p.copy(valueSlot = value, nameV = Present(name))).renderNamed
                }
        // A value bound one way that nothing hears: every star the reader picks is put back.
        Diagnostics.after(field)("Rating")(Diagnostic.unheard(valueSlot, Rating.valueName, on = !readonlyFlag && !disabledFlag.constTrue))
    end renderLive

    /** The tree the mount publishes once it has a name to group the radios by, the seam golden
      * tests render directly, since a mount shows only its placeholder there.
      */
    private[uic] def wired(name: String)(using Frame): UI = new Rating(p.copy(nameV = Present(name))).renderNamed

    private def renderNamed(using Frame): UI =
        val validity = FieldValidity(validityV)
        validity.follow(BoolValue.reactive(disabledFlag)(d => new Rating(p.copy(disabledFlag = d)).renderResolved(validity)))

    // Bound one way the native radios follow the signal, so kyo-ui puts back a star the owner does not take; the visual layer is
    // drawn from it. A constant somebody hears is the same, and a constant nobody hears is fixed (drawn read-only).
    private def renderResolved(validity: FieldValidity)(using Frame): UI =
        Controlled.drawField(valueSlot, unbound = 0)(f => body(f.value, validity, f.native))

    private def body(value: Int, validity: FieldValidity, native: Int | Signal[Int])(using Frame): UI =
        // The radio a Tab lands on, which is where the caller's id goes: the group itself takes no
        // focus, and `Label.forId`, `Commands.focusId` and focus-first-invalid all need an element
        // that does. A radio group's tab stop is its checked radio, else its first.
        val tabStop           = if value >= 1 && value <= starsV then value else 1
        val options: List[UI] = (1 to starsV).toList.map { i =>
            val active = i <= value
            var opt    = div.cssClass("p-rating-option")
            if active then opt = opt.cssClass("p-rating-option-active")
            if interactive then opt = opt.onClick(activate(i, value))

            // PrimeVue's hidden radio: real checked state, per-star aria-label, and native
            // keyboard semantics: the arrows move within the name group and select as they go,
            // which is the whole reason the group has a name.
            //
            // It carries NO handler of its own. Selecting a radio from the keyboard is an
            // activation: the browser checks it, fires `change`, and dispatches a `click` that
            // bubbles out of the input. A handler on each meant the change wrote the new star, the
            // region re-rendered around it, and the click then reached an option rebuilt with that
            // value in hand, read it as a second pick of the same star and cleared it, so every
            // arrow press ended at zero. The click is the one activation both inputs produce (a
            // press on the visible star reaches only the option, since the radio is clipped away),
            // so it is the one that acts.
            var hidden = radio
                .checked(Controlled.mapNative[Int, Boolean](native)(_ == i))
                .jsProp("value", i.toString)
                .aria("label" -> (if i == 1 then "1 star" else s"$i stars"))
            // Unnamed (the page as first rendered, before the mount mints a name), the browser does not
            // group the radios, so every star would be a tab stop of its own; the group is one stop
            // already, on the star the named group will tab to.
            nameV match
                case Present(n) => hidden = hidden.name(n)
                case Absent     => if i != tabStop then hidden = hidden.tabIndex(-1)
            if i == tabStop then access.id.foreach(v => hidden = hidden.id(v))
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
                    val eff: UI.Action = if e.key == Keyboard.Space then activate(i, value) else ()
                    eff
                }
            end if
            val hiddenSlot: UI = span.cssClass("p-hidden-accessible")(hidden: UI)

            val icon: UI =
                if active then GlyphSvg(onIconV.getOrElse(Icons.starFill), "p-rating-icon", "p-rating-on-icon")
                else GlyphSvg(offIconV.getOrElse(Icons.star), "p-rating-icon", "p-rating-off-icon")
            opt(hiddenSlot, icon)
        }

        // The radios inside form one group, so the box around them says so: an `aria-label` on a
        // roleless div labels nothing in particular.
        var el = div.cssClass("p-rating").cssClass("p-component").role("radiogroup")
        if readonlyFlag then el = el.cssClass("p-readonly").aria("readonly" -> "true").preventActivation
        if disabledFlag.constTrue then el = el.cssClass("p-disabled")
        el = validity.mark(el)
        access.name.foreach(v => el = el.aria("label" -> v))
        onBlurDoubleF.foreach { f =>
            el = el.onBlur(valueSlot.writable match
                case Present(r) => r.use(i => f(i.toDouble))
                case _          => f(value.toDouble))
        }
        el(UI.children(options)*)
    end body

    /** Prime's select semantics: picking the current value clears to 0. */
    private def activate(i: Int, current: Int)(using Frame): UI.Action =
        valueSlot.change(if i == current then 0 else i)
end Rating

object Rating:
    /** How a typed number field turns on the whole-number constraint: through `integer` ([[HasSetter]]). */
    given integer: HasSetter[Rating, "integer", Boolean] with
        def apply(c: Rating, v: Boolean)(using Frame): Rating = c.integer(v)

    final private[uic] case class Props(
        valueSlot: Controlled[Int] = Controlled(),
        starsV: Int = 5,
        readonlyFlag: Boolean = false,
        disabledFlag: Maybe[BoolValue] = Absent,
        nameV: Maybe[String] = Absent,
        onIconV: Maybe[IconGlyph] = Absent,
        offIconV: Maybe[IconGlyph] = Absent,
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        access: Accessibility = Accessibility.none,
        onBlurDoubleF: Maybe[Double => UI.Action] = Absent
    )

    def apply(): Rating = new Rating(Props())

    /** The whole stars of the number family's `Double`. */
    private def stars(d: Double): Int = math.rint(d).toInt

    private val valueName = Controlled.Name("Rating", "value", "a change", "onChange")
end Rating
