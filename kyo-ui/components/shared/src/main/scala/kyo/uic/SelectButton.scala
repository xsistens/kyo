package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** SelectButton — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * SelectButton anatomy: `div.p-selectbutton.p-component[.p-invalid]
  * [role=group]` whose children are full [[ToggleButton]]s — the extracted
  * `@primeuix` selectbutton CSS styles `.p-selectbutton .p-togglebutton`
  * directly, fusing the buttons into one segmented control).
  *
  * Options are TYPED like Select: `options(items)(label)` projects any `A` to
  * its visible text; `optionKey` (defaults to the label projection) supplies
  * the stable key written into the bound ref(s). Single-select binds
  * `value(SignalRef[String])`; `multiple(true)` switches to Set semantics bound
  * via `value(SignalRef[Set[String]])`. `allowEmpty(false)` blocks clearing the
  * last selection (Prime semantics).
  *
  * Both arities are form controls — [[TextFormControl]] single,
  * [[MultiSelectFormControl]] multiple — so `uic.SelectButton[A]().bind(field)`
  * wires value, validity, message and the blur trigger, and the field's value type
  * picks the arity.
  */
final case class SelectButton[A] private (
    items: List[A],
    labelF: A => String,
    keyF: Maybe[A => String] = Absent,
    itemTemplateF: Maybe[A => UI] = Absent,
    optionDisabledF: Maybe[A => Boolean] = Absent,
    valueRef: Maybe[SignalRef[String]] = Absent,
    valuesRef: Maybe[SignalRef[Set[String]]] = Absent,
    multipleFlag: Boolean = false,
    allowEmptyFlag: Boolean = true,
    sizeV: Size = Size.Normal,
    disabledFlag: Maybe[BoolValue] = Absent,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    onChangeF: Maybe[String => Any < Async] = Absent,
    onBlurF: Maybe[String => Any < Async] = Absent,
    onBlurSetF: Maybe[Set[String] => Any < Async] = Absent,
    idV: Maybe[String] = Absent
) extends Node, TextFormControl, MultiSelectFormControl, HasAccessibleNameRef:
    type Self = SelectButton[A]

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): SelectButton[A] = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Appends typed options with their text projection. */
    def options(is: Seq[A])(label: A => String): SelectButton[A] =
        copy(items = items ++ is.toList, labelF = label)

    /** Convenience for plain string options (label = identity). */
    def options(is: Seq[String])(using ev: String =:= A): SelectButton[A] =
        copy(items = items ++ is.map(ev).toList, labelF = a => ev.flip(a))

    /** Stable per-option key — the value written into the bound ref(s); defaults
      * to the label projection.
      */
    def optionKey(f: A => String): SelectButton[A] = copy(keyF = Present(f))

    /** Custom option content (Prime's option slot): arbitrary UI rendered inside
      * each fused button instead of the text projection. The label projection
      * stays the accessibility name and — via [[optionKey]] — the selection key.
      */
    def itemTemplate(f: A => UI): SelectButton[A] = copy(itemTemplateF = Present(f))

    /** Renders matching options disabled. */
    def optionDisabled(f: A => Boolean): SelectButton[A] = copy(optionDisabledF = Present(f))

    /** Binds the SINGLE selection two-way to `ref` (empty string = no selection). */
    def value(ref: SignalRef[String]): SelectButton[A] = copy(valueRef = Present(ref))

    /** Switches to multi-select semantics (pair with the `Set` [[value]] binding). */
    def multiple(v: Boolean): SelectButton[A] = copy(multipleFlag = v)

    /** Binds the MULTI selection two-way to `ref` (a set of option keys). Spelled `value`
      * like the single-select binding — the [[MultiSelectFormControl]] member — because
      * they are one concept at two arities, and the type already says which is which.
      */
    @targetName("valueKeys")
    def value(ref: SignalRef[Set[String]]): SelectButton[A] = copy(valuesRef = Present(ref))

    /** Whether clicking the selected option may clear the selection entirely
      * (Prime default true).
      */
    def allowEmpty(v: Boolean): SelectButton[A] = copy(allowEmptyFlag = v)

    /** Size of the fused buttons: `.p-togglebutton-sm` / default / `-lg`. */
    def size(v: Size): SelectButton[A] = copy(sizeV = v)

    /** Disables the whole group; a `Signal[Boolean]` toggles it reactively (re-render). */
    def disabled(v: Boolean | Signal[Boolean]): SelectButton[A] = copy(disabledFlag = Present(ReactiveValue(v)))

    private[uic] def withInvalid(v: Maybe[BoolValue]): SelectButton[A]                       = copy(invalidV = v)
    private[uic] def withInvalidMessage(v: Maybe[String]): SelectButton[A]                   = copy(invalidMsgV = v)
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): SelectButton[A] = copy(invalidMsgDynV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): SelectButton[A] = copy(accNameV = v)
    private[uic] def withAccessibleNameRef(v: Maybe[String]): SelectButton[A] = copy(accNameRefV = v)

    /** Fired with the clicked option's key after the selection write. */
    def onChange(f: String => Any < Async): SelectButton[A] = copy(onChangeF = Present(f))

    /** Fires on focus loss with the SINGLE selection (empty string = none) — the
      * validation layer's Blur trigger for a `String`-valued field.
      */
    def onBlur(f: String => Any < Async): SelectButton[A] = copy(onBlurF = Present(f))

    /** Fires on focus loss with the MULTI selection — the Blur trigger for a
      * `Set[String]`-valued field. `@targetName` because both erase to `onBlur(Function1)`.
      */
    @targetName("onBlurKeys")
    def onBlur(f: Set[String] => Any < Async): SelectButton[A] = copy(onBlurSetF = Present(f))

    /** The stable option key: [[optionKey]] if set, else the label projection. */
    private def key(a: A): String = keyF.getOrElse(labelF)(a)

    /** The ids the group moves focus between, and the way to move it.
      *
      * Only a single-select group has them: it is a radio group, so it holds ONE tab stop and the
      * arrows move within it, and moving focus needs an id to move it to.
      */
    private type Nav = Maybe[(List[String], String => Any < Async)]

    private[uic] def render(using Frame): UI =
        // Multiple select is a group of independent toggles, each its own tab stop, so it needs no
        // ids and no mount. Single select is a radio group, and the mount is where its ids come from.
        if multipleFlag then invalidGate(Absent)
        else
            UI.mounted {
                UI.commands.map { cmds =>
                    Kyo.foreach(items)(_ => cmds.freshId).map(ids =>
                        invalidGate(Present((ids.toList, (id: String) => cmds.focusId(id))))
                    )
                }
            }.placeholder(invalidGate(Absent))

    /** The seam the golden tests render, since a mount shows only its placeholder there. */
    private[uic] def wired(ids: List[String], focus: String => Any < Async)(using Frame): UI =
        invalidGate(Present((ids, focus)))

    private def invalidGate(nav: Nav)(using Frame): UI =
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => renderDisabledResolved(nav)
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                    copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent)
                        .renderDisabledResolved(nav)
                )

    private def renderDisabledResolved(nav: Nav)(using Frame): UI =
        BoolValue.reactive(disabledFlag): d =>
            copy(disabledFlag = d).renderResolved(nav)

    private def renderResolved(nav: Nav)(using Frame): UI =
        (valueRef, valuesRef) match
            case (Present(r), _) if !multipleFlag => r.render(v => body(if v.isEmpty then Set.empty else Set(v), nav))
            case (_, Present(r)) if multipleFlag  => r.render(body(_, nav))
            case _                                => body(Set.empty, nav)

    /** Writes `k` as THE selection, without the click's clear-on-same-value.
      *
      * An arrow moves and selects in one gesture (the radio pattern), so it must never land on an
      * option and clear it: Home pressed on the option already chosen would empty the group.
      */
    private def selectOnly(k: String)(using Frame): Any < Async =
        val write: Any < Async = valueRef match
            case Present(ref) => ref.set(k)
            case Absent       => ()
        val fire: Any < Async = onChangeF match
            case Present(f) => f(k)
            case Absent     => ()
        write.andThen(fire)
    end selectOnly

    /** One key on a single-select option: the arrows move focus to the next option a highlight may
      * sit on and select it, Home and End reach the ends the same way, and the group wraps as the
      * ARIA radio pattern does. Enter and Space are left alone: the option is a `<button>`, so the
      * browser and the dispatcher already agree on one activation between them.
      */
    private def optionKey(ids: List[String], navigable: List[Int], self: Int, focus: String => Any < Async)(
        e: KeyboardEvent
    )(using Frame): Any < Async =
        ListNav.onKey(navigable, self, e.key, wrap = true, ListNav.Orientation.Both) match
            case Present(step) if step.focus != self && ids.isDefinedAt(step.focus) && items.isDefinedAt(step.focus) =>
                focus(ids(step.focus)).andThen(selectOnly(key(items(step.focus))))
            case _ => ()

    private def body(selected: Set[String], nav: Nav)(using Frame): UI =
        var el = div.cssClass("p-selectbutton").cssClass("p-component")
        // A choice of ONE is a radio group: it says so, holds one tab stop, and moves inside it
        // with the arrows. Several independent choices stay a group of toggle buttons.
        el = el.role(if multipleFlag then "group" else "radiogroup")
        idV.foreach(v => el = el.id(v))
        if invalidV.constTrue then el = el.cssClass("p-invalid").aria("invalid", "true")
        // Blur fires even without a pick — the validation layer's Blur trigger. Reads the
        // bound ref LIVE, so it reports the selection at blur time. The element carries ONE
        // blur handler, so the arity in force decides which of the two is wired; a control
        // that declared both would otherwise silently keep whichever was applied last.
        val blurHandler: Maybe[Any < Async] =
            if multipleFlag then
                onBlurSetF.map(f =>
                    valuesRef match
                        case Present(r) => r.use(f)
                        case Absent     => f(Set.empty)
                )
            else
                onBlurF.map(f =>
                    valueRef match
                        case Present(r) => r.use(f)
                        case Absent     => f("")
                )
        blurHandler.foreach(h => el = el.onBlur(h))
        accNameV match
            case Present(TextValue.Const(v)) => el = el.aria("label", v)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        accNameRefV.foreach(v => el = el.aria("labelledby", v))

        // The positions an arrow may land on: a disabled option is out of the tab order, so the
        // movement steps over it.
        val navigable = items.zipWithIndex.collect {
            case (a, i) if !(disabledFlag.constTrue || optionDisabledF.exists(_(a))) => i
        }
        // The one tab stop of a radio group sits on the selected option, and on the first one a
        // reader may choose while nothing is selected: a group nobody has answered yet is still a
        // group they can tab into.
        val tabStop = items.indexWhere(a => selected.contains(key(a))) match
            case -1 => navigable.headOption.getOrElse(-1)
            case i  => i

        val buttons: List[UI] = items.zipWithIndex.map { (a, i) =>
            val k     = key(a)
            val label = labelF(a)
            var tb = ToggleButton()
                .checked(selected.contains(k))
                .onLabel(label)
                .offLabel(label)
                .size(sizeV)
            itemTemplateF.foreach(t => tb = tb.content(t(a)).accessibleName(label))
            if disabledFlag.constTrue || optionDisabledF.exists(_(a)) then tb = tb.disabled(true)
            else tb = tb.onChange(_ => activate(k))
            if !multipleFlag then
                nav match
                    case Present((ids, focus)) if ids.isDefinedAt(i) =>
                        tb = tb.id(ids(i)).asRadioOption(i == tabStop, optionKey(ids, navigable, i, focus))
                    // No mount, no ids: the static projection reports the role and the state, and
                    // every option keeps its own tab stop, which is what it had before the arrows.
                    case _ => tb = tb.asRadioOption(tabbable = true, _ => ())
            end if
            tb.render
        }
        FieldInvalid.withMessage(
            el((keyCollisionCard ++ buttons).map(toChild)*),
            invalidV.constTrue,
            invalidMsgV
        )
    end body

    /** The loud card rendered ahead of the fused buttons when the option keys
      * collide (see [[KeyDiagnostics]]): two options sharing a key toggle together.
      */
    private def keyCollisionCard(using Frame): List[UI] =
        val dups = KeyDiagnostics.duplicates(items.map(key))
        if dups.isEmpty then Nil
        else
            List(KeyDiagnostics.card(
                "SelectButton",
                if keyF.isEmpty then "option labels are not unique and optionKey is unset, so those options select together; set optionKey"
                else "optionKey is not unique across the options, so those options select together",
                dups
            ))
        end if
    end keyCollisionCard

    /** Clicking an option updates the bound selection (single replaces, multiple
      * toggles; `allowEmpty` gates clearing the last pick), then fires `onChange`.
      */
    private def activate(k: String)(using Frame): Any < Async =
        val write: Any < Async =
            if multipleFlag then
                valuesRef match
                    case Present(ref) =>
                        ref.getAndUpdate { cur =>
                            if cur.contains(k) then if allowEmptyFlag || cur.size > 1 then cur - k else cur
                            else cur + k
                        }
                    case Absent => ()
            else
                valueRef match
                    case Present(ref) =>
                        ref.getAndUpdate(cur => if cur == k then (if allowEmptyFlag then "" else cur) else k)
                    case Absent => ()
        val fire: Any < Async = onChangeF match
            case Present(f) => f(k)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end activate
end SelectButton

object SelectButton:
    def apply[A](): SelectButton[A] = new SelectButton[A](Nil, _.toString)
