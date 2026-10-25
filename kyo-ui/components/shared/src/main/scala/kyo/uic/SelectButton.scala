package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** SelectButton: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * SelectButton anatomy: `div.p-selectbutton.p-component[.p-invalid]
  * [role=group]` whose children are full [[ToggleButton]]s; the extracted
  * `@primeuix` selectbutton CSS styles `.p-selectbutton .p-togglebutton`
  * directly, fusing the buttons into one segmented control).
  *
  * Options are TYPED like Select: `options(items)(label)` projects any `A` to
  * its visible text; `optionKey` (defaults to the label projection) supplies
  * the stable key options are matched by. A SelectButton chooses ONE option:
  * `selection(ref)` binds it over `A`, or over `Maybe[A]` where nothing may be
  * chosen, and `optionsWithNone` adds a labelled none option to a group over
  * `Maybe`. `allowEmpty(false)` blocks clearing the last selection (Prime
  * semantics). `multiple` turns the group into a [[MultiSelectButton]], a type of
  * its own because what it holds is a `Set[A]`: each type has one binding and one
  * `onChange` of exactly its value type, and a set bound to a single-choice group
  * does not compile.
  *
  * It is a [[ChoiceFormControl]], so `uic.SelectButton[A]().bind(field)` wires the
  * selection, validity, message and the blur trigger for a `FormField[A]` or a
  * `FormField[Maybe[A]]`.
  *
  * `id` lands on the option a Tab reaches, since the group itself takes no focus: the selected option of a
  * single group (the first enabled one while nothing is selected), the first enabled option of a [[multiple]] one.
  * It moves with the selection, and that option's minted id gives way to it.
  */
final case class SelectButton[A] private (
    items: List[A],
    labelF: A => String,
    keyF: Maybe[A => String] = Absent,
    itemTemplateF: Maybe[A => UI] = Absent,
    optionDisabledF: Maybe[A => Boolean] = Absent,
    single: Maybe[SingleChoice[A]] = Absent,
    multi: Maybe[MultiChoice[A]] = Absent,
    multipleFlag: Boolean = false,
    allowEmptyFlag: Boolean = true,
    sizeV: Size = Size.Normal,
    disabledFlag: Maybe[BoolValue] = Absent,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    onChangeF: Maybe[Maybe[A] => UI.Action] = Absent,
    onPickF: Maybe[A => UI.Action] = Absent,
    onChangeSetF: Maybe[Set[A] => UI.Action] = Absent,
    onBlurF: Maybe[Maybe[A] => UI.Action] = Absent,
    onBlurSetF: Maybe[Set[A] => UI.Action] = Absent,
    loadingV: Maybe[BoolValue] = Absent,
    idV: Maybe[String] = Absent
) extends Node, ChoiceFormControl[A], HasAccessibleNameRef:
    type Self = SelectButton[A]

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): SelectButton[A] = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Appends typed options with their text projection. */
    def options(is: Iterable[A])(label: A => String): SelectButton[A] =
        copy(items = items ++ is.toList, labelF = label)

    /** Convenience for plain string options (label = identity). */
    def options(is: Iterable[String])(using ev: String =:= A): SelectButton[A] =
        copy(items = items ++ is.map(ev).toList, labelF = a => ev.flip(a))

    /** Stable per-option key: the value written into the bound ref(s); defaults
      * to the label projection.
      */
    def optionKey(f: A => String): SelectButton[A] = copy(keyF = Present(f))

    /** Custom option content (Prime's option slot): arbitrary UI rendered inside
      * each fused button instead of the text projection. The label projection
      * stays the accessibility name and (via [[optionKey]]) the selection key.
      */
    def itemTemplate(f: A => UI): SelectButton[A] = copy(itemTemplateF = Present(f))

    /** Renders matching options disabled. */
    def optionDisabled(f: A => Boolean): SelectButton[A] = copy(optionDisabledF = Present(f))

    /** Appends `none` and then `is` as the options of a group over `Maybe[B]`: `Absent` first, labelled `none`, then
      * `Present(b)` for each value, labelled `label(b)`. An [[itemTemplate]] sees the none option too. Only a group whose
      * options are `Maybe[B]` takes it.
      */
    def optionsWithNone[B](none: String, is: Iterable[B])(label: B => String)(using ev: Maybe[B] =:= A): SelectButton[A] =
        val all: List[Maybe[B]] = (Absent: Maybe[B]) :: is.toList.map(b => Present(b): Maybe[B])
        copy(items = items ++ all.map(ev).toList, labelF = a => ev.flip(a).fold(none)(label))
    end optionsWithNone

    /** Binds the SINGLE selection to the option itself, matched by [[optionKey]]. The value always holds an option, so a
      * click on the chosen one keeps it whatever [[allowEmpty]] says. A `SignalRef` binds two-way. Any other `Signal`, or a
      * constant, binds ONE-WAY: the field shows it, and a pick writes nothing and only reports through [[onChange]] and
      * [[onPick]], so the app writes its own state (a route, say) and the field follows. `ref.readOnly` opts a ref out.
      */
    def selection(v: A | Signal[A]): SelectButton[A] = copy(single = Present(SingleChoice.Value(ReactiveValue(v))))

    /** Binds the SINGLE selection to the option or none: a click on the chosen option clears it where [[allowEmpty]]
      * allows it. Two-way and one-way as for the option binding above.
      */
    @targetName("selectionMaybe")
    def selection(v: Maybe[A] | Signal[Maybe[A]]): SelectButton[A] =
        copy(single = Present(SingleChoice.Optional(ReactiveValue(v))))

    /** The same group with several options on at once: a [[MultiSelectButton]], which binds a `Set[A]`. What is set so far
      * (options, templates, state) carries over; a single-choice binding and its events do not, since they hold one option.
      */
    def multiple: MultiSelectButton[A] =
        MultiSelectButton(copy(multipleFlag = true, single = Absent, onChangeF = Absent, onPickF = Absent, onBlurF = Absent))

    /** Package-internal: the multi-choice half [[MultiSelectButton]] drives. */
    private[uic] def withMulti(ref: SignalRef[Set[A]]): SelectButton[A]                  = copy(multi = Present(MultiChoice(ref)))
    private[uic] def withChangeSet(f: Set[A] => UI.Action)(using Frame): SelectButton[A] = copy(onChangeSetF = Present(Callback.of(f)))
    private[uic] def withBlurSet(f: Set[A] => UI.Action)(using Frame): SelectButton[A]   = copy(onBlurSetF = Present(Callback.of(f)))

    /** Says the options are still arriving (`aria-busy` on the group): a held option no button carries yet is left
      * alone rather than reported in a `.p-uic-key-error` card. Prime's SelectButton has no loading look, so none is drawn.
      */
    def loading(v: Boolean | Signal[Boolean]): SelectButton[A] = copy(loadingV = Present(ReactiveValue(v)))

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

    /** Fired with the selection now held, after a click or an arrow changed it (`Absent` where a click cleared it).
      * A click that changes nothing fires nothing.
      */
    def onChange(f: Maybe[A] => UI.Action)(using Frame): SelectButton[A] = copy(onChangeF = Present(Callback.of(f)))

    /** Fired with the option picked, after a click or an arrow that changed the selection to it: [[onChange]] without the
      * `Maybe` and without the clear, for a group that is never empty or a one-way [[selection]] that writes its own state
      * here. Fires after [[onChange]] when both are set.
      */
    def onPick(f: A => UI.Action)(using Frame): SelectButton[A] = copy(onPickF = Present(Callback.of(f)))

    /** Fires on focus loss with the selection held (`Absent` for none): the validation layer's Blur trigger. */
    def onBlur(f: Maybe[A] => UI.Action)(using Frame): SelectButton[A] = copy(onBlurF = Present(Callback.of(f)))

    /** The stable option key: [[optionKey]] if set, else the label projection. */
    private def key(a: A): String = keyF.getOrElse(labelF)(a)

    // Lets the one region skip a write that holds what it already showed; options are still matched by key.
    private given CanEqual[A, A] = CanEqual.derived

    /** The ids the group moves focus between, and the way to move it.
      *
      * Only a single-select group has them: it is a radio group, so it holds ONE tab stop and the
      * arrows move within it, and moving focus needs an id to move it to.
      */
    private type Nav = Maybe[(List[String], String => UI.Action)]

    private[uic] def render(using Frame): UI =
        // Multiple select is a group of independent toggles, each its own tab stop, so it needs no
        // ids and no mount. Single select is a radio group, and the mount is where its ids come from.
        if multipleFlag then renderNav(Absent)
        else
            UI.mounted {
                UI.commands.map { cmds =>
                    Kyo.foreach(items)(_ => cmds.freshId).map(ids =>
                        renderNav(Present((ids.toList, (id: String) => cmds.focusId(id))))
                    )
                }
            }.placeholder(renderNav(Absent))

    /** The seam the golden tests render, since a mount shows only its placeholder there. */
    private[uic] def wired(ids: List[String], focus: String => UI.Action)(using Frame): UI =
        renderNav(Present((ids, focus)))

    private def renderNav(nav: Nav)(using Frame): UI =
        val validity = FieldValidity(invalidV, invalidMsgV, invalidMsgDynV)
        // The switches and the selection are read as ONE region (see [[Reading]]), pinned back into the slots as constants
        // so the body reads plain values.
        val read = Reading.optional(disabledFlag).and(Reading.optional(loadingV)).and(held)
        validity.follow(read.render { case ((d, l), h) =>
            copy(disabledFlag = d.map(BoolValue.Const(_)), loadingV = l.map(BoolValue.Const(_))).body(h, nav, validity)
        })
    end renderNav

    /** The options held, whichever binding holds them. */
    private def held(using Frame): Reading[List[A]] =
        (single, multi) match
            case (Present(c), _) if !multipleFlag => c.reading.map(_.toList)
            case (_, Present(c)) if multipleFlag  => Reading.Live(c.ref.map(_.toList))
            case _                                => Reading.Fixed(Nil)

    /** Writes `k` as THE selection, without the click's clear-on-same-value.
      *
      * An arrow moves and selects in one gesture (the radio pattern), so it must never land on an
      * option and clear it: Home pressed on the option already chosen would empty the group.
      */
    private def selectOnly(a: A)(using Frame): UI.Action =
        stepSingle(_ => Present(a))

    /** Applies `f` to the single selection through the binding, then fires `onChange` and `onPick` where that changed
      * what it holds ([[SingleChoice.report]]). Unbound, there is nothing held, and `f` of none is reported as it is.
      */
    private def stepSingle(f: Maybe[A] => Maybe[A])(using Frame): UI.Action =
        val changed: Maybe[Maybe[A]] < Sync =
            single match
                case Present(c) => c.update(key)(f)
                case Absent     => Present(f(Absent))
        changed.map(SingleChoice.report(_, onChangeF, onPickF))
    end stepSingle

    /** One key on a single-select option: the arrows move focus to the next option a highlight may
      * sit on and select it, Home and End reach the ends the same way, and the group wraps as the
      * ARIA radio pattern does. Enter and Space are left alone: the option is a `<button>`, so the
      * browser and the dispatcher already agree on one activation between them.
      */
    private def optionKey(ids: List[String], navigable: List[Int], self: Int, focus: String => UI.Action)(
        e: KeyboardEvent
    )(using Frame): UI.Action =
        ListNav.onKey(navigable, self, e.key, wrap = true, ListNav.Orientation.Both) match
            case Present(step) if step.focus != self && ids.isDefinedAt(step.focus) && items.isDefinedAt(step.focus) =>
                focus(ids(step.focus)).andThen(selectOnly(items(step.focus)))
            case _ => ()

    private def body(held: List[A], nav: Nav, validity: FieldValidity)(using Frame): UI =
        val selected = held.map(key).toSet
        // Held options no button carries, once the options are said to be complete.
        val unknown = if loadingV.constTrue then Nil else held.filterNot(a => items.exists(i => key(i) == key(a)))
        var el      = div.cssClass("p-selectbutton").cssClass("p-component")
        if loadingV.constTrue then el = el.aria("busy", "true")
        // A choice of ONE is a radio group: it says so, holds one tab stop, and moves inside it
        // with the arrows. Several independent choices stay a group of toggle buttons.
        el = el.role(if multipleFlag then "group" else "radiogroup")
        el = validity.mark(el)
        // Blur fires even without a pick: the validation layer's Blur trigger. Reads the
        // bound ref LIVE, so it reports the selection at blur time. The element carries ONE
        // blur handler, so the arity in force decides which of the two is wired; a control
        // that declared both would otherwise silently keep whichever was applied last.
        val blurHandler: Maybe[UI.Action] =
            if multipleFlag then
                onBlurSetF.map(f =>
                    multi match
                        case Present(c) => c.read(f)
                        case Absent     => f(Set.empty)
                )
            else
                onBlurF.map(f =>
                    single match
                        case Present(c) => c.read(f)
                        case Absent     => f(Absent)
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

        // The caller's id goes on the option a reader lands on by Tab, since a group takes no focus
        // of its own and `Label.forId`, `Commands.focusId` and focus-first-invalid all need an element
        // that does: the radio group's one tab stop, or the first option of a group of toggles. That
        // option's own id gives way to it, and the arrows move focus through the same list, so the
        // id an arrow targets is always the one the option is rendered with.
        val idAt: Int                      = if multipleFlag then navigable.headOption.getOrElse(0) else tabStop
        val optionIds: Maybe[List[String]] = nav.map { (ids, _) =>
            idV match
                case Present(v) if ids.isDefinedAt(idAt) => ids.updated(idAt, v)
                case _                                   => ids
        }

        val buttons: List[UI] = items.zipWithIndex.map { (a, i) =>
            val k     = key(a)
            val label = labelF(a)
            var tb    = ToggleButton()
                .checked(selected.contains(k))
                .onLabel(label)
                .offLabel(label)
                .size(sizeV)
            itemTemplateF.foreach(t => tb = tb.content(t(a)).accessibleName(label))
            if disabledFlag.constTrue || optionDisabledF.exists(_(a)) then tb = tb.disabled(true)
            else tb = tb.onChange(_ => activate(a))
            if !multipleFlag then
                (nav, optionIds) match
                    case (Present((_, focus)), Present(ids)) if ids.isDefinedAt(i) =>
                        tb = tb.id(ids(i)).asRadioOption(i == tabStop, optionKey(ids, navigable, i, focus))
                    // No mount, no ids: the page as first rendered has no arrows yet, but it is already
                    // the one tab stop the attached group is, on the option that carries the id.
                    case _ =>
                        tb = tb.asRadioOption(i == tabStop, _ => ())
                        if i == idAt then idV.foreach(v => tb = tb.id(v))
            else if i == idAt then idV.foreach(v => tb = tb.id(v))
            end if
            tb.render
        }
        el((keyCollisionCard ++ unknownCard(unknown) ++ buttons).map(toChild)*)
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

    /** The card naming held options no button carries (see [[KeyDiagnostics.unknown]]). */
    private def unknownCard(unknown: List[A])(using Frame): List[UI] =
        if unknown.isEmpty then Nil else List(KeyDiagnostics.unknown("SelectButton", unknown.map(labelF)))

    /** Clicking an option updates the bound selection (single replaces, multiple
      * toggles; `allowEmpty` gates clearing the last pick), then fires `onChange`
      * with what the selection now holds, where the click changed it.
      */
    private def activate(a: A)(using Frame): UI.Action =
        val k = key(a)
        if multipleFlag then
            val changed: Maybe[Set[A]] < Sync =
                multi match
                    case Present(c) =>
                        c.update(items, key) { cur =>
                            if cur.contains(k) then if allowEmptyFlag || cur.size > 1 then cur - k else cur
                            else cur + k
                        }
                    case Absent => Present(Set(a))
            changed.map {
                case Present(now) =>
                    onChangeSetF match
                        case Present(g) => g(now)
                        case Absent     => (): UI.Action
                case Absent => (): UI.Action
            }
        else stepSingle(held => if held.exists(h => key(h) == k) then (if allowEmptyFlag then Absent else held) else Present(a))
        end if
    end activate
end SelectButton

object SelectButton:
    def apply[A](): SelectButton[A] = new SelectButton[A](Nil, _.toString)

/** A [[SelectButton]] where several options may be on at once (Prime's `multiple`): a row of independent toggle buttons,
  * each its own tab stop, bound to the `Set[A]` of the options that are on. Made by [[SelectButton.multiple]], and a type
  * of its own because its value is a set: it has one binding and one `onChange`, both over `Set[A]`, and it is a
  * [[MultiChoiceFormControl]], so `bind` takes a `FormField[Set[A]]`.
  *
  * It renders through the group it was made from, so the two share every option, template and state setter and the
  * same anatomy.
  */
final case class MultiSelectButton[A] private[uic] (private[uic] val group: SelectButton[A])
    extends Node, MultiChoiceFormControl[A], HasAccessibleNameRef:
    type Self = MultiSelectButton[A]

    private def on(g: SelectButton[A]): MultiSelectButton[A] = copy(group = g)

    /** Appends typed options with their text projection, as [[SelectButton.options]]. */
    def options(is: Iterable[A])(label: A => String): MultiSelectButton[A] = on(group.options(is)(label))

    /** Convenience for plain string options (label = identity). */
    def options(is: Iterable[String])(using ev: String =:= A): MultiSelectButton[A] = on(group.options(is))

    /** Stable per-option key, what options are matched by; defaults to the label projection. */
    def optionKey(f: A => String): MultiSelectButton[A] = on(group.optionKey(f))

    /** Custom option content, as [[SelectButton.itemTemplate]]. */
    def itemTemplate(f: A => UI): MultiSelectButton[A] = on(group.itemTemplate(f))

    /** Renders matching options disabled. */
    def optionDisabled(f: A => Boolean): MultiSelectButton[A] = on(group.optionDisabled(f))

    /** Binds the set of chosen options two-way. A member no option carries stays in the set while the others toggle. */
    @targetName("selectionSet")
    def selection(ref: SignalRef[Set[A]]): MultiSelectButton[A] = on(group.withMulti(ref))

    /** Fired with the set now held, after a click changed it. */
    def onChange(f: Set[A] => UI.Action)(using Frame): MultiSelectButton[A] = on(group.withChangeSet(f))

    /** Fires on focus loss with the set held: the validation layer's Blur trigger. */
    @targetName("onBlurSet")
    def onBlur(f: Set[A] => UI.Action)(using Frame): MultiSelectButton[A] = on(group.withBlurSet(f))

    /** Whether clicking the last option on may turn it off (Prime default true). */
    def allowEmpty(v: Boolean): MultiSelectButton[A] = on(group.allowEmpty(v))

    /** Says the options are still arriving, as [[SelectButton.loading]]. */
    def loading(v: Boolean | Signal[Boolean]): MultiSelectButton[A] = on(group.loading(v))

    /** Size of the fused buttons. */
    def size(v: Size): MultiSelectButton[A] = on(group.size(v))

    /** Disables the whole group; a `Signal[Boolean]` toggles it reactively. */
    def disabled(v: Boolean | Signal[Boolean]): MultiSelectButton[A] = on(group.disabled(v))

    private[uic] def withElementId(v: Maybe[String]): MultiSelectButton[A] = on(group.withElementId(v))
    private[uic] def elementId: Maybe[String]                              = group.elementId

    private[uic] def withInvalid(v: Maybe[BoolValue]): MultiSelectButton[A]                       = on(group.withInvalid(v))
    private[uic] def withInvalidMessage(v: Maybe[String]): MultiSelectButton[A]                   = on(group.withInvalidMessage(v))
    private[uic] def withInvalidMessageDyn(v: Maybe[Signal[Maybe[String]]]): MultiSelectButton[A] = on(group.withInvalidMessageDyn(v))

    private[uic] def withAccessibleName(v: Maybe[TextValue]): MultiSelectButton[A] = on(group.withAccessibleName(v))
    private[uic] def withAccessibleNameRef(v: Maybe[String]): MultiSelectButton[A] = on(group.withAccessibleNameRef(v))

    private[uic] def render(using Frame): UI = group.render
end MultiSelectButton
