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
  * It is a `FormControl[Maybe[A]]` with a `selection` over `A`, so `uic.SelectButton[A]().bind(field)` wires the
  * selection, validity, message and the blur trigger for a `FormField[A]` or a
  * `FormField[Maybe[A]]`.
  *
  * `id` lands on the option a Tab reaches, since the group itself takes no focus: the selected option of a
  * single group (the first enabled one while nothing is selected), the first enabled option of a [[multiple]] one.
  * It moves with the selection, and that option's minted id gives way to it.
  */
final class SelectButton[A] private[uic] (private[uic] val p: SelectButton.Props[A]) extends Node, FormControl[Maybe[A]],
      HasAccessibleNameRef, HasProps:
    import p.*

    type Self = SelectButton[A]

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new SelectButton(p.copy(access = v))

    /** Appends typed options with their text projection: a collection, or a signal of options for a group whose choices
      * change while it is shown, read in the group's own region.
      */
    def options(is: Iterable[A] | Signal[Seq[A]])(label: A => String): SelectButton[A] =
        new SelectButton(p.copy(optionSources = optionSources :+ OptionSource.of(is), labelF = label))

    /** Convenience for plain string options (label = identity), a collection or a signal. */
    def options(is: Iterable[String] | Signal[Seq[String]])(using ev: String =:= A): SelectButton[A] =
        new SelectButton(p.copy(
            optionSources = optionSources :+ OptionSource.of(ev.substituteCo[[X] =>> Iterable[X] | Signal[Seq[X]]](is)),
            labelF = a => ev.flip(a)
        ))

    /** Stable per-option key: the value written into the bound ref(s); defaults
      * to the label projection.
      */
    def optionKey(f: A => String): SelectButton[A] = new SelectButton(p.copy(keyF = Present(f)))

    /** Custom option content (Prime's option slot): arbitrary UI rendered inside
      * each fused button instead of the text projection. The label projection
      * stays the accessibility name and (via [[optionKey]]) the selection key.
      */
    def itemTemplate(f: A => UI): SelectButton[A] = new SelectButton(p.copy(itemTemplateF = Present(f)))

    /** Renders matching options disabled. */
    def optionDisabled(f: A => Boolean): SelectButton[A] = new SelectButton(p.copy(optionDisabledF = Present(f)))

    /** Appends `none` and then `is` as the options of a group over `Maybe[B]`: `Absent` first, labelled `none`, then
      * `Present(b)` for each value, labelled `label(b)`. An [[itemTemplate]] sees the none option too. Only a group whose
      * options are `Maybe[B]` takes it.
      */
    def optionsWithNone[B](none: String, is: Iterable[B])(label: B => String)(using ev: Maybe[B] =:= A): SelectButton[A] =
        val all: List[Maybe[B]] = (Absent: Maybe[B]) :: is.toList.map(b => Present(b): Maybe[B])
        new SelectButton(p.copy(
            optionSources = optionSources :+ OptionSource.Fixed(all.map(m => OptionItem.Item(ev(m)))),
            labelF = a => ev.flip(a).fold(none)(label)
        ))
    end optionsWithNone

    /** Binds the SINGLE selection to the option itself, matched by [[optionKey]]. The value always holds an option, so a
      * click on the chosen one keeps it whatever [[allowEmpty]] says. A `SignalRef` binds two-way. Any other `Signal`, or a
      * constant, binds ONE-WAY: the field shows it, and a pick writes nothing and only reports through [[onChange]] and
      * [[onPick]], so the app writes its own state (a route, say) and the field follows. `ref.readOnly` opts a ref out.
      * With neither event set, a card says the reader cannot change it.
      */
    def selection(v: A | Signal[A]): SelectButton[A] = new SelectButton(p.copy(single = Present(SingleChoice.Value(ReactiveValue(v)))))

    /** Binds the SINGLE selection to the option or none: a click on the chosen option clears it where [[allowEmpty]]
      * allows it. Two-way and one-way as for the option binding above.
      */
    @targetName("selectionMaybe")
    def selection(v: Maybe[A] | Signal[Maybe[A]]): SelectButton[A] =
        new SelectButton(p.copy(single = Present(SingleChoice.Optional(ReactiveValue(v)))))

    /** The same group with several options on at once: a [[MultiSelectButton]], which binds a `Set[A]`. What is set so far
      * (options, templates, state) carries over; a single-choice binding and its events do not, since they hold one option.
      */
    def multiple: MultiSelectButton[A] =
        new MultiSelectButton(new SelectButton(p.copy(
            multipleFlag = true,
            single = Absent,
            onChangeF = Absent,
            onPickF = Absent,
            onBlurF = Absent
        )))

    /** Package-internal: the multi-choice half [[MultiSelectButton]] drives. */
    private[uic] def withMulti(v: Set[A] | Signal[Set[A]]): SelectButton[A] =
        new SelectButton(p.copy(multi = Present(MultiChoice(ReactiveValue[Set[A]](v)))))
    private[uic] def withChangeSet(f: Set[A] => UI.Action)(using Frame): SelectButton[A] =
        new SelectButton(p.copy(onChangeSetF = Present(Callback.of(f))))
    private[uic] def withBlurSet(f: Set[A] => UI.Action)(using Frame): SelectButton[A] =
        new SelectButton(p.copy(onBlurSetF = Present(Callback.of(f))))

    /** Says the options are still arriving (`aria-busy` on the group): a held option no button carries yet is left
      * alone rather than reported in a `.p-uic-diagnostic` card. Prime's SelectButton has no loading look, so none is drawn.
      */
    def loading(v: Boolean | Signal[Boolean]): SelectButton[A] = new SelectButton(p.copy(loadingV = Present(ReactiveValue(v))))

    /** Whether clicking the selected option may clear the selection entirely
      * (Prime default true).
      */
    def allowEmpty(v: Boolean): SelectButton[A] = new SelectButton(p.copy(allowEmptyFlag = v))

    /** Size of the fused buttons: `.p-togglebutton-sm` / default / `-lg`. */
    def size(v: Size): SelectButton[A] = new SelectButton(p.copy(sizeV = v))

    /** Disables the whole group; a `Signal[Boolean]` toggles it reactively (re-render). */
    def disabled(v: Boolean | Signal[Boolean]): SelectButton[A] = new SelectButton(p.copy(disabledFlag = Present(ReactiveValue(v))))

    private[uic] def validitySlots: FieldValidity.Slots                               = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self                       = new SelectButton(p.copy(validityV = v))
    private[uic] def bindRef(ref: SignalRef[Maybe[A]])(using Frame): SelectButton[A]  = selection(ref)
    private[uic] def hearBlur(f: Maybe[A] => UI.Action)(using Frame): SelectButton[A] = onBlur(f)

    /** Fired with the selection now held, after a click or an arrow changed it (`Absent` where a click cleared it).
      * A click that changes nothing fires nothing.
      */
    def onChange(f: Maybe[A] => UI.Action)(using Frame): SelectButton[A] = new SelectButton(p.copy(onChangeF = Present(Callback.of(f))))

    /** Fired with the option picked, after a click or an arrow that changed the selection to it: [[onChange]] without the
      * `Maybe` and without the clear, for a group that is never empty or a one-way [[selection]] that writes its own state
      * here. Fires after [[onChange]] when both are set.
      */
    def onPick(f: A => UI.Action)(using Frame): SelectButton[A] = new SelectButton(p.copy(onPickF = Present(Callback.of(f))))

    /** Fires on focus loss with the selection held (`Absent` for none): the validation layer's Blur trigger. */
    def onBlur(f: Maybe[A] => UI.Action)(using Frame): SelectButton[A] = new SelectButton(p.copy(onBlurF = Present(Callback.of(f))))

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
        // Multiple select is a group of independent toggles, each its own tab stop, so it needs no ids; unbound it keeps what the
        // reader presses in a mount, and bound it needs no mount at all.
        if multipleFlag then
            if multi.nonEmpty then renderNav(Absent)
            else
                UI.mounted(Signal.initRef(Set.empty[A]).map(own =>
                    new SelectButton(p.copy(multi = Present(MultiChoice(ReactiveVariable(own))))).renderNav(Absent)
                ))
        else
            // Single select is a radio group, and its mount is where the ids come from: ONE base the option ids derive from
            // (`$base-<n>`), so the ids follow the options however many there are. Unbound, the mount keeps what the reader presses too.
            UI.mounted {
                for
                    base <- UI.id
                    self <-
                        if single.nonEmpty then Kyo.lift(this)
                        else
                            Signal.initRef(Maybe.empty[A]).map(own =>
                                new SelectButton(p.copy(single = Present(SingleChoice.Optional(ReactiveVariable(own)))))
                            )
                yield self.renderNav(Present((base, focusById)))
            }

    /** The seam a test renders to choose the base itself. */
    private[uic] def wired(base: String, focus: String => UI.Action)(using Frame): UI =
        renderNav(Present((base, focus)))

    private def renderNav(minted: Maybe[(String, String => UI.Action)])(using Frame): UI =
        val validity = FieldValidity(validityV)
        // The switches and the selection are read as ONE region (see [[ReactiveValue.and]]), pinned back into the slots as constants
        // so the body reads plain values.
        val read =
            ReactiveValue.optional(disabledFlag).and(ReactiveValue.optional(loadingV)).and(held).and(OptionItem.reading(optionSources))
        validity.follow(read.render { case (((d, l), h), opts) =>
            val shownItems = OptionItem.flatten(opts)
            val nav: Nav   = minted.map((base, focus) => (shownItems.indices.map(i => s"$base-$i").toList, focus))
            new SelectButton(p.copy(
                items = shownItems,
                disabledFlag = d.map(BoolValue.Const(_)),
                loadingV = l.map(BoolValue.Const(_))
            )).body(h, nav, validity)
        })
    end renderNav

    /** The options held, whichever binding holds them. */
    private def held(using Frame): ReactiveValue[List[A]] =
        (single, multi) match
            case (Present(c), _) if !multipleFlag => c.reading.map(_.toList)
            case (_, Present(c)) if multipleFlag  => c.reading.map(_.toList)
            case _                                => ReactiveValue.Const(Nil)

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
        if loadingV.constTrue then el = el.aria("busy" -> "true")
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
        access.name.foreach(v => el = el.aria("label" -> v))
        access.nameRef.foreach(v => el = el.aria("labelledby" -> v))

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
            access.id match
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
            // A constant nobody hears is fixed: its buttons show it, read-only, and press nothing.
            else if fixed then tb = tb.readonly(true)
            else tb = tb.onChange(_ => activate(a))
            end if
            if !multipleFlag then
                (nav, optionIds) match
                    case (Present((_, focus)), Present(ids)) if ids.isDefinedAt(i) =>
                        tb = tb.id(ids(i)).asRadioOption(i == tabStop, optionKey(ids, navigable, i, focus))
                    // No mount, no ids: the page as first rendered has no arrows yet, but it is already
                    // the one tab stop the attached group is, on the option that carries the id.
                    case _ =>
                        tb = tb.asRadioOption(i == tabStop, _ => ())
                        if i == idAt then access.id.foreach(v => tb = tb.id(v))
            else if i == idAt then access.id.foreach(v => tb = tb.id(v))
            end if
            tb.renderInPlace
        }
        el(UI.children(Diagnostics.cards("SelectButton")(keyCollisionCard, Diagnostic.unknown(unknown.map(labelF)), unheardCard).toList ++
            buttons)*)
    end body

    /** The loud card rendered ahead of the fused buttons when the option keys
      * collide (see [[Diagnostics]]): two options sharing a key toggle together.
      */
    private def keyCollisionCard(using Frame): Diagnostic =
        Diagnostic.naming(
            if keyF.isEmpty then "option labels are not unique and optionKey is unset, so those options select together; set optionKey"
            else "optionKey is not unique across the options, so those options select together",
            Diagnostics.duplicates(items.map(key))
        )

    /** A selection bound one way that nothing hears: every click is painted away again. A disabled group takes no click,
      * so it has nothing to say.
      */
    private def unheardCard(using Frame): Diagnostic =
        val deaf =
            if multipleFlag then multi.exists(_.deaf(onChangeSetF))
            else single.exists(_.deaf(onChangeF.orElse(onPickF)))
        val name = Controlled.Name("SelectButton", "the selection", "a click", if multipleFlag then "onChange" else "onChange or onPick")
        Diagnostic(deaf && !disabledFlag.constTrue, name.advice)
    end unheardCard

    /** A constant nobody hears is fixed: the group is drawn read-only, its buttons pressing nothing. */
    private def fixed: Boolean =
        if multipleFlag then multi.exists(_.fixed(onChangeSetF))
        else single.exists(_.fixed(onChangeF.orElse(onPickF)))

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
    /** How a field that always holds an option binds: through `selection` over `A` ([[HasSetter]]). */
    given selection[A]: HasSetter[SelectButton[A], "selection", UI.Prop[A]] with
        def apply(c: SelectButton[A], v: UI.Prop[A])(using Frame): SelectButton[A] = c.selection(v)

    final private[uic] case class Props[A](
        // The options the body draws, resolved from `optionSources` by the region that draws them.
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
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        access: Accessibility = Accessibility.none,
        onChangeF: Maybe[Maybe[A] => UI.Action] = Absent,
        onPickF: Maybe[A => UI.Action] = Absent,
        onChangeSetF: Maybe[Set[A] => UI.Action] = Absent,
        onBlurF: Maybe[Maybe[A] => UI.Action] = Absent,
        onBlurSetF: Maybe[Set[A] => UI.Action] = Absent,
        loadingV: Maybe[BoolValue] = Absent,
        optionSources: List[OptionSource[A]] = Nil
    )

    def apply[A](): SelectButton[A] = new SelectButton[A](Props(Nil, _.toString))
end SelectButton

/** A [[SelectButton]] where several options may be on at once (Prime's `multiple`): a row of independent toggle buttons,
  * each its own tab stop, bound to the `Set[A]` of the options that are on. Made by [[SelectButton.multiple]], and a type
  * of its own because its value is a set: it has one binding and one `onChange`, both over `Set[A]`, and it is a
  * `FormControl[Set[A]]`, so `bind` takes a `FormField[Set[A]]`.
  *
  * It renders through the group it was made from, so the two share every option, template and state setter and the
  * same anatomy.
  */
final class MultiSelectButton[A] private[uic] (private[uic] val group: SelectButton[A])
    extends Node, FormControl[Set[A]], HasAccessibleNameRef, HasProps:
    type Self = MultiSelectButton[A]

    private[uic] def p: SelectButton[A] = group

    private def on(g: SelectButton[A]): MultiSelectButton[A] = new MultiSelectButton(g)

    /** Appends typed options with their text projection, as [[SelectButton.options]]. */
    def options(is: Iterable[A] | Signal[Seq[A]])(label: A => String): MultiSelectButton[A] = on(group.options(is)(label))

    /** Convenience for plain string options (label = identity), a collection or a signal. */
    def options(is: Iterable[String] | Signal[Seq[String]])(using ev: String =:= A): MultiSelectButton[A] = on(group.options(is))

    /** Stable per-option key, what options are matched by; defaults to the label projection. */
    def optionKey(f: A => String): MultiSelectButton[A] = on(group.optionKey(f))

    /** Custom option content, as [[SelectButton.itemTemplate]]. */
    def itemTemplate(f: A => UI): MultiSelectButton[A] = on(group.itemTemplate(f))

    /** Renders matching options disabled. */
    def optionDisabled(f: A => Boolean): MultiSelectButton[A] = on(group.optionDisabled(f))

    /** The set of chosen options. A `SignalRef` binds two way; a plain `Signal` or a constant binds one way, and a click
      * only reports the set it asks for to [[onChange]]. A member no option carries stays in the set while the others
      * toggle.
      */
    def selection(v: Set[A] | Signal[Set[A]]): MultiSelectButton[A] = on(group.withMulti(v))

    /** Fired with the set now held, after a click changed it. */
    def onChange(f: Set[A] => UI.Action)(using Frame): MultiSelectButton[A] = on(group.withChangeSet(f))

    /** Fires on focus loss with the set held: the validation layer's Blur trigger. */
    def onBlur(f: Set[A] => UI.Action)(using Frame): MultiSelectButton[A] = on(group.withBlurSet(f))

    /** Whether clicking the last option on may turn it off (Prime default true). */
    def allowEmpty(v: Boolean): MultiSelectButton[A] = on(group.allowEmpty(v))

    /** Says the options are still arriving, as [[SelectButton.loading]]. */
    def loading(v: Boolean | Signal[Boolean]): MultiSelectButton[A] = on(group.loading(v))

    /** Size of the fused buttons. */
    def size(v: Size): MultiSelectButton[A] = on(group.size(v))

    /** Disables the whole group; a `Signal[Boolean]` toggles it reactively. */
    def disabled(v: Boolean | Signal[Boolean]): MultiSelectButton[A] = on(group.disabled(v))

    private[uic] def accessibility: Accessibility                              = group.accessibility
    private[uic] def withAccessibility(v: Accessibility): MultiSelectButton[A] = on(group.withAccessibility(v))

    private[uic] def validitySlots: FieldValidity.Slots                                  = group.validitySlots
    private[uic] def withValidity(v: FieldValidity.Slots): MultiSelectButton[A]          = on(group.withValidity(v))
    private[uic] def bindRef(ref: SignalRef[Set[A]])(using Frame): MultiSelectButton[A]  = selection(ref)
    private[uic] def hearBlur(f: Set[A] => UI.Action)(using Frame): MultiSelectButton[A] = onBlur(f)

    private[uic] def render(using Frame): UI = group.render
end MultiSelectButton
