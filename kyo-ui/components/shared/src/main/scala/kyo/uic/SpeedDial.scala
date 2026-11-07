package kyo.uic

import kyo.*
import kyo.UI.*

/** Direction the [[SpeedDial]] action fan opens towards (Prime's linear
  * `direction` prop).
  */
enum SpeedDialDirection derives CanEqual:
    case Up, Down, Left, Right

    /** The `.p-speeddial-<token>` modifier suffix. */
    private[uic] def token: String = this match
        case SpeedDialDirection.Up    => "up"
        case SpeedDialDirection.Down  => "down"
        case SpeedDialDirection.Left  => "left"
        case SpeedDialDirection.Right => "right"

    /** The axis the fan lays its actions out along, which is the pair of arrows that walks them. */
    private[uic] def axis: ListNav.Orientation = this match
        case SpeedDialDirection.Up | SpeedDialDirection.Down => ListNav.Orientation.Vertical
        case _                                               => ListNav.Orientation.Horizontal

    /** The one arrow that points from the toggle INTO the fan, which is how the toggle opens it.
      * The opposite arrow points at the page behind the dial and stays the page's.
      */
    private[uic] def opening: Keyboard = this match
        case SpeedDialDirection.Up    => Keyboard.ArrowUp
        case SpeedDialDirection.Down  => Keyboard.ArrowDown
        case SpeedDialDirection.Left  => Keyboard.ArrowLeft
        case SpeedDialDirection.Right => Keyboard.ArrowRight
end SpeedDialDirection

/** SpeedDial: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * SpeedDial anatomy: `div.p-speeddial.p-component.p-speeddial-<direction>
  * [.p-speeddial-open]` > toggle `button.p-speeddial-button.p-speeddial-rotate`
  * (a rounded icon [[Button]]; the sheet rotates the plus glyph 45° while open)
  * + `ul.p-speeddial-list` of `li.p-speeddial-item` action buttons), so the
  * extracted `@primeuix` speeddial CSS applies verbatim. Prime lays the linear
  * directions out via inline styles; the theme glue expresses them as
  * `.p-speeddial-<direction>` CSS (the ToggleSwitch precedent).
  *
  * Actions are [[MenuItem]]s rendered as rounded secondary icon Buttons whose
  * `label` becomes the accessible name and native tooltip; `onSelect` runs and
  * the fan closes. `open(ref)` binds the fan two-way (self-managed otherwise).
  *
  * The fan is a real `role="menu"` and is operated like one. The dial is ONE tab
  * stop, the toggle: no action is in the tab sequence, focus enters the fan by
  * opening it and leaves by Tab, which closes it in either direction. Inside, the
  * arrows along the fan's own axis walk the actions and come round, Home and End
  * reach the ends, and Escape closes. Opening seeds focus onto the first action
  * and closing returns it to the toggle, both declaratively
  * (`focusAuto`/`focusRestore`), the way an [[Overlay]] panel does. A closed fan
  * renders no actions at all rather than hiding them with `scale(0)`: the sheet's
  * hidden buttons were still real tab stops, so a reader tabbing past a closed
  * dial walked through every action it had.
  *
  * Honest deferrals: only the LINEAR type (circle/semi-circle/quarter-circle
  * need JS-measured per-item offsets); the fan-in/out transition is omitted
  * (kyo re-renders replace the subtree, restarting CSS transitions); no mask
  * variant (Prime styles the mask via inline styles and a JS-managed backdrop);
  * no outside-click dismissal (the fan is not an Overlay; close with Escape or
  * by toggling).
  */
final class SpeedDial private[uic] (private[uic] val p: SpeedDial.Props) extends Node, HasProps:
    import p.*

    type Self = SpeedDial

    /** Appends actions ([[MenuItem]]: `icon` + `label` + `onSelect`), each built directly or from `_` (see [[MenuItemScope]]), or a
      * signal of actions that change while the dial is shown (see [[LiveEntry]]).
      */
    def items(is: MenuItemLiveEntry*): SpeedDial = new SpeedDial(p.copy(entriesV = entriesV ++ is))

    /** Fan direction (default `Up`). */
    def direction(v: SpeedDialDirection): SpeedDial = new SpeedDial(p.copy(directionV = v))

    /** Binds the fan visibility. A `SignalRef` binds two way: the toggle, the arrow into the fan, an action, Escape and
      * Tab write it, then [[onOpenChange]] hears the new state. A plain `Signal` binds one way: the fan follows it and
      * only reports. A constant nobody hears is fixed: the fan stays as it is and the toggle, still reachable, changes
      * nothing. Unbound, the dial keeps the state itself, starting closed. A signal nothing hears is reported in a card.
      */
    def open(v: Boolean | Signal[Boolean]): SpeedDial = new SpeedDial(p.copy(openSlot = openSlot.bind(v)))

    /** Fired with the fan visibility the reader asks for, after a two-way [[open]] is written. */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): SpeedDial = new SpeedDial(p.copy(openSlot = openSlot.hear(f)))

    /** Disables the toggle (and thereby the fan). */
    def disabled(v: Boolean): SpeedDial = new SpeedDial(p.copy(disabledFlag = v))

    /** Accessible name of the toggle button (`aria-label`). */
    def accessibleName(v: String): SpeedDial = new SpeedDial(p.copy(access = access.copy(name = Present(v))))

    private[uic] def render(using Frame): UI =
        val shownAt = openSlot.binding.flatMap(_.const).getOrElse(false)
        val dial    = UI.mounted {
            for
                base <- UI.id
                // Unbound, the dial keeps the fan state itself, starting closed.
                open <- openSlot.owned(false)
            yield new SpeedDial(p.copy(openSlot = open)).wiredOver(base, focusById)
        }
        // A signal nobody hears: every toggle the reader makes is painted away again.
        Diagnostics.before(dial)("SpeedDial")(Diagnostic.unheard(openSlot, SpeedDial.openName))
    end render

    /** The subscription tree the mount publishes, over a two-way ref (golden-test seam). */
    private[uic] def wired(open: SignalRef[Boolean], base: String, focus: String => UI.Action)(using Frame): UI =
        new SpeedDial(p.copy(openSlot = openSlot.bind(open))).wiredOver(base, focus)

    /** The subscription tree over the fan state however it is bound. */
    private def wiredOver(base: String, focus: String => UI.Action)(using Frame): UI =
        // See Menu.wired for why the resolution sits inside the mount's content. The items and the open state are read as
        // ONE region (see [[ReactiveValue.and]]).
        MenuRender.readItems(entriesV).and(openSlot.reading(false)).render((items, o) =>
            new SpeedDial(p.copy(itemsV = items)).body(o, base, focus)
        )

    /** The actions the fan actually fans out: a separator has no place in a row of round icon
      * buttons, the extracted sheet has no `.p-speeddial-separator` to style one, and rendering
      * it would have put an empty button in the menu.
      */
    private def actions: List[MenuItem] = itemsV.filterNot(_.p.separatorFlag)

    private def body(isOpen: Boolean, base: String, focus: String => UI.Action)(using
        Frame
    ): UI =
        val toggle: UI.Action  = openSlot.update(!_)
        def close: UI.Action   = openSlot.change(false)
        def openFan: UI.Action = openSlot.change(true)
        // A constant nobody hears is fixed: the toggle stays a tab stop and says it changes nothing.
        val fixed = openSlot.fixed

        val acts = actions
        // The positions the keyboard may land on: a disabled action is natively disabled, so it
        // is out of the tab order and an arrow steps over it.
        val navigable              = acts.zipWithIndex.collect { case (it, i) if !it.p.disabledFlag.constTrue => i }
        val first                  = navigable.headOption.getOrElse(-1)
        val listId                 = s"$base-list"
        def itemId(i: Int): String = s"$base-i$i"

        var toggleBtn = Button()
            .icon(Icons.plus)
            .rounded(true)
            .extraClass("p-speeddial-button")
            .extraClass("p-speeddial-rotate")
            .accessibleName(access.name.getOrElse("Toggle actions"))
            .ariaRaw("haspopup", "menu")
            .ariaRaw("expanded", isOpen.toString)
            .disabled(disabledFlag)
        if fixed then toggleBtn = toggleBtn.ariaDisabled(true)
        // A test may render with no base, and then there is no list id to point at.
        if base.nonEmpty then toggleBtn = toggleBtn.ariaRaw("controls", listId)
        if !disabledFlag && !fixed then
            toggleBtn = toggleBtn.onClick(toggle).onKeyDownRaw { e =>
                // Enter and Space are the browser's, since this IS a `<button>` and its click
                // already toggles. The arrow that points into the fan opens it, and the seed
                // carries focus in from there; on an already-open fan there is nothing to open,
                // so it moves focus instead of doing nothing.
                val eff: UI.Action =
                    if e.key == directionV.opening && navigable.nonEmpty then
                        if isOpen then focus(itemId(first))
                        else openFan
                    else if (e.key == Keyboard.Escape || e.key == Keyboard.Tab) && isOpen then close
                    else ()
                eff
            }
        end if

        // A closed fan renders nothing: its actions are unavailable, and the sheet's way of
        // saying so (`scale(0)`, `opacity: 0`, `pointer-events: none`) leaves them focusable,
        // which put every one of them in the Tab order of a dial the reader had not opened.
        val actionUIs: List[UI] =
            if !isOpen then Nil
            else
                acts.zipWithIndex.map { (it, i) =>
                    def activate: UI.Action =
                        for
                            _ <- it.p.actionV match
                                case Present(eff) => eff
                                case Absent       => (): UI.Action
                            _ <- close
                        yield ()
                    var btn = Button()
                        .rounded(true)
                        .severity(Severity.Secondary)
                        .size(Size.Small)
                        .accessibleName(it.p.labelV.constOrEmpty)
                        .tooltip(it.p.labelV.constOrEmpty)
                        .disabled(it.p.disabledFlag.constTrue)
                        .roleRaw("menuitem")
                        .id(itemId(i))
                    it.p.iconV.foreach(g => btn = btn.icon(g))
                    if !it.p.disabledFlag.constTrue then
                        // No action is in the tab sequence: the dial's one tab stop is the toggle,
                        // and focus reaches an action by opening the fan, which seeds it onto the
                        // first one. A tab stop here would be a second one, and Tab would then walk
                        // the reader between the toggle and a menu instead of past the dial.
                        btn = btn
                            .tabIndexRaw(-1)
                            .focusSeedRaw(i == first)
                            .onKeyDownRaw(itemKey(i, navigable, itemId, focus, close))
                        btn = btn.onClick(activate)
                    end if
                    li.cssClass("p-speeddial-item").role("presentation")(btn: UI)
                }

        var el = div
            .cssClass("p-speeddial")
            .cssClass("p-component")
            .cssClass(s"p-speeddial-${directionV.token}")
        // The arrows the dial answers would otherwise scroll the page out from under it. Declared
        // only where a key is actually consumed: a dial with no reachable action answers none.
        if navigable.nonEmpty && !disabledFlag then el = el.preventScrollKeys
        if isOpen then el = el.cssClass("p-speeddial-open")
        var list = ul.cssClass("p-speeddial-list").role("menu")
        if base.nonEmpty then list = list.id(listId)
        el(toggleBtn: UI, list(UI.children(actionUIs)*))
    end body

    /** Moves focus along the fan, and closes it on Escape or Tab.
      *
      * Tab closes in both directions, because either one carries the reader out of the menu: no
      * action holds a tab stop, so forward lands past the dial and backward on the toggle, and a
      * fan left open behind them would keep saying `aria-expanded="true"` over a menu nobody is
      * in. The close is a ref write like any other, so the browser has already moved focus by the
      * time it lands, which is what keeps the restore out of its way.
      *
      * Activation is deliberately not read from [[ListNav]]: an action IS a `<button>`, so the
      * browser and the dispatcher already agree on one activation per Enter or Space, and a third
      * from here would be one too many. The fan comes round because it is a menu, which is the
      * one thing [[ListNav]] leaves to its host and the ARIA menu pattern answers.
      */
    private def itemKey(
        self: Int,
        navigable: List[Int],
        itemId: Int => String,
        focus: String => UI.Action,
        close: UI.Action
    )(using Frame): KeyboardEvent => UI.Action = e =>
        if e.key == Keyboard.Tab then close
        else
            ListNav.onKey(navigable, self, e.key, wrap = true, directionV.axis) match
                case Present(step) if step.dismiss                                         => close
                case Present(step) if step.focus != self && navigable.contains(step.focus) => focus(itemId(step.focus))
                case _                                                                     => ()
end SpeedDial

object SpeedDial:
    final private[uic] case class Props(
        // The actions `body` draws, resolved from `entriesV` by the render.
        itemsV: List[MenuItem] = Nil,
        entriesV: List[MenuItemLiveEntry] = Nil,
        directionV: SpeedDialDirection = SpeedDialDirection.Up,
        openSlot: Slot[Boolean] = Slot(),
        disabledFlag: Boolean = false,
        access: Accessibility = Accessibility.none
    )

    /** An empty speed dial; add actions via [[SpeedDial.items]]. */
    def apply(): SpeedDial = new SpeedDial(Props())

    private val openName = Slot.Name("SpeedDial", "the fan", "a toggle", "onOpenChange")
end SpeedDial
