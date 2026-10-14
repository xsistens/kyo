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

/** SpeedDial — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
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
  * no outside-click dismissal (the fan is not an Overlay — close with Escape or
  * by toggling).
  */
final case class SpeedDial private (
    itemsV: List[MenuItem] = Nil,
    directionV: SpeedDialDirection = SpeedDialDirection.Up,
    openRefV: Maybe[SignalRef[Boolean]] = Absent,
    disabledFlag: Boolean = false,
    accNameV: Maybe[String] = Absent
) extends Node:
    type Self = SpeedDial

    /** Appends actions ([[MenuItem]] — `icon` + `label` + `onSelect`). */
    def items(is: MenuItem*): SpeedDial = copy(itemsV = itemsV ++ is.toList)

    /** Fan direction (default `Up`). */
    def direction(v: SpeedDialDirection): SpeedDial = copy(directionV = v)

    /** Binds the fan visibility two-way to `ref` (optional — self-managed
      * otherwise).
      */
    def open(ref: SignalRef[Boolean]): SpeedDial = copy(openRefV = Present(ref))

    /** Disables the toggle (and thereby the fan). */
    def disabled(v: Boolean): SpeedDial = copy(disabledFlag = v)

    /** Accessible name of the toggle button (`aria-label`). */
    def accessibleName(v: String): SpeedDial = copy(accNameV = Present(v))

    private[uic] def render(using Frame): UI =
        val stat: UI = body(false, Absent, "", _ => ())
        UI.mounted {
            for
                cmds <- UI.commands
                base <- cmds.freshId
                open <- openRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef(false)
            yield wired(open, base, id => cmds.focusId(id))
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes (golden-test seam). */
    private[uic] def wired(open: SignalRef[Boolean], base: String, focus: String => Any < Async)(using Frame): UI =
        // See Menu.wired for why the resolution sits inside the mount's content.
        MenuRender.resolveDisabled(itemsV) { items =>
            open.render(o => copy(itemsV = items).body(o, Present(open), base, focus))
        }

    /** The actions the fan actually fans out: a separator has no place in a row of round icon
      * buttons, the extracted sheet has no `.p-speeddial-separator` to style one, and rendering
      * it would have put an empty button in the menu.
      */
    private def actions: List[MenuItem] = itemsV.filterNot(_.separatorFlag)

    private def body(isOpen: Boolean, open: Maybe[SignalRef[Boolean]], base: String, focus: String => Any < Async)(using
        Frame
    ): UI =
        // Explicitly typed effect: an untyped match would infer `Any` and land the
        // effect inert in the by-name handler.
        val toggle: Any < Async =
            open match
                case Present(r) => r.getAndUpdate(!_)
                case Absent     => ()
        def close: Any < Async =
            open match
                case Present(r) => r.set(false)
                case Absent     => ()

        val acts = actions
        // The positions the keyboard may land on: a disabled action is natively disabled, so it
        // is out of the tab order and an arrow steps over it.
        val navigable              = acts.zipWithIndex.collect { case (it, i) if !it.disabledFlag.constTrue => i }
        val first                  = navigable.headOption.getOrElse(-1)
        val listId                 = s"$base-list"
        def itemId(i: Int): String = s"$base-i$i"

        var toggleBtn = Button()
            .icon(Icons.plus)
            .rounded(true)
            .extraClass("p-speeddial-button")
            .extraClass("p-speeddial-rotate")
            .accessibleName(accNameV.getOrElse("Toggle actions"))
            .ariaRaw("haspopup", "menu")
            .ariaRaw("expanded", isOpen.toString)
            .disabled(disabledFlag)
        // The ids come from the mount, so the placeholder render has none to point at and says
        // nothing rather than naming an element called `-list`.
        if base.nonEmpty then toggleBtn = toggleBtn.ariaRaw("controls", listId)
        if !disabledFlag then
            toggleBtn = toggleBtn.onClick(toggle).onKeyDownRaw { e =>
                // Enter and Space are the browser's, since this IS a `<button>` and its click
                // already toggles. The arrow that points into the fan opens it, and the seed
                // carries focus in from there; on an already-open fan there is nothing to open,
                // so it moves focus instead of doing nothing.
                val eff: Any < Async =
                    if e.key == directionV.opening && navigable.nonEmpty then
                        if isOpen then focus(itemId(first))
                        else
                            open match
                                case Present(r) => r.set(true)
                                case Absent     => ()
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
                    def activate: Any < Async =
                        open match
                            case Present(r) =>
                                for
                                    _ <- it.actionV match
                                        case Present(eff) => eff
                                        case Absent       => (): Any < Async
                                    _ <- r.set(false)
                                yield ()
                            case Absent => ()
                    var btn = Button()
                        .rounded(true)
                        .severity(Severity.Secondary)
                        .size(Size.Small)
                        .accessibleName(it.labelV.constOrEmpty)
                        .tooltip(it.labelV.constOrEmpty)
                        .disabled(it.disabledFlag.constTrue)
                        .roleRaw("menuitem")
                        .id(itemId(i))
                    it.iconV.foreach(g => btn = btn.icon(g))
                    if !it.disabledFlag.constTrue then
                        // No action is in the tab sequence: the dial's one tab stop is the toggle,
                        // and focus reaches an action by opening the fan, which seeds it onto the
                        // first one. A tab stop here would be a second one, and Tab would then walk
                        // the reader between the toggle and a menu instead of past the dial.
                        btn = btn
                            .tabIndexRaw(-1)
                            .focusSeedRaw(i == first)
                            .onKeyDownRaw(itemKey(i, navigable, itemId, focus, close))
                        if open.isDefined then btn = btn.onClick(activate)
                    end if
                    li.cssClass("p-speeddial-item").role("presentation")(toChild(btn: UI))
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
        el(toChild(toggleBtn: UI), toChild(list(actionUIs.map(toChild)*)))
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
        focus: String => Any < Async,
        close: Any < Async
    )(using Frame): KeyboardEvent => Any < Async = e =>
        if e.key == Keyboard.Tab then close
        else
            ListNav.onKey(navigable, self, e.key, wrap = true, directionV.axis) match
                case Present(step) if step.dismiss                                         => close
                case Present(step) if step.focus != self && navigable.contains(step.focus) => focus(itemId(step.focus))
                case _                                                                     => ()
end SpeedDial

object SpeedDial:
    /** An empty speed dial — add actions via [[SpeedDial.items]]. */
    def apply(): SpeedDial = new SpeedDial()
