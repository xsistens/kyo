package kyo.uic

import kyo.*
import kyo.UI.*

/** Menu — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Menu
  * anatomy: `div.p-menu.p-component` > `ul.p-menu-list[role=menu]` of
  * `li.p-menu-item` rows — each `div.p-menu-item-content` >
  * `a.p-menu-item-link` with icon + label — plus `li.p-menu-separator` dividers
  * and `li.p-menu-submenu-label` section headings for grouped items), so the
  * extracted `@primeuix` menu CSS applies verbatim.
  *
  * Two modes like Prime: inline (the default render) and `popup(openRef)` — the
  * popup wraps the same list in the [[Overlay]] primitive (Prime's
  * `.p-menu-overlay` skin): writes to the ref open and close it, an outside
  * click or Escape closes, and the LIST seeds focus on open so the keyboard
  * works without a prior click (the list, not the panel around it, because the
  * list is the `role="menu"` carrying `aria-activedescendant` and that attribute
  * is read off whatever holds focus). Place the popup inside a `position: relative`
  * anchor (stamp `p-uic-overlay-anchor` on your trigger's container — the
  * Select/Popover geometry story).
  *
  * Items are the shared typed [[MenuItem]] model: `onSelect(effect)` runs on
  * click or Enter and the popup closes; `url` rows render real anchors;
  * top-level items carrying `items(...)` render as flat labelled sections
  * (Prime's grouped Menu — for nested floating submenus use TieredMenu).
  * Keyboard (WAI-ARIA menu, the shared [[ListNav]] machine): the list is the
  * single tab stop; ArrowDown/ArrowUp move Prime's `.p-focus` row over the
  * enabled items (disabled and separators skipped) and cycle at the ends, so
  * ArrowUp with nothing highlighted lands on the last row; Home/End jump to the
  * first/last, Enter or Space activates it. An inline menu highlights its first
  * enabled row when it takes focus and drops the highlight when it loses it. The
  * item links carry `tabindex="-1"` (out of the Tab order — the list roves the
  * highlight), and `id(...)` wires `aria-activedescendant` to the focused row.
  *
  * Honest deferrals: no per-item templates; typeahead is not implemented.
  */
final case class Menu private (
    itemsV: List[MenuItem] = Nil,
    popupRefV: Maybe[SignalRef[Boolean]] = Absent,
    idV: Maybe[String] = Absent
) extends Node:
    type Self = Menu

    /** Appends items ([[MenuItem]] rows, `MenuItem.separator` dividers, and
      * grouped sections via nested `items`).
      */
    def items(is: MenuItem*): Menu = copy(itemsV = itemsV ++ is.toList)

    /** Base id for the list — enables `aria-activedescendant` (the focused row is
      * exposed as `s"$id-active"`). Give distinct ids to multiple menus on a page.
      * Without it the keyboard highlight still works; only the ARIA hook is
      * omitted.
      */
    def id(v: String): Menu = copy(idV = Present(v))

    /** Popup mode: the menu renders as a floating [[Overlay]] panel bound two-way
      * to `ref` — writes open and close it, outside click/Escape write back.
      */
    def popup(ref: SignalRef[Boolean]): Menu = copy(popupRefV = Present(ref))

    /** The flat row list: section items are flattened to a heading + their
      * children (Prime's grouped Menu model).
      */
    private def flatRows: List[MenuRow] =
        itemsV.flatMap { it =>
            if it.separatorFlag then List(MenuRow.Sep)
            else if it.itemsV.nonEmpty then
                MenuRow.Heading(it.labelV) :: it.itemsV.map { c =>
                    if c.separatorFlag then MenuRow.Sep else MenuRow.Item(c)
                }
            else List(MenuRow.Item(it))
        }

    private[uic] def render(using Frame): UI =
        // The keyboard highlight lives in a signal allocated by this effectful
        // mount; static projections (SSG, the SSR page HTML) render the same
        // anatomy inert until the client transport attaches.
        val stat: UI = body(-1, Absent)
        UI.mounted {
            for
                cmds <- UI.commands
                base <- cmds.freshId
                hi   <- Signal.initRef(-1)
            yield wired(hi, base)
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes — the seam golden tests render
      * directly (a full top-down re-render shows mounted regions as
      * placeholders).
      *
      * `base` is the id the announcement is built from, which the mount mints. A caller's own
      * `id(...)` still wins, but the announcement no longer waits for one: `aria-activedescendant`
      * is the whole of what a screen reader hears about the highlighted row, and a menu that had
      * to be given an id to say it said nothing in most of the pages that use one.
      */
    private[uic] def wired(hi: SignalRef[Int], base: String)(using Frame): UI =
        val self = if idV.isDefined then this else copy(idV = Present(base))
        hi.render(h => self.body(h, Present(hi)))
    end wired

    private def body(h: Int, hiRef: Maybe[SignalRef[Int]])(using Frame): UI =
        val rows = flatRows
        // Positions (into `rows`) of the keyboard-navigable rows.
        val navigable: List[Int] = rows.zipWithIndex.collect {
            case (MenuRow.Item(it), i) if !it.disabledFlag => i
        }
        // `h` is a ROW position, the same coordinate [[ListNav]] navigates in, so a row that
        // went disabled under a live update simply stops being the highlighted one.
        val hiRow: Int = if navigable.contains(h) then h else -1

        def activate(it: MenuItem): Any < Async =
            hiRef match
                case Present(hi) =>
                    for
                        _ <- it.actionV match
                            case Present(eff) => eff
                            case Absent       => (): Any < Async
                        _ <- popupRefV match
                            case Present(r) => r.set(false)
                            case Absent     => (): Any < Async
                        _ <- hi.set(-1)
                    yield ()
                case Absent => ()

        val keyHandler: KeyboardEvent => Any < Async = e =>
            hiRef match
                case Present(hi) =>
                    // The menu family cycles (the ARIA menu pattern, and what the nested menus
                    // driven by MenuNav already do), so ArrowDown on the last row comes back to
                    // the first and ArrowUp from nothing lands on the last.
                    ListNav.onKey(navigable, hiRow, e.key, wrap = true) match
                        case Present(step) =>
                            val chosen: Any < Async =
                                if step.activate then
                                    rows(step.focus) match
                                        case MenuRow.Item(it) => activate(it)
                                        case _                => ()
                                else ()
                            hi.set(step.focus).andThen(chosen)
                        case Absent => ()
                case Absent => ()

        val rowUIs: List[UI] = rows.zipWithIndex.map {
            case (MenuRow.Sep, _) =>
                li.cssClass("p-menu-separator").role("separator")
            case (MenuRow.Heading(text), _) =>
                text match
                    case TextValue.Const(t) => li.cssClass("p-menu-submenu-label").role("presentation")(t)
                    case TextValue.Dyn(s) =>
                        li.cssClass("p-menu-submenu-label").role("presentation")(toChild(s.render(t => stringToUI(t))))
            case (MenuRow.Item(it), i) =>
                // The row IS the menu item, not scaffolding around one: it is what
                // `aria-activedescendant` names, what carries the disabled state, and the only
                // child of a `role="menu"` that a reader can be told about. `role="presentation"`
                // stripped exactly that away, leaving a menu whose every child claimed to be
                // nothing. The link inside stays roleless, the way Prime renders it.
                var row = li.cssClass("p-menu-item").role("menuitem")
                if it.disabledFlag then row = row.cssClass("p-disabled").aria("disabled", "true")
                if i == hiRow then
                    row = row.cssClass("p-focus").scrollAuto(true)
                    idV.foreach(base => row = row.id(s"$base-active"))
                val act: Maybe[Any < Async] =
                    if hiRef.isDefined && !it.disabledFlag then Present(activate(it)) else Absent
                row(toChild(MenuRender.itemContent("menu", it, act, Absent, roving = true)))
        }

        var list = ul.cssClass("p-menu-list").role("menu")
        if hiRow >= 0 then idV.foreach(base => list = list.aria("activedescendant", s"$base-active"))
        popupRefV match
            case Present(openRef) =>
                // The LIST is what focus goes to, not the panel around it: the list is the
                // `role="menu"` and the thing carrying `aria-activedescendant`, and that attribute
                // is read off the focused element or off nothing at all. Escape is left to bubble
                // to the Overlay, which is the one place that decides what closing means.
                // Tab carries the reader out of the menu, so the menu goes with them: the keys live
                // on this list, and a panel left standing behind them answers none of them.
                val leave: Any < Async =
                    for
                        _ <- popupRefV match
                            case Present(r) => r.set(false)
                            case Absent     => (): Any < Async
                        _ <- hiRef match
                            case Present(hi) => hi.set(-1)
                            case Absent      => (): Any < Async
                    yield ()
                list = list
                    .tabIndex(-1)
                    .focusAuto(true)
                    .focusRestore(true)
                    .preventScrollKeys
                    .onKeyDown(e =>
                        if e.key == Keyboard.Escape then ()
                        else if e.key == Keyboard.Tab then leave
                        else keyHandler(e)
                    )
                Overlay(openRef)
                    .matchWidth(false)
                    .animate(false)
                    .panelClass("p-menu")
                    .panelClass("p-component")
                    .panelClass("p-menu-overlay")
                    .seedFocus(false)(list(rowUIs.map(toChild)*))
                    .render
            case Absent =>
                // Arriving on the list highlights the first enabled row, and leaving drops the
                // highlight again. Prime's sheet clears the list's outline and puts the only
                // focus mark on the row, so an inline menu with focus and no highlighted row
                // looks exactly like one nobody has touched.
                val seedFocus: Any < Async = hiRef match
                    case Present(hi) if hiRow < 0 && navigable.nonEmpty => hi.set(navigable.head)
                    case _                                              => ()
                if hiRef.isDefined then
                    list = list.tabIndex(0).preventScrollKeys.onKeyDown(keyHandler).onFocus(seedFocus)
                    hiRef.foreach(hi => list = list.onBlur(hi.set(-1)))
                div.cssClass("p-menu").cssClass("p-component")(
                    toChild(list(rowUIs.map(toChild)*))
                )
        end match
    end body
end Menu

/** Package-internal flat row model of the plain Menu (grouped items flatten to
  * a heading followed by their children — Prime's Menu is a flat list).
  */
private[uic] enum MenuRow derives CanEqual:
    case Item(it: MenuItem)
    case Heading(text: TextValue)
    case Sep
end MenuRow

object Menu:
    /** An empty menu — add rows via [[Menu.items]]. */
    def apply(): Menu = new Menu()
