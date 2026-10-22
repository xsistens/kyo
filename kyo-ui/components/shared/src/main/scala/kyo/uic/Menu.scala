package kyo.uic

import kyo.*
import kyo.UI.*

/** What a [[Menu]] is to a reader. Hand-authored, and lives with Menu, the only host offering it. */
enum MenuAccessibleRole derives CanEqual:
    /** An application menu (the default): `role="menu"` rows, one tab stop, and arrow keys that
      * move a highlight announced through `aria-activedescendant`.
      */
    case Menu

    /** A site's navigation: a `<nav>` landmark around a plain list of links, each link its own tab
      * stop and followed natively, the current page announced through `aria-current`. The ARIA
      * menu pattern is wrong for this, since it tells a screen reader to expect an application menu
      * and stops it reading the rows as links.
      */
    case Navigation
end MenuAccessibleRole

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
  * highlight), and `id(...)` wires `aria-activedescendant` to the focused row,
  * which is exposed as `s"$id-active"`. Give distinct ids to multiple menus on a
  * page. The mount mints a base when none was given, so the announcement works
  * unasked; a caller's own `id` still wins.
  *
  * `accessibleRole(MenuAccessibleRole.Navigation)` turns the same rows into a site's
  * navigation: the root becomes a `<nav>` landmark, the list keeps its native list
  * semantics, every link is a tab stop that the browser follows, and none of the
  * menu keyboard above applies. Rows mark the page the reader is on with
  * [[MenuItem.current]], and a row whose `current` is the constant `true` is scrolled
  * into view, so a long nav opened on a deep link shows where the reader is. A navigation popup lists the same links in the overlay and
  * does not close itself when one is followed, since following a url row runs no
  * handler: the page closes it from its route.
  *
  * Honest deferrals: no per-item templates; typeahead is not implemented.
  */
final case class Menu private (
    itemsV: List[MenuItem] = Nil,
    popupRefV: Maybe[SignalRef[Boolean]] = Absent,
    idV: Maybe[String] = Absent,
    roleV: MenuAccessibleRole = MenuAccessibleRole.Menu,
    accessibleNameV: Maybe[TextValue] = Absent
) extends Node, HasElementId, HasAccessibleName:
    type Self = Menu

    /** Appends items ([[MenuItem]] rows, `MenuItem.separator` dividers, and
      * grouped sections via nested `items`), each built directly or from `_` (see [[MenuItemScope]]).
      */
    def items(is: MenuItemEntry*): Menu = copy(itemsV = itemsV ++ MenuItemScope.resolve(is))

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): Menu = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Popup mode: the menu renders as a floating [[Overlay]] panel bound two-way
      * to `ref` — writes open and close it, outside click/Escape write back.
      */
    def popup(ref: SignalRef[Boolean]): Menu = copy(popupRefV = Present(ref))

    /** What the menu is to a reader: an application menu (the default) or a site's
      * navigation. See [[MenuAccessibleRole]].
      */
    def accessibleRole(v: MenuAccessibleRole): Menu = copy(roleV = v)

    private[uic] def withAccessibleName(v: Maybe[TextValue]): Menu = copy(accessibleNameV = v)

    /** Puts the accessible name on `el` as `aria-label`, a signal through the attribute channel. */
    private def named[E <: UI.Ast.Element { type Self = E }](el: E): E =
        accessibleNameV match
            case Present(TextValue.Const(n)) => el.aria("label", n)
            case Present(TextValue.Dyn(s))   => el.aria("label", s)
            case Absent                      => el

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
        roleV match
            case MenuAccessibleRole.Navigation =>
                // No mount: a navigation holds no highlight and mints no id, so there is no state
                // for one to allocate, and the static projection is already the live tree.
                navigation
            case MenuAccessibleRole.Menu =>
                // The keyboard highlight lives in a signal allocated by this effectful
                // mount; static projections (SSG, the SSR page HTML) render the same
                // anatomy inert until the client transport attaches.
                val stat: UI = body(-1, Absent, idV)
                UI.mounted {
                    for
                        cmds <- UI.commands
                        base <- cmds.freshId
                        hi   <- Signal.initRef(-1)
                    yield wired(hi, base)
                }.placeholder(stat)
    end render

    /** The navigation form: the rows as plain list items around links. Disabled rows are
      * resolved first, as the menu form resolves them, so a reactive `disabled` still dims a row
      * and takes its link out of the tab order.
      */
    private def navigation(using Frame): UI =
        MenuRender.resolveDisabled(itemsV) { items =>
            val rowUIs: List[UI] = copy(itemsV = items).flatRows.map {
                case MenuRow.Sep           => li.cssClass("p-menu-separator").role("separator")
                case MenuRow.Heading(text) =>
                    text match
                        case TextValue.Const(t) => li.cssClass("p-menu-submenu-label")(t)
                        case TextValue.Dyn(s)   => li.cssClass("p-menu-submenu-label")(s)
                case MenuRow.Item(it) =>
                    var row = MenuRender.markCurrent(li.cssClass("p-menu-item"), it)
                    if it.disabledFlag.constTrue then row = row.cssClass("p-disabled")
                    // A long nav opened on a deep link shows its top rows, and the current page can be
                    // screens further down. A constant `current` is known here, so its row asks to be
                    // scrolled in (`nearest`: a row already in view stays put). A reactive one is
                    // patched through a channel with no render, so there is no moment to ask in.
                    if it.currentFlag.constTrue then row = row.scrollAuto(true)
                    // Only a row with an action declares a click handler. A url row declares none,
                    // because kyo-ui prevent-defaults a click on any anchor carrying one, and the
                    // browser following the link IS what the row is for.
                    row(toChild(MenuRender.itemContent("menu", it, it.actionV, Absent, roving = false)))
            }
            val list = ul.cssClass("p-menu-list")(rowUIs.map(toChild)*)
            popupRefV match
                case Present(openRef) =>
                    // The links are the stops, so focus goes to the list only to bring the reader
                    // into the panel, and Tab walks on from there.
                    var panelList = named(list.tabIndex(-1).focusAuto(true).focusRestore(true))
                    idV.foreach(v => panelList = panelList.id(v))
                    Overlay(openRef)
                        .matchWidth(false)
                        .animate(false)
                        .panelClass("p-menu")
                        .panelClass("p-component")
                        .panelClass("p-menu-overlay")
                        .seedFocus(false)(panelList)
                        .render
                case Absent =>
                    var root = named(nav.cssClass("p-menu").cssClass("p-component"))
                    idV.foreach(v => root = root.id(v))
                    root(toChild(list))
            end match
        }
    end navigation

    /** The subscription tree the mount publishes — the seam golden tests render
      * directly (a full top-down re-render shows mounted regions as
      * placeholders).
      *
      * `base` is the id the announcement is built from, which the mount mints. A caller's own
      * `id(...)` still wins, but the announcement no longer waits for one: `aria-activedescendant`
      * is the whole of what a screen reader hears about the highlighted row, and a menu that had
      * to be given an id to say it said nothing in most of the pages that use one.
      *
      * The minted base is passed DOWN rather than written into `idV`, so the two questions stay
      * apart: `idV` is "the id the caller asked for", which lands on the element, and the base is
      * "what internal parts derive from", which may be machine-made. Writing the mint into `idV`
      * (as this did) makes them one value, and then either the caller's id never reaches the DOM
      * or a machine name does. Same split `AutoComplete` makes at its own mount.
      */
    private[uic] def wired(hi: SignalRef[Int], base: String)(using Frame): UI =
        roleV match
            // A navigation has no highlight to wire, so the seam shows what `render` does.
            case MenuAccessibleRole.Navigation => navigation
            case MenuAccessibleRole.Menu       =>
                // Resolved HERE and not in `render`: the resolution is a reactive region, and one
                // placed outside the mount would rebuild the mount on every emission — taking the
                // open state with it. Inside the mount's content it re-renders the rows and nothing
                // else.
                MenuRender.resolveDisabled(itemsV) { items =>
                    val self = copy(itemsV = items)
                    hi.render(h => self.body(h, Present(hi), Present(idV.getOrElse(base))))
                }
    end wired

    /** @param partBase what internal parts derive from (`s"$partBase-active"`) — the caller's id
      *                 when there is one, else the id the mount minted. Distinct from `idV`,
      *                 which is only ever the caller's and is what lands on the element.
      */
    private def body(h: Int, hiRef: Maybe[SignalRef[Int]], partBase: Maybe[String])(using Frame): UI =
        val rows = flatRows
        // Positions (into `rows`) of the keyboard-navigable rows.
        val navigable: List[Int] = rows.zipWithIndex.collect {
            case (MenuRow.Item(it), i) if !it.disabledFlag.constTrue => i
        }
        // `h` is a ROW position, the same coordinate [[ListNav]] navigates in, so a row that
        // went disabled under a live update simply stops being the highlighted one.
        val hiRow: Int = if navigable.contains(h) then h else -1

        def activate(it: MenuItem): UI.Action =
            hiRef match
                case Present(hi) =>
                    for
                        _ <- it.actionV match
                            case Present(eff) => eff
                            case Absent       => (): UI.Action
                        _ <- popupRefV match
                            case Present(r) => r.set(false)
                            case Absent     => (): UI.Action
                        _ <- hi.set(-1)
                    yield ()
                case Absent => ()

        val keyHandler: KeyboardEvent => UI.Action = e =>
            hiRef match
                case Present(hi) =>
                    // The menu family cycles (the ARIA menu pattern, and what the nested menus
                    // driven by MenuNav already do), so ArrowDown on the last row comes back to
                    // the first and ArrowUp from nothing lands on the last.
                    ListNav.onKey(navigable, hiRow, e.key, wrap = true) match
                        case Present(step) =>
                            val chosen: UI.Action =
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
                    case TextValue.Dyn(s)   =>
                        li.cssClass("p-menu-submenu-label").role("presentation")(s)
            case (MenuRow.Item(it), i) =>
                // The row IS the menu item, not scaffolding around one: it is what
                // `aria-activedescendant` names, what carries the disabled state, and the only
                // child of a `role="menu"` that a reader can be told about. `role="presentation"`
                // stripped exactly that away, leaving a menu whose every child claimed to be
                // nothing. The link inside stays roleless, the way Prime renders it.
                var row = MenuRender.markCurrent(li.cssClass("p-menu-item").role("menuitem"), it)
                if it.disabledFlag.constTrue then row = row.cssClass("p-disabled").aria("disabled", "true")
                if i == hiRow then
                    row = row.cssClass("p-focus").scrollAuto(true)
                    partBase.foreach(base => row = row.id(s"$base-active"))
                val act: Maybe[UI.Action] =
                    if hiRef.isDefined && !it.disabledFlag.constTrue then Present(activate(it)) else Absent
                row(toChild(MenuRender.itemContent("menu", it, act, Absent, roving = true)))
        }

        var list = named(ul.cssClass("p-menu-list").role("menu"))
        if hiRow >= 0 then partBase.foreach(base => list = list.aria("activedescendant", s"$base-active"))
        // The caller's id goes on the ELEMENT, the way `HasElementId` says a container puts it on
        // its own root — inline that root is the `div.p-menu` below; in a popup the component's
        // root is the overlay's panel, which the menu does not own, so it lands on the
        // `ul[role=menu]`, the element that IS the menu to a reader. Either way `#<id>` exists
        // and contains the rows, which is what a page addressing a menu by name is reaching for.
        if popupRefV.isDefined then idV.foreach(v => list = list.id(v))
        popupRefV match
            case Present(openRef) =>
                // The LIST is what focus goes to, not the panel around it: the list is the
                // `role="menu"` and the thing carrying `aria-activedescendant`, and that attribute
                // is read off the focused element or off nothing at all. Escape is left to bubble
                // to the Overlay, which is the one place that decides what closing means.
                // Tab carries the reader out of the menu, so the menu goes with them: the keys live
                // on this list, and a panel left standing behind them answers none of them.
                val leave: UI.Action =
                    for
                        _ <- popupRefV match
                            case Present(r) => r.set(false)
                            case Absent     => (): UI.Action
                        _ <- hiRef match
                            case Present(hi) => hi.set(-1)
                            case Absent      => (): UI.Action
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
                val seedFocus: UI.Action = hiRef match
                    case Present(hi) if hiRow < 0 && navigable.nonEmpty => hi.set(navigable.head)
                    case _                                              => ()
                if hiRef.isDefined then
                    list = list.tabIndex(0).preventScrollKeys.onKeyDown(keyHandler).onFocus(seedFocus)
                    hiRef.foreach(hi => list = list.onBlur(hi.set(-1)))
                var root = div.cssClass("p-menu").cssClass("p-component")
                idV.foreach(v => root = root.id(v))
                root(toChild(list(rowUIs.map(toChild)*)))
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
