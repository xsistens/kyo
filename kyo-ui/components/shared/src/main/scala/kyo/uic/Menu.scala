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

/** Menu: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Menu
  * anatomy: `div.p-menu.p-component` > `ul.p-menu-list[role=menu]` of
  * `li.p-menu-item` rows, each `div.p-menu-item-content` >
  * `a.p-menu-item-link` with icon + label, plus `li.p-menu-separator` dividers
  * and `li.p-menu-submenu-label` section headings for grouped items), so the
  * extracted `@primeuix` menu CSS applies verbatim.
  *
  * Two modes like Prime: inline (the default render) and [[popup]]: the
  * popup wraps the same list in the [[Overlay]] primitive (Prime's
  * `.p-menu-overlay` skin) over the bound open state, an outside
  * click or Escape closes, and the LIST seeds focus on open so the keyboard
  * works without a prior click (the list, not the panel around it, because the
  * list is the `role="menu"` carrying `aria-activedescendant` and that attribute
  * is read off whatever holds focus). Place the popup inside a `position: relative`
  * anchor (stamp `p-uic-overlay-anchor` on your trigger's container; the
  * Select/Popover geometry story).
  *
  * Items are the shared typed [[MenuItem]] model: `onSelect(effect)` runs on
  * click or Enter and the popup closes; `url` rows render real anchors;
  * top-level items carrying `items(...)` render as flat labelled sections
  * (Prime's grouped Menu; for nested floating submenus use TieredMenu).
  * Keyboard (WAI-ARIA menu, the shared [[ListNav]] machine): the list is the
  * single tab stop; ArrowDown/ArrowUp move Prime's `.p-focus` row over the
  * enabled items (disabled and separators skipped) and cycle at the ends, so
  * ArrowUp with nothing highlighted lands on the last row; Home/End jump to the
  * first/last, Enter or Space activates it. An inline menu highlights its first
  * enabled row when it takes focus and drops the highlight when it loses it. The
  * item links carry `tabindex="-1"` (out of the Tab order: the list roves the
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
    // The rows `body` draws: what a render resolved from `entriesV`, or what a host handed over through `rows`.
    itemsV: List[MenuItem] = Nil,
    entriesV: List[MenuItemLiveEntry] = Nil,
    popupSlot: Slot[Boolean] = Slot(),
    popupOwnFlag: Boolean = false,
    idV: Maybe[String] = Absent,
    roleV: MenuAccessibleRole = MenuAccessibleRole.Menu,
    accessibleNameV: Maybe[TextValue] = Absent
) extends Node, HasElementId, HasAccessibleName:
    type Self = Menu

    /** Appends items ([[MenuItem]] rows, `MenuItem.separator` dividers, and
      * grouped sections via nested `items`), each built directly or from `_` (see [[MenuItemScope]]), or a signal of
      * items for rows that change while the menu is shown (see [[LiveEntry]]).
      */
    def items(is: MenuItemLiveEntry*): Menu = copy(entriesV = entriesV ++ is)

    /** Package-internal: rows a host has already resolved in its own region, drawn as they are. */
    private[uic] def rows(items: List[MenuItem]): Menu = copy(itemsV = items, entriesV = items)

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): Menu = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    /** Popup mode: the menu renders as a floating [[Overlay]] panel (Prime's `.p-menu-overlay`) over the open state `v`. A
      * `SignalRef` binds two way: a pick, Tab, Escape and an outside click write false, then [[onPopupChange]] hears
      * it. A plain `Signal` binds one way: the panel follows it and only reports. A constant nobody hears is fixed: an
      * open panel stays, and nothing closes it. A signal nothing hears is reported in a card. Without `popup` the menu
      * renders inline.
      */
    def popup(v: Boolean | Signal[Boolean]): Menu = copy(popupSlot = popupSlot.bind(v), popupOwnFlag = false)

    /** Popup mode with the open state kept by the menu itself, starting open, as an overlay nothing inside can open
      * does: the panel shows until the reader closes it. [[onPopupChange]] still hears every close.
      */
    def popup(): Menu = copy(popupSlot = popupSlot.copy(binding = Absent), popupOwnFlag = true)

    /** Fired with the open state the reader asks for (false for a pick, Tab, Escape and an outside click), after a
      * two-way [[popup]] is written.
      */
    def onPopupChange(f: Boolean => UI.Action)(using Frame): Menu = copy(popupSlot = popupSlot.hear(f))

    /** Asks for the popup closed: a two-way binding is written, then the close reported. Inline, there is nothing to close. */
    private def closePopup(using Frame): UI.Action =
        if popupSlot.binding.isDefined then popupSlot.change(false) else ()

    /** The popup panel: the overlay wired to report a dismissal here, and quiet, since the menu reports its own binding. */
    private def popupOverlay(using Frame): Overlay = Overlay.withOpen(popupSlot.quieted)

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
        val menu = roleV match
            case MenuAccessibleRole.Navigation =>
                // No mount: a navigation holds no highlight and mints no id, so there is no state
                // for one to allocate, and the static projection is already the live tree.
                navigation
            case MenuAccessibleRole.Menu =>
                // `popup()`: the open state is the menu's own, allocated in the mount beside the highlight. Before the
                // mount runs the panel is drawn open from a constant signal.
                val ownsPopup = popupSlot.binding.isEmpty && popupOwnFlag
                val shown     = if ownsPopup then copy(popupSlot = popupSlot.standIn(true)) else this
                // The keyboard highlight lives in a signal allocated by this effectful
                // mount; static projections (SSG, the SSR page HTML) render the same
                // anatomy inert until the client transport attaches.
                val stat: UI = MenuRender.readItems(entriesV).render(items => shown.copy(itemsV = items).body(-1, Absent, idV))
                UI.mounted {
                    for
                        cmds  <- UI.commands
                        base  <- cmds.freshId
                        hi    <- Anchored.init(-1)
                        popup <- if ownsPopup then popupSlot.owned(true) else Kyo.lift(popupSlot)
                    yield copy(popupSlot = popup).wired(hi, base)
                }.placeholder(stat)
        Diagnostics.before(menu)("Menu")(Diagnostic.unheard(popupSlot, Menu.popupName))
    end render

    /** The navigation form: the rows as plain list items around links. Disabled rows are
      * resolved first, as the menu form resolves them, so a reactive `disabled` still dims a row
      * and takes its link out of the tab order.
      */
    private def navigation(using Frame): UI =
        // `popup()` with no mount to allocate in: the open state is kept by a mount of its own.
        Slot.owning(popupSlot, start = true, on = ReactiveValue.Const(popupOwnFlag))(s => copy(popupSlot = s).navigationOver)(
            copy(popupSlot = popupSlot.standIn(true)).navigationOver
        )
    end navigation

    private def navigationOver(using Frame): UI =
        MenuRender.readItems(entriesV).render { items =>
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
                    row(MenuRender.itemContent("menu", it, it.actionV, Absent, roving = false))
            }
            val list = ul.cssClass("p-menu-list")(UI.children(rowUIs)*)
            popupSlot.binding match
                case Present(_) =>
                    // The links are the stops, so focus goes to the list only to bring the reader
                    // into the panel, and Tab walks on from there.
                    var panelList = named(list.tabIndex(-1).focusAuto(true).focusRestore(true))
                    idV.foreach(v => panelList = panelList.id(v))
                    popupOverlay
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
                    root(list)
            end match
        }
    end navigationOver

    /** The subscription tree the mount publishes: the seam golden tests render
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
    private[uic] def wired(hi: Anchored[Int], base: String)(using Frame): UI =
        roleV match
            // A navigation has no highlight to wire, so the seam shows what `render` does.
            case MenuAccessibleRole.Navigation => navigation
            case MenuAccessibleRole.Menu       =>
                // Resolved HERE and not in `render`: the resolution is a reactive region, and one
                // placed outside the mount would rebuild the mount on every emission, taking the
                // open state with it. Inside the mount's content it re-renders the rows and nothing
                // else.
                // The items, the highlight and a popup's open state are read as ONE region (see [[ReactiveValue.and]]): leaving
                // writes the popup ref and the highlight together, and a nested region would repaint what it closed over.
                val popup = ReactiveValue.optional(popupSlot.binding)
                // The highlight follows its row over the roots shown ([[Anchored]], [[Menu.rowRebase]]).
                MenuRender.readRooted(entriesV).and(hi.reading).and(popup).render { case ((Rooted(roots, items), h), shown) =>
                    val hv = hi.view(h, roots)(using Menu.rowRebase)
                    copy(itemsV = items).body(hv.value, Present(hv), Present(idV.getOrElse(base)), shown)
                }
    end wired

    /** The popup menu for a host that already reads its items (resolved through [[MenuRender.readDisabled]]), the
      * highlight and the popup's open state in its own ONE region, and hands them over: the menu then adds no region of
      * its own. `SplitButton` hosts its menu this way.
      */
    private[uic] def hosted(h: Int, hi: Anchored.View[Int], base: String, shown: Boolean)(using Frame): UI =
        body(h, Present(hi), Present(idV.getOrElse(base)), Present(shown))

    /** @param partBase what internal parts derive from (`s"$partBase-active"`): the caller's id
      *                 when there is one, else the id the mount minted. Distinct from `idV`,
      *                 which is only ever the caller's and is what lands on the element.
      */
    private def body(h: Int, hiRef: Maybe[Anchored.View[Int]], partBase: Maybe[String], popupShown: Maybe[Boolean] = Absent)(using
        Frame
    ): UI =
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
                        _ <- closePopup
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
                row(MenuRender.itemContent("menu", it, act, Absent, roving = true))
        }

        var list = named(ul.cssClass("p-menu-list").role("menu"))
        if hiRow >= 0 then partBase.foreach(base => list = list.aria("activedescendant", s"$base-active"))
        // The caller's id goes on the ELEMENT, the way `HasElementId` says a container puts it on
        // its own root: inline that root is the `div.p-menu` below; in a popup the component's
        // root is the overlay's panel, which the menu does not own, so it lands on the
        // `ul[role=menu]`, the element that IS the menu to a reader. Either way `#<id>` exists
        // and contains the rows, which is what a page addressing a menu by name is reaching for.
        if popupSlot.binding.isDefined then idV.foreach(v => list = list.id(v))
        popupSlot.binding match
            case Present(_) =>
                // The LIST is what focus goes to, not the panel around it: the list is the
                // `role="menu"` and the thing carrying `aria-activedescendant`, and that attribute
                // is read off the focused element or off nothing at all. Escape is left to bubble
                // to the Overlay, which is the one place that decides what closing means.
                // Tab carries the reader out of the menu, so the menu goes with them: the keys live
                // on this list, and a panel left standing behind them answers none of them.
                val leave: UI.Action =
                    for
                        _ <- closePopup
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
                val overlay = popupOverlay
                    .matchWidth(false)
                    .animate(false)
                    .panelClass("p-menu")
                    .panelClass("p-component")
                    .panelClass("p-menu-overlay")
                    .seedFocus(false)(list(UI.children(rowUIs)*))
                // Where the host read the popup's open state in its own region it gates the panel itself.
                popupShown match
                    case Present(true)  => overlay.renderShown(partBase.map(b => s"$b-panel"))
                    case Present(false) => UI.empty
                    case Absent         => overlay.render
                end match
            case Absent =>
                // Arriving on the list highlights the first enabled row, and leaving drops the
                // highlight again. Prime's sheet clears the list's outline and puts the only
                // focus mark on the row, so an inline menu with focus and no highlighted row
                // looks exactly like one nobody has touched.
                val seedFocus: UI.Action = hiRef match
                    case Present(hi) if hiRow < 0 && navigable.nonEmpty => hi.set(navigable.head)
                    case _                                              => ()
                // The list is the menu's one tab stop from the first render, before the mount has a
                // highlight to rove: the keys are wired once it has.
                list = list.tabIndex(0)
                if hiRef.isDefined then
                    list = list.preventScrollKeys.onKeyDown(keyHandler).onFocus(seedFocus)
                    hiRef.foreach(hi => list = list.onBlur(hi.set(-1)))
                var root = div.cssClass("p-menu").cssClass("p-component")
                idV.foreach(v => root = root.id(v))
                root(list(UI.children(rowUIs)*))
        end match
    end body
end Menu

/** Package-internal flat row model of the plain Menu (grouped items flatten to
  * a heading followed by their children; Prime's Menu is a flat list).
  */
private[uic] enum MenuRow derives CanEqual:
    case Item(it: MenuItem)
    case Heading(text: TextValue)
    case Sep
end MenuRow

object Menu:
    /** An empty menu; add rows via [[Menu.items]]. */
    def apply(): Menu = new Menu()

    private val popupName = Slot.Name("Menu", "the popup", "a close", "onPopupChange")

    /** How a highlighted ROW position moves when the roots do. A row is a position in the flattened list (a section
      * spreads into its heading and its children), so it is located as a root item and an offset within it, the root is
      * found again ([[Anchored.index]]), and the row keeps its offset there. A row whose root is gone, or whose offset the
      * root found again no longer has (a section rebuilt with fewer children), leaves nothing highlighted.
      */
    private[uic] val rowRebase: Anchored.Rebase[Int] = (h, from, to) =>
        def span(root: AnyRef): Int = root match
            case it: MenuItem if !it.separatorFlag && it.itemsV.nonEmpty => 1 + it.itemsV.size
            case _                                                       => 1
        @scala.annotation.tailrec
        def locate(roots: List[AnyRef], i: Int, start: Int): Maybe[(Int, Int)] =
            roots match
                case Nil       => Absent
                case r :: rest =>
                    val n = span(r)
                    if h < start + n then Present((i, h - start)) else locate(rest, i + 1, start + n)
        if h < 0 then -1
        else
            locate(from, 0, 0).flatMap((i, offset) =>
                Anchored.index(i, from, to).filter(j => offset < span(to(j))).map(j => to.take(j).map(span).sum + offset)
            ).getOrElse(-1)
        end if
end Menu
