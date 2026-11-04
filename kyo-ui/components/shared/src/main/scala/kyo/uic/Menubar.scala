package kyo.uic

import kyo.*
import kyo.UI.*

/** Menubar: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Menubar anatomy: `div.p-menubar.p-component` > optional `div.p-menubar-start`
  * + `ul.p-menubar-root-list[role=menubar]` of `li.p-menubar-item` rows +
  * optional `div.p-menubar-end`), so the extracted `@primeuix` menubar CSS
  * applies verbatim.
  *
  * Items with children open a NESTED floating submenu: below the root row
  * (`OverlayAnchor.BottomStart`), to the side on deeper levels (`RightStart`,
  * the sheet's `left: 100%; top: 0` geometry). Every submenu is a real
  * [[Overlay]]: an outside click closes ONE level (the topmost backdrop catches
  * it), Escape closes ONE level too (each panel consumes its own keydown,
  * the Overlay per-level story), and each panel seeds focus on open. Open state is
  * click-driven: clicking a parent row toggles its submenu (a deliberate
  * deviation from Prime's desktop hover-open: a server round-trip per hover
  * is heavy; the sheet's hover styling is untouched). The open row carries
  * Prime's `.p-menubar-item-active`.
  *
  * Keyboard (WAI-ARIA menubar): the root list is the single tab stop.
  * ArrowLeft/Right move Prime's `.p-focus` between root items (closing any open
  * submenu), ArrowDown opens the focused root's submenu (landing on its first
  * row; ArrowUp opens on the last). Inside a submenu ArrowUp/Down move siblings,
  * ArrowRight opens a nested submenu (or, on a leaf, crosses to the next root),
  * and ArrowLeft climbs out, crossing to the previous root at the first level.
  * Home/End jump, Enter/Space activate a leaf, Escape closes a level. Submenu
  * panels do NOT seed focus (the root list keeps the keys); `id(...)` wires
  * `aria-activedescendant`.
  *
  * `id(...)` is the base for `aria-activedescendant`: the focused row is exposed
  * as `s"$id-active"`. Keyboard navigation works without one, only the ARIA hook
  * is omitted.
  *
  * Honest deferrals: no mobile/hamburger breakpoint mode; typeahead is not
  * implemented.
  */
final class Menubar private[uic] (private[uic] val s: Menubar.State) extends Node, HasElementId, Stated:
    import s.*

    type Self = Menubar

    /** Appends root items ([[MenuItem]]; nested `items(...)` open submenus), each built directly or from `_` (see
      * [[MenuItemScope]]), or a signal of items for a bar whose entries change while it is shown (see [[LiveEntry]]).
      */
    def items(is: MenuItemLiveEntry*): Menubar = new Menubar(s.copy(entriesV = entriesV ++ is))

    /** Content before the root list (`div.p-menubar-start`, the logo slot). */
    def start(ui: UI): Menubar = new Menubar(s.copy(startV = Present(ui)))

    /** Content after the root list (`div.p-menubar-end`, pushed to the far
      * edge by the sheet's `margin-left: auto`).
      */
    def end(ui: UI): Menubar = new Menubar(s.copy(endV = Present(ui)))

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): Menubar = new Menubar(s.copy(idV = v))

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    private[uic] def render(using Frame): UI =
        // The open submenus and the keyboard focus path, allocated by this effectful mount; static projections render the
        // closed anatomy inert.
        val stat: UI =
            MenuRender.readItems(entriesV).render(items => new Menubar(s.copy(itemsV = items)).body(Set.empty, Absent, Nil, Absent, idV))
        UI.mounted {
            for
                subs  <- Submenus.init()
                focus <- Anchored.init(List.empty[Int])
                cmds  <- UI.commands
                base  <- cmds.freshId
            yield wired(subs, focus, base)
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes (golden-test seam).
      *
      * `base` is the id the announcement is built from, which the mount mints. A caller's own
      * `id(...)` still wins, but the announcement no longer waits for one: `aria-activedescendant`
      * and the row id it names are the whole of what a screen reader hears about the highlighted
      * row.
      */
    private[uic] def wired(
        subs: Submenus,
        focus: Anchored[List[Int]],
        base: String
    )(using Frame): UI =
        // See Menu.wired for why the resolution sits inside the mount's content. The items, the highlight and the submenus
        // are read as ONE region (see [[ReactiveValue.and]]): a key writes the highlight and the submenus together, and a nested
        // region would repaint the values it closed over. Both follow their items over the roots shown ([[Anchored]]).
        MenuRender.readRooted(entriesV).and(focus.reading).and(subs.reading).render { case ((Rooted(roots, items), f), open) =>
            val fv = focus.view(f, roots)
            val sv = subs.view(open, roots)
            new Menubar(s.copy(itemsV = items)).body(sv.open, Present(sv), fv.value, Present(fv), Present(idV.getOrElse(base)))
        }
    end wired

    private def body(
        open: Set[List[Int]],
        subs: Maybe[Submenus.View],
        focus: List[Int],
        focusRef: Maybe[Anchored.View[List[Int]]],
        // What internal parts derive from: the caller's id when there is one, else the mount's
        // mint. Kept apart from `idV`, which is only ever the caller's (see Menu.wired).
        partBase: Maybe[String]
    )(using Frame): UI =
        val keyHandler: KeyboardEvent => UI.Action = e =>
            focusRef match
                case Present(fr) =>
                    MenuNav.onKey(itemsV, MenuNav.Orientation.Horizontal, focus, e.key) match
                        case Present(step) => MenuRender.applyStep(itemsV, subs, fr, (), step)
                        case Absent        => ()
                case Absent => ()

        val rowsUI = MenuTree.items(
            "menubar",
            itemsV,
            Nil,
            open,
            subs,
            rootSubmenuIcon = Icons.angleDown,
            rootAnchor = OverlayAnchor.BottomStart,
            focused = if focus.isEmpty then Absent else Present(focus),
            roving = true,
            idBase = partBase
        )
        val startUI: List[UI] = startV.toList.map(u => div.cssClass("p-menubar-start")(u))
        val endUI: List[UI]   = endV.toList.map(u => div.cssClass("p-menubar-end")(u))
        var list              = ul.cssClass("p-menubar-root-list").role("menubar")
        // The bar is one tab stop from the first render; its keys are wired once the mount has a focus to move.
        list = list.tabIndex(0)
        if focusRef.isDefined then list = list.preventScrollKeys.onKeyDown(keyHandler)
        if focus.nonEmpty then partBase.foreach(b => list = list.aria("activedescendant", s"$b-active"))
        val listUI: UI = list(UI.children(rowsUI)*)
        // The caller's id on the component's own root, as HasElementId requires of a container.
        var root = div.cssClass("p-menubar").cssClass("p-component")
        idV.foreach(v => root = root.id(v))
        root(UI.children((startUI :+ listUI) ++ endUI)*)
    end body
end Menubar

object Menubar:
    final private[uic] case class State(
        // The root rows `body` draws, resolved from `entriesV` by the render.
        itemsV: List[MenuItem] = Nil,
        entriesV: List[MenuItemLiveEntry] = Nil,
        startV: Maybe[UI] = Absent,
        endV: Maybe[UI] = Absent,
        idV: Maybe[String] = Absent
    )

    /** An empty menubar; add root items via [[Menubar.items]]. */
    def apply(): Menubar = new Menubar(State())
end Menubar

/** Package-internal recursive tree renderer shared by Menubar and TieredMenu
  * (`p-<prefix>-*` anatomy with nested [[Overlay]] submenus).
  */
private[uic] object MenuTree:

    /** Renders one level of items as `li.p-<prefix>-item` rows; items with
      * children get the submenu glyph and, while their path is open, a nested
      * Overlay panel stamped `p-<prefix>-submenu` (root level opens below for
      * the horizontal menubar, every other level to the side).
      *
      * `focused` stamps Prime's `.p-focus` on the row at that path (hosts with
      * panel-level keyboard navigation, e.g. ContextMenu); `onLeafActivate` runs
      * after a leaf item's action + the submenu-tree close (hosts whose whole
      * tree lives in a closable popup close their own root here).
      */
    def items(
        prefix: String,
        its: List[MenuItem],
        path: List[Int],
        open: Set[List[Int]],
        subs: Maybe[Submenus.View],
        rootSubmenuIcon: IconGlyph,
        rootAnchor: OverlayAnchor,
        focused: Maybe[List[Int]] = Absent,
        onLeafActivate: Maybe[UI.Action] = Absent,
        roving: Boolean = false,
        idBase: Maybe[String] = Absent
    )(using Frame): List[UI] =
        val nested = path.nonEmpty

        def closeAll: UI.Action =
            subs match
                case Present(s) => s.openExactly(Absent)
                case Absent     => ()

        its.zipWithIndex.map { (it, i) =>
            if it.s.separatorFlag then
                li.cssClass(s"p-$prefix-separator").role("separator")
            else
                val p = path :+ i
                if it.s.itemsV.nonEmpty then
                    val isOpen            = open(p)
                    def toggle: UI.Action =
                        subs match
                            case Present(s) =>
                                val target: Maybe[List[Int]] =
                                    if isOpen then (if p.size > 1 then Present(p.init) else Absent)
                                    else Present(p)
                                s.openExactly(target)
                            case Absent => ()
                    val subIcon = if nested then Icons.angleRight else rootSubmenuIcon
                    // A row that opens a submenu can be current too: a section whose page is the
                    // one open, in a nav that groups its links.
                    var row = MenuRender.markCurrent(
                        li
                            .cssClass(s"p-$prefix-item")
                            .cssClass("p-uic-overlay-anchor")
                            // `aria-haspopup` and `aria-expanded` below say this row opens a submenu,
                            // and neither means anything on a row that also claims to be presentational.
                            // The row IS the menu item; the link inside it stays roleless, as in Prime.
                            .role("menuitem")
                            .aria("haspopup", "menu")
                            .aria("expanded", isOpen.toString),
                        it
                    )
                    if isOpen then row = row.cssClass(s"p-$prefix-item-active")
                    if focused.exists(_ == p) then
                        row = row.cssClass("p-focus").scrollAuto(true)
                        idBase.foreach(b => row = row.id(s"$b-active"))
                    if it.s.disabledFlag.constTrue then row = row.cssClass("p-disabled").aria("disabled", "true")
                    val act: Maybe[UI.Action] =
                        if subs.isDefined && !it.s.disabledFlag.constTrue then Present(toggle) else Absent
                    val content = MenuRender.itemContent(prefix, it, act, Present(subIcon), roving)
                    // The enclosing wired render already reads the open submenus (Submenus.reading), so the overlay is
                    // host-gated: rendered only while open, via the single-subscription renderOpen form, and closing its
                    // own path when dismissed.
                    val panel: List[UI] = subs match
                        case Present(s) if isOpen =>
                            val anchor = if nested then OverlayAnchor.RightStart else rootAnchor
                            // The rows ride inside a real <ul> (display:contents glue):
                            // an <li> start tag implicitly closes the open <li> when the
                            // HTML parser meets it inside the panel <div>, hoisting the
                            // rows out of the panel.
                            val rows = ul
                                .cssClass("p-uic-submenu-list")
                                .role("menu")(
                                    UI.children(items(
                                        prefix,
                                        it.s.itemsV,
                                        p,
                                        open,
                                        subs,
                                        rootSubmenuIcon,
                                        rootAnchor,
                                        focused,
                                        onLeafActivate,
                                        roving,
                                        idBase
                                    ))*
                                )
                            List(
                                Overlay.gated(s.close(p))
                                    .anchor(anchor)
                                    .matchWidth(false)
                                    .animate(false)
                                    // Roving hosts keep focus on the root container and drive the
                                    // highlight declaratively, so a submenu must NOT seed focus:
                                    // that would pull focus off the host and stop its key handler.
                                    .seedFocus(!roving)
                                    .panelClass(s"p-$prefix-submenu")
                                    .apply(rows)
                                    .renderOpen
                            )
                        case _ => Nil
                    row(UI.children(content :: panel)*)
                else
                    var row = MenuRender.markCurrent(li.cssClass(s"p-$prefix-item").role("menuitem"), it)
                    if focused.exists(_ == p) then
                        row = row.cssClass("p-focus").scrollAuto(true)
                        idBase.foreach(b => row = row.id(s"$b-active"))
                    if it.s.disabledFlag.constTrue then row = row.cssClass("p-disabled").aria("disabled", "true")
                    def activate: UI.Action =
                        for
                            _ <- it.s.actionV match
                                case Present(eff) => eff
                                case Absent       => (): UI.Action
                            _ <- closeAll
                            _ <- onLeafActivate match
                                case Present(eff) => eff
                                case Absent       => (): UI.Action
                        yield ()
                    val act: Maybe[UI.Action] =
                        if subs.isDefined && !it.s.disabledFlag.constTrue then Present(activate) else Absent
                    row(MenuRender.itemContent(prefix, it, act, Absent, roving))
                end if
        }
    end items
end MenuTree
