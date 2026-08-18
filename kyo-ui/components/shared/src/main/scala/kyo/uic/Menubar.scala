package kyo.uic

import kyo.*
import kyo.UI.*

/** Menubar — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Menubar anatomy: `div.p-menubar.p-component` > optional `div.p-menubar-start`
  * + `ul.p-menubar-root-list[role=menubar]` of `li.p-menubar-item` rows +
  * optional `div.p-menubar-end`), so the extracted `@primeuix` menubar CSS
  * applies verbatim.
  *
  * Items with children open a NESTED floating submenu — below the root row
  * (`OverlayAnchor.BottomStart`), to the side on deeper levels (`RightStart`,
  * the sheet's `left: 100%; top: 0` geometry). Every submenu is a real
  * [[Overlay]]: an outside click closes ONE level (the topmost backdrop catches
  * it), Escape closes ONE level too (each panel consumes its own keydown —
  * the Overlay per-level story), and each panel seeds focus on open. Open state is
  * click-driven: clicking a parent row toggles its submenu (a deliberate
  * deviation from Prime's desktop hover-open — a server round-trip per hover
  * is heavy; the sheet's hover styling is untouched). The open row carries
  * Prime's `.p-menubar-item-active`.
  *
  * Keyboard (WAI-ARIA menubar): the root list is the single tab stop.
  * ArrowLeft/Right move Prime's `.p-focus` between root items (closing any open
  * submenu), ArrowDown opens the focused root's submenu (landing on its first
  * row; ArrowUp opens on the last). Inside a submenu ArrowUp/Down move siblings,
  * ArrowRight opens a nested submenu — or, on a leaf, crosses to the next root —
  * and ArrowLeft climbs out, crossing to the previous root at the first level.
  * Home/End jump, Enter/Space activate a leaf, Escape closes a level. Submenu
  * panels do NOT seed focus (the root list keeps the keys); `id(...)` wires
  * `aria-activedescendant`.
  *
  * Honest deferrals: no mobile/hamburger breakpoint mode; typeahead is not
  * implemented.
  */
final case class Menubar private (
    itemsV: List[MenuItem] = Nil,
    startV: Maybe[UI] = Absent,
    endV: Maybe[UI] = Absent,
    idV: Maybe[String] = Absent
) extends Node:
    type Self = Menubar

    /** Appends root items ([[MenuItem]]; nested `items(...)` open submenus). */
    def items(is: MenuItem*): Menubar = copy(itemsV = itemsV ++ is.toList)

    /** Content before the root list (`div.p-menubar-start` — logo slot). */
    def start(ui: UI): Menubar = copy(startV = Present(ui))

    /** Content after the root list (`div.p-menubar-end` — pushed to the far
      * edge by the sheet's `margin-left: auto`).
      */
    def end(ui: UI): Menubar = copy(endV = Present(ui))

    /** Base id for the bar — enables `aria-activedescendant` (the focused row is
      * exposed as `s"$id-active"`). Keyboard navigation works without it.
      */
    def id(v: String): Menubar = copy(idV = Present(v))

    /** Paths of the items carrying submenus (one open/closed ref each). */
    private[uic] def submenuPaths: List[List[Int]] = MenuRender.submenuPaths(itemsV)

    private[uic] def render(using Frame): UI =
        // One open/closed signal per submenu plus the keyboard focus path, allocated
        // by this effectful mount; static projections render the closed anatomy inert.
        val stat: UI = body(Map.empty.withDefaultValue(false), Absent, Nil, Absent)
        UI.mounted {
            for
                refs  <- Kyo.foreach(submenuPaths)(p => Signal.initRef(false).map(p -> _))
                focus <- Signal.initRef(List.empty[Int])
            yield wired(refs.toList, focus)
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes (golden-test seam). */
    private[uic] def wired(
        refs: List[(List[Int], SignalRef[Boolean])],
        focus: SignalRef[List[Int]]
    )(using Frame): UI =
        focus.render { f =>
            MenuRender.renderAll(refs)(open => body(open.withDefaultValue(false), Present(refs), f, Present(focus)))
        }

    private def body(
        open: Map[List[Int], Boolean],
        refs: Maybe[List[(List[Int], SignalRef[Boolean])]],
        focus: List[Int],
        focusRef: Maybe[SignalRef[List[Int]]]
    )(using Frame): UI =
        val keyHandler: KeyboardEvent => Any < Async = e =>
            focusRef match
                case Present(fr) =>
                    MenuNav.onKey(itemsV, MenuNav.Orientation.Horizontal, focus, e.key) match
                        case Present(step) => MenuRender.applyStep(itemsV, refs, fr, Absent, step)
                        case Absent        => ()
                case Absent => ()

        val rowsUI = MenuTree.items(
            "menubar",
            itemsV,
            Nil,
            open,
            refs,
            rootSubmenuIcon = Icons.angleDown,
            rootAnchor = OverlayAnchor.BottomStart,
            focused = if focus.isEmpty then Absent else Present(focus),
            roving = true,
            idBase = idV
        )
        val startUI: List[UI] = startV.toList.map(u => div.cssClass("p-menubar-start")(toChild(u)))
        val endUI: List[UI]   = endV.toList.map(u => div.cssClass("p-menubar-end")(toChild(u)))
        var list              = ul.cssClass("p-menubar-root-list").role("menubar")
        if focusRef.isDefined then list = list.tabIndex(0).preventScrollKeys.onKeyDown(keyHandler)
        if focus.nonEmpty then idV.foreach(b => list = list.aria("activedescendant", s"$b-active"))
        val listUI: UI = list(rowsUI.map(toChild)*)
        div.cssClass("p-menubar").cssClass("p-component")(
            ((startUI :+ listUI) ++ endUI).map(toChild)*
        )
    end body
end Menubar

object Menubar:
    /** An empty menubar — add root items via [[Menubar.items]]. */
    def apply(): Menubar = new Menubar()

/** Package-internal recursive tree renderer shared by Menubar and TieredMenu
  * (`p-<prefix>-*` anatomy with nested [[Overlay]] submenus).
  */
private[uic] object MenuTree:

    /** Renders one level of items as `li.p-<prefix>-item` rows; items with
      * children get the submenu glyph and, while their ref is open, a nested
      * Overlay panel stamped `p-<prefix>-submenu` (root level opens below for
      * the horizontal menubar, every other level to the side).
      *
      * `focused` stamps Prime's `.p-focus` on the row at that path (hosts with
      * panel-level keyboard navigation — ContextMenu); `onLeafActivate` runs
      * after a leaf item's action + the submenu-tree close (hosts whose whole
      * tree lives in a closable popup close their own root here).
      */
    def items(
        prefix: String,
        its: List[MenuItem],
        path: List[Int],
        open: Map[List[Int], Boolean],
        refs: Maybe[List[(List[Int], SignalRef[Boolean])]],
        rootSubmenuIcon: IconGlyph,
        rootAnchor: OverlayAnchor,
        focused: Maybe[List[Int]] = Absent,
        onLeafActivate: Maybe[Any < Async] = Absent,
        roving: Boolean = false,
        idBase: Maybe[String] = Absent
    )(using Frame): List[UI] =
        val nested = path.nonEmpty

        def closeAll: Any < Async =
            refs match
                case Present(rs) => MenuRender.openExactly(rs, Absent)
                case Absent      => ()

        its.zipWithIndex.map { (it, i) =>
            if it.separatorFlag then
                li.cssClass(s"p-$prefix-separator").role("separator")
            else
                val p = path :+ i
                if it.itemsV.nonEmpty then
                    val isOpen = open(p)
                    def toggle: Any < Async =
                        refs match
                            case Present(rs) =>
                                val target: Maybe[List[Int]] =
                                    if isOpen then (if p.size > 1 then Present(p.init) else Absent)
                                    else Present(p)
                                MenuRender.openExactly(rs, target)
                            case Absent => ()
                    val subIcon = if nested then Icons.angleRight else rootSubmenuIcon
                    var row = li
                        .cssClass(s"p-$prefix-item")
                        .cssClass("p-uic-overlay-anchor")
                        .role("presentation")
                        .aria("haspopup", "menu")
                        .aria("expanded", isOpen.toString)
                    if isOpen then row = row.cssClass(s"p-$prefix-item-active")
                    if focused.exists(_ == p) then
                        row = row.cssClass("p-focus")
                        idBase.foreach(b => row = row.id(s"$b-active"))
                    if it.disabledFlag then row = row.cssClass("p-disabled").aria("disabled", "true")
                    val act: Maybe[Any < Async] =
                        if refs.isDefined && !it.disabledFlag then Present(toggle) else Absent
                    val content = MenuRender.itemContent(prefix, it, act, Present(subIcon), roving)
                    // The enclosing wired render already subscribes to every submenu ref
                    // (MenuRender.renderAll), so the overlay is host-gated: rendered only
                    // while open, via the single-subscription renderOpen form.
                    val panel: List[UI] = refs match
                        case Present(rs) if isOpen =>
                            rs.collectFirst { case (`p`, ref) => ref } match
                                case Some(ref) =>
                                    val anchor = if nested then OverlayAnchor.RightStart else rootAnchor
                                    // The rows ride inside a real <ul> (display:contents glue):
                                    // an <li> start tag implicitly closes the open <li> when the
                                    // HTML parser meets it inside the panel <div>, hoisting the
                                    // rows out of the panel.
                                    val rows = ul
                                        .cssClass("p-uic-submenu-list")
                                        .role("menu")(
                                            items(
                                                prefix,
                                                it.itemsV,
                                                p,
                                                open,
                                                refs,
                                                rootSubmenuIcon,
                                                rootAnchor,
                                                focused,
                                                onLeafActivate,
                                                roving,
                                                idBase
                                            )
                                                .map(toChild)*
                                        )
                                    List(
                                        Overlay(ref)
                                            .anchor(anchor)
                                            .matchWidth(false)
                                            .animate(false)
                                            // Roving hosts keep focus on the root container and drive the
                                            // highlight declaratively, so a submenu must NOT seed focus —
                                            // that would pull focus off the host and stop its key handler.
                                            .seedFocus(!roving)
                                            .panelClass(s"p-$prefix-submenu")
                                            .apply(rows)
                                            .renderOpen
                                    )
                                case None => Nil
                        case _ => Nil
                    row((content :: panel).map(toChild)*)
                else
                    var row = li.cssClass(s"p-$prefix-item").role("presentation")
                    if focused.exists(_ == p) then
                        row = row.cssClass("p-focus")
                        idBase.foreach(b => row = row.id(s"$b-active"))
                    if it.disabledFlag then row = row.cssClass("p-disabled").aria("disabled", "true")
                    def activate: Any < Async =
                        for
                            _ <- it.actionV match
                                case Present(eff) => eff
                                case Absent       => (): Any < Async
                            _ <- closeAll
                            _ <- onLeafActivate match
                                case Present(eff) => eff
                                case Absent       => (): Any < Async
                        yield ()
                    val act: Maybe[Any < Async] =
                        if refs.isDefined && !it.disabledFlag then Present(activate) else Absent
                    row(toChild(MenuRender.itemContent(prefix, it, act, Absent, roving)))
                end if
        }
    end items
end MenuTree
