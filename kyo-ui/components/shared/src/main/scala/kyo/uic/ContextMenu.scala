package kyo.uic

import kyo.*
import kyo.UI.*

/** ContextMenu — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * ContextMenu anatomy: the floating `div.p-contextmenu.p-component` panel >
  * `ul.p-contextmenu-root-list[role=menu]` of `li.p-contextmenu-item` rows;
  * items with children open side-nested `p-contextmenu-submenu` panels), so the
  * extracted `@primeuix` contextmenu CSS applies verbatim.
  *
  * The component WRAPS its target region: a right-click (the `contextmenu`
  * event) anywhere inside the wrapped children opens the menu, and the kyo
  * client suppresses the browser's native context menu for the region (elements
  * without a context-menu handler in their ancestor chain keep the native
  * menu). Escape or an outside click closes it; picking a leaf item runs its
  * `onSelect` and closes everything.
  *
  * The panel opens AT THE POINTER, which is what a context menu is: the
  * `contextmenu` event carries its viewport position ([[kyo.UI.MouseEvent.position]]),
  * the component keeps it in a ref, and [[Overlay.pointerAnchor]] places the panel
  * there: down and to the right of the point, turning back over it near a viewport
  * edge. The panel therefore portals to `document.body`, so a table's own scroll
  * container cannot clip it either.
  *
  * A right-click while the menu is OPEN closes it, and the next one opens the menu
  * at the new place. The open panel's outside-click backdrop covers the viewport, so
  * that second right-click never reaches what is under it: moving the panel to the
  * new point instead would leave every host that reads the row under the pointer
  * (`DataTable.contextMenuRow`) pointing at the row of the FIRST click, which is a
  * menu saying one thing and acting on another. Closing keeps the two honest, and
  * costs the reader a second click.
  *
  * Derived ids ([[HasElementId]]): the panel is `s"$id-panel"` and the highlighted
  * row `s"$id-active"`; the wrapped region carries the id itself.
  *
  * Keyboard (WAI-ARIA menu): the open LIST seeds focus and holds it, so the
  * full navigation works without a prior click — ArrowUp/Down rove Prime's
  * `.p-focus` over the enabled rows, ArrowRight opens the focused submenu
  * (landing on its first row), ArrowLeft climbs back out, Home/End jump, Enter
  * or Space activates a leaf (runs its `onSelect`, closes the menu), and Escape
  * closes one level then the whole menu at the root. Nested submenu panels do
  * NOT seed focus — the highlight rides `aria-activedescendant` (wired by
  * `id(...)`) from the root panel that owns the keys. The focused row is exposed
  * as `s"$id-active"`; keyboard navigation works without an id, only the ARIA
  * hook is omitted.
  */
final case class ContextMenu private (
    itemsV: List[MenuItem] = Nil,
    kids: List[UI] = Nil,
    idV: Maybe[String] = Absent
) extends Node, HasElementId:
    type Self = ContextMenu

    /** Appends menu items ([[MenuItem]] rows, `MenuItem.separator` dividers;
      * nested `items(...)` open side submenus).
      */
    def items(is: MenuItem*): ContextMenu = copy(itemsV = itemsV ++ is.toList)

    /** Adds target children — the region whose right-click opens the menu. */
    def apply(cs: UI*): ContextMenu = copy(kids = kids ++ cs)

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): ContextMenu = copy(idV = v)

    /** Paths of the items carrying submenus (one open/closed ref each). */
    private[uic] def submenuPaths: List[List[Int]] = MenuRender.submenuPaths(itemsV)

    private[uic] def render(using Frame): UI =
        // Open state, keyboard focus path, and one signal per submenu are allocated
        // by this effectful mount; static projections (SSG, the SSR page HTML)
        // render the closed target region inert.
        val stat: UI = body(false, Absent, Nil, Absent, Map.empty.withDefaultValue(false), Absent, Absent)
        UI.mounted {
            for
                openRef <- Signal.initRef(false)
                focus   <- Signal.initRef(List.empty[Int])
                // Where the last right-click was. Not a render input: it is bound onto the open
                // panel as a style, so the point can change without rebuilding the panel.
                at   <- Signal.initRef(Absent: Maybe[UI.Point])
                refs <- Kyo.foreach(submenuPaths)(p => Signal.initRef(false).map(p -> _))
                cmds <- UI.commands
                base <- cmds.freshId
            yield wired(openRef, focus, at, refs.toList, base)
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
        openRef: SignalRef[Boolean],
        focus: SignalRef[List[Int]],
        at: SignalRef[Maybe[UI.Point]],
        refs: List[(List[Int], SignalRef[Boolean])],
        base: String
    )(using Frame): UI =
        // See Menu.wired for why the resolution sits inside the mount's content. It matters
        // most here: `openRef` IS the mount's state, so resolving one level higher would
        // close the menu on the right-click that opened it — which is the whole reason this
        // slot could not simply be rebuilt per target.
        MenuRender.resolveDisabled(itemsV) { items =>
            val self = copy(itemsV = items, idV = if idV.isDefined then idV else Present(base))
            openRef.render { isOpen =>
                focus.render { f =>
                    MenuRender.renderAll(refs) { open =>
                        self.body(
                            isOpen,
                            Present(openRef),
                            f,
                            Present(focus),
                            open.withDefaultValue(false),
                            Present(refs),
                            Present(at)
                        )
                    }
                }
            }
        }
    end wired

    private def body(
        isOpen: Boolean,
        openRef: Maybe[SignalRef[Boolean]],
        focus: List[Int],
        focusRef: Maybe[SignalRef[List[Int]]],
        open: Map[List[Int], Boolean],
        refs: Maybe[List[(List[Int], SignalRef[Boolean])]],
        at: Maybe[SignalRef[Maybe[UI.Point]]]
    )(using Frame): UI =
        def resetTree: Any < Async =
            refs match
                case Present(rs) => MenuRender.openExactly(rs, Absent)
                case Absent      => ()

        def resetFocus: Any < Async =
            focusRef match
                case Present(fr) => fr.set(Nil)
                case Absent      => ()

        // The mouse close path (a leaf click) and each right-click re-open funnel
        // through these so the next open starts clean.
        def closeMenu: Any < Async =
            openRef match
                case Present(r) => for _ <- r.set(false); _ <- resetTree; _ <- resetFocus yield ()
                case Absent     => ()

        // The point is written BEFORE the open, so the panel never exists without one to be at.
        val openMenu: MouseEvent => Any < Async = e =>
            (openRef, at) match
                case (Present(r), Present(pt)) =>
                    for _ <- resetTree; _ <- resetFocus; _ <- pt.set(e.position); _ <- r.set(true) yield ()
                case _ => ()

        val keyHandler: KeyboardEvent => Any < Async = e =>
            focusRef match
                case Present(fr) =>
                    MenuNav.onKey(itemsV, MenuNav.Orientation.Vertical, focus, e.key) match
                        case Present(step) => MenuRender.applyStep(itemsV, refs, fr, openRef, step)
                        case Absent        => ()
                case Absent => ()

        val panel: List[UI] =
            openRef match
                case Present(r) if isOpen =>
                    val rows = MenuTree.items(
                        "contextmenu",
                        itemsV,
                        Nil,
                        open,
                        refs,
                        rootSubmenuIcon = Icons.angleRight,
                        rootAnchor = OverlayAnchor.RightStart,
                        focused = if focus.isEmpty then Absent else Present(focus),
                        onLeafActivate = Present(closeMenu),
                        roving = true,
                        idBase = idV
                    )
                    var listEl = ul.cssClass("p-contextmenu-root-list").role("menu")
                    if focus.nonEmpty then idV.foreach(b => listEl = listEl.aria("activedescendant", s"$b-active"))
                    // The LIST is what focus goes to, not the panel around it: the list is the
                    // `role="menu"` and the thing carrying `aria-activedescendant`, and that
                    // attribute is read off the focused element or off nothing at all. Escape is
                    // left to bubble to the Overlay, which decides what closing means.
                    listEl = listEl
                        .tabIndex(-1)
                        .focusAuto(true)
                        .focusRestore(true)
                        .preventScrollKeys
                        .onKeyDown(e => if e.key == Keyboard.Escape then () else keyHandler(e))
                    val listUI: UI = listEl(rows.map(toChild)*)
                    // Host-gated (renderOpen): this component already subscribes to
                    // openRef in `wired`, and the single-subscription form avoids the
                    // duplicated-panel race. Escape/outside-click write false back;
                    // the highlight/tree reset rides the NEXT right-click's openMenu.
                    var ov = Overlay(r)
                        .matchWidth(false)
                        .animate(false)
                        .panelClass("p-contextmenu")
                        .panelClass("p-component")
                        .seedFocus(false)
                    // The pointer placement needs both halves: somewhere to read the point from,
                    // and a panel id the measure can address. The id is derived rather than minted
                    // because a mount that published this panel would freeze its rows (see
                    // Overlay.pointerRepositionTrigger), and by `wired` there is always one.
                    (at, idV) match
                        case (Present(pt), Present(b)) =>
                            ov = ov
                                .pointerAnchor(pt)
                                .panelId(s"$b-panel")
                                // The open panel's backdrop covers the viewport, so it is what a
                                // right-click anywhere else lands on; without this the browser's
                                // own menu would open over the panel.
                                .onBackdropContextMenu(_ => closeMenu)
                        case _ => ()
                    end match
                    List(ov(listUI).renderOpen)
                case _ => Nil

        var target = div
            .cssClass("p-uic-contextmenu-target")
            .cssClass("p-uic-overlay-anchor")
            .aria("haspopup", "menu")
        // The wrapped region is this component's root, so the element id lands here (HasElementId),
        // and the panel's derived id hangs off it.
        idV.foreach(b => target = target.id(b))
        if openRef.isDefined then target = target.onContextMenu(openMenu)
        target((kids ++ panel).map(toChild)*)
    end body
end ContextMenu

object ContextMenu:
    /** A context menu over `items`, wrapping the target region given via
      * `apply(...)` — right-click inside it opens the menu.
      */
    def apply(items: Seq[MenuItem]): ContextMenu = new ContextMenu(itemsV = items.toList)

    /** An empty context menu — add items via [[ContextMenu.items]] and target
      * children via `apply(...)`.
      */
    def apply(): ContextMenu = new ContextMenu()
end ContextMenu
