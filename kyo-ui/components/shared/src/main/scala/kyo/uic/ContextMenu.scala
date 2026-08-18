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
  * Honest deviation (server-side geometry is declared, not measured): kyo's
  * `MouseEvent` carries no pointer coordinates, so the menu anchors to the
  * TARGET REGION (an [[Overlay]] at `BottomStart` of the wrapped element), not
  * to the pointer position. Prime opens the panel at the click point.
  *
  * Keyboard (WAI-ARIA menu): the open panel seeds focus and holds it, so the
  * full navigation works without a prior click — ArrowUp/Down rove Prime's
  * `.p-focus` over the enabled rows, ArrowRight opens the focused submenu
  * (landing on its first row), ArrowLeft climbs back out, Home/End jump, Enter
  * or Space activates a leaf (runs its `onSelect`, closes the menu), and Escape
  * closes one level then the whole menu at the root. Nested submenu panels do
  * NOT seed focus — the highlight rides `aria-activedescendant` (wired by
  * `id(...)`) from the root panel that owns the keys.
  */
final case class ContextMenu private (
    itemsV: List[MenuItem] = Nil,
    kids: List[UI] = Nil,
    idV: Maybe[String] = Absent
) extends Node:
    type Self = ContextMenu

    /** Appends menu items ([[MenuItem]] rows, `MenuItem.separator` dividers;
      * nested `items(...)` open side submenus).
      */
    def items(is: MenuItem*): ContextMenu = copy(itemsV = itemsV ++ is.toList)

    /** Adds target children — the region whose right-click opens the menu. */
    def apply(cs: UI*): ContextMenu = copy(kids = kids ++ cs)

    /** Base id for the menu — enables `aria-activedescendant` (the focused row is
      * exposed as `s"$id-active"`). Keyboard navigation works without it.
      */
    def id(v: String): ContextMenu = copy(idV = Present(v))

    /** Paths of the items carrying submenus (one open/closed ref each). */
    private[uic] def submenuPaths: List[List[Int]] = MenuRender.submenuPaths(itemsV)

    private[uic] def render(using Frame): UI =
        // Open state, keyboard focus path, and one signal per submenu are allocated
        // by this effectful mount; static projections (SSG, the SSR page HTML)
        // render the closed target region inert.
        val stat: UI = body(false, Absent, Nil, Absent, Map.empty.withDefaultValue(false), Absent)
        UI.mounted {
            for
                openRef <- Signal.initRef(false)
                focus   <- Signal.initRef(List.empty[Int])
                refs    <- Kyo.foreach(submenuPaths)(p => Signal.initRef(false).map(p -> _))
            yield wired(openRef, focus, refs.toList)
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes (golden-test seam). */
    private[uic] def wired(
        openRef: SignalRef[Boolean],
        focus: SignalRef[List[Int]],
        refs: List[(List[Int], SignalRef[Boolean])]
    )(using Frame): UI =
        openRef.render { isOpen =>
            focus.render { f =>
                MenuRender.renderAll(refs) { open =>
                    body(isOpen, Present(openRef), f, Present(focus), open.withDefaultValue(false), Present(refs))
                }
            }
        }

    private def body(
        isOpen: Boolean,
        openRef: Maybe[SignalRef[Boolean]],
        focus: List[Int],
        focusRef: Maybe[SignalRef[List[Int]]],
        open: Map[List[Int], Boolean],
        refs: Maybe[List[(List[Int], SignalRef[Boolean])]]
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

        def openMenu: Any < Async =
            openRef match
                case Present(r) => for _ <- resetTree; _ <- resetFocus; _ <- r.set(true) yield ()
                case Absent     => ()

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
                    val listUI: UI = listEl(rows.map(toChild)*)
                    List(
                        // Host-gated (renderOpen): this component already subscribes to
                        // openRef in `wired` — the single-subscription form avoids the
                        // duplicated-panel race. Escape/outside-click write false back;
                        // the highlight/tree reset rides the NEXT right-click's openMenu.
                        Overlay(r)
                            .matchWidth(false)
                            .animate(false)
                            .panelClass("p-contextmenu")
                            .panelClass("p-component")
                            .onPanelKeyDown(keyHandler)(listUI)
                            .renderOpen
                    )
                case _ => Nil

        var target = div
            .cssClass("p-uic-contextmenu-target")
            .cssClass("p-uic-overlay-anchor")
            .aria("haspopup", "menu")
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
