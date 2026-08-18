package kyo.uic

import kyo.*
import kyo.UI.*

/** TieredMenu — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * TieredMenu anatomy: `div.p-tieredmenu.p-component` >
  * `ul.p-tieredmenu-root-list[role=menu]` of `li.p-tieredmenu-item` rows;
  * items with children open side-nested `p-tieredmenu-submenu` panels, the
  * sheet's `left: 100%; top: 0` geometry), so the extracted `@primeuix`
  * tieredmenu CSS applies verbatim.
  *
  * The vertical sibling of [[Menubar]] — the same nested-[[Overlay]] machinery:
  * click toggles a submenu (deliberate deviation from Prime's hover-open — a
  * server round-trip per hover is heavy), an outside click closes one level
  * (topmost backdrop), Escape closes one level (per-panel keydown
  * consumption), panels seed focus.
  *
  * Two modes like Prime: inline (default) and `popup(openRef)` — the popup
  * wraps the root list in an Overlay with Prime's `.p-tieredmenu-overlay` skin
  * (whose JS-era `will-change: transform` is neutralized by the theme glue: it
  * would otherwise trap the nested submenus' `position: fixed` backdrops).
  *
  * Keyboard (WAI-ARIA menu): the root list (inline) or the panel (popup) is the
  * single tab stop and seeds/holds focus; ArrowUp/Down move Prime's `.p-focus`
  * over the enabled siblings, ArrowRight opens the focused submenu (landing on
  * its first row), ArrowLeft climbs back out one level, Home/End jump, Enter or
  * Space activates a leaf (runs its action, then collapses the tree / closes the
  * popup), and Escape closes one level (then the popup at the root). The nested
  * submenu panels do NOT seed focus — the highlight rides `aria-activedescendant`
  * from the host, wired by `id(...)`.
  *
  * Honest deferrals as Menubar: no mobile mode; typeahead is not implemented.
  */
final case class TieredMenu private (
    itemsV: List[MenuItem] = Nil,
    popupRefV: Maybe[SignalRef[Boolean]] = Absent,
    idV: Maybe[String] = Absent
) extends Node:
    type Self = TieredMenu

    /** Appends root items ([[MenuItem]]; nested `items(...)` open side
      * submenus).
      */
    def items(is: MenuItem*): TieredMenu = copy(itemsV = itemsV ++ is.toList)

    /** Popup mode: the whole menu renders as a floating [[Overlay]] panel bound
      * two-way to `ref` (Prime's `.p-tieredmenu-overlay`).
      */
    def popup(ref: SignalRef[Boolean]): TieredMenu = copy(popupRefV = Present(ref))

    /** Base id for the menu — enables `aria-activedescendant` (the focused row is
      * exposed as `s"$id-active"`). Keyboard navigation works without it.
      */
    def id(v: String): TieredMenu = copy(idV = Present(v))

    /** Paths of the items carrying submenus (one open/closed ref each). */
    private[uic] def submenuPaths: List[List[Int]] = MenuRender.submenuPaths(itemsV)

    private[uic] def render(using Frame): UI =
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
                    MenuNav.onKey(itemsV, MenuNav.Orientation.Vertical, focus, e.key) match
                        case Present(step) => MenuRender.applyStep(itemsV, refs, fr, popupRefV, step)
                        case Absent        => ()
                case Absent => ()

        val rowsUI = MenuTree.items(
            "tieredmenu",
            itemsV,
            Nil,
            open,
            refs,
            rootSubmenuIcon = Icons.angleRight,
            rootAnchor = OverlayAnchor.RightStart,
            focused = if focus.isEmpty then Absent else Present(focus),
            roving = true,
            idBase = idV
        )
        var list = ul.cssClass("p-tieredmenu-root-list").role("menu")
        if focus.nonEmpty then idV.foreach(b => list = list.aria("activedescendant", s"$b-active"))
        popupRefV match
            case Present(openRef) =>
                val listUI: UI = list(rowsUI.map(toChild)*)
                Overlay(openRef)
                    .matchWidth(false)
                    .animate(false)
                    .panelClass("p-tieredmenu")
                    .panelClass("p-component")
                    .panelClass("p-tieredmenu-overlay")
                    .onPanelKeyDown(keyHandler)
                    .apply(listUI)
                    .render
            case Absent =>
                if focusRef.isDefined then list = list.tabIndex(0).preventScrollKeys.onKeyDown(keyHandler)
                div.cssClass("p-tieredmenu").cssClass("p-component")(toChild(list(rowsUI.map(toChild)*)))
        end match
    end body
end TieredMenu

object TieredMenu:
    /** An empty tiered menu — add root items via [[TieredMenu.items]]. */
    def apply(): TieredMenu = new TieredMenu()
