package kyo.uic

import kyo.*
import kyo.UI.*

/** TieredMenu: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * TieredMenu anatomy: `div.p-tieredmenu.p-component` >
  * `ul.p-tieredmenu-root-list[role=menu]` of `li.p-tieredmenu-item` rows;
  * items with children open side-nested `p-tieredmenu-submenu` panels, the
  * sheet's `left: 100%; top: 0` geometry), so the extracted `@primeuix`
  * tieredmenu CSS applies verbatim.
  *
  * The vertical sibling of [[Menubar]], with the same nested-[[Overlay]] machinery:
  * click toggles a submenu (deliberate deviation from Prime's hover-open, since a
  * server round-trip per hover is heavy), an outside click closes one level
  * (topmost backdrop), Escape closes one level (per-panel keydown
  * consumption), panels seed focus.
  *
  * Two modes like Prime: inline (default) and `popup(openRef)`; the popup
  * wraps the root list in an Overlay with Prime's `.p-tieredmenu-overlay` skin
  * (whose JS-era `will-change: transform` is neutralized by the theme glue: it
  * would otherwise trap the nested submenus' `position: fixed` backdrops).
  *
  * Keyboard (WAI-ARIA menu): the root list (inline) or the panel (popup) is the
  * single tab stop and seeds/holds focus; ArrowUp/Down move Prime's `.p-focus`
  * over the enabled siblings, ArrowRight opens the focused submenu (landing on
  * its first row), ArrowLeft climbs back out one level, Home/End jump, Enter or
  * Space activates a leaf (runs its action, then collapses the tree / closes the
  * popup), and Escape closes one level (then the popup at the root). Only the ROOT
  * list ever holds focus: the nested submenu panels do not seed it, and the
  * highlight rides `aria-activedescendant` from the root, wired by `id(...)`;
  * the focused row is exposed as `s"$id-active"`. Keyboard navigation works
  * without an id, only the ARIA hook is omitted.
  *
  * Honest deferrals as Menubar: no mobile mode; typeahead is not implemented.
  */
final case class TieredMenu private (
    // The root rows `body` draws, resolved from `entriesV` by the render.
    itemsV: List[MenuItem] = Nil,
    entriesV: List[MenuItemLiveEntry] = Nil,
    popupRefV: Maybe[SignalRef[Boolean]] = Absent,
    idV: Maybe[String] = Absent
) extends Node, HasElementId:
    type Self = TieredMenu

    /** Appends root items ([[MenuItem]]; nested `items(...)` open side
      * submenus), each built directly or from `_` (see [[MenuItemScope]]), or a signal of items for a menu whose
      * entries change while it is shown (see [[LiveEntry]]).
      */
    def items(is: MenuItemLiveEntry*): TieredMenu = copy(entriesV = entriesV ++ is)

    /** Popup mode: the whole menu renders as a floating [[Overlay]] panel bound
      * two-way to `ref` (Prime's `.p-tieredmenu-overlay`).
      */
    def popup(ref: SignalRef[Boolean]): TieredMenu = copy(popupRefV = Present(ref))

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): TieredMenu = copy(idV = v)

    /** Reads it back. */
    private[uic] def elementId: Maybe[String] = idV

    private[uic] def render(using Frame): UI =
        val stat: UI = MenuRender.readItems(entriesV).render(items => copy(itemsV = items).body(Set.empty, Absent, Nil, Absent, idV))
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
        // See Menu.wired for why the resolution sits inside the mount's content. The items, the highlight, the submenus and
        // a popup's open state are read as ONE region (see [[Reading]]): a key writes the highlight and the submenus (and
        // a popup's ref) together, and a nested region would repaint the values it closed over. Both follow their items
        // over the roots shown ([[Anchored]]).
        val popup = popupRefV.fold(Reading.Fixed(Maybe.empty[Boolean]))(r => Reading.Live(r.map(o => Present(o))))
        MenuRender.readRooted(entriesV).and(focus.reading).and(subs.reading).and(popup).render {
            case (((Rooted(roots, items), f), open), shown) =>
                val fv = focus.view(f, roots)
                val sv = subs.view(open, roots)
                copy(itemsV = items).body(sv.open, Present(sv), fv.value, Present(fv), Present(idV.getOrElse(base)), shown)
        }
    end wired

    private def body(
        open: Set[List[Int]],
        subs: Maybe[Submenus.View],
        focus: List[Int],
        focusRef: Maybe[Anchored.View[List[Int]]],
        // What internal parts derive from: the caller's id when there is one, else the mount's
        // mint. Kept apart from `idV`, which is only ever the caller's; see Menu.wired.
        partBase: Maybe[String],
        // Whether a popup is open, where the host read it in its own region; `Absent` lets the Overlay gate itself.
        popupShown: Maybe[Boolean] = Absent
    )(using Frame): UI =
        val keyHandler: KeyboardEvent => UI.Action = e =>
            focusRef match
                case Present(fr) =>
                    MenuNav.onKey(itemsV, MenuNav.Orientation.Vertical, focus, e.key) match
                        case Present(step) => MenuRender.applyStep(itemsV, subs, fr, popupRefV, step)
                        case Absent        => ()
                case Absent => ()

        val rowsUI = MenuTree.items(
            "tieredmenu",
            itemsV,
            Nil,
            open,
            subs,
            rootSubmenuIcon = Icons.angleRight,
            rootAnchor = OverlayAnchor.RightStart,
            focused = if focus.isEmpty then Absent else Present(focus),
            roving = true,
            idBase = partBase
        )
        var list = ul.cssClass("p-tieredmenu-root-list").role("menu")
        if focus.nonEmpty then partBase.foreach(b => list = list.aria("activedescendant", s"$b-active"))
        // In a popup the component's root is the overlay panel, which this does not own, so the
        // caller's id lands on the `ul[role=menu]`; see Menu.body for the same split.
        if popupRefV.isDefined then idV.foreach(v => list = list.id(v))
        popupRefV match
            case Present(openRef) =>
                // The LIST is what focus goes to, not the panel around it: the list is the
                // `role="menu"` and the thing carrying `aria-activedescendant`, and that attribute
                // is read off the focused element or off nothing at all. Escape is left to bubble
                // to the Overlay, which is the one place that decides what closing means.
                list = list
                    .tabIndex(-1)
                    .focusAuto(true)
                    .focusRestore(true)
                    .preventScrollKeys
                    .onKeyDown(e => if e.key == Keyboard.Escape then () else keyHandler(e))
                val listUI: UI = list(rowsUI.map(toChild)*)
                val overlay    = Overlay(openRef)
                    .matchWidth(false)
                    .animate(false)
                    .panelClass("p-tieredmenu")
                    .panelClass("p-component")
                    .panelClass("p-tieredmenu-overlay")
                    .seedFocus(false)
                    .apply(listUI)
                popupShown match
                    case Present(true)  => overlay.renderShown(partBase.map(b => s"$b-panel"))
                    case Present(false) => UI.empty
                    case Absent         => overlay.render
                end match
            case Absent =>
                // One tab stop from the first render; the keys are wired once the mount has a focus to move.
                list = list.tabIndex(0)
                if focusRef.isDefined then list = list.preventScrollKeys.onKeyDown(keyHandler)
                var root = div.cssClass("p-tieredmenu").cssClass("p-component")
                idV.foreach(v => root = root.id(v))
                root(toChild(list(rowsUI.map(toChild)*)))
        end match
    end body
end TieredMenu

object TieredMenu:
    /** An empty tiered menu; add root items via [[TieredMenu.items]]. */
    def apply(): TieredMenu = new TieredMenu()
