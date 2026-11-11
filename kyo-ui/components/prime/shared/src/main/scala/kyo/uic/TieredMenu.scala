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
  * Two modes like Prime: inline (default) and [[popup]]; the popup
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
final class TieredMenu private[uic] (private[uic] val p: TieredMenu.Props) extends Node, HasElementId, HasProps:
    import p.*

    type Self = TieredMenu

    /** Appends root items ([[MenuItem]]; nested `items(...)` open side
      * submenus), each built directly or from `_` (see [[MenuItemScope]]), or a signal of items for a menu whose
      * entries change while it is shown (see [[LiveEntry]]).
      */
    def items(is: MenuItemLiveEntry*): TieredMenu = new TieredMenu(p.copy(entriesV = entriesV ++ is))

    /** Popup mode: the menu renders as a floating [[Overlay]] panel (Prime's `.p-tieredmenu-overlay`) over the open state `v`. A
      * `SignalRef` binds two way: a pick, Tab, Escape and an outside click write false, then [[onPopupChange]] hears
      * it. A plain `Signal` binds one way: the panel follows it and only reports. A constant nobody hears is fixed: an
      * open panel stays, and nothing closes it. A signal nothing hears is reported in a card. Without `popup` the menu
      * renders inline.
      */
    def popup(v: Boolean | Signal[Boolean]): TieredMenu = new TieredMenu(p.copy(popupSlot = popupSlot.bind(v), popupOwnFlag = false))

    /** Popup mode with the open state kept by the menu itself, starting open, as an overlay nothing inside can open
      * does: the panel shows until the reader closes it. [[onPopupChange]] still hears every close.
      */
    def popup(): TieredMenu = new TieredMenu(p.copy(popupSlot = popupSlot.unbound, popupOwnFlag = true))

    /** Fired with the open state the reader asks for (false for a pick, Tab, Escape and an outside click), after a
      * two-way [[popup]] is written.
      */
    def onPopupChange(f: Boolean => UI.Action)(using Frame): TieredMenu = new TieredMenu(p.copy(popupSlot = popupSlot.hear(f)))

    /** Asks for the popup closed: a two-way binding is written, then the close reported. Inline, there is nothing to close. */
    private def closePopup(using Frame): UI.Action =
        if popupSlot.binding.isDefined then popupSlot.change(false) else ()

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new TieredMenu(p.copy(access = v))

    private[uic] def render(using Frame): UI =
        // `popup()`: the open state is the menu's own, allocated in the mount beside the highlight.
        val ownsPopup = popupSlot.binding.isEmpty && popupOwnFlag
        val menu      = UI.mounted {
            for
                subs  <- Submenus.init()
                focus <- Anchored.init(List.empty[Int])
                base  <- UI.id
                popup <- if ownsPopup then popupSlot.owned(true) else Kyo.lift(popupSlot)
            yield new TieredMenu(p.copy(popupSlot = popup)).wired(subs, focus, base)
        }
        // A popup bound one way that nothing hears: every close the reader asks for is painted away again.
        Diagnostics.before(menu)("TieredMenu")(Diagnostic.unheard(popupSlot, TieredMenu.popupName))
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
        // a popup's open state are read as ONE region (see [[ReactiveValue.and]]): a key writes the highlight and the submenus (and
        // a popup's ref) together, and a nested region would repaint the values it closed over. Both follow their items
        // over the roots shown ([[Anchored]]).
        val popup = ReactiveValue.optional(popupSlot.binding)
        MenuRender.readRooted(entriesV).and(focus.reading).and(subs.reading).and(popup).render {
            case (((Rooted(roots, items), f), open), shown) =>
                val fv = focus.view(f, roots)
                val sv = subs.view(open, roots)
                new TieredMenu(p.copy(itemsV =
                    items
                )).body(sv.open, Present(sv), fv.value, Present(fv), Present(access.id.getOrElse(base)), shown)
        }
    end wired

    private def body(
        open: Set[List[Int]],
        subs: Maybe[Submenus.View],
        focus: List[Int],
        focusRef: Maybe[Anchored.View[List[Int]]],
        // What internal parts derive from: the caller's id when there is one, else the mount's
        // mint. Kept apart from `access.id`, which is only ever the caller's; see Menu.wired.
        partBase: Maybe[String],
        // Whether a popup is open, where the host read it in its own region; `Absent` lets the Overlay gate itself.
        popupShown: Maybe[Boolean] = Absent
    )(using Frame): UI =
        val keyHandler: KeyboardEvent => UI.Action = e =>
            focusRef match
                case Present(fr) =>
                    MenuNav.onKey(itemsV, MenuNav.Orientation.Vertical, focus, e.key) match
                        case Present(step) => MenuRender.applyStep(itemsV, subs, fr, closePopup, step)
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
            // A pick by pointer closes the popup as a pick by key does (applyStep), after the leaf's action.
            onLeafActivate = if popupSlot.binding.isDefined then Present(closePopup) else Absent,
            roving = true,
            idBase = partBase
        )
        var list = ul.cssClass("p-tieredmenu-root-list").role("menu")
        if focus.nonEmpty then partBase.foreach(b => list = list.aria(Aria.activedescendant -> s"$b-active"))
        // In a popup the component's root is the overlay panel, which this does not own, so the
        // caller's id lands on the `ul[role=menu]`; see Menu.body for the same split.
        if popupSlot.binding.isDefined then access.id.foreach(v => list = list.id(v))
        popupSlot.binding match
            case Present(_) =>
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
                val listUI: UI = list(UI.children(rowsUI)*)
                // Quiet: the menu reports its own binding.
                val overlay = Overlay.withOpen(popupSlot.quieted)
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
                access.id.foreach(v => root = root.id(v))
                root(list(UI.children(rowsUI)*))
        end match
    end body
end TieredMenu

object TieredMenu:
    final private[uic] case class Props(
        // The root rows `body` draws, resolved from `entriesV` by the render.
        itemsV: List[MenuItem] = Nil,
        entriesV: List[MenuItemLiveEntry] = Nil,
        popupSlot: Controlled[Boolean] = Controlled(),
        popupOwnFlag: Boolean = false,
        access: Accessibility = Accessibility.none
    )

    /** An empty tiered menu; add root items via [[TieredMenu.items]]. */
    def apply(): TieredMenu = new TieredMenu(Props())

    private val popupName = Controlled.Name("TieredMenu", "the popup", "a close", "onPopupChange")
end TieredMenu
