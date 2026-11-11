package kyo.uic

import kyo.*
import kyo.UI.*

/** ContextMenu: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
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
  * WHAT THE WRAPPER IS, and what that rules out: a `div`
  * (`div.p-uic-contextmenu-target`). So the wrapping form can only go where a
  * `div` may go, and the placement it most often wants is the one place a `div`
  * may not: between a `<table>`/`<tbody>` and its `<tr>`. The HTML parser hoists
  * such a `div` clean out of the table, so a per-ROW menu cannot be had by
  * wrapping the row. Two ways round it, in order of preference:
  *   1. Wrap the TABLE and identify the row yourself. That is what [[DataTable]] does
  *      for the tables it owns ([[DataTable.contextMenuRow]] plus
  *      [[DataTable.onContextMenuRowChange]]), and what a hand-built table has to do
  *      by hand: a ref written from each row's own `onContextMenu`, which fires
  *      first by bubbling and leaves the menu reading a settled target.
  *   2. [[targetless]]: the panel alone, opened by a handler you put on
  *      whatever element you already have. That is the form for a `<tr>`, a
  *      `<canvas>` region, or a grid this library did not render.
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
  * full navigation works without a prior click: ArrowUp/Down rove Prime's
  * `.p-focus` over the enabled rows, ArrowRight opens the focused submenu
  * (landing on its first row), ArrowLeft climbs back out, Home/End jump, Enter
  * or Space activates a leaf (runs its `onSelect`, closes the menu), and Escape
  * closes one level then the whole menu at the root. Nested submenu panels do
  * NOT seed focus; the highlight rides `aria-activedescendant` (wired by
  * `id(...)`) from the root panel that owns the keys. The focused row is exposed
  * as `s"$id-active"`; keyboard navigation works without an id, only the ARIA
  * hook is omitted.
  */
final class ContextMenu private[uic] (private[uic] val p: ContextMenu.Props) extends Node, HasElementId, HasProps:
    import p.*

    type Self = ContextMenu

    /** Appends menu items ([[MenuItem]] rows, `MenuItem.separator` dividers;
      * nested `items(...)` open side submenus), each built directly or from `_` (see [[MenuItemScope]]), or a signal of
      * items for a menu whose entries follow what it was opened on (see [[LiveEntry]]).
      */
    def items(is: MenuItemLiveEntry*): ContextMenu = new ContextMenu(p.copy(entriesV = entriesV ++ is))

    /** Adds target children: the region whose right-click opens the menu. */
    def apply(cs: UI*): ContextMenu = new ContextMenu(p.copy(kids = kids ++ cs))

    /** Allocates the state a [[targetless]] menu keeps.
      *
      * Runs in the caller's own `UI.mounted`, which is where the menu's lifetime now sits: a
      * targetless menu mounts nothing of its own, so it has no instance to lose and nothing for a
      * re-render around it to reset. The open submenus are one set of paths ([[Submenus]]), so the
      * state does not depend on the items it is later rendered with.
      */
    def state(using Frame): ContextMenu.State < Sync =
        for
            open  <- Signal.initRef(false)
            focus <- Anchored.init(List.empty[Int])
            at    <- Signal.initRef(Absent: Maybe[UI.Point])
            subs  <- Submenus.init()
            base  <- UI.id
        yield new ContextMenu.State(open, at, focus, subs, base)

    /** The panel WITHOUT a target region: no wrapper `div`, no mount, no right-click listener of
      * its own; only the floating panel, gated on the state's open flag.
      *
      * This is the form for a target the wrapping `div` cannot reach: a `<tr>`, a `<canvas>`
      * region, a grid this library did not render. The listener and the panel come apart, so each
      * goes where it is legal: `ContextMenu.State.openAt` onto the element you already have, and
      * this render OUTSIDE the table (a sibling of it): the panel portals to `document.body`
      * once it opens, but the markup it is written into still has to be markup a `div` may live
      * in.
      *
      * What you give up against the wrapping form: it cannot suppress the browser's native menu
      * for you, because it does not own the element the right-click lands on. `openAt` on a typed
      * `onContextMenu` is what does that: the kyo client suppresses the native menu for any
      * element with a context-menu handler in its ancestor chain.
      */
    def targetless(state: ContextMenu.State): ContextMenu = new ContextMenu(p.copy(stateV = Present(state)))

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new ContextMenu(p.copy(access = v))

    private[uic] def render(using Frame): UI =
        stateV match
            case Present(st) => panelOnly(st)
            case Absent      => wrapping

    /** [[targetless]]: the panel alone, over the state the caller allocated. Nothing is mounted here
      * (the state came from the caller's own mount), so this render is pure, and the whole of
      * F-27 (a keyless mount reset by every enclosing emission) cannot arise in this form.
      */
    private def panelOnly(st: ContextMenu.State)(using Frame): UI =
        val self = new ContextMenu(p.copy(access = access.copy(id = if access.id.isDefined then access.id else Present(st.base))))
        self.panelRegion(st.open, st.focus, st.at, st.submenus)

    private def wrapping(using Frame): UI =
        // Open state, keyboard focus path, and the open submenus are allocated
        // by this mount, which draws in the render pass.
        val mount = UI.mounted {
            for
                openRef <- Signal.initRef(false)
                focus   <- Anchored.init(List.empty[Int])
                // Where the last right-click was. Not a render input: it is bound onto the open
                // panel as a style, so the point can change without rebuilding the panel.
                at   <- Signal.initRef(Absent: Maybe[UI.Point])
                subs <- Submenus.init()
                base <- UI.id
            yield wired(openRef, focus, at, subs, base)
        }
        // An id is the caller declaring which menu this is, so it is also the identity the
        // instance keeps. Without one the mount stays keyless, which is what it always was.
        //
        // Keyless was wrong for a menu that sits in a region something else re-renders: the
        // effect re-runs per enclosing emission, `openRef` (the mount's OWN state) resets,
        // and the whole subtree under the target is rebuilt each time. The wreckage is worse
        // than a reset flag: a subscription of the departing content
        // that emits into a range the rebuild already took away dies on `Unknown reactive
        // range`, and a dead subscription never paints again. Measured on a page whose store
        // writes re-render around the menu: the menu opened once and then never again.
        //
        // What a key costs: the effect runs ONCE, so `items(...)` and the target children are
        // read at mount time. Anything that must change afterwards belongs in a `Signal`,
        // which is what `MenuItem`'s reactive label and `disabled` are for, and what
        // `Overlay` already assumes by keying its own panel on the same id.
        access.id match
            case Present(id) => mount.keyed(id)
            case Absent      => mount
        end match
    end wrapping

    /** The subscription tree the mount publishes (golden-test seam).
      *
      * `base` is the id the announcement is built from, which the mount mints. A caller's own
      * `id(...)` still wins, but the announcement no longer waits for one: `aria-activedescendant`
      * and the row id it names are the whole of what a screen reader hears about the highlighted
      * row.
      */
    private[uic] def wired(
        openRef: SignalRef[Boolean],
        focus: Anchored[List[Int]],
        at: SignalRef[Maybe[UI.Point]],
        subs: Submenus,
        base: String
    )(using Frame): UI =
        // See Menu.wired for why the resolution sits inside the mount's content. It matters
        // most here: `openRef` IS the mount's state, so resolving one level higher would
        // close the menu on the right-click that opened it, which is the whole reason this
        // slot could not simply be rebuilt per target.
        //
        // What the regions may NOT contain is the target: the caller's content depends on none
        // of this, and putting it inside meant that an item reading a signal (a label counting
        // the selection, a `disabled` following it) rebuilt everything the menu was attached to
        // whenever that signal moved. Measured on a twenty-row keyed list under a menu whose label
        // counts the rows: one row changed, eighty-one rendered. So the shell and the right-click
        // that opens it are built once, out here, and only the panel is placed reactively.
        val self = new ContextMenu(p.copy(access = access.copy(id = if access.id.isDefined then access.id else Present(base))))
        self.targetShell(Present(self.openHandler(Present(openRef), Present(focus), Present(subs), Present(at)))) {
            List(self.panelRegion(openRef, focus, at, subs))
        }
    end wired

    /** The reactive half, and the ONLY half: every signal this component watches reaches the
      * panel and nothing else. Shared by both forms, which is what makes [[targetless]] a
      * placement choice rather than a second implementation.
      */
    private def panelRegion(
        openRef: SignalRef[Boolean],
        focus: Anchored[List[Int]],
        at: SignalRef[Maybe[UI.Point]],
        subs: Submenus
    )(using Frame): UI =
        // The items, the open state, the highlight and the submenus are read as ONE region (see [[ReactiveValue.and]]): opening at
        // a point writes the open state and the highlight together, and a nested region would repaint what it closed over.
        // The highlight and the submenus follow their items over the roots shown ([[Anchored]]).
        val read = MenuRender.readRooted(entriesV).and(ReactiveValue.Dyn(openRef)).and(focus.reading).and(subs.reading)
        read.render { case (((Rooted(roots, items), isOpen), f), open) =>
            val fv = focus.view(f, roots)
            val sv = subs.view(open, roots)
            UI.fragment(new ContextMenu(p.copy(itemsV = items)).panelFor(
                isOpen,
                Present(openRef),
                fv.value,
                Present(fv),
                sv.open,
                Present(sv),
                Present(at)
            )*)
        }
    end panelRegion

    /** The target region and whatever is asked to hang inside it.
      *
      * The children are the caller's own content, and they depend on nothing this component
      * watches: not the open state, not an item's label, not an item's `disabled`. Which is why
      * they are assembled HERE, outside every region, and only the panel is placed reactively:
      * rebuilding them would take with them every keyed list, open editor and scroll position the
      * caller put under the menu, invisibly, since the same markup comes back out.
      */
    private def targetShell(onOpen: Maybe[MouseEvent => UI.Action])(content: List[UI])(using Frame): UI =
        var target = div
            .cssClass("p-uic-contextmenu-target")
            .cssClass("p-uic-overlay-anchor")
            .aria(Aria.haspopup -> "menu")
        // The wrapped region is this component's root, so the element id lands here (HasElementId),
        // and the panel's derived id hangs off it.
        access.id.foreach(b => target = target.id(b))
        onOpen.foreach(h => target = target.onContextMenu(h))
        target(UI.children(kids ++ content)*)
    end targetShell

    /** The right-click that opens the menu. Reads only refs, so it is built once. */
    private def openHandler(
        openRef: Maybe[SignalRef[Boolean]],
        focusRef: Maybe[Anchored[List[Int]]],
        subs: Maybe[Submenus],
        at: Maybe[SignalRef[Maybe[UI.Point]]]
    )(using Frame): MouseEvent => UI.Action =
        val resetTree: UI.Action = subs match
            case Present(s) => s.closeAll
            case Absent     => ()
        val resetFocus: UI.Action = focusRef match
            case Present(fr) => fr.reset(Nil)
            case Absent      => ()
        e =>
            // The point is written BEFORE the open, so the panel never exists without one to be at.
            (openRef, at) match
                case (Present(r), Present(pt)) =>
                    for _ <- resetTree; _ <- resetFocus; _ <- pt.set(e.position); _ <- r.set(true) yield ()
                case _ => ()
    end openHandler

    /** The overlay panel, which is the only part of this component that any of its signals reach. */
    private def panelFor(
        isOpen: Boolean,
        openRef: Maybe[SignalRef[Boolean]],
        focus: List[Int],
        focusRef: Maybe[Anchored.View[List[Int]]],
        open: Set[List[Int]],
        subs: Maybe[Submenus.View],
        at: Maybe[SignalRef[Maybe[UI.Point]]]
    )(using Frame): List[UI] =
        def resetTree: UI.Action =
            subs match
                case Present(s) => s.openExactly(Absent)
                case Absent     => ()

        def resetFocus: UI.Action =
            focusRef match
                case Present(fr) => fr.set(Nil)
                case Absent      => ()

        // The mouse close path (a leaf click) and each right-click re-open funnel
        // through these so the next open starts clean.
        def closeMenu: UI.Action =
            openRef match
                case Present(r) => for _ <- r.set(false); _ <- resetTree; _ <- resetFocus yield ()
                case Absent     => ()

        val keyHandler: KeyboardEvent => UI.Action = e =>
            focusRef match
                case Present(fr) =>
                    MenuNav.onKey(itemsV, MenuNav.Orientation.Vertical, focus, e.key) match
                        case Present(step) => MenuRender.applyStep(itemsV, subs, fr, openRef.fold[UI.Action](())(_.set(false)), step)
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
                        subs,
                        rootSubmenuIcon = Icons.angleRight,
                        rootAnchor = OverlayAnchor.RightStart,
                        focused = if focus.isEmpty then Absent else Present(focus),
                        onLeafActivate = Present(closeMenu),
                        roving = true,
                        idBase = access.id
                    )
                    var listEl = ul.cssClass("p-contextmenu-root-list").role("menu")
                    if focus.nonEmpty then access.id.foreach(b => listEl = listEl.aria(Aria.activedescendant -> s"$b-active"))
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
                    val listUI: UI = listEl(UI.children(rows)*)
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
                    (at, access.id) match
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
        panel
    end panelFor
end ContextMenu

object ContextMenu:
    final private[uic] case class Props(
        // The rows the panel draws, resolved from `entriesV` by the panel's region.
        itemsV: List[MenuItem] = Nil,
        entriesV: List[MenuItemLiveEntry] = Nil,
        kids: List[UI] = Nil,
        access: Accessibility = Accessibility.none,
        stateV: Maybe[ContextMenu.State] = Absent
    )

    /** A context menu over `items`, wrapping the target region given via
      * `apply(...)`: right-click inside it opens the menu. Each item is built directly or from `_`
      * (`Seq(_.item("Copy"), _.item.separator)`, see [[MenuItemScope]]).
      */
    def apply(items: Seq[MenuItemLiveEntry]): ContextMenu = new ContextMenu(Props(entriesV = items.toList))

    /** An empty context menu; add items via [[ContextMenu.items]] and target
      * children via `apply(...)`.
      */
    def apply(): ContextMenu = new ContextMenu(Props())

    /** What a [[ContextMenu.targetless]] menu keeps between right-clicks, held by the caller
      * because the panel has no mount of its own. Allocate it with [[ContextMenu.state]].
      *
      * The fields are the component's, not the caller's: what a caller does with this value is
      * [[openAt]] on its own element and [[close]] when something else should shut the menu. That
      * boundary is the point. Opening is four writes in one order (collapse the submenu tree,
      * clear the highlight, write the point, THEN set open), and the ordering is not decorative:
      * the panel is placed from the point, so a panel that exists before there is a point to be
      * at renders at wherever the last one was. Handing out `openAt` instead of the refs is what
      * keeps that from being the caller's problem to get right.
      */
    final class State private[uic] (
        private[uic] val open: SignalRef[Boolean],
        private[uic] val at: SignalRef[Maybe[UI.Point]],
        private[uic] val focus: Anchored[List[Int]],
        private[uic] val submenus: Submenus,
        private[uic] val base: String
    ):
        /** The right-click handler for the caller's own element: `tr.onContextMenu(state.openAt)`.
          *
          * Bind it to the TYPED `onContextMenu` overload; the payload-free one registers the
          * event and drops the position, which still opens a menu, just not at the pointer.
          */
        def openAt(using Frame): MouseEvent => UI.Action =
            e =>
                for
                    _ <- submenus.closeAll
                    _ <- focus.reset(Nil)
                    _ <- at.set(e.position)
                    _ <- open.set(true)
                yield ()

        /** Closes the menu and forgets where it was, for the closes the panel cannot see: the
          * row it was opened on going away, a navigation, a caller's own Escape. Escape on the
          * panel, an outside click and a leaf activation already close it themselves.
          */
        def close(using Frame): UI.Action =
            for
                _ <- open.set(false)
                _ <- submenus.closeAll
                _ <- focus.reset(Nil)
            yield ()
    end State
end ContextMenu
