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
  * WHAT THE WRAPPER IS, and what that rules out: a `div`
  * (`div.p-uic-contextmenu-target`). So the wrapping form can only go where a
  * `div` may go, and the placement it most often wants is the one place a `div`
  * may not: between a `<table>`/`<tbody>` and its `<tr>`. The HTML parser hoists
  * such a `div` clean out of the table, so a per-ROW menu cannot be had by
  * wrapping the row. Two ways round it, in order of preference:
  *   1. Wrap the TABLE and identify the row yourself — what [[DataTable]] does
  *      for the tables it owns ([[DataTable.contextMenuRow]] plus
  *      [[DataTable.onRowContextMenu]]), and what a hand-built table has to do
  *      by hand: a ref written from each row's own `onContextMenu`, which fires
  *      first by bubbling and leaves the menu reading a settled target.
  *   2. [[targetless]] — the panel alone, opened by a handler you put on
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
    idV: Maybe[String] = Absent,
    stateV: Maybe[ContextMenu.State] = Absent
) extends Node, HasElementId:
    type Self = ContextMenu

    /** Appends menu items ([[MenuItem]] rows, `MenuItem.separator` dividers;
      * nested `items(...)` open side submenus).
      */
    def items(is: MenuItem*): ContextMenu = copy(itemsV = itemsV ++ is.toList)

    /** Adds target children — the region whose right-click opens the menu. */
    def apply(cs: UI*): ContextMenu = copy(kids = kids ++ cs)

    /** Allocates the state a [[targetless]] menu keeps, for these items.
      *
      * Runs in the caller's own `UI.mounted`, which is where the menu's lifetime now sits: a
      * targetless menu mounts nothing of its own, so it has no instance to lose and nothing for a
      * re-render around it to reset. Build it from the SAME value you later render — the submenu
      * refs are allocated per submenu path, and a state built from a different item tree would
      * leave the tree's open/closed slots unaddressed.
      */
    def state(using Frame): ContextMenu.State < (Sync & Env[UI.Commands]) =
        for
            open  <- Signal.initRef(false)
            focus <- Signal.initRef(List.empty[Int])
            at    <- Signal.initRef(Absent: Maybe[UI.Point])
            refs  <- Kyo.foreach(submenuPaths)(p => Signal.initRef(false).map(p -> _))
            cmds  <- UI.commands
            base  <- cmds.freshId
        yield new ContextMenu.State(open, at, focus, refs.toList, base)

    /** The panel WITHOUT a target region: no wrapper `div`, no mount, no right-click listener of
      * its own — only the floating panel, gated on the state's open flag.
      *
      * This is the form for a target the wrapping `div` cannot reach: a `<tr>`, a `<canvas>`
      * region, a grid this library did not render. The listener and the panel come apart, so each
      * goes where it is legal: `ContextMenu.State.openAt` onto the element you already have, and
      * this render OUTSIDE the table (a sibling of it) — the panel portals to `document.body`
      * once it opens, but the markup it is written into still has to be markup a `div` may live
      * in.
      *
      * What you give up against the wrapping form: it cannot suppress the browser's native menu
      * for you, because it does not own the element the right-click lands on. `openAt` on a typed
      * `onContextMenu` is what does that — the kyo client suppresses the native menu for any
      * element with a context-menu handler in its ancestor chain.
      */
    def targetless(state: ContextMenu.State): ContextMenu = copy(stateV = Present(state))

    /** Stores the element id. */
    private[uic] def withElementId(v: Maybe[String]): ContextMenu = copy(idV = v)

    /** Paths of the items carrying submenus (one open/closed ref each). */
    private[uic] def submenuPaths: List[List[Int]] = MenuRender.submenuPaths(itemsV)

    private[uic] def render(using Frame): UI =
        stateV match
            case Present(st) => panelOnly(st)
            case Absent      => wrapping

    /** [[targetless]]: the panel alone, over refs the caller allocated. Nothing is mounted here —
      * the state came from the caller's own mount — so this render is pure, and the whole of
      * F-27 (a keyless mount reset by every enclosing emission) cannot arise in this form.
      */
    private def panelOnly(st: ContextMenu.State)(using Frame): UI =
        val self = copy(idV = if idV.isDefined then idV else Present(st.base))
        self.panelRegion(st.open, st.focus, st.at, st.submenus)

    private def wrapping(using Frame): UI =
        // Open state, keyboard focus path, and one signal per submenu are allocated
        // by this effectful mount; static projections (SSG, the SSR page HTML)
        // render the closed target region inert.
        val stat: UI = body(false, Absent, Nil, Absent, Map.empty.withDefaultValue(false), Absent, Absent)
        val mount = UI.mounted {
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
        // An id is the caller declaring which menu this is, so it is also the identity the
        // instance keeps. Without one the mount stays keyless, which is what it always was.
        //
        // Keyless was wrong for a menu that sits in a region something else re-renders: the
        // effect re-runs per enclosing emission, `openRef` — the mount's OWN state — resets,
        // and the whole subtree under the target is rebuilt each time. kyo-ui says so out
        // loud after ten of them ("keyless UI.mounted at …: give it a stable identity"), and
        // the wreckage is worse than a reset flag: a subscription of the departing content
        // that emits into a range the rebuild already took away dies on `Unknown reactive
        // range`, and a dead subscription never paints again. Measured on a page whose store
        // writes re-render around the menu: the menu opened once and then never again.
        //
        // What a key costs: the effect runs ONCE, so `items(...)` and the target children are
        // read at mount time. Anything that must change afterwards belongs in a `Signal` —
        // which is what `MenuItem`'s reactive label and `disabled` are for, and what
        // `Overlay` already assumes by keying its own panel on the same id.
        idV match
            case Present(id) => mount.keyed(ContextMenu -> id)
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
        focus: SignalRef[List[Int]],
        at: SignalRef[Maybe[UI.Point]],
        refs: List[(List[Int], SignalRef[Boolean])],
        base: String
    )(using Frame): UI =
        // See Menu.wired for why the resolution sits inside the mount's content. It matters
        // most here: `openRef` IS the mount's state, so resolving one level higher would
        // close the menu on the right-click that opened it — which is the whole reason this
        // slot could not simply be rebuilt per target.
        //
        // What the regions may NOT contain is the target: the caller's content depends on none
        // of this, and putting it inside meant that an item reading a signal — a label counting
        // the selection, a `disabled` following it — rebuilt everything the menu was attached to
        // whenever that signal moved. Measured on a twenty-row keyed list under a menu whose label
        // counts the rows: one row changed, eighty-one rendered. So the shell and the right-click
        // that opens it are built once, out here, and only the panel is placed reactively.
        val self = copy(idV = if idV.isDefined then idV else Present(base))
        self.targetShell(Present(self.openHandler(Present(openRef), Present(focus), Present(refs), Present(at)))) {
            List(self.panelRegion(openRef, focus, at, refs))
        }
    end wired

    /** The reactive half, and the ONLY half: every signal this component watches reaches the
      * panel and nothing else. Shared by both forms, which is what makes [[targetless]] a
      * placement choice rather than a second implementation.
      */
    private def panelRegion(
        openRef: SignalRef[Boolean],
        focus: SignalRef[List[Int]],
        at: SignalRef[Maybe[UI.Point]],
        refs: List[(List[Int], SignalRef[Boolean])]
    )(using Frame): UI =
        MenuRender.resolveDisabled(itemsV) { items =>
            val resolved = copy(itemsV = items)
            openRef.render { isOpen =>
                focus.render { f =>
                    MenuRender.renderAll(refs) { open =>
                        UI.fragment(resolved.panelFor(
                            isOpen,
                            Present(openRef),
                            f,
                            Present(focus),
                            open.withDefaultValue(false),
                            Present(refs),
                            Present(at)
                        )*)
                    }
                }
            }
        }
    end panelRegion

    /** The target region and whatever is asked to hang inside it.
      *
      * The children are the caller's own content, and they depend on nothing this component
      * watches — not the open state, not an item's label, not an item's `disabled`. Which is why
      * they are assembled HERE, outside every region, and only the panel is placed reactively:
      * rebuilding them would take with them every keyed list, open editor and scroll position the
      * caller put under the menu, invisibly, since the same markup comes back out.
      */
    private def targetShell(onOpen: Maybe[MouseEvent => Any < Async])(content: List[UI])(using Frame): UI =
        var target = div
            .cssClass("p-uic-contextmenu-target")
            .cssClass("p-uic-overlay-anchor")
            .aria("haspopup", "menu")
        // The wrapped region is this component's root, so the element id lands here (HasElementId),
        // and the panel's derived id hangs off it.
        idV.foreach(b => target = target.id(b))
        onOpen.foreach(h => target = target.onContextMenu(h))
        target((kids ++ content).map(toChild)*)
    end targetShell

    /** The right-click that opens the menu. Reads only refs, so it is built once. */
    private def openHandler(
        openRef: Maybe[SignalRef[Boolean]],
        focusRef: Maybe[SignalRef[List[Int]]],
        refs: Maybe[List[(List[Int], SignalRef[Boolean])]],
        at: Maybe[SignalRef[Maybe[UI.Point]]]
    )(using Frame): MouseEvent => Any < Async =
        val resetTree: Any < Async = refs match
            case Present(rs) => MenuRender.openExactly(rs, Absent)
            case Absent      => ()
        val resetFocus: Any < Async = focusRef match
            case Present(fr) => fr.set(Nil)
            case Absent      => ()
        e =>
            // The point is written BEFORE the open, so the panel never exists without one to be at.
            (openRef, at) match
                case (Present(r), Present(pt)) =>
                    for _ <- resetTree; _ <- resetFocus; _ <- pt.set(e.position); _ <- r.set(true) yield ()
                case _ => ()
    end openHandler

    private def body(
        isOpen: Boolean,
        openRef: Maybe[SignalRef[Boolean]],
        focus: List[Int],
        focusRef: Maybe[SignalRef[List[Int]]],
        open: Map[List[Int], Boolean],
        refs: Maybe[List[(List[Int], SignalRef[Boolean])]],
        at: Maybe[SignalRef[Maybe[UI.Point]]]
    )(using Frame): UI =
        targetShell(if openRef.isDefined then Present(openHandler(openRef, focusRef, refs, at)) else Absent)(
            panelFor(isOpen, openRef, focus, focusRef, open, refs, at)
        )

    /** The overlay panel, which is the only part of this component that any of its signals reach. */
    private def panelFor(
        isOpen: Boolean,
        openRef: Maybe[SignalRef[Boolean]],
        focus: List[Int],
        focusRef: Maybe[SignalRef[List[Int]]],
        open: Map[List[Int], Boolean],
        refs: Maybe[List[(List[Int], SignalRef[Boolean])]],
        at: Maybe[SignalRef[Maybe[UI.Point]]]
    )(using Frame): List[UI] =
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
        panel
    end panelFor
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

    /** What a [[ContextMenu.targetless]] menu keeps between right-clicks, held by the caller
      * because the panel has no mount of its own. Allocate it with [[ContextMenu.state]].
      *
      * The fields are the component's, not the caller's: what a caller does with this value is
      * [[openAt]] on its own element and [[close]] when something else should shut the menu. That
      * boundary is the point. Opening is four writes in one order — collapse the submenu tree,
      * clear the highlight, write the point, THEN set open — and the ordering is not decorative:
      * the panel is placed from the point, so a panel that exists before there is a point to be
      * at renders at wherever the last one was. Handing out `openAt` instead of the refs is what
      * keeps that from being the caller's problem to get right.
      */
    final class State private[uic] (
        private[uic] val open: SignalRef[Boolean],
        private[uic] val at: SignalRef[Maybe[UI.Point]],
        private[uic] val focus: SignalRef[List[Int]],
        private[uic] val submenus: List[(List[Int], SignalRef[Boolean])],
        private[uic] val base: String
    ):
        /** The right-click handler for the caller's own element: `tr.onContextMenu(state.openAt)`.
          *
          * Bind it to the TYPED `onContextMenu` overload — the payload-free one registers the
          * event and drops the position, which still opens a menu, just not at the pointer.
          */
        def openAt(using Frame): MouseEvent => Any < Async =
            e =>
                for
                    _ <- MenuRender.openExactly(submenus, Absent)
                    _ <- focus.set(Nil)
                    _ <- at.set(e.position)
                    _ <- open.set(true)
                yield ()

        /** Closes the menu and forgets where it was, for the closes the panel cannot see: the
          * row it was opened on going away, a navigation, a caller's own Escape. Escape on the
          * panel, an outside click and a leaf activation already close it themselves.
          */
        def close(using Frame): Any < Async =
            for
                _ <- open.set(false)
                _ <- MenuRender.openExactly(submenus, Absent)
                _ <- focus.set(Nil)
            yield ()
    end State
end ContextMenu
