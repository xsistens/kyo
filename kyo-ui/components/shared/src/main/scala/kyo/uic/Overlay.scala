package kyo.uic

import kyo.*
import kyo.UI.*

/** Where an [[Overlay]] panel opens relative to its anchor: below or above the
  * anchor box, aligned to its start (left) or end (right) edge, plus the side
  * placements `RightStart`/`LeftStart` (panel beside the anchor, top-aligned;
  * Prime's nested-submenu geometry `left: 100%; top: 0`).
  */
enum OverlayAnchor derives CanEqual:
    case BottomStart, BottomEnd, TopStart, TopEnd, RightStart, LeftStart

    /** The `.p-uic-overlay-<token>` geometry class suffix. */
    private[uic] def token: String = this match
        case OverlayAnchor.BottomStart => "bottom-start"
        case OverlayAnchor.BottomEnd   => "bottom-end"
        case OverlayAnchor.TopStart    => "top-start"
        case OverlayAnchor.TopEnd      => "top-end"
        case OverlayAnchor.RightStart  => "right-start"
        case OverlayAnchor.LeftStart   => "left-start"
end OverlayAnchor

/** Overlay: the server-honest floating-panel building block (kyo primitive; the
  * machinery under Prime's Select/Popover/Menu panels).
  *
  * Renders, while the bound `open` ref is `true`:
  *   1. an invisible full-viewport backdrop (`div.p-uic-overlay-backdrop`,
  *      `position: fixed; inset: 0`, transparent, z-index one below the panel)
  *      whose `onClickSelf` writes `false` back (the proven Dialog pattern for
  *      outside-click dismissal), only with `dismissOnOutsideClick`, default on;
  *   2. the panel (`div.p-uic-overlay-panel` + the anchor geometry class),
  *      `position: absolute` against the nearest positioned ancestor, holding
  *      the given children.
  *
  * The ANCHOR ELEMENT (the box the panel attaches to) must be
  * `position: relative`. Hand the overlay your trigger via `trigger(ui)` and it
  * renders both inside an anchor container it owns
  * (`div.p-uic-overlay-anchor`), so the glue class cannot be forgotten; the
  * failure it prevents is silent and visual (an unstamped anchor positions the
  * panel against some far-away ancestor). Without a trigger the overlay renders
  * only backdrop + panel into whatever container the caller provides, which then
  * has to carry `p-uic-overlay-anchor` itself: the form the components in this
  * module use, because they already own their own root and stamp it there.
  *
  * The panel opens in the declared [[OverlayAnchor]] direction; `matchWidth`
  * (default on, Prime's field-panel behavior) gives it `min-width: 100%` of the
  * anchor.
  *
  * Focus story (`seedFocus`, default on): the panel carries kyo's declarative
  * focus attributes (`data-kyo-focus-auto` seeds focus onto the freshly
  * inserted panel, after the client's focus restore, so opening wins over
  * restore-to-trigger, and `data-kyo-focus-restore` returns focus to the
  * previously focused element when the panel leaves the document), plus
  * `tabindex="-1"` (the seed target must be focusable) and the renderer's
  * element-level `focusTrap`. With focus on the panel, its `onKeyDown` receives
  * Escape without any prior click (only with `dismissOnEscape`, default on).
  *
  * Nested overlays: each overlay renders its own backdrop, and a later-in-DOM
  * backdrop paints above an earlier one; an outside click therefore hits the
  * TOPMOST backdrop first and closes only that overlay; clicking again closes
  * the next. Escape dismisses PER LEVEL too: the panel's keydown handler
  * carries kyo's `stopPropagation(true)` (per-level event consumption: the
  * dispatch walk stops at the innermost panel that declared a keydown
  * handler), so one Escape closes only the innermost open overlay and leaves
  * the enclosing ones open; press again for the next level.
  *
  * Reposition (`autoFlip`, default on): the standard `render` path measures the
  * panel CONTINUOUSLY (once on open, then on every window scroll and resize,
  * via `observeViewportById`) and pushes the resolved geometry (the
  * vertical side flipped Bottom↔Top when the panel would overflow that viewport
  * edge, plus a horizontal shift to keep it inside) as ONE inline `Style` patched
  * IN PLACE via `bindStyleById`. The panel element itself is NEVER re-rendered,
  * so its focus seeding and open animation are not reset on a scroll tick. The
  * host-gated [[renderOpen]] path keeps the declared anchor UNMEASURED: its
  * single-subscription contract forbids nesting a measuring mount. Tall content
  * still uses `maxHeight` + the panel's own scrolling for the edge it does not
  * flip toward.
  *
  * Portal (`portal`, default off): with `portal(true)` the panel AND backdrop
  * re-home to `document.body` (the fork's element-level `portal(true)`; the
  * logical tree, dispatch, focus story and Escape chain stay unchanged), so
  * they escape ancestor `overflow` clipping, `transform`/`filter` containing
  * blocks and ancestor stacking contexts. Because a body-re-homed panel has no
  * CSS relationship to its anchor anymore, portal mode positions it with FIXED
  * viewport coordinates measured continuously through an invisible inset-0
  * anchor probe (flip + horizontal clamp included; `matchWidth` becomes a
  * measured `min-width`, since the `min-width: 100%` class would resolve against
  * the viewport at body). The panel stays `opacity: 0` until the first measure
  * lands, so it never flashes unpositioned; the enter fade is subsumed by that
  * gate, the leave ghost plays unchanged.
  *
  * Pointer anchor (`pointerAnchor`, default off): the panel can be placed at a POINT
  * instead of at a box: the viewport position a [[kyo.UI.MouseEvent]] carries, which is
  * how a context menu opens where the reader right-clicked. It portals (a viewport
  * coordinate needs a fixed-position panel), measures only the panel, and turns back over
  * the point per axis when that side cannot hold it. A change to the point MOVES the open
  * panel through the same in-place style patch, so the panel is not re-rendered and its
  * content, focus and open submenus survive being moved.
  *
  * Honest limitation WITHOUT portal (server-side geometry): the panel stays
  * inside the anchor's subtree, so do NOT place `transform` (or
  * `filter`/`will-change`) on overlay ancestors: they make the ancestor the
  * containing block for `position: fixed` and trap the backdrop (and any fixed
  * content) inside it. Or opt into `portal(true)`.
  *
  * Enter/leave animations: the panel fades in on open (a from-state
  * `enterTransition`) and plays Prime's own `p-anchored-overlay-leave-active`
  * keyframe on close via the body-level leave ghost. Opt out with `animate(false)`
  * (menu-family hosts that gate the panel's presence themselves do so).
  */
final class Overlay private[uic] (private[uic] val s: Overlay.State) extends Node, Stated:
    import s.*

    type Self = Overlay

    /** The element the panel anchors to. Rendering it here rather than beside the
      * overlay is what makes the anchor correct by construction: the overlay wraps
      * the trigger and its own panel in a `div.p-uic-overlay-anchor`, so the caller
      * never has to know that class exists. The trigger's own handlers are untouched:
      * this primitive does not toggle the ref for you. What it does know it says on the
      * trigger: that it opens a popup (`aria-haspopup`) and whether that popup is open
      * (`aria-expanded`). See [[PopupTrigger]] for what can be a trigger.
      */
    def trigger(t: UI.Ast.Interactive | PopupTrigger): Overlay = new Overlay(s.copy(triggerV = Present(t)))

    /** Which anchor corner the panel opens from (default [[OverlayAnchor.BottomStart]]). */
    def anchor(v: OverlayAnchor): Overlay = new Overlay(s.copy(anchorV = v))

    /** Whether the panel measures itself continuously (self-addressing:
      * on open, then on every window scroll/resize) and flips its vertical anchor
      * (Bottom↔Top) when it would overflow the viewport edge, plus shifts
      * horizontally when it would overflow left/right (default true). The resolved
      * geometry is patched onto the panel IN PLACE (`bindStyleById`), so the panel
      * never re-renders. Opt out to keep the declared anchor unconditionally, the
      * server-honest, un-measured behavior. Only the standard `render` path flips;
      * the host-gated [[renderOpen]] (menu-family submenus) keeps the declared
      * anchor to preserve its single-subscription contract.
      */
    def autoFlip(v: Boolean): Overlay = new Overlay(s.copy(autoFlipV = v))

    /** Whether the open panel and backdrop re-home to `document.body` (default false), the escape
      * from ancestor `overflow` clipping, `transform`/`filter` containing blocks and stacking
      * contexts. The panel is then positioned with measured FIXED viewport coordinates (continuous
      * anchor measure through an inset-0 probe; flip + clamp included; `matchWidth` as measured
      * `min-width`) and stays invisible until the first measure lands. Dispatch, Escape chain,
      * focus seeding/restore and the leave ghost work unchanged through the portal (see the class
      * doc). Only the standard `render` path portals; the host-gated [[renderOpen]] keeps its
      * single-subscription, in-tree contract.
      */
    def portal(v: Boolean): Overlay = new Overlay(s.copy(portalV = v))

    /** Places the panel at a POINT rather than at the anchor box: the pointer position a
      * [[kyo.UI.MouseEvent.position]] carries, in viewport coordinates.
      *
      * This is what a menu opening where the reader right-clicked needs, and it is the anchor
      * story turned inside out: there is no box to attach to, so the panel portals to
      * `document.body` (the point is a viewport coordinate, and only a fixed-position panel can
      * honour one) and is placed by the measured geometry alone. It opens down and to the right
      * of the point, turns back over the point on the side that cannot hold it, and clamps into
      * the viewport when neither side can. See [[Overlay.pointerGeometryFor]].
      *
      * The signal is read, never written: the host decides where the point comes from and when
      * it changes, and a change MOVES the open panel without re-rendering it (the geometry is
      * patched in place, as every other measured placement here is). While the signal holds
      * `Absent` there is nowhere for the panel to be, so it stays hidden rather than landing in
      * a corner: set the point before opening.
      *
      * `matchWidth` is meaningless here (a point has no width) and is ignored.
      */
    def pointerAnchor(sig: Signal[Maybe[UI.Point]]): Overlay = new Overlay(s.copy(pointerAnchorV = Present(sig)))

    /** Package-internal: the DOM id to stamp on the panel, for the host-gated [[renderOpen]],
      * [[renderShown]] and [[renderGated]] paths (the last two take it as a parameter), which have
      * no mount of their own to mint one in and cannot grow one: a mount that
      * PUBLISHED the panel would have to be keyed to survive the host's re-renders, and a keyed
      * mount freezes the content it published. Hosts derive it from their own element id
      * (`s"$id-panel"`), so it is stable across those re-renders and the measured placement
      * outlives them.
      */
    private[uic] def panelId(id: String): Overlay = new Overlay(s.copy(panelIdV = Present(id)))

    /** Package-internal: what a right-click landing on the outside-click backdrop means.
      *
      * The backdrop covers the viewport, so while the panel is open it is what a right-click
      * anywhere else hits. Without a handler the browser's own menu opens on top of the panel
      * (kyo suppresses the native menu only where a handler was declared), which is why a
      * pointer-anchored host wires this.
      */
    private[uic] def onBackdropContextMenu(f: MouseEvent => UI.Action): Overlay =
        new Overlay(s.copy(backdropCtxV = Present(f)))

    /** Package-internal: ends every click from inside the panel at the panel itself.
      *
      * The pickers render their panel INSIDE their field, and the field is what listens for the
      * click that opens and closes it. Dispatch walks the innermost target first and then every
      * ancestor, so without this an option click would run its pick and then reach the field's
      * toggle, and a click on a group header, a filter input or empty panel space would toggle the
      * panel shut. With it the panel declares a no-op click handler and kyo's `stopPropagation`, so
      * whatever an element inside does with the click it does first, and the click goes no further.
      *
      * Opt-in rather than always on because the panel is shared: a menu, a popover or a date
      * picker hosted in a clickable container keeps its clicks bubbling as before. `stopPropagation`
      * is per element, so it also ends a keydown on the panel where the panel declares one (a
      * `dismissOnEscape` panel already stops there, for its own reasons); the pickers declare none.
      * The backdrop stops there too: an outside click closes the overlay at the backdrop and goes no
      * further, so the host's toggle does not answer it a second time.
      */
    private[uic] def containClicks(v: Boolean): Overlay = new Overlay(s.copy(containClicksV = v))

    /** How the open overlay responds to scroll input outside its panel (default
      * [[Overlay.Scroll.Close]], the Prime family's hide-on-scroll). See [[Overlay.Scroll]].
      */
    def scroll(v: Overlay.Scroll): Overlay = new Overlay(s.copy(scrollV = v))

    /** `min-width: 100%` of the anchor on the panel (default true, Prime's
      * field-panel behavior).
      */
    def matchWidth(v: Boolean): Overlay = new Overlay(s.copy(matchWidthV = v))

    /** Caps the panel height (CSS length) and makes it scroll (`overflow-y: auto`):
      * the viewport-edge answer, same as Prime's scrollable panels.
      */
    def maxHeight(v: String): Overlay = new Overlay(s.copy(maxHeightV = Present(v)))

    /** Caps the panel height in pixels; see [[maxHeight(v:String)*]]. */
    def maxHeight(px: Int): Overlay = new Overlay(s.copy(maxHeightV = Present(s"${px}px")))

    /** Whether a click outside the panel closes the overlay (default true). */
    def dismissOnOutsideClick(v: Boolean): Overlay = new Overlay(s.copy(dismissOnOutsideClickV = v))

    /** Whether Escape (with focus in the panel) closes the overlay (default true). */
    def dismissOnEscape(v: Boolean): Overlay = new Overlay(s.copy(dismissOnEscapeV = v))

    /** Whether the panel seeds focus on open and restores it on close via the
      * `data-kyo-focus-*` client contract (default true).
      */
    def seedFocus(v: Boolean): Overlay = new Overlay(s.copy(seedFocusV = v))

    /** Whether the panel animates on open/close (default true): a fade-in on open
      * (the fork's `enterTransition` from-state) and Prime's own
      * `p-anchored-overlay-leave-active` keyframe on close (via the leave ghost).
      * Opt out for menu-family panels that manage their own presence.
      */
    def animate(v: Boolean): Overlay = new Overlay(s.copy(animateV = v))

    /** Package-internal class hook: hosts (Select) stamp Prime's panel skin class
      * (`p-select-overlay`) onto the panel element.
      */
    private[uic] def panelClass(cls: String): Overlay = new Overlay(s.copy(panelClassesV = panelClassesV :+ cls))

    /** Package-internal: extra keydown handling on the panel (hosts wire their
      * keyboard navigation here). Runs for every key; the overlay's own Escape
      * dismissal runs first.
      */
    private[uic] def onPanelKeyDown(f: KeyboardEvent => UI.Action): Overlay =
        new Overlay(s.copy(keyHandlerV = Present(f)))

    /** Adds panel children. */
    def apply(cs: UI*): Overlay = new Overlay(s.copy(kids = kids ++ cs))

    /** Fired with the visibility the reader asks for (false for an outside click, an outside scroll and Escape), after
      * a two-way [[Overlay.apply open]] binding is written.
      */
    def onOpenChange(f: Boolean => UI.Action)(using Frame): Overlay = new Overlay(s.copy(openSlot = openSlot.hear(f)))

    /** Package-internal: the component a binding mistake is reported for, when a host hands its own binding on. */
    private[uic] def reportedAs(component: String): Overlay = new Overlay(s.copy(reportedAsV = component))

    private[uic] def render(using Frame): UI =
        // Shown while the binding says so: a constant needs no region, a signal is one boundary.
        val gated: UI = openSlot.binding match
            case Absent        => openContent
            case Present(slot) =>
                slot.const match
                    case Present(v) => if v then openContent else UI.empty
                    case Absent     => when(slot.signal)(openContent)
        val drawn = triggerV match
            case Present(t) =>
                val announced = PopupTrigger.announced(t, "dialog", PopupTrigger.expanded(openSlot.binding))
                div.cssClass("p-uic-overlay-anchor")(announced, gated)
            case _ => gated
        // A signal nobody hears: every dismissal the reader asks for is painted away again.
        Diagnostics.before(drawn)(reportedAsV)(Diagnostic(openSlot.deaf, Overlay.openAdvice))
    end render

    /** A constant nobody hears is fixed: an open panel stays, and nothing outside dismisses it. */
    private def fixed: Boolean = openSlot.fixed

    /** The open panel content: the continuous flip/shift reposition when `autoFlip`, else the declared-anchor render.
      *
      * With a [[panelId]] from the host, the panel is drawn directly under that id and only the measuring triggers are
      * mounts, keyed by it. That is the form a host-gated panel needs: its host redraws the panel on every keystroke that
      * moves a highlight, and a mount around the panel would start again each time, showing an inert snapshot that no
      * click or key reaches while it does.
      *
      * Without one, which is the self-gated `render` path inside the `when(open)` region, each open mints a fresh id
      * in a mount of its own and a fresh reposition lifecycle; close tears it down (and with it the scroll/resize
      * observer, via the mount's Scope).
      */
    private def openContent(using Frame): UI =
        panelIdV match
            case Present(id) => placedOpenContent(id)
            case Absent      => mintedOpenContent

    /** The panel under the host's stable `id`, measured by keyed triggers that publish nothing. */
    private def placedOpenContent(id: String)(using Frame): UI =
        pointerAnchorV match
            case Present(at) =>
                renderOpenAt(anchorV, Present(id), portal = true, whileOpen = Present(pointerReposition(at, id)))
            case Absent =>
                if portalV then
                    val anchorId = s"$id-anchor"
                    renderOpenAt(
                        anchorV,
                        Present(id),
                        portal = true,
                        after = List(anchorProbe(anchorId)),
                        whileOpen = Present(portalReposition(anchorId, id))
                    )
                else if !autoFlipV then renderOpenAt(anchorV, Absent)
                else renderOpenAt(anchorV, Present(id), whileOpen = Present(reposition(id)))

    private def mintedOpenContent(using Frame): UI =
        pointerAnchorV match
            case Present(at) =>
                // A point needs no anchor probe: the placement is the point plus the panel's own size,
                // so only the panel is measured. Same self-addressing shape as the paths below (draw an id,
                // stamp, observe, patch in place): the panel is position:fixed and invisible from the first
                // frame instead of sitting in flow.
                UI.mounted {
                    for
                        id <- UI.id
                    yield renderOpenAt(anchorV, Present(id), portal = true, whileOpen = Present(pointerReposition(at, id)))
                }.keyed(siteV)
            case Absent => anchoredOpenContent

    /** The anchor-box placements: the portal path, the declared-anchor path, and the measured flip. */
    private def anchoredOpenContent(using Frame): UI =
        if portalV then
            // Portal: the panel (and backdrop) re-home to document.body via the fork's element-level
            // portal(true), so the CSS anchor geometry (absolute against the anchor) cannot position
            // them. Instead the anchor box is measured continuously through an inset-0 probe and the
            // panel placed with FIXED viewport coordinates patched in place, the portal analog of the
            // autoFlip reposition below (flip + clamp included). Same self-addressing shape: each open
            // draws its ids, close tears the observers down through the mount's Scope.
            UI.mounted {
                for
                    panelId  <- UI.id
                    anchorId <- UI.id
                yield renderOpenAt(
                    anchorV,
                    Present(panelId),
                    portal = true,
                    after = List(anchorProbe(anchorId)),
                    whileOpen = Present(portalReposition(anchorId, panelId))
                )
            }.keyed(siteV)
        else if !autoFlipV then renderOpenAt(anchorV, Absent)
        else
            // Self-addressing: on open, mint an id + stamp it on the panel,
            // then observe THAT panel by id, since a reusable component has no way to learn
            // its own render path. The panel renders ONCE at the DECLARED anchor (its
            // class provides the first paint); the measured geometry is patched onto it
            // IN PLACE through `geom` + `bindStyleById`, so the panel element is never
            // replaced and its focus seed + open animation survive every scroll tick.
            UI.mounted {
                UI.id.map(id => renderOpenAt(anchorV, Present(id), whileOpen = Present(reposition(id))))
            }.keyed(siteV)

    /* The reposition, which runs while the panel is shown (`renderOpenAt` puts it on the panel, keyed by the panel's id), so
     * `observeViewportById` resolves the freshly inserted panel
     * (getElementById would miss it if requested before it is on the page). It observes
     * the viewport (a `Signal[Maybe[Rect]]` the observer updates on every
     * scroll/resize, `Absent` until the first measure), maps it to the panel
     * geometry Style (`Absent` -> `Style.empty`, so the anchor CLASS shows until
     * measured), and binds THAT onto the panel in place. No intermediate SignalRef
     * + callback bridge: the measured stream IS the source. Both primitives detach
     * when the effect's Scope closes on overlay close.
     */

    /** An invisible inset-0 child of the anchor element: the overlay cannot address its host anchor
      * (it only renders INTO it), but an absolutely positioned probe filling it shares its box
      * (padding-box; the anchor's border widths are the documented deviation), so observing the
      * probe by id measures the anchor continuously.
      */
    private def anchorProbe(id: String)(using Frame): UI =
        div.id(id).cssClass("p-uic-overlay-anchor-probe")

    /** The portal twin of [[reposition]]: observes BOTH the anchor probe (where to attach)
      * and the panel (the flip decision needs its size), combines the two measured streams and
      * binds the resolved fixed-position geometry onto the panel in place. `Style.empty` until both
      * have measured: the panel stays hidden (`opacity: 0` from `.p-uic-overlay-portal`) until the
      * first geometry patch turns it visible, so it never flashes unpositioned at the body.
      */
    private def portalReposition(anchorId: String, panelId: String)(using Frame): Any < (Abort[Any] & Async & Scope) =
        for
            cmds      <- UI.commands
            anchorSig <- cmds.observeViewportById(anchorId)
            panelSig  <- cmds.observeViewportById(panelId)
            _         <- cmds.bindStyleById(
                panelId,
                anchorSig.combineLatest(panelSig).map {
                    case (Present(a), Present(p)) => Overlay.portalGeometryFor(anchorV, matchWidthV, a, p)
                    case _                        => Style.empty
                }
            )
        yield ()

    /** The pointer-anchored twin of [[reposition]]: observes the panel (the placement needs
      * its size), combines that with the point, and binds the resolved fixed-position geometry onto
      * the panel in place. `Style.empty` until both have landed, which keeps the panel hidden
      * (`.p-uic-overlay-portal` gates the unmeasured paint) rather than showing it unplaced.
      */
    private def pointerReposition(at: Signal[Maybe[UI.Point]], panelId: String)(using Frame): Any < (Abort[Any] & Async & Scope) =
        for
            cmds     <- UI.commands
            panelSig <- cmds.observeViewportById(panelId)
            _        <- cmds.bindStyleById(
                panelId,
                panelSig.combineLatest(at).map {
                    case (Present(panel), Present(at)) => Overlay.pointerGeometryFor(at, panel)
                    case _                             => Style.empty
                }
            )
        yield ()

    private def reposition(panelId: String)(using Frame): Any < (Abort[Any] & Async & Scope) =
        UI.commands.map { cmds =>
            cmds.observeViewportById(panelId).map { rectSig =>
                cmds.bindStyleById(
                    panelId,
                    rectSig.map {
                        case Present(rect) => Overlay.geometryFor(anchorV, rect)
                        case Absent        => Style.empty
                    }
                )
            }
        }

    /** Package-internal: renders backdrop + panel UNCONDITIONALLY, for hosts
      * (Menubar/TieredMenu/MegaMenu) that already subscribe to the open state in an
      * enclosing render and gate the overlay's presence themselves. Subscribing
      * twice to the same ref (the host's outer subscription plus this overlay's
      * own `when`) races the inner update against the outer replace and can
      * duplicate the panel subtree; the host-gated form has exactly one
      * subscription. Escape/outside-click still write `false` into the ref.
      */
    /** Package-internal: the open panel with the full placement of [[render]] (the measured flip, the portal, a pointer
      * anchor), for a host that reads the open state in its own region and renders this only while it is open. That host is
      * then ONE region over its open state and everything else it draws; [[render]]'s own `when(open)` inside it would
      * be a second region, drawing a panel that closed over the host's older state. Escape and an outside click still
      * write `false` into the ref, and the host's region takes the panel away.
      *
      * `panelId` is the panel's id, which the host derives from its own (`s"$base-panel"`) so that it is the same on every
      * redraw: the host redraws the panel with every change it reads, and only a stable id keeps the panel the same element
      * (see [[openContent]]). `Absent` is for a static projection with no session behind it.
      */
    private[uic] def renderShown(panelId: Maybe[String])(using Frame): UI = renderGated(true, panelId)

    /** [[renderShown]] while `open`, and only the trigger (where there is one) otherwise: for a host whose one region
      * reads the open state and always draws the trigger.
      */
    private[uic] def renderGated(open: Boolean, panelId: Maybe[String])(using Frame): UI =
        val content: List[UI] = if open then List(new Overlay(s.copy(panelIdV = panelId.orElse(panelIdV))).openContent) else Nil
        triggerV match
            case Present(t) =>
                val announced = PopupTrigger.announced(t, "dialog", open.toString)
                div.cssClass("p-uic-overlay-anchor")(UI.children(announced :: content)*)
            case _ => content.headOption.getOrElse(UI.empty)
        end match
    end renderGated

    private[uic] def renderOpen(using Frame): UI =
        (pointerAnchorV, panelIdV) match
            // The pointer placement survives here because nothing it needs is subscribed to the
            // host's ref: the id comes from the host, and the measuring effect paints nothing.
            case (Present(at), Present(id)) =>
                renderOpenAt(anchorV, Present(id), portal = true, whileOpen = Present(pointerReposition(at, id)))
            case _ => renderOpenAt(anchorV, Absent)

    /** The backdrop + panel at the DECLARED anchor, optionally id-stamped: the
      * shared builder behind both the host-gated [[renderOpen]] (no id) and the
      * autoFlip path (id-stamped, so `observeViewportById` can address it). The
      * measured flip/shift is applied separately, IN PLACE, via `bindStyleById`;
      * this builder always emits the declared-anchor geometry class, so the panel
      * is rendered exactly once.
      *
      * `whileOpen` runs while the panel is shown, keyed by the panel's id: the host re-renders the panel subtree on every keystroke that
      * moves its highlight, and an effect that started again each time would be a fresh observer, a fresh round trip, and `Style.empty`
      * in between, which blinks the panel out on every arrow key. The id comes from the host's element id, so it is stable across those
      * re-renders and distinct per open. `after` holds what only the measured content adds (its probe), placed AFTER
      * the panel in the same fragment. The client addresses a click by the path the element had when
      * the reader clicked, and the session resolves it against the tree it holds when the click
      * arrives, so a click on the placeholder's panel that arrives after the measured content
      * replaced it must find the same element at the same path. Anything placed before the panel
      * shifts every path inside it, and the click then lands in whatever took its place.
      */
    private def renderOpenAt(
        anchor: OverlayAnchor,
        panelId: Maybe[String],
        portal: Boolean = false,
        after: List[UI] = Nil,
        whileOpen: Maybe[Any < (Abort[Any] & Async & Scope)] = Absent
    )(using Frame): UI =
        // Typed branch by branch: a `fold` over `()` and the ref write infers `Any`, and the write would land inert.
        val close: UI.Action = closeV match
            case Present(action) => action
            case Absent          => openSlot.change(false)

        val outside            = dismissOnOutsideClickV && !fixed
        val escape             = dismissOnEscapeV && !fixed
        val backdrop: List[UI] =
            if outside then
                var b = div.cssClass("p-uic-overlay-backdrop").onClickSelf(close)
                // The full-viewport backdrop intercepts every outside wheel (a fixed element chains scroll
                // to the DOCUMENT scroller, so inner scroll containers never move), and each Scroll mode turns
                // that interception into a deliberate behavior instead of a dead page.
                scrollV match
                    case Overlay.Scroll.Close => b = b.onScroll(close)
                    case Overlay.Scroll.Lock  => ()
                // Explicitly a lambda over the event: `onContextMenu` also has a payload-free
                // overload, and binding to that one would register the event and drop the handler.
                backdropCtxV.foreach(f => b = b.onContextMenu((e: MouseEvent) => f(e)))
                // See `containClicks`: the close is the whole answer to an outside click, so the host's toggle
                // must not hear it a second time.
                if containClicksV then b = b.stopPropagation(true)
                if portal then b = b.portal(true)
                List(b)
            else Nil

        var panel = div
            .cssClass("p-uic-overlay-panel")
            .cssClass(s"p-uic-overlay-${anchor.token}")
        panelId.foreach { id =>
            panel = panel.id(id)
            whileOpen.foreach(effect => panel = panel.whileMountedKeyed(id)(effect))
        }
        // Portal mode re-homes the panel to document.body (fork portal) and switches to measured
        // fixed positioning: p-uic-overlay-portal pins position:fixed + the opacity gate, and the
        // match-width CLASS must not ride along: its min-width:100% would resolve against the
        // viewport at body (the leave-ghost lesson); the measured min-width replaces it.
        if portal then panel = panel.cssClass("p-uic-overlay-portal").portal(true)
        if matchWidthV && !portal then panel = panel.cssClass("p-uic-overlay-match-width")
        // Open fades the panel in (from-state transition); close plays Prime's own
        // anchored-overlay leave keyframe on the body-level leave ghost.
        if animateV then
            panel = panel.enterTransition("p-uic-enter-fade").leaveTransition("p-anchored-overlay-leave-active")
        panelClassesV.foreach(c => panel = panel.cssClass(c))
        maxHeightV.foreach { h =>
            panel = panel.style(Style.maxHeight(Length.parse(h)).overflowY(_.auto))
        }
        if seedFocusV then
            panel = panel.focusAuto(true).focusRestore(true).tabIndex(-1).focusTrap(true)
        // Per-level event consumption (kyo `data-kyo-stop`): keydown dispatched
        // from inside this panel stops HERE, so an Escape closes only this (the
        // innermost) overlay, ancestor panels keep their state. Only stamped when
        // this panel actually dismisses on Escape, so an Escape-inert panel stays
        // transparent to its enclosing overlay chain.
        if escape then panel = panel.stopPropagation(true)
        // See `containClicks`: the no-op handler is what makes the stop apply to a click.
        if containClicksV then panel = panel.onClick(()).stopPropagation(true)
        // A panel that navigates with arrow keys (Select/MultiSelect/Menu/... via onPanelKeyDown) must not let the
        // browser ALSO scroll the page underneath; the keydown is still delivered so the highlight moves.
        if keyHandlerV.isDefined then panel = panel.preventScrollKeys
        if escape || keyHandlerV.isDefined then
            panel = panel.onKeyDown { e =>
                e.key match
                    case Keyboard.Escape if escape => close
                    case _                         =>
                        keyHandlerV match
                            case Present(f) => f(e)
                            case Absent     => ()
            }
        end if
        val panelUI: UI = panel(UI.children(kids)*)

        fragment((backdrop ++ (panelUI :: after))*)
    end renderOpenAt
end Overlay

object Overlay:
    final private[uic] case class State(
        // Unbound for a panel its host gates and closes itself (see [[Overlay.gated]]).
        openSlot: Slot[Boolean],
        anchorV: OverlayAnchor = OverlayAnchor.BottomStart,
        matchWidthV: Boolean = true,
        maxHeightV: Maybe[String] = Absent,
        dismissOnOutsideClickV: Boolean = true,
        dismissOnEscapeV: Boolean = true,
        seedFocusV: Boolean = true,
        animateV: Boolean = true,
        autoFlipV: Boolean = true,
        portalV: Boolean = false,
        scrollV: Overlay.Scroll = Overlay.Scroll.Close,
        panelClassesV: List[String] = Nil,
        keyHandlerV: Maybe[KeyboardEvent => UI.Action] = Absent,
        pointerAnchorV: Maybe[Signal[Maybe[UI.Point]]] = Absent,
        panelIdV: Maybe[String] = Absent,
        backdropCtxV: Maybe[MouseEvent => UI.Action] = Absent,
        containClicksV: Boolean = false,
        triggerV: Maybe[UI.Ast.Interactive | PopupTrigger] = Absent,
        kids: List[UI] = Nil,
        closeV: Maybe[UI.Action] = Absent,
        reportedAsV: String = "Overlay",
        // Where the overlay was made, which keys the mount that mints its panel's ids: a region around it that runs again keeps the
        // panel and its effect rather than starting them over.
        siteV: UI.MountSite = kyo.internal.MountSites.MountSite.none
    )

    /** An overlay over the visibility `open`. A `SignalRef` binds two way: writes open and close the panel, and Escape,
      * an outside click or an outside scroll write false, then [[Overlay.onOpenChange]] hears it. A plain `Signal` binds
      * one way: the panel follows it and only reports a dismissal. A constant nobody hears is fixed: an open panel
      * stays, and nothing outside dismisses it. A signal nothing hears is reported in a card.
      */
    def apply(open: Boolean | Signal[Boolean])(using site: UI.MountSite): Overlay = new Overlay(State(Slot().bind(open), siteV = site))

    /** Package-internal: a panel its host draws only while open and closes with `close`, which Escape and an outside click
      * run. For a submenu whose open state is one path in a set the host keeps, rather than a ref of its own.
      */
    private[uic] def gated(close: UI.Action)(using site: UI.MountSite): Overlay =
        new Overlay(State(Slot(), closeV = Present(close), siteV = site))

    /** Package-internal: the panel of a host over the host's own open slot, reporting a dismissal to whoever hears it.
      * Quiet, since the host reports its binding itself.
      */
    private[uic] def hosted(open: Slot[Boolean])(using site: UI.MountSite): Overlay = new Overlay(State(open.quieted, siteV = site))

    /** Package-internal: the slot of a host that embeds this overlay, as the host wants it heard. */
    private[uic] def withOpen(slot: Slot[Boolean])(using site: UI.MountSite): Overlay = new Overlay(State(slot, siteV = site))

    private val openAdvice = Slot.Name("Overlay", "the visibility", "a dismissal", "onOpenChange").advice

    /** How an OPEN overlay responds to scroll input outside its panel. The invisible outside-click
      * backdrop covers the whole viewport, so page scrolling necessarily runs through it; the mode
      * decides what that means. Scrolling INSIDE the panel (a `maxHeight` listbox) is unaffected.
      *
      *   - [[Scroll.Close]] (default): the first outside scroll closes the overlay and the page
      *     scrolls on: the Prime family's hide-on-scroll (`ConnectedOverlayScrollHandler`) and
      *     native Chrome `<select>` behavior.
      *   - [[Scroll.Lock]]: outside scroll is swallowed while open; the page freezes, the panel
      *     stays put; the Radix/MUI modal scroll-lock feel.
      *
      * A third mode, scroll-through with the panel live-following its anchor (the Floating-UI
      * feel), needs a fork-level outside-click primitive in place of the hit-blocking backdrop
      * and is planned as `Scroll.Follow`.
      */
    enum Scroll derives CanEqual:
        case Close
        case Lock

    /** Viewport gutter (px) kept between a shifted/flipped panel and the edge. */
    private[uic] val Gutter = 4.0

    /** The anchor to render at given the panel's measured `rect`: flips the
      * vertical side (Bottom↔Top) when the panel overflows the corresponding
      * viewport edge, otherwise keeps the declared anchor. Horizontal alignment
      * (Start/End) is preserved; horizontal overflow is handled by [[shiftFor]].
      */
    private[uic] def flipAnchor(declared: OverlayAnchor, rect: UI.Rect): OverlayAnchor =
        val overflowsBottom = rect.y + rect.height > rect.viewportHeight
        val overflowsTop    = rect.y < 0
        declared match
            case OverlayAnchor.BottomStart if overflowsBottom => OverlayAnchor.TopStart
            case OverlayAnchor.BottomEnd if overflowsBottom   => OverlayAnchor.TopEnd
            case OverlayAnchor.TopStart if overflowsTop       => OverlayAnchor.BottomStart
            case OverlayAnchor.TopEnd if overflowsTop         => OverlayAnchor.BottomEnd
            case other                                        => other
        end match
    end flipAnchor

    /** The signed left-margin delta (px) to keep the panel inside the viewport:
      * negative pulls a right-overflowing panel left, positive pushes a
      * left-overflowing panel right, 0 when it already fits.
      */
    private[uic] def shiftFor(rect: UI.Rect): Double =
        val overRight = rect.x + rect.width - (rect.viewportWidth - Gutter)
        val overLeft  = Gutter - rect.x
        if overRight > 0 then -overRight
        else if overLeft > 0 then overLeft
        else 0.0
    end shiftFor

    /** The resolved inline geometry patched onto the panel after a measure: the
      * flipped vertical placement plus the horizontal shift, as ONE [[kyo.Style]]
      * pushed through `bindStyleById`. It ALWAYS sets BOTH vertical edges (one of
      * them `auto`): `bindStyleById` MERGES declarations and never clears, so a
      * flip back to the declared side must actively reset the edge it is leaving,
      * or the stale `top`/`bottom` would linger. Edges match the anchor CSS: a
      * "below" side ([[OverlayAnchor.BottomStart]]/`BottomEnd`) is `top:100%;
      * bottom:auto`, an "above" side (`TopStart`/`TopEnd`) is `top:auto;
      * bottom:100%`. The horizontal shift stays a `margin-left` delta (never
      * `transform`, so it cannot smear the leave keyframe), exactly as [[shiftFor]]
      * fed `renderOpenAt`. Side anchors (`RightStart`/`LeftStart`) are never
      * flipped by [[flipAnchor]] and place off `left`/`right` + `top:0`, so their
      * geometry is the shift ALONE; emitting top/bottom would fight their class
      * placement.
      */

    /** The fixed-position geometry for a PORTALED panel: viewport coordinates computed from the
      * measured anchor box and panel size, since a body-re-homed panel has no CSS relationship to its
      * anchor anymore. Flips the vertical side when the declared one cannot fit the panel (and the
      * other side can), right-aligns End anchors, clamps horizontally into the viewport (Gutter),
      * sizes `min-width` from the anchor for matchWidth, and turns the panel visible (`opacity`
      * gates the unmeasured first paint, see `.p-uic-overlay-portal`). Bottom-anchored panels pin
      * `top` (grow downward), top-anchored pin `bottom` (grow upward), so a content-size change
      * stays attached without waiting for the next measure. Always sets BOTH vertical and BOTH
      * horizontal edges (one of each `auto`): `bindStyleById` MERGES and never clears, so a flip
      * back must actively reset the edge it is leaving.
      */
    private[uic] def portalGeometryFor(declared: OverlayAnchor, matchWidth: Boolean, anchor: UI.Rect, panel: UI.Rect): Style =
        val vw        = anchor.viewportWidth
        val vh        = anchor.viewportHeight
        val fitsBelow = anchor.y + anchor.height + panel.height <= vh - Gutter
        val fitsAbove = anchor.y - panel.height >= Gutter
        val resolved  = declared match
            case OverlayAnchor.BottomStart if !fitsBelow && fitsAbove => OverlayAnchor.TopStart
            case OverlayAnchor.BottomEnd if !fitsBelow && fitsAbove   => OverlayAnchor.TopEnd
            case OverlayAnchor.TopStart if !fitsAbove && fitsBelow    => OverlayAnchor.BottomStart
            case OverlayAnchor.TopEnd if !fitsAbove && fitsBelow      => OverlayAnchor.BottomEnd
            case other                                                => other
        def clampedLeft(l: Double): Double =
            math.max(Gutter, math.min(l, vw - panel.width - Gutter))
        val vertical = resolved match
            case OverlayAnchor.BottomStart | OverlayAnchor.BottomEnd =>
                Style.top((anchor.y + anchor.height).px).bottom(Length.Auto)
            case OverlayAnchor.TopStart | OverlayAnchor.TopEnd =>
                Style.top(Length.Auto).bottom((vh - anchor.y).px)
            case OverlayAnchor.RightStart | OverlayAnchor.LeftStart =>
                Style.top(anchor.y.px).bottom(Length.Auto)
        val left = resolved match
            case OverlayAnchor.BottomEnd | OverlayAnchor.TopEnd => clampedLeft(anchor.x + anchor.width - panel.width)
            case OverlayAnchor.RightStart                       => clampedLeft(anchor.x + anchor.width)
            case OverlayAnchor.LeftStart                        => clampedLeft(anchor.x - panel.width)
            case _                                              => clampedLeft(anchor.x)
        val placed = vertical.left(left.px).right(Length.Auto)
        val sized  = if matchWidth then placed.minWidth(anchor.width.px) else placed
        sized.opacity(1.0)
    end portalGeometryFor

    /** The fixed-position geometry for a panel placed at a POINT ([[Overlay.pointerAnchor]]): where
      * the reader's pointer was, plus the measured panel size, as viewport coordinates.
      *
      * Down and to the right of the point is the resting placement, which is where a context menu
      * goes when there is room. Each axis turns back over the point on its own when the panel does
      * not fit that way AND fits the other, so a right-click near the bottom of the window opens
      * upward from the pointer rather than off the screen, and one in the corner turns on both
      * axes. When neither side fits (a panel taller than the viewport) the placement is clamped
      * into the viewport with the [[Gutter]], so the reader always sees the panel and its own
      * scrolling covers the rest.
      *
      * The pointer is a POINT, not a box, so unlike [[portalGeometryFor]] there is no anchor size
      * to align to and no `min-width` to match. Both edges of each axis are set (one `auto`) for
      * the same reason as there: `bindStyleById` merges and never clears, so a move that flips back
      * has to reset the edge it is leaving. `opacity` lifts the portal class's unmeasured-paint gate.
      */
    private[uic] def pointerGeometryFor(at: UI.Point, panel: UI.Rect): Style =
        val vw                                                         = panel.viewportWidth
        val vh                                                         = panel.viewportHeight
        val fitsBelow                                                  = at.y + panel.height <= vh - Gutter
        val fitsAbove                                                  = at.y - panel.height >= Gutter
        val fitsRight                                                  = at.x + panel.width <= vw - Gutter
        val fitsLeft                                                   = at.x - panel.width >= Gutter
        val top                                                        = if !fitsBelow && fitsAbove then at.y - panel.height else at.y
        val left                                                       = if !fitsRight && fitsLeft then at.x - panel.width else at.x
        def clamp(v: Double, extent: Double, viewport: Double): Double =
            math.max(Gutter, math.min(v, viewport - extent - Gutter))
        Style
            .top(clamp(top, panel.height, vh).px)
            .bottom(Length.Auto)
            .left(clamp(left, panel.width, vw).px)
            .right(Length.Auto)
            .opacity(1.0)
    end pointerGeometryFor

    private[uic] def geometryFor(declared: OverlayAnchor, rect: UI.Rect): Style =
        val flipped = flipAnchor(declared, rect)
        val shift   = Style.margin(0.px, 0.px, 0.px, shiftFor(rect).px)
        flipped match
            case OverlayAnchor.BottomStart | OverlayAnchor.BottomEnd => shift.top(100.pct).bottom(Length.Auto)
            case OverlayAnchor.TopStart | OverlayAnchor.TopEnd       => shift.top(Length.Auto).bottom(100.pct)
            case OverlayAnchor.RightStart | OverlayAnchor.LeftStart  => shift
        end match
    end geometryFor
end Overlay
