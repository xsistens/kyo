package kyo.uic

import kyo.*
import kyo.UI.*

/** Where an [[Overlay]] panel opens relative to its anchor: below or above the
  * anchor box, aligned to its start (left) or end (right) edge — plus the side
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

/** Overlay — the server-honest floating-panel building block (kyo primitive; the
  * machinery under Prime's Select/Popover/Menu panels).
  *
  * Renders, while the bound `open` ref is `true`:
  *   1. an invisible full-viewport backdrop (`div.p-uic-overlay-backdrop`,
  *      `position: fixed; inset: 0`, transparent, z-index one below the panel)
  *      whose `onClickSelf` writes `false` back — the proven Dialog pattern for
  *      outside-click dismissal (only with `dismissOnOutsideClick`, default on);
  *   2. the panel (`div.p-uic-overlay-panel` + the anchor geometry class),
  *      `position: absolute` against the nearest positioned ancestor, holding
  *      the given children.
  *
  * The ANCHOR ELEMENT — the box the panel attaches to — must be
  * `position: relative`. Hand the overlay your trigger via `trigger(ui)` and it
  * renders both inside an anchor container it owns
  * (`div.p-uic-overlay-anchor`), so the glue class cannot be forgotten; the
  * failure it prevents is silent and visual (an unstamped anchor positions the
  * panel against some far-away ancestor). Without a trigger the overlay renders
  * only backdrop + panel into whatever container the caller provides, which then
  * has to carry `p-uic-overlay-anchor` itself — the form the components in this
  * module use, because they already own their own root and stamp it there.
  *
  * The panel opens in the declared [[OverlayAnchor]] direction; `matchWidth`
  * (default on — Prime's field-panel behavior) gives it `min-width: 100%` of the
  * anchor.
  *
  * Focus story (`seedFocus`, default on): the panel carries kyo's declarative
  * focus attributes — `data-kyo-focus-auto` seeds focus onto the freshly
  * inserted panel (after the client's focus restore, so opening wins over
  * restore-to-trigger) and `data-kyo-focus-restore` returns focus to the
  * previously focused element when the panel leaves the document — plus
  * `tabindex="-1"` (the seed target must be focusable) and the renderer's
  * element-level `focusTrap`. With focus on the panel, its `onKeyDown` receives
  * Escape without any prior click (only with `dismissOnEscape`, default on).
  *
  * Nested overlays: each overlay renders its own backdrop, and a later-in-DOM
  * backdrop paints above an earlier one — an outside click therefore hits the
  * TOPMOST backdrop first and closes only that overlay; clicking again closes
  * the next. Escape dismisses PER LEVEL too: the panel's keydown handler
  * carries kyo's `stopPropagation(true)` (per-level event consumption — the
  * dispatch walk stops at the innermost panel that declared a keydown
  * handler), so one Escape closes only the innermost open overlay and leaves
  * the enclosing ones open; press again for the next level.
  *
  * Reposition (`autoFlip`, default on): the standard `render` path measures the
  * panel CONTINUOUSLY — once on open, then on every window scroll and resize
  * (`observeViewportById`) — and pushes the resolved geometry (the
  * vertical side flipped Bottom↔Top when the panel would overflow that viewport
  * edge, plus a horizontal shift to keep it inside) as ONE inline `Style` patched
  * IN PLACE via `bindStyleById`. The panel element itself is NEVER re-rendered,
  * so its focus seeding and open animation are not reset on a scroll tick. The
  * host-gated [[renderOpen]] path keeps the declared anchor UNMEASURED — its
  * single-subscription contract forbids nesting a measuring mount. Tall content
  * still uses `maxHeight` + the panel's own scrolling for the edge it does not
  * flip toward.
  *
  * Portal (`portal`, default off): with `portal(true)` the panel AND backdrop
  * re-home to `document.body` (the fork's element-level `portal(true)` — the
  * logical tree, dispatch, focus story and Escape chain stay unchanged), so
  * they escape ancestor `overflow` clipping, `transform`/`filter` containing
  * blocks and ancestor stacking contexts. Because a body-re-homed panel has no
  * CSS relationship to its anchor anymore, portal mode positions it with FIXED
  * viewport coordinates measured continuously through an invisible inset-0
  * anchor probe (flip + horizontal clamp included; `matchWidth` becomes a
  * measured `min-width` — the `min-width: 100%` class would resolve against
  * the viewport at body). The panel stays `opacity: 0` until the first measure
  * lands, so it never flashes unpositioned; the enter fade is subsumed by that
  * gate, the leave ghost plays unchanged.
  *
  * Honest limitation WITHOUT portal (server-side geometry): the panel stays
  * inside the anchor's subtree, so do NOT place `transform` (or
  * `filter`/`will-change`) on overlay ancestors: they make the ancestor the
  * containing block for `position: fixed` and trap the backdrop (and any fixed
  * content) inside it — or opt into `portal(true)`.
  *
  * Enter/leave animations: the panel fades in on open (a from-state
  * `enterTransition`) and plays Prime's own `p-anchored-overlay-leave-active`
  * keyframe on close via the body-level leave ghost. Opt out with `animate(false)`
  * (menu-family hosts that gate the panel's presence themselves do so).
  */
final case class Overlay private (
    openRef: SignalRef[Boolean],
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
    keyHandlerV: Maybe[KeyboardEvent => Any < Async] = Absent,
    triggerV: Maybe[UI] = Absent,
    kids: List[UI] = Nil
) extends Node:
    type Self = Overlay

    /** The element the panel anchors to. Rendering it here rather than beside the
      * overlay is what makes the anchor correct by construction: the overlay wraps
      * `ui` and its own panel in a `div.p-uic-overlay-anchor`, so the caller never
      * has to know that class exists. The trigger's own handlers are untouched —
      * this primitive does not toggle the ref for you.
      */
    def trigger(ui: UI): Overlay = copy(triggerV = Present(ui))

    /** Which anchor corner the panel opens from (default [[OverlayAnchor.BottomStart]]). */
    def anchor(v: OverlayAnchor): Overlay = copy(anchorV = v)

    /** Whether the panel measures itself continuously (self-addressing:
      * on open, then on every window scroll/resize) and flips its vertical anchor
      * (Bottom↔Top) when it would overflow the viewport edge, plus shifts
      * horizontally when it would overflow left/right (default true). The resolved
      * geometry is patched onto the panel IN PLACE (`bindStyleById`), so the panel
      * never re-renders. Opt out to keep the declared anchor unconditionally — the
      * server-honest, un-measured behavior. Only the standard `render` path flips;
      * the host-gated [[renderOpen]] (menu-family submenus) keeps the declared
      * anchor to preserve its single-subscription contract.
      */
    def autoFlip(v: Boolean): Overlay = copy(autoFlipV = v)

    /** Whether the open panel and backdrop re-home to `document.body` (default false) — the escape
      * from ancestor `overflow` clipping, `transform`/`filter` containing blocks and stacking
      * contexts. The panel is then positioned with measured FIXED viewport coordinates (continuous
      * anchor measure through an inset-0 probe; flip + clamp included; `matchWidth` as measured
      * `min-width`) and stays invisible until the first measure lands. Dispatch, Escape chain,
      * focus seeding/restore and the leave ghost work unchanged through the portal — see the class
      * doc. Only the standard `render` path portals; the host-gated [[renderOpen]] keeps its
      * single-subscription, in-tree contract.
      */
    def portal(v: Boolean): Overlay = copy(portalV = v)

    /** How the open overlay responds to scroll input outside its panel (default
      * [[Overlay.Scroll.Close]] — the Prime family's hide-on-scroll). See [[Overlay.Scroll]].
      */
    def scroll(v: Overlay.Scroll): Overlay = copy(scrollV = v)

    /** `min-width: 100%` of the anchor on the panel (default true — Prime's
      * field-panel behavior).
      */
    def matchWidth(v: Boolean): Overlay = copy(matchWidthV = v)

    /** Caps the panel height (CSS length) and makes it scroll (`overflow-y: auto`)
      * — the viewport-edge answer, same as Prime's scrollable panels.
      */
    def maxHeight(v: String): Overlay = copy(maxHeightV = Present(v))

    /** Caps the panel height in pixels — see [[maxHeight(v:String)*]]. */
    def maxHeight(px: Int): Overlay = copy(maxHeightV = Present(s"${px}px"))

    /** Whether a click outside the panel closes the overlay (default true). */
    def dismissOnOutsideClick(v: Boolean): Overlay = copy(dismissOnOutsideClickV = v)

    /** Whether Escape (with focus in the panel) closes the overlay (default true). */
    def dismissOnEscape(v: Boolean): Overlay = copy(dismissOnEscapeV = v)

    /** Whether the panel seeds focus on open and restores it on close via the
      * `data-kyo-focus-*` client contract (default true).
      */
    def seedFocus(v: Boolean): Overlay = copy(seedFocusV = v)

    /** Whether the panel animates on open/close (default true): a fade-in on open
      * (the fork's `enterTransition` from-state) and Prime's own
      * `p-anchored-overlay-leave-active` keyframe on close (via the leave ghost).
      * Opt out for menu-family panels that manage their own presence.
      */
    def animate(v: Boolean): Overlay = copy(animateV = v)

    /** Package-internal class hook: hosts (Select) stamp Prime's panel skin class
      * (`p-select-overlay`) onto the panel element.
      */
    private[uic] def panelClass(cls: String): Overlay = copy(panelClassesV = panelClassesV :+ cls)

    /** Package-internal: extra keydown handling on the panel (hosts wire their
      * keyboard navigation here). Runs for every key; the overlay's own Escape
      * dismissal runs first.
      */
    private[uic] def onPanelKeyDown(f: KeyboardEvent => Any < Async): Overlay =
        copy(keyHandlerV = Present(f))

    /** Adds panel children. */
    def apply(cs: UI*): Overlay = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        val gated: UI = when(openRef)(openContent)
        triggerV match
            case Present(t) => div.cssClass("p-uic-overlay-anchor")(toChild(t), toChild(gated))
            case Absent     => gated
    end render

    /** The open panel content for the standard (self-gated) `render` path: the
      * continuous flip/shift reposition when `autoFlip`, else the declared-anchor
      * render. Runs inside the `when(openRef)` region, so each open mints a fresh
      * id and a fresh reposition lifecycle; close tears it down (and with it the
      * scroll/resize observer, via the mount's Scope).
      */
    private def openContent(using Frame): UI =
        if portalV then
            // Portal: the panel (and backdrop) re-home to document.body via the fork's element-level
            // portal(true), so the CSS anchor geometry (absolute against the anchor) cannot position
            // them. Instead the anchor box is measured continuously through an inset-0 probe and the
            // panel placed with FIXED viewport coordinates patched in place — the portal analog of the
            // autoFlip reposition below (flip + clamp included). Same self-addressing shape: each open
            // mints fresh ids, close tears the observers down through the mount's Scope.
            UI.mounted {
                for
                    cmds     <- UI.commands
                    panelId  <- cmds.freshId
                    anchorId <- cmds.freshId
                yield fragment(
                    anchorProbe(anchorId),
                    portalRepositionTrigger(cmds, anchorId, panelId),
                    renderOpenAt(anchorV, Present(panelId), portal = true)
                )
                // The placeholder must be portal-shaped too: a non-portal placeholder panel sits IN FLOW
                // inside the anchor's scroll container for the frames before the effect content re-homes
                // to document.body — it overflows the container and the focus-auto seed scrolls it, which
                // flashes the container's scrollbar on every open. Portal-shaped, it is position:fixed and
                // opacity:0 (the unmeasured-paint gate) from the first frame, so it never touches layout.
            }.placeholder(renderOpenAt(anchorV, Absent, portal = true))
        else if !autoFlipV then renderOpenAt(anchorV, Absent)
        else
            // Self-addressing: on open, mint an id + stamp it on the panel,
            // then observe THAT panel by id — a reusable component has no way to learn
            // its own render path. The panel renders ONCE at the DECLARED anchor (its
            // class provides the first paint); the measured geometry is patched onto it
            // IN PLACE through `geom` + `bindStyleById`, so the panel element is never
            // replaced and its focus seed + open animation survive every scroll tick.
            UI.mounted {
                for
                    cmds <- UI.commands
                    id   <- cmds.freshId
                yield fragment(
                    repositionTrigger(cmds, id),
                    renderOpenAt(anchorV, Present(id))
                )
            }.placeholder(renderOpenAt(anchorV, Absent))

    /** An invisible sibling that wires the reposition AFTER the panel is in the
      * DOM: a nested `UI.mounted` whose effect runs once the enclosing content is
      * published, so `observeViewportById` resolves the freshly inserted panel
      * (getElementById would miss it if requested before the publish). It observes
      * the viewport — a `Signal[Maybe[Rect]]` the observer updates on every
      * scroll/resize (`Absent` until the first measure) — maps it to the panel
      * geometry Style (`Absent` -> `Style.empty`, so the anchor CLASS shows until
      * measured), and binds THAT onto the panel in place. No intermediate SignalRef
      * + callback bridge: the measured stream IS the source. Both primitives detach
      * when the mount's Scope closes on overlay close. Captures `cmds` so the inner
      * effect needs no `Env[Commands]`.
      */
    /** An invisible inset-0 child of the anchor element: the overlay cannot address its host anchor
      * (it only renders INTO it), but an absolutely positioned probe filling it shares its box
      * (padding-box — the anchor's border widths are the documented deviation), so observing the
      * probe by id measures the anchor continuously.
      */
    private def anchorProbe(id: String)(using Frame): UI =
        div.id(id).cssClass("p-uic-overlay-anchor-probe")

    /** The portal twin of [[repositionTrigger]]: observes BOTH the anchor probe (where to attach)
      * and the panel (the flip decision needs its size), combines the two measured streams and
      * binds the resolved fixed-position geometry onto the panel in place. `Style.empty` until both
      * have measured — the panel stays hidden (`opacity: 0` from `.p-uic-overlay-portal`) until the
      * first geometry patch turns it visible, so it never flashes unpositioned at the body.
      */
    private def portalRepositionTrigger(cmds: UI.Commands, anchorId: String, panelId: String)(using Frame): UI =
        UI.mounted {
            for
                anchorSig <- cmds.observeViewportById(anchorId)
                panelSig  <- cmds.observeViewportById(panelId)
                _ <- cmds.bindStyleById(
                    panelId,
                    anchorSig.combineLatest(panelSig).map {
                        case (Present(a), Present(p)) => Overlay.portalGeometryFor(anchorV, matchWidthV, a, p)
                        case _                        => Style.empty
                    }
                )
            yield UI.empty
        }.placeholder(UI.empty)

    private def repositionTrigger(cmds: UI.Commands, panelId: String)(using Frame): UI =
        UI.mounted {
            cmds.observeViewportById(panelId).map { rectSig =>
                cmds.bindStyleById(
                    panelId,
                    rectSig.map {
                        case Present(rect) => Overlay.geometryFor(anchorV, rect)
                        case Absent        => Style.empty
                    }
                )
            }.andThen(UI.empty)
        }.placeholder(UI.empty)

    /** Package-internal: renders backdrop + panel UNCONDITIONALLY — for hosts
      * (Menubar/TieredMenu/MegaMenu) that already subscribe to `openRef` in an
      * enclosing render and gate the overlay's presence themselves. Subscribing
      * twice to the same ref (the host's outer subscription plus this overlay's
      * own `when`) races the inner update against the outer replace and can
      * duplicate the panel subtree — the host-gated form has exactly one
      * subscription. Escape/outside-click still write `false` into the ref.
      */
    private[uic] def renderOpen(using Frame): UI =
        renderOpenAt(anchorV, Absent)

    /** The backdrop + panel at the DECLARED anchor, optionally id-stamped: the
      * shared builder behind both the host-gated [[renderOpen]] (no id) and the
      * autoFlip path (id-stamped, so `observeViewportById` can address it). The
      * measured flip/shift is applied separately, IN PLACE, via `bindStyleById` —
      * this builder always emits the declared-anchor geometry class, so the panel
      * is rendered exactly once.
      */
    private def renderOpenAt(anchor: OverlayAnchor, panelId: Maybe[String], portal: Boolean = false)(using Frame): UI =
        val close: Any < Async = openRef.set(false)

        val backdrop: List[UI] =
            if dismissOnOutsideClickV then
                var b = div.cssClass("p-uic-overlay-backdrop").onClickSelf(close)
                // The full-viewport backdrop intercepts every outside wheel (a fixed element chains scroll
                // to the DOCUMENT scroller, so inner scroll containers never move) — each Scroll mode turns
                // that interception into a deliberate behavior instead of a dead page.
                scrollV match
                    case Overlay.Scroll.Close => b = b.onScroll(close)
                    case Overlay.Scroll.Lock  => ()
                if portal then b = b.portal(true)
                List(b)
            else Nil

        var panel = div
            .cssClass("p-uic-overlay-panel")
            .cssClass(s"p-uic-overlay-${anchor.token}")
        panelId.foreach(id => panel = panel.id(id))
        // Portal mode re-homes the panel to document.body (fork portal) and switches to measured
        // fixed positioning: p-uic-overlay-portal pins position:fixed + the opacity gate, and the
        // match-width CLASS must not ride along — its min-width:100% would resolve against the
        // viewport at body (the leave-ghost lesson); the measured min-width replaces it.
        if portal then panel = panel.cssClass("p-uic-overlay-portal").portal(true)
        if matchWidthV && !portal then panel = panel.cssClass("p-uic-overlay-match-width")
        // Open fades the panel in (from-state transition); close plays Prime's own
        // anchored-overlay leave keyframe on the body-level leave ghost.
        if animateV then
            panel = panel.enterTransition("p-uic-enter-fade").leaveTransition("p-anchored-overlay-leave-active")
        panelClassesV.foreach(c => panel = panel.cssClass(c))
        maxHeightV.foreach { h =>
            panel = panel.style(Style.maxHeight(CssValue.length(h)).overflowY(_.auto))
        }
        if seedFocusV then
            panel = panel.focusAuto(true).focusRestore(true).tabIndex(-1).focusTrap(true)
        // Per-level event consumption (kyo `data-kyo-stop`): keydown dispatched
        // from inside this panel stops HERE — an Escape closes only this (the
        // innermost) overlay, ancestor panels keep their state. Only stamped when
        // this panel actually dismisses on Escape, so an Escape-inert panel stays
        // transparent to its enclosing overlay chain.
        if dismissOnEscapeV then panel = panel.stopPropagation(true)
        // A panel that navigates with arrow keys (Select/MultiSelect/Menu/... via onPanelKeyDown) must not let the
        // browser ALSO scroll the page underneath; the keydown is still delivered so the highlight moves.
        if keyHandlerV.isDefined then panel = panel.preventScrollKeys
        if dismissOnEscapeV || keyHandlerV.isDefined then
            panel = panel.onKeyDown { e =>
                e.key match
                    case Keyboard.Escape if dismissOnEscapeV => close
                    case _ =>
                        keyHandlerV match
                            case Present(f) => f(e)
                            case Absent     => ()
            }
        end if
        val panelUI: UI = panel(kids.map(toChild)*)

        fragment((backdrop :+ panelUI)*)
    end renderOpenAt
end Overlay

object Overlay:
    /** An overlay whose visibility binds two-way to `open`: writes to the ref
      * open and close the panel; Escape and outside clicks write `false` back.
      */
    def apply(open: SignalRef[Boolean]): Overlay = new Overlay(open)

    /** How an OPEN overlay responds to scroll input outside its panel. The invisible outside-click
      * backdrop covers the whole viewport, so page scrolling necessarily runs through it; the mode
      * decides what that means. Scrolling INSIDE the panel (a `maxHeight` listbox) is unaffected.
      *
      *   - [[Scroll.Close]] (default): the first outside scroll closes the overlay and the page
      *     scrolls on — the Prime family's hide-on-scroll (`ConnectedOverlayScrollHandler`) and
      *     native Chrome `<select>` behavior.
      *   - [[Scroll.Lock]]: outside scroll is swallowed while open — the page freezes, the panel
      *     stays put; the Radix/MUI modal scroll-lock feel.
      *
      * A third mode — scroll-through with the panel live-following its anchor (the Floating-UI
      * feel) — needs a fork-level outside-click primitive in place of the hit-blocking backdrop
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

    /** The resolved inline geometry patched onto the panel after a measure — the
      * flipped vertical placement plus the horizontal shift, as ONE [[kyo.Style]]
      * pushed through `bindStyleById`. It ALWAYS sets BOTH vertical edges (one of
      * them `auto`): `bindStyleById` MERGES declarations and never clears, so a
      * flip back to the declared side must actively reset the edge it is leaving,
      * or the stale `top`/`bottom` would linger. Edges match the anchor CSS — a
      * "below" side ([[OverlayAnchor.BottomStart]]/`BottomEnd`) is `top:100%;
      * bottom:auto`, an "above" side (`TopStart`/`TopEnd`) is `top:auto;
      * bottom:100%`. The horizontal shift stays a `margin-left` delta (never
      * `transform`, so it cannot smear the leave keyframe), exactly as [[shiftFor]]
      * fed `renderOpenAt`. Side anchors (`RightStart`/`LeftStart`) are never
      * flipped by [[flipAnchor]] and place off `left`/`right` + `top:0`, so their
      * geometry is the shift ALONE — emitting top/bottom would fight their class
      * placement.
      */
    /** The fixed-position geometry for a PORTALED panel: viewport coordinates computed from the
      * measured anchor box and panel size — a body-re-homed panel has no CSS relationship to its
      * anchor anymore. Flips the vertical side when the declared one cannot fit the panel (and the
      * other side can), right-aligns End anchors, clamps horizontally into the viewport (Gutter),
      * sizes `min-width` from the anchor for matchWidth, and turns the panel visible (`opacity`
      * gates the unmeasured first paint, see `.p-uic-overlay-portal`). Bottom-anchored panels pin
      * `top` (grow downward), top-anchored pin `bottom` (grow upward) — so a content-size change
      * stays attached without waiting for the next measure. Always sets BOTH vertical and BOTH
      * horizontal edges (one of each `auto`): `bindStyleById` MERGES and never clears, so a flip
      * back must actively reset the edge it is leaving.
      */
    private[uic] def portalGeometryFor(declared: OverlayAnchor, matchWidth: Boolean, anchor: UI.Rect, panel: UI.Rect): Style =
        val vw        = anchor.viewportWidth
        val vh        = anchor.viewportHeight
        val fitsBelow = anchor.y + anchor.height + panel.height <= vh - Gutter
        val fitsAbove = anchor.y - panel.height >= Gutter
        val resolved = declared match
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
