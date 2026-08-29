package kyo.uic

import kyo.*
import kyo.UI.*

/** VirtualScroller — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * VirtualScroller anatomy: the scroll viewport `div.p-virtualscroller` >
  * `div.p-virtualscroller-content` translated to the visible window), so the
  * extracted `@primeuix` virtualscroller CSS applies verbatim. Only the rows in
  * the visible window (plus a small overscan) are ever rendered — a 10 000-row
  * list ships a handful of `<div>`s, not ten thousand.
  *
  * Server-honest windowing: a fixed row height (`itemSize`) and a fixed
  * viewport height (`height`) define the window arithmetically. A scroll
  * position lives in an internal `SignalRef[Double]`; the window is `first =
  * floor(scroll / itemSize) − overscan` through the last visible row plus
  * overscan. The windowed rows are absolutely positioned at their natural offset
  * (`top = i·itemSize`) inside a full-height spacer, so the browser's native
  * scroll shows the correct rows. Writing the scroll ref re-renders the window.
  *
  * Interaction is native scroll (`overflow: auto`, a real scrollbar — matching
  * PrimeReact/PrimeVue): the browser owns the scroll and reports its resulting
  * position via the fork's `onScrollPosition`, which the viewport accumulates
  * into the scroll ref (clamped). `overscroll-behavior: contain` keeps the
  * wheel/trackpad from chaining to the page once the list reaches its edge. The
  * viewport is the stable reactive root, so its `scrollTop` persists across
  * window re-renders — the server reads scroll position but never writes it back,
  * so there is no feedback loop.
  *
  * HONEST cost: the row CONTENT is server-rendered, so a scroll round-trips to
  * recompute and repaint the window. The native scrollbar itself is instant (the
  * spacer defines the full height); over a slow link the rows repaint a beat
  * behind the scrollbar. This is the deliberate trade for a zero-client-logic,
  * server-authoritative list.
  *
  * Over a [[RowSource]] the sequence is not held at all. `VirtualScroller(source)`
  * writes the visible range into the source's demand on every scroll and draws what
  * the source published, so a list of any length costs the blocks the reader has
  * actually looked at. Rows inside the window that have not arrived yet are drawn as
  * `Skeleton` slots of the same height, which is what keeps the geometry stable while
  * they load. The published window carries the row index it starts at, and the rows are
  * positioned by THAT rather than by the range the viewport asked for, so a window still
  * describing the previous range is never drawn at the new range's offsets.
  *
  * How far the spacer reaches then comes from [[Total]]: a known count spans the whole
  * list, and an unknown one spans what has loaded plus one screen while anything
  * follows, which is infinite scrolling and needs no separate mode.
  *
  * HONEST deferral: auto-sizing the viewport from a `requestMeasure` is deferred, since
  * kyo-ui gives a reusable component no way to learn its own render PATH, which is what
  * `requestMeasure` targets, so the viewport height is taken explicitly. The measure
  * round-trip is exercised in the demo, where the measured element's path is known.
  */
final case class VirtualScroller[A] private (
    items: Seq[A],
    sourceV: Maybe[RowSource[?, A]] = Absent,
    itemSizeV: Int = 40,
    heightV: Int = 200,
    overscanV: Int = 3,
    template: Maybe[A => UI] = Absent
) extends Node:
    type Self = VirtualScroller[A]

    /** Fixed row height in px (Prime's `itemSize`; every row is clipped to it). */
    def itemSize(px: Int): VirtualScroller[A] = copy(itemSizeV = math.max(1, px))

    /** Fixed viewport height in px (Prime's scroll-height). */
    def height(px: Int): VirtualScroller[A] = copy(heightV = math.max(1, px))

    /** Extra rows rendered above and below the visible window (default 3). */
    def overscan(n: Int): VirtualScroller[A] = copy(overscanV = math.max(0, n))

    /** Sets the per-item template and completes the builder. */
    def apply(f: A => UI): VirtualScroller[A] = copy(template = Present(f))

    private def totalHeight: Int               = items.length * itemSizeV
    private def maxScroll: Double              = math.max(0.0, (totalHeight - heightV).toDouble)
    private def clampScroll(v: Double): Double = math.max(0.0, math.min(maxScroll, v))

    /** Rows that fit the viewport, which is the window before the overscan is added. */
    private def visible: Int = math.ceil(heightV.toDouble / itemSizeV).toInt

    /** The row range the arithmetic puts at `scrollTop`, overscan included and before any
      * clamp at the end: the scroll handler has no row count to clamp against, and the
      * render that does clamps it there.
      */
    private def span(scrollTop: Double): (Int, Int) =
        val first = math.max(0, (math.max(0.0, scrollTop) / itemSizeV).toInt - overscanV)
        (first, first + visible + 2 * overscanV + 1)

    /** How many rows the spacer spans over a source. A known count says it outright; an
      * unknown one spans what has loaded plus one screen while anything follows, so there
      * is always somewhere left to scroll into and the next scroll asks for it.
      */
    private def extent(total: Total, loadedEnd: Int): Int = total match
        case Total.Known(n)                => math.max(0, n)
        case Total.Unknown(true, atLeast)  => math.max(loadedEnd, atLeast) + visible
        case Total.Unknown(false, atLeast) => math.max(loadedEnd, atLeast)

    private[uic] def render(using Frame): UI =
        // The scroll position lives in a signal allocated by this effectful mount. The VIEWPORT shell is stable — only
        // its inner window is reactive — so the browser-owned `scrollTop` survives every window re-render. This matters
        // because a reactive region updates via `el.outerHTML =` (full element replacement); if the shell itself were
        // reactive, each scroll would recreate it and reset `scrollTop` to 0, fighting the user. Static projections (SSG,
        // the SSR GET) show the placeholder window at scroll 0, inert, until the transport attaches.
        UI.mounted {
            for
                scroll <- Signal.initRef(0.0)
                // The first range has to be asked for by someone, and no scroll has
                // happened yet to ask for it.
                _ <- (sourceV match
                    case Present(src) => src.demand.set(RowSource.Demand(0, visible + 2 * overscanV + 1))
                    case _            => ()
                ): Unit < Async
            yield wired(scroll)
        }.placeholder(shell(Absent)(placeholderWindow))

    /** The subscription tree the mount publishes — the golden seam. */
    private[uic] def wired(scroll: SignalRef[Double])(using Frame): UI =
        sourceV match
            case Present(src) =>
                shell(Present(scroll))(
                    scroll.render(top => src.window.render(w => src.total.render(t => sourceWindow(top, w, t))))
                )
            case Absent => shell(Present(scroll))(scroll.render(top => window(top)))

    /** What a static projection of a source-backed list shows: one screen of slots with
      * nothing in them yet, which is the shape the rows will take.
      */
    private def placeholderWindow(using Frame): UI =
        if sourceV.isEmpty then window(0.0)
        else sourceWindow(0.0, RowSource.Window(0, Seq.empty[A]), Total.Unknown(true))

    /** The stable viewport shell: fixed sizing, the native-scroll container class, and the `onScrollPosition` handler.
      * Rendered once at mount-attach and never re-rendered, so its `scrollTop` stays browser-owned as the window inside
      * updates.
      */
    private def shell(ref: Maybe[SignalRef[Double]])(inner: UI)(using Frame): UI =
        template match
            case Absent => div.cssClass("p-virtualscroller").cssClass("p-component")
            case Present(_) =>
                var v = div
                    .cssClass("p-virtualscroller")
                    .cssClass("p-component")
                    .cssClass("p-uic-vs-viewport")
                    .style(_.height(heightV.px).minHeight(heightV.px).position(_.relative))
                // The server reads scroll position but never writes it back — no feedback loop.
                ref.foreach { r =>
                    v = v.onScrollPosition { (e: ScrollPositionEvent) =>
                        // Position and demand come out of the SAME reported number, so the
                        // range asked for is always the range being drawn.
                        (sourceV match
                            case Present(src) =>
                                val top           = math.max(0.0, e.scrollTop)
                                val (first, last) = span(top)
                                r.set(top).andThen(src.demand.set(RowSource.Demand(first, last - first)))
                            case _ => r.set(clampScroll(e.scrollTop))
                        ): Unit < Async
                    }
                }
                v(toChild(inner))

    /** The reactive window for scroll position `scrollTop`: the full-height spacer plus the absolutely-positioned rows
      * of the visible window (+ overscan above and below). Replacing this on scroll (via the reactive `outerHTML`
      * update) never touches the shell's `scrollTop`.
      */
    private def window(scrollTop: Double)(using Frame): UI =
        template match
            case Absent => div.cssClass("p-virtualscroller-content")
            case Present(tpl) =>
                val (firstIdx, reach) = span(clampScroll(scrollTop))
                val lastIdx           = math.min(items.length, reach)
                // Positional children (no keys): the window of row slots is reused across scrolls, the diff just updates each
                // slot's content + `top` — keying by absolute index would churn add/remove on every scroll. Each row is
                // absolutely positioned at its natural offset (`.p-uic-vs-item` supplies position/left/right/overflow), so
                // native scroll shows the right rows. The container is unpositioned, so rows resolve against the shell.
                val rows: List[UI] = (firstIdx until lastIdx).toList.map { i =>
                    div
                        .cssClass("p-uic-vs-item")
                        .style(_.top((i * itemSizeV).px).height(itemSizeV.px))(toChild(tpl(items(i))))
                }
                // Prime's spacer holds the full scroll extent (`.p-virtualscroller-spacer`): an out-of-flow 1px column of the
                // total height, so the shell's native scrollbar spans all rows.
                val spacer = div
                    .cssClass("p-virtualscroller-spacer")
                    .style(_.position(_.absolute).width(1.px).height(totalHeight.px))
                div
                    .cssClass("p-virtualscroller-content")
                    .data("uic-vs-first", firstIdx.toString)
                    .data("uic-vs-count", (lastIdx - firstIdx).toString)((spacer :: rows).map(toChild)*)
    end window

    /** The window over a [[RowSource]]: the same geometry, with the rows taken from what
      * the source published and a slot left empty where it has not reached yet.
      *
      * The rows are placed by the offset the SOURCE published, not by the range this
      * viewport last asked for: those two differ for exactly as long as a fetch is in
      * flight, which is when a list is looked at hardest.
      */
    private[uic] def sourceWindow(scrollTop: Double, w: RowSource.Window[A], total: Total)(using Frame): UI =
        template match
            case Absent => div.cssClass("p-virtualscroller-content")
            case Present(tpl) =>
                val loadedEnd         = w.offset + w.rows.size
                val count             = extent(total, loadedEnd)
                val room              = math.max(0.0, (count * itemSizeV - heightV).toDouble)
                val (firstIdx, reach) = span(math.max(0.0, math.min(room, scrollTop)))
                val lastIdx           = math.min(count, reach)
                val rows: List[UI] = (firstIdx until lastIdx).toList.map { i =>
                    val cell =
                        if i >= w.offset && i < loadedEnd then tpl(w.rows(i - w.offset))
                        else Skeleton().height(s"${itemSizeV}px").render
                    div
                        .cssClass("p-uic-vs-item")
                        .style(_.top((i * itemSizeV).px).height(itemSizeV.px))(toChild(cell))
                }
                val spacer = div
                    .cssClass("p-virtualscroller-spacer")
                    .style(_.position(_.absolute).width(1.px).height((count * itemSizeV).px))
                div
                    .cssClass("p-virtualscroller-content")
                    .data("uic-vs-first", firstIdx.toString)
                    .data("uic-vs-count", (lastIdx - firstIdx).toString)((spacer :: rows).map(toChild)*)
    end sourceWindow
end VirtualScroller

object VirtualScroller:
    /** A virtual scroller over `items`; set the row template via `apply(template)`. */
    def apply[A](items: Seq[A]): VirtualScroller[A] = new VirtualScroller[A](items)

    /** A virtual scroller over a [[RowSource]], which holds no sequence at all: the
      * visible range is written into the source on every scroll, and what has not arrived
      * yet is drawn as a slot of the same height.
      */
    def apply[A](source: RowSource[?, A]): VirtualScroller[A] =
        new VirtualScroller[A](Seq.empty, Present(source))
end VirtualScroller
