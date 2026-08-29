package kyo.uic

import kyo.*
import kyo.UI.*

/** VirtualScroller over a [[RowSource]]: where the rows it was handed end up, what the
  * spacer spans, and what a scroll asks for.
  *
  * The window is driven directly rather than through a fetch, because what is under test
  * is the arithmetic between a published window and the slots on the screen, and a fetch
  * would only put timing between the two.
  */
class VirtualScrollerTest extends UicTest:

    /** Five rows fit the viewport (200 / 40) and no overscan is added, so a window at
      * scroll 0 is slots 0 to 5.
      */
    private def scroller(using Frame) =
        uic.VirtualScroller(Seq.empty[String]).itemSize(40).height(200).overscan(0)(s => span(s))

    /** The row slots of a rendered window, in the order they were emitted. */
    private def slots(ui: UI): List[UI.Ast.Element] = ui match
        case e: UI.Ast.Element =>
            e.children.collect {
                case c: UI.Ast.Element if c.attrs.cssClasses.contains("p-uic-vs-item") => c
            }.toList
        case _ => Nil

    private def spacerHeight(ui: UI): Maybe[Length] = ui match
        case e: UI.Ast.Element =>
            Maybe.fromOption(
                e.children.collect {
                    case c: UI.Ast.Element if c.attrs.cssClasses.contains("p-virtualscroller-spacer") => c
                }.flatMap(_.attrs.uiStyle.props.collect { case Style.Prop.Height(v) => v }).headOption
            )
        case _ => Absent

    /** What one slot shows: the text of a row, or nothing where it has not arrived. */
    private def label(slot: UI.Ast.Element): String =
        slot.children.collect { case e: UI.Ast.Element => e }
            .flatMap(_.children.collect { case t: UI.Ast.Text => t.value }).mkString

    private def pending(slot: UI.Ast.Element): Boolean =
        slot.children.collect { case e: UI.Ast.Element => e }.exists(_.attrs.cssClasses.contains("p-skeleton"))

    private def topOf(slot: UI.Ast.Element): Maybe[Length] =
        Maybe.fromOption(slot.attrs.uiStyle.props.collect { case Style.Prop.Top(v) => v }.headOption)

    // The offset travels with the rows for exactly this reason: a window that still
    // describes an earlier range must not be drawn at the range the viewport asked for.
    "the rows are placed at the offset the source published, not at the range asked for" in {
        val ui    = scroller.sourceWindow(0.0, RowSource.Window(3, Seq("x", "y")), Total.Known(100))
        val cells = slots(ui)
        assert(cells.size == 6, "five rows fit the viewport, plus the one the arithmetic reaches into")
        assert(cells.map(label) == List("", "", "", "x", "y", ""), "rows 3 and 4 hold what arrived")
        assert(cells.map(pending) == List(true, true, true, false, false, true), "and the rest hold a slot")
        assert(topOf(cells(3)) == Present(120.px), "row 3 sits three rows down, wherever it came from")
    }

    "a known total spans the whole list" in {
        val ui = scroller.sourceWindow(0.0, RowSource.Window(0, Seq("a")), Total.Known(100))
        assert(spacerHeight(ui) == Present(4000.px), "a hundred rows of forty")
    }

    // Infinite scrolling falls out of the total rather than needing a mode of its own.
    "an unknown total leaves a screen to scroll into while anything follows" in {
        val loaded = (0 until 10).map(_.toString)
        val more   = scroller.sourceWindow(0.0, RowSource.Window(0, loaded), Total.Unknown(true))
        val done   = scroller.sourceWindow(0.0, RowSource.Window(0, loaded), Total.Unknown(false))
        assert(spacerHeight(more) == Present(600.px), "ten loaded plus the five that fit")
        assert(spacerHeight(done) == Present(400.px), "and nothing following stops at the ten")
    }

    "the spacer reaches as far as the source has ever served" in {
        val ui = scroller.sourceWindow(0.0, RowSource.Window(0, Seq("a")), Total.Unknown(true, 40))
        assert(
            spacerHeight(ui) == Present(1800.px),
            "forty rows are known to exist even with one on the screen, plus the screen ahead"
        )
    }

    "the window attributes report what was drawn" in {
        val ui = scroller.sourceWindow(400.0, RowSource.Window(10, Seq("a", "b")), Total.Known(100))
        val e  = ui.asInstanceOf[UI.Ast.Element]
        assert(e.attrs.dataAttrs.get("uic-vs-first").contains("10"), "scrolled ten rows down")
        assert(e.attrs.dataAttrs.get("uic-vs-count").contains("6"))
    }

    "a scroll asks the source for the range it is about to draw" in {
        for
            query  <- Signal.initRef("a")
            source <- RowSource.init(query, pageSize = 6)((_, o, l) => (Seq.empty[String], Total.Known(0)))
            scroll <- Signal.initRef(0.0)
            ui = uic.VirtualScroller(source).itemSize(40).height(200).overscan(0)(s => span(s)).wired(scroll)
            handler = ui match
                case e: UI.Ast.Element => e.attrs.onScrollPos
                case _                 => Absent
            _ <- handler.getOrElse(throw new AssertionError("the viewport reports no scroll"))(
                UI.ScrollPositionEvent(400.0, 0.0, Absent)
            )
            at  <- scroll.get
            ask <- source.demand.get
        yield
            assert(at == 400.0, "the position the browser reported")
            assert(ask == RowSource.Demand(10, 6), "and the rows that position puts on the screen")
    }

end VirtualScrollerTest
