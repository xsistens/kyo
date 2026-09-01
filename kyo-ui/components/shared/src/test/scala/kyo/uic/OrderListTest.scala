package kyo.uic

import kyo.*
import kyo.UI.*

/** `uic.OrderList`: the one-column half of the pair, and the same rules as [[PickListTest]] holds
  * for the two-column one.
  */
class OrderListTest extends UicTest:

    private def list(items: Seq[String], selected: Set[String])(using
        Frame
    ): (SignalRef[Seq[String]], SignalRef[Set[String]], UI) < Async =
        for
            xs  <- Signal.initRef(items)
            sel <- Signal.initRef(selected)
            ui = uic.OrderList[String]().items(xs)(identity).selected(sel).render
        yield (xs, sel, ui)

    private def button(ui: UI, name: String)(using Frame): UI.Ast.Button < Sync =
        elements(ui).map(_.collectFirst {
            case b: UI.Ast.Button if b.attrs.ariaAttrs.get("label").contains(name) => b
        }.getOrElse(throw new AssertionError(s"no button labelled $name")))

    "the whole control is ONE reactive region, so nothing can repaint an older snapshot" in {
        for
            (_, _, ui) <- list(Seq("a", "b"), Set("a"))
            regions    <- regionsAbove(ui, "p-orderlist")
        yield assert(regions == 1)
    }

    "a move button reorders the selection and leaves it selected" in {
        for
            (xs, sel, ui) <- list(Seq("a", "b", "c"), Set("a"))
            down          <- button(ui, "Move Down")
            _             <- click(down)
            order         <- xs.get
            still         <- sel.get
        yield assert(order == Seq("b", "a", "c") && still == Set("a"))
    }

    "the move buttons stay in the tab order and say aria-disabled instead of leaving it" in {
        for
            (_, _, ui) <- list(Seq("a", "b"), Set.empty)
            up         <- button(ui, "Move Up")
        yield
            assert(!up.disabled.contains(true))
            assert(up.attrs.ariaAttrs.get("disabled").contains("true"))
    }

end OrderListTest
