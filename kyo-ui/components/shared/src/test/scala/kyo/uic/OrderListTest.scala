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
            ui = uic.OrderList[String]().items(xs)(identity).selection(sel).render
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

    /** An OrderList wired the way its mount wires it, with the cursor its list roves. */
    private def wired(items: Seq[String], selected: Set[String])(using
        Frame
    ): (SignalRef[Seq[String]], UI) < Async =
        for
            xs  <- Signal.initRef(items)
            sel <- Signal.initRef(selected)
            hi  <- Signal.initRef(-1)
            ui = uic.OrderList[String]().items(xs)(identity).selection(sel)
                .wired(Present(uic.ListReorder.Cursor(hi, "o")))
        yield (xs, ui)

    private def listOf(ui: UI)(using Frame): UI.Ast.Element < Sync =
        elementWithClass(ui, "p-listbox-list")

    "the list is ONE tab stop with a highlight roving inside it" in {
        for
            (_, ui) <- wired(Seq("a", "b"), Set.empty)
            list    <- listOf(ui)
            rows    <- elementsWithClass(ui, "p-listbox-option")
        yield
            assert(list.attrs.tabIndex.contains(0))
            assert(rows.forall(_.attrs.tabIndex.isEmpty))
    }

    "a carried arrow makes the move its button makes" in {
        for
            (xs, ui) <- wired(Seq("a", "b", "c"), Set("a"))
            list     <- listOf(ui)
            _        <- press(list, UI.Keyboard.ArrowDown, UI.Modifiers.none.copy(meta = true))
            order    <- xs.get
        yield assert(order == Seq("b", "a", "c"))
    }

    "and with Shift it goes to the end" in {
        for
            (xs, ui) <- wired(Seq("a", "b", "c"), Set("a"))
            list     <- listOf(ui)
            _        <- press(list, UI.Keyboard.ArrowUp, UI.Modifiers.none.copy(meta = true, shift = true))
            _        <- press(list, UI.Keyboard.ArrowDown, UI.Modifiers.none.copy(ctrl = true, shift = true))
            order    <- xs.get
        yield assert(order == Seq("b", "c", "a"))
    }

    "a plain arrow moves the highlight and leaves the order alone" in {
        for
            (xs, ui) <- wired(Seq("a", "b"), Set("a"))
            list     <- listOf(ui)
            _        <- press(list, UI.Keyboard.ArrowDown)
            order    <- xs.get
        yield assert(order == Seq("a", "b"))
    }

    "a horizontal arrow is nobody's, since there is no other column to send anything to" in {
        for
            (xs, ui) <- wired(Seq("a", "b"), Set("a"))
            list     <- listOf(ui)
            _        <- press(list, UI.Keyboard.ArrowRight, UI.Modifiers.none.copy(meta = true))
            order    <- xs.get
        yield assert(order == Seq("a", "b"))
    }

    // The order and the selection live somewhere else: the list shows them and hands a move or a pick over.
    "an order and a selection bound one way are reported and not written" in {
        for
            xs        <- Signal.initRef(Seq("a", "b", "c"))
            sel       <- Signal.initRef(Set("a"))
            seenOrder <- Signal.initRef(Seq.empty[String])
            seenSel   <- Signal.initRef(Set.empty[String])
            ui = uic.OrderList[String]().items(xs.readOnly)(identity).onItemsChange(seenOrder.set)
                .selection(sel.readOnly).onSelectionChange(seenSel.set).render
            down <- button(ui, "Move Down")
            _    <- click(down)
            rows <- elementsWithClass(ui, "p-listbox-option")
            _    <- click(rows(2))
            o    <- xs.get
            s    <- sel.get
            so   <- seenOrder.get
            ss   <- seenSel.get
        yield
            assert(o == Seq("a", "b", "c") && so == Seq("b", "a", "c"), s"order held $o, reported $so")
            assert(s == Set("a") && ss == Set("a", "c"), s"selection held $s, reported $ss")
    }

    "a two-way order is written before the list reports it" in {
        for
            xs   <- Signal.initRef(Seq("a", "b", "c"))
            sel  <- Signal.initRef(Set("c"))
            seen <- Signal.initRef((Seq.empty[String], Seq.empty[String]))
            ui = uic.OrderList[String]().items(xs)(identity).onItemsChange(n => xs.get.map(h => seen.set((n, h)))).selection(sel).render
            up <- button(ui, "Move Up")
            _  <- click(up)
            s  <- seen.get
        yield assert(s == (Seq("a", "c", "b"), Seq("a", "c", "b")), s"reported, held: $s")
    }

    "one-way bindings nothing hears say so" in {
        for
            xs   <- Signal.initRef(Seq("a", "b"))
            sel  <- Signal.initRef(Set("a"))
            deaf <-
                elementsWithClass(uic.OrderList[String]().items(xs.readOnly)(identity).selection(sel.readOnly).render, "p-uic-key-error")
        yield
            val text = deaf.map(textOf).mkString
            assert(text.contains("onItemsChange") && text.contains("onSelectionChange"), s"nobody hears: $text")
    }

end OrderListTest
