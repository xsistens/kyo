package kyo.uic

import kyo.*
import kyo.UI.*

/** `uic.OrderList`: the one-column half of the pair, and the same rules as [[PickListTest]] holds
  * for the two-column one. Its order and its selection are held to rule A ([[ReactiveValueContract]]).
  */
class OrderListTest extends ReactiveValueContract:

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

    "a disabled list says nothing about who hears it, since the reader cannot change it" in {
        for
            xs  <- Signal.initRef(Seq("a", "b"))
            off <- elementsWithClass(uic.OrderList[String]().items(xs.readOnly)(identity).disabled(true).render, "p-uic-key-error")
        yield assert(off.isEmpty, s"nobody can change it: ${off.map(textOf)}")
    }

    // ---- rule A ----

    private def rowTexts(t: UI)(using Frame): Seq[String] < Sync = elementsWithClass(t, "p-listbox-option").map(_.map(textOf).toList)

    /** Whether no move button moves anything, and every one says so. */
    private def movesFixed(t: UI)(using Frame): Boolean < Sync =
        elementsWithClass(t, "p-button").map(bs =>
            bs.size == 4 && bs.forall(b => !declaresClick(b) && b.attrs.ariaAttrs.get("disabled").contains("true"))
        )

    // The rows are the items' own, so the slot has no unbound form. A move acts on the selection, which is held at `a`.
    holds(new Slot[uic.OrderList[String], Seq[String]](
        name = "items",
        bare = Absent,
        bind = uic.OrderList[String]().items(_)(identity).selection(Set("a")),
        hear = (o, f) => o.onItemsChange(f),
        event = "onItemsChange",
        render = _.render,
        start = Seq("a", "b", "c"),
        next = Seq("b", "a", "c"),
        control = button(_, "Move Down"),
        shows = rowTexts(_),
        isFixed = Present(movesFixed(_))
    ))

    private def row(t: UI, text: String)(using Frame): UI.Ast.Element < Sync =
        elementsWithClass(t, "p-listbox-option").map(_.find(textOf(_) == text).getOrElse(throw new AssertionError(s"no row $text")))

    /** Whether no row picks anything, and the list says the selection is read-only (an option has no such state of its own). */
    private def picksFixed(t: UI)(using Frame): Boolean < Sync =
        for
            rows <- elementsWithClass(t, "p-listbox-option")
            list <- listOf(t)
        yield rows.nonEmpty && rows.forall(r => !declaresClick(r)) && list.attrs.ariaAttrs.get("readonly").contains("true")

    private def items(using Frame) = uic.OrderList[String]().items(Seq("a", "b", "c"))(identity)

    holds(new Slot[uic.OrderList[String], Set[String]](
        name = "selection",
        bare = Present(items),
        bind = items.selection(_),
        hear = (o, f) => o.onSelectionChange(f),
        event = "onSelectionChange",
        render = _.render,
        start = Set.empty,
        next = Set("b"),
        control = row(_, "b"),
        shows = elementsWithClass(_, "p-listbox-option-selected").map(_.map(textOf).toSet),
        isFixed = Present(picksFixed(_)),
        // Unbound, the selection is the mount's.
        ready = Present(elementsWithClass(_, "p-listbox-option").map(_.exists(declaresClick)))
    ))

end OrderListTest
