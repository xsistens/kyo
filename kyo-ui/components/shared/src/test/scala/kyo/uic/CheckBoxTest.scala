package kyo.uic

import kyo.*

/** The `checked` slot holds to rule A ([[ReactiveValueContract]]); the cases after it are what only a native box does: it
  * paints a toggle its owner does not take away again, and it stays quiet wherever nobody can toggle it.
  */
class CheckBoxTest extends ReactiveValueContract:

    holds(nativeBoolean[uic.CheckBox](
        name = "checked",
        bare = Present(uic.CheckBox()),
        bind = uic.CheckBox().checked(_),
        hear = (c, f) => c.onChange(f),
        event = "onChange",
        render = _.render,
        input = "p-checkbox-input",
        root = "p-checkbox",
        checkedClass = "p-checkbox-checked"
    ))

    private def box(ui: UI)(using Frame): UI.Ast.Checkbox < Sync =
        elements(ui).map(_.collectFirst { case c: UI.Ast.Checkbox => c }.getOrElse(throw new AssertionError("no checkbox")))

    private def toggle(b: UI.Ast.Checkbox, to: Boolean)(using Frame): Any < Async =
        handled(b.onChange.getOrElse(throw new AssertionError("no toggle wired"))(to))

    "a signal nobody hears says nothing where it reaches somebody or nobody can toggle it: a ref, readonly, disabled" in {
        def errors(c: uic.CheckBox): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef(false)
            twoWay <- errors(uic.CheckBox().checked(held))
            ro     <- errors(uic.CheckBox().checked(held.readOnly).readonly(true))
            off    <- errors(uic.CheckBox().checked(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can toggle")
        end for
    }

    "a toggle its owner does not take is painted away again" in {
        for
            held  <- Signal.initRef(false)
            again <- repaintsAfter(uic.CheckBox().checked(held.readOnly).onChange(_ => ()).render, held)(t => box(t).map(toggle(_, true)))
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

    "a constant with a listener paints itself again after a report, so a toggle the owner does not take is put back" in {
        for again <- repaintsAfter(uic.CheckBox().checked(false).onChange(_ => ()).render)(t => box(t).map(toggle(_, true)))
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

    "a fixed constant of true is drawn ticked" in {
        for
            b       <- box(uic.CheckBox().checked(true).render)
            root    <- elementWithClass(uic.CheckBox().checked(true).render, "p-checkbox")
            checked <- carriesClass(root, "p-checkbox-checked")
        yield assert(b.checked.exists { case UI.Bound.Const(c) => c; case _ => false } && checked, s"shows the constant: ${b.checked}")
    }

    "an unbound box somebody hears keeps its own state and reports the toggle as well" in {
        for
            seen  <- Signal.initRef(List.empty[Boolean])
            after <- publishedAfter(uic.CheckBox("Free").onChange(v => seen.updateAndGet(_ :+ v).unit).render)(t =>
                box(t).map(toggle(_, true))
            )
            ticked <- after.fold(Kyo.lift(false))(t => elementsWithClass(t, "p-checkbox-checked").map(_.nonEmpty))
            s      <- seen.get
        yield
            assert(ticked, "the drawn box stayed empty after the reader ticked it")
            assert(s == List(true), s"reported $s")
    }

    "an unbound box starts over when a region around it re-renders: its state ends with its mount instance" in {
        for
            parent <- Signal.initRef(0)
            after  <- publishedAfterEach(UI.render(parent)(_ => uic.CheckBox("Free").render), parent)(
                t => box(t).map(toggle(_, true)),
                // The tick the toggle draws has to be through, or the next gesture would count it as what the re-render drew.
                t => elementsWithClass(t, "p-checkbox-checked").map(c => if c.isEmpty then throw new AssertionError("not ticked yet")),
                _ => parent.set(1)
            )
            ticked <- after.fold(Kyo.lift(true))(t => elementsWithClass(t, "p-checkbox-checked").map(_.nonEmpty))
        yield assert(!ticked, "the box kept its tick through the re-render")
    }

end CheckBoxTest
