package kyo.uic

import kyo.*

/** The `value` slot: a `SignalRef` binds two-way, any other signal one way, and a constant is the starting text. A one-way
  * signal nobody hears says so; a constant does not, since the reader's text stays where they typed it.
  */
class InputTest extends UicTest:

    /** Whether a field's binding is the constant `v`, which is how a one-way value reaches the element. */
    private def shows[V](bound: Maybe[UI.Bound[V]], v: V)(using CanEqual[V, V]): Boolean =
        bound.exists { case UI.Bound.Const(c) => c == v; case _ => false }

    private def field(ui: UI)(using Frame): (UI.Ast.Element & UI.Ast.TextInput) < Sync =
        elements(ui).map(_.collectFirst { case t: (UI.Ast.Element & UI.Ast.TextInput) => t }
            .getOrElse(throw new AssertionError("no text field")))

    "a value bound one way is shown, an edit is reported without being written, and the field follows the owner" in {
        for
            held <- Signal.initRef("a")
            seen <- Signal.initRef(List.empty[String])
            ui = uic.Input().value(held.readOnly).onInput(v => seen.updateAndGet(_ :+ v).unit).render
            f     <- field(ui)
            _     <- handled(f.onInput.getOrElse(throw new AssertionError("no input handler"))("ab"))
            h     <- held.get
            s     <- seen.get
            _     <- held.set("xy")
            again <- field(ui)
        yield
            assert(shows(f.value, "a"), s"shows the signal: ${f.value}")
            assert(s == List("ab") && h == "a", s"reported $s, held $h")
            assert(shows(again.value, "xy"), s"follows the owner: ${again.value}")
    }

    "a ref stays kyo-ui's own two-way binding" in {
        for
            held <- Signal.initRef("a")
            f    <- field(uic.Input().value(held).render)
        yield assert(f.value.exists { case UI.Bound.Ref(r) => r eq held; case _ => false }, s"binds the ref: ${f.value}")
    }

    "a signal nobody hears says so; a constant, onInput, onChange, a ref, readonly or disabled keep it quiet" in {
        def errors(c: uic.Input): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held     <- Signal.initRef("a")
            deaf     <- errors(uic.Input().value(held.readOnly))
            constant <- errors(uic.Input().value("a"))
            input    <- errors(uic.Input().value(held.readOnly).onInput(_ => ()))
            change   <- errors(uic.Input().value(held.readOnly).onChange(_ => ()))
            twoWay   <- errors(uic.Input().value(held))
            ro       <- errors(uic.Input().value(held.readOnly).readonly(true))
            off      <- errors(uic.Input().value(held.readOnly).disabled(true))
        yield
            assert(deaf.size == 1 && textOf(deaf.head).contains("onInput or onChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(
                constant.isEmpty && input.isEmpty && change.isEmpty && twoWay.isEmpty && ro.isEmpty && off.isEmpty,
                "the text stays, somebody hears, or nobody can edit"
            )
        end for
    }

    "an edit its owner does not take is painted away again" in {
        for
            held  <- Signal.initRef("a")
            again <- repaintsAfter(uic.Input().value(held.readOnly).onInput(_ => ()).render, held)(t =>
                field(t).map(f => handled(f.onInput.getOrElse(throw new AssertionError("no input handler"))("ab")))
            )
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

end InputTest
