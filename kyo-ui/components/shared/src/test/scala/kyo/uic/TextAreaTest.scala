package kyo.uic

import kyo.*

/** The `value` slot: a `SignalRef` binds two-way, any other signal one way, and a constant is the starting text. A one-way
  * signal nobody hears says so.
  */
class TextAreaTest extends UicTest:

    /** Whether a field's binding is the constant `v`, which is how a one-way value reaches the element. */
    private def shows[V](bound: Maybe[UI.Bound[V]], v: V)(using CanEqual[V, V]): Boolean =
        bound.exists { case UI.Bound.Const(c) => c == v; case _ => false }

    private def field(ui: UI)(using Frame): UI.Ast.Textarea < Sync =
        elements(ui).map(_.collectFirst { case t: UI.Ast.Textarea => t }.getOrElse(throw new AssertionError("no textarea")))

    "a value bound one way is shown, an edit is reported without being written, and the field follows the owner" in {
        for
            held <- Signal.initRef("a")
            seen <- Signal.initRef(List.empty[String])
            ui = uic.TextArea().value(held.readOnly).onInput(v => seen.updateAndGet(_ :+ v).unit).render
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

    "a signal nobody hears says so; a constant, onInput, onChange, a ref, readonly or disabled keep it quiet" in {
        def errors(c: uic.TextArea): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held     <- Signal.initRef("a")
            deaf     <- errors(uic.TextArea().value(held.readOnly))
            constant <- errors(uic.TextArea().value("a"))
            input    <- errors(uic.TextArea().value(held.readOnly).onInput(_ => ()))
            change   <- errors(uic.TextArea().value(held.readOnly).onChange(_ => ()))
            twoWay   <- errors(uic.TextArea().value(held))
            ro       <- errors(uic.TextArea().value(held.readOnly).readonly(true))
            off      <- errors(uic.TextArea().value(held.readOnly).disabled(true))
        yield
            assert(deaf.size == 1 && textOf(deaf.head).contains("onInput or onChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(
                constant.isEmpty && input.isEmpty && change.isEmpty && twoWay.isEmpty && ro.isEmpty && off.isEmpty,
                "the text stays, somebody hears, or nobody can edit"
            )
        end for
    }

end TextAreaTest
