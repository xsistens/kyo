package kyo.uic

import kyo.*

/** The `value` slot: a `SignalRef` binds two-way, any other signal one way (the strength meter follows it too), and a
  * one-way signal nobody hears says so.
  */
class PasswordTest extends UicTest:

    /** Whether a field's binding is the constant `v`, which is how a one-way value reaches the element. */
    private def shows[V](bound: Maybe[UI.Bound[V]], v: V)(using CanEqual[V, V]): Boolean =
        bound.exists { case UI.Bound.Const(c) => c == v; case _ => false }

    private def field(ui: UI)(using Frame): (UI.Ast.Element & UI.Ast.TextInput) < Sync =
        elements(ui).map(_.collectFirst {
            case t: (UI.Ast.Element & UI.Ast.TextInput) if t.attrs.cssClasses.contains("p-password-input") => t
        }.getOrElse(throw new AssertionError("no password field")))

    "a value bound one way is shown with its strength, and an edit is reported without being written" in {
        for
            held <- Signal.initRef("abcDEF12")
            seen <- Signal.initRef(List.empty[String])
            ui = uic.Password().feedback(true).value(held.readOnly).onInput(v => seen.updateAndGet(_ :+ v).unit).render
            f     <- field(ui)
            meter <- elementsWithClass(ui, "p-password-meter-strong")
            _     <- handled(f.onInput.getOrElse(throw new AssertionError("no input handler"))("x"))
            h     <- held.get
            s     <- seen.get
        yield
            assert(shows(f.value, "abcDEF12"), s"shows the signal: ${f.value}")
            assert(meter.size == 1, "and the meter judges what it shows")
            assert(s == List("x") && h == "abcDEF12", s"reported $s, held $h")
    }

    "a signal nobody hears says so; a constant, onInput, onChange, a ref, readonly or disabled keep it quiet" in {
        def errors(c: uic.Password): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held     <- Signal.initRef("a")
            deaf     <- errors(uic.Password().value(held.readOnly))
            constant <- errors(uic.Password().value("a"))
            input    <- errors(uic.Password().value(held.readOnly).onInput(_ => ()))
            change   <- errors(uic.Password().value(held.readOnly).onChange(_ => ()))
            twoWay   <- errors(uic.Password().value(held))
            ro       <- errors(uic.Password().value(held.readOnly).readonly(true))
            off      <- errors(uic.Password().value(held.readOnly).disabled(true))
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
            again <- repaintsAfter(uic.Password().value(held.readOnly).onInput(_ => ()).render, held)(t =>
                field(t).map(f => handled(f.onInput.getOrElse(throw new AssertionError("no input handler"))("ab")))
            )
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

end PasswordTest
