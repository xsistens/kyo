package kyo.uic

import kyo.*

/** The `value` slot: a `SignalRef` binds two-way, any other signal one way, and a one-way signal nobody hears says so in
  * InputMask's own words, since its field is an inner Input whose events it does not expose.
  */
class InputMaskTest extends UicTest:

    /** Whether a field's binding is the constant `v`, which is how a one-way value reaches the element. */
    private def shows[V](bound: Maybe[UI.Bound[V]], v: V)(using CanEqual[V, V]): Boolean =
        bound.exists { case UI.Bound.Const(c) => c == v; case _ => false }

    private def field(ui: UI)(using Frame): (UI.Ast.Element & UI.Ast.TextInput) < Sync =
        elements(ui).map(_.collectFirst { case t: (UI.Ast.Element & UI.Ast.TextInput) => t }
            .getOrElse(throw new AssertionError("no masked field")))

    "a value bound one way is shown, and a commit is reported to onChange and onComplete without being written" in {
        for
            held <- Signal.initRef("1")
            seen <- Signal.initRef(List.empty[String])
            done <- Signal.initRef(List.empty[String])
            ui = uic.InputMask("99").value(held.readOnly)
                .onChange(v => seen.updateAndGet(_ :+ v).unit)
                .onComplete(v => done.updateAndGet(_ :+ v).unit)
                .render
            f <- field(ui)
            _ <- handled(f.onChange.getOrElse(throw new AssertionError("no change handler"))("12"))
            h <- held.get
            s <- seen.get
            d <- done.get
        yield
            assert(shows(f.value, "1"), s"shows the signal: ${f.value}")
            assert(s == List("12") && d == List("12") && h == "1", s"reported $s and $d, held $h")
    }

    "a signal nobody hears says so; a constant, onChange, onComplete, a ref, readonly or disabled keep it quiet" in {
        def errors(c: uic.InputMask): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held     <- Signal.initRef("1")
            deaf     <- errors(uic.InputMask("99").value(held.readOnly))
            constant <- errors(uic.InputMask("99").value("1"))
            change   <- errors(uic.InputMask("99").value(held.readOnly).onChange(_ => ()))
            complete <- errors(uic.InputMask("99").value(held.readOnly).onComplete(_ => ()))
            twoWay   <- errors(uic.InputMask("99").value(held))
            ro       <- errors(uic.InputMask("99").value(held.readOnly).readonly(true))
            off      <- errors(uic.InputMask("99").value(held.readOnly).disabled(true))
        yield
            val text = deaf.map(textOf).mkString
            assert(
                deaf.size == 1 && text.startsWith("InputMask:") && text.contains("onChange or onComplete"),
                s"nobody hears: $text"
            )
            assert(
                constant.isEmpty && change.isEmpty && complete.isEmpty && twoWay.isEmpty && ro.isEmpty && off.isEmpty,
                "the text stays, somebody hears, or nobody can edit"
            )
        end for
    }

    "a commit its owner does not take is painted away again" in {
        for
            held  <- Signal.initRef("1")
            again <- repaintsAfter(uic.InputMask("99").value(held.readOnly).onChange(_ => ()).render, held)(t =>
                field(t).map(f => handled(f.onChange.getOrElse(throw new AssertionError("no change handler"))("12")))
            )
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

end InputMaskTest
