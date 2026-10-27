package kyo.uic

import kyo.*

/** The `value` slot: a `SignalRef` binds two-way, any other signal one way, and a one-way signal nobody hears says so. */
class InputOtpTest extends UicTest:

    private def cells(ui: UI)(using Frame): Chunk[UI.Ast.Element & UI.Ast.TextInput] < Sync =
        elements(ui).map(_.collect {
            case t: (UI.Ast.Element & UI.Ast.TextInput) if t.attrs.cssClasses.contains("p-inputotp-input") => t
        })

    "a code bound one way is shown, and a cell edit reports the code it asks for without writing it" in {
        for
            held <- Signal.initRef("1")
            seen <- Signal.initRef(List.empty[String])
            ui = uic.InputOtp().length(3).value(held.readOnly).onChange(v => seen.updateAndGet(_ :+ v).unit).render
            cs <- cells(ui)
            _  <- handled(cs(1).onInput.getOrElse(throw new AssertionError("no input handler"))("7"))
            h  <- held.get
            s  <- seen.get
        yield
            val shown = cs.map(_.value.fold("-") { case UI.Bound.Const(c) => c; case _ => "ref" })
            assert(shown == Chunk("1", "", ""), s"shows the signal: $shown")
            assert(s == List("17") && h == "1", s"reported $s, held $h")
    }

    "a code a ref holds is written before onChange hears it" in {
        for
            held <- Signal.initRef("1")
            seen <- Signal.initRef(("", ""))
            ui = uic.InputOtp().length(3).value(held).onChange(v => held.get.map(h => seen.set((v, h)))).render
            cs <- cells(ui)
            _  <- handled(cs(1).onInput.getOrElse(throw new AssertionError("no input handler"))("7"))
            s  <- seen.get
        yield assert(s == ("17", "17"), s"reported, held: $s")
    }

    "a signal nobody hears says so; a constant, onChange, a ref, readonly or disabled keep it quiet" in {
        def errors(c: uic.InputOtp): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held     <- Signal.initRef("1")
            deaf     <- errors(uic.InputOtp().value(held.readOnly))
            constant <- errors(uic.InputOtp().value("1"))
            change   <- errors(uic.InputOtp().value(held.readOnly).onChange(_ => ()))
            twoWay   <- errors(uic.InputOtp().value(held))
            ro       <- errors(uic.InputOtp().value(held.readOnly).readonly(true))
            off      <- errors(uic.InputOtp().value(held.readOnly).disabled(true))
        yield
            assert(deaf.size == 1 && textOf(deaf.head).contains("onChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(
                constant.isEmpty && change.isEmpty && twoWay.isEmpty && ro.isEmpty && off.isEmpty,
                "the code stays, somebody hears, or nobody can edit"
            )
        end for
    }

    "a cell edit its owner does not take is painted away again" in {
        for
            held  <- Signal.initRef("1")
            again <- repaintsAfter(uic.InputOtp().length(3).value(held.readOnly).onChange(_ => ()).render, held)(t =>
                cells(t).map(cs => handled(cs(1).onInput.getOrElse(throw new AssertionError("no input handler"))("7")))
            )
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

end InputOtpTest
