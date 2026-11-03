package kyo.uic

import kyo.*

/** The `value` slot holds to rule A ([[ReactiveValueContract]]) as a text field does, a constant being the code it
  * starts with. The code is built from several cells, so it is the component's to keep, not the browser's: unbound or
  * started from a constant, the code a cell edit reports is the whole code the cells hold. The cases after the contract
  * are what only this field does: the cells show the code one character each, and a cell edit its owner does not take
  * is painted away.
  */
class InputOtpTest extends ReactiveValueContract:

    private def cells(ui: UI)(using Frame): Chunk[UI.Ast.Element & UI.Ast.TextInput] < Sync =
        elements(ui).map(_.collect {
            case t: (UI.Ast.Element & UI.Ast.TextInput) if t.attrs.cssClasses.contains("p-inputotp-input") => t
        })

    /** The code the cells show: one character each, an empty cell a gap, trailing gaps dropped. */
    private def code(ui: UI)(using Frame): String < Sync =
        cells(ui).map(cs => Kyo.foreach(cs)(textIn)).map(_.map(c => if c.isEmpty then " " else c).mkString.stripTrailing)

    private def otp(using Frame) = uic.InputOtp().length(3)

    holds(new SlotCase[uic.InputOtp, String](
        name = "value",
        bare = Present(otp),
        bind = otp.value(_),
        hear = (c, f) => c.onChange(f),
        event = "onChange",
        render = _.render,
        start = "",
        next = "7",
        control = t => cells(t).map(_.headOption.getOrElse(throw new AssertionError("no cell"))),
        shows = code(_),
        gesture = (c: UI.Ast.Element) => typed(c, "7", commit = false),
        answers = editable,
        saysFixed = {
            case t: UI.Ast.TextInput => t.readOnly.contains(true)
            case _                   => false
        },
        // The mount mints an id for every cell, which it needs to move the focus on; the static render has none.
        ready = Present(t => cells(t).map(cs => cs.size == 3 && cs(1).attrs.identifier.isDefined)),
        startText = Present("12")
    ))

    /** What `ui` reported to `seen` after the reader typed `first` into cell 1 and then `second` into cell 2. */
    private def twoEdits(ui: SignalRef[List[String]] => UI, first: String, second: String)(using
        Frame,
        kyo.test.AssertScope
    ): List[String] < Async =
        for
            seen <- Signal.initRef(List.empty[String])
            _    <- publishedAfterEach(ui(seen))(
                t => cells(t).map(cs => typed(cs(1), first, commit = false)),
                t => cells(t).map(cs => typed(cs(2), second, commit = false))
            )
            s <- seen.get
        yield s

    "unbound, the second cell edit reports the code the first one left" in {
        for s <- twoEdits(seen => otp.onChange(v => seen.updateAndGet(_ :+ v).unit).render, "1", "2")
        yield assert(s == List(" 1", " 12"), s"reported $s")
    }

    "started from a constant, every cell edit reports the whole code the cells hold" in {
        for s <- twoEdits(seen => otp.value("5").onChange(v => seen.updateAndGet(_ :+ v).unit).render, "7", "8")
        yield assert(s == List("57", "578"), s"reported $s")
    }

    // The second keystroke reaches the cells the first one was typed into, before they were drawn again.
    "two keystrokes on one render: the second builds on the code the first one wrote" in {
        for
            held <- Signal.initRef("")
            ui = otp.value(held).render
            cs <- cells(ui)
            _  <- typed(cs(1), "7", commit = false)
            _  <- typed(cs(2), "8", commit = false)
            h  <- held.get
        yield assert(h == " 78", s"held '$h'")
    }

    "a cell edit bound one way reports the code built on what the owner holds, which it does not write" in {
        for
            held <- Signal.initRef("1")
            seen <- Signal.initRef(List.empty[String])
            ui = otp.value(held.readOnly).onChange(v => seen.updateAndGet(_ :+ v).unit).render
            cs <- cells(ui)
            _  <- typed(cs(1), "7", commit = false)
            h  <- held.get
            s  <- seen.get
        yield assert(s == List("17") && h == "1", s"reported $s, held $h")
    }

    "a code bound one way is shown one character a cell, and follows its owner" in {
        for
            held <- Signal.initRef("1")
            ui = otp.value(held.readOnly).onChange(_ => ()).render
            first <- cells(ui).map(cs => Kyo.foreach(cs)(textIn))
            _     <- held.set("42")
            again <- code(ui)
        yield assert(first == Chunk("1", "", "") && again == "42", s"shows $first, then $again")
    }

    "a signal nobody hears says nothing where it reaches somebody or nobody can edit it: a ref, readonly, disabled" in {
        def errors(c: uic.InputOtp): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-diagnostic")
        for
            held   <- Signal.initRef("1")
            twoWay <- errors(uic.InputOtp().value(held))
            ro     <- errors(uic.InputOtp().value(held.readOnly).readonly(true))
            off    <- errors(uic.InputOtp().value(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can edit")
        end for
    }

    "a cell edit its owner does not take is put back" in {
        for
            held    <- Signal.initRef("1")
            patches <- assertsAfter(
                otp.value(held.readOnly).onChange(_ => ()).render,
                mounted = true
            )("Please enter OTP character 2")(kyo.internal.UIEvent.Input(_, "7"))
        yield assert(patches == Chunk(("value", "")), s"patches: $patches")
        end for
    }

end InputOtpTest
