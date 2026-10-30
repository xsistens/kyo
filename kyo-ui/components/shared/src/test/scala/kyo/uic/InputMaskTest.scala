package kyo.uic

import kyo.*

/** The `value` slot holds to rule A ([[ReactiveValueContract]]) as a text field does: a constant is the starting text,
  * and unbound the text is the browser's. A commit reaches `onChange`, and a complete one `onComplete` as well, so the
  * slot is held through each. The cases after them are what only this field does: its card speaks in InputMask's own
  * words, since its field is an inner Input whose events it does not expose, and `onComplete` waits for a full mask.
  */
class InputMaskTest extends ReactiveValueContract:

    private val valueSlot = nativeText[uic.InputMask](
        name = "value",
        bare = Present(uic.InputMask("99")),
        bind = uic.InputMask("99").value(_),
        hear = (c, f) => c.onChange(f),
        event = "onChange or onComplete",
        render = _.render,
        field = "p-inputmask",
        next = "12",
        startText = "1",
        commit = true
    )

    holds(valueSlot)
    blurs(valueSlot, (c, f) => c.onBlur(f))

    heardBy(nativeText[uic.InputMask](
        name = "value heard by onComplete",
        bare = Present(uic.InputMask("99")),
        bind = uic.InputMask("99").value(_),
        hear = (c, f) => c.onComplete(f),
        event = "onChange or onComplete",
        render = _.render,
        field = "p-inputmask",
        next = "12",
        startText = "1",
        commit = true
    ))

    private def field(ui: UI)(using Frame): (UI.Ast.Element & UI.Ast.TextInput) < Sync =
        elements(ui).map(_.collectFirst { case t: (UI.Ast.Element & UI.Ast.TextInput) => t }
            .getOrElse(throw new AssertionError("no masked field")))

    // The field is an Input, so it carries the Input's marker; a starting text the mask shows counts.
    "the field marks itself filled for the client, and the static render carries p-filled from a starting text" in {
        for
            started <- renderHtml(uic.InputMask("99").value("12").render)
            empty   <- renderHtml(uic.InputMask("99").value("").render)
            bare    <- renderHtml(uic.InputMask("99").render)
        yield
            assert(classInHtml(started, "p-filled") && started.contains("""data-kyo-filled="p-filled""""), started)
            Seq(empty, bare).foreach(h => assert(!classInHtml(h, "p-filled") && h.contains("""data-kyo-filled="p-filled""""), h))
        end for
    }

    "a value bound one way follows its owner" in {
        for
            held <- Signal.initRef("1")
            ui = uic.InputMask("99").value(held.readOnly).onChange(_ => ()).render
            first <- field(ui).map(textIn)
            _     <- held.set("34")
            again <- field(ui).map(textIn)
        yield assert(first == "1" && again == "34", s"shows $first, then $again")
    }

    "a commit that leaves a slot of the mask empty reaches onChange and not onComplete" in {
        for
            seen <- Signal.initRef(List.empty[String])
            done <- Signal.initRef(List.empty[String])
            ui = uic.InputMask("99").onChange(v => seen.updateAndGet(_ :+ v).unit).onComplete(v => done.updateAndGet(_ :+ v).unit).render
            f <- field(ui)
            _ <- commitInto(f, "1")
            s <- seen.get
            d <- done.get
        yield assert(s == List("1") && d.isEmpty, s"changed $s, completed $d")
    }

    "the card for a signal nobody hears names InputMask, not the Input inside it" in {
        for
            held <- Signal.initRef("1")
            deaf <- elementsWithClass(uic.InputMask("99").value(held.readOnly).render, "p-uic-key-error")
        yield
            val text = deaf.map(textOf).mkString
            assert(deaf.size == 1 && text.startsWith("InputMask:"), s"nobody hears: $text")
    }

    "a signal nobody hears says nothing where it reaches somebody or nobody can edit it: a ref, readonly, disabled" in {
        def errors(c: uic.InputMask): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef("1")
            twoWay <- errors(uic.InputMask("99").value(held))
            ro     <- errors(uic.InputMask("99").value(held.readOnly).readonly(true))
            off    <- errors(uic.InputMask("99").value(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can edit")
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
