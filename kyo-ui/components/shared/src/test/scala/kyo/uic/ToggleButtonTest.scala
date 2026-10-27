package kyo.uic

import kyo.*

/** The `checked` slot: a `SignalRef` binds two-way, any other signal or a constant one way, and a one-way binding nobody
  * hears says so.
  */
class ToggleButtonTest extends UicTest:

    "a state bound one way is shown, and a click is reported without being written" in {
        for
            held <- Signal.initRef(false)
            seen <- Signal.initRef(List.empty[Boolean])
            ui = uic.ToggleButton().checked(held.readOnly).onChange(v => seen.updateAndGet(_ :+ v).unit).render
            el <- elementWithClass(ui, "p-togglebutton")
            _  <- click(el)
            h  <- held.get
            s  <- seen.get
        yield
            assert(el.attrs.ariaAttrs.get("pressed").contains("false"), s"shows the signal: ${el.attrs.ariaAttrs}")
            assert(s == List(true) && !h, s"reported $s, held $h")
    }

    "a state bound one way that nothing hears says so; onChange, a ref, readonly or disabled keep it quiet" in {
        def errors(c: uic.ToggleButton): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef(false)
            deaf   <- errors(uic.ToggleButton().checked(held.readOnly))
            heard  <- errors(uic.ToggleButton().checked(held.readOnly).onChange(_ => ()))
            twoWay <- errors(uic.ToggleButton().checked(held))
            ro     <- errors(uic.ToggleButton().checked(held.readOnly).readonly(true))
            off    <- errors(uic.ToggleButton().checked(held.readOnly).disabled(true))
        yield
            assert(deaf.size == 1 && textOf(deaf.head).contains("onChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(heard.isEmpty && twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can toggle")
        end for
    }

end ToggleButtonTest
