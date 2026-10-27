package kyo.uic

import kyo.*

/** The `checked` slot: a `SignalRef` binds two-way, any other signal or a constant one way, and a one-way binding nobody
  * hears says so.
  */
class RadioButtonTest extends UicTest:

    /** Whether a field's binding is the constant `v`, which is how a one-way value reaches the element. */
    private def shows[V](bound: Maybe[UI.Bound[V]], v: V)(using CanEqual[V, V]): Boolean =
        bound.exists { case UI.Bound.Const(c) => c == v; case _ => false }

    private def radio(ui: UI)(using Frame): UI.Ast.Radio < Sync =
        elements(ui).map(_.collectFirst { case r: UI.Ast.Radio => r }.getOrElse(throw new AssertionError("no radio")))

    "a state bound one way is shown, and a selection is reported without being written" in {
        for
            held <- Signal.initRef(false)
            seen <- Signal.initRef(List.empty[Boolean])
            r    <- radio(uic.RadioButton().checked(held.readOnly).onChange(v => seen.updateAndGet(_ :+ v).unit).render)
            _    <- handled(r.onChange.getOrElse(throw new AssertionError("no handler wired"))(true))
            h    <- held.get
            s    <- seen.get
        yield
            assert(shows(r.checked, false), s"shows the signal: ${r.checked}")
            assert(s == List(true) && !h, s"reported $s, held $h")
    }

    "a state bound one way that nothing hears says so; onChange, a ref, readonly or disabled keep it quiet" in {
        def errors(c: uic.RadioButton): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef(false)
            deaf   <- errors(uic.RadioButton().checked(held.readOnly))
            heard  <- errors(uic.RadioButton().checked(held.readOnly).onChange(_ => ()))
            twoWay <- errors(uic.RadioButton().checked(held))
            ro     <- errors(uic.RadioButton().checked(held.readOnly).readonly(true))
            off    <- errors(uic.RadioButton().checked(held.readOnly).disabled(true))
        yield
            assert(deaf.size == 1 && textOf(deaf.head).contains("onChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(heard.isEmpty && twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can select")
        end for
    }

end RadioButtonTest
