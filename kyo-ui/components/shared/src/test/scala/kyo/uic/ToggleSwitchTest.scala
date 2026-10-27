package kyo.uic

import kyo.*

/** The `checked` slot: a `SignalRef` binds two-way, any other signal or a constant one way, and a one-way binding nobody
  * hears says so.
  */
class ToggleSwitchTest extends UicTest:

    /** Whether a field's binding is the constant `v`, which is how a one-way value reaches the element. */
    private def shows[V](bound: Maybe[UI.Bound[V]], v: V)(using CanEqual[V, V]): Boolean =
        bound.exists { case UI.Bound.Const(c) => c == v; case _ => false }

    private def box(ui: UI)(using Frame): UI.Ast.Checkbox < Sync =
        elements(ui).map(_.collectFirst { case c: UI.Ast.Checkbox => c }.getOrElse(throw new AssertionError("no switch input")))

    "a state bound one way is shown, and a toggle is reported without being written" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef(List.empty[Boolean])
            b    <- box(uic.ToggleSwitch().checked(held.readOnly).onChange(v => seen.updateAndGet(_ :+ v).unit).render)
            _    <- handled(b.onChange.getOrElse(throw new AssertionError("no toggle wired"))(false))
            h    <- held.get
            s    <- seen.get
        yield
            assert(shows(b.checked, true), s"shows the signal: ${b.checked}")
            assert(s == List(false) && h, s"reported $s, held $h")
    }

    "a state bound one way that nothing hears says so; onChange, a ref, readonly or disabled keep it quiet" in {
        def errors(c: uic.ToggleSwitch): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef(false)
            deaf   <- errors(uic.ToggleSwitch().checked(held.readOnly))
            heard  <- errors(uic.ToggleSwitch().checked(held.readOnly).onChange(_ => ()))
            twoWay <- errors(uic.ToggleSwitch().checked(held))
            ro     <- errors(uic.ToggleSwitch().checked(held.readOnly).readonly(true))
            off    <- errors(uic.ToggleSwitch().checked(held.readOnly).disabled(true))
        yield
            assert(deaf.size == 1 && textOf(deaf.head).contains("onChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(heard.isEmpty && twoWay.isEmpty && ro.isEmpty && off.isEmpty, "somebody hears, or nobody can toggle")
        end for
    }

end ToggleSwitchTest
