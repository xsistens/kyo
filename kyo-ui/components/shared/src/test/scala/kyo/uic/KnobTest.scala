package kyo.uic

import kyo.*

/** The `value` slot: a `SignalRef` binds two-way, any other signal one way, and either can be turned by the pointer as
  * well as the keys. A one-way signal nobody hears says so.
  */
class KnobTest extends UicTest:

    /** The top of a 100 by 100 dial: the middle of a 0 to 100 range. */
    private val top = UI.PointerEvent(50, 0, 0, 0, 100, 100, 1, Absent)

    "a value bound one way is turned by the pointer, and the turn is reported without being written" in {
        for
            held <- Signal.initRef(10.0)
            seen <- Signal.initRef(List.empty[Double])
            root <- elementWithClass(uic.Knob().value(held.readOnly).onChange(v => seen.updateAndGet(_ :+ v).unit).render, "p-knob")
            _    <- handled(root.attrs.onPointerDown.getOrElse(throw new AssertionError("the dial takes no pointer"))(top))
            h    <- held.get
            s    <- seen.get
        yield assert(s == List(50.0) && h == 10.0, s"reported $s, held $h")
    }

    "a value bound one way that nothing hears says so; onChange, a ref, a constant or disabled keep it quiet" in {
        def errors(k: uic.Knob): Chunk[UI.Ast.Element] < Sync = elementsWithClass(k.render, "p-uic-key-error")
        for
            held     <- Signal.initRef(10.0)
            deaf     <- errors(uic.Knob().value(held.readOnly))
            change   <- errors(uic.Knob().value(held.readOnly).onChange(_ => ()))
            twoWay   <- errors(uic.Knob().value(held))
            constant <- errors(uic.Knob().value(10.0))
            off      <- errors(uic.Knob().value(held.readOnly).disabled(true))
        yield
            assert(deaf.size == 1 && textOf(deaf.head).contains("onChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(change.isEmpty && twoWay.isEmpty && constant.isEmpty && off.isEmpty, "somebody hears, or nobody can turn it")
        end for
    }

end KnobTest
