package kyo.uic

import kyo.*

/** The `value` slot holds to rule A ([[ReactiveValueContract]]) through each of the two ways a reader turns the knob: the
  * pointer on the outer box, which asks for the value under it whatever the knob held, and the keys on the dial, which
  * step from it. Either way the dial is what says the knob is fixed (`aria-readonly` on its `slider` role), since the box
  * the pointer lands on has no role to say it in.
  */
class KnobTest extends ReactiveValueContract:

    /** The top of a 100 by 100 dial: the middle of a 0 to 100 range. */
    private val top = UI.PointerEvent(50, 0, 0, 0, 100, 100, 1, Absent)

    private def dial(t: UI)(using Frame): UI.Ast.Element < Sync = elementWithClass(t, "p-uic-knob-dial")

    /** The value the dial announces, which its arcs and label are drawn from. */
    private def announced(t: UI)(using Frame): Double < Sync =
        dial(t).map(d =>
            d.attrs.ariaAttrs.get("valuenow").flatMap(_.toDoubleOption)
                .getOrElse(throw new AssertionError(s"the dial announces no value: ${d.attrs.ariaAttrs}"))
        )

    private def readonlyDial(d: UI.Ast.Element): Boolean = d.attrs.ariaAttrs.get("readonly").contains("true")

    holds(new Slot[uic.Knob, Double](
        name = "value turned by the pointer",
        bare = Present(uic.Knob()),
        bind = uic.Knob().value(_),
        hear = (c, f) => c.onChange(f),
        event = "onChange",
        render = _.render,
        start = 0.0,
        next = 50.0,
        control = elementWithClass(_, "p-knob"),
        shows = announced(_),
        // A pointer asks for the value at the angle it lands on, here the top of the dial.
        gesture = (c: UI.Ast.Element) =>
            handled(c.attrs.onPointerDown.getOrElse(throw new AssertionError("the box takes no pointer"))(top)),
        answers = _.attrs.onPointerDown.nonEmpty,
        fixedOn = Present(dial(_)),
        saysFixed = readonlyDial
    ))

    holds(new Slot[uic.Knob, Double](
        name = "value stepped by the keys",
        bare = Present(uic.Knob()),
        bind = uic.Knob().value(_),
        hear = (c, f) => c.onChange(f),
        event = "onChange",
        render = _.render,
        start = 0.0,
        next = 1.0,
        control = dial(_),
        shows = announced(_),
        gesture = (c: UI.Ast.Element) => press(c, UI.Keyboard.ArrowUp),
        answers = _.attrs.onKeyDown.nonEmpty,
        saysFixed = readonlyDial
    ))

    "a value bound one way says nothing where it reaches somebody or nobody can turn it: a ref, disabled" in {
        def errors(k: uic.Knob): Chunk[UI.Ast.Element] < Sync = elementsWithClass(k.render, "p-uic-key-error")
        for
            held   <- Signal.initRef(10.0)
            twoWay <- errors(uic.Knob().value(held))
            off    <- errors(uic.Knob().value(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && off.isEmpty, "somebody hears, or nobody can turn it")
        end for
    }

end KnobTest
