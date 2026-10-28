package kyo.uic

import kyo.*

/** The `value` slot bound one way: the slider shows what its owner holds, and a drag its owner does not take is painted
  * away again, since the native range input has already moved under the reader's hand.
  */
class SliderTest extends UicTest:

    private def range(ui: UI)(using Frame): UI.Ast.RangeInput < Sync =
        elements(ui).map(_.collectFirst { case r: UI.Ast.RangeInput => r }.getOrElse(throw new AssertionError("no range input")))

    "a drag its owner does not take is painted away again" in {
        for
            held  <- Signal.initRef(20.0)
            again <- repaintsAfter(uic.Slider().value(held.readOnly).onChange(_ => ()).render, held)(t =>
                range(t).map(r => handled(r.onChange.getOrElse(throw new AssertionError("no change handler"))(60.0)))
            )
        yield assert(again, "the field kept what the reader did, though its owner never took it")
    }

    "a value bound one way that nothing hears says so; onChange, a ref or disabled keep it quiet" in {
        def errors(c: uic.Slider): Chunk[UI.Ast.Element] < Sync = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef(20.0)
            deaf   <- errors(uic.Slider().value(held.readOnly))
            change <- errors(uic.Slider().value(held.readOnly).onChange(_ => ()))
            twoWay <- errors(uic.Slider().value(held))
            off    <- errors(uic.Slider().value(held.readOnly).disabled(true))
        yield
            assert(deaf.size == 1 && textOf(deaf.head).contains("onChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(change.isEmpty && twoWay.isEmpty && off.isEmpty, "somebody hears, or nobody can change it")
        end for
    }

    "readonly keeps the value where it is: the native input is declined, says so, and reports nothing" in {
        for r <- range(uic.Slider().value(30.0).onChange(_ => ()).readonly(true).render)
        yield
            assert(r.attrs.dataAttrs.get("kyo-inert").contains("1"), "the browser's own move is declined")
            assert(r.attrs.ariaAttrs.get("readonly").contains("true"), "and it says it is read-only")
            assert(r.onChange.isEmpty, "with no handler behind it")
    }

    "a constant nobody hears is fixed: read-only, and no card" in {
        for
            r    <- range(uic.Slider().value(30.0).render)
            card <- elementsWithClass(uic.Slider().value(30.0).render, "p-uic-key-error")
        yield
            assert(r.attrs.ariaAttrs.get("readonly").contains("true") && r.onChange.isEmpty, "a fixed slider moves nothing")
            assert(card.isEmpty, s"a fixed constant: ${card.map(textOf)}")
    }

    "an unbound slider keeps where the reader puts it" in {
        def at(v: Double)(t: UI): Boolean < Sync =
            elements(t).map(_.exists {
                case r: UI.Ast.RangeInput => r.value.exists { case UI.Bound.Const(x) => x == v; case _ => false }
                case _                    => false
            })
        for
            after <- publishedAfterReady(uic.Slider().render, t => range(t).map(_.onChange.nonEmpty), until = at(60.0))(t =>
                range(t).map(r => handled(r.onChange.getOrElse(throw new AssertionError("no change handler"))(60.0)))
            )
        yield assert(after.nonEmpty, "the slider never showed where the reader put it")
    }

end SliderTest
