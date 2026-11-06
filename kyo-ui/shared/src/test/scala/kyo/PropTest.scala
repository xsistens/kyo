package kyo

import kyo.UI.*
import kyo.UI.Ast.*

/** A prop keeps a constant a constant and gives a signal one region, and a child takes it directly. */
class PropTest extends UITest:

    private def isRegion(ui: UI): Boolean = ui.isInstanceOf[Reactive[?]]

    "a setter-shaped value takes a constant and a signal with no wrapper" in {
        Signal.initRef("live").map { ref =>
            val c: Prop[String] = ""
            val s: Prop[String] = ref
            assert((c.isInstanceOf[Signal[?]], s.isInstanceOf[Signal[?]]) == (false, true))
        }
    }

    "render" - {
        "a constant draws the node itself, with no region" in {
            val on: Prop[Boolean] = true
            val ui                = Prop.render(on)(b => if b then span("on") else UI.empty)
            assert(!isRegion(ui) && ui.isInstanceOf[SpanElement], s"$ui")
        }

        "a signal draws one region that compares the value it was drawn from" in {
            Signal.initRef(true).map { ref =>
                val on: Prop[Boolean] = ref
                Prop.render(on)(b => if b then span("on") else UI.empty) match
                    case r: Reactive[?] => assert(r.source.exists(_.isInstanceOf[Reactive.Source.Projected[?]]), s"${r.source}")
                    case other          => fail(s"no region: $other")
            }
        }
    }

    "map" - {
        "a constant gives a constant" in {
            val n: Prop[Int] = 2
            val m            = Prop.map(n)(_ * 10)
            assert(!m.isInstanceOf[Signal[?]] && m.asInstanceOf[Int] == 20)
        }

        "a signal gives a signal that follows it" in {
            Signal.initRef(2).map { ref =>
                val n: Prop[Int] = ref
                val m            = Prop.map(n)(_ * 10).asInstanceOf[Signal[Int]]
                for
                    before <- m.current
                    _      <- ref.set(3)
                    after  <- m.current
                yield assert((before, after) == (20, 30))
                end for
            }
        }
    }

    "zip" - {
        "two constants stay a constant" in {
            val a: Prop[Boolean] = true
            val b: Prop[Boolean] = false
            val z                = Prop.zip(a, b)
            assert(!z.isInstanceOf[Signal[?]] && z.asInstanceOf[(Boolean, Boolean)] == (true, false))
        }

        "a constant and a signal give one signal of both" in {
            Signal.initRef(false).map { ref =>
                val a: Prop[Boolean] = true
                val b: Prop[Boolean] = ref
                val z                = Prop.zip(a, b).asInstanceOf[Signal[(Boolean, Boolean)]]
                for
                    before <- z.current
                    _      <- ref.set(true)
                    after  <- z.current
                yield assert((before, after) == ((true, false), (true, true)))
                end for
            }
        }
    }

    "writable is the ref of a two-way binding and nothing else" in {
        Signal.initRef(1).map { ref =>
            val two: Prop[Int]   = ref
            val one: Prop[Int]   = ref.readOnly
            val fixed: Prop[Int] = 1
            assert((Prop.writable(two).isDefined, Prop.writable(one).isDefined, Prop.writable(fixed).isDefined) == (true, false, false))
        }
    }

    "a child" - {
        "a text prop: a constant is a text node, a signal a region" in {
            Signal.initRef("live").map { ref =>
                val c: Prop[String] = "fixed"
                val s: Prop[String] = ref
                val kids            = div(c, s).children
                assert((kids(0).isInstanceOf[Text], isRegion(kids(1))) == (true, true), s"$kids")
            }
        }

        "an element prop: a constant is the element, a signal a region" in {
            Signal.initRef(span("a")).map { ref =>
                val c: Prop[SpanElement] = span("fixed")
                val s: Prop[SpanElement] = ref
                val kids                 = div(c, s).children
                assert((kids(0).isInstanceOf[SpanElement], isRegion(kids(1))) == (true, true), s"$kids")
            }
        }

        "a plain String and Signal[String] still take their own conversions" in {
            Signal.initRef("live").map { ref =>
                val kids = div("fixed", ref).children
                assert((kids(0).isInstanceOf[Text], isRegion(kids(1))) == (true, true), s"$kids")
            }
        }
    }
end PropTest
