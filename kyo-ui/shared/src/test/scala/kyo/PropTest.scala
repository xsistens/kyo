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

    "current" - {
        "a constant reads as itself" in {
            val n: Prop[Int] = 7
            Prop.current(n).map(v => assert(v == 7))
        }

        "a signal reads its value at the moment of the read" in {
            for
                ref <- Signal.initRef(1)
                n: Prop[Int] = ref.map(_ * 10)
                before <- Prop.current(n)
                _      <- ref.set(2)
                after  <- Prop.current(n)
            yield assert((before, after) == (10, 20))
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

    "an aria attribute" - {
        "a constant prop is a plain attribute, a signal prop a channel" in {
            Signal.initRef("live").map { ref =>
                val c: Prop[String] = "fixed"
                val s: Prop[String] = ref
                val a               = span.aria(AriaKey.label -> c)
                val b               = span.aria(AriaKey.label -> s)
                assert(
                    (a.attrs.ariaAttrs.get("label"), a.attrs.reactiveAttrs.contains("aria-label")) == (Some("fixed"), false) &&
                        (b.attrs.ariaAttrs.get("label"), b.attrs.reactiveAttrs.contains("aria-label")) == (None, true),
                    s"${a.attrs} ${b.attrs}"
                )
            }
        }

        "a plain String and Signal[String] still take their own overloads" in {
            Signal.initRef("live").map { ref =>
                val a = span.aria(AriaKey.label -> "fixed")
                val b = span.aria(AriaKey.label -> ref)
                assert(a.attrs.ariaAttrs.get("label") == Some("fixed") && b.attrs.reactiveAttrs.contains("aria-label"))
            }
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
    "the element type comes from the argument, whatever its static type" - {
        "a union whose alias a pattern lost reads as its element" in {
            val held: Maybe[Prop[Boolean]] = Present(true)
            val read: Prop[Maybe[Boolean]] = held match
                case Present(on) => Prop.map(on)(Present(_))
                case _           => Absent
            val union: Boolean | Signal[Boolean] = false
            val flipped                          = Prop.map(union)(!_)
            val constants                        = !read.isInstanceOf[Signal[?]] && !flipped.isInstanceOf[Signal[?]]
            assert(constants, s"$read, $flipped")
            assert((read.asInstanceOf[Maybe[Boolean]], flipped.asInstanceOf[Boolean]) == (Present(true), true))
        }

        "a bare signal and a ref read as their element, not as the signal" in {
            for
                ref <- Signal.initRef(2)
                sig     = ref.map(_ + 1)
                doubled = Prop.map(sig)(_ * 10).asInstanceOf[Signal[Int]]
                tripled = Prop.map(ref)(_ * 3).asInstanceOf[Signal[Int]]
                both    = Prop.zip(sig, ref).asInstanceOf[Signal[(Int, Int)]]
                d <- doubled.current
                t <- tripled.current
                b <- both.current
            yield assert((d, t, b, Prop.writable(ref).isDefined) == (30, 6, (3, 2), true))
        }
    }

end PropTest
