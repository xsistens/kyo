package kyo.uic

import kyo.*
import kyo.UI.*

class DiagnosticsTest extends UicTest:

    private val cls = "p-uic-key-error"

    private def texts(cs: Chunk[UI])(using Frame): Chunk[String] < Sync =
        elementsWithClass(fragment(cs*), cls).map(_.map(textOf))

    "cards" - {
        "nothing fires, no card" in {
            assert(Diagnostics.cards("X")(Diagnostic(false, "a"), Diagnostic.naming("b", Nil)).isEmpty)
        }

        "the order of the cards is the order of the diagnoses" in {
            texts(Diagnostics.cards("X")(Diagnostic(true, "a"), Diagnostic(false, "b"), Diagnostic(true, "c")))
                .map(got => assert(got == Chunk("X: a", "X: c")))
        }

        "keys are named, cut at five with an ellipsis" in {
            for
                five <- texts(Diagnostics.cards("X")(Diagnostic.naming("a", Seq("1", "2", "3", "4", "5"))))
                six  <- texts(Diagnostics.cards("X")(Diagnostic.naming("a", Seq("1", "2", "3", "4", "5", "6"))))
            yield
                assert(five == Chunk("X: a (1, 2, 3, 4, 5)"))
                assert(six == Chunk("X: a (1, 2, 3, 4, 5, ...)"))
        }

        "a card is an alert" in {
            elementWithClass(fragment(Diagnostics.cards("X")(Diagnostic(true, "a"))*), cls)
                .map(e => assert(e.attrs.role.contains("alert")))
        }
    }

    "before and after" - {
        val ui = div("body")

        "nothing fires: the very ui back, no fragment, so the bytes stay" in {
            assert(Diagnostics.before(ui)("X")(Diagnostic(false, "a")) eq ui)
            assert(Diagnostics.after(ui)("X")(Diagnostic(false, "a")) eq ui)
            assert(Diagnostics.beforeWhile(ui)(ReactiveValue.Const(false))("X")(b => Seq(Diagnostic(b, "a"))) eq ui)
        }

        "before puts the card in front, after behind" in {
            def order(t: UI): String = t match
                case f: UI.Ast.Fragment[?] =>
                    f.children.map {
                        case e: UI.Ast.Element if e.attrs.cssClasses.contains(cls) => "card"
                        case _                                                     => "ui"
                    }.mkString(",")
                case other => throw new AssertionError(s"not a fragment: $other")
            assert(order(Diagnostics.before(ui)("X")(Diagnostic(true, "a"))) == "card,ui")
            assert(order(Diagnostics.after(ui)("X")(Diagnostic(true, "a"))) == "ui,card")
        }
    }

    "cardsWhile" - {
        "over a constant asks once and draws no region" in {
            for
                on  <- reactiveNodes(fragment(Diagnostics.cardsWhile(ReactiveValue.Const(true))("X")(b => Seq(Diagnostic(b, "a")))*))
                off <- reactiveNodes(fragment(Diagnostics.cardsWhile(ReactiveValue.Const(false))("X")(b => Seq(Diagnostic(b, "a")))*))
            yield
                assert(on.isEmpty)
                assert(off.isEmpty)
        }

        "over a signal the card follows it, in a region of its own" in {
            for
                sw <- Signal.initRef(true)
                cs = Diagnostics.cardsWhile(ReactiveValue.Dyn(sw))("X")(b => Seq(Diagnostic(b, "a")))
                on   <- elementsWithClass(fragment(cs*), cls)
                _    <- sw.set(false)
                off  <- elementsWithClass(fragment(cs*), cls)
                _    <- sw.set(true)
                back <- elementsWithClass(fragment(cs*), cls)
                held <- reactiveNodes(fragment(cs*))
            yield
                assert(on.size == 1)
                assert(off.isEmpty)
                assert(back.size == 1)
                assert(held.size == 1)
        }
    }

    "the diagnoses" - {
        val name = Slot.Name("X", "the thing", "a press", "onThing")

        "the template names the subject, the gesture and the event" in {
            assert(name.advice ==
                "the thing is bound one way and nothing hears a press, so the reader cannot change it; add onThing, or bind a SignalRef")
        }

        "a consequence of its own replaces the one a reader cannot change" in {
            assert(name.copy(consequence = "the commit reaches nobody").advice.contains("so the commit reaches nobody;"))
        }

        "unheard fires for a signal nobody hears, while the switch is on, and only then" in {
            val deaf = Slot[Int]().bind(Signal.initConst(1))
            assert(Diagnostic.unheard(deaf, name).when)
            assert(!Diagnostic.unheard(deaf, name, on = false).when)
            assert(!Diagnostic.unheard(deaf.heardToo, name).when)
            assert(!Diagnostic.unheard(deaf.quieted, name).when)
            assert(!Diagnostic.unheard(Slot[Int]().bind(1), name).when)
        }

        "naming fires when there is something to name" in {
            assert(!Diagnostic.naming("a", Nil).when)
            assert(Diagnostic.naming("a", Seq("k")).when)
        }

        "unknown names the values no option carries" in {
            assert(!Diagnostic.unknown(Nil).when)
            assert(Diagnostic.unknown(Seq("v")).keys == Seq("v"))
        }
    }

    "duplicates" - {
        "in first-seen order, over a key that is no String" in {
            assert(Diagnostics.duplicates(Seq(3, 1, 3, 2, 1, 4, 2)) == Seq(3, 1, 2))
        }

        "nothing repeated, nothing named" in {
            assert(Diagnostics.duplicates(Seq("a", "b")).isEmpty)
        }
    }
end DiagnosticsTest
