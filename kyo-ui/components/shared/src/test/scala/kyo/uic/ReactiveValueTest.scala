package kyo.uic

import kyo.*

/** The dispatch behind every `A | Signal[A]` setter in the module.
  *
  * One setter per slot carries all three bindings a value slot can hold, and which one a call
  * lands on is decided HERE, on the runtime class of the argument. That makes this the place the
  * module's whole setter surface rests on: a `SignalRef` reaching the `Dyn` branch would silently
  * downgrade every editable control (Slider, Knob, Rating, InputNumber) from two-way to one-way,
  * with no compile error anywhere.
  */
class ReactiveValueTest extends UicTest:

    "constant binds Const" in {
        ReactiveValue(42) match
            case ReactiveValue.Const(v) => assert(v == 42)
            case other                  => fail(s"expected a constant binding, got $other")
    }

    "a writable SignalRef binds two-way" in {
        for ref <- Signal.initRef(1)
        yield ReactiveValue(ref) match
            case ReactiveVariable(r) => assert(r eq ref)
            case other               => fail(s"expected a two-way binding, got $other")
    }

    "any other Signal binds one-way" in {
        for
            ref <- Signal.initRef(1)
            sig = ref.map(_ * 2)
        yield ReactiveValue(sig) match
            case ReactiveVariable(_)  => fail("a derived signal must not bind two-way")
            case ReactiveValue.Dyn(s) => assert(s eq sig)
            case other                => fail(s"expected a one-way binding, got $other")
    }

    "readOnly is how a caller opts a ref out of write-back" in {
        // The choice is made on the runtime class, so ascribing `ref: Signal[Int]` would NOT opt
        // out: only handing over a different object does.
        for
            ref <- Signal.initRef(1)
            view = ref.readOnly
        yield
            val ascribed: Signal[Int] = ref
            assert(ReactiveValue(ascribed).isInstanceOf[ReactiveVariable[?]])
            assert(!ReactiveValue(view).isInstanceOf[ReactiveVariable[?]])
    }

    "a two-way binding still matches the read-only Dyn path" in {
        // Read-only hosts match `case Dyn(sig)` and must keep matching a ReactiveVariable, which is
        // what lets `disabled`/`readonly` share one constructor with the editable value slots.
        for ref <- Signal.initRef(true)
        yield ReactiveValue(ref) match
            case ReactiveValue.Dyn(s) => assert(s eq ref)
            case other                => fail(s"a two-way binding must read as Dyn, got $other")
    }

    "the reactive cases are exact: SignalRef is Signal's only named subclass" in {
        for
            ref <- Signal.initRef(1)
            derived = ref.map(identity)
        yield assert(
            ref.isInstanceOf[Signal.SignalRef[?]] &&
                !derived.isInstanceOf[Signal.SignalRef[?]] &&
                !ref.readOnly.isInstanceOf[Signal.SignalRef[?]]
        )
    }

    "the union setter reaches a component's slot" in {
        for
            ref <- Signal.initRef(5.0)
            sig = ref.map(_ * 2)
        yield
            def bindingOf(s: uic.Slider): String = s.s.valueSlot.binding match
                case Present(ReactiveVariable(_))    => "two-way"
                case Present(ReactiveValue.Dyn(_))   => "one-way"
                case Present(ReactiveValue.Const(_)) => "const"
                case Absent                          => "unset"
            assert(bindingOf(uic.Slider().value(1.0)) == "const")
            assert(bindingOf(uic.Slider().value(ref)) == "two-way")
            assert(bindingOf(uic.Slider().value(sig)) == "one-way")
            assert(bindingOf(uic.Slider().value(ref.readOnly)) == "one-way")
    }
    "the binding a native control takes: a signal or a ref as it is, a constant as a signal that stays" in {
        for
            ref     <- Signal.initRef("a")
            onConst <- ReactiveValue[String]("c").native match
                case s: Signal[String] @unchecked => s.current
                case other                        => throw new AssertionError(s"a constant stayed a value: $other")
        yield
            assert(onConst == "c")
            assert(ReactiveValue[String](ref).native.isInstanceOf[SignalRef[?]])
            assert(ReactiveValue[String](ref.readOnly).native.isInstanceOf[Signal[?]])
        end for
    }

    "what a render reads" - {
        def isConst[A](v: ReactiveValue[A], expected: A)(using CanEqual[A, A], Frame, kyo.test.AssertScope): Unit =
            v match
                case ReactiveValue.Const(a) => assert(a == expected, s"holds $a")
                case other                  => fail(s"expected a constant, got $other")

        /** The value `sig` delivers once `write` ran, with an observer parked on `ref` first: the barrier, no clock. */
        def delivered[A, B](sig: Signal[A], ref: SignalRef[B], write: B)(using CanEqual[A, A], Frame): A < Async =
            for
                next <- Fiber.initUnscoped(sig.next)
                _    <- settles(ref.waiters.map(_ > 0), 5.seconds)
                _    <- ref.set(write)
                got  <- next.get
            yield got

        "current reads a constant, a signal and a ref as they are now" in {
            for
                ref <- Signal.initRef(7)
                c   <- ReactiveValue.Const(1).current
                s   <- ReactiveValue.Dyn(ref.map(_ + 1)).current
                r   <- ReactiveVariable(ref).current
            yield assert(c == 1 && s == 8 && r == 7, s"const $c, signal $s, ref $r")
        }

        "and over two constants is a constant of the pair, and renders with no region" in {
            val both = ReactiveValue.Const(1).and(ReactiveValue.Const("a"))
            regionsAbove(both.render { case (i, s) => UI.div.cssClass(s"p-t-$i-$s") }, "p-t-1-a").map { n =>
                discard(isConst(both, (1, "a")))
                assert(n == 0, s"$n regions")
            }
        }

        "and over a constant and a signal follows the signal, and the constant does not fire it" in {
            for
                ref <- Signal.initRef(2)
                both = ReactiveValue.Const(1).and(ReactiveValue.Dyn(ref.map(identity)))
                now <- both.current
                got <- both match
                    case ReactiveValue.Dyn(sig) => delivered(sig, ref, 5)
                    case other                  => fail(s"expected a signal, got $other")
            yield assert(now == (1, 2) && got == (1, 5), s"now $now, delivered $got")
        }

        "and over two signals renders as one region" in {
            for
                a <- Signal.initRef(1)
                b <- Signal.initRef(2)
                n <- regionsAbove(ReactiveValue[Int](a).and(ReactiveValue[Int](b)).render(_ => UI.div.cssClass("p-t")), "p-t")
            yield assert(n == 1, s"$n regions")
        }

        "map over a ref is one way: nothing writes through what a render read" in {
            for
                ref <- Signal.initRef(1)
                mapped = ReactiveVariable(ref).map(_ + 1)
                now <- mapped.current
            yield
                assert(mapped.writable.isEmpty, "a mapped binding offers a ref to write")
                assert(!mapped.bound.isInstanceOf[SignalRef[?]], s"handed on as ${mapped.bound}")
                assert(!ReactiveValue[Int](mapped.bound).isInstanceOf[ReactiveVariable[?]], "bound again it binds two way")
                assert(now == 2, s"holds $now")
        }

        "map over a constant is a constant" in {
            isConst(ReactiveValue.Const(1).map(_ + 1), 2)
        }

        "a ref stays two way until it is mapped" in {
            Signal.initRef(1).map(ref => assert(ReactiveVariable(ref).writable.exists(_ eq ref)))
        }

        "render draws a constant with no region and a signal as one" in {
            for
                ref   <- Signal.initRef(1)
                fixed <- regionsAbove(ReactiveValue.Const(1).render(_ => UI.div.cssClass("p-t")), "p-t")
                live  <- regionsAbove(ReactiveValue[Int](ref).render(_ => UI.div.cssClass("p-t")), "p-t")
            yield assert(fixed == 0 && live == 1, s"constant $fixed, signal $live")
        }

        "children are as they are for a constant, and one region for a signal" in {
            def rows(ns: List[Int]): List[UI] = ns.map(n => UI.div.cssClass("p-t").cssClass(s"p-t-$n"))
            for
                ref   <- Signal.initRef(List(1, 2))
                fixed <- Kyo.lift(ReactiveValue.Const(List(1, 2)).children(rows))
                live = ReactiveValue[List[Int]](ref).children(rows)
                inLive  <- elementsWithClass(live.head, "p-t")
                regions <- regionsAbove(live.head, "p-t-2")
            yield
                assert(fixed.size == 2, s"${fixed.size} children for a constant")
                assert(live.size == 1, s"${live.size} children for a signal")
                assert(inLive.size == 2, s"${inLive.size} rows inside the region")
                assert(regions == 1, s"$regions regions")
            end for
        }

        "concat with no parts is an empty constant" in {
            isConst(ReactiveValue.concat[Int](Nil), Nil)
        }

        "concat joins neighbouring fixed lists and stays a constant" in {
            isConst(ReactiveValue.concat[Int](Seq(List(1), List(2, 3))), List(1, 2, 3))
        }

        "concat of one signal follows it, as a list" in {
            for
                ref <- Signal.initRef(Seq(1, 2))
                now <- ReactiveValue.concat[Int](Seq(ref)).current
            yield assert(now == List(1, 2), s"holds $now")
        }

        "concat keeps the order with a signal between fixed lists" in {
            given CanEqual[List[Int], List[Int]] = CanEqual.derived
            for
                ref <- Signal.initRef(Seq(2, 3))
                all = ReactiveValue.concat[Int](Seq(List(1), ref, List(4), List(5)))
                now <- all.current
                got <- all match
                    case ReactiveValue.Dyn(sig) => delivered(sig, ref, Seq(9))
                    case other                  => fail(s"expected a signal, got $other")
            yield assert(now == List(1, 2, 3, 4, 5) && got == List(1, 9, 4, 5), s"now $now, delivered $got")
            end for
        }

        "keyed over nothing is an empty constant" in {
            isConst(ReactiveValue.keyed[String, Int](Nil), Map.empty[String, Int])
        }

        "keyed reads each signal under its key" in {
            given CanEqual[Map[String, Int], Map[String, Int]] = CanEqual.derived
            for
                a <- Signal.initRef(1)
                b <- Signal.initRef(2)
                all = ReactiveValue.keyed(Seq("a" -> a, "b" -> b))
                now <- all.current
                got <- all match
                    case ReactiveValue.Dyn(sig) => delivered(sig, b, 7)
                    case other                  => fail(s"expected a signal, got $other")
            yield assert(now == Map("a" -> 1, "b" -> 2) && got == Map("a" -> 1, "b" -> 7), s"now $now, delivered $got")
            end for
        }

        "optional over nothing is a constant Absent, and over a value holds it" in {
            discard(isConst(ReactiveValue.optional[Int](Absent), Absent))
            isConst(ReactiveValue.optional[Int](Present(ReactiveValue.Const(1))), Present(1))
        }

        "optional over a signal follows it, one way" in {
            for
                ref <- Signal.initRef(1)
                opt = ReactiveValue.optional[Int](Present(ReactiveVariable(ref)))
                now <- opt.current
            yield
                assert(now == Present(1), s"holds $now")
                assert(opt.writable.isEmpty, "an optional reading offers a ref to write")
        }

        "orElse over nothing is the fallback, a constant, and over a signal follows it" in {
            for
                ref <- Signal.initRef(1)
                now <- ReactiveValue.orElse[Int](Present(ref), 9).current
            yield
                discard(isConst(ReactiveValue.orElse[Int](Absent, 9), 9))
                assert(now == 1, s"holds $now")
        }
    }

end ReactiveValueTest
