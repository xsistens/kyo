package kyo.uic

import kyo.*

class SlotTest extends UicTest:

    /** The four bindings a slot can hold, with what each answers: writable, a constant, a signal. */
    private def bindings(using Frame): Seq[(String, Maybe[ReactiveValue[Int]], Boolean, Boolean, Boolean)] =
        Seq(
            ("nothing", Absent, false, false, false),
            ("a constant", Present(ReactiveValue(1)), false, true, false),
            ("a signal", Present(ReactiveValue.Dyn(Signal.initConst(1))), false, false, true)
        )

    private val hearerF: Int => UI.Action = _ => ()

    "the truth table" - {
        "of heard, fixed and deaf over nothing, a constant and a signal, by hearer, by another event and by quiet" in {
            for
                (name, binding, writable, constant, signal) <- bindings
                hearer                                      <- Seq(false, true)
                elsewhere                                   <- Seq(false, true)
                quiet                                       <- Seq(false, true)
            do
                val s     = Slot[Int](binding, if hearer then Present(hearerF) else Absent, elsewhere, quiet)
                val where = s"$name, hearer $hearer, elsewhere $elsewhere, quiet $quiet"
                val heard = writable || hearer || elsewhere
                assert(s.heard == heard, s"heard: $where")
                assert(s.fixed == (constant && !heard), s"fixed: $where")
                assert(s.deaf == (signal && !heard && !quiet), s"deaf: $where")
            end for
            succeed
        }

        "a ref is heard by itself, is never fixed and never deaf" in {
            Signal.initRef(1).map { ref =>
                val s = Slot[Int]().bind(ref)
                assert(s.heard)
                assert(!s.fixed)
                assert(!s.deaf)
            }
        }

        "a read-only view of a ref is a signal, not a ref" in {
            Signal.initRef(1).map { ref =>
                val s = Slot[Int]().bind(ref.readOnly)
                assert(!s.heard)
                assert(s.deaf)
            }
        }

        "fixedAt says fixed at that value, and nothing else" in {
            val s = Slot[Int]().bind(1)
            assert(s.fixedAt(1))
            assert(!s.fixedAt(2))
            assert(!s.hear(hearerF).fixedAt(1))
            assert(!Slot[Int]().fixedAt(1))
        }
    }

    "bind" - {
        "takes the runtime class of the value, as ReactiveValue does" in {
            Signal.initRef(1).map { ref =>
                assert(Slot[Int]().bind(1).binding.exists(_.const.contains(1)))
                assert(Slot[Int]().bind(ref.readOnly).binding.exists(b => b.dyn.isDefined && b.writable.isEmpty))
                assert(Slot[Int]().bind(ref).binding.exists(_.writable.isDefined))
            }
        }

        "keeps who hears it" in {
            val s = Slot[Int]().hear(hearerF).bind(1)
            assert(s.hearer.isDefined)
        }
    }

    "change" - {
        "writes a ref before it reports" in {
            for
                ref  <- Signal.initRef(0)
                seen <- Signal.initRef(Chunk.empty[(Int, Int)])
                s = Slot[Int]().bind(ref).hear(v => ref.get.map(h => seen.updateAndGet(_.append((v, h))).unit))
                _   <- s.change(3)
                got <- seen.get
            yield assert(got == Chunk((3, 3)))
        }

        "a signal is not written, and the hearer hears what the reader asked for" in {
            for
                ref  <- Signal.initRef(0)
                seen <- Signal.initRef(Chunk.empty[Int])
                s = Slot[Int]().bind(ref.readOnly).hear(v => seen.updateAndGet(_.append(v)).unit)
                _    <- s.change(3)
                got  <- seen.get
                held <- ref.get
            yield
                assert(got == Chunk(3))
                assert(held == 0)
        }

        "unbound, the hearer hears it" in {
            for
                seen <- Signal.initRef(Chunk.empty[Int])
                _    <- Slot[Int]().hear(v => seen.updateAndGet(_.append(v)).unit).change(3)
                got  <- seen.get
            yield assert(got == Chunk(3))
        }

        "a second hear replaces the first: one hearer, as every setter in this module" in {
            for
                first  <- Signal.initRef(0)
                second <- Signal.initRef(0)
                s = Slot[Int]().hear(v => first.set(v)).hear(v => second.set(v))
                _ <- s.change(3)
                a <- first.get
                b <- second.get
            yield
                assert(a == 0)
                assert(b == 3)
        }
    }

    "update" - {
        "steps a ref atomically: steps that interleave lose nothing" in {
            for
                ref <- Signal.initRef(0)
                s = Slot[Int]().bind(ref)
                _    <- Async.fill(50, 50)(s.update(_ + 1))
                held <- ref.get
            yield assert(held == 50)
        }

        "steps a constant and a signal from the value shown, writes nothing, and reports the result" in {
            for
                ref  <- Signal.initRef(5)
                seen <- Signal.initRef(Chunk.empty[Int])
                hear = (v: Int) => seen.updateAndGet(_.append(v)).unit
                _    <- Slot[Int]().bind(10).hear(hear).update(_ + 1)
                _    <- Slot[Int]().bind(ref.readOnly).hear(hear).update(_ + 1)
                got  <- seen.get
                held <- ref.get
            yield
                assert(got == Chunk(11, 6))
                assert(held == 5)
        }

        "unbound does nothing and reports nothing: there is nothing to step from" in {
            for
                seen <- Signal.initRef(Chunk.empty[Int])
                _    <- Slot[Int]().hear(v => seen.updateAndGet(_.append(v)).unit).update(_ + 1)
                got  <- seen.get
            yield assert(got.isEmpty)
        }
    }

    "what a render reads" - {
        "reading stands in for nothing bound, follows a signal and a ref, and stays fixed over a constant" in {
            Signal.initRef(7).map { ref =>
                Slot[Int]().reading(1) match
                    case Reading.Fixed(v) => assert(v == 1)
                    case other            => throw new AssertionError(s"nothing bound reads as $other")
                Slot[Int]().bind(2).reading(1) match
                    case Reading.Fixed(v) => assert(v == 2)
                    case other            => throw new AssertionError(s"a constant reads as $other")
                Slot[Int]().bind(ref).reading(1) match
                    case Reading.Live(sig) => sig.current.map(v => assert(v == 7))
                    case other             => throw new AssertionError(s"a ref reads as $other")
            }
        }

        "current reads the binding at this moment, or what stands in" in {
            for
                ref <- Signal.initRef(7)
                a   <- Slot[Int]().current(1)
                b   <- Slot[Int]().bind(2).current(1)
                c   <- Slot[Int]().bind(ref).current(1)
                _   <- ref.set(8)
                d   <- Slot[Int]().bind(ref).current(1)
            yield assert((a, b, c, d) == (1, 2, 7, 8))
        }

        "bound gives the binding back as the caller gave it" in {
            Signal.initRef(7).map { ref =>
                assert(Slot[Int]().bound.isEmpty)
                assert(Slot[Int]().bind(2).bound.exists(_.equals(2)))
                assert(Slot[Int]().bind(ref).bound.exists(_.equals(ref)))
            }
        }

        "standIn draws as a signal that is no mistake: not fixed, and no card" in {
            val s = Slot[Int]().standIn(1)
            assert(s.binding.exists(_.dyn.isDefined))
            assert(!s.fixed)
            assert(!s.deaf)
        }
    }
end SlotTest
