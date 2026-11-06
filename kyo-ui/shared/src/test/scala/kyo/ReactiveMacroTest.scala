package kyo

import kyo.UI.*
import kyo.UI.Ast.*

/** `UI.reactive`: one read per value, a constant from constants, one signal or one region otherwise, and a compile error for every
  * read the block would not have evaluated once while it runs.
  */
class ReactiveMacroTest extends UITest:

    "a block over constants runs once and is the plain value it yields" in {
        val a: Prop[Int] = 1
        val b: Prop[Int] = 2
        val r            = UI.reactive(a.read + b.read)
        assert(!r.isInstanceOf[Signal[?]], s"$r")
        Prop.signal(r).current.map(v => assert(v == 3))
    }

    "a block over a signal follows it, with the constants it reads as they are" in {
        for
            ref <- Signal.initRef(1)
            c: Prop[Int] = 10
            r            = UI.reactive(ref.read + c.read)
            _ <- ref.set(5)
            _ <- assertEventually(Prop.signal(r).current.map(_ == 15))
        yield assert(r.isInstanceOf[Signal[?]], s"$r")
    }

    "a UI block over constants draws once, with no region" in {
        val on: Prop[Boolean] = true
        val ui                = UI.reactive(if on.read then span("on") else UI.empty)
        assert(!ui.isInstanceOf[Reactive[?]] && ui.isInstanceOf[SpanElement], s"$ui")
    }

    "a UI block over two signals is one region, compared by the values read" in {
        for
            a <- Signal.initRef("x")
            b <- Signal.initRef(1)
        yield UI.reactive(span(s"${a.read}${b.read}")) match
            case r: Reactive[?] => assert(r.source.exists(_.isInstanceOf[Reactive.Source.Projected[?]]), s"${r.source}")
            case other          => fail(s"not one region: $other")
    }

    "a signal read twice, by one path or by two, is one read, so the block never sees two of its values" in {
        Scope.run {
            for
                x    <- Signal.initRef(0)
                seen <- AtomicRef.init(Chunk.empty[(Int, Int)])
                y = x
                r = Prop.signal(UI.reactive((x.read - y.read + x.read - x.read, x.read)))
                _ <- Fiber.init(r.observe(v => seen.getAndUpdate(_.append(v)).unit))
                _ <- Kyo.foreachDiscard(1 to 5)(x.set)
                _ <- assertEventually(seen.get.map(_.lastMaybe.contains((0, 5))))
                s <- seen.get
            yield assert(s.forall(_._1 == 0), s"seen $s")
        }
    }

    "a read the block would not evaluate once while it runs is a compile error that says what to write" - {
        "a read outside a block does not exist" in {
            typeCheckFailure("""
                import kyo.*
                val s: Signal[Int] = ???
                s.read
            """)("read is not a member")
        }

        "a read of a value the block defines" in {
            typeCheckFailure("""
                import kyo.*
                UI.reactive { val q: UI.Prop[Int] = 1; q.read }
            """)("of a value the `UI.reactive` block defines")
        }

        "a read inside a lambda" in {
            typeCheckFailure("""
                import kyo.*
                val p: UI.Prop[Int] = 1
                UI.reactive(List(1, 2).map(_ + p.read).sum)
            """)("inside a lambda")
        }

        "a read in a by-name argument" in {
            typeCheckFailure("""
                import kyo.*
                def later(v: => Int): Int = v
                val p: UI.Prop[Int]       = 1
                UI.reactive(later(p.read))
            """)("inside a by-name argument")
        }

        "a read in a local def" in {
            typeCheckFailure("""
                import kyo.*
                val p: UI.Prop[Int] = 1
                UI.reactive { def twice = p.read * 2; twice }
            """)("inside the local `def twice`")
        }

        "a read inside what another read reads" in {
            typeCheckFailure("""
                import kyo.*
                val p: UI.Prop[Int] = 1
                UI.reactive((if p.read > 0 then p else p).read)
            """)("inside what another `.read` reads")
        }

        "the block's scope handed on to a helper" in {
            typeCheckFailure("""
                import kyo.*
                val p: UI.Prop[Int]                    = 1
                def helper(using UI.ReactiveScope): Int = p.read
                UI.reactive(helper)
            """)("only where the block itself is written")
        }
    }

end ReactiveMacroTest
