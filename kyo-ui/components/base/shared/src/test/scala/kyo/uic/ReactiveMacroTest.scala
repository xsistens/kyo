package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.UI.Ast.*

/** `UI.reactive`: one read per value, a constant from constants, one signal or one region otherwise, and a compile error for every
  * read the block would not have evaluated once while it runs.
  */
class ReactiveMacroTest extends kyo.test.Test[Any]:

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

    "a read of a prop built by a function of its own is taken out with that function" in {
        for
            ref <- Signal.initRef(1)
            p: Prop[Int] = ref
            r            = UI.reactive(Prop.map(p)(x => x * 10).read + Prop.map(p)(x => x + 1).read)
            _ <- ref.set(2)
            _ <- assertEventually(Prop.signal(r).current.map(_ == 23))
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

    "a block inside a block keeps its own reads: the outer region follows its read, the inner one its own" in {
        for
            outer <- Signal.initRef(false)
            inner <- Signal.initRef("a")
            ui = UI.reactive(if outer.read then span("custom") else UI.reactive(span(inner.read)))
            first <- ui match
                case r: Reactive[?] => r.signal.current
                case other          => Kyo.lift(other)
            _    <- inner.set("b")
            _    <- outer.set(true)
            last <- ui match
                case r: Reactive[?] => r.signal.current
                case other          => Kyo.lift(other)
        yield
            assert(ui.isInstanceOf[Reactive[?]], s"the outer block is no region: $ui")
            assert(first.isInstanceOf[Reactive[?]], s"the inner block is no region of its own: $first")
            assert(last.isInstanceOf[SpanElement], s"$last")
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

    "a read in an argument whose parameter takes a prop is that argument's channel, not a read of the block" - {
        "the attribute follows the signal in place and the block draws once, with no region" in {
            for
                on <- Signal.initRef(false)
                ui = UI.reactive(span.aria("checked", on.read.toString))
                el <- ui match
                    case s: SpanElement => Kyo.lift(s)
                    case other          => Kyo.lift(fail(s"the block drew a region: $other"))
                chan = el.attrs.reactiveAttrs.get("aria-checked")
                before <- chan.fold(Kyo.lift(""))(_.current)
                _      <- on.set(true)
                _      <- assertEventually(chan.fold(Kyo.lift(false))(_.current.map(_ == "true")))
            yield assert(before == "false", s"$before")
        }

        "a bare read hands on what it reads" in {
            for
                on <- Signal.initRef(false)
            yield UI.reactive(span.cssClass("x", on.read)) match
                case s: SpanElement => assert(s.attrs.reactiveClasses.get("x").exists(_ eq on), s"${s.attrs.reactiveClasses}")
                case other          => fail(s"the block drew a region: $other")
        }

        "a read handed to cssClass.matching is its class-name channel" in {
            for
                on <- Signal.initRef(false)
            yield UI.reactive(span.cssClass.matching(on.read)(b => if b then "x" else Absent)) match
                case s: SpanElement => assert(s.attrs.reactiveClassNames.size == 1, s"${s.attrs}")
                case other          => fail(s"the block drew a region: $other")
        }

        "over a constant it is the plain attribute" in {
            val on: Prop[Boolean] = true
            UI.reactive(span.aria("checked", on.read.toString)) match
                case s: SpanElement =>
                    assert(s.attrs.ariaAttrs.get("checked").contains("true") && s.attrs.reactiveAttrs.isEmpty, s"${s.attrs}")
                case other => fail(s"$other")
            end match
        }

        "a read the structure uses beside it stays the block's" in {
            for
                big <- Signal.initRef(false)
                on  <- Signal.initRef(false)
                ui = UI.reactive(if big.read then div.aria("checked", on.read.toString) else span.aria("checked", on.read.toString))
                drawn <- ui match
                    case r: Reactive[?] => r.signal.current
                    case other          => Kyo.lift(fail(s"the condition drew no region: $other"))
            yield drawn match
                case s: SpanElement => assert(s.attrs.reactiveAttrs.contains("aria-checked"), s"${s.attrs}")
                case other          => fail(s"$other")
        }

        "a parameter that takes only the value keeps the read in the block" in {
            def label(v: String)(using Frame): UI = span(v)
            for
                on <- Signal.initRef(false)
            yield UI.reactive(label(on.read.toString)) match
                case _: Reactive[?] => succeed
                case other          => fail(s"no region for a read the block needs: $other")
        }
    }

    "a val of the block is a read to whoever uses it, so naming a read changes nothing" - {
        "a val that only reaches a channel is a channel too, with no region" in {
            for
                on <- Signal.initRef(false)
                ui = UI.reactive {
                    val x     = on.read
                    val label = x.toString
                    span.aria("checked", label)
                }
                el <- ui match
                    case s: SpanElement => Kyo.lift(s)
                    case other          => Kyo.lift(fail(s"the block drew a region: $other"))
                chan = el.attrs.reactiveAttrs.get("aria-checked")
                _ <- on.set(true)
                _ <- assertEventually(chan.fold(Kyo.lift(false))(_.current.map(_ == "true")))
            yield succeed
        }

        "a val the structure also uses is the region's, and its channel use takes the run's value" in {
            for
                on <- Signal.initRef(true)
                ui = UI.reactive {
                    val x = on.read
                    if x then div.aria("checked", x.toString) else span
                }
                drawn <- ui match
                    case r: Reactive[?] => r.signal.current
                    case other          => Kyo.lift(fail(s"the condition drew no region: $other"))
            yield drawn match
                case d: Div => assert(d.attrs.ariaAttrs.get("checked").contains("true") && d.attrs.reactiveAttrs.isEmpty, s"${d.attrs}")
                case other  => fail(s"$other")
        }

        "a read the region already has is its value of the run in a channel argument too" in {
            for
                on <- Signal.initRef(true)
                ui = UI.reactive(if on.read then div.aria("checked", on.read.toString) else span)
                drawn <- ui match
                    case r: Reactive[?] => r.signal.current
                    case other          => Kyo.lift(fail(s"the condition drew no region: $other"))
            yield drawn match
                case d: Div => assert(d.attrs.ariaAttrs.get("checked").contains("true") && d.attrs.reactiveAttrs.isEmpty, s"${d.attrs}")
                case other  => fail(s"$other")
        }

        "a channel argument follows its own read and takes the region's as a value of the run" in {
            for
                a <- Signal.initRef(true)
                b <- Signal.initRef("x")
                ui = UI.reactive(if a.read then div.aria("label", s"${a.read}-${b.read}") else span)
                drawn <- ui match
                    case r: Reactive[?] => r.signal.current
                    case other          => Kyo.lift(fail(s"the condition drew no region: $other"))
                chan <- drawn match
                    case d: Div => Kyo.lift(d.attrs.reactiveAttrs.get("aria-label"))
                    case other  => Kyo.lift(fail(s"$other"))
                _ <- b.set("y")
                _ <- assertEventually(chan.fold(Kyo.lift(false))(_.current.map(_ == "true-y")))
            yield succeed
        }

        "a val a lambda uses is the region's" in {
            for
                on <- Signal.initRef(false)
            yield UI.reactive {
                val x = on.read
                span(List(1).map(_ => x.toString).mkString)
            } match
                case _: Reactive[?] => succeed
                case other          => fail(s"no region for a val a lambda needs: $other")
        }
    }

    "a read in an event handler is the prop's value when its event comes" - {
        def run(action: Maybe[UI.Action])(using Frame): Unit < (Async & Abort[Throwable]) =
            Abort.run[Any](action.getOrElse(Kyo.unit)).map(_ => ())

        "a read in a handler reads when the handler runs, and draws no region" in {
            for
                on  <- Signal.initRef(false)
                hit <- Signal.initRef(0)
                ui = UI.reactive(button("go").onClick(if on.read then hit.set(1) else hit.set(2)))
                el <- ui match
                    case b: Button => Kyo.lift(b)
                    case other     => Kyo.lift(fail(s"a read in a handler drew a region: $other"))
                _ <- on.set(true)
                _ <- run(el.attrs.onClick)
                v <- hit.get
            yield assert(v == 1, s"the handler saw the value of the draw: $v")
        }

        "a val a channel and a handler use is a channel, and the handler reads it when its event comes" in {
            for
                on  <- Signal.initRef(false)
                hit <- Signal.initRef(0)
                ui = UI.reactive {
                    val off = on.read
                    button("go").disabled(off).onClick(if off then hit.set(1) else hit.set(2))
                }
                el <- ui match
                    case b: Button => Kyo.lift(b)
                    case other     => Kyo.lift(fail(s"the val drew a region: $other"))
                _ <- on.set(true)
                _ <- run(el.attrs.onClick)
                v <- hit.get
            yield assert(el.attrs.reactiveBoolAttrs.contains("disabled") && v == 1, s"${el.attrs.reactiveBoolAttrs} / $v")
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
                def helper(using ReactiveScope): Int = p.read
                UI.reactive(helper)
            """)("only where the block itself is written")
        }
    }

    "a UI block that draws a region refuses what each of its runs takes from the reader (UI.Loss)" - {
        "a native field's value stated as a constant" in {
            typeCheckFailure("""
                import kyo.*
                val n: UI.Prop[Int] = 1
                UI.reactive(UI.div(UI.span(n.read.toString), UI.input.value("start")))
            """)("every run of the block puts it back")
        }

        "a checkbox's tick stated as a constant" in {
            typeCheckFailure("""
                import kyo.*
                val n: UI.Prop[Int] = 1
                UI.reactive(UI.div(UI.span(n.read.toString), UI.checkbox.checked(false)))
            """)("every run of the block puts it back")
        }

        "an option's selection from a read" in {
            typeCheckFailure("""
                import kyo.*
                val n: UI.Prop[Int] = 1
                UI.reactive { val k = n.read; UI.select(UI.option.value("1").selected(k == 1)("one")) }
            """)("every run of the block puts it back")
        }

        "a mount with no key" in {
            typeCheckFailure("""
                import kyo.*
                val n: UI.Prop[Int] = 1
                UI.reactive(UI.div(UI.span(n.read.toString), UI.mounted(Sync.defer(UI.span("own")))))
            """)("every run of the block starts it over")
        }

        "and allows what a run keeps: a value that reads, a signal, a keyed mount, and a block with no region" in {
            for
                n    <- Signal.initRef(1)
                text <- Signal.initRef("text")
                // The value reads, so it is a channel of its own and the field follows it.
                follows = UI.reactive(UI.div(UI.span(n.read.toString), UI.input.value(n.read.toString)))
                // A signal is followed, and a keyed mount keeps its instance.
                kept = UI.reactive(UI.div(
                    UI.span(n.read.toString),
                    UI.input.value(text.readOnly),
                    UI.mounted(Sync.defer(UI.span("own"))).keyed("k")
                ))
                // Every read reaches a channel, so the block draws once and nothing in it runs again.
                once = UI.reactive(UI.div(UI.span.cssClass("on", n.read > 0), UI.input.value("start")))
            yield
                assert(follows.isInstanceOf[Reactive[?]] && kept.isInstanceOf[Reactive[?]], s"$follows $kept")
                assert(!once.isInstanceOf[Reactive[?]], s"no region: $once")
            end for
        }
    }

end ReactiveMacroTest
