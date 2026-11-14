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

    "a block over one signal alone follows it, value and region alike" in {
        for
            ref <- Signal.initRef(1)
            r = UI.reactive(ref.read + 1)
            _ <- assertEventually(Prop.signal(r).current.map(_ == 2))
            _ <- ref.set(5)
            _ <- assertEventually(Prop.signal(r).current.map(_ == 6))
        yield assert(r.isInstanceOf[Signal[?]], s"$r")
    }

    "a block over two distinct signals follows each of them, whichever changes" in {
        for
            a <- Signal.initRef(1)
            b <- Signal.initRef(10)
            r = UI.reactive(a.read + b.read)
            _ <- assertEventually(Prop.signal(r).current.map(_ == 11))
            _ <- a.set(2)
            _ <- assertEventually(Prop.signal(r).current.map(_ == 12))
            _ <- b.set(20)
            _ <- assertEventually(Prop.signal(r).current.map(_ == 22))
        yield assert(r.isInstanceOf[Signal[?]], s"$r")
    }

    "a block over one signal read through two names is one signal, its value on both reads" in {
        for
            a <- Signal.initRef(3)
            b = a
            r = UI.reactive(a.read * b.read)
            _ <- assertEventually(Prop.signal(r).current.map(_ == 9))
            _ <- a.set(4)
            _ <- assertEventually(Prop.signal(r).current.map(_ == 16))
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

    "a value picked by a condition shows the branch taken and follows it through each flip" in {
        for
            c <- Signal.initRef(true)
            a <- Signal.initRef("a")
            b <- Signal.initRef("b")
            r = UI.reactive(if c.read then a.read else b.read)
            first <- Prop.signal(r).current
            _     <- a.set("a2")
            _     <- assertEventually(Prop.signal(r).current.map(_ == "a2"))
            _     <- c.set(false)
            _     <- assertEventually(Prop.signal(r).current.map(_ == "b"))
            _     <- b.set("b2")
            _     <- assertEventually(Prop.signal(r).current.map(_ == "b2"))
            _     <- c.set(true)
            _     <- assertEventually(Prop.signal(r).current.map(_ == "a2"))
        yield assert(first == "a", s"$first")
    }

    "a condition joined with && or || reads its right side only while the left side lets it" in {
        for
            g <- Signal.initRef(false)
            s <- Signal.initRef("x")
            h <- Signal.initRef(true)
            both   = UI.reactive(if g.read && s.read.nonEmpty then "on" else "off")
            either = UI.reactive(if h.read || s.read.isEmpty then "yes" else "no")
            first <- Prop.signal(both).current
            _     <- g.set(true)
            _     <- assertEventually(Prop.signal(both).current.map(_ == "on"))
            _     <- s.set("")
            _     <- assertEventually(Prop.signal(both).current.map(_ == "off"))
            early <- Prop.signal(either).current
            _     <- s.set("z")
            _     <- h.set(false)
            _     <- assertEventually(Prop.signal(either).current.map(_ == "no"))
        yield assert(first == "off" && early == "yes", s"$first $early")
    }

    "a branch the constant condition leaves is never built, so its failure does not happen" in {
        val c: Prop[Boolean] = false
        val d: Prop[Int]     = 0
        val r                = UI.reactive(if c.read && (10 / d.read) > 1 then "x" else "y")
        Prop.signal(r).current.map(v => assert(v == "y", s"$v"))
    }

    "a read a block takes and a branch takes too is one slot, so the block sees one value of it" in {
        for
            a <- Signal.initRef(1)
            c <- Signal.initRef(true)
            r = UI.reactive(a.read + (if c.read then a.read else 0))
            first <- Prop.signal(r).current
            _     <- a.set(3)
            _     <- assertEventually(Prop.signal(r).current.map(_ == 6))
        yield assert(first == 2, s"$first")
    }

    "a value picked by a match shows the case taken and follows it through each change of the selector" in {
        for
            k <- Signal.initRef(0)
            a <- Signal.initRef("a")
            b <- Signal.initRef(7)
            r = UI.reactive(k.read match
                case 0 => a.read
                case n => s"$n:${b.read}")
            first <- Prop.signal(r).current
            _     <- a.set("a2")
            _     <- assertEventually(Prop.signal(r).current.map(_ == "a2"))
            _     <- k.set(3)
            _     <- assertEventually(Prop.signal(r).current.map(_ == "3:7"))
            _     <- b.set(8)
            _     <- assertEventually(Prop.signal(r).current.map(_ == "3:8"))
            _     <- k.set(0)
            _     <- assertEventually(Prop.signal(r).current.map(_ == "a2"))
        yield assert(first == "a", s"$first")
    }

    "a match guard on a pattern variable decides the case, and the case taken follows the selector" in {
        for
            k <- Signal.initRef(4)
            a <- Signal.initRef("big")
            r = UI.reactive(k.read match
                case n if n > 3 => s"$n:${a.read}"
                case _          => "small")
            first <- Prop.signal(r).current
            _     <- k.set(1)
            _     <- assertEventually(Prop.signal(r).current.map(_ == "small"))
            _     <- k.set(9)
            _     <- assertEventually(Prop.signal(r).current.map(_ == "9:big"))
            _     <- a.set("huge")
            _     <- assertEventually(Prop.signal(r).current.map(_ == "9:huge"))
        yield assert(first == "4:big", s"$first")
    }

    "a UI block that matches a case by a signal draws the case taken in one region" in {
        for
            k <- Signal.initRef(1)
            r = UI.reactive(k.read match
                case 0 => span("zero")
                case _ => span("other"))
        yield assert(r.isInstanceOf[Reactive[?]], s"$r")
    }

    "a UI block that picks a branch by a signal follows the condition in one region" in {
        for
            on <- Signal.initRef(true)
            r = UI.reactive(if on.read then span("on") else span("off"))
        yield assert(r.isInstanceOf[Reactive[?]], s"$r")
    }

    "a UI block over constants draws once, with no region" in {
        val on: Prop[Boolean] = true
        val ui                = UI.reactive(if on.read then span("on") else UI.empty)
        assert(!ui.isInstanceOf[Reactive[?]] && ui.isInstanceOf[SpanElement], s"$ui")
    }

    "a UI block over two signals is one region, compared by the values read" in {
        // A parameter that takes only the value, so the reads shape what the block draws.
        def label(v: String)(using Frame): UI = span(v)
        for
            a <- Signal.initRef("x")
            b <- Signal.initRef(1)
        yield UI.reactive(label(s"${a.read}${b.read}")) match
            case r: Reactive[?] => assert(r.source.exists(_.isInstanceOf[Reactive.Source.Projected[?]]), s"${r.source}")
            case other          => fail(s"not one region: $other")
        end for
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
            // The inner block's read is its child text's channel: its own read, over its own signal.
            val own = first match
                case s: SpanElement =>
                    s.children.toSeq match
                        case Seq(r: Reactive[?]) => r.source.exists {
                                case Reactive.Source.Lifted(sig) => sig eq inner
                                case _                           => false
                            }
                        case _ => false
                case _ => false
            assert(own, s"the inner block does not follow its own read: $first")
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

    "an optional prop reads as an option of its value, and a signal inside it is followed" - {
        "a constant inside is a plain value, and an unset prop reads Absent" in {
            val set: kyo.uic.Opt[Prop[Int]]   = 2
            val unset: kyo.uic.Opt[Prop[Int]] = kyo.uic.Opt.unset
            val r                             = UI.reactive((set.read, unset.read))
            assert(!r.isInstanceOf[Signal[?]], s"$r")
            Prop.signal(r).current.map(v => assert(v == (Present(2), Absent), s"$v"))
        }

        "a signal inside is followed" in {
            for
                ref <- Signal.initRef(1)
                o: kyo.uic.Opt[Prop[Int]] = ref
                r                         = UI.reactive(o.read.map(_ * 10))
                _ <- ref.set(5)
                _ <- assertEventually(Prop.signal(r).current.map(_ == Present(50)))
            yield assert(r.isInstanceOf[Signal[?]], s"$r")
        }

        "read twice by one path, it is one read, so the block never sees two of its values" in {
            Scope.run {
                for
                    x    <- Signal.initRef(0)
                    seen <- AtomicRef.init(Chunk.empty[(Maybe[Int], Maybe[Int])])
                    o: kyo.uic.Opt[Prop[Int]] = x
                    r                         = Prop.signal(UI.reactive((o.read, o.read)))
                    _ <- Fiber.init(r.observe(v => seen.getAndUpdate(_.append(v)).unit))
                    _ <- Kyo.foreachDiscard(1 to 5)(x.set)
                    _ <- assertEventually(seen.get.map(_.lastMaybe.contains((Present(5), Present(5)))))
                    s <- seen.get
                yield assert(s.forall(v => v._1 == v._2), s"seen $s")
            }
        }

        "a handler reads it when its event comes" in {
            for
                ref <- Signal.initRef(1)
                hit <- Signal.initRef(0)
                o: kyo.uic.Opt[Prop[Int]] = ref
                ui                        = UI.reactive(button("go").onClick(hit.set(o.read.getOrElse(0))))
                el <- ui match
                    case b: Button => Kyo.lift(b)
                    case other     => Kyo.lift(fail(s"a read in a handler drew a region: $other"))
                _ <- ref.set(7)
                _ <- Abort.run[Any](el.attrs.onClick.getOrElse(Kyo.unit))
                v <- hit.get
            yield assert(v == 7, s"the handler saw $v")
        }
    }

    "a read in an argument whose parameter takes a prop is that argument's channel, not a read of the block" - {
        "the attribute follows the signal in place and the block draws once, with no region" in {
            for
                on <- Signal.initRef(false)
                ui = UI.reactive(span.aria(Aria.checked -> on.read.toString))
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
            yield UI.reactive(span.when(on.read)(_.cssClass("x"))) match
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
            UI.reactive(span.aria(Aria.checked -> on.read.toString)) match
                case s: SpanElement =>
                    assert(s.attrs.ariaAttrs.get("checked").contains("true") && s.attrs.reactiveAttrs.isEmpty, s"${s.attrs}")
                case other => fail(s"$other")
            end match
        }

        "a read the structure uses beside it stays the block's" in {
            for
                big <- Signal.initRef(false)
                on  <- Signal.initRef(false)
                ui = UI.reactive(if big.read then div.aria(Aria.checked -> on.read.toString)
                else span.aria(Aria.checked -> on.read.toString))
                drawn <- ui match
                    case r: Reactive[?] => r.signal.current
                    case other          => Kyo.lift(fail(s"the condition drew no region: $other"))
            yield drawn match
                case s: SpanElement => assert(s.attrs.reactiveAttrs.contains("aria-checked"), s"${s.attrs}")
                case other          => fail(s"$other")
        }

        "a read handed to aria or data as an entry's value is that entry's channel, with no region" in {
            for
                on <- Signal.initRef(false)
                n  <- Signal.initRef(1)
                ui = UI.reactive(span.aria(Aria.pressed -> on.read, Aria.busy -> true).data(data"data-uic-n" -> n.read))
                el <- ui match
                    case s: SpanElement => Kyo.lift(s)
                    case other          => Kyo.lift(fail(s"the block drew a region: $other"))
                pressed = el.attrs.reactiveAttrs.get("aria-pressed")
                count   = el.attrs.reactiveAttrs.get("data-uic-n")
                before <- pressed.fold(Kyo.lift(""))(_.current)
                _      <- on.set(true)
                _      <- n.set(2)
                _      <- assertEventually(pressed.fold(Kyo.lift(false))(_.current.map(_ == "true")))
                _      <- assertEventually(count.fold(Kyo.lift(false))(_.current.map(_ == "2")))
            yield assert(before == "false" && el.attrs.ariaAttrs == Map("busy" -> "true"), s"$before ${el.attrs}")
        }

        "a read handed to aria under an Aria is that entry's channel, as it is under the string name" in {
            for
                on <- Signal.initRef(false)
                ui = UI.reactive(span.aria(Aria.pressed -> on.read))
                el <- ui match
                    case s: SpanElement => Kyo.lift(s)
                    case other          => Kyo.lift(fail(s"the block drew a region: $other"))
                pressed = el.attrs.reactiveAttrs.get("aria-pressed")
                before <- pressed.fold(Kyo.lift(""))(_.current)
                _      <- on.set(true)
                _      <- assertEventually(pressed.fold(Kyo.lift(false))(_.current.map(_ == "true")))
            yield assert(before == "false" && el.attrs.ariaAttrs.isEmpty, s"$before ${el.attrs}")
        }

        "a read of a Maybe as an entry's value is the channel that leaves the attribute out while Absent" in {
            for
                ref <- Signal.initRef[Maybe[String]](Absent)
                name: Prop[Maybe[String]] = ref
            yield UI.reactive(span.aria(Aria.label -> name.read)) match
                case s: SpanElement => assert(s.attrs.reactiveMaybeAttrs.contains("aria-label"), s"${s.attrs}")
                case other          => fail(s"the block drew a region: $other")
        }

        "an entry's value over a constant is the plain attribute" in {
            val on: Prop[Boolean] = true
            UI.reactive(span.aria(Aria.checked -> on.read)) match
                case s: SpanElement =>
                    assert(s.attrs.ariaAttrs.get("checked").contains("true") && s.attrs.reactiveAttrs.isEmpty, s"${s.attrs}")
                case other => fail(s"$other")
            end match
        }

        "a parameter that takes only the value keeps the read in the block" in {
            def label(v: String)(using Frame): UI = span(v)
            for
                on <- Signal.initRef(false)
            yield UI.reactive(label(on.read.toString)) match
                case _: Reactive[?] => succeed
                case other          => fail(s"no region for a read the block needs: $other")
        }

        "a read in a child text is that text's channel: the block draws no region, and the text follows the read" in {
            for
                a <- Signal.initRef(1)
                ui = UI.reactive(span(s"v=${a.read}"))
                text <- ui match
                    case s: SpanElement =>
                        s.children.toSeq match
                            case Seq(r: Reactive[?]) => Kyo.lift(r)
                            case other               => Kyo.lift(fail(s"the child is no text channel: $other"))
                    case other => Kyo.lift(fail(s"the block drew a region: $other"))
                lifted = text.source match
                    case Present(Reactive.Source.Lifted(sig)) => Present(sig)
                    case _                                    => Absent
                before <- lifted.fold(Kyo.lift(""))(_.current)
                _      <- a.set(2)
                _      <- assertEventually(lifted.fold(Kyo.lift(false))(_.current.map(_ == "v=2")))
            yield assert(lifted.isDefined && before == "v=1", s"${text.source}, $before")
        }

        "a val that only reaches a child text is that text's channel too, with no region" in {
            for
                a <- Signal.initRef(1)
            yield UI.reactive {
                val label = s"v=${a.read}"
                span(label)
            } match
                case s: SpanElement => assert(s.children.toSeq.forall(_.isInstanceOf[Reactive[?]]), s"${s.children}")
                case other          => fail(s"the block drew a region: $other")
        }

        "a child text over a constant is the plain text" in {
            val a: Prop[Int] = 1
            UI.reactive(span(s"v=${a.read}")) match
                case s: SpanElement =>
                    s.children.toSeq match
                        case Seq(t: Text) => assert(t.value == "v=1", s"$t")
                        case other        => fail(s"$other")
                case other => fail(s"$other")
            end match
        }
    }

    "a read that decides the structure only through a derived condition" - {
        given CanEqual[Any, Any] = CanEqual.derived

        "is a region over the condition's value, and feeds its channels otherwise" in {
            for
                x <- Signal.initRef(40)
                ui = UI.reactive {
                    val pct = x.read
                    div.inlineStyle(Style.width(pct.pct))(if pct != 0 then span(s"$pct%") else UI.empty)
                }
                region <- ui match
                    case r: Reactive[?] => Kyo.lift(r)
                    case other          => Kyo.lift(fail(s"no region over the condition: $other"))
                compared <- region.source match
                    case Present(p: Reactive.Source.Projected[?]) => Kyo.lift(p.signal.asInstanceOf[Signal[Any]])
                    case other                                    => Kyo.lift(fail(s"the region keeps no value it compares: $other"))
                at40  <- compared.current
                drawn <- region.signal.current
                _     <- x.set(60)
                at60  <- compared.current
                _     <- x.set(0)
                at0   <- compared.current
            yield
                // The region compares what the condition gives: 40 and 60 are one value of it, 0 another.
                assert(at40 == at60 && at40 != at0, s"the region compares $at40, $at60, $at0")
                drawn match
                    case d: Div =>
                        assert(d.attrs.reactiveStyle.isDefined, s"the width is no channel: ${d.attrs}")
                        d.children.toSeq match
                            case Seq(s: SpanElement) => assert(s.children.toSeq.forall(_.isInstanceOf[Reactive[?]]), s"${s.children}")
                            case other               => fail(s"the label is not drawn: $other")
                        end match
                    case other => fail(s"$other")
                end match
        }

        "keyed by the read itself, it stays the region's value of the run" in {
            for
                on <- Signal.initRef(true)
                ui = UI.reactive {
                    val x = on.read
                    if x then div.aria(Aria.pressed -> x) else span
                }
                drawn <- ui match
                    case r: Reactive[?] => r.signal.current
                    case other          => Kyo.lift(fail(s"the condition drew no region: $other"))
            yield drawn match
                case d: Div => assert(d.attrs.ariaAttrs.get("pressed").contains("true") && d.attrs.reactiveAttrs.isEmpty, s"${d.attrs}")
                case other  => fail(s"$other")
        }

        "a channel inside never shows a value of a moment the structure has left" in {
            Scope.run {
                for
                    x <- Signal.initRef(40)
                    ui = UI.reactive {
                        val pct = x.read
                        div.inlineStyle(Style.width(pct.pct))(if pct != 0 then span(s"$pct%") else UI.empty)
                    }
                    region <- ui match
                        case r: Reactive[?] => Kyo.lift(r)
                        case other          => Kyo.lift(fail(s"no region over the condition: $other"))
                    drawn <- region.signal.current
                    label <- drawn match
                        case d: Div =>
                            d.children.toSeq match
                                case Seq(s: SpanElement) =>
                                    s.children.toSeq match
                                        case Seq(r: Reactive[?]) => Kyo.lift(r.signal)
                                        case other               => Kyo.lift(fail(s"$other"))
                                case other => Kyo.lift(fail(s"$other"))
                        case other => Kyo.lift(fail(s"$other"))
                    seen <- AtomicRef.init(Chunk.empty[String])
                    _    <- Fiber.init(label.observe {
                        case t: Text => seen.getAndUpdate(_.append(t.value)).unit
                        case _       => Kyo.unit
                    })
                    _ <- assertEventually(seen.get.map(_.nonEmpty))
                    _ <- x.set(70)
                    _ <- assertEventually(seen.get.map(_.lastMaybe.contains("70%")))
                    // The label of a run drawn for `pct != 0`: at 0 the run is left, and the label keeps what it showed.
                    _ <- x.set(0)
                    _ <- x.set(80)
                    _ <- assertEventually(seen.get.map(_.lastMaybe.contains("80%")))
                    s <- seen.get
                yield
                    val steps = s.foldLeft(Chunk.empty[String])((acc, t) => if acc.lastMaybe.contains(t) then acc else acc.append(t))
                    assert(!s.contains("0%") && steps == Chunk("40%", "70%", "80%"), s"the label showed $s")
            }
        }
    }

    "a val of the block is a read to whoever uses it, so naming a read changes nothing" - {
        "a val that only reaches a channel is a channel too, with no region" in {
            for
                on <- Signal.initRef(false)
                ui = UI.reactive {
                    val x     = on.read
                    val label = x.toString
                    span.aria(Aria.checked -> label)
                }
                el <- ui match
                    case s: SpanElement => Kyo.lift(s)
                    case other          => Kyo.lift(fail(s"the block drew a region: $other"))
                chan = el.attrs.reactiveAttrs.get("aria-checked")
                _ <- on.set(true)
                _ <- assertEventually(chan.fold(Kyo.lift(false))(_.current.map(_ == "true")))
            yield succeed
        }

        "a val that only reaches an entry's value is that entry's channel too, with no region" in {
            for
                on <- Signal.initRef(false)
                ui = UI.reactive {
                    val held = on.read
                    span.aria(Aria.pressed -> held)
                }
                el <- ui match
                    case s: SpanElement => Kyo.lift(s)
                    case other          => Kyo.lift(fail(s"the block drew a region: $other"))
                chan = el.attrs.reactiveAttrs.get("aria-pressed")
                _ <- on.set(true)
                _ <- assertEventually(chan.fold(Kyo.lift(false))(_.current.map(_ == "true")))
            yield succeed
        }

        "a val the structure and an entry use is the region's, and the entry writes the run's value" in {
            for
                on <- Signal.initRef(true)
                ui = UI.reactive {
                    val x = on.read
                    if x then div.aria(Aria.pressed -> x) else span
                }
                drawn <- ui match
                    case r: Reactive[?] => r.signal.current
                    case other          => Kyo.lift(fail(s"the condition drew no region: $other"))
            yield drawn match
                case d: Div => assert(d.attrs.ariaAttrs.get("pressed").contains("true") && d.attrs.reactiveAttrs.isEmpty, s"${d.attrs}")
                case other  => fail(s"$other")
        }

        "a val the structure also uses is the region's, and its channel use takes the run's value" in {
            for
                on <- Signal.initRef(true)
                ui = UI.reactive {
                    val x = on.read
                    if x then div.aria(Aria.checked -> x.toString) else span
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
                ui = UI.reactive(if on.read then div.aria(Aria.checked -> on.read.toString) else span)
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
                ui = UI.reactive(if a.read then div.aria(Aria.label -> s"${a.read}-${b.read}") else span)
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

    "a read handed to when(Boolean) over toggle setters is the toggle's channel, by the rule of a prop argument" - {
        // The class `x` on `el` now: a toggled class whose channel holds true, or a static one.
        def classed(el: Element, cls: String)(using Frame): Boolean < Sync =
            if el.attrs.cssClasses.contains(cls) then Kyo.lift(true)
            else el.attrs.reactiveClasses.get(cls).fold(Kyo.lift(false))(_.current)

        def drawnOnce(ui: UI)(using Frame, kyo.test.AssertScope): SpanElement < Sync = ui match
            case s: SpanElement => Kyo.lift(s)
            case other          => Kyo.lift(fail(s"the block drew a region: $other"))

        "a bare read: the class follows it in place and the block draws once, with no region" in {
            for
                on     <- Signal.initRef(false)
                el     <- drawnOnce(UI.reactive(span.when(on.read)(_.cssClass("x"))))
                before <- classed(el, "x")
                _      <- on.set(true)
                _      <- assertEventually(classed(el, "x"))
            yield assert(!before && el.attrs.reactiveClasses.get("x").isDefined, s"${el.attrs}")
        }

        "aria and data entries with constant values are toggle setters: the condition is their channel, with no region" in {
            for
                on <- Signal.initRef(false)
                el <-
                    drawnOnce(UI.reactive(span.when(on.read)(_.aria(
                        Aria.pressed  -> true,
                        Aria.valuenow -> 3
                    ).data(data"data-uic-x" ->
                        "on"))))
            yield assert(
                Set("aria-pressed", "aria-valuenow", "data-uic-x").subsetOf(el.attrs.reactiveMaybeAttrs.keySet) &&
                    el.attrs.ariaAttrs.isEmpty && el.attrs.dataAttrs.isEmpty,
                s"${el.attrs}"
            )
        }

        "a tab index is a toggle setter: the tab stop follows the read in place, with no region" in {
            for
                on <- Signal.initRef(false)
                el <- drawnOnce(UI.reactive(span.role("button").when(!on.read)(_.tabIndex(0))))
            yield assert(el.attrs.reactiveMaybeAttrs.contains("tabindex") && el.attrs.tabIndex.isEmpty, s"${el.attrs}")
        }

        "a val that only reaches the condition is a channel too, so naming the read changes nothing" in {
            for
                on <- Signal.initRef(false)
                el <- drawnOnce(UI.reactive {
                    val held = on.read
                    span.when(held)(_.cssClass("x").aria(Aria.pressed -> "true")).aria(Aria.busy -> held)
                })
                _ <- on.set(true)
                _ <- assertEventually(classed(el, "x"))
            yield assert(el.attrs.reactiveMaybeAttrs.contains("aria-pressed"), s"${el.attrs}")
        }

        "a condition the structure also uses stays the region's, and the toggle applies its setters as constants" in {
            for
                on <- Signal.initRef(true)
                ui = UI.reactive {
                    val held = on.read
                    if held then span.when(held)(_.cssClass("x")) else div
                }
                drawn <- ui match
                    case r: Reactive[?] => r.signal.current
                    case other          => Kyo.lift(fail(s"the condition drew no region: $other"))
            yield drawn match
                case s: SpanElement => assert(s.attrs.cssClasses == Chunk("x") && s.attrs.reactiveClasses.isEmpty, s"${s.attrs}")
                case other          => fail(s"$other")
        }

        "a setter a toggle lacks keeps the condition the block's: a region, as written" in {
            for
                on   <- Signal.initRef(false)
                name <- Signal.initRef("l")
            yield
                val handler = UI.reactive(span.when(on.read)(_.onClick(Kyo.unit)))
                val mixed   = UI.reactive(span.when(on.read)(_.cssClass("x").style(_.bold)))
                val signal  = UI.reactive(span.when(on.read)(_.aria(Aria.label -> (name: Signal[String])).cssClass("x")))
                assert(
                    handler.isInstanceOf[Reactive[?]] && mixed.isInstanceOf[Reactive[?]] && signal.isInstanceOf[Reactive[?]],
                    s"$handler | $mixed | $signal"
                )
        }

        "a condition with no read is left as written" in {
            val flag = true
            val ui   = UI.reactive(span.when(flag)(_.cssClass("x")))
            ui match
                case s: SpanElement => assert(s.attrs.cssClasses == Chunk("x"), s"${s.attrs}")
                case other          => fail(s"$other")
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
        "a read outside a block is the value now, and a signal's value is its current one" in {
            for
                ref <- Signal.initRef(1)
                c: Prop[Int] = 10
                a <- ref.read
                _ <- ref.set(5)
                b <- ref.read
                k <- c.read
            yield assert(a == 1 && b == 5 && k == 10)
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

        "a read inside a branch is refused where a lambda in it would take it" in {
            typeCheckFailure("""
                import kyo.*
                val c: UI.Prop[Boolean] = true
                val p: UI.Prop[Int]     = 1
                UI.reactive(if c.read then List(1).map(_ + p.read).sum else 0)
            """)("inside a lambda")
        }

        "a read inside a branch is refused where a by-name argument in it would take it" in {
            typeCheckFailure("""
                import kyo.*
                def later(v: => Int): Int = v
                val c: UI.Prop[Boolean]   = true
                val p: UI.Prop[Int]       = 1
                UI.reactive(if c.read then later(p.read) else 0)
            """)("inside a by-name argument")
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
        "a native field's default that reads" in {
            typeCheckFailure("""
                import kyo.*
                val n: UI.Prop[Int] = 1
                UI.reactive(UI.div(UI.span.id(s"n${n.read}"), UI.input.defaultValue(s"start ${n.read}")))
            """)("a new one starts the field over")
        }

        "a checkbox's default tick from a read" in {
            typeCheckFailure("""
                import kyo.*
                val n: UI.Prop[Int] = 1
                UI.reactive(UI.div(UI.span.id(s"n${n.read}"), UI.checkbox.defaultChecked(n.read == 1)))
            """)("a new one starts the field over")
        }

        "an option's default selection from a read" in {
            typeCheckFailure("""
                import kyo.*
                val n: UI.Prop[Int] = 1
                UI.reactive { val k = n.read; UI.select(UI.option.value("1").defaultSelected(k == 1)("one")) }
            """)("a new one starts the field over")
        }

        "a mount with no key" in {
            typeCheckFailure("""
                import kyo.*
                val n: UI.Prop[Int] = 1
                UI.reactive(UI.div(UI.span.id(s"n${n.read}"), UI.mounted(Sync.defer(UI.span("own")))))
            """)("every run of the block starts it over")
        }

        "and allows what a run keeps: a binding, a constant default, a keyed mount, and a block with no region" in {
            for
                n    <- Signal.initRef(1)
                text <- Signal.initRef("text")
                // The value reads, so it is a channel of its own and the field follows it.
                follows = UI.reactive(UI.div(UI.span.id(s"n${n.read}"), UI.input.value(n.read.toString)))
                // A signal is followed, a constant binds (UI.Bound), a default that does not read never moves, and a keyed mount
                // keeps its instance.
                kept = UI.reactive(UI.div(
                    UI.span.id(s"n${n.read}"),
                    UI.input.value(text.readOnly),
                    UI.input.value("start"),
                    UI.checkbox.checked(false),
                    UI.select(UI.option.value("1").selected(true)("one")),
                    UI.input.defaultValue("start"),
                    UI.mounted(Sync.defer(UI.span("own"))).keyed("k")
                ))
                // Every read reaches a channel, so the block draws once and nothing in it runs again.
                once = UI.reactive(UI.div(UI.span.when(n.read > 0)(_.cssClass("on")), UI.input.value("start")))
            yield
                assert(follows.isInstanceOf[Reactive[?]] && kept.isInstanceOf[Reactive[?]], s"$follows $kept")
                assert(!once.isInstanceOf[Reactive[?]], s"no region: $once")
            end for
        }
    }

end ReactiveMacroTest
