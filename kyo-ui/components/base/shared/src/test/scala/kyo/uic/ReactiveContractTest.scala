package kyo.uic

import java.util.concurrent.atomic.AtomicInteger
import kyo.*
import kyo.Length.*
import kyo.UI.*
import kyo.internal.HtmlRenderer
import kyo.internal.ReactiveUI
import kyo.internal.RecordingExchange
import kyo.internal.RecordingExchange.Op
import scala.compiletime.testing.typeCheckErrors

/** What a `UI.reactive` block does, seen from outside: what a client is shown, what the transport is sent, how often the block's own
  * code runs, and how many observers wait on each input. Nothing here reads how the block is expanded, so the suite holds any
  * implementation of it (the applicative expansion of today, a Solid-like one that tracks what each run reads) to the same terms.
  *
  * Three groups:
  *
  *   - correctness: the client shows what the block computes from the current inputs, at every step and with no value that mixes two
  *     moments of one source. Asserted.
  *   - cost: per change of an input, the runs of the block's code, the regions drawn again and the patches sent, and the observers an
  *     input holds. A floor every implementation has to meet is asserted; the ideal is reported (`REACTIVE|...` lines, `✓` met, `✗`
  *     missed), so two implementations can be compared on it.
  *   - expressiveness: which ways of writing a read compile. Reported, since what is refused is a decision, not a defect.
  *
  * The client is a model of what a browser shows for the element `id="out"`: its text, `title`, whether it has the class `hot`, and
  * its inline `width`, from the first render and then from every paint and patch in the order the transport was told.
  */
class ReactiveContractTest extends kyo.test.Test[Any]:

    // ---- What the client shows ----

    /** The element `id="out"` as a client shows it. */
    final private case class Out(text: String, title: String, hot: Boolean, width: String) derives CanEqual

    private object Out:
        val none: Out = Out("", "", false, "")

        /** `was` updated from the element `id="out"` in `html`, or `was` where `html` holds no such element. */
        def in(html: String, was: Out): Out =
            val at = html.indexOf("id=\"out\"")
            if at < 0 then was
            else
                val start                      = html.lastIndexOf('<', at)
                val end                        = html.indexOf('>', at)
                val tag                        = html.substring(start, end)
                def attr(name: String): String = s"""(?:^|\\s)$name="([^"]*)"""".r.findFirstMatchIn(tag).fold("")(_.group(1))
                val width                      = """width:\s*([^;]+)""".r.findFirstMatchIn(attr("style")).fold("")(_.group(1).trim)
                Out(textOf(html, end + 1), attr("title"), attr("class").split(' ').contains("hot"), width)
            end if
        end in

        /** The text inside the element whose start tag ends before `from`, up to its own closing tag. */
        private def textOf(html: String, from: Int): String =
            val sb = new StringBuilder
            @scala.annotation.tailrec
            def loop(i: Int, depth: Int): String =
                if i >= html.length || depth == 0 then sb.toString
                // A comment (a region's or a text channel's markers) shows nothing and opens nothing.
                else if html.startsWith("<!--", i) then loop(html.indexOf("-->", i) + 3, depth)
                else if html.charAt(i) == '<' then
                    val close = html.indexOf('>', i)
                    val tag   = html.substring(i, close + 1)
                    val next  = if tag.startsWith("</") then depth - 1 else if tag.endsWith("/>") then depth else depth + 1
                    loop(close + 1, next)
                else
                    sb.append(html.charAt(i))
                    loop(i + 1, depth)
            loop(from, 1)
        end textOf
    end Out

    extension [A](c: Chunk[A])
        /** The values in order, each run of equal ones kept once: what a reader saw change. */
        private def steps(using CanEqual[A, A]): Chunk[A] =
            c.foldLeft(Chunk.empty[A])((acc, a) => if acc.lastMaybe.contains(a) then acc else acc.append(a))
    end extension

    /** Counts the runs of a block's own code: `runs.tick()` written as a statement of the block. */
    final private class Runs:
        private val n                      = new AtomicInteger(0)
        def tick(): Unit                   = discard(n.incrementAndGet())
        def count(using Frame): Int < Sync = Sync.defer(n.get())
    end Runs

    /** A running UI and what its transport was told. */
    final private class Live(rec: RecordingExchange, initial: String):
        def paints(using Frame): Int < Sync = rec.ops.map(_.count {
            case _: Op.Paint => true
            case _           => false
        })
        def patches(using Frame): Int < Sync = rec.ops.map(_.count {
            case _: Op.Attr | _: Op.BoolAttr | _: Op.Class | _: Op.Style | _: Op.Text => true
            case _                                                                    => false
        })

        /** Every state of `id="out"` the client showed, the first render first. */
        def shown(using Frame): Chunk[Out] < Sync =
            rec.ops.map(ops =>
                Kyo.foldLeft(ops)(Chunk(Out.in(initial, Out.none))) { (acc, op) =>
                    val was = acc.last
                    op match
                        // A text channel the transport cannot write in place (one over several signals binds no synchronous
                        // write) repaints its lone text node, which is the element's text, as a text patch is.
                        case Op.Paint(_, _, t: UI.Ast.Text) => acc.append(was.copy(text = t.value))
                        case Op.Paint(path, _, ui)          => HtmlRenderer.render(ui, path).map(h => acc.append(Out.in(h, was)))
                        case Op.Attr(_, n, v, _)            => if n == "title" then acc.append(was.copy(title = v)) else acc
                        case Op.Class(_, n, on, _)          => if n == "hot" then acc.append(was.copy(hot = on)) else acc
                        case Op.Style(_, set, r, _)         =>
                            val w = set.collectFirst { case ("width", v) => v }.getOrElse(if r.contains("width") then "" else was.width)
                            acc.append(was.copy(width = w))
                        case Op.Text(_, v) => acc.append(was.copy(text = v))
                        case _             => acc
                    end match
                }
            )

        def now(using Frame): Out < Sync = shown.map(_.last)
    end Live

    /** `ui` running against a transport that records what it is sent, channels patching on the writer's stack as in a browser. */
    private def live[A](ui: UI)(f: Live => A < (Async & Abort[Any]))(using Frame): A < (Async & Abort[Any]) =
        Scope.run {
            for
                initial <- HtmlRenderer.render(ui, Seq.empty)
                rec = new RecordingExchange(synchronous = true)
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec)
                a    <- f(new Live(rec, initial))
            yield a
        }

    /** What a static render of `ui` shows now: the block computed from the current inputs. */
    private def truth(ui: UI)(using Frame): Out < Sync = HtmlRenderer.render(ui, Seq.empty).map(Out.in(_, Out.none))

    private val within = 20.seconds

    /** Waits until the client shows a state `p` accepts. */
    private def shows(l: Live)(p: Out => Boolean)(using Frame, kyo.test.AssertScope): Unit < Async =
        assertEventually(l.now.map(p), within)

    /** Waits until `ref` has at least `n` observers parked on it: the UI took its first value and waits for the next. */
    private def observed[A](ref: Signal.SignalRef[A], n: Int = 1)(using Frame, kyo.test.AssertScope): Unit < Async =
        assertEventually(ref.waiters.map(_ >= n), within)

    /** Writes `v` and waits until every observer that waited on `ref` took it and waits again, so what it sent is recorded. */
    private def settle[A](ref: Signal.SignalRef[A], v: A)(using Frame, kyo.test.AssertScope): Unit < Async =
        ref.waiters.map(before => ref.set(v).andThen(assertEventually(ref.waiters.map(_ >= before), within)))

    /** What one change cost: runs of the block's code, regions drawn again, patches sent. */
    final private case class Cost(runs: Int, paints: Int, patches: Int) derives CanEqual

    private def cost(l: Live, runs: Runs)(change: => Unit < (Async & Abort[Any]))(using Frame): Cost < (Async & Abort[Any]) =
        for
            r0 <- runs.count
            p0 <- l.paints
            t0 <- l.patches
            _  <- change
            r1 <- runs.count
            p1 <- l.paints
            t1 <- l.patches
        yield Cost(r1 - r0, p1 - p0, t1 - t0)

    /** One reported line: each metric as measured, with its ideal and whether it met it. */
    private def report(group: String, name: String)(metrics: (String, Int, Int)*): Unit =
        val parts = metrics.map((m, v, ideal) => s"$m=$v (ideal $ideal ${if v <= ideal then "✓" else "✗"})")
        println(s"REACTIVE|$group|$name|${parts.mkString(" · ")}")

    /** The element `id` in what `ui` shows now, with its handlers. */
    private def find(ui: UI, id: String)(using Frame): Maybe[UI.Ast.Element] < Sync =
        def first(children: Chunk[UI]): Maybe[UI.Ast.Element] < Sync =
            Kyo.foldLeft(children)(Absent: Maybe[UI.Ast.Element])((found, c) => if found.isDefined then found else find(c, id))
        ui match
            case e: UI.Ast.Element if e.attrs.identifier.contains(id) => Present(e)
            case e: UI.Ast.Element                                    => first(e.children)
            case f: UI.Ast.Fragment[?]                                => first(f.children)
            case r: UI.Ast.Reactive[?]                                => r.signal.current(using r.frame).map(find(_, id))
            case _                                                    => Absent
        end match
    end find

    /** Presses the element `id` of what `ui` shows now. */
    private def click(ui: UI, id: String)(using Frame): Unit < (Async & Abort[Any]) =
        find(ui, id).map(_.flatMap(_.attrs.onClick).fold[Unit < (Async & Abort[Any])](Abort.fail(s"nothing to press at $id"))(action =>
            Abort.run[Any](action).unit
        ))

    // ---- Correctness ----

    "correctness" - {

        "over constants the block is its value, and the client is sent nothing" in {
            val a: Prop[Int] = 1
            val value        = UI.reactive(a.read + 1)
            val ui           = UI.reactive(span.id("out")(s"v=${a.read}"))
            live(ui) { l =>
                for
                    shown   <- l.now
                    paints  <- l.paints
                    patches <- l.patches
                yield assert(
                    !value.isInstanceOf[Signal[?]] && shown.text == "v=1" && paints == 0 && patches == 0,
                    s"value $value, shown $shown, $paints paints, $patches patches"
                )
            }
        }

        "a text follows one signal at every step" in {
            for
                a <- Signal.initRef(1)
                ui = UI.reactive(span.id("out")(s"v=${a.read}"))
                r <- live(ui) { l =>
                    for
                        _     <- observed(a)
                        _     <- Kyo.foreachDiscard(2 to 4)(n => settle(a, n).andThen(shows(l)(_.text == s"v=$n")))
                        t     <- truth(ui)
                        shown <- l.shown
                    yield (t, shown.map(_.text).steps)
                }
            yield assert(r._1.text == "v=4" && r._2 == Chunk("v=1", "v=2", "v=3", "v=4"), s"truth ${r._1}, shown ${r._2}")
        }

        "a text over two signals follows a change of either" in {
            for
                a <- Signal.initRef("a")
                b <- Signal.initRef(1)
                ui = UI.reactive(span.id("out")(s"${a.read}${b.read}"))
                last <- live(ui) { l =>
                    for
                        _ <- observed(a)
                        _ <- settle(a, "x").andThen(shows(l)(_.text == "x1"))
                        _ <- settle(b, 2).andThen(shows(l)(_.text == "x2"))
                        o <- l.now
                    yield o
                }
            yield assert(last.text == "x2", s"$last")
        }

        "one source read by two paths never shows two of its values at once" in {
            for
                x <- Signal.initRef(0)
                y = x
                z = (x: Signal[Int])
                // The source itself is shown beside the difference, so the wait ends only once the last write is drawn.
                ui = UI.reactive(span.id("out")(s"${x.read - y.read + z.read - x.read}|${x.read}"))
                (seen, t) <- live(ui) { l =>
                    for
                        _ <- observed(x)
                        _ <- Kyo.foreachDiscard(1 to 20)(n => x.set(n))
                        _ <- shows(l)(_.text == "0|20")
                        s <- l.shown
                        t <- truth(ui)
                    yield (s.map(_.text.takeWhile(_ != '|')).steps, t.text)
                }
            yield assert(seen == Chunk("0") && t == "0|20", s"shown $seen, truth $t")
        }

        "a source and a signal derived from it are read at one moment (the diamond)" in {
            for
                x <- Signal.initRef(1)
                d  = x.map(_ * 2)
                ui = UI.reactive(span.id("out")(s"${d.read - 2 * x.read}|${x.read}"))
                seen <- live(ui) { l =>
                    for
                        _ <- observed(x)
                        _ <- Kyo.foreachDiscard(2 to 20)(n => x.set(n))
                        _ <- shows(l)(_.text == "0|20")
                        s <- l.shown
                    yield s.map(_.text.takeWhile(_ != '|')).steps
                }
            yield assert(seen == Chunk("0"), s"a mixed moment was shown: $seen")
        }

        "a condition picks which signal shows, and each change of the shown one shows" in {
            for
                c <- Signal.initRef(true)
                a <- Signal.initRef("a")
                b <- Signal.initRef("b")
                ui = UI.reactive(span.id("out")(if c.read then a.read else b.read))
                last <- live(ui) { l =>
                    for
                        _ <- observed(c)
                        _ <- settle(a, "a2").andThen(shows(l)(_.text == "a2"))
                        _ <- settle(c, false).andThen(shows(l)(_.text == "b"))
                        _ <- settle(b, "b2").andThen(shows(l)(_.text == "b2"))
                        _ <- settle(a, "a3")
                        _ <- settle(c, true).andThen(shows(l)(_.text == "a3"))
                        o <- l.now
                    yield o
                }
            yield assert(last.text == "a3", s"$last")
        }

        "a condition that reads nothing decides once, in front of a read" in {
            for
                s <- Signal.initRef("x")
                t <- Signal.initRef(0)
                off = UI.reactive(span.id("out")(s"${t.read}:" + (if false && s.read.nonEmpty then "on" else "none")))
                on  = UI.reactive(span.id("out")(s"${t.read}:" + (if true && s.read.nonEmpty then s"on ${s.read}" else "none")))
                r <- live(off)(l => observed(t).andThen(settle(s, "y")).andThen(settle(t, 1)).andThen(shows(l)(_.text == "1:none")))
                q <- live(on)(l => observed(s).andThen(settle(s, "z")).andThen(shows(l)(_.text == "1:on z")).andThen(l.now))
            yield assert(q.text == "1:on z", s"on $q")
        }

        "a block inside a block follows its own signal, the outer one its own" in {
            for
                o <- Signal.initRef(true)
                i <- Signal.initRef("a")
                ui = UI.reactive(div(if o.read then UI.reactive(span.id("out")(i.read)) else span.id("out")("off")))
                last <- live(ui) { l =>
                    for
                        _ <- observed(o)
                        _ <- settle(i, "b").andThen(shows(l)(_.text == "b"))
                        _ <- settle(o, false).andThen(shows(l)(_.text == "off"))
                        _ <- settle(i, "c")
                        _ <- settle(o, true).andThen(shows(l)(_.text == "c"))
                        x <- l.now
                    yield x
                }
            yield assert(last.text == "c", s"$last")
        }

        "a read named in a val shows what the read written in place shows" in {
            def run(ui: Signal.SignalRef[Int] => UI)(using Frame, kyo.test.AssertScope): Chunk[String] < (Async & Abort[Any]) =
                Signal.initRef(1).map(a =>
                    live(ui(a))(l =>
                        observed(a).andThen(Kyo.foreachDiscard(2 to 4)(n => settle(a, n).andThen(shows(l)(_.text == s"$n"))))
                            .andThen(l.shown.map(_.map(_.text).steps))
                    )
                )
            for
                inPlace <- run(a => UI.reactive(span.id("out")(s"${a.read}")))
                named   <- run(a =>
                    UI.reactive {
                        val v = a.read
                        span.id("out")(s"$v")
                    }
                )
            yield assert(inPlace == named && named == Chunk("1", "2", "3", "4"), s"in place $inPlace, named $named")
            end for
        }

        "an optional prop reads as its value, unset, constant or from a signal" in {
            def ui(o: Opt[Prop[String]])(using Frame): UI = UI.reactive(span.id("out")(o.read.getOrElse("none")))
            for
                s        <- Signal.initRef("s1")
                unset    <- truth(ui(Opt.unset))
                constant <- truth(ui("c"))
                last     <- live(ui(s))(l => observed(s).andThen(settle(s, "s2")).andThen(shows(l)(_.text == "s2")).andThen(l.now))
            yield assert(
                unset.text == "none" && constant.text == "c" && last.text == "s2",
                s"unset $unset, constant $constant, signal $last"
            )
            end for
        }

        "a class follows a read flag" in {
            for
                hot <- Signal.initRef(false)
                ui = UI.reactive(span.id("out").when(hot.read)(_.cssClass("hot"))("x"))
                steps <- live(ui) { l =>
                    for
                        a <- l.now
                        _ <- settle(hot, true).andThen(shows(l)(_.hot))
                        _ <- settle(hot, false).andThen(shows(l)(!_.hot))
                        s <- l.shown
                    yield (a.hot, s.map(_.hot).steps)
                }
            yield assert(!steps._1 && steps._2 == Chunk(false, true, false), s"$steps")
        }

        "an attribute follows a read" in {
            for
                a <- Signal.initRef(1)
                ui = UI.reactive(span.id("out").title(s"t${a.read}")("x"))
                last <- live(ui)(l => observed(a).andThen(settle(a, 2)).andThen(shows(l)(_.title == "t2")).andThen(l.now))
            yield assert(last.title == "t2", s"$last")
        }

        "an inline style follows a read" in {
            for
                w <- Signal.initRef(10)
                ui = span.id("out").inlineStyle(UI.reactive(Style.width(w.read.px)))("x")
                last <- live(ui)(l => observed(w).andThen(settle(w, 30)).andThen(shows(l)(_.width == "30px")).andThen(l.now))
            yield assert(last.width == "30px", s"$last")
        }

        "a value block outside an element feeds its attribute" in {
            for
                a <- Signal.initRef("a")
                ui = span.id("out").title(UI.reactive(a.read + "!"))("x")
                last <- live(ui)(l => observed(a).andThen(settle(a, "b")).andThen(shows(l)(_.title == "b!")).andThen(l.now))
            yield assert(last.title == "b!", s"$last")
        }

        "a handler reads the value its event comes with" in {
            for
                a   <- Signal.initRef(1)
                hit <- Signal.initRef(0)
                ui = UI.reactive(button("go").id("go").onClick(hit.set(a.read)))
                v <- live(ui)(_ => a.set(7).andThen(click(ui, "go")).andThen(hit.get))
            yield assert(v == 7, s"the handler saw $v")
        }

        "a handler that writes what the block reads shows each write" in {
            for
                n <- Signal.initRef(0)
                ui = UI.reactive(div(span.id("out")(s"n=${n.read}"), button("+").id("go").onClick(n.set(n.read + 1))))
                last <- live(ui) { l =>
                    for
                        _ <- observed(n)
                        _ <- Kyo.foreachDiscard(1 to 3)(k => click(ui, "go").andThen(shows(l)(_.text == s"n=$k")))
                        o <- l.now
                    yield o
                }
            yield assert(last.text == "n=3", s"$last")
        }

        "writing the value already shown changes nothing the client sees" in {
            for
                a <- Signal.initRef(1)
                ui = UI.reactive(span.id("out")(s"v=${a.read}"))
                seen <- live(ui)(l => observed(a).andThen(settle(a, 1)).andThen(l.shown.map(_.map(_.text).steps)))
            yield assert(seen == Chunk("v=1"), s"$seen")
        }

        "writes in a row with no pause end at the last one" in {
            for
                a <- Signal.initRef("A")
                ui = UI.reactive(span.id("out")(a.read))
                last <- live(ui) { l =>
                    for
                        _ <- observed(a)
                        // The last write differs from the first value, so the wait cannot end on the first render.
                        _ <- a.set("B").andThen(a.set("A")).andThen(a.set("B")).andThen(a.set("A")).andThen(a.set("C"))
                        _ <- shows(l)(_.text == "C")
                        // Waiting again for the next value: everything written so far is drawn, and nothing came after C.
                        _ <- observed(a)
                        t <- truth(ui)
                        o <- l.now
                    yield (o, t)
                }
            yield assert(last._1.text == "C" && last._2.text == "C", s"$last")
        }

        "a block over fifty signals follows a change of any one" in {
            for
                refs <- Kyo.fill(50)(Signal.initRef(1))
                sigs = Chunk.from(refs)
                ui   = UI.reactive(span.id("out")(s"${sigs(0).read + sigs(1).read + sigs(2).read + sigs(3).read + sigs(4).read}|" +
                    s"${sigs(49).read}"))
                last <- live(ui)(l =>
                    observed(sigs(49)).andThen(settle(sigs(49), 9)).andThen(settle(sigs(2), 3)).andThen(shows(l)(_.text == "7|9"))
                        .andThen(l.now)
                )
            yield assert(last.text == "7|9", s"$last")
        }

        "a list drawn from a read shows each version of the list" in {
            for
                items <- Signal.initRef(Chunk("a", "b"))
                ui = UI.reactive(span.id("out")(items.read.map(i => span(i): UI)*))
                last <- live(ui)(l =>
                    observed(items).andThen(settle(items, Chunk("c", "a", "b"))).andThen(shows(l)(_.text == "cab")).andThen(l.now)
                )
            yield assert(last.text == "cab", s"$last")
        }
    }

    // ---- Cost ----

    "cost" - {

        "a read that only feeds an attribute patches it and runs nothing again" in {
            val runs = new Runs
            for
                a <- Signal.initRef(1)
                ui = UI.reactive {
                    runs.tick()
                    span.id("out").title(s"t${a.read}")("x")
                }
                c <- live(ui)(l => observed(a).andThen(cost(l, runs)(settle(a, 2).andThen(shows(l)(_.title == "t2")))))
            yield
                report("cost", "channel-only read")(("runs", c.runs, 0), ("paints", c.paints, 0), ("patches", c.patches, 1))
                assert(c.runs == 0 && c.paints == 0 && c.patches <= 1, s"$c")
            end for
        }

        "a read that shapes the element runs the block and draws once per change" in {
            val runs = new Runs
            // A parameter that takes only the value: what it builds from the read is structure the block cannot see into.
            def shaped(v: Int)(using Frame): UI = span.id("out")(s"v=$v")
            for
                a <- Signal.initRef(1)
                ui = UI.reactive {
                    runs.tick()
                    shaped(a.read)
                }
                c <- live(ui)(l => observed(a).andThen(cost(l, runs)(settle(a, 2).andThen(shows(l)(_.text == "v=2")))))
            yield
                report("cost", "structural read")(("runs", c.runs, 1), ("paints", c.paints, 1))
                assert(c.runs <= 1 && c.paints <= 1, s"$c")
            end for
        }

        "a read in a child text patches the text and runs nothing again" in {
            val runs = new Runs
            for
                a <- Signal.initRef(1)
                ui = UI.reactive {
                    runs.tick()
                    span.id("out")(s"v=${a.read}")
                }
                r <- live(ui) { l =>
                    for
                        _    <- observed(a)
                        c    <- cost(l, runs)(settle(a, 2).andThen(shows(l)(_.text == "v=2")))
                        seen <- l.shown
                    yield (c, seen)
                }
            yield
                val (c, seen) = r
                report("cost", "text channel")(("runs", c.runs, 0), ("paints", c.paints, 0), ("patches", c.patches, 1))
                // The guarantee of the rule (ISC-47): the text patches in place, the block runs and draws nothing again.
                assert(c.runs == 0 && c.paints == 0 && seen.map(_.text).steps == Chunk("v=1", "v=2"), s"$c, ${seen.map(_.text)}")
            end for
        }

        "writing the value already held costs nothing" in {
            val runs = new Runs
            for
                a <- Signal.initRef(1)
                ui = UI.reactive {
                    runs.tick()
                    span.id("out")(s"v=${a.read}")
                }
                c <- live(ui)(l => observed(a).andThen(cost(l, runs)(settle(a, 1))))
            yield
                report("cost", "same value written")(("runs", c.runs, 0), ("paints", c.paints, 0), ("patches", c.patches, 0))
                assert(c == Cost(0, 0, 0), s"$c")
            end for
        }

        "a signal of the branch not taken" in {
            val runs = new Runs
            for
                c <- Signal.initRef(true)
                a <- Signal.initRef("a")
                b <- Signal.initRef("b")
                ui = UI.reactive {
                    runs.tick()
                    span.id("out")(if c.read then a.read else b.read)
                }
                r <- live(ui) { l =>
                    for
                        _       <- observed(c)
                        waiting <- b.waiters
                        k       <- cost(l, runs)(settle(b, "b2"))
                        shown   <- l.now
                    yield (waiting, k, shown)
                }
            yield
                val (waiting, k, shown) = r
                report("cost", "untaken branch")(("observers", waiting, 0), ("runs", k.runs, 0), ("paints", k.paints, 0))
                assert(shown.text == "a", s"the untaken branch showed: $shown")
            end for
        }

        "a branch the condition leaves has no observer once the condition flips" in {
            val runs = new Runs
            for
                c <- Signal.initRef(true)
                a <- Signal.initRef("a")
                b <- Signal.initRef("b")
                ui = UI.reactive {
                    runs.tick()
                    span.id("out")(if c.read then a.read else b.read)
                }
                r <- live(ui) { l =>
                    for
                        _        <- observed(c)
                        _        <- settle(c, false)
                        _        <- observed(b)
                        _        <- assertEventually(a.waiters.map(_ == 0), within)
                        waitingA <- a.waiters
                        waitingB <- b.waiters
                        shown    <- l.now
                    yield (waitingA, waitingB, shown)
                }
            yield
                val (waitingA, waitingB, shown) = r
                report("cost", "branch left after a flip")(("observers", waitingA, 0))
                assert(waitingA == 0 && waitingB == 1, s"observers of the left branch $waitingA, of the taken one $waitingB")
                assert(shown.text == "b", s"after the flip it showed: $shown")
            end for
        }

        "a channel in the branch taken patches its attribute in place and draws nothing again" in {
            for
                c <- Signal.initRef(true)
                a <- Signal.initRef(1)
                ui = UI.reactive(if c.read then span.id("out").title(s"t${a.read}")("x") else span.id("out")("y"))
                k <- live(ui)(l => observed(a).andThen(cost(l, new Runs)(settle(a, 2).andThen(shows(l)(_.title == "t2")))))
            yield
                report("cost", "channel in the branch taken")(("paints", k.paints, 0), ("patches", k.patches, 1))
                assert(k.paints == 0 && k.patches <= 1, s"$k")
            end for
        }

        "a case the selector leaves has no observer once the selector flips" in {
            val runs = new Runs
            for
                k <- Signal.initRef(0)
                a <- Signal.initRef("a")
                b <- Signal.initRef("b")
                ui = UI.reactive {
                    runs.tick()
                    span.id("out")(k.read match
                        case 0 => a.read
                        case _ => b.read)
                }
                r <- live(ui) { l =>
                    for
                        _        <- observed(k)
                        _        <- settle(k, 1)
                        _        <- observed(b)
                        _        <- assertEventually(a.waiters.map(_ == 0), within)
                        waitingA <- a.waiters
                        waitingB <- b.waiters
                        shown    <- l.now
                        k        <- cost(l, runs)(settle(a, "a2"))
                    yield (waitingA, waitingB, shown, k)
                }
            yield
                val (waitingA, waitingB, shown, k) = r
                report("cost", "case left after a flip")(("observers", waitingA, 0), ("runs", k.runs, 0), ("paints", k.paints, 0))
                assert(waitingA == 0 && waitingB == 1, s"observers of the case left $waitingA, of the case taken $waitingB")
                assert(shown.text == "b", s"after the flip it showed: $shown")
                assert(k.runs == 0 && k.paints == 0 && k.patches == 0, s"a write to the case left cost $k")
            end for
        }

        "a case nested in an element that the selector leaves has no observer, and a write to it draws nothing" in {
            for
                k <- Signal.initRef(0)
                n <- Signal.initRef(2)
                ui = UI.reactive(span.id("out")(k.read match
                    case 0 => div(Seq.tabulate(n.read)(i => span(s"$i"))*)
                    case _ => span("off")))
                r <- live(ui) { l =>
                    for
                        _        <- observed(n)
                        _        <- settle(k, 1)
                        _        <- shows(l)(_.text == "off")
                        _        <- assertEventually(n.waiters.map(_ == 0), within)
                        waitingN <- n.waiters
                        c        <- cost(l, new Runs)(settle(n, 3))
                        _        <- settle(k, 0)
                        _        <- shows(l)(_.text == "012")
                    yield (waitingN, c)
                }
            yield
                val (waitingN, c) = r
                report("cost", "case nested in an element left")(("observers", waitingN, 0), ("paints", c.paints, 0))
                assert(waitingN == 0 && c.paints == 0 && c.patches == 0, s"observers $waitingN, write cost $c")
            end for
        }

        "an untaken UI branch nested in an element has no observer, and a write to it draws nothing" in {
            for
                c <- Signal.initRef(true)
                n <- Signal.initRef(2)
                ui = UI.reactive(span.id("out")(if c.read then div(Seq.tabulate(n.read)(i => span(s"$i"))*) else span("off")))
                r <- live(ui) { l =>
                    for
                        _        <- observed(n)
                        _        <- settle(c, false)
                        _        <- shows(l)(_.text == "off")
                        _        <- assertEventually(n.waiters.map(_ == 0), within)
                        waitingN <- n.waiters
                        k        <- cost(l, new Runs)(settle(n, 3))
                        _        <- settle(c, true)
                        _        <- observed(n)
                        shown    <- l.now
                    yield (waitingN, k, shown)
                }
            yield
                val (waitingN, k, shown) = r
                report("cost", "untaken UI branch nested in an element")(("observers", waitingN, 0), ("paints", k.paints, 0))
                assert(waitingN == 0 && k.paints == 0 && k.patches == 0, s"observers $waitingN, write cost $k")
                assert(shown.text == "012", s"after the flip it showed: $shown")
            end for
        }

        "a read behind a condition that reads nothing and is false" in {
            val runs = new Runs
            for
                s <- Signal.initRef("x")
                t <- Signal.initRef(0)
                ui = UI.reactive {
                    runs.tick()
                    span.id("out")(s"${t.read}:" + (if false && s.read.nonEmpty then "on" else "none"))
                }
                r <- live(ui) { l =>
                    for
                        // `t` is always read, so once it has its observer the block waits for its next value, on whatever it reads.
                        _       <- observed(t)
                        waiting <- s.waiters
                        k       <- cost(l, runs)(settle(s, "y"))
                    yield (waiting, k)
                }
            yield
                val (waiting, k) = r
                report(
                    "cost",
                    "read behind a false read-free guard"
                )(("observers", waiting, 0), ("runs", k.runs, 0), ("paints", k.paints, 0))
                assert(k.paints <= 1 && k.runs <= 1, s"$k")
            end for
        }

        "an input that changes while what it decides does not" in {
            val runs = new Runs
            for
                x <- Signal.initRef(1)
                ui = UI.reactive {
                    runs.tick()
                    span.id("out")(if x.read > 0 then "pos" else "neg")
                }
                k <- live(ui)(l => observed(x).andThen(cost(l, runs)(settle(x, 2))))
            yield
                report("cost", "output unchanged")(("runs", k.runs, 1), ("paints", k.paints, 0))
                assert(k.runs <= 1 && k.paints <= 1, s"$k")
            end for
        }

        "the diamond runs once per change of its source" in {
            val runs = new Runs
            for
                x <- Signal.initRef(1)
                d  = x.map(_ * 2)
                ui = UI.reactive {
                    runs.tick()
                    span.id("out")(s"${d.read + x.read}")
                }
                k <- live(ui)(l => observed(x).andThen(cost(l, runs)(settle(x, 2).andThen(shows(l)(_.text == "6")))))
            yield
                report("cost", "diamond")(("runs", k.runs, 1), ("paints", k.paints, 1))
                assert(k.runs <= 1 && k.paints <= 1, s"$k")
            end for
        }

        "an inner block's change leaves the outer one alone" in {
            val outer = new Runs
            val inner = new Runs
            for
                o <- Signal.initRef(true)
                i <- Signal.initRef("a")
                ui = UI.reactive {
                    outer.tick()
                    div(if o.read then UI.reactive { inner.tick(); span.id("out")(i.read) } else span("off"))
                }
                r <- live(ui) { l =>
                    for
                        _  <- observed(o)
                        o0 <- outer.count
                        k  <- cost(l, inner)(settle(i, "b").andThen(shows(l)(_.text == "b")))
                        o1 <- outer.count
                    yield (o1 - o0, k)
                }
            yield
                val (outerRuns, k) = r
                report("cost", "nested inner change")(("outer runs", outerRuns, 0), ("inner runs", k.runs, 1), ("paints", k.paints, 1))
                assert(outerRuns == 0 && k.runs <= 1 && k.paints <= 1, s"outer $outerRuns, $k")
            end for
        }

        "a signal read only in a handler has no observer" in {
            for
                a   <- Signal.initRef(1)
                hit <- Signal.initRef(0)
                ui = UI.reactive(button("go").id("go").onClick(hit.set(a.read)))
                waiting <- live(ui)(_ => a.waiters)
            yield
                report("cost", "handler-only read")(("observers", waiting, 0))
                assert(waiting == 0, s"$waiting observers")
        }

        "a block over fifty signals runs once per change of one" in {
            val runs = new Runs
            for
                refs <- Kyo.fill(50)(Signal.initRef(1))
                sigs = Chunk.from(refs)
                ui   = UI.reactive {
                    runs.tick()
                    span.id("out")(s"${sigs(0).read + sigs(10).read + sigs(20).read + sigs(30).read + sigs(40).read + sigs(49).read}")
                }
                r <- live(ui) { l =>
                    for
                        _       <- observed(sigs(49))
                        waiting <- sigs(10).waiters
                        k       <- cost(l, runs)(settle(sigs(10), 5).andThen(shows(l)(_.text == "10")))
                    yield (waiting, k)
                }
            yield
                val (waiting, k) = r
                report("cost", "fan-in of six reads")(("observers per input", waiting, 1), ("runs", k.runs, 1), ("paints", k.paints, 1))
                assert(k.runs <= 1 && k.paints <= 1, s"$k")
            end for
        }

        "a thousand writes to an attribute's read are patches alone" in {
            val runs = new Runs
            for
                a <- Signal.initRef(0)
                ui = UI.reactive {
                    runs.tick()
                    span.id("out").title(s"${a.read}")("x")
                }
                r <- live(ui) { l =>
                    for
                        _  <- observed(a)
                        t0 <- Sync.defer(java.lang.System.nanoTime())
                        k  <- cost(l, runs)(Kyo.foreachDiscard(1 to 1000)(n => a.set(n)).andThen(shows(l)(_.title == "1000")))
                        t1 <- Sync.defer(java.lang.System.nanoTime())
                    yield (k, (t1 - t0) / 1000000)
                }
            yield
                val (k, ms) = r
                report(
                    "cost",
                    s"1000 attribute writes (${ms} ms)"
                )(("runs", k.runs, 0), ("paints", k.paints, 0), ("patches", k.patches, 1000))
                assert(k.runs == 0 && k.paints == 0 && k.patches <= 1000, s"$k")
            end for
        }

        "a thousand writes to a structural read draw at most once each" in {
            val runs = new Runs
            for
                a <- Signal.initRef(0)
                ui = UI.reactive {
                    runs.tick()
                    span.id("out")(s"v=${a.read}")
                }
                r <- live(ui) { l =>
                    for
                        _  <- observed(a)
                        t0 <- Sync.defer(java.lang.System.nanoTime())
                        k  <- cost(l, runs)(Kyo.foreachDiscard(1 to 1000)(n => a.set(n)).andThen(shows(l)(_.text == "v=1000")))
                        t1 <- Sync.defer(java.lang.System.nanoTime())
                    yield (k, (t1 - t0) / 1000000)
                }
            yield
                val (k, ms) = r
                report("cost", s"1000 structural writes (${ms} ms)")(("runs", k.runs, 1000), ("paints", k.paints, 1000))
                assert(k.runs <= 1000 && k.paints <= 1000, s"$k")
            end for
        }
    }

    // ---- Expressiveness ----

    /** `yes` where `code` compiles, else `no` and the compiler's first message, on one line. Each snippet brings its own imports and
      * fixtures, as the probe types it apart from this file. `transparent`, or the probe types a block without the scope its reads
      * need, and a plain read reads as one outside a block.
      */
    private transparent inline def compiles(inline code: String): String =
        val errors = typeCheckErrors(code)
        if errors.isEmpty then "yes" else s"no: ${errors.head.message.linesIterator.nextOption().getOrElse("")}"

    "expressiveness" - {
        "which ways of writing a read compile (reported)" in {
            // The probe itself: a read written in place has to compile, or every line below says nothing.
            val baseline = compiles("""
                import kyo.*
                import kyo.UI.*
                val a: UI.Prop[Int] = 1
                UI.reactive(span(s"${a.read}"))
            """)
            val cases = Seq(
                "a read in a condition, then in its branch" -> compiles("""
                    import kyo.*
                    import kyo.UI.*
                    val a: UI.Prop[Int] = 1
                    UI.reactive(span(if a.read > 0 then s"${a.read}" else "no"))
                """),
                "a read in a match guard" -> compiles("""
                    import kyo.*
                    import kyo.UI.*
                    val a: UI.Prop[Int] = 1
                    UI.reactive(span(1 match { case v if v < a.read => "lt"; case _ => "ge" }))
                """),
                "a read in a try" -> compiles("""
                    import kyo.*
                    import kyo.UI.*
                    val a: UI.Prop[Int] = 1
                    UI.reactive(span(try s"${a.read}" catch { case _: Exception => "e" }))
                """),
                "a read in a lambda over a list" -> compiles("""
                    import kyo.*
                    import kyo.UI.*
                    val a: UI.Prop[Int] = 1
                    UI.reactive(div(Seq(1, 2).map(i => span(s"${i + a.read}"): UI)*))
                """),
                "a read in a for over a range" -> compiles("""
                    import kyo.*
                    import kyo.UI.*
                    val a: UI.Prop[Int] = 1
                    UI.reactive(div((for i <- 1 to 3 yield span(s"${i + a.read}"): UI)*))
                """),
                "a read in a local def" -> compiles("""
                    import kyo.*
                    import kyo.UI.*
                    val a: UI.Prop[Int] = 1
                    UI.reactive { def v = a.read; span(s"$v") }
                """),
                "a read in a lazy val" -> compiles("""
                    import kyo.*
                    import kyo.UI.*
                    val a: UI.Prop[Int] = 1
                    UI.reactive { lazy val v = a.read; span(s"$v") }
                """),
                "a read in a by-name argument" -> compiles("""
                    import kyo.*
                    import kyo.UI.*
                    def later(v: => Int): Int = v
                    val a: UI.Prop[Int]       = 1
                    UI.reactive(span(s"${later(a.read)}"))
                """),
                "a read in a helper that takes the block's scope" -> compiles("""
                    import kyo.*
                    import kyo.UI.*
                    import kyo.uic.ReactiveScope
                    def shown(a: UI.Prop[Int])(using ReactiveScope): String = s"${a.read}"
                    val a: UI.Prop[Int] = 1
                    UI.reactive(span(shown(a)))
                """),
                "a block of its own in a lambda" -> compiles("""
                    import kyo.*
                    import kyo.UI.*
                    val a: UI.Prop[Int] = 1
                    UI.reactive(div(Seq(1, 2).map(i => UI.reactive(span(s"${i + a.read}")): UI)*))
                """),
                "a read of a value the block defines" -> compiles("""
                    import kyo.*
                    import kyo.UI.*
                    UI.reactive { val p: UI.Prop[Int] = 2; span(s"${p.read}") }
                """),
                "a read of a signal the block makes" -> compiles("""
                    import kyo.*
                    import kyo.UI.*
                    val a: Signal[Int] = Signal.initConst(1)
                    UI.reactive(span(s"${a.map(_ + 1).read}"))
                """)
            )
            println(s"REACTIVE|expressiveness|the probe: a read written in place|compiles=$baseline")
            cases.foreach((name, ok) => println(s"REACTIVE|expressiveness|$name|compiles=$ok"))
            assert(baseline == "yes", s"the probe does not compile a plain read: $baseline")
        }
    }
end ReactiveContractTest
