package kyo

import kyo.internal.HtmlRenderer
import kyo.internal.KeyboardEventData
import kyo.internal.MouseEventData
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.RecordingExchange
import kyo.internal.UIEvent
import kyo.internal.UIExchange

/** What [[kyo.UI.boundary]] shows while the mounts it catches load and when they fail, judged by what a user would see.
  *
  * The [[Screen]] applies every paint to the page the way the client does, replacing the content between a range's markers, and
  * reads the visible text of the result: text inside an element with the `hidden` attribute or inside a `template` is not visible.
  * Tests wait for a state of the screen or for a step of an effect through promises, never by polling.
  */
class BoundaryTest extends kyo.test.Test[Any]:

    import UI.*

    private class Screen(initial: String) extends UIExchange:
        import AllowUnsafe.embrace.danger

        private val page    = AtomicRef.Unsafe.init(initial)
        private val history = AtomicRef.Unsafe.init(Chunk(Screen.visible(initial)))
        private val painted = AtomicRef.Unsafe.init(Chunk.empty[Seq[String]])
        private val waiters = AtomicRef.Unsafe.init(Chunk.empty[(String => Boolean, Promise[Unit, Any])])

        def onChange(
            region: ReactiveRegion,
            path: Seq[String],
            context: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            ui: UI
        )(using Frame): Unit < Async =
            // Rendered the way the WebSocket transport renders a region, so nested ranges carry the ids the engine addresses.
            val mode =
                if ReactiveRegion.owns(region, context) then ReactiveRegion.BoundaryMode.Suppress else ReactiveRegion.BoundaryMode.Emit
            HtmlRenderer.renderRegionWithCss(ui, path, context, region, parentContext, mode).map { (inner, _) =>
                Sync.Unsafe.defer {
                    region match
                        case ReactiveRegion.HtmlRange(id) => discard(page.getAndUpdate(Screen.splice(_, id, inner)))
                        case _                            => ()
                    val shown = Screen.visible(page.get())
                    discard(history.getAndUpdate(_.append(shown)))
                    discard(painted.getAndUpdate(_.append(path)))
                    var ready = Chunk.empty[Promise[Unit, Any]]
                    discard(waiters.getAndUpdate { all =>
                        val (met, rest) = all.partition(_._1(shown))
                        ready = met.map(_._2)
                        rest
                    })
                    ready
                }.map(ready => Kyo.foreachDiscard(ready)(_.completeUnitDiscard))
            }
        end onChange

        def now(using Frame): String < Sync  = Sync.Unsafe.defer(Screen.visible(page.get()))
        def html(using Frame): String < Sync = Sync.Unsafe.defer(page.get())

        /** Every state the screen went through, oldest first. */
        def states(using Frame): Chunk[String] < Sync = Sync.Unsafe.defer(history.get())

        /** How many paints replaced the range at exactly `path`, which replaces every node inside it. */
        def paintsAt(path: Seq[String])(using Frame): Int < Sync = Sync.Unsafe.defer(painted.get().count(_ == path))

        /** Waits until the visible text satisfies `pred`. */
        def await(label: String)(pred: String => Boolean)(using Frame, kyo.test.AssertScope): Unit < Async =
            for
                promise <- Promise.init[Unit, Any]
                _       <- Sync.Unsafe.defer(discard(waiters.getAndUpdate(_.append((pred, promise)))))
                current <- now
                _       <- if pred(current) then promise.completeUnitDiscard else Kyo.unit
                _       <- promise.get
            yield ()
    end Screen

    private object Screen:
        /** Replaces what range `id` holds. A start marker may carry flags after the id (`<!--kyo-rs:r.1 s-->`). */
        def splice(page: String, id: String, inner: String): String =
            val open  = s"<!--kyo-rs:$id"
            val start = Iterator.iterate(page.indexOf(open))(at => page.indexOf(open, at + 1)).takeWhile(_ >= 0)
                .find(at => page.startsWith("-->", at + open.length) || page.startsWith(" ", at + open.length))
                .getOrElse(-1)
            val opened = if start < 0 then -1 else page.indexOf("-->", start) + 3
            val end    = if start < 0 then -1 else page.indexOf(s"<!--kyo-re:$id-->", opened)
            if start < 0 || end < 0 then page
            else page.substring(0, opened) + inner + page.substring(end)
        end splice

        private val voids    = Set("area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr")
        private val hiddenAt = """(^|\s)hidden(\s|=|/|$)""".r

        def visible(html: String): String =
            val out    = new StringBuilder
            var hidden = List.empty[Boolean]
            var i      = 0
            while i < html.length do
                if html.startsWith("<!--", i) then
                    val end = html.indexOf("-->", i)
                    i = if end < 0 then html.length else end + 3
                else if html.charAt(i) == '<' then
                    val end = html.indexOf('>', i)
                    val tag = html.substring(i + 1, if end < 0 then html.length else end)
                    i = if end < 0 then html.length else end + 1
                    if tag.startsWith("/") then hidden = hidden.drop(1)
                    else
                        val name = tag.takeWhile(c => !c.isWhitespace && c != '/').toLowerCase
                        if !tag.endsWith("/") && !voids(name) then
                            val inherited = hidden.headOption.getOrElse(false)
                            hidden = (inherited || name == "template" || hiddenAt.findFirstIn(tag).nonEmpty) :: hidden
                    end if
                else
                    val next = html.indexOf('<', i) match
                        case -1 => html.length
                        case n  => n
                    if !hidden.headOption.getOrElse(false) then discard(out.append(html.substring(i, next)).append(' '))
                    i = next
                end if
            end while
            out.toString
        end visible
    end Screen

    private def mount[A](ui: UI)(f: Screen => A < (Async & Scope))(using Frame): A < Async =
        mountWith(ui)((screen, _) => f(screen))

    /** Also hands out the subscription's event handler, which events go to as if the user clicked on the page. */
    private def mountWith[A](ui: UI)(f: (Screen, (Seq[String], UIEvent) => Boolean < Async) => A < (Async & Scope))(using
        Frame
    ): A < Async =
        Scope.run {
            for
                initial <- HtmlRenderer.render(ui, Seq.empty)
                screen = new Screen(initial)
                root <- ReactiveUI.normalize(ui, Seq.empty)
                sub  <- ReactiveUI.subscribe(root, screen)
                a    <- f(screen, sub.handle)
            yield a
        }

    private def count(text: String, of: String): Int = text.split(java.util.regex.Pattern.quote(of), -1).length - 1

    private case class Boom(msg: String) extends Exception(msg)
    private case object Missing          extends Exception("missing")

    private def gated(gate: Promise[Unit, Any], text: String)(using Frame): UI.Ast.Mounted = UI.mounted(gate.get.andThen(span(text)))

    private def failing(msg: String)(using Frame): UI.Ast.Mounted = UI.mounted(Abort.fail(Boom(msg)))

    /** A mount that completes `p` when it starts, which is when the content around it is subscribed, and shows nothing. */
    private def reporter(p: Promise[Unit, Any])(using Frame): UI.Ast.Mounted = UI.mounted(p.completeUnitDiscard.andThen(UI.empty))

    private val card: PartialFunction[MountError, UI] = { case e => span(s"card:${e.message}") }

    // ---- Keyboard, focus, input and submit ----

    private val kinds = Chunk("keydown", "keyup", "focus", "blur", "input", "change", "submit")

    /** A form showing `name` and one text field, which logs every keyboard, focus, input and submit event it receives as `name:kind`. */
    private def probe(name: String, log: AtomicRef[Chunk[String]])(using Frame): UI =
        def note(kind: String) = log.getAndUpdate(_.append(s"$name:$kind")).unit
        form(
            span(name),
            input
                .onKeyDown(_ => note("keydown"))
                .onKeyUp(_ => note("keyup"))
                .onFocus(note("focus"))
                .onBlur(note("blur"))
                .onInput(_ => note("input"))
                .onChange(_ => note("change"))
        ).onSubmit(note("submit"))
    end probe

    /** Sends one event of every kind to the probe whose form is at `formPath`: the field's to its field, the submit to the form. */
    private def sendAll(handle: (Seq[String], UIEvent) => Boolean < Async, formPath: Seq[String])(using Frame): Unit < Async =
        val field    = formPath :+ "1"
        val mouse    = MouseEventData(UI.Modifiers.none, Absent)
        val keyboard = KeyboardEventData("a", UI.Modifiers.none, Absent)
        Kyo.foreachDiscard(Seq(
            field    -> UIEvent.KeyDown(field, keyboard),
            field    -> UIEvent.KeyUp(field, keyboard),
            field    -> UIEvent.Focus(field, mouse),
            field    -> UIEvent.Blur(field, mouse),
            field    -> UIEvent.Input(field, "x"),
            field    -> UIEvent.Change(field, "x"),
            formPath -> UIEvent.Submit(formPath, mouse)
        ))((path, event) => handle(path, event).unit)
    end sendAll

    /** How often `name` received each kind, in the order of [[kinds]]. */
    private def received(log: Chunk[String], name: String): Chunk[Int] = kinds.map(kind => log.count(_ == s"$name:$kind"))

    // ---- One mount: its own modifiers and a boundary around it behave the same ----

    private case class Form(name: String, wrap: (UI.Ast.Mounted, Boundary) => UI)

    private val forms = Seq(
        Form(
            "modifiers",
            (m, b) =>
                val p = b.placeholderView match
                    case Present(ui) => m.placeholder(ui)
                    case Absent      => m
                val e = b.errorView match
                    case Present(pf) => p.onError(pf)
                    case Absent      => p
                b.pending match
                    case Present(policy) => e.whilePending(policy)
                    case Absent          => e
        ),
        Form("a boundary", (m, b) => b(m))
    )

    forms.foreach { form =>
        s"${form.name}: the placeholder shows until the mount publishes, then the content" in {
            for
                gate <- Promise.init[Unit, Any]
                ui = div(form.wrap(gated(gate, "ready"), UI.boundary.placeholder(span("loading"))))
                _ <- mount(ui) { screen =>
                    for
                        _ <- screen.await("loading")(_.contains("loading"))
                        _ <- gate.completeUnitDiscard
                        _ <- screen.await("ready")(v => v.contains("ready") && !v.contains("loading"))
                    yield ()
                }
            yield succeed
        }

        s"${form.name}: a failure shows the error view in the mount's place" in {
            val ui = div(span("before"), form.wrap(failing("boom"), UI.boundary.onError(card)), span("after"))
            mount(ui)(_.await("the error view between its siblings")(v => v.contains("before card:boom") && v.contains("after")))
                .andThen(succeed)
        }

        s"${form.name}: the placeholder waits for the mounts in the content of the mount" in {
            for
                started <- Promise.init[Unit, Any]
                inner   <- Promise.init[Unit, Any]
                outer = UI.mounted(Kyo.lift(div(
                    span("outer"),
                    UI.mounted(started.completeUnitDiscard.andThen(inner.get).andThen(span("inner")))
                )))
                ui = div(form.wrap(outer, UI.boundary.placeholder(span("loading"))))
                _ <- mount(ui) { screen =>
                    for
                        _     <- started.get
                        shown <- screen.now
                        _     <- inner.completeUnitDiscard
                        _     <- screen.await("both")(v => v.contains("outer") && v.contains("inner") && !v.contains("loading"))
                    yield assert(shown.contains("loading") && !shown.contains("outer"), s"while the nested mount loads: $shown")
                }
            yield succeed
        }

        s"${form.name}: under KeepLatest a new instance keeps the old content until it loads" in {
            for
                key     <- Signal.initRef("a")
                started <- Promise.init[Unit, Any]
                gateB   <- Promise.init[Unit, Any]
                effect = (k: String) =>
                    if k == "a" then Kyo.lift(span("content:a"))
                    else started.completeUnitDiscard.andThen(gateB.get).andThen(span("content:b"))
                ui = div(key.render(k =>
                    div(span(s"key:$k"), form.wrap(UI.mounted(effect(k)).keyed(k), UI.boundary.placeholder(span("loading"))))
                ))
                _ <- mount(ui) { screen =>
                    for
                        _ <- screen.await("content:a")(_.contains("content:a"))
                        _ <- key.set("b")
                        _ <- started.get
                        _ <- screen.await("the old content under the new key")(v =>
                            v.contains("key:b") && v.contains("content:a") && !v.contains("loading")
                        )
                        _ <- gateB.completeUnitDiscard
                        _ <- screen.await("content:b")(v => v.contains("content:b") && !v.contains("content:a"))
                    yield ()
                }
            yield succeed
        }

        s"${form.name}: a click on the placeholder reaches it while the mount loads, and the content's handler after" in {
            for
                cancels <- AtomicInt.init(0)
                readies <- AtomicInt.init(0)
                gate    <- Promise.init[Unit, Any]
                content = UI.mounted(gate.get.andThen(button("ready").onClick(readies.incrementAndGet)))
                ui      = div(form.wrap(content, UI.boundary.placeholder(button("cancel").onClick(cancels.incrementAndGet))))
                // The placeholder and, once it publishes, the content are each the root of the node's range at path 0.
                target = Seq("0")
                _ <- mountWith(ui) { (screen, handle) =>
                    def click = handle(target, UIEvent.Click(target, MouseEventData(UI.Modifiers.none, Absent)))
                    for
                        _       <- screen.await("cancel")(_.contains("cancel"))
                        _       <- click
                        pending <- cancels.get
                        _       <- gate.completeUnitDiscard
                        _       <- screen.await("ready")(v => v.contains("ready") && !v.contains("cancel"))
                        _       <- click
                        after   <- cancels.get
                        ready   <- readies.get
                    yield
                        assert(pending == 1, s"the click on the placeholder reached it $pending times")
                        assert(after == 1 && ready == 1, s"after the reveal: placeholder ${after - 1} more, content $ready")
                    end for
                }
            yield succeed
        }

        s"${form.name}: keyboard, focus, input and submit reach the placeholder while the mount loads, and the content after" in {
            for
                log  <- AtomicRef.init(Chunk.empty[String])
                gate <- Promise.init[Unit, Any]
                content = UI.mounted(gate.get.andThen(probe("content", log)))
                ui      = div(form.wrap(content, UI.boundary.placeholder(probe("placeholder", log))))
                // The placeholder's form and, once the mount publishes, the content's are the root of the node's range at path 0.
                at = Seq("0")
                _ <- mountWith(ui) { (screen, handle) =>
                    for
                        _       <- screen.await("the placeholder")(_.contains("placeholder"))
                        _       <- sendAll(handle, at)
                        pending <- log.get
                        _       <- gate.completeUnitDiscard
                        _       <- screen.await("the content")(v => v.contains("content") && !v.contains("placeholder"))
                        _       <- sendAll(handle, at)
                        after   <- log.get
                    yield
                        val once = kinds.map(_ => 1)
                        assert(received(pending, "placeholder") == once, s"while loading: $pending")
                        assert(received(after, "placeholder") == once && received(after, "content") == once, s"after the reveal: $after")
                    end for
                }
            yield succeed
        }

        s"${form.name}: under Placeholder a new instance shows the placeholder again" in {
            for
                key   <- Signal.initRef("a")
                gateB <- Promise.init[Unit, Any]
                effect = (k: String) => if k == "a" then Kyo.lift(span("content:a")) else gateB.get.andThen(span("content:b"))
                b      = UI.boundary.placeholder(span("loading")).whilePending(PendingPolicy.Placeholder)
                ui     = div(key.render(k => div(span(s"key:$k"), form.wrap(UI.mounted(effect(k)).keyed(k), b))))
                _ <- mount(ui) { screen =>
                    for
                        _ <- screen.await("content:a")(_.contains("content:a"))
                        _ <- key.set("b")
                        _ <- screen.await("the placeholder under the new key")(v =>
                            v.contains("key:b") && v.contains("loading") && !v.contains("content:a")
                        )
                        _ <- gateB.completeUnitDiscard
                        _ <- screen.await("content:b")(v => v.contains("content:b") && !v.contains("loading"))
                    yield ()
                }
            yield succeed
        }
    }

    // ---- Several mounts: one placeholder, one reveal ----

    "a boundary around several mounts is server-rendered as its placeholder, once, in its own place" in {
        for
            a <- Promise.init[Unit, Any]
            b <- Promise.init[Unit, Any]
            ui = div(
                span("head"),
                UI.boundary.placeholder(span("skel"))(div(gated(a, "A").placeholder(span("ownA")), gated(b, "B"), gated(b, "C"))),
                span("foot")
            )
            html <- HtmlRenderer.render(ui, Seq.empty)
            shown = Screen.visible(html)
        yield assert(shown.contains("head skel") && shown.contains("foot") && count(shown, "skel") == 1 && !shown.contains("ownA"), shown)
    }

    "a boundary reveals its content at once, when its last mount is ready" in {
        for
            a <- Promise.init[Unit, Any]
            b <- Promise.init[Unit, Any]
            first = UI.mounted(a.get.andThen(div(span("A"), reporter(b))))
            ui    = div(UI.boundary.placeholder(span("skel"))(div(first, gated(b, "B"))))
            states <- mount(ui) { screen =>
                for
                    _ <- screen.await("skel")(v => count(v, "skel") == 1)
                    _ <- a.completeUnitDiscard
                    _ <- screen.await("A and B")(v => v.contains("A") && v.contains("B") && !v.contains("skel"))
                    s <- screen.states
                yield s
            }
            halfway = states.filter(v => v.contains("A") != v.contains("B") || (v.contains("skel") && v.contains("A")))
        yield assert(halfway.isEmpty, s"half-revealed states: $halfway")
    }

    "a mount with its own placeholder does not hold the boundary" in {
        for
            a <- Promise.init[Unit, Any]
            b <- Promise.init[Unit, Any]
            ui = div(UI.boundary.placeholder(span("skel"))(div(gated(a, "A"), gated(b, "B").placeholder(span("ownB")))))
            _ <- mount(ui) { screen =>
                for
                    _ <- screen.await("skel alone")(v => v.contains("skel") && !v.contains("ownB"))
                    _ <- a.completeUnitDiscard
                    _ <- screen.await("A beside its sibling's own placeholder")(v =>
                        v.contains("A") && v.contains("ownB") && !v.contains("skel")
                    )
                    _ <- b.completeUnitDiscard
                    _ <- screen.await("B")(v => v.contains("B") && !v.contains("ownB"))
                yield ()
            }
        yield succeed
    }

    "a boundary inside another one catches its own mounts" in {
        for
            a <- Promise.init[Unit, Any]
            b <- Promise.init[Unit, Any]
            ui = div(UI.boundary.placeholder(span("outer"))(div(gated(a, "A"), UI.boundary.placeholder(span("inner"))(gated(b, "B")))))
            _ <- mount(ui) { screen =>
                for
                    _ <- screen.await("the outer placeholder alone")(v => v.contains("outer") && !v.contains("inner"))
                    _ <- a.completeUnitDiscard
                    _ <- screen.await("A beside the inner placeholder")(v => v.contains("A") && v.contains("inner") && !v.contains("outer"))
                    _ <- b.completeUnitDiscard
                    _ <- screen.await("B")(v => v.contains("B") && !v.contains("inner"))
                yield ()
            }
        yield succeed
    }

    "a boundary without a placeholder does not wait" in {
        for
            a <- Promise.init[Unit, Any]
            b <- Promise.init[Unit, Any]
            ui = div(UI.boundary.onError(card)(div(gated(a, "A"), gated(b, "B"))))
            _ <- mount(ui) { screen =>
                for
                    _ <- a.completeUnitDiscard
                    _ <- screen.await("A alone")(v => v.contains("A") && !v.contains("B"))
                yield ()
            }
        yield succeed
    }

    "a failed mount counts as loaded and shows the error view in its own place" in {
        for
            b    <- Promise.init[Unit, Any]
            boom <- Promise.init[Unit, Any]
            ui = div(UI.boundary.placeholder(span("skel")).onError(card)(div(
                UI.mounted(boom.get.andThen(Abort.fail(Boom("x")))),
                UI.mounted(b.get.andThen(div(span("B"), reporter(boom))))
            )))
            states <- mount(ui) { screen =>
                for
                    _ <- b.completeUnitDiscard
                    _ <- screen.await("the error view beside B")(v => v.contains("card:x") && v.contains("B") && !v.contains("skel"))
                    s <- screen.states
                yield s
            }
            early = states.filter(v => v.contains("B") && !v.contains("card:x"))
        yield assert(early.isEmpty, s"revealed before the failure: $early")
    }

    "a boundary whose mounts all fail reveals their error views" in {
        val ui = div(UI.boundary.placeholder(span("skel")).onError(card)(div(failing("x"), failing("y"))))
        mount(ui)(_.await("both error views")(v => v.contains("card:x") && v.contains("card:y") && !v.contains("skel"))).andThen(succeed)
    }

    // ---- Error views across boundaries ----

    "an error view that does not match a failure leaves it to the boundary around it" in {
        val inner: PartialFunction[MountError, UI] = { case e if e.error == Missing => span("inner:missing") }
        val outer: PartialFunction[MountError, UI] = { case e => span(s"outer:${e.message}") }
        val ui = div(UI.boundary.onError(outer)(div(UI.boundary.onError(inner)(div(UI.mounted(Abort.fail(Missing)), failing("x"))))))
        mount(ui)(_.await("both views")(v => v.contains("inner:missing") && v.contains("outer:x"))).andThen(succeed)
    }

    "a mount's own onError wins over the boundary around it, which still catches its siblings" in {
        val ui = div(UI.boundary.onError(card)(div(failing("a").onError(e => span(s"own:${e.message}")), failing("b"))))
        mount(ui)(_.await("own:a beside card:b")(v => v.contains("own:a") && v.contains("card:b") && !v.contains("card:a")))
            .andThen(succeed)
    }

    "a boundary with a placeholder and no mounts shows its content at once" in {
        val ui = div(UI.boundary.placeholder(span("skel"))(div(span("static"))))
        for
            html <- HtmlRenderer.render(ui, Seq.empty)
            served = Screen.visible(html)
            states <- mount(ui)(screen => screen.await("static")(_.contains("static")).andThen(screen.states))
        yield assert(
            served.contains("static") && !served.contains("skel") && states.forall(!_.contains("skel")),
            s"served: $served; states: $states"
        )
        end for
    }

    "a failure no error view matches gets the built-in error node" in {
        val only: PartialFunction[MountError, UI] = { case e if e.error == Missing => span("missing") }
        val ui                                    = div(UI.boundary.onError(only)(div(failing("other"))))
        mount(ui) { screen =>
            for
                _    <- screen.await("the failure")(_.contains("other"))
                html <- screen.html
            yield assert(html.contains("kyo-mount-error"), html)
        }
    }

    "an error view reaches a mount in a region below the boundary, also after the region re-renders" in {
        for
            tick <- Signal.initRef(0)
            ui = div(UI.boundary.onError(card)(div(tick.render(n => div(failing(s"boom$n"))))))
            _ <- mount(ui) { screen =>
                for
                    _ <- screen.await("card:boom0")(_.contains("card:boom0"))
                    _ <- tick.set(1)
                    _ <- screen.await("card:boom1")(_.contains("card:boom1"))
                yield ()
            }
        yield succeed
    }

    "an error view reaches a mount in the content of another mount" in {
        val ui = div(UI.boundary.onError(card)(div(UI.mounted(Kyo.lift(div(failing("inner")))))))
        mount(ui)(_.await("card:inner")(_.contains("card:inner"))).andThen(succeed)
    }

    "an error view reaches a mount that fails after it published" in {
        for
            feed <- Promise.init[Unit, Any]
            ui = div(UI.boundary.onError(card)(div(UI.mounted(UI.fork(feed.get.andThen(Abort.fail(Boom("late")))).andThen(span("up"))))))
            _ <- mount(ui) { screen =>
                for
                    _ <- screen.await("up")(_.contains("up"))
                    _ <- feed.completeUnitDiscard
                    _ <- screen.await("card:late")(v => v.contains("card:late") && !v.contains("up"))
                yield ()
            }
        yield succeed
    }

    "an error view does not reach a sibling outside the boundary" in {
        val ui = div(UI.boundary.onError(card)(div()), div(failing("outside")))
        mount(ui) { screen =>
            for
                _    <- screen.await("the failure")(_.contains("outside"))
                html <- screen.html
            yield assert(html.contains("kyo-mount-error") && !html.contains("card:"), html)
        }
    }

    // ---- After the first reveal ----

    "a mount that appears after the boundary revealed does not bring the placeholder back" in {
        for
            items   <- Signal.initRef(Chunk("a"))
            gateA   <- Promise.init[Unit, Any]
            started <- Promise.init[Unit, Any]
            gateC   <- Promise.init[Unit, Any]
            row = (k: String) =>
                if k == "a" then gated(gateA, "A")
                else UI.mounted(started.completeUnitDiscard.andThen(gateC.get).andThen(span("C")))
            ui = div(UI.boundary.placeholder(span("skel"))(ul(items.foreachKeyed(identity)(k => li(row(k))))))
            _ <- mount(ui) { screen =>
                for
                    _     <- screen.await("skel")(_.contains("skel"))
                    _     <- gateA.completeUnitDiscard
                    _     <- screen.await("A")(v => v.contains("A") && !v.contains("skel"))
                    _     <- items.set(Chunk("a", "c"))
                    _     <- started.get
                    shown <- screen.now
                    _     <- gateC.completeUnitDiscard
                    _     <- screen.await("C")(_.contains("C"))
                yield assert(shown.contains("A") && !shown.contains("skel"), s"while the new row loads: $shown")
            }
        yield succeed
    }

    "while a later mount loads, the rest of the boundary stays live and is not repainted" in {
        for
            items   <- Signal.initRef(Chunk("a"))
            counter <- Signal.initRef(0)
            started <- Promise.init[Unit, Any]
            gateC   <- Promise.init[Unit, Any]
            row = (k: String) =>
                if k == "a" then UI.mounted(Kyo.lift(span("A")))
                else UI.mounted(started.completeUnitDiscard.andThen(gateC.get).andThen(span("C")))
            ui = div(UI.boundary.placeholder(span("skel"))(div(
                counter.render(n => button(s"count:$n").onClick(counter.getAndUpdate(_ + 1).unit)),
                ul(items.foreachKeyed(identity)(k => li(row(k))))
            )))
            _ <- mountWith(ui) { (screen, handle) =>
                for
                    _      <- screen.await("A and count:0")(v => v.contains("A") && v.contains("count:0") && !v.contains("skel"))
                    before <- screen.paintsAt(Seq("0"))
                    _      <- items.set(Chunk("a", "c"))
                    _      <- started.get
                    _      <- handle(Seq("0", "0"), UIEvent.Click(Seq("0", "0"), MouseEventData(UI.Modifiers.none, Absent)))
                    _      <- screen.await("the click while C loads")(v => v.contains("count:1") && v.contains("A") && !v.contains("C"))
                    _      <- gateC.completeUnitDiscard
                    _      <- screen.await("C")(v => v.contains("C") && v.contains("count:1"))
                    after  <- screen.paintsAt(Seq("0"))
                yield assert(after == before, s"the boundary's whole range was repainted ${after - before} times")
            }
        yield succeed
    }

    "navigating to another page keeps the old page, live, until the new page and the mounts in its content have loaded" in {
        for
            route           <- Signal.initRef("1")
            counter         <- Signal.initRef(0)
            detailStarted   <- Promise.init[Unit, Any]
            detailGate      <- Promise.init[Unit, Any]
            commentsStarted <- Promise.init[Unit, Any]
            commentsGate    <- Promise.init[Unit, Any]
            // The issue tracker's detail page: a keyed mount whose content holds a keyed mount of its own, the comments.
            comments = (id: String) =>
                val load = if id == "1" then Kyo.unit else commentsStarted.completeUnitDiscard.andThen(commentsGate.get)
                UI.mounted(load.andThen(span(s"comments:$id"))).keyed(id)
            detail = (id: String) =>
                val load = if id == "1" then Kyo.unit else detailStarted.completeUnitDiscard.andThen(detailGate.get)
                UI.mounted(load.andThen(div(span(s"title:$id"), comments(id)))).keyed(id)
            ui = div(UI.boundary.placeholder(span("skel"))(div(
                counter.render(n => button(s"count:$n").onClick(counter.getAndUpdate(_ + 1).unit)),
                route.render(id => div(detail(id)))
            )))
            states <- mountWith(ui) { (screen, handle) =>
                val click = handle(Seq("0", "0"), UIEvent.Click(Seq("0", "0"), MouseEventData(UI.Modifiers.none, Absent)))
                for
                    _      <- screen.await("issue 1")(v => v.contains("title:1") && v.contains("comments:1") && !v.contains("skel"))
                    before <- screen.paintsAt(Seq("0"))
                    _      <- route.set("2")
                    _      <- detailStarted.get
                    _      <- click
                    _      <- screen.await("the click on the old page")(v => v.contains("count:1") && v.contains("title:1"))
                    _      <- detailGate.completeUnitDiscard
                    _      <- commentsStarted.get
                    shown  <- screen.now
                    _      <- commentsGate.completeUnitDiscard
                    _      <- screen.await("issue 2")(v =>
                        v.contains("title:2") && v.contains("comments:2") && !v.contains("title:1") && v.contains("count:1")
                    )
                    after <- screen.paintsAt(Seq("0"))
                    all   <- screen.states
                yield
                    assert(
                        shown.contains("title:1") && shown.contains("comments:1") && !shown.contains("title:2") && !shown.contains("skel"),
                        s"while issue 2's comments load: $shown"
                    )
                    assert(after == before, s"the boundary's whole range was repainted ${after - before} times")
                    all
                end for
            }
            halfway = states.filter(v => v.contains("title:2") && !v.contains("comments:2"))
        yield assert(halfway.isEmpty, s"issue 2 without its comments: $halfway")
    }

    "a click on the old page while the new one loads reaches no handler, and after the reveal reaches the new page" in {
        for
            route           <- Signal.initRef("1")
            likes1          <- AtomicInt.init(0)
            likes2          <- AtomicInt.init(0)
            headers         <- AtomicInt.init(0)
            detailRuns      <- AtomicInt.init(0)
            commentsRuns    <- AtomicInt.init(0)
            detailGate      <- Promise.init[Unit, Any]
            commentsStarted <- Promise.init[Unit, Any]
            commentsGate    <- Promise.init[Unit, Any]
            comments = (id: String) =>
                val load =
                    if id == "1" then Kyo.unit
                    else commentsRuns.incrementAndGet.andThen(commentsStarted.completeUnitDiscard).andThen(commentsGate.get)
                UI.mounted(load.andThen(span(s"comments:$id"))).keyed(id)
            detail = (id: String) =>
                val load  = if id == "1" then Kyo.unit else detailRuns.incrementAndGet.andThen(detailGate.get)
                val likes = if id == "1" then likes1 else likes2
                UI.mounted(load.andThen(div(
                    span(s"title:$id"),
                    button(s"like:$id").onClick(likes.incrementAndGet),
                    comments(id)
                ))).keyed(id)
            ui = div(UI.boundary.placeholder(span("skel"))(div(
                button("header").onClick(headers.incrementAndGet),
                route.render(id => div(detail(id)))
            )))
            // The like button: boundary 0, its div, the route region at 0.1, its div, the detail mount at 0.1.0, the button second in it.
            like   = Seq("0", "1", "0", "1")
            header = Seq("0", "0")
            _ <- mountWith(ui) { (screen, handle) =>
                def click(path: Seq[String]) = handle(path, UIEvent.Click(path, MouseEventData(UI.Modifiers.none, Absent)))
                for
                    _         <- screen.await("issue 1")(v => v.contains("like:1") && v.contains("comments:1"))
                    _         <- click(like)
                    onPage1   <- likes1.get
                    _         <- route.set("2")
                    _         <- detailGate.completeUnitDiscard
                    _         <- commentsStarted.get
                    shown     <- screen.now
                    _         <- click(like)
                    _         <- click(header)
                    waiting1  <- likes1.get
                    waiting2  <- likes2.get
                    onHeader  <- headers.get
                    _         <- commentsGate.completeUnitDiscard
                    _         <- screen.await("issue 2")(v => v.contains("like:2") && v.contains("comments:2"))
                    _         <- click(like)
                    revealed2 <- likes2.get
                    details   <- detailRuns.get
                    comments2 <- commentsRuns.get
                    html      <- screen.html
                yield
                    assert(onPage1 == 1, s"the click on issue 1 before navigating: $onPage1")
                    assert(shown.contains("like:1") && !shown.contains("like:2"), s"while issue 2 loads: $shown")
                    assert(
                        waiting1 == 1 && waiting2 == 0,
                        s"a click on the waiting page reached issue 1 ${waiting1 - 1}, issue 2 $waiting2 times"
                    )
                    assert(onHeader == 1, s"a click outside the waiting range, on the header, reached it $onHeader times")
                    assert(revealed2 == 1, s"the click after the reveal reached issue 2 $revealed2 times")
                    assert(details == 1 && comments2 == 1, s"issue 2 loaded $details times, its comments $comments2 times")
                    assert(!html.contains("kyo-mount-error"), html)
                end for
            }
        yield succeed
    }

    "keyboard, focus, input and submit on the old page while the new one loads reach no handler, outside it they do" in {
        for
            log             <- AtomicRef.init(Chunk.empty[String])
            route           <- Signal.initRef("1")
            detailGate      <- Promise.init[Unit, Any]
            commentsStarted <- Promise.init[Unit, Any]
            commentsGate    <- Promise.init[Unit, Any]
            comments = (id: String) =>
                val load = if id == "1" then Kyo.unit else commentsStarted.completeUnitDiscard.andThen(commentsGate.get)
                UI.mounted(load.andThen(span(s"comments:$id"))).keyed(id)
            detail = (id: String) =>
                val load = if id == "1" then Kyo.unit else detailGate.get
                UI.mounted(load.andThen(div(span(s"title:$id"), probe(s"issue$id", log), comments(id)))).keyed(id)
            ui = div(UI.boundary.placeholder(span("skel"))(div(probe("header", log), route.render(id => div(detail(id))))))
            // The issue's form: the route region at 0.1, its div, the detail mount at 0.1.0, the form second in it.
            issueForm  = Seq("0", "1", "0", "1")
            headerForm = Seq("0", "0")
            _ <- mountWith(ui) { (screen, handle) =>
                for
                    _       <- screen.await("issue 1")(v => v.contains("issue1") && v.contains("comments:1"))
                    _       <- sendAll(handle, issueForm)
                    before  <- log.get
                    _       <- route.set("2")
                    _       <- detailGate.completeUnitDiscard
                    _       <- commentsStarted.get
                    shown   <- screen.now
                    _       <- sendAll(handle, issueForm)
                    _       <- sendAll(handle, headerForm)
                    waiting <- log.get
                    _       <- commentsGate.completeUnitDiscard
                    _       <- screen.await("issue 2")(v => v.contains("issue2") && v.contains("comments:2"))
                    _       <- sendAll(handle, issueForm)
                    after   <- log.get
                yield
                    val once = kinds.map(_ => 1)
                    val none = kinds.map(_ => 0)
                    assert(received(before, "issue1") == once, s"on issue 1 before navigating: $before")
                    assert(shown.contains("issue1") && !shown.contains("issue2"), s"while issue 2 loads: $shown")
                    assert(received(waiting, "issue1") == once && received(waiting, "issue2") == none, s"on the waiting page: $waiting")
                    assert(received(waiting, "header") == once, s"on the header while issue 2 loads: $waiting")
                    assert(received(after, "issue2") == once && received(after, "issue1") == once, s"after the reveal: $after")
                end for
            }
        yield succeed
    }

    "navigating on before the new page loaded leaves no range held: clicks reach the page shown after it" in {
        for
            route    <- Signal.initRef("1")
            likes    <- AtomicInt.init(0)
            started2 <- Promise.init[Unit, Any]
            never    <- Promise.init[Unit, Any]
            detail = (id: String) =>
                val load = id match
                    case "2" => started2.completeUnitDiscard.andThen(never.get)
                    case _   => Kyo.unit
                UI.mounted(load.andThen(div(span(s"title:$id"), button(s"like:$id").onClick(likes.incrementAndGet)))).keyed(id)
            ui   = div(UI.boundary.placeholder(span("skel"))(div(span("header"), route.render(id => div(detail(id))))))
            like = Seq("0", "1", "0", "1")
            _ <- mountWith(ui) { (screen, handle) =>
                for
                    _     <- screen.await("issue 1")(_.contains("title:1"))
                    _     <- route.set("2")
                    _     <- started2.get
                    _     <- route.set("3")
                    _     <- screen.await("issue 3")(v => v.contains("title:3") && !v.contains("title:1"))
                    _     <- handle(like, UIEvent.Click(like, MouseEventData(UI.Modifiers.none, Absent)))
                    count <- likes.get
                yield assert(count == 1, s"the click on issue 3 reached it $count times")
            }
        yield succeed
    }

    "under KeepLatest a boundary keeps its whole old content while a re-render loads" in {
        for
            key     <- Signal.initRef("a")
            started <- Promise.init[Unit, Any]
            gate    <- Promise.init[Unit, Any]
            load = (k: String, name: String) =>
                if k == "a" then Kyo.lift(span(s"$name:a"))
                else (if name == "A" then started.completeUnitDiscard else Kyo.unit).andThen(gate.get).andThen(span(s"$name:$k"))
            ui = div(UI.boundary.placeholder(span("skel"))(key.render(k =>
                div(span(s"key:$k"), UI.mounted(load(k, "A")).keyed(k), UI.mounted(load(k, "B")).keyed(k))
            )))
            _ <- mount(ui) { screen =>
                for
                    _     <- screen.await("the first content")(v => v.contains("A:a") && v.contains("B:a"))
                    _     <- key.set("b")
                    _     <- started.get
                    shown <- screen.now
                    _     <- gate.completeUnitDiscard
                    _     <- screen.await("the new content")(v => v.contains("key:b") && v.contains("A:b") && v.contains("B:b"))
                yield assert(
                    shown.contains("key:a") && shown.contains("A:a") && shown.contains("B:a") && !shown.contains("key:b") &&
                        !shown.contains("skel"),
                    s"while the re-render loads: $shown"
                )
            }
        yield succeed
    }

    "under Placeholder a boundary shows its placeholder again while a re-render loads" in {
        for
            key  <- Signal.initRef("a")
            gate <- Promise.init[Unit, Any]
            load = (k: String, name: String) => if k == "a" then Kyo.lift(span(s"$name:a")) else gate.get.andThen(span(s"$name:$k"))
            ui   = div(UI.boundary.placeholder(span("skel")).whilePending(PendingPolicy.Placeholder)(key.render(k =>
                div(span(s"key:$k"), UI.mounted(load(k, "A")).keyed(k), UI.mounted(load(k, "B")).keyed(k))
            )))
            _ <- mount(ui) { screen =>
                for
                    _ <- screen.await("the first content")(v => v.contains("A:a") && v.contains("B:a"))
                    _ <- key.set("b")
                    _ <- screen.await("the placeholder again")(v => v.contains("skel") && !v.contains("A:a"))
                    _ <- gate.completeUnitDiscard
                    _ <- screen.await("the new content")(v => v.contains("A:b") && v.contains("B:b") && !v.contains("skel"))
                yield ()
            }
        yield succeed
    }

    "a re-render whose mounts are all kept shows at once, and keeps their instances" in {
        for
            runs <- AtomicInt.init(0)
            tick <- Signal.initRef(0)
            ui = div(UI.boundary.placeholder(span("skel"))(tick.render(n =>
                div(span(s"tick:$n"), UI.mounted(runs.incrementAndGet.map(r => span(s"runs:$r"))).keyed("box"))
            )))
            _ <- mount(ui) { screen =>
                for
                    _ <- screen.await("runs:1")(_.contains("runs:1"))
                    _ <- tick.set(1)
                    _ <- screen.await("tick:1")(v => v.contains("tick:1") && v.contains("runs:1"))
                yield ()
            }
            total <- runs.get
        yield assert(total == 1, s"runs: $total")
    }
end BoundaryTest
