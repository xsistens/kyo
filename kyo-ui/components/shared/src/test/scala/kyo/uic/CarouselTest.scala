package kyo.uic

import kyo.*
import kyo.UI.*

/** The carousel's indicator dots rove their tabindex, so every inactive dot is out of the Tab
  * order and the arrows are the only way to reach it.
  *
  * The wiring lives in the mount that mints the strip id, so these drive the `wired` seam with a
  * recording focus function rather than a `UI.Commands`.
  */
class CarouselTest extends UicTest:

    private def strip(using Frame) =
        uic.Carousel[String]().items(Seq("a", "b", "c"))(s => p(s)).numVisible(1).numScroll(1)

    /** Presses `key` on the dot at `at` and returns the id focus was moved to, if any. */
    private def move(at: Int, key: UI.Keyboard, circular: Boolean = false)(using Frame): Maybe[String] < Async =
        for
            page  <- Signal.initRef(0)
            start <- Signal.initRef(0.0)
            moved <- Signal.initRef(Absent: Maybe[String])
            ui = (if circular then strip.circular(true) else strip)
                .wired(page, "car", start, id => moved.set(Present(id)))
            dots <- elementsWithClass(ui, "p-carousel-indicator-button")
            _    <- press(dots(at), key)
            to   <- moved.get
        yield to

    private def nextButton(ui: UI)(using Frame): UI.Ast.Element < Sync =
        elementsWithClass(ui, "p-carousel-next-button").map(_.headOption.getOrElse(throw new AssertionError("no next button")))

    // The page lives somewhere else (a route, a parent): the carousel shows it and hands a turn over.
    "a page bound one way is shown, and a turn is reported without being written" in {
        for
            held  <- Signal.initRef(0)
            seen  <- Signal.initRef(-1)
            start <- Signal.initRef(0.0)
            ui = strip.page(held.readOnly).onPageChange(seen.set).wired(held.readOnly, "car", start, _ => ())
            next <- nextButton(ui)
            _    <- click(next)
            h    <- held.get
            s    <- seen.get
        yield assert(h == 0 && s == 1, s"held $h, reported $s")
    }

    "a two-way page is written before the carousel reports it" in {
        for
            page  <- Signal.initRef(0)
            seen  <- Signal.initRef((-1, -1))
            start <- Signal.initRef(0.0)
            ui = strip.page(page).onPageChange(p => page.get.map(h => seen.set((p, h)))).wired(page, "car", start, _ => ())
            next <- nextButton(ui)
            _    <- click(next)
            s    <- seen.get
        yield assert(s == (1, 1), s"reported, held: $s")
    }

    "a page bound one way with nothing to hear a turn says so" in {
        for
            held  <- Signal.initRef(0)
            deaf  <- elementsWithClass(strip.page(held.readOnly).render, "p-uic-key-error")
            heard <- elementsWithClass(strip.page(1).onPageChange(_ => ()).render, "p-uic-key-error")
        yield
            assert(deaf.size == 1 && textOf(deaf.head).contains("onPageChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(heard.isEmpty, s"somebody hears: ${heard.map(textOf)}")
    }

    "ArrowRight moves to the next dot" in move(0, UI.Keyboard.ArrowRight).map(to => assert(to == Present("car-d1")))

    "ArrowLeft moves back" in move(2, UI.Keyboard.ArrowLeft).map(to => assert(to == Present("car-d1")))

    "Home and End reach the ends" in {
        for
            home <- move(2, UI.Keyboard.Home)
            end  <- move(0, UI.Keyboard.End)
        yield assert(home == Present("car-d0") && end == Present("car-d2"))
    }

    "the vertical arrows belong to the page under a horizontal carousel" in {
        for
            down <- move(0, UI.Keyboard.ArrowDown)
            up   <- move(0, UI.Keyboard.ArrowUp)
        yield assert(down == Absent && up == Absent)
    }

    "Enter and Space move nothing, since a dot is a button the browser activates" in {
        for
            enter <- move(0, UI.Keyboard.Enter)
            space <- move(0, UI.Keyboard.Space)
        yield assert(enter == Absent && space == Absent)
    }

    "only the active dot is in the Tab order" in {
        for
            page  <- Signal.initRef(1)
            start <- Signal.initRef(0.0)
            ui    <- Kyo.lift(strip.wired(page, "car", start, _ => ()))
            dots  <- elementsWithClass(ui, "p-carousel-indicator-button")
        yield
            assert(dots(1).attrs.tabIndex.contains(0), "the active dot is the tab stop")
            assert(dots(0).attrs.tabIndex.contains(-1) && dots(2).attrs.tabIndex.contains(-1))
    }

    "a carousel that does not wrap stops at its ends, and a circular one comes round" in {
        // `wrap` follows `circular`, so the dots agree with what the next-page button would do.
        for
            stopped <- move(2, UI.Keyboard.ArrowRight)
            round   <- move(2, UI.Keyboard.ArrowRight, circular = true)
        yield
            assert(stopped == Absent, "the last dot has nowhere further to go")
            assert(round == Present("car-d0"), "unless the carousel is circular")
    }

    /** Runs an autoplaying carousel whose `onPageChange` is `change`, under a boundary sink, and yields the first two notices. */
    private def autoplayNotices(change: Int => UI.Action)(using Frame): (String, String) < (Async & Abort[Closed]) =
        Scope.run {
            for
                notices <- Channel.initUnscoped[String](16)
                page    <- Signal.initRef(0)
                cmds    <- UI.Commands.init(_ => ())
                car = strip.page(page).autoplay(1).onPageChange(change)
                ui  = UI.boundary.onNotice { case UI.Failed(raw) => Abort.run[Closed](notices.put(String.valueOf(raw))).unit }(car.render)
                _ <- kyo.internal.UICommands.commands.let(Present(cmds)) {
                    kyo.internal.ReactiveUI.normalize(ui, Seq.empty).map { root =>
                        kyo.internal.ReactiveUI.subscribe(root, kyo.internal.UIExchange.noop).unit
                    }
                }
                first  <- notices.take
                second <- notices.take
            yield (first, second)
        }

    "an autoplay step whose onPageChange fails is reported to the rules, and autoplay goes on" in {
        Abort.run[Closed](autoplayNotices(n => Abort.fail(s"page $n"))).map { r =>
            assert(r.getOrThrow == ("page 1", "page 2"), "the failed step was reported, and the next one still ran")
        }
    }

    "an autoplay step whose onPageChange throws is reported the same way" in {
        Abort.run[Closed](autoplayNotices(n => throw new IllegalStateException(s"thrown $n"))).map { r =>
            val (first, second) = r.getOrThrow
            assert(first.contains("thrown 1") && second.contains("thrown 2"), s"a panic is a notice too; got $first, $second")
        }
    }

    "items a signal takes away from the page shown leave the last page there is active" in {
        // The strip falls back to the last page; the items it shows there must be the active ones too.
        def active(html: String): Int = "p-carousel-item-active".r.findAllMatchIn(html).size
        for
            live  <- Signal.initRef(Seq("A", "B", "C", "D"))
            page  <- Signal.initRef(1)
            start <- Signal.initRef(0.0)
            ui = uic.Carousel[String]().items(live)(s => span(s)).numVisible(2).numScroll(2).wired(page, "car", start)
            before <- renderHtml(ui)
            _      <- live.set(Seq("A", "B"))
            after  <- renderHtml(ui)
        yield
            assert(active(before) == 2, s"page 2 shows C and D: ${active(before)}")
            assert(active(after) == 2, s"A and B, the page the strip falls back to, are active: ${active(after)}")
        end for
    }

    "a constant page nobody hears is fixed: no button turns it, and no card" in {
        for
            buttons <- elementsWithClass(strip.page(1).render, "p-carousel-next-button")
            card    <- elementsWithClass(strip.page(1).render, "p-uic-key-error")
        yield
            assert(buttons.forall(b => !declaresClick(b)), "a fixed carousel turns nowhere")
            assert(card.isEmpty, s"a fixed constant: ${card.map(textOf)}")
    }

    "an unbound carousel keeps the page the reader turns to" in {
        def onSecond(t: UI): Boolean < Sync =
            elementsWithClass(t, "p-carousel-indicator").map(is =>
                is.indexWhere(_.attrs.cssClasses.contains("p-carousel-indicator-active")) == 1
            )
        for
            after <- publishedAfterReady(
                strip.render,
                t => elementsWithClass(t, "p-carousel-next-button").map(_.exists(declaresClick)),
                until = onSecond
            )(t => nextButton(t).map(click))
        yield assert(after.nonEmpty, "the carousel never showed the page the reader turned to")
    }

end CarouselTest
