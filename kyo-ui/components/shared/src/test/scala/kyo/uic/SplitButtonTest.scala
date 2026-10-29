package kyo.uic

import kyo.*

/** [[SplitButton]]: its rows, and whether its menu is open, held to rule A ([[ReactiveValueContract]]). */
class SplitButtonTest extends ReactiveValueContract:

    "the rows follow a signal of items, so a page whose list changes keeps its split button" in {
        for
            rows <- Signal.initRef(Seq(MenuItem("Projects"), MenuItem("Viewer")))
            open <- Signal.initRef(true)
            hi   <- Anchored.init(-1)
            ui = SplitButton("Release").items(rows).wired(open, hi, "b")
            before <- elementsWithClass(ui, "p-menu-item").map(_.map(textOf))
            _      <- rows.set(Seq(MenuItem("Projects")))
            after  <- elementsWithClass(ui, "p-menu-item").map(_.map(textOf))
        yield
            assert(before == Chunk("Projects", "Viewer"), s"rows before: $before")
            assert(after == Chunk("Projects"), s"rows after the list lost Viewer: $after")
    }

    "rows given directly come first, then the ones from the signal" in {
        for
            rows <- Signal.initRef(Seq(MenuItem("Viewer")))
            open <- Signal.initRef(true)
            hi   <- Anchored.init(-1)
            ui = SplitButton("Release").items(MenuItem("All")).items(rows).wired(open, hi, "b")
            shown <- elementsWithClass(ui, "p-menu-item").map(_.map(textOf))
        yield assert(shown == Chunk("All", "Viewer"), s"rows: $shown")
    }

    "a disabled flag on a row from the signal is honoured like one given directly" in {
        for
            off  <- Signal.initRef(true)
            rows <- Signal.initRef(Seq(MenuItem("Viewer").disabled(off)))
            open <- Signal.initRef(true)
            hi   <- Anchored.init(-1)
            ui = SplitButton("Release").items(rows).wired(open, hi, "b")
            before <- elementsWithClass(ui, "p-disabled").map(_.size)
            _      <- off.set(false)
            after  <- elementsWithClass(ui, "p-disabled").map(_.size)
        yield assert(before == 1 && after == 0, s"disabled rows before $before, after $after")
    }

    "an open split button whose rows change stays open and mounted, and a click right after each change is heard" in {
        import kyo.internal.*
        Scope.run {
            for
                rows     <- Signal.initRef(Seq.empty[MenuItem])
                open     <- Signal.initRef(true)
                presses  <- AtomicInt.init(0)
                observed <- AtomicRef.init(Chunk.empty[String])
                painted  <- Channel.init[Unit](64)
                measured <- Promise.init[Unit, Any]
                cmds     <- UI.Commands.init {
                    case HtmlOp.ObserveViewportById(id) => observed.updateAndGet(_.append(id)).andThen(measured.completeUnitDiscard)
                    case _                              => Kyo.unit
                }
                exchange = new UIExchange:
                    def onChange(
                        region: ReactiveRegion,
                        path: Seq[String],
                        contentContext: ReactiveRegion.RegionIdentity,
                        parentContext: ReactiveRegion.ParentContext,
                        previous: Maybe[UI],
                        changed: UI
                    )(using Frame): Unit < Async = Abort.run[Closed](painted.put(())).unit
                ui = UI.div(SplitButton("Release").onClick(presses.incrementAndGet).open(open).items(rows).render)
                html <- HtmlRenderer.render(ui, Seq.empty)
                // The primary segment, by the path the page shows it at: it is the same before and after the mount.
                primary = """<button[^>]*p-splitbutton-button[^>]*>""".r.findFirstIn(html)
                    .flatMap(tag => """data-kyo-path="([^"]*)"""".r.findFirstMatchIn(tag).map(_.group(1).split('.').toSeq))
                    .getOrElse(throw new AssertionError(s"no primary segment in $html"))
                lists = List(Seq("Projects"), Seq("Projects", "Viewer"), Seq("Viewer"), Seq.empty, Seq("Issues"))
                result <- UICommands.commands.let(Present(cmds)) {
                    for
                        root <- ReactiveUI.normalize(ui, Seq.empty)
                        sub  <- ReactiveUI.subscribe(root, exchange)
                        _    <- measured.get
                        _    <- Kyo.foreach(lists) { labels =>
                            Abort.run[Closed](painted.drain)
                                .andThen(rows.set(labels.map(MenuItem(_))))
                                .andThen(Abort.run[Closed](painted.take))
                                .andThen(sub.handle(primary, clickAt(primary)))
                        }
                        heard <- presses.get
                        o     <- open.get
                        ids   <- observed.get
                    yield (heard, o, ids.size)
                }
            yield
                val (heard, stillOpen, panelIds) = result
                assert(heard == lists.size, s"the primary segment heard $heard of ${lists.size} clicks sent right after a change")
                assert(stillOpen, "the menu closed although nothing closed it")
                assert(panelIds == 1, s"the menu panel was observed $panelIds times, so it or its trigger was mounted again")
        }
    }

    private def split(using Frame) = SplitButton("Release").items(MenuItem("Draft").onSelect(()), MenuItem("Ship").onSelect(()))

    private def dropdowns(t: UI)(using Frame) = elementsWithClass(t, "p-splitbutton-dropdown")

    /** The button as its mount publishes it, the dropdown wired. */
    private def mounted(ui: UI)(using Frame, kyo.test.AssertScope): UI < Async =
        mountedWhen(ui)(t => dropdowns(t).map(_.nonEmpty))

    holds(new Slot[SplitButton, Boolean](
        name = "open",
        bare = Present(split),
        bind = split.open(_),
        hear = (b, f) => b.onOpenChange(f),
        event = "onOpenChange",
        render = _.render,
        start = false,
        next = true,
        control = elementWithClass(_, "p-splitbutton-dropdown"),
        shows = elementsWithClass(_, "p-menu-item").map(_.nonEmpty),
        ready = Present(dropdowns(_).map(_.nonEmpty))
    ))

    "a pick in a menu bound one way is reported as a close" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            t    <- mounted(split.open(held.readOnly).onOpenChange(v => seen.set(Present(v))).render)
            ls   <- elementsWithClass(t, "p-menu-item-link").map(_.filter(declaresClick))
            _    <- click(ls.head)
            h    <- held.get
            s    <- seen.get
        yield assert(h && s == Present(false), s"held $h, reported $s")
    }

end SplitButtonTest
