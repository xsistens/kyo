package kyo.uic

import kyo.*
import kyo.internal.HtmlOp
import kyo.internal.HtmlRenderer
import kyo.internal.ReactiveUI
import kyo.internal.UICommands

/** What a click addressed to the opening paint of a panel reaches.
  *
  * An open overlay paints twice: the mount's placeholder at once, then the measured content once its
  * effect has minted the ids it observes. The client addresses a click by the path the element had on
  * the page when the reader clicked, and the session resolves that path against the tree it holds
  * when the click arrives. A reader who clicks during the first paint therefore hits whatever the
  * second paint put at that path, so the two paints must put the same element there.
  */
class OverlayTest extends UicTest:

    /** Opens `overlay` inside a clickable host, waits until the measured content is the tree the session holds, then
      * clicks the option at the path the opening paint showed it at. Answers how often the option and the host heard it,
      * and whether the overlay is still open.
      */
    private def clickOpening(overlay: SignalRef[Boolean] => Overlay)(using Frame): (Int, Int, Boolean) < Async =
        Scope.run {
            for
                open    <- Signal.initRef(true)
                picks   <- AtomicInt.init(0)
                toggles <- AtomicInt.init(0)
                // The measured content observes the panel it stamped, and that command goes out once the content is the tree
                // the session holds.
                measured <- Promise.init[Unit, Any]
                cmds     <- UI.Commands.init {
                    case _: HtmlOp.ObserveViewportById => measured.completeUnitDiscard
                    case _                             => Kyo.unit
                }
                ui = UI.div.onClick(toggles.incrementAndGet)(
                    overlay(open)(UI.button.id("opt").onClick(picks.incrementAndGet)("Cherry")).render
                )
                html <- HtmlRenderer.render(ui, Seq.empty)
                path = pathOf(html, "opt")
                result <- UICommands.commands.let(Present(cmds)) {
                    for
                        root <- ReactiveUI.normalize(ui, Seq.empty)
                        sub  <- ReactiveUI.subscribe(root, kyo.internal.UIExchange.noop)
                        _    <- measured.get
                        _    <- sub.handle(path, clickAt(path))
                        p    <- picks.get
                        t    <- toggles.get
                        o    <- open.get
                    yield (p, t, o)
                }
            yield result
        }

    "a click on an element of the opening paint reaches the same element once the measured panel replaced it" in {
        clickOpening(open => Overlay(open).containClicks(true)).map { (picks, toggles, open) =>
            assert(picks == 1, s"the option the reader clicked ran $picks time(s)")
            assert(toggles == 0, s"and the click went on to the host $toggles time(s)")
            assert(open, "the overlay is still open, since the option does not close it")
        }
    }

    "the same holds for a portal panel" in {
        clickOpening(open => Overlay(open).containClicks(true).portal(true)).map { (picks, toggles, _) =>
            assert(picks == 1 && toggles == 0, s"option $picks, host $toggles")
        }
    }

    "for a panel placed at a point" in {
        clickOpening(open => Overlay(open).containClicks(true).pointerAnchor(Signal.initConst(Present(UI.Point(10, 20)))))
            .map { (picks, toggles, _) =>
                assert(picks == 1 && toggles == 0, s"option $picks, host $toggles")
            }
    }

    "and for a panel without a backdrop" in {
        clickOpening(open => Overlay(open).containClicks(true).dismissOnOutsideClick(false)).map { (picks, toggles, _) =>
            assert(picks == 1 && toggles == 0, s"option $picks, host $toggles")
        }
    }

    /** Opens `overlay` under a host that reads the open state and a highlight in ONE region and draws the panel with
      * `renderGated`, as the pickers and menus do. Each round moves the highlight, waits for the host's repaint and clicks
      * the option at once. Answers how many clicks the option heard and how often an element was observed: every start
      * of the panel's mount or of a measuring trigger observes again, whether under a fresh id or the same one.
      */
    private def hostRepaints(overlay: SignalRef[Boolean] => Overlay, rounds: Int)(using Frame): (Int, Int) < Async =
        Scope.run {
            for
                open     <- Signal.initRef(true)
                hi       <- Signal.initRef(0)
                picks    <- AtomicInt.init(0)
                observed <- AtomicRef.init(Chunk.empty[String])
                painted  <- Channel.init[Unit](64)
                measured <- Promise.init[Unit, Any]
                cmds     <- UI.Commands.init {
                    case HtmlOp.ObserveViewportById(id) => observed.updateAndGet(_.append(id)).andThen(measured.completeUnitDiscard)
                    case _                              => Kyo.unit
                }
                exchange = new kyo.internal.UIExchange:
                    def onChange(
                        region: kyo.internal.ReactiveRegion,
                        path: Seq[String],
                        contentContext: kyo.internal.ReactiveRegion.RegionIdentity,
                        parentContext: kyo.internal.ReactiveRegion.ParentContext,
                        previous: Maybe[UI],
                        changed: UI
                    )(using Frame): Unit < Async = Abort.run[Closed](painted.put(())).unit
                ui = UI.div(Reading.Live(open).and(Reading.Live(hi)).render { (o, h) =>
                    overlay(open)(UI.button.id("opt").onClick(picks.incrementAndGet)(s"row $h")).renderGated(o, Present("host-panel"))
                })
                html <- HtmlRenderer.render(ui, Seq.empty)
                path = pathOf(html, "opt")
                result <- UICommands.commands.let(Present(cmds)) {
                    for
                        root <- ReactiveUI.normalize(ui, Seq.empty)
                        sub  <- ReactiveUI.subscribe(root, exchange)
                        _    <- measured.get
                        _    <- Kyo.foreach(1 to rounds) { i =>
                            Abort.run[Closed](painted.drain).andThen(hi.set(i)).andThen(Abort.run[Closed](painted.take))
                                .andThen(sub.handle(path, clickAt(path)))
                        }
                        p <- picks.get
                        n <- observed.get
                    yield (p, n.size)
                }
            yield result
        }

    "a host repainting around an open panel neither remounts it nor loses a click on it" in {
        hostRepaints(open => Overlay(open).containClicks(true), rounds = 10).map { (picks, ids) =>
            assert(picks == 10, s"the option heard $picks of 10 clicks sent right after the host repainted")
            assert(ids == 1, s"the panel was observed $ids times, once per mount")
        }
    }

    "the same holds for a portal panel under a repainting host" in {
        // A portal mount observes two ids: the anchor probe and the panel.
        hostRepaints(open => Overlay(open).containClicks(true).portal(true), rounds = 10).map { (picks, ids) =>
            assert(picks == 10 && ids == 2, s"clicks heard $picks of 10, $ids observations")
        }
    }

    "and for a panel placed at a point under a repainting host" in {
        hostRepaints(
            open => Overlay(open).containClicks(true).pointerAnchor(Signal.initConst(Present(UI.Point(10, 20)))),
            rounds = 10
        ).map { (picks, ids) =>
            assert(picks == 10 && ids == 1, s"clicks heard $picks of 10, $ids observations")
        }
    }

    private def panel(open: Boolean | Signal[Boolean])(using Frame) = Overlay(open).autoFlip(false)(UI.span("inside"))

    private def backdrops(t: UI)(using Frame) = elementsWithClass(t, "p-uic-overlay-backdrop")

    private def onBackdrop(e: UI.Ast.Element): Boolean = e.attrs.cssClasses.contains("p-uic-overlay-backdrop")

    "a constant nobody hears is fixed: an open panel stays, and nothing outside dismisses it" in {
        for
            outside <- backdrops(panel(true).render)
            p       <- elementWithClass(panel(true).render, "p-uic-overlay-panel")
            card    <- elementsWithClass(panel(true).render, "p-uic-key-error")
            shut    <- elementsWithClass(panel(false).render, "p-uic-overlay-panel")
        yield
            assert(outside.isEmpty, "no backdrop takes an outside click")
            assert(p.attrs.onKeyDown.isEmpty, "Escape closes nothing")
            assert(card.isEmpty, s"a fixed state is not a mistake: ${card.map(textOf)}")
            assert(shut.isEmpty, "a constant false shows nothing")
    }

    "a constant somebody hears reports an outside click" in {
        for
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            _    <- clickBubbling(panel(true).onOpenChange(v => seen.set(Present(v))).render)(onBackdrop)
            s    <- seen.get
        yield assert(s == Present(false), s"reported $s")
    }

    "a signal bound one way is followed, and a dismissal is reported without being written" in {
        for
            held <- Signal.initRef(true)
            seen <- Signal.initRef(Absent: Maybe[Boolean])
            _    <- clickBubbling(panel(held.readOnly).onOpenChange(v => seen.set(Present(v))).render)(onBackdrop)
            h    <- held.get
            s    <- seen.get
            deaf <- elementsWithClass(panel(held.readOnly).render, "p-uic-key-error")
        yield
            assert(h && s == Present(false), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onOpenChange"), s"nobody hears: ${deaf.map(textOf)}")
        end for
    }

end OverlayTest
