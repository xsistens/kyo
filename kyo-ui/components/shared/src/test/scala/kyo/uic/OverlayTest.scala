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
                        sub  <- ReactiveUI.subscribe(root, quietExchange)
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

end OverlayTest
