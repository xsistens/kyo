package kyo

import kyo.UI.render
import kyo.internal.DomBackend
import org.scalajs.dom
import scala.scalajs.js as scalajs

class DomBackendMarkerTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    private def mountFresh(ui: UI, booted: => Boolean)(using Frame, kyo.test.AssertScope): Unit < (Async & Scope) =
        val ready = new DomTestEnv.MountReady
        for
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- Scope.ensure(fiber.interrupt.andThen(fiber.getResult).unit)
            _     <- assertEventually(Sync.defer(ready.installed && booted))
        yield ()
        end for
    end mountFresh

    /** Records the ids `scrollIntoView` is called on, from before the mount, and puts it back after. */
    private def recordingScrolls(seen: scalajs.Array[String])(using Frame): Unit < (Sync & Scope) =
        val proto = scalajs.Dynamic.global.window.Element.prototype
        val prior = proto.scrollIntoView
        for
            _ <- Scope.ensure(Sync.defer(proto.scrollIntoView = prior))
            _ <- Sync.defer(proto.scrollIntoView =
                (
                    (self: dom.Element, _: scalajs.Any) => discard(seen.push(self.id))
                ): scalajs.ThisFunction1[dom.Element, scalajs.Any, Unit]
            )
        yield ()
        end for
    end recordingScrolls

    "the first paint scrolls a flag inside a scroll container of its own into view" in {
        // A list that opens on a selected row, a rail opened on a deep link: the container is the
        // page's own, the browser restores nothing inside it, and without this the row the page is
        // about starts off screen.
        val seen = scalajs.Array[String]()
        Scope.run {
            for
                _ <- recordingScrolls(seen)
                ui = UI.div.style(_.overflowY(_.auto).height(40.px))(
                    UI.div("a"),
                    UI.div("b").id("in-box").scrollAuto(true)
                )
                _ <- mountFresh(ui, true)
                _ <- assertEventually(Sync.defer(seen.contains("in-box")))
            yield succeed
            end for
        }
    }

    "the first paint leaves a flag the document scrolls to the browser" in {
        // The document's scroll position is the one the browser restores on reload and on back,
        // and scrolling it at load would fight that restoration.
        val seen = scalajs.Array[String]()
        Scope.run {
            for
                _    <- recordingScrolls(seen)
                show <- Signal.initRef(false)
                ui = UI.div(
                    UI.div("x").id("at-top").scrollAuto(true),
                    show.render(on => if on then UI.span("later").id("probe-done") else UI.span("none"))
                )
                _ <- mountFresh(ui, true)
                // A later patch is the barrier: once it has landed, the first paint's sweep is long done.
                _ <- show.set(true)
                _ <- assertEventually(Sync.defer(dom.document.getElementById("probe-done") != null))
            yield assert(!seen.contains("at-top"), "the document-level flag was scrolled at load")
            end for
        }
    }

end DomBackendMarkerTest
