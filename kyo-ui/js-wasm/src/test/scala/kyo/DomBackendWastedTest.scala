package kyo

import java.util.concurrent.atomic.AtomicReference
import kyo.UI.foreachKeyed
import kyo.UI.render
import kyo.internal.DomBackend
import org.scalajs.dom

/** Which repaints a browser mount reports to a devtools sink as wasted: those that left the page as it was.
  *
  * The shared engine only carries the verdict (see `DevtoolsReportTest`); the backend reaches it, an HTML range by comparing
  * fingerprints of what it painted, an SVG region by comparing its markup with the live element. The first repaint after the mount
  * counts like any other: the region's first value was painted by the enclosing render, and is what it repeats.
  */
class DomBackendWastedTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    private def text(id: String): String =
        val el = dom.document.getElementById(id)
        if el == null then "" else el.textContent

    /** Mounts `ui` under a devtools sink until the scope ends; `repaints` reads the `wasted` flag of every paint a signal
      * caused so far, whole or per list row.
      *
      * `mount` returns once the region's subscription has taken its first value, which it reports as `Created`. A write
      * before that would reach the region as its first value, which is never compared and is not a signal's paint.
      */
    final private class Observed(ui: UI):
        private val events = new AtomicReference(Vector.empty[UI.RenderEvent])

        def repaints: Vector[Boolean] = events.get().collect {
            case e if e.cause == UI.RenderCause.Signal && paints(e.kind) => e.wasted
        }

        private def paints(kind: UI.RenderKind): Boolean = kind match
            case UI.RenderKind.Repaint | _: UI.RenderKind.ListPatch => true
            case _                                                  => false

        def mount(booted: => Boolean)(using Frame, kyo.test.AssertScope): Unit < (Async & Scope) =
            val ready = new DomTestEnv.MountReady
            for
                _     <- Sync.defer(dom.document.body.innerHTML = """<div id="wt-root"></div>""")
                fiber <- UI.devtools(e => discard(events.updateAndGet(_ :+ e)))(
                    Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, "#wt-root", ready)))
                )
                _ <- Scope.ensure(fiber.interrupt.andThen(fiber.getResult).unit)
                _ <- assertEventually(Sync.defer(ready.installed && booted && events.get().exists(_.cause == UI.RenderCause.Created)))
            yield ()
            end for
        end mount
    end Observed

    "an HTML region that repaints the markup it already shows reports the repaint as wasted" in {
        Scope.run {
            for
                n <- Signal.initRef(1)
                page = Observed(UI.div(n.render(v => UI.span(if v > 0 then "pos" else "neg").id("wt-sign"))))
                _ <- page.mount(text("wt-sign") == "pos")
                _ <- n.set(2)
                _ <- assertEventually(Sync.defer(page.repaints.size == 1))
                _ <- n.set(-1)
                _ <- assertEventually(Sync.defer(page.repaints.size == 2))
            yield assert(page.repaints == Vector(true, false) && text("wt-sign") == "neg", s"wasted flags: ${page.repaints}")
        }
    }

    "a whole list repaint after the list changed in between is not a repeat of the one before" in {
        // A row painting as a fragment has nothing for a key to name: an emission that rebuilds it repaints the whole list, and
        // while it stands every later emission does too. The fragment shows its key, not its item, so the last emission repaints
        // exactly the markup of the first; the row added in between changed the page, so it is not a repeat. (A list patch drops
        // the range's fingerprint for the same reason, but every whole repaint that could meet it after a patch changes the
        // markup anyway, so no test can tell that drop from its absence.)
        def row(item: String): UI =
            if item.startsWith("m") then UI.fragment(UI.span("m1"), UI.span("m2")) else UI.span(item)
        Scope.run {
            for
                rows <- Signal.initRef(Chunk("a"))
                page = Observed(UI.div(rows.foreachKeyed(_.take(1))(row)).id("wt-list"))
                _ <- page.mount(text("wt-list") == "a")
                _ <- rows.set(Chunk("a", "m-1"))
                _ <- assertEventually(Sync.defer(text("wt-list") == "am1m2"))
                _ <- rows.set(Chunk("a", "m-1", "b"))
                _ <- assertEventually(Sync.defer(text("wt-list") == "am1m2b"))
                _ <- rows.set(Chunk("a", "m-2"))
                _ <- assertEventually(Sync.defer(text("wt-list") == "am1m2" && page.repaints.size == 3))
            yield assert(page.repaints == Vector(false, false, false), s"wasted flags: ${page.repaints}")
        }
    }

    "an SVG region that repaints the markup it already shows reports the repaint as wasted" in {
        def radius = Maybe(dom.document.getElementById("wt-circle")).map(_.getAttribute("r"))
        Scope.run {
            for
                n <- Signal.initRef(1)
                page = Observed(Svg.svg(n.render(v => Svg.circle.r(if v > 0 then 2 else 3).id("wt-circle"))))
                _     <- page.mount(radius.nonEmpty)
                first <- Sync.defer(radius)
                _     <- n.set(2)
                _     <- assertEventually(Sync.defer(page.repaints.size == 1))
                _     <- n.set(-1)
                _     <- assertEventually(Sync.defer(page.repaints.size == 2))
            yield assert(page.repaints == Vector(true, false) && radius != first, s"wasted flags: ${page.repaints}")
        }
    }

end DomBackendWastedTest
