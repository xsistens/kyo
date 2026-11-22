package kyo

import kyo.UI.foreachKeyed
import kyo.UI.render
import kyo.internal.DomBackend
import kyo.internal.DomBackend.Marker
import kyo.internal.HtmlRenderer
import org.scalajs.dom
import scala.scalajs.js as scalajs

class DomBackendMarkerTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    "a list of plain rows carries no marker a patch scans for" in {
        for
            hot   <- Signal.initRef(false)
            title <- Signal.initRef("t")
            rows  <- Signal.initRef(Chunk("a", "b", "c"))
            ui = UI.div(
                UI.ul(rows.foreachKeyed(identity) { k =>
                    UI.li(
                        UI.span(k).cssClass("label").when(hot: Signal[Boolean])(_.cssClass("hot")),
                        UI.button("x").title(title: Signal[String]).onClick(()),
                        UI.input.value(k)
                    ).id(s"row-$k")
                }),
                Svg.svg(Svg.rect)
            )
            html <- HtmlRenderer.render(ui, Seq.empty)
        yield assert(DomBackend.markersIn(html) == 0, html)
    }

    "every optional feature is found in the markup it renders" in {
        val cases: Seq[(String, (UI, Int))] = Seq(
            "enter"             -> (UI.div("x").enterTransition("fade"), Marker.Enter),
            "leave"             -> (UI.div("x").leaveTransition("fade"), Marker.Leave),
            "focus-auto"        -> (UI.input.focusAuto(true), Marker.FocusAuto),
            "scroll-auto"       -> (UI.div("x").scrollAuto(true), Marker.ScrollAuto),
            "js property"       -> (UI.input.jsProp("indeterminate", "true"), Marker.JsProp),
            "animate"           -> (Svg.svg(Svg.rect(Svg.animate)), Marker.SvgAnim),
            "animate transform" -> (Svg.svg(Svg.rect(Svg.animateTransform)), Marker.SvgAnim),
            "animate motion"    -> (Svg.svg(Svg.rect(Svg.animateMotion)), Marker.SvgAnim),
            "portal"            -> (UI.div("x").portal(true), Marker.Portal),
            "attach"            -> (UI.div("x").onAttached(_ => Kyo.unit), Marker.Attach)
        )
        Kyo.foreach(cases) { case (name, (ui, marker)) =>
            HtmlRenderer.render(ui, Seq.empty).map { html =>
                (name, DomBackend.markersIn(html), DomBackend.markersIn(html, known = marker))
            }
        }.map { found =>
            val wrong = found.filter { case (name, all, rest) =>
                all != cases.toMap.apply(name)._2 || rest != 0
            }
            assert(wrong.isEmpty, s"(feature, markers, markers beyond the known one): $wrong")
        }
    }

    /** Mounts `ui` until the scope ends.
      *
      * The marker record is per document and the suites of one run share it, so whether a leaf below meets its marker for the
      * first time depends on what ran before it. Nothing in production can forget a marker, and a reset kept only for tests
      * would be one; the first sighting is pinned where it is decided, by [[DomBackend.markersIn]] above.
      */
    private def mountFresh(ui: UI, booted: => Boolean)(using Frame, kyo.test.AssertScope): Unit < (Async & Scope) =
        val ready = new DomTestEnv.MountReady
        for
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- Scope.ensure(fiber.interrupt.andThen(fiber.getResult).unit)
            _     <- assertEventually(Sync.defer(ready.installed && booted))
        yield ()
        end for
    end mountFresh

    "a row that brings the first enter transition onto the page enters when a list patch adds it" in {
        // The enter classes are removed on the next animation frame; holding frames back keeps them readable.
        val window = scalajs.Dynamic.global.window
        val raf    = window.requestAnimationFrame
        Scope.run {
            for
                rows <- Signal.initRef(Chunk("a", "b"))
                ui = UI.div(UI.ul(rows.foreachKeyed(identity) { k =>
                    if k == "new" then UI.li(k).id(s"marker-row-$k").enterTransition("marker-entering")
                    else UI.li(k).id(s"marker-row-$k")
                }))
                _ <- mountFresh(ui, dom.document.getElementById("marker-row-a") != null)
                _ <- Scope.ensure(Sync.defer(window.requestAnimationFrame = raf))
                _ <- Sync.defer(window.requestAnimationFrame =
                    ((_: scalajs.Function1[Double, Unit]) => 0): scalajs.Function1[scalajs.Function1[Double, Unit], Int]
                )
                _        <- rows.set(Chunk("a", "b", "new"))
                _        <- assertEventually(Sync.defer(dom.document.getElementById("marker-row-new") != null))
                entering <- Sync.defer(dom.document.getElementById("marker-row-new").classList.contains("marker-entering"))
            yield assert(entering, "the new row did not enter")
            end for
        }
    }

    "a region that brings the first scroll-auto flag onto the page scrolls it into view" in {
        val proto = scalajs.Dynamic.global.window.Element.prototype
        val prior = proto.scrollIntoView
        val seen  = scalajs.Array[String]()
        Scope.run {
            for
                show <- Signal.initRef(false)
                ui = UI.div(show.render { on =>
                    if on then UI.div("x").id("marker-scroll").scrollAuto(true) else UI.span("none")
                })
                _ <- mountFresh(ui, true)
                _ <- Scope.ensure(Sync.defer(proto.scrollIntoView = prior))
                _ <- Sync.defer(proto.scrollIntoView =
                    (
                        (self: dom.Element, _: scalajs.Any) => discard(seen.push(self.id))
                    ): scalajs.ThisFunction1[dom.Element, scalajs.Any, Unit]
                )
                _ <- show.set(true)
                _ <- assertEventually(Sync.defer(seen.contains("marker-scroll")))
            yield succeed
            end for
        }
    }

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
