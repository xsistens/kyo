package kyo.uic

import kyo.*
import kyo.UI.Prop
import kyo.UI.render
import kyo.internal.ReactiveUI

/** What placing a component gives it: the place the app writes it is the identity of the instance it draws, when it draws one
  * through `UI.mountedWith`, so a region around it that runs again keeps that instance and hands it the new props.
  */
class NodePlacementTest extends UicTest:

    private case class Labeled(label: Prop[String]) derives CanEqual

    /** Counts its draws under `name` and keeps the label it read, drawn once per instance through `mountedWith`. */
    final private class Drawing(name: String, label: String, log: NodePlacementTest.Log) extends Node:
        type Self = Drawing
        private[uic] def render(using Frame): UI =
            UI.mountedWith(Labeled(label)) { live =>
                val read = live(_.label)
                log.drew(name, read).andThen(UI.span(read))
            }
    end Drawing

    /** The same, drawn through a plain `UI.mounted`, which carries no props to hand on. */
    final private class Plain(name: String, label: String, log: NodePlacementTest.Log) extends Node:
        type Self = Plain
        private[uic] def render(using Frame): UI = UI.mounted(log.drew(name, label).andThen(UI.span(label)))
    end Plain

    /** Runs `ui`, waits for the first draws, runs `tick` once more and answers the draws per name and each name's label now. */
    private def afterOneMoreRun(log: NodePlacementTest.Log, tick: SignalRef[Int], names: Int, ui: UI)(using
        Frame,
        kyo.test.AssertScope
    ): (Map[String, Int], Map[String, String]) < Async =
        ReactiveUI.subscribed(ui) { _ =>
            for
                _      <- assertEventually(log.draws.get.map(_.size == names))
                _      <- assertEventually(tick.waiters.map(_ == 1))
                _      <- tick.set(1)
                _      <- assertEventually(tick.waiters.map(_ == 1))
                _      <- assertEventually(log.labels.map(_.values.forall(_.endsWith("1"))))
                draws  <- log.draws.get
                labels <- log.labels
            yield (draws, labels)
        }

    "a component placed in a region that runs again is one instance per place, which reads the latest run's props" in {
        for
            log  <- NodePlacementTest.Log.init
            tick <- Signal.initRef(0)
            ui = UI.div(tick.render(n => UI.div(Drawing("a", s"a$n", log), Drawing("b", s"b$n", log))))
            (draws, labels) <- afterOneMoreRun(log, tick, 2, ui)
        yield assert(draws == Map("a" -> 1, "b" -> 1) && labels == Map("a" -> "a1", "b" -> "b1"), s"draws $draws, labels $labels")
    }

    "components placed by one placement in a map are told apart by their order" in {
        for
            log  <- NodePlacementTest.Log.init
            tick <- Signal.initRef(0)
            ui = UI.div(tick.render(n => UI.fragment(List("x", "y").map(k => (Drawing(k, s"$k$n", log): UI))*)))
            (draws, labels) <- afterOneMoreRun(log, tick, 2, ui)
        yield assert(draws == Map("x" -> 1, "y" -> 1) && labels == Map("x" -> "x1", "y" -> "y1"), s"draws $draws, labels $labels")
    }

    "a component that draws through a plain mount has no props to hand on, so each run of its region draws it anew" in {
        for
            log  <- NodePlacementTest.Log.init
            tick <- Signal.initRef(0)
            ui = UI.div(tick.render(n => UI.div(Plain("p", s"p$n", log))))
            (draws, labels) <- afterOneMoreRun(log, tick, 1, ui)
        yield assert(draws == Map("p" -> 2) && labels == Map("p" -> "p1"), s"draws $draws, labels $labels")
    }

end NodePlacementTest

object NodePlacementTest:

    /** The draws per name, and the label each name read last: a constant, or a signal read now. */
    final class Log private (val draws: AtomicRef[Map[String, Int]], read: AtomicRef[Map[String, Prop[String]]]):
        def drew(name: String, label: Prop[String])(using Frame): Unit < Sync =
            draws.getAndUpdate(m => m.updated(name, m.getOrElse(name, 0) + 1)).andThen(read.getAndUpdate(_.updated(name, label))).unit

        def labels(using Frame): Map[String, String] < Sync =
            read.get.map(m => Kyo.foreach(m.toSeq)((k, p) => Prop.signal(p).current.map(k -> _)).map(_.toMap))
    end Log

    object Log:
        def init(using Frame): Log < Sync =
            AtomicRef.init(Map.empty[String, Int]).map(d => AtomicRef.init(Map.empty[String, Prop[String]]).map(new Log(d, _)))
    end Log
end NodePlacementTest
