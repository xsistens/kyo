package kyo.internal

import kyo.*
import kyo.UI.foreach
import kyo.internal.RecordingExchange.Op

/** Direct-engine tests pinning the skip of redundant first emissions after subscribe: normalize captures
  * render-time snapshots (normalize happens-before the HTML render on every paint path), so a freshly painted
  * region or channel must NOT be repainted by its observer's initial emission, while a change landing between
  * normalize and subscribe differs from the snapshot and must still fire.
  */
class ReactiveSubscribeSkipTest extends kyo.test.Test[Any]:

    final private class Recording:
        val exchange          = new RecordingExchange
        def changes: Int      = exchange.recorded.count(_.isInstanceOf[Op.Paint])
        def classPatches: Int = exchange.recorded.count(_.isInstanceOf[Op.Class])
    end Recording

    "subscribe does not repaint a just-normalized region nor re-patch its channels" in {
        Scope.run {
            for
                items <- Signal.initRef(Chunk("a", "b"))
                on    <- Signal.initRef(false)
                rec = new Recording
                ui  = UI.div(items.foreach(i => UI.span(i)), UI.span("x").when(on)(_.cssClass("hot")))
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                // Each observer waits on its signal again only once it has handled the first value, the one it skips: whatever
                // that handling paints or patches has happened by then.
                _ <- assertEventually(items.waiters.map(_ == 1))
                _ <- assertEventually(on.waiters.map(_ == 1))
                silentChanges = rec.changes
                silentClasses = rec.classPatches
                _ <- items.set(Chunk("a", "b", "c"))
                _ <- assertEventually(Sync.defer(rec.changes >= silentChanges + 1))
                _ <- on.set(true)
                _ <- assertEventually(Sync.defer(rec.classPatches >= 1))
            yield assert(silentChanges == 0 && silentClasses == 0)
        }
    }

    "a change landing between normalize and subscribe still fires" in {
        Scope.run {
            for
                items <- Signal.initRef(Chunk("a"))
                rec = new Recording
                ui  = UI.div(items.foreach(i => UI.span(i)))
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- items.set(Chunk("a", "b")) // differs from the normalize-time snapshot
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- assertEventually(Sync.defer(rec.changes >= 1))
            yield assert(true)
        }
    }

    "an unpainted tree sends every first emission, regions and channels alike" in {
        Scope.run {
            for
                items <- Signal.initRef(Chunk("a", "b"))
                on    <- Signal.initRef(true)
                rec = new Recording
                ui  = UI.div(items.foreach(i => UI.span(i)), UI.span("x").when(on)(_.cssClass("hot")))
                root <- ReactiveUI.normalize(ui, Seq.empty).map(ReactiveUI.unpainted)
                _    <- ReactiveUI.subscribe(root, rec.exchange)
                _    <- assertEventually(Sync.defer(rec.changes >= 1 && rec.classPatches >= 1))
            yield assert(true)
        }
    }

end ReactiveSubscribeSkipTest
