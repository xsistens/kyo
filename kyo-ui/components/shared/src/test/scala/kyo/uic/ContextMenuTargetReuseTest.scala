package kyo.uic

import kyo.*
import kyo.UI.foreachKeyed
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.UIExchange

/** What a `ContextMenu` costs the content it is attached to.
  *
  * A menu's items may read signals — that is the point of a reactive label and a reactive
  * `disabled`, and it is how an item says "Add 3 selected to queue" while the menu stays open. The
  * question here is what else those signals reach. The target children are what the reader is
  * actually looking at, they do not depend on the items at all, and if they are rebuilt whenever an
  * item's signal moves then every keyed list, every open editor and every scroll position under a
  * context menu is rebuilt with them.
  *
  * Measured rather than argued, because the damage is invisible: the same markup comes out either
  * way, and what is lost is the reuse underneath.
  */
class ContextMenuTargetReuseTest extends UicTest:

    final case class Item(id: String, name: String) derives CanEqual

    private val items = List.tabulate(20)(i => Item(s"k$i", s"row-$i"))

    final private class Renders:
        private var n     = 0
        def bump(): Unit  = synchronized { n += 1 }
        def get: Int      = synchronized(n)
        def reset(): Unit = synchronized { n = 0 }
    end Renders

    private val quiet: UIExchange =
        new UIExchange:
            def onChange(
                region: ReactiveRegion,
                path: Seq[String],
                contentContext: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                previous: Maybe[UI],
                changed: UI
            )(using Frame): Unit < Async = ()

    "a signal the menu's items read does not rebuild the target's keyed list" in {
        Scope.run {
            val counted = new Renders
            for
                rows <- Signal.initRef(Chunk.from(items))
                // The spotify shape: the menu's label counts something about the same list the
                // target renders, so writing one row emits into both.
                menu = uic.ContextMenu(Seq(
                    uic.MenuItem(rows.map(rs => s"Add ${rs.size} to queue"))
                        .disabled(rows.map(_.isEmpty))
                )).id("m")
                target = UI.ul(rows.foreachKeyed(_.id) { r =>
                    counted.bump()
                    UI.li(r.name)
                })
                // `wired` rather than `render`: the mount's own state is what a menu is made of,
                // and this harness has no session to mint it. This is the seam the goldens use.
                openRef <- Signal.initRef(false)
                focus   <- Signal.initRef(List.empty[Int])
                at      <- Signal.initRef(Absent: Maybe[UI.Point])
                root    <- ReactiveUI.normalize(menu(target).wired(openRef, focus, at, Nil, "m"), Seq.empty)
                _       <- ReactiveUI.subscribe(root, quiet)
                _       <- Async.sleep(150.millis)
                first = counted.get
                _     = counted.reset()
                _ <- rows.set(Chunk.from(items.updated(5, Item("k5", "row-5-changed"))))
                _ <- Async.sleep(400.millis)
                after = counted.get
            yield
                assert(first > 0, s"the target never rendered at all (first pass: $first)")
                assert(
                    after == 1,
                    s"one row changed and the target rendered $after of ${items.size} rows " +
                        s"(first pass: $first)"
                )
            end for
        }
    }

end ContextMenuTargetReuseTest
