package kyo.internal

import java.util.concurrent.atomic.AtomicInteger
import kyo.*
import kyo.UI.foreachKeyed

class ClickTargetResolutionTest extends kyo.test.Test[Any]:

    private val exchange = new UIExchange:
        def onChange(
            region: ReactiveRegion,
            path: Seq[String],
            contentContext: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            changed: UI
        )(using Frame): Unit < Async = Kyo.unit

    "each ancestor of a list row resolves the clicked row once, whatever it asks about it" in {
        Scope.run {
            for
                renders <- Sync.defer(new AtomicInteger(0))
                clicks  <- Sync.defer(new AtomicInteger(0))
                rows    <- Signal.initRef(Chunk("r0"))
                ui = UI.div(UI.div(UI.ul(rows.foreachKeyed(identity) { item =>
                    discard(renders.incrementAndGet())
                    UI.li(item).onClick(Sync.defer(discard(clicks.incrementAndGet())))
                })))
                root     <- ReactiveUI.normalize(ui, Seq.empty)
                dispatch <- ReactiveUI.subscribe(root, exchange)
                // The list region takes its first emission as the painted rows and then parks on the signal
                // again; measuring before that would count its row renders as the click's.
                _ <- assertEventually(rows.waiters.map(_ == 1))
                base   = renders.get
                target = Seq("0", "0", "0", "r0")
                _ <- dispatch.handle(target, UIEvent.Click(target, MouseEventData(UI.Modifiers.none, Absent)))
                _ <- assertEventually(Sync.defer(clicks.get == 1))
                delta = renders.get - base
            // The list region resolves the row through its registry and renders nothing. Each of the three
            // ancestors then asks whether the target is disabled, a button or behind a control, and answers
            // all of it from one resolution: one row render each. A resolution per question would cost two
            // per ancestor.
            yield assert(delta == 3, s"row renders during one click: $delta")
            end for
        }
    }

end ClickTargetResolutionTest
