package kyo

import kyo.internal.HtmlRenderer
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.UIExchange

/** Behavior tests for the non-fatal failure channel: [[UI.notices]] installs the app's sink, [[UI.notify]]
  * reports into it. Unlike the mount error routing, a notice never replaces content, so these assert the
  * sink and the surviving node rather than a rendering.
  *
  * The sink rides an inheritable `Local`, so the interesting case is reach: a notice raised inside a mount
  * effect, on a renderer fiber the engine forked, still finds a sink installed around the subscribe root.
  */
class NoticeTest extends kyo.test.Test[Any]:

    private def silentExchange: UIExchange =
        new UIExchange:
            def onChange(
                region: ReactiveRegion,
                path: Seq[String],
                contentContext: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                previous: Maybe[UI],
                changed: UI
            )(using Frame): Unit < Async = Kyo.unit

    private def subscribe(ui: UI)(using Frame): Unit < (Async & Scope) =
        ReactiveUI.normalize(ui, Seq.empty).map(root => ReactiveUI.subscribe(root, silentExchange).unit)

    private def boom = new RuntimeException("degraded")

    "notify reaches the installed sink" in {
        for
            seen <- AtomicRef.init[Maybe[String]](Absent)
            _    <- UI.notices(t => seen.set(Present(t.getMessage)))(UI.notify(boom))
            got  <- seen.get
        yield assert(got == Present("degraded"))
    }

    "the innermost sink wins" in {
        for
            outer <- AtomicInt.init(0)
            inner <- AtomicInt.init(0)
            _ <- UI.notices(_ => outer.incrementAndGet.unit) {
                UI.notices(_ => inner.incrementAndGet.unit)(UI.notify(boom))
            }
            o <- outer.get
            i <- inner.get
        yield
            assert(o == 0)
            assert(i == 1)
    }

    "notify without a sink is logged, not raised" in {
        UI.notify(boom).andThen(assert(true))
    }

    "a notice raised inside a mount effect reaches a sink installed around the subscribe root" in {
        for
            seen <- AtomicRef.init[Maybe[String]](Absent)
            gate <- Promise.init[Unit, Any]
            _ <- UI.notices(t => seen.set(Present(t.getMessage)).andThen(gate.completeUnitDiscard)) {
                Scope.run {
                    val effect = UI.notify(boom).andThen(UI.span("content").id("c"): UI)
                    subscribe(UI.div(UI.mounted(effect).keyed("k"))).andThen(gate.get)
                }
            }
            got <- seen.get
        yield assert(got == Present("degraded"))
    }

    "a notice does not replace the node's content" in {
        for
            seen  <- Promise.init[String, Any]
            htmls <- AtomicRef.init(List.empty[String])
            exchange = new UIExchange:
                def onChange(
                    region: ReactiveRegion,
                    path: Seq[String],
                    contentContext: ReactiveRegion.RegionIdentity,
                    parentContext: ReactiveRegion.ParentContext,
                    previous: Maybe[UI],
                    changed: UI
                )(using Frame): Unit < Async =
                    HtmlRenderer.render(changed, path).map { html =>
                        htmls.getAndUpdate(html :: _).unit.andThen {
                            if html.contains("content") then seen.completeDiscard(Result.succeed(html))
                            else Kyo.unit
                        }
                    }
            _ <- UI.notices(_ => Kyo.unit) {
                Scope.run {
                    val effect = UI.notify(boom).andThen(UI.span("content").id("c"): UI)
                    ReactiveUI.normalize(UI.div(UI.mounted(effect).keyed("k")), Seq.empty)
                        .map(root => ReactiveUI.subscribe(root, exchange).unit)
                        .andThen(seen.get)
                }
            }
            all <- htmls.get
        yield assert(!all.exists(_.contains("kyo-mount-error")))
    }
end NoticeTest
