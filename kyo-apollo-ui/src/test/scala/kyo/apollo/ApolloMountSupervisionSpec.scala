package kyo.apollo

import java.util.concurrent.CopyOnWriteArrayList
import kyo.*
import kyo.apollo.exception.DefaultApolloException
import kyo.internal.HtmlRenderer
import kyo.internal.ReactiveUI
import kyo.internal.UIExchange

/** End-to-end check of the seam between kyo-apollo's live operations and kyo-ui's node-scope
  * supervision: a component mounted with `UI.mounted` opens an apollo `dataSignal` in its effect, the
  * source turns into a tail `QueryState.Failure` long after the node published its UI, and the node
  * has to flip into its error rendering.
  *
  * The escalating `dataSignal` forks its follow observer with `UI.fork`, which is what carries the
  * failure to the node; a plain `Fiber.init` there dies just as loudly but the node never learns of
  * it, and the second test pins exactly that difference so the seam cannot silently rot.
  *
  * The engine is driven directly (`ReactiveUI.normalize`/`subscribe` with a recording `UIExchange`),
  * so this needs no browser and no network: the apollo source is a plain `Signal.initRef`.
  */
class ApolloMountSupervisionSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** Records every emission's rendered HTML and completes marker promises on first match. */
    final private class Recording(markers: Map[String, Fiber.Promise.Unsafe[String, Any]]):
        val emissions = new CopyOnWriteArrayList[String]()
        val exchange = new UIExchange:
            def onChange(path: Seq[String], changed: UI, mount: Boolean)(using Frame): Unit < Async =
                HtmlRenderer.render(changed, path).map { html =>
                    discard(emissions.add(html))
                    markers.foreach { (marker, p) =>
                        if html.contains(marker) then
                            import AllowUnsafe.embrace.danger
                            discard(p.complete(Result.succeed(html)))
                    }
                }
        def count(marker: String): Int =
            import scala.jdk.CollectionConverters.*
            emissions.asScala.count(_.contains(marker))
        def indexOf(marker: String): Int =
            import scala.jdk.CollectionConverters.*
            emissions.asScala.indexWhere(_.contains(marker))
    end Recording

    private def recording(markers: String*)(using Frame): (Recording, Map[String, Fiber[String, Any]]) < Sync =
        Sync.Unsafe.defer {
            val ps  = markers.map(m => m -> Fiber.Promise.Unsafe.init[String, Any]()).toMap
            val rec = new Recording(ps)
            (rec, ps.view.mapValues(_.safe: Fiber[String, Any]).toMap)
        }

    private def run(ui: UI, rec: Recording)(using Frame): Unit < (Async & Scope) =
        ReactiveUI.normalize(ui, Seq.empty).map(root => ReactiveUI.subscribe(root, rec.exchange).unit)

    "a tail failure of an apollo dataSignal flips the mounted node that opened it" in {
        Scope.run {
            for
                src          <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                (rec, marks) <- recording("value:1", "kyo-mount-error")
                effect = src.dataSignal.map(sig => UI.span(sig.map(n => s"value:$n")).id("content"): UI)
                _ <- run(UI.div(UI.mounted(effect).keyed("apollo")), rec)
                _ <- marks("value:1").get
                _ <- src.set(QueryState.Failure(DefaultApolloException("socket died")))
                _ <- marks("kyo-mount-error").get
            yield assert(rec.indexOf("value:1") < rec.indexOf("kyo-mount-error"))
        }
    }

    "the same feed forked with a plain Fiber.init leaves the node untouched (supervision is opt-in)" in {
        Scope.run {
            for
                src          <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                (rec, marks) <- recording("value:1")
                effect =
                    for
                        ref <- Signal.initRef(1)
                        _ <- Fiber.init(src.observe {
                            case QueryState.Success(d, _, _) => ref.set(d)
                            case QueryState.Failure(ex, _)   => Abort.fail(ex)
                            case _                           => (): Unit
                        })
                    yield UI.span(ref.map(n => s"value:$n")).id("content"): UI
                _ <- run(UI.div(UI.mounted(effect).keyed("apollo")), rec)
                _ <- marks("value:1").get
                _ <- src.set(QueryState.Failure(DefaultApolloException("socket died")))
                _ <- Async.sleep(200L.millis)
            yield assert(rec.count("kyo-mount-error") == 0)
        }
    }
end ApolloMountSupervisionSpec
