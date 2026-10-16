package kyo.apollo

import CountryFixture.*
import java.util.concurrent.CopyOnWriteArrayList
import kyo.*
import kyo.apollo.exception.ApolloException
import kyo.apollo.exception.DefaultApolloException
import kyo.internal.FailureFrame
import kyo.internal.Failures
import kyo.internal.HtmlRenderer
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.UIExchange

/** A live operation that fails after its first data has no effect row left to abort into, so it hands the failure to kyo-ui's
  * `onFailure` rules: the follow of a [[dataSignal]], a poll tick, a fire-and-forget mutation. The rules decide what the user sees,
  * the same way they decide it for a failed event handler.
  *
  * Notices are awaited on the channel the boundary's sink writes to, values on the signal that carries them; nothing waits on time.
  */
class ApolloFailureRulesSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** A boundary sink that records each notice's message, and the channel to await them on. */
    private def notices(using Frame): (Channel[String], Throwable => Unit < Async) < Sync =
        Channel.initUnscoped[String](64).map { ch =>
            val message: Throwable => String = {
                case apollo: ApolloException => apollo.message
                case other                   => String.valueOf(other.getMessage)
            }
            (ch, (ex: Throwable) => Abort.run[Closed](ch.put(message(ex))).unit)
        }

    private def nextNotice(ch: Channel[String])(using Frame): String < Async =
        Abort.run[Closed](ch.take).map(_.getOrThrow)

    /** Runs `v` with a boundary around it that has `sink` as its `onNotice`, as a tree would for the code inside it. */
    private def underSink[A, S](sink: Throwable => Unit < Async)(v: A < S)(using Frame): A < S =
        Failures.within(FailureFrame.Boundary(UI.boundary.onNotice(sink)))(v)

    /** Records every repaint's HTML and completes a promise the first time a marker shows. */
    final private class Recording(markers: Map[String, Fiber.Promise.Unsafe[String, Any]]):
        val emissions = new CopyOnWriteArrayList[String]()
        val exchange  = new UIExchange:
            def onChange(
                region: ReactiveRegion,
                path: Seq[String],
                contentContext: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                previous: Maybe[UI],
                changed: UI
            )(using Frame): Unit < Async =
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
    end Recording

    private def recording(markers: String*)(using Frame): (Recording, Map[String, Fiber[String, Any]]) < Sync =
        Sync.Unsafe.defer {
            val ps  = markers.map(m => m -> Fiber.Promise.Unsafe.init[String, Any]()).toMap
            val rec = new Recording(ps)
            (rec, ps.view.mapValues(_.safe: Fiber[String, Any]).toMap)
        }

    private def run(ui: UI, rec: Recording)(using Frame): Unit < (Async & Scope) =
        ReactiveUI.normalize(ui, Seq.empty).map(root => ReactiveUI.subscribe(root, rec.exchange).unit)

    "dataSignal" - {

        "a boundary rule that notifies keeps the mounted content, and the follow goes on" in {
            Scope.run {
                for
                    (seen, sink) <- notices
                    src          <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                    (rec, marks) <- recording("value:1", "value:2")
                    effect = src.dataSignal.map(sig => UI.span(sig.map(n => s"value:$n")).id("content"): UI)
                    page   = UI.boundary
                        .onFailure { case _: ApolloException => UI.Reaction.Notify }
                        .onNotice(sink)(UI.div(UI.mounted(effect).keyed("apollo")))
                    _        <- run(page, rec)
                    _        <- marks("value:1").get
                    _        <- src.set(QueryState.Failure(DefaultApolloException("socket hiccup")))
                    surfaced <- nextNotice(seen)
                    _        <- src.set(QueryState.Success(2, fromCache = false))
                    _        <- marks("value:2").get
                yield
                    assert(surfaced == "socket hiccup")
                    assert(rec.count("kyo-mount-error") == 0)
            }
        }

        "dataSignal(Notify) keeps the content where no rule matches, and the follow goes on" in {
            Scope.run {
                for
                    (seen, sink) <- notices
                    src          <- Signal.initRef[QueryState[Int]](QueryState.Success(1, fromCache = false))
                    (rec, marks) <- recording("value:1", "value:2")
                    effect = src.dataSignal(UI.Reaction.Notify).map(sig => UI.span(sig.map(n => s"value:$n")).id("content"): UI)
                    _        <- run(UI.boundary.onNotice(sink)(UI.div(UI.mounted(effect).keyed("apollo"))), rec)
                    _        <- marks("value:1").get
                    _        <- src.set(QueryState.Failure(DefaultApolloException("stale")))
                    surfaced <- nextNotice(seen)
                    _        <- src.set(QueryState.Success(2, fromCache = false))
                    _        <- marks("value:2").get
                yield
                    assert(surfaced == "stale")
                    assert(rec.count("kyo-mount-error") == 0)
            }
        }
    }

    "a failed poll tick reaches the rules, and the poll goes on" - {

        "pollingSignal" in {
            Scope.run {
                for
                    (seen, sink) <- notices
                    client       <- cached(new StaticEngine(body("Alice"), status = 503))
                    first        <- underSink(sink) {
                        Apollo.pollingSignal(client.query(CurrentUserQuery()), 5.millis).andThen(nextNotice(seen))
                    }
                    second <- nextNotice(seen)
                yield
                    assert(first.nonEmpty)
                    assert(second.nonEmpty)
            }
        }

        "QueryHandle.polling" in {
            Scope.run {
                for
                    (seen, sink) <- notices
                    client       <- cached(new StaticEngine(body("Alice"), status = 503))
                    _            <- underSink(sink) {
                        Apollo.query(client.query(CurrentUserQuery())).map(_.polling(5.millis))
                    }
                    _ <- nextNotice(seen)
                    _ <- nextNotice(seen)
                yield succeed
            }
        }

        "QueryHandle.startPolling" in {
            Scope.run {
                for
                    (seen, sink) <- notices
                    client       <- cached(new StaticEngine(body("Alice"), status = 503))
                    q            <- Apollo.query(client.query(CurrentUserQuery()))
                    _            <- underSink(sink)(q.startPolling(5.millis))
                    _            <- nextNotice(seen)
                    _            <- nextNotice(seen)
                    _            <- q.stopPolling
                yield succeed
            }
        }
    }

    "a poll tick that panics is reported like one that fails" in {
        for
            (seen, sink) <- notices
            _            <- underSink(sink)(ApolloSignal.pollTick(Abort.panic(new IllegalStateException("tick blew up"))))
            surfaced     <- nextNotice(seen)
        yield assert(surfaced.contains("tick blew up"))
    }

    "mutation" - {

        "submit records a failure in the state and does not raise it" in {
            Scope.run {
                for
                    client  <- cacheless(new StaticEngine(body("Alice"), status = 500))
                    m       <- Apollo.mutation(client.query(CurrentUserQuery()))
                    outcome <- Abort.run[Any](m.submit(()))
                    state   <- m.state.current
                yield
                    assert(outcome.isSuccess)
                    assert(state match
                        case MutationState.Failure(_) => true
                        case _                        => false)
            }
        }

        "run raises the failure, so a handler's onFailure rules see it" in {
            Scope.run {
                for
                    client <- cacheless(new StaticEngine(body("Alice"), status = 401))
                    m      <- Apollo.mutation(client.query(CurrentUserQuery()))
                    action: UI.Action = m.run(())
                    outcome <- Abort.run[Any](action)
                yield outcome match
                    case Result.Failure(ApolloException.Unauthenticated(_)) => succeed
                    case other                                              => fail(s"expected an unauthenticated failure, got $other")
            }
        }
    }
end ApolloFailureRulesSpec
