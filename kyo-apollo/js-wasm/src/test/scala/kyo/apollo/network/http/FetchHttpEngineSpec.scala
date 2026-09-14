package kyo.apollo.network.http

import kyo.*
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.HttpEngineFailure
import org.scalajs.dom
import scala.scalajs.js as sjs

/** The [[FetchHttpEngine]]'s request lifecycle against a stubbed global `fetch`: an
  * interrupted request aborts its `fetch` through the signal the engine hands it, and
  * a rejection with `AbortError` reaches kyo as an interrupt, not as a network failure.
  *
  * The stub replaces `globalThis.fetch` for the duration of a leaf; the leaves run
  * sequentially so no two stubs overlap.
  */
class FetchHttpEngineSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    override def config = super.config.sequential

    private val engine = new FetchHttpEngine()

    private val request =
        HttpEngine.request(
            HttpMethod.POST,
            HttpUrl(Present("http"), "stub.invalid", 80, "/graphql", Absent),
            HttpHeaders.empty,
            HttpRequestBody.Text("{}")
        )

    /** A `fetch` that never answers. It completes [[called]] when invoked, and
      * [[aborted]] with `true` once the `signal` it was handed aborts (rejecting with an
      * `AbortError`, as the platform does) or with `false` at once when it was handed no
      * signal, so a test can wait on the outcome either way.
      */
    final private class HangingFetch:
        private given AllowUnsafe  = AllowUnsafe.embrace.danger
        private val calledPromise  = Fiber.Promise.Unsafe.init[Unit, Any]()
        private val abortedPromise = Fiber.Promise.Unsafe.init[Boolean, Any]()

        def called(using Frame): Unit < Async     = calledPromise.safe.get
        def aborted(using Frame): Boolean < Async = abortedPromise.safe.get

        val fn: sjs.Function2[sjs.Any, sjs.Dynamic, sjs.Promise[sjs.Any]] = (_, init) =>
            new sjs.Promise[sjs.Any]((_, reject) =>
                val signal = init.signal
                if sjs.isUndefined(signal) || signal == null then discard(abortedPromise.complete(Result.succeed(false)))
                else
                    signal.asInstanceOf[dom.AbortSignal].addEventListener(
                        "abort",
                        (_: dom.Event) =>
                            discard(abortedPromise.complete(Result.succeed(true)))
                            reject(abortError())
                    )
                end if
                discard(calledPromise.completeUnitDiscard())
            )
    end HangingFetch

    /** The platform's rejection value for an aborted `fetch`. */
    private def abortError(): sjs.Any =
        sjs.Dynamic.newInstance(sjs.Dynamic.global.DOMException)("The operation was aborted.", "AbortError")

    /** Run `v` with `stub` installed as the global `fetch`, restoring the original after. */
    private def withFetch[A, S](stub: sjs.Any)(v: => A < S)(using Frame): A < (S & Sync) =
        Sync.defer {
            val global   = sjs.Dynamic.global.globalThis
            val original = global.fetch
            global.fetch = stub
            original
        }.map(original => Sync.ensure(Sync.defer(sjs.Dynamic.global.globalThis.fetch = original))(v))

    "FetchHttpEngine" - {

        "an interrupted request aborts its fetch: under a timeout the signal aborts and the result is the timeout, not a network failure" in
            Clock.withTimeControl { control =>
                val fetch = HangingFetch()
                withFetch(fetch.fn) {
                    for
                        fiber <- Fiber.init(Abort.run[Timeout](Async.timeout(10.millis)(
                            Abort.run[HttpEngineFailure](engine.execute(request))
                        )))
                        _       <- fetch.called
                        _       <- control.advance(10.millis, Duration.Zero)
                        result  <- fiber.get
                        aborted <- fetch.aborted
                    yield
                        assert(aborted, "the fetch was not aborted when its request was interrupted")
                        result match
                            case Result.Failure(_: Timeout) => ()
                            case other                      => fail(s"expected the timeout, got $other")
                        end match
                    end for
                }
            }

        "streaming: an interrupt before the response head aborts the fetch too" in Clock.withTimeControl { control =>
            val fetch = HangingFetch()
            withFetch(fetch.fn) {
                for
                    fiber <- Fiber.init(Abort.run[Timeout](Async.timeout(10.millis)(
                        Abort.run[HttpEngineFailure](Scope.run(engine.executeStreaming(request).unit))
                    )))
                    _       <- fetch.called
                    _       <- control.advance(10.millis, Duration.Zero)
                    result  <- fiber.get
                    aborted <- fetch.aborted
                yield
                    assert(aborted, "the streaming fetch was not aborted when its request was interrupted")
                    assert(result.failure.exists(_.isInstanceOf[Timeout]), s"expected the timeout, got $result")
                end for
            }
        }

        "streaming: the fetch stays open while the Scope holds the response and is aborted when the Scope closes" in {
            var signal: sjs.UndefOr[dom.AbortSignal] = sjs.undefined
            val answering: sjs.Function2[sjs.Any, sjs.Dynamic, sjs.Promise[sjs.Any]] = (_, init) =>
                signal = init.signal.asInstanceOf[sjs.UndefOr[dom.AbortSignal]]
                val body = sjs.Dynamic.newInstance(sjs.Dynamic.global.ReadableStream)(sjs.Dynamic.literal())
                val response = sjs.Dynamic.newInstance(sjs.Dynamic.global.Response)(
                    body,
                    sjs.Dynamic.literal(status = 200, headers = sjs.Dynamic.literal("Content-Type" -> "multipart/mixed; boundary=b"))
                )
                sjs.Promise.resolve[sjs.Any](response)
            withFetch(answering) {
                for
                    openWhileHeld <- Scope.run(Abort.run[HttpEngineFailure](engine.executeStreaming(request)).map { head =>
                        Sync.defer((head.isSuccess, signal.exists(s => !s.aborted)))
                    })
                    abortedAfter <- Sync.defer(signal.exists(_.aborted))
                yield
                    assert(openWhileHeld == (true, true), s"the head did not arrive with an open fetch: $openWhileHeld")
                    assert(abortedAfter, "closing the Scope did not abort the streamed fetch")
                end for
            }
        }

        "a fetch rejected with AbortError is an interrupt, not an ApolloNetworkException" in {
            val rejecting: sjs.Function2[sjs.Any, sjs.Any, sjs.Promise[sjs.Any]] =
                (_, _) => sjs.Promise.reject(abortError()).asInstanceOf[sjs.Promise[sjs.Any]]
            withFetch(rejecting) {
                Abort.run[Throwable](engine.execute(request)).map {
                    case Result.Panic(e: Interrupted) => assert(e.by.exists(_.contains("aborted")), s"$e")
                    case Result.Failure(e: ApolloNetworkException) =>
                        fail(s"an AbortError was reported as a network failure: $e")
                    case other => fail(s"expected an Interrupted panic, got $other")
                }
            }
        }
    }
end FetchHttpEngineSpec
