package kyo.apollo.interceptor

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.HttpMethod
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse

/** Drives [[BatchingHttpInterceptor]] against a recording engine under
  * `Clock.withTimeControl`: the batching window is an `Async.sleep` on the
  * controlled clock, so `control.advance` ends a window deterministically and
  * `awaitPendingSleepers` is the barrier "a window is open". No wall clock, no
  * platform timers.
  *
  * Covers coalescing within a window, the size cap, the unwrapped single request,
  * the pass-through GET, the failure shapes (non-2xx shared, an engine failure
  * shared as that failure, malformed array fails all with an engine failure, a
  * throwing send panics all), and the two ownership contracts from
  * P1-40: an interrupted caller is not sent on its behalf, and ending the owning
  * `Scope` stops the window and answers whoever is still waiting.
  */
class BatchingHttpInterceptorSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** An [[HttpEngine]] that records every request and answers via `respond`. Lead
      * callers send from their own fibers, so the record is an atomic, appended when
      * the send runs.
      */
    final class RecordingEngine(respond: HttpRequest => HttpResponse < Abort[HttpEngineFailure]) extends HttpEngine:
        private val recorded        = AtomicRef.Unsafe.init(List.empty[HttpRequest])(using AllowUnsafe.embrace.danger)
        def seen: List[HttpRequest] = recorded.get()(using AllowUnsafe.embrace.danger)
        def execute(request: HttpRequest)(using Frame): HttpResponse < (Async & Abort[HttpEngineFailure]) =
            recorded.safe.updateAndGet(_ :+ request).andThen(respond(request))
    end RecordingEngine

    private def post(body: String): HttpRequest =
        HttpRequest(HttpMethod.Post, "https://example.com/graphql", Nil, Some(body))

    private def batchAwareEngine: RecordingEngine =
        RecordingEngine { req =>
            req.body match
                case Some(b) if b.startsWith("[") =>
                    HttpResponse(200, Nil, """[{"data":{"value":1}},{"data":{"value":2}}]""")
                case _ => HttpResponse(200, Nil, """{"data":{"value":9}}""")
        }

    /** The interceptor wired the way the client wires it: at position 0 of a chain
      * whose next link is the engine, so the batched send reaches the engine.
      */
    private def chainOf(batching: BatchingHttpInterceptor, engine: HttpEngine): HttpInterceptorChain =
        DefaultHttpInterceptorChain(Chunk(batching), 0, engine)

    /** Yield to the scheduler until `batching` holds exactly `n` queued callers.
      * Forking and joining an empty fiber parks this fiber behind every runnable
      * one, so a forked `proceed` (or a withdrawing finalizer) gets to run before
      * the loop looks again — a barrier that needs no clock and no wall delay.
      */
    private def untilQueued(batching: BatchingHttpInterceptor, n: Int)(using Frame): Unit < Async =
        Loop.foreach {
            batching.queued.map { q =>
                if q == n then Loop.done
                else Fiber.use(())(_.get).andThen(Loop.continue)
            }
        }

    private val interval = 10.millis

    "BatchingHttpInterceptor" - {

        "two requests inside the window leave as one array and split back" in Clock.withTimeControl { control =>
            val engine = batchAwareEngine
            for
                batching <- BatchingHttpInterceptor.init(interval)
                chain = chainOf(batching, engine)
                fa <- Fiber.init(chain.proceed(post("""{"query":"a"}""")))
                _  <- control.awaitPendingSleepers(1) // a is queued and the window is open
                fb <- Fiber.init(chain.proceed(post("""{"query":"b"}""")))
                _  <- untilQueued(batching, 2)
                _  <- control.advance(interval, Duration.Zero)
                ra <- fa.get
                rb <- fb.get
            yield
                assert(engine.seen.length == 1) // one batched round trip
                assert(engine.seen.head.body == Some("""[{"query":"a"},{"query":"b"}]"""))
                assert(ra.body == """{"data":{"value":1}}""")
                assert(rb.body == """{"data":{"value":2}}""")
            end for
        }

        "reaching maxBatchSize dispatches at once, without waiting for the window" in Clock.withTimeControl { control =>
            val engine = batchAwareEngine
            for
                batching <- BatchingHttpInterceptor.init(interval, maxBatchSize = 2)
                chain = chainOf(batching, engine)
                fa <- Fiber.init(chain.proceed(post("""{"query":"a"}""")))
                _  <- control.awaitPendingSleepers(1)
                fb <- Fiber.init(chain.proceed(post("""{"query":"b"}""")))
                // No `advance`: the virtual clock never moves, so only the size cap can
                // have sent the batch.
                ra <- fa.get
                rb <- fb.get
            yield
                assert(engine.seen.length == 1)
                assert(engine.seen.head.body == Some("""[{"query":"a"},{"query":"b"}]"""))
                assert(ra.body == """{"data":{"value":1}}""")
                assert(rb.body == """{"data":{"value":2}}""")
            end for
        }

        "a single request is sent unwrapped, not as an array" in Clock.withTimeControl { control =>
            val engine = batchAwareEngine
            for
                batching <- BatchingHttpInterceptor.init(interval)
                chain = chainOf(batching, engine)
                fa <- Fiber.init(chain.proceed(post("""{"query":"solo"}""")))
                _  <- control.awaitPendingSleepers(1)
                _  <- control.advance(interval, Duration.Zero)
                ra <- fa.get
            yield
                assert(engine.seen.length == 1)
                assert(engine.seen.head.body == Some("""{"query":"solo"}""")) // no [ ]
                assert(ra.body == """{"data":{"value":9}}""")
            end for
        }

        "a bodiless GET is forwarded immediately, never batched" in Clock.withTimeControl { control =>
            val engine = batchAwareEngine
            val get    = HttpRequest(HttpMethod.Get, "https://example.com/graphql?query=x")
            for
                batching <- BatchingHttpInterceptor.init(interval)
                response <- chainOf(batching, engine).proceed(get)
            yield
                assert(engine.seen == List(get)) // passed straight through, alone
                assert(response.body == """{"data":{"value":9}}""")
            end for
        }

        "an interrupted caller is not in the batch" in Clock.withTimeControl { control =>
            val engine = batchAwareEngine
            for
                batching <- BatchingHttpInterceptor.init(interval)
                chain = chainOf(batching, engine)
                fa <- Fiber.init(chain.proceed(post("""{"query":"a"}""")))
                _  <- control.awaitPendingSleepers(1) // a is queued and the window is open
                _  <- fa.interrupt
                ra <- fa.getResult                    // a has run its finalizer and left the queue
                _  <- untilQueued(batching, 0)
                fb <- Fiber.init(chain.proceed(post("""{"query":"b"}""")))
                _  <- untilQueued(batching, 1)        // b alone is queued when the window ends
                _  <- control.advance(interval, Duration.Zero)
                rb <- fb.get
            yield
                // A batch of one: b left unwrapped — a's slot was gone before the window ended.
                assert(engine.seen.length == 1)
                assert(engine.seen.head.body == Some("""{"query":"b"}"""))
                assert(rb.body == """{"data":{"value":9}}""")
                assert(ra.isPanic)
            end for
        }

        "ending the owning Scope stops the window and answers waiting callers with Closed" in Clock.withTimeControl {
            control =>
                val engine = batchAwareEngine
                for
                    chainP <- Promise.init[HttpInterceptorChain, Any]
                    stop   <- Latch.init(1)
                    owner <- Fiber.init(Scope.run {
                        BatchingHttpInterceptor.init(interval).map { batching =>
                            chainP.completeDiscard(Result.succeed(chainOf(batching, engine))).andThen(stop.await)
                        }
                    })
                    chain  <- chainP.get
                    caller <- Fiber.init(chain.proceed(post("""{"query":"a"}""")))
                    _      <- control.awaitPendingSleepers(1) // a is queued and the window is open
                    _      <- stop.release                    // the owner leaves its Scope
                    early  <- caller.getResult
                    _      <- owner.get
                    _      <- control.advance(interval * 10, Duration.Zero)
                    late   <- Abort.run[Throwable](chain.proceed(post("""{"query":"b"}""")))
                yield
                    assert(engine.seen.isEmpty) // no window fired after the Scope ended
                    assert(early.panic.exists(_.isInstanceOf[Closed]))
                    assert(late.panic.exists(_.isInstanceOf[Closed]))
                end for
        }

        "a non-2xx batched response is shared verbatim by every caller" in Clock.withTimeControl { control =>
            val engine = RecordingEngine(_ => HttpResponse(503, Nil, "unavailable"))
            for
                batching <- BatchingHttpInterceptor.init(interval, maxBatchSize = 2)
                chain = chainOf(batching, engine)
                fa <- Fiber.init(chain.proceed(post("""{"query":"a"}""")))
                _  <- control.awaitPendingSleepers(1)
                fb <- Fiber.init(chain.proceed(post("""{"query":"b"}""")))
                ra <- fa.get
                rb <- fb.get
            yield
                assert(engine.seen.length == 1)
                assert(ra == HttpResponse(503, Nil, "unavailable"))
                assert(rb == HttpResponse(503, Nil, "unavailable"))
            end for
        }

        "a 2xx body that is not an array of the batch size fails every caller" in Clock.withTimeControl { control =>
            val engine = RecordingEngine(_ => HttpResponse(200, Nil, """[{"data":{"value":1}}]"""))
            for
                batching <- BatchingHttpInterceptor.init(interval, maxBatchSize = 2)
                chain = chainOf(batching, engine)
                fa <- Fiber.init(chain.proceed(post("""{"query":"a"}""")))
                _  <- control.awaitPendingSleepers(1)
                fb <- Fiber.init(chain.proceed(post("""{"query":"b"}""")))
                ra <- fa.getResult
                rb <- fb.getResult
            yield
                assert(engine.seen.length == 1)
                // No caller received its own response: an engine failure, which the
                // transport folds into a value, not a panic that would crash the query.
                Seq(ra, rb).foreach { r =>
                    assert(r.failure.exists {
                        case e: ApolloNetworkException => e.message.contains("not a JSON array of 2")
                    })
                }
            end for
        }

        "an engine failure of the batched send is every caller's failure, not a panic" in Clock.withTimeControl { control =>
            val down   = ApolloNetworkException("server unreachable")
            val engine = RecordingEngine(_ => Abort.fail(down))
            for
                batching <- BatchingHttpInterceptor.init(interval, maxBatchSize = 2)
                chain = chainOf(batching, engine)
                fa <- Fiber.init(chain.proceed(post("""{"query":"a"}""")))
                _  <- control.awaitPendingSleepers(1)
                fb <- Fiber.init(chain.proceed(post("""{"query":"b"}""")))
                ra <- fa.getResult
                rb <- fb.getResult
            yield
                assert(engine.seen.length == 1)
                assert(ra.failure.exists(_ eq down))
                assert(rb.failure.exists(_ eq down))
            end for
        }

        "a send that throws fails every caller of the batch" in Clock.withTimeControl { control =>
            val engine = RecordingEngine(_ => throw new RuntimeException("boom"))
            for
                batching <- BatchingHttpInterceptor.init(interval, maxBatchSize = 2)
                chain = chainOf(batching, engine)
                fa <- Fiber.init(chain.proceed(post("""{"query":"a"}""")))
                _  <- control.awaitPendingSleepers(1)
                fb <- Fiber.init(chain.proceed(post("""{"query":"b"}""")))
                ra <- fa.getResult
                rb <- fb.getResult
            yield
                assert(engine.seen.length == 1)
                assert(ra.panic.exists(_.getMessage == "boom"))
                assert(rb.panic.exists(_.getMessage == "boom"))
            end for
        }
    }
end BatchingHttpInterceptorSpec
