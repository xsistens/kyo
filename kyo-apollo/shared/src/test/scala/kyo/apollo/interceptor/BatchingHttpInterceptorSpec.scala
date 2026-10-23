package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequestBody

/** Drives [[BatchingHttpInterceptor]] against a recording engine under
  * `Clock.withTimeControl`: the batching window is an `Async.sleep` on the
  * controlled clock, so `control.advance` ends a window deterministically and
  * `awaitPendingSleepers` is the barrier "a window is open". No wall clock, no
  * platform timers.
  *
  * Covers coalescing within a window, the size cap, the unwrapped single request,
  * the pass-through GET, the failure shapes (non-2xx shared, an engine failure
  * shared as that failure, malformed array fails all with an engine failure, a
  * throwing send panics all), and the two ownership contracts: an interrupted
  * caller is not sent on its behalf, and ending the owning `Scope` stops the window
  * and answers whoever is still waiting. Two leaves run on real worker threads: a
  * batch member interrupted while the batch is cut must not leave the others
  * without a lead, and a caller interrupted as it joins an empty queue must not
  * leave the caller after it without a window.
  */
class BatchingHttpInterceptorSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** An [[HttpEngine]] that records every request and answers via `respond`. Lead
      * callers send from their own fibers, so the record is an atomic, appended when
      * the send runs.
      */
    final class RecordingEngine(respond: HttpEngine.Request => HttpEngine.Response < Abort[HttpEngineFailure]) extends HttpEngine:
        private val recorded               = AtomicRef.Unsafe.init(List.empty[HttpEngine.Request])(using AllowUnsafe.embrace.danger)
        def seen: List[HttpEngine.Request] = recorded.get()(using AllowUnsafe.embrace.danger)
        def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) =
            recorded.safe.updateAndGet(_ :+ request).andThen(respond(request))
    end RecordingEngine

    private val url = HttpUrl(Present("https"), "example.com", 443, "/graphql", Absent)

    private def post(body: String): HttpEngine.Request =
        HttpEngine.request(HttpMethod.POST, url, HttpHeaders.empty, HttpRequestBody.Text(body))

    private def batchAwareEngine: RecordingEngine =
        RecordingEngine { req =>
            req.fields.body match
                case HttpRequestBody.Text(b) if b.startsWith("[") =>
                    HttpEngine.response(HttpStatus.OK, """[{"data":{"value":1}},{"data":{"value":2}}]""")
                case _ => HttpEngine.response(HttpStatus.OK, """{"data":{"value":9}}""")
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
                else yieldOnce.andThen(Loop.continue)
            }
        }

    /** Park behind every runnable fiber once: forking and joining an empty fiber. */
    private def yieldOnce(using Frame): Unit < Async = Fiber.use[Nothing, Unit, Any, Async](())(_.get)

    private val interval = 10.millis

    private val queryName = """"query":"(\w+)"""".r

    /** Answers every query with its own name, `{"data":{"echo":"<name>"}}`, as an array for a batch, so each caller can tell
      * its own share from another's.
      */
    private def echoEngine: RecordingEngine =
        RecordingEngine { req =>
            req.fields.body match
                case HttpRequestBody.Text(b) =>
                    val echoes = queryName.findAllMatchIn(b).map(m => s"""{"data":{"echo":"${m.group(1)}"}}""").toList
                    HttpEngine.response(HttpStatus.OK, if b.startsWith("[") then echoes.mkString("[", ",", "]") else echoes.mkString)
                case _ => HttpEngine.response(HttpStatus(400), "not a JSON body")
        }

    /** Advance the controlled clock one window at a time, yielding between ticks, until `fiber` is done: a batch given
      * back to the queue gets its next window without any wall delay.
      */
    private def advanceUntilDone[A, S](control: Clock.TimeControl, fiber: Fiber[A, S])(using Frame): Unit < Async =
        fiber.done.map { done =>
            if done then ()
            else
                for
                    _ <- control.advance(interval, Duration.Zero)
                    _ <- yieldOnce
                    _ <- advanceUntilDone(control, fiber)
                yield ()
        }

    /** One round of the cut-versus-interrupt race on real worker threads: `a` waits alone, then `b` arrives and fills the
      * batch (the size cap cuts it in `b`'s fiber) while another fiber interrupts `a` after `lag` suspension steps, both
      * released by one latch. Returns `b`'s response; a `b` that no member leads never gets one.
      */
    private def interruptWhileCut(lag: Int)(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) =
        Clock.withTimeControl { control =>
            Scope.run {
                for
                    batching <- BatchingHttpInterceptor.init(interval, maxBatchSize = 2)
                    chain = chainOf(batching, echoEngine)
                    fa    <- Fiber.init(chain.proceed(post("""{"query":"a"}""")))
                    _     <- untilQueued(batching, 1)
                    start <- Latch.init(1)
                    fb    <- Fiber.init(start.await.andThen(chain.proceed(post("""{"query":"b"}"""))))
                    fi    <- Fiber.init(start.await.andThen(Loop.repeat(lag)(Sync.defer(()))).andThen(fa.interrupt))
                    _     <- start.release
                    _     <- fi.get
                    _     <- fa.getResult
                    _     <- advanceUntilDone(control, fb)
                    rb    <- fb.get
                yield rb
            }
        }

    /** One round of the join-versus-interrupt race on real worker threads: `x` joins an empty queue while another fiber
      * interrupts it after `lag` suspension steps and then sends `y`, both released by one latch. Returns whether `y` was
      * answered within a bounded number of windows and scheduler passes; a `y` that no window ever cuts never is.
      */
    private def interruptWhileJoining(lag: Int)(using Frame): Boolean < Async =
        Clock.withTimeControl { control =>
            Scope.run {
                for
                    batching <- BatchingHttpInterceptor.init(interval, maxBatchSize = 3)
                    chain = chainOf(batching, batchAwareEngine)
                    start <- Latch.init(1)
                    fx    <- Fiber.init(start.await.andThen(chain.proceed(post("""{"query":"x"}"""))))
                    fy    <- Fiber.init(
                        start.await.andThen(Loop.repeat(lag)(Sync.defer(()))).andThen(fx.interrupt)
                            .andThen(chain.proceed(post("""{"query":"y"}""")))
                    )
                    _        <- start.release
                    _        <- fx.getResult
                    answered <- Loop.indexed { i =>
                        fy.done.map { done =>
                            if done then Loop.done[Unit, Boolean](true)
                            else if i == 1000 then Loop.done[Unit, Boolean](false)
                            else control.advance(interval, Duration.Zero).andThen(yieldOnce).andThen(Loop.continue)
                        }
                    }
                yield answered
            }
        }

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
                assert(engine.seen.head.fields.body == HttpRequestBody.Text("""[{"query":"a"},{"query":"b"}]"""))
                assert(ra.fields.body == """{"data":{"value":1}}""")
                assert(rb.fields.body == """{"data":{"value":2}}""")
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
                assert(engine.seen.head.fields.body == HttpRequestBody.Text("""[{"query":"a"},{"query":"b"}]"""))
                assert(ra.fields.body == """{"data":{"value":1}}""")
                assert(rb.fields.body == """{"data":{"value":2}}""")
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
                assert(engine.seen.head.fields.body == HttpRequestBody.Text("""{"query":"solo"}""")) // no [ ]
                assert(ra.fields.body == """{"data":{"value":9}}""")
            end for
        }

        "a bodiless GET is forwarded immediately, never batched" in Clock.withTimeControl { control =>
            val engine = batchAwareEngine
            val get = HttpEngine.request(HttpMethod.GET, url.copy(rawQuery = Present("query=x")), HttpHeaders.empty, HttpRequestBody.Empty)
            for
                batching <- BatchingHttpInterceptor.init(interval)
                response <- chainOf(batching, engine).proceed(get)
            yield
                assert(engine.seen == List(get)) // passed straight through, alone
                assert(response.fields.body == """{"data":{"value":9}}""")
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
                assert(engine.seen.head.fields.body == HttpRequestBody.Text("""{"query":"b"}"""))
                assert(rb.fields.body == """{"data":{"value":9}}""")
                assert(ra.isPanic)
            end for
        }

        "a member interrupted while its batch is cut never strands the others" in {
            // The cut and the interrupt race on two worker threads, so one round rarely lands in the narrow window
            // between them (on JVM about 1 round in 1000 did before the fix). Many rounds, 8 at a time, and an interrupt
            // shifted by a few more suspension steps each round cover every order. A stranded b never completes, so a
            // regression shows as this leaf reaching its timeout.
            val rounds = 40000
            Async.foreach(0 until rounds, 8)(i => interruptWhileCut((i % 50) * 4)).map { responses =>
                assert(responses.map(_.fields.body).distinct == Chunk("""{"data":{"echo":"b"}}"""))
            }
        }

        "a caller interrupted while it waits for the window does not strand the rest of its batch" in Clock.withTimeControl {
            control =>
                // x and y wait in an open window; one fiber interrupts x and then queues z, which reaches the size cap
                // and cuts [x, y, z] at once. Whether x leaves before the cut, after it, or while the lead is being
                // handed out is up to the scheduler; y and z must be answered either way. A stranded caller waits on
                // nothing, so each round gives the batch a bounded number of windows and scheduler passes to finish;
                // a round that ends unanswered failed.
                val rounds   = 200
                val patience = 1000

                def round: Boolean < (Async & Scope) =
                    for
                        batching <- BatchingHttpInterceptor.init(interval, maxBatchSize = 3)
                        chain = chainOf(batching, batchAwareEngine)
                        fx       <- Fiber.init(chain.proceed(post("""{"query":"x"}""")))
                        _        <- untilQueued(batching, 1)
                        fy       <- Fiber.init(chain.proceed(post("""{"query":"y"}""")))
                        _        <- untilQueued(batching, 2)
                        fz       <- Fiber.init(fx.interrupt.andThen(chain.proceed(post("""{"query":"z"}"""))))
                        _        <- fx.getResult
                        answered <- Loop.indexed { i =>
                            fy.done.map { y =>
                                fz.done.map { z =>
                                    if y && z then Loop.done[Unit, Boolean](true)
                                    else if i == patience then Loop.done[Unit, Boolean](false)
                                    else control.advance(interval, Duration.Zero).andThen(yieldOnce).andThen(Loop.continue)
                                }
                            }
                        }
                    yield answered

                Kyo.foreach(Chunk.from(1 to rounds))(_ => Scope.run(round)).map { answered =>
                    val stranded = answered.count(!_)
                    assert(stranded == 0, s"$stranded of $rounds rounds left y or z waiting forever")
                }
        }

        "a caller interrupted as it joins an empty queue does not leave the next caller without a window" in {
            // The interrupt has to land between x joining the queue and x asking for its window, two steps with nothing
            // in between that a test could hold, so the leaf races them on worker threads with the interrupt shifted a
            // few suspension steps further each round. Before joining and asking became one step, about 1 round in 100
            // left y queued behind a window nobody had asked for.
            val rounds = 40000
            Async.foreach(0 until rounds, 8)(i => interruptWhileJoining((i % 50) * 4)).map { answered =>
                val stranded = answered.count(!_)
                assert(stranded == 0, s"$stranded of $rounds rounds left y waiting forever")
            }
        }

        "ending the owning Scope stops the window and answers waiting callers with Closed" in Clock.withTimeControl {
            control =>
                val engine = batchAwareEngine
                for
                    chainP <- Promise.init[HttpInterceptorChain, Any]
                    stop   <- Latch.init(1)
                    owner  <- Fiber.init(Scope.run {
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
            val engine = RecordingEngine(_ => HttpEngine.response(HttpStatus(503), "unavailable"))
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
                assert(ra == HttpEngine.response(HttpStatus(503), "unavailable"))
                assert(rb == HttpEngine.response(HttpStatus(503), "unavailable"))
            end for
        }

        "a 2xx body that is not an array of the batch size fails every caller" in Clock.withTimeControl { control =>
            val engine = RecordingEngine(_ => HttpEngine.response(HttpStatus.OK, """[{"data":{"value":1}}]"""))
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
