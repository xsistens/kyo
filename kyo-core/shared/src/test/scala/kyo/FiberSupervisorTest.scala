package kyo

import java.util.concurrent.CopyOnWriteArrayList

/** Tests for [[Fiber.Supervisor]]: the scoped `Fiber.init` reports non-interrupt terminal failures to the
  * ambient supervisor ([[Fiber.Supervisor.let]]), exactly once per fiber; interrupts never report;
  * `initUnscoped` and `Supervisor.none` opt out; no supervisor installed is the zero-delta path (the existing
  * FiberTest suite is the real regression check for that).
  */
class FiberSupervisorTest extends kyo.test.Test[Any]:

    final private class Recorder extends Fiber.Supervisor:
        val errors = new CopyOnWriteArrayList[Result.Error[Any]]()
        def onFailure(error: Result.Error[Any])(using AllowUnsafe): Unit =
            discard(errors.add(error))
        def failures: List[Result.Error[Any]] =
            import scala.jdk.CollectionConverters.*
            errors.asScala.toList
    end Recorder

    private val boom = new RuntimeException("boom")

    "supervised" - {
        "observes a scoped fiber's Abort failure" in {
            val sup = new Recorder
            Fiber.Supervisor.let(sup) {
                Scope.run {
                    for
                        fiber <- Fiber.init(Abort.fail(boom))
                        _     <- fiber.getResult
                    yield ()
                }
            }.map { _ =>
                assert(sup.failures == List(Result.Failure(boom)))
            }
        }

        "observes a panic" in {
            val sup = new Recorder
            Fiber.Supervisor.let(sup) {
                Scope.run {
                    for
                        // Sync.defer: the throw must happen INSIDE the running fiber (an eager
                        // by-name throw would panic the spawning fiber instead).
                        fiber <- Fiber.init(Sync.defer((throw boom): Unit))
                        _     <- fiber.getResult
                    yield ()
                }
            }.map { _ =>
                assert(sup.failures == List(Result.Panic(boom)))
            }
        }

        "interrupt does not notify" in {
            val sup = new Recorder
            Fiber.Supervisor.let(sup) {
                Scope.run {
                    for
                        fiber <- Fiber.init(Async.never)
                        _     <- fiber.interrupt
                        _     <- fiber.getResult.map(_ => ()).handle(Abort.run[Throwable](_))
                    yield ()
                }
            }.map { _ =>
                assert(sup.failures.isEmpty)
            }
        }

        "scope-close interrupt does not notify" in {
            val sup = new Recorder
            Fiber.Supervisor.let(sup) {
                Scope.run {
                    Fiber.init(Async.never).unit
                }
            }.map { _ =>
                assert(sup.failures.isEmpty)
            }
        }

        "an instantly-failed fiber still notifies (post-completion registration)" in {
            val sup = new Recorder
            Fiber.Supervisor.let(sup) {
                Scope.run {
                    for
                        fiber <- Fiber.init(Abort.fail(boom))
                        _     <- fiber.getResult
                    // registration happened after the fiber may already have settled;
                    // IOPromise.onComplete on a settled promise fires immediately.
                    yield ()
                }
            }.map { _ =>
                assert(sup.failures == List(Result.Failure(boom)))
            }
        }

        "a nested scoped fork inherits the supervisor (grandchild failure notifies)" in {
            val sup = new Recorder
            Fiber.Supervisor.let(sup) {
                Scope.run {
                    for
                        outer <- Fiber.init {
                            Scope.run {
                                Fiber.init(Abort.fail(boom)).map(_.getResult.unit)
                            }
                        }
                        _ <- outer.getResult
                    yield ()
                }
            }.map { _ =>
                assert(sup.failures == List(Result.Failure(boom)))
            }
        }

        "a successful fiber does not notify" in {
            val sup = new Recorder
            Fiber.Supervisor.let(sup) {
                Scope.run {
                    Fiber.init(42).map(_.get).map(r => assert(r == 42))
                }
            }.map { _ =>
                assert(sup.failures.isEmpty)
            }
        }

        "a non-Throwable typed failure reaches the supervisor as Result.Failure" in {
            val sup = new Recorder
            Fiber.Supervisor.let(sup) {
                Scope.run {
                    Fiber.init(Abort.fail("domain error")).map(_.getResult.unit)
                }
            }.map { _ =>
                assert(sup.failures == List(Result.Failure("domain error")))
            }
        }
    }

    "opt-out" - {
        "initUnscoped is not supervised" in {
            val sup = new Recorder
            Fiber.Supervisor.let(sup) {
                for
                    fiber <- Fiber.initUnscoped(Abort.fail(boom))
                    _     <- fiber.getResult
                yield ()
            }.map { _ =>
                assert(sup.failures.isEmpty)
            }
        }

        "Supervisor.none opts a subtree out" in {
            val sup = new Recorder
            Fiber.Supervisor.let(sup) {
                Fiber.Supervisor.none {
                    Scope.run {
                        Fiber.init(Abort.fail(boom)).map(_.getResult.unit)
                    }
                }
            }.map { _ =>
                assert(sup.failures.isEmpty)
            }
        }
    }

    "no supervisor installed: init behaves identically (failure stays unobserved)" in {
        Scope.run {
            for
                fiber  <- Fiber.init(Abort.fail(boom))
                result <- fiber.getResult
            yield assert(result == Result.Failure(boom))
        }
    }
end FiberSupervisorTest
