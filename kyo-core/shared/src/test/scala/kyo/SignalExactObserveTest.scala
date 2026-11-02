package kyo

// Unsafe: the exact reads this suite builds by hand are unsafe-tier objects.
import AllowUnsafe.embrace.danger

/** The exact observation of each combinator (`Signal.readExact`, `Signal.ExactRead`), as a table with one row per combinator.
  *
  * Every leaf runs under `Clock.withTimeControl` with nothing advancing it, and observes with a finite repair interval so that `zip` and
  * `zipAll` observe exactly too: no repair timer can hide a lost wakeup, which shows as a barrier that is never reached.
  */
class SignalExactObserveTest extends kyo.test.Test[Any]:

    private val neverRepairs = 1.hour

    /** One row of the table, over three sources that all start at zero.
      *
      * @param signal
      *   the observed signal over the sources
      * @param waiting
      *   the waiter count each source holds while the observer is idle, for the sources' values
      * @param image
      *   the value the signal holds for the sources' values
      */
    final private case class Kind(
        name: String,
        signal: Chunk[SignalRef[Int]] => Signal[Chunk[Int]],
        waiting: Chunk[Int] => Chunk[Int],
        image: Chunk[Int] => Chunk[Int]
    )

    private def fold(step: (Signal[Chunk[Int]], Signal[Int]) => Signal[Chunk[Int]])(refs: Chunk[SignalRef[Int]])(using
        Frame
    ): Signal[Chunk[Int]] =
        refs.tail.foldLeft(refs.head.map(Chunk(_)): Signal[Chunk[Int]])(step)

    private def plain(name: String, signal: Chunk[SignalRef[Int]] => Signal[Chunk[Int]]): Kind =
        Kind(name, signal, values => values.map(_ => 1), identity)

    private val kinds: Chunk[Kind] = Chunk(
        plain("combineLatest", fold((acc, s) => acc.combineLatest(s).map((c, v) => c.append(v)))),
        plain("combineLatestAll", refs => Signal.combineLatestAll(refs)),
        plain("zip", fold((acc, s) => acc.zip(s).map((c, v) => c.append(v)))),
        plain("zipAll", refs => Signal.zipAll(refs)),
        // Source 0 selects one of the other two; the value is the selected index and its value.
        Kind(
            "switchMap",
            refs => refs.head.switchMap(sel => refs(sel % 2 + 1).map(v => Chunk(sel % 2, v))),
            values => Chunk(1, if values(0) % 2 == 0 then 1 else 0, if values(0) % 2 == 1 then 1 else 0),
            values => Chunk(values(0) % 2, values(values(0) % 2 + 1))
        )
    )

    private def sources(using Frame): Chunk[SignalRef[Int]] < Sync =
        Kyo.foreach(Chunk(0, 0, 0))(Signal.initRef(_))

    private def valuesOf(refs: Chunk[SignalRef[Int]])(using Frame): Chunk[Int] < Sync =
        Kyo.foreach(refs)(_.current)

    private def waitersOf(refs: Chunk[SignalRef[Int]])(using Frame): Chunk[Int] < Sync =
        Kyo.foreach(refs)(_.waiters)

    /** Holds once the observer has delivered the image of the sources' values and parked on what they ask for. */
    private def settled(kind: Kind, refs: Chunk[SignalRef[Int]], seen: AtomicRef[Chunk[Chunk[Int]]])(using Frame): Boolean < Sync =
        for
            values  <- valuesOf(refs)
            waiting <- waitersOf(refs)
            last    <- seen.get.map(_.lastMaybe)
        yield waiting == kind.waiting(values) && last == Present(kind.image(values))

    private def record(seen: AtomicRef[Chunk[Chunk[Int]]], v: Chunk[Int])(using Frame): Unit < Sync =
        seen.updateAndGet(_.append(v)).unit

    kinds.foreach { kind =>
        kind.name - {

            "delivers every change to what it reads, once, and parks with one waiter on each" in {
                Clock.withTimeControl { _ =>
                    for
                        refs  <- sources
                        seen  <- AtomicRef.init(Chunk.empty[Chunk[Int]])
                        fiber <- Fiber.initUnscoped(kind.signal(refs).observe(neverRepairs)(record(seen, _)))
                        _     <- assertEventually(settled(kind, refs, seen))
                        _     <- Kyo.foreachDiscard(Chunk(0, 1, 2)) { i =>
                            refs(i).set(i * 2 + 1).andThen(assertEventually(settled(kind, refs, seen)))
                        }
                        result <- seen.get
                        _      <- fiber.interrupt
                    yield
                        val images = Chunk(Chunk(0, 0, 0), Chunk(1, 0, 0), Chunk(1, 3, 0), Chunk(1, 3, 5)).map(kind.image)
                        // A write to the source a switchMap has not selected changes nothing, so it delivers nothing.
                        val distinct =
                            images.foldLeft(Chunk.empty[Chunk[Int]])((acc, v) => if acc.lastMaybe == Present(v) then acc else acc.append(v))
                        assert(result == distinct, s"$result")
                    end for
                }
            }

            "a write from inside f reaches the observer with no timer" in {
                Clock.withTimeControl { _ =>
                    for
                        refs  <- sources
                        seen  <- AtomicRef.init(Chunk.empty[Chunk[Int]])
                        fiber <- Fiber.initUnscoped(kind.signal(refs).observe(neverRepairs) { v =>
                            record(seen, v).andThen(if v == kind.image(Chunk(0, 0, 0)) then refs(0).set(1) else Kyo.unit)
                        })
                        _      <- assertEventually(settled(kind, refs, seen))
                        result <- seen.get
                        _      <- fiber.interrupt
                    yield assert(result == Chunk(kind.image(Chunk(0, 0, 0)), kind.image(Chunk(1, 0, 0))), s"$result")
                }
            }

            "two writes while f runs deliver the newest value once" in {
                Clock.withTimeControl { _ =>
                    for
                        refs    <- sources
                        seen    <- AtomicRef.init(Chunk.empty[Chunk[Int]])
                        started <- Latch.init(1)
                        gate    <- Latch.init(1)
                        fiber   <- Fiber.initUnscoped(kind.signal(refs).observe(neverRepairs) { v =>
                            record(seen, v).andThen(if v == kind.image(Chunk(0, 0, 0)) then started.release.andThen(gate.await)
                            else Kyo.unit)
                        })
                        _      <- started.await
                        _      <- refs(0).set(1)
                        _      <- refs(0).set(3)
                        _      <- gate.release
                        _      <- assertEventually(settled(kind, refs, seen))
                        result <- seen.get
                        _      <- fiber.interrupt
                    yield assert(result == Chunk(kind.image(Chunk(0, 0, 0)), kind.image(Chunk(3, 0, 0))), s"$result")
                }
            }

            "a write and its revert while f runs deliver nothing, and the next change does" in {
                Clock.withTimeControl { _ =>
                    for
                        refs    <- sources
                        seen    <- AtomicRef.init(Chunk.empty[Chunk[Int]])
                        started <- Latch.init(1)
                        gate    <- Latch.init(1)
                        fiber   <- Fiber.initUnscoped(kind.signal(refs).observe(neverRepairs) { v =>
                            record(seen, v).andThen(if v == kind.image(Chunk(0, 0, 0)) then started.release.andThen(gate.await)
                            else Kyo.unit)
                        })
                        _      <- started.await
                        _      <- refs(0).set(1)
                        _      <- refs(0).set(0)
                        _      <- gate.release
                        _      <- assertEventually(settled(kind, refs, seen))
                        held   <- seen.get
                        _      <- refs(0).set(1)
                        _      <- assertEventually(settled(kind, refs, seen))
                        result <- seen.get
                        _      <- fiber.interrupt
                    yield
                        assert(held == Chunk(kind.image(Chunk(0, 0, 0))), s"$held")
                        assert(result == Chunk(kind.image(Chunk(0, 0, 0)), kind.image(Chunk(1, 0, 0))), s"$result")
                    end for
                }
            }

            "an interrupted parked observer leaves no registration" in {
                Clock.withTimeControl { _ =>
                    for
                        refs  <- sources
                        seen  <- AtomicRef.init(Chunk.empty[Chunk[Int]])
                        fiber <- Fiber.initUnscoped(kind.signal(refs).observe(neverRepairs)(record(seen, _)))
                        _     <- assertEventually(settled(kind, refs, seen))
                        _     <- fiber.interrupt
                        _     <- fiber.getResult
                        left  <- waitersOf(refs)
                    yield assert(left == Chunk(0, 0, 0), s"$left")
                }
            }

            "an observer interrupted while f waits leaves no registration and closes the value's scope" in {
                Clock.withTimeControl { _ =>
                    for
                        refs     <- sources
                        started  <- Latch.init(1)
                        never    <- Latch.init(1)
                        released <- AtomicBoolean.init(false)
                        fiber    <- Fiber.initUnscoped(kind.signal(refs).observe(neverRepairs) { _ =>
                            Scope.ensure(released.set(true)).andThen(started.release).andThen(never.await)
                        })
                        _    <- started.await
                        _    <- refs(0).set(1)
                        _    <- fiber.interrupt
                        _    <- fiber.getResult
                        left <- waitersOf(refs)
                        // The Scope's finalizers drain on a fiber of their own, so the result can come first.
                        _ <- assertEventually(released.get)
                    yield assert(left == Chunk(0, 0, 0), s"$left")
                }
            }
        }
    }

    "an exact read" - {

        def read(refs: Chunk[SignalRef[Int]]): Signal.ExactRead =
            val r = new Signal.ExactRead(Map.empty)
            discard(Signal.readAllExact(refs, r, wakesOnAnyInput = true))
            r
        end read

        "that a write beat returns at once and registers nowhere" in {
            for
                refs <- sources
                r    <- Sync.defer(read(refs))
                _    <- refs(1).set(1)
                _    <- r.awaitChange
                left <- waitersOf(refs)
            yield assert(left == Chunk(0, 0, 0), s"$left")
        }

        "parked on several refs is woken by a write to one and is registered on none of them once that write returns" in {
            for
                refs   <- sources
                r      <- Sync.defer(read(refs))
                waiter <- Fiber.initUnscoped(r.awaitChange)
                _      <- assertEventually(waitersOf(refs).map(_ == Chunk(1, 1, 1)))
                _      <- refs(1).set(1)
                left   <- waitersOf(refs)
                _      <- waiter.get
            yield assert(left == Chunk(0, 0, 0), s"$left")
        }
    }

end SignalExactObserveTest
