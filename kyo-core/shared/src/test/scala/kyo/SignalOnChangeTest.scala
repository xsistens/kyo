package kyo

/** `Signal.onChange`: a scoped observation whose callback runs inside the writer's `set` where the signal has a callback path, and
  * on a fiber of the enclosing Scope everywhere else.
  */
class SignalOnChangeTest extends kyo.test.Test[Any]:

    private def recorder[A](using Frame): (AtomicRef[Chunk[A]], A => Unit < Sync) < Sync =
        AtomicRef.init(Chunk.empty[A]).map(seen => (seen, (a: A) => seen.updateAndGet(_.append(a)).unit))

    "on the callback path" - {

        "a ref delivers inside the writer's set" in {
            Scope.run {
                for
                    ref         <- Signal.initRef(0)
                    (seen, rec) <- recorder[Int]
                    _           <- ref.onChange(rec)
                    _           <- ref.set(1)
                    // No yield between `set` and the read: the delivery has already happened.
                    result <- seen.get
                yield assert(result == Chunk(0, 1))
            }
        }

        "a map chain delivers inside the writer's set and only its distinct values" in {
            Scope.run {
                for
                    ref         <- Signal.initRef(0)
                    (seen, rec) <- recorder[Boolean]
                    _           <- ref.map(_ * 2).map(_ == 4).onChange(rec)
                    _           <- ref.set(1)
                    _           <- ref.set(2)
                    afterTwo    <- seen.get
                    _           <- ref.set(3)
                    _           <- ref.set(5)
                    result      <- seen.get
                yield assert(afterTwo == Chunk(false, true) && result == Chunk(false, true, false))
            }
        }

        "a write that does not change the value delivers nothing" in {
            Scope.run {
                for
                    ref         <- Signal.initRef(0)
                    (seen, rec) <- recorder[Int]
                    _           <- ref.onChange(rec)
                    _           <- ref.set(0)
                    _           <- ref.set(1)
                    _           <- ref.set(1)
                    result      <- seen.get
                yield assert(result == Chunk(0, 1))
            }
        }

        "the baseline skips the current value while it still holds" in {
            Scope.run {
                for
                    ref         <- Signal.initRef(0)
                    (seen, rec) <- recorder[Int]
                    _           <- ref.onChange(Present(0))(rec)
                    initial     <- seen.get
                    _           <- ref.set(1)
                    result      <- seen.get
                yield assert(initial.isEmpty && result == Chunk(1))
            }
        }

        "a baseline that no longer holds delivers the current value" in {
            Scope.run {
                for
                    ref         <- Signal.initRef(1)
                    (seen, rec) <- recorder[Int]
                    _           <- ref.onChange(Present(0))(rec)
                    result      <- seen.get
                yield assert(result == Chunk(1))
            }
        }

        "closing the scope releases the registration and stops delivery" in {
            for
                ref         <- Signal.initRef(0)
                (seen, rec) <- recorder[Int]
                parked      <- Scope.run(ref.onChange(rec).andThen(ref.onChange(rec)).andThen(ref.waiters))
                released    <- ref.waiters
                _           <- ref.set(1)
                result      <- seen.get
            yield assert(parked == 1 && released == 0 && result == Chunk(0, 0))
        }

        "a failing callback neither aborts the writer's set nor silences the other subscribers" in {
            Scope.run {
                for
                    ref         <- Signal.initRef(0)
                    (seen, rec) <- recorder[Int]
                    _           <- ref.onChange(v => if v == 1 then throw new RuntimeException("boom") else Kyo.unit)
                    _           <- ref.onChange(rec)
                    _           <- ref.set(1)
                    _           <- ref.set(2)
                    result      <- seen.get
                yield assert(result == Chunk(0, 1, 2))
            }
        }

        "a failing callback keeps its subscription" in {
            Scope.run {
                for
                    ref         <- Signal.initRef(0)
                    (seen, rec) <- recorder[Int]
                    _           <- ref.onChange(v => rec(v).andThen(if v == 1 then throw new RuntimeException("boom")))
                    _           <- ref.set(1)
                    _           <- ref.set(2)
                    result      <- seen.get
                yield assert(result == Chunk(0, 1, 2))
            }
        }
    }

    "on the fiber path" - {

        def raw(ref: SignalRef[Int])(using Frame): Signal[Int] =
            Signal.initRaw[Int](
                currentWith = [B, S] => f => ref.currentWith(f),
                nextWith = [B, S] => f => ref.nextWith(f)
            )

        "a combinator delivers its changes and stops when the scope closes" in {
            for
                a           <- Signal.initRef(0)
                b           <- Signal.initRef(0)
                (seen, rec) <- recorder[(Int, Int)]
                _           <- Scope.run {
                    for
                        _ <- a.combineLatest(b).onChange(rec)
                        _ <- assertEventually(seen.get.map(_ == Chunk((0, 0))))
                        _ <- a.set(1)
                        _ <- assertEventually(seen.get.map(_.lastMaybe == Present((1, 0))))
                    yield ()
                }
                closed <- seen.get
                _      <- a.set(2)
                _      <- Async.sleep(50.millis)
                result <- seen.get
                _      <- assertEventually(a.waiters.map(_ == 0))
            yield assert(result == closed)
        }

        "a raw signal honours the baseline and stops when the scope closes" in {
            for
                ref         <- Signal.initRef(0)
                (seen, rec) <- recorder[Int]
                _           <- Scope.run {
                    for
                        _ <- raw(ref).onChange(Present(0))(rec)
                        _ <- ref.set(1)
                        _ <- assertEventually(seen.get.map(_ == Chunk(1)))
                    yield ()
                }
                _      <- ref.set(2)
                _      <- Async.sleep(50.millis)
                result <- seen.get
                _      <- assertEventually(ref.waiters.map(_ == 0))
            yield assert(result == Chunk(1))
        }

        "a failing callback is logged and the fiber keeps observing" in {
            Scope.run {
                for
                    ref         <- Signal.initRef(0)
                    (seen, rec) <- recorder[Int]
                    _           <- raw(ref).onChange(v => rec(v).andThen(if v == 1 then throw new RuntimeException("boom")))
                    _           <- assertEventually(seen.get.map(_ == Chunk(0)))
                    _           <- ref.set(1)
                    _           <- assertEventually(seen.get.map(_ == Chunk(0, 1)))
                    _           <- ref.set(2)
                    _           <- assertEventually(seen.get.map(_ == Chunk(0, 1, 2)))
                yield succeed
            }
        }
    }

end SignalOnChangeTest
