package kyo

/** `SignalRef.zoom`: a writable `SignalRef` over one part of another reference's value. */
class SignalZoomTest extends kyo.test.Test[Any]:

    final private case class Query(sort: String, page: Int, size: Int) derives CanEqual

    /** Like the sort of a table, whose change sends the reader back to the first page. */
    private def sortOf(query: SignalRef[Query])(using Frame): SignalRef[String] =
        query.zoom(_.sort)((q, s) => q.copy(sort = s, page = 0))

    private def pageOf(query: SignalRef[Query])(using Frame): SignalRef[Int] =
        query.zoom(_.page)((q, p) => q.copy(page = p))

    private def start(using Frame): SignalRef[Query] < Sync = Signal.initRef(Query("name", 3, 10))

    "reads its part and writes through to the whole" in {
        for
            query <- start
            sort = sortOf(query)
            read   <- sort.current
            _      <- sort.set("price")
            part   <- sort.get
            whole  <- query.get
            _      <- query.set(Query("stock", 1, 10))
            direct <- sort.current
        yield
            assert(read == "name" && part == "price" && direct == "stock")
            assert(whole == Query("price", 0, 10))
    }

    "writing the part it already holds writes nothing" in {
        for
            query <- start
            sort = sortOf(query)
            seen   <- AtomicRef.init(Chunk.empty[Query])
            before <- Sync.Unsafe.defer(query.unsafe.version())
            _      <- Scope.run {
                for
                    _ <- query.onChange(q => seen.updateAndGet(_.append(q)).unit)
                    _ <- sort.set("name")
                    _ <- sort.getAndSet("name")
                    _ <- sort.updateAndGet(identity)
                    _ <- sort.getAndUpdate(identity)
                    r <- sort.compareAndSet("name", "name")
                yield assert(r)
            }
            after  <- Sync.Unsafe.defer(query.unsafe.version())
            whole  <- query.get
            result <- seen.get
        yield
            // The write would have reset the page.
            assert(whole == Query("name", 3, 10), s"$whole")
            assert(after == before, s"the version moved from $before to $after")
            assert(result == Chunk(Query("name", 3, 10)), s"$result")
    }

    "observe runs once per distinct part and not on a change to another field" in {
        Clock.withTimeControl { _ =>
            for
                query <- start
                sort = sortOf(query)
                seen  <- AtomicRef.init(Chunk.empty[String])
                fiber <- Fiber.initUnscoped(sort.observe(s => seen.updateAndGet(_.append(s)).unit))
                _     <- assertEventually(seen.get.map(_ == Chunk("name")))
                _     <- assertEventually(query.waiters.map(_ == 1))
                _     <- query.updateAndGet(_.copy(size = 20))
                // The change woke the observer, which found its part unchanged and parked again.
                _      <- assertEventually(query.waiters.map(_ == 1))
                _      <- query.updateAndGet(_.copy(page = 5))
                _      <- assertEventually(query.waiters.map(_ == 1))
                still  <- seen.get
                _      <- sort.set("price")
                _      <- assertEventually(seen.get.map(_.size == 2))
                result <- seen.get
                _      <- fiber.interrupt
            yield
                assert(still == Chunk("name"), s"$still")
                assert(result == Chunk("name", "price"), s"$result")
        }
    }

    "onChange runs inside the writer's set, and not on a change to another field" in {
        Scope.run {
            for
                query <- start
                sort = sortOf(query)
                seen  <- AtomicRef.init(Chunk.empty[String])
                _     <- sort.onChange(s => seen.updateAndGet(_.append(s)).unit)
                first <- seen.get
                _     <- query.updateAndGet(_.copy(size = 20))
                other <- seen.get
                _     <- sort.set("price")
                // No yield between the set and the read: the delivery has already happened.
                written <- seen.get
                waiting <- query.waiters
            yield
                assert(first == Chunk("name") && other == Chunk("name"), s"$first, $other")
                assert(written == Chunk("name", "price"), s"$written")
                assert(waiting == 1, s"$waiting")
        }
    }

    "compareAndSet fails when the part differs, and looks at nothing else of the whole" in {
        for
            query <- start
            sort = sortOf(query)
            stale <- sort.compareAndSet("price", "stock")
            kept  <- query.get
            _     <- query.updateAndGet(_.copy(size = 20))
            fresh <- sort.compareAndSet("name", "stock")
            whole <- query.get
        yield
            assert(!stale && kept == Query("name", 3, 10), s"$stale, $kept")
            assert(fresh && whole == Query("stock", 0, 20), s"$fresh, $whole")
    }

    "a zoom of a zoom reads and writes through both, and observes its own part" in {
        Scope.run {
            for
                outer <- Signal.initRef(("left", Query("name", 3, 10)))
                query = outer.zoom(_._2)((o, q) => (o._1, q))
                sort  = sortOf(query)
                seen  <- AtomicRef.init(Chunk.empty[String])
                _     <- sort.onChange(s => seen.updateAndGet(_.append(s)).unit)
                read  <- sort.current
                _     <- sort.set("price")
                whole <- outer.get
                _     <- outer.updateAndGet(o => (o._1, o._2.copy(size = 20)))
                _     <- outer.updateAndGet(o => ("right", o._2))
                last  <- seen.get
            yield
                assert(read == "name")
                assert(whole == ("left", Query("price", 0, 10)), s"$whole")
                assert(last == Chunk("name", "price"), s"$last")
        }
    }

    "next" - {

        "completes only when the part changes" in {
            for
                query <- start
                sort = sortOf(query)
                fiber <- Fiber.initUnscoped(sort.next)
                _     <- assertEventually(query.waiters.map(_ == 1))
                _     <- query.updateAndGet(_.copy(size = 20))
                _     <- assertEventually(Kyo.zip(query.waiters, fiber.done).map((w, d) => w == 1 || d))
                done  <- fiber.done
                _     <- sort.set("price")
                v     <- fiber.get
            yield assert(!done && v == "price", s"$done, $v")
        }

        "an interrupted waiter leaves no registration on the root" in {
            for
                query <- start
                fiber <- Fiber.initUnscoped(sortOf(query).next)
                _     <- assertEventually(query.waiters.map(_ == 1))
                _     <- fiber.interrupt
                _     <- fiber.getResult
                left  <- query.waiters
            yield assert(left == 0, s"$left")
        }

        "through the unsafe tier" in {
            for
                query <- start
                sort = sortOf(query).unsafe
                fiber <- Sync.Unsafe.defer(sort.next())
                _     <- assertEventually(query.waiters.map(_ == 1))
                _     <- query.updateAndGet(_.copy(size = 20))
                _     <- assertEventually(Kyo.zip(query.waiters, Sync.Unsafe.defer(fiber.done())).map((w, d) => w == 1 || d))
                done  <- Sync.Unsafe.defer(fiber.done())
                _     <- Sync.Unsafe.defer(sort.set("price"))
                v     <- fiber.safe.get
            yield assert(!done && v == "price", s"$done, $v")
        }

        "a write landing before the wait is not lost" in {
            for
                query <- start
                sort = sortOf(query)
                fiber <- Sync.Unsafe.defer(sort.unsafe.next())
                // The baseline is the value when `next` was asked for, whether or not the fiber has started yet.
                _ <- sort.set("price")
                v <- fiber.safe.get
            yield assert(v == "price", s"$v")
        }

        "a map over it does not wait past a write landing inside its function" in {
            // Unsafe: a map's function is pure in its type, so only the unsafe tier can write from inside it.
            import AllowUnsafe.embrace.danger
            for
                query <- start
                page = pageOf(query)
                v <- page.map { p =>
                    if p == 3 then page.unsafe.set(4)
                    p % 2
                }.next
            yield assert(v == 0, s"$v")
            end for
        }
    }

    "is a SignalRef to whoever holds it" - {

        "its atomic operations return the part" in {
            for
                query <- start
                page = pageOf(query)
                a     <- page.getAndSet(4)
                b     <- page.getAndUpdate(_ + 1)
                c     <- page.updateAndGet(_ + 1)
                whole <- query.get
            yield assert(a == 3 && b == 4 && c == 6 && whole == Query("name", 6, 10), s"$a, $b, $c, $whole")
        }

        "its unsafe tier reads and writes the part" in {
            for
                query <- start
                sort = sortOf(query).unsafe
                read  <- Sync.Unsafe.defer(sort.get())
                _     <- Sync.Unsafe.defer(sort.set("price"))
                back  <- Sync.Unsafe.defer(sort.safe.unsafe.get())
                moved <- Sync.Unsafe.defer(sort.version())
                root  <- Sync.Unsafe.defer(query.unsafe.version())
            yield assert(read == "name" && back == "price" && moved == root, s"$read, $back, $moved, $root")
        }

        "a map and a readOnly over it deduplicate on the part" in {
            Scope.run {
                for
                    query <- start
                    sort = sortOf(query)
                    mapped <- AtomicRef.init(Chunk.empty[Int])
                    viewed <- AtomicRef.init(Chunk.empty[String])
                    _      <- sort.map(_.length).onChange(n => mapped.updateAndGet(_.append(n)).unit)
                    _      <- sort.readOnly.onChange(s => viewed.updateAndGet(_.append(s)).unit)
                    _      <- query.updateAndGet(_.copy(page = 9))
                    _      <- sort.set("cost")
                    _      <- sort.set("price")
                    m      <- mapped.get
                    v      <- viewed.get
                yield assert(m == Chunk(4, 5) && v == Chunk("name", "cost", "price"), s"$m, $v")
            }
        }

        "a combinator over it observes exactly" in {
            Clock.withTimeControl { _ =>
                for
                    query <- start
                    other <- Signal.initRef(0)
                    sort = sortOf(query)
                    seen  <- AtomicRef.init(Chunk.empty[(String, Int)])
                    fiber <- Fiber.initUnscoped(sort.combineLatest(other).observe(1.hour)(v => seen.updateAndGet(_.append(v)).unit))
                    _     <- assertEventually(seen.get.map(_ == Chunk(("name", 0))))
                    _     <- assertEventually(query.waiters.map(_ == 1))
                    _     <- sort.set("price")
                    _     <- assertEventually(seen.get.map(_.lastMaybe == Present(("price", 0))))
                    _     <- fiber.interrupt
                    _     <- fiber.getResult
                    left  <- query.waiters
                    all   <- seen.get
                yield assert(left == 0 && all == Chunk(("name", 0), ("price", 0)), s"$left, $all")
            }
        }
    }

end SignalZoomTest
