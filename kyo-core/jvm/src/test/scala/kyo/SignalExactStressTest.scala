package kyo

import AllowUnsafe.embrace.danger
import java.util.concurrent.CyclicBarrier
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future as JFuture
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.LockSupport
import kyo.Signal.SignalRef

/** JVM-only stress tests for the exact observation of combinators (`Signal.readExact` / `Signal.ExactRead`).
  *
  * They need OS-thread parallelism: the races are between a fiber registering one `ExactRead` on several next-change promises and other
  * threads completing those promises (by a write) or interrupting the fiber. Every observation uses `observe(1.hour)` (or awaits an
  * `ExactRead` directly), so a lost wakeup cannot be hidden by the repair timer: it shows up as a missed deadline far below the interval.
  */
class SignalExactStressTest extends kyo.test.Test[Any]:

    private val wakeDeadlineMs   = 2000L
    private val settleDeadlineMs = 2000L

    private def now(): Long = java.lang.System.nanoTime()

    /** Writes 1..`writes` to `src` in order once `start` opens, with short random pauses, so the source only grows. */
    private def writer(start: java.util.concurrent.CountDownLatch, src: SignalRef.Unsafe[Int], writes: Int): Runnable = () =>
        start.await()
        val tr = ThreadLocalRandom.current()
        var i  = 1
        while i <= writes do
            src.set(i)
            if tr.nextInt(2) == 0 then spinNanos(tr.nextLong(4_000L))
            i += 1
        end while
    end writer

    /** Waits until `cond` holds, spinning briefly and then parking. Answers whether it held before the deadline. */
    private def waitUntil(deadlineMs: Long)(cond: => Boolean): Boolean =
        val end   = now() + deadlineMs * 1_000_000L
        var spins = 0
        while !cond do
            if now() > end then return cond
            if spins < 2000 then
                Thread.onSpinWait()
                spins += 1
            else LockSupport.parkNanos(20_000L)
            end if
        end while
        true
    end waitUntil

    private def report(q: java.util.concurrent.ConcurrentLinkedQueue[String], msg: String): Unit = discard(q.add(msg))

    private def spinNanos(n: Long): Unit =
        val end = now() + n
        while now() < end do Thread.onSpinWait()

    private def waitersOf(refs: Array[SignalRef.Unsafe[?]]): Seq[Int] = refs.toSeq.map(_.waiters())

    private def withPool[A](n: Int)(f: ExecutorService => A): A =
        val pool = Executors.newFixedThreadPool(
            n,
            r =>
                val t = new Thread(r, "signal-exact-stress")
                t.setDaemon(true)
                t
        )
        try f(pool)
        finally
            pool.shutdownNow()
            discard(pool.awaitTermination(5, TimeUnit.SECONDS))
        end try
    end withPool

    // ----------------------------------------------------------------------------------------------------------------
    // Layer 1: one ExactRead, raced directly.
    // ----------------------------------------------------------------------------------------------------------------

    "ExactRead under a racing write or interrupt" - {

        // Each round reads 2..8 fresh refs into one ExactRead and parks a fiber on `awaitChange`. A second thread waits until the
        // fiber has registered on ref `j` (its waiter count turns 1), which puts it inside or just past the registration loop, then
        // writes ref `k`. With `k > j` the write can land before `k` is registered (needs the re-check after `watch`); with `k <= j` it
        // completes a promise already registered while the loop still runs (needs the `done()` re-check, or the registrations made after
        // the completion leak). The fiber must wake, and afterwards no ref may hold a registration.
        "a write racing registration always wakes the waiter and leaves no registration".timeout(120.seconds) in {
            Sync.defer {
                val rounds                                     = 40_000
                val barrier                                    = new CyclicBarrier(2)
                val lost                                       = new AtomicInteger
                val leaks                                      = new AtomicInteger
                val firstLeak                                  = new AtomicReference[String](null)
                val firstLost                                  = new AtomicReference[String](null)
                @volatile var refs: Array[SignalRef.Unsafe[?]] = null
                @volatile var j                                = 0
                @volatile var k                                = 0

                val writer = new Thread(() =>
                    var running = true
                    while running do
                        barrier.await()
                        val rs = refs
                        if rs == null then running = false
                        else
                            // Bounded: if the fiber never registers, write anyway so the round still ends.
                            discard(waitUntil(wakeDeadlineMs)(rs(j).waiters() > 0))
                            rs(k).asInstanceOf[SignalRef.Unsafe[Int]].set(1)
                            discard(barrier.await())
                        end if
                    end while
                )
                writer.setDaemon(true)
                writer.start()

                val rnd = ThreadLocalRandom.current()
                var r   = 0
                val t0  = now()
                while r < rounds && lost.get() + leaks.get() < 5 do
                    val n  = 2 + rnd.nextInt(7)
                    val rs = Array.fill[SignalRef.Unsafe[?]](n)(SignalRef.Unsafe.init(0))
                    refs = rs
                    j = rnd.nextInt(n)
                    k = rnd.nextInt(n)
                    val read = new Signal.ExactRead
                    discard(Signal.readAllExact(Chunk.from(rs.map(_.asInstanceOf[SignalRef.Unsafe[Int]].safe)), read, true))
                    barrier.await()
                    val fiber = Fiber.Unsafe.init(read.awaitChange)
                    barrier.await() // write done
                    if !waitUntil(wakeDeadlineMs)(fiber.done()) then
                        discard(lost.incrementAndGet())
                        discard(firstLost.compareAndSet(null, s"round $r: n=$n j=$j k=$k waiters=${waitersOf(rs)}"))
                        discard(fiber.interrupt())
                    else if !waitUntil(settleDeadlineMs)(rs.forall(_.waiters() == 0)) then
                        discard(leaks.incrementAndGet())
                        discard(firstLeak.compareAndSet(null, s"round $r: n=$n j=$j k=$k waiters=${waitersOf(rs)}"))
                    end if
                    r += 1
                end while
                refs = null
                discard(barrier.await())
                val ms = (now() - t0) / 1_000_000
                java.lang.System.err.println(s"[SignalExactStressTest] write-vs-registration: $r rounds in $ms ms, lost=${lost
                        .get()} leaks=${leaks.get()}")
                assert(
                    lost.get() == 0 && leaks.get() == 0,
                    s"$r rounds: ${lost.get()} lost wakeups (first: ${firstLost.get()}), ${leaks.get()} leaked registrations (first: ${firstLeak.get()})"
                )
            }
        }

        // Each round parks a fiber on a fresh ExactRead and interrupts it from another thread, either once it has registered on ref `j`
        // or after a random delay (which also lands before it runs or while it runs). In a third of the rounds a write races too. Once the
        // fiber is done no ref may hold a registration.
        "an interrupt racing registration leaves no registration".timeout(120.seconds) in {
            Sync.defer {
                val rounds                                     = 40_000
                val barrier                                    = new CyclicBarrier(2)
                val leaks                                      = new AtomicInteger
                val hung                                       = new AtomicInteger
                val firstLeak                                  = new AtomicReference[String](null)
                @volatile var refs: Array[SignalRef.Unsafe[?]] = null
                val fiber                                      = new AtomicReference[Fiber.Unsafe[Unit, Any]]
                @volatile var j                                = 0
                @volatile var mode                             = 0
                @volatile var delay                            = 0L

                val interrupter = new Thread(() =>
                    var running = true
                    while running do
                        barrier.await()
                        val rs = refs
                        if rs == null then running = false
                        else
                            val f = fiber.get()
                            if mode == 0 then discard(waitUntil(wakeDeadlineMs)(rs(j).waiters() > 0))
                            else spinNanos(delay)
                            if mode == 2 then rs(j).asInstanceOf[SignalRef.Unsafe[Int]].set(1)
                            discard(f.interrupt())
                            discard(barrier.await())
                        end if
                    end while
                )
                interrupter.setDaemon(true)
                interrupter.start()

                val rnd = ThreadLocalRandom.current()
                var r   = 0
                val t0  = now()
                while r < rounds && hung.get() + leaks.get() < 5 do
                    val n  = 2 + rnd.nextInt(7)
                    val rs = Array.fill[SignalRef.Unsafe[?]](n)(SignalRef.Unsafe.init(0))
                    refs = rs
                    j = rnd.nextInt(n)
                    mode = rnd.nextInt(3)
                    delay = rnd.nextLong(30_000L)
                    val read = new Signal.ExactRead
                    discard(Signal.readAllExact(Chunk.from(rs.map(_.asInstanceOf[SignalRef.Unsafe[Int]].safe)), read, true))
                    val f = Fiber.Unsafe.init(read.awaitChange)
                    fiber.set(f)
                    barrier.await()
                    barrier.await() // interrupt sent
                    if !waitUntil(wakeDeadlineMs)(f.done()) then discard(hung.incrementAndGet())
                    else if !waitUntil(settleDeadlineMs)(rs.forall(_.waiters() == 0)) then
                        discard(leaks.incrementAndGet())
                        discard(firstLeak.compareAndSet(null, s"round $r: n=$n j=$j mode=$mode waiters=${waitersOf(rs)}"))
                    end if
                    r += 1
                end while
                refs = null
                discard(barrier.await())
                val ms = (now() - t0) / 1_000_000
                java.lang.System.err.println(
                    s"[SignalExactStressTest] interrupt-vs-registration: $r rounds in $ms ms, hung=${hung.get()} leaks=${leaks.get()}"
                )
                assert(
                    hung.get() == 0 && leaks.get() == 0,
                    s"$r rounds: ${hung.get()} fibers never finished after interrupt, ${leaks.get()} leaked registrations (first: ${firstLeak.get()})"
                )
            }
        }

        // A write to `b` completes the observer's ExactRead and then removes it from `a` on the writer's thread. Many callbacks
        // registered on `a` after it make that removal slow, and the observer is interrupted at a random moment after the write. Once
        // its result is set, `a` must hold only those callbacks: the interrupt may not end the observer while the writer still removes.
        "an interrupt while a write still deregisters leaves no registration once the result is set".timeout(120.seconds) in {
            val others = 20_000
            Kyo.foreach(Chunk.from(1 to 60)) { round =>
                val ua = SignalRef.Unsafe.init(0)
                val ub = SignalRef.Unsafe.init(0)
                for
                    first <- Latch.init(1)
                    fiber <- Fiber.initUnscoped(ua.safe.combineLatest(ub.safe).observe(1.hour)(_ => first.release))
                    _     <- first.await
                    _     <- Sync.defer(discard(waitUntil(wakeDeadlineMs)(ua.waiters() == 1 && ub.waiters() == 1)))
                    _     <- Sync.defer {
                        val p = ua.next().lower
                        var i = 0
                        while i < others do
                            p.onComplete(_ => ())
                            i += 1
                    }
                    write <- Fiber.initUnscoped(ub.safe.set(1))
                    _     <- Sync.defer {
                        discard(waitUntil(wakeDeadlineMs)(ub.version() != 0L))
                        spinNanos(ThreadLocalRandom.current().nextLong(100_000L))
                    }
                    _     <- fiber.interrupt
                    _     <- fiber.getResult
                    after <- Sync.defer((ua.waiters(), ub.waiters()))
                    _     <- write.get
                yield if after == (others, 0) then Absent else Present(s"round $round: waiters $after, expected ($others, 0)")
                end for
            }.map { found =>
                val leaks = found.collect { case Present(msg) => msg }
                assert(leaks.isEmpty, s"${leaks.size} of 60 rounds, first: ${leaks.headOption.getOrElse("")}")
            }
        }
    }

    // ----------------------------------------------------------------------------------------------------------------
    // Layer 2: observers of each combinator, end to end.
    // ----------------------------------------------------------------------------------------------------------------

    /** What one combinator round needs: the observed signal over `sources`, and the waiter count each source holds while the observer
      * is idle on the final value.
      */
    private case class Setup(
        sources: Array[SignalRef.Unsafe[Int]],
        signal: Signal[Chunk[Int]],
        idleWaiters: () => Seq[Int],
        expected: () => Chunk[Int],
        monotonic: Boolean
    )

    private def foldWith(
        refs: Array[SignalRef.Unsafe[Int]],
        step: (Signal[Chunk[Int]], Signal[Int]) => Signal[Chunk[Int]]
    ): Signal[Chunk[Int]] =
        refs.tail.foldLeft(refs.head.safe.map(Chunk(_)): Signal[Chunk[Int]])((acc, r) => step(acc, r.safe))

    private def plain(refs: Array[SignalRef.Unsafe[Int]], sig: Signal[Chunk[Int]]): Setup =
        Setup(refs, sig, () => refs.toSeq.map(_ => 1), () => Chunk.from(refs.map(_.get())), monotonic = true)

    private val kinds: Seq[(String, Int => Setup)] = Seq(
        "combineLatest" -> { n =>
            val refs = Array.fill(n)(SignalRef.Unsafe.init(0))
            plain(refs, foldWith(refs, (acc, s) => acc.combineLatest(s).map((c, v) => c.append(v))))
        },
        "combineLatestAll" -> { n =>
            val refs = Array.fill(n)(SignalRef.Unsafe.init(0))
            plain(refs, Signal.combineLatestAll(refs.toSeq.map(_.safe)))
        },
        "zip" -> { n =>
            val refs = Array.fill(n)(SignalRef.Unsafe.init(0))
            plain(refs, foldWith(refs, (acc, s) => acc.zip(s).map((c, v) => c.append(v))))
        },
        "zipAll" -> { n =>
            val refs = Array.fill(n)(SignalRef.Unsafe.init(0))
            plain(refs, Signal.zipAll(refs.toSeq.map(_.safe)))
        },
        "switchMap" -> { n =>
            // Source 0 selects one of the other sources; the value is (selected index, its value), so consecutive values are distinct
            // exactly when the selection or the selected source moved.
            val refs   = Array.fill(n)(SignalRef.Unsafe.init(0))
            val inners = refs.tail.zipWithIndex.map((r, i) => r.safe.map(v => Chunk(i, v)))
            val sig    = refs.head.safe.switchMap(sel => inners(sel % inners.length))
            Setup(
                refs,
                sig,
                () =>
                    val sel = refs.head.get() % inners.length
                    1 +: (0 until inners.length).map(i => if i == sel then 1 else 0)
                ,
                () =>
                    val sel = refs.head.get() % inners.length
                    Chunk(sel, refs(sel + 1).get())
                ,
                monotonic = false
            )
        }
    )

    kinds.foreach { (name, mk) =>
        s"$name observer under concurrent writers and interrupts" - {

            // Each round: 2..8 sources, one observer with `observe(1.hour)`, one writer thread per source writing 1..K in order (so every
            // source only grows), with a little work in `f` so writes land while it runs and while it re-registers. In half the rounds the
            // observer is interrupted at a random moment while the writers run. Otherwise, once the writers stop, it must deliver the final
            // values within the deadline (no repair timer can help), then hold exactly the idle waiter count per source, and none once
            // interrupted. Throughout, `f` must never see the same value twice in a row, and (for the non-switching combinators) never a
            // source going backwards.
            s"converges without the repair timer and leaves no registration".timeout(120.seconds) in {
                Sync.defer {
                    val rounds = 1500
                    withPool(9) { pool =>
                        val rnd           = ThreadLocalRandom.current()
                        val failures      = new java.util.concurrent.ConcurrentLinkedQueue[String]
                        var maxConvergeUs = 0L
                        var deliveries    = 0L
                        val t0            = now()
                        var r             = 0
                        while r < rounds && failures.size < 5 do
                            val n         = 2 + rnd.nextInt(7)
                            val setup     = mk(n)
                            val sources   = setup.sources
                            val last      = new AtomicReference[Chunk[Int]](null)
                            val count     = new AtomicInteger
                            val violation = new AtomicReference[String](null)
                            val work      = rnd.nextInt(3)
                            val observer  = Fiber.Unsafe.init(setup.signal.observe(1.hour) { v =>
                                Sync.defer {
                                    val prev = last.get()
                                    if prev != null then
                                        if prev == v then discard(violation.compareAndSet(null, s"delivered $v twice in a row"))
                                        else if setup.monotonic && (0 until prev.size).exists(i => v(i) < prev(i)) then
                                            discard(violation.compareAndSet(null, s"went backwards: $prev then $v"))
                                    end if
                                    last.set(v)
                                    discard(count.incrementAndGet())
                                    if work > 0 then spinNanos(ThreadLocalRandom.current().nextLong(work * 5_000L))
                                }
                            })
                            if !waitUntil(wakeDeadlineMs)(last.get() != null) then report(failures, s"round $r: no first delivery")
                            else
                                val interruptMidway          = rnd.nextBoolean()
                                val start                    = new java.util.concurrent.CountDownLatch(1)
                                val writes                   = 1 + rnd.nextInt(200)
                                val futures: Seq[JFuture[?]] = sources.toSeq.map(src => pool.submit(writer(start, src, writes)))
                                start.countDown()
                                if interruptMidway then
                                    spinNanos(rnd.nextLong(50_000L))
                                    discard(observer.interrupt())
                                futures.foreach(f => discard(f.get(10, TimeUnit.SECONDS)))
                                if !interruptMidway then
                                    val tw       = now()
                                    val expected = setup.expected()
                                    if !waitUntil(wakeDeadlineMs)(last.get() == expected) then
                                        report(
                                            failures,
                                            s"round $r (n=$n): lost wakeup, last delivered ${last.get()}, expected $expected, waiters ${sources.toSeq
                                                    .map(_.waiters())}"
                                        )
                                    else
                                        maxConvergeUs = math.max(maxConvergeUs, (now() - tw) / 1000)
                                        val idle = setup.idleWaiters()
                                        if !waitUntil(settleDeadlineMs)(sources.toSeq.map(_.waiters()) == idle) then
                                            report(
                                                failures,
                                                s"round $r (n=$n): idle waiters ${sources.toSeq.map(_.waiters())}, expected $idle"
                                            )
                                        end if
                                    end if
                                    discard(observer.interrupt())
                                end if
                                if !waitUntil(wakeDeadlineMs)(observer.done()) then report(failures, s"round $r: observer did not stop")
                                else if !waitUntil(settleDeadlineMs)(sources.forall(_.waiters() == 0)) then
                                    report(
                                        failures,
                                        s"round $r (n=$n, interruptMidway=$interruptMidway): waiters after interrupt ${sources.toSeq
                                                .map(_.waiters())}"
                                    )
                                end if
                            end if
                            discard(observer.interrupt())
                            Option(violation.get()).foreach(v => report(failures, s"round $r (n=$n): $v"))
                            deliveries += count.get()
                            r += 1
                        end while
                        val ms = (now() - t0) / 1_000_000
                        java.lang.System.err.println(
                            s"[SignalExactStressTest] $name: $r rounds in $ms ms, $deliveries deliveries, max convergence $maxConvergeUs us, failures ${failures.size}"
                        )
                        assert(failures.isEmpty, failures.toArray.mkString("\n"))
                    }
                }
            }
        }
    }

end SignalExactStressTest
