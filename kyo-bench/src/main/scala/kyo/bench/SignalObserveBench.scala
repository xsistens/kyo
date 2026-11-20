package kyo.bench

import kyo.*
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Level
import org.openjdk.jmh.annotations.TearDown
import scala.compiletime.uninitialized

/** The observation paths of `Signal`, the ones a UI tree runs a thousand times over.
  *
  *   - `observeStartStop`: an observation of a `SignalRef` started and stopped, the setup a region pays per subscription. The pass fails
  *     with an `Abort`, which ends the observation without a fiber or a scheduler hop.
  *   - `fanOutExact`: one write to a `SignalRef` woken observers read through a `map`, the exact protocol of the `SignalRef` override.
  *   - `fanOutCallback`: the same fan-out through `onChange`, delivered inside the write: the control the fiber paths are compared to.
  *   - `refSetGet`: writes and reads of a `SignalRef` nobody observes.
  *   - `withLocalRead`: reads of a signal chosen from a `Local`, on the caller's stack, as a synchronous UI pass reads it.
  *
  * Each fan-out invocation is one write and waits for all `size` observers to see it; the other cases repeat their step `size` times.
  */
class SignalObserveBench extends BaseBench:

    import AllowUnsafe.embrace.danger

    val size: Int = 1000

    private case object Stop

    private val local: Local[Boolean] = Local.init(false)

    private var ref: SignalRef[Int]                       = uninitialized
    private var observers: Chunk[Fiber[Unit, Any]]        = Chunk.empty
    private val remaining                                 = new java.util.concurrent.atomic.AtomicInteger(0)
    @volatile private var done: Promise.Unsafe[Unit, Any] = uninitialized
    private var value                                     = 0

    private def run[A](v: A < (Async & Abort[Throwable])): A =
        Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(v).flatMap(_.block(Duration.Infinity))).getOrThrow

    /** One observer saw the write: the last of them ends the invocation. */
    private def arrive(): Unit =
        if remaining.decrementAndGet() == 0 then done.completeDiscard(Result.succeed(()))

    private def expect(n: Int): Promise.Unsafe[Unit, Any] =
        val p = Promise.Unsafe.init[Unit, Any]()
        done = p
        remaining.set(n)
        p
    end expect

    /** Starts `size` observers with `observe` and waits for their first delivery. */
    private def start(observe: Int => Unit < Sync => Unit < Async): Unit =
        ref = Sync.Unsafe.evalOrThrow(Signal.initRef(0))
        value = 0
        val first = expect(size)
        observers = Chunk.from((0 until size).map { i =>
            Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(observe(i)(Sync.Unsafe.defer(arrive()))))
        })
        run(first.safe.get)
    end start

    /** One write that every observer delivers. */
    private def fanOut(): Int =
        value += 1
        val p = expect(size)
        ref.unsafe.set(value)
        run(p.safe.get)
        value
    end fanOut

    // Each benchmark runs in a fork of its own, so a fan-out case starts its observers in its first warmup invocation and keeps
    // them for the trial.
    @TearDown(Level.Trial)
    def stopObservers(): Unit =
        observers.foreach(f => Sync.Unsafe.evalOrThrow(f.interrupt.unit))
        observers = Chunk.empty
    end stopObservers

    @Benchmark def observeStartStop(): Int =
        val r = Sync.Unsafe.evalOrThrow(Signal.initRef(0))
        run(Loop.repeat(size)(Abort.run[Stop.type](r.observe(_ => Abort.fail(Stop)))).andThen(size))
    end observeStartStop

    @Benchmark def fanOutExact(): Int =
        if observers.isEmpty then start(_ => body => ref.map(_ + 1).observe(_ => body))
        fanOut()

    @Benchmark def fanOutCallback(): Int =
        if observers.isEmpty then start(_ => body => Scope.run(ref.map(_ + 1).onChange(_ => body.unit).andThen(Async.never[Unit])))
        fanOut()

    @Benchmark def refSetGet(): Int =
        val r = Sync.Unsafe.evalOrThrow(Signal.initRef(0)).unsafe
        @annotation.tailrec
        def loop(i: Int, acc: Int): Int =
            if i == size then acc
            else
                r.set(i)
                loop(i + 1, acc + r.get())
        loop(0, 0)
    end refSetGet

    @Benchmark def withLocalRead(): Int =
        val on      = Sync.Unsafe.evalOrThrow(Signal.initRef(1))
        val off     = Sync.Unsafe.evalOrThrow(Signal.initRef(2))
        val chosen  = Signal.withLocal(local)(l => if l then on else off)
        val program =
            kyo.internal.SignalNow.Locals.take {
                Sync.Unsafe.defer {
                    // The read a synchronous UI pass makes.
                    @annotation.tailrec
                    def loop(i: Int, acc: Int): Int =
                        if i == size then acc
                        else loop(i + 1, acc + kyo.internal.SignalNow[Int](chosen))
                    loop(0, 0)
                }
            }
        val sum = Sync.Unsafe.evalOrThrow(local.let(true)(program))
        assert(sum == size, s"read the Local's default: $sum")
        sum
    end withLocalRead

end SignalObserveBench
