package kyo.uic.bench

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong
import kyo.*
import kyo.UI.*
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.UIExchange
import kyo.uic.*
import org.openjdk.jmh.annotations.{Scope as JmhScope, *}
import scala.compiletime.uninitialized

/** `UI.reactive` against the best hand-written form of each example, on what it costs to build and what one change costs.
  *
  * Each example is written twice: once as a block, the way a component writes it, and once by hand with the signal operations an author
  * who knows the data flow would pick (`map`, `combineLatest`, `render`, a channel setter handed the signal, a region per branch). Both
  * forms show the same thing; the contract suite (`ReactiveContractTest`) holds what they show, this measures what it costs.
  *
  *   - [[ReactiveBuildBench]]: building the UI or the prop, what a component pays on every draw.
  *   - [[ReactiveChangeBench]]: one write to an input until its effect is done. A prop is read back (`Prop.current`). A UI runs against a
  *     transport that only counts, and the write is done once every observer that waited on the input waits again, so a region has
  *     drawn and a channel has patched.
  *
  * Run outside sbt's run task: `Jmh/run` repackages every dependency as a jar, and kyo-net's native guard refuses its classifier jar where
  * BoringSSL is not staged, though nothing here touches the network. So `Jmh/compile`, then `export kyo-ui-components-baseJVM/Jmh/fullClasspath`
  * and `java -cp <that classpath> org.openjdk.jmh.Main -prof gc kyo.uic.bench`, for time and bytes allocated per operation.
  */
private object ReactiveBench:

    import AllowUnsafe.embrace.danger

    def ref[A](a: A)(using CanEqual[A, A], Frame): SignalRef[A] = Sync.Unsafe.evalOrThrow(Signal.initRef(a))

    /** A transport that paints nothing and records nothing, so a long run does not grow: channels patch on the writer's stack. */
    final class Counting extends UIExchange:
        val work = new AtomicLong(0)
        def onChange(
            region: ReactiveRegion,
            path: Seq[String],
            contentContext: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            ui: UI
        )(using Frame): Unit < Async = Sync.defer(discard(work.incrementAndGet()))
        override val attrPatcherNow: Maybe[(Seq[String], String, String) => Unit]      = Present((_, _, _) => discard(work.incrementAndGet()))
        override val boolAttrPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit] = Present((_, _, _) => discard(work.incrementAndGet()))
        override val classPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit]    = Present((_, _, _) => discard(work.incrementAndGet()))
        override val stylePatcherNow: Maybe[(Seq[String], Seq[(String, String)], Seq[String]) => Unit] =
            Present((_, _, _) => discard(work.incrementAndGet()))
        override val textPatcherNow: Maybe[(Seq[String], String) => Unit] = Present((_, _) => discard(work.incrementAndGet()))
    end Counting

    /** An example over props: the block, the hand-written prop, and the write a change makes. */
    abstract class PropExample:
        def block(using Frame): UI.Prop[Int]
        def hand(using Frame): UI.Prop[Int]
        def write(n: Int): Unit
    end PropExample

    /** An example over a UI: the block, the hand-written UI, the input a change writes, and a write that is sure to be seen. */
    abstract class UIExample:
        def block(using Frame): UI
        def hand(using Frame): UI
        def written: SignalRef[?]
        def write(n: Int): Unit
        def prime(n: Int): Unit
    end UIExample

    def propExample(name: String)(using Frame): PropExample = name match
        case "one-signal" =>
            new PropExample:
                val a                                = ref(0)
                def block(using Frame): UI.Prop[Int] = UI.reactive(a.read + 1)
                def hand(using Frame): UI.Prop[Int]  = a.map(_ + 1)
                def write(n: Int): Unit              = a.unsafe.set(n)
        case "two-signals" =>
            new PropExample:
                val a                                = ref(0)
                val b                                = ref(1)
                def block(using Frame): UI.Prop[Int] = UI.reactive(a.read + b.read)
                def hand(using Frame): UI.Prop[Int]  = a.combineLatest(b).map((x, y) => x + y)
                def write(n: Int): Unit              = a.unsafe.set(n)
        case "diamond" =>
            new PropExample:
                val x                                = ref(0)
                val d                                = x.map(_ * 2)
                def block(using Frame): UI.Prop[Int] = UI.reactive(d.read + x.read)
                def hand(using Frame): UI.Prop[Int]  = x.map(v => 3 * v)
                def write(n: Int): Unit              = x.unsafe.set(n)
        case "three-signals" =>
            new PropExample:
                val a                                = ref(0)
                val b                                = ref(1)
                val c                                = ref(2)
                def block(using Frame): UI.Prop[Int] = UI.reactive(a.read + b.read + c.read)
                def hand(using Frame): UI.Prop[Int]  = Signal.combineLatestAll(Chunk(a, b, c)).map(_.sum)
                def write(n: Int): Unit              = a.unsafe.set(n)
        case "four-signals" =>
            new PropExample:
                val a                                = ref(0)
                val b                                = ref(1)
                val c                                = ref(2)
                val d                                = ref(3)
                def block(using Frame): UI.Prop[Int] = UI.reactive(a.read + b.read + c.read + d.read)
                def hand(using Frame): UI.Prop[Int]  = Signal.combineLatestAll(Chunk(a, b, c, d)).map(_.sum)
                def write(n: Int): Unit              = a.unsafe.set(n)
        case "fan-in-six" =>
            new PropExample:
                val rs                               = Chunk.from(Seq.fill(6)(ref(1)))
                val (r0, r1, r2, r3, r4, r5)         = (rs(0), rs(1), rs(2), rs(3), rs(4), rs(5))
                def block(using Frame): UI.Prop[Int] = UI.reactive(r0.read + r1.read + r2.read + r3.read + r4.read + r5.read)
                def hand(using Frame): UI.Prop[Int]  = Signal.combineLatestAll(rs).map(_.sum)
                def write(n: Int): Unit              = r0.unsafe.set(n)
        case "optional" =>
            new PropExample:
                val a                                = ref(0)
                val o: Opt[UI.Prop[Int]]             = a
                def block(using Frame): UI.Prop[Int] = UI.reactive(o.read.getOrElse(0) + 1)
                def hand(using Frame): UI.Prop[Int]  = o.fold[UI.Prop[Int]](1)(p => UI.Prop.map(p)(_ + 1))
                def write(n: Int): Unit              = a.unsafe.set(n)
        case other => throw new IllegalArgumentException(s"no prop example $other")

    /** A block over constants and its hand-written form. Here and not in a benchmark class: JMH reflects the methods of its
      * classes, and a method the expansion leaves in one names a type the reflection cannot load.
      */
    private val k: UI.Prop[Int]              = 1
    def constantBlock(using Frame): UI     = UI.reactive(span(s"v=${k.read}"))
    def constantHand(using Frame): UI      = span(s"v=${1}")

    def uiExample(name: String)(using Frame): UIExample = name match
        case "attribute" =>
            new UIExample:
                val a                       = ref(0)
                def block(using Frame): UI  = UI.reactive(span.title(s"t${a.read}")("x"))
                def hand(using Frame): UI   = span.title(a.map(v => s"t$v"))("x")
                def written: SignalRef[?]   = a
                def write(n: Int): Unit     = a.unsafe.set(n)
                def prime(n: Int): Unit     = a.unsafe.set(n)
        case "class-flag" =>
            new UIExample:
                val hot                     = ref(false)
                def block(using Frame): UI  = UI.reactive(span.when(hot.read)(_.cssClass("hot"))("x"))
                def hand(using Frame): UI   = span.when(hot)(_.cssClass("hot"))("x")
                def written: SignalRef[?]   = hot
                def write(n: Int): Unit     = hot.unsafe.set(n % 2 == 0)
                def prime(n: Int): Unit     = hot.unsafe.set(!hot.unsafe.get())
        case "text" =>
            new UIExample:
                val a                       = ref(0)
                def block(using Frame): UI  = UI.reactive(span(s"v=${a.read}"))
                def hand(using Frame): UI   = a.render(v => span(s"v=$v"))
                def written: SignalRef[?]   = a
                def write(n: Int): Unit     = a.unsafe.set(n)
                def prime(n: Int): Unit     = a.unsafe.set(n)
        case "output-unchanged" =>
            new UIExample:
                val x                       = ref(1)
                def block(using Frame): UI  = UI.reactive(span(if x.read > 0 then "pos" else "neg"))
                def hand(using Frame): UI   = x.map(_ > 0).render(p => span(if p then "pos" else "neg"))
                def written: SignalRef[?]   = x
                def write(n: Int): Unit     = x.unsafe.set(n + 1)
                def prime(n: Int): Unit     = x.unsafe.set(-x.unsafe.get())
        case "untaken-branch" =>
            new UIExample:
                val c                       = ref(true)
                val a                       = ref("a")
                val b                       = ref("b")
                def block(using Frame): UI  = UI.reactive(span(if c.read then a.read else b.read))
                def hand(using Frame): UI   = c.render(on => if on then a.render(span(_)) else b.render(span(_)))
                def written: SignalRef[?]   = b
                def write(n: Int): Unit     = b.unsafe.set(s"b$n")
                def prime(n: Int): Unit     = a.unsafe.set(s"a$n")
        case "false-guard" =>
            new UIExample:
                val g                       = false
                val s                       = ref("x")
                val t                       = ref(0)
                def block(using Frame): UI  = UI.reactive(span(s"${t.read}:" + (if g && s.read.nonEmpty then "on" else "none")))
                def hand(using Frame): UI   =
                    if g then s.combineLatest(t).render((_, tv) => span(s"$tv:on")) else t.render(tv => span(s"$tv:none"))
                def written: SignalRef[?]   = s
                def write(n: Int): Unit     = s.unsafe.set(s"s$n")
                def prime(n: Int): Unit     = t.unsafe.set(n)
        case "flip-if" =>
            // The condition toggles on every write, so each write flips the branch: a lazy branch subscribes and tears down.
            new UIExample:
                val c                       = ref(true)
                val a                       = ref("a")
                val b                       = ref("b")
                def block(using Frame): UI  = UI.reactive(span(if c.read then a.read else b.read))
                def hand(using Frame): UI   = c.render(on => if on then a.render(span(_)) else b.render(span(_)))
                def written: SignalRef[?]   = c
                def write(n: Int): Unit     = c.unsafe.set(n % 2 == 0)
                def prime(n: Int): Unit     = c.unsafe.set(!c.unsafe.get())
        case "match-body" =>
            // The selector stays on one case and the case's read changes on every write: no switch, only the taken case's emissions.
            new UIExample:
                val k                       = ref(0)
                val a                       = ref("a")
                def block(using Frame): UI  = UI.reactive(span(k.read match
                    case 0 => a.read
                    case _ => "other"))
                def hand(using Frame): UI   = k.render(v => if v == 0 then a.render(span(_)) else span("other"))
                def written: SignalRef[?]   = a
                def write(n: Int): Unit     = a.unsafe.set(s"a$n")
                def prime(n: Int): Unit     = a.unsafe.set(s"p$n")
        case "flip-match" =>
            // The selector changes on every write, so each write switches the case: a lazy case subscribes and tears down.
            new UIExample:
                val k                       = ref(0)
                val a                       = ref("a")
                val b                       = ref("b")
                def block(using Frame): UI  = UI.reactive(span(k.read match
                    case 0 => a.read
                    case _ => b.read))
                def hand(using Frame): UI   = k.render(v => if v == 0 then a.render(span(_)) else b.render(span(_)))
                def written: SignalRef[?]   = k
                def write(n: Int): Unit     = k.unsafe.set(n % 2)
                def prime(n: Int): Unit     = k.unsafe.set(1 - k.unsafe.get())
        case "flip-when" =>
            // The `when` condition toggles on every write; the branch sets a plain attribute, which is not a toggle setter.
            new UIExample:
                val hot                     = ref(false)
                def block(using Frame): UI  = UI.reactive(span.when(hot.read)(_.title("on"))("x"))
                def hand(using Frame): UI   = hot.render(on => span.when(on)(_.title("on"))("x"))
                def written: SignalRef[?]   = hot
                def write(n: Int): Unit     = hot.unsafe.set(n % 2 == 0)
                def prime(n: Int): Unit     = hot.unsafe.set(!hot.unsafe.get())
        case other => throw new IllegalArgumentException(s"no UI example $other")
end ReactiveBench

/** Building each example: the block's expansion against the hand-written form, per draw. */
@State(JmhScope.Benchmark)
@Fork(1)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class ReactiveBuildBench:
    import ReactiveBench.*

    @Param(Array(
        "constant",
        "one-signal",
        "two-signals",
        "diamond",
        "fan-in-six",
        "optional",
        "attribute",
        "class-flag",
        "text",
        "output-unchanged",
        "untaken-branch",
        "false-guard"
    ))
    var example: String = uninitialized

    @Param(Array("block", "hand"))
    var form: String = uninitialized

    private var builder: () => Any = uninitialized

    @Setup(Level.Trial)
    def setup(): Unit =
        builder = example match
            case "constant" => if form == "block" then () => constantBlock else () => constantHand
            case name if Set("one-signal", "two-signals", "diamond", "fan-in-six", "optional").contains(name) =>
                val ex = propExample(name)
                if form == "block" then () => ex.block else () => ex.hand
            case name =>
                val ex = uiExample(name)
                if form == "block" then () => ex.block else () => ex.hand
    end setup

    @Benchmark
    def build(): Any = builder()
end ReactiveBuildBench

/** One write to an input, until its effect is done: the block's expansion against the hand-written form. */
@State(JmhScope.Benchmark)
@Fork(1)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class ReactiveChangeBench:
    import ReactiveBench.*
    import AllowUnsafe.embrace.danger

    @Param(Array(
        "one-signal",
        "two-signals",
        "three-signals",
        "four-signals",
        "diamond",
        "fan-in-six",
        "optional",
        "attribute",
        "class-flag",
        "text",
        "output-unchanged",
        "untaken-branch",
        "false-guard",
        "flip-if",
        "flip-match",
        "match-body",
        "flip-when"
    ))
    var example: String = uninitialized

    @Param(Array("block", "hand"))
    var form: String = uninitialized

    private var n                                = 0
    private var changer: () => Any                = uninitialized
    private var session: Maybe[Fiber[Unit, Any]] = Absent

    /** Spins until `cond` holds, so a write is measured to the end of its effect; fails a setup that never gets there. */
    private def await(what: String)(cond: => Boolean): Unit =
        val deadline = java.lang.System.nanoTime() + 10_000_000_000L
        while !cond do
            if java.lang.System.nanoTime() > deadline then throw new IllegalStateException(s"$example/$form: never $what")
            Thread.onSpinWait()
    end await

    @Setup(Level.Trial)
    def setup(): Unit =
        example match
            case name if Set("one-signal", "two-signals", "three-signals", "four-signals", "diamond", "fan-in-six", "optional").contains(name) =>
                val ex   = propExample(name)
                val prop = if form == "block" then ex.block else ex.hand
                changer = () =>
                    n += 1
                    ex.write(n)
                    Sync.Unsafe.evalOrThrow(UI.Prop.current(prop))
            case name =>
                val ex       = uiExample(name)
                val ui       = if form == "block" then ex.block else ex.hand
                val exchange = new Counting
                session = Present(Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(Scope.run(
                    ReactiveUI.normalize(ui, Seq.empty).map(root => ReactiveUI.subscribe(root, exchange).andThen(Async.never[Unit]))
                ))))
                // Live once a write that is sure to be seen reaches the transport. The session subscribes on its own fiber, and a write
                // that lands before it does is only its first value, so each try writes a new one.
                val before = exchange.work.get()
                var tries  = 0
                await("drew the first change") {
                    tries += 1
                    if tries % 1000 == 1 then ex.prime(tries)
                    exchange.work.get() > before
                }
                val written = ex.written
                changer = () =>
                    n += 1
                    val waiting = written.unsafe.waiters()
                    ex.write(n)
                    await("settled")(written.unsafe.waiters() >= waiting)
        end match
    end setup

    @TearDown(Level.Trial)
    def stop(): Unit =
        session.foreach(f => Sync.Unsafe.evalOrThrow(f.interrupt.unit))
        session = Absent

    @Benchmark
    def change(): Any = changer()
end ReactiveChangeBench
