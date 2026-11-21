package kyo.internal

import kyo.*
import scala.annotation.tailrec

/** A transport that paints nothing and remembers everything it was told, oldest first.
  *
  * The engine's tests read what reached the page from here. A test waits on [[awaitOp]], which a promise releases, or on an observer parked
  * on its signal; neither reads a clock.
  *
  * @param synchronous
  *   offers the `*Now` sinks, as the DOM backend does, so a channel patches on the writer's stack. Otherwise the channels go through their
  *   fibers, as on the server transport
  * @param keepsRows
  *   answers a list patch the way the DOM backend does: recorded as one [[RecordingExchange.Op.List]], the kept rows left in place. Otherwise
  *   it goes through `onChange` as a whole fragment, as on the server transport
  * @param beforePaint
  *   runs before a paint is recorded, and may suspend it there, as a slow transport would
  */
private[kyo] class RecordingExchange(
    synchronous: Boolean = false,
    keepsRows: Boolean = false,
    beforePaint: (Seq[String], UI) => Unit < Async = (_, _) => ()
) extends UIExchange:
    import RecordingExchange.*

    // Unsafe: a sink runs on the writer's stack, where no effect context exists, and the log has to take what it reports.
    private val log = AtomicRef.Unsafe.init(Chunk.empty[Op])(using AllowUnsafe.embrace.danger)
    // Unsafe: as for the log. A waiter is released from the same stack that records the op it waits for.
    private val waiting =
        AtomicRef.Unsafe.init(Chunk.empty[(Op => Boolean, Promise.Unsafe[Unit, Any])])(using AllowUnsafe.embrace.danger)

    private def record(op: Op)(using AllowUnsafe): Unit =
        discard(log.getAndUpdate(_.append(op)))
        releaseMet(op)

    /** A waiter that registers after this looked finds the op in the log by itself. */
    @tailrec private def releaseMet(op: Op)(using AllowUnsafe): Unit =
        val all         = waiting.get()
        val (met, rest) = all.partition(_._1(op))
        if met.nonEmpty then
            if waiting.compareAndSet(all, rest) then met.foreach(_._2.completeUnitDiscard())
            else releaseMet(op)
    end releaseMet

    private def recording(op: Op)(using Frame): Unit < Sync = Sync.Unsafe.defer(record(op))

    def onChange(
        region: ReactiveRegion,
        path: Seq[String],
        contentContext: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        previous: Maybe[UI],
        ui: UI
    )(using Frame): Unit < Async =
        beforePaint(path, ui).andThen(recording(Op.Paint(path, previous, ui)))

    override def onListPatch(
        region: ReactiveRegion,
        path: Seq[String],
        contentContext: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        previous: Maybe[UI],
        rows: Seq[ListRow]
    )(using Frame): Boolean < Async =
        if keepsRows then recording(Op.List(path, rows)).andThen(true)
        else super.onListPatch(region, path, contentContext, parentContext, previous, rows)

    override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
        recording(Op.Attr(path, name, value, Sink.Fiber))

    override def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async =
        recording(Op.BoolAttr(path, name, value, Sink.Fiber))

    override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
        recording(Op.Class(path, name, on, Sink.Fiber))

    override def onStylePatch(path: Seq[String], set: Seq[(String, String)], removed: Seq[String])(using Frame): Unit < Async =
        recording(Op.Style(path, set, removed, Sink.Fiber))

    // Unsafe: each of these four sinks is called on the writer's stack, inside its `set`, where no effect context exists.
    override val attrPatcherNow: Maybe[(Seq[String], String, String) => Unit] =
        if synchronous then Present((path, name, value) => record(Op.Attr(path, name, value, Sink.Now))(using AllowUnsafe.embrace.danger))
        else Absent
    override val boolAttrPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit] =
        if synchronous then
            Present((path, name, value) => record(Op.BoolAttr(path, name, value, Sink.Now))(using AllowUnsafe.embrace.danger))
        else Absent
    override val classPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit] =
        if synchronous then Present((path, name, on) => record(Op.Class(path, name, on, Sink.Now))(using AllowUnsafe.embrace.danger))
        else Absent
    override val stylePatcherNow: Maybe[(Seq[String], Seq[(String, String)], Seq[String]) => Unit] =
        if synchronous then
            Present((path, set, removed) => record(Op.Style(path, set, removed, Sink.Now))(using AllowUnsafe.embrace.danger))
        else Absent

    /** Everything recorded so far, oldest first. */
    def ops(using Frame): Chunk[Op] < Sync = Sync.Unsafe.defer(log.get())

    /** [[ops]] where a test reads counts in a pure position, right after the write that delivers on the writer's stack. */
    def recorded: Chunk[Op] =
        // Unsafe: a read of the log, for a test that compares it right away; nothing here suspends or writes.
        log.get()(using AllowUnsafe.embrace.danger)

    def paints(using Frame): Chunk[Op.Paint] < Sync         = ops.map(_.collect { case op: Op.Paint => op })
    def listPatches(using Frame): Chunk[Op.List] < Sync     = ops.map(_.collect { case op: Op.List => op })
    def attrPatches(using Frame): Chunk[Op.Attr] < Sync     = ops.map(_.collect { case op: Op.Attr => op })
    def boolPatches(using Frame): Chunk[Op.BoolAttr] < Sync = ops.map(_.collect { case op: Op.BoolAttr => op })
    def classPatches(using Frame): Chunk[Op.Class] < Sync   = ops.map(_.collect { case op: Op.Class => op })
    def stylePatches(using Frame): Chunk[Op.Style] < Sync   = ops.map(_.collect { case op: Op.Style => op })

    /** Waits until an op that `pred` accepts has been recorded, whether already or later. */
    def awaitOp(pred: Op => Boolean)(using Frame): Unit < Async =
        Sync.Unsafe.defer {
            val promise = Promise.Unsafe.init[Unit, Any]()
            discard(waiting.getAndUpdate(_.append((pred, promise))))
            if log.get().exists(pred) then promise.completeUnitDiscard()
            promise.safe
        }.map(_.get)
end RecordingExchange

private[kyo] object RecordingExchange:

    /** Which way a channel's patch came: on the writer's stack, or from the channel's fiber. */
    enum Sink derives CanEqual:
        case Now, Fiber

    enum Op derives CanEqual:
        case Paint(path: Seq[String], previous: Maybe[UI], ui: UI)
        case List(path: Seq[String], rows: Seq[ListRow])
        case Attr(path: Seq[String], name: String, value: String, sink: Sink)
        case BoolAttr(path: Seq[String], name: String, value: Boolean, sink: Sink)
        case Class(path: Seq[String], name: String, on: Boolean, sink: Sink)
        case Style(path: Seq[String], set: Seq[(String, String)], removed: Seq[String], sink: Sink)
    end Op
end RecordingExchange
