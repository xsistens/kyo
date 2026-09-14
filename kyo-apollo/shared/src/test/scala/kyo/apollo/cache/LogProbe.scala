package kyo.apollo.cache

import kyo.*

/** A logger that records every line it is handed instead of printing it, so a test
  * can assert what the code under test logged. Bind it with [[run]]; read the lines
  * with [[lines]], which first drains the asynchronous log daemon (JVM/Native) so
  * every line logged before the call is visible.
  */
final class LogProbe private (recorded: AtomicRef.Unsafe[Chunk[LogProbe.Line]])
    extends Log.Unsafe.ConsoleLogger("LogProbe", Log.Level.trace):

    // Both the inline (JS) and the daemon (JVM) dispatch end here.
    override def emit(event: Log.Event)(using AllowUnsafe): Unit =
        discard(recorded.updateAndGet(_.append(LogProbe.Line(event.level, event.message, event.throwable))))

    /** Run `v` with this probe as the ambient logger. */
    def run[A, S](v: A < S)(using Frame): A < S = Log.let(Log(this))(v)

    /** Every line logged through this probe so far, in order. */
    def lines(using Frame): Chunk[LogProbe.Line] < Async =
        Log.flush.andThen(Sync.Unsafe.defer(recorded.get()))

    /** The lines logged at [[Log.Level.error]]. */
    def errors(using Frame): Chunk[LogProbe.Line] < Async =
        lines.map(_.filter(_.level == Log.Level.error))
end LogProbe

object LogProbe:
    final case class Line(level: Log.Level, message: String, error: Maybe[Throwable])

    def init(using Frame): LogProbe < Sync =
        Sync.Unsafe.defer(new LogProbe(AtomicRef.Unsafe.init(Chunk.empty[Line])))
end LogProbe
