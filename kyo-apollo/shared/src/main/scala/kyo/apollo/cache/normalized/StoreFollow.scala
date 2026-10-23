package kyo.apollo.cache.normalized

import kyo.<
import kyo.Async
import kyo.Channel
import kyo.Chunk
import kyo.Emit
import kyo.Frame
import kyo.Kyo
import kyo.Maybe
import kyo.Stream
import kyo.Sync
import kyo.Tag

/** A store read that follows its records, built by [[ApolloStore.follow]]: the value
  * of the first read, and a read again after every write that changes a record the
  * last read depended on. `changed` is `Absent` for a follow with nothing to watch,
  * which has its first value and no updates. The listener, the retained keys and the
  * channel end with the `Scope` the follow was built in, and so do the updates.
  */
final private[apollo] class StoreFollow[V] private[normalized] (
    val initial: Maybe[V],
    changed: Maybe[Channel[Unit]],
    next: Maybe[V] < Sync
):

    /** Run `publish` on the value of every later read that answers one; a read that
      * misses publishes nothing. Returns once the follow's `Scope` has closed.
      */
    def foreachUpdate(publish: V => Unit < Sync)(using Frame): Unit < Async =
        changed.fold(Kyo.unit)(_.streamUntilClosed().foreach(_ => next.map(_.fold(Kyo.unit)(publish))))

    /** The values of the later reads that answer one, ending when the follow's `Scope`
      * closes.
      */
    def updates(using Frame, Tag[Emit[Chunk[V]]]): Stream[V, Async] =
        changed.fold(Stream.empty[V])(_.streamUntilClosed().collect(_ => next))

end StoreFollow
