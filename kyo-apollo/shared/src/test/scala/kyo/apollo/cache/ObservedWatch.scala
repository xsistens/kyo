package kyo.apollo.cache

import kyo.*
import kyo.apollo.ApolloCall
import kyo.apollo.StreamProbe
import kyo.apollo.cache.normalized.WatchObserver
import kyo.apollo.cache.normalized.observedWatch
import kyo.apollo.network.ApolloResponse

/** A watch opened for a spec: the [[StreamProbe.Pull]] over its emissions plus
  * barriers on the watch's own fibers, so a spec writes only once the watch has
  * finished reacting instead of racing it.
  *
  * A fetched response is offered before its key set is established, and the
  * establishing step may itself re-read and emit on the fetching fiber. A spec
  * that writes right after pulling that response can therefore interleave with
  * the watch's own follow-up and see an extra emission. [[awaitEstablished]] is
  * the point after which the fetching fiber has nothing left to do for that
  * response; [[awaitRefetchEnded]] is the same for a network refetch flight, and
  * [[close]] returns once the watch's teardown has run.
  */
final class ObservedWatch[D] private (
    val pull: StreamProbe.Pull[ApolloResponse[D]],
    established: Channel[ApolloResponse[?]],
    refetchesEnded: Channel[Boolean],
    tornDown: Latch
):

    /** Wait until the next fetched response (initial fetch or refetch, in order) is
      * established: offered, its key set adopted, its window closed, and every
      * reaction that ran on the fetching fiber meanwhile done.
      */
    def awaitEstablished(using Frame): ApolloResponse[?] < Async =
        Abort.run[Closed](established.take).map(_.getOrThrow)

    /** Wait until the next network refetch flight has ended, follow-up included, and
      * yield whether it booked a rerun.
      */
    def awaitRefetchEnded(using Frame): Boolean < Async =
        Abort.run[Closed](refetchesEnded.take).map(_.getOrThrow)

    /** End the watch's `Scope` and return once its teardown has run: unsubscribed,
      * inactive, its fibers interrupted. [[pull]] is closed from here on.
      */
    def close(using Frame): Unit < Async =
        pull.cancel.andThen(tornDown.await)
end ObservedWatch

object ObservedWatch:

    /** Open `call` as a watch. `onOffered` runs on the fetching fiber right after a
      * fetched response is offered and before its key set is established — the
      * position of a consumer that writes as soon as it sees the emission.
      */
    def open[D](call: ApolloCall[D], onOffered: ApolloResponse[?] => Unit < Sync = _ => Kyo.unit)(using
        Frame,
        Tag[Emit[Chunk[ApolloResponse[D]]]]
    ): ObservedWatch[D] < (Async & Scope) =
        for
            established    <- Channel.init[ApolloResponse[?]](Int.MaxValue)
            refetchesEnded <- Channel.init[Boolean](Int.MaxValue)
            tornDown       <- Latch.init(1)
            observer = WatchObserver(
                offered = onOffered,
                established = response => Abort.run[Closed](established.offer(response)).unit,
                refetchEnded = rerun => Abort.run[Closed](refetchesEnded.offer(rerun)).unit
            )
            // Registered in the stream's Scope before the watch registers its own
            // finalizers, so it is released after they have run.
            stream = Stream.unwrap(Scope.ensure(tornDown.release).andThen(call.observedWatch(observer)))
            pull <- StreamProbe.Pull.open(stream)
        yield new ObservedWatch(pull, established, refetchesEnded, tornDown)
end ObservedWatch
