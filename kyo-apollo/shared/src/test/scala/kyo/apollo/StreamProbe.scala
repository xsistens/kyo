package kyo.apollo

import kyo.*

/** Effect-native stream probes for the kyo-test suites.
  *
  * `core` is kyo-effect-native (an operation yields `ApolloResponse[D] < (Async
  * & Scope)`, an interceptor chain a `Stream[ApolloResponse[D], Async & Scope]`)
  * and, since the move to kyo-test, so are the specs: each test leaf body IS a
  * kyo effect the runner discharges, so there is no `Future` to bridge into. The
  * old `KyoTestRun` object — which shuttled an effect/stream into a `Future` for
  * munit to await — is therefore gone; what remains here is the one genuinely
  * reused, non-trivial piece: a channel-backed [[Pull]] handle for specs that
  * interleave synchronous side effects (store writes, scripted socket frames)
  * between *exact*, ordered stream emissions. Everything else a spec used to get
  * from the bridge is now a one-liner on the effect itself (`stream.run`,
  * `stream.take(1).run`), inlined at the call site.
  *
  * `collect` / `first` are kept only as readable names for those one-liners; they
  * return the effect (`A < (Async & Scope)`), never a `Future`.
  */
object StreamProbe:

    /** All of a finite stream's emissions, in order — for a query/mutation or a
      * live stream already bounded by `.take(n)`.
      */
    def collect[A, S](stream: Stream[A, S])(using
        Frame,
        Tag[Emit[Chunk[A]]]
    ): List[A] < S =
        stream.run.map(_.toList)

    /** A stream's first emission (the single-response convenience). A
      * never-completing stream must be `.take(n)`-bounded first.
      */
    def first[A](stream: Stream[A, Async & Scope])(using
        Frame,
        Tag[Emit[Chunk[A]]]
    ): A < (Async & Scope) =
        stream.take(1).run.map(_.head)

    /** A channel-backed pull handle over a live stream (a watch, a subscription,
      * …): [[Pull.next]] genuinely suspends (on Kyo's own scheduler) until the
      * stream's next emission has actually landed. This lets a spec interleave
      * synchronous side effects between *exact* emissions with no timing
      * guesswork — the effect-native form of the assert-as-you-go pattern.
      *
      * The draining fiber and its backing channel are registered with the leaf's
      * ambient `Scope` (via [[Pull.open]]), so the runner tears them down when the
      * test leaf exits even if a spec never calls [[Pull.cancel]] explicitly —
      * `cancel` remains available for tests that must stop one stream MID-leaf
      * while others keep running.
      *
      * An absence is proven after a positive barrier, never after a pause: the
      * spec first pulls a later emission that the unwanted one would have had to
      * precede, then asserts `tryNext == Absent`.
      */
    final class Pull[A] private[StreamProbe] (channel: Channel[A], interruptDrain: Unit < Sync)(using
        Frame
    ):

        /** Await the next emission. Fails (`Abort`/panic surfaced as a thrown
          * `NoSuchElementException`) if the stream has already ended with nothing
          * left to pull.
          */
        def next(using Frame): A < Async =
            Abort.run[Closed](channel.take).map {
                case Result.Success(a) => a
                case Result.Failure(closed) =>
                    throw new NoSuchElementException(s"pulled from an already-closed stream: $closed")
                case Result.Panic(cause) => throw cause
            }

        /** Non-suspending check for an already-buffered emission — the tool for
          * asserting a write did *not* cause a re-emission. Emissions reach this
          * handle through a draining fiber, so an unwanted one may not have landed
          * yet right after the write that would cause it: call this only after a
          * [[next]] on a later emission the unwanted one would have had to precede.
          * On a cancelled pull it is always `Absent`.
          */
        def tryNext(using Frame): Maybe[A] < Async =
            Abort.run[Closed](channel.poll).map {
                case Result.Success(Present(a)) => Present(a)
                case Result.Success(Absent)     => Absent
                case Result.Failure(_)          => Absent
                case Result.Panic(cause)        => throw cause
            }

        /** Interrupt the producing fiber and close the channel — the explicit,
          * mid-leaf teardown of one stream while others keep running. Leaf exit tears
          * both down anyway (both are `Scope`-managed), so an un-cancelled pull never
          * leaks. The interrupt does not wait for the stream's own finalizers.
          */
        def cancel(using Frame): Unit < Sync =
            for
                _ <- interruptDrain
                // `close` now hands back the drained backlog on a fiber; the probe wants the channel shut,
                // not its leftovers, and must stay in Sync.
                _ <- channel.closeDiscard
            yield ()
    end Pull

    object Pull:
        /** Open a [[Pull]] over `stream`. Both the backing channel ([[Channel.init]])
          * and the draining fiber ([[Fiber.init]]) are `Scope`-managed, so the runner
          * reclaims them when the test leaf's `Scope` exits.
          */
        def open[A](
            stream: Stream[A, Async & Scope]
        )(using Frame, Tag[Emit[Chunk[A]]]): Pull[A] < (Async & Scope) =
            for
                channel <- Channel.init[A](Int.MaxValue)
                fiber   <- Fiber.init(Scope.run(stream.foreach(a => channel.put(a))))
            yield new Pull[A](channel, fiber.interrupt.unit)
    end Pull
end StreamProbe
