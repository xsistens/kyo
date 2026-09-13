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
    def collect[A](stream: Stream[A, Async & Scope])(using
        Frame,
        Tag[Emit[Chunk[A]]]
    ): List[A] < (Async & Scope) =
        stream.run.map(_.toList)

    /** A stream's first emission (the single-response convenience). A
      * never-completing stream must be `.take(n)`-bounded first.
      */
    def first[A](stream: Stream[A, Async & Scope])(using
        Frame,
        Tag[Emit[Chunk[A]]]
    ): A < (Async & Scope) =
        stream.take(1).run.map(_.head)

    /** Fire-and-forget drain: fork a `Scope`-managed fiber that runs `onNext` for
      * each emission as it is produced. The effect-native form of the deleted
      * `KyoTestRun.subscribe` — for WS specs that accumulate emissions into a `var`
      * while they script socket frames and `Async.sleep` between them. The fiber is
      * `Scope`-managed (via [[Fiber.init]]), so the runner interrupts it on leaf
      * exit; the returned handle lets a spec join/interrupt it explicitly if needed.
      */
    def drain[A](stream: Stream[A, Async & Scope])(onNext: A => Unit)(using
        Frame,
        Tag[Emit[Chunk[A]]]
    ) =
        Fiber.init(Scope.run(stream.foreach(a => Sync.defer(onNext(a)))))

    /** A channel-backed pull handle over a live stream (a watch, a subscription,
      * …): [[Pull.next]] genuinely suspends (on Kyo's own scheduler) until the
      * stream's next emission has actually landed. Unlike a fire-and-forget drain,
      * this lets a spec interleave synchronous side effects between *exact*
      * emissions with no timing guesswork — the effect-native form of the
      * assert-as-you-go pattern.
      *
      * The draining fiber and its backing channel are registered with the leaf's
      * ambient `Scope` (via [[Pull.open]]), so the runner tears them down when the
      * test leaf exits even if a spec never calls [[Pull.cancel]] explicitly —
      * `cancel` remains available for tests that must stop a watcher MID-leaf and
      * then assert no further emission arrives.
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
          * asserting a write did *not* cause a re-emission. A `CacheOnly` re-read's
          * effect on the channel (if any) is synchronous with the triggering write,
          * so an immediate poll faithfully observes whether one landed.
          */
        def tryNext(using Frame): Maybe[A] < Async =
            Abort.run[Closed](channel.poll).map {
                case Result.Success(Present(a)) => Present(a)
                case Result.Success(Absent)     => Absent
                case Result.Failure(_)          => Absent
                case Result.Panic(cause)        => throw cause
            }

        /** Interrupt the producing fiber and close the channel — the explicit,
          * mid-leaf teardown a spec uses to prove emissions stop after cancel. Leaf
          * exit tears both down anyway (both are `Scope`-managed), so an un-cancelled
          * pull never leaks.
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
