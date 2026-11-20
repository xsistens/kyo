package kyo.internal

import kyo.*

/** A command that waits for the paints it belongs to: `send` once they are on the page, `drop` when they never will be. `answered`
  * marks one whose caller waits for a reply, a measure.
  */
final private[kyo] case class HeldCommand(send: Unit < Async, drop: Unit < Sync, answered: Boolean = false)

/** The commands of one mount instance that wait for its own content to be on the page.
  *
  * A command a mount sends is about the content that mount shows, and that content reaches the page later than the effect that built
  * it runs: after the effect publishes and the region over the instance paints it. So a command waits here until the instance's own
  * content has been painted, and goes out behind that paint. Content that never shows takes its commands with it: [[drop]] answers
  * each the way a missing element answers.
  *
  * A measure while the effect still runs goes out at once. The content it would wait for is what the effect is building, and an
  * effect that measures before it yields waits for the answer, so holding it would hold the effect forever; what it measures can only
  * be something already on the page.
  */
final private[kyo] class AfterShown:
    import AfterShown.State
    import AllowUnsafe.embrace.danger

    private val state    = AtomicRef.Unsafe.init[State](State.Waiting(Chunk.empty))
    private val building = AtomicBoolean.Unsafe.init(true)

    /** The effect has returned: from now on a measure, too, is about content on its way. */
    def built(using Frame): Unit < Sync = Sync.Unsafe.defer(building.set(false))

    /** Sends `command` once the instance's content is shown, at once when it already is. */
    def send(command: HeldCommand)(using Frame): Unit < Async =
        Sync.Unsafe.defer(command.answered && building.get()).map { now =>
            if now then command.send else hold(command)
        }

    private def hold(command: HeldCommand)(using Frame): Unit < Async =
        Sync.Unsafe.defer {
            state.getAndUpdate {
                case State.Waiting(held) => State.Waiting(held.append(command))
                case other               => other
            }
        }.map {
            case State.Waiting(_) => Kyo.unit
            case State.Shown      => command.send
            case State.Dropped    => command.drop
        }

    /** The instance's own content was just painted: what waited goes out behind that paint, and so does what comes later. */
    def shown(using Frame): Unit < Async =
        Sync.Unsafe.defer {
            state.getAndUpdate {
                case State.Dropped => State.Dropped
                case _             => State.Shown
            }
        }.map {
            case State.Waiting(held) => Kyo.foreachDiscard(held)(_.send)
            case _                   => Kyo.unit
        }

    /** The instance is gone: nothing it sent is on its way to an element any more. */
    def drop(using Frame): Unit < Sync =
        Sync.Unsafe.defer(state.getAndSet(State.Dropped)).map {
            case State.Waiting(held) => Kyo.foreachDiscard(held)(_.drop)
            case _                   => Kyo.unit
        }
end AfterShown

private[kyo] object AfterShown:
    private enum State derives CanEqual:
        case Waiting(held: Chunk[HeldCommand])
        case Shown
        case Dropped
    end State
end AfterShown
