package kyo.internal

import kyo.*
import kyo.UI

/** Render instrumentation: the engine's account of every update it applied, silent unless an app installs a
  * sink with [[kyo.UI.devtools]].
  *
  * The unit of observation is the reactive region, because in kyo-ui that is the only thing that can re-run.
  * There is no component re-execution to count: a static subtree has no region, is walked once, and never
  * renders again. A counter per region is therefore exactly a counter per place where a re-render is
  * possible, with no filtering and no opt-in per component.
  */
private[kyo] object Devtools:

    /** The app's render-event sink, installed by [[kyo.UI.devtools]]. Inheritable, so it reaches the region
      * fibers, the mount effects and the channel callbacks below the installation point, all of which descend
      * from the runner's caller, the same reach [[ReactiveUI.noticeSink]] has. `Absent` means off: a region no
      * [[kyo.UI.devtools]] call encloses reports nowhere. The server-push runner, whose sessions do not descend
      * from its caller, carries the caller's sink to them itself (see [[UIServer.sessionServing]]).
      *
      * A plain function rather than an effect. The sink is called from a region fiber between its walk and
      * its paint, and from the synchronous channel callbacks that run inside a writer's own `set`; neither
      * can afford to suspend. Same reasoning, and the same shape, as [[UIExchange]]'s `*PatcherNow` sinks.
      */
    val sink: Local[Maybe[UI.RenderEvent => Unit]] = Local.init(Maybe.empty[UI.RenderEvent => Unit])

    /** What a devtools installation adds to a server-push app beyond the sink: the overlay script to inline into the
      * page, and the statistics payload each session ships to it.
      *
      * Handed in rather than imported: kyo-ui must not depend on the devtools artifact, or the overlay would be linked
      * into every app that uses the framework and the separation would have bought nothing.
      */
    final case class ServerHooks(pageScript: String, stats: () => String)

    /** The server hooks of the devtools installation in effect, `Absent` for every app without one. Like [[sink]], it is
      * captured where the handlers are built (see [[UIServer.sessionServing]]), so two apps in one process never see each
      * other's overlay.
      */
    val server: Local[Maybe[ServerHooks]] = Local.init(Maybe.empty[ServerHooks])

    /** How often a server-push session ships statistics. Matches the browser mount's push interval: below it
      * the badges visibly lag the page, above it nothing is gained and the frames are pure overhead.
      */
    private[kyo] val statsInterval: Duration = 100.millis

    /** Per-paint scratch the backend fills in while a region is painting.
      *
      * A repaint's story is known in two places: the region fiber knows whose render it is, what caused it and
      * how long it took; only the exchange knows how much of it actually reached the DOM. Rather than widen
      * [[UIExchange]] (every backend and a dozen test doubles implement it), the fiber puts a probe in scope
      * for the duration of its `onChange` call and reads it back afterwards.
      *
      * One probe per paint, created and read by the single fiber that owns the region, written only from
      * inside that fiber's own `onChange`. `Absent` (and never allocated) when no sink is installed.
      */
    final class Probe:
        var bytes: Int      = 0
        var wasted: Boolean = false
    end Probe

    val probe: Local[Maybe[Probe]] = Local.init(Maybe.empty[Probe])

    /** A probe to hand the next paint, `Absent` when devtools is off so the off path allocates nothing. */
    def newProbe(using Frame): Maybe[Probe] < Sync =
        sink.use { local =>
            if local.isEmpty then Absent
            else Sync.defer(Present(new Probe))
        }

    /** The backend's report of what a paint cost: bytes that reached the DOM, and whether the render turned
      * out to change nothing at all. A no-op when no paint is being observed.
      */
    def notePaint(bytes: Int, wasted: Boolean)(using Frame): Unit < Sync =
        probe.use {
            case Present(p) =>
                Sync.defer {
                    p.bytes = bytes
                    p.wasted = wasted
                }
            case Absent => Kyo.unit
        }

    /** Whether a paint is being observed at all, so a backend can skip work whose only consumer is
      * [[notePaint]] (hashing a rendered region to decide `wasted`, say).
      */
    def observing(using Frame): Boolean < Sync = probe.use(_.nonEmpty)

    def emit(event: => UI.RenderEvent)(using Frame): Unit < Sync =
        sink.use { local =>
            local match
                case Present(f) => Sync.defer(f(event))
                case Absent     => Kyo.unit
        }

    /** The installed sink, read once so a synchronous callback can carry it.
      *
      * A channel write fires inside the writer's own `set`, outside any effect context, and cannot read a
      * `Local` at that moment. Reading it at bind time is also what keeps the off path free: with no sink
      * installed the callback is passed through unwrapped.
      */
    def currentSink(using Frame): Maybe[UI.RenderEvent => Unit] < Sync = sink.get

end Devtools
