package kyo.internal

import kyo.*
import kyo.kernel.ContextEffect

/** Where an [[UIEvent.Attach]] finds what it starts a hook with: the element the browser mount found in the document and the scope
  * that lives as long as the element stays there.
  *
  * Neither can ride the event, which is a wire type. The browser mount dispatches the event with both set here, and dispatch reaches
  * the element through the same chain a click takes, so the hook meets the mounts around it and the session's `UI.commands`. An
  * `Attach` that arrives without them, from a server-push client say, starts nothing.
  */
private[kyo] object AttachSite:

    final case class Site(element: Any, scope: Scope.Finalizer)

    val local: Local[Maybe[Site]] = Local.init(Maybe.empty[Site])

    /** Starts `hook` on a fiber the site's scope owns, so a hook that waits never holds up the dispatch, and closing the scope interrupts
      * it and releases what it acquired. A failure it leaves is logged, as a failing event handler's is.
      */
    def start(hook: Any => Any < (Async & Scope & Abort[Any]))(using Frame): Unit < Async =
        local.use {
            case Absent        => Kyo.unit
            case Present(site) =>
                val run: Unit < Async =
                    Abort.run[Any](ContextEffect.handleInheritable(Tag[Scope], site.scope)(hook(site.element)).unit).map {
                        case Result.Success(_)            => Kyo.unit
                        case Result.Panic(_: Interrupted) => Kyo.unit
                        case Result.Failure(failure)      => Log.error(s"onAttached hook failed: $failure")
                        case Result.Panic(t)              => Log.error("onAttached hook panicked", t)
                    }
                ContextEffect.handleInheritable(Tag[Scope], site.scope)(Fiber.init(run)).unit
        }
end AttachSite
