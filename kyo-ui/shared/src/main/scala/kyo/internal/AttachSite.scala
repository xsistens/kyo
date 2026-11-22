package kyo.internal

import kyo.*
import kyo.kernel.ContextEffect

/** Where an [[UIEvent.Attach]] finds what it starts a hook with: the element the browser mount found in the document and the scope
  * that lives as long as the element stays there.
  *
  * Neither can ride the event, which is a wire type. The browser mount dispatches the event with both set here, and dispatch reaches
  * the element through the same chain a click takes, so the hook meets the element's `onFailure` rules, the mounts and boundaries
  * around it, the session's `UI.commands` and `UI.batch`. An `Attach` that arrives without them, from a server-push client say, starts
  * nothing.
  */
private[kyo] object AttachSite:

    final case class Site(element: Any, scope: Scope.Finalizer)

    val local: Local[Maybe[Site]] = Local.init(Maybe.empty[Site])

    /** Starts `hook` on a fiber the site's scope owns, so a hook that waits never holds up the dispatch, and closing the scope interrupts
      * it and releases what it acquired. A failure it leaves goes to the rules the dispatch put in effect.
      */
    def start(hook: Any => Any < (Async & Scope & Abort[Any]))(using Frame): Unit < Async =
        local.use {
            case Absent        => Kyo.unit
            case Present(site) =>
                val run: Unit < Async =
                    Failures.recover(())(ContextEffect.handleInheritable(Tag[Scope], site.scope)(hook(site.element)).unit)
                ContextEffect.handleInheritable(Tag[Scope], site.scope)(Fiber.init(run)).unit
        }
end AttachSite
