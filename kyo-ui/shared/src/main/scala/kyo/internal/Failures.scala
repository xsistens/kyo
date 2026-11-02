package kyo.internal

import kyo.*

/** One place around a failure that has a say in what becomes of it, innermost first in a [[Failures.Scope]]. */
private[kyo] enum FailureFrame:
    /** An element's `onFailure` rules, which apply to the handlers of the element and of everything inside it. */
    case Rules(rules: PartialFunction[Any, UI.Reaction])

    /** A boundary: its error view, its `onFailure` rules and its `onNotice` sink. */
    case Boundary(settings: UI.Boundary)

    /** A mount, which an `Escalate` fails. */
    case Mount(supervision: MountSupervision)
end FailureFrame

/** What becomes of a failure nothing handled: an event handler's, a [[kyo.UI.fork]] fiber's, a [[kyo.UI.report]].
  *
  * The frames around the failure are a list, innermost first. Event dispatch builds it on its way down to the target, and a mount
  * instance builds it from the tables its boundaries are on and hands it to its effect. Because the list travels with the tree and not
  * with the fiber a transport runs on, a failure meets the same rules in the browser engine and under server-push.
  */
private[kyo] object Failures:

    type Scope = List[FailureFrame]

    /** The frames around the running code: set by event dispatch and by a mount instance for its effect. */
    val local: Local[Scope] = Local.init(List.empty[FailureFrame])

    /** Runs `v` with `frame` innermost. */
    def within[A, S](frame: FailureFrame)(v: A < S)(using Frame): A < S = local.update(frame :: _)(v)

    /** The error views of the boundaries in `scope`, innermost first. */
    def views(scope: Scope): List[PartialFunction[UI.MountError, UI]] =
        scope.collect { case FailureFrame.Boundary(settings) if settings.errorView.nonEmpty => settings.errorView.get }

    /** The `Throwable` a failure stands for where one is expected. */
    def throwable(failure: Any)(using Frame): Throwable =
        failure match
            case t: Throwable => t
            case other        => UI.FailureException(other)

    /** Picks the reaction for `failure` from the innermost rule that matches it, `otherwise` when none does, and carries it out. */
    def handle(scope: Scope, failure: Any, otherwise: UI.Reaction, source: UI.MountErrorSource)(using Frame): Unit < Async =
        val reaction = scope.iterator.map {
            case FailureFrame.Rules(rules)                                         => rules.lift(failure)
            case FailureFrame.Boundary(settings) if settings.failureRules.nonEmpty => settings.failureRules.get.lift(failure)
            case _                                                                 => None
        }.collectFirst { case Some(r) => r }.getOrElse(otherwise)
        reaction match
            case UI.Reaction.Notify   => notify(scope, failure)
            case UI.Reaction.Escalate =>
                scope.collectFirst { case FailureFrame.Mount(supervision) => supervision } match
                    case Some(supervision) => supervision.fail(failure, source)
                    case None              => notify(scope, failure)
            case UI.Reaction.Run(action) =>
                Abort.run[Any](action).map {
                    case Result.Success(_)            => Kyo.unit
                    case Result.Panic(_: Interrupted) => Kyo.unit
                    case Result.Failure(again)        => notify(scope, again)
                    case Result.Panic(again)          => notify(scope, again)
                }
        end match
    end handle

    /** Hands `failure` to the innermost boundary sink in `scope`, and logs it when there is none. */
    def notify(scope: Scope, failure: Any)(using Frame): Unit < Async =
        scope.collectFirst { case FailureFrame.Boundary(settings) if settings.noticeSink.nonEmpty => settings.noticeSink.get } match
            case Some(sink) => sink(throwable(failure))
            case None       => Log.warn(s"kyo-ui: notice with no onNotice sink installed: $failure")

    /** Runs a handler's effect and hands a failure it leaves to the rules in effect, so the dispatch around it goes on. */
    def recover[A](fallback: A)(v: A < (Async & Abort[Any]))(using Frame): A < Async =
        Abort.run[Any](v).map {
            case Result.Success(a)            => a
            case Result.Panic(_: Interrupted) => fallback
            case Result.Failure(failure) => local.use(handle(_, failure, UI.Reaction.Notify, UI.MountErrorSource.Handler)).andThen(fallback)
            case Result.Panic(panic)     => local.use(handle(_, panic, UI.Reaction.Notify, UI.MountErrorSource.Handler)).andThen(fallback)
        }

end Failures
