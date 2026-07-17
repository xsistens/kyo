package kyo

import kyo.internal.Reducible

/** The non-consuming siblings of `Abort.recover`/`Abort.recoverError`: run a SIDE EFFECT on the failure, then
  * re-raise it unchanged — the error stays in the effect row, so enclosing handlers (a page's recovery, the UI
  * root's handler-error policy, a mounted node's supervision) still see it. Use these for intermediate
  * observation (logging, metrics) without deciding the failure's fate.
  *
  * Defined as extensions on the `Abort` companion so call sites read identically to the consuming pair
  * (`Abort.tap(...)` next to `Abort.recover(...)`). They live in kyo-core for fork-surface economy (kyo-core is
  * already patched for `Fiber.Supervisor`); the idiomatic upstream home is the `Abort` companion in kyo-prelude.
  */
extension (self: Abort.type)

    /** Runs `onFail` when `v` fails with an `E`, then re-raises the same failure. Success values and panics
      * pass through untouched (see [[tapError]] to observe panics too).
      */
    def tap[E](
        using frame: Frame
    )[A, S, S2, ER](onFail: E => Any < S2)(v: => A < (Abort[E | ER] & S))(
        using ConcreteTag[E]
    ): A < (Abort[E | ER] & S & S2) =
        Abort.recover[E](e => onFail(e).andThen(Abort.fail(e)))(v)

    /** Runs `onError` when `v` fails with an `E` OR panics, then re-raises the same error. The observed value
      * is the full [[Result.Error]] (`Failure(e)` or `Panic(t)`), mirroring `Abort.recoverError`.
      */
    def tapError[E](
        using frame: Frame
    )[A, S, S2, ER](onError: Result.Error[E] => Any < S2)(v: => A < (Abort[E | ER] & S))(
        using ConcreteTag[E]
    ): A < (Abort[E | ER] & S & S2) =
        Abort.recoverError[E](err => onError(err).andThen(Abort.error(err)))(v)
end extension
