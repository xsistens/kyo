package kyo.uic

import kyo.*

/** A step of the fold into [[ReactiveValue]], gone by the end of it: the old names over the merged type, so every call site compiles
  * unchanged against what it will become.
  */
private[uic] type Reading[A] = ReactiveValue[A]

private[uic] object Reading:
    export ReactiveValue.Const as Fixed
    export ReactiveValue.Dyn as Live
    export ReactiveValue.concat
    export ReactiveValue.keyed
    export ReactiveValue.optional
    export ReactiveValue.orElse

    def of[A](v: ReactiveValue[A]): Reading[A] = v
end Reading

extension [A](r: Reading[A])
    private[uic] def slot: A | Signal[A] = r.bound
end extension
