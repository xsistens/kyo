package kyo.uic

/** Const-or-reactive [[Severity]] carrier — the generic [[ReactiveValue]] specialized to
  * `Severity`. A `Dyn` severity is realized through kyo-ui's reactive CLASS channel: the
  * component binds one `cssClass(token, sig.map(_ == s))` per variant, so the active
  * accent class swaps IN PLACE (`classList.toggle`) on change — no re-render.
  */
private[uic] type SeverityValue = ReactiveValue[Severity]

/** Companion forwarding the [[ReactiveValue]] cases, so `SeverityValue.Const(...)` /
  * `SeverityValue.Dyn(...)` construction and pattern matching read against the alias.
  */
private[uic] object SeverityValue:
    export ReactiveValue.Const
    export ReactiveValue.Dyn
