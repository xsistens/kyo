package kyo.uic

/** Const-or-reactive [[Severity]] carrier: the generic [[ReactiveValue]] specialized to
  * `Severity`. A `Dyn` severity is realized through kyo-ui's class-name channel: the
  * component binds `cssClass.matching(sig)(severity => class)`, so the active accent class
  * swaps IN PLACE (`classList.toggle`) on change, with no re-render.
  */
private[uic] type SeverityValue = ReactiveValue[Severity]

/** Companion forwarding the [[ReactiveValue]] cases, so `SeverityValue.Const(...)` /
  * `SeverityValue.Dyn(...)` construction and pattern matching read against the alias.
  */
private[uic] object SeverityValue:
    export ReactiveValue.Const
    export ReactiveValue.Dyn
