package kyo.uic.form

/** How a validation is ACTIVATED — when its verdict is (re)computed. One concept across
  * every binding, but its inhabited values live where they are type-valid:
  *
  *   - a FIELD re-checks on DOM events — the combinable [[Activation.Field]] values
  *     ([[Activation.Change]] / [[Activation.Blur]] / [[Activation.Submit]]) declared on
  *     `Form.field`;
  *   - a FORM / ROW constraint is activated by the PREDICATE TYPE you hand `Form.satisfy`,
  *     because activation and predicate type are correlated: a `=> Boolean < Async` thunk is
  *     SUBMIT-pull (evaluated once per submit) and a `Signal[Boolean]` is REACTIVE-push
  *     (re-evaluated whenever its inputs change);
  *   - an EXTERNAL verdict is pushed imperatively via `FormField.setError` / `Form.raise`
  *     — there is no predicate, so no activation value to declare.
  *
  * Only the field branch is an inhabited value type; the other two are activation POINTS
  * reached through the API named above. They are kept OUT of the enum so illegal
  * combinations are unrepresentable — a field cannot be "activated" externally, and a
  * submit-pull `satisfy` thunk cannot be "activated" reactively over a signal it does not have.
  */
sealed trait Activation derives CanEqual

object Activation:

    /** A field's re-check schedule: the DOM events it validates on. Combinable — a field
      * validates on the union of the values it is declared with.
      *
      *   - [[Change]] — every value change (an observer on the bound value; the error surfaces
      *     inline as the user types, RHF `onChange` mode). Works the same on field-array rows:
      *     a row has no ambient `Scope`, so its observer is unscoped and the array owns it,
      *     cancelling it when the row is removed or the form unmounts.
      *   - [[Blur]]   — focus loss (wired to the field's `onBlur`), even without an edit.
      *   - [[Submit]] — form submit (driven by `Form.submit`).
      */
    sealed trait Field extends Activation

    case object Change extends Field
    case object Blur   extends Field
    case object Submit extends Field
end Activation
