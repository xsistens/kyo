package kyo.uic.form

/** How a validation is ACTIVATED: when its verdict is (re)computed. One concept across
  * every binding, but its inhabited values live where they are type-valid:
  *
  *   - a field RULE runs on DOM events, the combinable [[Activation.Field]] values
  *     ([[Activation.Change]] / [[Activation.Blur]] / [[Activation.Submit]]), declared on the
  *     rule with [[Rule.on]] or, as the fallback for the rules that declare none, on the field
  *     with `FieldSpec.on`;
  *   - a FORM / ROW constraint is activated by the PREDICATE TYPE you hand `Form.satisfy`,
  *     because activation and predicate type are correlated: a `=> Boolean < Async` thunk is
  *     SUBMIT-pull (evaluated once per submit) and a `Signal[Boolean]` is REACTIVE-push
  *     (re-evaluated whenever its inputs change);
  *   - an EXTERNAL verdict is pushed imperatively via `FormField.setError` / `Form.raise`;
  *     there is no predicate, so no activation value to declare.
  *
  * Only the field branch is an inhabited value type; the other two are activation POINTS
  * reached through the API named above. They are kept OUT of the enum so illegal
  * combinations are unrepresentable: a field cannot be "activated" externally, and a
  * submit-pull `satisfy` thunk cannot be "activated" reactively over a signal it does not have.
  */
sealed trait Activation derives CanEqual

object Activation:

    /** The DOM events a field rule runs on. Combinable: a rule runs on the union of the values
      * it is declared with.
      *
      * Which set applies to a rule is decided nearest first: `rule.on(...)` ([[Rule.on]]; inside
      * nested ones the innermost wins), else the field's `on` (`FieldSpec.on`), else the rule's
      * own default: [[Blur]] + [[Change]] for every rule except [[Validator.async]], whose default
      * is [[Blur]] alone. A rule not due for an event keeps its last verdict while the value is
      * the one it judged, and is otherwise "not yet checked": no error shows, the rules after it
      * wait, and the form's ungated `isValid()` is false until it runs.
      *
      * The field's own set also says which events TOUCH the field (default: [[Blur]] + [[Change]],
      * quiet until the reader leaves the field, live from then on). What a reader SEES is gated
      * separately by [[Reveal]], whose own default (`WhenTouched`) is what keeps the first
      * keystroke silent. Together they are "reward early, punish late".
      *
      *   - [[Change]]: every value change (an observer on the bound value; the error surfaces
      *     inline as the user types, RHF `onChange` mode). Works the same on field-array rows:
      *     a row has no ambient `Scope`, so its observer is unscoped and the array owns it,
      *     cancelling it when the row is removed or the form unmounts.
      *   - [[Blur]]: focus loss (wired to the field's `onBlur`), even without an edit.
      *   - [[Submit]]: submit ONLY, and it is how you opt OUT of the events rather than a
      *     trigger that adds anything: every rule runs at submit whatever it declares
      *     (`FormField.validateForSubmit` ignores every set), and so do a DataTable cell's
      *     commit and a change of a signal the field `dependsOn` (a change of a signal a rule
      *     READS is the rule's own matter, see `Dep`). Declaring it alone therefore
      *     means "never re-check while the reader is in the field", which is a real choice for an
      *     expensive async rule, and reads as one, where an empty declaration would read as an
      *     omission.
      */
    sealed trait Field extends Activation

    case object Change extends Field
    case object Blur   extends Field
    case object Submit extends Field
end Activation
