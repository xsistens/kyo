package kyo.uic.form

/** When a failing validation is DISPLAYED — orthogonal to its [[Activation]] (which says
  * when it is re-COMPUTED). The same knob applies to fields ([[FormField.revealWhen]]) and to
  * form-level reactive invariants and submit checks (both [[Form.satisfy]]); not every value is
  * meaningful for every binding, so each notes where it applies.
  */
enum Reveal derives CanEqual:

    /** FIELD DEFAULT: shown once THIS field is touched (its first blur/change) or after a
      * submit — the field stays quiet until the user has interacted with it. A field-only
      * notion (a scope-level [[Form.satisfy]] has nothing to "touch", so it
      * treats `WhenTouched` as [[OnSubmit]]).
      */
    case WhenTouched

    /** Hidden until the first submit, then live. The default for scope-level invariants and
      * checks; on a field it suppresses the touched-gate so the field stays quiet while
      * editing and only flags at submit.
      */
    case OnSubmit

    /** Shown as soon as it is invalid, before any interaction — a field flags an invalid
      * initial value immediately; an invariant flags live from the start. For a SUBMIT-pull
      * [[Form.satisfy]] check there is nothing to show before submit, so `Immediate` coincides with
      * [[OnSubmit]].
      */
    case Immediate

    /** Never auto-displayed in a summary or inline — still feeds `isValid` and blocks submit;
      * the consumer builds its own display (from the field's `error` / the invariant's own
      * `Signal` / a `check`'s submit outcome).
      */
    case Manual
end Reveal
