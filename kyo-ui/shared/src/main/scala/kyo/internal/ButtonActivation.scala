package kyo.internal

/** Whether activating a button submits its form.
  *
  * The event dispatcher ([[kyo.internal.ReactiveUI]]) is the only place that decides it. Neither client reads a
  * button's `type`: both leave a button's native activation to the dispatcher (see [[KeyPolicy.doubleActivates]]), so
  * there is no second copy of this rule to keep in step.
  *
  * ==The rule, and where it comes from==
  *
  * A button's `type` is an HTML enumerated attribute with three states, and both its
  * missing-value default and its invalid-value default are Submit Button. Its keywords
  * are matched ASCII case-insensitively and are not whitespace-trimmed. So exactly two
  * spellings do not submit (`button` and `reset`, in any case), and everything else
  * does, including a missing attribute, an empty one, a misspelt one and a padded one.
  *
  * Measured in Chrome on a plain HTML page rather than read off the specification, one
  * `element.click()` per row with a `submit` listener on the form:
  *
  * {{{
  * markup                 element.type   submits
  * <button type="button">  "button"      no
  * <button type="BUTTON">  "button"      no      // keyword match is case-insensitive
  * <button type="reset">   "reset"       no      // fires reset instead
  * <button type="ReSeT">   "reset"       no
  * <button type="submit">  "submit"      yes
  * <button>                "submit"      yes     // missing value default
  * <button type="SUBMIT">  "submit"      yes
  * <button type="bogus">   "submit"      yes     // invalid value default
  * <button type="">        "submit"      yes
  * <button type=" button "> "submit"     yes     // no trimming: unrecognised
  * }}}
  *
  * The padded row is the one that rules out a `trim()`, and the misspelt row the one
  * that rules out comparing against `"submit"`.
  */
private[kyo] object ButtonActivation:

    /** The two keywords that name a button which does not submit. Everything else is the
      * Submit Button state, by one default or the other.
      */
    val nonSubmitTypes: Seq[String] = Seq("button", "reset")

    /** Does activating a button whose `type` reads `rawType` submit its form? An absent
      * attribute is `""`: unrecognised, therefore Submit.
      */
    def submits(rawType: String): Boolean =
        val declared = if rawType == null then "" else rawType
        !nonSubmitTypes.exists(_.equalsIgnoreCase(declared))
    end submits

end ButtonActivation
