package kyo.internal

/** Whether activating a button submits its form.
  *
  * Three places decide this and they have to agree, because they answer for the same
  * markup: the SPA event dispatcher ([[kyo.internal.ReactiveUI]]), the SPA DOM client
  * ([[kyo.internal.DomBackend]]) and the server-push client, which ships its copy as
  * JavaScript text ([[kyo.internal.HtmlRenderer]]). They did not agree — the dispatcher
  * never read the type at all, and the two clients compared it against `"submit"`, which
  * gets an invalid value backwards. Same arrangement [[KeyPolicy]] uses for the keyboard
  * rules: the rule lives here, the Scala callers call it, the JavaScript is built from
  * it, and a test holds the emitted script against these values.
  *
  * ==The rule, and where it comes from==
  *
  * A button's `type` is an HTML enumerated attribute with three states, and BOTH its
  * missing-value default and its invalid-value default are Submit Button. Its keywords
  * are matched ASCII case-insensitively and are not whitespace-trimmed. So exactly two
  * spellings do not submit — `button` and `reset`, in any case — and everything else
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
  * <button type="submit">  "submit"      YES
  * <button>                "submit"      YES     // missing value default
  * <button type="SUBMIT">  "submit"      YES
  * <button type="bogus">   "submit"      YES     // invalid value default
  * <button type="">        "submit"      YES
  * <button type=" button "> "submit"     YES     // no trimming: unrecognised
  * }}}
  *
  * The padded row is the one that rules out a `trim()`, and the misspelt row the one
  * that rules out comparing against `"submit"`.
  */
private[kyo] object ButtonActivation:

    /** The two keywords that name a button which does NOT submit. Everything else is the
      * Submit Button state, by one default or the other.
      */
    val nonSubmitTypes: Seq[String] = Seq("button", "reset")

    /** Does activating a button whose `type` reads `rawType` submit its form? An absent
      * attribute is `""` — unrecognised, therefore Submit.
      */
    def submits(rawType: String): Boolean =
        val declared = if rawType == null then "" else rawType
        !nonSubmitTypes.exists(_.equalsIgnoreCase(declared))
    end submits

    /** The same test as a JavaScript expression over `value`, which the caller must have
      * lower-cased already (JS `toLowerCase` is Unicode-aware; the Scala side compares
      * case-insensitively instead, which needs no locale).
      */
    def jsSubmits(value: String): String =
        nonSubmitTypes.map(t => s"""$value!=="$t"""").mkString("&&")

end ButtonActivation
