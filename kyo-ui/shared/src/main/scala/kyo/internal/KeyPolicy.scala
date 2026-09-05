package kyo.internal

/** The keyboard rules the client applies to a key press before it becomes a kyo event.
  *
  * There are two clients: the SPA backend, which is Scala ([[kyo.internal.DomBackend]]), and the
  * server-push renderer, which ships the same logic as JavaScript text ([[kyo.internal.HtmlRenderer]]).
  * They have to answer identically for the same key on the same target, and they used to do it as two
  * hand-kept copies of the same lists. The answer lives here now: the Scala client calls these
  * predicates, the JavaScript is built from these values, and `KeyPolicyTest` holds the emitted
  * JavaScript against them. Neither transport can drift without failing that test.
  *
  * Nothing here touches the DOM, so every rule is a pure function of the key and what the client can
  * read off the target in one property access.
  */
private[kyo] object KeyPolicy:

    /** Keys whose browser default scrolls the page along its main axis. */
    val verticalScrollKeys: Seq[String] = Seq("ArrowUp", "ArrowDown", "PageUp", "PageDown")

    /** Keys whose browser default scrolls sideways or jumps to an edge of the page. */
    val edgeScrollKeys: Seq[String] = Seq("ArrowLeft", "ArrowRight", "Home", "End")

    /** Tags that take typed text, so a navigation key belongs to the caret rather than to the page. */
    val editableTags: Seq[String] = Seq("INPUT", "TEXTAREA", "SELECT")

    /** Tags that consume a vertical key themselves. A single-line `INPUT` is deliberately absent: it
      * has nowhere to move a caret vertically, which is what lets a combobox field walk its list with
      * the arrows while the reader types into it.
      */
    val verticalConsumerTags: Seq[String] = Seq("TEXTAREA", "SELECT")

    /** Tags the browser activates on Space by itself, so suppressing Space would take the activation
      * with it. An anchor is deliberately absent: with a link focused, Space scrolls the page.
      */
    val spaceActivatedTags: Seq[String] = Seq("BUTTON", "SUMMARY")

    /** Keys that activate a button. Both, as the browser and the ARIA button pattern have it. */
    val buttonActivationKeys: Seq[String] = Seq("Enter", " ")

    /** Keys that follow a link. Enter alone: Space scrolls the page with a link focused, and the ARIA
      * link pattern says the same.
      */
    val linkActivationKeys: Seq[String] = Seq("Enter")

    def activatesButton(key: String): Boolean = buttonActivationKeys.contains(key)

    def activatesLink(key: String): Boolean = linkActivationKeys.contains(key)

    /** Whether to suppress the browser's page scroll for `key` on this target.
      *
      * Only consulted inside a region that asked for it with `UI.preventScrollKeys`, which is how a
      * component that navigates with the arrows keeps the page still under it. Space is in here for
      * the same reason the arrows are: a list that is one tab stop has no native control to consume
      * it, so a Space that picks the highlighted row also scrolled the page a screenful.
      */
    def preventsPageScroll(key: String, tag: String, contentEditable: Boolean): Boolean =
        val editable         = contentEditable || editableTags.contains(tag)
        val verticalConsumer = contentEditable || verticalConsumerTags.contains(tag)
        if verticalScrollKeys.contains(key) then !verticalConsumer
        else if edgeScrollKeys.contains(key) then !editable
        else if key == " " then !editable && !spaceActivatedTags.contains(tag)
        else false
        end if
    end preventsPageScroll

    /** Whether the browser's own activation would run a handler the dispatcher runs again.
      *
      * The dispatcher emulates keyboard activation so a component behaves the same where no browser
      * runs it. As soon as anything in the chain declares a keydown, the key reaches both, and the
      * handler runs twice: one press of Space on an accordion header opened a panel and closed it
      * again. The browser's is the one to suppress, since the dispatcher's runs after the element's
      * own `onKeyDown` and keeps the two in a defined order.
      *
      * The three branches differ because what the dispatcher emulates differs.
      *
      *   - A '''button''' is suppressed whenever a keydown is posted at all. The dispatcher answers a
      *     keydown on a button by SYNTHESIZING a click at it, which is what a browser does and what
      *     HTML means by activation behaviour, so the browser's own activation would be the second
      *     one whether or not the button carries a kyo handler. `declaresClick` used to gate this and
      *     could not: a submitting button with no click handler still submits.
      *   - An '''anchor''' keeps that gate. Suppressing Enter on a plain link would take its
      *     navigation with it, and navigation is the one thing the emulation does not carry. An
      *     anchor that DOES carry a click handler already has its navigation suppressed on the click
      *     path, so the two agree.
      *   - '''Anything else''' inside a form is HTML's implicit submission: Enter's default is to
      *     click the form's default button. The dispatcher synthesizes that click too, so the
      *     browser's must go, or the form hears both.
      *
      * A SUBMITTING button used to be exempted here altogether, on the grounds that the emulation did
      * not carry a form submit. That was the shape of the bug rather than a reason: the emulation did
      * not carry it because activation was emulated as a direct `onClick` call at the target, which
      * does not bubble, so a form never saw it and needed a keydown rule of its own. Two rules for one
      * platform behaviour, and they disagreed — `onSubmit` ran twice on both DOM transports. Now the
      * dispatcher produces a real click and this is the only rule.
      */
    def doubleActivates(
        key: String,
        tag: String,
        declaresClick: Boolean,
        declaresKeyDown: Boolean,
        insideForm: Boolean
    ): Boolean =
        declaresKeyDown && {
            tag match
                case "BUTTON" => activatesButton(key)
                case "A"      => declaresClick && activatesLink(key)
                case _        => insideForm && key == "Enter"
        }
    end doubleActivates

    /** `keys` as a JavaScript disjunction over `e.key`, for the server-push client. */
    /** Keys whose browser default CHANGES a native control's own value: Space toggles a checkbox or
      * a radio and presses a button, and the arrows walk a radio group and move a range's thumb.
      *
      * Enter is deliberately absent. It changes no native control's value (it submits the form
      * around it), and taking it would cost a reader the key that submits.
      */
    val activationKeys: Seq[String] =
        Seq(" ", "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight")

    /** Whether `key` would change the value of a native control inside a region that declared
      * itself inert (`preventActivation`).
      *
      * A kyo handler runs asynchronously, and remotely on the server-push transport, so it cannot
      * decline the browser default in time. A readonly checkbox therefore cannot say "do not
      * toggle" from a handler: either it is natively `disabled`, which takes it out of the tab
      * order and makes readonly indistinguishable from disabled, or the client declines the
      * default on its behalf. This is the second.
      */
    def suppressesActivation(key: String): Boolean = activationKeys.contains(key)

    def jsKeyTest(keys: Seq[String]): String = keys.map(k => s"""e.key==="$k"""").mkString("||")

    /** `tags` as the body of a JavaScript regexp alternation, for the server-push client. */
    def jsTagTest(tags: Seq[String]): String = tags.mkString("|")

end KeyPolicy
