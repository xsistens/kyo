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
      * Only an element that carries a kyo click handler qualifies, and only where the emulation
      * carries the whole of what the browser would do. It does not carry a form submit, so a
      * submitting button is left alone; it does not carry navigation either, but an anchor with a
      * click handler already has its navigation suppressed on the click path, so the two agree.
      */
    def doubleActivates(
        key: String,
        tag: String,
        submits: Boolean,
        declaresClick: Boolean,
        declaresKeyDown: Boolean
    ): Boolean =
        declaresClick && declaresKeyDown && {
            tag match
                case "BUTTON" => activatesButton(key) && !submits
                case "A"      => activatesLink(key)
                case _        => false
        }
    end doubleActivates

    /** `keys` as a JavaScript disjunction over `e.key`, for the server-push client. */
    def jsKeyTest(keys: Seq[String]): String = keys.map(k => s"""e.key==="$k"""").mkString("||")

    /** `tags` as the body of a JavaScript regexp alternation, for the server-push client. */
    def jsTagTest(tags: Seq[String]): String = tags.mkString("|")

end KeyPolicy
