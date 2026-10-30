package kyo.internal

/** The keyboard rules the client applies to a key press before it becomes a kyo event.
  *
  * There are two clients: the SPA backend, which is Scala ([[kyo.internal.DomBackend]]), and the
  * server-push renderer, which ships the same logic as JavaScript text ([[kyo.internal.HtmlRenderer]]).
  * They have to answer identically for the same key on the same target, so the answer lives here: the
  * Scala client calls these predicates, the JavaScript is built from these values, and `KeyPolicyTest`
  * holds the emitted JavaScript against them. Neither transport can drift without failing that test.
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

    /** The vertical arrows, which a stepping input (below) uses to change its value. */
    val arrowStepKeys: Seq[String] = Seq("ArrowUp", "ArrowDown")

    /** The page keys, which a slider uses to move by a page step. */
    val pageStepKeys: Seq[String] = Seq("PageUp", "PageDown")

    /** Input types that change their value on the vertical arrows: a number spins, a slider moves, a date or a time steps
      * the part the caret is in. A single-line text input is not among them (see [[verticalConsumerTags]]), so for these the
      * arrows belong to the field even inside a navigation region: a table's number cell editor stayed on its first value
      * whatever the reader pressed.
      */
    val arrowSteppingInputTypes: Seq[String] = Seq("number", "range", "date", "time", "datetime-local", "month", "week")

    /** Input types that also move on the page keys: a slider steps by a page. */
    val pageSteppingInputTypes: Seq[String] = Seq("range")

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

    /** Keys that submit a form from a field: HTML's implicit submission, Enter alone. */
    val implicitSubmissionKeys: Seq[String] = Seq("Enter")

    def activatesButton(key: String): Boolean = buttonActivationKeys.contains(key)

    def submitsImplicitly(key: String): Boolean = implicitSubmissionKeys.contains(key)

    /** Keys that can activate anything at all: a button's, a link's, or a form's implicit submission. Every other key is decided
      * without reading the target, so the clients test this before any walk.
      */
    val anyActivationKeys: Seq[String] = (buttonActivationKeys ++ linkActivationKeys ++ implicitSubmissionKeys).distinct

    def mayActivate(key: String): Boolean = anyActivationKeys.contains(key)

    def activatesLink(key: String): Boolean = linkActivationKeys.contains(key)

    /** Whether to suppress the browser's page scroll for `key` on this target.
      *
      * Only consulted inside a region that asked for it with `UI.preventScrollKeys`, which is how a
      * component that navigates with the arrows keeps the page still under it. Space is in here for
      * the same reason the arrows are: a list that is one tab stop has no native control to consume
      * it, so a Space that picks the highlighted row also scrolled the page a screenful. `inputType` is an `INPUT`'s `type`,
      * which decides whether the field steps its own value on a vertical key.
      */
    def preventsPageScroll(key: String, tag: String, contentEditable: Boolean, inputType: String = ""): Boolean =
        val editable = contentEditable || editableTags.contains(tag)
        val steps    = tag == "INPUT" && (
            (arrowStepKeys.contains(key) && arrowSteppingInputTypes.contains(inputType.toLowerCase)) ||
                (pageStepKeys.contains(key) && pageSteppingInputTypes.contains(inputType.toLowerCase))
        )
        val verticalConsumer = contentEditable || verticalConsumerTags.contains(tag) || steps
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
      *     keydown on a button by synthesizing a click at it, which is what a browser does and what
      *     HTML means by activation behaviour, so the browser's own activation would be the second
      *     one whether or not the button carries a kyo handler. `declaresClick` cannot gate this: a
      *     submitting button with no click handler still submits.
      *   - An '''anchor''' keeps that gate. Suppressing Enter on a plain link would take its
      *     navigation with it, and navigation is the one thing the emulation does not carry. An
      *     anchor that does carry a click handler already has its navigation suppressed on the click
      *     path, so the two agree.
      *   - An '''input''' inside a form is HTML's implicit submission: Enter's default is to click the
      *     form's default button. The dispatcher synthesizes that click too, so the browser's must go,
      *     or the form hears both. Nothing else submits on Enter: a textarea takes it as a line break
      *     and a select as a dropdown interaction, and both keep it.
      *
      * A submitting button is not exempt: the synthesized click bubbles to its form like any other
      * click, so the form's submission comes from that click and the browser's would be a second one.
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
                case "INPUT"  => insideForm && submitsImplicitly(key)
                case _        => false
        }
    end doubleActivates

    /** Keys whose browser default changes a native control's own value: Space toggles a checkbox or
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

    /** The keys a browser walks a radio group with: each one moves the focus AND the selection to the next radio. */
    val radioStepKeys: Seq[String] = Seq("ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight")

    /** What an arrow pressed on a radio does to its group, see [[radioStep]]. */
    enum RadioStep derives CanEqual:
        /** The browser's own walk: right as long as no radio in the group is inert. */
        case Native

        /** Nothing moves: no other radio may be selected, or the focused one is inert and selected, so leaving it would
          * change it.
          */
        case Hold

        /** The client focuses and selects the radio at this index of the group, which is where the browser's walk would
          * land if every inert radio were disabled.
          */
        case Select(index: Int)
    end RadioStep

    /** The walk an arrow key takes through a radio group in which some radio is inert (`preventActivation`).
      *
      * The browser's walk skips a disabled radio, but an inert one is focusable, so the walk lands on it: its selection
      * is declined (the client cancels the click the walk simulates), the focus stays on it, and every further arrow is
      * declined there too, which leaves the reader stuck on a radio they cannot select (measured in Chrome, `RadioTest`).
      * So the client walks instead: on in the key's direction, wrapping, to the next radio that is neither inert nor
      * disabled, which is where the browser's walk lands when the inert radios are disabled. An inert radio that is
      * selected holds the group, because walking away from it would change it. Down and Right walk forward, Up and Left
      * back; in a right-to-left group Left and Right swap, as they do natively.
      *
      * @param inert
      *   for each radio of the group in tree order, whether it is inside an inert region
      * @param disabled
      *   for each radio of the group in tree order, whether it is disabled
      * @param from
      *   the index of the radio the key was pressed on
      * @param fromChecked
      *   whether that radio is selected
      */
    def radioStep(
        key: String,
        rtl: Boolean,
        inert: Seq[Boolean],
        disabled: Seq[Boolean],
        from: Int,
        fromChecked: Boolean
    ): RadioStep =
        val n = inert.size
        if !radioStepKeys.contains(key) || !inert.contains(true) then RadioStep.Native
        else if inert(from) && fromChecked then RadioStep.Hold
        else
            val forward = key match
                case "ArrowDown"  => true
                case "ArrowUp"    => false
                case "ArrowRight" => !rtl
                case _            => rtl
            val step = if forward then 1 else n - 1
            @scala.annotation.tailrec
            def walk(i: Int, left: Int): RadioStep =
                if left == 0 then RadioStep.Hold
                else if !inert(i) && !disabled(i) then RadioStep.Select(i)
                else walk((i + step) % n, left - 1)
            walk((from + step) % n, n - 1)
        end if
    end radioStep

    /** `keys` as a JavaScript disjunction over `e.key`, for the server-push client. */
    def jsKeyTest(keys: Seq[String]): String = keys.map(k => s"""e.key==="$k"""").mkString("||")

    /** `tags` as the body of a JavaScript regexp alternation, for the server-push client. */
    def jsTagTest(tags: Seq[String]): String = tags.mkString("|")

end KeyPolicy
