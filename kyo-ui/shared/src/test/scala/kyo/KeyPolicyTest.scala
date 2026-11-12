package kyo

import kyo.internal.HtmlRenderer
import kyo.internal.KeyPolicy

/** The keyboard rules the client applies before a key becomes a kyo event.
  *
  * Two things are pinned here. The first is the rules themselves, as a truth table: which key on
  * which target loses its browser default, and which activation the browser would run twice. The
  * second is that the server-push client, which ships these rules as JavaScript text, is built from
  * the same values the SPA client calls. That second part is the point of the file: the two clients
  * were two hand-kept copies, and a copy that no test compares is a copy that drifts.
  */
class KeyPolicyTest extends kyo.test.Test[Any]:

    // ---- page scroll ----

    "an arrow key on a list that is one tab stop loses its page scroll" in {
        assert(KeyPolicy.preventsPageScroll("ArrowDown", "UL", contentEditable = false))
        assert(KeyPolicy.preventsPageScroll("ArrowUp", "DIV", contentEditable = false))
        assert(KeyPolicy.preventsPageScroll("PageDown", "UL", contentEditable = false))
    }

    "a vertical key keeps its default where the target moves a caret with it" in {
        assert(!KeyPolicy.preventsPageScroll("ArrowDown", "TEXTAREA", contentEditable = false))
        assert(!KeyPolicy.preventsPageScroll("ArrowDown", "SELECT", contentEditable = false))
        assert(!KeyPolicy.preventsPageScroll("ArrowDown", "DIV", contentEditable = true))
    }

    "an input that steps its value with the vertical keys keeps them" in {
        // A number field in a table's navigation region: suppressing the arrows as a page scroll left it on its first value.
        for t <- Seq("number", "range", "date", "time", "datetime-local", "month", "week") do
            assert(!KeyPolicy.preventsPageScroll("ArrowUp", "INPUT", contentEditable = false, inputType = t), t)
            assert(!KeyPolicy.preventsPageScroll("ArrowDown", "INPUT", contentEditable = false, inputType = t), t)
        end for
        assert(KeyPolicy.preventsPageScroll("ArrowDown", "INPUT", contentEditable = false, inputType = "text"))
        assert(KeyPolicy.preventsPageScroll("PageDown", "INPUT", contentEditable = false, inputType = "number"))
        // A slider moves by a page step on PageUp and PageDown too.
        assert(!KeyPolicy.preventsPageScroll("PageDown", "INPUT", contentEditable = false, inputType = "range"))
        assert(!KeyPolicy.preventsPageScroll("PageUp", "INPUT", contentEditable = false, inputType = "range"))
    }

    "a single-line input stays suppressed for the vertical keys, which is the combobox case" in {
        assert(KeyPolicy.preventsPageScroll("ArrowDown", "INPUT", contentEditable = false))
    }

    "an edge key keeps its default in anything that takes text" in {
        assert(KeyPolicy.preventsPageScroll("Home", "UL", contentEditable = false))
        assert(!KeyPolicy.preventsPageScroll("Home", "INPUT", contentEditable = false))
        assert(!KeyPolicy.preventsPageScroll("ArrowLeft", "TEXTAREA", contentEditable = false))
        assert(!KeyPolicy.preventsPageScroll("End", "DIV", contentEditable = true))
    }

    "Space on a list that is one tab stop loses its page scroll" in {
        // The bug this closes: Space picked the highlighted row AND scrolled the page a screenful,
        // because nothing native was focused to consume it.
        assert(KeyPolicy.preventsPageScroll(" ", "UL", contentEditable = false))
        assert(KeyPolicy.preventsPageScroll(" ", "DIV", contentEditable = false))
    }

    "Space keeps its default wherever it would type or activate" in {
        assert(!KeyPolicy.preventsPageScroll(" ", "INPUT", contentEditable = false))
        assert(!KeyPolicy.preventsPageScroll(" ", "TEXTAREA", contentEditable = false))
        assert(!KeyPolicy.preventsPageScroll(" ", "DIV", contentEditable = true))
        assert(!KeyPolicy.preventsPageScroll(" ", "BUTTON", contentEditable = false))
    }

    "Space on an anchor loses its default, because a link does not consume it" in {
        assert(KeyPolicy.preventsPageScroll(" ", "A", contentEditable = false))
    }

    "a key that scrolls nothing is left alone" in {
        assert(!KeyPolicy.preventsPageScroll("Enter", "UL", contentEditable = false))
        assert(!KeyPolicy.preventsPageScroll("Tab", "UL", contentEditable = false))
        assert(!KeyPolicy.preventsPageScroll("a", "UL", contentEditable = false))
    }

    // ---- double activation ----

    private def doubles(
        key: String,
        tag: String,
        declaresClick: Boolean = true,
        insideForm: Boolean = false,
        role: String = ""
    ): Boolean =
        KeyPolicy.doubleActivates(key, tag, declaresClick, declaresKeyDown = true, insideForm, role)

    "an element with role button and a click handler declines the browser's Enter and Space, which the dispatcher answers" in {
        // A span or a div does nothing on Enter and scrolls the page on Space; the dispatcher's click is the activation.
        assert(doubles("Enter", "SPAN", role = "button"))
        assert(doubles(" ", "DIV", role = "button"))
        // An anchor that says it is a button answers Space as well, and its own Enter would be a second click.
        assert(doubles(" ", "A", role = "button"))
        assert(doubles("Enter", "A", role = "button"))
    }

    "an element with role button and no click handler keeps the browser's keys" in {
        // Nothing is emulated without a click handler on the element itself: a navigation link styled as a button still
        // follows its href on Enter, which the dispatcher does not carry.
        assert(!doubles("Enter", "SPAN", declaresClick = false, role = "button"))
        assert(!doubles("Enter", "A", declaresClick = false, role = "button"))
        assert(!doubles("a", "SPAN", role = "button"))
    }

    "the dispatcher and the clients read one rule for what a key activates on an element with role button" in {
        assert(KeyPolicy.activatesAsButton("Enter", declaresClick = true, role = "button"))
        assert(KeyPolicy.activatesAsButton(" ", declaresClick = true, role = "button"))
        assert(!KeyPolicy.activatesAsButton("Escape", declaresClick = true, role = "button"))
        assert(!KeyPolicy.activatesAsButton("Enter", declaresClick = true, role = "link"))
        assert(!KeyPolicy.activatesAsButton("Enter", declaresClick = false, role = "button"))
    }

    "a button is activated twice by Enter and by Space, submitting or not, handler or not" in {
        // A submitting button is not exempt: the dispatcher's synthesized click carries the form
        // submit, so the browser's own activation is the second one whatever the type says.
        // `declaresClick` does not gate a button either: a submitting button with no click handler
        // still submits, so there would still be two of everything.
        assert(doubles("Enter", "BUTTON"))
        assert(doubles(" ", "BUTTON"))
        assert(doubles("Enter", "BUTTON", declaresClick = false))
        assert(doubles(" ", "BUTTON", declaresClick = false))
    }

    "Enter in an input inside a form is implicit submission, which the dispatcher also answers with a click" in {
        assert(doubles("Enter", "INPUT", declaresClick = false, insideForm = true))
    }

    "Enter inside a form keeps its default everywhere it does not submit" in {
        // A textarea takes Enter as a line break and a select as a dropdown interaction; on any other element
        // inside a form a browser submits nothing, and neither does the dispatcher.
        assert(!doubles("Enter", "TEXTAREA", declaresClick = false, insideForm = true))
        assert(!doubles("Enter", "SELECT", declaresClick = false, insideForm = true))
        assert(!doubles("Enter", "DIV", declaresClick = false, insideForm = true))
    }

    "Enter outside a form activates nothing, so there is nothing to suppress" in {
        assert(!doubles("Enter", "INPUT"))
        assert(!doubles("Enter", "DIV"))
    }

    "Space in a field types a space; only Enter submits implicitly" in {
        assert(!doubles(" ", "INPUT", insideForm = true))
    }

    "an anchor with a click handler is activated twice by Enter" in {
        // The same defect as the button's, one element type over: the browser turns Enter into a
        // click on the anchor, and the dispatcher emulates the activation from the keydown.
        assert(doubles("Enter", "A"))
    }

    "an anchor is not activated by Space at all, so there is nothing to suppress" in {
        assert(!doubles(" ", "A"))
    }

    "an anchor without a click handler keeps its navigation, which the dispatcher does not carry" in {
        assert(!doubles("Enter", "A", declaresClick = false))
    }

    "nothing in the chain declaring a keydown means no keydown is posted, so nothing is emulated" in {
        assert(!KeyPolicy.doubleActivates("Enter", "BUTTON", declaresClick = true, declaresKeyDown = false, insideForm = true, role = ""))
        assert(!KeyPolicy.doubleActivates("Enter", "INPUT", declaresClick = false, declaresKeyDown = false, insideForm = true, role = ""))
    }

    "an element the browser does not activate, outside a form, is left alone" in {
        assert(!doubles("Enter", "DIV"))
        assert(!doubles(" ", "SPAN"))
    }

    "a key outside the activation keys activates nothing, so the clients may skip reading the target" in {
        val keys       = Seq("a", "Tab", "Escape", "ArrowDown", "Backspace", "Enter", " ")
        val tags       = Seq("BUTTON", "A", "INPUT", "DIV", "SPAN")
        val activating =
            for
                key  <- keys
                tag  <- tags
                ck   <- Seq(true, false)
                form <- Seq(true, false)
                role <- Seq("", "button")
                if KeyPolicy.doubleActivates(key, tag, declaresClick = ck, declaresKeyDown = true, insideForm = form, role = role)
            yield key
        assert(activating.distinct.forall(KeyPolicy.mayActivate), activating.distinct.mkString(","))
        assert(!KeyPolicy.mayActivate("a") && !KeyPolicy.mayActivate("Tab"))
    }

    // ---- radio groups with an inert radio ----

    import KeyPolicy.RadioStep

    /** The walk over a group of three, the middle one inert, from `from`. */
    private def step(key: String, from: Int, inert: Seq[Boolean] = Seq(false, true, false), rtl: Boolean = false): RadioStep =
        KeyPolicy.radioStep(key, rtl, inert, disabled = Seq.fill(inert.size)(false), from = from, fromChecked = !inert(from))

    "a group with no inert radio keeps the browser's own walk" in {
        for key <- KeyPolicy.radioStepKeys do
            assert(step(key, 0, inert = Seq(false, false, false)) == RadioStep.Native, key)
    }

    "an arrow walks past an inert radio to the next one the reader may select, the way it walks past a disabled one" in {
        // Measured in Chrome (RadioTest): with the middle radio disabled, Down and Right from the first select the third.
        assert(step("ArrowDown", 0) == RadioStep.Select(2))
        assert(step("ArrowRight", 0) == RadioStep.Select(2))
        assert(step("ArrowUp", 2) == RadioStep.Select(0))
        assert(step("ArrowLeft", 2) == RadioStep.Select(0))
    }

    "the walk wraps around the group, past an inert radio at either end" in {
        assert(step("ArrowDown", 1, inert = Seq(false, false, true)) == RadioStep.Select(0))
        assert(step("ArrowUp", 0, inert = Seq(true, false, false)) == RadioStep.Select(2))
        assert(step("ArrowDown", 2) == RadioStep.Select(0))
    }

    "a disabled radio is passed over too" in {
        val s = KeyPolicy.radioStep("ArrowDown", rtl = false, Seq(false, true, false, false), Seq(false, false, true, false), 0, true)
        assert(s == RadioStep.Select(3))
    }

    "an inert radio that is not selected is left for the next one the reader may select" in {
        assert(KeyPolicy.radioStep("ArrowDown", rtl = false, Seq(false, true, false), Seq.fill(3)(false), 1, false) == RadioStep.Select(2))
    }

    "an inert radio that is selected holds the group, since any walk away from it would change it" in {
        assert(KeyPolicy.radioStep("ArrowDown", rtl = false, Seq(false, true, false), Seq.fill(3)(false), 1, true) == RadioStep.Hold)
    }

    "a group with no other radio the reader may select holds" in {
        assert(step("ArrowDown", 0, inert = Seq(false, true, true)) == RadioStep.Hold)
    }

    "right to left swaps Left and Right, and leaves Up and Down" in {
        assert(step("ArrowLeft", 0, rtl = true) == RadioStep.Select(2))
        assert(step("ArrowRight", 2, rtl = true) == RadioStep.Select(0))
        assert(step("ArrowDown", 0, rtl = true) == RadioStep.Select(2))
    }

    "a key that does not walk a group leaves it to the browser" in {
        assert(step(" ", 0) == RadioStep.Native)
        assert(step("Enter", 0) == RadioStep.Native)
        assert(step("Home", 0) == RadioStep.Native)
    }

    // ---- the two clients agree ----

    /** The server-push client's script, which is where the JavaScript twin of these rules lives. */
    private val clientJs: String = HtmlRenderer.clientJs("")

    "the server-push client tests exactly the keys the policy names" in {
        val expected = List(
            KeyPolicy.verticalScrollKeys,
            KeyPolicy.edgeScrollKeys,
            KeyPolicy.buttonActivationKeys,
            KeyPolicy.linkActivationKeys,
            KeyPolicy.implicitSubmissionKeys,
            KeyPolicy.anyActivationKeys,
            KeyPolicy.activationKeys,
            KeyPolicy.radioStepKeys
        )
        val missing = expected.filterNot(keys => clientJs.contains(KeyPolicy.jsKeyTest(keys)))
        assert(missing.isEmpty, s"the client script does not carry these key sets verbatim: $missing")
    }

    "the server-push client tests exactly the tags the policy names" in {
        val expected = List(
            KeyPolicy.editableTags,
            KeyPolicy.verticalConsumerTags,
            KeyPolicy.spaceActivatedTags,
            KeyPolicy.arrowSteppingInputTypes,
            KeyPolicy.pageSteppingInputTypes
        )
        val missing = expected.filterNot(tags => clientJs.contains(KeyPolicy.jsTagTest(tags)))
        assert(missing.isEmpty, s"the client script does not carry these tag sets verbatim: $missing")
    }

    "the scroll block names no key by hand" in {
        // A literal `e.key==="ArrowUp"` inside this block, beside the generated disjunctions, is how
        // the two copies drifted apart before. Scoped to the block on purpose: other features name
        // the same keys for their own reasons (a number input steps on the vertical arrows, a focus
        // group cycles on the horizontal ones), and those are not this policy's to own.
        val from  = clientJs.indexOf("data-kyo-scroll-keys")
        val until = clientJs.indexOf("__spk)e.preventDefault();", from)
        assert(from >= 0 && until > from, "the scroll block is no longer recognizable in the client script")
        val block     = clientJs.substring(from, until)
        val generated = List(KeyPolicy.verticalScrollKeys, KeyPolicy.edgeScrollKeys, KeyPolicy.arrowStepKeys, KeyPolicy.pageStepKeys)
            .map(KeyPolicy.jsKeyTest)
        val stripped = generated.foldLeft(block)((js, gen) => js.replace(gen, ""))
        val governed = (KeyPolicy.verticalScrollKeys ++ KeyPolicy.edgeScrollKeys).distinct
        val leaked   = governed.filter(k => stripped.contains(s"""e.key==="$k""""))
        assert(leaked.isEmpty, s"these keys are still named by hand in the scroll block: $leaked")
    }

    "the scroll block and the policy agree on which tags exempt Space" in {
        val from  = clientJs.indexOf("data-kyo-scroll-keys")
        val until = clientJs.indexOf("__spk)e.preventDefault();", from)
        val block = clientJs.substring(from, until)
        assert(block.contains(KeyPolicy.jsTagTest(KeyPolicy.spaceActivatedTags)))
        assert(!block.contains("\"A\""), "an anchor must not be exempted: Space scrolls with a link focused")
    }

    "the inert block names no key by hand either" in {
        val from  = clientJs.indexOf("data-kyo-inert")
        val until = clientJs.indexOf("e.preventDefault();", from)
        assert(from >= 0 && until > from, "the inert block is no longer recognizable in the client script")
        val stripped = clientJs.substring(from, until).replace(KeyPolicy.jsKeyTest(KeyPolicy.activationKeys), "")
        val leaked   = KeyPolicy.activationKeys.filter(k => stripped.contains(s"""e.key==="$k""""))
        assert(leaked.isEmpty, s"these keys are still named by hand in the inert block: $leaked")
    }

    "the activation keys are the ones that change a native value, and Enter is not among them" in {
        assert(KeyPolicy.suppressesActivation(" "), "Space toggles a checkbox and walks a radio group")
        assert(KeyPolicy.activationKeys.count(_.startsWith("Arrow")) == 4, "all four arrows move a radio or a range")
        assert(!KeyPolicy.suppressesActivation("Enter"), "Enter submits the form; it changes no native value")
        assert(!KeyPolicy.suppressesActivation("Home"))
        assert(!KeyPolicy.suppressesActivation("Tab"), "an inert control is still one a reader can leave")
    }

end KeyPolicyTest
