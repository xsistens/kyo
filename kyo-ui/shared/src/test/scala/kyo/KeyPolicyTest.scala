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

    private def doubles(key: String, tag: String, declaresClick: Boolean = true, insideForm: Boolean = false): Boolean =
        KeyPolicy.doubleActivates(key, tag, declaresClick, declaresKeyDown = true, insideForm)

    "a button is activated twice by Enter and by Space, submitting or not, handler or not" in {
        // A SUBMITTING button used to be exempted, on the grounds that the dispatcher's emulation
        // did not carry a form submit. It does now — as a synthesized click — so the browser's own
        // activation is the second one whatever the type says (GAPS.md F-36). `declaresClick` does
        // not gate a button either: a submitting button with no click handler still submits, so
        // there would still be two of everything.
        assert(doubles("Enter", "BUTTON"))
        assert(doubles(" ", "BUTTON"))
        assert(doubles("Enter", "BUTTON", declaresClick = false))
        assert(doubles(" ", "BUTTON", declaresClick = false))
    }

    "Enter inside a form is implicit submission, which the dispatcher also answers with a click" in {
        assert(doubles("Enter", "INPUT", declaresClick = false, insideForm = true))
        assert(doubles("Enter", "DIV", declaresClick = false, insideForm = true))
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
        assert(!KeyPolicy.doubleActivates("Enter", "BUTTON", declaresClick = true, declaresKeyDown = false, insideForm = true))
        assert(!KeyPolicy.doubleActivates("Enter", "INPUT", declaresClick = false, declaresKeyDown = false, insideForm = true))
    }

    "an element the browser does not activate, outside a form, is left alone" in {
        assert(!doubles("Enter", "DIV"))
        assert(!doubles(" ", "SPAN"))
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
            KeyPolicy.activationKeys
        )
        val missing = expected.filterNot(keys => clientJs.contains(KeyPolicy.jsKeyTest(keys)))
        assert(missing.isEmpty, s"the client script does not carry these key sets verbatim: $missing")
    }

    "the server-push client tests exactly the tags the policy names" in {
        val expected = List(KeyPolicy.editableTags, KeyPolicy.verticalConsumerTags, KeyPolicy.spaceActivatedTags)
        val missing  = expected.filterNot(tags => clientJs.contains(KeyPolicy.jsTagTest(tags)))
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
        val generated = List(KeyPolicy.verticalScrollKeys, KeyPolicy.edgeScrollKeys).map(KeyPolicy.jsKeyTest)
        val stripped  = generated.foldLeft(block)((js, gen) => js.replace(gen, ""))
        val governed  = (KeyPolicy.verticalScrollKeys ++ KeyPolicy.edgeScrollKeys).distinct
        val leaked    = governed.filter(k => stripped.contains(s"""e.key==="$k""""))
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
