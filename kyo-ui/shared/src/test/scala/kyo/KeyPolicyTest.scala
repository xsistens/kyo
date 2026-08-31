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

    private def doubles(key: String, tag: String, submits: Boolean = false): Boolean =
        KeyPolicy.doubleActivates(key, tag, submits, declaresClick = true, declaresKeyDown = true)

    "a button with a click handler is activated twice by Enter and by Space" in {
        assert(doubles("Enter", "BUTTON"))
        assert(doubles(" ", "BUTTON"))
    }

    "a submitting button is left alone, since the emulation does not carry the submit" in {
        assert(!doubles("Enter", "BUTTON", submits = true))
        assert(!doubles(" ", "BUTTON", submits = true))
    }

    "an anchor with a click handler is activated twice by Enter" in {
        // The same defect as the button's, one element type over: the browser turns Enter into a
        // click on the anchor, and the dispatcher emulates the activation from the keydown.
        assert(doubles("Enter", "A"))
    }

    "an anchor is not activated by Space at all, so there is nothing to suppress" in {
        assert(!doubles(" ", "A"))
    }

    "an element with no click handler, or none in the chain declaring a keydown, is left alone" in {
        assert(!KeyPolicy.doubleActivates("Enter", "BUTTON", false, declaresClick = false, declaresKeyDown = true))
        assert(!KeyPolicy.doubleActivates("Enter", "BUTTON", false, declaresClick = true, declaresKeyDown = false))
    }

    "an element the browser does not activate is left alone" in {
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
            KeyPolicy.linkActivationKeys
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

end KeyPolicyTest
