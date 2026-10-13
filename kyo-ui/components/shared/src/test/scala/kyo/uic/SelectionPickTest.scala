package kyo.uic

import kyo.*

/** The two rulesets behind [[SelectionMode]], which six components share.
  *
  * Written as a unit test rather than six behavioural ones because the rule is the thing that
  * must not drift: each host's own test proves it is WIRED, this proves what it is wired to.
  * The expectations come from PrimeVue's `DataTable.onRowClick`, read rather than remembered.
  */
class SelectionPickTest extends UicTest:

    private def pick(
        mode: SelectionMode,
        metaKeySelection: Boolean,
        meta: Boolean,
        id: String,
        current: Set[String]
    ): Set[String] = SelectionPick.next(mode, metaKeySelection, meta, id, current)

    "without the flag every activation toggles, and the modifier means nothing" in {
        assert(pick(SelectionMode.Multiple, false, false, "b", Set("a")) == Set("a", "b"))
        assert(pick(SelectionMode.Multiple, false, true, "b", Set("a")) == Set("a", "b"))
        assert(pick(SelectionMode.Multiple, false, false, "a", Set("a", "b")) == Set("b"))
    }

    "without the flag a single selection clears when it is picked again" in {
        assert(pick(SelectionMode.Single, false, false, "a", Set("a")) == Set.empty)
        assert(pick(SelectionMode.Single, false, false, "b", Set("a")) == Set("b"))
    }

    "with the flag a plain activation replaces the whole set" in {
        assert(pick(SelectionMode.Multiple, true, false, "c", Set("a", "b")) == Set("c"))
    }

    "with the flag a plain activation on a picked id collapses to it rather than clearing" in {
        // The corner that makes the gesture work: it leaves an anchor under the pointer, so
        // click-then-shift-click is a range every time and not every other time.
        assert(pick(SelectionMode.Multiple, true, false, "a", Set("a", "b")) == Set("a"))
        assert(pick(SelectionMode.Single, true, false, "a", Set("a")) == Set("a"))
    }

    "with the flag a modified activation adds, and removes what is already there" in {
        assert(pick(SelectionMode.Multiple, true, true, "c", Set("a", "b")) == Set("a", "b", "c"))
        assert(pick(SelectionMode.Multiple, true, true, "a", Set("a", "b")) == Set("b"))
    }

    "a single selection cannot grow, even with the modifier" in {
        assert(pick(SelectionMode.Single, true, true, "b", Set("a")) == Set("b"))
        assert(pick(SelectionMode.Single, true, true, "a", Set("a")) == Set.empty)
    }

    "Checkbox keeps toggling under either flag, because the box already said so" in {
        assert(pick(SelectionMode.Checkbox, true, false, "b", Set("a")) == Set("a", "b"))
        assert(pick(SelectionMode.Checkbox, true, true, "a", Set("a", "b")) == Set("b"))
    }

    "None is inert whatever is held down" in {
        assert(pick(SelectionMode.None, true, true, "b", Set("a")) == Set("a"))
        assert(pick(SelectionMode.None, false, false, "b", Set("a")) == Set("a"))
    }

end SelectionPickTest
