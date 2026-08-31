package kyo.uic

import kyo.*

/** Pure-logic assertions for [[Typeahead]]: where a typed letter moves a list's highlight.
  */
class TypeaheadTest extends UicTest:

    // Positions 1 and 4 are a heading and a disabled option, so the highlight skips them.
    private val labels    = Seq("Argentina", "AMERICAS", "Brazil", "Bolivia", "Belgium", "Canada")
    private val navigable = List(0, 2, 3, 5)

    private def jump(from: Int, c: Char) = Typeahead.jump(labels, navigable, from, c)

    "a letter from nothing highlighted lands on the first match" in assert(
        jump(-1, 'b') == Present(2)
    )

    "the same letter again walks to the next match" in assert(
        jump(2, 'b') == Present(3)
    )

    "and wraps back to the first once the matches run out" in assert(
        jump(3, 'b') == Present(2)
    )

    "matching ignores case in both directions" in assert(
        jump(-1, 'C') == Present(5) && jump(-1, 'a') == Present(0)
    )

    "a row the keyboard cannot land on is not a match, however it reads" in assert(
        // Position 1 is "AMERICAS", the only other entry starting with A, and it is not navigable.
        jump(0, 'a') == Present(0)
    )

    "a letter nothing starts with moves nothing, so the host leaves the key alone" in assert(
        jump(-1, 'z') == Absent
    )

    "an empty list matches nothing" in assert(
        Typeahead.jump(labels, Nil, -1, 'a') == Absent
    )

    "a label indented for layout still begins with its own first letter" in assert(
        Typeahead.jump(Seq("   Denmark"), List(0), -1, 'd') == Present(0)
    )

    "a label with nothing in it matches nothing" in assert(
        Typeahead.jump(Seq("", "   "), List(0, 1), -1, ' ') == Absent
    )

    "a position with no label is skipped rather than throwing" in assert(
        Typeahead.jump(Seq("Apple"), List(0, 7), -1, 'a') == Present(0)
    )

end TypeaheadTest
