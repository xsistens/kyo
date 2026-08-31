package kyo.uic

import kyo.*

/** Pure jump-to-label state machine: where a printable character moves a list's highlight.
  *
  * The ARIA combobox and listbox patterns both say a reader who types a letter lands on the next
  * option beginning with it, and lands on the one after that if they type the same letter again.
  * That is the whole of what this does, in the shape [[ListNav]], [[MenuNav]] and [[GridNav]] use:
  * no DOM, no effects, unit-tested directly (`TypeaheadTest`).
  *
  * ONE character, not a buffer. A multi-character buffer has to expire, and an expiry needs a
  * clock; a clock in a pure machine is a clock the tests have to control and the hosts have to
  * thread through. The single-character form covers what a reader actually does in a list of a
  * few dozen options, and repeating the letter walks the matches, so nothing is out of reach.
  */
private[uic] object Typeahead:

    /** The position `c` jumps to, searching forward from just after `from` and wrapping once.
      *
      * `Absent` when nothing matches, which the host reads as "leave the key alone": a Char that
      * moved no highlight belongs to whatever else wants it, a filter field most of all.
      *
      * Searching from `from + 1` rather than from `from` is what makes a repeated letter walk its
      * matches instead of standing still, and a `from` of `-1` (nothing highlighted yet) starts at
      * the top without a special case.
      */
    def jump(labels: Seq[String], navigable: List[Int], from: Int, c: Char): Maybe[Int] =
        if navigable.isEmpty then Absent
        else
            val target = c.toLower
            val order  = navigable.filter(_ > from) ++ navigable.filter(_ <= from)
            Maybe.fromOption(order.find(i => labels.isDefinedAt(i) && starts(labels(i), target)))
    end jump

    /** Whether `label` begins with `target`, which is already lower-cased. Leading whitespace is
      * skipped, since a label that is indented for layout still begins with its own first letter.
      */
    private def starts(label: String, target: Char): Boolean =
        val trimmed = label.dropWhile(_.isWhitespace)
        trimmed.nonEmpty && trimmed.charAt(0).toLower == target

end Typeahead
