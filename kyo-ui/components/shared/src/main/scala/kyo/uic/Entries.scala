package kyo.uic

/** One entry of a list setter (`items`, `nodes`, `panels`, `tabs`): the entry itself, or a function of the setter's scope that
  * builds it.
  *
  * The scope is what `_` stands for, so both spellings read the same and can be mixed in one call:
  *
  * {{{
  * uic.SplitButton("Save").items(
  *     _.item("Update").icon(uic.Icons.refresh),
  *     _.item.separator,
  *     uic.MenuItem("Homepage").url("https://kyo.dev")
  * )
  * }}}
  *
  * Each scope offers the builders of the one entry type its setter takes, and nothing else, so a completion after `_.` lists what the
  * component accepts there. It is one setter over a union rather than two overloads: with two alternatives the arguments are typed
  * without an expected type, and `_` would have no type to complete against.
  *
  * Setters name it through one alias per entry type, `MenuItemEntry` for `Entry[MenuItem, MenuItemScope]` and so on, declared next
  * to its scope.
  */
type Entry[A, S] = A | (S => A)

private[uic] object Entry:

    /** The entries as values, each function applied to `scope`.
      *
      * No entry type is a function, so a function in the sequence is always the builder half of the union; the casts restate what the
      * union already says and erasure keeps from the match.
      */
    def resolve[A, S](scope: S, entries: Seq[Entry[A, S]]): List[A] =
        entries.iterator.map {
            case build: Function1[?, ?] => build.asInstanceOf[S => A](scope)
            case value                  => value.asInstanceOf[A]
        }.toList
end Entry
