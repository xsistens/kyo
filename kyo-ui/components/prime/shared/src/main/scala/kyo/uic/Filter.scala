package kyo.uic

/** Ready-made matchers for a picker's `filter` ([[Select.filter]], [[MultiSelect.filter]], [[AutoComplete.filter]]).
  *
  * A matcher is a plain `(A, String) => Boolean`: the option, and the query as the reader typed it. Each one here
  * takes the texts of an option it should search, as projections, and matches where ANY of them matches, so a user
  * is found by name or by email with `Filter.contains(_.name, _.email)`. Both sides are lowercased before they are
  * compared, so the match ignores case. An empty query matches every option.
  *
  * The option type comes from the picker the matcher is handed to, so a call site needs no type argument:
  * {{{
  * uic.Select[User]().options(users)(_.name).filter(uic.Filter.contains(_.name, _.email))
  * }}}
  *
  * Without a `filter`, Select and MultiSelect match `contains` on the label projection and AutoComplete
  * `startsWithPerTerm` on it. [[all]] matches everything, for an app that filters the options itself (on a server,
  * say) and hands the picker only what should show. Any other function of the same shape is a matcher too.
  */
object Filter:
    /** Matches where the query occurs anywhere in any of `texts`. */
    def contains[A](texts: (A => String)*): (A, String) => Boolean =
        matching(texts)((text, query) => text.contains(query))

    /** Matches where any of `texts` starts with the query. */
    def startsWith[A](texts: (A => String)*): (A, String) => Boolean =
        matching(texts)((text, query) => text.startsWith(query))

    /** Matches where any whitespace-separated word of any of `texts` starts with the query: "hop" finds
      * "Grace Hopper". AutoComplete's default.
      */
    def startsWithPerTerm[A](texts: (A => String)*): (A, String) => Boolean =
        matching(texts)((text, query) => text.split("\\s+").exists(_.startsWith(query)))

    /** Matches every option, whatever was typed. */
    def all[A]: (A, String) => Boolean = (_, _) => true

    /** A matcher that lowercases the query and each text of the option, and asks `test` of each pair until one holds. */
    private def matching[A](texts: Seq[A => String])(test: (String, String) => Boolean): (A, String) => Boolean =
        (a, query) =>
            val q = query.toLowerCase
            texts.exists(t => test(t(a).toLowerCase, q))

end Filter
