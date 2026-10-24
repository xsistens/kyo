package kyo.uic

import kyo.*

/** The ready-made matchers a picker's `filter` takes: each is `(option, typed query) => Boolean`, case-insensitive on
  * both sides, and true where ANY of the texts it is given matches.
  */
class FilterTest extends UicTest:

    private val ada   = FilterTest.User("Ada Lovelace", "ada@engine.org")
    private val grace = FilterTest.User("Grace Hopper", "grace@navy.mil")

    "contains matches anywhere in any of its texts, whatever the case" in {
        val f = Filter.contains[FilterTest.User](_.name, _.email)
        assert(f(grace, "HOPP"), "inside the name")
        assert(f(grace, "Navy.M"), "inside the second text")
        assert(!f(ada, "navy"), "a text of another option")
        assert(!f(grace, "zzz"))
    }

    "startsWith matches only the start of a whole text" in {
        val f = Filter.startsWith[FilterTest.User](_.name, _.email)
        assert(f(grace, "gRA"), "the start of the name")
        assert(f(ada, "ADA@"), "the start of the second text")
        assert(!f(grace, "hopper"), "the start of a later word is not the start of the text")
        assert(!f(grace, "race"))
    }

    "startsWithPerTerm matches the start of any word of any text" in {
        val f = Filter.startsWithPerTerm[FilterTest.User](_.name)
        assert(f(grace, "Hop"), "the start of the second word")
        assert(f(grace, "gr"), "the start of the first")
        assert(!f(grace, "opper"), "but not the inside of a word")
        val both = Filter.startsWithPerTerm[FilterTest.User](_.name, _.email)
        assert(both(ada, "ADA@E"), "and any word of the second text")
    }

    "an empty query matches every option, as the panel shows everything before anything is typed" in {
        assert(Filter.contains[FilterTest.User](_.name)(ada, ""))
        assert(Filter.startsWith[FilterTest.User](_.name)(ada, ""))
        assert(Filter.startsWithPerTerm[FilterTest.User](_.name)(ada, ""))
    }

    "all matches everything, for an app that filters the options itself" in {
        val f = Filter.all[FilterTest.User]
        assert(f(ada, "zzz") && f(grace, "") && f(grace, "grace"))
    }

    "the option type comes from the picker the matcher is handed to, with no type argument" in {
        typeCheck("""
            import kyo.*
            import kyo.uic
            final case class User(name: String, email: String)
            def x =
                (
                    uic.Select[User]().options(Seq.empty[User])(_.name).filter(uic.Filter.contains(_.name, _.email)),
                    uic.MultiSelect[User]().options(Seq.empty[User])(_.name).filter(uic.Filter.startsWith(_.name, _.email)),
                    uic.AutoComplete[User]().options(Seq.empty[User])(_.name).filter(uic.Filter.startsWithPerTerm(_.name)),
                    uic.AutoComplete[User]().options(Seq.empty[User])(_.name).filter(uic.Filter.all),
                    uic.Select[User]().filter((u, q) => u.email.endsWith(q))
                )
        """)
    }

    "a matcher over another option type is refused" in {
        typeCheckFailure("""
            import kyo.*
            import kyo.uic
            final case class User(name: String, email: String)
            def x(f: (String, String) => Boolean) = uic.Select[User]().filter(f)
        """)
    }

end FilterTest

object FilterTest:
    final case class User(name: String, email: String) derives CanEqual
end FilterTest
