package kyo.uic

import kyo.*

/** `uic.Listbox`'s header filter: on and off through `filterable`, constant or signal, over a query that is kept
  * whichever way it is switched. Its selection and its query are held to rule A ([[ReactiveValueContract]]).
  */
class ListboxTest extends ReactiveValueContract:

    private def fruit(using Frame) = uic.Listbox[String]().item("Apple", "a").item("Banana", "b").item("Pineapple", "p")

    private def shownTexts(ui: UI)(using Frame): Chunk[String] < Sync =
        def walk(u: UI): String = u match
            case t: UI.Ast.Text     => t.value
            case el: UI.Ast.Element => el.children.map(walk).mkString
            case _                  => ""
        elementsWithClass(ui, "p-listbox-option").map(_.map(e => e.children.map(walk).mkString))
    end shownTexts

    private def hasHeader(ui: UI)(using Frame): Boolean < Sync = elementsWithClass(ui, "p-listbox-header").map(_.nonEmpty)

    "filterable(true) renders the header and filters by contains on the label" in {
        for
            q <- Signal.initRef("APP")
            ui = fruit.filterQuery(q).filterable(true).render
            header <- hasHeader(ui)
            shown  <- shownTexts(ui)
        yield assert(header && shown == Chunk("Apple", "Pineapple"), s"header=$header shown=$shown")
    }

    "an explicit filterable(false) wins over filterQuery: no header, every option" in {
        for
            q <- Signal.initRef("app")
            ui = fruit.filterQuery(q).filterable(false).render
            header <- hasHeader(ui)
            shown  <- shownTexts(ui)
        yield assert(!header && shown == Chunk("Apple", "Banana", "Pineapple"), s"header=$header shown=$shown")
    }

    "a filterable signal shows and hides the header, filters only while on, and keeps the query" in {
        for
            q  <- Signal.initRef("app")
            on <- Signal.initRef(true)
            ui = fruit.filterQuery(q).filterable(on).render
            headerOn    <- hasHeader(ui)
            shownOn     <- shownTexts(ui)
            _           <- on.set(false)
            headerOff   <- hasHeader(ui)
            shownOff    <- shownTexts(ui)
            kept        <- q.get
            _           <- on.set(true)
            headerAgain <- hasHeader(ui)
            shownAgain  <- shownTexts(ui)
        yield
            assert(headerOn && shownOn == Chunk("Apple", "Pineapple"), s"on: header=$headerOn shown=$shownOn")
            assert(!headerOff && shownOff == Chunk("Apple", "Banana", "Pineapple"), s"off: header=$headerOff shown=$shownOff")
            assert(kept == "app", s"the query is kept, only not applied: $kept")
            assert(headerAgain && shownAgain == Chunk("Apple", "Pineapple"), s"back on, the old query filters at once: $shownAgain")
    }

    "the header comes and goes in a region of its own inside the listbox, not around it" in {
        for
            q  <- Signal.initRef("")
            on <- Signal.initRef(true)
            live  = fruit.filterQuery(q).filterable(on).render
            fixed = fruit.filterQuery(q).filterable(true).render
            root   <- regionsAbove(live, "p-listbox")
            fixedR <- regionsAbove(fixed, "p-listbox")
            header <- regionsAbove(live, "p-listbox-header")
        yield
            assert(root == fixedR, s"the switch puts no region over the listbox: $root, a constant has $fixedR")
            assert(header == root + 1, s"the header is one region below the listbox root: $header vs $root")
    }

    "the selection and the query are ONE region over the list" in {
        for
            q   <- Signal.initRef("")
            sel <- Signal.initRef(Set("a"))
            ui = fruit.filterQuery(q).selection(sel).render
            regions <- regionsAbove(ui, "p-listbox")
        yield assert(
            regions == 1,
            s"a row click writes the highlight and the selection; nested regions can repaint the older state: $regions"
        )
    }

    // ---- rule A ----

    private def multiple(using Frame) = fruit.selectionMode(uic.SelectionMode.Multiple).metaKeySelection(false)

    private def rows(t: UI)(using Frame): Chunk[UI.Ast.Element] < Sync = elementsWithClass(t, "p-listbox-option")

    private def row(text: String)(t: UI)(using Frame): UI.Ast.Element < Sync =
        rows(t).map(_.find(textOf(_) == text).getOrElse(throw new AssertionError(s"no row $text")))

    private val ids = Map("Apple" -> "a", "Banana" -> "b", "Pineapple" -> "p")

    /** The ids of the rows the list draws selected. */
    private def chosen(t: UI)(using Frame): Set[String] < Sync =
        rows(t).map(_.filter(_.attrs.ariaAttrs.get("selected").contains("true")).map(r => ids(textOf(r))).toSet)

    // The row is what a reader clicks; the list is the one tab stop, and it is what says the selection is fixed.
    private def selectionCase(name: String, hear: (uic.Listbox[String], Set[String] => UI.Action) => Frame ?=> uic.Listbox[String]) =
        new SlotCase[uic.Listbox[String], Set[String]](
            name = name,
            bare = Present(multiple),
            bind = multiple.selection(_),
            hear = hear,
            event = "onChange",
            render = _.render,
            start = Set.empty,
            next = Set("b"),
            control = row("Banana")(_),
            shows = chosen(_),
            saysFixed = _.attrs.ariaAttrs.get("readonly").contains("true"),
            ready = Present(rows(_).map(_.exists(declaresClick))),
            fixedOn = Present(elementWithClass(_, "p-listbox-list"))
        )

    holds(selectionCase("selection", (l, f) => l.onChange(f)))
    heardBy(selectionCase("selection, heard by onItemClick", (l, f) => l.onItemClick(k => f(Set(k)))))

    "onItemClick hears a selection bound one way, so it draws no card" in {
        for
            held  <- Signal.initRef(Set("b"))
            byOld <- elementsWithClass(multiple.selection(held.readOnly).onItemClick(_ => ()).render, "p-uic-diagnostic")
        yield assert(byOld.isEmpty, s"onItemClick hears: ${byOld.map(textOf)}")
    }

    "a fixed list holds every row to it, not only the one a gesture reaches" in {
        for rs <- rows(multiple.selection(Set("a")).render)
        yield assert(rs.size == 3 && rs.forall(r => !declaresClick(r)), "a fixed list picks nothing")
    }

    "a fixed list stays reachable: the list keeps its tab stop" in {
        for
            static <- elementWithClass(multiple.selection(Set("a")).render, "p-listbox-list")
            live   <- mountedWhen(multiple.selection(Set("a")).render)(t => elementsWithClass(t, "p-listbox-list").map(_.nonEmpty))
            list   <- elementWithClass(live, "p-listbox-list")
        yield
            assert(static.attrs.tabIndex.contains(0), "the page as first rendered offers the list to Tab")
            assert(list.attrs.tabIndex.contains(0), "and so does the list the mount publishes")
        end for
    }

    "a host's fixed list stays fixed through render, whatever the list's own binding says" in {
        for
            ref  <- Signal.initRef(Set("a"))
            rows <- elementsWithClass(fruit.selectionMode(uic.SelectionMode.Multiple).selection(ref).fixed(true).render, "p-listbox-option")
        yield assert(rows.nonEmpty && rows.forall(r => !declaresClick(r)), "the host asked for a fixed list")
    }

    /** The listbox as its mount publishes it. */
    private def mounted(ui: UI)(using Frame, kyo.test.AssertScope): UI < Async =
        mountedWhen(ui)(t => elementsWithClass(t, "p-listbox-filter").map(_.nonEmpty))

    /** Whether the filter field in `t` is the one the mount wired: the page as first rendered draws it with no typing. */
    private def filterWired(t: UI)(using Frame): Boolean < Sync =
        elementsWithClass(t, "p-listbox-filter").map(_.exists {
            case f: UI.Ast.TextInput => f.onInput.isDefined
            case _                   => false
        })

    // The query is a text the list keeps itself, unbound and from a default, since it filters the rows by it; the page as
    // first rendered draws the field with nothing in it, so the cases read what the mount published.
    holds(new SlotCase[uic.Listbox[String], String](
        name = "filterQuery",
        bare = Present(fruit.filterable(true)),
        bind = fruit.filterQuery(_),
        hear = (l, f) => l.onFilterQueryChange(f),
        event = "onFilterQueryChange",
        render = _.render,
        start = "",
        next = "pine",
        control = textField(_, "p-listbox-filter"),
        shows = t => textField(t, "p-listbox-filter").map(textIn),
        gesture = (c: UI.Ast.Element) => typed(c, "pine", commit = false),
        answers = editable,
        saysFixed = {
            case t: UI.Ast.TextInput => t.readOnly.contains(true)
            case _                   => false
        },
        ready = Present(filterWired(_)),
        startText = Present("apple"),
        defaults = Present((l, q) => l.defaultFilterQuery(q)),
        mountDraws = true
    ))

    "a query bound one way filters the list by what the signal holds" in {
        for
            held  <- Signal.initRef("an")
            t     <- mounted(fruit.filterQuery(held.readOnly).onFilterQueryChange(_ => ()).render)
            shown <- shownTexts(t)
        yield assert(shown == Chunk("Banana"), s"the list filters by the signal: $shown")
    }

    "a default query is the starting text" in {
        for
            t     <- mounted(fruit.defaultFilterQuery("apple").render)
            shown <- shownTexts(t)
        yield assert(shown == Chunk("Apple", "Pineapple"), s"the list starts filtered: $shown")
    }

    // A constant is a binding (UI.Bound): the field shows it, and kyo-ui puts back what the reader types over it.
    "a constant query holds the list filtered by it, the filter field bound to the constant" in {
        for
            t     <- mounted(fruit.filterQuery("apple").onFilterQueryChange(_ => ()).render)
            shown <- shownTexts(t)
            field <- textField(t, "p-listbox-filter")
        yield
            assert(shown == Chunk("Apple", "Pineapple"), s"the list filters by the constant: $shown")
            field match
                case f: UI.Ast.TextInput =>
                    assert(f.value.exists(_.isInstanceOf[UI.Bound.Const[?]]), s"the filter field is bound as ${f.value}")
                case other => fail(s"not a text field: $other")
            end match
        end for
    }

    // A reader who cannot act has nothing to hear: a disabled list takes no query.
    "a disabled list says nothing about a query bound one way" in {
        for
            query <- Signal.initRef("")
            said  <- elementsWithClass(fruit.disabled(true).filterQuery(query.readOnly).render, UicTest.cardClass)
        yield assert(said.isEmpty, s"${said.map(textOf)}")
    }

end ListboxTest
