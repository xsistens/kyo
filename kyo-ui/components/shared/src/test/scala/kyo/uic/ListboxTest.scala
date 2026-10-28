package kyo.uic

import kyo.*

/** `uic.Listbox`'s header filter: on and off through `filterable`, constant or signal, over a query that is kept
  * whichever way it is switched.
  */
class ListboxTest extends UicTest:

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

    // The chosen set lives somewhere else (a route, a parent): the list shows it and hands a click over.
    "a selection bound one way is shown, and a click is reported without being written" in {
        for
            held <- Signal.initRef(Set("b"))
            seen <- Signal.initRef(Set.empty[String])
            ui = fruit.selectionMode(uic.SelectionMode.Multiple).selection(held.readOnly).onChange(seen.set).render
            rows  <- elementsWithClass(ui, "p-listbox-option")
            _     <- click(rows(0))
            h     <- held.get
            s     <- seen.get
            deaf  <- elementsWithClass(fruit.selectionMode(uic.SelectionMode.Multiple).selection(held.readOnly).render, "p-uic-key-error")
            byOld <- elementsWithClass(
                fruit.selectionMode(uic.SelectionMode.Multiple).selection(held.readOnly).onItemClick(_ => ()).render,
                "p-uic-key-error"
            )
        yield
            assert(h == Set("b") && s == Set("a", "b"), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onChange"), s"nobody hears: ${deaf.map(textOf)}")
            assert(byOld.isEmpty, s"onItemClick hears: ${byOld.map(textOf)}")
        end for
    }

    "a two-way selection is written before the list reports it" in {
        for
            sel  <- Signal.initRef(Set.empty[String])
            seen <- Signal.initRef((Set.empty[String], Set.empty[String]))
            ui = fruit.selectionMode(uic.SelectionMode.Multiple).selection(sel).onChange(n => sel.get.map(h => seen.set((n, h)))).render
            rows <- elementsWithClass(ui, "p-listbox-option")
            _    <- click(rows(1))
            s    <- seen.get
        yield assert(s == (Set("b"), Set("b")), s"reported, held: $s")
    }

    "a constant nobody hears is fixed: its options click nothing, and no card" in {
        val fixed = fruit.selectionMode(uic.SelectionMode.Multiple).selection(Set("a"))
        for
            rows <- elementsWithClass(fixed.render, "p-listbox-option")
            card <- elementsWithClass(fixed.render, "p-uic-key-error")
        yield
            assert(rows.nonEmpty && rows.forall(r => !declaresClick(r)), "a fixed list picks nothing")
            assert(card.isEmpty, s"a fixed constant: ${card.map(textOf)}")
        end for
    }

    "an unbound list keeps the options the reader picks" in {
        val free = fruit.selectionMode(uic.SelectionMode.Multiple).metaKeySelection(false)
        for
            after <- publishedAfterReady(
                free.render,
                t => elementsWithClass(t, "p-listbox-option").map(_.exists(declaresClick)),
                until = t => elementsWithClass(t, "p-listbox-option-selected").map(_.exists(e => textOf(e) == "Banana"))
            )(t => clickBubbling(t)((e: UI.Ast.Element) => e.attrs.cssClasses.contains("p-listbox-option") && textOf(e) == "Banana"))
        yield assert(after.nonEmpty, "the list never showed the option the reader picked")
    }

    "a host's fixed list stays fixed through render, whatever the list's own binding says" in {
        for
            ref  <- Signal.initRef(Set("a"))
            rows <- elementsWithClass(fruit.selectionMode(uic.SelectionMode.Multiple).selection(ref).fixed(true).render, "p-listbox-option")
        yield assert(rows.nonEmpty && rows.forall(r => !declaresClick(r)), "the host asked for a fixed list")
    }

end ListboxTest
