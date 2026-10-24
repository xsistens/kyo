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

end ListboxTest
