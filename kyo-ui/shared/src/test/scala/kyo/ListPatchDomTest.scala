package kyo

import kyo.Browser.*
import kyo.UI.foreachKeyed
import scala.language.implicitConversions

/** Chrome-backed regressions for a keyed list under the structural list command, over the SERVER transport.
  *
  * `withUI` serves the app through `UI.runHandlers`, so what runs here is `UIExchange`'s DEFAULT
  * `onListPatch` — the one that reassembles the whole fragment and hands it to `onChange`. That is the point:
  * introducing a structural command must leave every transport that does not implement it painting exactly
  * what it painted before, and this is the suite that says so. `DomBackend`'s own override is Scala.js-only
  * and has no DOM harness in this build; it is covered by the browser bundle's smoke run.
  *
  * Every assert is about placement — which rows, in what order, how many. A test that only checked presence
  * would pass on a list that reconciled into the wrong sequence, which is the failure this path can produce.
  *
  * [[ListPatchProtocolTest]] covers the command the region emits; this covers painting it unchanged.
  */
class ListPatchDomTest extends UITest:

    /** A list plus one button per mutation, so a test drives the region the way an app does — through a real
      * signal write — instead of reaching into the engine.
      */
    private def app(initial: Seq[String], mutations: (String, Seq[String])*): UI < Async =
        for rows <- Signal.initRef(Chunk.from(initial))
        yield UI.div(
            UI.ul(rows.foreachKeyed(identity)(item => UI.li(item).cssClass("row"))),
            UI.div(mutations.map((id, next) => UI.button(id).id(id).onClick(rows.set(Chunk.from(next))))*)
        )

    "removing from the middle keeps the survivors in order" in {
        withUI(app(Seq("a", "b", "c", "d"), "go" -> Seq("a", "b", "d"))) {
            for
                _ <- Browser.assertCount(Selector.css("li.row"), 4)
                _ <- Browser.click(Selector.id("go"))
                _ <- Browser.assertCount(Selector.css("li.row"), 3)
                _ <- Browser.assertPageTextOrder(Seq("a", "b", "d"))
            yield succeed
        }
    }

    "swapping the ends moves only those two" in {
        withUI(app(Seq("a", "b", "c", "d", "e"), "go" -> Seq("e", "b", "c", "d", "a"))) {
            for
                _ <- Browser.click(Selector.id("go"))
                _ <- Browser.assertCount(Selector.css("li.row"), 5)
                _ <- Browser.assertPageTextOrder(Seq("e", "b", "c", "d", "a"))
            yield succeed
        }
    }

    "moving the last row to the front" in {
        // The case a forward-only cursor gets wrong: the key it wants sits BEHIND the cursor, so a walk that
        // can only insert in front of itself drags every row in between along with it.
        withUI(app(Seq("a", "b", "c", "d"), "go" -> Seq("d", "a", "b", "c"))) {
            for
                _ <- Browser.click(Selector.id("go"))
                _ <- Browser.assertPageTextOrder(Seq("d", "a", "b", "c"))
                _ <- Browser.assertCount(Selector.css("li.row"), 4)
            yield succeed
        }
    }

    "inserting into the middle lands between its neighbours" in {
        withUI(app(Seq("a", "b", "e"), "go" -> Seq("a", "b", "c", "d", "e"))) {
            for
                _ <- Browser.click(Selector.id("go"))
                _ <- Browser.assertCount(Selector.css("li.row"), 5)
                _ <- Browser.assertPageTextOrder(Seq("a", "b", "c", "d", "e"))
            yield succeed
        }
    }

    "replacing every key leaves no stale row behind" in {
        // Nothing is retained here, so every live row must fall out as a leftover; a placement pass that
        // forgets the trailing removal would leave the old list appended to the new one.
        withUI(app(Seq("a", "b", "c"), "go" -> Seq("x", "y"))) {
            for
                _ <- Browser.click(Selector.id("go"))
                _ <- Browser.assertCount(Selector.css("li.row"), 2)
                _ <- Browser.assertPageTextOrder(Seq("x", "y"))
            yield succeed
        }
    }

    "clearing and refilling rebuilds the list" in {
        withUI(app(Seq("a", "b", "c"), "clear" -> Seq.empty, "fill" -> Seq("p", "q"))) {
            for
                _ <- Browser.click(Selector.id("clear"))
                _ <- Browser.assertCount(Selector.css("li.row"), 0)
                _ <- Browser.click(Selector.id("fill"))
                _ <- Browser.assertCount(Selector.css("li.row"), 2)
                _ <- Browser.assertPageTextOrder(Seq("p", "q"))
            yield succeed
        }
    }

    "a retained row keeps the class its own channel patched in" in {
        // The reason retained rows may be skipped at all: their DOM is already correct, INCLUDING in-place
        // channel patches that were never part of any rendered payload. Repainting them would revert this.
        val ui: UI < Async =
            for
                rows     <- Signal.initRef(Chunk("a", "b", "c"))
                selected <- Signal.initRef("")
            yield UI.div(
                UI.ul(rows.foreachKeyed(identity)(item => UI.li(item).cssClass("hot", selected.map(_ == item)))),
                UI.button("sel").id("sel").onClick(selected.set("a")),
                UI.button("drop").id("drop").onClick(rows.set(Chunk("a", "c")))
            )
        withUI(ui) {
            for
                _ <- Browser.click(Selector.id("sel"))
                _ <- Browser.assertCount(Selector.css("li.hot"), 1)
                _ <- Browser.click(Selector.id("drop"))
                _ <- Browser.assertCount(Selector.css("li"), 2)
                _ <- Browser.assertCount(Selector.css("li.hot"), 1)
            yield succeed
        }
    }

    "a row painting several roots still reconciles" in {
        // Such a row owns no single node that a key could name — its roots paint under sub-paths — so a
        // key-addressed backend has to recognise that and repaint wholesale. The region must keep emitting a
        // well-formed command either way, which is what breaks first if the row shape is assumed.
        val ui: UI < Async =
            for rows <- Signal.initRef(Chunk("a", "b", "c"))
            yield UI.div(
                UI.div(rows.foreachKeyed(identity)(item => UI.fragment(UI.span(item).cssClass("l"), UI.span(item + "!").cssClass("r")))),
                UI.button("go").id("go").onClick(rows.set(Chunk("a", "c")))
            )
        withUI(ui) {
            for
                _ <- Browser.assertCount(Selector.css("span.l"), 3)
                _ <- Browser.click(Selector.id("go"))
                _ <- Browser.assertCount(Selector.css("span.l"), 2)
                _ <- Browser.assertCount(Selector.css("span.r"), 2)
                _ <- Browser.assertPageTextOrder(Seq("a", "a!", "c", "c!"))
            yield succeed
        }
    }

    "a row whose value changed repaints while its neighbours are left alone" in {
        val ui: UI < Async =
            for rows <- Signal.initRef(Chunk(("1", "one"), ("2", "two"), ("3", "three")))
            yield UI.div(
                UI.ul(rows.foreachKeyed(_._1)((_, label) => UI.li(label).cssClass("row"))),
                UI.button("go").id("go").onClick(rows.set(Chunk(("1", "one"), ("2", "TWO"), ("3", "three"))))
            )
        withUI(ui) {
            for
                _ <- Browser.click(Selector.id("go"))
                _ <- Browser.assertCount(Selector.css("li.row"), 3)
                _ <- Browser.assertPageTextOrder(Seq("one", "TWO", "three"))
            yield succeed
        }
    }

end ListPatchDomTest
