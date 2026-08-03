package kyo

import kyo.Browser.*
import kyo.UI.foreachKeyed
import scala.language.implicitConversions

/** Chrome-backed regressions for a keyed list under the structural list command, over the SERVER transport.
  *
  * `withUI` serves the app through `UI.runHandlers`, so what these drive is the real socket and the real
  * client script: `UIServer.onListPatch` sends `HtmlOp.PatchList`, and the page reconciles it in
  * `__kyoApplyList`. That client half is a hand-written JavaScript twin of `DomBackend.applyListPatchIn` living
  * inside a string constant, so no compiler checks a word of it and this suite is what does. (No-oping
  * `__kyoApplyList` fails eight of the nine leaves below; the ninth is the fallback case.) `DomBackend`'s own
  * override is Scala.js-only and has no DOM harness in this build — the browser bundle's smoke run covers it.
  *
  * Every assert is about placement — which rows, in what order, how many. A test that only checked presence
  * would pass on a list that reconciled into the wrong sequence, which is the failure this path can produce.
  *
  * [[ListPatchProtocolTest]] covers the command the region emits and [[ListPatchWireTest]] what reaches the
  * socket; this covers what the client makes of it.
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

    "a moved row keeps focus AND the caret inside it" in {
        // The assumption this catches out: "a retained row cannot lose the caret, so every pass may skip it".
        // True for a row left in place, FALSE for one moved — insertBefore on a live node is a remove and an
        // insert, and that blurs whatever it holds. A pure reorder disturbs no row at all, which makes it
        // exactly the case a focus capture scoped to the disturbed rows gets wrong.
        val ui: UI < Async =
            for rows <- Signal.initRef(Chunk("a", "b", "c"))
            yield UI.div(
                UI.ul(rows.foreachKeyed(identity)(item => UI.li(UI.input.id(s"i-$item")))),
                UI.button("go").id("go").onClick(rows.set(Chunk("c", "b", "a")))
            )
        withUI(ui) {
            for
                _ <- Browser.fill(Selector.id("i-b"), "hello")
                _ <- Browser.evalDiscard("document.getElementById('i-b').focus();document.getElementById('i-b').setSelectionRange(2,2)")
                // The reorder is triggered through `.click()` rather than a real mouse click, which would
                // move focus to the button first and leave nothing in the region to preserve.
                _     <- Browser.evalDiscard("document.getElementById('go').click()")
                _     <- Browser.assertFocused(Selector.id("i-b"))
                caret <- Browser.evalInt("document.getElementById('i-b').selectionStart")
            yield assert(caret == 2, s"caret landed at $caret")
        }
    }

    "rows that are regions rather than elements move as whole spans" in {
        // A row whose root is a reactive region owns no element of its own: it paints a marker span, is keyed
        // by its region path, and has to be moved marker-and-content together. Bumping the inner signal
        // AFTERWARDS is the second half of the test — a span that lost its registry entry on the way past
        // would sit there silently ignoring its own updates.
        val ui: UI < Async =
            for
                rows <- Signal.initRef(Chunk("a", "b", "c"))
                n    <- Signal.initRef(1)
            yield UI.div(
                UI.div(rows.foreachKeyed(identity)(item => (n.map(v => UI.span(s"$item$v").cssClass("row"): UI): UI))),
                UI.button("go").id("go").onClick(rows.set(Chunk("c", "a"))),
                UI.button("bump").id("bump").onClick(n.set(2))
            )
        withUI(ui) {
            for
                _ <- Browser.click(Selector.id("go"))
                _ <- Browser.assertCount(Selector.css("span.row"), 2)
                _ <- Browser.assertPageTextOrder(Seq("c1", "a1"))
                _ <- Browser.click(Selector.id("bump"))
                _ <- Browser.assertPageTextOrder(Seq("c2", "a2"))
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
