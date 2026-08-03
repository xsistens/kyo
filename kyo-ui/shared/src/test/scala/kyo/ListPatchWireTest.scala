package kyo

import kyo.Browser.*
import kyo.UI.foreachKeyed
import kyo.internal.HtmlOp
import scala.language.implicitConversions

/** What a keyed list emission actually puts ON THE WIRE, read off a real socket in a real Chrome.
  *
  * [[ListPatchDomTest]] proves the rows land in the right places. It cannot prove which command put them
  * there: a structural patch and a whole-list replace paint identical DOM by design, so a gate that quietly
  * stopped emitting the command would leave every one of those assertions green while the transport went
  * back to sending the entire rendered list for a one-row change. That difference is only visible in the
  * frame, which is what this reads.
  *
  * The frames are captured by `UITest.captureFrames`.
  *
  * `HttpWebSocket.connect` would be the direct route (see [[UIServerWsTest]]), but its three behavior leaves
  * hang indefinitely on this build, so the frames are read through the transport that does work end to end.
  */
class ListPatchWireTest extends UITest:

    /** Enough rows that a whole-list replace could not be mistaken for a small frame. */
    private val rowCount = 200

    private val keys: Seq[String] = (0 until rowCount).map(i => s"k$i")

    private def app(mutations: (String, Seq[String])*): UI < Async =
        for rows <- Signal.initRef(Chunk.from(keys))
        yield UI.div(
            UI.ul(rows.foreachKeyed(identity)(item => UI.li(item).cssClass("row"))),
            UI.div(mutations.map((id, next) => UI.button(id).id(id).onClick(rows.set(Chunk.from(next))))*)
        )

    "removing one row of two hundred renders no row at all" in {
        withUI(app("go" -> keys.filterNot(_ == "k100"))) {
            for
                _ <- Browser.evalDiscard(captureFrames)
                _ <- Browser.click(Selector.id("go"))
                // Waits for the patch to land, so the frame below is the one that carried it.
                _  <- Browser.assertCount(Selector.css("li.row"), rowCount - 1)
                op <- lastOp
            yield op match
                case HtmlOp.PatchList(_, ks, changed, html) =>
                    // The whole point: the order travels, the rows do not. A Replace here would carry 199
                    // rendered <li> elements to say that one row left.
                    assert(ks.size == rowCount - 1, s"expected ${rowCount - 1} keys, got ${ks.size}")
                    assert(!ks.contains("k100"), "the removed row is still in the order")
                    assert(changed.isEmpty, s"nothing was repainted, but $changed is flagged")
                    assert(html.isEmpty, s"expected an empty payload, got ${html.length} chars")
                case other => fail(s"expected a structural list patch, got $other")
        }
    }

    "appending renders exactly the appended row" in {
        withUI(app("go" -> (keys :+ "kNew"))) {
            for
                _  <- Browser.evalDiscard(captureFrames)
                _  <- Browser.click(Selector.id("go"))
                _  <- Browser.assertCount(Selector.css("li.row"), rowCount + 1)
                op <- lastOp
            yield op match
                case HtmlOp.PatchList(_, ks, changed, html) =>
                    assert(ks.size == rowCount + 1 && ks.last == "kNew", s"order ends ${ks.takeRight(2)}")
                    assert(changed == Seq("kNew"), s"expected only the new row flagged, got $changed")
                    // One row rendered, not two hundred and one. Counting the opening tags says so directly.
                    assert(html.sliding(3).count(_ == "<li") == 1, s"payload carries more than one row: $html")
                case other => fail(s"expected a structural list patch, got $other")
        }
    }

    "reordering moves rows without rendering any" in {
        withUI(app("go" -> keys.reverse)) {
            for
                _  <- Browser.evalDiscard(captureFrames)
                _  <- Browser.click(Selector.id("go"))
                _  <- Browser.assertPageTextOrder(Seq("k199", "k198", "k197"))
                op <- lastOp
            yield op match
                case HtmlOp.PatchList(_, ks, changed, html) =>
                    assert(ks == keys.reverse, s"order came back as ${ks.take(3)}")
                    assert(changed.isEmpty && html.isEmpty, s"a reorder rendered ${html.length} chars")
                case other => fail(s"expected a structural list patch, got $other")
        }
    }

    "a row that no key can name travels as a whole-list replace" in {
        // The shape gate. Such a row paints its roots under sub-paths, so a key-addressed frame could not
        // describe it — and unlike the local backend, this transport cannot discover that after the fact and
        // re-send: the rows it left out are not on the client to fall back to.
        val ui: UI < Async =
            for rows <- Signal.initRef(Chunk("a", "b", "c"))
            yield UI.div(
                UI.div(rows.foreachKeyed(identity)(item => UI.fragment(UI.span(item).cssClass("l"), UI.span(item + "!")))),
                UI.button("go").id("go").onClick(rows.set(Chunk("a", "c")))
            )
        withUI(ui) {
            for
                _  <- Browser.evalDiscard(captureFrames)
                _  <- Browser.click(Selector.id("go"))
                _  <- Browser.assertCount(Selector.css("span.l"), 2)
                op <- lastOp
            yield op match
                case _: HtmlOp.Replace => succeed
                case other             => fail(s"expected the whole-list replace, got $other")
        }
    }

end ListPatchWireTest
