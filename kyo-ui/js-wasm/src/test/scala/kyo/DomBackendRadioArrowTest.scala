package kyo

import kyo.internal.DomBackend
import org.scalajs.dom

/** The SPA client's walk through a radio group that has an inert radio in it ([[DomBackend.radioArrow]]).
  *
  * jsdom has no keyboard walk of its own, so these drive the client's half directly: given the radio an arrow was pressed
  * on, it lands where the browser's walk lands when the inert radios are disabled, and its click is what selects that
  * radio and fires its change. The browser's own half (that it lands on an inert radio and sticks there) is measured in
  * Chrome by `RadioTest`, which holds the server-push twin of this function to the same walk.
  */
class DomBackendRadioArrowTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    /** A group of radios named "g" with the given ids in a fresh container, inert where `inert` says so, and the ids whose
      * change event fired.
      */
    private def group(ids: Seq[String], inert: Set[String], rtl: Boolean = false)
        : (dom.Element, Map[String, dom.html.Input], scala.collection.mutable.ArrayBuffer[String]) =
        val box     = dom.document.createElement("div")
        val changed = scala.collection.mutable.ArrayBuffer.empty[String]
        val radios  = ids.map { id =>
            val r = dom.document.createElement("input").asInstanceOf[dom.html.Input]
            r.`type` = "radio"
            r.name = "g"
            r.id = id
            if inert.contains(id) then r.setAttribute("data-kyo-inert", "1")
            if rtl then r.setAttribute("style", "direction: rtl")
            r.addEventListener("change", (_: dom.Event) => changed += id)
            discard(box.appendChild(r))
            id -> r
        }.toMap
        discard(dom.document.body.appendChild(box))
        (box, radios, changed)
    end group

    "an arrow walks past an inert radio to the next one the reader may select, selecting and reporting it" in {
        val (box, r, changed) = group(Seq("a", "b", "c"), inert = Set("b"))
        r("a").checked = true
        val handled = DomBackend.radioArrow("ArrowDown", r("a"))
        val result  = (handled, r("a").checked, r("b").checked, r("c").checked, dom.document.activeElement eq r("c"), changed.toList)
        box.remove()
        assert(result == (true, false, false, true, true, List("c")), s"handled, a, b, c checked, c focused, changes: $result")
    }

    "an inert radio that is not selected is left for the next one the reader may select" in {
        val (box, r, changed) = group(Seq("a", "b", "c"), inert = Set("b"))
        r("a").checked = true
        val handled = DomBackend.radioArrow("ArrowDown", r("b"))
        val result  = (handled, r("c").checked, dom.document.activeElement eq r("c"), changed.toList)
        box.remove()
        assert(result == (true, true, true, List("c")), s"handled, c checked, c focused, changes: $result")
    }

    "an inert radio that is selected holds the group, and the browser's walk is still declined" in {
        val (box, r, changed) = group(Seq("a", "b", "c"), inert = Set("b"))
        r("b").checked = true
        val handled = DomBackend.radioArrow("ArrowDown", r("b"))
        val result  = (handled, r("b").checked, r("c").checked, changed.toList)
        box.remove()
        assert(result == (true, true, false, List.empty[String]), s"handled, b, c checked, changes: $result")
    }

    "a group with nothing inert is left to the browser" in {
        val (box, r, changed) = group(Seq("a", "b", "c"), inert = Set.empty)
        r("a").checked = true
        val handled = DomBackend.radioArrow("ArrowDown", r("a"))
        val result  = (handled, r("a").checked, changed.toList)
        box.remove()
        assert(result == (false, true, List.empty[String]), s"handled, a checked, changes: $result")
    }

    "in a right-to-left group Right walks back past an inert radio" in {
        val (box, r, changed) = group(Seq("a", "b", "c", "e"), inert = Set("b"), rtl = true)
        r("c").checked = true
        val handled = DomBackend.radioArrow("ArrowRight", r("c"))
        val result  = (handled, r("a").checked, changed.toList)
        box.remove()
        assert(result == (true, true, List("a")), s"handled, a checked, changes: $result")
    }

end DomBackendRadioArrowTest
