package kyo.uic

import kyo.*

/** [[OrganizationChart]]'s expand toggle follows Prime's anatomy and is an `<a>` with no `href`,
  * which the browser treats as ordinary text: no focus, no Enter. The tab stop is the component's
  * own, so the keys are too.
  */
class OrganizationChartTest extends UicTest:

    private val chart = uic.OrgChartNode("CEO", "ceo", children = List(uic.OrgChartNode("CTO", "cto")))

    private def toggledBy(key: UI.Keyboard)(using Frame): Set[String] < Async =
        for
            expanded <- Signal.initRef(Set("ceo"))
            ui = uic.OrganizationChart().node(chart).expanded(expanded).render
            el    <- elementWithClass(ui, "p-organizationchart-node-toggle-button")
            _     <- press(el, key)
            after <- expanded.get
        yield after

    "Enter collapses an expanded node" in toggledBy(UI.Keyboard.Enter).map(s => assert(!s.contains("ceo")))

    "and so does Space" in toggledBy(UI.Keyboard.Space).map(s => assert(!s.contains("ceo")))

    "a key that is not an activation leaves the node as it was" in
        toggledBy(UI.Keyboard.ArrowRight).map(s => assert(s == Set("ceo")))

    "the toggle says it is a button, since an anchor without an href does not" in {
        for
            expanded <- Signal.initRef(Set("ceo"))
            ui = uic.OrganizationChart().node(chart).expanded(expanded).render
            el <- elementWithClass(ui, "p-organizationchart-node-toggle-button")
        yield
            assert(el.attrs.role.contains("button"))
            assert(el.attrs.tabIndex.contains(0))
    }

end OrganizationChartTest
