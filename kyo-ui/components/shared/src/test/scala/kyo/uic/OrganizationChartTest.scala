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
            ui = uic.OrganizationChart[String]().node(chart).expanded(expanded).render
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
            ui = uic.OrganizationChart[String]().node(chart).expanded(expanded).render
            el <- elementWithClass(ui, "p-organizationchart-node-toggle-button")
        yield
            assert(el.attrs.role.contains("button"))
            assert(el.attrs.tabIndex.contains(0))
    }

    private def org(using Frame) = uic.OrganizationChart[String]().node(chart)

    private def toggles(t: UI)(using Frame) = elementsWithClass(t, "p-organizationchart-node-toggle-button")
    private def nodes(t: UI)(using Frame)   = elementsWithClass(t, "p-organizationchart-node")

    "expanded() renders the toggles and keeps the open subtrees itself, starting fully open" in {
        for
            seen <- Signal.initRef(List.empty[Set[String]])
            _    <- publishedAfterReady(
                org.expanded().onExpandedChange(e => seen.updateAndGet(_ :+ e).unit).render,
                t => toggles(t).map(_.exists(declaresClick))
            )(t => toggles(t).map(ts => click(ts.head)))
            s <- seen.get
        yield assert(s == List(Set.empty[String]), s"the first toggle closes the open root: $s")
    }

    "an unbound selection is the chart's own, so a second click takes back the first" in {
        for
            seen <- Signal.initRef(List.empty[Set[String]])
            _    <- publishedAfterReady(
                org.selectionMode(uic.SelectionMode.Single).onSelectionChange(v => seen.updateAndGet(_ :+ v).unit).render,
                t => nodes(t).map(_.exists(declaresClick))
            )(t => nodes(t).map(ns => click(ns.head)), t => nodes(t).map(ns => click(ns.head)))
            s <- seen.get
        yield assert(s == List(Set("ceo"), Set.empty[String]), s"reported $s")
    }

    "constants nobody hears are fixed: toggles and nodes stay, inert" in {
        val fixed = org.expanded(Set("ceo")).selectionMode(uic.SelectionMode.Single).selection(Set("cto"))
        for
            ts   <- toggles(fixed.render)
            ns   <- nodes(fixed.render)
            card <- elementsWithClass(fixed.render, "p-uic-key-error")
        yield
            assert(ts.nonEmpty && ts.forall(t => !declaresClick(t) && t.attrs.ariaAttrs.get("disabled").contains("true")), "inert toggles")
            assert(ns.forall(n => !declaresClick(n)), "inert nodes")
            assert(card.isEmpty, s"fixed states are not mistakes: ${card.map(textOf)}")
        end for
    }

    "bindings one way are followed, and a toggle and a click are reported without being written" in {
        for
            exp  <- Signal.initRef(Set("ceo"))
            sel  <- Signal.initRef(Set.empty[String])
            seen <- Signal.initRef(List.empty[String])
            ui = org.expanded(exp.readOnly).onExpandedChange(e => seen.updateAndGet(_ :+ s"exp $e").unit)
                .selectionMode(uic.SelectionMode.Single).selection(sel.readOnly)
                .onSelectionChange(v => seen.updateAndGet(_ :+ s"sel $v").unit).render
            ts   <- toggles(ui)
            _    <- click(ts.head)
            ns   <- nodes(ui)
            _    <- click(ns.head)
            e    <- exp.get
            v    <- sel.get
            s    <- seen.get
            deaf <- elementsWithClass(
                org.expanded(exp.readOnly).selectionMode(uic.SelectionMode.Single).selection(sel.readOnly).render,
                "p-uic-key-error"
            )
        yield
            assert(e == Set("ceo") && v.isEmpty, s"held $e, $v")
            assert(s == List("exp Set()", "sel Set(ceo)"), s"reported $s")
            assert(deaf.size == 2, s"nobody hears either: ${deaf.map(textOf)}")
        end for
    }

end OrganizationChartTest
