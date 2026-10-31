package kyo.uic

import kyo.*

/** [[OrganizationChart]]'s expand toggle follows Prime's anatomy and is an `<a>` with no `href`,
  * which the browser treats as ordinary text: no focus, no Enter. The tab stop is the component's
  * own, so the keys are too. The expanded subtrees and the selection are held to rule A ([[ReactiveValueContract]]).
  */
class OrganizationChartTest extends ReactiveValueContract:

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

    // ---- rule A ----

    private def toggle(t: UI)(using Frame): UI.Ast.Element < Sync =
        toggles(t).map(_.headOption.getOrElse(throw new AssertionError("no toggle")))

    // The toggle is a button and nothing else, so a fixed one is a tab stop that says it changes nothing.
    holds(new Slot[uic.OrganizationChart[String], Set[String]](
        name = "expanded",
        // `expanded()` turns the toggles on over subtrees the chart keeps itself, starting fully open.
        bare = Present(org.expanded()),
        bind = org.expanded(_),
        hear = (o, f) => o.onExpandedChange(f),
        event = "onExpandedChange",
        render = _.render,
        start = Set("ceo"),
        next = Set.empty,
        control = toggle(_),
        shows = toggle(_).map(t => if t.attrs.ariaAttrs.get("expanded").contains("true") then Set("ceo") else Set.empty),
        ready = Present(toggles(_).map(_.exists(declaresClick)))
    ))

    private val idsByLabel = Map("CEO" -> "ceo", "CTO" -> "cto")

    private def single(using Frame) = org.selectionMode(uic.SelectionMode.Single)

    // A node box has no role, so a fixed selection has no words to say it in: it is fixed because no node takes a click.
    holds(new Slot[uic.OrganizationChart[String], Set[String]](
        name = "selection",
        bare = Present(single),
        bind = single.selection(_),
        hear = (o, f) => o.onSelectionChange(f),
        event = "onSelectionChange",
        render = _.render,
        start = Set.empty,
        next = Set("ceo"),
        control = nodes(_).map(_.headOption.getOrElse(throw new AssertionError("no node"))),
        shows = elementsWithClass(_, "p-organizationchart-node-selected").map(_.map(n => idsByLabel(textOf(n))).toSet),
        isFixed = Present(nodes(_).map(ns => ns.nonEmpty && ns.forall(n => !declaresClick(n)))),
        ready = Present(nodes(_).map(_.exists(declaresClick)))
    ))

    "the older per-node events hear the slots too: neither a constant they hear nor a signal is a mistake" in {
        for
            held  <- Signal.initRef(Set("ceo"))
            heard <- elementsWithClass(
                org.expanded(held.readOnly).onNodeToggle(_ => ()).selectionMode(uic.SelectionMode.Single)
                    .selection(held.readOnly).onNodeClick(_ => ()).render,
                "p-uic-key-error"
            )
            live = org.expanded(Set("ceo")).onNodeToggle(_ => ()).selectionMode(uic.SelectionMode.Single).selection(Set("ceo"))
                .onNodeClick(_ => ()).render
            t  <- toggle(live)
            ns <- nodes(live)
        yield
            assert(heard.isEmpty, s"somebody hears both: ${heard.map(textOf)}")
            assert(declaresClick(t) && ns.forall(declaresClick), "a constant somebody hears is not fixed")
        end for
    }

end OrganizationChartTest
