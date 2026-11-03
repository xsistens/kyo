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
    private def expandedCase(
        name: String,
        hear: (uic.OrganizationChart[String], Set[String] => UI.Action) => Frame ?=> uic.OrganizationChart[String]
    ) = new SlotCase[uic.OrganizationChart[String], Set[String]](
        name = name,
        // `expanded()` turns the toggles on over subtrees the chart keeps itself, starting fully open.
        bare = Present(org.expanded()),
        bind = org.expanded(_),
        hear = hear,
        event = "onExpandedChange",
        render = _.render,
        start = Set("ceo"),
        next = Set.empty,
        control = toggle(_),
        shows = toggle(_).map(t => if t.attrs.ariaAttrs.get("expanded").contains("true") then Set("ceo") else Set.empty),
        ready = Present(toggles(_).map(_.exists(declaresClick)))
    )

    holds(expandedCase("expanded", (o, f) => o.onExpandedChange(f)))
    heardBy(expandedCase("expanded, heard by onNodeToggle", (o, f) => o.onNodeToggle(_ => f(Set.empty))))

    private val idsByLabel = Map("CEO" -> "ceo", "CTO" -> "cto")

    private def single(using Frame) = org.selectionMode(uic.SelectionMode.Single)

    // A node box has no role, so a fixed selection has no words to say it in: it is fixed because no node takes a click.
    private def selectionCase(
        name: String,
        hear: (uic.OrganizationChart[String], Set[String] => UI.Action) => Frame ?=> uic.OrganizationChart[String]
    ) = new SlotCase[uic.OrganizationChart[String], Set[String]](
        name = name,
        bare = Present(single),
        bind = single.selection(_),
        hear = hear,
        event = "onSelectionChange",
        render = _.render,
        start = Set.empty,
        next = Set("ceo"),
        control = nodes(_).map(_.headOption.getOrElse(throw new AssertionError("no node"))),
        shows = elementsWithClass(_, "p-organizationchart-node-selected").map(_.map(n => idsByLabel(textOf(n))).toSet),
        isFixed = Present(nodes(_).map(ns => ns.nonEmpty && ns.forall(n => !declaresClick(n)))),
        ready = Present(nodes(_).map(_.exists(declaresClick)))
    )

    holds(selectionCase("selection", (o, f) => o.onSelectionChange(f)))
    heardBy(selectionCase("selection, heard by onNodeClick", (o, f) => o.onNodeClick(k => f(Set(k)))))

    // ---- the node cursor ----
    //
    // A selectable chart is ONE tab stop, on the node the cursor is on, and the arrows move it the way the chart is drawn:
    // down to the first child, up to the parent, sideways to a sibling. The semantics are OrgChartNav's, so only the
    // wiring is asserted here.

    private val team = uic.OrgChartNode(
        "CEO",
        "ceo",
        children = List(
            uic.OrgChartNode("CFO", "cfo"),
            uic.OrgChartNode("CTO", "cto", children = List(uic.OrgChartNode("Dev", "dev")))
        )
    )

    private val labelOf = Map("CEO" -> "ceo", "CFO" -> "cfo", "CTO" -> "cto", "Dev" -> "dev")

    private def teamChart(using Frame) = uic.OrganizationChart[String]().node(team).selectionMode(uic.SelectionMode.Single)

    /** The node boxes that are tab stops, by id. */
    private def stopsIn(t: UI)(using Frame): Chunk[String] < Sync =
        nodes(t).map(_.filter(_.attrs.tabIndex.contains(0)).map(n => labelOf(textOf(n))))

    /** Presses `key` on the node box `at` once the mount has wired the keys, and answers where the tab stop went. */
    private def stopAfter(ui: UI, at: String, key: UI.Keyboard)(using Frame, kyo.test.AssertScope): Chunk[String] < Async =
        publishedAfterReady(
            ui,
            t => nodes(t).map(_.exists(_.attrs.onKeyDown.isDefined)),
            until = t => stopsIn(t).map(s => s.nonEmpty && !s.contains(at))
        )(t => nodes(t).map(ns => press(ns.find(n => labelOf(textOf(n)) == at).get, key)))
            .map(_.fold(Kyo.lift(Chunk.empty[String]))(stopsIn))

    "the page as first rendered is one tab stop, on the root, where the attached chart puts it" in {
        for
            stops  <- stopsIn(teamChart.render)
            chosen <- stopsIn(teamChart.selection(Set("cto")).onSelectionChange(_ => ()).render)
        yield
            assert(stops == Chunk("ceo"), s"one stop, on the root: $stops")
            assert(chosen == Chunk("cto"), s"or on the chosen node: $chosen")
        end for
    }

    "ArrowDown moves the tab stop to the first child, ArrowRight to the next sibling" in {
        for
            down  <- stopAfter(teamChart.render, "ceo", UI.Keyboard.ArrowDown)
            right <- stopAfter(teamChart.selection(Set("cfo")).onSelectionChange(_ => ()).render, "cfo", UI.Keyboard.ArrowRight)
        yield
            assert(down == Chunk("cfo"), s"down to the first child: $down")
            assert(right == Chunk("cto"), s"right to the next sibling: $right")
        end for
    }

    "ArrowUp moves the tab stop to the parent" in {
        for up <- stopAfter(teamChart.selection(Set("dev")).onSelectionChange(_ => ()).render, "dev", UI.Keyboard.ArrowUp)
        yield assert(up == Chunk("cto"), s"up to the parent: $up")
    }

    "Enter and Space select the node they are pressed on" in {
        for
            seen <- Signal.initRef(List.empty[Set[String]])
            _    <- actedOn(
                teamChart.onSelectionChange(v => seen.updateAndGet(_ :+ v).unit).render,
                t => nodes(t).map(_.exists(_.attrs.onKeyDown.isDefined))
            )(
                t => nodes(t).map(ns => press(ns.head, UI.Keyboard.Enter)),
                t => nodes(t).map(ns => press(ns(1), UI.Keyboard.Space))
            )
            s <- seen.get
        yield assert(s == List(Set("ceo"), Set("cfo")), s"reported $s")
    }

    "a click seeds the cursor, so the keyboard carries on from the node the pointer chose" in {
        for
            after <- publishedAfterReady(
                teamChart.render,
                t => nodes(t).map(_.exists(_.attrs.onKeyDown.isDefined)),
                until = t => stopsIn(t).map(_ == Chunk("dev"))
            )(t => nodes(t).map(ns => click(ns.find(n => textOf(n) == "Dev").get)))
        yield assert(after.nonEmpty, "the tab stop never followed the click")
    }

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
