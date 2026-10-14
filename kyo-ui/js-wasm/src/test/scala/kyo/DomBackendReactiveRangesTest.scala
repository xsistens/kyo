package kyo

import kyo.UI.foreach
import kyo.UI.foreachKeyed
import kyo.internal.DomBackend
import org.scalajs.dom
import scala.scalajs.js as scalajs

class DomBackendReactiveRangesTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    private def click(id: String)(using Frame): Unit < Sync =
        Sync.defer {
            val event = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].MouseEvent)(
                "click",
                scalajs.Dynamic.literal(bubbles = true)
            )
            discard(dom.document.getElementById(id).asInstanceOf[scalajs.Dynamic].dispatchEvent(event))
        }

    "local mount preserves direct table row topology through events and repeated updates" in {
        for
            rows <- Signal.initRef(Chunk("A"))
            ui = UI.div(
                UI.table(
                    UI.tr(UI.td("before").id("before")),
                    rows.foreach(value => UI.tr(UI.td(value).id(s"row-$value"))),
                    UI.tr(UI.td("after").id("after"))
                ).id("table"),
                UI.button("add").id("add").onClick(rows.getAndUpdate(_ :+ "B").unit),
                UI.button("clear").id("clear").onClick(rows.set(Chunk.empty))
            )
            ready = new DomTestEnv.MountReady
            fiber   <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _       <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("row-A") != null))
            initial <- Sync.defer(topology)
            _       <- click("add")
            _       <- assertEventually(Sync.defer(dom.document.getElementById("row-B") != null))
            updated <- Sync.defer(topology)
            _       <- click("clear")
            _       <- assertEventually(Sync.defer(dom.document.getElementById("row-A") == null))
            cleared <- Sync.defer(topology)
            _       <- fiber.interrupt
            _       <- fiber.getResult
        yield
            assert(initial.rows == 1)
            assert(initial.groups == 3)
            assert(initial.spans == 0)
            assert(initial.anchorParent == "TBODY")
            assert(!initial.staticCaptured)
            assert(updated.rows == 2)
            assert(updated.groups == 3)
            assert(updated.spans == 0)
            assert(updated.anchorParent == "TBODY")
            assert(!updated.staticCaptured)
            assert(cleared.rows == 0)
            assert(cleared.groups == 2)
            assert(cleared.spans == 0)
            assert(cleared.anchorParent == "TABLE")
            assert(!cleared.staticCaptured)
        end for
    }

    "local nested table ranges leave the row host to the content boundary" in {
        for
            outer     <- Signal.initRef(true)
            sectioned <- Signal.initRef(false)
            content = outer.map(_ =>
                (sectioned.map { authored =>
                    if authored then UI.tbody(UI.tr(UI.td("authored"))).id("local-authored").data("state", "kept"): UI
                    else UI.tr(UI.td("row").id("local-row")): UI
                }: UI)
            )
            ui = UI.div(
                UI.table(content).id("local-nested-table"),
                UI.button("section").id("local-section").onClick(sectioned.set(true)),
                UI.button("row").id("local-return-row").onClick(sectioned.set(false))
            )
            ready = new DomTestEnv.MountReady
            fiber    <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _        <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("local-row") != null))
            _        <- click("local-section")
            _        <- assertEventually(Sync.defer(dom.document.getElementById("local-authored") != null))
            authored <- Sync.defer {
                val section = dom.document.getElementById("local-authored")
                (section.parentNode.asInstanceOf[dom.Element].id, section.getAttribute("data-state"))
            }
            _         <- click("local-return-row")
            _         <- assertEventually(Sync.defer(dom.document.getElementById("local-row") != null))
            rowParent <- Sync.defer(
                dom.document.getElementById("local-row").closest("tbody").parentNode.asInstanceOf[dom.Element].id
            )
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield
            assert(authored == ("local-nested-table", "kept"))
            assert(rowParent == "local-nested-table")
    }

    "local mixed authored sections and transparent rows retain table topology across updates" in {
        for
            outer <- Signal.initRef(0)
            rows  <- Signal.initRef(Chunk("A"))
            ui = UI.div(
                UI.table(
                    outer.map(_ =>
                        UI.fragment(
                            UI.tbody(UI.tr(UI.td("before"))).id("local-mixed-before"),
                            rows.foreach(value => UI.tr(UI.td(value).id(s"local-mixed-$value"))),
                            UI.tbody(UI.tr(UI.td("after"))).id("local-mixed-after")
                        )
                    )
                ).id("local-mixed-table")
            )
            ready = new DomTestEnv.MountReady
            fiber    <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _        <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("local-mixed-A") != null))
            initial  <- Sync.defer(mixedTableTopology("local-mixed-table"))
            _        <- rows.set(Chunk("A", "B"))
            _        <- assertEventually(Sync.defer(dom.document.getElementById("local-mixed-B") != null))
            inner    <- Sync.defer(mixedTableTopology("local-mixed-table"))
            _        <- outer.getAndUpdate(_ + 1)
            _        <- assertEventually(Sync.defer(dom.document.getElementById("local-mixed-B") != null))
            repeated <- Sync.defer(mixedTableTopology("local-mixed-table"))
            _        <- fiber.interrupt
            _        <- fiber.getResult
        yield
            assert(initial == (3, "local-mixed-before", 1, "local-mixed-after", 0))
            assert(inner == (3, "local-mixed-before", 2, "local-mixed-after", 0))
            assert(repeated == inner)
    }

    "local mount preserves every exposed restricted parent keyed identity and nested JS properties" in {
        for
            rows       <- Signal.initRef(Chunk("A"))
            nestedProp <- Signal.initRef(false)
            ui = UI.div(
                UI.table(UI.tbody(rows.foreach(value => UI.tr(UI.td(value).id(s"local-grouped-$value"))))).id("local-grouped"),
                UI.table(UI.tbody(UI.tr(rows.foreach(value => UI.td(value).id(s"local-td-$value"))))).id("local-td-table"),
                UI.table(UI.tbody(UI.tr(rows.foreach(value => UI.th(value).id(s"local-th-$value"))))).id("local-th-table"),
                UI.ul(rows.foreach(value => UI.li(value).id(s"local-ul-$value"))).id("local-ul"),
                UI.ol(rows.foreach(value => UI.li(value).id(s"local-ol-$value"))).id("local-ol"),
                UI.select(rows.foreach(value => UI.option(value).value(value).id(s"local-option-$value"))).id("local-select"),
                UI.div(rows.foreachKeyed(identity)(value => UI.button(value).id(s"local-keyed-$value"))).id("local-keyed"),
                nestedProp.map(value => UI.div(UI.checkbox.id("local-nested-property").indeterminate(value)))
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("local-option-A") != null))
            _     <- rows.set(Chunk("A", "B"))
            _     <- nestedProp.set(true)
            // Each region above subscribes to the signal on its own, so one region having applied the update
            // says nothing about the other seven. Waiting on a single representative and then reading the
            // whole topology races the rest, which is why a loaded runner sees a count of 1 where 2 is
            // expected. Settle on the entire tuple instead. This keeps the assertion's full strength: a
            // region that never updates still fails, by exhausting the retry budget rather than by racing.
            readTopology = Sync.defer {
                def count(selector: String) = dom.document.querySelectorAll(selector).length
                val property                = dom.document.getElementById("local-nested-property")
                (
                    count("#local-grouped > tbody > tr"),
                    count("#local-td-table > tbody > tr > td"),
                    count("#local-th-table > tbody > tr > th"),
                    count("#local-ul > li"),
                    count("#local-ol > li"),
                    count("#local-select > option"),
                    count("#local-keyed > button"),
                    property.asInstanceOf[scalajs.Dynamic].indeterminate.asInstanceOf[Boolean],
                    property.getAttribute("data-kyo-prop-indeterminate"),
                    count("span[data-kyo-reactive]")
                )
            }
            expected: (Int, Int, Int, Int, Int, Int, Int, Boolean, String, Int) = (2, 2, 2, 2, 2, 2, 2, true, null, 0)
            _        <- assertEventually(readTopology.map(_ == expected))
            topology <- readTopology
            _        <- fiber.interrupt
            _        <- fiber.getResult
        yield assert(topology == expected)
    }

    "local mount updates directly nested logical ranges" in {
        for
            outer  <- Signal.initRef(true)
            middle <- Signal.initRef(true)
            value  <- Signal.initRef("one")
            ui = UI.div(
                outer.map(_ => (middle.map(_ => (value.map(v => UI.span(v).id("value")): UI)): UI)),
                UI.button("update").id("update").onClick(value.set("two"))
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer {
                val element = dom.document.getElementById("value")
                ready.installed && element != null && element.textContent == "one"
            })
            _ <- click("update")
            _ <- assertEventually(Sync.defer {
                val element = dom.document.getElementById("value")
                element != null && element.textContent == "two"
            })
            markerIds <- Sync.defer {
                val walker = dom.document.createTreeWalker(dom.document.body, 128, null, false)
                Iterator.continually(walker.nextNode()).takeWhile(_ != null).map(_.nodeValue).filter(_.startsWith("kyo-rs:")).toSeq
            }
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield
            assert(markerIds.size == 3)
            assert(markerIds.distinct.size == 3)
    }

    "direct signal-bound element inside another reactive updates independently" in {
        for
            outer <- Signal.initRef(true)
            value <- Signal.initRef("initial")
            ui = UI.div(
                outer.map(_ => (UI.input.id("field").value(value): UI)),
                UI.button("external").id("external").onClick(value.set("updated"))
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("field") != null))
            _     <- click("external")
            _     <- assertEventually(Sync.defer {
                val field = dom.document.getElementById("field")
                field != null && field.getAttribute("value") == "updated"
            })
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield ()
    }

    "a region's first observed value leaves the nodes the mount rendered in place" in {
        for
            text  <- Signal.initRef("one")
            email <- Signal.initRef("one@example.com")
            ui = UI.div(
                UI.input.id("first-text").value(text),
                UI.emailInput.id("first-email").value(email)
            )
            removals <- AtomicInt.init(0)
            observed <- Sync.defer {
                val count: scalajs.Array[scalajs.Dynamic] => Unit = records =>
                    records.foreach { record =>
                        val removed = record.removedNodes.asInstanceOf[scalajs.Array[dom.Node]]
                        removed.foreach { node =>
                            if node.nodeType == 1 && node.asInstanceOf[dom.Element].id == "first-email" then
                                import AllowUnsafe.embrace.danger
                                // Unsafe: the observer callback is a plain JS callback with no effect context.
                                discard(removals.unsafe.incrementAndGet())
                        }
                    }
                val callback: scalajs.Function2[scalajs.Array[scalajs.Dynamic], scalajs.Dynamic, Unit] = (records, _) =>
                    count(records)
                val observer = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].MutationObserver)(callback)
                discard(observer.observe(dom.document.body, scalajs.Dynamic.literal(childList = true, subtree = true)))
                (observer, count)
            }
            (observer, count) = observed
            ready             = new DomTestEnv.MountReady
            fiber    <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _        <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("first-email") != null))
            mounted  <- Sync.defer(dom.document.getElementById("first-email"))
            _        <- assertEventually(Sync.defer(ready.applied == 2))
            _        <- text.set("two")
            _        <- assertEventually(Sync.defer(dom.document.getElementById("first-text").getAttribute("value") == "two"))
            _        <- assertEventually(Sync.defer(ready.applied == 3))
            _        <- Sync.defer(count(observer.takeRecords().asInstanceOf[scalajs.Array[scalajs.Dynamic]]))
            _        <- Sync.defer(discard(observer.disconnect()))
            replaced <- removals.get
            live     <- Sync.defer(dom.document.getElementById("first-email") eq mounted)
            _        <- fiber.interrupt
            _        <- fiber.getResult
        yield
            assert(replaced == 0)
            assert(live)
    }

    "local own-bound text email and number inputs morph in place while focused" in {
        for
            text   <- Signal.initRef("one")
            email  <- Signal.initRef("one@example.com")
            number <- Signal.initRef("1")
            ui = UI.div(
                UI.input.id("morph-text").value(text),
                UI.emailInput.id("morph-email").value(email),
                UI.numberInput.id("morph-number").value(number)
            )
            ready = new DomTestEnv.MountReady
            fiber    <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _        <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("morph-number") != null))
            original <- Sync.defer(
                (
                    dom.document.getElementById("morph-text"),
                    dom.document.getElementById("morph-email"),
                    dom.document.getElementById("morph-number")
                )
            )
            _ <- Sync.defer {
                val field = original._1.asInstanceOf[scalajs.Dynamic]
                discard(field.focus())
                discard(field.setSelectionRange(1, 1))
            }
            _         <- text.set("two")
            _         <- assertEventually(Sync.defer(dom.document.getElementById("morph-text").getAttribute("value") == "two"))
            textState <- Sync.defer(
                (
                    dom.document.getElementById("morph-text") eq original._1,
                    dom.document.activeElement eq original._1,
                    original._1.asInstanceOf[scalajs.Dynamic].selectionStart.asInstanceOf[Int]
                )
            )
            _ <- Sync.defer(discard(original._2.asInstanceOf[scalajs.Dynamic].focus()))
            _ <- email.set("two@example.com")
            _ <- assertEventually(Sync.defer(dom.document.getElementById("morph-email").getAttribute("value") == "two@example.com"))
            emailState <- Sync.defer(
                (dom.document.getElementById("morph-email") eq original._2, dom.document.activeElement eq original._2)
            )
            _           <- Sync.defer(discard(original._3.asInstanceOf[scalajs.Dynamic].focus()))
            _           <- number.set("2")
            _           <- assertEventually(Sync.defer(dom.document.getElementById("morph-number").getAttribute("value") == "2"))
            numberState <- Sync.defer(
                (dom.document.getElementById("morph-number") eq original._3, dom.document.activeElement eq original._3)
            )
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield
            assert(textState == (true, true, 1))
            assert(emailState == (true, true))
            assert(numberState == (true, true))
    }

    "bound Dropdown updates repeatedly inside its initial logical range" in {
        for
            selected <- Signal.initRef("a")
            ui    = UI.div(UI.dropdown("Alpha" -> "a", "Beta" -> "b").id("local-dropdown").value(selected))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("local-dropdown-trigger") != null))
            _     <- selected.set("b")
            _     <- assertEventually(Sync.defer {
                val trigger = dom.document.getElementById("local-dropdown-trigger")
                trigger != null && trigger.textContent == "Beta ▾"
            })
            _ <- selected.set("a")
            _ <- assertEventually(Sync.defer {
                val trigger = dom.document.getElementById("local-dropdown-trigger")
                trigger != null && trigger.textContent == "Alpha ▾"
            })
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield ()
    }

    "local SVG replacement retains SVG boundaries for nested empty regions" in {
        for
            outer <- Signal.initRef(true)
            show  <- Signal.initRef(false)
            innerSignal = show.map(value => if value then Svg.circle.id("circle"): UI else UI.fragment(): UI)
            inner       = UI.Ast.Reactive[Svg.Circle](innerSignal)
            outerSignal = outer.map(_ => UI.Ast.Fragment[UI](Chunk(inner)): UI)
            outerNode   = UI.Ast.Reactive[Svg.G](outerSignal)
            svg         = Svg.Root(children = Chunk(outerNode))
            ui          = UI.div(svg, UI.button("show").id("show").onClick(show.set(true)))
            ready       = new DomTestEnv.MountReady
            fiber    <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _        <- assertEventually(Sync.defer(ready.installed && dom.document.querySelector("svg g[data-kyo-reactive]") != null))
            _        <- click("show")
            _        <- assertEventually(Sync.defer(dom.document.getElementById("circle") != null))
            comments <- Sync.defer {
                val walker = dom.document.createTreeWalker(dom.document.querySelector("svg"), 128, null, false)
                Iterator.continually(walker.nextNode()).takeWhile(_ != null).map(_.nodeValue).filter(_.startsWith("kyo-rs:")).toSeq
            }
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield assert(comments.isEmpty)
    }

    "range replacement restores focus and caret for raw HTML without a path" in {
        for
            value <- Signal.initRef("one")
            ui    = UI.div(value.map(v => UI.rawHtml(s"<input id='raw' value='$v'>")))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("raw") != null))
            _     <- Sync.defer {
                val raw = dom.document.getElementById("raw").asInstanceOf[scalajs.Dynamic]
                discard(raw.focus())
                discard(raw.setSelectionRange(1, 1))
            }
            _ <- value.set("two")
            _ <- assertEventually(Sync.defer {
                val raw = dom.document.getElementById("raw")
                raw != null && raw.getAttribute("value") == "two"
            })
            focused <- Sync.defer(dom.document.activeElement.id)
            caret   <- Sync.defer(dom.document.activeElement.asInstanceOf[scalajs.Dynamic].selectionStart.asInstanceOf[Int])
            _       <- fiber.interrupt
            _       <- fiber.getResult
        yield
            assert(focused == "raw")
            assert(caret == 1)
    }

    "table range replacement restores raw focus relative to the reactive rows" in {
        for
            value <- Signal.initRef("one")
            ui = UI.table(
                value.map(v => UI.tr(UI.td(UI.rawHtml(s"<input id='raw-table' value='$v'>"))))
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("raw-table") != null))
            _     <- Sync.defer {
                val raw = dom.document.getElementById("raw-table").asInstanceOf[scalajs.Dynamic]
                discard(raw.focus())
                discard(raw.setSelectionRange(1, 1))
            }
            _ <- value.set("two")
            _ <- assertEventually(Sync.defer {
                val raw = dom.document.getElementById("raw-table")
                raw != null && raw.getAttribute("value") == "two"
            })
            focused <- Sync.defer(dom.document.activeElement.id)
            caret   <- Sync.defer(dom.document.activeElement.asInstanceOf[scalajs.Dynamic].selectionStart.asInstanceOf[Int])
            _       <- fiber.interrupt
            _       <- fiber.getResult
        yield
            assert(focused == "raw-table")
            assert(caret == 1)
    }

    "range focus restoration stays within its selector-targeted mount" in {
        for
            left  <- Signal.initRef("left")
            right <- Signal.initRef("right")
            _     <- Sync.defer {
                dom.document.body.innerHTML = "<div id='left-mount'></div><div id='right-mount'></div>"
            }
            leftReady  = new DomTestEnv.MountReady
            rightReady = new DomTestEnv.MountReady
            leftFiber <- Fiber.initUnscoped(
                Scope.run(DomBackend.mount(left.map(v => UI.input.id("left-field").value(v)), "#left-mount", leftReady))
            )
            rightFiber <- Fiber.initUnscoped(
                Scope.run(DomBackend.mount(right.map(v => UI.input.id("right-field").value(v)), "#right-mount", rightReady))
            )
            _ <- assertEventually(Sync.defer(
                leftReady.installed && rightReady.installed && dom.document.getElementById("right-field") != null
            ))
            _       <- Sync.defer(discard(dom.document.getElementById("right-field").asInstanceOf[scalajs.Dynamic].focus()))
            _       <- right.set("updated")
            _       <- assertEventually(Sync.defer(dom.document.getElementById("right-field").getAttribute("value") == "updated"))
            focused <- Sync.defer(dom.document.activeElement.id)
            _       <- rightFiber.interrupt
            _       <- rightFiber.getResult
            _       <- leftFiber.interrupt
            _       <- leftFiber.getResult
        yield assert(focused == "right-field")
    }

    "range replacement applies JS properties on a top-level element" in {
        for
            state <- Signal.initRef(false)
            ui = UI.div(
                UI.checkbox.id("property-checkbox").indeterminate(state),
                UI.button("indeterminate").id("set-property").onClick(state.set(true))
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("property-checkbox") != null))
            _     <- click("set-property")
            _     <- assertEventually(Sync.defer {
                val checkbox = dom.document.getElementById("property-checkbox")
                checkbox != null && checkbox.asInstanceOf[scalajs.Dynamic].indeterminate.asInstanceOf[Boolean]
            })
            sourceAttribute <- Sync.defer(dom.document.getElementById("property-checkbox").getAttribute("data-kyo-prop-indeterminate"))
            _               <- fiber.interrupt
            _               <- fiber.getResult
        yield assert(sourceAttribute == null)
    }

    "local synthetic host transition treats an authored tbody as the semantic lifecycle root" in {
        for
            section <- Signal.initRef(false)
            content = section.map { authored =>
                if authored then
                    UI.tbody(UI.tr(UI.td("section")))
                        .id("local-semantic-host")
                        .tabIndex(-1)
                        .focusAuto(true)
                        .enterTransition("local-semantic-enter")
                        .leaveTransition("local-semantic-leave")
                        .jsProp("rangeprobe", "applied"): UI
                else UI.tr(UI.td("row").id("local-semantic-row")): UI
            }
            ui = UI.div(
                UI.table(content).id("local-semantic-table"),
                UI.button("section").id("local-show-section").onClick(section.set(true)),
                UI.button("row").id("local-show-row").onClick(section.set(false))
            )
            ready = new DomTestEnv.MountReady
            fiber         <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _             <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("local-semantic-row") != null))
            _             <- click("local-show-section")
            _             <- assertEventually(Sync.defer(dom.document.getElementById("local-semantic-host") != null))
            authoredState <- Sync.defer {
                val authored = dom.document.getElementById("local-semantic-host")
                (
                    authored.asInstanceOf[scalajs.Dynamic].rangeprobe.asInstanceOf[String],
                    authored.getAttribute("data-kyo-prop-rangeprobe"),
                    dom.document.activeElement.id,
                    authored.hasAttribute("data-kyo-range-host")
                )
            }
            _          <- click("local-show-row")
            _          <- assertEventually(Sync.defer(dom.document.getElementById("local-semantic-row") != null))
            leaveGhost <- Sync.defer(dom.document.querySelector("[data-kyo-ghost]") != null)
            _          <- fiber.interrupt
            _          <- fiber.getResult
        yield
            assert(authoredState == ("applied", null, "local-semantic-host", false))
            assert(leaveGhost)
    }

    "a region repaint inserts in front of a last row whose first root changed its tag" in {
        // A row whose first root changes its tag is replaced, not patched; matched from the tail, the replacement has
        // to become the boundary the rest of the pass inserts in front of. Rows of two roots are not addressable by
        // key, so the list repaints whole and the morph's own two-ended pass reconciles it.
        for
            rows <- Signal.initRef(Chunk(("a", false), ("b", false)))
            ui = UI.div(
                UI.div(rows.foreachKeyed(_._1)((k, asSpan) =>
                    UI.fragment(
                        if asSpan then UI.span(k).id(s"mt-${k}0") else UI.p(k).id(s"mt-${k}0"),
                        UI.div(s"$k!").id(s"mt-${k}1")
                    )
                )).id("mthost"),
                UI.span("after").id("mtafter")
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("mthost").textContent == "aa!bb!"))
            _     <- rows.set(Chunk(("a", false), ("c", false), ("b", true)))
            _     <- assertEventually(Sync.defer(dom.document.getElementById("mthost").textContent == "aa!cc!bb!"))
            state <- Sync.defer((dom.document.getElementById("mt-b0").tagName, dom.document.getElementById("mtafter").textContent))
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield assert(state == ("SPAN", "after"), s"state=$state")
        end for
    }

    private def topology: (rows: Int, groups: Int, spans: Int, anchorParent: String, staticCaptured: Boolean) =
        val table  = dom.document.getElementById("table")
        val groups = table.querySelectorAll(":scope > tbody")
        val walker = dom.document.createTreeWalker(table, 128, null, false)
        val anchor = Iterator
            .continually(walker.nextNode())
            .takeWhile(_ != null)
            .find(_.nodeValue.startsWith("kyo-rs:"))
            .get
        val parent         = anchor.parentNode.asInstanceOf[dom.Element]
        var current        = anchor.nextSibling
        var staticCaptured = false
        while current != null && !(current.nodeType == 8 && current.nodeValue.startsWith("kyo-re:")) do
            if current.nodeType == 1 then
                val element = current.asInstanceOf[dom.Element]
                staticCaptured ||= element.id == "before" || element.id == "after" || element.querySelector("#before,#after") != null
            current = current.nextSibling
        end while
        (
            rows = table.querySelectorAll("[id^='row-']").length,
            groups = groups.length,
            spans = table.querySelectorAll("span[data-kyo-reactive]").length,
            anchorParent = parent.tagName,
            staticCaptured = staticCaptured
        )
    end topology

    private def mixedTableTopology(id: String): (Int, String, Int, String, Int) =
        val table  = dom.document.getElementById(id)
        val bodies = table.querySelectorAll(":scope > tbody")
        (
            bodies.length,
            bodies(0).asInstanceOf[dom.Element].id,
            bodies(1).asInstanceOf[dom.Element].children.length,
            bodies(2).asInstanceOf[dom.Element].id,
            table.querySelectorAll(":scope > tbody > tbody").length
        )
    end mixedTableTopology

    "a write to a region inside a just-closed portal leaves the regions outside it live" in {
        // A write aimed at the region inside a portal whose twin already left the document, in the same step that
        // closed it and again afterwards, must not take the page's other regions down with the dead one.
        for
            open  <- Signal.initRef(true)
            inner <- Signal.initRef(0)
            other <- Signal.initRef(0)
            ui = UI.div(
                UI.when(open)(UI.div.id("stpanel").portal(true)(inner.map(i => UI.span(s"i:$i").id("stin"): UI))),
                other.map(o => UI.span(s"o:$o").id("stother"): UI)
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && text("stin") == "i:0"))
            _     <- Kyo.foreachDiscard(1 to 5) { n =>
                for
                    _ <- open.set(false)
                    _ <- inner.getAndUpdate(_ + 1)
                    _ <- assertEventually(Sync.defer(dom.document.getElementById("stpanel") == null))
                    // The twin is gone by now: this write can only reach a dead region.
                    _ <- inner.getAndUpdate(_ + 1)
                    _ <- other.set(n)
                    _ <- assertEventually(Sync.defer(text("stother") == s"o:$n"))
                    _ <- open.set(true)
                    _ <- assertEventually(Sync.defer(text("stin") == s"i:${2 * n}"))
                yield ()
            }
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield succeed
        end for
    }

    "a portal that opens with a focus-auto field keeps the focus through its move to body" in {
        // Focus seeding runs while the panel is still inline; the move to <body> drops DOM focus, so the adoption has
        // to carry it over.
        for
            open <- Signal.initRef(false)
            ui    = UI.div(UI.when(open)(UI.div.id("fpanel").portal(true)(UI.input.id("fin").focusAuto(true))))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed))
            _     <- open.set(true)
            _     <- assertEventually(Sync.defer(
                Maybe(dom.document.getElementById("fpanel")).exists(_.parentNode eq dom.document.body)
            ))
            focused <- Sync.defer(dom.document.activeElement.id)
            _       <- fiber.interrupt
            _       <- fiber.getResult
        yield assert(focused == "fin", s"focused=$focused")
        end for
    }

    "a portal inside a portal is re-homed too, and both leave with the outer one" in {
        def state: (Boolean, Boolean, Int) =
            def atBody(id: String) = Maybe(dom.document.getElementById(id)).exists(_.parentNode eq dom.document.body)
            (atBody("npo"), atBody("npi"), dom.document.querySelectorAll("[data-kyo-portal-slot]").length)
        for
            open <- Signal.initRef(true)
            ui = UI.div(UI.when(open)(
                UI.div.id("npo").portal(true)(UI.span("outer"), UI.div.id("npi").portal(true)(UI.span("inner")))
            ))
            ready = new DomTestEnv.MountReady
            fiber  <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _      <- assertEventually(Sync.defer(ready.installed && state == (true, true, 2)))
            _      <- open.set(false)
            _      <- assertEventually(Sync.defer(dom.document.getElementById("npo") == null))
            closed <- Sync.defer((dom.document.getElementById("npi") == null, state._3))
            _      <- fiber.interrupt
            _      <- fiber.getResult
        yield assert(closed == (true, 0), s"closed=$closed")
        end for
    }

    "a closing portal with a leave transition leaves through a ghost" in {
        for
            open <- Signal.initRef(true)
            ui    = UI.div(UI.when(open)(UI.div.id("gpanel").portal(true).leaveTransition("gp-leave")(UI.span("x"))))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(
                ready.installed && Maybe(dom.document.getElementById("gpanel")).exists(_.parentNode eq dom.document.body)
            ))
            _     <- open.set(false)
            _     <- assertEventually(Sync.defer(dom.document.getElementById("gpanel") == null))
            state <- Sync.defer(
                (dom.document.querySelector("[data-kyo-ghost]") != null, dom.document.querySelectorAll("[data-kyo-portal-slot]").length)
            )
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield assert(state == (true, 0), s"state=$state")
        end for
    }

    "an SVG portal leaves an SVG group as its slot" in {
        // A span cannot sit inside an <svg>, so the placeholder of an SVG element is a <g> in the SVG namespace.
        val ui    = UI.div(Svg.svg(Svg.g.id("sgp").portal(true)(Svg.rect)).id("shost"))
        val ready = new DomTestEnv.MountReady
        for
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(
                ready.installed && Maybe(dom.document.getElementById("sgp")).exists(_.parentNode eq dom.document.body)
            ))
            slot <- Sync.defer {
                val s = dom.document.querySelector("#shost [data-kyo-portal-slot]")
                if s == null then ("", "") else (s.tagName, s.namespaceURI)
            }
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield assert(slot == ("g", "http://www.w3.org/2000/svg"), s"slot=$slot")
        end for
    }

    "local portaled region closed and reopened still updates" in {
        for
            open  <- Signal.initRef(true)
            inner <- Signal.initRef(0)
            ui = UI.div(
                UI.button("toggle").id("lrt").onClick(open.getAndUpdate(!_).unit),
                UI.button("inner").id("lri").onClick(inner.getAndUpdate(_ + 1).unit),
                open.map(o =>
                    if o then UI.div.id("lrpanel").portal(true)(inner.map(i => UI.span(s"v:$i").id("lrtxt"): UI)): UI
                    else UI.span("closed").id("lrclosed"): UI
                )
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && text("lrtxt") == "v:0"))
            _     <- click("lrt")
            _     <- assertEventually(Sync.defer(text("lrclosed") == "closed"))
            _     <- click("lrt")
            _     <- assertEventually(Sync.defer(text("lrtxt") == "v:0"))
            _     <- click("lri")
            _     <- assertEventually(Sync.defer(text("lrtxt") == "v:1"))
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield succeed
        end for
    }

    "a list patch adopts a portal in a new row, keeps it while other rows change, and retires it with its row" in {
        def portalState: (Boolean, Int, Int) =
            val el = dom.document.getElementById("lppanel")
            val p  = if el == null then "" else el.getAttribute("data-kyo-path")
            (
                el != null && (el.parentNode eq dom.document.body),
                dom.document.querySelectorAll("[data-kyo-portal-slot]").length,
                if el == null then 0 else dom.document.querySelectorAll(s"""[data-kyo-path="$p"]""").length
            )
        end portalState
        for
            rows <- Signal.initRef(Chunk(1, 2))
            ui = UI.div(UI.ul(rows.foreachKeyed(_.toString)(i =>
                if i == 3 then UI.li.id("lprow")(UI.div.id("lppanel").portal(true)(UI.span("panel")))
                else UI.li(s"row $i").id(s"lprow-$i")
            )))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("lprow-2") != null))
            _     <- rows.set(Chunk(1, 2, 3))
            _     <- assertEventually(Sync.defer(portalState == (true, 1, 1)))
            _     <- rows.set(Chunk(0, 1, 2, 3))
            _     <- assertEventually(Sync.defer(dom.document.getElementById("lprow-0") != null))
            kept  <- Sync.defer(portalState)
            _     <- rows.set(Chunk(0, 1, 2))
            // A patch that only removes rows inserts nothing to sweep; the twin must still leave with its placeholder.
            _       <- assertEventually(Sync.defer(dom.document.getElementById("lppanel") == null), 10.seconds)
            retired <- Sync.defer(portalState)
            _       <- fiber.interrupt
            _       <- fiber.getResult
        yield assert(kept == (true, 1, 1) && retired == (false, 0, 0), s"kept=$kept retired=$retired")
        end for
    }

    "a repaint that puts text in front of an open portal keeps the portaled element and its focus" in {
        // The slot is matched by the portal's path, not by position. Empty text paints no node, so the repaint puts
        // a node in front of the slot while every path stays the same; paired by position, that text node would
        // meet the slot, the slot would go and the adopted twin would be replaced by a fresh copy.
        for
            tick <- Signal.initRef(0)
            ui = UI.div(tick.map(t =>
                val panel = UI.div.id("psib-panel").portal(true)(UI.input.id("psib-in"))
                UI.div.id("psib-host")(if t == 0 then "" else "x", panel): UI
            ))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(
                ready.installed && Maybe(dom.document.getElementById("psib-panel")).exists(_.parentNode eq dom.document.body)
            ))
            _ <- Sync.defer {
                dom.document.getElementById("psib-panel").asInstanceOf[scalajs.Dynamic].__probe = 1
                dom.document.getElementById("psib-in").asInstanceOf[dom.html.Input].focus()
            }
            _     <- tick.set(1)
            _     <- assertEventually(Sync.defer(dom.document.getElementById("psib-host").textContent == "x"))
            state <- Sync.defer {
                val panel = dom.document.getElementById("psib-panel")
                val path  = panel.getAttribute("data-kyo-path")
                (
                    !scalajs.isUndefined(panel.asInstanceOf[scalajs.Dynamic].__probe),
                    dom.document.activeElement.id,
                    dom.document.querySelectorAll("#psib-host > [data-kyo-portal-slot]").length,
                    dom.document.querySelectorAll(s"""[data-kyo-path="$path"]""").length
                )
            }
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield assert(state == (true, "psib-in", 1, 1), s"state=$state")
        end for
    }

    private def text(id: String): String =
        val el = dom.document.getElementById(id)
        if el == null then "" else el.textContent

    // A row whose root changes its tag cannot be patched in place, so the pass replaces it. When that row is matched
    // from the tail, the replacement has to become the boundary the rest of the pass places rows in front of: the
    // replaced node is no longer in the document.
    "a list patch inserts in front of a last row whose root changed its tag" in {
        for
            rows <- Signal.initRef(Chunk(("a", false), ("b", false)))
            ui = UI.div(
                UI.div(rows.foreachKeyed(_._1)((k, asSpan) =>
                    if asSpan then UI.span(k).id(s"lt-$k") else UI.p(k).id(s"lt-$k")
                )).id("lthost"),
                UI.span("after").id("ltafter")
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && text("lthost") == "ab"))
            _     <- rows.set(Chunk(("a", false), ("c", false), ("b", true)))
            _     <- assertEventually(Sync.defer(text("lthost") == "acb"))
            state <- Sync.defer((dom.document.getElementById("lt-b").tagName, text("ltafter")))
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield assert(state == ("SPAN", "after"), s"state=$state")
        end for
    }

    "a list patch moves retained rows instead of rebuilding them" in {
        // Only the rows that changed are rendered; every other row is named by key and keeps its DOM node. An
        // expando on each row survives only if the patch moved the node rather than replacing it.
        for
            rows <- Signal.initRef(Chunk("a", "b", "c", "d"))
            ui    = UI.div(UI.ul(rows.foreachKeyed(identity)(k => UI.li(k).id(s"row-$k"))).id("moved-list"))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("row-d") != null))
            _     <- Sync.defer(Seq("a", "b", "c", "d").foreach(k =>
                dom.document.getElementById(s"row-$k").asInstanceOf[scalajs.Dynamic].__mark = k
            ))
            _     <- rows.set(Chunk("d", "b", "e", "a"))
            _     <- assertEventually(Sync.defer(dom.document.getElementById("moved-list").textContent == "dbea"))
            marks <- Sync.defer(Seq("d", "b", "e", "a").map(k =>
                val mark = dom.document.getElementById(s"row-$k").asInstanceOf[scalajs.Dynamic].__mark
                if scalajs.isUndefined(mark) then "-" else mark.asInstanceOf[String]
            ))
            gone <- Sync.defer(dom.document.getElementById("row-c") == null)
            _    <- fiber.interrupt
            _    <- fiber.getResult
        yield assert(marks == Seq("d", "b", "-", "a") && gone, s"marks=$marks gone=$gone")
        end for
    }

    "a row a list patch inserts or replaces runs its enter transition and takes its focus" in {
        // The enter classes come off on the next animation frame; holding frames back keeps them readable.
        val window                        = scalajs.Dynamic.global.window
        val raf                           = window.requestAnimationFrame
        def entering(id: String): Boolean =
            Maybe(dom.document.getElementById(id)).exists(_.classList.contains("row-entering"))
        for
            rows <- Signal.initRef(Chunk("a" -> false, "b" -> false))
            ui = UI.div(UI.ul(rows.foreachKeyed(_._1) {
                case ("focus", _) => UI.li(UI.input.id("enter-focus").focusAuto(true)).id("enter-row-focus")
                case (k, true)    => UI.div(k).id(s"enter-row-$k").enterTransition("row-entering")
                case ("c", _)     => UI.li("c").id("enter-row-c").enterTransition("row-entering")
                case (k, false)   => UI.li(k).id(s"enter-row-$k")
            }))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("enter-row-b") != null))
            _     <- Sync.defer(window.requestAnimationFrame =
                ((_: scalajs.Function1[Double, Unit]) => 0): scalajs.Function1[scalajs.Function1[Double, Unit], Int]
            )
            _        <- rows.set(Chunk("a" -> false, "b" -> false, "c" -> false))
            _        <- assertEventually(Sync.defer(dom.document.getElementById("enter-row-c") != null))
            inserted <- Sync.defer(entering("enter-row-c"))
            // A changed row whose root element changes is replaced rather than morphed.
            _        <- rows.set(Chunk("a" -> false, "b" -> true, "c" -> false))
            _        <- assertEventually(Sync.defer(dom.document.getElementById("enter-row-b").tagName == "DIV"))
            replaced <- Sync.defer(entering("enter-row-b"))
            _        <- rows.set(Chunk("a" -> false, "b" -> true, "c" -> false, "focus" -> false))
            _        <- assertEventually(Sync.defer(dom.document.getElementById("enter-focus") != null))
            focused  <- Sync.defer(Maybe(dom.document.activeElement).map(_.id).getOrElse(""))
            _        <- Sync.defer(window.requestAnimationFrame = raf)
            _        <- fiber.interrupt
            _        <- fiber.getResult
        yield assert(
            inserted && replaced && focused == "enter-focus",
            s"inserted=$inserted replaced=$replaced focused=$focused"
        )
        end for
    }

    "bound row labels keep writing through swap, removal, insertion and a row repaint" in {
        // Every row holds a bound text region, and a list patch must leave the registry addressing each of them,
        // whether the row stayed, moved, arrived, or was painted again. A registry that lost a row's range shows
        // up here as a label that silently stops changing.
        final case class Row(id: String, label: SignalRef[String]) derives CanEqual
        def text(id: String): String =
            Maybe(dom.document.getElementById(s"lbl-$id")).map(_.textContent).getOrElse("<gone>")
        for
            labels <- Kyo.foreach(Chunk("a", "b", "c", "d", "e"))(id => Signal.initRef(s"${id}0").map(Row(id, _)))
            extra  <- Signal.initRef("f0").map(Row("f", _))
            again  <- Signal.initRef("c1").map(Row("c", _))
            rows   <- Signal.initRef(labels)
            byId = labels.map(r => r.id -> r).toMap
            ui   = UI.div(
                UI.ul(rows.foreachKeyed(_.id)(row => UI.li(UI.span(row.label: Signal[String]).id(s"lbl-${row.id}")))).id("bound-list")
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && text("e") == "e0"))
            // swap the ends
            _ <- rows.set(Chunk(byId("e"), byId("b"), byId("c"), byId("d"), byId("a")))
            _ <- assertEventually(Sync.defer(dom.document.getElementById("bound-list").textContent == "e0b0c0d0a0"))
            _ <- byId("a").label.set("a1")
            _ <- byId("b").label.set("b1")
            _ <- assertEventually(Sync.defer(text("a") == "a1" && text("b") == "b1"))
            // remove one, insert one, repaint one (same key, a different row value)
            _ <- rows.set(Chunk(byId("e"), extra, again, byId("d"), byId("a")))
            _ <- assertEventually(Sync.defer(dom.document.getElementById("bound-list").textContent == "e0f0c1d0a1"))
            _ <- byId("b").label.set("b2")
            _ <- extra.label.set("f1")
            _ <- again.label.set("c2")
            _ <- byId("d").label.set("d1")
            _ <- assertEventually(Sync.defer(text("f") == "f1" && text("c") == "c2" && text("d") == "d1"))
            // and the list still patches afterwards
            _       <- rows.set(Chunk(byId("a"), byId("d")))
            _       <- assertEventually(Sync.defer(dom.document.getElementById("bound-list").textContent == "a1d1"))
            _       <- byId("a").label.set("a2")
            _       <- assertEventually(Sync.defer(text("a") == "a2"))
            removed <- Sync.defer(text("b"))
            _       <- fiber.interrupt
            _       <- fiber.getResult
        yield assert(removed == "<gone>")
        end for
    }

    "a list that showed a key twice shows it once when its keys become distinct" in {
        for
            rows <- Signal.initRef(Chunk("x", "x"))
            ui    = UI.div(UI.ul(rows.foreachKeyed(identity)(k => UI.li(k))).id("dupkeys"))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && text("dupkeys") == "xx"))
            _     <- rows.set(Chunk("x", "y"))
            _     <- assertEventually(Sync.defer(text("dupkeys") == "xy"), 10.seconds)
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield succeed
        end for
    }

end DomBackendReactiveRangesTest
