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

    // Regression: the HTML parser drops one line break right after `<textarea>`, so a value that starts with one
    // reached the field without it, on the first render and on every repaint.
    "a textarea value that starts with a line break keeps it" in {
        for
            text <- Signal.initRef("\nsecond line")
            ui    = UI.div(UI.textarea.id("lead").value(text))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("lead") != null))
            first <- Sync.defer(dom.document.getElementById("lead").asInstanceOf[scalajs.Dynamic].value.asInstanceOf[String])
            // Each repaint writes a new value and reads the field back. A browser normalizes CR LF and a lone CR to
            // LF in a textarea's value, so the expected reading is the normalized form. The repaint has landed once
            // the content matches apart from leading line breaks, the ones the bug drops.
            repainted <- Kyo.foreach(LineBreakValues) { (written, expected) =>
                text.set(written)
                    .andThen(assertEventually(Sync.defer(
                        dom.document.getElementById("lead").asInstanceOf[scalajs.Dynamic].defaultValue.asInstanceOf[String]
                            .dropWhile(_ == '\n') == expected.dropWhile(_ == '\n')
                    )))
                    .andThen(Sync.defer(
                        (expected, dom.document.getElementById("lead").asInstanceOf[scalajs.Dynamic].value.asInstanceOf[String])
                    ))
            }
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield
            assert(first == "\nsecond line", s"the first render shows '${first.replace("\n", "\\n")}'")
            repainted.foreach((expected, shown) =>
                assert(shown == expected, s"a repaint shows '${shown.replace("\n", "\\n")}'")
            )
    }

    /** Values whose line breaks a renderer could drop, double or leave unnormalized, with what the field reads back. */
    private val LineBreakValues = Chunk(
        "\n\nthird line"      -> "\n\nthird line",
        "ends with a break\n" -> "ends with a break\n",
        "a\n\n\nb"            -> "a\n\n\nb",
        "\r\nwindows"         -> "\nwindows",
        "a\rb"                -> "a\nb"
    )

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

    // Regression: the focused-field branch treated every focused input as two-way bound and wrote the incoming
    // `value` attribute into it; an unbound field renders none, so a repaint around it while the reader typed
    // emptied it.
    "a repaint around a focused unbound field keeps what the reader typed" in {
        for
            outer <- Signal.initRef(0)
            ui = UI.div(outer.map(o =>
                UI.div(UI.input.id("focus-free"), UI.textarea.id("focus-free-area"), UI.span(s"o=$o").id("focus-free-status"))
            ))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("focus-free") != null))
            field <- element("focus-free")
            area  <- element("focus-free-area")
            _     <- Sync.defer {
                area.value = "area kept"
                discard(field.focus())
                field.value = "kept"
            }
            _     <- outer.set(1)
            _     <- assertEventually(Sync.defer(dom.document.getElementById("focus-free-status").textContent == "o=1"))
            _     <- Sync.defer(discard(area.focus()))
            _     <- outer.set(2)
            _     <- assertEventually(Sync.defer(dom.document.getElementById("focus-free-status").textContent == "o=2"))
            same  <- element("focus-free").map(_ eq field)
            shown <- Sync.defer((field.value.asInstanceOf[String], area.value.asInstanceOf[String]))
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield
            assert(same)
            assert(shown == ("kept", "area kept"), s"the focused unbound fields show $shown after a repaint around them")
    }

    /** Edge whitespace, a line break and every character HTML escapes. */
    private val TextareaTricky = "  a\n<b> & \"c\" 'd'  "

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

    private def element(id: String)(using Frame): scalajs.Dynamic < Sync =
        Sync.defer(dom.document.getElementById(id).asInstanceOf[scalajs.Dynamic])

    /** Picks `value` in the select `id` the way a reader does: the property moves, then `change` fires. */
    private def choose(id: String, value: String)(using Frame): Unit < Sync =
        element(id).map { select =>
            select.value = value
            val change = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].Event)(
                "change",
                scalajs.Dynamic.literal(bubbles = true)
            )
            discard(select.dispatchEvent(change))
        }

    // Regression: a select has no `value` attribute in HTML, so a bound select rendered as `<select value="b">`
    // showed its first option, on the first render and after every ref write.
    "a bound select shows its value on the first render and after a ref write" in {
        for
            picked <- Signal.initRef("b")
            ui = UI.div(
                UI.select(UI.option("A").value("a"), UI.option("B").value("b"), UI.option("C").value("c"))
                    .id("pick")
                    .value(picked),
                picked.map(v => UI.span(s"picked=$v").id("pick-status"))
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("pick") != null))
            first <- element("pick").map(_.value.asInstanceOf[String])
            _     <- choose("pick", "c")
            _     <- assertEventually(Sync.defer(dom.document.getElementById("pick-status").textContent == "picked=c"))
            _     <- picked.set("a")
            // The select's own paint has landed once its rendered value names a.
            _     <- assertEventually(Sync.defer(dom.document.getElementById("pick").getAttribute("value") == "a"))
            after <- element("pick").map(_.value.asInstanceOf[String])
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield
            assert(first == "b", s"the select shows '$first' on its first render")
            assert(after == "a", s"the select shows '$after' after its ref was set to a")
    }

    "a bound select keeps its value when its options arrive later" in {
        for
            options <- Signal.initRef(Chunk.empty[String])
            picked  <- Signal.initRef("b")
            ui = UI.div(
                UI.select(options.foreach(v => UI.option(v.toUpperCase).value(v))).id("late").value(picked),
                options.map(o => UI.span(s"n=${o.size}").id("late-status"))
            )
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("late") != null))
            _     <- options.set(Chunk("a", "b", "c"))
            _     <- assertEventually(Sync.defer(dom.document.querySelectorAll("#late > option").length == 3))
            shown <- element("late").map(_.value.asInstanceOf[String])
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield assert(shown == "b", s"the select shows '$shown' once its options arrived")
    }

    // Regression: after the first click a checkbox shows its `checked` PROPERTY, which no longer follows the
    // attribute, and the morph wrote only the attribute. A list's "Clear" emptied the selection, the `checked`
    // attributes went, and every row the reader had ticked stayed ticked.
    "a render that unchecks a checkbox the reader ticked clears the tick" in {
        for
            on <- Signal.initRef(false)
            ui    = UI.div(on.map(v => UI.checkbox.id("tick").checked(v).onChange(on.set(_))))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("tick") != null))
            box   <- element("tick")
            _     <- Sync.defer(discard(box.click()))
            _     <- assertEventually(Sync.defer(dom.document.getElementById("tick").hasAttribute("checked")))
            _     <- on.set(false)
            _     <- assertEventually(Sync.defer(!dom.document.getElementById("tick").hasAttribute("checked")))
            shown <- element("tick").map(_.checked.asInstanceOf[Boolean])
            same  <- element("tick").map(_ eq box)
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield
            assert(same)
            assert(!shown, "the checkbox still shows the tick the app took back")
    }

    "a ref write unchecks a two-way bound checkbox the reader ticked" in {
        for
            on <- Signal.initRef(false)
            ui    = UI.div(UI.checkbox.id("bound-tick").checked(on), on.map(v => UI.span(s"on=$v").id("bound-status")))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("bound-tick") != null))
            _     <- element("bound-tick").map(box => discard(box.click()))
            _     <- assertEventually(Sync.defer(dom.document.getElementById("bound-status").textContent == "on=true"))
            _     <- on.set(false)
            _     <- assertEventually(Sync.defer(dom.document.getElementById("bound-status").textContent == "on=false"))
            _     <- assertEventually(Sync.defer(!dom.document.getElementById("bound-tick").hasAttribute("checked")))
            shown <- element("bound-tick").map(_.checked.asInstanceOf[Boolean])
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield assert(!shown, "the two-way bound checkbox still shows the tick its ref took back")
    }

    "a render that moves a radio group back to the first choice checks it again" in {
        for
            choice <- Signal.initRef("a")
            ui = UI.div(choice.map(c =>
                UI.div(
                    UI.radio.id("radio-a").name("g").checked(c == "a").onChange(on => if on then choice.set("a") else Kyo.unit),
                    UI.radio.id("radio-b").name("g").checked(c == "b").onChange(on => if on then choice.set("b") else Kyo.unit)
                )
            ))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("radio-b") != null))
            // The reader goes to b, back to a and to b again, so both radios have been toggled by hand.
            _ <- Kyo.foreachDiscard(Seq("b", "a", "b")) { c =>
                element(s"radio-$c").map(radio => discard(radio.click()))
                    .andThen(assertEventually(Sync.defer(dom.document.getElementById(s"radio-$c").hasAttribute("checked"))))
            }
            _ <- choice.set("a")
            _ <- assertEventually(Sync.defer(dom.document.getElementById("radio-a").hasAttribute("checked")))
            a <- element("radio-a").map(_.checked.asInstanceOf[Boolean])
            b <- element("radio-b").map(_.checked.asInstanceOf[Boolean])
            _ <- fiber.interrupt
            _ <- fiber.getResult
        yield assert((a, b) == (true, false), s"the group shows a=$a b=$b after the app chose a")
    }

    // Guard for the fix above: an unbound checkbox renders no `checked` on every pass, so a repaint of the region
    // around it must leave the reader's tick alone, as it leaves an unbound field's text.
    "a repaint around an unbound checkbox keeps the reader's tick" in {
        for
            outer <- Signal.initRef(0)
            ui    = UI.div(outer.map(o => UI.div(UI.checkbox.id("free-tick"), UI.span(s"o=$o").id("free-tick-status"))))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("free-tick") != null))
            box   <- element("free-tick")
            _     <- Sync.defer(discard(box.click()))
            _     <- outer.set(1)
            _     <- assertEventually(Sync.defer(dom.document.getElementById("free-tick-status").textContent == "o=1"))
            same  <- element("free-tick").map(_ eq box)
            shown <- element("free-tick").map(_.checked.asInstanceOf[Boolean])
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield
            assert(same)
            assert(shown, "the unbound checkbox lost the reader's tick in a repaint around it")
    }

    "a render that selects an option the reader moved away from selects it again" in {
        for
            chosen <- Signal.initRef("a")
            ui = UI.div(chosen.map(c =>
                UI.select(
                    UI.option("A").value("a").selected(c == "a"),
                    UI.option("B").value("b").selected(c == "b"),
                    UI.option("C").value("c").selected(c == "c")
                ).id("opts")
            ))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("opts") != null))
            // The reader visits B and comes back to A; the app follows neither, its choice is still a.
            _     <- choose("opts", "b")
            _     <- choose("opts", "a")
            _     <- chosen.set("b")
            _     <- assertEventually(Sync.defer(dom.document.querySelector("#opts > option[value='b']").hasAttribute("selected")))
            shown <- element("opts").map(_.value.asInstanceOf[String])
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield assert(shown == "b", s"the select shows '$shown' after the app selected b")
    }

    // Regression: `indeterminate` exists only as a property, set through a JS property that nothing ever set back,
    // so a checkbox that left the mixed state kept its dash.
    "a checkbox that leaves the mixed state loses its dash" in {
        for
            mixed <- Signal.initRef(true)
            // One region paints the box and its status, so a status that moved says the box was painted too.
            ui = UI.div(mixed.map(m =>
                UI.div(UI.checkbox.id("mixed").indeterminate(m), UI.span(s"m=$m").id("mixed-status"))
            ))
            ready = new DomTestEnv.MountReady
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("mixed") != null))
            first <- element("mixed").map(_.indeterminate.asInstanceOf[Boolean])
            _     <- mixed.set(false)
            _     <- assertEventually(Sync.defer(dom.document.getElementById("mixed-status").textContent == "m=false"))
            _     <- mixed.set(true)
            _     <- assertEventually(Sync.defer(dom.document.getElementById("mixed-status").textContent == "m=true"))
            again <- element("mixed").map(_.indeterminate.asInstanceOf[Boolean])
            _     <- mixed.set(false)
            _     <- assertEventually(Sync.defer(dom.document.getElementById("mixed-status").textContent == "m=false"))
            shown <- element("mixed").map(_.indeterminate.asInstanceOf[Boolean])
            _     <- fiber.interrupt
            _     <- fiber.getResult
        yield
            assert(first && again)
            assert(!shown, "the checkbox still shows the mixed dash after leaving the mixed state")
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

end DomBackendReactiveRangesTest
