package kyo

import kyo.Browser.*
import kyo.internal.HtmlOp
import kyo.internal.KeyboardEventData
import kyo.internal.MouseEventData
import kyo.internal.ReactiveUI
import kyo.internal.UIEvent
import kyo.internal.UIExchange

class ReactiveUITest extends UITest:

    // ---- HtmlOp construction ----

    "HtmlOp.Replace carries path and html" in {
        val op = HtmlOp.Replace(Seq("0", "1"), "<span>hello</span>")
        assert(op.path == Seq("0", "1"))
        assert(op.html == "<span>hello</span>")
    }

    "HtmlOp.Remove carries path" in {
        val op = HtmlOp.Remove(Seq("root", "child"))
        assert(op.path == Seq("root", "child"))
    }

    "HtmlOp.InjectCss carries css string" in {
        val op = HtmlOp.InjectCss(".foo { color: red; }")
        assert(op.css == ".foo { color: red; }")
    }

    // ---- UIEvent construction ----

    "UIEvent.Click carries path" in {
        val ev = UIEvent.Click(Seq("0"), MouseEventData(UI.Modifiers.none, Absent))
        assert(ev.path == Seq("0"))
    }

    "UIEvent.Input carries path and value" in {
        val ev = UIEvent.Input(Seq("0", "1"), "hello")
        ev match
            case UIEvent.Input(path, value) =>
                assert(path == Seq("0", "1"))
                assert(value == "hello")
            case _ => fail("expected Input")
        end match
    }

    "UIEvent.Change carries path and value" in {
        val ev = UIEvent.Change(Seq("root"), "changed")
        ev match
            case UIEvent.Change(_, value) => assert(value == "changed")
            case _                        => fail("expected Change")
    }

    "UIEvent.ChangeChecked carries checked boolean" in {
        val ev = UIEvent.ChangeChecked(Seq("0"), checked = true)
        ev match
            case UIEvent.ChangeChecked(_, checked) => assert(checked)
            case _                                 => fail("expected ChangeChecked")
    }

    "UIEvent.KeyDown carries keyboard data" in {
        val kbData = KeyboardEventData("Enter", UI.Modifiers(ctrl = false, alt = false, shift = true, meta = false), Absent)
        val ev     = UIEvent.KeyDown(Seq("0"), kbData)
        ev match
            case UIEvent.KeyDown(_, keyboard) =>
                assert(keyboard.key == "Enter")
                assert(keyboard.modifiers.shift)
                assert(!keyboard.modifiers.ctrl)
            case _ => fail("expected KeyDown")
        end match
    }

    "UIEvent.Submit carries path" in {
        val ev = UIEvent.Submit(Seq("form"), MouseEventData(UI.Modifiers.none, Absent))
        assert(ev.path == Seq("form"))
    }

    "UIEvent.Focus carries path" in {
        val ev = UIEvent.Focus(Seq("input"), MouseEventData(UI.Modifiers.none, Absent))
        assert(ev.path == Seq("input"))
    }

    // ---- Browser-level reactive behaviour ----

    "reactive span updates on signal change" in {
        val app: UI < Async =
            for ref <- Signal.initRef("initial")
            yield UI.div(
                UI.button("Change").id("btn").onClick(ref.set("updated")),
                ref.map(v => UI.span(v).id("val"))
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("val"), "initial")
                _ <- Browser.click(Selector.id("btn"))
                _ <- Browser.assertText(Selector.id("val"), "updated")
            yield ()
        }
    }

    "UI.when hides element when signal is false" in {
        val app: UI < Async =
            for show <- Signal.initRef(true)
            yield UI.div(
                UI.button("Toggle").id("tog").onClick(show.getAndUpdate(!_).unit),
                UI.when(show)(UI.span("visible").id("target"))
            )
        withUI(app) {
            for
                _ <- Browser.assertExists(Selector.id("target"))
                _ <- Browser.click(Selector.id("tog"))
                _ <- Browser.assertNotExists(Selector.id("target"))
            yield ()
        }
    }

    "two reactive zones update independently" in {
        val app: UI < Async =
            for
                a <- Signal.initRef("A0")
                b <- Signal.initRef("B0")
            yield UI.div(
                UI.button("UpdateA").id("ua").onClick(a.set("A1")),
                UI.button("UpdateB").id("ub").onClick(b.set("B1")),
                a.map(v => UI.span(v).id("va")),
                b.map(v => UI.span(v).id("vb"))
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("va"), "A0")
                _ <- Browser.assertText(Selector.id("vb"), "B0")
                _ <- Browser.click(Selector.id("ua"))
                _ <- Browser.assertText(Selector.id("va"), "A1")
                _ <- Browser.assertText(Selector.id("vb"), "B0")
                _ <- Browser.click(Selector.id("ub"))
                _ <- Browser.assertText(Selector.id("va"), "A1")
                _ <- Browser.assertText(Selector.id("vb"), "B1")
            yield ()
        }
    }

    "nested reactive within reactive updates correctly" in {
        val app: UI < Async =
            for
                outer <- Signal.initRef(true)
                inner <- Signal.initRef("inner-val")
            yield UI.div(
                UI.button("HideOuter").id("ho").onClick(outer.set(false)),
                UI.button("UpdateInner").id("ui").onClick(inner.set("inner-new")),
                UI.when(outer)(UI.div(inner.map(t => UI.span(t).id("inner"))))
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("inner"), "inner-val")
                _ <- Browser.click(Selector.id("ui"))
                _ <- Browser.assertText(Selector.id("inner"), "inner-new")
                _ <- Browser.click(Selector.id("ho"))
                _ <- Browser.assertNotExists(Selector.id("inner"))
            yield ()
        }
    }

    "reactive re-subscribes after DOM replacement" in {
        val app: UI < Async =
            for
                toggle <- Signal.initRef(false)
                inner  <- Signal.initRef("v0")
            yield UI.div(
                UI.button("Flip").id("flip").onClick(toggle.getAndUpdate(!_).unit),
                UI.button("Set").id("set").onClick(inner.set("v1")),
                toggle.map { t =>
                    UI.div.id(if t then "box-b" else "box-a")(
                        inner.map(v => UI.span(v).id("ispan"))
                    )
                }
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("ispan"), "v0")
                _ <- Browser.click(Selector.id("flip"))             // outer reactive re-renders
                _ <- Browser.assertText(Selector.id("ispan"), "v0") // inner still works
                _ <- Browser.click(Selector.id("set"))
                _ <- Browser.assertText(Selector.id("ispan"), "v1")
            yield ()
        }
    }

    "signal update after multiple renders is idempotent" in {
        val app: UI < Async =
            for counter <- Signal.initRef(0)
            yield UI.div(
                UI.button("Inc").id("inc").onClick(counter.getAndUpdate(_ + 1).unit),
                counter.map(n => UI.span(n.toString).id("cnt"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("inc"))
                _ <- Browser.click(Selector.id("inc"))
                _ <- Browser.click(Selector.id("inc"))
                _ <- Browser.assertText(Selector.id("cnt"), "3")
                _ <- Browser.click(Selector.id("inc"))
                _ <- Browser.click(Selector.id("inc"))
                _ <- Browser.assertText(Selector.id("cnt"), "5")
            yield ()
        }
    }

    // ---- keyboard activation the dispatcher emulates ----
    //
    // No browser runs these: the tree is normalized, subscribed, and handed a `UIEvent.KeyDown`
    // directly, which is what both transports post once the client shim has had its say. The shim's
    // own half (which browser default to suppress) is `KeyPolicy`'s, pinned in `KeyPolicyTest`.

    private class NoopExchange extends UIExchange:
        def onChange(path: Seq[String], ui: UI, mount: Boolean)(using Frame): Unit < Async = ()

    /** Runs one key against a single element carrying a counting click handler. */
    private def activations(target: AtomicInt => UI, key: String)(using Frame): Int < Async =
        for
            hits <- AtomicInt.init(0)
            dispatch <- Scope.run {
                for
                    root         <- ReactiveUI.normalize(target(hits), Seq.empty)
                    subscription <- ReactiveUI.subscribe(root, new NoopExchange)
                yield subscription.handle
            }
            _     <- dispatch(Seq.empty, UIEvent.KeyDown(Seq.empty, KeyboardEventData(key, UI.Modifiers.none, Absent)))
            count <- hits.get
        yield count

    private def anchor(hits: AtomicInt): UI =
        UI.a.href(UI.Href.Path("/somewhere")).onClick(hits.incrementAndGet.unit)

    private def button(hits: AtomicInt): UI =
        UI.button("go").onClick(hits.incrementAndGet.unit)

    "Enter activates an anchor" in {
        activations(anchor, "Enter").map(n => assert(n == 1))
    }

    "Space does not activate an anchor" in {
        // The ARIA link pattern is Enter-only, and the browser agrees: with a link focused Space
        // scrolls the page, it does not follow the link. Emulating a Space activation here made
        // every kyo link behave unlike every other link on the web.
        activations(anchor, " ").map(n => assert(n == 0))
    }

    "Enter activates a button" in {
        activations(button, "Enter").map(n => assert(n == 1))
    }

    "Space activates a button" in {
        activations(button, " ").map(n => assert(n == 1))
    }

    "a key that activates nothing leaves the click handler alone" in {
        activations(anchor, "ArrowDown").map(n => assert(n == 0))
    }

end ReactiveUITest
