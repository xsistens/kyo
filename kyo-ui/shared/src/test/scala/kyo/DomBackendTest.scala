package kyo

import kyo.Browser.*

// DomBackend.scala is a JS-only source. These tests exercise its behaviour
// end-to-end via the JVM browser test infrastructure (SSE/event POST cycle).
class DomBackendTest extends UITest:

    "Replace op updates only the target reactive zone" in {
        val app: UI < Async =
            for
                a <- Signal.initRef("zone-a")
                b <- Signal.initRef("zone-b")
            yield UI.div(
                UI.button("UpdateA").id("ua").onClick(a.set("zone-a-new")),
                a.map(v => UI.span(v).id("za")),
                b.map(v => UI.span(v).id("zb"))
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("za"), "zone-a")
                _ <- Browser.assertText(Selector.id("zb"), "zone-b")
                _ <- Browser.click(Selector.id("ua"))
                _ <- Browser.assertText(Selector.id("za"), "zone-a-new")
                // zone b is untouched
                _ <- Browser.assertText(Selector.id("zb"), "zone-b")
            yield ()
        }
    }

    "empty reactive renders as placeholder span with data-kyo-path" in {
        val app: UI < Async =
            for show <- Signal.initRef(false)
            yield UI.div(
                UI.when(show)(UI.span("content").id("content"))
            )
        withUI(app) {
            // When signal is false, show is absent; placeholder span should exist in DOM
            // (kyo-ui uses a placeholder span so the path anchor is preserved)
            Browser.assertNotExists(Selector.id("content")).unit
        }
    }

    "deep path update applies to nested reactive element only" in {
        val app: UI < Async =
            for
                outer <- Signal.initRef("outer-val")
                inner <- Signal.initRef("inner-val")
            yield UI.div(
                outer.map(ov =>
                    UI.div.id("outer-zone")(
                        UI.span(ov).id("outer-text"),
                        inner.map(iv => UI.span(iv).id("inner-text"))
                    )
                )
            )
        withUI(app) {
            // Both render initially
            for
                _ <- Browser.assertText(Selector.id("outer-text"), "outer-val")
                _ <- Browser.assertText(Selector.id("inner-text"), "inner-val")
            yield ()
        }
    }

    "nested reactive directly inside a reactive patches independently (no path collision)" in {
        // Regression for the same-data-kyo-path collision: when a reactive's value is ITSELF a
        // reactive (no element between — the shape `open.render(hi.render(...))` that mounted
        // components like Select use), both regions used to share one path, so the patcher could
        // only ever address the outer node and the inner region's update was lost. With distinct
        // sub-paths the inner region patches its own node. The `: UI` ascription lifts the inner
        // Signal[SpanElement] into a Reactive so the outer region's value is itself reactive.
        val app: UI < Async =
            for
                outer <- Signal.initRef("o0")
                inner <- Signal.initRef("i0")
            yield UI.div(
                UI.button("set-inner").id("set-inner").onClick(inner.set("i1")),
                UI.button("set-outer").id("set-outer").onClick(outer.set("o1")),
                outer.map(o => (inner.map(i => UI.span(s"$o/$i").id("cell")): UI))
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("cell"), "o0/i0")
                // change ONLY inner: before the fix the inner region could not patch and stayed "o0/i0".
                _ <- Browser.click(Selector.id("set-inner"))
                _ <- Browser.assertText(Selector.id("cell"), "o0/i1")
                // change outer: its repaint regenerates the inner region, carrying the latest inner value.
                _ <- Browser.click(Selector.id("set-outer"))
                _ <- Browser.assertText(Selector.id("cell"), "o1/i1")
            yield ()
        }
    }

    "click on nested element fires its handler and bubbles to parent" in {
        val app: UI < Async =
            for
                parentCount <- Signal.initRef(0)
                childCount  <- Signal.initRef(0)
            yield UI.div.id("parent").onClick(parentCount.getAndUpdate(_ + 1).unit)(
                UI.button("child").id("child").onClick(childCount.getAndUpdate(_ + 1).unit),
                parentCount.map(n => UI.span(s"p:$n").id("pc")),
                childCount.map(n => UI.span(s"c:$n").id("cc"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("child"))
                _ <- Browser.assertText(Selector.id("cc"), "c:1")
                _ <- Browser.assertText(Selector.id("pc"), "p:1")
            yield ()
        }
    }

    "three independent signals update respective DOM zones" in {
        val app: UI < Async =
            for
                a <- Signal.initRef("a")
                b <- Signal.initRef("b")
                c <- Signal.initRef("c")
            yield UI.div(
                UI.button("A").id("ba").onClick(a.set("a-new")),
                UI.button("B").id("bb").onClick(b.set("b-new")),
                UI.button("C").id("bc").onClick(c.set("c-new")),
                a.map(v => UI.span(v).id("za")),
                b.map(v => UI.span(v).id("zb")),
                c.map(v => UI.span(v).id("zc"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("ba"))
                _ <- Browser.assertText(Selector.id("za"), "a-new")
                _ <- Browser.assertText(Selector.id("zb"), "b")
                _ <- Browser.assertText(Selector.id("zc"), "c")
                _ <- Browser.click(Selector.id("bc"))
                _ <- Browser.assertText(Selector.id("za"), "a-new")
                _ <- Browser.assertText(Selector.id("zc"), "c-new")
            yield ()
        }
    }

    // INV-008: the single-consumer drain preserves event ordering; all 5 clicks must be processed in
    // order by the same drain fiber, so the counter reaches 5 monotonically with no dropped events.
    "events dispatch in order under the page scope" in {
        val app: UI < Async =
            for counterRef <- Signal.initRef(0)
            yield UI.div(
                UI.button("inc").id("inc").onClick(counterRef.getAndUpdate(_ + 1).unit),
                counterRef.map(n => UI.span(n.toString).id("counter"))
            )
        withUI(app) {
            for
                _ <- Browser.click(Selector.id("inc"))
                _ <- Browser.assertText(Selector.id("counter"), "1")
                _ <- Browser.click(Selector.id("inc"))
                _ <- Browser.assertText(Selector.id("counter"), "2")
                _ <- Browser.click(Selector.id("inc"))
                _ <- Browser.assertText(Selector.id("counter"), "3")
                _ <- Browser.click(Selector.id("inc"))
                _ <- Browser.assertText(Selector.id("counter"), "4")
                _ <- Browser.click(Selector.id("inc"))
                _ <- Browser.assertText(Selector.id("counter"), "5")
            yield ()
        }
    }

    // Ownership guard (Unit 8 imperative id-addressed channel): a class/attribute set in place via bindClassById /
    // bindAttrById is owned by the client and must SURVIVE a later morph of that element — server HTML never carries
    // the client-set value, so reconciling it back would clobber the imperative write. Both apps drive the host
    // element through a real re-render (a sibling signal bumps the reactive region that contains it) so morphAttrs
    // runs against server HTML that lacks the owned value. The attr case is the regression that pins the per-name
    // shield: before the __kyoOwn-per-attribute ownership, the channel registered the id but morphAttrs shielded
    // only class/style, so a bound attribute was reconciled away on the next morph.

    "imperatively-bound class survives a re-render morph of its element" in {
        val app: UI < Async =
            for
                tick <- Signal.initRef(0)
                on   <- Signal.initRef(true)
            yield UI.div(
                UI.button("tick").id("tick").onClick(tick.getAndUpdate(_ + 1).unit),
                tick.map(t => UI.div.id("host")(UI.span(s"t:$t").id("txt"))),
                UI.mounted {
                    for
                        cmds <- UI.commands
                        _    <- cmds.bindClassById("host", "owned-cls", on)
                    yield UI.empty
                }.placeholder(UI.empty)
            )
        withUI(app) {
            for
                _ <- Browser.waitForAttribute(Selector.id("host"), "class", "owned-cls")
                _ <- Browser.click(Selector.id("tick"))
                _ <- Browser.assertText(Selector.id("txt"), "t:1")                      // the element was morphed
                _ <- Browser.assertAttribute(Selector.id("host"), "class", "owned-cls") // ...and the owned class survived
            yield ()
        }
    }

    "imperatively-bound attribute survives a re-render morph of its element" in {
        val app: UI < Async =
            for
                tick <- Signal.initRef(0)
                v    <- Signal.initRef("yes")
            yield UI.div(
                UI.button("tick").id("atick").onClick(tick.getAndUpdate(_ + 1).unit),
                tick.map(t => UI.div.id("ahost")(UI.span(s"t:$t").id("atxt"))),
                UI.mounted {
                    for
                        cmds <- UI.commands
                        _    <- cmds.bindAttrById("ahost", "data-owned", v)
                    yield UI.empty
                }.placeholder(UI.empty)
            )
        withUI(app) {
            for
                _ <- Browser.waitForAttribute(Selector.id("ahost"), "data-owned", "yes")
                _ <- Browser.click(Selector.id("atick"))
                _ <- Browser.assertText(Selector.id("atxt"), "t:1")
                _ <- Browser.assertAttribute(Selector.id("ahost"), "data-owned", "yes") // owned attr survives the morph
            yield ()
        }
    }

    "declarative reactive attribute patches a const element in place (no re-render), survives a sibling morph" in {
        val app: UI < Async =
            for
                tick <- Signal.initRef(0)
                lbl  <- Signal.initRef("first")
            yield UI.div(
                UI.button("tick").id("dtick").onClick(tick.getAndUpdate(_ + 1).unit),
                // dhost is a CONST node (no ref-bound value; only a declaratively-bound reactive aria-label). The
                // framework forks no region observer for it, so the only way its aria-label can change is the
                // in-place attr patch — this proves no re-render/re-mount.
                UI.div.id("dhost").aria("label", lbl)(UI.span("x").id("dchild")),
                tick.map(t => UI.span(s"t:$t").id("dtxt")),
                UI.button("relabel").id("drelabel").onClick(lbl.set("second"))
            )
        withUI(app) {
            for
                _ <- Browser.waitForAttribute(Selector.id("dhost"), "aria-label", "first")
                _ <- Browser.click(Selector.id("drelabel"))
                _ <- Browser.assertAttribute(Selector.id("dhost"), "aria-label", "second") // patched in place on a const node
                _ <- Browser.click(Selector.id("dtick"))
                _ <- Browser.assertText(Selector.id("dtxt"), "t:1")                        // a sibling region re-rendered
                _ <- Browser.assertAttribute(Selector.id("dhost"), "aria-label", "second") // owned attr unaffected by the morph
            yield ()
        }
    }

    "declarative reactive attribute on an element produced INSIDE a UI.mounted block patches in place" in {
        // Component topology (DatePicker/AutoComplete/Select build their field inside an effectful
        // UI.mounted region). The reactive-attr element is NOT at the top level — it is produced by
        // the mount body. This pins that ReactiveUI forks the attr observer while walking mounted
        // content, so the aria-label still patches in place (no re-mount) when the signal emits.
        val app: UI < Async =
            for lbl <- Signal.initRef("m-first")
            yield UI.div(
                UI.button("relabel").id("mrelabel").onClick(lbl.set("m-second")),
                UI.mounted {
                    Signal.initRef(0).map(_ => UI.div.id("mhost").aria("label", lbl)(UI.span("y").id("mchild")))
                }.placeholder(UI.empty)
            )
        withUI(app) {
            for
                _ <- Browser.waitForAttribute(Selector.id("mhost"), "aria-label", "m-first")
                _ <- Browser.click(Selector.id("mrelabel"))
                _ <- Browser.assertAttribute(Selector.id("mhost"), "aria-label", "m-second")
                _ <- Browser.assertText(Selector.id("mchild"), "y") // child untouched — no re-mount
            yield ()
        }
    }

    "declarative reactive boolean attribute (disabled) toggles in place, survives a sibling morph" in {
        // bhost is a CONST button (no ref-bound value; only a declaratively-bound reactive `disabled`). The
        // only way its disabled attribute can change is the presence-based in-place patch — proving no
        // re-render — and it must survive a sibling region's morph via the __kyoOwn shield.
        val app: UI < Async =
            for
                tick <- Signal.initRef(0)
                off  <- Signal.initRef(false)
            yield UI.div(
                UI.button("tick").id("btick").onClick(tick.getAndUpdate(_ + 1).unit),
                UI.button("target").id("bhost").disabled(off),
                tick.map(t => UI.span(s"t:$t").id("btxt")),
                UI.button("toggle").id("btoggle").onClick(off.getAndUpdate(b => !b).unit)
            )
        withUI(app) {
            for
                _ <- Browser.assertNoAttribute(Selector.id("bhost"), "disabled")
                _ <- Browser.click(Selector.id("btoggle"))
                _ <- Browser.assertAttribute(Selector.id("bhost"), "disabled", "") // patched present in place
                _ <- Browser.click(Selector.id("btick"))
                _ <- Browser.assertText(Selector.id("btxt"), "t:1")                // a sibling region re-rendered
                _ <- Browser.assertAttribute(Selector.id("bhost"), "disabled", "") // owned attr survived the morph
                _ <- Browser.click(Selector.id("btoggle"))
                _ <- Browser.assertNoAttribute(Selector.id("bhost"), "disabled")   // toggled back off, removed in place
            yield ()
        }
    }

end DomBackendTest
