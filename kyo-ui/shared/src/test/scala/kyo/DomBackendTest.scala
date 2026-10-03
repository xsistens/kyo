package kyo

import kyo.Browser.*

// DomBackend.scala is a JS-only source. These tests exercise its behaviour
// end-to-end via the JVM browser test infrastructure (SSE/event POST cycle).
class DomBackendTest extends UITest:

    /** Counts every `data-kyo-ghost` node the client appends from now on. Ghosts remove themselves on
      * transitionend or a 1s timeout, so polling the DOM for their presence would race the cleanup; a
      * MutationObserver records the spawn itself.
      */
    private val ghostCounterJs =
        "window.__kyoGhostCount=0;" +
            "new MutationObserver(function(ms){ms.forEach(function(m){m.addedNodes.forEach(function(n){" +
            "if(n.nodeType===1&&n.hasAttribute&&n.hasAttribute('data-kyo-ghost'))window.__kyoGhostCount++;" +
            "});});}).observe(document.body,{childList:true,subtree:true})"

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

    "morph reuses an inner element (preserving its DOM-local state) across a re-render of its region" in {
        val app: UI < Async =
            for outer <- Signal.initRef[Int](0)
            yield UI.div(
                outer.map(o =>
                    UI.div(
                        UI.div("keep-me").id("inner"),
                        UI.span(s"o=$o").id("status")
                    )
                ),
                UI.button("bump").id("bump").onClick(outer.getAndUpdate(_ + 1).unit)
            )
        withUI(app) {
            for
                _      <- Browser.assertText(Selector.id("status"), "o=0")
                _      <- Browser.evalDiscard("document.getElementById('inner').__kyoMark = 4242;")
                before <- Browser.evalJson[Int]("document.getElementById('inner').__kyoMark || 0")
                _      <- Browser.evalDiscard("document.getElementById('bump').click()")
                _      <- Browser.assertText(Selector.id("status"), "o=1")
                after  <- Browser.evalJson[Int]("document.getElementById('inner').__kyoMark || 0")
            yield
                assert(before == 4242)
                assert(after == 4242) // node reused; an outerHTML replace would recreate it (mark gone)
        }
    }

    "empty reactive remains updateable without an element placeholder" in {
        val app: UI < Async =
            for show <- Signal.initRef(false)
            yield UI.div(
                UI.when(show)(UI.span("content").id("content"))
            )
        withUI(app) {
            // The empty logical range has no element content.
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

    // The single-consumer drain preserves event ordering; all 5 clicks must be processed in
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

    "nested reactive directly inside a reactive patches independently (no path collision)" in {
        // Two regions must not share a data-kyo-path when a reactive's value is ITSELF a reactive (e.g.
        // `open.render(hi.render(...))`). The `: UI` ascription lifts the inner Signal into a Reactive so the outer value is itself reactive.
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
                // change ONLY inner: before the fix this stayed "o0/i0".
                _ <- Browser.click(Selector.id("set-inner"))
                _ <- Browser.assertText(Selector.id("cell"), "o0/i1")
                _ <- Browser.click(Selector.id("set-outer"))
                _ <- Browser.assertText(Selector.id("cell"), "o1/i1")
            yield ()
        }
    }

    // Mount-slot reconciliation: a keyless UI.mounted and a Reactive share the reactiveContentSegment key, so
    // swapping one for the other collides on the same data-kyo-path. The live mount is preserved ONLY when the
    // incoming top-down node is itself a mount slot (data-kyo-mount-slot); otherwise the stale mount is removed.

    "a region swapping a keyless mount for colliding reactive content removes the stale mount DOM" in {
        val app: UI < Async =
            for
                sel   <- Signal.initRef("a")
                inner <- Signal.initRef("B-content")
            yield UI.div(
                UI.button("swap").id("swap").onClick(sel.set("b")),
                sel.map {
                    // both branches are reactive content -> they reconcile at the SAME positional key
                    case "a" =>
                        UI.mounted {
                            Signal.initRef(0).map(_ => UI.span("A-content").id("mount-a"))
                        }.placeholder(UI.empty): UI
                    case _ =>
                        inner.map(v => UI.span(v).id("plain-b")): UI
                }
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("mount-a"), "A-content") // mount painted (data-kyo-mount)
                _ <- Browser.click(Selector.id("swap"))
                _ <- Browser.assertText(Selector.id("plain-b"), "B-content")
                _ <- Browser.assertNotExists(Selector.id("mount-a"))         // stale mount DOM gone
            yield ()
        }
    }

    "a region closing a gate over a mount removes the mount DOM" in {
        // Empty-slot path, keyed on the ABSENCE of a mount-slot marker: open -> false yields no mount, so the morph empties it.
        val app: UI < Async =
            for open <- Signal.initRef(true)
            yield UI.div(
                UI.button("gclose").id("gclose").onClick(open.set(false)),
                open.map {
                    case true  => UI.mounted { Signal.initRef(0).map(_ => UI.span("panel").id("gpanel")) }.placeholder(UI.empty): UI
                    case false => UI.empty: UI
                }
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("gpanel"), "panel")
                _ <- Browser.click(Selector.id("gclose"))
                _ <- Browser.assertNotExists(Selector.id("gpanel"))
            yield ()
        }
    }

    "a re-rendered region keeps a mount that is still present (no wipe)" in {
        // Safety companion: the SAME mount stays across a region re-render. Its placeholder carries the mount-slot
        // marker, so the guard preserves the live mount (the legitimate case the opaque-mount guard exists for).
        val app: UI < Async =
            for tick <- Signal.initRef(0)
            yield UI.div(
                UI.button("ktick").id("ktick").onClick(tick.getAndUpdate(_ + 1).unit),
                tick.map { t =>
                    UI.div(
                        UI.span(s"t:$t").id("ktxt"),
                        UI.mounted { Signal.initRef(0).map(_ => UI.span("kept").id("kpanel")) }.placeholder(UI.empty)
                    ): UI
                }
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("kpanel"), "kept")
                _ <- Browser.click(Selector.id("ktick"))
                _ <- Browser.assertText(Selector.id("ktxt"), "t:1")
                _ <- Browser.assertText(Selector.id("kpanel"), "kept") // mount preserved across the morph
            yield ()
        }
    }

    "a keyed mount whose key changes still resets its slot" in {
        // The other half of the `k` contract: opacity must NOT outlive the key. A changed key evicts the
        // instance, so the span has to fall through to the morph, otherwise the evicted instance's
        // content would stay painted forever. Node identity must NOT survive here.
        val app: UI < Async =
            for which <- Signal.initRef("a")
            yield UI.div(
                UI.button("swap").id("kswap").onClick(which.set("b")),
                which.map(w =>
                    UI.mounted(Kyo.lift[UI, Async](UI.div(s"body-$w").id("kbody")))
                        .keyed(w)
                        .placeholder(UI.span("..."))
                )
            )
        withUI(app) {
            for
                _      <- Browser.assertText(Selector.id("kbody"), "body-a")
                _      <- Browser.evalDiscard("document.getElementById('kbody').__kyoMark = 9;")
                _      <- Browser.click(Selector.id("kswap"))
                _      <- Browser.assertText(Selector.id("kbody"), "body-b")
                marked <- Browser.evalJson[Int]("document.getElementById('kbody').__kyoMark || 0")
            yield assert(marked == 0)
        }
    }

    // Leave ghosts vs mount repaints. Two mechanisms conspire to keep a repaint from faking a departure:
    // the mount region's OWN republish never prepares ghosts at all (bookkeeping, not leaving), and the
    // ENCLOSING region's repaint prepares them but drops any whose source the morph preserved. Both are
    // needed here: an enclosing repaint republishes the mount AND predicts a ghost for the mount's
    // leave-marked content, which the opaque mount boundary then keeps alive.

    "closing a gate above a keyless mount still plays the leave ghost" in {
        val app: UI < Async =
            for open <- Signal.initRef(true)
            yield UI.div(
                UI.button("gclose2").id("gclose2").onClick(open.set(false)),
                UI.when(open)(
                    UI.mounted {
                        Signal.initRef(0).map(_ => UI.div("panel").id("gopanel").leaveTransition("ghost-probe-leave"): UI)
                    }.placeholder(UI.empty)
                )
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("gopanel"), "panel")
                _ <- Browser.evalDiscard(ghostCounterJs)
                _ <- Browser.click(Selector.id("gclose2"))
                _ <- Browser.assertNotExists(Selector.id("gopanel"))
                g <- Browser.evalJson[Int]("window.__kyoGhostCount")
            yield assert(g == 1)
        }
    }

    "an adopted keyed mount keeps its live DOM across a parent re-render" in {
        // The `m`+`k` marker contract. A keyed mount survives its enclosing region's re-render as an
        // INSTANCE (the effect does not re-run), but until the region claimed `m`, the parent's paint
        // projected the mount to its placeholder and morphed the live subtree away, so every node below
        // it was recreated. Focus, caret, scroll and in-place bindings all died with it. Pinned here by
        // node identity: the expando survives only if the parent left the span alone.
        val app: UI < Async =
            for tick <- Signal.initRef[Int](0)
            yield UI.div(
                UI.button("tick").id("ktick").onClick(tick.getAndUpdate(_ + 1).unit),
                tick.map(t =>
                    UI.div(
                        UI.span(s"t:$t").id("ktxt"),
                        UI.mounted(Kyo.lift[UI, Async](UI.div("live").id("kinner")))
                            .keyed("stable")
                            .placeholder(UI.span("..."))
                    )
                )
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("kinner"), "live")
                // The FIRST re-render is the one that matters. A named slot is opaque from this pass on,
                // not from the next: the render that emitted it named the instance that owns it, so waiting
                // for the client's own `m` would cost one destructive morph per slot, and the regions inside
                // the discarded subtree would leave the registry with it.
                _      <- Browser.evalDiscard("document.getElementById('kinner').__kyoMark = 7;")
                _      <- Browser.click(Selector.id("ktick"))
                _      <- Browser.assertText(Selector.id("ktxt"), "t:1")
                first  <- Browser.evalJson[Int]("document.getElementById('kinner').__kyoMark || 0")
                _      <- Browser.evalDiscard("document.getElementById('kinner').__kyoMark = 7;")
                before <- Browser.evalJson[Int]("document.getElementById('kinner').__kyoMark || 0")
                // Steady state: the live marker names the same mount the incoming slot does, so the parent
                // leaves the span alone entirely.
                _     <- Browser.click(Selector.id("ktick"))
                _     <- Browser.assertText(Selector.id("ktxt"), "t:2")
                _     <- Browser.assertText(Selector.id("kinner"), "live")
                after <- Browser.evalJson[Int]("document.getElementById('kinner').__kyoMark || 0")
            yield
                assert(first == 7, "the first parent re-render already leaves the named slot alone")
                assert(before == 7)
                assert(after == 7)
        }
    }

    "a keyless mount republish does not ghost leave-marked content (enclosing region repaint)" in {
        val app: UI < Async =
            for tick <- Signal.initRef(0)
            yield UI.div(
                UI.button("gtick").id("gtick").onClick(tick.getAndUpdate(_ + 1).unit),
                tick.map { t =>
                    UI.div(
                        UI.span(s"t:$t").id("gtxt"),
                        UI.mounted {
                            Signal.initRef(0).map(_ =>
                                // Effect shape is fragment(marker, panel), so the panel's path differs from
                                // the placeholder's and the survivor set cannot match it.
                                UI.fragment(
                                    UI.span("m").id("gmark"),
                                    UI.div("panel").id("glpanel").leaveTransition("ghost-probe-leave")
                                ): UI
                            )
                        }.placeholder(UI.div("panel").id("glpanel").leaveTransition("ghost-probe-leave"))
                    ): UI
                }
            )
        withUI(app) {
            for
                _ <- Browser.assertText(Selector.id("glpanel"), "panel")
                _ <- Browser.evalDiscard(ghostCounterJs)
                _ <- Browser.click(Selector.id("gtick"))
                // The enclosing region repainted, which republished the mount.
                _ <- Browser.assertText(Selector.id("gtxt"), "t:1")
                _ <- Browser.assertText(Selector.id("glpanel"), "panel")
                g <- Browser.evalJson[Int]("window.__kyoGhostCount")
            yield assert(g == 0)
        }
    }

    "declarative reactive attribute patches a const element in place (no re-render), survives a sibling morph" in {
        val app: UI < Async =
            for
                tick <- Signal.initRef(0)
                lbl  <- Signal.initRef("first")
            yield UI.div(
                UI.button("tick").id("dtick").onClick(tick.getAndUpdate(_ + 1).unit),
                // dhost is CONST (only a reactive aria-label, no ref-bound value), so its label can change only
                // via the in-place attr patch, proving no re-render/re-mount.
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
                _ <- Browser.assertText(Selector.id("dtxt"), "t:1")
                _ <- Browser.assertAttribute(Selector.id("dhost"), "aria-label", "second") // owned attr unaffected by the morph
            yield ()
        }
    }

    "declarative reactive attribute on an element produced INSIDE a UI.mounted block patches in place" in {
        // The topology of a picker or a select: the reactive-attr element is produced by an
        // effectful UI.mounted body, NOT at the top level. Pins that the attr observer forks while walking
        // mounted content, so the aria-label still patches in place (no re-mount) on emission.
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
                _ <- Browser.assertText(Selector.id("mchild"), "y") // child untouched, no re-mount
            yield ()
        }
    }

    "declarative reactive boolean attribute (disabled) toggles in place, survives a sibling morph" in {
        // bhost is a CONST button (only a reactive `disabled`), so its disabled attribute can change only via
        // the presence-based in-place patch (proving no re-render) and must survive a sibling morph.
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
                _ <- Browser.assertText(Selector.id("btxt"), "t:1")
                _ <- Browser.assertAttribute(Selector.id("bhost"), "disabled", "") // owned attr survived the morph
                _ <- Browser.click(Selector.id("btoggle"))
                _ <- Browser.assertNoAttribute(Selector.id("bhost"), "disabled")   // toggled back off, removed in place
            yield ()
        }
    }

    "declarative reactive class toggles in place, survives a sibling morph" in {
        // chost is a CONST div with a static class plus a reactive class, so the reactive class can change only
        // via the in-place classList.toggle patch (proving no re-render), and must survive a sibling morph.
        val app: UI < Async =
            for
                tick <- Signal.initRef(0)
                on   <- Signal.initRef(false)
            yield UI.div(
                UI.button("tick").id("ctick").onClick(tick.getAndUpdate(_ + 1).unit),
                UI.div.id("chost").cssClass("base").cssClass("hot", on)(UI.span("z").id("cchild")),
                tick.map(t => UI.span(s"t:$t").id("ctxt")),
                UI.button("toggle").id("ctoggle").onClick(on.getAndUpdate(b => !b).unit)
            )
        withUI(app) {
            for
                _ <- Browser.assertAttributeSatisfies(Selector.id("chost"), "class", "no hot initially")(c => !c.contains("hot"))
                _ <- Browser.click(Selector.id("ctoggle"))
                _ <- Browser.assertAttributeSatisfies(Selector.id("chost"), "class", "hot added in place")(_.contains("hot"))
                _ <- Browser.click(Selector.id("ctick"))
                _ <- Browser.assertText(Selector.id("ctxt"), "t:1")
                _ <- Browser.assertAttributeSatisfies(Selector.id("chost"), "class", "hot survives morph")(_.contains("hot"))
                _ <- Browser.click(Selector.id("ctoggle"))
                _ <- Browser.assertAttributeSatisfies(Selector.id("chost"), "class", "hot removed in place")(c => !c.contains("hot"))
            yield ()
        }
    }

    // The sibling `tick` signal bumps the reactive region containing `host`, so morphAttrs runs against server HTML
    // that lacks the owned class, proving the ownership guard shields the client-set class from reconciliation.
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
                _ <- Browser.assertText(Selector.id("txt"), "t:1") // the element was morphed
                _ <- Browser.assertAttribute(Selector.id("host"), "class", "owned-cls")
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

end DomBackendTest
