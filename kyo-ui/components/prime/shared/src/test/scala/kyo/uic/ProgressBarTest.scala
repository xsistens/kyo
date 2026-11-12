package kyo.uic

import kyo.*

/** A progress bar keeps no state, and only whether its label shows decides its structure: the mode is a class channel on the root,
  * the percent shown the `aria-valuenow` and fill inline-style channels, the label's text a text channel, and the label itself a
  * region below the fill over whether it shows, so a value moving within 1 to 100 draws nothing again.
  */
class ProgressBarTest extends UicTest:

    // Armed: a region that takes the reader's state again fails the leaf.
    override private[uic] def armed: Boolean = true

    /** The element's inline style now: its static declarations and what its channel holds. */
    private def styleNow(e: UI.Ast.Element)(using Frame): Style < Sync =
        e.attrs.reactiveStyle match
            case Present(sig) => sig.current.map(e.attrs.uiStyle ++ _)
            case Absent       => Kyo.lift(e.attrs.uiStyle)

    /** The bar as it shows now: `aria-valuenow`, the fill's style, and the label's text where a label is drawn. */
    private def shows(bar: uic.ProgressBar)(using Frame): (Maybe[String], Style, Maybe[String]) < Sync =
        for
            root   <- elementWithClass(bar.render, "p-progressbar")
            now    <- ariaNow(root, "valuenow")
            fill   <- elementWithClass(bar.render, "p-progressbar-value").map(styleNow)
            labels <- elementsWithClass(bar.render, "p-progressbar-label")
            text   <- Kyo.foreach(labels)(textNow)
        yield (now, fill, Maybe.fromOption(text.headOption))

    private def width(v: Int): Style = Style.width(Length.Pct(v.toDouble))

    "a value from a signal patches aria-valuenow, the fill and the label's text in place, with no region around the bar" in {
        for
            value <- Signal.initRef(40)
            bar = uic.ProgressBar().value(value)
            before <- shows(bar)
            after  <- value.set(150).andThen(shows(bar))
            above  <- regionsAbove(bar.render, "p-progressbar")
            label  <- regionsAbove(bar.render, "p-progressbar-label")
        yield assert(
            before == (Present("40"), width(40), Present("40%")) && after == (Present("100"), width(100), Present("100%")) &&
                above == 0 && label == 1,
            s"before $before, after $after, $above regions above the bar, $label above the label"
        )
    }

    "a value moving within 1 to 100 draws no element again, and only crossing zero draws the label's region" in {
        val log                                              = new kyo.internal.RecordingExchange
        def patched(name: String, v: String): Boolean < Sync = Sync.defer(UicTest.patchedIn(log).contains((name, v)))
        def paints: List[UI] < Sync                          = Sync.defer(UicTest.publishedIn(log))
        // A text channel paints its text alone; a region that runs again paints elements.
        def textPainted(text: String): Boolean < Sync = paints.map(_.exists {
            case t: UI.Ast.Text => t.value == text
            case _              => false
        })
        def drawn(trees: List[UI]): Int < Sync =
            Kyo.foreach(trees)(t => UicTest.walkingLive.let(true)(elements(t)).map(_.size)).map(_.sum)
        for
            value <- Signal.initRef(40)
            bar = uic.ProgressBar().value(value)
            seen <- headlessCommands.map { cmds =>
                kyo.internal.ReactiveUI.subscribed(UI.div(bar.render), log, commands = Present(cmds)) { _ =>
                    for
                        _        <- waitFor(value.waiters.map(_ > 0))
                        first    <- paints.map(_.size)
                        _        <- value.set(60)
                        _        <- waitFor(patched("aria-valuenow", "60"))
                        _        <- waitFor(textPainted("60%"))
                        _        <- waitFor(value.waiters.map(_ > 0))
                        moved    <- paints
                        _        <- value.set(0)
                        _        <- waitFor(patched("aria-valuenow", "0"))
                        _        <- waitFor(paints.map(_.size > moved.size))
                        _        <- waitFor(value.waiters.map(_ > 0))
                        crossed  <- paints
                        atMove   <- drawn(moved.take(moved.size - first))
                        atZero   <- Kyo.lift(crossed.take(crossed.size - moved.size))
                        labelled <- Kyo.foreach(atZero)(t => UicTest.walkingLive.let(true)(elementsWithClass(t, "p-progressbar-label")))
                    yield (atMove, atZero.size, labelled.map(_.size).sum)
                }
            }
        yield assert(
            seen == (0, 1, 0),
            s"(elements drawn for 40 to 60, paints for 60 to 0, labels in them) $seen, expected (0, 1, 0)"
        )
        end for
    }

    "a mode from a signal swaps the class, the fill, aria-valuenow and the label in place, with no region around the bar" in {
        for
            mode <- Signal.initRef(uic.ProgressBarMode.Determinate)
            bar = uic.ProgressBar().value(40).mode(mode)
            root <- elementWithClass(bar.render, "p-progressbar")
            names = Seq("p-progressbar-determinate", "p-progressbar-indeterminate")
            det    <- Kyo.foreach(names)(carriesClass(root, _))
            shown  <- shows(bar)
            indet  <- mode.set(uic.ProgressBarMode.Indeterminate).andThen(Kyo.foreach(names)(carriesClass(root, _)))
            hidden <- shows(bar)
            again  <- mode.set(uic.ProgressBarMode.Determinate).andThen(shows(bar))
            above  <- regionsAbove(bar.render, "p-progressbar")
        yield assert(
            det == Seq(true, false) && indet == Seq(false, true) &&
                shown ==
                (Present("40"), width(40), Present("40%")) &&
                hidden ==
                (Absent, Style.empty, Absent) && again == shown && above == 0,
            s"determinate $det $shown, indeterminate $indet $hidden, again $again, $above regions"
        )
    }

    "showValue from a signal draws or drops the label in its region, with no region around the bar" in {
        for
            showing <- Signal.initRef(true)
            bar = uic.ProgressBar().value(40).showValue(showing).valueTemplate(v => s"$v of 100")
            on    <- shows(bar)
            off   <- showing.set(false).andThen(shows(bar))
            above <- regionsAbove(bar.render, "p-progressbar")
        yield assert(
            on == (Present("40"), width(40), Present("40 of 100")) && off == (Present("40"), width(40), Absent) && above == 0,
            s"on $on, off $off, $above regions"
        )
    }

    "constants draw no region and bind no channel" in {
        val bars = Seq(
            uic.ProgressBar().value(40).valueTemplate(v => s"$v of 100").accessibleName("Upload"),
            uic.ProgressBar().value(0),
            uic.ProgressBar().mode(uic.ProgressBarMode.Indeterminate)
        )
        Kyo.foreach(bars) { bar =>
            for
                nodes <- reactiveNodes(bar.render)
                root  <- elementWithClass(bar.render, "p-progressbar")
                fill  <- elementWithClass(bar.render, "p-progressbar-value")
            yield nodes.isEmpty && root.attrs.reactiveAttrs.isEmpty && root.attrs.reactiveMaybeAttrs.isEmpty &&
                fill.attrs.reactiveStyle.isEmpty
        }.map(ok => assert(ok.forall(identity), s"constant bars without a channel: $ok"))
    }

    "an id lands on the root, and with none set the root has no id" in {
        for
            named <- elementWithClass(uic.ProgressBar().value(40).id("upload").render, "p-progressbar")
            plain <- elementWithClass(uic.ProgressBar().value(40).render, "p-progressbar")
            html  <- renderHtml(uic.ProgressBar().value(40).id("upload").render)
        yield assert(
            named.attrs.identifier.contains("upload") && plain.attrs.identifier.isEmpty && html.contains("""id="upload""""),
            s"named ${named.attrs.identifier}, plain ${plain.attrs.identifier}: $html"
        )
    }

    "a progress bar holds no instance of its own" in {
        holdsMount(uic.ProgressBar().value(40).render).map(mount => assert(!mount, s"mount $mount"))
    }
end ProgressBarTest
