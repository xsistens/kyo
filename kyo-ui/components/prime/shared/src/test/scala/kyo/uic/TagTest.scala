package kyo.uic

import kyo.*

/** A tag keeps no state and nothing of it decides its structure once it is built: its severity and its rounding are class channels
  * on the root, its label a text channel, and an icon signal swaps the glyph in a region of its own below the tag, so the tag draws
  * no region and holds no instance, whatever follows a signal.
  */
class TagTest extends UicTest:

    // Armed: a region that takes the reader's state again fails the leaf.
    override private[uic] def armed: Boolean = true

    "a severity and a rounding from signals swap their classes in place, with no region" in {
        for
            sev   <- Signal.initRef(uic.Severity.Info)
            round <- Signal.initRef(false)
            tag = uic.Tag("t").severity(sev).rounded(round)
            el <- elementWithClass(tag.render, "p-tag")
            names = Seq("p-tag-info", "p-tag-contrast", "p-tag-rounded")
            before <- Kyo.foreach(names)(carriesClass(el, _))
            after  <- sev.set(uic.Severity.Contrast).andThen(round.set(true)).andThen(Kyo.foreach(names)(carriesClass(el, _)))
            help   <- sev.set(uic.Severity.Help).andThen(Kyo.foreach(names)(carriesClass(el, _)))
            n      <- regionsAbove(tag.render, "p-tag")
        yield assert(
            before == Seq(true, false, false) && after == Seq(false, true, true) && help == Seq(false, false, true) && n == 0,
            s"before $before, after $after, help $help, $n regions"
        )
    }

    "an icon from a signal swaps the glyph in its own region, below the tag" in {
        for
            glyph <- Signal.initRef(uic.Icons.check)
            tag = uic.Tag("t").icon(glyph)
            above  <- regionsAbove(tag.render, "p-tag")
            inside <- regionsAbove(tag.render, "p-tag-icon")
            before <- renderHtml(tag.render)
            after  <- glyph.set(uic.Icons.times).andThen(renderHtml(tag.render))
        yield assert(
            above == 0 && inside == 1 &&
                before.contains(s"""data-uic-icon="${uic.Icons.check.name}"""") &&
                after.contains(s"""data-uic-icon="${uic.Icons.times.name}""""),
            s"$above regions above the tag, $inside above the icon: $before | $after"
        )
    }

    "constants draw no region and no reactive node" in {
        val tag = uic.Tag("Done").severity(uic.Severity.Success).rounded(true).icon(uic.Icons.check).onClick(())
        for
            n     <- regionsAbove(tag.render, "p-tag")
            nodes <- reactiveNodes(tag.render)
        yield assert(n == 0 && nodes.isEmpty, s"$n regions, ${nodes.size} reactive nodes")
        end for
    }

    "an unset severity is the primary one" in {
        for
            unset   <- renderHtml(uic.Tag("t").render)
            primary <- renderHtml(uic.Tag("t").severity(uic.Severity.Primary).render)
        yield assert(unset == primary, s"$unset | $primary")
    }

    "a tag holds no instance of its own" in {
        holdsMount(uic.Tag("t").onClick(()).render).map(mount => assert(!mount, s"mount $mount"))
    }
end TagTest
