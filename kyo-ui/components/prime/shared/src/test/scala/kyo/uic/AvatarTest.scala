package kyo.uic

import kyo.*

/** An avatar keeps no state, and what decides its structure is fixed when it is built: its shape, size and `disabled` are class
  * channels on the root, its initials a text channel, its name the `aria-label` and the image's `alt` channels, its image the `src`
  * channel, and an icon signal swaps the glyph in a region of its own below it. A control's tab stop and silence follow `disabled`
  * through their channels too.
  */
class AvatarTest extends UicTest:

    // Armed: a region that takes the reader's state again fails the leaf.
    override private[uic] def armed: Boolean = true

    "a shape, a size and disabled from signals swap their classes in place, with no region" in {
        for
            shape <- Signal.initRef(uic.AvatarShape.Square)
            size  <- Signal.initRef(uic.Size.Normal: uic.ExtendedSize)
            off   <- Signal.initRef(false)
            avatar = uic.Avatar().initials("AL").shape(shape).size(size).disabled(off)
            el <- elementWithClass(avatar.render, "p-avatar")
            names = Seq("p-avatar-circle", "p-avatar-xl", "p-disabled")
            before <- Kyo.foreach(names)(carriesClass(el, _))
            after  <- shape.set(uic.AvatarShape.Circle).andThen(size.set(uic.Size.XLarge)).andThen(off.set(true))
                .andThen(Kyo.foreach(names)(carriesClass(el, _)))
            html <- renderHtml(avatar.render)
            n    <- regionsAbove(avatar.render, "p-avatar")
        yield assert(
            before == Seq(false, false, false) && after == Seq(true, true, true) && html.contains("""aria-disabled="true"""") &&
                n == 0,
            s"before $before, after $after, $n regions: $html"
        )
    }

    "disabled from a signal takes a control out of the tab order and silences it in place, with no region" in {
        for
            off <- Signal.initRef(false)
            avatar = uic.Avatar().initials("AL").disabled(off).onClick(())
            on   <- renderHtml(avatar.render)
            n    <- regionsAbove(avatar.render, "p-avatar")
            lock <- off.set(true).andThen(renderHtml(avatar.render))
        yield assert(
            on.contains("""tabindex="0"""") && on.contains("data-kyo-ev") && !on.contains("p-disabled") &&
                !lock.contains("tabindex") && !lock.contains("data-kyo-ev") && lock.contains("p-disabled") && n == 0,
            s"$n regions: $on | $lock"
        )
    }

    "initials from a signal patch the text and the name in place" in {
        for
            initials <- Signal.initRef("AL")
            avatar = uic.Avatar().initials(initials)
            before <- renderHtml(avatar.render)
            after  <- initials.set("JD").andThen(renderHtml(avatar.render))
            n      <- regionsAbove(avatar.render, "p-avatar")
        yield assert(
            before.contains("""-->AL<!--kyo-re""") && before.contains("""aria-label="AL"""") &&
                after.contains("""-->JD<!--kyo-re""") && after.contains("""aria-label="JD"""") && n == 0,
            s"$n regions: $before | $after"
        )
    }

    "an image and a name from signals patch the src and the alt in place, with no region" in {
        for
            url  <- Signal.initRef("/a.png")
            name <- Signal.initRef("Ada")
            avatar = uic.Avatar().image(url).accessibleName(name)
            before <- renderHtml(avatar.render)
            after  <- url.set("/b.png").andThen(name.set("Grace")).andThen(renderHtml(avatar.render))
            n      <- regionsAbove(avatar.render, "p-avatar")
        yield assert(
            before.contains("""src="/a.png"""") && before.contains("""alt="Ada"""") && before.contains("""aria-label="Ada"""") &&
                after.contains("""src="/b.png"""") && after.contains("""alt="Grace"""") && after.contains("""aria-label="Grace"""") &&
                n == 0,
            s"$n regions: $before | $after"
        )
    }

    "an icon from a signal swaps the glyph in its own region, below the avatar" in {
        for
            glyph <- Signal.initRef(uic.Icons.user)
            avatar = uic.Avatar().icon(glyph)
            above  <- regionsAbove(avatar.render, "p-avatar")
            inside <- regionsAbove(avatar.render, "p-avatar-icon")
            after  <- glyph.set(uic.Icons.check).andThen(renderHtml(avatar.render))
        yield assert(
            above == 0 && inside == 1 && after.contains(s"""data-uic-icon="${uic.Icons.check.name}""""),
            s"$above regions above the avatar, $inside above the icon: $after"
        )
    }

    "constants draw no region and no reactive node, a control included" in {
        val avatars = Seq(
            uic.Avatar().initials("AL").size(uic.Size.Large).shape(uic.AvatarShape.Circle).disabled(true),
            uic.Avatar().image("/a.png").accessibleName("Ada"),
            uic.Avatar().initials("AL").onClick(()).disabled(true)
        )
        for
            ns    <- Kyo.foreach(avatars)(a => regionsAbove(a.render, "p-avatar"))
            nodes <- Kyo.foreach(avatars)(a => reactiveNodes(a.render).map(_.size))
        yield assert(ns.forall(_ == 0) && nodes.forall(_ == 0), s"regions $ns, reactive nodes $nodes")
        end for
    }

    "an id lands on the root" in {
        renderHtml(uic.Avatar().initials("AL").id("me").render).map(html =>
            assert(html.contains("""id="me"""") && html.contains("p-avatar"), html)
        )
    }

    "an avatar holds no instance of its own" in {
        holdsMount(uic.Avatar().initials("AL").onClick(()).render).map(mount => assert(!mount, s"mount $mount"))
    }
end AvatarTest
