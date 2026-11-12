package kyo.uic

import kyo.*

/** A skeleton keeps no state and nothing of it decides its structure: its shape and its animation are class channels on the root and
  * its dimensions and radius one inline-style channel, so it draws no region and holds no instance, whatever follows a signal.
  */
class SkeletonTest extends UicTest:

    // Armed: a region that takes the reader's state again fails the leaf.
    override private[uic] def armed: Boolean = true

    private def regions(s: uic.Skeleton)(using Frame): Int < Sync = regionsAbove(s.render, "p-skeleton")

    /** The element's inline style now: its static declarations and what its channel holds. */
    private def styleNow(e: UI.Ast.Element)(using Frame): Style < Sync =
        e.attrs.reactiveStyle match
            case Present(sig) => sig.current.map(e.attrs.uiStyle ++ _)
            case Absent       => Kyo.lift(e.attrs.uiStyle)

    private def dims(width: String, height: String): Style = Style.width(Length.parse(width)).height(Length.parse(height))

    "a width and a height from signals patch the inline style in place, with no region" in {
        for
            width  <- Signal.initRef("40px")
            height <- Signal.initRef("1rem")
            el     <- elementWithClass(uic.Skeleton().width(width).height(height).render, "p-skeleton")
            before <- styleNow(el)
            after  <- width.set("10rem").andThen(height.set("4px")).andThen(styleNow(el))
            n      <- regions(uic.Skeleton().width(width).height(height))
        yield assert(
            before == dims("40px", "1rem") && after == dims("10rem", "4px") && n == 0,
            s"before $before, after $after, $n regions"
        )
    }

    "a size from a signal sets both dimensions and wins over width and height" in {
        for
            size   <- Signal.initRef("2rem")
            el     <- elementWithClass(uic.Skeleton().width("10rem").height("4rem").size(size).render, "p-skeleton")
            before <- styleNow(el)
            after  <- size.set("3px").andThen(styleNow(el))
        yield assert(before == dims("2rem", "2rem") && after == dims("3px", "3px"), s"before $before, after $after")
    }

    "a radius from a signal applies in px or %, and another unit keeps the token radius" in {
        for
            radius <- Signal.initRef("16px")
            el     <- elementWithClass(uic.Skeleton().borderRadius(radius).render, "p-skeleton")
            px     <- styleNow(el)
            rem    <- radius.set("1rem").andThen(styleNow(el))
            pct    <- radius.set("50%").andThen(styleNow(el))
            n      <- regions(uic.Skeleton().borderRadius(radius))
        yield assert(
            px == dims("100%", "1rem").rounded(Length.Px(16)) && rem == dims("100%", "1rem") &&
                pct == dims("100%", "1rem").rounded(Length.Pct(50)) && n == 0,
            s"px $px, rem $rem, pct $pct, $n regions"
        )
    }

    "a shape and an animation from signals swap their classes in place, with no region" in {
        for
            shape    <- Signal.initRef(uic.SkeletonShape.Rectangle)
            animated <- Signal.initRef(true)
            el       <- elementWithClass(uic.Skeleton().shape(shape).animation(animated).render, "p-skeleton")
            before   <- Kyo.foreach(Seq("p-skeleton-circle", "p-skeleton-animation-none"))(carriesClass(el, _))
            after    <- shape.set(uic.SkeletonShape.Circle).andThen(animated.set(false))
                .andThen(Kyo.foreach(Seq("p-skeleton-circle", "p-skeleton-animation-none"))(carriesClass(el, _)))
            n <- regions(uic.Skeleton().shape(shape).animation(animated))
        yield assert(before == Seq(false, false) && after == Seq(true, true) && n == 0, s"before $before, after $after, $n regions")
    }

    "constants draw no region and bind no style channel" in {
        val s = uic.Skeleton().shape(uic.SkeletonShape.Circle).size("4rem").borderRadius("4px").animation(false)
        for
            n  <- regions(s)
            el <- elementWithClass(s.render, "p-skeleton")
        yield assert(n == 0 && el.attrs.reactiveStyle.isEmpty, s"$n regions, channel ${el.attrs.reactiveStyle}")
        end for
    }

    "a skeleton holds no instance of its own" in {
        holdsMount(uic.Skeleton().width("40px").render).map(mount => assert(!mount, s"mount $mount"))
    }
end SkeletonTest
