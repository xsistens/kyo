package kyo.uic

import kyo.*

/** A spinner keeps no state and nothing of it decides its structure: its size and its accessible name are channels on the root, so it
  * draws no region and holds no instance, whatever follows a signal.
  */
class ProgressSpinnerTest extends UicTest:

    // Armed: a region that takes the reader's state again fails the leaf.
    override private[uic] def armed: Boolean = true

    private def regions(s: uic.ProgressSpinner)(using Frame): Int < Sync = regionsAbove(s.render, "p-progressspinner")

    "a size from a signal swaps its class in place, with no region" in {
        for
            size  <- Signal.initRef[Size](Size.Normal)
            el    <- elementWithClass(uic.ProgressSpinner().size(size).render, "p-progressspinner")
            small <-
                size.set(Size.Small).andThen(Kyo.foreach(Seq("p-uic-progressspinner-sm", "p-uic-progressspinner-lg"))(carriesClass(el, _)))
            large <-
                size.set(Size.Large).andThen(Kyo.foreach(Seq("p-uic-progressspinner-sm", "p-uic-progressspinner-lg"))(carriesClass(el, _)))
            plain <-
                size.set(Size.Normal).andThen(Kyo.foreach(Seq("p-uic-progressspinner-sm", "p-uic-progressspinner-lg"))(carriesClass(el, _)))
            n <- regions(uic.ProgressSpinner().size(size))
        yield assert(
            small == Seq(true, false) && large == Seq(false, true) && plain == Seq(false, false) && n == 0,
            s"small $small, large $large, normal $plain, $n regions"
        )
    }

    "an accessible name from a signal patches aria-label in place, with no region" in {
        for
            name   <- Signal.initRef("Loading")
            el     <- elementWithClass(uic.ProgressSpinner().accessibleName(name).render, "p-progressspinner")
            before <- ariaNow(el, "label")
            after  <- name.set("Saving").andThen(ariaNow(el, "label"))
            n      <- regions(uic.ProgressSpinner().accessibleName(name))
        yield assert(before == Present("Loading") && after == Present("Saving") && n == 0, s"before $before, after $after, $n regions")
    }

    "constants draw no region and no label when none is set" in {
        for
            n     <- regions(uic.ProgressSpinner().size(Size.Large).accessibleName("Loading"))
            el    <- elementWithClass(uic.ProgressSpinner().render, "p-progressspinner")
            label <- ariaNow(el, "label")
        yield assert(n == 0 && label == Absent, s"$n regions, label $label")
    }

    "a spinner holds no instance of its own" in {
        holdsMount(uic.ProgressSpinner().size(Size.Small).render).map(mount => assert(!mount, s"mount $mount"))
    }
end ProgressSpinnerTest
