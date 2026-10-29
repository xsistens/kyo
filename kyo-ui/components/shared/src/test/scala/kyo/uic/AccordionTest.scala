package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Accordion]]'s open panels under every way they can be bound, in single mode and, through [[MultipleAccordion]], in
  * multiple mode: unbound they are the accordion's own, a constant nobody hears is fixed, a signal is followed and a click
  * only reported, and a ref is written before the report.
  */
class AccordionTest extends ReactiveValueContract:

    private def accordion(using Frame) = uic.Accordion[String]().panel("Alpha", "a")(p("one")).panel("Beta", "b")(p("two"))

    private def headers(t: UI)(using Frame) = elementsWithClass(t, "p-accordionheader")

    /** The accordion as its mount publishes it, the headers wired. */
    private def mounted(ui: UI)(using Frame, kyo.test.AssertScope): UI < Async =
        mountedWhen(ui)(t => headers(t).map(_.exists(declaresClick)))

    /** The panel shown open, by its id: the accordion here has two, Alpha and Beta. */
    private def open(t: UI)(using Frame): Maybe[String] < Sync =
        elementsWithClass(t, "p-accordionpanel-active").map(ps =>
            Maybe.fromOption(ps.headOption.map(p => if textOf(p).contains("Alpha") then "a" else "b"))
        )

    holds(new Slot[Accordion[String], Maybe[String]](
        name = "value",
        bare = accordion,
        bind = _.value(_),
        hear = (a, f) => a.onValueChange(f),
        event = "onValueChange",
        render = _.render,
        start = Absent,
        next = Present("a"),
        control = headers(_).map(_.head),
        shows = open(_),
        ready = Present(headers(_).map(_.exists(declaresClick))),
        nextPatch = Present(_ == ("aria-expanded", "true"))
    ))

    "unbound, a second click on the open header closes it again" in {
        for
            seen <- Signal.initRef(List.empty[Maybe[String]])
            t    <- mounted(accordion.onValueChange(v => seen.updateAndGet(_ :+ v).unit).render)
            hs   <- headers(t)
            _    <- click(hs.head)
            _    <- click(hs.head)
            s    <- seen.get
        yield assert(s == List(Present("a"), Absent), s"reported $s")
    }

    "a fixed accordion holds every header to it, not only the one a gesture reaches" in {
        for hs <- headers(accordion.value(Present("a")).render)
        yield
            assert(hs.size == 2 && hs.forall(h => !declaresClick(h)), "nothing opens or closes")
            assert(hs.forall(_.attrs.ariaAttrs.get("disabled").contains("true")), "and the headers say so")
    }

    "a set bound one way is followed, and a click is reported without being written" in {
        for
            held <- Signal.initRef(Set("b"))
            seen <- Signal.initRef(Absent: Maybe[Set[String]])
            t    <- mounted(accordion.value(held.readOnly).onValueChange(v => seen.set(Present(v))).render)
            hs   <- headers(t)
            _    <- click(hs.head)
            h    <- held.get
            s    <- seen.get
        yield assert(h == Set("b") && s == Present(Set("a", "b")), s"held $h, reported $s")
    }

    "a ref is written before the accordion reports it, and onToggle comes after both" in {
        for
            held  <- Signal.initRef(Maybe.empty[String])
            order <- Signal.initRef(List.empty[String])
            t     <- mounted(
                accordion.value(held)
                    .onValueChange(v => held.get.map(h => order.updateAndGet(_ :+ s"change $v held $h").unit))
                    .onToggle(id => order.updateAndGet(_ :+ s"toggle $id").unit)
                    .render
            )
            hs <- headers(t)
            _  <- click(hs.head)
            o  <- order.get
        yield assert(o == List("change a held a", "toggle a"), s"order: $o")
    }

    "a value bound one way follows the signal: a write from outside opens the panel it names" in {
        for
            held   <- Signal.initRef(Maybe.empty[String])
            opened <- attrPatchedAfter(
                accordion.value(held.readOnly).onValueChange(_ => ()).render,
                t => headers(t).map(_.exists(declaresClick))
            )(_ => held.set(Present("b")))((n, v) => n.endsWith("expanded") && v == "true")
        yield assert(opened, "the panel the signal names never opened")
    }

    "a set bound one way follows the signal: a write from outside opens the panels it names" in {
        for
            held   <- Signal.initRef(Set.empty[String])
            opened <- attrPatchedAfter(
                accordion.value(held.readOnly).onValueChange(_ => ()).render,
                t => headers(t).map(_.exists(declaresClick))
            )(_ => held.set(Set("a")))((n, v) => n.endsWith("expanded") && v == "true")
        yield assert(opened, "the panel the signal names never opened")
    }

    "single mode keeps one panel open: opening the second closes the first" in {
        for
            held <- Signal.initRef(Maybe.empty[String])
            seen <- Signal.initRef(List.empty[Maybe[String]])
            t    <- mounted(accordion.value(held).onValueChange(v => seen.updateAndGet(_ :+ v).unit).render)
            hs   <- headers(t)
            _    <- click(hs(0))
            _    <- click(hs(1))
            h    <- held.get
            s    <- seen.get
        yield
            assert(h == Present("b"), s"held $h")
            assert(s == List(Present("a"), Present("b")), s"reported $s")
        end for
    }

    "multiple mode toggles membership in a ref, and reports the whole set each time" in {
        for
            held <- Signal.initRef(Set.empty[String])
            seen <- Signal.initRef(List.empty[Set[String]])
            t    <- mounted(accordion.value(held).onValueChange(v => seen.updateAndGet(_ :+ v).unit).render)
            hs   <- headers(t)
            _    <- click(hs(0))
            _    <- click(hs(1))
            _    <- click(hs(0))
            h    <- held.get
            s    <- seen.get
        yield
            assert(h == Set("b"), s"held $h")
            assert(s == List(Set("a"), Set("a", "b"), Set("b")), s"reported $s")
        end for
    }

end AccordionTest
