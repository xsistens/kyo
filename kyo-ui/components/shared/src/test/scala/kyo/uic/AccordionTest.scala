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

    /** The panels shown open, by their ids: the accordion here has two, Alpha and Beta. A mounted accordion patches the
      * open class in place, so it is read as a class channel too.
      */
    private def opened(t: UI)(using Frame): Set[String] < Sync =
        elementsWithClass(t, "p-accordionpanel").map(ps =>
            Kyo.foreach(ps)(p =>
                carriesClass(p, "p-accordionpanel-active").map(on =>
                    if on then Set(if textOf(p).contains("Alpha") then "a" else "b") else Set.empty[String]
                )
            ).map(_.foldLeft(Set.empty[String])(_ ++ _))
        )

    /** The one panel shown open, in single mode. */
    private def open(t: UI)(using Frame): Maybe[String] < Sync = opened(t).map(ids => Maybe.fromOption(ids.headOption))

    private def valueCase(name: String, hear: (Accordion[String], Maybe[String] => UI.Action) => Frame ?=> Accordion[String]) =
        new SlotCase[Accordion[String], Maybe[String]](
            name = name,
            bare = Present(accordion),
            bind = accordion.value(_),
            hear = hear,
            event = "onValueChange",
            render = _.render,
            start = Absent,
            next = Present("a"),
            control = headers(_).map(_.head),
            shows = open(_),
            ready = Present(headers(_).map(_.exists(declaresClick))),
            nextPatch = Present(_ == ("aria-expanded", "true"))
        )

    holds(valueCase("value", (a, f) => a.onValueChange(f)))
    heardBy(valueCase("value, heard by onToggle", (a, f) => a.onToggle(k => f(Present(k)))))

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

    // A set binds the accordion into multiple mode, which is another type, so this slot has no unbound form.
    holds(new SlotCase[MultipleAccordion[String], Set[String]](
        name = "value, multiple",
        bare = Absent,
        bind = accordion.value(_),
        hear = (a, f) => a.onValueChange(f),
        event = "onValueChange",
        render = _.render,
        start = Set("b"),
        next = Set("a", "b"),
        control = headers(_).map(_.head),
        shows = opened(_),
        ready = Present(headers(_).map(_.exists(declaresClick)))
    ))

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
