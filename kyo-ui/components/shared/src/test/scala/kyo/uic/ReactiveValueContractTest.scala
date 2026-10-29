package kyo.uic

import kyo.*

/** Rule A for state slots, written once: the six ways a slot can be bound and what each must do. `ReactiveValue` is where
  * the rule lives (`owned`, `heard`, `fixed`, `deaf`, `change`), so a suite that extends this holds a component's slots to
  * it with [[holds]], one call per slot, and adds only what that component alone does.
  *
  * A [[Slot]] names the slot through its setter and its event, the one control the reader changes it with, and what a
  * render shows of it. `start` is where an unbound slot starts and `next` is what one `gesture` on that control asks for
  * from `start`; every case binds `start` and expects `next` to be asked for.
  *
  * The file ends in `Test.scala` although nothing runs it on its own: inside the kyo package a `Frame` is derived only in
  * such a file, and the cases below derive one.
  */
abstract class ReactiveValueContract extends UicTest:

    /** One state slot of a component, as the cases below drive it.
      *
      * @param name
      *   what the cases are prefixed with, the slot's setter where a component has one slot of a kind
      * @param bare
      *   the component as a page writes it, with the slot's feature switched on and nothing bound in it
      * @param bind
      *   the slot's setter
      * @param hear
      *   the slot's event
      * @param event
      *   the event's name, which the card for a slot nobody hears has to say
      * @param control
      *   the control the reader changes the slot with
      * @param shows
      *   what a render shows of the slot
      * @param gesture
      *   the reader asking for `next` on the control
      * @param answers
      *   whether the control answers that gesture at all
      * @param saysFixed
      *   whether the control says it changes nothing, in the vocabulary its role has
      * @param ready
      *   for a component whose control is wired by its mount, in every binding: when what the mount published is the tree
      *   a gesture acts on. The static render shows the placeholder, whose handlers reach nothing. Absent, a gesture acts
      *   on the static render. A fixed slot is always judged on the static render, which must already say so.
      * @param nextPatch
      *   for a component that shows its own state's change in place, through an attribute channel rather than by
      *   publishing again: the patch, as `(name, value)`, that says `next` is shown. Absent, a change is seen in what the
      *   component publishes, through `shows`.
      * @param fixedOn
      *   for a component whose fixed state is said by another element than the one the gesture lands on (a wrapper that
      *   takes the click around the control the reader focuses): that element. Absent, the control says it.
      */
    final class Slot[C, A](
        val name: String,
        val bare: Frame ?=> C,
        val bind: (C, A | Signal[A]) => C,
        val hear: (C, A => UI.Action) => Frame ?=> C,
        val event: String,
        val render: C => Frame ?=> UI,
        val start: A,
        val next: A,
        val control: UI => Frame ?=> UI.Ast.Element < Sync,
        val shows: UI => Frame ?=> A < Sync,
        val gesture: UI.Ast.Element => Frame ?=> Any < Async = (c: UI.Ast.Element) => click(c),
        val answers: UI.Ast.Element => Boolean = (c: UI.Ast.Element) => declaresClick(c),
        val saysFixed: UI.Ast.Element => Boolean = (c: UI.Ast.Element) => c.attrs.ariaAttrs.get("disabled").contains("true"),
        val ready: Maybe[UI => Frame ?=> Boolean < Sync] = Absent,
        val nextPatch: Maybe[((String, String)) => Boolean] = Absent,
        val fixedOn: Maybe[UI => Frame ?=> UI.Ast.Element < Sync] = Absent
    )(using val equal: CanEqual[A, A]):

        /** The element that says whether the slot is fixed. */
        def sayer(t: UI)(using Frame): UI.Ast.Element < Sync = fixedOn.fold(control(t))(f => f(t))

        /** The tree the cases act on: what the mount published once [[ready]] holds, else the render itself. */
        def live(ui: UI)(using Frame, kyo.test.AssertScope): UI < Async =
            ready match
                case Present(r) => mountedWhen(ui)(t => r(t))
                case Absent     => Kyo.lift(ui)
    end Slot

    private def cards(t: UI)(using Frame) = elementsWithClass(t, "p-uic-key-error")

    /** Registers the six cases of rule A for `s`. */
    def holds[C, A](s: Slot[C, A]): Unit =
        import s.equal

        s"${s.name}: unbound, the slot is the component's own: it starts at its start and a gesture changes it" in {
            val wired: UI => Boolean < Sync = t =>
                s.ready.fold(Kyo.lift(true))(r => r(t)).map(mounted =>
                    if mounted then s.control(t).map(s.answers) else Kyo.lift(false)
                )
            val act: UI => Any < Async = t => s.control(t).map(c => s.gesture(c))
            for
                first   <- s.shows(s.render(s.bare))
                changed <- s.nextPatch match
                    case Present(patch) => attrPatchedAfter(s.render(s.bare), wired)(act)(patch)
                    case Absent         =>
                        publishedAfterReady(s.render(s.bare), wired, until = t => s.shows(t).map(_ == s.next))(act)
                            .map(_.nonEmpty)
                card <- cards(s.render(s.bare))
            yield
                assert(first == s.start, s"the page as first rendered shows $first")
                assert(changed, s"a gesture never showed ${s.next}")
                assert(card.isEmpty, s"state the component keeps itself: ${card.map(textOf)}")
            end for
        }

        s"${s.name}: the page as first rendered does not call an unbound slot fixed" in {
            for c <- s.sayer(s.render(s.bare))
            yield assert(!s.saysFixed(c), "the control the reader will use does not say it changes nothing")
        }

        s"${s.name}: a constant nobody hears is fixed: the control stays, changes nothing, and says so" in {
            val fixed = s.render(s.bind(s.bare, s.start))
            // The page as first rendered already says it: a fixed slot is no mount's to wire.
            for
                c     <- s.control(fixed)
                sayer <- s.sayer(fixed)
                shown <- s.shows(fixed)
                card  <- cards(fixed)
            yield
                assert(shown == s.start, s"the component shows $shown for the constant ${s.start}")
                assert(!s.answers(c), "a gesture changes nothing")
                assert(s.saysFixed(sayer), "and the control says so")
                assert(card.isEmpty, s"a fixed state is not a mistake: ${card.map(textOf)}")
            end for
        }

        s"${s.name}: a constant somebody hears reports the gesture with what it asks for" in {
            for
                seen <- Signal.initRef(Absent: Maybe[A])
                t    <- s.live(s.render(s.hear(s.bind(s.bare, s.start), v => seen.set(Present(v)))))
                c    <- s.control(t)
                _    <- s.gesture(c)
                got  <- seen.get
            yield assert(got == Present(s.next), s"reported $got")
        }

        s"${s.name}: a signal bound one way is followed, and a gesture is reported without being written" in {
            for
                held  <- Signal.initRef(s.start)
                seen  <- Signal.initRef(Absent: Maybe[A])
                t     <- s.live(s.render(s.hear(s.bind(s.bare, held.readOnly), v => seen.set(Present(v)))))
                shown <- s.shows(t)
                c     <- s.control(t)
                _     <- s.gesture(c)
                h     <- held.get
                got   <- seen.get
                deaf  <- cards(s.render(s.bind(s.bare, held.readOnly)))
                heard <- cards(s.render(s.hear(s.bind(s.bare, held.readOnly), _ => ())))
            yield
                assert(shown == s.start, s"the component follows the signal: it shows $shown")
                assert(h == s.start && got == Present(s.next), s"held $h, reported $got")
                assert(deaf.size == 1 && textOf(deaf.head).contains(s.event), s"nobody hears: ${deaf.map(textOf)}")
                assert(heard.isEmpty, s"a signal somebody hears is no mistake: ${heard.map(textOf)}")
            end for
        }

        s"${s.name}: a ref is written before the component reports it" in {
            for
                held <- Signal.initRef(s.start)
                seen <- Signal.initRef(Absent: Maybe[(A, A)])
                t    <- s.live(s.render(s.hear(s.bind(s.bare, held), v => held.get.map(h => seen.set(Present((v, h)))))))
                c    <- s.control(t)
                _    <- s.gesture(c)
                got  <- seen.get
            yield assert(got == Present((s.next, s.next)), s"reported, held: $got")
        }
    end holds

end ReactiveValueContract
