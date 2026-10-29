package kyo.uic

import kyo.*

/** Rule A for state slots, written once: the six ways a slot can be bound and what each must do. `ReactiveValue` is where
  * the rule lives (`owned`, `heard`, `fixed`, `deaf`, `change`), so a suite that extends this holds a component's slots to
  * it with [[holds]], one call per slot, and adds only what that component alone does.
  *
  * A [[Slot]] names the slot through its setter and its event, the one control the reader changes it with, and what a
  * render shows of it. `start` is where an unbound slot starts and `next` is what one gesture on that control asks for
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
      *   the component as a page writes it, with the slot's feature switched on and nothing bound in it. Absent for a slot
      *   that has no unbound form (a constructor argument, or a binding that changes the component's type): the two
      *   unbound cases are then not the slot's.
      * @param bind
      *   the component with the slot bound to the value, through its setter
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
      * @param act
      *   for a gesture that is not one element's own (a click a wrapper takes only for itself): the reader asking for
      *   `next` on the whole tree. Absent, it is `gesture` on the control.
      * @param answers
      *   whether the control answers that gesture at all
      * @param saysFixed
      *   whether the control says it changes nothing, in the vocabulary its role has
      * @param isFixed
      *   for a slot with no control of its own that could say anything (an overlay that only an outside click or Escape
      *   closes): whether the tree is fixed, which is that nothing in it changes the slot. Absent, the control answering
      *   nothing and saying so is what fixed means.
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
        val bare: Frame ?=> Maybe[C],
        val bind: (A | Signal[A]) => Frame ?=> C,
        val hear: (C, A => UI.Action) => Frame ?=> C,
        val event: String,
        val render: C => Frame ?=> UI,
        val start: A,
        val next: A,
        val control: UI => Frame ?=> UI.Ast.Element < Sync,
        val shows: UI => Frame ?=> A < Sync,
        val gesture: UI.Ast.Element => Frame ?=> Any < Async = (c: UI.Ast.Element) => click(c),
        val act: Maybe[UI => Frame ?=> Any < Async] = Absent,
        val answers: UI.Ast.Element => Boolean = (c: UI.Ast.Element) => declaresClick(c),
        val saysFixed: UI.Ast.Element => Boolean = (c: UI.Ast.Element) => c.attrs.ariaAttrs.get("disabled").contains("true"),
        val isFixed: Maybe[UI => Frame ?=> Boolean < Sync] = Absent,
        val ready: Maybe[UI => Frame ?=> Boolean < Sync] = Absent,
        val nextPatch: Maybe[((String, String)) => Boolean] = Absent,
        val fixedOn: Maybe[UI => Frame ?=> UI.Ast.Element < Sync] = Absent
    )(using val equal: CanEqual[A, A]):

        /** The element that says whether the slot is fixed. */
        def sayer(t: UI)(using Frame): UI.Ast.Element < Sync = fixedOn.fold(control(t))(f => f(t))

        /** Whether `t` is fixed: nothing in it changes the slot, and it says so where it has a control to say it. */
        def fixedIn(t: UI)(using Frame): Boolean < Sync =
            isFixed match
                case Present(f) => f(t)
                case Absent     => control(t).map(c => sayer(t).map(say => !answers(c) && saysFixed(say)))

        /** The reader asking for `next` on `t`. */
        def ask(t: UI)(using Frame): Any < Async = act.fold(control(t).map(c => gesture(c)))(f => f(t))

        /** The tree the cases act on: what the mount published once [[ready]] holds, else the render itself. */
        def live(ui: UI)(using Frame, kyo.test.AssertScope): UI < Async =
            ready match
                case Present(r) => mountedWhen(ui)(t => r(t))
                case Absent     => Kyo.lift(ui)
    end Slot

    private def cards(t: UI)(using Frame) = elementsWithClass(t, "p-uic-key-error")

    /** Registers the cases of rule A for `s`: all six, or the four of them a slot with no unbound form has. */
    def holds[C, A](s: Slot[C, A]): Unit =
        import s.equal

        // Evaluated here only to know whether the slot has an unbound form; each case builds its own.
        val unbound = s.bare.isDefined

        if unbound then
            s"${s.name}: unbound, the slot is the component's own: it starts at its start and a gesture changes it" in {
                val bare = s.bare.get
                // A tree the control is not in yet is not ready: the first tree a mount publishes can be empty, and a
                // lookup that fails there must wait for the next one rather than end the wait.
                val wired: UI => Boolean < Sync = t =>
                    s.ready.fold(Kyo.lift(true))(r => r(t)).map(mounted =>
                        if !mounted then Kyo.lift(false)
                        else
                            Abort.run[AssertionError](Abort.catching[AssertionError](s.control(t).map(s.answers)))
                                .map(_.getOrElse(false))
                    )
                for
                    first   <- s.shows(s.render(bare))
                    changed <- s.nextPatch match
                        case Present(patch) => attrPatchedAfter(s.render(bare), wired)(t => s.ask(t))(patch)
                        case Absent         =>
                            publishedAfterReady(s.render(bare), wired, until = t => s.shows(t).map(_ == s.next))(t => s.ask(t))
                                .map(_.nonEmpty)
                    card <- cards(s.render(bare))
                yield
                    assert(first == s.start, s"the page as first rendered shows $first")
                    assert(changed, s"a gesture never showed ${s.next}")
                    assert(card.isEmpty, s"state the component keeps itself: ${card.map(textOf)}")
                end for
            }

            s"${s.name}: the page as first rendered does not call an unbound slot fixed" in {
                for fixed <- s.fixedIn(s.render(s.bare.get))
                yield assert(!fixed, "the control the reader will use does not say it changes nothing")
            }
        end if

        s"${s.name}: a constant nobody hears is fixed: nothing changes it, and it says so" in {
            val fixed = s.render(s.bind(s.start))
            // The page as first rendered already says it: a fixed slot is no mount's to wire.
            for
                isFixed <- s.fixedIn(fixed)
                shown   <- s.shows(fixed)
                card    <- cards(fixed)
            yield
                assert(shown == s.start, s"the component shows $shown for the constant ${s.start}")
                assert(isFixed, "nothing changes it, and where there is a control, it says so")
                assert(card.isEmpty, s"a fixed state is not a mistake: ${card.map(textOf)}")
            end for
        }

        s"${s.name}: a constant somebody hears reports the gesture with what it asks for" in {
            for
                seen <- Signal.initRef(Absent: Maybe[A])
                t    <- s.live(s.render(s.hear(s.bind(s.start), v => seen.set(Present(v)))))
                _    <- s.ask(t)
                got  <- seen.get
            yield assert(got == Present(s.next), s"reported $got")
        }

        s"${s.name}: a signal bound one way is followed, and a gesture is reported without being written" in {
            for
                held  <- Signal.initRef(s.start)
                seen  <- Signal.initRef(Absent: Maybe[A])
                t     <- s.live(s.render(s.hear(s.bind(held.readOnly), v => seen.set(Present(v)))))
                shown <- s.shows(t)
                _     <- s.ask(t)
                h     <- held.get
                got   <- seen.get
                deaf  <- cards(s.render(s.bind(held.readOnly)))
                heard <- cards(s.render(s.hear(s.bind(held.readOnly), _ => ())))
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
                t    <- s.live(s.render(s.hear(s.bind(held), v => held.get.map(h => seen.set(Present((v, h)))))))
                _    <- s.ask(t)
                got  <- seen.get
            yield assert(got == Present((s.next, s.next)), s"reported, held: $got")
        }
    end holds

end ReactiveValueContract
