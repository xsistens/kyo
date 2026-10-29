package kyo.uic

import kyo.*

/** Rule A for one state slot, written once: the six ways a slot can be bound and what each must do, against the
  * component a suite names. `ReactiveValue` is where the rule lives (`owned`, `heard`, `fixed`, `deaf`, `change`), so a
  * suite that extends this holds a component to it, and adds only what that component alone does.
  *
  * A suite names the slot through `bind` and `hear`, the one control the reader changes it with, and what a render
  * shows of it. `start` is where an unbound slot starts and `next` is what one `gesture` on that control asks for from
  * `start`; every case below binds `start` and expects `next` to be asked for.
  *
  * The file ends in `Test.scala` although nothing runs it on its own: inside the kyo package a `Frame` is derived only in
  * such a file, and the cases below derive one.
  */
abstract class ReactiveValueContract[C, A](using CanEqual[A, A]) extends UicTest:

    /** The component as the page writes it, with nothing bound in the slot. */
    def bare(using Frame): C

    /** The slot's setter, `collapsed`, `open`, ... */
    def bind(c: C, v: A | Signal[A]): C

    /** The slot's event, `onCollapsedChange`, `onOpenChange`, ... */
    def hear(c: C, f: A => UI.Action)(using Frame): C

    /** The event's name, which the card for a slot nobody hears has to say. */
    def event: String

    def render(c: C)(using Frame): UI

    def start: A
    def next: A

    /** The control the reader changes the slot with. */
    def control(t: UI)(using Frame): UI.Ast.Element < Sync

    /** What the render shows of the slot. */
    def shows(t: UI)(using Frame): A < Sync

    /** The reader asking for `next`. */
    def gesture(control: UI.Ast.Element)(using Frame): Any < Async = click(control)

    /** Whether the control answers the gesture at all. */
    def answers(control: UI.Ast.Element): Boolean = declaresClick(control)

    /** Whether the control says it changes nothing, in the vocabulary its role has. */
    def saysFixed(control: UI.Ast.Element): Boolean = control.attrs.ariaAttrs.get("disabled").contains("true")

    private def cards(t: UI)(using Frame) = elementsWithClass(t, "p-uic-key-error")

    "unbound, the slot is the component's own: it starts at its start and a gesture changes it" in {
        for
            first <- shows(render(bare))
            after <- publishedAfterReady(render(bare), t => control(t).map(answers), until = t => shows(t).map(_ == next))(t =>
                control(t).map(gesture)
            )
            card <- cards(render(bare))
        yield
            assert(first == start, s"the page as first rendered shows $first")
            assert(after.nonEmpty, s"a gesture never showed $next")
            assert(card.isEmpty, s"state the component keeps itself: ${card.map(textOf)}")
        end for
    }

    "the page as first rendered does not call an unbound slot fixed" in {
        for c <- control(render(bare))
        yield assert(!saysFixed(c), "the control the reader will use does not say it changes nothing")
    }

    "a constant nobody hears is fixed: the control stays, changes nothing, and says so" in {
        val fixed = render(bind(bare, start))
        for
            c     <- control(fixed)
            shown <- shows(fixed)
            card  <- cards(fixed)
        yield
            assert(shown == start, s"the component shows $shown for the constant $start")
            assert(!answers(c), "a gesture changes nothing")
            assert(saysFixed(c), "and the control says so")
            assert(card.isEmpty, s"a fixed state is not a mistake: ${card.map(textOf)}")
        end for
    }

    "a constant somebody hears reports the gesture with what it asks for" in {
        for
            seen <- Signal.initRef(Absent: Maybe[A])
            c    <- control(render(hear(bind(bare, start), v => seen.set(Present(v)))))
            _    <- gesture(c)
            s    <- seen.get
        yield assert(s == Present(next), s"reported $s")
    }

    "a signal bound one way is followed, and a gesture is reported without being written" in {
        for
            held <- Signal.initRef(start)
            seen <- Signal.initRef(Absent: Maybe[A])
            ui = render(hear(bind(bare, held.readOnly), v => seen.set(Present(v))))
            shown <- shows(ui)
            c     <- control(ui)
            _     <- gesture(c)
            h     <- held.get
            s     <- seen.get
            deaf  <- cards(render(bind(bare, held.readOnly)))
        yield
            assert(shown == start, s"the component follows the signal: it shows $shown")
            assert(h == start && s == Present(next), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains(event), s"nobody hears: ${deaf.map(textOf)}")
        end for
    }

    "a ref is written before the component reports it" in {
        for
            held <- Signal.initRef(start)
            seen <- Signal.initRef(Absent: Maybe[(A, A)])
            c    <- control(render(hear(bind(bare, held), v => held.get.map(h => seen.set(Present((v, h)))))))
            _    <- gesture(c)
            s    <- seen.get
        yield assert(s == Present((next, next)), s"reported, held: $s")
    }

end ReactiveValueContract
