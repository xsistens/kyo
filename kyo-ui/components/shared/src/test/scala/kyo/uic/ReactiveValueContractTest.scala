package kyo.uic

import kyo.*

/** Rule A for state slots, written once: the six ways a slot can be bound and what each must do. `ReactiveValue` is where
  * the rule lives (`owned`, `heard`, `fixed`, `deaf`, `change`), so a suite that extends this holds a component's slots to
  * it with [[holds]], one call per slot, and adds only what that component alone does.
  *
  * A [[Slot]] names the slot through its setter and its event, the one control the reader changes it with, and what a
  * render shows of it. `start` is where an unbound slot starts and `next` is what one gesture on that control asks for
  * from `start`; every case binds `start` and expects `next` to be asked for, and a report to say so exactly once.
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
      *   for a component whose control is wired by its mount: when what the mount published is the tree a gesture acts
      *   on. The static render shows the placeholder, whose handlers reach nothing, so `ready` must not hold there. A
      *   binding drawn with no mount at all is acted on as it renders, once `ready` holds on it. Absent, a gesture acts on
      *   the static render. A fixed slot is always judged on the static render, which must already say so.
      * @param nextPatch
      *   for a component that shows its own state's change in place, through an attribute channel rather than by
      *   publishing again: the patch, as `(name, value)`, that says `next` is shown. Absent, a change is seen in what the
      *   component publishes, through `shows`.
      * @param fixedOn
      *   for a component whose fixed state is said by another element than the one the gesture lands on (a wrapper that
      *   takes the click around the control the reader focuses): that element. Absent, the control says it.
      * @param startText
      *   for a text field (`TextFormControl`), whose constant is the text it starts with (the HTML `value` attribute the
      *   reader edits) rather than a value meant to stay: such a constant, other than `start` so a case can see it shown.
      *   A constant nobody hears is then shown, editable and not called fixed, with no card; and where the component keeps
      *   the text itself (`keptNatively` absent), a gesture on a constant changes it as it would unbound. Absent, a
      *   constant nobody hears is fixed.
      * @param keptNatively
      *   for a field whose unbound state is the browser's own (a native text field nobody binds keeps what the reader
      *   typed, and a headless run has no browser to keep it): whether the tree leaves it to the browser, drawing nothing
      *   of its own over it. The unbound case then checks that instead of watching a gesture. Absent, the unbound case
      *   watches a gesture change what the component shows.
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
        val fixedOn: Maybe[UI => Frame ?=> UI.Ast.Element < Sync] = Absent,
        val startText: Maybe[A] = Absent,
        val keptNatively: Maybe[UI => Frame ?=> Boolean < Sync] = Absent
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

        /** The tree the cases act on: what the mount published once [[ready]] holds (the render itself where it holds no
          * mount), else the render itself.
          */
        def live(ui: UI)(using Frame, kyo.test.AssertScope): UI < Async =
            ready match
                case Present(r) => readyTree(ui)(t => r(t))
                case Absent     => Kyo.lift(ui)
    end Slot

    private def cards(t: UI)(using Frame) = elementsWithClass(t, "p-uic-key-error")

    /** The slot of a field whose state is a native boolean input (a checkbox, a switch, a radio), from `false` to `true`.
      *
      * The reader changes it through the browser's change event, which carries the state asked for, not through a click.
      * Fixed is what `readonly` is (see `ReadonlyTest`): the input says so in its role's vocabulary (`fixedAria`), the
      * client declines the browser's own toggle (`preventActivation`), and it stays a tab stop, so it is not natively
      * disabled. What a render shows is the native state, and the box drawn around it must agree: `checkedClass` on the
      * element with class `root` is there exactly when the input is checked.
      *
      * @param input
      *   the class of the native input
      * @param root
      *   the class of the element that draws the state
      * @param checkedClass
      *   the class that element carries while checked
      * @param fixedAria
      *   the `aria-*` state, set to `"true"`, that says the input changes nothing: `readonly` for a checkbox or a switch,
      *   `disabled` for a radio, whose role has no readonly state
      */
    def nativeBoolean[C](
        name: String,
        bare: Frame ?=> Maybe[C],
        bind: (Boolean | Signal[Boolean]) => Frame ?=> C,
        hear: (C, Boolean => UI.Action) => Frame ?=> C,
        event: String,
        render: C => Frame ?=> UI,
        input: String,
        root: String,
        checkedClass: String,
        fixedAria: String = "readonly"
    ): Slot[C, Boolean] =
        def control(t: UI)(using Frame): UI.Ast.Element < Sync =
            elements(t).map(_.collectFirst {
                case b: UI.Ast.BooleanInput if b.attrs.cssClasses.contains(input) => b
            }.getOrElse(throw new AssertionError(s"no input with class $input")))
        def shows(t: UI)(using Frame): Boolean < Sync =
            for
                in    <- control(t)
                box   <- elementWithClass(t, root)
                drawn <- carriesClass(box, checkedClass)
            yield
                val native = in match
                    case b: UI.Ast.BooleanInput =>
                        b.checked match
                            case Present(UI.Bound.Const(v)) => v
                            case other                      => throw new AssertionError(s"the input is bound as $other")
                    case other => throw new AssertionError(s"not a boolean input: $other")
                if drawn != native then throw new AssertionError(s"drawn checked $drawn over a native state $native")
                native
        new Slot[C, Boolean](
            name = name,
            bare = bare,
            bind = bind,
            hear = hear,
            event = event,
            render = render,
            start = false,
            next = true,
            control = control(_),
            shows = shows(_),
            gesture = (c: UI.Ast.Element) =>
                c match
                    case b: UI.Ast.BooleanInput => handled(b.onChange.getOrElse(throw new AssertionError("no change handler"))(true))
                    case other                  => throw new AssertionError(s"not a boolean input: $other"),
            answers = {
                case b: UI.Ast.BooleanInput => b.onChange.nonEmpty
                case _                      => false
            },
            saysFixed = {
                case b: UI.Ast.BooleanInput =>
                    b.attrs.ariaAttrs.get(fixedAria).contains("true") && b.attrs.dataAttrs.get("kyo-inert").contains("1") &&
                    !b.disabled.contains(true)
                case _ => false
            }
        )
    end nativeBoolean

    /** The text a native field holds as a render draws it: its constant, what the ref it binds holds, or the empty text
      * when it draws no `value` (where the text is the browser's).
      */
    def textIn(c: UI.Ast.Element)(using Frame): String < Sync =
        c match
            case t: UI.Ast.TextInput =>
                t.value match
                    case Present(UI.Bound.Const(v)) => Kyo.lift(v)
                    case Present(UI.Bound.Ref(r))   => r.get
                    case Absent                     => Kyo.lift("")
                    case other                      => throw new AssertionError(s"the field is bound as $other")
            case other => throw new AssertionError(s"not a text field: $other")

    /** The native text field with class `cls` in `t`. */
    def textField(t: UI, cls: String)(using Frame): UI.Ast.Element < Sync =
        elements(t).map(_.collectFirst {
            case f: (UI.Ast.Element & UI.Ast.TextInput) if f.attrs.cssClasses.contains(cls) => f
        }.getOrElse(throw new AssertionError(s"no text field with class $cls")))

    /** Whether a native text field lets the reader edit it: neither readonly nor disabled. */
    def editable(c: UI.Ast.Element): Boolean =
        c match
            case t: UI.Ast.TextInput => !t.readOnly.contains(true) && !t.disabled.contains(true)
            case _                   => false

    /** The reader typing (or, with `commit`, committing) `text` in a native field, as the transport delivers it. */
    def typed(c: UI.Ast.Element, text: String, commit: Boolean)(using Frame): Any < Async =
        c match
            case t: UI.Ast.TextInput => if commit then commitInto(t, text) else typeInto(t, text)
            case other               => throw new AssertionError(s"not a text field: $other")

    /** The value of a native text field (an `input` or a `textarea` the browser edits), from `""` to `next`.
      *
      * The reader changes it by typing, which the transport delivers as an input event, or, for a slot whose event hears
      * the commit, as a change event: a field bound to a ref has it written first, then the handler is told
      * ([[typeInto]], [[commitInto]]). What a render shows is the field's `value` ([[textIn]]). The field answers by being
      * editable, since the browser edits it whether or not anybody listens, and fixed is what native `readonly` is, which
      * keeps the field focusable. A constant is the starting text (`startText`), and unbound the text is the browser's,
      * which the field leaves to it by drawing no `value` of its own.
      *
      * @param field
      *   the class of the native field
      * @param next
      *   the text the reader types
      * @param startText
      *   the constant the field starts from
      * @param commit
      *   whether the slot's event hears the commit (`onChange`) rather than every keystroke (`onInput`)
      * @param ready
      *   as [[Slot]]'s, for a field whose handlers its mount wires
      */
    def nativeText[C](
        name: String,
        bare: Frame ?=> Maybe[C],
        bind: (String | Signal[String]) => Frame ?=> C,
        hear: (C, String => UI.Action) => Frame ?=> C,
        event: String,
        render: C => Frame ?=> UI,
        field: String,
        next: String = "typed",
        startText: String = "start",
        commit: Boolean = false,
        ready: Maybe[UI => Frame ?=> Boolean < Sync] = Absent
    ): Slot[C, String] =
        new Slot[C, String](
            name = name,
            bare = bare,
            bind = bind,
            hear = hear,
            event = event,
            render = render,
            start = "",
            next = next,
            control = textField(_, field),
            shows = t => textField(t, field).map(textIn),
            gesture = (c: UI.Ast.Element) => typed(c, next, commit),
            answers = editable,
            saysFixed = {
                case t: UI.Ast.TextInput => t.readOnly.contains(true)
                case _                   => false
            },
            ready = ready,
            startText = Present(startText),
            keptNatively = Present(t =>
                textField(t, field).map {
                    case f: UI.Ast.TextInput => f.value.isEmpty
                    case _                   => false
                }
            )
        )
    end nativeText

    /** Whether a gesture on `ui` changes what the slot shows to `next`: on the tree the mount published once it is ready
      * and its control answers, seen in what it publishes after the gesture, or in a patch where the slot has one.
      */
    private def changes[C, A](s: Slot[C, A], ui: => UI)(using Frame, kyo.test.AssertScope): Boolean < Async =
        import s.equal
        // A tree the control is not in yet is not ready: the first tree a mount publishes can be empty, and a lookup that
        // fails there must wait for the next one rather than end the wait.
        val wired: UI => Boolean < Sync = t =>
            s.ready.fold(Kyo.lift(true))(r => r(t)).map(mounted =>
                if !mounted then Kyo.lift(false)
                else
                    Abort.run[AssertionError](Abort.catching[AssertionError](s.control(t).map(s.answers)))
                        .map(_.getOrElse(false))
            )
        s.nextPatch match
            case Present(patch) => attrPatchedAfter(ui, wired)(t => s.ask(t))(patch)
            case Absent         =>
                publishedAfterReady(ui, wired, until = t => s.shows(t).map(_ == s.next))(t => s.ask(t)).map(_.nonEmpty)
        end match
    end changes

    /** Registers the cases of rule A for `s`: all six, or the four of them a slot with no unbound form has. */
    def holds[C, A](s: Slot[C, A]): Unit =
        import s.equal

        // Evaluated here only to know whether the slot has an unbound form; each case builds its own.
        val unbound = s.bare.isDefined

        if unbound then
            s.keptNatively match
                case Present(kept) =>
                    s"${s.name}: unbound, the text is the browser's: it starts at its start, the reader edits it, and the component leaves it there" in {
                        val bare = s.bare.get
                        for
                            first <- s.shows(s.render(bare))
                            edits <- s.control(s.render(bare)).map(s.answers)
                            left  <- kept(s.render(bare))
                            card  <- cards(s.render(bare))
                        yield
                            assert(first == s.start, s"the page as first rendered shows $first")
                            assert(edits, "the reader cannot edit it")
                            assert(left, "the component draws a text of its own over what the reader types")
                            assert(card.isEmpty, s"state the browser keeps: ${card.map(textOf)}")
                        end for
                    }
                case Absent =>
                    s"${s.name}: unbound, the slot is the component's own: it starts at its start and a gesture changes it" in {
                        val bare = s.bare.get
                        for
                            first   <- s.shows(s.render(bare))
                            changed <- changes(s, s.render(bare))
                            card    <- cards(s.render(bare))
                        yield
                            assert(first == s.start, s"the page as first rendered shows $first")
                            assert(changed, s"a gesture never showed ${s.next}")
                            assert(card.isEmpty, s"state the component keeps itself: ${card.map(textOf)}")
                        end for
                    }
            end match

            s"${s.name}: the page as first rendered does not call an unbound slot fixed" in {
                for fixed <- s.fixedIn(s.render(s.bare.get))
                yield assert(!fixed, "the control the reader will use does not say it changes nothing")
            }
        end if

        s.startText match
            case Present(text) =>
                s"${s.name}: a constant nobody hears is the text it starts with: shown, editable, and nothing to say" in {
                    val started = s.render(s.bind(text))
                    for
                        isFixed <- s.fixedIn(started)
                        shown   <- s.shows(started)
                        card    <- cards(started)
                    yield
                        assert(shown == text, s"the component shows $shown for the constant $text")
                        assert(!isFixed, "the reader edits a starting text, so nothing may call it fixed")
                        assert(card.isEmpty, s"a starting text is not a mistake: ${card.map(textOf)}")
                    end for
                }
                if s.keptNatively.isEmpty then
                    s"${s.name}: a constant is only where it starts: a gesture changes it, and the component keeps the change" in {
                        for changed <- changes(s, s.render(s.bind(s.start)))
                        yield assert(changed, s"a gesture never showed ${s.next}")
                    }
                end if
            case Absent =>
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
        end match

        heardBy(s)
    end holds

    /** Registers the three cases of rule A a hearer takes part in (a constant somebody hears, a signal bound one way, a
      * ref) for a slot that has more than one event: [[holds]] holds the slot once, through one of them, and this holds
      * it through each of the others, which `s` names with its own `name`, `hear`, `event` and `gesture`.
      */
    def heardBy[C, A](s: Slot[C, A]): Unit =
        import s.equal

        s"${s.name}: a constant somebody hears reports the gesture with what it asks for" in {
            for
                seen <- Signal.initRef(Chunk.empty[A])
                t    <- s.live(s.render(s.hear(s.bind(s.start), v => seen.updateAndGet(_.append(v)).unit)))
                _    <- s.ask(t)
                got  <- seen.get
            yield assert(got.size == 1 && got.head == s.next, s"reported $got")
        }

        s"${s.name}: a signal bound one way is followed, and a gesture is reported without being written" in {
            for
                held  <- Signal.initRef(s.start)
                seen  <- Signal.initRef(Chunk.empty[A])
                t     <- s.live(s.render(s.hear(s.bind(held.readOnly), v => seen.updateAndGet(_.append(v)).unit)))
                shown <- s.shows(t)
                _     <- s.ask(t)
                h     <- held.get
                got   <- seen.get
                deaf  <- cards(s.render(s.bind(held.readOnly)))
                heard <- cards(s.render(s.hear(s.bind(held.readOnly), _ => ())))
            yield
                assert(shown == s.start, s"the component follows the signal: it shows $shown")
                assert(h == s.start && got.size == 1 && got.head == s.next, s"held $h, reported $got")
                assert(deaf.size == 1 && textOf(deaf.head).contains(s.event), s"nobody hears: ${deaf.map(textOf)}")
                assert(heard.isEmpty, s"a signal somebody hears is no mistake: ${heard.map(textOf)}")
            end for
        }

        s"${s.name}: a ref is written before the component reports it" in {
            for
                held <- Signal.initRef(s.start)
                seen <- Signal.initRef(Chunk.empty[(A, A)])
                t    <- s.live(s.render(s.hear(s.bind(held), v => held.get.map(h => seen.updateAndGet(_.append((v, h))).unit))))
                _    <- s.ask(t)
                got  <- seen.get
            yield assert(got.size == 1 && got.head._1 == s.next && got.head._2 == s.next, s"reported, held: $got")
        }
    end heardBy

end ReactiveValueContract
