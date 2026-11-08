package kyo.uic

import kyo.*

/** Rule A for state slots, written once: the six ways a slot can be bound and what each must do. `Slot` is where the rule
  * lives (`owned`, `heard`, `fixed`, `deaf`, `change`), so a suite that extends this holds a component's slots to
  * it with [[holds]], one call per slot, and adds only what that component alone does.
  *
  * A [[SlotCase]] names the slot through its setter and its event, the one control the reader changes it with, and what a
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
      * @param after
      *   for a slot the reader changes in more than one step (open a panel with the control, then pick a row in it): the
      *   steps after the gesture on the control, each acting on what was published since and tried again until it finds
      *   what it acts on, since what the gesture opened is published after it. Empty, the gesture alone asks for `next`.
      * @param mountDraws
      *   for a slot only the mount draws (a field or a page button inside a panel the page as first rendered shows
      *   closed): every case reads the tree the mount published once `ready` holds, the fixed one included, since the
      *   page as first rendered shows nothing of the slot. Absent, the cases that judge the page as first rendered read
      *   the static render.
      * @param layout
      *   for a gesture the component works out from geometry it measures (a column dragged across the header, a boundary
      *   moved): where the elements it measures sit, by id, which the headless page answers its measures with
      *   ([[UicTest.layout]]). Empty, the page has no layout.
      * @param disable
      *   for a component that draws itself again when `disabled` changes: the component with its `disabled` following the
      *   signal. What the reader did has to outlast that, wherever the state is kept, so the mount that keeps it sits above the
      *   region. Absent, the component has no such region: a channel-only field has nothing that could fail.
      * @param placed
      *   for a component that is an instance kept across its host's runs (it draws through `UI.mountedWith`): the component as
      *   an app places it, which keys the instance on that place. A host region around it that runs again with new props must
      *   leave what the reader did on screen. Absent, the component is not held to it yet (its wave has not reached it).
      */
    final class SlotCase[C, A](
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
        val keptNatively: Maybe[UI => Frame ?=> Boolean < Sync] = Absent,
        val after: Seq[UI => Frame ?=> Any < Async] = Nil,
        val mountDraws: Boolean = false,
        val layout: Map[String, UI.Rect] = Map.empty,
        val disable: Maybe[(C, Signal[Boolean]) => Frame ?=> C] = Absent,
        val placed: Maybe[C => (Frame, UI.MountSite) ?=> UI] = Absent
    )(using val equal: CanEqual[A, A]):

        /** `v` run on a page laid out as [[layout]] says. */
        def laidOut[B, S](v: B < S)(using Frame): B < S = UicTest.layout.let(layout)(v)

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

        /** The tree a case that judges the page as first rendered reads: [[live]] for a slot only the mount draws, else the
          * static render.
          */
        def page(ui: UI)(using Frame, kyo.test.AssertScope): UI < Async =
            if mountDraws then live(ui) else Kyo.lift(ui)

        /** Whether `t` is a tree a gesture acts on: ready, and with a control that answers. A tree the control is not in yet
          * is not ready: the first tree a mount publishes can be empty, and a lookup that fails there must wait for the next
          * one rather than end the wait.
          */
        def wired(t: UI)(using Frame): Boolean < Sync =
            ready.fold(Kyo.lift(true))(r => r(t)).map(mounted =>
                if !mounted then Kyo.lift(false)
                else
                    Abort.run[AssertionError](Abort.catching[AssertionError](control(t).map(answers)))
                        .map(_.getOrElse(false))
            )

        /** Every step of the reader asking for `next`: the gesture, then [[after]]. */
        def steps(using Frame): Seq[UI => Any < Async] = ((t: UI) => ask(t)) +: after.map(f => (t: UI) => f(t))

        /** The reader asking for `next` on `ui` as it runs: on the tree [[live]] gives where the gesture is the whole of it,
          * else under the engine, each step on what was published before it.
          */
        def asked(ui: UI)(using Frame, kyo.test.AssertScope): Any < Async =
            if after.isEmpty then live(ui).map(t => ask(t))
            else actedOn(ui, t => wired(t))(steps*)
    end SlotCase

    private def cards(t: UI)(using Frame) = elementsWithClass(t, UicTest.cardClass)

    /** Whether nothing in a paginator (one of its own, or one a table or a view embeds) goes to another page, and every
      * link and button that would says so: a fixed page.
      */
    def paginatorPagesFixed(t: UI)(using Frame): Boolean < Sync =
        for
            links <- elementsWithClass(t, "p-paginator-page")
            navs  <- Kyo.foreach(List("first", "prev", "next", "last"))(c => elementsWithClass(t, s"p-paginator-$c")).map(_.flatten)
        yield
            val says =
                (links.filterNot(_.attrs.cssClasses.contains("p-paginator-page-selected")) ++
                    navs.filterNot(_.attrs.cssClasses.contains("p-disabled")))
            links.nonEmpty && (links ++ navs).forall(e => !declaresClick(e)) &&
            says.nonEmpty && says.forall(_.attrs.ariaAttrs.get("disabled").contains("true"))
        end for
    end paginatorPagesFixed

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
    ): SlotCase[C, Boolean] =
        def control(t: UI)(using Frame): UI.Ast.Element < Sync =
            elements(t).map(_.collectFirst {
                case b: UI.Ast.BooleanInput if b.attrs.cssClasses.contains(input) => b
            }.getOrElse(throw new AssertionError(s"no input with class $input")))
        def shows(t: UI)(using Frame): Boolean < Sync =
            for
                in     <- control(t)
                box    <- elementWithClass(t, root)
                drawn  <- carriesClass(box, checkedClass)
                native <- in match
                    case b: UI.Ast.BooleanInput =>
                        b.checked match
                            case Present(UI.Bound.Const(v))    => Kyo.lift(v)
                            case Present(UI.Bound.Follow(sig)) => sig.current
                            case other                         => throw new AssertionError(s"the input is bound as $other")
                    case other => throw new AssertionError(s"not a boolean input: $other")
            yield
                if drawn != native then throw new AssertionError(s"drawn checked $drawn over a native state $native")
                native
        new SlotCase[C, Boolean](
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
                    case Present(UI.Bound.Const(v))    => Kyo.lift(v)
                    case Present(UI.Bound.Ref(r))      => r.get
                    case Present(UI.Bound.Follow(sig)) => sig.current
                    case Absent                        => Kyo.lift("")
                    case other                         => throw new AssertionError(s"the field is bound as $other")
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
      *   as [[SlotCase]]'s, for a field whose handlers its mount wires
      * @param placed
      *   as [[SlotCase]]'s, for a field that is an instance kept across its host's runs
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
        ready: Maybe[UI => Frame ?=> Boolean < Sync] = Absent,
        placed: Maybe[C => (Frame, UI.MountSite) ?=> UI] = Absent
    ): SlotCase[C, String] =
        new SlotCase[C, String](
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
            placed = placed,
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
    private def changes[C, A](s: SlotCase[C, A], ui: => UI)(using Frame, kyo.test.AssertScope): Boolean < Async =
        import s.equal
        // What was published since the gesture is read the way a ready tree is: a component of several regions publishes
        // them in either order, so a lookup that fails because the region showing the slot has not come yet means "not
        // yet", never "no", or the wait ends at the first region that publishes (a closing panel before its field).
        val shown: UI => Boolean < Sync = t =>
            Abort.run[AssertionError](Abort.catching[AssertionError](s.shows(t))).map(_.toMaybe.exists(_ == s.next))
        s.nextPatch match
            case Present(patch) => attrPatchedAfter(ui, t => s.wired(t))(s.steps*)(patch)
            case Absent         => publishedAfterReady(ui, t => s.wired(t), until = shown)(s.steps*).map(_.nonEmpty)
        end match
    end changes

    /** Whether `next`, asked for on `build`, is still shown after `disabled` went through `flips`, each written once what
      * follows it has settled, so no value is overtaken by the next. A region around the state would end the mount that holds
      * it with the first flip.
      */
    private def keptThrough[C, A](s: SlotCase[C, A], disable: (C, Signal[Boolean]) => Frame ?=> C, build: Frame ?=> C, flips: Seq[Boolean])(
        using
        Frame,
        kyo.test.AssertScope
    ): Boolean < Async =
        import s.equal
        val shown: UI => Boolean < Sync = t =>
            Abort.run[AssertionError](Abort.catching[AssertionError](s.shows(t))).map(_.toMaybe.exists(_ == s.next))
        Signal.initRef(false).map { gate =>
            val turns = flips.map(v => (_: UI) => setSettled(gate, v): Any < Async)
            publishedAfterReady(s.render(disable(build, gate)), t => s.wired(t), until = shown, reads = Seq(gate))((s.steps ++ turns)*)
                .map(_.nonEmpty)
        }
    end keptThrough

    /** Registers the cases for a `disabled` signal over state the component keeps: what the reader did stays on screen while
      * it is on, and is still there when it goes off.
      */
    private def holdsThroughDisabled[C, A](s: SlotCase[C, A]): Unit =
        s.disable.foreach { disable =>
            def starts(from: String, build: Frame ?=> C): Unit =
                s"${s.name}: $from, a disabled signal going on leaves on screen what the reader did" in s.laidOut {
                    keptThrough(s, disable, build, Seq(true)).map(kept => assert(kept, s"${s.next} is gone while disabled"))
                }
                s"${s.name}: $from, a disabled signal going on and off keeps what the reader did" in s.laidOut {
                    keptThrough(s, disable, build, Seq(true, false)).map(kept => assert(kept, s"${s.next} is lost after disabled went off"))
                }
            end starts
            if s.bare.isDefined then starts("unbound", s.bare.get)
            if s.startText.isDefined && s.keptNatively.isEmpty then starts("from a constant", s.bind(s.start))
        }

    /** Registers the cases of rule A for `s`: all six, or the four of them a slot with no unbound form has. */
    def holds[C, A](s: SlotCase[C, A]): Unit =
        import s.equal

        // Evaluated here only to know whether the slot has an unbound form; each case builds its own.
        val unbound = s.bare.isDefined

        if unbound then
            s.keptNatively match
                case Present(kept) =>
                    s"${s.name}: unbound, the text is the browser's: it starts at its start, the reader edits it, and the component leaves it there" in
                        s.laidOut {
                            val bare = s.bare.get
                            for
                                page  <- s.page(s.render(bare))
                                first <- s.shows(page)
                                edits <- s.control(page).map(s.answers)
                                left  <- kept(page)
                                card  <- cards(s.render(bare))
                            yield
                                assert(first == s.start, s"the page as first rendered shows $first")
                                assert(edits, "the reader cannot edit it")
                                assert(left, "the component draws a text of its own over what the reader types")
                                assert(card.isEmpty, s"state the browser keeps: ${card.map(textOf)}")
                            end for
                        }
                case Absent =>
                    s"${s.name}: unbound, the slot is the component's own: it starts at its start and a gesture changes it" in s.laidOut {
                        val bare = s.bare.get
                        for
                            first   <- s.page(s.render(bare)).map(s.shows(_))
                            changed <- changes(s, s.render(bare))
                            card    <- cards(s.render(bare))
                        yield
                            assert(first == s.start, s"the page as first rendered shows $first")
                            assert(changed, s"a gesture never showed ${s.next}")
                            assert(card.isEmpty, s"state the component keeps itself: ${card.map(textOf)}")
                        end for
                    }
            end match

            s"${s.name}: the page as first rendered does not call an unbound slot fixed" in s.laidOut {
                for fixed <- s.page(s.render(s.bare.get)).map(s.fixedIn(_))
                yield assert(!fixed, "the control the reader will use does not say it changes nothing")
            }
        end if

        s.startText match
            case Present(text) =>
                s"${s.name}: a constant nobody hears is the text it starts with: shown, editable, and nothing to say" in s.laidOut {
                    val started = s.render(s.bind(text))
                    for
                        page    <- s.page(started)
                        isFixed <- s.fixedIn(page)
                        shown   <- s.shows(page)
                        card    <- cards(started)
                    yield
                        assert(shown == text, s"the component shows $shown for the constant $text")
                        assert(!isFixed, "the reader edits a starting text, so nothing may call it fixed")
                        assert(card.isEmpty, s"a starting text is not a mistake: ${card.map(textOf)}")
                    end for
                }
                if s.keptNatively.isEmpty then
                    s"${s.name}: a constant is only where it starts: a gesture changes it, and the component keeps the change" in
                        s.laidOut {
                            for changed <- changes(s, s.render(s.bind(s.start)))
                            yield assert(changed, s"a gesture never showed ${s.next}")
                        }
                end if
            case Absent =>
                s"${s.name}: a constant nobody hears is fixed: nothing changes it, and it says so" in s.laidOut {
                    val fixed = s.render(s.bind(s.start))
                    // The page as first rendered already says it: a fixed slot is no mount's to wire, unless only the mount
                    // draws the slot at all.
                    for
                        page    <- s.page(fixed)
                        isFixed <- s.fixedIn(page)
                        shown   <- s.shows(page)
                        card    <- cards(fixed)
                    yield
                        assert(shown == s.start, s"the component shows $shown for the constant ${s.start}")
                        assert(isFixed, "nothing changes it, and where there is a control, it says so")
                        assert(card.isEmpty, s"a fixed state is not a mistake: ${card.map(textOf)}")
                    end for
                }
        end match

        holdsThroughDisabled(s)
        holdsThroughHostRuns(s)
        heardBy(s)
    end holds

    /** Registers the case for a component that is an instance kept across its host's runs ([[SlotCase.placed]]): unbound, what the
      * reader did is still shown after a host region around it runs again and hands it new props. Each run gives it a hearer of its
      * own, so the props of the run differ from the ones before whatever else the component takes.
      */
    private def holdsThroughHostRuns[C, A](s: SlotCase[C, A]): Unit =
        for
            place <- s.placed
            bare  <- s.bare
        do
            s"${s.name}: unbound, a host region that runs again with new props keeps what the reader did" in s.laidOut {
                import s.equal
                def ran(t: UI): Boolean < Sync = elementsWithClass(t, UicTest.hostRunClass).map(_.exists(e => textOf(e) == "run 1"))
                // The host's tree of run 1, which holds the kept instance only as its place.
                def marks(u: UI): Boolean = u match
                    case e: UI.Ast.Element =>
                        (e.attrs.cssClasses.contains(UicTest.hostRunClass) && textOf(e) == "run 1") || e.children.exists(marks)
                    case f: UI.Ast.Fragment[?]   => f.children.exists(marks)
                    case k: UI.Ast.KeyedChild[?] => marks(k.child)
                    case _                       => false
                // The instance sends its tree into that place after it, from a path below the host's; until it has, the place is
                // all a reader of the published trees has, and reading it draws the component anew from its start.
                // The instance's tree as the page shows it after the host's run: the newest tree that came after the host's tree of
                // run 1 from the instance's own path, the one nearest below the host's that published before the gesture. A region
                // inside the instance (Password's meter) publishes from deeper down, in either order with the instance, and is not it.
                def refill(since: Chunk[(Seq[String], UI)], prior: Chunk[(Seq[String], UI)]): Maybe[UI] =
                    val host = since.indexWhere((_, t) => marks(t))
                    if host <= 0 then Absent
                    else
                        val hostAt                                 = since(host)._1
                        def below(trees: Chunk[(Seq[String], UI)]) =
                            trees.filter((path, _) => path.size > hostAt.size)
                        val after = below(since.take(host))
                        below(prior).map(_._1).sortBy(_.size).headOption match
                            case Some(at) => Maybe.fromOption(after.collectFirst { case (path, t) if path == at => t })
                            case None     => if after.isEmpty then Absent else Present(after.minBy(_._1.size)._2)
                    end if
                end refill
                def paths(trees: Chunk[(Seq[String], UI)]): String = trees.map(_._1.mkString(".")).mkString(" ")
                for
                    run <- Signal.initRef(0)
                    ui = UI.div(run.map(n =>
                        UI.div(UI.span.cssClass(UicTest.hostRunClass)(s"run $n"), place(s.hear(bare, _ => Kyo.unit)))
                    ))
                    steps = s.steps :+ ((_: UI) => setSettled(run, 1): Any < Async)
                    _ <- s.keptNatively match
                        // The text is the browser's, and a run keeps the field it is in (the morph keeps the element): what the
                        // instance must not do after the run is draw a value of its own over it.
                        case Present(kept) =>
                            paintedAround(ui, t => s.wired(t), ran)(steps*).map((every, _, _) =>
                                kept(every).map(k =>
                                    assert(k, "after the host's run the field draws a value of its own over the browser's text")
                                )
                            )
                        case Absent =>
                            paintedAround(ui, t => s.wired(t), ran, refill(_, _).isDefined)(steps*).map { (_, since, prior) =>
                                s.shows(refill(since, prior).get).map(shown =>
                                    assert(
                                        shown == s.next,
                                        s"after the host's run the component shows $shown, the reader had made it ${s.next}; " +
                                            s"published after the gesture at ${paths(since)}, before it at ${paths(prior)}"
                                    )
                                )
                            }
                yield ()
                end for
            }
    end holdsThroughHostRuns

    /** Registers the cases for the blur of a text slot: whatever binds the field, focus leaving it reports the text the
      * reader left in it, which the transport reads off the field and carries with the blur ([[blurOut]]). The reader
      * types `next` first, so a field that reported its binding instead would report `start` or the constant.
      */
    def blurs[C](s: SlotCase[C, String], onBlur: (C, String => UI.Action) => Frame ?=> C): Unit =

        def reported(build: Frame ?=> C)(using Frame, kyo.test.AssertScope): Chunk[String] < Async =
            for
                seen <- Signal.initRef(Chunk.empty[String])
                t    <- s.live(s.render(onBlur(build, v => seen.updateAndGet(_.append(v)).unit)))
                _    <- s.ask(t)
                c    <- s.control(t)
                _    <- c match
                    case f: UI.Ast.TextInput => blurOut(f, s.next)
                    case other               => throw new AssertionError(s"not a text field: $other")
                got <- seen.get
            yield got

        if s.bare.isDefined then
            s"${s.name}: unbound, a blur reports what the reader typed" in {
                reported(s.bare.get).map(got => assert(got == Chunk(s.next), s"reported $got"))
            }
        end if

        s"${s.name}: from a constant, a blur reports the reader's edit, not the constant" in {
            reported(s.bind(s.startText.getOrElse(s.start))).map(got => assert(got == Chunk(s.next), s"reported $got"))
        }

        s"${s.name}: bound one way, a blur reports the text the field holds" in {
            Signal.initRef(s.start).map(held =>
                reported(s.hear(s.bind(held.readOnly), _ => ())).map(got => assert(got == Chunk(s.next), s"reported $got"))
            )
        }

        s"${s.name}: bound to a ref, a blur reports what the reader typed" in {
            Signal.initRef(s.start).map(held => reported(s.bind(held)).map(got => assert(got == Chunk(s.next), s"reported $got")))
        }
    end blurs

    /** Registers the three cases of rule A a hearer takes part in (a constant somebody hears, a signal bound one way, a
      * ref) for a slot that has more than one event: [[holds]] holds the slot once, through one of them, and this holds
      * it through each of the others, which `s` names with its own `name`, `hear`, `event` and `gesture`.
      */
    def heardBy[C, A](s: SlotCase[C, A]): Unit =
        import s.equal

        s"${s.name}: a constant somebody hears reports the gesture with what it asks for, and is no mistake" in s.laidOut {
            for
                seen <- Signal.initRef(Chunk.empty[A])
                _    <- s.asked(s.render(s.hear(s.bind(s.start), v => seen.updateAndGet(_.append(v)).unit)))
                got  <- seen.get
                card <- cards(s.render(s.hear(s.bind(s.start), _ => ())))
            yield
                assert(got.size == 1 && got.head == s.next, s"reported $got")
                assert(card.isEmpty, s"a constant somebody hears is no mistake: ${card.map(textOf)}")
            end for
        }

        s"${s.name}: a signal bound one way is followed, and a gesture is reported without being written" in s.laidOut {
            for
                held <- Signal.initRef(s.start)
                seen <- Signal.initRef(Chunk.empty[A])
                ui = s.render(s.hear(s.bind(held.readOnly), v => seen.updateAndGet(_.append(v)).unit))
                t     <- s.live(ui)
                shown <- s.shows(t)
                // A gesture of several steps acts on the tree as it runs, which the one read above has stopped running.
                _     <- if s.after.isEmpty then s.ask(t) else s.asked(ui)
                h     <- held.get
                got   <- seen.get
                deaf  <- cards(s.render(s.bind(held.readOnly)))
                heard <- cards(s.render(s.hear(s.bind(held.readOnly), _ => ())))
                lives <- cardsOnPage(s.render(s.bind(held.readOnly)))
            yield
                assert(shown == s.start, s"the component follows the signal: it shows $shown")
                assert(h == s.start && got.size == 1 && got.head == s.next, s"held $h, reported $got")
                assert(deaf.size == 1 && textOf(deaf.head).contains(s.event), s"nobody hears: ${deaf.map(textOf)}")
                assert(heard.isEmpty, s"a signal somebody hears is no mistake: ${heard.map(textOf)}")
                assert(lives.size == 1, s"the page as it lives says it once: ${lives.map(textOf)}")
            end for
        }

        s"${s.name}: a ref is written before the component reports it" in s.laidOut {
            for
                held <- Signal.initRef(s.start)
                seen <- Signal.initRef(Chunk.empty[(A, A)])
                _    <- s.asked(s.render(s.hear(s.bind(held), v => held.get.map(h => seen.updateAndGet(_.append((v, h))).unit))))
                got  <- seen.get
            yield assert(got.size == 1 && got.head._1 == s.next && got.head._2 == s.next, s"reported, held: $got")
        }

        s"${s.name}: a zoom is a ref: a gesture writes through it into the whole, and only its part" in s.laidOut {
            for
                whole <- Signal.initRef(Zoomed(s.start, 0))
                part = whole.zoom(_.part)((w, a) => w.copy(part = a))
                seen   <- Signal.initRef(Chunk.empty[(A, A)])
                _      <- s.asked(s.render(s.hear(s.bind(part), v => part.get.map(h => seen.updateAndGet(_.append((v, h))).unit))))
                got    <- seen.get
                result <- whole.get
            yield
                assert(got.size == 1 && got.head._1 == s.next && got.head._2 == s.next, s"reported, held: $got")
                assert(result == Zoomed(s.next, 0), s"the whole holds $result")
            end for
        }
    end heardBy

    /** A value with a part a zoom binds, and a field beside it that no gesture may touch. */
    final private case class Zoomed[A](part: A, other: Int)
    private object Zoomed:
        given [A](using CanEqual[A, A]): CanEqual[Zoomed[A], Zoomed[A]] = CanEqual.derived

end ReactiveValueContract
