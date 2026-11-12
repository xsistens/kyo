package kyo.uic

import kyo.*
import kyo.UI.*

/** `uic.SelectButton`'s two arities, which are two different widgets.
  *
  * A choice of ONE out of several is a radio group however it is drawn: one tab stop for the whole
  * group, the arrows moving and selecting inside it, and options that report `aria-checked` rather
  * than a toggle button's `aria-pressed`. A choice of SEVERAL is what it looks like, a row of
  * independent toggle buttons, each its own tab stop, and it stays that way. Both selections are held
  * to rule A ([[ReactiveValueContract]]).
  */
class SelectButtonTest extends ReactiveValueContract:

    private val sizes = Seq("Small", "Medium", "Large")

    private def group(using Frame) = uic.SelectButton[String]().options(sizes)

    private val ids = "sb"

    /** The group wired the way its mount wires it, with the focus moves recorded. */
    private def wired(selected: String, disabled: String => Boolean = _ => false)(using
        Frame
    ): (SignalRef[Maybe[String]], SignalRef[List[String]], UI) < Async =
        for
            value <- Signal.initRef(if selected.isEmpty then Maybe.empty[String] else Maybe(selected))
            moved <- Signal.initRef(List.empty[String])
        yield (
            value,
            moved,
            group.optionDisabled(disabled).selection(value).wired(ids, id => moved.updateAndGet(_ :+ id).unit)
        )

    private def options(ui: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elementsWithClass(ui, "p-togglebutton")

    /** Presses `key` on the option at `at` and reports where focus went and what got selected. */
    private def after(at: Int, key: UI.Keyboard, selected: String = "Medium")(using
        Frame
    ): (List[String], String) < Async =
        for
            (value, moved, ui) <- wired(selected)
            os                 <- options(ui)
            _                  <- press(os(at), key)
            went               <- moved.get
            got                <- value.get.map(_.getOrElse(""))
        yield (went, got)

    "the group is ONE reactive region over everything it reads, single or multiple" in {
        for
            disabled <- Signal.initRef(false)
            loading  <- Signal.initRef(false)
            value    <- Signal.initRef(Maybe("Small"))
            several  <- Signal.initRef(Set("Small"))
            single = group.disabled(disabled).loading(loading).selection(value).wired(ids, _ => ())
            multi  = group.multiple.disabled(disabled).loading(loading).selection(several).render
            singleRegions <- regionsAbove(single, "p-selectbutton")
            multiRegions  <- regionsAbove(multi, "p-selectbutton")
        yield assert(
            singleRegions == 1 && multiRegions == 1,
            s"one subscription per signal nests each inside the last: $singleRegions / $multiRegions"
        )
    }

    "the group says it is one, and its options say they are radios" in {
        for
            (_, _, ui) <- wired("Medium")
            root       <- elementWithClass(ui, "p-selectbutton")
            os         <- options(ui)
        yield
            assert(root.attrs.role.contains("radiogroup"))
            assert(os.forall(_.attrs.role.contains("radio")))
            assert(os.map(_.attrs.ariaAttrs.getOrElse("checked", "-")) == Chunk("false", "true", "false"))
            assert(os.forall(!_.attrs.ariaAttrs.contains("pressed")), "one role, one state")
    }

    "one tab stop, and it sits on the option that is chosen" in {
        for
            (_, _, ui) <- wired("Large")
            os         <- options(ui)
        yield assert(os.map(_.attrs.tabIndex.getOrElse(99)) == Chunk(-1, -1, 0))
    }

    "and on the first option a reader may choose while nothing is" in {
        for
            (_, _, ui) <- wired("")
            os         <- options(ui)
        yield assert(os.map(_.attrs.tabIndex.getOrElse(99)) == Chunk(0, -1, -1), "a group nobody has answered is still tabbable")
    }

    "an arrow moves the focus and picks what it lands on, in both axes" in {
        for
            right <- after(1, UI.Keyboard.ArrowRight)
            down  <- after(1, UI.Keyboard.ArrowDown)
            left  <- after(1, UI.Keyboard.ArrowLeft)
            up    <- after(1, UI.Keyboard.ArrowUp)
        yield assert(
            right == (List("sb-2"), "Large") && down == (List("sb-2"), "Large") &&
                left == (List("sb-0"), "Small") && up == (List("sb-0"), "Small"),
            "the radio pattern answers Down and Right alike, and Up and Left alike"
        )
    }

    "the group wraps at its ends, as a radio group does" in {
        for
            past  <- after(2, UI.Keyboard.ArrowRight)
            below <- after(0, UI.Keyboard.ArrowLeft, selected = "Small")
        yield assert(past == (List("sb-0"), "Small") && below == (List("sb-2"), "Large"))
    }

    "Home and End reach the ends and pick there too" in {
        for
            home <- after(1, UI.Keyboard.Home)
            end  <- after(1, UI.Keyboard.End)
        yield assert(home == (List("sb-0"), "Small") && end == (List("sb-2"), "Large"))
    }

    "a key that lands where it already is picks nothing, so the choice cannot be cleared by moving" in {
        for
            (value, moved, ui) <- wired("Small")
            os                 <- options(ui)
            _                  <- press(os(0), UI.Keyboard.Home)
            went               <- moved.get
            got                <- value.get.map(_.getOrElse(""))
        yield assert(went.isEmpty && got == "Small", "Home on the chosen option is not a second click on it")
    }

    "a disabled option is stepped over rather than landed on" in {
        for
            (value, moved, ui) <- wired("Small", disabled = _ == "Medium")
            os                 <- options(ui)
            _                  <- press(os(0), UI.Keyboard.ArrowRight)
            went               <- moved.get
            got                <- value.get.map(_.getOrElse(""))
        yield assert(went == List("sb-2") && got == "Large")
    }

    "Enter and Space are the button's own, so this handler adds nothing to them" in {
        for
            enter <- after(1, UI.Keyboard.Enter)
            space <- after(1, UI.Keyboard.Space)
        yield assert(
            enter == (Nil, "Medium") && space == (Nil, "Medium"),
            "a third activation beside the browser's and the dispatcher's would be one too many"
        )
    }

    "a click still clears the chosen option, which is what allowEmpty means" in {
        for
            value <- Signal.initRef(Maybe("Medium"))
            ui = group.selection(value).wired(ids, _ => ())
            os  <- options(ui)
            _   <- click(os(1))
            got <- value.get.map(_.getOrElse(""))
        yield assert(got.isEmpty)
    }

    /** The options the buttons draw pressed. */
    private def pressed(t: UI)(using Frame): Set[String] < Sync =
        options(t).map(os => os.zip(sizes).collect { case (o, s) if o.attrs.ariaAttrs.get("pressed").contains("true") => s }.toSet)

    // A button is the control, and each one says it presses nothing once the set is fixed.
    holds(new SlotCase[uic.MultiSelectButton[String], Set[String]](
        name = "selection, several",
        bare = Present(group.multiple),
        bind = group.multiple.selection(_),
        hear = (b, f) => b.onChange(f),
        event = "onChange",
        render = _.render,
        start = Set.empty,
        next = Set("Medium"),
        control = options(_).map(_(1)),
        shows = pressed(_),
        answers = c => declaresClick(c) || c.attrs.onKeyDown.nonEmpty,
        ready = Present(options(_).map(_.exists(declaresClick))),
        disable = Present((c, on) => c.disabled(on))
    ))

    "several choices stay a group of toggle buttons, each its own tab stop" in {
        for
            value <- Signal.initRef(Set("Small"))
            ui = uic.SelectButton[String]().options(sizes).multiple.selection(value).render
            root <- elementWithClass(ui, "p-selectbutton")
            os   <- options(ui)
        yield
            assert(root.attrs.role.contains("group"))
            assert(os.forall(_.attrs.role.isEmpty), "a toggle button is a button")
            assert(os.map(_.attrs.ariaAttrs.getOrElse("pressed", "-")) == Chunk("true", "false", "false"))
            assert(os.forall(_.attrs.tabIndex.isEmpty), "each keeps the tab stop a button has by itself")
    }

    enum Size derives CanEqual:
        case S, M, L

    private def typed(using Frame) = uic.SelectButton[Size]().options(Size.values.toSeq)(_.toString)

    "selection binds the option itself, and a click on the chosen one keeps it, since the ref has no none" in {
        for
            ref    <- Signal.initRef(Size.M)
            os     <- options(typed.selection(ref).wired(ids, _ => ()))
            _      <- click(os(2))
            picked <- ref.get
            os2    <- options(typed.selection(ref).wired(ids, _ => ()))
            _      <- click(os2(2))
            kept   <- ref.get
        yield
            assert(os.map(_.attrs.ariaAttrs.getOrElse("checked", "-")) == Chunk("false", "true", "false"))
            assert(picked == Size.L && kept == Size.L)
    }

    "a selection over Maybe clears to Absent on a click on the chosen one, as allowEmpty says" in {
        for
            ref <- Signal.initRef(Maybe(Size.S))
            os  <- options(typed.selection(ref).wired(ids, _ => ()))
            _   <- click(os(0))
            got <- ref.get
        yield assert(got == Absent)
    }

    "a selection over a Set toggles options, and keeps a member no option carries" in {
        for
            ref <- Signal.initRef(Set(Size.S))
            os  <- options(uic.SelectButton[Size]().options(Seq(Size.M, Size.L))(_.toString).multiple.selection(ref).render)
            _   <- click(os(1))
            got <- ref.get
        yield assert(got == Set(Size.S, Size.L))
    }

    "optionsWithNone gives the template the none option too" in {
        for
            ref <- Signal.initRef(Maybe.empty[Size])
            ui = uic.SelectButton[Maybe[Size]]()
                .optionsWithNone("Any", Size.values.toSeq)(_.toString)
                .itemTemplate(m => span(m.fold("any size")(s => s"size $s")))
                .selection(ref)
                .wired(ids, _ => ())
            os  <- options(ui)
            _   <- click(os(2))
            got <- ref.get
        yield
            assert(os.map(_.attrs.ariaAttrs.getOrElse("label", "-")) == Chunk("Any", "S", "M", "L"))
            assert(got == Present(Size.M))
    }

    "onPick fires with the option picked, after onChange, and not on a clear" in {
        for
            ref  <- Signal.initRef(Maybe(Size.S))
            seen <- Signal.initRef(List.empty[String])
            base = typed.selection(ref)
                .onChange(m => seen.updateAndGet(_ :+ s"change ${m.fold("-")(_.toString)}").unit)
                .onPick(s => seen.updateAndGet(_ :+ s"pick $s").unit)
            os  <- options(base.wired(ids, _ => ()))
            _   <- click(os(1))
            os2 <- options(base.wired(ids, _ => ()))
            _   <- click(os2(1))
            got <- seen.get
        yield assert(got == List("change M", "pick M", "change -"), "a click on the chosen option clears, which picks nothing")
    }

    "an arrow that selects is a pick too" in {
        for
            ref   <- Signal.initRef(Size.S)
            picks <- Signal.initRef(List.empty[Size])
            os    <- options(typed.selection(ref).onPick(s => picks.updateAndGet(_ :+ s).unit).wired(ids, _ => ()))
            _     <- press(os(0), UI.Keyboard.ArrowRight)
            got   <- picks.get
        yield assert(got == List(Size.M))
    }

    /** The option the buttons draw checked. */
    private def checkedSize(t: UI)(using Frame): Maybe[Size] < Sync =
        options(t).map(os =>
            Maybe.fromOption(os.zip(Size.values).collectFirst {
                case (o, s) if o.attrs.ariaAttrs.get("checked").contains("true") => s
            })
        )

    private def presses(c: UI.Ast.Element): Boolean =
        declaresClick(c) || (!silentConst(c) && !partialConstHides(c, "keydown") && c.attrs.onKeyDown.nonEmpty)

    private def answering(t: UI)(using Frame): Boolean < Sync = options(t).map(_.exists(declaresClick))

    private def maybeSlot(
        name: String,
        hear: (uic.SelectButton[Size], Maybe[Size] => UI.Action) => Frame ?=> uic.SelectButton[Size],
        event: String
    ) =
        new SlotCase[uic.SelectButton[Size], Maybe[Size]](
            name = name,
            bare = Present(typed),
            bind = v => typed.selection(v),
            hear = hear,
            event = event,
            render = _.render,
            start = Absent,
            next = Present(Size.L),
            control = options(_).map(_(2)),
            shows = checkedSize(_),
            answers = presses,
            ready = Present(answering(_)),
            disable = Present((c, on) => c.disabled(on))
        )

    // A pick is reported to onChange, then to onPick: each of them hears it.
    holds(maybeSlot("selection over Maybe", (b, f) => b.onChange(f), "onChange"))
    heardBy(maybeSlot("selection over Maybe, heard by onPick", (b, f) => b.onPick(s => f(Present(s))), "onPick"))

    private def sizeSlot(
        name: String,
        hear: (uic.SelectButton[Size], Size => UI.Action) => Frame ?=> uic.SelectButton[Size],
        event: String
    ) =
        new SlotCase[uic.SelectButton[Size], Size](
            name = name,
            // A group over the option itself always holds one: unbound, a group holds `Maybe`.
            bare = Absent,
            bind = v => typed.selection(v),
            hear = hear,
            event = event,
            render = _.render,
            start = Size.M,
            next = Size.L,
            control = options(_).map(_(2)),
            shows = t => checkedSize(t).map(_.getOrElse(throw new AssertionError("no option is checked"))),
            answers = presses,
            ready = Present(answering(_))
        )

    holds(sizeSlot("selection", (b, f) => b.onPick(f), "onPick"))
    heardBy(sizeSlot(
        "selection, heard by onChange",
        (b, f) => b.onChange(m => m.fold(throw new AssertionError("a group over the option reported none"))(f)),
        "onChange"
    ))

    "a single choice a ref holds, or one a disabled group shows, draws no card" in {
        def errors(b: uic.SelectButton[Size]): Chunk[UI.Ast.Element] < Async = elementsWithClass(b.render, "p-uic-diagnostic")
        for
            held   <- Signal.initRef(Size.M)
            twoWay <- errors(typed.selection(held))
            off    <- errors(typed.selection(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && off.isEmpty, s"the ref takes the pick, or nobody can pick: ${(twoWay ++ off).map(textOf)}")
        end for
    }

    "several choices bound one way stay quiet while the group is disabled" in {
        for
            held <- Signal.initRef(Set("Small"))
            off  <- elementsWithClass(group.multiple.selection(held.readOnly).disabled(true).render, "p-uic-diagnostic")
        yield assert(off.isEmpty, s"nobody can pick: ${off.map(textOf)}")
    }

    "a fixed group holds every button to it, not only the one a gesture reaches" in {
        for
            one     <- options(typed.selection(Size.M).render)
            several <- options(group.multiple.selection(Set("Small")).render)
        yield
            assert(one.size == 3 && one.forall(o => !presses(o)), "a fixed single choice presses nothing")
            assert(several.size == 3 && several.forall(o => !presses(o)), "a fixed set presses nothing")
    }

end SelectButtonTest
