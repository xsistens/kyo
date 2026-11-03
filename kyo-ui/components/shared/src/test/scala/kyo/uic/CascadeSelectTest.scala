package kyo.uic

import kyo.*
import kyo.UI.*

/** Where `uic.CascadeSelect` puts the keyboard highlight when its chain opens, and what closes the
  * chain again.
  *
  * Focus never leaves the trigger, however deep the chain goes, so the whole cascade keyboard runs
  * from there: the key that opens it has to land ON a row, and the Tab that carries the reader out
  * of the widget has to take every open panel with it. Its selection and its open state are held to
  * rule A ([[ReactiveValueContract]]).
  */
class CascadeSelectTest extends ReactiveValueContract:

    private val options = List(
        uic.CascadeItem.group("Europe")(
            uic.CascadeItem.group("Germany")(uic.CascadeItem.leaf("Berlin"), uic.CascadeItem.leaf("Hamburg")),
            uic.CascadeItem.leaf("Bern")
        ),
        uic.CascadeItem.leaf("Sydney")
    )

    private def cascade(using Frame) = uic.CascadeSelect[String]().options(options)(identity).id("cs")

    private def refsOf(using Frame): Submenus < Async =
        Submenus.init(groupPaths(cascade).filter(p => false).toSet)

    /** Presses `key` on the CLOSED trigger and reports whether the chain opened and where the
      * highlight landed.
      */
    private def opening(key: UI.Keyboard)(using Frame): (Boolean, List[Int]) < Async =
        for
            open  <- Signal.initRef(false)
            refs  <- refsOf
            focus <- Anchored.init(List.empty[Int])
            value <- Signal.initRef(Maybe.empty[String])
            ui = cascade.selection(value).wired(open, refs, focus, Present("cs"))
            trigger <- elementWithClass(ui, "p-cascadeselect")
            _       <- press(trigger, key)
            isOpen  <- open.get
            at      <- focus.get
        yield (isOpen, at)

    "an opening key lands on the first option, so the next arrow moves rather than starts" in {
        for
            down  <- opening(UI.Keyboard.ArrowDown)
            enter <- opening(UI.Keyboard.Enter)
            space <- opening(UI.Keyboard.Space)
        yield assert(down == (true, List(0)) && enter == (true, List(0)) && space == (true, List(0)))
    }

    "Tab closes the whole chain, in both directions, however deep it was opened" in {
        val shift = UI.Modifiers(shift = true)
        for
            open  <- Signal.initRef(true)
            refs  <- refsOf
            _     <- refs.openUnanchored(List(0, 0))
            focus <- Anchored.init(List(0, 0, 1))
            value <- Signal.initRef(Maybe.empty[String])
            ui = cascade.selection(value).wired(open, refs, focus, Present("cs"))
            trigger <- elementWithClass(ui, "p-cascadeselect")
            _       <- press(trigger, UI.Keyboard.Tab)
            still   <- open.get
            groups  <- refs.openPaths
            back    <- Signal.initRef(true)
            refs2   <- refsOf
            focus2  <- Anchored.init(List(0))
            ui2 = cascade.selection(value).wired(back, refs2, focus2, Present("cs"))
            trigger2  <- elementWithClass(ui2, "p-cascadeselect")
            _         <- press(trigger2, UI.Keyboard.Tab, shift)
            stillBack <- back.get
        yield assert(
            !still && !stillBack && groups.isEmpty,
            "the chain's keyboard lives on the trigger the Tab is leaving, so no panel outlives it"
        )
        end for
    }

    "the arrows still walk the level the highlight is on" in {
        for
            open  <- Signal.initRef(true)
            refs  <- refsOf
            focus <- Anchored.init(List(0))
            value <- Signal.initRef(Maybe.empty[String])
            ui = cascade.selection(value).wired(open, refs, focus, Present("cs"))
            trigger <- elementWithClass(ui, "p-cascadeselect")
            _       <- press(trigger, UI.Keyboard.ArrowDown)
            at      <- focus.get
        yield assert(at == List(1), "Sydney, the second root option")
    }

    /** The chain wired open with every group open too, so every leaf row is on the tree. */
    private def allOpen(c: uic.CascadeSelect[String])(using Frame): UI < Async =
        for
            open  <- Signal.initRef(true)
            refs  <- Submenus.init(groupPaths(c).filter(p => true).toSet)
            focus <- Anchored.init(List.empty[Int])
        yield c.wired(open, refs, focus, Present("cs"))

    private def leaf(ui: UI, text: String)(using Frame): UI.Ast.Element < Sync =
        elementsWithClass(ui, "p-cascadeselect-option-content").map(_.find(_.children.exists {
            case el: UI.Ast.Element => el.children.exists {
                    case t: UI.Ast.Text => t.value == text
                    case _              => false
                }
            case _ => false
        }).getOrElse(throw new AssertionError(s"no leaf $text")))

    "selection binds the leaf itself, and a pick writes it" in {
        for
            ref    <- Signal.initRef("Bern")
            ui     <- allOpen(cascade.selection(ref))
            chosen <- elementsWithClass(ui, "p-cascadeselect-option-selected")
            target <- leaf(ui, "Hamburg")
            _      <- click(target)
            got    <- ref.get
        yield
            assert(chosen.size == 1)
            assert(got == "Hamburg")
    }

    "a selection over Maybe starts empty and takes the leaf picked" in {
        for
            ref    <- Signal.initRef(Maybe.empty[String])
            ui     <- allOpen(cascade.placeholder("City").selection(ref))
            chosen <- elementsWithClass(ui, "p-cascadeselect-option-selected")
            target <- leaf(ui, "Sydney")
            _      <- click(target)
            got    <- ref.get
        yield
            assert(chosen.isEmpty)
            assert(got == Present("Sydney"))
    }

    "onPick fires with the leaf picked, after onChange, and not for the leaf already held" in {
        for
            ref  <- Signal.initRef("Bern")
            seen <- Signal.initRef(List.empty[String])
            base = cascade.selection(ref)
                .onChange(m => seen.updateAndGet(_ :+ s"change ${m.getOrElse("-")}").unit)
                .onPick(l => seen.updateAndGet(_ :+ s"pick $l").unit)
            ui   <- allOpen(base)
            same <- leaf(ui, "Bern")
            _    <- click(same)
            // A pick closes the chain, so the second one opens it again.
            ui2    <- allOpen(base)
            target <- leaf(ui2, "Sydney")
            _      <- click(target)
            got    <- seen.get
        yield assert(got == List("change Sydney", "pick Sydney"))
    }

    // Shown, reported and not written is the contract's (see the selection slots below); this is the row the panel marks.
    "a one-way selection marks the leaf it holds in the panel" in {
        for
            route  <- Signal.initRef("Bern")
            ui     <- allOpen(cascade.selection(route.readOnly).onPick(_ => ()))
            chosen <- elementsWithClass(ui, "p-cascadeselect-option-selected")
        yield assert(chosen.size == 1 && textOf(chosen.head).contains("Bern"), "the leaf the signal holds is the chosen one")
    }

    private def classed(cls: String)(e: UI.Ast.Element): Boolean = e.attrs.cssClasses.contains(cls)

    private def saying(cls: String, text: String)(e: UI.Ast.Element): Boolean =
        classed(cls)(e) && e.children.exists {
            case t: UI.Ast.Text => t.value == text
            case _              => false
        }

    /** The cascade wired with its root panel `open` and every group `groupsOpen`. */
    private def field(open: Boolean, groupsOpen: Boolean = false)(using
        Frame
    ): (SignalRef[Boolean], Submenus, SignalRef[Maybe[String]], UI) < Async =
        for
            o     <- Signal.initRef(open)
            refs  <- Submenus.init(groupPaths(cascade).filter(p => groupsOpen).toSet)
            focus <- Anchored.init(List.empty[Int])
            value <- Signal.initRef(Maybe.empty[String])
        yield (o, refs, value, cascade.selection(value).wired(o, refs, focus, Present("cs")))

    "the wired field is ONE reactive region over everything it reads, open or closed" in {
        for
            loading <- Signal.initRef(false)
            ph      <- Signal.initRef("Pick a city")
            open    <- Signal.initRef(true)
            refs    <- Submenus.init(groupPaths(cascade).filter(p => true).toSet)
            focus   <- Anchored.init(List(0))
            value   <- Signal.initRef(Maybe.empty[String])
            ui = cascade.loading(loading).placeholder(ph).selection(value).wired(open, refs, focus, Present("cs"))
            whenOpen   <- regionsAbove(ui, "p-cascadeselect")
            _          <- open.set(false)
            whenClosed <- regionsAbove(ui, "p-cascadeselect")
        yield assert(
            whenOpen == 1 && whenClosed == 1,
            s"a pick writes the value and closes the chain; a nested region can repaint the older state: $whenOpen / $whenClosed"
        )
    }

    "a click anywhere on the field opens it, the element carrying its id included" in {
        for
            (byId, _, _, ui1)   <- field(open = false)
            _                   <- clickBubbling(ui1)(_.attrs.identifier.contains("cs"))
            viaId               <- byId.get
            (byIcon, _, _, ui2) <- field(open = false)
            _                   <- clickBubbling(ui2)(classed("p-cascadeselect-dropdown"))
            viaChevron          <- byIcon.get
            (shut, _, _, ui3)   <- field(open = true)
            _                   <- clickBubbling(ui3)(_.attrs.identifier.contains("cs"))
            closed              <- shut.get
        yield
            assert(viaId, "the field itself is the control, so a click on its padding or by its id opens it")
            assert(viaChevron && !closed)
        end for
    }

    "a click on a leaf picks it once and closes the whole chain" in {
        for
            seen  <- Signal.initRef(0)
            open  <- Signal.initRef(true)
            refs  <- Submenus.init(groupPaths(cascade).filter(p => true).toSet)
            focus <- Anchored.init(List.empty[Int])
            value <- Signal.initRef(Maybe.empty[String])
            ui = cascade.selection(value).onChange(_ => seen.updateAndGet(_ + 1).unit).wired(open, refs, focus, Present("cs"))
            _       <- clickBubbling(ui)(saying("p-cascadeselect-option-text", "Hamburg"))
            got     <- value.get
            changes <- seen.get
            still   <- open.get
            groups  <- refs.openPaths
        yield assert(got == Present("Hamburg") && changes == 1 && !still && groups.isEmpty)
    }

    "a click inside the open chain that lands on no leaf leaves it open and picks nothing" in {
        val targets: List[(String, UI.Ast.Element => Boolean)] = List(
            ("group row", saying("p-cascadeselect-option-text", "Europe")),
            ("list space", classed("p-cascadeselect-list-container")),
            ("panel frame", classed("p-cascadeselect-overlay"))
        )
        Kyo.foreach(targets) { (name, target) =>
            for
                (open, _, value, ui) <- field(open = true)
                _                    <- clickBubbling(ui)(target)
                still                <- open.get
                got                  <- value.get
            yield (name, still && got.isEmpty)
        }.map(rs => assert(rs.forall(_._2), rs.filterNot(_._2).map(_._1).mkString("toggled or picked: ", ", ", "")))
    }

    "a click on a group row opens that group, and the chain stays open" in {
        for
            (open, refs, _, ui) <- field(open = true)
            _                   <- clickBubbling(ui)(saying("p-cascadeselect-option-text", "Europe"))
            still               <- open.get
            europe              <- refs.openPaths.map(_.contains(List(0)))
        yield assert(still && europe)
    }

    "the field is the one element listening for a click: not its label, not its chevron" in {
        def listening(ui: UI)(using Frame): Chunk[String] < Sync =
            elements(ui).map(_.filter(declaresClick).map(_.attrs.cssClasses.headOption.getOrElse("?")))
        for
            (_, _, _, closed) <- field(open = false)
            live              <- listening(closed)
            open              <- Signal.initRef(false)
            refs              <- refsOf
            focus             <- Anchored.init(List.empty[Int])
            disabled          <- listening(cascade.disabled(true).wired(open, refs, focus, Present("cs")))
        yield
            assert(live == Chunk("p-cascadeselect"), live.toString)
            assert(disabled.isEmpty, "a disabled field opens nothing")
        end for
    }

    "an option the signal adds before an open group leaves that group open" in {
        val europe =
            uic.CascadeItem.group("Europe")(uic.CascadeItem.group("Germany")(uic.CascadeItem.leaf("Berlin")), uic.CascadeItem.leaf("Bern"))
        val sydney = uic.CascadeItem.leaf("Sydney")
        for
            live  <- Signal.initRef(Seq(europe, sydney))
            open  <- Signal.initRef(true)
            refs  <- Submenus.init()
            focus <- Anchored.init(List.empty[Int])
            value <- Signal.initRef(Absent: Maybe[String])
            ui = uic.CascadeSelect[String]().options(live)(identity).id("cs").selection(value).wired(open, refs, focus, Present("cs"))
            group  <- leaf(ui, "Europe")
            _      <- click(group)
            before <- renderHtml(ui)
            _      <- live.set(Seq(uic.CascadeItem.leaf("Asia"), europe, sydney))
            after  <- renderHtml(ui)
        yield
            assert(before.contains("Germany"), "Europe's group is open")
            assert(after.contains("Germany"), "and stays open once Asia arrives before it")
        end for
    }

    // ---- rule A ----

    private val leaves = Set("Berlin", "Hamburg", "Bern", "Sydney")

    /** The trigger, which opens the chain and carries its keyboard: what a reader reaches, and what says it is fixed. */
    private def trigger(t: UI)(using Frame): UI.Ast.Element < Sync = elementWithClass(t, "p-cascadeselect")

    private def opens(c: UI.Ast.Element): Boolean = declaresClick(c) || c.attrs.onKeyDown.nonEmpty

    private def readOnly(c: UI.Ast.Element): Boolean = c.attrs.ariaAttrs.get("readonly").contains("true")

    private def triggered(t: UI)(using Frame): Boolean < Sync = elementsWithClass(t, "p-cascadeselect").map(_.exists(declaresClick))

    /** The leaf the label shows. */
    private def picked(t: UI)(using Frame): Maybe[String] < Sync =
        elementWithClass(t, "p-cascadeselect-label").map(l => Maybe(textOf(l)).filter(leaves.contains))

    /** A click on the root leaf Sydney in the chain the trigger opened. */
    private def pickSydney(t: UI)(using Frame): Any < Async =
        clickBubbling(t)((e: UI.Ast.Element) => e.attrs.cssClasses.contains("p-cascadeselect-option-text") && textOf(e) == "Sydney")

    private def maybeSlot(
        name: String,
        hear: (uic.CascadeSelect[String], Maybe[String] => UI.Action) => Frame ?=> uic.CascadeSelect[String],
        event: String
    ) =
        new SlotCase[uic.CascadeSelect[String], Maybe[String]](
            name = name,
            bare = Present(cascade),
            bind = v => cascade.selection(v),
            hear = hear,
            event = event,
            render = _.render,
            start = Absent,
            next = Present("Sydney"),
            control = trigger(_),
            shows = picked(_),
            answers = opens,
            saysFixed = readOnly,
            ready = Present(triggered(_)),
            after = Seq(pickSydney(_))
        )

    // A pick is reported to onChange, then to onPick: each of them hears it.
    holds(maybeSlot("selection over Maybe", (c, f) => c.onChange(f), "onChange"))
    heardBy(maybeSlot("selection over Maybe, heard by onPick", (c, f) => c.onPick(l => f(Present(l))), "onPick"))

    private def leafSlot(
        name: String,
        hear: (uic.CascadeSelect[String], String => UI.Action) => Frame ?=> uic.CascadeSelect[String],
        event: String
    ) =
        new SlotCase[uic.CascadeSelect[String], String](
            name = name,
            // A field over the leaf itself always holds one: unbound, a cascade holds `Maybe`.
            bare = Absent,
            bind = v => cascade.selection(v),
            hear = hear,
            event = event,
            render = _.render,
            start = "Bern",
            next = "Sydney",
            control = trigger(_),
            shows = t => picked(t).map(_.getOrElse(throw new AssertionError("the field shows no leaf"))),
            answers = opens,
            saysFixed = readOnly,
            ready = Present(triggered(_)),
            after = Seq(pickSydney(_))
        )

    holds(leafSlot("selection", (c, f) => c.onPick(f), "onPick"))
    heardBy(leafSlot(
        "selection, heard by onChange",
        (c, f) => c.onChange(m => m.fold(throw new AssertionError("a field over the leaf reported none"))(f)),
        "onChange"
    ))

    "a selection a ref holds, or one a disabled field shows, draws no card" in {
        def errors(c: uic.CascadeSelect[String]): Chunk[UI.Ast.Element] < Async = elementsWithClass(c.render, "p-uic-key-error")
        for
            held   <- Signal.initRef("Bern")
            twoWay <- errors(cascade.selection(held))
            off    <- errors(cascade.selection(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && off.isEmpty, s"the ref takes the pick, or nobody can pick: ${(twoWay ++ off).map(textOf)}")
        end for
    }

    holds(new SlotCase[uic.CascadeSelect[String], Boolean](
        name = "open",
        bare = Present(cascade),
        bind = cascade.open(_),
        hear = (c, f) => c.onOpenChange(f),
        event = "onOpenChange",
        render = _.render,
        start = false,
        next = true,
        control = trigger(_),
        shows = opened(_),
        answers = opens,
        saysFixed = readOnly,
        ready = Present(triggered(_)),
        // A click on the chevron reaches the trigger: the field is the one element listening.
        act = Present(clickBubbling(_)(_.attrs.cssClasses.contains("p-cascadeselect-dropdown")))
    ))

    private def opened(t: UI)(using Frame) = elementsWithClass(t, "p-cascadeselect-overlay").map(_.nonEmpty)

    // A chain open by a constant somebody hears, bound one way or to a ref still closes on a click: the reader asks, and
    // the owner decides.
    heardBy(new SlotCase[uic.CascadeSelect[String], Boolean](
        name = "open, from open",
        bare = Absent,
        bind = cascade.open(_),
        hear = (c, f) => c.onOpenChange(f),
        event = "onOpenChange",
        render = _.render,
        start = true,
        next = false,
        control = trigger(_),
        shows = opened(_),
        answers = opens,
        ready = Present(triggered(_)),
        act = Present(clickBubbling(_)(_.attrs.cssClasses.contains("p-cascadeselect-dropdown")))
    ))

    // The combobox has no readonly state for whether its list is open, so a chain fixed open says nothing beyond
    // aria-expanded: it is fixed because nothing on the field pretends to close it, while the leaves stay the reader's.
    "a panel fixed open stays: nothing outside dismisses it, and the field answers no click that would close it" in {
        for
            t     <- mountedWhen(cascade.open(true).render)(opened(_))
            shown <- opened(t)
            out   <- elementsWithClass(t, "p-uic-overlay-backdrop")
            tr    <- trigger(t)
        yield
            assert(shown, "the panel shows")
            assert(out.isEmpty, "no backdrop takes an outside click")
            assert(!declaresClick(tr), "a click on the field would close nothing, so the field takes none")
            assert(tr.attrs.tabIndex.contains(0) && tr.attrs.onKeyDown.nonEmpty, "the field stays the tab stop with the chain's keyboard")
            assert(tr.attrs.ariaAttrs.get("expanded").contains("true"), "and says the list is open")
            assert(!readOnly(tr), "the selection is still the reader's, so the field is not read-only")
        end for
    }

end CascadeSelectTest
