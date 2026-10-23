package kyo.uic

import kyo.*
import kyo.UI.*

/** Where `uic.CascadeSelect` puts the keyboard highlight when its chain opens, and what closes the
  * chain again.
  *
  * Focus never leaves the trigger, however deep the chain goes, so the whole cascade keyboard runs
  * from there: the key that opens it has to land ON a row, and the Tab that carries the reader out
  * of the widget has to take every open panel with it.
  */
class CascadeSelectTest extends UicTest:

    private val options = List(
        uic.CascadeItem.group("Europe")(
            uic.CascadeItem.group("Germany")(uic.CascadeItem.leaf("Berlin"), uic.CascadeItem.leaf("Hamburg")),
            uic.CascadeItem.leaf("Bern")
        ),
        uic.CascadeItem.leaf("Sydney")
    )

    private def cascade(using Frame) = uic.CascadeSelect[String]().options(options)(identity).id("cs")

    private def refsOf(using Frame): List[(List[Int], SignalRef[Boolean])] < Async =
        Kyo.foreach(cascade.groupPaths)(p => Signal.initRef(false).map(p -> _)).map(_.toList)

    /** Presses `key` on the CLOSED trigger and reports whether the chain opened and where the
      * highlight landed.
      */
    private def opening(key: UI.Keyboard)(using Frame): (Boolean, List[Int]) < Async =
        for
            open  <- Signal.initRef(false)
            refs  <- refsOf
            focus <- Signal.initRef(List.empty[Int])
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
            _     <- Kyo.foreach(refs)((_, r) => r.set(true))
            focus <- Signal.initRef(List(0, 0, 1))
            value <- Signal.initRef(Maybe.empty[String])
            ui = cascade.selection(value).wired(open, refs, focus, Present("cs"))
            trigger <- elementWithClass(ui, "p-cascadeselect")
            _       <- press(trigger, UI.Keyboard.Tab)
            still   <- open.get
            groups  <- Kyo.foreach(refs)((_, r) => r.get)
            back    <- Signal.initRef(true)
            refs2   <- refsOf
            focus2  <- Signal.initRef(List(0))
            ui2 = cascade.selection(value).wired(back, refs2, focus2, Present("cs"))
            trigger2  <- elementWithClass(ui2, "p-cascadeselect")
            _         <- press(trigger2, UI.Keyboard.Tab, shift)
            stillBack <- back.get
        yield assert(
            !still && !stillBack && groups.forall(_ == false),
            "the chain's keyboard lives on the trigger the Tab is leaving, so no panel outlives it"
        )
        end for
    }

    "the arrows still walk the level the highlight is on" in {
        for
            open  <- Signal.initRef(true)
            refs  <- refsOf
            focus <- Signal.initRef(List(0))
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
            refs  <- Kyo.foreach(c.groupPaths)(p => Signal.initRef(true).map(p -> _)).map(_.toList)
            focus <- Signal.initRef(List.empty[Int])
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

    "a one-way selection shows the signal, and a pick reports through onPick without writing" in {
        for
            route  <- Signal.initRef("Bern")
            picks  <- Signal.initRef(List.empty[String])
            ui     <- allOpen(cascade.selection(route.readOnly).onPick(l => picks.updateAndGet(_ :+ l).unit))
            chosen <- elementsWithClass(ui, "p-cascadeselect-option-selected")
            target <- leaf(ui, "Hamburg")
            _      <- click(target)
            held   <- route.get
            got    <- picks.get
        yield
            assert(chosen.size == 1, "the leaf the signal holds is the chosen one")
            assert(got == List("Hamburg"))
            assert(held == "Bern")
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
    ): (SignalRef[Boolean], List[(List[Int], SignalRef[Boolean])], SignalRef[Maybe[String]], UI) < Async =
        for
            o     <- Signal.initRef(open)
            refs  <- Kyo.foreach(cascade.groupPaths)(p => Signal.initRef(groupsOpen).map(p -> _)).map(_.toList)
            focus <- Signal.initRef(List.empty[Int])
            value <- Signal.initRef(Maybe.empty[String])
        yield (o, refs, value, cascade.selection(value).wired(o, refs, focus, Present("cs")))

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
            refs  <- Kyo.foreach(cascade.groupPaths)(p => Signal.initRef(true).map(p -> _)).map(_.toList)
            focus <- Signal.initRef(List.empty[Int])
            value <- Signal.initRef(Maybe.empty[String])
            ui = cascade.selection(value).onChange(_ => seen.updateAndGet(_ + 1).unit).wired(open, refs, focus, Present("cs"))
            _       <- clickBubbling(ui)(saying("p-cascadeselect-option-text", "Hamburg"))
            got     <- value.get
            changes <- seen.get
            still   <- open.get
            groups  <- Kyo.foreach(refs)((_, r) => r.get)
        yield assert(got == Present("Hamburg") && changes == 1 && !still && groups.forall(!_))
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
            europe              <- refs.collectFirst { case (List(0), r) => r }.get.get
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
            focus             <- Signal.initRef(List.empty[Int])
            disabled          <- listening(cascade.disabled(true).wired(open, refs, focus, Present("cs")))
        yield
            assert(live == Chunk("p-cascadeselect"), live.toString)
            assert(disabled.isEmpty, "a disabled field opens nothing")
        end for
    }

end CascadeSelectTest
