package kyo.uic

import java.time.LocalDate
import kyo.*
import kyo.UI.*
import kyo.uic.DateKind.given

/** A control's validity is patched in place, never rebuilt.
  *
  * Every form control's reactive `invalid(Signal)` / `invalidMessage(Signal)` goes through [[FieldValidity]]. The
  * control is built once: `.p-invalid` is a reactive class and `aria-invalid` a reactive attribute on the element
  * that carries them in the constant render, and the message row is a region of its own AFTER the control, so only
  * the row comes and goes. A control rebuilt on a validity change replaces the element the reader is typing into:
  * the keystroke that turns a field invalid lands in a node that is already gone, and focus and caret go with it.
  */
class FieldValidityTest extends UicTest:

    private def message(text: String)(using Frame): SignalRef[Maybe[String]] < Sync =
        Signal.initRef[Maybe[String]](Present(text))

    /** The element's static classes without `p-invalid`, which is what names it in both renders. */
    private def nameOf(e: UI.Ast.Element)
        : String = (e.getClass.getSimpleName +: e.attrs.cssClasses.filterNot(_ == "p-invalid")).mkString(".")

    private def messageRows(ui: UI)(using Frame): Int < Sync =
        elementsWithClass(ui, "p-uic-invalid-message").map(_.size)

    "a text field with a reactive message is built once, and its validity is patched on it" - {
        def check(build: Signal[Maybe[String]] => UI, cls: String)(using Frame, kyo.test.AssertScope) =
            for
                msg <- message("Required")
                ui = build(msg)
                regions <- regionsAbove(ui, cls)
                before  <- elementWithClass(ui, cls)
                redOn   <- before.attrs.reactiveClasses.get("p-invalid").fold(Kyo.lift(Maybe.empty[Boolean]))(_.current.map(Present(_)))
                ariaOn  <- before.attrs.reactiveAttrs.get("aria-invalid").fold(Kyo.lift(Maybe.empty[String]))(_.current.map(Present(_)))
                rowsOn  <- messageRows(ui)
                rowAt   <- regionsAbove(ui, "p-uic-invalid-message")
                _       <- msg.set(Absent)
                after   <- elementWithClass(ui, cls)
                redOff  <- after.attrs.reactiveClasses.get("p-invalid").fold(Kyo.lift(Maybe.empty[Boolean]))(_.current.map(Present(_)))
                ariaOff <- after.attrs.reactiveAttrs.get("aria-invalid").fold(Kyo.lift(Maybe.empty[String]))(_.current.map(Present(_)))
                rowsOff <- messageRows(ui)
            yield
                assert(regions == 0, s"the $cls element sits under $regions validity region(s); it must sit under none")
                assert(before eq after, s"the $cls element was rebuilt when its message cleared")
                assert(redOn == Present(true) && redOff == Present(false), s".p-invalid is a class channel: $redOn, $redOff")
                assert(ariaOn == Present("true") && ariaOff == Present("false"), s"aria-invalid is an attribute channel: $ariaOn, $ariaOff")
                assert(rowsOn == 1 && rowsOff == 0, s"the message row comes and goes: $rowsOn, $rowsOff")
                assert(rowAt == 1, s"the message row is one region of its own, found at depth $rowAt")
        "Input" in check(m => Input().invalidMessage(m).render, "p-inputtext")
        "TextArea" in check(m => TextArea().invalidMessage(m).render, "p-textarea")
    }

    "the message row is a sibling after the control, not a child of it" in {
        for
            msg <- message("Required")
            ui = TextArea().invalidMessage(msg).render
            area <- elementWithClass(ui, "p-textarea")
            rows <- messageRows(area)
        yield
            ui match
                case f: UI.Ast.Fragment[?] =>
                    assert(f.children.size == 2, s"control and message region: ${f.children}")
                    assert(f.children(0) eq area, "the control comes first")
                    assert(f.children(1).isInstanceOf[UI.Ast.Reactive[?]], "the message row is a region of its own")
                case other => fail(s"expected the control followed by its message region, got $other")
            end match
            assert(rows == 0, "nothing inside the control is the message")
    }

    "where a card trails the field, the message row stays the field's immediate next sibling" - {
        // The pickers render a `.p-uic-key-error` card after the field when the held value names no option. The message
        // belongs to the field, so it sits between the two, as the constant render always put it.
        def order(ui: UI, root: String)(using Frame): Chunk[String] < Sync =
            def kind(n: UI): Chunk[String] < Sync = n match
                case e: UI.Ast.Element if e.attrs.cssClasses.contains(root)                    => Chunk("field")
                case e: UI.Ast.Element if e.attrs.cssClasses.contains("p-uic-key-error")       => Chunk("card")
                case e: UI.Ast.Element if e.attrs.cssClasses.contains("p-uic-invalid-message") => Chunk("message")
                case r: UI.Ast.Reactive[?]                                                     =>
                    r.signal.current(using r.frame).map(c =>
                        elementsWithClass(c, "p-uic-invalid-message").map(ms => if ms.nonEmpty then Chunk("message") else Chunk.empty)
                    )
                case _ => Chunk.empty
            def find(n: UI): Chunk[String] < Sync = n match
                case f: UI.Ast.Fragment[?] =>
                    // Nested fragments are one run of siblings in the DOM, so they are flattened first.
                    def siblings(n: UI): Chunk[UI] = n match
                        case g: UI.Ast.Fragment[?] => Chunk.from(g.children).flatMap(siblings)
                        case other                 => Chunk(other)
                    Kyo.foreach(siblings(f))(kind).map(_.flattenChunk).map { ks =>
                        if ks.contains("field") then Kyo.lift(ks) else Kyo.foreach(f.children)(find).map(_.flattenChunk)
                    }
                case e: UI.Ast.Element       => Kyo.foreach(e.children)(find).map(_.flattenChunk)
                case r: UI.Ast.Reactive[?]   => r.signal.current(using r.frame).map(find)
                case k: UI.Ast.KeyedChild[?] => find(k.child)
                case m: UI.Ast.Mounted       => m.placeholderUI.fold(Kyo.lift(Chunk.empty[String]))(find)
                case _                       => Chunk.empty
            find(ui)
        end order
        def check(build: (Signal[Maybe[String]], Boolean) => UI < Sync, root: String)(using Frame, kyo.test.AssertScope) =
            for
                msg   <- message("Required")
                live  <- build(msg, false).map(order(_, root))
                fixed <- build(msg, true).map(order(_, root))
            yield
                assert(fixed == Chunk("field", "message", "card"), s"the constant render: $fixed")
                assert(live == Chunk("field", "message", "card"), s"the reactive render: $live")
        "Select" in check(
            (m, const) =>
                val s = Select[String]().options(Seq("a")).selection(Maybe("z"))
                (if const then s.invalid(true).invalidMessage("Required") else s.invalidMessage(m)).render
            ,
            "p-select"
        )
        "CascadeSelect" in check(
            (m, const) =>
                val s = CascadeSelect[String]().options(Seq(CascadeItem.leaf("a")))(identity).selection(Maybe("z"))
                (if const then s.invalid(true).invalidMessage("Required") else s.invalidMessage(m)).render
            ,
            "p-cascadeselect"
        )
        "MultiSelect" in check(
            (m, const) =>
                Signal.initRef(Set("z")).map { held =>
                    val s = MultiSelect[String]().options(Seq("a")).selection(held)
                    (if const then s.invalid(true).invalidMessage("Required") else s.invalidMessage(m)).render
                },
            "p-multiselect"
        )
        "TreeSelect" in check(
            (m, const) =>
                Signal.initRef(Set("z")).map { held =>
                    val s = TreeSelect[String]().nodes(_.node("a", "a")).selection(held)
                    (if const then s.invalid(true).invalidMessage("Required") else s.invalidMessage(m)).render
                },
            "p-treeselect"
        )
    }

    "an explicit invalid(Signal) overrides the message: the row shows only while both say so" in {
        for
            msg <- message("Required")
            red <- Signal.initRef(false)
            ui = Input().invalid(red).invalidMessage(msg).render
            off   <- messageRows(ui)
            _     <- red.set(true)
            on    <- messageRows(ui)
            field <- elementWithClass(ui, "p-inputtext")
            cls   <- Kyo.foreach(field.attrs.reactiveClasses.get("p-invalid").toSeq)(_.current)
        yield
            assert(off == 0, "not red, so no row although a message is set")
            assert(on == 1, "red with a message shows the row")
            assert(cls == Chunk(true), s"the class follows the explicit override: $cls")
    }

    "an explicit constant invalid wins over a reactive message, as an explicit signal does" in {
        for
            msg <- message("Required")
            red = Input().invalid(true).invalidMessage(msg).render
            field  <- elementWithClass(red, "p-inputtext")
            on     <- messageRows(red)
            _      <- msg.set(Absent)
            redCls <- Kyo.foreach(field.attrs.reactiveClasses.get("p-invalid").toSeq)(_.current)
            redNow = field.attrs.cssClasses.contains("p-invalid") || redCls == Chunk(true)
            other <- message("Required")
            calm = Input().invalid(false).invalidMessage(other).render
            calmField <- elementWithClass(calm, "p-inputtext")
            calmRows  <- messageRows(calm)
            calmCls   <- Kyo.foreach(calmField.attrs.reactiveClasses.get("p-invalid").toSeq)(_.current)
        yield
            assert(on == 1, "invalid(true) with a message shows the row")
            assert(redNow, s"invalid(true) stays red after the message clears: $redCls")
            assert(calmRows == 0, "invalid(false) hides the row although a message is set")
            assert(!calmField.attrs.cssClasses.contains("p-invalid") && calmCls != Chunk(true), s"invalid(false) is not red: $calmCls")
    }

    "the constant path is unchanged: no channel, no region" in {
        val ui = Input().invalid(true).invalidMessage("Required").render
        for
            field   <- elementWithClass(ui, "p-inputtext")
            regions <- reactiveNodes(ui)
        yield
            assert(field.attrs.cssClasses.contains("p-invalid"))
            assert(field.attrs.ariaAttrs.get("invalid") == Some("true"))
            assert(field.attrs.reactiveClasses.isEmpty && field.attrs.reactiveAttrs.isEmpty)
            assert(regions.isEmpty, "a constant validity renders no region")
        end for
    }

    /** One row per control that carries the validity slots: how to build it with the slots set, and the class of its
      * root element.
      */
    private type Control = FormControl { type Self <: FormControl }

    private def controls: Seq[(String, Control, String)] =
        def row(name: String, c: Control, root: String) = (name, c, root)
        Seq(
            row("Input", Input(), "p-inputtext"),
            row("TextArea", TextArea(), "p-textarea"),
            row("InputMask", InputMask("999"), "p-inputmask"),
            row("Password", Password(), "p-password"),
            row("InputNumber", InputNumber(), "p-inputnumber"),
            row("InputOtp", InputOtp(), "p-inputotp"),
            row("AutoComplete", AutoComplete[String]().options(Seq("a")), "p-autocomplete"),
            row("CheckBox", CheckBox("a"), "p-checkbox"),
            row("RadioButton", RadioButton("a"), "p-radiobutton"),
            row("ToggleSwitch", ToggleSwitch(), "p-toggleswitch"),
            row("ToggleButton", ToggleButton(), "p-togglebutton"),
            row("Knob", Knob(), "p-knob"),
            row("Slider", Slider(), "p-slider"),
            row("Rating", Rating(), "p-rating"),
            row("ColorPicker", ColorPicker(), "p-colorpicker"),
            row("ColorPicker inline", ColorPicker().inline(true), "p-colorpicker"),
            row("FileUpload", FileUpload(), "p-fileupload"),
            row("DatePicker", DatePicker[LocalDate](), "p-datepicker"),
            row("DatePicker inline", DatePicker[LocalDate]().inline(true), "p-datepicker"),
            row("Select", Select[String]().options(Seq("a", "b")), "p-select"),
            row("MultiSelect", MultiSelect[String]().options(Seq("a", "b")), "p-multiselect"),
            row(
                "CascadeSelect",
                CascadeSelect[String]().options(Seq(CascadeItem.leaf("a"), CascadeItem.leaf("b")))(identity),
                "p-cascadeselect"
            ),
            row("TreeSelect", TreeSelect[String]().nodes(_.node("a", "a")), "p-treeselect"),
            row("Listbox", Listbox[String]().item("A", "a"), "p-listbox"),
            row("SelectButton", SelectButton[String]().options(Seq("a", "b")), "p-selectbutton")
        )
    end controls

    "no control is rebuilt by its validity, and each patches the elements its constant render marks" - {
        controls.foreach { (name, control, root) =>
            name in {
                for
                    msg <- message("Required")
                    live  = (control.invalidMessage(msg): Node).render
                    fixed = (control.invalid(true).invalidMessage("Required"): Node).render
                    regions  <- regionsAbove(live, root)
                    liveEls  <- elements(live)
                    fixedEls <- elements(fixed)
                    rowAt    <- regionsAbove(live, "p-uic-invalid-message")
                    rows     <- messageRows(live)
                yield
                    val classed = fixedEls.filter(_.attrs.cssClasses.contains("p-invalid")).map(nameOf)
                    val aria    = fixedEls.filter(_.attrs.ariaAttrs.contains("invalid")).map(nameOf)
                    assert(classed.nonEmpty, s"$name: the constant render marks no element .p-invalid")
                    assert(regions == 0, s"$name: its root sits under $regions validity region(s); it must sit under none")
                    assert(
                        liveEls.filter(_.attrs.reactiveClasses.contains("p-invalid")).map(nameOf) == classed,
                        s"$name: .p-invalid must be a class channel on exactly $classed"
                    )
                    assert(
                        liveEls.filter(_.attrs.reactiveAttrs.contains("aria-invalid")).map(nameOf) == aria,
                        s"$name: aria-invalid must be an attribute channel on exactly $aria"
                    )
                    assert(rows == 1 && rowAt == 1, s"$name: one message row, one region deep ($rows at $rowAt)")
            }
        }
    }
end FieldValidityTest
