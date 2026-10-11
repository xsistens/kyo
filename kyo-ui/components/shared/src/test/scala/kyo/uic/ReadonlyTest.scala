package kyo.uic

import kyo.*
import kyo.UI.*

/** What `readonly` means, once, across every control that has it.
  *
  * It meant three different things. Select dropped its tab stop; CheckBox, ToggleSwitch and
  * Rating disabled their native input, which also drops the tab stop and makes readonly
  * indistinguishable from disabled to a keyboard and to a screen reader; ToggleButton alone kept
  * the reader on it and simply refused to toggle.
  *
  * ToggleButton was right. A readonly control is one a reader can REACH and read the value of
  * and cannot change: focusable, saying so in the vocabulary its own role has, and inert to every
  * key and click that would change it. `disabled` is the different thing, and stays native.
  *
  * The last part is not expressible from a handler, because a kyo handler runs asynchronously and
  * remotely: by the time it is asked, the browser has already toggled the box. `preventActivation`
  * is the kyo-ui primitive that declines the default in the client instead, the same way
  * `preventScrollKeys` declines the page scroll.
  */
class ReadonlyTest extends UicTest:

    private def input(ui: UI, cls: String)(using Frame): UI.Ast.Element < Sync = elementWithClass(ui, cls)

    private def checkbox(ui: UI, cls: String)(using Frame): UI.Ast.Checkbox < Sync =
        elements(ui).map(_.collectFirst {
            case c: UI.Ast.Checkbox if c.attrs.cssClasses.contains(cls) => c
        }.getOrElse(throw new AssertionError(s"no checkbox with class $cls")))

    "a readonly checkbox stays reachable and refuses to toggle" in {
        for
            ref <- Signal.initRef(true)
            ui = uic.CheckBox().checked(ref).readonly(true).render
            box <- checkbox(ui, "p-checkbox-input")
        yield
            assert(box.attrs.dataAttrs.get("kyo-inert").contains("1"), "the browser's own toggle is declined")
            assert(box.attrs.ariaAttrs.get("readonly").contains("true"))
            assert(!box.disabled.contains(true), "and it is still a tab stop")
            assert(box.onChange.isEmpty, "with no handler wired behind it")
    }

    "a disabled checkbox is the other thing, and stays native" in {
        for
            ref <- Signal.initRef(true)
            ui = uic.CheckBox().checked(ref).disabled(true).render
            box <- checkbox(ui, "p-checkbox-input")
        yield
            assert(box.disabled.contains(true))
            assert(box.attrs.dataAttrs.get("kyo-inert").isEmpty, "disabled needs nothing declined: it is out of reach")
    }

    "a readonly switch answers the same way" in {
        for
            ref <- Signal.initRef(true)
            ui = uic.ToggleSwitch().checked(ref).readonly(true).render
            box <- checkbox(ui, "p-toggleswitch-input")
        yield
            assert(box.attrs.dataAttrs.get("kyo-inert").contains("1"))
            assert(box.attrs.ariaAttrs.get("readonly").contains("true"))
            assert(!box.disabled.contains(true), "and it is still a tab stop")
    }

    "a readonly rating keeps its stars in the tab order" in {
        for
            ui   <- Kyo.lift(uic.Rating().value(3).readonly(true).wired("r"))
            root <- elementWithClass(ui, "p-rating")
            all  <- elements(ui)
        yield
            val radios = all.filter(_.attrs.ariaAttrs.get("label").exists(_.endsWith("stars")))
            assert(root.attrs.dataAttrs.get("kyo-inert").contains("1"))
            assert(root.attrs.ariaAttrs.get("readonly").contains("true"), "the GROUP reports it")
            assert(radios.nonEmpty && radios.forall(_.attrs.ariaAttrs.get("readonly").isEmpty), "a radio has no such state")
    }

    "a readonly select keeps its tab stop and opens nothing" in {
        for
            value <- Signal.initRef("")
            ui = uic.Select[String]().options(Seq("a"))(identity).optionKey(identity).value(value).readonly(true).render
            field <- elementWithClass(ui, "p-select")
        yield
            assert(field.attrs.tabIndex.contains(0), "a readonly field is still one a reader can land on")
            assert(field.attrs.ariaAttrs.get("readonly").contains("true"))
            assert(field.attrs.onKeyDown.isEmpty, "and it answers no key")
    }

    "a readonly toggle button says what a button role can say" in {
        for
            ref <- Signal.initRef(false)
            ui = uic.ToggleButton().checked(ref).readonly(true).render
            btn <- elements(ui).map(_.collectFirst { case b: UI.Ast.Button => b }.get)
        yield
            assert(!btn.disabled.contains(true), "not natively disabled, or the reader could not read it")
            assert(btn.attrs.ariaAttrs.get("disabled").contains("true"), "role=button has no readonly state")
            assert(btn.attrs.ariaAttrs.get("readonly").isEmpty, "so it does not claim one")
            assert(btn.attrs.onClick.isEmpty)
    }

end ReadonlyTest
