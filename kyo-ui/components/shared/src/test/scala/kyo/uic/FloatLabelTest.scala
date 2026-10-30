package kyo.uic

import kyo.*
import kyo.UI.*

/** How `uic.FloatLabel` learns that the field it wraps is filled. */
class FloatLabelTest extends UicTest:

    "around an AutoComplete it adds no region: the field stamps p-filled from the text it already reads" in {
        for
            text <- Signal.initRef("")
            ui = uic.FloatLabel(uic.AutoComplete[String]().options(Seq("Berlin"))(identity).value(text), "City").render
            regions <- regionsAbove(ui, "p-autocomplete-input")
            empty   <- elementWithClass(ui, "p-autocomplete-input")
            _       <- text.set("Ber")
            typed   <- elementWithClass(ui, "p-autocomplete-input")
        yield
            assert(regions == 1, s"the AutoComplete's own region only; one around its mount rebuilds it when the field fills: $regions")
            assert(!empty.attrs.cssClasses.contains("p-filled") && typed.attrs.cssClasses.contains("p-filled"))
    }

    // The field marks itself filled for the client (kyo-ui's filledClass), so the label needs no region around the field,
    // which drew the whole field again on every keystroke into a ref and never knew the text of an unbound one.
    "around an Input or a TextArea it adds no region: the field marks itself filled for the client" in {
        for
            text <- Signal.initRef("Ada")
            input = uic.FloatLabel(uic.Input().value(text), "Name").render
            area  = uic.FloatLabel(uic.TextArea().value(text), "Notes").render
            inRegion <- regionsAbove(input, "p-inputtext")
            taRegion <- regionsAbove(area, "p-textarea")
            inHtml   <- renderHtml(input)
            taHtml   <- renderHtml(area)
        yield
            assert(inRegion == 0 && taRegion == 0, s"regions above the field: input $inRegion, textarea $taRegion")
            assert(classInHtml(inHtml, "p-filled") && inHtml.contains("""data-kyo-filled="p-filled""""), inHtml)
            assert(classInHtml(taHtml, "p-filled") && taHtml.contains("""data-kyo-filled="p-filled""""), taHtml)
        end for
    }

end FloatLabelTest
