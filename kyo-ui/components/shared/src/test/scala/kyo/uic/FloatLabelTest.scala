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

end FloatLabelTest
