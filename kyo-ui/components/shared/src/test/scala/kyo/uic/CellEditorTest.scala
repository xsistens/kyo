package kyo.uic

import kyo.*
import kyo.UI.*

/** The provided cell editors and how they bind the draft. */
class CellEditorTest extends UicTest:

    "the select editor is the Select's own mount, under no region of the draft" in {
        for
            draft <- Signal.initRef("a")
            ui = CellEditor.select(Seq("a", "b"))(EditorParams(draft, "cell", Kyo.unit, Kyo.unit))
            label <- elementWithClass(ui, "p-select-label")
        yield
            val mountAtRoot = ui match
                case _: UI.Ast.Mounted => true
                case _                 => false
            assert(mountAtRoot, s"a region of the draft around the mount rebuilds it, open panel and all, on every write: $ui")
            assert(label.children.exists {
                case t: UI.Ast.Text => t.value == "a"
                case _              => false
            })
    }

end CellEditorTest
