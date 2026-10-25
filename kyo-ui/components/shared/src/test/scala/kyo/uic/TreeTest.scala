package kyo.uic

import kyo.*
import kyo.UI.*

/** How `uic.Tree` subscribes the state its rows are drawn from. */
class TreeTest extends UicTest:

    private val nodes = List(
        TreeNode("src", "src", children = List(TreeNode("main", "main"), TreeNode("test", "test"))),
        TreeNode("build.sbt", "build")
    )

    "the expansion, the selection and the highlight are ONE region over the tree" in {
        for
            exp <- Signal.initRef(Set("src"))
            sel <- Signal.initRef(Set("main"))
            hi  <- Signal.initRef(0)
            ui = uic.Tree[String]().nodes(nodes*).expanded(exp).selection(sel).wired(hi, "t")
            regions <- regionsAbove(ui, "p-tree")
        yield assert(
            regions == 1,
            s"a key writes the highlight, the expansion and the selection; nested regions repaint older state: $regions"
        )
    }

end TreeTest
