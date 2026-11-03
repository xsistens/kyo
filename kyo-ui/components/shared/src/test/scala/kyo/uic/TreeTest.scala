package kyo.uic

import kyo.*
import kyo.UI.*

/** How `uic.Tree` subscribes the state its rows are drawn from, and its expansion and selection held to rule A
  * ([[ReactiveValueContract]]).
  */
class TreeTest extends ReactiveValueContract:

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

    private def labelOf(ui: UI, text: String)(using Frame): UI.Ast.Element < Sync =
        elementsWithClass(ui, "p-tree-node-label").map(_.find(textOf(_) == text).getOrElse(throw new AssertionError(s"no node $text")))

    "onNodeToggle and onItemClick hear the slots too: neither a constant they hear nor a signal is a mistake" in {
        for
            exp <- Signal.initRef(Set.empty[String])
            sel <- Signal.initRef(Set.empty[String])
            base = uic.Tree[String]().nodes(nodes*).selectionMode(uic.SelectionMode.Single).expanded(exp.readOnly).selection(sel.readOnly)
            byOld <- elementsWithClass(base.onNodeToggle(_ => ()).onItemClick(_ => ()).render, "p-uic-diagnostic")
            live = uic.Tree[String]().nodes(nodes*).selectionMode(uic.SelectionMode.Single).expanded(Set.empty[String])
                .onNodeToggle(_ => ()).selection(Set.empty[String]).onItemClick(_ => ()).render
            toggles <- elementsWithClass(live, "p-tree-node-toggle-button")
            label   <- labelOf(live, "build.sbt")
        yield
            assert(byOld.isEmpty, s"the older events hear: ${byOld.map(textOf)}")
            assert(toggles.exists(declaresClick) && declaresClick(label), "a constant somebody hears is not fixed")
        end for
    }

    // ---- rule A ----

    private def tree(using Frame) = uic.Tree[String]().nodes(nodes*)

    private def toggles(t: UI)(using Frame) = elementsWithClass(t, "p-tree-node-toggle-button")

    // The toggle is what a pointer opens a branch with. A tree has no readonly state for its expansion and its toggles are
    // hidden from assistive tech, so a fixed expansion says nothing beyond each branch's aria-expanded: it is fixed because
    // no toggle answers (the keys ignore a fixed expansion too, `toggleNode`'s guard).
    private def expandedCase(name: String, hear: (uic.Tree[String], Set[String] => UI.Action) => Frame ?=> uic.Tree[String]) =
        new SlotCase[uic.Tree[String], Set[String]](
            name = name,
            bare = Present(tree),
            bind = tree.expanded(_),
            hear = hear,
            event = "onExpandedChange",
            render = _.render,
            start = Set.empty,
            next = Set("src"),
            control = toggles(_).map(_.headOption.getOrElse(throw new AssertionError("no toggle"))),
            shows = elementsWithClass(_, "p-tree-node-label").map(ls => if ls.exists(textOf(_) == "main") then Set("src") else Set.empty),
            isFixed = Present(toggles(_).map(ts => ts.nonEmpty && ts.forall(t => !declaresClick(t)))),
            ready = Present(toggles(_).map(_.exists(declaresClick)))
        )

    holds(expandedCase("expanded", (t, f) => t.onExpandedChange(f)))
    heardBy(expandedCase("expanded, heard by onNodeToggle", (t, f) => t.onNodeToggle(k => f(Set(k)))))

    private val idsByLabel = Map("src" -> "src", "main" -> "main", "test" -> "test", "build.sbt" -> "build")

    private def single(using Frame) = tree.selectionMode(uic.SelectionMode.Single)

    // A tree has no readonly state for its selection either, and a treeitem's aria-disabled would also take the branch it
    // opens away from the reader, so a fixed selection says nothing beyond each node's aria-selected: it is fixed because no
    // label takes a click (the keys ignore it too, `selectNode`'s guard).
    private def selectionCase(name: String, hear: (uic.Tree[String], Set[String] => UI.Action) => Frame ?=> uic.Tree[String]) =
        new SlotCase[uic.Tree[String], Set[String]](
            name = name,
            bare = Present(single),
            bind = single.selection(_),
            hear = hear,
            event = "onSelectionChange",
            render = _.render,
            start = Set.empty,
            next = Set("build"),
            control = labelOf(_, "build.sbt"),
            shows = elementsWithClass(_, "p-tree-node-selected").map(_.map(n => idsByLabel(textOf(n))).toSet),
            isFixed = Present(elementsWithClass(_, "p-tree-node-label").map(ls => ls.nonEmpty && ls.forall(l => !declaresClick(l)))),
            ready = Present(elementsWithClass(_, "p-tree-node-label").map(_.exists(declaresClick)))
        )

    holds(selectionCase("selection", (t, f) => t.onSelectionChange(f)))
    heardBy(selectionCase("selection, heard by onItemClick", (t, f) => t.onItemClick(k => f(Set(k)))))

end TreeTest
