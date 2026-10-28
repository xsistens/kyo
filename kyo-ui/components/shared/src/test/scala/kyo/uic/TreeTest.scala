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

    private def labelOf(ui: UI, text: String)(using Frame): UI.Ast.Element < Sync =
        elementsWithClass(ui, "p-tree-node-label").map(_.find(textOf(_) == text).getOrElse(throw new AssertionError(s"no node $text")))

    // Which nodes are open and chosen lives somewhere else: the tree shows it and hands a toggle or a pick over.
    "expansion and selection bound one way are reported and not written" in {
        for
            exp     <- Signal.initRef(Set("src"))
            sel     <- Signal.initRef(Set.empty[String])
            seenExp <- Signal.initRef(Set.empty[String])
            seenSel <- Signal.initRef(Set.empty[String])
            hi      <- Signal.initRef(-1)
            ui = uic.Tree[String]().nodes(nodes*).selectionMode(uic.SelectionMode.Single)
                .expanded(exp.readOnly).onExpandedChange(seenExp.set)
                .selection(sel.readOnly).onSelectionChange(seenSel.set).wired(hi, "t")
            toggles <- elementsWithClass(ui, "p-tree-node-toggle-button")
            _       <- click(toggles(0))
            main    <- labelOf(ui, "main")
            _       <- click(main)
            e       <- exp.get
            s       <- sel.get
            se      <- seenExp.get
            ss      <- seenSel.get
        yield
            assert(e == Set("src") && se.isEmpty, s"expansion held $e, reported $se")
            assert(s.isEmpty && ss == Set("main"), s"selection held $s, reported $ss")
    }

    "one-way bindings that nothing hears say so, and onNodeToggle or onItemClick count as hearing" in {
        for
            exp <- Signal.initRef(Set.empty[String])
            sel <- Signal.initRef(Set.empty[String])
            hi  <- Signal.initRef(-1)
            base = uic.Tree[String]().nodes(nodes*).selectionMode(uic.SelectionMode.Single).expanded(exp.readOnly).selection(sel.readOnly)
            deaf  <- elementsWithClass(base.wired(hi, "t"), "p-uic-key-error")
            byOld <- elementsWithClass(base.onNodeToggle(_ => ()).onItemClick(_ => ()).wired(hi, "t"), "p-uic-key-error")
        yield
            val text = deaf.map(textOf).mkString
            assert(text.contains("onExpandedChange") && text.contains("onSelectionChange"), s"nobody hears: $text")
            assert(byOld.isEmpty, s"the older events hear: ${byOld.map(textOf)}")
    }

    "constants nobody hears are fixed: no toggle opens, no click picks, and no card" in {
        val fixed = uic.Tree[String]().nodes(nodes*).selectionMode(uic.SelectionMode.Single).expanded(Set("src")).selection(Set("main"))
        for
            toggles <- elementsWithClass(fixed.render, "p-tree-node-toggle-button")
            labels  <- elementsWithClass(fixed.render, "p-tree-node-label")
            card    <- elementsWithClass(fixed.render, "p-uic-key-error")
        yield
            assert(toggles.nonEmpty && toggles.forall(t => !declaresClick(t)), "a fixed expansion opens nothing")
            assert(labels.nonEmpty && labels.forall(l => !declaresClick(l)), "a fixed selection picks nothing")
            assert(card.isEmpty, s"fixed constants: ${card.map(textOf)}")
        end for
    }

    "an unbound tree keeps what the reader opens and picks" in {
        def opened(t: UI): Boolean < Sync = elementsWithClass(t, "p-tree-node-label").map(_.exists(l => textOf(l) == "main"))
        def picked(t: UI): Boolean < Sync =
            elementsWithClass(t, "p-tree-node-selected").map(_.exists(n => textOf(n).contains("build.sbt")))
        val free = uic.Tree[String]().nodes(nodes*).selectionMode(uic.SelectionMode.Single)
        for
            open <- publishedAfterReady(
                free.render,
                t => elementsWithClass(t, "p-tree-node-toggle-button").map(_.exists(declaresClick)),
                until = opened
            )(t => elementsWithClass(t, "p-tree-node-toggle-button").map(bs => click(bs.head)))
            pick <- publishedAfterReady(
                free.render,
                t => elementsWithClass(t, "p-tree-node-label").map(_.exists(declaresClick)),
                until = picked
            )(t =>
                labelOf(t, "build.sbt").map(click)
            )
        yield
            assert(open.nonEmpty, "the tree never showed the branch the reader opened")
            assert(pick.nonEmpty, "the tree never showed the node the reader picked")
        end for
    }

end TreeTest
