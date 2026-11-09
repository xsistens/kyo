package kyo.uic

import kyo.*
import kyo.UI.Keyboard

/** Pure keyboard map of an [[OrganizationChart]]'s node cursor: which node a key moves it to, over the chart as it is
  * drawn. No DOM and no effects, so the semantics are unit-tested directly (`OrgChartNavTest`), the shape [[TreeNav]] and
  * [[GridNav]] already use.
  *
  * The arrows follow the drawing rather than the document: a chart is laid out top down with a node's children side by
  * side under it, so Down steps to the first child of an open node, Up to the parent, and Left and Right to the previous
  * and next sibling. Home is the root and End the last node in document order that is on the screen. Nothing wraps: a
  * chart is a place, like a tree, and a reader who holds Right expects to stay on the last sibling.
  *
  * A node under a closed one is not on the screen, so the cursor never lands there: Down from a closed node is not the
  * map's to answer. `Absent` means the key is not this map's, so the host leaves it to the browser.
  */
private[uic] object OrgChartNav:

    /** The chain of nodes from the root down to the node with `key`, root first, or empty when no node has it. */
    def pathTo[K](root: OrgChartNode[K], key: K)(using CanEqual[K, K]): List[OrgChartNode[K]] =
        def walk(n: OrgChartNode[K]): Maybe[List[OrgChartNode[K]]] =
            if n.id == key then Present(List(n))
            else
                n.children.foldLeft(Absent: Maybe[List[OrgChartNode[K]]])((found, c) => found.orElse(walk(c).map(n :: _)))
        walk(root).getOrElse(Nil)
    end pathTo

    /** The nodes on the screen, in document order: the root, and every node whose ancestors are all open. */
    def visible[K](root: OrgChartNode[K], isExpanded: K => Boolean): List[OrgChartNode[K]] =
        def walk(n: OrgChartNode[K]): List[OrgChartNode[K]] =
            n :: (if isExpanded(n.id) then n.children.flatMap(walk) else Nil)
        walk(root)
    end visible

    /** The node `key` moves the cursor at `at` to, or `Absent` when the key is not this map's or has nowhere to go. */
    def onKey[K](root: OrgChartNode[K], isExpanded: K => Boolean, at: K, key: Keyboard)(using CanEqual[K, K]): Maybe[K] =
        import Keyboard.*
        pathTo(root, at) match
            case Nil  => Absent
            case path =>
                val node                       = path.last
                val parent                     = Maybe.fromOption(path.dropRight(1).lastOption)
                def sibling(by: Int): Maybe[K] =
                    parent.flatMap { p =>
                        val i = p.children.indexWhere(_.id == at)
                        Maybe.fromOption(p.children.lift(i + by)).map(_.id)
                    }
                key match
                    case ArrowDown =>
                        if node.children.nonEmpty && isExpanded(node.id) then Present(node.children.head.id) else Absent
                    case ArrowUp    => parent.map(_.id)
                    case ArrowLeft  => sibling(-1)
                    case ArrowRight => sibling(1)
                    case Home       => if root.id == at then Absent else Present(root.id)
                    case End        =>
                        val last = visible(root, isExpanded).last.id
                        if last == at then Absent else Present(last)
                    case _ => Absent
                end match
        end match
    end onKey

end OrgChartNav
