package kyo.internal

import kyo.*
import org.scalajs.dom
import scala.collection.mutable
import scala.scalajs.js

/** Mount-scoped registry of live HTML reactive range anchors. */
final private[kyo] class DomReactiveRegions private (
    private val document: dom.Document,
    private val ranges: DomReactiveRegions.RangeTable
):

    private var open = true

    def replace(regionId: String, html: String)(using Frame): Unit < Sync =
        replaceWith(regionId, html)(_ => false)((_, _) => ())((_, _, _) => ())

    /** Replace the content of range `regionId` with `html`, morphing it in place where the morph can.
      *
      * `tryMorph` is offered the whole reconciliation and answers whether it took it. It receives a
      * [[DomReactiveRegions.MorphTarget]] rather than the two element lists a wholesale replacement needs, because a
      * morph reconciles NODES: text between two elements is part of the range and invisible in an element list, and
      * the marker pair is the only thing that says where the range stops. Handing over the anchors and the parsed
      * fragment is a deliberate widening of this seam against upstream; a narrower one cannot express an in-place
      * reconciliation, and dropping back to element lists would silently reintroduce the wholesale rebuild that
      * destroys node identity on every region re-render.
      *
      * The offer is made only for the plain sibling case. A synthetic `<tbody>` host on either side juggles the
      * anchors between the host and its table, which is the wholesale path's business; the morph declines those by
      * never being asked.
      */
    private[kyo] def replaceWith[A](regionId: String, html: String)(
        tryMorph: DomReactiveRegions.MorphTarget => Boolean
    )(
        before: (Seq[dom.Element], Seq[dom.Element]) => A
    )(
        after: (A, Seq[dom.Element], Boolean) => Unit
    )(using Frame): Unit < Sync =
        Sync.defer {
            ensureOpen()
            if !ReactiveRegion.isValidHtmlId(regionId) then fail(s"Malformed reactive range id: $regionId")
            val endpoints = ranges.getOrElse(regionId, fail(s"Unknown reactive range: $regionId"))
            val parent    = validatedParent(regionId, endpoints)
            val range     = document.createRange()
            range.setStartAfter(endpoints.start)
            range.setEndBefore(endpoints.end)

            val liveHost = parent match
                case element: dom.Element
                    if element.tagName == "TBODY" && element.getAttribute("data-kyo-range-host") == regionId =>
                    val table = DomReactiveRegions.parent(element).getOrElse(fail(s"Reactive table range host is detached: $regionId"))
                    DomReactiveRegions.LiveHost.Synthetic(element, table)
                case _ => DomReactiveRegions.LiveHost.Siblings(parent)
            val fragment = liveHost match
                case DomReactiveRegions.LiveHost.Synthetic(host, _) =>
                    val parser = document.createRange()
                    parser.selectNode(host)
                    parser.createContextualFragment(html)
                case DomReactiveRegions.LiveHost.Siblings(_) =>
                    range.createContextualFragment(html)
            val incoming = DomReactiveRegions.scan(document, fragment)
            // Gone from the registry's point of view: inside the range about to be patched, or stranded in a tree
            // this one no longer shares. The second case is a portal twin the sweep retired — its markers went with
            // it into a detached subtree, and the sweep moves DOM without walking the registry, so the entry
            // outlives the nodes it names. A dead entry is not a duplicate of the region coming back; it is the same
            // region's corpse, and keeping it would refuse the live one.
            //
            // The test is "same tree", not "in the document": a whole mount can legitimately be patched while it is
            // still detached, and a live portal twin under `<body>` shares the document with the range being
            // patched, so both stay.
            val liveRoot = treeRoot(endpoints.start)
            val removed = ranges.iterator.collect {
                case (id, nested)
                    if id != regionId && (intersects(range, nested.start) || !(treeRoot(nested.start) eq liveRoot)) =>
                    id
            }.toSet

            // A region registered inside a portal twin is not a second copy of itself: the payload carries the portal
            // element inline (the twin's inline original), and this patch either morphs the twin in place — markers,
            // and so the registration, untouched — or replaces the range wholesale, in which case the incoming markers
            // take the id over and the sweep retires the stale twin. It stays out of `removed` for the same reason:
            // nothing inside the live range re-registers it, so dropping it would leave the next write to that region
            // with an unknown id.
            incoming.keysIterator.foreach { id =>
                if ranges.contains(id) && !removed.contains(id) && !inPortalTwin(ranges(id).start) then
                    fail(s"Duplicate reactive range id: $id")
            }

            val oldElements     = elementsBetween(endpoints)
            val incomingContent = classifyIncoming(regionId, fragment)
            val newElements     = incomingContent.semanticRoots
            val morphTarget: Maybe[DomReactiveRegions.MorphTarget] = (liveHost, incomingContent) match
                case (DomReactiveRegions.LiveHost.Siblings(siblingParent), DomReactiveRegions.IncomingContent.Semantic(_)) =>
                    Present(
                        DomReactiveRegions.MorphTarget(
                            siblingParent,
                            endpoints.start,
                            endpoints.end,
                            fragment,
                            oldElements,
                            newElements,
                            incoming.isEmpty
                        )
                    )
                case _ => Absent
            // `before` reads the pre-patch DOM (the focused node, the enter and leave path sets, the ghost clones of
            // what is about to depart) and `after` applies the post-patch work to whatever ended up in the range.
            // Both run for the morph as well as for the replacement: a reconciliation is still a patch, and a leave
            // transition, a portal re-home or a focus-auto seed has no business depending on which path painted it.
            val state = before(oldElements, newElements)
            val morphed = morphTarget match
                case Present(target) => tryMorph(target)
                case Absent          => false
            if !morphed then
                range.deleteContents()
                removed.foreach(ranges.remove)
                val insertedRoots = (liveHost, incomingContent) match
                    case (
                            DomReactiveRegions.LiveHost.Synthetic(host, _),
                            DomReactiveRegions.IncomingContent.Synthetic(incomingHost, _)
                        ) =>
                        syncHostAttributes(host, incomingHost, regionId)
                        var child = DomReactiveRegions.firstChild(incomingHost)
                        while child.nonEmpty do
                            val next = DomReactiveRegions.next(child.get)
                            discard(host.insertBefore(child.get, endpoints.end))
                            child = next
                        end while
                        elementsBetween(endpoints)
                    case (
                            DomReactiveRegions.LiveHost.Synthetic(host, table),
                            DomReactiveRegions.IncomingContent.Semantic(roots)
                        ) =>
                        discard(table.insertBefore(endpoints.start, host))
                        discard(table.insertBefore(fragment, host))
                        discard(table.insertBefore(endpoints.end, host))
                        discard(table.removeChild(host))
                        roots
                    case (
                            DomReactiveRegions.LiveHost.Siblings(parent),
                            DomReactiveRegions.IncomingContent.Synthetic(host, roots)
                        ) =>
                        discard(parent.insertBefore(fragment, endpoints.end))
                        DomReactiveRegions.firstChild(host) match
                            case Present(first) => discard(host.insertBefore(endpoints.start, first))
                            case Absent         => discard(host.appendChild(endpoints.start))
                        discard(host.appendChild(endpoints.end))
                        roots
                    case (
                            DomReactiveRegions.LiveHost.Siblings(parent),
                            DomReactiveRegions.IncomingContent.Semantic(roots)
                        ) =>
                        discard(parent.insertBefore(fragment, endpoints.end))
                        roots
                ranges.addAll(incoming)
                after(state, insertedRoots, false)
            else
                // The morph reconciles nested ranges in place, so which markers ended up in the range is a property
                // of the DOM rather than of the payload: `incoming` describes comments that were cloned or dropped,
                // never the live ones. Reading the range back is the only account that cannot drift.
                removed.foreach(ranges.remove)
                ranges.addAll(rescanRange(regionId, endpoints))
                after(state, elementsBetween(endpoints), true)
            end if
        }
    end replaceWith

    /** The first element inside the range that owns `path`, `Absent` when no range owns it or the range has no
      * element in it. A range is delimited by comments, so a region path addresses no element of its own; a
      * path-addressed command or measure resolves through here when the path carries no element. The client
      * twin is `__kyoResolveEl` in `HtmlRenderer.clientJs`; keep the two in lockstep.
      */
    private[kyo] def firstElementAt(path: Seq[String]): Maybe[dom.Element] =
        var found = Maybe.empty[dom.Element]
        // Nested ranges that share the path hold the same content, so the outermost answers for all of them.
        ranges.atPath(path, innermost = false).foreach { endpoints =>
            var current = DomReactiveRegions.next(endpoints.start)
            while found.isEmpty && current.nonEmpty && (current.get ne endpoints.end) do
                current.get match
                    case element: dom.Element => found = Present(element)
                    case _                    => ()
                current = DomReactiveRegions.next(current.get)
            end while
        }
        found
    end firstElementAt

    /** Write `value` as the whole content of the range that owns `path`; `false` when no range owns it.
      *
      * A text node holds literal characters, so the string is written as-is: no escaping, no parser, and no
      * repaint of the surrounding region. The common shape (the range holds exactly one text node) assigns
      * that node's data, which keeps its identity and every DOM-local thing hanging off it; any other shape
      * is replaced by a single fresh text node.
      */
    private[kyo] def setTextAt(path: Seq[String], value: String): Boolean =
        // The innermost range of the path: only a leaf region binds its text, and writing through an outer range
        // would take the inner one's markers for content and delete them.
        ranges.atPath(path, innermost = true) match
            case Absent => false
            case Present(endpoints) =>
                val first = DomReactiveRegions.next(endpoints.start)
                first match
                    case Present(node: dom.Text) if DomReactiveRegions.next(node).exists(_ eq endpoints.end) =>
                        if node.data != value then node.data = value
                    case _ =>
                        DomReactiveRegions.parent(endpoints.start).foreach { parent =>
                            var current = DomReactiveRegions.next(endpoints.start)
                            while current.nonEmpty && (current.get ne endpoints.end) do
                                val next = DomReactiveRegions.next(current.get)
                                discard(parent.removeChild(current.get))
                                current = next
                            end while
                            discard(parent.insertBefore(document.createTextNode(value), endpoints.end))
                        }
                end match
                true
        end match
    end setTextAt

    private[kyo] def size(using Frame): Int < Sync =
        Sync.defer(ranges.size)

    /** How many paths the path index holds. It is derived from the ranges, so it can never exceed [[size]]. */
    private[kyo] def indexedPaths(using Frame): Int < Sync =
        Sync.defer(ranges.indexedPaths)

    /** The ids registered right now. */
    private[kyo] def ids(using Frame): Set[String] < Sync =
        Sync.defer(ranges.iterator.map(_._1).toSet)

    /** How many entries the registry has written or dropped so far: what a patch cost it, as a number. */
    private[kyo] def registryWrites(using Frame): Int < Sync =
        Sync.defer(ranges.writes)

    private[kyo] def contains(regionId: String)(using Frame): Boolean < Sync =
        Sync.defer(ranges.contains(regionId))

    private[kyo] def close(using Frame): Unit < Sync =
        Sync.defer {
            if open then
                open = false
                ranges.clear()
        }

    private def ensureOpen()(using Frame): Unit =
        if !open then fail("Reactive range registry is closed")

    /** Hand `reconcile` the live range of `regionId` and a payload parsed in its context, and keep the registry
      * honest if it takes the job.
      *
      * `replaceWith` owns one policy: the payload IS the new content. A list patch has a different one: the
      * payload holds only the rows that changed, and which live row each one replaces is decided by key rather
      * than by position. That decision needs the parsed nodes and the live range, which is what this hands over;
      * everything the two policies share, parsing in the right context and reading the registry back off the DOM
      * afterwards, stays here rather than being written twice.
      *
      * `reconcile` answers whether it took the job. `false` must mean it changed nothing, since the caller then
      * falls back to a whole-list repaint over the very same range.
      *
      * The registry follows the patch through the [[DomReactiveRegions.PatchScope]] the reconciler reports to, and
      * so pays for the rows that changed. A row the patch only MOVES keeps its marker nodes, and the entries naming
      * them stay true; re-reading the whole range instead tore down and rebuilt a thousand entries to swap two
      * rows of a thousand.
      */
    private[kyo] def withRegionFragment(regionId: String, html: String)(
        reconcile: (DomReactiveRegions.MorphTarget, DomReactiveRegions.PatchScope) => Boolean
    )(using Frame): Boolean < Sync =
        Sync.defer {
            ensureOpen()
            if !ReactiveRegion.isValidHtmlId(regionId) then fail(s"Malformed reactive range id: $regionId")
            ranges.get(regionId) match
                // A range that was never painted has nothing to patch, and nothing a repaint would find either.
                case None => true
                case Some(endpoints) =>
                    val parent = validatedParent(regionId, endpoints)
                    val range  = document.createRange()
                    range.setStartAfter(endpoints.start)
                    range.setEndBefore(endpoints.end)
                    val fragment = range.createContextualFragment(html)
                    val incoming = DomReactiveRegions.scan(document, fragment)
                    val target = DomReactiveRegions.MorphTarget(
                        parent,
                        endpoints.start,
                        endpoints.end,
                        fragment,
                        elementsBetween(endpoints),
                        childElements(fragment),
                        incoming.isEmpty
                    )
                    val scope = new DomReactiveRegions.PatchScope(regionId)
                    val took  = reconcile(target, scope)
                    if took then
                        scope.retired.foreach(ranges.remove)
                        ranges.addAll(scope.found)
                    took
            end match
        }
    end withRegionFragment

    /** The ranges the live content of `regionId` holds right now, read off the DOM.
      *
      * The morph keeps the markers of a range that survived and clones or drops the rest, so the registry cannot be
      * updated from the payload the way the wholesale path does it. Walking what is actually between the anchors is
      * both simpler and the only version that stays true when the two disagree.
      */
    private def rescanRange(regionId: String, endpoints: DomReactiveRegions.Endpoints)(using
        Frame
    ): mutable.HashMap[String, DomReactiveRegions.Endpoints] =
        val found = mutable.HashMap.empty[String, DomReactiveRegions.Endpoints]
        DomReactiveRegions.scanSiblings(regionId, DomReactiveRegions.next(endpoints.start), endpoints.end, found)
        found
    end rescanRange

    private def validatedParent(regionId: String, endpoints: DomReactiveRegions.Endpoints)(using Frame): dom.Node =
        // The opening marker may carry a flag section after its id (a mount slot, and the `m` the client stamps on
        // once it adopts one), so the id is compared rather than the whole payload.
        if !DomReactiveRegions.startMarkerId(endpoints.start).contains(regionId) ||
            endpoints.end.data != s"${DomReactiveRegions.EndPrefix}$regionId"
        then fail(s"Reactive range markers are corrupted: $regionId")
        (DomReactiveRegions.parent(endpoints.start), DomReactiveRegions.parent(endpoints.end)) match
            case (Present(startParent), Present(endParent)) if startParent eq endParent =>
                var current = DomReactiveRegions.next(endpoints.start)
                while current.nonEmpty && (current.get ne endpoints.end) do
                    current = DomReactiveRegions.next(current.get)
                if current.isEmpty then fail(s"Reactive range end is not after its start: $regionId")
                startParent
            case _ => fail(s"Reactive range anchors are no longer siblings: $regionId")
        end match
    end validatedParent

    private def fail(message: String)(using Frame): Nothing =
        throw UIException(message)

    private def intersects(range: dom.Range, node: dom.Node): Boolean =
        range.asInstanceOf[js.Dynamic].intersectsNode(node).asInstanceOf[Boolean]

    /** The topmost node above this one: the Document for anything attached, the detached subtree's own root
      * otherwise. Two nodes share a tree exactly when this returns the same node for both.
      */
    private def treeRoot(node: dom.Node): dom.Node =
        var top    = node
        var parent = DomReactiveRegions.parent(top)
        while parent.nonEmpty do
            top = parent.get
            parent = DomReactiveRegions.parent(top)
        top
    end treeRoot

    /** Does this live marker sit inside a portal twin — an element `portalSweep` re-homed to `<body>`?
      *
      * Twin of `__kyoInPortalTwin` in HtmlRenderer.clientJs; keep the two in lockstep.
      */
    private def inPortalTwin(node: dom.Node): Boolean =
        // The registry is handed a bare Document (so a detached one can be tested against) and `body` lives on
        // HTMLDocument; every document a mount runs over is one.
        val body    = document.asInstanceOf[dom.HTMLDocument].body
        var current = DomReactiveRegions.parent(node)
        var found   = false
        while !found && current.exists(_.nodeType == dom.Node.ELEMENT_NODE) do
            val element = current.get.asInstanceOf[dom.Element]
            if DomReactiveRegions.parent(element).exists(_ eq body) && element.hasAttribute("data-kyo-portal")
            then found = true
            else current = DomReactiveRegions.parent(element)
        end while
        found
    end inPortalTwin

    private def elementsBetween(endpoints: DomReactiveRegions.Endpoints): Seq[dom.Element] =
        val elements = mutable.ArrayBuffer.empty[dom.Element]
        var current  = DomReactiveRegions.next(endpoints.start)
        while current.nonEmpty && (current.get ne endpoints.end) do
            current.get match
                case element: dom.Element => elements += element
                case _                    => ()
            current = DomReactiveRegions.next(current.get)
        end while
        elements.toSeq
    end elementsBetween

    private def classifyIncoming(regionId: String, fragment: dom.DocumentFragment): DomReactiveRegions.IncomingContent =
        val roots = childElements(fragment)
        if roots.size == 1 && roots.head.tagName == "TBODY" && roots.head.getAttribute("data-kyo-range-host") == regionId then
            val host = roots.head
            DomReactiveRegions.IncomingContent.Synthetic(host, childElements(host))
        else DomReactiveRegions.IncomingContent.Semantic(roots)
        end if
    end classifyIncoming

    private[kyo] def childElements(node: dom.Node): Seq[dom.Element] =
        val elements = mutable.ArrayBuffer.empty[dom.Element]
        var current  = DomReactiveRegions.firstChild(node)
        while current.nonEmpty do
            current.get match
                case element: dom.Element => elements += element
                case _                    => ()
            current = DomReactiveRegions.next(current.get)
        end while
        elements.toSeq
    end childElements

    private def syncHostAttributes(host: dom.Element, incoming: dom.Element, regionId: String): Unit =
        var i = host.attributes.length - 1
        while i >= 0 do
            val name = host.attributes(i).name
            if name != "data-kyo-range-host" then host.removeAttribute(name)
            i -= 1
        end while
        i = 0
        while i < incoming.attributes.length do
            val attribute = incoming.attributes(i)
            if attribute.name != "data-kyo-range-host" then host.setAttribute(attribute.name, attribute.value)
            i += 1
        end while
        host.setAttribute("data-kyo-range-host", regionId)
    end syncHostAttributes

end DomReactiveRegions

private[kyo] object DomReactiveRegions:

    private val StartPrefix = "kyo-rs:"
    private val EndPrefix   = "kyo-re:"

    final private case class Endpoints(start: dom.Comment, end: dom.Comment)

    /** The live ranges by id, and the ids of each path.
      *
      * A range id encodes its path ([[ReactiveRegion.htmlId]]), so the range of a path is found by ENCODING the path
      * once and reading the table. Decoding every held id to compare paths costs the size of the registry times the
      * length of an id, per lookup: a bound text write in a thousand-row table spent 4.4 million hex-digit decodes
      * finding its one text node.
      *
      * One path can own several ranges: a region nested transparently in another shares its path and differs only
      * in the depth suffix. `byBase` keeps those together under the depth-less id, in no particular order; the
      * depth is read off the id when one is picked. Every mutation goes through here so the two maps cannot drift.
      */
    final private class RangeTable private (byId: mutable.HashMap[String, Endpoints]):

        private val byBase = mutable.HashMap.empty[String, mutable.ArrayBuffer[String]]
        byId.keysIterator.foreach(index)

        private def index(id: String): Unit =
            val ids = byBase.getOrElseUpdate(ReactiveRegion.baseIdOf(id), mutable.ArrayBuffer.empty[String])
            if !ids.contains(id) then ids += id

        private var written                              = 0
        def writes: Int                                  = written
        def indexedPaths: Int                            = byBase.size
        def size: Int                                    = byId.size
        def contains(id: String): Boolean                = byId.contains(id)
        def get(id: String): Option[Endpoints]           = byId.get(id)
        def apply(id: String): Endpoints                 = byId(id)
        def getOrElse(id: String, default: => Endpoints) = byId.getOrElse(id, default)
        def iterator: Iterator[(String, Endpoints)]      = byId.iterator

        def addAll(ranges: mutable.HashMap[String, Endpoints]): Unit =
            ranges.foreachEntry { (id, endpoints) =>
                byId.update(id, endpoints)
                index(id)
                written += 1
            }

        def remove(id: String): Unit =
            if byId.remove(id).nonEmpty then
                written += 1
                val base = ReactiveRegion.baseIdOf(id)
                byBase.get(base).foreach { ids =>
                    ids -= id
                    if ids.isEmpty then discard(byBase.remove(base))
                }

        def clear(): Unit =
            byId.clear()
            byBase.clear()

        /** The range that owns `path`: the most deeply nested one when `innermost`, the outermost otherwise. */
        def atPath(path: Seq[String], innermost: Boolean): Maybe[Endpoints] =
            byBase.get(ReactiveRegion.htmlId(path)) match
                case None => Absent
                case Some(ids) =>
                    var best = ids(0)
                    var i    = 1
                    while i < ids.length do
                        // Depth suffixes are fixed-width hex, and the bare id is a prefix of every suffixed one,
                        // so string order IS depth order.
                        val id = ids(i)
                        if (id > best) == innermost then best = id
                        i += 1
                    end while
                    Present(byId(best))
        end atPath
    end RangeTable

    private object RangeTable:
        def apply(ranges: mutable.HashMap[String, Endpoints]): RangeTable = new RangeTable(ranges)

    /** What a row-level patch tells the registry, so the registry follows the patch instead of re-reading the list.
      *
      * `retire` must be called BEFORE the patch mutates a logical child it is about to repaint or remove: a morph
      * is free to drop markers, and a dropped marker's id can no longer be read off the DOM afterwards. `placed`
      * is called AFTER a child was repainted or inserted, with the sibling run the result occupies, and reads the
      * ranges standing there at that moment; marker nodes keep their identity through any later move, so reading
      * early is as good as reading at the end.
      */
    final private[kyo] class PatchScope private[DomReactiveRegions] (regionId: String)(using Frame):
        private[DomReactiveRegions] val retired = mutable.ArrayBuffer.empty[String]
        private[DomReactiveRegions] val found   = mutable.HashMap.empty[String, Endpoints]

        /** Every range inside the logical child starting at `first`, the child's own markers included. */
        def retire(first: dom.Node): Unit =
            eachSpanNode(first)(collectStartIds(_, retired))

        /** The ranges in the sibling run from `first` up to, not including, `until`. */
        def placed(first: dom.Node, until: dom.Node): Unit =
            scanSiblings(regionId, if first == null then Absent else Present(first), until, found)
    end PatchScope

    private def collectStartIds(node: dom.Node, into: mutable.ArrayBuffer[String]): Unit =
        startMarkerId(node).foreach(into += _)
        var child = firstChild(node)
        while child.nonEmpty do
            collectStartIds(child.get, into)
            child = next(child.get)
    end collectStartIds

    /** Read the ranges of the sibling run `[first, until)` off the DOM, nested ones included.
      *
      * A run is walked rather than a payload trusted because a morph keeps the markers of a range that survived
      * and clones or drops the rest: what stands in the DOM is the only account that cannot drift.
      */
    private def scanSiblings(
        regionId: String,
        first: Maybe[dom.Node],
        until: dom.Node,
        found: mutable.HashMap[String, Endpoints]
    )(using Frame): Unit =
        val open = mutable.ArrayBuffer.empty[(String, dom.Comment)]
        def visit(node: dom.Node): Unit =
            startMarkerId(node) match
                case Present(id) => open += ((id, node.asInstanceOf[dom.Comment]))
                case Absent =>
                    endMarkerId(node).foreach { id =>
                        if open.isEmpty || open.last._1 != id then
                            fail(s"Crossed reactive ranges after morphing $regionId: found $id")
                        val (_, start) = open.remove(open.length - 1)
                        found.update(id, Endpoints(start, node.asInstanceOf[dom.Comment]))
                    }
            end match
            var child = firstChild(node)
            while child.nonEmpty do
                val following = next(child.get)
                visit(child.get)
                child = following
            end while
        end visit
        var node = first
        while node.nonEmpty && !(node.get eq until) do
            val following = next(node.get)
            visit(node.get)
            node = following
        end while
        if open.nonEmpty then fail(s"Reactive range start marker has no end after morphing $regionId: ${open.last._1}")
    end scanSiblings

    /** The matching close marker for the span `open` starts, `Absent` when the run is unbalanced (the caller then
      * treats the comment as an ordinary node). Ids are unique among siblings, so a direct match suffices.
      */
    private[kyo] def spanClose(open: dom.Node, id: String): Maybe[dom.Node] =
        var node   = open.nextSibling
        var result = Maybe.empty[dom.Node]
        while result.isEmpty && node != null do
            if endMarkerId(node).contains(id) then result = Present(node)
            node = node.nextSibling
        end while
        result
    end spanClose

    /** Apply `f` to every node of the logical child starting at `first`, its markers included. */
    private[kyo] def eachSpanNode(first: dom.Node)(f: dom.Node => Unit): Unit =
        val last = startMarkerId(first).flatMap(spanClose(first, _)).getOrElse(first)
        var node = first
        var stop = false
        while !stop && node != null do
            val following = node.nextSibling
            stop = node eq last
            f(node)
            node = following
        end while
    end eachSpanNode

    /** Everything a range morph reconciles against.
      *
      * `start` and `end` are the live anchors: the range is the sibling run strictly between them, which is why the
      * morph is handed the anchors rather than a node list (a list cannot say where to append). `fragment` holds the
      * parsed incoming content, `oldElements` and `newElements` the semantic roots either side, and
      * `incomingRangesEmpty` records whether the payload declares nested ranges of its own, which a morph that does
      * not maintain the registry has to decline.
      */
    final private[kyo] case class MorphTarget(
        parent: dom.Node,
        start: dom.Comment,
        end: dom.Comment,
        fragment: dom.DocumentFragment,
        oldElements: Seq[dom.Element],
        newElements: Seq[dom.Element],
        incomingRangesEmpty: Boolean
    )

    private enum LiveHost:
        case Siblings(parent: dom.Node)
        case Synthetic(host: dom.Element, table: dom.Node)

    private enum IncomingContent:
        case Semantic(roots: Seq[dom.Element])
        case Synthetic(host: dom.Element, roots: Seq[dom.Element])

        def semanticRoots: Seq[dom.Element] = this match
            case Semantic(roots)     => roots
            case Synthetic(_, roots) => roots
    end IncomingContent

    def init(root: dom.Element)(using Frame): DomReactiveRegions < (Sync & Scope) =
        for
            registry <- Sync.defer(new DomReactiveRegions(ownerDocument(root), RangeTable(scan(ownerDocument(root), root))))
            _        <- Scope.ensure(registry.close)
        yield registry

    private def scan(document: dom.Document, root: dom.Node)(using Frame): mutable.HashMap[String, Endpoints] =
        val found  = mutable.HashMap.empty[String, Endpoints]
        val seen   = mutable.HashSet.empty[String]
        val open   = mutable.ArrayBuffer.empty[(String, dom.Comment)]
        val walker = commentWalker(document, root)
        var next   = nextComment(walker)
        while next.nonEmpty do
            val comment = next.get
            val value   = comment.data
            if value.startsWith(StartPrefix) then
                val id = ReactiveRegion.markerIdOf(value.substring(StartPrefix.length))
                if !ReactiveRegion.isValidHtmlId(id) then fail(s"Malformed reactive range id: $id")
                if seen.contains(id) then fail(s"Duplicate reactive range id: $id")
                seen += id
                open += ((id, comment))
            else if value.startsWith(EndPrefix) then
                val id = value.substring(EndPrefix.length)
                if !ReactiveRegion.isValidHtmlId(id) then fail(s"Malformed reactive range id: $id")
                if open.isEmpty then fail(s"Reactive range end marker has no start: $id")
                val (expected, start) = open.last
                if id != expected then fail(s"Crossed reactive ranges: expected $expected, found $id")
                discard(open.remove(open.length - 1))
                (parent(start), parent(comment)) match
                    case (Present(startParent), Present(endParent)) if startParent eq endParent =>
                        found(id) = Endpoints(start, comment)
                    case _ => fail(s"Reactive range anchors are not siblings: $id")
                end match
            end if
            next = nextComment(walker)
        end while
        if open.nonEmpty then fail(s"Reactive range start marker has no end: ${open.last._1}")
        found
    end scan

    /** Whether `node` is one of the comments that delimit a reactive range. */
    private[kyo] def isRangeMarker(node: dom.Node): Boolean =
        startMarkerId(node).nonEmpty || endMarkerId(node).nonEmpty

    /** The range id `node` opens, `Absent` when it is not an opening marker.
      *
      * A morph walks the range as LOGICAL children, where a nested range and everything between its markers is one
      * child; recognizing the two ends is what makes that possible, so the prefixes are read here rather than spelled
      * again at every caller.
      */
    private[kyo] def startMarkerId(node: dom.Node): Maybe[String] = markerId(node, StartPrefix)

    /** The range id `node` closes, `Absent` when it is not a closing marker. */
    private[kyo] def endMarkerId(node: dom.Node): Maybe[String] = markerId(node, EndPrefix)

    private def markerId(node: dom.Node, prefix: String): Maybe[String] =
        if node.nodeType != dom.Node.COMMENT_NODE then Absent
        else
            val data = node.asInstanceOf[dom.Comment].data
            if data.startsWith(prefix) then Present(ReactiveRegion.markerIdOf(data.substring(prefix.length)))
            else Absent

    /** The payload of an opening marker for `id` carrying `flags` (which already includes its leading space). */
    private[kyo] def openMarkerData(id: String, flags: String): String = s"$StartPrefix$id$flags"

    /** The flag section of an opening marker (`m`, `s`, `k=`), `""` when it carries none. */
    private[kyo] def markerFlags(node: dom.Node): String =
        if node.nodeType != dom.Node.COMMENT_NODE then ""
        else
            val data  = node.asInstanceOf[dom.Comment].data
            val space = data.indexOf(' ')
            if space < 0 then "" else data.substring(space + 1)

    private def nextComment(walker: dom.TreeWalker): Maybe[dom.Comment] =
        val node = walker.nextNode()
        if node == null then Absent else Present(node.asInstanceOf[dom.Comment])

    private def parent(node: dom.Node): Maybe[dom.Node] =
        val value = node.parentNode
        if value == null then Absent else Present(value)

    private def firstChild(node: dom.Node): Maybe[dom.Node] =
        val value = node.firstChild
        if value == null then Absent else Present(value)

    private def next(node: dom.Node): Maybe[dom.Node] =
        val value = node.nextSibling
        if value == null then Absent else Present(value)

    private def ownerDocument(node: dom.Node): dom.Document =
        val value = node.ownerDocument
        if value == null then dom.document else value

    private def commentWalker(document: dom.Document, root: dom.Node): dom.TreeWalker =
        document.createTreeWalker(root, 128, null, false)

    private def fail(message: String)(using Frame): Nothing =
        throw UIException(message)

end DomReactiveRegions
