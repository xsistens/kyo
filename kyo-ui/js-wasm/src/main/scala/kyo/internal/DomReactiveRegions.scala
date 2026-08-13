package kyo.internal

import kyo.*
import org.scalajs.dom
import scala.collection.mutable
import scala.scalajs.js

/** Mount-scoped registry of live HTML reactive range anchors. */
final private[kyo] class DomReactiveRegions private (
    private val document: dom.Document,
    private val ranges: mutable.HashMap[String, DomReactiveRegions.Endpoints]
):

    private var open = true

    def replace(regionId: String, html: String)(using Frame): Unit < Sync =
        replaceWith(regionId, html)((_, _, _) => false)((_, _) => ())((_, _) => ())

    private[kyo] def replaceWith[A](regionId: String, html: String)(
        tryMorph: (Seq[dom.Element], Seq[dom.Element], Boolean) => Boolean
    )(
        before: (Seq[dom.Element], Seq[dom.Element]) => A
    )(
        after: (A, Seq[dom.Element]) => Unit
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
            val removed = ranges.iterator.collect {
                case (id, nested) if id != regionId && intersects(range, nested.start) => id
            }.toSet

            incoming.keysIterator.foreach { id =>
                if ranges.contains(id) && !removed.contains(id) then
                    fail(s"Duplicate reactive range id: $id")
            }

            val oldElements     = elementsBetween(endpoints)
            val incomingContent = classifyIncoming(regionId, fragment)
            val newElements     = incomingContent.semanticRoots
            if !tryMorph(oldElements, newElements, incoming.isEmpty) then
                val state = before(oldElements, newElements)
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
                after(state, insertedRoots)
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
        ranges.foreachEntry { (id, endpoints) =>
            if found.isEmpty && ReactiveRegion.pathOf(id).contains(path) then
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
        var written = false
        ranges.foreachEntry { (id, endpoints) =>
            if !written && ReactiveRegion.pathOf(id).contains(path) then
                written = true
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
        }
        written
    end setTextAt

    private[kyo] def size(using Frame): Int < Sync =
        Sync.defer(ranges.size)

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

    private def validatedParent(regionId: String, endpoints: DomReactiveRegions.Endpoints)(using Frame): dom.Node =
        if endpoints.start.data != s"${DomReactiveRegions.StartPrefix}$regionId" ||
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

    private def childElements(node: dom.Node): Seq[dom.Element] =
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
            registry <- Sync.defer(new DomReactiveRegions(ownerDocument(root), scan(ownerDocument(root), root)))
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
                val id = value.substring(StartPrefix.length)
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
