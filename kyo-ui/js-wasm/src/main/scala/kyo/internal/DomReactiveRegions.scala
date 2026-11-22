package kyo.internal

import kyo.*
import org.scalajs.dom
import scala.collection.mutable

/** Mount-scoped registry of live HTML reactive range anchors. */
final private[kyo] class DomReactiveRegions private (
    private val document: dom.Document,
    private val root: dom.Node,
    private val ranges: DomReactiveRegions.RangeTable,
    private val inserted: mutable.HashMap[String, String]
):

    private var open = true

    /** How many registry entries the replacements so far looked at to find the ranges they remove. */
    private var examinedEntries = 0L

    /** The fingerprint of what each range was last painted with, filled only while a devtools sink observes (see
      * `DomBackend.noteWasted`). Kept here because this is where a range's end is known: an entry leaves with its range.
      */
    private val fingerprints = mutable.HashMap.empty[String, Long]

    /** Marks the elements of the range inert and busy while it keeps showing old content as its new content loads, taking focus out of
      * them, since nothing they would send reaches the engine; or takes the marks back. Only the marks this set are taken back, so an
      * `inert` the page set stays. The same marking as the server-push client's.
      */
    def hold(regionId: String, held: Boolean)(using Frame): Unit < Sync =
        Sync.defer {
            if open then
                ranges.get(regionId).foreach { endpoints =>
                    val page   = document.asInstanceOf[dom.html.Document]
                    val active = page.activeElement
                    elementsBetween(endpoints).foreach { element =>
                        if held then
                            if !element.hasAttribute("inert") then
                                element.setAttribute("inert", "")
                                element.setAttribute("aria-busy", "true")
                                element.setAttribute("data-kyo-held", "")
                                if active != null && (active ne page.body) && element.contains(active) then
                                    active.asInstanceOf[dom.html.Element].blur()
                            end if
                        else unmark(element)
                    }
                }
        }

    /** Makes every mount slot inside the range `regionId` one no instance has adopted, for a new instance about to paint it: the slots
      * belong to the instances that showed there before, and a repaint leaves an adopted slot as it is (see `__kyoPatchLogical` in [[MorphScript]]).
      */
    def releaseSlots(regionId: String)(using Frame): Unit < Sync =
        Sync.defer {
            if open then
                ranges.get(regionId).foreach { endpoints =>
                    var current = DomReactiveRegions.next(endpoints.start)
                    while current.nonEmpty && (current.get ne endpoints.end) do
                        DomReactiveRegions.releaseSlotsUnder(document, current.get)
                        current = DomReactiveRegions.next(current.get)
                    end while
                }
        }

    /** Takes back the marks of every held range, for a mount that ends while a range waits and leaves its markup on the page. */
    def releaseHolds(using Frame): Unit < Sync =
        Sync.defer {
            if open then ranges.iterator.foreach((_, endpoints) => elementsBetween(endpoints).foreach(unmark))
        }

    private def unmark(element: dom.Element): Unit =
        if element.hasAttribute("data-kyo-held") then
            element.removeAttribute("inert")
            element.removeAttribute("aria-busy")
            element.removeAttribute("data-kyo-held")

    /** The fingerprint of range `regionId`'s latest observed paint, if one is recorded. */
    private[kyo] def fingerprint(regionId: String): Maybe[Long] = Maybe.fromOption(fingerprints.get(regionId))

    private[kyo] def recordFingerprint(regionId: String, fingerprint: Long): Unit = fingerprints(regionId) = fingerprint

    /** Forgets range `regionId`'s fingerprint after a change that painted no whole region (a list patch), so the next
      * repaint is not compared against markup the range no longer shows.
      */
    private[kyo] def forgetFingerprint(regionId: String): Unit = discard(fingerprints.remove(regionId))

    def replace(regionId: String, html: String)(using Frame): Unit < Sync =
        replaceWith(regionId, html, keepCurrent = false)(_ => false)((_, _) => ())((_, _, _) => ())

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
      *
      * `keepCurrent` marks `html` as the range's first value since its markup was inserted. Such a value is kept, with
      * no node touched, when its serialized markup equals the serialization of the range's markup as it was inserted.
      * The comparison is against the inserted markup, not the live nodes, because the live nodes are rewritten once
      * inserted. It covers markup only (structure, attributes, text): live property state such as an input's typed value
      * is deliberately left alone.
      *
      * Later values also retain nodes when the live content already matches, including form properties. An edited field
      * therefore still gets repaired even when its attributes match the replacement markup.
      */
    private[kyo] def replaceWith[A](regionId: String, html: String, keepCurrent: Boolean)(
        tryMorph: DomReactiveRegions.MorphTarget => Boolean
    )(
        before: (Seq[dom.Element], Seq[dom.Element]) => A
    )(
        after: (A, Seq[dom.Element], Boolean) => Unit
    )(using Frame): Unit < Sync =
        Sync.Unsafe.defer {
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
            val incomingContent = classifyIncoming(regionId, fragment)
            val unchanged       = keepCurrent &&
                (Maybe.fromOption(inserted.remove(regionId)) match
                    case Present(recorded) =>
                        (liveHost, incomingContent) match
                            case (DomReactiveRegions.LiveHost.Siblings(_), DomReactiveRegions.IncomingContent.Semantic(_)) =>
                                recorded == serialize(fragment.cloneNode(true))
                            case _ => false
                    case Absent => false)
            if !unchanged then
                replaceContent(regionId, endpoints, range, liveHost, fragment, incomingContent)(tryMorph)(before)(after)
        }
    end replaceWith

    private def serialize(content: dom.Node): String =
        val probe = document.createElement("div")
        discard(probe.appendChild(content))
        probe.innerHTML
    end serialize

    private def serializeBetween(endpoints: DomReactiveRegions.Endpoints): String =
        val content = document.createDocumentFragment()
        var current = DomReactiveRegions.next(endpoints.start)
        while current.nonEmpty && (current.get ne endpoints.end) do
            discard(content.appendChild(current.get.cloneNode(true)))
            current = DomReactiveRegions.next(current.get)
        end while
        serialize(content)
    end serializeBetween

    private def recordInserted(): Unit =
        ranges.iterator.foreach((id, endpoints) => inserted(id) = serializeBetween(endpoints))

    private def replaceContent[A](
        regionId: String,
        endpoints: DomReactiveRegions.Endpoints,
        range: dom.Range,
        liveHost: DomReactiveRegions.LiveHost,
        fragment: dom.DocumentFragment,
        incomingContent: DomReactiveRegions.IncomingContent
    )(
        tryMorph: DomReactiveRegions.MorphTarget => Boolean
    )(
        before: (Seq[dom.Element], Seq[dom.Element]) => A
    )(
        after: (A, Seq[dom.Element], Boolean) => Unit
    )(using Frame, AllowUnsafe): Unit =
        val incoming = DomReactiveRegions.scan(document, fragment)
        // Gone from the registry's point of view: inside the range about to be patched, or stranded in a tree
        // this one no longer shares. Every change that takes registered markers out of the document outside a
        // range patch drops their entries as it does it (a retired portal twin through forgetRangesUnder, a
        // rewritten twin or SVG region through rewrite), so a stranded entry is not expected; the test stays as the
        // net for one that slipped through. A dead entry is not a duplicate of the region coming back; it is the
        // same region's corpse, and keeping it would refuse the live one.
        //
        // The test is "same tree", not "in the document": a whole mount can legitimately be patched while it is
        // still detached, and a live portal twin under `<body>` shares the document with the range being
        // patched, so both stay. It is asked only of the ids the payload brings back, since only those can be
        // refused, so a patch pays for its own range and its own payload, never for the rest of the registry.
        val inside        = nestedIn(endpoints)
        lazy val liveRoot = treeRoot(endpoints.start)
        val stranded      = Set.newBuilder[String]

        // A region registered inside a portal twin is not a second copy of itself: the payload carries the portal
        // element inline (the twin's inline original), and this patch either morphs the twin in place (markers,
        // and so the registration, untouched) or replaces the range wholesale, in which case the incoming markers
        // take the id over and the sweep retires the stale twin. It stays out of `removed` for the same reason:
        // nothing inside the live range re-registers it, so dropping it would leave the next write to that region
        // with an unknown id.
        incoming.keysIterator.foreach { id =>
            ranges.get(id).foreach { live =>
                if !inside.contains(id) then
                    if !(treeRoot(live.start) eq liveRoot) then stranded += id
                    else if !inPortalTwin(live.start) then fail(s"Duplicate reactive range id: $id")
            }
        }
        val removed = inside ++ stranded.result()

        val oldElements                                        = elementsBetween(endpoints)
        val newElements                                        = incomingContent.semanticRoots
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
        // Content equal to what the range shows is left as it is, which keeps a pressed click on its node; it then
        // takes the in-place path, as a reconciliation that changes nothing.
        val unchanged = (liveHost, incomingContent) match
            case (DomReactiveRegions.LiveHost.Synthetic(host, _), DomReactiveRegions.IncomingContent.Synthetic(fresh, _)) =>
                host.cloneNode(false).isEqualNode(fresh.cloneNode(false)) && sameElementState(host, fresh) && sameContent(endpoints, fresh)
            case (DomReactiveRegions.LiveHost.Siblings(_), DomReactiveRegions.IncomingContent.Semantic(_)) =>
                sameContent(endpoints, fragment)
            case _ => false
        // Read off the fragment before anything moves its nodes, so it is the markup as inserted.
        val incomingMarkup = incoming.iterator.map((id, nested) => id -> serializeBetween(nested)).toSeq
        // `before` reads the pre-patch DOM (the focused node, the enter and leave path sets, the ghost clones of
        // what is about to depart) and `after` applies the post-patch work to whatever ended up in the range.
        // Both run for the morph as well as for the replacement: a reconciliation is still a patch, and a leave
        // transition, a portal re-home or a focus-auto seed has no business depending on which path painted it.
        val state   = before(oldElements, newElements)
        val morphed = unchanged ||
            (morphTarget match
                case Present(target) => tryMorph(target)
                case Absent          => false)
        if unchanged then discard(DomBackend.morphScript.adoptSlots(endpoints.start, endpoints.end))
        if !morphed then
            range.deleteContents()
            removed.foreach { id =>
                ranges.remove(id)
                discard(inserted.remove(id))
                discard(fingerprints.remove(id))
            }
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
            discard(inserted.addAll(incomingMarkup))
            after(state, insertedRoots, false)
        else
            adoptLive(removed, rescanRange(regionId, endpoints), incomingMarkup)
            after(state, elementsBetween(endpoints), true)
        end if
    end replaceContent

    /** Brings the registry in line with a range that was reconciled in place rather than replaced.
      *
      * The reconciliation keeps nested ranges in place, so which markers ended up in the range is a property of the DOM
      * rather than of the payload: `incoming` describes comments that were cloned or dropped, never the live ones.
      * `live` is therefore read off the DOM, by a rescan of the range or by the [[DomReactiveRegions.PatchScope]] a row
      * patch reports to, and `removed` names the entries the reconciliation may have invalidated.
      *
      * A nested range still waiting for its first value now holds its incoming markup, kept or cloned, so that markup is
      * what its first value is compared against (see [[replaceWith]]). One whose first value already arrived has no entry
      * and gets none; one that is gone takes its entry with it.
      *
      * Only where the range now holds that markup. A reconciliation leaves a keyed slot its instance owns as it is (see
      * `__kyoPatchLogical` in [[MorphScript]]), so such a range can still show its placeholder while the payload brought the instance's
      * content. Recorded anyway, that content would be what the instance's own paint of the same content is compared
      * against, and the paint that has to bring it would be taken for one that changes nothing. Such a range gets no entry,
      * so its next value is applied.
      */
    private def adoptLive(
        removed: Set[String],
        live: mutable.HashMap[String, DomReactiveRegions.Endpoints],
        incomingMarkup: Seq[(String, String)]
    ): Unit =
        removed.foreach(ranges.remove)
        removed.foreach { id =>
            if !live.contains(id) then
                discard(inserted.remove(id))
                discard(fingerprints.remove(id))
        }
        ranges.addAll(live)
        incomingMarkup.foreach { (id, markup) =>
            if live.contains(id) && (!removed.contains(id) || inserted.contains(id)) then
                if serializeBetween(live(id)) == markup then inserted(id) = markup
                else discard(inserted.remove(id))
        }
    end adoptLive

    /** Runs `change`, which replaces or reconciles `subtree` outside any range patch, and brings the registry in line
      * with what stands afterwards: the ranges registered under `subtree` are retired before it runs (a change may drop
      * markers, and a dropped marker's id is unreadable afterwards), and the ranges under the node `change` answers
      * are read off the DOM after it. A portal twin reconciled by the morph of the range around its slot, and an SVG
      * region whose element is replaced, are such rewrites: neither lies between the anchors of a range, so no range
      * patch accounts for the markers they drop and bring.
      */
    private[kyo] def rewrite[N <: dom.Node](subtree: dom.Node)(change: => Maybe[N])(using Frame): Maybe[N] =
        if !open then change
        else
            val before = registeredUnder(subtree)
            val after  = change
            val live   =
                after.map(DomReactiveRegions.scan(document, _)).getOrElse(mutable.HashMap.empty[String, DomReactiveRegions.Endpoints])
            adoptLive(before, live, live.iterator.map((id, endpoints) => id -> serializeBetween(endpoints)).toSeq)
            after
    end rewrite

    /** Drops the ranges registered under `node`, which is about to leave the document. */
    private def forgetUnder(node: dom.Node): Unit =
        if open then
            registeredUnder(node).foreach { id =>
                ranges.remove(id)
                discard(inserted.remove(id))
                discard(fingerprints.remove(id))
            }

    /** The ids of the registered ranges whose opening marker is `node` or lies under it. */
    private def registeredUnder(node: dom.Node): Set[String] =
        val found                        = Set.newBuilder[String]
        def take(marker: dom.Node): Unit =
            DomReactiveRegions.startMarkerId(marker).foreach { id =>
                if ranges.get(id).exists(_.start eq marker) then found += id
            }
        take(node)
        val walker = DomReactiveRegions.commentWalker(document, node)
        while walker.nextNode() != null do take(walker.currentNode)
        found.result()
    end registeredUnder

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
        ranges.innermostAt(path) match
            case Absent             => false
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

    private def sameContent(endpoints: DomReactiveRegions.Endpoints, content: dom.Node)(using AllowUnsafe): Boolean =
        @scala.annotation.tailrec
        def loop(left: Maybe[dom.Node], right: Maybe[dom.Node]): Boolean =
            (left, right) match
                case (Present(node), Absent)                                            => node eq endpoints.end
                case (Present(a), Present(b)) if (a ne endpoints.end) && sameNode(a, b) =>
                    loop(DomReactiveRegions.next(a), DomReactiveRegions.next(b))
                case _ => false
        loop(DomReactiveRegions.next(endpoints.start), DomReactiveRegions.firstChild(content))
    end sameContent

    private def sameNode(left: dom.Node, right: dom.Node)(using AllowUnsafe): Boolean =
        if !left.isEqualNode(right) then false
        else
            (left, right) match
                case (a: dom.Element, b: dom.Element) =>
                    var same          = sameElementState(a, b)
                    val leftChildren  = a.querySelectorAll("*")
                    val rightChildren = b.querySelectorAll("*")
                    var index         = 0
                    while same && index < leftChildren.length do
                        same = sameElementState(leftChildren(index), rightChildren(index))
                        index += 1
                    end while
                    same
                case _ => true
    end sameNode

    private def sameElementState(left: dom.Element, right: dom.Element)(using AllowUnsafe): Boolean =
        var index = 0
        while index < left.attributes.length do
            if left.attributes(index).name.startsWith("data-kyo-prop-") then return false
            index += 1
        end while
        if left.namespaceURI != "http://www.w3.org/1999/xhtml" then true
        else
            // Unsafe: isEqualNode established equal namespaces and tag names, which determine both HTML DOM interfaces.
            left.tagName match
                case "INPUT" =>
                    val a = left.asInstanceOf[dom.html.Input]
                    val b = right.asInstanceOf[dom.html.Input]
                    a.value == b.value && a.checked == b.checked && a.indeterminate == b.indeterminate
                case "TEXTAREA" =>
                    left.asInstanceOf[dom.html.TextArea].value == right.asInstanceOf[dom.html.TextArea].value
                case "SELECT" =>
                    left.asInstanceOf[dom.html.Select].selectedIndex == right.asInstanceOf[dom.html.Select].selectedIndex
                case "OPTION" =>
                    left.asInstanceOf[dom.html.Option].selected == right.asInstanceOf[dom.html.Option].selected
                case _ => true
        end if
    end sameElementState

    private[kyo] def size(using Frame): Int < Sync =
        Sync.defer(ranges.size)

    /** How many paths the path index holds. It is derived from the ranges, so it can never exceed [[size]]. */
    private[kyo] def indexedPaths(using Frame): Int < Sync =
        Sync.defer(ranges.indexedPaths)

    /** The entries registered right now, by id. An entry the registry rewrites is a new object, so comparing two reads by
      * identity says which entries a change touched.
      */
    private[kyo] def entries(using Frame): Map[String, DomReactiveRegions.Endpoints] < Sync =
        Sync.defer(ranges.iterator.toMap)

    /** How many registry entries the replacements so far looked at to find the ranges they remove. */
    private[kyo] def examined(using Frame): Long < Sync =
        Sync.defer(examinedEntries)

    private[kyo] def contains(regionId: String)(using Frame): Boolean < Sync =
        Sync.defer(ranges.contains(regionId))

    private[kyo] def close(using Frame): Unit < Sync =
        Sync.defer {
            if open then
                open = false
                DomReactiveRegions.opened -= this
                ranges.clear()
                inserted.clear()
                fingerprints.clear()
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
      * so pays for the rows that changed. A row the patch only moves keeps its marker nodes, and the entries naming
      * them stay true, so swapping two rows of a thousand writes no entry.
      */
    private[kyo] def withRegionFragment(regionId: String, html: String)(
        reconcile: (DomReactiveRegions.MorphTarget, DomReactiveRegions.PatchScope) => Boolean
    )(using Frame): Boolean < Sync =
        Sync.defer {
            ensureOpen()
            if !ReactiveRegion.isValidHtmlId(regionId) then fail(s"Malformed reactive range id: $regionId")
            ranges.get(regionId) match
                // A range that was never painted has nothing to patch, and nothing a repaint would find either.
                case None            => true
                case Some(endpoints) =>
                    val parent = validatedParent(regionId, endpoints)
                    val range  = document.createRange()
                    range.setStartAfter(endpoints.start)
                    range.setEndBefore(endpoints.end)
                    val fragment = range.createContextualFragment(html)
                    val incoming = DomReactiveRegions.scan(document, fragment)
                    val target   = DomReactiveRegions.MorphTarget(
                        parent,
                        endpoints.start,
                        endpoints.end,
                        fragment,
                        elementsBetween(endpoints),
                        childElements(fragment),
                        incoming.isEmpty
                    )
                    // Read off the fragment before `reconcile` moves its nodes, so it is the markup as inserted.
                    val incomingMarkup = incoming.iterator.map((id, nested) => id -> serializeBetween(nested)).toSeq
                    val scope          = new DomReactiveRegions.PatchScope(regionId)
                    val took           = reconcile(target, scope)
                    if took then adoptLive(scope.retired.toSet, scope.found, incomingMarkup)
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

    /** The ids of the registered ranges inside the range `endpoints` delimits, nested at any depth.
      *
      * Ranges nest properly, so a range lies inside exactly when its opening marker does, and the opening markers
      * inside are the comments between the two anchors in document order. Reading them off the DOM costs the size
      * of the range rather than the size of the registry: a row patch in a list of a thousand rows no longer looks
      * at every other row's entries. An id counts only when the registry's entry for it opens at the very marker
      * found, so a marker the registry does not name (a portal twin's inline original, a stale copy) is not taken
      * for the range it names, and a region registered inside a portal twin under `<body>` stays out.
      */
    private def nestedIn(endpoints: DomReactiveRegions.Endpoints): Set[String] =
        val found  = Set.newBuilder[String]
        val walker = DomReactiveRegions.commentWalker(document, endpoints.start.parentNode)
        walker.currentNode = endpoints.start
        var node = walker.nextNode()
        while node != null && (node ne endpoints.end) do
            DomReactiveRegions.startMarkerId(node).foreach { id =>
                examinedEntries += 1
                if ranges.get(id).exists(_.start eq node) then found += id
            }
            node = walker.nextNode()
        end while
        found.result()
    end nestedIn

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

    /** Does this live marker sit inside a portal twin, an element `adoptPortals` re-homed to `<body>`?
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

    final private[kyo] case class Endpoints(start: dom.Comment, end: dom.Comment)

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
            }

        def remove(id: String): Unit =
            if byId.remove(id).nonEmpty then
                val base = ReactiveRegion.baseIdOf(id)
                byBase.get(base).foreach { ids =>
                    ids -= id
                    if ids.isEmpty then discard(byBase.remove(base))
                }

        def clear(): Unit =
            byId.clear()
            byBase.clear()

        /** The most deeply nested range that owns `path`. */
        def innermostAt(path: Seq[String]): Maybe[Endpoints] =
            byBase.get(ReactiveRegion.htmlId(path)) match
                case None      => Absent
                case Some(ids) =>
                    var best = ids(0)
                    var i    = 1
                    while i < ids.length do
                        // The depth suffix is decimal and the bare id is a prefix of every suffixed one, so a deeper
                        // range has the longer id, and of two ids of one length the greater lies deeper.
                        val id = ids(i)
                        if id.length > best.length || (id.length == best.length && id > best) then best = id
                        i += 1
                    end while
                    Present(byId(best))
        end innermostAt
    end RangeTable

    private object RangeTable:
        def apply(ranges: mutable.HashMap[String, Endpoints]): RangeTable = new RangeTable(ranges)

    /** What a row-level patch tells the registry, so the registry follows the patch instead of re-reading the list.
      *
      * `retire` must be called before the patch mutates a logical child it is about to repaint or remove: a morph
      * is free to drop markers, and a dropped marker's id can no longer be read off the DOM afterwards. `placed`
      * is called after a child was repainted or inserted, with the sibling run the result occupies, and reads the
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
        val open                        = mutable.ArrayBuffer.empty[(String, dom.Comment)]
        def visit(node: dom.Node): Unit =
            startMarkerId(node) match
                case Present(id) => open += ((id, node.asInstanceOf[dom.Comment]))
                case Absent      =>
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

    /** The registries of the mounts open right now. A portal twin lives under `<body>`, outside the mount whose region
      * put it there, so a rewrite of one is matched to its registry by the slot it stands in for. JavaScript runs one
      * thread, and every change happens inside a mount's init or close.
      */
    private val opened = mutable.ArrayBuffer.empty[DomReactiveRegions]

    /** How many registries are open now: none once every mount on the page has closed. For tests. */
    private[kyo] def openCount: Int = opened.size

    /** Whether `registry` is among the open ones. For tests. */
    private[kyo] def isOpen(registry: DomReactiveRegions): Boolean = opened.exists(_ eq registry)

    /** Runs `change` over `subtree` and keeps the registry of the mount holding `owner` true to what it leaves (see
      * [[DomReactiveRegions.rewrite]]). Without an open registry holding `owner` it only runs `change`.
      *
      * The newest registry that holds it: a mount writes its container's whole content, so what stands there is the last one's. An older
      * registry over the same container can still be open, since a mount's result comes before the last finalizers of its scope.
      */
    private[kyo] def rewriting[N <: dom.Node](owner: dom.Node, subtree: dom.Node)(change: => Maybe[N])(using Frame): Maybe[N] =
        opened.findLast(registry => registry.root.contains(owner)) match
            case Some(registry) => registry.rewrite(subtree)(change)
            case None           => change

    /** Drops, from every open registry, the ranges whose opening marker is `node` or lies under it, for a node about to
      * leave the document outside any range patch: a portal twin the sweep retires. Each registry keeps only entries
      * whose markers stand in its mount's tree, so nothing has to find and drop a dead entry later. Only an entry whose
      * opening marker IS the node found goes, so a marker the registry does not name leaves its id's entry alone.
      */
    private[kyo] def forgetRangesUnder(node: dom.Node): Unit =
        opened.foreach(_.forgetUnder(node))

    def init(root: dom.Element)(using Frame): DomReactiveRegions < (Sync & Scope) =
        for
            registry <- Sync.defer {
                val document = ownerDocument(root)
                val registry = new DomReactiveRegions(document, root, RangeTable(scan(document, root)), mutable.HashMap.empty)
                registry.recordInserted()
                opened += registry
                registry
            }
            _ <- Scope.ensure(registry.close)
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

    /** Drops the adoption flags (`m`, `s`) of `node` and of every slot marker under it, keeping each marker's id and key. */
    private[kyo] def releaseSlotsUnder(document: dom.Document, node: dom.Node): Unit =
        releaseSlot(node)
        if node.nodeType == dom.Node.ELEMENT_NODE then
            val walker = commentWalker(document, node)
            while walker.nextNode() != null do releaseSlot(walker.currentNode)
    end releaseSlotsUnder

    private def releaseSlot(node: dom.Node): Unit =
        startMarkerId(node).foreach { id =>
            val flags = markerFlags(node).split(' ').filter(_.nonEmpty)
            val kept  = flags.filterNot(flag => flag == "m" || flag == "s")
            if kept.length != flags.length then node.asInstanceOf[dom.Comment].data = openMarkerData(id, kept.map(" " + _).mkString)
        }

    private def commentWalker(document: dom.Document, root: dom.Node): dom.TreeWalker =
        document.createTreeWalker(root, 128, null, false)

    private def fail(message: String)(using Frame): Nothing =
        throw UIException(message)

end DomReactiveRegions
