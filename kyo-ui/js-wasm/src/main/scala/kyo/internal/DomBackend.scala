package kyo.internal

import kyo.*
import org.scalajs.dom
import org.scalajs.dom.document
import scala.annotation.tailrec
import scala.scalajs.js

/** Scala.js UI backend. Mounts a UI into the browser DOM. */
private[kyo] object DomBackend:

    private[kyo] trait MountDiagnostics:
        def channelClosed(): Unit
        def drainInterrupting(): Unit
        def drainJoined(): Unit
        def dragRuntimeInstalled(runtime: DomDragRuntime.Handle): Unit = ()
        def dragEventQueued(event: UIEvent): Unit                      = ()
        def dragEventHandled(event: UIEvent): Unit                     = ()
        def drainInstalled(drain: Fiber[Unit, Any]): Unit              = ()
        def drainStarted(): Unit                                       = ()
    end MountDiagnostics

    private object NoMountDiagnostics extends MountDiagnostics:
        def channelClosed(): Unit     = ()
        def drainInterrupting(): Unit = ()
        def drainJoined(): Unit       = ()
    end NoMountDiagnostics

    /** One seeded `data-kyo-focus-auto` element and where focus should go when it leaves the document.
      *
      * @param path
      *   `data-kyo-path` of the seeded element
      * @param returnTo
      *   `data-kyo-path` of the element focused just before seeding, `Absent` when nothing was focused
      * @param restore
      *   whether the seeded element declared `data-kyo-focus-restore`
      */
    final private case class FocusSeed(path: String, returnTo: Maybe[String], restore: Boolean)

    /** Seeded focus-auto elements, innermost last. Mirrors `__focusReturnStack` in HtmlRenderer.clientJs. Module-level
      * mutable state is safe: all mutation runs inside `Sync.defer` on the single-threaded JS runtime.
      */
    private var focusReturnStack: Chunk[FocusSeed] = Chunk.empty

    /** The page-scoped drain channel, captured once per mount so the viewport scroll/resize listeners (raw JS
      * callbacks, outside any Kyo context) can bridge their `deliverMeasureById` effect back in via [[fireFromJs]].
      * Set in `mountInto` before any op can be emitted. Module-level mutable state is safe on the single-threaded runtime.
      */
    private var sessionEvents: Maybe[Channel[Unit < Async]] = Absent

    /** Live viewport observers for the SPA transport, keyed by element id. Each entry is the single handler
      * registered for BOTH `window` scroll (capture phase) and resize; Unobserve removes it from both and drops the
      * entry. Backed by a native `js.Map` (mirrors `UIMouseEventOps`), giving `contains`/`apply`/`update`/`remove`.
      */
    private val viewportObservers: js.WrappedMap[String, js.Function1[dom.Event, Unit]] =
        new js.WrappedMap(js.Map.empty[String, js.Function1[dom.Event, Unit]])

    /** The attribute names on `el` the imperative id-addressed channel owns (see [[markOwned]]). */
    private def ownedAttrs(el: dom.Element): Set[String] =
        val d = el.asInstanceOf[js.Dynamic]
        if js.isUndefined(d.__kyoOwn) then Set.empty
        else d.__kyoOwn.asInstanceOf[js.Dictionary[Boolean]].keySet.toSet
    end ownedAttrs

    /** Mark attribute `name` on `el` as owned by the imperative id-addressed channel (SetClassById/SetStyleById),
      * applied out of the render pass so CSS transitions on the toggled class/style fire. The owned names live in a
      * `__kyoOwn` expando dict ON the element, so the flag is reclaimed with the node (no session-lived set that only
      * ever grows) and `morphAttrs` shields each owned attribute BY NAME. Mirrors `__kyoMark` in HtmlRenderer.clientJs.
      */
    private def markOwned(el: dom.Element, name: String): Unit =
        val d = el.asInstanceOf[js.Dynamic]
        val own =
            if js.isUndefined(d.__kyoOwn) then
                val fresh = js.Dictionary.empty[Boolean]
                d.__kyoOwn = fresh.asInstanceOf[js.Any]
                fresh
            else d.__kyoOwn.asInstanceOf[js.Dictionary[Boolean]]
        own.update(name, true)
    end markOwned

    /** Mirror a patched `value` onto the field's DOM PROPERTY, which is what an input or textarea actually renders.
      * The property stops tracking the attribute the first time the user types, so `setAttribute("value", ...)` alone
      * is invisible on any field that has been touched: a clear button writes the bound ref, the attribute changes,
      * and the field still shows what was typed. Assigning only on a real difference leaves a focused field's caret
      * alone, since the echo of the user's own keystroke compares equal. Mirrors `__kyoSyncField` in
      * HtmlRenderer.clientJs.
      */
    private def syncFieldProperty(el: dom.Element, name: String, value: String): Unit =
        if name == "value" && (el.tagName == "INPUT" || el.tagName == "TEXTAREA") then
            val dyn = el.asInstanceOf[js.Dynamic]
            if dyn.value.asInstanceOf[String] != value then dyn.value = value
    end syncFieldProperty

    // ---- Range morph ---------------------------------------------------------------------------------------
    //
    // Reconciling a re-rendered region against its live nodes instead of replacing them is what keeps node identity
    // across a paint: focus, caret, scroll position, an imperatively bound attribute, anything a DOM-local expando
    // holds. Replacing the range is always correct and always loses all of it, so it stays the fallback rather than
    // the default. Twin of `__kyoMorphNode` and its neighbours in HtmlRenderer.clientJs; keep the two in lockstep.

    /** Reconcile a live portal slot against the portal element the payload carries inline, and answer whether that
      * is what this pair is.
      *
      * The pair matches when the incoming node is a portal element, the live node is the inert slot standing in for
      * it at the same path, and the twin the first sweep parked under `<body>` is still there. The twin is then the
      * node that gets reconciled; the slot stays exactly where it is. Anything else is not this case and reconciles
      * the ordinary way.
      */
    private def morphPortalPair(from: dom.Node, to: dom.Node): Boolean =
        (from.nodeType == dom.Node.ELEMENT_NODE && to.nodeType == dom.Node.ELEMENT_NODE) && {
            val slot     = from.asInstanceOf[dom.Element]
            val incoming = to.asInstanceOf[dom.Element]
            val path     = incoming.getAttribute("data-kyo-path")
            path != null &&
            incoming.hasAttribute("data-kyo-portal") &&
            slot.getAttribute("data-kyo-portal-slot") == path && {
                val twin = portalTwin(path)
                (twin != null) && { morphEl(twin, incoming); true }
            }
        }

    /** Whether `to` can be reconciled onto `from` in place, or has to replace it outright. */
    private def morphCompatible(from: dom.Node, to: dom.Node): Boolean =
        from.nodeType == to.nodeType && {
            if from.nodeType != dom.Node.ELEMENT_NODE then true
            else
                val fromEl = from.asInstanceOf[dom.Element]
                val toEl   = to.asInstanceOf[dom.Element]
                fromEl.tagName == toEl.tagName && fromEl.namespaceURI == toEl.namespaceURI
        }

    /** Reconcile one live node toward `toNode`. A node of a different kind or tag cannot be patched into the target,
      * so it is replaced; everything else keeps its identity.
      */
    private def morphNode(fromNode: dom.Node, toNode: dom.Node): Unit =
        if fromNode.nodeType != dom.Node.ELEMENT_NODE then
            if fromNode.nodeValue != toNode.nodeValue then fromNode.nodeValue = toNode.nodeValue
        else morphEl(fromNode.asInstanceOf[dom.Element], toNode.asInstanceOf[dom.Element])

    private def morphEl(fromEl: dom.Element, toEl: dom.Element): Unit =
        morphAttrs(fromEl, toEl)
        // A focused contenteditable would lose its caret if its children were rewritten mid-edit; leave its subtree
        // alone (INPUT and TEXTAREA have no element children, so they need no such guard).
        val editing = (fromEl eq document.activeElement) && fromEl.hasAttribute("contenteditable")
        if !editing then morphChildren(fromEl, toEl)
    end morphEl

    private def morphAttrs(fromEl: dom.Element, toEl: dom.Element): Unit =
        val tag = fromEl.tagName
        val activeInput =
            (fromEl eq document.activeElement) && (tag == "INPUT" || tag == "TEXTAREA")
        // An attribute the imperative id-addressed channel owns is never reconciled: rendered HTML never carries the
        // client-set value, so reconciling would clobber it. Twin of the `own` shield in `__kyoMorphAttrs`.
        val owned   = ownedAttrs(fromEl)
        val toAttrs = toEl.attributes
        var i       = 0
        while i < toAttrs.length do
            val attribute = toAttrs(i)
            val name      = attribute.name
            if !owned.contains(name) then
                if fromEl.getAttribute(name) != attribute.value then fromEl.setAttribute(name, attribute.value)
                // A field renders its `value` PROPERTY, and the property stops tracking the attribute the first time
                // the user types. Writing the attribute alone is therefore invisible on any field that has been typed
                // into: a ref write that clears it leaves the typed text on screen. The attribute may even be
                // unchanged in that case (both empty), so the property is written on every pass rather than only when
                // the attribute moves. The focused field is the exception the block below owns: overwriting its live
                // value with its own echo would move the caret.
                if !activeInput then syncFieldProperty(fromEl, name, attribute.value)
            end if
            i += 1
        end while
        // Remove attributes gone from `to`. Walk the live NamedNodeMap backward so a removal never shifts an index
        // still to be visited (no intermediate collection allocated).
        val fromAttrs = fromEl.attributes
        var j         = fromAttrs.length - 1
        while j >= 0 do
            val name = fromAttrs(j).name
            if !owned.contains(name) && !toEl.hasAttribute(name) then fromEl.removeAttribute(name)
            j -= 1
        end while
        // Active-input preservation: two-way binding echoes each keystroke back as a re-render. Never overwrite the
        // focused field's live `.value` (its caret) with its own echo (the value already matches); assign only a
        // genuine external change (a submit-clear, a programmatic update).
        if activeInput then
            val incoming =
                if tag == "TEXTAREA" then toEl.textContent
                else Maybe(toEl.getAttribute("value")).getOrElse("")
            val dynamic = fromEl.asInstanceOf[js.Dynamic]
            if dynamic.value.asInstanceOf[String] != incoming then dynamic.value = incoming
        end if
    end morphAttrs

    private def morphChildren(fromParent: dom.Element, toParent: dom.Element): Unit =
        morphNodeRun(fromParent, fromParent.firstChild, null, toParent.firstChild, null)

    // ---- logical children ----------------------------------------------------------------------------------
    //
    // A nested range is opened and closed by comments that are siblings of its content, so to a naive sibling walk it
    // looks like several unrelated children. It is one: the markers carry the identity the registry is keyed by, and
    // pairing them up positionally would rewrite marker text and hand the registry a range that is no longer there.
    // The morph therefore walks LOGICAL children, where an opening marker stands for its whole span.

    /** The matching close marker for the span `open` starts, `Absent` when the run is unbalanced (the caller then
      * treats the comment as an ordinary node). Ids are unique among siblings, so a direct match suffices.
      */
    private def spanClose(open: dom.Node, id: String): Maybe[dom.Node] =
        var node   = open.nextSibling
        var result = Maybe.empty[dom.Node]
        while result.isEmpty && node != null do
            if DomReactiveRegions.endMarkerId(node).contains(id) then result = Present(node)
            node = node.nextSibling
        end while
        result
    end spanClose

    /** The reconciliation key of a logical child: an element's `data-kyo-path`, a span's range id, else `Absent` for
      * text and plain comments, which reconcile positionally.
      */
    private def logicalKey(node: dom.Node): Maybe[String] =
        if node.nodeType == dom.Node.ELEMENT_NODE then
            Maybe(node.asInstanceOf[dom.Element].getAttribute("data-kyo-path"))
        else DomReactiveRegions.startMarkerId(node).filter(id => spanClose(node, id).nonEmpty)

    /** The next logical sibling: past the whole span for an opening marker, the next node otherwise. */
    private def logicalNext(node: dom.Node): dom.Node =
        val close: Maybe[dom.Node] = DomReactiveRegions.startMarkerId(node).flatMap(spanClose(node, _))
        close match
            case Present(marker) => marker.nextSibling
            case Absent          => node.nextSibling
    end logicalNext

    /** Apply `f` to every node of the logical child starting at `first`, its markers included. */
    private def eachSpanNode(first: dom.Node)(f: dom.Node => Unit): Unit =
        val last = DomReactiveRegions.startMarkerId(first).flatMap(spanClose(first, _)).getOrElse(first)
        var node = first
        var stop = false
        while !stop && node != null do
            val next = node.nextSibling
            stop = node eq last
            f(node)
            node = next
        end while
    end eachSpanNode

    private def removeLogical(parent: dom.Element, node: dom.Node): Unit =
        eachSpanNode(node)(current => discard(parent.removeChild(current)))

    private def insertLogicalClone(parent: dom.Element, toNode: dom.Node, ref: dom.Node): Unit =
        eachSpanNode(toNode) { current =>
            val fresh = document.importNode(current, true)
            discard(parent.insertBefore(fresh, ref))
            // SMIL animations only start on a node this pass actually inserted; a reused one is already running, and
            // restarting it every paint would snap a chart transition back to its beginning.
            fresh match
                case element: dom.Element => beginAnimationsSync(element)
                case _                    => ()
        }

    /** Reconcile the live sibling run of `parent` that starts at `fromStart` and stops before `fromEnd` (null meaning
      * the end of the parent) toward the run starting at `toStart`, which lives in a detached fragment.
      *
      * Positional, pair by pair: this pass reuses a node where the shapes line up and replaces it where they do not.
      * Reordering a keyed list therefore costs more than it has to here, which is the two-ended keyed pass's job; what
      * matters at this level is that the nodes that did not move keep their identity.
      */
    /** Reconcile the live logical run `[fromStart, fromEnd)` of `parent` toward `[toStart, toEnd)`.
      *
      * A two-ended keyed pass runs first, then a single cursor over whatever it could not settle. The cursor alone
      * can only insert IN FRONT of itself, so a key it finds behind itself has to be dragged forward past every
      * sibling in between: swapping two rows of a thousand costs 997 moves and a full relayout. Matching both ends
      * first relocates only the children that actually changed place, two for that swap and none for a removal in
      * the middle.
      */
    private def morphNodeRun(
        parent: dom.Element,
        fromStart: dom.Node,
        fromEnd: dom.Node,
        toStart: dom.Node,
        toEnd: dom.Node
    ): Unit =
        val fromNodes = js.Array[dom.Node]()
        val fromKeys  = js.Array[String]()
        val toNodes   = js.Array[dom.Node]()
        val toKeys    = js.Array[String]()
        collectLogical(fromStart, fromEnd, fromNodes, fromKeys)
        collectLogical(toStart, toEnd, toNodes, toKeys)

        var fromKeyed: js.Dictionary[dom.Node] = null
        var toKeyed: js.Dictionary[Boolean]    = null
        var i                                  = 0
        while i < fromKeys.length do
            if fromKeys(i) != null then
                if fromKeyed == null then fromKeyed = js.Dictionary.empty[dom.Node]
                fromKeyed(fromKeys(i)) = fromNodes(i)
            i += 1
        end while
        i = 0
        while i < toKeys.length do
            if toKeys(i) != null then
                if toKeyed == null then toKeyed = js.Dictionary.empty[Boolean]
                toKeyed(toKeys(i)) = true
            i += 1
        end while

        // Invariant: the children still to place are exactly fromNodes[head..tail], a contiguous DOM run ending
        // immediately before `tailBoundary`. Only run boundaries are ever moved, and only out to a boundary, so the
        // run stays contiguous and the snapshot stays valid.
        var head         = 0
        var tail         = fromNodes.length - 1
        var toHead       = 0
        var toTail       = toNodes.length - 1
        var tailBoundary = fromEnd
        var scanning     = true
        while scanning && head <= tail && toHead <= toTail do
            // Unkeyed at either end: positional reconciliation is the cursor's job, so hand over.
            if fromKeys(head) == null || fromKeys(tail) == null || toKeys(toHead) == null || toKeys(toTail) == null
            then scanning = false
            else if fromKeys(head) == toKeys(toHead) then
                patchLogical(parent, fromNodes(head), toNodes(toHead))
                head += 1
                toHead += 1
            else if fromKeys(tail) == toKeys(toTail) then
                patchLogical(parent, fromNodes(tail), toNodes(toTail))
                tailBoundary = fromNodes(tail)
                tail -= 1
                toTail -= 1
            else if fromKeys(head) == toKeys(toTail) then
                // The run's head belongs at its tail. A single remaining child already sits there.
                if head != tail then moveLogicalBefore(parent, fromNodes(head), tailBoundary)
                patchLogical(parent, fromNodes(head), toNodes(toTail))
                tailBoundary = fromNodes(head)
                head += 1
                toTail -= 1
            else if fromKeys(tail) == toKeys(toHead) then
                if tail != head then moveLogicalBefore(parent, fromNodes(tail), fromNodes(head))
                patchLogical(parent, fromNodes(tail), toNodes(toHead))
                tail -= 1
                toHead += 1
            else scanning = false
            end if
        end while

        // Hand the unresolved middle to the cursor. The dictionaries stay whole: keys are unique among siblings, so
        // a key consumed at an end cannot be asked for again from the middle.
        val cursorFrom  = if head <= tail then fromNodes(head) else tailBoundary
        val cursorToEnd = if toTail + 1 < toNodes.length then toNodes(toTail + 1) else toEnd
        val cursorTo    = if toHead <= toTail then toNodes(toHead) else cursorToEnd
        morphRangeCursor(parent, cursorFrom, tailBoundary, cursorTo, cursorToEnd, fromKeyed, toKeyed)
    end morphNodeRun

    /** Snapshot the logical children of `[start, end)` into `nodes` and their keys into `keys`, null where unkeyed.
      * One pass: the two-ended walk reads keys four times per step, and finding a span's close marker is not free.
      */
    private def collectLogical(
        start: dom.Node,
        end: dom.Node,
        nodes: js.Array[dom.Node],
        keys: js.Array[String]
    ): Unit =
        var scan = start
        while scan != null && !(scan eq end) do
            discard(nodes.push(scan))
            discard(keys.push(logicalKey(scan).getOrElse(null)))
            scan = logicalNext(scan)
        end while
    end collectLogical

    /** Single-cursor reconciliation of whatever the two-ended pass left over: a keyed child is pulled to the cursor
      * by key, an unkeyed one morphs positionally against the first compatible live child.
      */
    private def morphRangeCursor(
        parent: dom.Element,
        fromStart: dom.Node,
        fromEnd: dom.Node,
        toStart: dom.Node,
        toEnd: dom.Node,
        fromKeyed: js.Dictionary[dom.Node],
        toKeyed: js.Dictionary[Boolean]
    ): Unit =
        var curFrom = fromStart
        var curTo   = toStart
        while curTo != null && !(curTo eq toEnd) do
            val toNext = logicalNext(curTo)
            if curFrom != null && !(curFrom eq fromEnd) && morphPortalPair(curFrom, curTo) then
                curFrom = logicalNext(curFrom)
            else
                val toKey = logicalKey(curTo).getOrElse(null)
                if toKey != null then
                    val match_ = if fromKeyed != null then fromKeyed.get(toKey).orNull else null
                    if match_ != null then
                        if match_ ne curFrom then moveLogicalBefore(parent, match_, curFrom)
                        else curFrom = logicalNext(curFrom)
                        patchLogical(parent, match_, curTo)
                    else insertLogicalClone(parent, curTo, curFrom)
                    end if
                else
                    var handled = false
                    var loop    = true
                    while loop && curFrom != null && !(curFrom eq fromEnd) do
                        val fromNext = logicalNext(curFrom)
                        val fromKey  = logicalKey(curFrom).getOrElse(null)
                        if fromKey != null then
                            // A keyed live child at an unkeyed slot: keep it if the payload reuses it elsewhere (its
                            // own slot moves it into place), else it is stale and goes.
                            if toKeyed == null || !toKeyed.contains(fromKey) then removeLogical(parent, curFrom)
                            curFrom = fromNext
                        else if morphCompatible(curFrom, curTo) then
                            morphNode(curFrom, curTo)
                            curFrom = fromNext
                            handled = true
                            loop = false
                        else
                            removeLogical(parent, curFrom)
                            curFrom = fromNext
                        end if
                    end while
                    if !handled then insertLogicalClone(parent, curTo, curFrom)
                end if
            end if
            curTo = toNext
        end while
        while curFrom != null && !(curFrom eq fromEnd) do
            val fromNext = logicalNext(curFrom)
            removeLogical(parent, curFrom)
            curFrom = fromNext
        end while
    end morphRangeCursor

    /** Reconcile a list region against a ROW ORDER rather than a rendered document.
      *
      * The payload holds only the rows that changed; every other row is named by key and stays where it is. That
      * is the whole point: a retained row costs one string comparison here, where a whole-list repaint pays a
      * render, a parse and a reconciliation for it. The walk itself is the same two-ended keyed pass
      * [[morphNodeRun]] uses, with the row order standing in for the `to` side.
      *
      * Answers `false`, having changed nothing, when the rows are not individually addressable: a row that paints
      * as several roots or as none has no single key naming its DOM, and a live range that disagrees with the
      * region about which rows are on screen cannot be matched either. The caller then repaints the whole list,
      * which needs no such structure because it diffs whole documents.
      */
    private def applyListPatch(target: DomReactiveRegions.MorphTarget, path: Seq[String], rows: Seq[ListRow]): Boolean =
        val parent   = target.parent.asInstanceOf[dom.Element]
        val pathAttr = path.mkString(".")

        val parsedKeyed      = js.Dictionary.empty[dom.Node]
        var parsedCount      = 0
        var parsed: dom.Node = target.fragment.firstChild
        while parsed != null do
            logicalKey(parsed).foreach(key => parsedKeyed(key) = parsed)
            parsedCount += 1
            parsed = logicalNext(parsed)
        end while

        val fromNodes = js.Array[dom.Node]()
        val fromKeys  = js.Array[String]()
        collectLogical(target.start.nextSibling, target.end, fromNodes, fromKeys)
        val fromKeyed = js.Dictionary.empty[dom.Node]
        var i         = 0
        while i < fromKeys.length do
            if fromKeys(i) != null then fromKeyed(fromKeys(i)) = fromNodes(i)
            i += 1
        end while

        val toKeys       = js.Array[String]()
        val toNodes      = js.Array[dom.Node]()
        val changedCount = rows.count(_.changed)
        var addressable  = parsedCount == changedCount && parsedKeyed.size == changedCount
        i = 0
        while addressable && i < rows.length do
            val row  = rows(i)
            val key  = if pathAttr.isEmpty then row.key else s"$pathAttr.${row.key}"
            val node = if row.changed then parsedKeyed.get(key).orNull else null
            addressable = if row.changed then node != null else fromKeyed.contains(key)
            discard(toKeys.push(key))
            discard(toNodes.push(node))
            i += 1
        end while
        if !addressable then false
        else
            // A retained row is bit-identical before and after, so it can contribute nothing to the enter, leave
            // or focus-auto sets: only the rows leaving and the rows being repainted are read. Scanning the rest
            // would be one whole-subtree query per row per set, for entries nothing can match.
            val targetKeys = js.Dictionary.empty[Boolean]
            i = 0
            while i < toKeys.length do
                targetKeys(toKeys(i)) = true
                i += 1
            end while
            val leaving = js.Array[dom.Node]()
            i = 0
            while i < fromKeys.length do
                if fromKeys(i) == null || !targetKeys.contains(fromKeys(i)) then discard(leaving.push(fromNodes(i)))
                i += 1
            end while
            val repainted = js.Array[dom.Node]()
            i = 0
            while i < toKeys.length do
                if toNodes(i) != null then
                    val live = fromKeyed.get(toKeys(i)).orNull
                    if live != null then discard(repainted.push(live))
                i += 1
            end while

            // Focus spans the WHOLE region, not just the disturbed rows: a retained row keeps its DOM, but MOVING
            // it is a remove and an insert, which blurs whatever it holds. A pure reorder disturbs no row at all
            // and is precisely the case that would drop the caret if this were scoped to the disturbed ones.
            val active = Maybe(document.activeElement)
                .filter(el => (el ne document.body) && containsAny(target.oldElements, el))
            val locator                        = active.flatMap(focusLocator(target.oldElements, _))
            val (selectionStart, selectionEnd) = active.map(readSelection).getOrElse((Absent, Absent))
            val disturbed                      = logicalElementsOf(leaving) ++ logicalElementsOf(repainted)
            val survivors                      = leavePaths(target.newElements)
            val oldEnter                       = enterPaths(logicalElementsOf(repainted))
            val oldFocusAuto                   = focusAutoPaths(logicalElementsOf(repainted))
            val ghosts                         = prepareLeaveGhosts(disturbed, survivors)

            val touched = scala.collection.mutable.ArrayBuffer.empty[dom.Element]
            def place(from: dom.Node, index: Int): Unit =
                val to = toNodes(index)
                if to != null then
                    patchLogical(parent, from, to)
                    fromKeyed.get(toKeys(index)).orNull match
                        case element: dom.Element => touched += element
                        case _                    => ()
                end if
            end place

            var head                   = 0
            var tail                   = fromNodes.length - 1
            var toHead                 = 0
            var toTail                 = toKeys.length - 1
            var tailBoundary: dom.Node = target.end
            var scanning               = true
            while scanning && head <= tail && toHead <= toTail do
                if fromKeys(head) == null || fromKeys(tail) == null then scanning = false
                else if fromKeys(head) == toKeys(toHead) then
                    place(fromNodes(head), toHead)
                    head += 1
                    toHead += 1
                else if fromKeys(tail) == toKeys(toTail) then
                    place(fromNodes(tail), toTail)
                    tailBoundary = fromNodes(tail)
                    tail -= 1
                    toTail -= 1
                else if fromKeys(head) == toKeys(toTail) then
                    if head != tail then moveLogicalBefore(parent, fromNodes(head), tailBoundary)
                    place(fromNodes(head), toTail)
                    tailBoundary = fromNodes(head)
                    head += 1
                    toTail -= 1
                else if fromKeys(tail) == toKeys(toHead) then
                    if tail != head then moveLogicalBefore(parent, fromNodes(tail), fromNodes(head))
                    place(fromNodes(tail), toHead)
                    tail -= 1
                    toHead += 1
                else scanning = false
                end if
            end while

            // The unresolved middle, by key: every remaining row either names a live node, which moves into place,
            // or is new, which is cloned in. Rows the order no longer names are removed at the end.
            var cursor: dom.Node = if head <= tail then fromNodes(head) else tailBoundary
            var index            = toHead
            while index <= toTail do
                val key  = toKeys(index)
                val live = fromKeyed.get(key).orNull
                if live != null then
                    if live ne cursor then moveLogicalBefore(parent, live, cursor)
                    else cursor = logicalNext(cursor)
                    place(live, index)
                else
                    toNodes(index) match
                        case null => ()
                        case node =>
                            insertLogicalClone(parent, node, cursor)
                            logicalElementsOf(js.Array[dom.Node](node)).foreach(touched += _)
                end if
                index += 1
            end while
            i = 0
            while i < fromKeys.length do
                val key = fromKeys(i)
                if key == null || !targetKeys.contains(key) then
                    if document.contains(fromNodes(i)) then removeLogical(parent, fromNodes(i))
                i += 1
            end while

            val finalRoots = target.oldElements
            val live       = elementsBetweenLive(target)
            live.foreach(applyJsPropsSync)
            locator.flatMap(resolveFocus(live, _)).foreach { element =>
                focusNoScroll(element)
                (selectionStart, selectionEnd) match
                    case (Present(start), Present(end)) => setSelection(element, start, end)
                    case _                              => ()
            }
            seedEnter(touched.toSeq, oldEnter)
            seedFocusAuto(touched.toSeq, oldFocusAuto)
            touched.foreach(portalSweep)
            spawnGhosts(ghosts)
            sweepFocusAuto()
            sweepScrollAuto()
            discard(finalRoots)
            true
        end if
    end applyListPatch

    /** The elements of a set of logical children, each span contributing the elements it brackets. */
    private def logicalElementsOf(nodes: js.Array[dom.Node]): Seq[dom.Element] =
        val out = scala.collection.mutable.ArrayBuffer.empty[dom.Element]
        var i   = 0
        while i < nodes.length do
            eachSpanNode(nodes(i)) {
                case element: dom.Element => out += element
                case _                    => ()
            }
            i += 1
        end while
        out.toSeq
    end logicalElementsOf

    private def elementsBetweenLive(target: DomReactiveRegions.MorphTarget): Seq[dom.Element] =
        val out            = scala.collection.mutable.ArrayBuffer.empty[dom.Element]
        var node: dom.Node = target.start.nextSibling
        while node != null && !(node eq target.end) do
            node match
                case element: dom.Element => out += element
                case _                    => ()
            node = node.nextSibling
        end while
        out.toSeq
    end elementsBetweenLive

    private def moveLogicalBefore(parent: dom.Element, node: dom.Node, ref: dom.Node): Unit =
        eachSpanNode(node)(current => discard(parent.insertBefore(current, ref)))

    private def hasFlag(flags: String, flag: String): Boolean =
        flags.nonEmpty && flags.split(' ').exists(_ == flag)

    private def flagKey(flags: String): Maybe[String] =
        if flags.isEmpty then Absent
        else Maybe.fromOption(flags.split(' ').find(_.startsWith("k=")).map(_.substring(2)))

    /** The flag section a live marker carries once it has adopted the slot: `m`, and the key it adopted. */
    private def adoptedFlags(key: Maybe[String]): String =
        key match
            case Present(value) => s" m k=$value"
            case Absent         => " m"

    /** Reconcile one matched pair of logical children.
      *
      * Two spans of the same id are the same region still sitting here, so the pass recurses into their contents and
      * never touches the live markers: the registry stays keyed by the nodes it already holds. Any other mismatch,
      * including a span against a plain node, is replaced whole, markers and all.
      */
    private def patchLogical(parent: dom.Element, fromNode: dom.Node, toNode: dom.Node): Unit =
        val fromSpan = DomReactiveRegions.startMarkerId(fromNode).flatMap(id => spanClose(fromNode, id).map((id, _)))
        val toSpan   = DomReactiveRegions.startMarkerId(toNode).flatMap(id => spanClose(toNode, id).map((id, _)))
        (fromSpan, toSpan) match
            case (Present((fromId, fromClose)), Present((toId, toClose))) if fromId == toId =>
                val liveFlags     = DomReactiveRegions.markerFlags(fromNode)
                val incomingFlags = DomReactiveRegions.markerFlags(toNode)
                val incomingSlot  = hasFlag(incomingFlags, "s")
                // A mount that already owns this slot repaints its own content, so the span is opaque and its live
                // marker is left alone: reconciling it against the placeholder the parent rendered would morph the
                // instance's subtree away, and with it focus, caret and every DOM-local thing hanging off it. The key
                // is what makes that safe. A differing key means a different instance, which falls through to the
                // morph and resets the slot exactly as it did before flags existed.
                //
                // A NAMED slot is opaque from the first pass, not from the second. `m` says the client adopted the
                // span; `s` with the same key says the same thing one beat earlier, because the render that emitted
                // the slot named the instance that owns it. Waiting for `m` bought exactly one destructive morph per
                // slot, and the damage was not confined to the DOM: the regions inside the discarded subtree leave
                // the registry with it, the instance republishes asynchronously, and a subscription that emits in
                // between dies on an unknown range — permanently, since a dead subscription never paints again. An
                // UNNAMED slot keeps the old rule: without a key there is nothing to tell one instance from the next,
                // and a keyless mount is rebuilt by every enclosing emission by design.
                val namedSlot = flagKey(incomingFlags).isDefined && flagKey(liveFlags) == flagKey(incomingFlags)
                val ownsSlot  = hasFlag(liveFlags, "m") || (namedSlot && hasFlag(liveFlags, "s"))
                if ownsSlot && incomingSlot && flagKey(liveFlags) == flagKey(incomingFlags) then ()
                else
                    morphNodeRun(parent, fromNode.nextSibling, fromClose, toNode.nextSibling, toClose)
                    if incomingSlot then
                        fromNode.asInstanceOf[dom.Comment].data =
                            DomReactiveRegions.openMarkerData(fromId, adoptedFlags(flagKey(incomingFlags)))
                end if
            case (Absent, Absent) if morphCompatible(fromNode, toNode) =>
                morphNode(fromNode, toNode)
            case _ =>
                insertLogicalClone(parent, toNode, fromNode)
                removeLogical(parent, fromNode)
        end match
    end patchLogical

    /** Mount a UI into the page body. */
    def mount(ui: UI)(using Frame): Unit < (Async & Scope) =
        mountInto(ui, document.body, NoMountDiagnostics)

    private[kyo] def mount(ui: UI, diagnostics: MountDiagnostics)(using Frame): Unit < (Async & Scope) =
        mountInto(ui, document.body, diagnostics)

    /** Mount a UI into a specific DOM element selected by CSS selector. */
    def mount(ui: UI, selector: String)(using Frame): Unit < (Async & Scope) =
        mount(ui, selector, NoMountDiagnostics)

    private[kyo] def mount(ui: UI, selector: String, diagnostics: MountDiagnostics)(using Frame): Unit < (Async & Scope) =
        Sync.defer {
            val target = document.querySelector(selector)
            if target == null then Abort.panic(UIException(s"Element not found: $selector"))
            else mountInto(ui, target.asInstanceOf[dom.Element], diagnostics)
        }
    end mount

    /** Injects a rendered stylesheet CSS string into the live document.
      *
      * The base reset is injected first (idempotently) so it precedes the authored CSS in document
      * order, matching the SSG page head where `baseCss` is emitted before `head.css`. The reset is a
      * foundational layer authored stylesheets are meant to override (e.g. `body { font-family }`); if
      * it were appended AFTER the sheet (as happens when an app calls `runStylesheet` before `runMount`,
      * which injects the reset), its equal-specificity `body` rule would win on document order and clobber
      * the app's own `body` font, producing a fallback-font flash. Injecting the reset first here makes the
      * cascade order independent of which entry point runs first.
      */
    private[kyo] def injectStylesheet(sheet: Stylesheet)(using Frame): Unit < Sync =
        DomStyleSheet.injectBase().andThen(Sync.defer(DomStyleSheet.injectStylesheet(sheet.render)))

    private def mountInto(ui: UI, container: dom.Element, diagnostics: MountDiagnostics)(using Frame): Unit < (Async & Scope) =
        // Late-bound to break the emit<->Commands construction cycle (emit resolves measure callbacks via
        // Commands, Commands needs emit). Set before any op is emitted.
        var sessionCommands: UI.Commands = null
        for
            _       <- DomStyleSheet.injectBase()
            root    <- ReactiveUI.normalize(ui, Seq.empty)
            html    <- HtmlRenderer.render(ui, Seq.empty)
            _       <- Sync.defer { noteMarkers(html); container.innerHTML = html }
            regions <- DomReactiveRegions.init(container)
            _       <- applyJsProps(container)
            _       <- Sync.defer(seedEnter(container, Set.empty))
            _       <- Sync.defer(seedFocusAuto(container, Set.empty))
            // Record the initial carriers without scrolling: the first paint is where the reader starts, not a
            // movement to follow.
            _ <- Sync.defer(sweepScrollAuto(scroll = false))
            // Portal adopt for the initial paint: a portal element present at load re-homes immediately.
            _        <- Sync.defer(portalSweep(container))
            _        <- Sync.defer(beginAnimationsSync(container))
            commands <- UI.Commands.init(op => applyOpLocal(op, regions, () => sessionCommands))
            _ = sessionCommands = commands
            // Env.run so component handlers and mounted effects resolve `UI.commands` at run time: the region
            // fibers subscribe forks and the event drain all start inside this scope.
            _ <- Env.run(commands) {
                for
                    exchange <- Kyo.lift(LocalExchange(regions))
                    dispatch <- ReactiveUI.subscribe(root, exchange)
                    // A subscription interrupt is asynchronous. Stop accepting publications before the Scope begins
                    // interrupting region fibers, so an already-woken observer cannot publish into the registry after its
                    // close finalizer has run.
                    _ <- Scope.ensure(exchange.close)
                    // Single-consumer drain owned by the ambient page Scope. The single consumer preserves event ordering.
                    events <- Channel.init[Unit < Async](256)
                    _ = sessionEvents = Present(events)
                    // runPartial captures only the Closed failure (the channel closed on page teardown -> stop draining); a
                    // Panic propagates rather than being silently swallowed as a clean drain end.
                    // The drain carries the session's scroll sink: a handler calling UI.scrollIntoView scrolls the
                    // local document, the browser-mount counterpart of the server session's WebSocket op.
                    drain <- Scope.acquireRelease(
                        Fiber.initUnscoped(UICommands.scrollSink.let(Present(scrollLocal)) {
                            DragCommands.resolveSink.let(Present(resolveLocal)) {
                                Sync.defer(diagnostics.drainStarted()).andThen(
                                    Loop.foreach(Abort.runPartial[Closed](events.take).map {
                                        case Result.Success(eff) => eff.andThen(Loop.continue)
                                        case Result.Failure(_)   => Loop.done
                                    })
                                )
                            }
                        })
                    )(drain =>
                        Sync.defer(diagnostics.drainInterrupting()).andThen(drain.interrupt).andThen(drain.getResult).andThen {
                            Sync.defer(diagnostics.drainJoined())
                        }
                    )
                    _ <- Sync.defer(diagnostics.drainInstalled(drain))
                    // Finalizers are LIFO. Register the drain first, then the channel, then listeners, so teardown
                    // removes callbacks before closing their queue and finally interrupts and joins the drain.
                    _ <- Scope.ensure(events.close.andThen(Sync.defer(diagnostics.channelClosed())).unit)
                    _ <- setupEventDelegation(dispatch.handle, events)
                    _ <- setupPointerDelegation(dispatch.handle, events)
                    _ <- setupInputMasking()
                    dragRuntime <- DomDragRuntime.install(
                        container,
                        event =>
                            diagnostics.dragEventQueued(event)
                            fireFromJs(
                                events,
                                dispatch.handle(event.path, event).map { result =>
                                    diagnostics.dragEventHandled(event)
                                    result
                                }.unit
                            )
                    )
                    _ <- Sync.defer(diagnostics.dragRuntimeInstalled(dragRuntime))
                    _ <- Async.never
                yield ()
            }
        yield ()
        end for
    end mountInto

    // The local-document scroll sink installed on the mount's event-drain fiber; mirrors the embedded
    // client's ScrollIntoView handling exactly (missing id = no-op, smooth scroll to the block start),
    // so the same command behaves the same under either runner.
    private def scrollLocal(id: String)(using Frame): Unit < Async =
        Sync.defer {
            val el = document.getElementById(id)
            if el != null then
                discard(el.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal(behavior = "smooth", block = "start")))
        }

    /** Publishes a typed drag resolution for the local drag runtime without retaining listeners or
      * session state. DomDragRuntime consumes this reserved document event.
      */
    private def resolveLocal(sessionId: String, decision: Drag.Decision)(using Frame): Unit < Async =
        Sync.defer {
            val detail = Json.encode[HtmlOp](HtmlOp.ResolveDrag(sessionId, decision))
            val publish = () =>
                val event = js.Dynamic.newInstance(dom.window.asInstanceOf[js.Dynamic].CustomEvent)(
                    "kyo:resolve-drag",
                    js.Dynamic.literal(detail = detail)
                )
                discard(document.asInstanceOf[js.Dynamic].dispatchEvent(event))
            discard(dom.window.setTimeout(publish, 0))
        }

    final private case class RangePatchState(
        active: Maybe[RangeFocus],
        selectionStart: Maybe[Int],
        selectionEnd: Maybe[Int],
        oldEnter: Set[String],
        ghosts: Seq[(dom.Element, dom.Element, String)],
        oldFocusAuto: Set[String]
    )

    final private case class RangeFocus(path: Maybe[String], rootIndex: Int, childIndexes: Seq[Int])

    // ---- local op application for the SPA transport (Command / RequestMeasure) ----

    /** `[data-kyo-path="..."]` for a dot-joined render path, with the two characters a quoted CSS attribute
      * selector cannot carry raw escaped. Twin of `__kyoPathSel` in `HtmlRenderer.clientJs`.
      */
    private def pathSelector(joinedPath: String): String =
        val escaped = joinedPath.replace("\\", "\\\\").replace("\"", "\\\"")
        s"[data-kyo-path=\"$escaped\"]"

    private def queryByPath(path: Seq[String]): dom.Element =
        document.querySelector(pathSelector(path.mkString(".")))

    /** Resolve a path-addressed command/measure target: the element carrying the path, else (a region
      * path: a comment-delimited range has no element of its own) the range's first element, else null.
      */
    private def resolveElementByPath(regions: DomReactiveRegions, path: Seq[String]): dom.Element =
        val el = queryByPath(path)
        if el != null then el
        else regions.firstElementAt(path).getOrElse(null)
    end resolveElementByPath

    /** A conservative "focusable" CSS selector: what a focus command may land on. Mirrors
      * the reactive-focus-restore query used elsewhere in this backend / HtmlRenderer.
      */
    private val FocusableSelector = "input,textarea,select,button,a[href],[tabindex],[contenteditable]"

    /** Focus `el` if it is itself focusable, else its FIRST focusable descendant. Lets a
      * focus command target a non-focusable WRAPPER (e.g. an InputGroup around several
      * fields) and land on the first field inside it. A focusable element (an `<input>`,
      * …) matches the selector and focuses itself, so existing focus targets are unchanged.
      */
    private def focusInto(el: dom.Element): Unit =
        if el != null then
            val dyn         = el.asInstanceOf[scalajs.js.Dynamic]
            val selfMatches = scalajs.js.typeOf(dyn.matches) == "function" && dyn.matches(FocusableSelector).asInstanceOf[Boolean]
            val target      = if selfMatches then el else el.querySelector(FocusableSelector)
            if target != null then
                val tdyn = target.asInstanceOf[scalajs.js.Dynamic]
                if scalajs.js.typeOf(tdyn.focus) == "function" then discard(tdyn.focus())

    /** Apply a whitelisted `verb` to `el` (shared by path- and id-addressed commands). Unknown verbs are ignored. */
    private def applyVerbDom(el: dom.Element, verb: String): Unit =
        if el != null then
            val dyn = el.asInstanceOf[scalajs.js.Dynamic]
            verb match
                case "focus" => focusInto(el)
                case "scrollIntoView" =>
                    if scalajs.js.typeOf(dyn.scrollIntoView) == "function" then
                        discard(dyn.scrollIntoView(scalajs.js.Dynamic.literal(block = "nearest")))
                case _ => ()
            end match
        end if
    end applyVerbDom

    private def applyCommandDom(regions: DomReactiveRegions, path: Seq[String], verb: String): Unit =
        applyVerbDom(resolveElementByPath(regions, path), verb)

    /** Self-addressing: resolve the command target by DOM id (getElementById) instead of the render path. */
    private def applyCommandDomById(id: String, verb: String): Unit =
        applyVerbDom(document.getElementById(id), verb)

    private def measureRect(el: dom.Element): Maybe[UI.Rect] =
        if el == null then Absent
        else
            val r = el.getBoundingClientRect()
            Present(UI.Rect(r.left, r.top, r.width, r.height, dom.window.innerWidth, dom.window.innerHeight))

    private def measureDom(regions: DomReactiveRegions, path: Seq[String]): Maybe[UI.Rect] =
        measureRect(resolveElementByPath(regions, path))

    /** Self-addressing: measure the element with DOM id `id` (getElementById). */
    private def measureDomById(id: String): Maybe[UI.Rect] =
        measureRect(document.getElementById(id))

    private def applyOpLocal(op: HtmlOp, regions: DomReactiveRegions, commands: () => UI.Commands)(using Frame): Unit < Async =
        op match
            case HtmlOp.Command(path, verb) => Sync.defer(applyCommandDom(regions, path, verb))
            case HtmlOp.RequestMeasure(path) =>
                Sync.defer(measureDom(regions, path)).map {
                    case Present(rect) => commands().deliverMeasure(path, rect)
                    case Absent        => Kyo.unit
                }
            case HtmlOp.CommandById(id, verb) => Sync.defer(applyCommandDomById(id, verb))
            case HtmlOp.RequestMeasureById(id) =>
                Sync.defer(measureDomById(id)).map {
                    case Present(rect) => commands().deliverMeasureById(id, rect)
                    case Absent        => Kyo.unit
                }
            case HtmlOp.SetClassById(id, className, on) =>
                Sync.defer {
                    val el = document.getElementById(id)
                    if el != null then
                        markOwned(el, "class")
                        discard(el.classList.toggle(className, on))
                }
            case HtmlOp.SetStyleById(id, css) =>
                Sync.defer {
                    val el = document.getElementById(id)
                    if el != null then
                        markOwned(el, "style")
                        mergeStyleDomById(id, css)
                }
            // set an attribute in place (element stays in the DOM, so a CSS `>` anchored on it keeps matching).
            case HtmlOp.SetAttrById(id, name, value) =>
                Sync.defer {
                    val el = document.getElementById(id)
                    if el != null then
                        markOwned(el, name)
                        el.setAttribute(name, value)
                        syncFieldProperty(el, name, value)
                    end if
                }
            // measure now + deliver, then attach the continuous scroll/resize observer for `id`.
            case HtmlOp.ObserveViewportById(id) =>
                Sync.defer(registerViewportObserver(id, commands)).andThen(
                    Sync.defer(measureDomById(id)).map {
                        case Present(rect) => commands().deliverMeasureById(id, rect)
                        case Absent        => Kyo.unit
                    }
                )
            case HtmlOp.UnobserveViewportById(id) =>
                Sync.defer(unregisterViewportObserver(id))
            // Replace/Remove/InjectCss reach the DOM through LocalExchange, never this imperative channel.
            case _ => Kyo.unit
    end applyOpLocal

    /** Merges a serialized `Style` declaration string ("prop:val;prop:val") onto getElementById(id) with setProperty
      * per declaration, so it merges over other inline props rather than clobbering them (unlike a full `style=""`
      * replace). Blank declarations and those without a `:` are skipped.
      */
    private def mergeStyleDomById(id: String, css: String): Unit =
        val el = document.getElementById(id)
        if el != null then
            val style = el.asInstanceOf[dom.HTMLElement].style
            css.split(';').foreach { decl =>
                val trimmed = decl.trim
                if trimmed.nonEmpty then
                    val colon = trimmed.indexOf(':')
                    if colon > 0 then
                        style.setProperty(trimmed.substring(0, colon).trim, trimmed.substring(colon + 1).trim)
                end if
            }
        end if
    end mergeStyleDomById

    /** Attaches a single handler to `window` scroll (capture) + resize that re-measures getElementById(id) and
      * bridges the deliver back into the drain via [[fireFromJs]]. Guards against double-registration for the same id.
      */
    private def registerViewportObserver(id: String, commands: () => UI.Commands)(using Frame): Unit =
        if !viewportObservers.contains(id) then
            val handler: js.Function1[dom.Event, Unit] = (_: dom.Event) =>
                measureDomById(id) match
                    case Present(rect) => sessionEvents.foreach(ev => fireFromJs(ev, commands().deliverMeasureById(id, rect)))
                    case Absent        => ()
            viewportObservers(id) = handler
            dom.window.addEventListener("scroll", handler, true)
            dom.window.addEventListener("resize", handler)
        end if
    end registerViewportObserver

    /** Removes the scroll/resize handler registered for `id` (from both listeners) and drops the map entry. */
    private def unregisterViewportObserver(id: String): Unit =
        if viewportObservers.contains(id) then
            val handler = viewportObservers(id)
            dom.window.removeEventListener("scroll", handler, true)
            dom.window.removeEventListener("resize", handler)
            discard(viewportObservers.remove(id))
        end if
    end unregisterViewportObserver

    /** Exchange that renders UI to HTML and applies directly to the DOM. */
    private class LocalExchange(regions: DomReactiveRegions) extends UIExchange:

        private var open = true

        private[kyo] def close(using Frame): Unit < Sync = Sync.defer { open = false }

        /** Fingerprint of the HTML each region painted last, kept ONLY while a devtools sink is observing.
          *
          * The server transport learns "this render changed nothing" for free, because it compares rendered
          * bytes against what it last sent in order to decide what to put on the socket. This one has no such
          * comparison to borrow: it renders the region and morphs, and a morph that finds everything equal is
          * indistinguishable from the outside. So it is asked here, on the string the paint already produced.
          *
          * A fingerprint, not the HTML: keeping the painted bytes of the whole tree alive would double the
          * page's memory to answer one boolean. Length in the high word, hash in the low, which makes a
          * collision require both to agree — and the cost of one is a render reported as wasted that was not.
          */
        private val paintedFingerprints = scala.collection.mutable.HashMap.empty[String, Long]

        private def noteWasted(path: Seq[String], html: String)(using Frame): Unit < Sync =
            Devtools.observing.map { observing =>
                if !observing then Kyo.unit
                else
                    Sync.defer {
                        val key         = path.mkString(".")
                        val fingerprint = (html.length.toLong << 32) | (html.hashCode() & 0xffffffffL)
                        val wasted      = paintedFingerprints.get(key).contains(fingerprint)
                        discard(paintedFingerprints.put(key, fingerprint))
                        wasted
                    }.map(Devtools.notePaint(html.length, _))
            }

        // In-process: the update is a DOM write, not bytes on a wire, so this replaces the region whole
        // (morphing where it can) rather than diffing it. `previous` is what the server-side exchange
        // uses to send only what moved.
        def onChange(
            region: ReactiveRegion,
            path: Seq[String],
            contentContext: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            ui: UI
        )(using Frame): Unit < Async =
            Sync.defer(open).flatMap { isOpen =>
                if !isOpen then Kyo.unit
                else
                    val boundaryMode =
                        if ReactiveRegion.owns(region, contentContext) then ReactiveRegion.BoundaryMode.Suppress
                        else ReactiveRegion.BoundaryMode.Emit
                    HtmlRenderer.renderRegion(ui, path, contentContext, region, parentContext, boundaryMode).flatMap { html =>
                        // Every painted byte passes here, so this is where the optional-feature flags learn about
                        // an attribute that only a later re-render brings in (see noteMarkers), and where a
                        // devtools sink learns whether this render produced anything (see noteWasted).
                        noteMarkers(html)
                        noteWasted(path, html).andThen(Sync.defer(open)).flatMap { stillOpen =>
                            if !stillOpen then Kyo.unit
                            else
                                region match
                                    case ReactiveRegion.HtmlRange(regionId) =>
                                        // A region that is not painted right now has nothing to patch. It is the
                                        // sibling of the `open` guard above, and it is reachable for the same kind
                                        // of reason: a patch somewhere above discarded this subtree, and whatever
                                        // owns it has not repainted yet. The emission that lands in that window is
                                        // either obsolete (the next paint renders the signal's CURRENT value, so
                                        // nothing is lost) or early (same). Neither is worth the registry's
                                        // strict diagnostic, which panics — and a panic here does not just skip a
                                        // frame, it ends the subscription fiber, so the region never paints again.
                                        // The registry keeps that diagnostic for callers who really are addressing
                                        // a range that ought to exist; the engine simply does not ask when it knows
                                        // there is none. Same reading `withRegionFragment` already applies to a
                                        // range that was never painted.
                                        regions.contains(regionId).flatMap { painted =>
                                            if !painted then Kyo.unit
                                            else
                                                regions.replaceWith(regionId, html)(tryMorphRange)(
                                                    prepareRangePatch
                                                )(finishRangePatch)
                                        }
                                    case svgRegion: ReactiveRegion.SvgElement =>
                                        replaceSvg(svgRegion, html)
                                end match
                        }
                    }
            }
        end onChange

        /** Render ONLY the rows that changed, and as ONE payload rather than one parse per row: a full replacement
          * must not lose the single bulk parse it has today just because a one-row removal wants to skip it.
          *
          * Rows that turn out not to be addressable one-to-one leave the DOM untouched and fall through to the
          * inherited whole-list repaint, which needs no per-row structure at all.
          */
        override def onListPatch(
            region: ReactiveRegion,
            path: Seq[String],
            contentContext: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            rows: Seq[ListRow]
        )(using Frame): Unit < Async =
            def repaint: Unit < Async = super.onListPatch(region, path, contentContext, parentContext, previous, rows)
            region match
                case ReactiveRegion.HtmlRange(regionId) =>
                    val host = ReactiveRegion.renderHost(
                        region,
                        parentContext,
                        ReactiveRegion.tableContent(rows.iterator.map(_.ui))
                    )
                    Kyo.foreach(Chunk.from(rows.filter(_.changed))) { row =>
                        HtmlRenderer.renderRow(row.ui, path :+ row.key, contentContext.child(row.key), host)
                    }.map(_.mkString).flatMap { changed =>
                        Sync.defer(open).flatMap { isOpen =>
                            if !isOpen then Kyo.unit
                            else
                                regions.withRegionFragment(regionId, changed)(applyListPatch(_, path, rows)).flatMap {
                                    applied =>
                                        if applied then Devtools.notePaint(changed.length, wasted = false)
                                        else repaint
                                }
                        }
                    }
                // An SVG region replaces its own group element; there is no row range to address.
                case _: ReactiveRegion.SvgElement => repaint
            end match
        end onListPatch

        // Each channel patches in place and marks the attribute owned (__kyoOwn), so a parent region's morph
        // will not reconcile the live value back. The `*Now` forms are the patches themselves, written without
        // the Sync wrapper for a caller that already holds the thread (see UIExchange's synchronous sinks); the
        // effectful twins do nothing but defer to them, so the two can never drift apart.
        private def attrPatchNow(path: Seq[String], name: String, value: String): Unit =
            val el = queryByPath(path)
            if el != null then
                markOwned(el, name)
                el.setAttribute(name, value)
                syncFieldProperty(el, name, value)
            end if
        end attrPatchNow

        private def boolAttrPatchNow(path: Seq[String], name: String, value: Boolean): Unit =
            val el = queryByPath(path)
            if el != null then
                markOwned(el, name)
                if value then el.setAttribute(name, "") else el.removeAttribute(name)
        end boolAttrPatchNow

        // Toggle in place (so CSS transitions fire) rather than re-render; owns "class" against the morph.
        private def classPatchNow(path: Seq[String], name: String, on: Boolean): Unit =
            val el = queryByPath(path)
            if el != null then
                markOwned(el, "class")
                discard(el.classList.toggle(name, on))
        end classPatchNow

        override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
            Sync.defer(attrPatchNow(path, name, value))

        override def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async =
            Sync.defer(boolAttrPatchNow(path, name, value))

        override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
            Sync.defer(classPatchNow(path, name, on))

        // A bound text region: the value goes into the text node as-is. Unlike patchLoneText, which receives
        // RENDERED html and must therefore refuse payloads carrying `<` or `&`, this receives the string itself
        // — and a text node holds literal characters, so every string is writable here with no escaping and no
        // parser. An unpainted path is a silent no-op, exactly as for the attribute patches above.
        private def textPatchNow(path: Seq[String], value: String): Unit =
            discard(regions.setTextAt(path, value))

        override val attrPatcherNow: Maybe[(Seq[String], String, String) => Unit]      = Present(attrPatchNow)
        override val boolAttrPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit] = Present(boolAttrPatchNow)
        override val classPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit]    = Present(classPatchNow)
        override val textPatcherNow: Maybe[(Seq[String], String) => Unit]              = Present(textPatchNow)

        /** Reconcile the region's live nodes toward the incoming payload instead of replacing them.
          *
          * Nested ranges are reconciled as logical children, so a region containing another region morphs like any
          * other; `replaceWith` re-reads the live markers afterwards and hands the registry what is actually there.
          */
        private def tryMorphRange(target: DomReactiveRegions.MorphTarget): Boolean =
            morphNodeRun(
                target.parent.asInstanceOf[dom.Element],
                target.start.nextSibling,
                target.end,
                target.fragment.firstChild,
                null
            )
            true
        end tryMorphRange

        private def prepareRangePatch(oldElements: Seq[dom.Element], newElements: Seq[dom.Element]): RangePatchState =
            val active = Maybe(document.activeElement).filter(el => (el ne document.body) && containsAny(oldElements, el))
            val (selectionStart, selectionEnd) = active.map(readSelection).getOrElse((Absent, Absent))
            RangePatchState(
                active.flatMap(focusLocator(oldElements, _)),
                selectionStart,
                selectionEnd,
                enterPaths(oldElements),
                prepareLeaveGhosts(oldElements, leavePaths(newElements)),
                focusAutoPaths(oldElements)
            )
        end prepareRangePatch

        private def finishRangePatch(state: RangePatchState, newElements: Seq[dom.Element], morphed: Boolean): Unit =
            newElements.foreach { element =>
                applyJsPropsSync(element)
                // A replacement inserted every one of these roots, so every animation under them is new. A morph
                // reused what it could and started the animations of the nodes it did insert as it inserted them.
                if !morphed then beginAnimationsSync(element)
            }
            state.active.flatMap(resolveFocus(newElements, _)).foreach { target =>
                focusNoScroll(target)
                (state.selectionStart, state.selectionEnd) match
                    case (Present(start), Present(end)) => setSelection(target, start, end)
                    case _                              => ()
            }
            seedEnter(newElements, state.oldEnter)
            // Seed after focus restoration so a newly appeared focus-auto element wins over the trigger.
            seedFocusAuto(newElements, state.oldFocusAuto)
            // Portal upkeep: re-home portal elements this patch inserted, and retire the placeholder of one it
            // removed. Runs over the patched roots, so an untouched part of the page is not swept.
            newElements.foreach(portalSweep)
            spawnGhosts(state.ghosts)
            sweepFocusAuto()
            // A patch that left the flag on a different element scrolls that element into view; one that left it
            // where it was scrolls nothing.
            sweepScrollAuto()
        end finishRangePatch

        private def replaceSvg(region: ReactiveRegion.SvgElement, html: String)(using Frame): Unit < Sync =
            Sync.defer {
                val pathAttr  = region.path.mkString(".")
                val finalHtml = HtmlRenderer.wrapReactiveRegion(region, html)
                val element   = document.querySelector(s"""[data-kyo-path="$pathAttr"]""")
                if element != null && element.outerHTML != finalHtml then
                    val active = Maybe(document.activeElement).filter(el => (el ne document.body) && element.contains(el))
                    val (selectionStart, selectionEnd) = active.map(readSelection).getOrElse((Absent, Absent))
                    val activePath                     = active.flatMap(el => Maybe(el.getAttribute("data-kyo-path")))
                    val oldEnter                       = enterPaths(Seq(element))
                    val ghosts                         = prepareLeaveGhosts(Seq(element), leaveSurvSet(finalHtml))
                    val oldFocus                       = focusAutoPaths(Seq(element))
                    element.outerHTML = finalHtml
                    val updated = Maybe(document.querySelector(s"""[data-kyo-path="$pathAttr"]""")).toList
                    updated.foreach { newElement =>
                        applyJsPropsSync(newElement)
                        beginAnimationsSync(newElement)
                    }
                    activePath.foreach(path => restoreSvgFocus(path, selectionStart, selectionEnd))
                    seedEnter(updated, oldEnter)
                    seedFocusAuto(updated, oldFocus)
                    spawnGhosts(ghosts)
                    sweepFocusAuto()
                end if
            }
        end replaceSvg
    end LocalExchange

    private def readSelection(el: dom.Element): (Maybe[Int], Maybe[Int]) =
        val dyn = el.asInstanceOf[scalajs.js.Dynamic]
        def asInt(v: scalajs.js.Dynamic): Maybe[Int] =
            if scalajs.js.typeOf(v) == "number" then Present(v.asInstanceOf[Int]) else Absent
        (asInt(dyn.selectionStart), asInt(dyn.selectionEnd))
    end readSelection

    /** Engine-driven focus (restore after a patch, focus-auto seeding, focus-return pop) must never scroll. The
      * browser's default `focus()` scrolls every scrollable ancestor to reveal the target, so a bookkeeping focus after
      * a patch would yank container scroll positions the user never touched, visible as a scrollbar flash or a content
      * jump. Explicit app focus commands ([[applyVerbDom]]) and user-driven keyboard navigation keep the native
      * scrolling semantics: there the scroll is the point.
      */
    private def focusNoScroll(el: dom.Element): Unit =
        discard(el.asInstanceOf[scalajs.js.Dynamic].focus(scalajs.js.Dynamic.literal(preventScroll = true)))

    private def restoreSvgFocus(capturedPath: String, selStart: Maybe[Int], selEnd: Maybe[Int]): Unit =
        val located = document.querySelector(pathSelector(capturedPath))
        if located != null then
            focusNoScroll(located)
            (selStart, selEnd) match
                case (Present(s), Present(e)) => setSelection(located, s, e)
                case _                        => ()
        end if
    end restoreSvgFocus

    /** The set of `data-kyo-path` values of every `data-kyo-focus-auto` element inside `root`, `root` itself included.
      *
      * Callers capture this BEFORE replacing a region so that [[seedFocusAuto]] can tell a newly appeared element from
      * one that was already on screen: an echo re-render of an open overlay must not steal focus back from the user.
      */
    private def focusAutoPaths(root: dom.Element): Set[String] =
        if !sawFocusAuto then return Set.empty
        val els = root.querySelectorAll("[data-kyo-focus-auto]")
        val descendants = (0 until els.length).flatMap { i =>
            Maybe(els(i).asInstanceOf[dom.Element].getAttribute("data-kyo-path")).toList
        }.toSet
        if root.hasAttribute("data-kyo-focus-auto") && root.hasAttribute("data-kyo-path") then
            descendants + root.getAttribute("data-kyo-path")
        else descendants
    end focusAutoPaths

    private def focusAutoPaths(roots: Seq[dom.Element]): Set[String] =
        roots.iterator.flatMap(focusAutoPaths).toSet

    /** Seed the FIRST `data-kyo-focus-auto` element under `newRoot` whose path is not in `oldSet` (i.e. it newly
      * appeared): record the previously focused element's path plus the focus-restore flag on the stack, then call
      * `.focus()` on it. On the initial mount `oldSet` is empty, so any focus-auto element is seeded, like native
      * `autofocus`. Mirrors `seedFocusAuto` in HtmlRenderer.clientJs.
      */
    private def seedFocusAuto(newRoot: dom.Element, oldSet: Set[String]): Unit =
        seedFocusAuto(Seq(newRoot), oldSet)

    private def seedFocusAuto(newRoots: Seq[dom.Element], oldSet: Set[String]): Unit =
        // An app that never renders the attribute pays no selector sweep for it (see noteMarkers).
        if !sawFocusAuto then return
        val candidates = newRoots.flatMap { newRoot =>
            val els = newRoot.querySelectorAll("[data-kyo-focus-auto]")
            (if newRoot.hasAttribute("data-kyo-focus-auto") then Seq(newRoot) else Seq.empty) ++
                (0 until els.length).map(els(_).asInstanceOf[dom.Element])
        }
        candidates.find { el =>
            val p = el.getAttribute("data-kyo-path")
            p != null && !oldSet.contains(p)
        }.foreach { el =>
            val ae = document.activeElement
            val ret =
                if ae != null && (ae ne document.body) then Maybe(ae.getAttribute("data-kyo-path"))
                else Absent
            focusReturnStack =
                focusReturnStack.append(
                    FocusSeed(el.getAttribute("data-kyo-path"), ret, el.hasAttribute("data-kyo-focus-restore"))
                )
            focusNoScroll(el)
        }
    end seedFocusAuto

    private def containsAny(roots: Seq[dom.Element], element: dom.Element): Boolean =
        roots.exists(root => (root eq element) || root.contains(element))

    private def focusLocator(roots: Seq[dom.Element], active: dom.Element): Maybe[RangeFocus] =
        Maybe.fromOption(roots.indexWhere(root => (root eq active) || root.contains(active)) match
            case -1    => None
            case index => Some(index)).map { rootIndex =>
            val indexes = scala.collection.mutable.ArrayBuffer.empty[Int]
            var current = active
            val root    = roots(rootIndex)
            while current ne root do
                val parent = current.parentNode.asInstanceOf[dom.Element]
                var index  = 0
                while index < parent.children.length && (parent.children(index) ne current) do index += 1
                indexes.prepend(index)
                current = parent
            end while
            RangeFocus(Maybe(active.getAttribute("data-kyo-path")), rootIndex, indexes.toSeq)
        }
    end focusLocator

    private def resolveFocus(roots: Seq[dom.Element], locator: RangeFocus): Maybe[dom.Element] =
        val byPath = locator.path.flatMap { path =>
            Maybe.fromOption(roots.iterator.flatMap { root =>
                val descendants = root.querySelectorAll("[data-kyo-path]")
                val candidates =
                    Iterator.single(root) ++ (0 until descendants.length).iterator.map { i =>
                        descendants(i).asInstanceOf[dom.Element]
                    }
                candidates.find(_.getAttribute("data-kyo-path") == path)
            }.nextOption())
        }
        byPath.orElse {
            Maybe.fromOption(roots.lift(locator.rootIndex)).flatMap { root =>
                var current: dom.Element = root
                var valid                = true
                val indexes              = locator.childIndexes.iterator
                while valid && indexes.hasNext do
                    val index = indexes.next()
                    if index >= current.children.length then valid = false
                    else current = current.children(index).asInstanceOf[dom.Element]
                end while
                if valid then Present(current) else Absent
            }
        }
    end resolveFocus

    /** Unwind stack entries whose seeded focus-auto element left the document, returning focus at most once.
      *
      * Stops at the first entry whose element is still in the document: that seed is still on screen, and
      * restoring an entry below it would move focus out of it. Below that, exactly one restore may land: a
      * deeper entry belongs to a seed that closed while a newer one stayed open, so its return target is stale
      * and must not override the one just restored. Its entry is still dropped, so it cannot fire on a later
      * sweep either. Mirrors `sweepFocusAuto` in HtmlRenderer.clientJs.
      *
      * A restore only lands where the removal actually took the focus with it (see [[focusWasLost]]).
      */
    @tailrec
    private def sweepFocusAuto(restored: Boolean = false): Unit =
        focusReturnStack.lastMaybe match
            case Present(seed)
                if document.querySelector(s"""[data-kyo-path="${seed.path}"][data-kyo-focus-auto]""") == null =>
                focusReturnStack = focusReturnStack.dropLeftAndRight(0, 1)
                val landed =
                    !restored && seed.restore && focusWasLost && seed.returnTo.exists(retPath => focusIfPresent(retPath))
                sweepFocusAuto(restored || landed)
            case _ => ()
    end sweepFocusAuto

    /** Paths of the elements that carried `data-kyo-scroll-auto` after the last patch.
      *
      * The flag MOVES (a highlight walking down a list), which is the difference from focus-auto's
      * per-region "newly appeared" test: both the row losing it and the row taking it were already on
      * screen, so only the carrier set tells them apart. Mirrors `__scrollAutoPaths` in HtmlRenderer.clientJs.
      */
    private var scrollAutoPaths: Set[String] = Set.empty

    /** Scrolls the element that newly carries `data-kyo-scroll-auto` into view, at most one per patch.
      *
      * `block: "nearest"` is the whole point: it moves the nearest scrollable ancestor by the least it can and
      * does nothing when the element is already visible, so a highlight walking within view never moves the
      * panel under the reader. A patch that leaves the flag where it was scrolls nothing.
      */
    private def sweepScrollAuto(scroll: Boolean = true): Unit =
        if !sawScrollAuto then return
        val els  = document.querySelectorAll("[data-kyo-scroll-auto]")
        val list = (0 until els.length).map(els(_).asInstanceOf[dom.Element])
        val now  = list.flatMap(el => Maybe(el.getAttribute("data-kyo-path")).toList).toSet
        val fresh = list.find { el =>
            val p = el.getAttribute("data-kyo-path")
            p != null && !scrollAutoPaths.contains(p)
        }
        scrollAutoPaths = now
        // The first paint only records what is already carrying the flag: a page that arrives with a
        // highlight has not moved it, and scrolling on load would fight the browser's own restoration.
        if scroll then
            fresh.foreach(el =>
                discard(el.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal(block = "nearest", inline = "nearest")))
            )
        end if
    end sweepScrollAuto

    /** Whether the patch that removed the seeded element took the focus with it.
      *
      * Removing the focused element leaves `document.activeElement` on `body` (or null), which is the case a
      * restore exists for: the reader was inside the panel and would otherwise be dropped at the top of the
      * document. Any other element there means they moved focus themselves before the panel went, a Tab out of
      * a combobox being the everyday case, and putting it back would undo the move they just made. Twin of the
      * same check in HtmlRenderer.clientJs.
      */
    private def focusWasLost: Boolean =
        val active = document.activeElement
        active == null || (active eq document.body)

    /** Focus the element carrying `path`; `false` when it is no longer in the document (nothing focused). */
    private def focusIfPresent(path: String): Boolean =
        val el = document.querySelector(s"""[data-kyo-path="$path"]""")
        if el == null then false
        else
            focusNoScroll(el)
            true
        end if
    end focusIfPresent

    /** Moves the caret on `el`, tolerating the two documented ways that is a no-op.
      *
      * Elements outside input and textarea (select, contenteditable) have no `setSelectionRange` at all, and on
      * input types without a text selection (email, number, both of which kyo-ui offers as text inputs) it throws
      * `InvalidStateError`. In either case the value is already set and only the caret stays put. Any other
      * JavaScript exception is a real failure and propagates rather than being swallowed. Mirrored by
      * `kyoSetCaret` in `HtmlRenderer.clientJs`.
      */
    private def setSelection(el: dom.Element, start: Int, end: Int): Unit =
        val dyn = el.asInstanceOf[scalajs.js.Dynamic]
        if scalajs.js.typeOf(dyn.setSelectionRange) == "function" then
            try discard(dyn.setSelectionRange(start, end))
            catch
                case ex: scalajs.js.JavaScriptException
                    if ex.exception.asInstanceOf[scalajs.js.Dynamic].name.asInstanceOf[String] == "InvalidStateError" =>
                    ()
        end if
    end setSelection

    // Bridge a Kyo Async computation from a JS callback boundary by offering it to the page-scoped drain
    // channel. The single AllowUnsafe site narrows to the offer crossing (the JS callback has no Kyo
    // context); a drop on a closed channel is fine (the page is being torn down anyway).
    private def fireFromJs(events: Channel[Unit < Async], eff: Unit < Async)(using Frame): Unit =
        // Unsafe: JS event callbacks run outside any Kyo context; this is the one controlled crossing point.
        import AllowUnsafe.embrace.danger
        // runPartial drops only a Closed (offer on a torn-down channel); a Panic propagates to evalOrThrow and
        // surfaces (thrown at the boundary) rather than being swallowed by the discard.
        discard(Sync.Unsafe.evalOrThrow(Abort.runPartial[Closed](events.offer(eff)).unit))
    end fireFromJs

    /** Scan `root` and all descendants for `data-kyo-prop-*` attributes, apply each as a direct
      * DOM property on the element, then remove the data attribute so it does not linger.
      */
    private def applyJsProps(root: dom.Element)(using Frame): Unit < Sync =
        Sync.defer(applyJsPropsSync(root))

    private def applyJsPropsSync(root: dom.Element): Unit =
        if !sawJsProp then return
        val propPrefix = "data-kyo-prop-"
        // CSS has no attribute-name-prefix selector, so `[data-kyo-prop-*]` is not a valid selector and
        // throws SyntaxError. Visit the root and every descendant, reading property names directly from
        // the live attribute map.
        def applyElement(el: dom.Element): Unit =
            var i = el.attributes.length - 1
            while i >= 0 do
                val attrName = el.attributes(i).name
                if attrName.startsWith(propPrefix) then
                    val propName = attrName.stripPrefix(propPrefix)
                    val value    = el.getAttribute(attrName)
                    el.asInstanceOf[scalajs.js.Dynamic].updateDynamic(propName)(value)
                    el.removeAttribute(attrName)
                end if
                i -= 1
            end while
        end applyElement

        applyElement(root)
        val elements = root.querySelectorAll("*")
        var i        = 0
        while i < elements.length do
            applyElement(elements(i).asInstanceOf[dom.Element])
            i += 1
        end while
    end applyJsPropsSync

    /** Start every freshly-inserted SMIL animation under `root`.
      *
      * Chart transition `<animate>` elements use `begin="indefinite"` so they do not auto-play against the
      * shared SVG document timeline (which would make a post-load update snap to the frozen `to` value).
      * Calling `beginElement()` after the node is inserted starts the tween relative to now. The call is
      * deferred one animation frame so the SMIL engine has registered the newly inserted elements; a node
      * that was already replaced again by then throws and is ignored.
      */
    private def beginAnimationsSync(root: dom.Element): Unit =
        if !sawSvgAnim then return
        val anims = root.querySelectorAll("animate,animateTransform,animateMotion")
        if anims.length > 0 then
            discard(dom.window.requestAnimationFrame { (_: Double) =>
                var i = 0
                while i < anims.length do
                    try anims(i).asInstanceOf[scalajs.js.Dynamic].beginElement()
                    catch case _: Throwable => ()
                    i += 1
                end while
            })
        end if
    end beginAnimationsSync

    /** True when `start` or any of its ancestors below `document.body` declares event type `t` in `data-kyo-ev`.
      *
      * ReactiveUI.dispatchToElement bubbles an event to every ancestor that declared a handler for its type, so the
      * SPA forwarding gate must forward when ANY ancestor declared it, not just the target (checking only the
      * target's own data-kyo-ev would drop e.g. a keydown meant for an ancestor panel before bubble dispatch runs).
      *
      * The walk climbs the LOGICAL tree, which the portal makes distinct from the DOM one: see [[logicalAncestor]].
      * Twin of `he` in HtmlRenderer.clientJs; keep the two in lockstep.
      */
    /** The next element up the LOGICAL tree. That is the DOM parent everywhere except at a portal: a re-homed
      * element sits directly under `document.body`, so the climb hops back to its placeholder slot first and
      * continues from there. Twin of `__kyoLogicalParent` in HtmlRenderer.clientJs.
      */
    private def logicalAncestor(n: dom.Element): dom.Element =
        val hopped =
            if n.hasAttribute("data-kyo-portal") && (n.parentNode eq document.body) then
                val p = n.getAttribute("data-kyo-path")
                if p == null then null else document.querySelector(s"""[data-kyo-portal-slot="$p"]""")
            else n
        if hopped == null then null
        else
            hopped.parentNode match
                case p: dom.Element => p
                case _              => null
        end if
    end logicalAncestor

    private[kyo] def declaredInChain(start: dom.Element, t: String): Boolean =
        var n: dom.Element = start
        var found          = false
        while !found && n != null && (n ne document.body) do
            val ev = n.getAttribute("data-kyo-ev")
            if ev != null && ev.split(",").contains(t) then found = true
            else n = logicalAncestor(n)
        end while
        found
    end declaredInChain

    private def addScopedListener(
        eventType: String,
        listener: scalajs.js.Function1[dom.Event, Unit],
        capture: Boolean
    )(using Frame): Unit < (Scope & Sync) =
        final case class Installed(
            target: dom.EventTarget,
            eventType: String,
            listener: scalajs.js.Function1[dom.Event, Unit],
            capture: Boolean
        )
        Scope.acquireRelease {
            Sync.defer {
                val target = document.body
                target.addEventListener(eventType, listener, capture)
                Installed(target, eventType, listener, capture)
            }
        }(installed =>
            Sync.defer(installed.target.removeEventListener(installed.eventType, installed.listener, installed.capture))
        ).unit
    end addScopedListener

    private def addScopedListener(
        eventType: String,
        listener: scalajs.js.Function1[dom.Event, Unit],
        options: dom.EventListenerOptions
    )(using Frame): Unit < (Scope & Sync) =
        final case class Installed(
            target: dom.EventTarget,
            eventType: String,
            listener: scalajs.js.Function1[dom.Event, Unit],
            options: dom.EventListenerOptions
        )
        Scope.acquireRelease {
            Sync.defer {
                val target = document.body
                target.addEventListener(eventType, listener, options)
                Installed(target, eventType, listener, options)
            }
        }(installed =>
            Sync.defer(installed.target.removeEventListener(installed.eventType, installed.listener, installed.options))
        ).unit
    end addScopedListener

    /** Set up capture-phase event delegation on document.body. */
    private def setupEventDelegation(dispatch: (Seq[String], UIEvent) => Boolean < Async, events: Channel[Unit < Async])(using
        Frame
    ): Unit < (Scope & Sync) =
        final class ChainTypes(target: dom.Element):
            def contains(t: String): Boolean = declaredInChain(target, t)

        // A submit button's click makes the browser fire a native `submit` right after, but the Click dispatch
        // already emulates onSubmit; this flag (set on Click, cleared on a 0-timeout) suppresses that one
        // following native submit so the form handler runs once. Mirrors clientJs's `_kyoClickSubmit` guard.
        var clickSubmitGuard = false

        val handler: scalajs.js.Function1[dom.Event, Unit] = (e: dom.Event) =>
            // Runs before path resolution so it fires even when the focused descendant is not a path element;
            // the keydown is still forwarded below, so the region's own onKeyDown runs (see `scrollKeyPrevented`).
            if e.`type` == "keydown" then
                val ke  = e.asInstanceOf[dom.KeyboardEvent]
                val tgt = e.target.asInstanceOf[dom.Element]
                // preventActivation: an inert region declines the browser default for the keys that change a
                // native control's value, so a readonly checkbox stays focusable rather than being disabled.
                // The keydown is still forwarded. Twin of the `data-kyo-inert` block in clientJs.
                if tgt != null && KeyPolicy.suppressesActivation(ke.key) && tgt.closest("[data-kyo-inert]") != null then
                    e.preventDefault()
                if tgt != null && scrollKeyPrevented(ke.key, tgt) && tgt.closest("[data-kyo-scroll-keys]") != null then
                    e.preventDefault()
                if tgt != null && doubleActivation(ke.key, tgt) then e.preventDefault()
            end if
            // The click's default goes the same way, so a readonly control does not toggle under the pointer.
            if e.`type` == "click" then
                val tgt = e.target.asInstanceOf[dom.Element]
                if tgt != null && tgt.closest("[data-kyo-inert]") != null then e.preventDefault()
            findPathElement(e.target.asInstanceOf[dom.Element]).foreach { target =>
                val path    = parsePath(target.getAttribute("data-kyo-path"))
                val evTypes = ChainTypes(target)
                val t       = e.`type`

                val event: Maybe[UIEvent] =
                    if t == "click" then
                        val targetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val me       = e.asInstanceOf[dom.MouseEvent]
                        val mouse = MouseEventData(
                            modifiers = UI.Modifiers(me.ctrlKey, me.altKey, me.shiftKey, me.metaKey),
                            targetId = targetId,
                            position = Present(UI.Point(me.clientX, me.clientY))
                        )
                        // Prevent the browser's default navigation only when the anchor carries a kyo
                        // click handler (so the handler, not the href, drives the action). A plain href
                        // keeps native behavior: an in-page `#anchor` scrolls, and a cross-document route
                        // is handled by UILocation's interceptor. Prevent-defaulting every anchor here
                        // would also kill those.
                        //
                        // A MODIFIED click is never "let the handler drive the action": it is the user
                        // asking the browser, explicitly, for a new tab or window, so the default stays
                        // theirs. The handler still runs — the effect (analytics, closing a menu,
                        // notifying a parent) is wanted either way, and only the navigation belongs to
                        // the browser. This is the test `UILocation`'s anchor interceptor already
                        // applies to the same click; without it here, an anchor that also runs an effect
                        // silently loses the one capability it has over a button.
                        //
                        // `button != 0` carries no weight on a current browser — the middle button fires
                        // `auxclick`, which kyo listens for nowhere, so it never reaches this branch. It
                        // is kept for the browsers that still route a non-primary click through `click`,
                        // and to read identically to `UILocation`.
                        val modifiedClick =
                            me.ctrlKey || me.metaKey || me.shiftKey || me.altKey || me.button != 0
                        if !modifiedClick && target.tagName.toLowerCase == "a" && evTypes.contains("click") then
                            e.preventDefault()
                        clickSubmitGuard = true
                        discard(dom.window.setTimeout(() => clickSubmitGuard = false, 0))
                        Present(UIEvent.Click(path, mouse))
                    else if t == "contextmenu" && evTypes.contains("contextmenu") then
                        // Suppress the native menu only when a handler was declared (this branch fired).
                        e.preventDefault()
                        val targetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val me       = e.asInstanceOf[dom.MouseEvent]
                        Present(UIEvent.ContextMenu(
                            path,
                            MouseEventData(
                                modifiers = UI.Modifiers(me.ctrlKey, me.altKey, me.shiftKey, me.metaKey),
                                targetId = targetId,
                                position = Present(UI.Point(me.clientX, me.clientY))
                            )
                        ))
                    else if t == "input" && evTypes.contains("input") then
                        Present(UIEvent.Input(path, e.target.asInstanceOf[dom.html.Input].value))
                    else if t == "change" && evTypes.contains("change") then
                        val tgt = e.target.asInstanceOf[dom.html.Input]
                        val typ = tgt.`type`
                        if typ == "checkbox" || typ == "radio" then
                            Present(UIEvent.ChangeChecked(path, tgt.checked))
                        else if typ == "number" || typ == "range" then
                            Present(UIEvent.ChangeNumeric(path, tgt.value.toDouble))
                        else if typ == "file" then
                            val files = tgt.files
                            if evTypes.contains("fileselect") then
                                // A FileReader per file, indexed so order survives; post FileSelect once all complete.
                                val n = files.length
                                if n > 0 then
                                    val results = scala.collection.mutable.ArrayBuffer.fill[Maybe[UI.FilePayload]](n)(Absent)
                                    var doneCt  = 0
                                    var i       = 0
                                    while i < n do
                                        val ix     = i
                                        val f      = files(ix)
                                        val reader = new dom.FileReader()
                                        reader.onload = (_: dom.Event) =>
                                            val content = reader.result.asInstanceOf[String]
                                            results(ix) = Present(UI.FilePayload(f.name, f.size.toLong, f.`type`, content))
                                            doneCt += 1
                                            if doneCt == n then
                                                val payloads = results.toSeq.collect { case Present(p) => p }
                                                fireFromJs(events, dispatch(path, UIEvent.FileSelect(path, payloads)).unit)
                                        reader.readAsText(f)
                                        i += 1
                                    end while
                                end if
                                Absent
                            else
                                // Legacy: first file's text -> Change (byte-compatible content-only).
                                if files.length > 0 then
                                    val reader = new dom.FileReader()
                                    reader.onload = (_: dom.Event) =>
                                        val content = reader.result.asInstanceOf[String]
                                        fireFromJs(events, dispatch(path, UIEvent.Change(path, content)).unit)
                                    reader.readAsText(files(0))
                                end if
                                Absent
                            end if
                        else
                            Present(UIEvent.Change(path, tgt.value))
                        end if
                    else if t == "submit" && evTypes.contains("submit") then
                        e.preventDefault()
                        if clickSubmitGuard then Absent
                        else
                            val submitTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                            val submitMouse = MouseEventData(
                                modifiers = UI.Modifiers.none,
                                targetId = submitTargetId
                            )
                            Present(UIEvent.Submit(path, submitMouse))
                        end if
                    else if t == "keydown" && evTypes.contains("keydown") then
                        val ke         = e.asInstanceOf[dom.KeyboardEvent]
                        val kdTargetId = Maybe(ke.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        Present(UIEvent.KeyDown(
                            path,
                            KeyboardEventData(
                                key = ke.key,
                                modifiers = UI.Modifiers(ke.ctrlKey, ke.altKey, ke.shiftKey, ke.metaKey),
                                targetId = kdTargetId
                            )
                        ))
                    else if t == "keyup" && evTypes.contains("keyup") then
                        val ke         = e.asInstanceOf[dom.KeyboardEvent]
                        val kuTargetId = Maybe(ke.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        Present(UIEvent.KeyUp(
                            path,
                            KeyboardEventData(
                                key = ke.key,
                                modifiers = UI.Modifiers(ke.ctrlKey, ke.altKey, ke.shiftKey, ke.metaKey),
                                targetId = kuTargetId
                            )
                        ))
                    else if t == "focus" && evTypes.contains("focus") then
                        val focusTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        // FocusEvent does not carry modifier keys (not a MouseEvent); use Modifiers.none
                        Present(UIEvent.Focus(
                            path,
                            MouseEventData(UI.Modifiers.none, focusTargetId)
                        ))
                    else if t == "blur" && evTypes.contains("blur") then
                        val blurTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        Present(UIEvent.Blur(path, MouseEventData(UI.Modifiers.none, blurTargetId)))
                    else if t == "mouseover" && evTypes.contains("mouseover") then
                        val hoverTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val me            = e.asInstanceOf[dom.MouseEvent]
                        Present(UIEvent.Hover(
                            path,
                            MouseEventData(
                                modifiers = UI.Modifiers(me.ctrlKey, me.altKey, me.shiftKey, me.metaKey),
                                targetId = hoverTargetId,
                                position = Present(UI.Point(me.clientX, me.clientY))
                            )
                        ))
                    else if t == "mouseout" && evTypes.contains("mouseout") then
                        val unhoverTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val me              = e.asInstanceOf[dom.MouseEvent]
                        Present(UIEvent.Unhover(
                            path,
                            MouseEventData(
                                modifiers = UI.Modifiers(me.ctrlKey, me.altKey, me.shiftKey, me.metaKey),
                                targetId = unhoverTargetId,
                                position = Present(UI.Point(me.clientX, me.clientY))
                            )
                        ))
                    else if t == "wheel" && evTypes.contains("wheel") then
                        val wheelTargetId = Maybe(e.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
                        val we            = e.asInstanceOf[dom.WheelEvent]
                        Present(UIEvent.Scroll(
                            path,
                            deltaX = we.deltaX,
                            deltaY = we.deltaY,
                            modifiers = UI.Modifiers(we.ctrlKey, we.altKey, we.shiftKey, we.metaKey),
                            targetId = wheelTargetId
                        ))
                    else
                        Absent

                event.foreach { ev =>
                    fireFromJs(events, dispatch(path, ev).unit)
                }
            }
        end handler

        val wheelOptions = js.Dynamic.literal(capture = true, passive = false).asInstanceOf[dom.EventListenerOptions]
        // onScrollPosition: rAF-coalesces a scroll burst to one dispatch per frame. Scroll does not bubble, but the
        // capture-phase listener catches descendant viewport scrolls; a page-level scroll (no data-kyo-path) is ignored.
        var scrRaf               = 0
        var scrEl: dom.Element   = null
        var scrPath: Seq[String] = Seq.empty
        val scrollHandler: scalajs.js.Function1[dom.Event, Unit] = (e: dom.Event) =>
            findPathElement(e.target.asInstanceOf[dom.Element]).foreach { target =>
                if declaredInChain(target, "scroll") then
                    scrEl = target
                    scrPath = parsePath(target.getAttribute("data-kyo-path"))
                    if scrRaf == 0 then
                        scrRaf = dom.window.requestAnimationFrame { (_: Double) =>
                            scrRaf = 0
                            val sid = Maybe(scrEl.id).filter(_.nonEmpty)
                            fireFromJs(
                                events,
                                dispatch(scrPath, UIEvent.ScrollPosition(scrPath, scrEl.scrollTop, scrEl.scrollLeft, sid)).unit
                            )
                        }
                    end if
            }
        val scrollOptions = js.Dynamic.literal(capture = true, passive = true).asInstanceOf[dom.EventListenerOptions]
        for
            _ <- addScopedListener("click", handler, true)
            _ <- addScopedListener("contextmenu", handler, true)
            _ <- addScopedListener("input", handler, true)
            _ <- addScopedListener("change", handler, true)
            _ <- addScopedListener("submit", handler, true)
            _ <- addScopedListener("keydown", handler, true)
            _ <- addScopedListener("keyup", handler, true)
            _ <- addScopedListener("focus", handler, true)
            _ <- addScopedListener("blur", handler, true)
            _ <- addScopedListener("mouseover", handler, true)
            _ <- addScopedListener("mouseout", handler, true)
            _ <- addScopedListener("wheel", handler, wheelOptions)
            _ <- addScopedListener("scroll", scrollHandler, scrollOptions)
        yield ()
        end for
    end setupEventDelegation

    private def findPathElement(el: dom.Element): Maybe[dom.Element] =
        if el == null || (el eq document.body) then Absent
        else if el.hasAttribute("data-kyo-path") then Present(el)
        else
            el.parentNode match
                case p: dom.Element => findPathElement(p)
                case _              => Absent

    private def parsePath(p: String): Seq[String] =
        if p == null || p.isEmpty then Seq.empty
        else p.split("\\.").toSeq

    // ---- feature-marker gates ----

    /** Has any HTML that ever entered this document carried the marker at all?
      *
      * Sticky by design, and a false flag is a proof rather than a guess: every element in the document arrives
      * either through the boot `innerHTML` or through [[parseToContainer]], both of which record here first, and
      * no code path adds these attributes to a live element afterwards (`data-kyo-ghost` is the only imperative
      * one, and nothing scans for it). So while a flag is false, every scan it guards would walk a subtree to
      * return nothing.
      *
      * That is the common case, not an edge case: an app using no enter/leave transition, no focus-auto, no JS
      * property and no SMIL animation still paid four whole-subtree selector queries per inserted row and three
      * per removed one, including `querySelectorAll("*")`, which visits every descendant of every new row.
      */
    private var sawEnter      = false
    private var sawLeave      = false
    private var sawFocusAuto  = false
    private var sawScrollAuto = false
    private var sawJsProp     = false
    private var sawSvgAnim    = false

    private def noteMarkers(html: String): Unit =
        if !sawEnter && html.contains("data-kyo-enter") then sawEnter = true
        if !sawLeave && html.contains("data-kyo-leave") then sawLeave = true
        if !sawFocusAuto && html.contains("data-kyo-focus-auto") then sawFocusAuto = true
        if !sawScrollAuto && html.contains("data-kyo-scroll-auto") then sawScrollAuto = true
        if !sawJsProp && html.contains("data-kyo-prop-") then sawJsProp = true
        // Covers <animate, <animateTransform and <animateMotion in one search.
        if !sawSvgAnim && html.contains("<animate") then sawSvgAnim = true
    end noteMarkers

    // ---- enter/leave transition mirror (SPA transport) ----

    /** The set of `data-kyo-path` values of every `data-kyo-enter` element inside `root`, `root` itself included. */
    private def enterPaths(root: dom.Element): Set[String] =
        if !sawEnter then return Set.empty
        val els = root.querySelectorAll("[data-kyo-enter]")
        val ds = (0 until els.length).flatMap { i =>
            Maybe(els(i).asInstanceOf[dom.Element].getAttribute("data-kyo-path")).toList
        }.toSet
        if root.hasAttribute("data-kyo-enter") && root.hasAttribute("data-kyo-path") then
            ds + root.getAttribute("data-kyo-path")
        else ds
    end enterPaths

    private def enterPaths(roots: Seq[dom.Element]): Set[String] =
        roots.iterator.flatMap(enterPaths).toSet

    /** Animate every `data-kyo-enter` element under `newRoot` (root included) whose path is not in `oldSet`: add the enter
      * classes, force a reflow, then remove them next frame so the CSS transition runs from the enter-from state.
      */
    private def seedEnter(newRoot: dom.Element, oldSet: Set[String]): Unit =
        seedEnter(Seq(newRoot), oldSet)

    private def seedEnter(newRoots: Seq[dom.Element], oldSet: Set[String]): Unit =
        if !sawEnter then return
        newRoots.foreach { newRoot =>
            val els = newRoot.querySelectorAll("[data-kyo-enter]")
            val cand =
                (if newRoot.hasAttribute("data-kyo-enter") then Seq(newRoot) else Seq.empty) ++
                    (0 until els.length).map(els(_).asInstanceOf[dom.Element])
            cand.foreach { el =>
                val p = el.getAttribute("data-kyo-path")
                if p != null && !oldSet.contains(p) then
                    val cls     = el.getAttribute("data-kyo-enter").split("\\s+").filter(_.nonEmpty)
                    val clsList = el.asInstanceOf[scalajs.js.Dynamic].classList
                    cls.foreach(c => clsList.add(c))
                    val _ = el.asInstanceOf[scalajs.js.Dynamic].offsetWidth // force reflow
                    discard(dom.window.requestAnimationFrame { (_: Double) =>
                        cls.foreach(c => clsList.remove(c))
                    })
                end if
            }
        }
    end seedEnter

    private def leavePaths(roots: Seq[dom.Element]): Set[String] =
        roots.iterator.flatMap { root =>
            val descendants = root.querySelectorAll("[data-kyo-leave]")
            val elements =
                (if root.hasAttribute("data-kyo-leave") then Iterator.single(root) else Iterator.empty) ++
                    (0 until descendants.length).iterator.map(descendants(_).asInstanceOf[dom.Element])
            elements.flatMap(element => Maybe(element.getAttribute("data-kyo-path")).toList)
        }.toSet

    /** The set of paths of `data-kyo-leave` elements in an HTML fragment that survive an element replacement. */
    private def leaveSurvSet(html: String): Set[String] =
        val tpl = document.createElement("template").asInstanceOf[scalajs.js.Dynamic]
        tpl.innerHTML = html
        val content = tpl.content.asInstanceOf[dom.DocumentFragment]
        val els     = content.querySelectorAll("[data-kyo-leave]")
        (0 until els.length).flatMap { i =>
            Maybe(els(i).asInstanceOf[dom.Element].getAttribute("data-kyo-path")).toList
        }.toSet
    end leaveSurvSet

    /** Strip `data-kyo-*` and `id` from a subtree so a ghost clone is inert (no selector collisions). */
    private def stripKyo(el: dom.Element): Unit =
        def strip(e: dom.Element): Unit =
            val dyn = e.asInstanceOf[scalajs.js.Dynamic]
            if scalajs.js.typeOf(dyn.getAttributeNames) == "function" then
                val names = dyn.getAttributeNames().asInstanceOf[scalajs.js.Array[String]]
                names.foreach(n => if n.startsWith("data-kyo-") || n == "id" then e.removeAttribute(n))
        end strip
        strip(el)
        val ds = el.querySelectorAll("*")
        (0 until ds.length).foreach(i => strip(ds(i).asInstanceOf[dom.Element]))
    end stripKyo

    /** Prepare leave ghosts for the OUTERMOST `data-kyo-leave` elements under `root` being removed (path not in `surv`).
      * Captures rect + clone WHILE the node is still in the DOM; returns (sourceNode, ghostNode, leaveClasses)
      * descriptors. The survivor set is a PREDICTION: a preserved subtree survives the patch despite not matching, which
      * is exactly what the opaque mount boundary does when it keeps a live mount's content under differently-shaped
      * incoming html. [[spawnGhosts]] therefore re-checks the SOURCE at spawn time and drops nodes still in the document.
      */
    private def prepareLeaveGhosts(root: dom.Element, surv: Set[String]): Seq[(dom.Element, dom.Element, String)] =
        prepareLeaveGhosts(Seq(root), surv)

    private def prepareLeaveGhosts(roots: Seq[dom.Element], surv: Set[String]): Seq[(dom.Element, dom.Element, String)] =
        if !sawLeave then return Seq.empty
        val cand = roots.flatMap { root =>
            val els = root.querySelectorAll("[data-kyo-leave]")
            (if root.getAttribute("data-kyo-leave") != null then Seq(root) else Seq.empty) ++
                (0 until els.length).map(els(_).asInstanceOf[dom.Element])
        }
        val removed = cand.filter { e =>
            val p = e.getAttribute("data-kyo-path")
            p == null || !surv.contains(p)
        }
        val outer = removed.filterNot(e => removed.exists(o => (o ne e) && o.contains(e)))
        outer.map { node =>
            val rect  = node.asInstanceOf[scalajs.js.Dynamic].getBoundingClientRect()
            val leave = node.getAttribute("data-kyo-leave")
            val g     = node.cloneNode(true).asInstanceOf[dom.Element]
            stripKyo(g)
            val st = g.asInstanceOf[scalajs.js.Dynamic].style
            st.position = "fixed"
            st.left = rect.left.asInstanceOf[Double].toString + "px"
            st.top = rect.top.asInstanceOf[Double].toString + "px"
            st.width = rect.width.asInstanceOf[Double].toString + "px"
            st.height = rect.height.asInstanceOf[Double].toString + "px"
            st.margin = "0"
            st.pointerEvents = "none"
            g.setAttribute("data-kyo-ghost", "1")
            (node, g, if leave == null then "" else leave)
        }
    end prepareLeaveGhosts

    /** Append prepared ghosts to `<body>`, add their leave classes next frame, remove on transitionend/animationend or a
      * 1s safety. A ghost whose SOURCE node is still in the document is dropped: the patch preserved it, so playing a
      * leave animation over the live element would be a false departure. Removal-based rather than predictive, so every
      * preservation mechanism the morph grows is covered without a matching change here.
      */
    private def spawnGhosts(ghosts: Seq[(dom.Element, dom.Element, String)]): Unit =
        ghosts.foreach { case (src, g, leave) =>
            if !document.contains(src) then spawnGhost(g, leave)
        }
    end spawnGhosts

    private def spawnGhost(g: dom.Element, leave: String): Unit =
        discard(document.body.appendChild(g))
        val cls     = leave.split("\\s+").filter(_.nonEmpty)
        val clsList = g.asInstanceOf[scalajs.js.Dynamic].classList
        discard(dom.window.requestAnimationFrame((_: Double) => cls.foreach(c => clsList.add(c))))
        var done = false
        def cleanup(): Unit =
            if !done then
                done = true
                if g.parentNode != null then discard(g.parentNode.removeChild(g))
        val listener: scalajs.js.Function1[dom.Event, Unit] = (_: dom.Event) => cleanup()
        g.addEventListener("transitionend", listener)
        g.addEventListener("animationend", listener)
        val to: scalajs.js.Function0[Unit] = () => cleanup()
        discard(dom.window.setTimeout(to, 1000.0))
    end spawnGhost

    // ---- input filter/mask (SPA transport) ----
    // The character-level decisions live in the shared InputMasking so they are testable without a DOM;
    // what stays here is the DOM wiring.

    private def dispatchInput(t: dom.EventTarget): Unit =
        val ctor = scalajs.js.Dynamic.global.Event
        val ev   = scalajs.js.Dynamic.newInstance(ctor)("input", scalajs.js.Dynamic.literal(bubbles = true))
        discard(t.asInstanceOf[scalajs.js.Dynamic].dispatchEvent(ev))
    end dispatchInput

    private def setValue(t: dom.html.Input, v: String): Unit =
        t.value = v
        setSelection(t, v.length, v.length)
        dispatchInput(t)
    end setValue

    private def setFilteredAt(t: dom.html.Input, txt: String, s: Int, e: Int): Unit =
        val v = t.value
        t.value = v.substring(0, s) + txt + v.substring(e)
        val np = s + txt.length
        setSelection(t, np, np)
        dispatchInput(t)
    end setFilteredAt

    private def setupInputMasking()(using Frame): Unit < (Scope & Sync) =
        val handler: scalajs.js.Function1[dom.Event, Unit] = (e: dom.Event) =>
            val tRaw = e.target
            if tRaw != null then
                val el = tRaw.asInstanceOf[dom.Element]
                // Interactive.data lets any element carry data-kyo-filter, and everything below assumes a value
                // property and a text selection. Throwing from a beforeinput capture listener would break typing
                // for the whole page, so anything but a text field is left alone.
                val isTextField = el.tagName == "INPUT" || el.tagName == "TEXTAREA"
                val filt        = if isTextField then el.getAttribute("data-kyo-filter") else null
                val mask        = if isTextField then el.getAttribute("data-kyo-mask") else null
                if filt != null || mask != null then
                    val t   = tRaw.asInstanceOf[dom.html.Input]
                    val dyn = e.asInstanceOf[scalajs.js.Dynamic]
                    val it  = if scalajs.js.typeOf(dyn.inputType) == "string" then dyn.inputType.asInstanceOf[String] else ""
                    def selStart: Int =
                        val d = t.asInstanceOf[scalajs.js.Dynamic]
                        if scalajs.js.typeOf(d.selectionStart) == "number" then d.selectionStart.asInstanceOf[Int] else t.value.length
                    def selEnd: Int =
                        val d = t.asInstanceOf[scalajs.js.Dynamic]
                        if scalajs.js.typeOf(d.selectionEnd) == "number" then d.selectionEnd.asInstanceOf[Int] else selStart
                    def transferText: String =
                        val dt = dyn.dataTransfer
                        if dt != null && scalajs.js.typeOf(dt) == "object" then dt.getData("text").asInstanceOf[String]
                        else if scalajs.js.typeOf(dyn.data) == "string" then dyn.data.asInstanceOf[String]
                        else ""
                    end transferText
                    // insertCompositionText is deliberately absent below: preventDefault on it does not filter the
                    // input, it aborts the composition, which breaks CJK input, dead keys and mobile autocorrect.
                    // Composition is let through and the finished text is corrected by the compositionend listener.
                    if filt != null then
                        if it.startsWith("delete") then ()
                        else if it == "insertText" || it == "insertReplacementText" then
                            if scalajs.js.typeOf(dyn.data) == "string" then
                                val ds = dyn.data.asInstanceOf[String]
                                val f1 = InputMasking.filterStr(filt, ds, t.value)
                                if f1 != ds then
                                    e.preventDefault()
                                    if f1.nonEmpty then setFilteredAt(t, f1, selStart, selEnd)
                        else if it == "insertFromPaste" || it == "insertFromDrop" then
                            e.preventDefault()
                            val f2 = InputMasking.filterStr(filt, transferText, t.value)
                            if f2.nonEmpty then setFilteredAt(t, f2, selStart, selEnd)
                        end if
                    else if mask != null then
                        val tokens = InputMasking.parseMask(mask)
                        if it.startsWith("delete") then
                            e.preventDefault()
                            val raw = InputMasking.maskRaw(tokens, t.value)
                            val nr  = if raw.nonEmpty then raw.substring(0, raw.length - 1) else raw
                            setValue(t, InputMasking.maskFormat(tokens, nr))
                        else if it == "insertText" || it == "insertReplacementText" ||
                            it == "insertFromPaste" || it == "insertFromDrop"
                        then
                            e.preventDefault()
                            val ins = if it == "insertFromPaste" || it == "insertFromDrop" then transferText
                            else if scalajs.js.typeOf(dyn.data) == "string" then dyn.data.asInstanceOf[String]
                            else ""
                            var raw2 = InputMasking.maskRaw(tokens, t.value)
                            var ci   = 0
                            var full = false
                            while ci < ins.length && !full do
                                InputMasking.maskClassAt(tokens, raw2.length) match
                                    case Present(cls) =>
                                        val ch = ins.charAt(ci)
                                        if InputMasking.maskOk(cls, ch) then raw2 = raw2 + ch
                                    case Absent => full = true
                                end match
                                ci += 1
                            end while
                            setValue(t, InputMasking.maskFormat(tokens, raw2))
                        end if
                    end if
                end if
            end if
        addScopedListener("beforeinput", handler, true).andThen {
            addScopedListener("compositionend", compositionEndHandler, true)
        }
    end setupInputMasking

    /** Corrects the whole value once a composition finishes.
      *
      * An IME, a dead key or mobile autocorrect produces its text only when the composition ends, so there is no
      * per-character event to constrain; the finished value is filtered or formatted here instead. Writing back only
      * on a change keeps a composition that already conforms free of a caret jump and of a spurious input event.
      * Mirrored by `kyoCompositionEnd` in `HtmlRenderer.clientJs`.
      */
    private val compositionEndHandler: scalajs.js.Function1[dom.Event, Unit] = (e: dom.Event) =>
        val tRaw = e.target
        if tRaw != null then
            val el = tRaw.asInstanceOf[dom.Element]
            if el.tagName == "INPUT" || el.tagName == "TEXTAREA" then
                val t    = tRaw.asInstanceOf[dom.html.Input]
                val filt = el.getAttribute("data-kyo-filter")
                val mask = el.getAttribute("data-kyo-mask")
                val v    = t.value
                val nv =
                    if filt != null then InputMasking.filterStr(filt, v, "")
                    else if mask != null then InputMasking.maskNormalize(mask, v)
                    else v
                if nv != v then setValue(t, nv)
            end if
        end if

    /** Whether the browser's own activation of `target` would run a handler the dispatcher runs again.
      *
      * The rule itself is [[KeyPolicy.doubleActivates]], shared with the server-push client; this
      * reads the three facts it needs off the DOM.
      */
    private def doubleActivation(key: String, target: dom.Element): Boolean =
        val declared = target.getAttribute("data-kyo-ev")
        KeyPolicy.doubleActivates(
            key = key,
            tag = target.tagName,
            submits = submits(target),
            declaresClick = declared != null && declared.split(",").contains("click"),
            declaresKeyDown = declaredInChain(target, "keydown")
        )
    end doubleActivation

    /** Whether this button's native activation would submit a form, which is the one thing the
      * dispatcher's emulation does not carry and so the one case to leave alone.
      *
      * The type is read from the prop channel first: the server render writes `type="submit"`
      * on every button and carries the intended one in `data-kyo-prop-type` for the client to
      * apply, so the attribute alone answers differently on the two transports for the same
      * component. A button outside a form submits nothing whatever its type says.
      */
    private def submits(target: dom.Element): Boolean =
        val declaredType = target.getAttribute("data-kyo-prop-type")
        val attrType     = target.getAttribute("type")
        // An absent type is "" and submits, because that is what a browser does with it — the
        // rule and its measurements live in ButtonActivation, which the dispatcher and the
        // server-push client apply to the same markup. Comparing against "submit" instead (as
        // this did) gets `type="bogus"` and `type=""` backwards: both submit.
        val effective = if declaredType != null then declaredType else if attrType != null then attrType else ""
        ButtonActivation.submits(effective) && target.closest("form") != null
    end submits

    /** Whether a `preventScrollKeys` region should suppress the browser's page scroll for `key` on
      * `target`. The rule itself is [[KeyPolicy.preventsPageScroll]], shared with the server-push
      * client; this reads the two facts it needs off the DOM.
      */
    private def scrollKeyPrevented(key: String, target: dom.Element): Boolean =
        // `isContentEditable` is an HTMLElement member: an SVG target does not carry it, and jsdom implements
        // contentEditable on no element at all, so the property reads as undefined. Casting undefined straight to
        // Boolean throws a ClassCastException out of the keydown listener, which takes the whole delegation path
        // down with it. Read the property defensively and treat an absent one as "not editable".
        val value = target.asInstanceOf[scalajs.js.Dynamic].isContentEditable
        KeyPolicy.preventsPageScroll(
            key = key,
            tag = target.tagName,
            contentEditable = !scalajs.js.isUndefined(value) && value != null && value.asInstanceOf[Boolean]
        )
    end scrollKeyPrevented
    private val SvgNs = "http://www.w3.org/2000/svg"

    /** A live portal placeholder (`data-kyo-portal-slot="<path>"`) left at the logical position of an element the
      * client re-homed to document.body. Carries NO `data-kyo-path` of its own, so the body twin stays the document's
      * only carrier of the path and every path-addressed lookup keeps hitting the real element; [[logicalKey]] keys the
      * slot by the slot attribute so the sibling reconciliation still matches the incoming portal element against it.
      * Twin of `__kyoIsPortalSlot` in clientJs.
      */
    private def isPortalSlot(node: dom.Node): Boolean =
        node.nodeType == 1 && node.asInstanceOf[dom.Element].hasAttribute("data-kyo-portal-slot")

    /** An element that declared `portal(true)` (`data-kyo-portal`): rendered inline by the server, re-homed to
      * document.body by [[portalSweep]]. Twin of `__kyoIsPortal` in clientJs.
      */
    private def isPortalEl(node: dom.Node): Boolean =
        node.nodeType == 1 && node.asInstanceOf[dom.Element].hasAttribute("data-kyo-portal")

    /** The body twin of a portal slot: the (unique) re-homed element carrying `path`, a direct body child. */
    private def portalTwin(path: String): dom.Element =
        document.querySelector(s"""body > [data-kyo-path="$path"][data-kyo-portal]""")

    /** Portal upkeep after a patch (twin of `__kyoPortalSweep` in HtmlRenderer.clientJs; keep them in lockstep).
      *
      * ADOPT: every `data-kyo-portal` element still sitting inline under `root` (root included, so a freshly inserted
      * one is caught) is re-homed to document.body behind an inert placeholder stamped `data-kyo-portal-slot="<path>"`
      * at its logical position. Must run AFTER focus and enter seeding, both of which are subtree-scoped and need the
      * element inline. Reparenting drops DOM focus, so focus and caret held inside the moved subtree are captured and
      * re-applied after the move; the enter transition is unaffected, since its from-state classes release on the NEXT
      * frame.
      *
      * ORPHANS: a body twin whose placeholder is gone (its logical slot was removed or replaced by this patch) left
      * with its region, so its leave ghost is prepared, the twin removed, and the ghost spawned, in that order, since
      * [[spawnGhosts]] drops ghosts whose source is still connected. Document-wide by necessity: the twin sits outside
      * every region subtree, so the regular leave sweep cannot see it.
      */
    private def portalSweep(root: dom.Element): Unit =
        if root != null then
            val els = root.querySelectorAll("[data-kyo-portal]")
            val cand =
                (if root.hasAttribute("data-kyo-portal") then Seq(root) else Seq.empty) ++
                    (0 until els.length).map(els(_).asInstanceOf[dom.Element])
            cand.foreach { el =>
                val path = el.getAttribute("data-kyo-path")
                // Only path-carrying elements can portal (the placeholder must key the slot); the parent check keeps
                // the sweep idempotent, since an adopted twin is a direct body child and never under a region again.
                if path != null && el.parentNode != null && (el.parentNode ne document.body) then
                    // A stale twin from a lost placeholder would collide on the path: drop it first (defensive).
                    val stale = portalTwin(path)
                    if stale != null && (stale ne el) then discard(document.body.removeChild(stale))
                    val slot =
                        if el.namespaceURI == SvgNs then document.createElementNS(SvgNs, "g")
                        else document.createElement("span")
                    slot.setAttribute("data-kyo-portal-slot", path)
                    slot.setAttribute("hidden", "")
                    val ae                 = document.activeElement
                    val hadFocus           = ae != null && (ae ne document.body) && ((el eq ae) || el.contains(ae))
                    val (selStart, selEnd) = if hadFocus then readSelection(ae) else (Absent, Absent)
                    discard(el.parentNode.insertBefore(slot, el))
                    discard(document.body.appendChild(el))
                    if hadFocus then
                        focusNoScroll(ae)
                        (selStart, selEnd) match
                            case (Present(s), Present(e)) => setSelection(ae, s, e)
                            case _                        => ()
                    end if
                end if
            }
        end if
        val twins = document.querySelectorAll("body > [data-kyo-portal][data-kyo-path]")
        (0 until twins.length).foreach { i =>
            val twin = twins(i).asInstanceOf[dom.Element]
            val p    = twin.getAttribute("data-kyo-path")
            if document.querySelector(s"""[data-kyo-portal-slot="$p"]""") == null then
                val ghosts = prepareLeaveGhosts(twin, Set.empty)
                discard(document.body.removeChild(twin))
                spawnGhosts(ghosts)
            end if
        }
    end portalSweep

    // ---- pointer/drag delegation (SPA transport) ----

    // Drag-session state. Module-level mutable is safe on the single-threaded JS runtime (mutated only inside JS
    // event callbacks). A session is active between a pointerdown on a declaring element and its pointerup.
    private var ptrActive: Boolean             = false
    private var ptrEl: dom.Element             = null
    private var ptrPath: Seq[String]           = Seq.empty
    private var ptrRaf: Int                    = 0
    private var ptrPendingEv: dom.PointerEvent = null

    private def pointerPayload(el: dom.Element, ev: dom.PointerEvent): UI.PointerEvent =
        val r   = el.getBoundingClientRect()
        val tid = Maybe(ev.target.asInstanceOf[dom.Element].id).filter(_.nonEmpty)
        UI.PointerEvent(
            x = ev.clientX - r.left,
            y = ev.clientY - r.top,
            rectX = r.left,
            rectY = r.top,
            rectW = r.width,
            rectH = r.height,
            buttons = ev.buttons,
            targetId = tid
        )
    end pointerPayload

    private def setupPointerDelegation(
        dispatch: (Seq[String], UIEvent) => Boolean < Async,
        events: Channel[Unit < Async]
    )(using Frame): Unit < (Scope & Sync) =
        val down: scalajs.js.Function1[dom.Event, Unit] = (e0: dom.Event) =>
            val e = e0.asInstanceOf[dom.PointerEvent]
            findPathElement(e.target.asInstanceOf[dom.Element]).foreach { el =>
                if declaredInChain(el, "pointerdown") then
                    try
                        val d = el.asInstanceOf[scalajs.js.Dynamic]
                        if scalajs.js.typeOf(d.setPointerCapture) == "function" then discard(d.setPointerCapture(e.pointerId))
                    catch case _: Throwable => ()
                    end try
                    ptrActive = true
                    ptrEl = el
                    ptrPath = parsePath(el.getAttribute("data-kyo-path"))
                    fireFromJs(events, dispatch(ptrPath, UIEvent.PointerDown(ptrPath, pointerPayload(el, e))).unit)
                end if
            }

        val move: scalajs.js.Function1[dom.Event, Unit] = (e0: dom.Event) =>
            // Only stream during an active session; coalesce to at most one dispatch per animation frame.
            if ptrActive && ptrEl != null then
                ptrPendingEv = e0.asInstanceOf[dom.PointerEvent]
                if ptrRaf == 0 then
                    ptrRaf = dom.window.requestAnimationFrame { (_: Double) =>
                        ptrRaf = 0
                        if ptrActive && ptrEl != null && ptrPendingEv != null then
                            val ev = ptrPendingEv
                            ptrPendingEv = null
                            fireFromJs(events, dispatch(ptrPath, UIEvent.PointerMove(ptrPath, pointerPayload(ptrEl, ev))).unit)
                        end if
                    }
                end if

        val up: scalajs.js.Function1[dom.Event, Unit] = (e0: dom.Event) =>
            if ptrActive && ptrEl != null then
                val e = e0.asInstanceOf[dom.PointerEvent]
                try
                    val d = ptrEl.asInstanceOf[scalajs.js.Dynamic]
                    if scalajs.js.typeOf(d.releasePointerCapture) == "function" then discard(d.releasePointerCapture(e.pointerId))
                catch case _: Throwable => ()
                end try
                if ptrRaf != 0 then
                    dom.window.cancelAnimationFrame(ptrRaf)
                    ptrRaf = 0
                ptrPendingEv = null
                val el   = ptrEl
                val path = ptrPath
                ptrActive = false
                ptrEl = null
                ptrPath = Seq.empty
                fireFromJs(events, dispatch(path, UIEvent.PointerUp(path, pointerPayload(el, e))).unit)

        // Scoped like every other listener this mount installs. Registering them unscoped left three
        // capture-phase listeners on the body per mount cycle, each closing over the torn-down mount's
        // event channel.
        for
            _ <- addScopedListener("pointerdown", down, true)
            _ <- addScopedListener("pointermove", move, true)
            _ <- addScopedListener("pointerup", up, true)
        yield ()
        end for
    end setupPointerDelegation

end DomBackend
