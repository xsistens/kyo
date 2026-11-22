package kyo.internal

import DragSessions.*
import ReactiveUI.*
import kyo.*
import kyo.UI.*
import kyo.UI.Ast.*

private[kyo] object EventDispatch:

    private[internal] type Handler = (Seq[String], UIEvent) => Boolean < (Async & Abort[Any])

    private[internal] type ValidatedHandler = (Seq[String], DragProtocol.ValidatedEvent) => Boolean < Async

    /** Dispatch an event through an element. */
    private[internal] def dispatch(
        elem: Element,
        myPath: Seq[String],
        at: ReactiveRegion.RegionIdentity,
        targetPath: Seq[String],
        event: UIEvent,
        reactiveChildren: Seq[ReactiveUI],
        staticHandlers: Seq[(Int, Handler)],
        mounts: MountDispatch
    )(using Frame): Boolean < (Async & Abort[Any]) =
        elem.attrs.onFailure match
            case Present(rules) => Failures.within(FailureFrame.Rules(rules))(dispatchThrough(
                    elem,
                    myPath,
                    at,
                    targetPath,
                    event,
                    reactiveChildren,
                    staticHandlers,
                    mounts
                ))
            case Absent => dispatchThrough(elem, myPath, at, targetPath, event, reactiveChildren, staticHandlers, mounts)

    /** [[dispatch]] with the element's `onFailure` rules in effect. A handler's failure goes to the rules and the dispatch goes on. */
    private def dispatchThrough(
        elem: Element,
        myPath: Seq[String],
        at: ReactiveRegion.RegionIdentity,
        targetPath: Seq[String],
        event: UIEvent,
        reactiveChildren: Seq[ReactiveUI],
        staticHandlers: Seq[(Int, Handler)],
        mounts: MountDispatch
    )(using Frame): Boolean < (Async & Abort[Any]) =
        if targetPath == myPath then
            Failures.recover(true)(dispatchToElement(elem, event, isTarget = true, selfPath = myPath))
        else if targetPath.startsWith(myPath) && targetPath.size > myPath.size then
            val childSegment = targetPath(myPath.size)
            val childIdx     = childSegment.toIntOption

            // Find the reactive child whose path is a prefix of (or equals) targetPath. Matching by
            // `lastOption.contains(childSegment)` is incorrect when reactive children are nested
            // multiple levels deep; their last segment may collide with a sibling's index at the
            // current level. Using prefix-match correctly routes only when the target lies inside.
            val reactiveChild = reactiveChildren.find(rc => targetPath.startsWith(rc.path))
            // `reactiveChildren` also holds reactive descendants of static children. A target inside a static child goes
            // through that child's handler chain, so an element between this one and the region (a button around a
            // reactive label) still sees the event; only a reactive DIRECT child, which has no static handler, is entered
            // directly.
            val staticChild = Maybe.fromOption(childIdx).flatMap(i => Maybe.fromOption(staticHandlers.find(_._1 == i)).map(_._2))

            val isClick   = event.isInstanceOf[UIEvent.Click]
            val isKeyDown = event.isInstanceOf[UIEvent.KeyDown]
            for
                // Disabled-target (Form submit suppression), Button-target (only Button clicks submit
                // forms), Select-target (Enter on Select must not submit) and control-below all resolve
                // through any Reactive/Foreach boundary wrapping the target, so signal-typed setters do
                // not hide it. All four come out of one resolution: they are four questions about one
                // element, not four searches for it.
                facts <-
                    if isClick || isKeyDown then targetFacts(elem, myPath, at, targetPath, mounts)
                    else Kyo.lift(TargetFacts.none)
                // "Is it disabled" is a question about the current value behind a channel, not about a
                // field (see boolAttrNow), so it is read from the resolved element rather than carried.
                targetDisabled <-
                    if isClick then facts.target.fold(Kyo.lift(false))(isDisabled) else Kyo.lift(false)
                // A button's type decides whether activating it submits, not its element kind:
                // `type="button"` submits nothing in a browser (see ButtonActivation). `Absent` here
                // means the target is no button at all, which is not the same as "a button that does
                // not submit".
                targetButtonSubmits = facts.target match
                    case Present(b: Button) => Present(ButtonActivation.submits(b.attrs.jsProps.getOrElse("type", "")))
                    case _                  => Absent
                // Enter submits a form implicitly from a single-line field or a button, as a browser does; from a textarea it is a
                // line break, from a select a dropdown interaction, and from anything else nothing at all.
                targetEnterSubmits = isKeyDown && facts.target.exists { t =>
                    val tag = HtmlRenderer.tagName(t)
                    tag == "input" || tag == "button"
                }
                // Whether a control of the reader's own stands between this element and the target,
                // reported to the handler rather than acted on here: only the handler knows whether
                // its click means something the control already means (see UI.MouseEvent.onControl).
                targetOnControl = isClick && facts.controlBelow
                bubble          = Failures.recover(true)(dispatchToElement(
                    elem,
                    event,
                    isTarget = false,
                    selfPath = myPath,
                    disabledTarget = targetDisabled,
                    submitOrigin = isClick && targetButtonSubmits.getOrElse(false),
                    enterSubmits = targetEnterSubmits,
                    controlTarget = targetOnControl
                ))
                result <- staticChild match
                    case Present(childHandle) =>
                        safeDispatch(childHandle, targetPath, event).map(keep => if keep then bubble else false)
                    case Absent =>
                        reactiveChild match
                            case Some(child) =>
                                safeDispatch(child.handle, targetPath, event).map(keep => if keep then bubble else false)
                            // Every child this element holds has a static handler or is a reactive child at its own path, so a
                            // target naming neither is one that has left the page since the reader acted on it (a second click
                            // on an option its own pick just closed away). Its handlers went with it, and this element's are no
                            // stand-in: a field would read the option's click as its own and open the panel again.
                            case None => Kyo.lift(false)
            yield result
            end for
        else
            true
        end if
    end dispatchThrough

    /** Where a keydown records the element whose click it stands for, read by [[subscribe]]'s wrapper once
      * the keydown has finished bubbling.
      *
      * HTML has exactly one route to a form submit: an activation behaviour, which runs as part of a
      * click. Keyboard activation of a button is a click the browser fires, and Enter in a field is
      * implicit submission, which the specification also defines as firing a click, at the form's
      * default button. Neither is "submit on a key". Recording a path here and dispatching a real click
      * at it afterwards keeps activation on that one path: an ancestor sees a keyboard activation
      * exactly as it sees a mouse click, and a form submits from that click.
      */
    private[internal] val activationTarget: Local[Maybe[AtomicRef[Maybe[Seq[String]]]]] = Local.init(Absent)

    /** Record that this keydown's activation is a click at `path`, unless one is already recorded.
      *
      * First writer wins, and the order is the bubble order: the target runs before its ancestors, so a
      * button's own activation is what a form sees rather than the form's implicit submission.
      */
    private def recordActivation(path: Seq[String])(using Frame): Unit < Async =
        activationTarget.use {
            case Present(ref) => ref.updateAndGet(existing => if existing.isDefined then existing else Present(path)).unit
            case Absent       => Kyo.lift(())
        }

    /** The form's default button: the first button in tree order whose `type` submits, as HTML defines it.
      *
      * Only the statically known children are walked. A button behind a signal is not found, and the
      * caller falls back to submitting the form directly, the answer HTML gives a form with no submit
      * button at all.
      */
    private def defaultSubmitButton(elem: Element, basePath: Seq[String]): Maybe[Seq[String]] =
        val start: Maybe[Seq[String]] = Absent
        elem.children.toSeq.zipWithIndex.foldLeft(start) { case (found, (child, i)) =>
            if found.isDefined then found
            else
                val childPath = basePath :+ i.toString
                child match
                    case b: Button if ButtonActivation.submits(b.attrs.jsProps.getOrElse("type", "")) =>
                        Present(childPath)
                    case c: Element => defaultSubmitButton(c, childPath)
                    case _          => Absent
                end match
        }
    end defaultSubmitButton

    /** Submit the form itself, HTML's fallback when implicit submission finds no default button to click.
      *
      * Skipped when the keydown already recorded an activation: that click reaches this form on its own
      * and submitting here as well would be the double all over again.
      */
    private def submitDirectly(f: Form)(using Frame): Unit < (Async & Abort[Any]) =
        activationTarget.use {
            case Present(ref) => ref.get.map(recorded => if recorded.isDefined then Kyo.lift(()) else invoke(f.onSubmit))
            case Absent       => invoke(f.onSubmit)
        }

    /** Invoke a Maybe[UI.Action] handler, discarding the result. */
    private def invoke(handler: Maybe[UI.Action])(using Frame): Unit < (Async & Abort[Any]) =
        if handler.isEmpty then () else handler.get.unit

    /** Invoke a Maybe[A => UI.Action] handler with a value, discarding the result. */
    private def invokeWith[A](handler: Maybe[A => UI.Action], value: A)(using Frame): Unit < (Async & Abort[Any]) =
        if handler.isEmpty then () else handler.get(value).unit

    /** Dispatch an event safely, logging and recovering from handler errors without stopping the bubble chain. */
    private[internal] def safeDispatch(handle: Handler, path: Seq[String], event: UIEvent)(using
        Frame
    ): Boolean < Async =
        DragCommands.current.use { context =>
            val decisions = context.flatMap(_.decisions)
            Abort.recover[Any](
                onFail = err =>
                    Log.error(s"Handler error during ${event.getClass.getSimpleName} at ${path.mkString(".")}: $err")
                        .andThen(markDecisionFailure(event, decisions))
                        .andThen(true), // continue bubbling
                onPanic = thr =>
                    Log.error(s"Handler panic during ${event.getClass.getSimpleName} at ${path.mkString(".")}: ${thr.getMessage}")
                        .andThen(markDecisionFailure(event, decisions))
                        .andThen(true) // continue bubbling
            )(handle(path, event))
        }

    private def markDecisionFailure(event: UIEvent, decisions: Maybe[AtomicRef[DragCommands.DecisionState]])(using
        Frame
    ): Unit < Async =
        event match
            case _: UIEvent.Drop     => failDecision(decisions, "The drop handler failed.")
            case _: UIEvent.SortMove => failDecision(decisions, "The sort handler failed.")
            case _                   => ()

    /** Read current checked value from Maybe[Bound[Boolean]]. */
    private def readChecked(checked: Maybe[Bound[Boolean]])(using Frame): Boolean < Sync =
        checked match
            case Present(Bound.Const(b))    => b
            case Present(Bound.Ref(ref))    => ref.get
            case Present(Bound.Follow(sig)) => sig.current
            case _                          => false

    /** Toggle checked and auto-set SignalRef if bound. Returns the new value. */
    private def toggleChecked(checked: Maybe[Bound[Boolean]])(using Frame): Boolean < Sync =
        checked match
            case Present(Bound.Ref(ref)) =>
                ref.getAndUpdate(!_).map(!_)
            case Present(Bound.Const(b))    => !b
            case Present(Bound.Follow(sig)) => sig.current.map(!_)
            case _                          => true

    /** Cycle a Select to the next or previous option. Updates SignalRef and fires onChange. */
    private def cycleSelectOption(sel: Select, forward: Boolean)(using Frame): Unit < (Async & Abort[Any]) =
        val options = sel.children.toSeq.collect { case opt: Opt => opt }
        val values  = options.flatMap(_.value.toList)
        if values.isEmpty then Kyo.lift(())
        else
            for
                current <- sel.value match
                    case Present(Bound.Ref(ref))    => ref.get
                    case Present(Bound.Const(s))    => Kyo.lift(s)
                    case Present(Bound.Follow(sig)) => sig.current
                    case _                          => Kyo.lift("")
                currentIdx = values.indexOf(current).max(0)
                nextIdx    =
                    if forward then (currentIdx + 1)    % values.size
                    else (currentIdx - 1 + values.size) % values.size
                newValue = values(nextIdx)
                _ <- sel.value match
                    case Present(Bound.Ref(ref)) => ref.set(newValue)
                    case _                       => Kyo.lift(())
                _ <- invokeWith(sel.onChange, newValue)
            yield ()
        end if
    end cycleSelectOption

    private def formatDouble(v: Double): String = NumberFormat.double(v)

    /** Is the boolean attribute `name` set on `elem` RIGHT NOW, statically or through the reactive channel a
      * `Signal`-typed setter installs.
      *
      * `.disabled(Signal)` and `.readOnly(Signal)` do not wrap their element and do not fill the static field:
      * they register a boolean-attribute channel (`Attrs.reactiveBoolAttrs`) that patches the live attribute
      * in place. A guard reading only the static field therefore counts every signal-disabled element as
      * enabled, and silently, because a browser suppresses the click on a rendered `disabled` button, so it shows
      * only where dispatch decides for itself: the server transport, and any forged event.
      *
      * WHERE A CHANNEL EXISTS, THE CHANNEL IS THE VALUE: the static field is not consulted at all. Same rule
      * the renderer follows (`renderElementAttrs`), and the only one consistent with the client, whose channel
      * patch sets and REMOVES the attribute on every emission whatever the initial HTML said.
      */
    private def boolAttrNow(elem: Element, name: String, static: => Boolean)(using Frame): Boolean < Sync =
        elem.attrs.reactiveBoolAttrs.get(name) match
            case Some(sig) => sig.current
            case None      => static

    /** Check if element is disabled. */
    private def isDisabled(elem: Element)(using Frame): Boolean < Sync =
        boolAttrNow(
            elem,
            "disabled",
            elem match
                case hd: HasDisabled => hd.disabled.getOrElse(false)
                case _               => false
        )

    /** Check if element is readOnly. The channel name is the ATTRIBUTE spelling (`readonly`), not the setter's. */
    private def isReadOnly(elem: Element)(using Frame): Boolean < Sync =
        boolAttrNow(
            elem,
            "readonly",
            elem match
                case ti: TextInput => ti.readOnly.getOrElse(false)
                case _             => false
        )

    /** A TARGET that is inert (disabled, hidden, read-only) ignores its own handler but still lets the event
      * bubble, so this yields `true` (keep bubbling) without running `body`. Only the target is guarded: an
      * ancestor on the bubble path is not inert just because the target was.
      */
    private def unlessInert(isTarget: Boolean, inert: => Boolean < Sync)(body: => Boolean < (Async & Abort[Any]))(using
        Frame
    ): Boolean < (Async & Abort[Any]) =
        if !isTarget then body
        else inert.map(i => if i then true else body)

    /** Check if element is hidden. Reads the channel too: `.hidden(Signal)` installs a boolean-attribute
      * channel exactly like `.disabled(Signal)`, so the static field is empty whenever the signal form was used.
      */
    private def isHidden(elem: Element)(using Frame): Boolean < Sync =
        boolAttrNow(elem, "hidden", elem.attrs.hidden.getOrElse(false))

    /** What a bubbling dispatch needs to know about the event's target, resolved once.
      *
      * @param target
      *   the concrete element the path ends at, `Absent` when it does not resolve
      * @param controlBelow
      *   whether any element strictly below the dispatching one, the target included, is a control of
      *   the reader's own
      */
    final private case class TargetFacts(target: Maybe[Element], controlBelow: Boolean)

    private object TargetFacts:
        /** Nothing was asked, so nothing was found: the answer for an event that resolves no target. */
        val none: TargetFacts = TargetFacts(Absent, false)

    /** Resolve the (possibly reactive) node at `targetPath` to the concrete element there, noting on the way
      * whether the path passed through a control.
      *
      * Mirrors the renderer's path scheme: element children are index-addressed, a `Reactive`'s rendered content occupies the same path as
      * the boundary, `Foreach` items live at `path :+ key` (or `:+ index`), and `Fragment` children at `path :+ index`. Resolving through
      * these boundaries is what lets an element wrapped by a signal-typed setter (e.g. `.hidden(Signal)`, which wraps it in a `Reactive`)
      * still be recognized by its concrete type, so button-click form submit and disabled detection keep working through such wrappers.
      *
      * One walk answers every question a click asks. A walk is not free: resolving through a `Foreach`
      * renders the target row to reach it, so a walk per question would render the same row once per
      * question on every click.
      *
      * `controlBelow` is accumulated rather than tested at the leaf because it is a question about the
      * whole chain: a click on the icon inside a button targets the icon, and the icon alone cannot say
      * that a control was involved. `rootDepth` is what makes it strictly below: the dispatching element
      * never answers for itself, or a button would decline its own click.
      */
    private def resolveTarget(
        node: UI,
        nodePath: Seq[String],
        at: ReactiveRegion.RegionIdentity,
        targetPath: Seq[String],
        rootDepth: Int,
        controlBelow: Boolean,
        mounts: MountDispatch
    )(using
        Frame
    ): TargetFacts < Sync =
        // A path that stops short still answers the control question: a control above a boundary the
        // resolver cannot see through (a mount, a text leaf) counts, so the flag is carried out.
        def unresolved(seen: Boolean): TargetFacts = TargetFacts(Absent, seen)
        node match
            case kc: KeyedChild[?] =>
                resolveTarget(kc.child, nodePath, at, targetPath, rootDepth, controlBelow, mounts)
            case r: Reactive[?] =>
                // A Reactive's rendered content occupies the same path as the boundary, so re-resolve at nodePath.
                r.signal.current(using r.frame).map(cur =>
                    resolveTarget(cur, nodePath, at.transparent, targetPath, rootDepth, controlBelow, mounts)
                )
            case Fragment(children) =>
                if targetPath.size <= nodePath.size then unresolved(controlBelow)
                else
                    val seg = targetPath(nodePath.size)
                    Maybe.fromOption(seg.toIntOption) match
                        case Present(i) if i >= 0 && i < children.size =>
                            resolveTarget(children(i), nodePath :+ seg, at.child(seg), targetPath, rootDepth, controlBelow, mounts)
                        case _ => unresolved(controlBelow)
                    end match
            case fe: Foreach[?, ?] @unchecked =>
                if targetPath.size <= nodePath.size then unresolved(controlBelow)
                else
                    val seg = targetPath(nodePath.size)
                    fe.applyTyped {
                        [T] => (signal, keyFn, renderFn) =>
                            signal.current(using fe.frame).map { items =>
                                val idx = keyFn match
                                    case Present(f) => items.indexWhere(it => f(it) == seg)
                                    case Absent     => Maybe.fromOption(seg.toIntOption).getOrElse(-1)
                                if idx >= 0 && idx < items.size then
                                    // A row's key is part of the ids of the keyed nodes in it.
                                    DrawContext.scope.use(s =>
                                        DrawContext.scope.let(s.copy(rows = s.rows.prepended(seg)))(resolveTarget(
                                            renderFn(idx, items(idx)),
                                            nodePath :+ seg,
                                            at.child(seg),
                                            targetPath,
                                            rootDepth,
                                            controlBelow,
                                            mounts
                                        ))
                                    )
                                else unresolved(controlBelow)
                                end if
                            }
                    }
            case e: Element =>
                val seen = controlBelow || (nodePath.size > rootDepth && isOwnControl(e))
                if targetPath.size <= nodePath.size then TargetFacts(Present(e), seen)
                else
                    val seg = targetPath(nodePath.size)
                    Maybe.fromOption(seg.toIntOption) match
                        case Present(i) if i >= 0 && i < e.children.size =>
                            resolveTarget(e.children(i), nodePath :+ seg, at.child(seg), targetPath, rootDepth, seen, mounts)
                        case _ => unresolved(seen)
                    end match
                end if
            // Text, RawHtml and StyleElement are leaf content with no kyo-addressable Element children, so no event
            // target can resolve through them.
            case _: Text | _: RawHtml | _: StyleElement => unresolved(controlBelow)
            // A mount's content lives with its instance, found through the dispatch table like the node's handler finds it, and
            // occupies the node's path. An inert snapshot resolves to nothing, since no event reaches it.
            case m: Mounted if Boundaries.implicitOf(m).nonEmpty =>
                resolveTarget(Boundaries.implicitOf(m).get, nodePath, at, targetPath, rootDepth, controlBelow, mounts)
            case m: Mounted =>
                mounts.instanceForEvent(at.transparent, m).map {
                    case Absent            => unresolved(controlBelow)
                    case Present(instance) =>
                        instance.cell.current.map {
                            case MountState.Pending(Present(_)) => unresolved(controlBelow)
                            case state                          =>
                                val content = state.content(MountedSpec(m, nodePath, Chunk.empty))
                                DrawContext.scope.let(DrawContext.IdScope(instance.children.idBase, Chunk.empty))(
                                    resolveTarget(content, nodePath, at.transparent, targetPath, rootDepth, controlBelow, mounts)
                                )
                        }
                }
            // A drawn mount's content is on the page from the first paint, before an instance has taken it.
            case d: Drawn =>
                mounts.instanceForEvent(at.transparent, d).map {
                    case Present(instance) =>
                        instance.cell.current.map(state =>
                            DrawContext.scope.let(DrawContext.IdScope(instance.children.idBase, Chunk.empty))(resolveTarget(
                                state.content(MountedSpec(d, nodePath, Chunk.empty)),
                                nodePath,
                                at.transparent,
                                targetPath,
                                rootDepth,
                                controlBelow,
                                mounts
                            ))
                        )
                    case Absent =>
                        // Outside any walk, so the order of the call in its range comes from the walk that last met it there.
                        DrawContext.scope.use(scope =>
                            Sync.Unsafe.defer(mounts.occurrenceAt(at.transparent, d)).map(mounts.drawn(at.transparent, d, scope, _))
                        ).map((content, base) =>
                            DrawContext.scope.let(DrawContext.IdScope(base, Chunk.empty))(
                                resolveTarget(content, nodePath, at.transparent, targetPath, rootDepth, controlBelow, mounts)
                            )
                        )
                }
            // Held-back content is not on the page; once revealed it occupies the boundary's path.
            case b: Bounded =>
                mounts.boundary(at.transparent).map {
                    case Present(gate) if !gate.isRevealed =>
                        gate.livePlaceholder.fold(Kyo.lift(unresolved(controlBelow)))(ph =>
                            resolveTarget(ph, nodePath, at.transparent, targetPath, rootDepth, controlBelow, mounts)
                        )
                    case _ =>
                        mounts.held(targetPath).map(held =>
                            if held then unresolved(controlBelow)
                            else resolveTarget(b.child, nodePath, at.transparent, targetPath, rootDepth, controlBelow, mounts)
                        )
                }
        end match
    end resolveTarget

    /** The facts a bubbling dispatch reads about its target, in a single resolution. */
    private def targetFacts(
        elem: Element,
        myPath: Seq[String],
        at: ReactiveRegion.RegionIdentity,
        targetPath: Seq[String],
        mounts: MountDispatch
    )(using
        Frame
    ): TargetFacts < Sync =
        resolveTarget(elem, myPath, at, targetPath, myPath.size, false, mounts)

    /** Whether an element is a control in its own right, something the reader operates directly, as
      * opposed to markup an ancestor made clickable.
      *
      * `Focusable` is the set: every input flavour, `Textarea`, `Select`, the checkbox/radio pair,
      * `Button` and `Anchor`. An anchor qualifies only when it actually goes somewhere or does
      * something, since `a` with neither an href nor a handler is inert markup that happens to be in
      * the hierarchy.
      */
    private def isOwnControl(e: Element): Boolean = e match
        case a: Anchor    => a.href.isDefined || a.attrs.onClick.nonEmpty || a.attrs.onClickEvt.nonEmpty
        case _: Focusable => true
        case _            => false

    /** Bubble-continue value after an element handled `event`: `false` (consume) only when the element set
      * `stopPropagation(true)` AND actually declared a handler for this event's type (`declared`). The result flows up the
      * dispatch recursion, so a `false` makes every element ABOVE this one skip its own handler.
      */
    private def keepBubbling(elem: Element, declared: Boolean): Boolean =
        !(declared && elem.attrs.stopPropagation.getOrElse(false))

    /** Where an edit that reached an element following a signal records what puts it back, read by [[ReactiveUI.subscribe]]'s wrapper once
      * the event has finished bubbling, and after a handler that failed too, since a failure is recovered on the way.
      */
    private[internal] val followEdit: Local[Maybe[AtomicRef[Maybe[UIExchange => Unit < Async]]]] = Local.init(Absent)

    private def recordEdit(elem: Element, event: UIEvent, isTarget: Boolean, path: Seq[String])(using Frame): Unit < Sync =
        if !isTarget || !Channels.follows(elem) then Kyo.unit
        else
            followEdit.use {
                case Present(ref) => Channels.putBack(elem, event, path).fold(Kyo.unit)(send => ref.set(Present(send)))
                case Absent       => Kyo.unit
            }

    /** Dispatch event to element. isTarget=true when element is the click target, false on bubble. disabledTarget=true when the original
      * click target was disabled (prevents Form onSubmit on bubble).
      *
      * `submitOrigin` is the only question left about whether the button under the event submits, and
      * it is asked on the click path alone. Keyboard activation reaches that same path as a synthesized
      * click (see [[activationTarget]]), so there is nothing for the keydown path to decide.
      *
      * `selfPath` is this element's own path, needed because a keydown records where its activation
      * click belongs rather than acting on it here.
      */
    private def dispatchToElement(
        elem: Element,
        event: UIEvent,
        isTarget: Boolean,
        selfPath: Seq[String] = Seq.empty,
        disabledTarget: Boolean = false,
        submitOrigin: Boolean = false,
        enterSubmits: Boolean = false,
        controlTarget: Boolean = false
    )(
        using Frame
    ): Boolean < (Async & Abort[Any]) =
        val attrs = elem.attrs
        event match
            case ev: UIEvent.Click =>
                // Disabled or hidden elements ignore their own click handler, but allow bubbling
                unlessInert(isTarget, isDisabled(elem).map(d => if d then true else isHidden(elem))) {
                    val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position, controlTarget)
                    // The target element is the nearest one that carries a path, so `isTarget` alone would also hold
                    // for a click on a plain descendant; `self` is the client's word that it was this element.
                    val onSelf = isTarget && ev.mouse.self
                    val self   = if onSelf then
                        invoke(attrs.onClickSelf).andThen(invokeWith(attrs.onClickSelfEvt, mouse))
                    else Kyo.lift(())
                    // Checkbox/radio toggle is handled by UIControlSession.click() which dispatches
                    // ChangeChecked after Click. Don't toggle here to avoid double-toggle.
                    val activateToggle = Kyo.lift(())
                    // When a Click on a submitting Button bubbles to a Form, trigger onSubmit (browser
                    // behavior). Not every Button: `type="button"` and `type="reset"` activate without
                    // submitting (see ButtonActivation).
                    val formSubmit = if !isTarget && !disabledTarget && submitOrigin then
                        elem match
                            case f: Form =>
                                invoke(f.onSubmit).andThen(invokeWith(f.onSubmitEvt, mouse))
                            case _ => Kyo.lift(())
                    else Kyo.lift(())
                    // Self handlers only count as "declared" when they actually fired.
                    val declared = attrs.onClick.nonEmpty || attrs.onClickEvt.nonEmpty ||
                        (onSelf && (attrs.onClickSelf.nonEmpty || attrs.onClickSelfEvt.nonEmpty))
                    self
                        .andThen(invokeWith(attrs.onClickEvt, mouse))
                        .andThen(invoke(attrs.onClick))
                        .andThen(activateToggle)
                        .andThen(formSubmit)
                        .andThen(keepBubbling(elem, declared))
                }
            case ev: UIEvent.ContextMenu =>
                // Mirrors Click: a disabled or hidden target skips its own handler but still bubbles.
                unlessInert(isTarget, isDisabled(elem).map(d => if d then true else isHidden(elem))) {
                    val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position)
                    invoke(attrs.onContextMenu)
                        .andThen(invokeWith(attrs.onContextMenuEvt, mouse))
                        .andThen(keepBubbling(elem, attrs.onContextMenu.nonEmpty || attrs.onContextMenuEvt.nonEmpty))
                }
            case _: UIEvent.Attach =>
                if isTarget && attrs.onAttached.nonEmpty then AttachSite.start(attrs.onAttached.get).andThen(true)
                else true
            case ev: UIEvent.Focus =>
                val isFocusable = elem.isInstanceOf[Focusable] || elem.attrs.tabIndex.nonEmpty
                if isTarget && isFocusable then
                    val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position)
                    invoke(attrs.onFocus).andThen(invokeWith(attrs.onFocusEvt, mouse)).andThen(true)
                else if isTarget then Kyo.lift(false) // not focusable; reject
                else true
                end if
            case ev: UIEvent.Blur =>
                if isTarget then
                    val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position)
                    // A text field's blur carries the text the client read off it; one that carries none (a transport
                    // with no element to read) reports what the field holds instead.
                    val text: Unit < (Async & Abort[Any]) = elem match
                        case ti: TextInput if ti.onBlurText.nonEmpty =>
                            val held: String < Sync = ev.value match
                                case Present(v) => v
                                case Absent     =>
                                    ti.value match
                                        case Present(Bound.Ref(ref))    => ref.get
                                        case Present(Bound.Const(v))    => v
                                        case Present(Bound.Follow(sig)) => sig.current
                                        case _                          => ""
                            held.map(v => ti.onBlurText.get(mouse, v).unit)
                        case _ => ()
                    invoke(attrs.onBlur).andThen(invokeWith(attrs.onBlurEvt, mouse)).andThen(text).andThen(true)
                else true
            case e: UIEvent.KeyDown =>
                unlessInert(isTarget, isDisabled(elem)) {
                    val kbEvent = UI.KeyboardEvent(
                        key = Keyboard.fromString(e.keyboard.key),
                        modifiers = e.keyboard.modifiers,
                        targetId = e.keyboard.targetId
                    )
                    // The self handler runs first, as onClickSelf does, and only for a key on this element itself.
                    val onSelf     = isTarget && e.keyboard.self
                    val selfKey    = if onSelf then invokeWith(attrs.onKeyDownSelf, kbEvent) else Kyo.lift(())
                    val keyHandler = selfKey.andThen(invokeWith(attrs.onKeyDown, kbEvent))
                    // Keyboard activation, emulated here so a component behaves the same where no browser
                    // runs it (the TUI, the pure tests). The browser's own activation is suppressed by
                    // KeyPolicy.doubleActivates, so exactly one of the two reaches the handler.
                    //
                    // A button takes Enter and Space; an anchor takes Enter alone. That is not a
                    // simplification of the browser but the browser: Space with a link focused scrolls
                    // the page, it does not follow the link, and the ARIA link pattern says the same.
                    // An element that says it is a button and takes a click takes both keys, an anchor
                    // among them, as ARIA asks of it and no browser does (KeyPolicy.activatesAsButton).
                    val activateClick = if isTarget && KeyPolicy.activatesButton(e.keyboard.key) then
                        val clicks = attrs.onClick.nonEmpty || attrs.onClickEvt.nonEmpty ||
                            attrs.onClickSelf.nonEmpty || attrs.onClickSelfEvt.nonEmpty
                        elem match
                            case _: Button => recordActivation(selfPath)
                            case _ if KeyPolicy.activatesAsButton(e.keyboard.key, clicks, attrs.role.getOrElse("")) =>
                                recordActivation(selfPath)
                            case _: Anchor if KeyPolicy.activatesLink(e.keyboard.key) => recordActivation(selfPath)
                            case _                                                    => Kyo.lift(())
                        end match
                    else Kyo.lift(())
                    // Enter/Space on checkbox/radio triggers onChange. Space is the browser's; Enter is
                    // kyo's own, kept deliberately so a checkbox answers the same key in the TUI, where
                    // Enter is the one activation key a terminal reliably delivers. The client shim
                    // synthesizes the same Enter click for browser parity (see HtmlRenderer's keydown
                    // branch), so both transports agree.
                    val activateToggle = if isTarget && (e.keyboard.key == "Enter" || e.keyboard.key == " ") then
                        elem match
                            case cb: Checkbox =>
                                toggleChecked(cb.checked).map(newVal => invokeWith(cb.onChange, newVal))
                            case rb: Radio =>
                                val autoSet = rb.checked match
                                    case Present(Bound.Ref(ref)) => ref.set(true)
                                    case _                       => Kyo.lift(())
                                autoSet.andThen(invokeWith(rb.onChange, true))
                            case _ => Kyo.lift(())
                    else Kyo.lift(())
                    // ArrowDown/ArrowUp/Space/Enter on Select cycles through options (browser behavior)
                    val selectCycle =
                        if isTarget &&
                            (e.keyboard.key == "ArrowDown" || e.keyboard.key == "ArrowUp" || e.keyboard.key == " " ||
                                e.keyboard.key == "Enter")
                        then
                            elem match
                                case sel: Select => cycleSelectOption(sel, forward = e.keyboard.key != "ArrowUp")
                                case _           => Kyo.lift(())
                        else Kyo.lift(())
                    // Implicit submission: HTML's rule for Enter in a field, and the only thing a keydown
                    // does about submitting. The specification fires a click at the form's default
                    // button, so that is what gets recorded; the form's own onSubmit runs from that
                    // click, through the same `submitOrigin` a mouse click uses. A form with no
                    // reachable submit button has no click to fire, and HTML submits it directly.
                    //
                    // Nothing is recorded when the target already recorded its own activation (a
                    // button's Enter is that button's click, not the form's implicit submission), and
                    // only a target that submits on Enter submits at all (see `enterSubmits` at the dispatch site).
                    val formSubmit =
                        if !isTarget && KeyPolicy.submitsImplicitly(e.keyboard.key) && enterSubmits then
                            elem match
                                case f: Form =>
                                    defaultSubmitButton(f, selfPath) match
                                        case Present(buttonPath) => recordActivation(buttonPath)
                                        case Absent              => submitDirectly(f)
                                case _ => Kyo.lift(())
                        else Kyo.lift(())
                    keyHandler.andThen(activateClick).andThen(activateToggle).andThen(selectCycle).andThen(formSubmit)
                        .andThen(keepBubbling(elem, attrs.onKeyDown.nonEmpty || (onSelf && attrs.onKeyDownSelf.nonEmpty)))
                }
            case e: UIEvent.KeyUp =>
                val kbEvent = UI.KeyboardEvent(
                    key = Keyboard.fromString(e.keyboard.key),
                    modifiers = e.keyboard.modifiers,
                    targetId = e.keyboard.targetId
                )
                invokeWith(attrs.onKeyUp, kbEvent).andThen(keepBubbling(elem, attrs.onKeyUp.nonEmpty))
            case e: UIEvent.Input =>
                unlessInert(isTarget, isDisabled(elem).map(d => if d then true else isReadOnly(elem))) {
                    recordEdit(elem, e, isTarget, selfPath).andThen(elem match
                        case ti: TextInput =>
                            val autoSet = ti.value match
                                case Present(Bound.Ref(ref)) => ref.set(e.value)
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(ti.onInput, e.value)).andThen(true)
                        case _ => true)
                }
            case e: UIEvent.Change =>
                unlessInert(isTarget, isDisabled(elem).map(d => if d then true else isReadOnly(elem))) {
                    recordEdit(elem, e, isTarget, selfPath).andThen(elem match
                        case ti: TextInput =>
                            val autoSet = ti.value match
                                case Present(Bound.Ref(ref)) => ref.set(e.value)
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(ti.onChange, e.value)).andThen(true)
                        case dd: Dropdown =>
                            val autoSet = dd.value match
                                case Present(Bound.Ref(ref)) => ref.set(e.value)
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(dd.onChange, e.value)).andThen(true)
                        case pi: PickerInput =>
                            val autoSet = pi.value match
                                case Present(Bound.Ref(ref)) => ref.set(e.value)
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(pi.onChange, e.value)).andThen(true)
                        case fi: FileInput => invokeWith(fi.onChange, e.value).andThen(true)
                        case _             => true)
                }
            case e: UIEvent.FileSelect =>
                // Bubbles like Change: the files ride the payload, nothing is read from the element here.
                unlessInert(isTarget, isDisabled(elem)) {
                    invokeWith(attrs.onFileSelect, e.files).andThen(keepBubbling(elem, attrs.onFileSelect.nonEmpty))
                }
            case e: UIEvent.ChangeChecked =>
                unlessInert(isTarget, isDisabled(elem)) {
                    recordEdit(elem, e, isTarget, selfPath).andThen(elem match
                        case bi: BooleanInput =>
                            val autoSet = bi.checked match
                                case Present(Bound.Ref(ref)) => ref.set(e.checked)
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(bi.onChange, e.checked)).andThen(true)
                        case _ => true)
                }
            case e: UIEvent.ChangeNumeric =>
                unlessInert(isTarget, isDisabled(elem)) {
                    recordEdit(elem, e, isTarget, selfPath).andThen(elem match
                        case ni: NumberInput =>
                            val autoSet = ni.value match
                                case Present(Bound.Ref(ref)) => ref.set(formatDouble(e.value))
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(ni.onChangeNumeric, e.value)).andThen(true)
                        case ri: RangeInput =>
                            val autoSet = ri.value match
                                case Present(Bound.Ref(ref)) => ref.set(e.value)
                                case _                       => Kyo.lift(())
                            autoSet.andThen(invokeWith(ri.onChange, e.value)).andThen(true)
                        case _ => true)
                }
            case ev: UIEvent.Submit =>
                elem match
                    case f: Form =>
                        val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position)
                        invoke(f.onSubmit).andThen(invokeWith(f.onSubmitEvt, mouse)).andThen(true)
                    case _ => true
            case ev: UIEvent.Hover =>
                val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position)
                invoke(attrs.onHover).andThen(invokeWith(attrs.onHoverEvt, mouse))
                    .andThen(keepBubbling(elem, attrs.onHover.nonEmpty || attrs.onHoverEvt.nonEmpty))
            case ev: UIEvent.Unhover =>
                val mouse = UI.MouseEvent(ev.mouse.targetId, ev.mouse.modifiers, ev.mouse.position)
                invoke(attrs.onUnhover).andThen(invokeWith(attrs.onUnhoverEvt, mouse))
                    .andThen(keepBubbling(elem, attrs.onUnhover.nonEmpty || attrs.onUnhoverEvt.nonEmpty))
            case ev: UIEvent.Scroll =>
                val wheel = UI.WheelEvent(ev.deltaX, ev.deltaY, ev.targetId, ev.modifiers)
                invoke(attrs.onScroll).andThen(invokeWith(attrs.onScrollEvt, wheel))
                    .andThen(keepBubbling(elem, attrs.onScroll.nonEmpty || attrs.onScrollEvt.nonEmpty))
            case _: UIEvent.DragStart =>
                if !isTarget then true
                else
                    DragCommands.current.use {
                        case Present(DragCommands.Dispatch(DragCommands.Payload.Event(value), _)) =>
                            invokeDrag(attrs.onDragStart, attrs.onDragStartEvt.map(_(value)))
                                .andThen(keepBubbling(elem, attrs.onDragStart.nonEmpty || attrs.onDragStartEvt.nonEmpty))
                        case _ => true
                    }
            case _: UIEvent.DragEnd =>
                if !isTarget then true
                else
                    DragCommands.current.use {
                        case Present(DragCommands.Dispatch(DragCommands.Payload.End(value), _)) =>
                            invokeDrag(attrs.onDragEnd, attrs.onDragEndEvt.map(_(value)))
                                .andThen(keepBubbling(elem, attrs.onDragEnd.nonEmpty || attrs.onDragEndEvt.nonEmpty))
                        case _ => true
                    }
            case _: UIEvent.DragEnter => dispatchDragEvent(elem, attrs.onDragEnter, attrs.onDragEnterEvt)
            case _: UIEvent.DragLeave => dispatchDragEvent(elem, attrs.onDragLeave, attrs.onDragLeaveEvt)
            case _: UIEvent.DragOver  => dispatchDragEvent(elem, attrs.onDragOver, attrs.onDragOverEvt)
            case _: UIEvent.Drop      =>
                DragCommands.current.use {
                    case Present(DragCommands.Dispatch(DragCommands.Payload.Event(value), decisions)) =>
                        invokeDecision(attrs.onDrop, decisions, "The drop handler failed.")
                            .andThen(invokeDecision(attrs.onDropEvt.map(_(value)), decisions, "The drop handler failed."))
                            .andThen(keepBubbling(elem, attrs.onDrop.nonEmpty || attrs.onDropEvt.nonEmpty))
                    case _ => true
                }
            case _: UIEvent.SortMove =>
                DragCommands.current.use {
                    case Present(DragCommands.Dispatch(DragCommands.Payload.Move(value), decisions)) =>
                        invokeDecision(attrs.onSortMove, decisions, "The sort handler failed.")
                            .andThen(invokeDecision(attrs.onSortMoveEvt.map(_(value)), decisions, "The sort handler failed."))
                            .andThen(keepBubbling(elem, attrs.onSortMove.nonEmpty || attrs.onSortMoveEvt.nonEmpty))
                    case _ => true
                }
            // Payload (UI.PointerEvent) rides the wire directly like FileSelect, so nothing is read from the element.
            case ev: UIEvent.PointerDown =>
                invokeWith(attrs.onPointerDown, ev.pointer).andThen(keepBubbling(elem, attrs.onPointerDown.nonEmpty))
            case ev: UIEvent.PointerMove =>
                invokeWith(attrs.onPointerMove, ev.pointer).andThen(keepBubbling(elem, attrs.onPointerMove.nonEmpty))
            case ev: UIEvent.PointerUp =>
                invokeWith(attrs.onPointerUp, ev.pointer).andThen(keepBubbling(elem, attrs.onPointerUp.nonEmpty))
            case ev: UIEvent.ScrollPosition =>
                val sp = UI.ScrollPositionEvent(ev.scrollTop, ev.scrollLeft, ev.targetId)
                invokeWith(attrs.onScrollPos, sp)
                    .andThen(keepBubbling(elem, attrs.onScrollPos.nonEmpty))
            case _ => true
        end match
    end dispatchToElement

    private def invokeDrag(action: Maybe[UI.Action], typed: Maybe[UI.Action])(using Frame): Unit < Async =
        def safe(handler: Maybe[UI.Action]): Unit < Async =
            handler.fold((): Unit < Async)(effect => Failures.recover(())(effect.unit))
        safe(action).andThen(safe(typed))
    end invokeDrag

    private def dispatchDragEvent(
        elem: Element,
        action: Maybe[UI.Action],
        typed: Maybe[Drag.Event => UI.Action]
    )(using Frame): Boolean < Async =
        DragCommands.current.use {
            case Present(DragCommands.Dispatch(DragCommands.Payload.Event(value), _)) =>
                invokeDrag(action, typed.map(_(value)))
                    .andThen(keepBubbling(elem, action.nonEmpty || typed.nonEmpty))
            case _ => true
        }

    private def invokeDecision(
        handler: Maybe[Drag.Decision < (Async & Abort[Any])],
        decisions: Maybe[AtomicRef[DragCommands.DecisionState]],
        failure: String
    )(using Frame): Unit < Async =
        handler.fold((): Unit < Async) { effect =>
            for
                result <- Abort.run[Any](effect)
                _      <- result match
                    case Result.Success(decision)     => recordDecision(decisions, decision)
                    case Result.Panic(_: Interrupted) => failDecision(decisions, failure)
                    case Result.Failure(err)          => failDecision(decisions, failure).andThen(Failures.recover(())(Abort.fail(err)))
                    case Result.Panic(panic)          => failDecision(decisions, failure).andThen(Failures.recover(())(Abort.panic(panic)))
            yield ()
        }

    private def recordDecision(ref: Maybe[AtomicRef[DragCommands.DecisionState]], decision: Drag.Decision)(using
        Frame
    ): Unit < Sync =
        ref.fold((): Unit < Sync)(_.getAndUpdate {
            case failed: DragCommands.DecisionState.Failed     => failed
            case rejected: DragCommands.DecisionState.Rejected => rejected
            case _                                             =>
                decision match
                    case Drag.Decision.Accept         => DragCommands.DecisionState.Accepted
                    case reject: Drag.Decision.Reject => DragCommands.DecisionState.Rejected(reject)
        }.unit)

    private def failDecision(ref: Maybe[AtomicRef[DragCommands.DecisionState]], reason: String)(using Frame): Unit < Sync =
        ref.fold((): Unit < Sync)(_.getAndUpdate(_ =>
            DragCommands.DecisionState.Failed(Drag.Decision.Reject(Drag.Rejection.Application(reason)))
        ).unit)

end EventDispatch
