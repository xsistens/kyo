package kyo.uic

import kyo.*
import kyo.UI.*
import scala.annotation.targetName

/** TreeSelect — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * TreeSelect anatomy: `div.p-treeselect.p-component.p-inputwrapper
  * [.p-invalid][.p-disabled][.p-treeselect-sm|-lg][.p-variant-filled]
  * [.p-treeselect-fluid][.p-treeselect-open]` > `div.p-treeselect-label-container`
  * > `div.p-treeselect-label` + `div.p-treeselect-dropdown` with the chevron,
  * floating `div.p-treeselect-overlay` panel > `div.p-treeselect-tree-container`
  * hosting the EXISTING [[Tree]] component's `.p-tree` anatomy), so the
  * extracted `@primeuix` treeselect CSS applies verbatim — its
  * `.p-treeselect-overlay .p-tree` rule reaches through the composition.
  *
  * Select's tree-shaped sibling: the same [[Overlay]]-based floating panel
  * (outside click / Escape close it, the panel seeds focus on open), the value
  * a `SignalRef[Set[String]]` of selected NODE ids. The panel hosts the real
  * `uic.Tree` — its [[TreeNode]] model, expansion refs, and [[SelectionMode]]
  * semantics are reused wholesale: `Single` replaces the selection and CLOSES
  * the panel on pick (Prime), `Multiple`/`Checkbox` toggle ids and keep the
  * panel open (Checkbox renders Prime's per-node checkbox column; parent/child
  * check-state cascade stays deferred exactly as in Tree). Expansion state is
  * component-internal unless `expanded(ref)` binds it out.
  *
  * Options come in the same shape as the flat pickers': `options(roots)(label)
  * (children)` projects any `A` to its text and its sub-options, with the option
  * key defaulting to the label as in [[Select.optionKey]]. The extra `children`
  * projection is the whole difference between a tree-shaped picker and a flat
  * one. `nodes(TreeNode*)` remains for hand-authored trees whose nodes carry an
  * icon, a tooltip or their own accessible name.
  *
  * The trigger shows the selected nodes' labels (tree order, comma-joined) or
  * the placeholder while empty.
  *
  * Honest deferrals: chip display mode, the header filter, showClear, and
  * checkbox cascade propagation (as in Tree); arrow-key tree navigation (nodes
  * toggle/select by click; the panel seeds focus so Escape works without a
  * prior click).
  */
final case class TreeSelect private (
    nodeList: List[TreeNode] = Nil,
    valueRef: Maybe[SignalRef[Set[String]]] = Absent,
    expandedRefV: Maybe[SignalRef[Set[String]]] = Absent,
    openRefV: Maybe[SignalRef[Boolean]] = Absent,
    selectionModeV: SelectionMode = SelectionMode.Single,
    placeholderV: Maybe[TextValue] = Absent,
    emptyMessageV: Maybe[TextValue] = Absent,
    disabledFlag: Boolean = false,
    nameV: Maybe[String] = Absent,
    tooltipV: Maybe[TextValue] = Absent,
    sizeV: Size = Size.Normal,
    variantV: FieldVariant = FieldVariant.Outlined,
    fluidFlag: Boolean = false,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    onChangeF: Maybe[Set[String] => Any < Async] = Absent,
    idV: Maybe[String] = Absent,
    onBlurF: Maybe[Set[String] => Any < Async] = Absent
) extends Node, MultiSelectFormControl:
    type Self = TreeSelect

    /** Native `id` on the trigger — pair with `Label.forId`. */
    def id(v: String): TreeSelect = copy(idV = Present(v))

    /** Appends typed root options in the picker family's shape: `label` projects an
      * `A` to its visible text and `children` to its sub-options, so the tree
      * structure arrives as a projection like every other key. The option key
      * defaults to the label, exactly as in [[Select.optionKey]]; use the
      * three-projection overload when the label is not a stable identity.
      *
      * The family shape does not fit every tree, because a `TreeNode` also carries
      * an icon, a tooltip and an accessible name that no `A => String` supplies —
      * reach for [[nodes]] with the hand-authored model when you need those.
      */
    def options[A](roots: Seq[A])(label: A => String)(children: A => Seq[A]): TreeSelect =
        options(roots)(label, label)(children)

    /** [[options]] with an explicit stable key per option — the id written into the
      * bound selection set.
      */
    def options[A](roots: Seq[A])(label: A => String, optionKey: A => String)(children: A => Seq[A]): TreeSelect =
        copy(nodeList = nodeList ++ roots.toList.map(a => TreeSelect.project(a, label, optionKey, children)))

    /** Appends root nodes of the hand-authored [[TreeNode]] model — the escape hatch
      * from [[options]] when a node needs an icon, a tooltip or its own accessible
      * name. [[Listbox.items]] stands in the same relation to [[Listbox.item]].
      */
    def nodes(ns: TreeNode*): TreeSelect = copy(nodeList = nodeList ++ ns.toList)

    /** Binds the selected node id set two-way to `ref`: picks write the updated
      * set back, ref changes reselect the matching nodes.
      */
    @targetName("valueKeys")
    def value(ref: SignalRef[Set[String]]): TreeSelect = copy(valueRef = Present(ref))

    /** Binds the expansion set two-way to `ref` (optional — expansion is
      * component-internal otherwise).
      */
    def expanded(ref: SignalRef[Set[String]]): TreeSelect = copy(expandedRefV = Present(ref))

    /** Binds the panel visibility two-way to `ref` (optional — self-managed
      * otherwise).
      */
    def open(ref: SignalRef[Boolean]): TreeSelect = copy(openRefV = Present(ref))

    /** Selection semantics (Tree's modes): `Single` (default — pick replaces the
      * selection and closes the panel), `Multiple` (toggle, panel stays open), or
      * `Checkbox` (toggle with Prime's per-node checkbox column, panel stays open).
      */
    def selectionMode(v: SelectionMode): TreeSelect = copy(selectionModeV = v)

    /** Text shown on the closed trigger while the bound set is empty. */
    def placeholder(v: String): TreeSelect = copy(placeholderV = Present(TextValue.Const(v)))

    /** Reactive placeholder — re-renders the label in place on signal emission (resolved INSIDE the
      * mount subscription, so the open tree/expansion state survives). For locale-driven text.
      */
    def placeholder(sig: Signal[String]): TreeSelect = copy(placeholderV = Present(TextValue.Dyn(sig)))

    /** Text of the `div.p-treeselect-empty-message` when the tree has no nodes
      * (default "No results found" — Prime's default).
      */
    def emptyMessage(v: String): TreeSelect = copy(emptyMessageV = Present(TextValue.Const(v)))

    /** Reactive variant — re-renders the empty message in place on signal emission. */
    def emptyMessage(sig: Signal[String]): TreeSelect = copy(emptyMessageV = Present(TextValue.Dyn(sig)))

    def disabled(v: Boolean): TreeSelect = copy(disabledFlag = v)

    /** HTML form participation: emits a hidden `<input name=...>` carrying the
      * comma-joined selected node ids.
      */
    def name(v: String): TreeSelect = copy(nameV = Present(v))

    /** Native tooltip (`title`). */
    def tooltip(v: String): TreeSelect = copy(tooltipV = Present(TextValue.Const(v)))

    /** Reactive tooltip — native `title` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def tooltip(sig: Signal[String]): TreeSelect = copy(tooltipV = Present(TextValue.Dyn(sig)))

    /** Size: `.p-treeselect-sm` / default / `.p-treeselect-lg`. */
    def size(v: Size): TreeSelect = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): TreeSelect = copy(variantV = v)

    /** Spans the full width of its container (`.p-treeselect-fluid`). */
    def fluid(v: Boolean): TreeSelect = copy(fluidFlag = v)

    /** Marks the field invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): TreeSelect = copy(invalidV = Present(BoolValue.Const(v)))

    /** Message rendered below the field while `invalid(true)` (kyo extension). */
    def invalidMessage(v: String): TreeSelect = copy(invalidMsgV = Present(v))

    /** Reactive validity: the bound signal toggles `.p-invalid` + `aria-invalid` in
      * place. Explicit override of the message-derived red default.
      */
    def invalid(sig: Signal[Boolean]): TreeSelect = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Reactive invalid message — `Present` shows the row and (by default) turns the
      * field red; `Absent` clears both. Re-renders in place on emission.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): TreeSelect = copy(invalidMsgDynV = Present(sig))

    /** Accessible name → `aria-label`. */
    def accessibleName(v: String): TreeSelect = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): TreeSelect = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Accessible name reference → `aria-labelledby`. */
    def accessibleNameRef(v: String): TreeSelect = copy(accNameRefV = Present(v))

    /** Fired with the FULL updated node id set after every selection change. */
    def onChange(f: Set[String] => Any < Async): TreeSelect = copy(onChangeF = Present(f))

    /** Fires on focus loss (native `blur`) with the current selection — unlike
      * `onChange` it fires even when nothing was changed. The form-validation layer
      * wires its `Blur` trigger here.
      */
    @targetName("onBlurKeys")
    def onBlur(f: Set[String] => Any < Async): TreeSelect = copy(onBlurF = Present(f))

    /** (id, label) pairs of every node in tree order — the trigger-label lookup. */
    private def flatNodes: List[(String, String)] =
        def loop(ns: List[TreeNode]): List[(String, String)] =
            ns.flatMap(n => (n.id, n.text) :: loop(n.children))
        loop(nodeList)
    end flatNodes

    /** The loud card shown at the top of the panel when two nodes share an id (see
      * [[KeyDiagnostics]]). The ids are the selection keys, so a collision merges two
      * nodes into one selection entry — and [[options]] defaults the key to the label,
      * exactly the way [[Select.optionKey]] does, so the same duplicate-label data set
      * produces it.
      */
    private def keyCollisionCard(using Frame): List[UI] =
        val dups = KeyDiagnostics.duplicates(flatNodes.map(_._1))
        if dups.isEmpty then Nil
        else
            List(KeyDiagnostics.card(
                "TreeSelect",
                "node ids are not unique, so those nodes share one selection entry; give options an explicit key " +
                    "projection, or distinct TreeNode ids",
                dups
            ))
        end if
    end keyCollisionCard

    private[uic] def render(using Frame): UI =
        // Open + (unless bound) expansion state live in signals allocated by this
        // effectful mount; static projections render the closed anatomy inert.
        val stat: UI = withValue(sel => body(sel, Set.empty, false, Absent))
        UI.mounted {
            for
                open <- openRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef(false)
                exp <- expandedRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef(Set.empty[String])
            yield wired(open, exp)
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes (golden-test seam). */
    private[uic] def wired(open: SignalRef[Boolean], exp: SignalRef[Set[String]])(using Frame): UI =
        open.render { o =>
            exp.render { e =>
                withValue(sel => body(sel, e, o, Present((open, exp))))
            }
        }

    private def withValue(f: Set[String] => UI)(using Frame): UI =
        valueRef match
            case Present(ref) => ref.render(f)
            case Absent       => f(Set.empty)

    private def body(
        current: Set[String],
        exp: Set[String],
        isOpen: Boolean,
        st: Maybe[(SignalRef[Boolean], SignalRef[Set[String]])]
    )(using Frame): UI =
        // Reactive-placeholder + -invalid gates (INSIDE the mount subscription — never around the
        // UI.mounted node): with a reactive slot set, re-render the field + message through the shared
        // helper; otherwise the untouched static form. The resolved placeholder text threads through.
        TextValue.reactive(placeholderV): ph =>
            (invalidV.dynSig, invalidMsgDynV) match
                case (Absent, Absent) => bodyStatic(current, exp, isOpen, st, ph)
                case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                        copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent)
                            .bodyStatic(current, exp, isOpen, st, ph)
                    )

    private def bodyStatic(
        current: Set[String],
        exp: Set[String],
        isOpen: Boolean,
        st: Maybe[(SignalRef[Boolean], SignalRef[Set[String]])],
        placeholder: Maybe[String]
    )(using Frame): UI =
        val selectedLabels = flatNodes.collect { case (id, text) if current.contains(id) => text }

        def openPanel: Any < Async =
            st match
                case Present((open, _)) => open.set(true)
                case Absent             => ()

        def toggle: Any < Async =
            st match
                case Present((open, _)) => open.set(!isOpen)
                case Absent             => ()

        // Fired by the hosted Tree AFTER its selection write: report the new set
        // via onChange; Single mode also closes the panel (Prime).
        def afterPick: Any < Async =
            st match
                case Present((open, _)) =>
                    for
                        _ <- onChangeF match
                            case Present(g) =>
                                valueRef match
                                    case Present(r) => r.get.map(g)
                                    case Absent     => (): Any < Async
                            case Absent => (): Any < Async
                        _ <- selectionModeV match
                            case SelectionMode.Single | SelectionMode.Radio => open.set(false)
                            case _                                          => (): Any < Async
                    yield ()
                case Absent => ()

        // === closed trigger ======================================================
        val showPlaceholder = selectedLabels.isEmpty && placeholder.isDefined
        val labelText       = if showPlaceholder then placeholder.getOrElse("") else selectedLabels.mkString(", ")

        var lbl = div.cssClass("p-treeselect-label")
        if showPlaceholder then lbl = lbl.cssClass("p-placeholder")
        if selectedLabels.isEmpty && placeholder.isEmpty then lbl = lbl.cssClass("p-treeselect-label-empty")
        // NBSP keeps the empty label's line box (Prime renders 'empty').
        val labelUI: UI = lbl(if labelText.isEmpty then " " else labelText)

        var lblContainer = div.cssClass("p-treeselect-label-container")
        idV.foreach(v => lblContainer = lblContainer.id(v))
        if !disabledFlag && st.isDefined then lblContainer = lblContainer.onClick(toggle)
        val labelContainerUI: UI = lblContainer(toChild(labelUI))

        var dd = div.cssClass("p-treeselect-dropdown").role("button").aria("haspopup", "tree").aria("expanded", isOpen.toString)
        if !disabledFlag && st.isDefined then dd = dd.onClick(toggle)
        val dropdownUI: UI = dd(toChild(GlyphSvg(Icons.chevronDown, "p-treeselect-dropdown-icon", "p-icon")))

        val hiddenCarrier: List[UI] =
            nameV.toList.map { n =>
                hiddenInput.jsProp("name", n).value(flatNodes.collect { case (id, _) if current.contains(id) => id }.mkString(","))
            }

        // === floating panel ======================================================
        val panelUI: List[UI] = st.toList.map { (open, expRef) =>
            val treeUI: UI =
                if nodeList.isEmpty then
                    emptyMessageV.getOrElse(TextValue.Const("No results found")) match
                        case TextValue.Const(t) => div.cssClass("p-treeselect-empty-message")(t): UI
                        case TextValue.Dyn(s)   => s.render(t => div.cssClass("p-treeselect-empty-message")(t))
                else
                    var tree = Tree()
                        .nodes(nodeList*)
                        .expanded(expRef)
                        .selectionMode(selectionModeV)
                        .onItemClick(_ => afterPick)
                    valueRef.foreach(r => tree = tree.selected(r))
                    // The enclosing wired render already subscribes to the expansion and
                    // value refs — the resolved seam renders the tree WITHOUT its own
                    // nested subscriptions (handlers still write through the bound refs).
                    tree.resolved(exp, current)
            Overlay(open)
                .panelClass("p-treeselect-overlay")
                .panelClass("p-component")(
                    (keyCollisionCard :+ (div.cssClass("p-treeselect-tree-container")(toChild(treeUI)): UI))*
                )
                .render
        }

        // === field root ==========================================================
        var el = div.cssClass("p-treeselect").cssClass("p-component").cssClass("p-inputwrapper")
        if selectedLabels.nonEmpty then el = el.cssClass("p-inputwrapper-filled")
        if isOpen then el = el.cssClass("p-treeselect-open").cssClass("p-uic-overlay-anchor")
        if invalidV.constTrue then el = el.cssClass("p-invalid").aria("invalid", "true")
        if disabledFlag then el = el.cssClass("p-disabled")
        sizeV match
            case Size.Small  => el = el.cssClass("p-treeselect-sm")
            case Size.Large  => el = el.cssClass("p-treeselect-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then el = el.cssClass("p-variant-filled")
        if fluidFlag then el = el.cssClass("p-treeselect-fluid")
        tooltipV match
            case Present(TextValue.Const(v)) => el = el.jsProp("title", v)
            case Present(TextValue.Dyn(s))   => el = el.title(s)
            case Absent                      => ()
        end match
        accNameV match
            case Present(TextValue.Const(v)) => el = el.aria("label", v)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        accNameRefV.foreach(v => el = el.aria("labelledby", v))
        el = el.aria("haspopup", "tree").aria("expanded", isOpen.toString)
        if !disabledFlag then
            el = el.tabIndex(0).preventScrollKeys
            if st.isDefined then
                el = el.onKeyDown { e =>
                    e.key match
                        case Keyboard.ArrowDown | Keyboard.Enter | Keyboard.Space if !isOpen => openPanel
                        case _                                                               => ()
                }
            end if
            // Focus-loss on the trigger reports the currently bound selection (or the
            // resolved current set when unbound) — the validation layer's Blur trigger.
            onBlurF.foreach { f =>
                el = el.onBlur(valueRef match
                    case Present(r) => r.use(f)
                    case Absent     => f(current))
            }
        end if

        FieldInvalid.withMessage(
            el(((labelContainerUI :: dropdownUI :: hiddenCarrier) ++ panelUI).map(toChild)*),
            invalidV.constTrue,
            invalidMsgV
        )
    end bodyStatic
end TreeSelect

object TreeSelect:
    def apply(): TreeSelect = new TreeSelect()

    /** Projects one typed option (and, recursively, its sub-options) into the
      * [[TreeNode]] model the panel's [[Tree]] renders. Termination is the caller's
      * `children` projection: a cyclic one does not terminate, exactly as a cyclic
      * hand-authored `TreeNode` structure would not.
      */
    private def project[A](a: A, label: A => String, key: A => String, children: A => Seq[A]): TreeNode =
        TreeNode(label(a), key(a), children = children(a).toList.map(project(_, label, key, children)))
end TreeSelect
