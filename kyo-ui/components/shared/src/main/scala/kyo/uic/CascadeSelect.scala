package kyo.uic

import kyo.*
import kyo.UI.*

/** One entry of a [[CascadeSelect]] option tree — the typed nested analogue of
  * Select's flat options: a `Leaf` carries a pickable value `A` (projected to
  * text by the component's label function), a `Group` carries its own label and
  * nested children (opening a side sub-panel).
  */
enum CascadeItem[A] derives CanEqual:
    case Leaf(value: A)
    case Group(label: String, children: List[CascadeItem[A]])

object CascadeItem:
    /** A pickable leaf option. */
    def leaf[A](value: A): CascadeItem[A] = CascadeItem.Leaf(value)

    /** A group labelled `label` opening a nested sub-panel of `children`. */
    def group[A](label: String)(children: CascadeItem[A]*): CascadeItem[A] =
        CascadeItem.Group(label, children.toList)
end CascadeItem

/** CascadeSelect — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * CascadeSelect anatomy: `div.p-cascadeselect.p-component.p-inputwrapper
  * [.p-invalid][.p-disabled][.p-cascadeselect-sm|-lg][.p-variant-filled]
  * [.p-cascadeselect-fluid][.p-cascadeselect-open]` > `span.p-cascadeselect-label`
  * + `div.p-cascadeselect-dropdown` with the chevron, floating
  * `div.p-cascadeselect-overlay` panel > `div.p-cascadeselect-list-container` >
  * `ul.p-cascadeselect-list[role=tree]` of `li.p-cascadeselect-option` rows —
  * groups (`.p-cascadeselect-option-group`, trailing `.p-cascadeselect-group-icon`)
  * open SIDE-NESTED sub-panels stamped `.p-cascadeselect-overlay
  * .p-cascadeselect-option-list`), so the extracted `@primeuix` cascadeselect
  * CSS applies verbatim.
  *
  * Options are a TYPED tree of [[CascadeItem]]s: `options(items)(label)`
  * projects leaf values to text, groups carry their own label; `optionKey`
  * (default: the label projection) supplies the stable key written into the
  * bound ref. Clicking a group row opens exactly its chain (the Menubar
  * `openExactly` machinery — sibling branches close); clicking a LEAF writes
  * the key, fires `onChange`, and closes the whole chain. Every panel is a real
  * [[Overlay]]: an outside click closes ONE level (the topmost backdrop catches
  * it), Escape closes ONE level too (each panel consumes its own keydown —
  * the Overlay per-level story), each panel seeds focus on open. Group opening is click-driven (a
  * deliberate deviation from Prime's hover-open — a server round-trip per hover
  * is heavy; the sheet's hover styling is untouched).
  *
  * DEVIATION: a nested sub-panel is a `div` Overlay panel holding a real
  * `ul.p-cascadeselect-list` (Prime renders the nested `ul` itself as the
  * panel) — the panel div carries the `.p-cascadeselect-overlay` skin, the
  * inner `ul` Prime's list padding, so the visible box computes identically.
  *
  * Honest deferrals: arrow-key row navigation (rows open/pick by click; the
  * panels seed focus so Escape works without a prior click), the mobile query
  * mode, option templates, showClear, and loading state.
  */
final case class CascadeSelect[A] private (
    items: List[CascadeItem[A]],
    labelF: A => String,
    keyF: Maybe[A => String] = Absent,
    valueRef: Maybe[SignalRef[String]] = Absent,
    openRefV: Maybe[SignalRef[Boolean]] = Absent,
    placeholderV: Maybe[TextValue] = Absent,
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
    onChangeF: Maybe[String => Any < Async] = Absent,
    idV: Maybe[String] = Absent,
    onBlurF: Maybe[String => Any < Async] = Absent
) extends Node, TextFormControl:
    type Self = CascadeSelect[A]

    /** Native `id` on the trigger label — pair with `Label.forId`. */
    def id(v: String): CascadeSelect[A] = copy(idV = Present(v))

    /** Appends typed option-tree roots with the leaf text projection. */
    def options(is: Seq[CascadeItem[A]])(label: A => String): CascadeSelect[A] =
        copy(items = items ++ is.toList, labelF = label)

    /** Stable per-leaf key — the value written into the bound ref (defaults to
      * the label projection).
      */
    def optionKey(f: A => String): CascadeSelect[A] = copy(keyF = Present(f))

    /** Binds the selection two-way to `ref`, keyed by [[optionKey]]: leaf picks
      * write the key back, ref changes reselect the matching leaf.
      */
    def value(ref: SignalRef[String]): CascadeSelect[A] = copy(valueRef = Present(ref))

    /** Binds the ROOT panel visibility two-way to `ref` (optional — self-managed
      * otherwise); the nested group panels stay component-internal.
      */
    def open(ref: SignalRef[Boolean]): CascadeSelect[A] = copy(openRefV = Present(ref))

    /** Text shown on the closed trigger while the bound value is empty. */
    def placeholder(v: String): CascadeSelect[A] = copy(placeholderV = Present(TextValue.Const(v)))

    /** Reactive placeholder — re-renders the label in place on signal emission (resolved INSIDE the
      * mount subscription, so the open panel chain survives). For locale-driven text.
      */
    def placeholder(sig: Signal[String]): CascadeSelect[A] = copy(placeholderV = Present(TextValue.Dyn(sig)))

    def disabled(v: Boolean): CascadeSelect[A] = copy(disabledFlag = v)

    /** HTML form participation: emits a hidden `<input name=...>` carrying the
      * current leaf key.
      */
    def name(v: String): CascadeSelect[A] = copy(nameV = Present(v))

    /** Native tooltip (`title`). */
    def tooltip(v: String): CascadeSelect[A] = copy(tooltipV = Present(TextValue.Const(v)))

    /** Reactive tooltip — native `title` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def tooltip(sig: Signal[String]): CascadeSelect[A] = copy(tooltipV = Present(TextValue.Dyn(sig)))

    /** Size: `.p-cascadeselect-sm` / default / `.p-cascadeselect-lg`. */
    def size(v: Size): CascadeSelect[A] = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): CascadeSelect[A] = copy(variantV = v)

    /** Spans the full width of its container (`.p-cascadeselect-fluid`). */
    def fluid(v: Boolean): CascadeSelect[A] = copy(fluidFlag = v)

    /** Marks the field invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): CascadeSelect[A] = copy(invalidV = Present(BoolValue.Const(v)))

    /** Message rendered below the field while `invalid(true)` (kyo extension). */
    def invalidMessage(v: String): CascadeSelect[A] = copy(invalidMsgV = Present(v))

    /** Reactive validity: the bound signal toggles `.p-invalid` + `aria-invalid` in
      * place. Explicit override of the message-derived red default.
      */
    def invalid(sig: Signal[Boolean]): CascadeSelect[A] = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Reactive invalid message — `Present` shows the row and (by default) turns the
      * field red; `Absent` clears both. Re-renders in place on emission.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): CascadeSelect[A] = copy(invalidMsgDynV = Present(sig))

    /** Accessible name → `aria-label`. */
    def accessibleName(v: String): CascadeSelect[A] = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): CascadeSelect[A] = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Accessible name reference → `aria-labelledby`. */
    def accessibleNameRef(v: String): CascadeSelect[A] = copy(accNameRefV = Present(v))

    /** Fired with the newly picked leaf key. */
    def onChange(f: String => Any < Async): CascadeSelect[A] = copy(onChangeF = Present(f))

    /** Fires on focus loss (native `blur`) with the current selection key — unlike
      * `onChange` it fires even when nothing was changed. The form-validation layer
      * wires its `Blur` trigger here.
      */
    def onBlur(f: String => Any < Async): CascadeSelect[A] = copy(onBlurF = Present(f))

    /** The stable leaf key: [[optionKey]] if set, else the label projection. */
    private def key(a: A): String = keyF.getOrElse(labelF)(a)

    /** Paths (index chains from the root) of every GROUP item, in tree order —
      * one open/closed ref is allocated per path (the Menubar machinery).
      */
    private[uic] def groupPaths: List[List[Int]] =
        def loop(is: List[CascadeItem[A]], prefix: List[Int]): List[List[Int]] =
            is.zipWithIndex.flatMap {
                case (CascadeItem.Group(_, children), i) =>
                    val p = prefix :+ i
                    p :: loop(children, p)
                case _ => Nil
            }
        loop(items, Nil)
    end groupPaths

    /** The label of the leaf whose key is `k` (searched over the whole tree). */
    private def labelForKey(k: String): Maybe[String] =
        def loop(is: List[CascadeItem[A]]): Maybe[String] =
            is.foldLeft(Absent: Maybe[String]) {
                case (found @ Present(_), _)             => found
                case (_, CascadeItem.Leaf(a))            => if key(a) == k then Present(labelF(a)) else Absent
                case (_, CascadeItem.Group(_, children)) => loop(children)
            }
        loop(items)
    end labelForKey

    private[uic] def render(using Frame): UI =
        // One open/closed signal for the root panel plus one per group, allocated
        // by this effectful mount; static projections render the closed anatomy inert.
        val stat: UI = withValue(cur => body(cur, false, Map.empty.withDefaultValue(false), Absent))
        UI.mounted {
            for
                open <- openRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef(false)
                refs <- Kyo.foreach(groupPaths)(p => Signal.initRef(false).map(p -> _))
            yield wired(open, refs.toList)
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes (golden-test seam): ONE reactive
      * chain over the root ref plus every group ref (MenuRender precedent).
      */
    private[uic] def wired(open: SignalRef[Boolean], refs: List[(List[Int], SignalRef[Boolean])])(using Frame): UI =
        open.render { o =>
            MenuRender.renderAll(refs) { openMap =>
                withValue(cur => body(cur, o, openMap.withDefaultValue(false), Present((open, refs))))
            }
        }

    private def withValue(f: String => UI)(using Frame): UI =
        valueRef match
            case Present(ref) => ref.render(f)
            case Absent       => f("")

    private def body(
        current: String,
        isOpen: Boolean,
        openMap: Map[List[Int], Boolean],
        st: Maybe[(SignalRef[Boolean], List[(List[Int], SignalRef[Boolean])])]
    )(using Frame): UI =
        // Reactive-placeholder + -invalid gates (INSIDE the mount subscription — never around the
        // UI.mounted node): with a reactive slot set, re-render the field + message through the shared
        // helper; otherwise the untouched static form. The resolved placeholder text threads through.
        TextValue.reactive(placeholderV): ph =>
            (invalidV.dynSig, invalidMsgDynV) match
                case (Absent, Absent) => bodyStatic(current, isOpen, openMap, st, ph)
                case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) =>
                        copy(invalidV = Present(BoolValue.Const(red)), invalidMsgV = msg, invalidMsgDynV = Absent)
                            .bodyStatic(current, isOpen, openMap, st, ph)
                    )

    private def bodyStatic(
        current: String,
        isOpen: Boolean,
        openMap: Map[List[Int], Boolean],
        st: Maybe[(SignalRef[Boolean], List[(List[Int], SignalRef[Boolean])])],
        placeholder: Maybe[String]
    )(using Frame): UI =

        def closeAll: Any < Async =
            st match
                case Present((open, refs)) =>
                    for
                        _ <- MenuRender.openExactly(refs, Absent)
                        _ <- open.set(false)
                    yield ()
                case Absent => ()

        // Opens with every group closed (fresh chain).
        def openPanel: Any < Async =
            st match
                case Present((open, refs)) =>
                    for
                        _ <- MenuRender.openExactly(refs, Absent)
                        _ <- open.set(true)
                    yield ()
                case Absent => ()

        def toggleRoot: Any < Async =
            if isOpen then closeAll else openPanel

        // Writes the picked leaf key, fires onChange, closes the whole chain.
        def pick(a: A): Any < Async =
            val k = key(a)
            for
                _ <- valueRef match
                    case Present(r) => r.set(k)
                    case Absent     => (): Any < Async
                _ <- onChangeF match
                    case Present(g) => g(k)
                    case Absent     => (): Any < Async
                _ <- closeAll
            yield ()
            end for
        end pick

        // === closed trigger ======================================================
        val selText         = if current.isEmpty then "" else labelForKey(current).getOrElse(current)
        val showPlaceholder = current.isEmpty && placeholder.isDefined
        val labelText       = if showPlaceholder then placeholder.getOrElse("") else selText

        var lbl = span.cssClass("p-cascadeselect-label")
        idV.foreach(v => lbl = lbl.id(v))
        if showPlaceholder then lbl = lbl.cssClass("p-placeholder")
        if labelText.isEmpty then lbl = lbl.cssClass("p-cascadeselect-label-empty")
        if !disabledFlag && st.isDefined then lbl = lbl.onClick(toggleRoot)
        val labelUI: UI = lbl(if labelText.isEmpty then " " else labelText)

        var dd = div.cssClass("p-cascadeselect-dropdown")
        if !disabledFlag && st.isDefined then dd = dd.onClick(toggleRoot)
        val dropdownUI: UI = dd(toChild(GlyphSvg(Icons.chevronDown, "p-cascadeselect-dropdown-icon", "p-icon")))

        val hiddenCarrier: List[UI] =
            nameV.toList.map(n => hiddenInput.jsProp("name", n).value(current))

        // === floating panel chain ================================================
        val panelUI: List[UI] = st.toList.collect {
            case (open, refs) if isOpen =>
                Overlay(open)
                    .animate(false)
                    .panelClass("p-cascadeselect-overlay")
                    .panelClass("p-component")(
                        div.cssClass("p-cascadeselect-list-container")(
                            toChild(list(items, Nil, current, openMap, Present((open, refs)), pick))
                        )
                    )
                    .renderOpen
        }

        // === field root ==========================================================
        var el = div.cssClass("p-cascadeselect").cssClass("p-component").cssClass("p-inputwrapper")
        if current.nonEmpty then el = el.cssClass("p-inputwrapper-filled")
        if isOpen then el = el.cssClass("p-cascadeselect-open").cssClass("p-uic-overlay-anchor")
        if invalidV.constTrue then el = el.cssClass("p-invalid").aria("invalid", "true")
        if disabledFlag then el = el.cssClass("p-disabled")
        sizeV match
            case Size.Small  => el = el.cssClass("p-cascadeselect-sm")
            case Size.Large  => el = el.cssClass("p-cascadeselect-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then el = el.cssClass("p-variant-filled")
        if fluidFlag then el = el.cssClass("p-cascadeselect-fluid")
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
            // Focus-loss on the trigger reports the currently bound key (or the resolved
            // current value when unbound) — the validation layer's Blur trigger.
            onBlurF.foreach { f =>
                el = el.onBlur(valueRef match
                    case Present(r) => r.use(f)
                    case Absent     => f(current))
            }
        end if

        FieldInvalid.withMessage(
            el(((labelUI :: dropdownUI :: hiddenCarrier) ++ panelUI).map(toChild)*),
            invalidV.constTrue,
            invalidMsgV
        )
    end bodyStatic

    /** One panel level: `ul.p-cascadeselect-list` (role `tree` at the root,
      * `group` nested) of leaf and group rows; an OPEN group row anchors its
      * side-nested Overlay sub-panel (host-gated single-subscription form — the
      * wired chain already subscribes to every group ref).
      */
    private def list(
        its: List[CascadeItem[A]],
        path: List[Int],
        current: String,
        openMap: Map[List[Int], Boolean],
        st: Maybe[(SignalRef[Boolean], List[(List[Int], SignalRef[Boolean])])],
        pick: A => Any < Async
    )(using Frame): UI =
        val rows: List[UI] = its.zipWithIndex.map {
            case (CascadeItem.Leaf(a), _) =>
                val isSel = key(a) == current
                var row   = li.cssClass("p-cascadeselect-option").role("treeitem").aria("selected", isSel.toString)
                if isSel then row = row.cssClass("p-cascadeselect-option-selected")
                var content = div.cssClass("p-cascadeselect-option-content")
                if st.isDefined then content = content.onClick(pick(a))
                row(toChild(content(toChild(span.cssClass("p-cascadeselect-option-text")(labelF(a))))))

            case (CascadeItem.Group(label, children), i) =>
                val p         = path :+ i
                val groupOpen = openMap(p)
                def toggleGroup: Any < Async =
                    st match
                        case Present((_, refs)) =>
                            val target: Maybe[List[Int]] =
                                if groupOpen then (if p.size > 1 then Present(p.init) else Absent)
                                else Present(p)
                            MenuRender.openExactly(refs, target)
                        case Absent => ()
                var row = li
                    .cssClass("p-cascadeselect-option")
                    .cssClass("p-cascadeselect-option-group")
                    .cssClass("p-uic-overlay-anchor")
                    .role("treeitem")
                    .aria("expanded", groupOpen.toString)
                    .aria("level", (p.size).toString)
                if groupOpen then row = row.cssClass("p-cascadeselect-option-active")
                var content = div.cssClass("p-cascadeselect-option-content")
                if st.isDefined then content = content.onClick(toggleGroup)
                val contentUI: UI = content(
                    toChild(span.cssClass("p-cascadeselect-option-text")(label)),
                    toChild(
                        span.cssClass("p-cascadeselect-group-icon-container")(
                            toChild(GlyphSvg(Icons.angleRight, "p-cascadeselect-group-icon"))
                        )
                    )
                )
                // The side sub-panel: Prime's nested list skin on the Overlay primitive
                // (RightStart = the sheet's `inset-inline-start: 100%; inset-block-start: 0`).
                val panel: List[UI] = st match
                    case Present((_, refs)) if groupOpen =>
                        refs.collectFirst { case (`p`, ref) => ref } match
                            case Some(ref) =>
                                List(
                                    Overlay(ref)
                                        .anchor(OverlayAnchor.RightStart)
                                        .matchWidth(false)
                                        .animate(false)
                                        .panelClass("p-cascadeselect-overlay")
                                        .panelClass("p-cascadeselect-option-list")(
                                            list(children, p, current, openMap, st, pick)
                                        )
                                        .renderOpen
                                )
                            case None => Nil
                    case _ => Nil
                row((contentUI :: panel).map(toChild)*)
        }
        ul.cssClass("p-cascadeselect-list").role(if path.isEmpty then "tree" else "group")(rows.map(toChild)*)
    end list
end CascadeSelect

object CascadeSelect:
    def apply[A](): CascadeSelect[A] = new CascadeSelect[A](Nil, _.toString)
