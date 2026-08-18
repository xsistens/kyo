package kyo.uic

import kyo.*
import kyo.UI.*

/** Select — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Select
  * anatomy: `div.p-select.p-component[.p-invalid][.p-disabled][.p-select-sm|-lg]
  * [.p-variant-filled][.p-select-fluid][.p-select-open]` > `span.p-select-label`
  * + optional `button.p-select-clear-icon` + `span.p-select-dropdown` with the
  * chevron, floating `div.p-select-overlay` panel > optional
  * `div.p-select-header` filter > `div.p-select-list-container` >
  * `ul.p-select-list[role=listbox]` of `li.p-select-option` rows), so the
  * extracted `@primeuix` select CSS applies verbatim.
  *
  * The option panel is the REAL floating panel, built on the [[Overlay]]
  * primitive: the trigger toggles it, an outside click or Escape closes it, and
  * the panel seeds focus on open (kyo's `data-kyo-focus-*` client contract) so
  * the keyboard works without any prior click — ArrowDown/ArrowUp move the
  * `.p-focus` highlight (disabled options skipped), Enter picks the highlighted
  * option, Escape closes and returns focus to the trigger. `filterable(true)`
  * renders Prime's header filter input (case-insensitive contains on the label
  * projection) over a query the select owns, `filterQuery(ref)` over one the app
  * owns; `showClear(true)` the clear affordance on the trigger while a value is
  * set; `checkmark(true)` Prime's per-option check column.
  *
  * Options are TYPED: `options(items)(label)` projects any `A` to its visible
  * text; `optionKey` (defaults to the label projection) supplies the stable value
  * written into the bound ref. `value(ref)` binds two-way; `open(ref)` optionally
  * exposes the panel visibility (it is self-managed otherwise). The interaction
  * state (open/highlight/filter query) lives in signals allocated by an
  * effectful `UI.mounted` region; static projections (SSG, the SSR page HTML)
  * render the same closed anatomy inert until the client transport attaches.
  * Form participation: `name(...)` emits a hidden `<input>` carrying the current
  * key (a div trigger has no native form value).
  *
  * Honest deferrals: option groups, editable mode, virtual scrolling, and
  * per-option templates are not implemented yet. Arrow keys on the open panel no
  * longer scroll the page underneath — the panel declares `preventScrollKeys`.
  */
final case class Select[A] private (
    items: List[A],
    labelF: A => String,
    keyF: Maybe[A => String] = Absent,
    optionDisabledF: Maybe[A => Boolean] = Absent,
    valueRef: Maybe[SignalRef[String]] = Absent,
    openRefV: Maybe[SignalRef[Boolean]] = Absent,
    placeholderV: Maybe[TextValue] = Absent,
    filterableFlag: Boolean = false,
    filterQueryRefV: Maybe[SignalRef[String]] = Absent,
    showClearFlag: Boolean = false,
    checkmarkFlag: Boolean = false,
    emptyMessageV: Maybe[TextValue] = Absent,
    disabledFlag: Boolean = false,
    readonlyFlag: Boolean = false,
    requiredFlag: Boolean = false,
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
    onBlurF: Maybe[String => Any < Async] = Absent,
    currentV: Maybe[String] = Absent,
    extraClassesV: List[String] = Nil,
    idV: Maybe[String] = Absent,
    inputwrapperFlag: Boolean = false,
    portalFlag: Boolean = false,
    scrollV: Overlay.Scroll = Overlay.Scroll.Close
) extends Node, TextFormControl:
    type Self = Select[A]

    /** Package-internal class hook: hosts (Paginator) stamp Prime's contextual
      * class (`p-paginator-rpp-dropdown`) onto the field root.
      */
    private[uic] def extraClass(cls: String): Select[A] = copy(extraClassesV = extraClassesV :+ cls)

    /** Package-internal: FloatLabel stamps Prime's wrapper-state classes
      * (`p-inputwrapper` + `p-inputwrapper-filled` while a value is selected or a
      * non-empty placeholder shows text) — stamped INSIDE the component's own
      * subscription so the filled state tracks the bound ref.
      */
    private[uic] def inputwrapper: Select[A] = copy(inputwrapperFlag = true)

    /** Native `id` on the trigger — pair with `Label.forId` / `FloatLabel.forId`. */
    def id(v: String): Select[A] = copy(idV = Present(v))

    /** Package-internal: hosts that already render inside their own subscription
      * supply the resolved selection directly (no nested subscription; writes go
      * through `onChange`).
      */
    private[uic] def current(v: String): Select[A] = copy(currentV = Present(v))

    /** Appends typed options with their text projection. */
    def options(is: Seq[A])(label: A => String): Select[A] =
        copy(items = items ++ is.toList, labelF = label)

    /** Convenience for plain string options (label = identity). */
    def options(is: Seq[String])(using ev: String =:= A): Select[A] =
        copy(items = items ++ is.map(ev).toList, labelF = a => ev.flip(a))

    /** Stable per-option key — the value written into the bound ref.
      *
      * Defaults to the label projection, which is correct only while the labels are
      * unique. When they are not, two options share one key and a pick applies to
      * the wrong one — a failure that appears only with the data that triggers it,
      * so it survives development and shows up in production. The panel therefore
      * renders a loud `.p-uic-key-error` card whenever the derived keys collide,
      * naming the offending keys.
      */
    def optionKey(f: A => String): Select[A] = copy(keyF = Present(f))

    /** Renders matching options disabled (`.p-disabled` rows — shown in the panel
      * but not pickable; the keyboard highlight skips them).
      */
    def optionDisabled(f: A => Boolean): Select[A] = copy(optionDisabledF = Present(f))

    /** Binds the selection two-way to `ref`, keyed by [[optionKey]]: picks write the
      * key back into the ref, ref changes reselect the matching option.
      */
    def value(ref: SignalRef[String]): Select[A] = copy(valueRef = Present(ref))

    /** Binds the panel visibility two-way to `ref` (optional — the panel is
      * self-managed otherwise): writes open and close it, trigger/outside-click/
      * Escape write back.
      */
    def open(ref: SignalRef[Boolean]): Select[A] = copy(openRefV = Present(ref))

    /** Text shown on the closed trigger while the bound value is empty (Prime's
      * `p-placeholder` label skin).
      */
    def placeholder(v: String): Select[A] = copy(placeholderV = Present(TextValue.Const(v)))

    /** Reactive placeholder — re-renders the label in place on signal emission (resolved INSIDE the
      * mount subscription, so open/highlight/filter state survives). For locale-driven text.
      */
    def placeholder(sig: Signal[String]): Select[A] = copy(placeholderV = Present(TextValue.Dyn(sig)))

    /** Renders Prime's header filter (`div.p-select-header` >
      * `input.p-select-filter`) over a query the select allocates itself,
      * filtering the options by a case-insensitive contains on the label
      * projection; the query resets on every open. Same word and same meaning on
      * [[MultiSelect.filterable]] and [[Listbox.filterable]].
      */
    def filterable(v: Boolean): Select[A] = copy(filterableFlag = v)

    /** Renders the same header filter over a query the APP owns: typing writes
      * `ref`, external writes re-filter the panel. Implies [[filterable]].
      * Server-driven option lists bind this and re-`options` on emission; the
      * matching binding on [[Listbox.filterQuery]] takes the same shape.
      */
    def filterQuery(ref: SignalRef[String]): Select[A] = copy(filterQueryRefV = Present(ref))

    /** Renders the clear affordance on the trigger while a value is set — clears
      * the bound ref to `""` and fires `onChange("")`.
      */
    def showClear(v: Boolean): Select[A] = copy(showClearFlag = v)

    /** Whether the floating options panel (and its backdrop) re-home to `document.body`
      * ([[Overlay.portal]]) — the escape from ancestor `overflow` clipping and `transform`
      * containing blocks, e.g. for a Select inside a ScrollPanel or DataTable cell (default false).
      */
    def portal(v: Boolean): Select[A] = copy(portalFlag = v)

    /** How the open options panel responds to scroll outside it (default
      * [[Overlay.Scroll.Close]] — Prime's hide-on-scroll). See [[Overlay.Scroll]].
      */
    def scroll(v: Overlay.Scroll): Select[A] = copy(scrollV = v)

    /** Renders Prime's option check column: the check glyph on the selected row
      * (`.p-select-option-check-icon`), a blank placeholder on the rest.
      */
    def checkmark(v: Boolean): Select[A] = copy(checkmarkFlag = v)

    /** Text of the `li.p-select-empty-message` row when no options render
      * (default "No results found" — Prime's default).
      */
    def emptyMessage(v: String): Select[A] = copy(emptyMessageV = Present(TextValue.Const(v)))

    /** Reactive variant — re-renders the empty message in place on signal emission. */
    def emptyMessage(sig: Signal[String]): Select[A] = copy(emptyMessageV = Present(TextValue.Dyn(sig)))

    def disabled(v: Boolean): Select[A] = copy(disabledFlag = v)

    /** `readonly` — interaction is blocked (the panel never opens), but unlike
      * `disabled` the field keeps its normal look (`.p-uic-select-readonly`).
      */
    def readonly(v: Boolean): Select[A] = copy(readonlyFlag = v)

    /** Marks the field required (`aria-required`). */
    def required(v: Boolean): Select[A] = copy(requiredFlag = v)

    /** HTML form participation: emits a hidden `<input name=...>` carrying the
      * current option key (the div trigger has no native form value).
      */
    def name(v: String): Select[A] = copy(nameV = Present(v))

    /** Native tooltip (`title`). */
    def tooltip(v: String): Select[A] = copy(tooltipV = Present(TextValue.Const(v)))

    /** Reactive tooltip — native `title` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def tooltip(sig: Signal[String]): Select[A] = copy(tooltipV = Present(TextValue.Dyn(sig)))

    /** Size: `.p-select-sm` / default / `.p-select-lg`. */
    def size(v: Size): Select[A] = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): Select[A] = copy(variantV = v)

    /** Spans the full width of its container (`.p-select-fluid`). */
    def fluid(v: Boolean): Select[A] = copy(fluidFlag = v)

    /** Marks the field invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): Select[A] = copy(invalidV = Present(BoolValue.Const(v)))

    /** Message rendered below the field while `invalid(true)` (kyo extension —
      * `div.p-uic-invalid-message`).
      */
    def invalidMessage(v: String): Select[A] = copy(invalidMsgV = Present(v))

    /** Reactive validity: the bound signal toggles `.p-invalid` + `aria-invalid` in
      * place. Explicit override of the message-derived red default.
      */
    def invalid(sig: Signal[Boolean]): Select[A] = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Reactive invalid message — `Present` shows the row and (by default) turns the
      * field red; `Absent` clears both. Re-renders in place on emission.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): Select[A] = copy(invalidMsgDynV = Present(sig))

    /** Accessible name → `aria-label`. */
    def accessibleName(v: String): Select[A] = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Select[A] = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Accessible name reference → `aria-labelledby`. */
    def accessibleNameRef(v: String): Select[A] = copy(accNameRefV = Present(v))

    /** Fired with the newly selected option key (also with `""` on clear). */
    def onChange(f: String => Any < Async): Select[A] = copy(onChangeF = Present(f))

    /** Fires when the trigger loses focus, with the currently selected key — the
      * form-validation layer's Blur trigger.
      */
    def onBlur(f: String => Any < Async): Select[A] = copy(onBlurF = Present(f))

    /** The stable option key: [[optionKey]] if set, else the label projection. */
    private def key(a: A): String = keyF.getOrElse(labelF)(a)

    private def isOptionDisabled(a: A): Boolean = optionDisabledF.exists(_(a))

    /** Whether the header filter renders: asked for by [[filterable]], or implied
      * by an app-owned [[filterQuery]].
      */
    private def filtering: Boolean = filterableFlag || filterQueryRefV.isDefined

    private def interactive: Boolean = !disabledFlag && !readonlyFlag

    /** The wired interaction state, allocated per mount. */
    final private case class State(
        open: SignalRef[Boolean],
        isOpen: Boolean,
        hi: SignalRef[Int],
        hiV: Int,
        q: SignalRef[String],
        qV: String
    )

    private[uic] def render(using Frame): UI =
        // The interaction state (open/highlight/filter) lives in signals allocated
        // by this effectful mount. Static projections (SSG, the SSR page GET,
        // initial render) show the placeholder — the same closed anatomy, inert —
        // until the transport attaches and the mount publishes the wired tree.
        val stat: UI = withValue(cur => body(cur, Absent))
        UI.mounted {
            for
                open <- openRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef(false)
                hi <- Signal.initRef(-1)
                // Same shape as `open` above: an app-owned query ref takes over the
                // slot the component would otherwise allocate for itself.
                q <- filterQueryRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef("")
            yield wired(open, hi, q)
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes — the seam golden tests render
      * directly (a full top-down re-render shows mounted regions as placeholders,
      * so the wired anatomy is only reachable here).
      */
    private[uic] def wired(open: SignalRef[Boolean], hi: SignalRef[Int], q: SignalRef[String])(using Frame): UI =
        open.render { o =>
            hi.render { h =>
                q.render { qq =>
                    withValue(cur => body(cur, Present(State(open, o, hi, h, q, qq))))
                }
            }
        }

    /** Renders through the value binding: hosts supply `currentV` resolved, a
      * bound ref subscribes, an unbound select is empty.
      */
    private def withValue(f: String => UI)(using Frame): UI =
        currentV match
            case Present(cur) => f(cur)
            case Absent =>
                valueRef match
                    case Present(ref) => ref.render(f)
                    case Absent       => f("")

    private def body(current: String, st: Maybe[State])(using Frame): UI =
        // Reactive-placeholder + -invalid gates (INSIDE the mount subscription — never around the
        // UI.mounted node). `State` is a path-dependent inner type, so both reactive re-renders thread
        // their resolved values (placeholder text; red flag + message) into `bodyStatic` directly rather
        // than through `copy(...)` (a copied instance's `State` would not match).
        TextValue.reactive(placeholderV): ph =>
            (invalidV.dynSig, invalidMsgDynV) match
                case (Absent, Absent) => bodyStatic(current, st, ph, invalidV.constTrue, invalidMsgV)
                case _ =>
                    FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) => bodyStatic(current, st, ph, red, msg))

    private def bodyStatic(current: String, st: Maybe[State], placeholder: Maybe[String], redFlag: Boolean, redMsg: Maybe[String])(using
        Frame
    ): UI =
        val isOpen  = st.exists(_.isOpen)
        val selIdx  = items.indexWhere(a => key(a) == current)
        val selText = if selIdx >= 0 then labelF(items(selIdx)) else ""

        // Writes the picked key, fires onChange, closes the panel.
        def pick(a: A)(s: State): Any < Async =
            val k = key(a)
            for
                _ <- valueRef match
                    case Present(r) => r.set(k)
                    case Absent     => (): Any < Async
                _ <- onChangeF match
                    case Present(g) => g(k)
                    case Absent     => (): Any < Async
                _ <- s.open.set(false)
            yield ()
            end for
        end pick

        // Opens with the highlight seeded on the selected option and a fresh query.
        def openPanel(s: State): Any < Async =
            for
                _ <- s.q.set("")
                _ <- s.hi.set(selIdx)
                _ <- s.open.set(true)
            yield ()

        def toggle(s: State): Any < Async =
            if s.isOpen then s.open.set(false) else openPanel(s)

        // === closed trigger ======================================================
        val showPlaceholder = current.isEmpty && placeholder.isDefined
        val labelText       = if showPlaceholder then placeholder.getOrElse("") else selText

        var lbl = span.cssClass("p-select-label")
        idV.foreach(v => lbl = lbl.id(v))
        if showPlaceholder then lbl = lbl.cssClass("p-placeholder")
        if labelText.isEmpty then lbl = lbl.cssClass("p-select-label-empty")
        st.foreach(s => if interactive then lbl = lbl.onClick(toggle(s)))
        // NBSP keeps the empty label's line box (Prime renders &nbsp;).
        val labelUI: UI = lbl(if labelText.isEmpty then " " else labelText)

        // Explicitly typed effect: an untyped match would infer `Any`, passing the
        // effect as an inert VALUE into the by-name handler (never executed).
        val clearEffect: Any < Async =
            st match
                case Present(_) =>
                    for
                        _ <- valueRef match
                            case Present(r) => r.set("")
                            case Absent     => (): Any < Async
                        _ <- onChangeF match
                            case Present(g) => g("")
                            case Absent     => (): Any < Async
                    yield ()
                case Absent => ()

        val clearUI: List[UI] =
            if showClearFlag && interactive && current.nonEmpty then
                List(
                    button
                        .cssClass("p-select-clear-icon")
                        .jsProp("type", "button")
                        .aria("label", "Clear")
                        .onClick(clearEffect)(toChild(GlyphSvg(Icons.times, "p-icon")))
                )
            else Nil

        var dd = span.cssClass("p-select-dropdown")
        st.foreach(s => if interactive then dd = dd.onClick(toggle(s)))
        val dropdownUI: UI = dd(toChild(GlyphSvg(Icons.chevronDown, "p-select-dropdown-icon", "p-icon")))

        // Hidden form carrier: a div trigger has no native form value.
        val hiddenCarrier: List[UI] =
            nameV.toList.map(n => hiddenInput.jsProp("name", n).value(current))

        // === floating panel ======================================================
        val panelUI: List[UI] = st.toList.map(s => overlayPanel(current, s, pick))

        // === field root ==========================================================
        var el = div.cssClass("p-select").cssClass("p-component")
        extraClassesV.foreach(c => el = el.cssClass(c))
        if inputwrapperFlag then
            el = el.cssClass("p-inputwrapper")
            // The float hook: a selected value fills the field; a non-empty placeholder
            // shows text too, so the label must float over it.
            if current.nonEmpty || placeholder.exists(_.nonEmpty) then el = el.cssClass("p-inputwrapper-filled")
        end if
        if isOpen then el = el.cssClass("p-select-open").cssClass("p-uic-overlay-anchor")
        if redFlag then el = el.cssClass("p-invalid").aria("invalid", "true")
        if disabledFlag then el = el.cssClass("p-disabled")
        if readonlyFlag then el = el.cssClass("p-uic-select-readonly").aria("readonly", "true")
        if requiredFlag then el = el.aria("required", "true")
        sizeV match
            case Size.Small  => el = el.cssClass("p-select-sm")
            case Size.Large  => el = el.cssClass("p-select-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then el = el.cssClass("p-variant-filled")
        if fluidFlag then el = el.cssClass("p-select-fluid")
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
        el = el.aria("haspopup", "listbox").aria("expanded", isOpen.toString)
        if interactive then
            el = el.tabIndex(0).preventScrollKeys
            // Trigger keys open the panel while CLOSED; while open the seeded panel
            // handles the keyboard and this bubble handler stands down.
            st.foreach { s =>
                el = el.onKeyDown { e =>
                    e.key match
                        case Keyboard.ArrowDown | Keyboard.Enter | Keyboard.Space if !s.isOpen => openPanel(s)
                        case _                                                                 => ()
                }
            }
            // Focus-loss on the trigger reports the currently bound key (or the resolved
            // current value when unbound) — the validation layer's Blur trigger.
            onBlurF.foreach { f =>
                el = el.onBlur(valueRef match
                    case Present(r) => r.use(f)
                    case Absent     => f(current))
            }
        end if

        FieldInvalid.withMessage(
            el(((labelUI :: clearUI) ++ (dropdownUI :: hiddenCarrier) ++ panelUI).map(toChild)*),
            redFlag,
            redMsg
        )
    end bodyStatic

    /** The floating option panel: Prime's `.p-select-overlay` skin on the
      * [[Overlay]] primitive, holding the optional filter header and the option
      * list.
      */
    private def overlayPanel(current: String, s: State, pick: A => State => Any < Async)(using Frame): UI =
        val shown =
            if filtering && s.qV.nonEmpty then
                items.filter(a => labelF(a).toLowerCase.contains(s.qV.toLowerCase))
            else items
        val hiEff = if shown.isEmpty then -1 else math.min(s.hiV, shown.size - 1)

        /** Next enabled index from `from` in direction `step` (no wrap — Prime). */
        def move(from: Int, step: Int): Int =
            var i = from + step
            while i >= 0 && i < shown.size && isOptionDisabled(shown(i)) do i += step
            if i >= 0 && i < shown.size then i else from
        end move

        val rows: List[UI] = shown.zipWithIndex.map { (a, i) =>
            val isSel = key(a) == current
            val isDis = isOptionDisabled(a)
            val checkSlot: List[UI] =
                if checkmarkFlag then
                    if isSel then List(GlyphSvg(Icons.check, "p-select-option-check-icon", "p-icon"))
                    else List(span.cssClass("p-select-option-blank-icon").cssClass("p-icon").aria("hidden", "true"))
                else Nil
            var row = li.cssClass("p-select-option").role("option").aria("selected", isSel.toString)
            if isSel then row = row.cssClass("p-select-option-selected")
            if isDis then row = row.cssClass("p-disabled").aria("disabled", "true")
            if i == hiEff then row = row.cssClass("p-focus")
            if !isDis then row = row.onClick(pick(a)(s))
            row((checkSlot :+ (span.cssClass("p-select-option-label")(labelF(a)): UI)).map(toChild)*)
        }
        val emptyRow: List[UI] =
            if shown.isEmpty then
                List(emptyMessageV.getOrElse(TextValue.Const("No results found")) match
                    case TextValue.Const(t) => li.cssClass("p-select-empty-message").role("presentation")(t): UI
                    case TextValue.Dyn(s)   => s.render(t => li.cssClass("p-select-empty-message").role("presentation")(t)))
            else Nil

        val listUI: UI =
            div.cssClass("p-select-list-container")(
                toChild(ul.cssClass("p-select-list").role("listbox")((rows ++ emptyRow).map(toChild)*))
            )

        // Prime wraps the filter in an IconField with a trailing search icon; the
        // sibling :not(:last-child) sheet rule pads the input for the icon.
        val header: List[UI] =
            if filtering then
                List(
                    div.cssClass("p-select-header")(
                        toChild(
                            div.cssClass("p-iconfield")(
                                toChild(
                                    input
                                        .cssClass("p-select-filter")
                                        .cssClass("p-inputtext")
                                        .cssClass("p-component")
                                        // The bound input renders inside a reactive wrapper span,
                                        // which resets sibling positions — the sheet's
                                        // :not(:last-child) padding cannot see the trailing icon,
                                        // so stamp the deterministic IconField padding class.
                                        .cssClass("p-uic-iconfield-end")
                                        .role("searchbox")
                                        .aria("label", "Filter")
                                        .value(s.q)
                                        // Typing re-filters; highlight the first match (Prime behavior).
                                        .onInput(_ => s.hi.set(0))
                                ),
                                toChild(
                                    span.cssClass("p-inputicon")(
                                        toChild(GlyphSvg(Icons.search, "p-icon"))
                                    )
                                )
                            )
                        )
                    )
                )
            else Nil

        Overlay(s.open)
            .portal(portalFlag)
            .scroll(scrollV)
            .panelClass("p-select-overlay")
            .panelClass("p-component")
            .onPanelKeyDown { e =>
                e.key match
                    case Keyboard.ArrowDown => s.hi.set(move(hiEff, 1))
                    case Keyboard.ArrowUp   => s.hi.set(move(hiEff, -1))
                    case Keyboard.Enter if hiEff >= 0 && hiEff < shown.size && !isOptionDisabled(shown(hiEff)) =>
                        pick(shown(hiEff))(s)
                    case _ => ()
            }((header ++ keyCollisionCard ++ List(listUI))*)
            .render
    end overlayPanel

    /** The loud card shown at the top of the panel when the option keys collide, so
      * a duplicate-label data set cannot silently select the wrong row (see
      * [[KeyDiagnostics]]). Empty in the healthy case, which is every existing
      * render.
      */
    private def keyCollisionCard(using Frame): List[UI] =
        val dups = KeyDiagnostics.duplicates(items.map(key))
        if dups.isEmpty then Nil
        else
            List(KeyDiagnostics.card(
                "Select",
                if keyF.isEmpty then
                    "option labels are not unique and optionKey is unset, so picks apply to the wrong option; set optionKey"
                else "optionKey is not unique across the options, so picks apply to the wrong option",
                dups
            ))
        end if
    end keyCollisionCard
end Select

object Select:
    def apply[A](): Select[A] = new Select[A](Nil, _.toString)
