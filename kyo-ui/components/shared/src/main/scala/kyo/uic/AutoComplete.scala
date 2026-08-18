package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.UI.Ast.HtmlChildVal

/** Suggestion filter variants — how the option list is matched against the typed
  * text. `StartsWithPerTerm` (the default) matches the start of any word,
  * `StartsWith` only the start of the whole text, `Contains` anywhere, and
  * `None` disables filtering (the caller pre-filters, e.g. server-side).
  */
enum FilterMode derives CanEqual:
    case StartsWithPerTerm, StartsWith, Contains, None

/** AutoComplete — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * AutoComplete anatomy: `div.p-autocomplete.p-component[.p-invalid]
  * [.p-autocomplete-fluid]` > `input.p-autocomplete-input.p-inputtext.p-component`
  * + optional `button.p-autocomplete-dropdown`, over the floating suggestion
  * panel `div.p-autocomplete-overlay.p-component` >
  * `div.p-autocomplete-list-container` > `ul.p-autocomplete-list[role=listbox]`
  * of `li.p-autocomplete-option[role=option]` rows plus an optional
  * `li.p-autocomplete-empty-message`), so the extracted `@primeuix` autocomplete
  * CSS applies verbatim.
  *
  * The suggestion panel is the REAL floating panel, built on the [[Overlay]]
  * primitive with `seedFocus(false)` — focus STAYS in the input the whole time.
  * Typing opens it (once the text reaches `minQueryLength`, default 1); a pick,
  * Escape, or an outside click closes it. The keyboard lives on the FIELD:
  * ArrowDown opens the panel / moves Prime's `.p-focus` highlight down, ArrowUp
  * moves it up, Enter picks the highlighted suggestion, Escape closes.
  * `dropdown(true)` renders Prime's dropdown trigger button, which opens the
  * FULL (unfiltered) list. Because the Overlay backdrop covers the page while
  * open, a click back into the input closes the panel first (typing reopens it).
  *
  * Options are TYPED like [[Select]]: `options(items)(label)` with `optionKey`
  * defaulting to the label projection; `itemTemplate` renders arbitrary UI per
  * suggestion row (the old `additionalText` column is expressed through it).
  * `value(ref)` binds the raw TEXT two-way — typing and picks write back, ref
  * writes update the field and re-filter the list. The interaction state
  * (open/highlight/show-all) lives in signals allocated by an effectful
  * `UI.mounted` region; static projections (SSG, the SSR page HTML) render the
  * same closed anatomy inert until the client transport attaches.
  *
  * Honest deferrals: multiple selection (chips), option groups, forceSelection,
  * and virtual scrolling are not implemented yet. Arrow keys on the field no
  * longer scroll the page underneath — the field declares `preventScrollKeys`
  * (vertical keys navigate; the caret still moves with Left/Right/Home/End).
  */
final case class AutoComplete[A] private (
    items: List[A],
    labelF: A => String,
    keyF: Maybe[A => String] = Absent,
    itemTemplateF: Maybe[A => UI] = Absent,
    valueBinding: Maybe[Input.Value] = Absent,
    filterV: FilterMode = FilterMode.StartsWithPerTerm,
    minQueryLengthV: Int = 1,
    emptyMessageV: Maybe[TextValue] = Absent,
    placeholderText: Maybe[TextValue] = Absent,
    loadingV: Maybe[BoolValue] = Absent,
    showClearFlag: Boolean = false,
    dropdownFlag: Boolean = false,
    disabledFlag: Boolean = false,
    readonlyFlag: Boolean = false,
    requiredFlag: Boolean = false,
    nameV: Maybe[String] = Absent,
    sizeV: Size = Size.Normal,
    variantV: FieldVariant = FieldVariant.Outlined,
    fluidFlag: Boolean = false,
    invalidV: Maybe[BoolValue] = Absent,
    invalidMsgV: Maybe[String] = Absent,
    invalidMsgDynV: Maybe[Signal[Maybe[String]]] = Absent,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    onInputF: Maybe[String => Any < Async] = Absent,
    onChangeF: Maybe[String => Any < Async] = Absent,
    onSelectF: Maybe[A => Any < Async] = Absent,
    fieldExtraClassesV: List[String] = Nil,
    idV: Maybe[String] = Absent,
    onBlurF: Maybe[String => Any < Async] = Absent
) extends Node, TextFormControl:
    type Self = AutoComplete[A]

    /** Package-internal class hook: wrappers (FloatLabel) stamp Prime's state
      * classes (`p-filled`) onto the inner text field.
      */
    private[uic] def fieldExtraClass(cls: String): AutoComplete[A] =
        copy(fieldExtraClassesV = fieldExtraClassesV :+ cls)

    /** Native `id` on the inner text input — pair with `Label.forId` /
      * `FloatLabel.forId`.
      */
    def id(v: String): AutoComplete[A] = copy(idV = Present(v))

    /** Appends typed options with their text projection. */
    def options(is: Seq[A])(label: A => String): AutoComplete[A] =
        copy(items = items ++ is.toList, labelF = label)

    /** Convenience for plain string options (label = identity). */
    def options(is: Seq[String])(using ev: String =:= A): AutoComplete[A] =
        copy(items = items ++ is.map(ev).toList, labelF = a => ev.flip(a))

    /** Stable per-option key (defaults to the label projection). */
    def optionKey(f: A => String): AutoComplete[A] = copy(keyF = Present(f))

    /** Custom suggestion-row content, replacing the plain label text (the way to
      * render secondary columns, icons, highlights, ...).
      */
    def itemTemplate(f: A => UI): AutoComplete[A] = copy(itemTemplateF = Present(f))

    /** Sets a constant text value. */
    def value(v: String): AutoComplete[A] = copy(valueBinding = Present(Input.Value.Const(v)))

    /** Binds the raw text two-way to `ref`: typing and option picks write back into
      * the ref, ref changes update the field and re-filter the suggestions.
      */
    def value(ref: SignalRef[String]): AutoComplete[A] = copy(valueBinding = Present(Input.Value.Ref(ref)))

    /** How typed text is MATCHED against the option list (default:
      * `StartsWithPerTerm`). This is the matching strategy, not an on/off switch
      * and not a query binding: an AutoComplete filters by construction, so
      * `filterable` and `filterQuery` (the other two pickers' words) have nothing
      * to mean here. Pass `FilterMode.None` when the caller pre-filters.
      */
    def filterMode(v: FilterMode): AutoComplete[A] = copy(filterV = v)

    /** Minimum number of typed characters before typing opens the suggestion
      * panel (Prime's `minLength`; default 1). The dropdown trigger ignores it.
      */
    def minQueryLength(n: Int): AutoComplete[A] = copy(minQueryLengthV = math.max(0, n))

    /** Text of the `li.p-autocomplete-empty-message` row when the query matches no
      * option; without it the panel is suppressed instead.
      */
    def emptyMessage(v: String): AutoComplete[A] = copy(emptyMessageV = Present(TextValue.Const(v)))

    /** Reactive variant — re-renders the empty message in place on signal emission. */
    def emptyMessage(sig: Signal[String]): AutoComplete[A] = copy(emptyMessageV = Present(TextValue.Dyn(sig)))

    def placeholder(v: String): AutoComplete[A] = copy(placeholderText = Present(TextValue.Const(v)))

    /** Reactive placeholder that tracks `sig` — patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render/re-mount of the field) on each emission.
      */
    def placeholder(sig: Signal[String]): AutoComplete[A] = copy(placeholderText = Present(TextValue.Dyn(sig)))

    /** Busy state — renders Prime's spinning `span.p-autocomplete-loader` inside
      * the field while suggestions are being fetched.
      */
    def loading(v: Boolean): AutoComplete[A] = copy(loadingV = Present(BoolValue.Const(v)))

    /** Reactive busy state — bind to the suggestion-fetch in-flight signal; only the in-field loader
      * spinner toggles in its own sub-region on emission (no re-render of the field).
      */
    def loading(sig: Signal[Boolean]): AutoComplete[A] = copy(loadingV = Present(BoolValue.Dyn(sig)))

    /** Renders Prime's `.p-autocomplete-clear-icon` button inside the field
      * (visible while the bound text is non-empty) that resets the ref to `""`.
      */
    def showClear(v: Boolean): AutoComplete[A] = copy(showClearFlag = v)

    /** Renders Prime's dropdown trigger (`button.p-autocomplete-dropdown`) fused
      * to the field's end — clicking it toggles the panel with the FULL,
      * unfiltered option list (Prime's `dropdown` prop).
      */
    def dropdown(v: Boolean): AutoComplete[A] = copy(dropdownFlag = v)

    def disabled(v: Boolean): AutoComplete[A] = copy(disabledFlag = v)

    /** Native `readonly` on the text field; the suggestion panel is suppressed so
      * picks cannot mutate the value either.
      */
    def readonly(v: Boolean): AutoComplete[A] = copy(readonlyFlag = v)

    /** Marks the field required (`aria-required` + native constraint). */
    def required(v: Boolean): AutoComplete[A] = copy(requiredFlag = v)

    /** Native `name` for HTML form participation. */
    def name(v: String): AutoComplete[A] = copy(nameV = Present(v))

    /** Size of the field: `.p-inputtext-sm` / default / `.p-inputtext-lg`. */
    def size(v: Size): AutoComplete[A] = copy(sizeV = v)

    /** Fill variant — `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): AutoComplete[A] = copy(variantV = v)

    /** Spans the full width of its container (`.p-autocomplete-fluid`). */
    def fluid(v: Boolean): AutoComplete[A] = copy(fluidFlag = v)

    /** Marks the field invalid (`.p-invalid` + `aria-invalid`). */
    def invalid(v: Boolean): AutoComplete[A] = copy(invalidV = Present(BoolValue.Const(v)))

    /** Message rendered below the field while `invalid(true)` (kyo extension —
      * `div.p-uic-invalid-message`).
      */
    def invalidMessage(v: String): AutoComplete[A] = copy(invalidMsgV = Present(v))

    /** Reactive validity: the bound signal toggles `.p-invalid` + `aria-invalid` in
      * place. Explicit override of the message-derived red default.
      */
    def invalid(sig: Signal[Boolean]): AutoComplete[A] = copy(invalidV = Present(BoolValue.Dyn(sig)))

    /** Reactive invalid message — `Present` shows the row and (by default) turns the
      * field red; `Absent` clears both. Re-renders in place on emission.
      */
    def invalidMessage(sig: Signal[Maybe[String]]): AutoComplete[A] = copy(invalidMsgDynV = Present(sig))

    /** Accessible name → `aria-label`. */
    def accessibleName(v: String): AutoComplete[A] = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): AutoComplete[A] = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Accessible name reference → `aria-labelledby`. */
    def accessibleNameRef(v: String): AutoComplete[A] = copy(accNameRefV = Present(v))

    /** Fired on every keystroke with the current text. */
    def onInput(f: String => Any < Async): AutoComplete[A] = copy(onInputF = Present(f))

    /** Fired when the field commits (blur/Enter) or an option is picked — with the
      * committed text (a pick commits the option's label).
      */
    def onChange(f: String => Any < Async): AutoComplete[A] = copy(onChangeF = Present(f))

    /** Fired with the picked option after its label is written into the ref. */
    def onSelect(f: A => Any < Async): AutoComplete[A] = copy(onSelectF = Present(f))

    /** Fires on focus loss (native `blur`) with the field's current text — unlike
      * `onChange` it fires even when the value was not edited. The form-validation
      * layer wires its `Blur` trigger here.
      */
    def onBlur(f: String => Any < Async): AutoComplete[A] = copy(onBlurF = Present(f))

    private def interactive: Boolean = !disabledFlag && !readonlyFlag

    /** The wired interaction state, allocated per mount: panel visibility, the
      * keyboard highlight index, and the dropdown's show-the-full-list flag.
      */
    final private case class State(
        open: SignalRef[Boolean],
        isOpen: Boolean,
        hi: SignalRef[Int],
        hiV: Int,
        all: SignalRef[Boolean],
        allV: Boolean
    )

    private[uic] def render(using Frame): UI =
        // The interaction state (open/highlight/show-all) lives in signals allocated
        // by this effectful mount. Static projections (SSG, the SSR page GET,
        // initial render) show the placeholder — the same closed anatomy, inert —
        // until the transport attaches and the mount publishes the wired tree.
        val stat: UI = withText(cur => body(cur, Absent))
        if !interactive then stat
        else
            UI.mounted {
                for
                    open <- Signal.initRef(false)
                    hi   <- Signal.initRef(-1)
                    all  <- Signal.initRef(false)
                yield wired(open, hi, all)
            }.placeholder(stat)
        end if
    end render

    /** The subscription tree the mount publishes — the seam golden tests render
      * directly (a full top-down re-render shows mounted regions as placeholders,
      * so the wired anatomy is only reachable here).
      */
    private[uic] def wired(open: SignalRef[Boolean], hi: SignalRef[Int], all: SignalRef[Boolean])(using Frame): UI =
        open.render { o =>
            hi.render { h =>
                all.render { a =>
                    withText(cur => body(cur, Present(State(open, o, hi, h, all, a))))
                }
            }
        }

    /** Renders through the text binding: a bound ref subscribes, constants and
      * unbound fields render with a fixed query.
      */
    private def withText(f: String => UI)(using Frame): UI =
        valueBinding match
            case Present(Input.Value.Ref(ref)) => ref.render(f)
            case Present(Input.Value.Const(v)) => f(v)
            case Absent                        => f("")

    /** Applies the configured [[FilterMode]] to one option; matching is
      * case-insensitive on the option label.
      */
    private def matchesFilter(a: A, q: String): Boolean =
        val t = labelF(a).toLowerCase
        filterV match
            case FilterMode.StartsWithPerTerm => t.split("\\s+").exists(_.startsWith(q))
            case FilterMode.StartsWith        => t.startsWith(q)
            case FilterMode.Contains          => t.contains(q)
            case FilterMode.None              => true
        end match
    end matchesFilter

    private def body(query: String, st: Maybe[State])(using Frame): UI =
        // Reactive-invalid gate (INSIDE the mount subscription — never around the
        // UI.mounted node). `State` is a path-dependent inner type, so the reactive
        // re-render threads the resolved (red, message) into `bodyStatic` directly
        // rather than through `copy(...)` (a copied instance's `State` would not match).
        (invalidV.dynSig, invalidMsgDynV) match
            case (Absent, Absent) => bodyStatic(query, st, invalidV.constTrue, invalidMsgV)
            case _ => FieldInvalid.reactive(invalidV.dynSig, invalidMsgDynV, invalidMsgV)((red, msg) => bodyStatic(query, st, red, msg))

    private def bodyStatic(query: String, st: Maybe[State], redFlag: Boolean, redMsg: Maybe[String])(using Frame): UI =
        // The suggestion list the panel and the field's keyboard share: the full
        // list on a dropdown-trigger open, the filtered list otherwise.
        val visible: List[A] =
            if st.exists(_.allV) then items
            else items.filter(a => matchesFilter(a, query.toLowerCase))
        // Typing gates on minQueryLength; the dropdown trigger shows the full list
        // regardless. The panel is suppressed with nothing to show and no message.
        val panelShown: Boolean = st.exists { s =>
            s.isOpen && (s.allV || query.length >= minQueryLengthV) &&
            (visible.nonEmpty || emptyMessageV.isDefined)
        }

        // Writes the picked label, fires onSelect/onChange, closes the panel.
        def pick(a: A)(s: State): Any < Async =
            val text = labelF(a)
            val setValue: Any < Async = valueBinding match
                case Present(Input.Value.Ref(r)) => r.set(text)
                case _                           => ()
            val fireSelect: Any < Async = onSelectF match
                case Present(f) => f(a)
                case Absent     => ()
            val fireChange: Any < Async = onChangeF match
                case Present(f) => f(text)
                case Absent     => ()
            for
                _ <- setValue
                _ <- fireSelect
                _ <- fireChange
                _ <- s.hi.set(-1)
                r <- s.open.set(false)
            yield r
            end for
        end pick

        // === text field ==========================================================
        var f = input.cssClass("p-autocomplete-input").cssClass("p-inputtext").cssClass("p-component")
        fieldExtraClassesV.foreach(c => f = f.cssClass(c))
        idV.foreach(v => f = f.id(v))
        sizeV match
            case Size.Small  => f = f.cssClass("p-inputtext-sm")
            case Size.Large  => f = f.cssClass("p-inputtext-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then f = f.cssClass("p-variant-filled")
        if redFlag then f = f.cssClass("p-invalid").aria("invalid", "true")
        valueBinding match
            case Present(Input.Value.Const(v)) => f = f.value(v)
            case Present(Input.Value.Ref(r))   => f = f.value(r)
            case Absent                        => ()
        end match
        f = placeholderText match
            case Present(TextValue.Const(v)) => f.placeholder(v)
            case Present(TextValue.Dyn(sig)) => f.placeholder(sig)
            case Absent                      => f
        if disabledFlag then f = f.disabled(true)
        if readonlyFlag then f = f.readOnly(true)
        if requiredFlag then f = f.aria("required", "true").jsProp("required", "true")
        f = nameV.map(v => f.jsProp("name", v)).getOrElse(f)
        f = accNameV match
            case Present(TextValue.Const(v)) => f.aria("label", v)
            case Present(TextValue.Dyn(s))   => f.aria("label", s)
            case Absent                      => f
        f = accNameRefV.map(v => f.aria("labelledby", v)).getOrElse(f)
        f = f.aria("haspopup", "listbox").aria("expanded", panelShown.toString)
        st.foreach { s =>
            // Typing opens the panel on the filtered list and resets the highlight.
            f = f.onInput { v =>
                val user: Any < Async = onInputF match
                    case Present(g) => g(v)
                    case Absent     => ()
                for
                    _ <- s.all.set(false)
                    _ <- s.hi.set(-1)
                    _ <- s.open.set(true)
                    r <- user
                yield r
                end for
            }
            // The keyboard stays on the FIELD (the panel never takes focus):
            // ArrowDown opens / moves the highlight down, ArrowUp up (no wrap),
            // Enter picks the highlighted suggestion, Escape closes.
            f = f.preventScrollKeys.onKeyDown { e =>
                e.key match
                    case Keyboard.ArrowDown if !s.isOpen => s.open.set(true)
                    case Keyboard.ArrowDown if panelShown =>
                        s.hi.set(math.min(s.hiV + 1, visible.size - 1))
                    case Keyboard.ArrowUp if panelShown =>
                        s.hi.set(math.max(s.hiV - 1, 0))
                    case Keyboard.Enter if panelShown && s.hiV >= 0 && s.hiV < visible.size =>
                        pick(visible(s.hiV))(s)
                    case Keyboard.Escape if s.isOpen => s.open.set(false)
                    case _                           => ()
            }
            // Native onBlur carries no payload → read the current text from the bound ref
            // (constant/unbound fall back to the fixed/empty value).
            onBlurF.foreach { g =>
                f = f.onBlur(valueBinding match
                    case Present(Input.Value.Ref(r))   => r.use(g)
                    case Present(Input.Value.Const(v)) => g(v)
                    case Absent                        => g(""))
            }
        }
        val field: UI = onChangeF.map(f.onChange(_)).getOrElse(f)

        // === in-field affordances ================================================
        def loaderSpan: UI =
            span.cssClass("p-autocomplete-loader")(
                toChild(GlyphSvg(Icons.spinner, "p-icon", "p-icon-spin"))
            )
        val loader: List[UI] = loadingV match
            case Present(BoolValue.Const(true))  => List(loaderSpan)
            case Present(BoolValue.Dyn(sig))     => List(sig.render(b => if b then loaderSpan else UI.empty))
            case Present(BoolValue.Const(false)) => Nil
            case Absent                          => Nil

        val clear: List[UI] = valueBinding match
            case Present(Input.Value.Ref(r)) if showClearFlag && interactive && query.nonEmpty =>
                List(
                    button
                        .cssClass("p-autocomplete-clear-icon")
                        .jsProp("type", "button")
                        .aria("label", "Clear")
                        .onClick(r.set(""))(toChild(GlyphSvg(Icons.times, "p-icon")))
                )
            case _ => Nil

        // The dropdown trigger opens the FULL list (Prime's dropdown prop);
        // re-clicking while open lands on the Overlay backdrop and closes.
        val dropdownUI: List[UI] =
            if dropdownFlag then
                var dd = button
                    .cssClass("p-autocomplete-dropdown")
                    .jsProp("type", "button")
                    .aria("label", "Show options")
                st match
                    case Present(s) =>
                        dd = dd.onClick {
                            for
                                _ <- s.all.set(true)
                                _ <- s.hi.set(-1)
                                r <- s.open.set(true)
                            yield r
                        }
                    case Absent => if !interactive then dd = dd.disabled(true)
                end match
                List(dd(toChild(GlyphSvg(Icons.chevronDown, "p-icon"))))
            else Nil

        // === floating panel ======================================================
        val panel: List[UI] =
            st.toList.filter(_ => panelShown).map(s => overlayPanel(visible, s, pick))

        // === field root ==========================================================
        var el = div.cssClass("p-autocomplete").cssClass("p-component")
        if redFlag then el = el.cssClass("p-invalid")
        if disabledFlag then el = el.cssClass("p-disabled")
        if fluidFlag then el = el.cssClass("p-autocomplete-fluid")
        if panelShown then el = el.cssClass("p-uic-overlay-anchor")
        val root = el(((field :: loader) ++ clear ++ dropdownUI ++ panel).map(toChild)*)
        FieldInvalid.withMessage(root, redFlag, redMsg)
    end bodyStatic

    /** The floating suggestion panel: Prime's `.p-autocomplete-overlay` skin on
      * the [[Overlay]] primitive — `seedFocus(false)`, focus stays in the input;
      * Escape is handled by the field, outside clicks by the backdrop.
      */
    private def overlayPanel(visible: List[A], s: State, pick: A => State => Any < Async)(using Frame): UI =
        val hiEff = if visible.isEmpty then -1 else math.min(s.hiV, visible.size - 1)
        val rows: List[UI] = visible.zipWithIndex.map { (a, i) =>
            val content: HtmlChildVal = itemTemplateF match
                case Present(t) => toChild(t(a))
                case Absent     => toChild(stringToUI(labelF(a)))
            var row = li
                .cssClass("p-autocomplete-option")
                .role("option")
                .data("uic-option-key", keyF.getOrElse(labelF)(a))
            if i == hiEff then row = row.cssClass("p-focus")
            row.onClick(pick(a)(s))(content)
        }
        val emptyRow: List[UI] =
            if visible.isEmpty then
                emptyMessageV.toList.map {
                    case TextValue.Const(t) => li.cssClass("p-autocomplete-empty-message").role("presentation")(t): UI
                    case TextValue.Dyn(s)   => s.render(t => li.cssClass("p-autocomplete-empty-message").role("presentation")(t))
                }
            else Nil
        val listUI: UI =
            div.cssClass("p-autocomplete-list-container")(
                toChild(ul.cssClass("p-autocomplete-list").role("listbox")((rows ++ emptyRow).map(toChild)*))
            )
        Overlay(s.open)
            .panelClass("p-autocomplete-overlay")
            .panelClass("p-component")
            .seedFocus(false)
            .dismissOnEscape(false)(listUI)
            .render
    end overlayPanel
end AutoComplete

object AutoComplete:
    def apply[A](): AutoComplete[A] = new AutoComplete[A](Nil, _.toString)
