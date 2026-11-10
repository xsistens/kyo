package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.UI.Ast.HtmlChildVal

/** AutoComplete: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
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
  * primitive with `seedFocus(false)`: focus STAYS in the input the whole time.
  * Typing opens it (once the text reaches `minQueryLength`, default 1); a pick,
  * Escape, or an outside click closes it. The keyboard lives on the FIELD:
  * ArrowDown opens the panel / moves Prime's `.p-focus` highlight down, ArrowUp
  * moves it up, Enter picks the highlighted suggestion, Escape closes.
  * `dropdown(true)` renders Prime's dropdown trigger button, which opens the
  * FULL (unfiltered) list. Because the Overlay backdrop covers the page while
  * open, a click back into the input closes the panel first (typing reopens it).
  *
  * Options are TYPED like [[Select]]: `options(items)(label)` with `optionKey`
  * defaulting to the label projection, or `optionGroups(...)` for
  * [[OptionItem]] rows under labelled headers; `itemTemplate` renders arbitrary UI per
  * suggestion row (the old `additionalText` column is expressed through it).
  * `value(ref)` binds the raw TEXT two-way: typing and picks write back, ref
  * writes update the field and re-filter the list. The interaction state
  * (open/highlight/show-all) lives in signals allocated by an effectful
  * `UI.mounted` region, which draws in the render pass: a static page shows the real field.
  *
  * Honest deferrals: multiple selection (chips), forceSelection, and virtual
  * scrolling are not implemented yet. Arrow keys on the field no longer scroll
  * the page underneath: the field declares `preventScrollKeys` (vertical keys
  * navigate; the caret still moves with Left/Right/Home/End).
  */
final class AutoComplete[A] private[uic] (private[uic] val p: AutoComplete.Props[A]) extends Node, FormControl[String], HasEmptyContent,
      HasPlaceholder, HasAccessibleNameRef, HasProps:
    import p.*

    type Self = AutoComplete[A]

    /** Package-internal: a wrapper (FloatLabel) asks for Prime's `p-filled` on the inner text field while it holds text. It
      * is stamped inside this control's own region, from the text it already reads, so the wrapper needs no region of its
      * own around the mount (a region there rebuilds the mount, and the open panel with it, when the field fills).
      */
    private[uic] def stampFilled: AutoComplete[A] = new AutoComplete(p.copy(stampFilledFlag = true))

    // The options are compared as a list where they are read as a signal; the rows are matched by key, never by `==`.
    private given CanEqual[A, A] = CanEqual.derived

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new AutoComplete(p.copy(access = v))

    /** Appends typed options with their text projection: a collection, or a signal of suggestions for a list that
      * changes while the field is shown (a server's answer to the query, say), read in the field's own region.
      */
    def options(is: Iterable[A] | Signal[Seq[A]])(label: A => String): AutoComplete[A] =
        new AutoComplete(p.copy(optionSources = optionSources :+ OptionSource.of(is), labelF = label))

    /** Convenience for plain string options (label = identity), a collection or a signal. */
    def options(is: Iterable[String] | Signal[Seq[String]])(using ev: String =:= A): AutoComplete[A] =
        new AutoComplete(p.copy(
            optionSources = optionSources :+ OptionSource.of(ev.substituteCo[[X] =>> Iterable[X] | Signal[Seq[X]]](is)),
            labelF = a => ev.flip(a)
        ))

    /** Appends grouped suggestions: [[OptionItem.group]] rows render their
      * options under a labelled header, [[OptionItem.item]] rows beside them.
      *
      * A separate name rather than an `options` overload, because both would
      * erase to the same signature. Grouping changes the panel only, and a query
      * that empties a group removes its header with it.
      */
    def optionGroups(is: Iterable[OptionItem[A]] | Signal[Seq[OptionItem[A]]])(label: A => String): AutoComplete[A] =
        new AutoComplete(p.copy(optionSources = optionSources :+ OptionSource.rows(is), labelF = label))

    /** Stable per-option key (defaults to the label projection). */
    def optionKey(f: A => String): AutoComplete[A] = new AutoComplete(p.copy(keyF = Present(f)))

    /** Custom suggestion-row content, replacing the plain label text (the way to
      * render secondary columns, icons, highlights, ...).
      */
    def itemTemplate(f: A => UI): AutoComplete[A] = new AutoComplete(p.copy(itemTemplateF = Present(f)))

    /** The raw text. A `SignalRef` binds TWO-WAY: typing, option picks and a clear write back into the ref, and ref
      * changes update the field and re-filter the suggestions. Any other signal binds ONE-WAY: the field shows it, and an
      * edit writes nothing and only reports: typing through [[onInput]], a pick through [[onChange]] and [[onSelect]], a
      * clear through [[onChange]]. A card names each edit nothing hears. A constant is the starting text: unbound or from
      * a constant, the field keeps the text itself, so the suggestions follow what the reader types, a pick shows its
      * label and the clear button empties it. `ref.readOnly` opts a ref out.
      */
    def value(v: String | Signal[String]): AutoComplete[A] = new AutoComplete(p.copy(valueSlot = valueSlot.bind(v)))

    /** How a suggestion matches the typed text, as `(option, text) => Boolean` with the text as typed. [[Filter]]
      * holds the ready-made ones: `filter(Filter.contains(_.name, _.email))` suggests an option by either text, and
      * `Filter.all` suggests every option, for a caller that pre-filters (server-side, say). Without it the match is
      * `Filter.startsWithPerTerm` on the label projection: the start of any word, ignoring case. This is the matching
      * strategy alone: an AutoComplete filters by construction and its text is the query, so `filterable` and
      * `filterQuery` (the other pickers' words) have nothing to mean here.
      */
    def filter(f: (A, String) => Boolean): AutoComplete[A] = new AutoComplete(p.copy(filterF = Present(f)))

    /** Minimum number of typed characters before typing opens the suggestion
      * panel (Prime's `minLength`; default 1). The dropdown trigger ignores it.
      */
    def minQueryLength(n: Int): AutoComplete[A] = new AutoComplete(p.copy(minQueryLengthV = math.max(0, n)))

    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): AutoComplete[A] = new AutoComplete(p.copy(emptyContentV = v))

    private[uic] def withPlaceholder(v: Maybe[Prop[String]]): AutoComplete[A] = new AutoComplete(p.copy(placeholderText = v))

    /** Busy state: renders Prime's spinning `span.p-autocomplete-loader` inside
      * the field while suggestions are being fetched. Bind a `Signal[Boolean]` to the
      * suggestion-fetch in-flight signal; only the in-field loader spinner toggles in its
      * own sub-region on emission (no re-render of the field).
      */
    def loading(v: Boolean | Signal[Boolean]): AutoComplete[A] = new AutoComplete(p.copy(loadingV = Present(ReactiveValue(v))))

    /** Renders Prime's `.p-autocomplete-clear-icon` button inside the field
      * (visible while the bound text is non-empty) that clears it: it writes `""` where the text binds two-way, and
      * reports the clear to [[onChange]] either way.
      */
    def showClear(v: Boolean): AutoComplete[A] = new AutoComplete(p.copy(showClearFlag = v))

    /** Renders Prime's dropdown trigger (`button.p-autocomplete-dropdown`) fused
      * to the field's end; clicking it toggles the panel with the FULL,
      * unfiltered option list (Prime's `dropdown` prop).
      */
    def dropdown(v: Boolean): AutoComplete[A] = new AutoComplete(p.copy(dropdownFlag = v))

    def disabled(v: Boolean): AutoComplete[A] = new AutoComplete(p.copy(disabledFlag = v))

    /** Native `readonly` on the text field; the suggestion panel is suppressed so
      * picks cannot mutate the value either.
      */
    def readonly(v: Boolean): AutoComplete[A] = new AutoComplete(p.copy(readonlyFlag = v))

    /** Marks the field required (`aria-required` + native constraint). */
    def required(v: Boolean): AutoComplete[A] = new AutoComplete(p.copy(requiredFlag = v))

    /** Native `name` for HTML form participation. */
    def name(v: String): AutoComplete[A] = new AutoComplete(p.copy(nameV = Present(v)))

    /** Size of the field: `.p-inputtext-sm` / default / `.p-inputtext-lg`. */
    def size(v: Size): AutoComplete[A] = new AutoComplete(p.copy(sizeV = v))

    /** Fill variant: `Filled` renders `.p-variant-filled`; `Outlined` is the default. */
    def variant(v: FieldVariant): AutoComplete[A] = new AutoComplete(p.copy(variantV = v))

    /** Spans the full width of its container (`.p-autocomplete-fluid`). */
    def fluid(v: Boolean): AutoComplete[A] = new AutoComplete(p.copy(fluidFlag = v))

    private[uic] def validitySlots: FieldValidity.Slots                             = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self                     = new AutoComplete(p.copy(validityV = v))
    private[uic] def bindRef(ref: SignalRef[String])(using Frame): AutoComplete[A]  = value(ref)
    private[uic] def hearBlur(f: String => UI.Action)(using Frame): AutoComplete[A] = onBlur(f)

    /** Fired on every keystroke with the current text. */
    def onInput(f: String => UI.Action)(using Frame): AutoComplete[A] = new AutoComplete(p.copy(onInputF = Present(Callback.of(f))))

    /** Fired when the field commits (blur/Enter) or an option is picked, with the
      * committed text (a pick commits the option's label).
      */
    def onChange(f: String => UI.Action)(using Frame): AutoComplete[A] = new AutoComplete(p.copy(valueSlot = valueSlot.hear(f)))

    /** Fired with the picked option after its label is written into the ref. */
    def onSelect(f: A => UI.Action)(using Frame): AutoComplete[A] = new AutoComplete(p.copy(onSelectF = Present(Callback.of(f))))

    /** Fires on focus loss (native `blur`) with the text the field holds as focus
      * leaves it, what the reader typed or picked. Unlike `onChange` it fires even
      * when the value was not edited. The form-validation layer wires its `Blur`
      * trigger here.
      */
    def onBlur(f: String => UI.Action)(using Frame): AutoComplete[A] = new AutoComplete(p.copy(onBlurF = Present(Callback.of(f))))

    private def interactive: Boolean = !disabledFlag && !readonlyFlag

    /** The wired interaction state, allocated per mount: panel visibility, the
      * keyboard highlight index, and the dropdown's show-the-full-list flag.
      */
    final private case class Wired(
        open: SignalRef[Boolean],
        isOpen: Boolean,
        hi: SignalRef[Int],
        hiV: Int,
        all: SignalRef[Boolean],
        allV: Boolean,
        /** The id the highlight is announced through, as the rest of the combobox family carries
          * it: a caller's own `id` wins, and a minted one stands in where there is none. It is the
          * FIELD's id too, which is what the dropdown trigger hands focus back to.
          */
        idBase: Maybe[String],
        /** Moves DOM focus to an id. The dropdown trigger is the only caller: it is its own tab
          * stop, and every key this component answers is answered by the field.
          */
        focus: String => UI.Action
    )

    private def optionId(base: String, index: Int): String = s"$base-option-$index"

    /** The id of the suggestion list, which is what the combobox points `aria-controls` at. */
    private def listId(base: String): String = s"$base-list"

    /** Where the highlight lands when the panel opens: on the option the field's current text
      * already names, and on the first one otherwise.
      *
      * Opening onto nothing is the shape a reader cannot act on. They pressed a key to see the
      * list, and the next key they press should pick from it rather than only start moving.
      */
    private def openHighlight(shown: Seq[A], text: String): Int =
        val at = shown.indexWhere(a => labelF(a) == text)
        if at >= 0 then at else if shown.isEmpty then -1 else 0

    /** One key over the FIELD, which is where this combobox's keyboard lives: the panel never
      * takes focus, so every key arrives here.
      *
      * That is also what limits it. The field is a text box, so Home, End, Space and every
      * printable key belong to the caret, and [[ListNav]] is read for the two vertical arrows
      * alone. There is no typeahead for the same reason: the reader is already typing, and what
      * they type filters.
      *
      * ArrowDown on a closed panel opens it and lands on an option, which is the combobox pattern
      * and the one case ListNav cannot express, since a closed panel has no highlight to move.
      */
    private def fieldKey(visible: List[A], text: String, panelShown: Boolean, s: Wired, pick: A => Wired => UI.Action)(
        e: KeyboardEvent
    )(using Frame): UI.Action =
        val navigable = visible.indices.toList
        val hiEff     = if visible.isEmpty then -1 else math.min(s.hiV, visible.size - 1)
        e.key match
            case Keyboard.ArrowDown if !s.isOpen =>
                // Opening lands ON an option, the same one the dropdown trigger lands on, so the
                // next key picks rather than only starting to move.
                s.hi.set(openHighlight(visible, text)).andThen(s.open.set(true))
            case Keyboard.ArrowUp | Keyboard.ArrowDown if panelShown =>
                ListNav.onKey(navigable, hiEff, e.key, wrap = false) match
                    case Present(step) => s.hi.set(step.focus)
                    case Absent        => ()
            case Keyboard.Enter if panelShown && hiEff >= 0 && visible.isDefinedAt(hiEff) => pick(visible(hiEff))(s)
            // Escape closes, and so does Tab: the panel's keyboard lives on this field, the one
            // the Tab is carrying the reader away from, so a panel left open behind them is one
            // nothing answers.
            case Keyboard.Escape | Keyboard.Tab if s.isOpen => s.open.set(false)
            case _                                          => ()
        end match
    end fieldKey

    private[uic] def render(using Frame): UI =
        // Unbound or started from a constant, the text is the field's own: the suggestions filter by it, a pick writes its
        // label into it and a clear empties it, and a field drawn with a constant would be given the constant back by the
        // render that typing's open causes. A ref the field keeps for as long as it is shown holds it, beside the interaction state
        // (open/highlight/show-all) the mount allocates.
        if !interactive then
            val validity = FieldValidity(validityV)
            validity.follow(OptionItem.reading(optionSources).and(text).render((opts, cur) =>
                new AutoComplete(p.copy(optionsV = opts)).body(cur, Absent, validity)
            ))
        else
            val field = UI.mounted {
                for
                    value <- valueSlot.ownedFrom("")
                    base  <- access.id.map(v => Kyo.lift(v)).getOrElse(UI.id)
                    open  <- Signal.initRef(false)
                    hi    <- Signal.initRef(-1)
                    all   <- Signal.initRef(false)
                yield new AutoComplete(p.copy(valueSlot = value)).wired(open, hi, all, Present(base), focusById)
            }
            Diagnostics.after(field)("AutoComplete")(
                Diagnostic.naming(
                    "value is bound one way and an edit reaches nobody, so the reader cannot make it; add the event, or bind a SignalRef",
                    unheardEdits
                )
            )
        end if
    end render

    /** The edits of a one-way text that reach nobody, each with the event that would hear it. They report through
      * different events: typing through `onInput` (without it the suggestions keep filtering the text the owner holds),
      * a pick through `onChange` or `onSelect`, and a clear through `onChange` alone. Empty for a two-way ref, a constant
      * (the starting text, which stays in the field) and an unbound field.
      */
    private def unheardEdits: List[String] =
        if !valueSlot.binding.exists(v => v.dyn.isDefined && v.writable.isEmpty) then Nil
        else
            (if onInputF.isEmpty then List("typing: add onInput") else Nil) ++
                (if valueSlot.hearer.isEmpty && onSelectF.isEmpty then List("a pick: add onChange or onSelect") else Nil) ++
                (if showClearFlag && valueSlot.hearer.isEmpty then List("a clear: add onChange") else Nil)

    /** The tree the mount draws: the seam a test renders directly to hand it refs of its own
      * (a full top-down render allocates its own,
      * so the wired anatomy is only reachable here).
      */
    private[uic] def wired(
        open: SignalRef[Boolean],
        hi: SignalRef[Int],
        all: SignalRef[Boolean],
        base: Maybe[String] = Absent,
        focus: String => UI.Action = _ => ()
    )(
        using Frame
    ): UI =
        val validity = FieldValidity(validityV)
        val panel    = ReactiveValue.Dyn(open).and(ReactiveValue.Dyn(hi)).and(ReactiveValue.Dyn(all))
        validity.follow(panel.and(text).and(OptionItem.reading(optionSources)).render {
            case (((((o, h), a), cur), opts)) =>
                val self = new AutoComplete(p.copy(optionsV = opts))
                self.body(cur, Present(self.Wired(open, o, hi, h, all, a, base, focus)), validity)
        })
    end wired

    /** The typed text: a bound signal is read live, constants and unbound fields hold a fixed query. Read beside the
      * panel's state as ONE region (see [[ReactiveValue.and]]).
      */
    private def text(using Frame): ReactiveValue[String] =
        valueSlot.reading("")

    /** How a suggestion matches the typed text: [[filter]], else the start of any word of the label. */
    private def matcher: (A, String) => Boolean = filterF.getOrElse(Filter.startsWithPerTerm(labelF))

    private def body(query: String, st: Maybe[Wired], validity: FieldValidity)(using Frame): UI =
        // The suggestion list the panel and the field's keyboard share: the full
        // list on a dropdown-trigger open, the filtered list otherwise.
        val visibleGroups: List[OptionItem[A]] =
            if st.exists(_.allV) then optionsV
            else
                val m = matcher
                OptionItem.filter(optionsV, a => m(a, query))
        val visible: List[A] = OptionItem.flatten(visibleGroups)
        // Typing gates on minQueryLength; the dropdown trigger shows the full list
        // regardless. The panel is suppressed with nothing to show and no message.
        val panelShown: Boolean = st.exists { s =>
            s.isOpen && (s.allV || query.length >= minQueryLengthV) &&
            (visible.nonEmpty || emptyContentV.isDefined)
        }

        // Writes the picked label, fires onSelect/onChange, closes the panel.
        def pick(a: A)(s: Wired): UI.Action =
            val text                  = labelF(a)
            val setValue: UI.Action   = valueSlot.writable.fold[UI.Action](())(_.set(text))
            val fireSelect: UI.Action = onSelectF match
                case Present(f) => f(a)
                case Absent     => ()
            val fireChange: UI.Action = valueSlot.hearer match
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
        if stampFilledFlag && query.nonEmpty then f = f.cssClass("p-filled")
        // The FIELD carries the base id. A caller's own `id` already landed here and is the base;
        // a minted one had nowhere to go, which left the dropdown trigger with nothing to hand
        // focus to.
        access.id.foreach(v => f = f.id(v))
        st.flatMap(_.idBase).foreach(b => f = f.id(b))
        sizeV match
            case Size.Small  => f = f.cssClass("p-inputtext-sm")
            case Size.Large  => f = f.cssClass("p-inputtext-lg")
            case Size.Normal => ()
        end match
        if variantV == FieldVariant.Filled then f = f.cssClass("p-variant-filled")
        f = validity.mark(f)
        // A two-way ref is kyo-ui's own binding, a one-way signal is followed in place, a constant is the starting text.
        valueSlot.bound.foreach(v => f = f.value(v))
        f = f.placeholder(placeholderText)
        if disabledFlag then f = f.disabled(true)
        if readonlyFlag then f = f.readOnly(true)
        if requiredFlag then f = f.aria("required" -> "true").jsProp("required", "true")
        f = nameV.map(v => f.jsProp("name", v)).getOrElse(f)
        f = access.name match
            case Present(v) => f.aria("label" -> v)
            case _          => f
        f = access.nameRef.map(v => f.aria("labelledby" -> v)).getOrElse(f)
        // A text box that opens a list of suggestions IS a combobox. This one keeps DOM focus in
        // the input the whole time (the panel is `seedFocus(false)`), so the input is also where
        // `aria-activedescendant` belongs: on the suggestion list it named the right option but
        // hung it off an element nothing was focused on, and a screen reader reads the attribute
        // of the FOCUSED element or of none at all.
        f = f.role("combobox").aria("haspopup" -> "listbox").aria("expanded" -> panelShown)
        st.foreach { s =>
            s.idBase.foreach { b =>
                f = f.aria("controls" -> listId(b))
                val hiEff = if visible.isEmpty then -1 else math.min(s.hiV, visible.size - 1)
                if panelShown && hiEff >= 0 then f = f.aria("activedescendant" -> optionId(b, hiEff))
            }
        }
        st.foreach { s =>
            // Typing opens the panel on the filtered list and resets the highlight.
            f = f.onInput { v =>
                val user: UI.Action = onInputF match
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
            f = f.preventScrollKeys.onKeyDown(fieldKey(visible, query, panelShown, s, pick))
            // The blur carries the text the field holds as focus leaves it, the same text the field's own (or the
            // caller's) ref holds, since kyo-ui writes it on every keystroke; a one-way text reports the field's.
            onBlurF.foreach(g => f = f.onBlur((_, text) => g(text)))
        }
        val field: UI = valueSlot.hearer.map(g => f.onChange(g)).getOrElse(f)

        // === in-field affordances ================================================
        def loaderSpan: UI =
            span.cssClass("p-autocomplete-loader")(
                GlyphSvg(Icons.spinner, "p-icon", "p-icon-spin")
            )
        val loader: List[UI] = loadingV match
            case Present(BoolValue.Const(true))  => List(loaderSpan)
            case Present(BoolValue.Dyn(sig))     => List(sig.render(b => if b then loaderSpan else UI.empty))
            case Present(BoolValue.Const(false)) => Nil
            case Absent                          => Nil

        val clear: List[UI] = valueSlot.binding match
            case Present(ReactiveValue.Dyn(_)) if showClearFlag && interactive && query.nonEmpty =>
                List(
                    button
                        .cssClass("p-autocomplete-clear-icon")
                        .jsProp("type", "button")
                        .aria("label" -> "Clear")
                        .onClick(valueSlot.change(""))(GlyphSvg(Icons.times, "p-icon"))
                )
            case _ => Nil

        // The dropdown trigger opens the FULL list (Prime's dropdown prop);
        // re-clicking while open lands on the Overlay backdrop and closes.
        val dropdownUI: List[UI] =
            if dropdownFlag then
                var dd = button
                    .cssClass("p-autocomplete-dropdown")
                    .jsProp("type", "button")
                    .aria("label" -> "Show options")
                st match
                    case Present(s) =>
                        // The trigger is an affordance OF the combobox, not a widget beside it: it
                        // is its own tab stop, but every key this component answers is answered by
                        // the field. Opening from here without handing focus over left the reader
                        // standing on a button with no highlight to act on, no arrows and no
                        // Escape. Focus moves FIRST, while the field is still the element the
                        // render produced; the re-render that follows restores it by path.
                        dd = dd.onClick {
                            for
                                _ <- s.idBase match
                                    case Present(b) => s.focus(b)
                                    case Absent     => (): UI.Action
                                _ <- s.all.set(true)
                                _ <- s.hi.set(openHighlight(OptionItem.flatten(optionsV), query))
                                r <- s.open.set(true)
                            yield r
                        }
                    case Absent => if !interactive then dd = dd.disabled(true)
                end match
                List(dd(GlyphSvg(Icons.chevronDown, "p-icon")))
            else Nil

        // === floating panel ======================================================
        val panel: List[UI] =
            st.toList.filter(_ => panelShown).map(s => overlayPanel(visibleGroups, s, pick))

        // === field root ==========================================================
        var el = div.cssClass("p-autocomplete").cssClass("p-component")
        el = validity.markClass(el)
        if disabledFlag then el = el.cssClass("p-disabled")
        if fluidFlag then el = el.cssClass("p-autocomplete-fluid")
        if panelShown then el = el.cssClass("p-uic-overlay-anchor")
        el(UI.children((field :: loader) ++ clear ++ dropdownUI ++ panel)*)
    end body

    /** The floating suggestion panel: Prime's `.p-autocomplete-overlay` skin on
      * the [[Overlay]] primitive with `seedFocus(false)`, so focus stays in the input;
      * Escape is handled by the field, outside clicks by the backdrop.
      */
    private def overlayPanel(visibleGroups: List[OptionItem[A]], s: Wired, pick: A => Wired => UI.Action)(using Frame): UI =
        val visible        = OptionItem.flatten(visibleGroups)
        val hiEff          = if visible.isEmpty then -1 else math.min(s.hiV, visible.size - 1)
        val rows: List[UI] = OptionItem.rows(visibleGroups, "p-autocomplete-option-group") { (a, i) =>
            val content: HtmlChildVal = itemTemplateF match
                case Present(t) => t(a)
                case Absent     => stringToUI(labelF(a))
            var row = li
                .cssClass("p-autocomplete-option")
                .role("option")
                .data("uic-option-key" -> keyF.getOrElse(labelF)(a))
            if i == hiEff then row = row.cssClass("p-focus").scrollAuto(true)
            s.idBase.foreach(b => row = row.id(optionId(b, i)))
            row.onClick(pick(a)(s))(content)
        }
        val emptyRow: List[UI] =
            if visible.isEmpty then
                EmptyContent.whenSet(emptyContentV)(c =>
                    li.cssClass("p-autocomplete-empty-message").role("presentation")(c)
                )
            else Nil
        val listUI: UI =
            div.cssClass("p-autocomplete-list-container")(
                {
                    var list = ul.cssClass("p-autocomplete-list").role("listbox")
                    s.idBase.foreach(b => list = list.id(listId(b)))
                    list(UI.children(rows ++ emptyRow)*)
                }
            )
        Overlay(s.open)
            .panelClass("p-autocomplete-overlay")
            .panelClass("p-component")
            .seedFocus(false)
            .dismissOnEscape(false)(listUI)
            .renderShown(s.idBase.map(b => s"$b-panel"))
    end overlayPanel
end AutoComplete

object AutoComplete:
    final private[uic] case class Props[A](
        // The rows the body draws, resolved from `optionSources` by the region that draws them.
        optionsV: List[OptionItem[A]],
        labelF: A => String,
        keyF: Maybe[A => String] = Absent,
        itemTemplateF: Maybe[A => UI] = Absent,
        valueSlot: Controlled[String] = Controlled(),
        filterF: Maybe[(A, String) => Boolean] = Absent,
        minQueryLengthV: Int = 1,
        emptyContentV: Maybe[EmptyContent] = Absent,
        placeholderText: Maybe[Prop[String]] = Absent,
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
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        access: Accessibility = Accessibility.none,
        onInputF: Maybe[String => UI.Action] = Absent,
        onSelectF: Maybe[A => UI.Action] = Absent,
        stampFilledFlag: Boolean = false,
        onBlurF: Maybe[String => UI.Action] = Absent,
        optionSources: List[OptionSource[A]] = Nil
    )

    def apply[A](): AutoComplete[A] = new AutoComplete[A](Props(Nil, _.toString))
end AutoComplete
