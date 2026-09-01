package kyo.uic.test

import kyo.*
import kyo.UI.*
import kyo.uic
import kyo.uic.UicTest
import scala.language.implicitConversions

/** Golden SSR-render assertions. Renders components through kyo-ui's real
  * `HtmlRenderer` and asserts the wire-level contract the theme and the client
  * depend on: class hooks, data attributes, event registration (`data-kyo-ev`),
  * and two-way value binding.
  *
  * Every body is an effect rather than a blocking discharge, so the suite runs on
  * all four platforms: rendering is the same shared source everywhere, and JS,
  * Wasm, and Native have no way to block a thread on a result.
  */
class GoldenRenderTest extends UicTest:

    private def renderHtml(ui: UI)(using Frame): String < Async =
        UI.runRender(ui).take(1).run.map(_.mkString)

    /** DatePicker's live tree. Its keyboard, its minted month ref and its cell ids all come from
      * a mount, and a golden render shows a mount only as its placeholder, so these go through
      * the `wired` seam like every other mount-based component.
      */
    private def dpHtml(dp: uic.DatePicker, oref: SignalRef[Boolean], mref: Maybe[SignalRef[String]] = Absent)(using
        Frame
    ): String < Async =
        for
            minted <- Signal.initRef("")
            cursor <- Signal.initRef("")
            // The mount lifts a caller's view ref and mints one at the static view otherwise;
            // rendering the seam by hand has to do the same or the golden shows the wrong grid.
            view <- dp.currentViewRefV match
                case Present(r) => Kyo.lift(r)
                case Absent     => Signal.initRef(dp.viewV)
            seed    <- Signal.initRef(false)
            restore <- Signal.initRef(Maybe.empty[uic.DatePicker.Restore])
            refs = uic.DatePicker.Refs(oref, mref.getOrElse(minted), cursor, view, seed, restore)
            out <- UI.runRender(dp.wired(refs, "dp", _ => ())).take(1).run
        yield out.mkString

    /** Matches one table row group by tag AND class: the renderer writes
      * `data-kyo-path` between the two, so a plain `contains` cannot pair them.
      */
    private def groupTag(tag: String, cls: String): scala.util.matching.Regex =
        s"<$tag [^>]*class=\"$cls\"".r

    "Button emits Prime anatomy: class hooks, severity suffix, label span, click registration" in {
        for
            html <- renderHtml(
                uic.Button("Save").severity(uic.Severity.Danger).icon(uic.Icons.check).onClick(())
            )
            primary <- renderHtml(uic.Button("OK"))
            loading <- renderHtml(uic.Button("Busy").loading(true))
        yield
            assert(html.contains("p-button"), "has p-button class")
            assert(html.contains("p-component"), "has p-component class")
            assert(html.contains("p-button-danger"), "has severity suffix class")
            assert(html.contains("p-button-label"), "label wrapped in p-button-label span")
            assert(html.contains("""data-uic-icon="check""""), "has icon data attr")
            assert(html.contains("click"), "registers click event")
            assert(html.contains("<button"), "renders a real <button>")
            assert(!primary.contains("p-button-primary"), "Primary is the unsuffixed default")
            assert(loading.contains("p-icon-spin"), "loading spins the glyph")
            assert(loading.contains("""aria-busy="true""""), "loading exposes aria-busy")
            assert(loading.contains("disabled"), "loading blocks clicks like disabled")
    }

    "Input binds two-way: Prime classes, initial ref value + change registration" in {
        for
            html <-
                for
                    ref <- Signal.initRef("Ada")
                    out <- UI.runRender(uic.Input().placeholder("Your name").value(ref)).take(1).run
                yield out.mkString
            invalid  <- renderHtml(uic.Input().value("x").invalid(true).invalidMessage("This value is invalid"))
            seeded   <- renderHtml(uic.Input().value("x").focusAuto(true))
            unseeded <- renderHtml(uic.Input().value("x"))
            number   <- renderHtml(uic.InputNumber().value(3.0).focusAuto(true))
        yield
            assert(html.contains("p-inputtext"), "has p-inputtext class")
            assert(html.contains("p-component"), "has p-component class")
            assert(html.contains("""placeholder="Your name""""), "has placeholder")
            assert(html.contains("""value="Ada""""), "renders the ref's current value")
            assert(html.contains("change"), "registers change for two-way binding")
            assert(invalid.contains("p-invalid"), "invalid(true) renders .p-invalid")
            assert(invalid.contains("""aria-invalid="true""""), "invalid(true) sets aria-invalid")
            assert(invalid.contains("p-uic-invalid-message"), "invalidMessage renders the message row")
            assert(invalid.contains("This value is invalid"), "message text rendered")
            assert(seeded.contains("""data-kyo-focus-auto="1""""), "focusAuto(true) seeds focus on insert")
            assert(!unseeded.contains("data-kyo-focus-auto"), "and a field that did not ask for it carries no seed")
            assert(number.contains("""data-kyo-focus-auto="1""""), "InputNumber carries the same seed")
    }

    "Dialog (open) renders Prime mask + anatomy; (closed) renders no mask" in {
        def dialog(ref: SignalRef[Boolean])(using Frame): UI =
            uic.Dialog()
                .open(ref)
                .header("Confirm")
                .severity(uic.Severity.Danger)
                .footer(span("actions"))(p("Sure?"))

        for
            open <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(dialog(ref)).take(1).run
                yield out.mkString
            closed <-
                for
                    ref <- Signal.initRef(false)
                    out <- UI.runRender(dialog(ref)).take(1).run
                yield out.mkString
            maximized <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(uic.Dialog().open(ref).header("Big").maximized(true)(p("x"))).take(1).run
                yield out.mkString
            noFocus <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(
                        uic.Dialog().open(ref).header("Quiet").preventInitialFocus(true).preventFocusRestore(true)(p("x"))
                    ).take(1).run
                yield out.mkString
        yield
            assert(open.contains("""role="dialog""""), "open: dialog role")
            assert(open.contains("aria-modal"), "open: modal aria")
            assert(open.contains("p-overlay-mask"), "open: Prime overlay mask backdrop")
            // p-overlay-mask-enter-active is deliberately ABSENT: kept permanently (we
            // have no transition lifecycle) its fill-forwards var() keyframe paints the
            // mask transparent in Chromium.
            assert(!open.contains("p-overlay-mask-enter-active"), "open: no transient enter class (paints transparent when permanent)")
            assert(open.contains("p-dialog-mask"), "open: dialog mask class")
            assert(open.contains("p-dialog"), "open: dialog box class")
            assert(open.contains("p-dialog-header"), "open: header element")
            assert(open.contains("p-dialog-title"), "open: title span")
            assert(open.contains("p-dialog-header-actions"), "open: header actions container")
            assert(open.contains("p-dialog-close-button"), "open: Prime close button")
            assert(open.contains("p-button-icon-only"), "open: close button is icon-only Button anatomy")
            assert(open.contains("""data-uic-icon="times""""), "open: times glyph on the close button")
            assert(open.contains("p-dialog-content"), "open: content element")
            assert(open.contains("p-dialog-footer"), "open: footer element")
            assert(open.contains("p-uic-dialog-danger"), "open: severity accent class (kyo extension)")
            assert(open.contains("p-uic-dialog-severity-icon"), "open: severity icon slot")
            assert(open.contains("Confirm"), "open: header text")
            // The REAL focus mechanism (kyo client contract) replaces the old inert
            // data-uic-* hints: the box seeds focus on open (Escape works without a
            // prior click) and returns it to the opener on close.
            assert(open.contains("""data-kyo-focus-auto="1""""), "open: box seeds focus (data-kyo-focus-auto)")
            assert(open.contains("""data-kyo-focus-restore="1""""), "open: box restores focus (data-kyo-focus-restore)")
            assert(!open.contains("data-uic-initial-focus"), "open: old initial-focus hint retired")
            assert(!open.contains("data-uic-prevent-initial-focus"), "open: old prevent-initial hint retired")
            assert(!open.contains("data-uic-prevent-focus-restore"), "open: old prevent-restore hint retired")
            assert(!noFocus.contains("data-kyo-focus-auto"), "preventInitialFocus omits the seed attribute")
            assert(!noFocus.contains("data-kyo-focus-restore"), "preventFocusRestore omits the restore attribute")
            assert(maximized.contains("p-dialog-maximized"), "maximized modifier class")
            assert(!closed.contains("p-overlay-mask"), "closed: no mask rendered")
        end for
    }

    "Theme renders Prime tokens + component CSS + kyo remainder; NO sap* rules remain" in {
        val prime = uic.Theme.primeCss
        assert(prime.contains("--p-button-primary-background"), "declares Prime button token")
        assert(prime.contains(".p-button"), "extracted Prime component CSS present")
        assert(prime.contains(".p-inputtext"), "extracted inputtext CSS present")
        assert(prime.contains(".p-message"), "extracted message CSS present")
        assert(prime.contains(".p-progressbar"), "extracted progressbar CSS present")
        assert(prime.contains(".p-progressspinner"), "extracted progressspinner CSS present")
        assert(prime.contains(".p-tag"), "extracted tag CSS present")
        assert(prime.contains(".p-avatar"), "extracted avatar CSS present")
        assert(prime.contains(".p-card"), "extracted card CSS present")
        assert(prime.contains(".p-panel"), "extracted panel CSS present")
        assert(prime.contains(".p-toolbar"), "extracted toolbar CSS present")
        assert(prime.contains(".p-listbox"), "extracted listbox CSS present")
        assert(prime.contains(".p-tabs"), "extracted tabs CSS present")
        assert(prime.contains(".p-tree"), "extracted tree CSS present")
        assert(prime.contains(".p-breadcrumb"), "extracted breadcrumb CSS present")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-link"), "link skin (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-option-extra"), "listbox option extra (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-tab-count"), "tab count badge (remainder)")
        assert(
            uic.Theme.primeExtraCss.contains(".p-tablist, .p-tablist-tab-list, .p-breadcrumb-list"),
            "tablist/breadcrumb row restorers (remainder)"
        )
        assert(uic.Theme.primeExtraCss.contains("li.p-tree-node { flex-direction: column"), "tree node column restorer (remainder)")
        // One ring per focused component, and never two at once: the listbox rings itself
        // because Prime rings nothing, the tree rings its focused node and so must not also
        // let the browser ring the whole list. Both against :focus-visible, so a mouse click
        // moves the highlight without drawing a keyboard indicator.
        assert(
            uic.Theme.primeExtraCss.contains(".p-listbox:not(.p-disabled):has(.p-listbox-list:focus-visible)"),
            "listbox focus ring (remainder — Prime clears the list outline and stamps no ring)"
        )
        assert(
            uic.Theme.primeExtraCss.contains(".p-tree-root-children:focus-visible .p-tree-node-content.p-focus"),
            "tree node ring only while the element holding focus is keyboard-focused (remainder)"
        )
        assert(
            uic.Theme.primeExtraCss.contains(".p-uic-overlay-panel:focus-visible .p-tree-node-content.p-focus"),
            "and the same inside a TreeSelect panel, which is the focus holder there (remainder)"
        )
        assert(
            uic.Theme.primeExtraCss.contains(".p-tree-root-children:focus { outline: none; }"),
            "tree list draws no ring of its own (remainder — the node ring is the indicator)"
        )
        assert(uic.Theme.primeExtraCss.contains("p-icon-spin"), "loading spinner keyframes (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-invalid-message"), "invalidMessage row CSS (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-label"), "label CSS (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-title--h1"), "title rem scale (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-text--clamp"), "text line-clamp (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-avatar-badge"), "avatar badge overlay (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-card-caption-row"), "card caption row (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-progressspinner-sm"), "spinner size presets (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-flex"), "flex primitive (remainder)")
        // The extracted CSS for the last components is
        // present, the kyo remainder carries the inline-overlay statics, and NOT ONE
        // sap* class rule remains anywhere in the theme.
        assert(prime.contains(".p-select"), "extracted select CSS present")
        assert(prime.contains(".p-autocomplete"), "extracted autocomplete CSS present")
        assert(prime.contains(".p-datepicker"), "extracted datepicker CSS present")
        assert(prime.contains(".p-dialog"), "extracted dialog CSS present")
        assert(prime.contains(".p-overlay-mask"), "extracted overlay-mask CSS present (base.css)")
        assert(prime.contains(".p-toast"), "extracted toast CSS present")
        assert(prime.contains(".p-datatable"), "extracted datatable CSS present")
        assert(prime.contains(".p-paginator"), "extracted paginator CSS present")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-select-readonly"), "select readonly skin (remainder)")
        assert(!uic.Theme.primeExtraCss.contains("select.p-select-label"), "native-select chrome strip retired")
        assert(uic.Theme.primeExtraCss.contains(".p-autocomplete { flex-direction: row"), "autocomplete row restorer + anchor (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-datepicker { flex-wrap: wrap; }"), "inline datepicker panel wrap (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-dialog-mask { display: flex"), "dialog mask centering (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-dialog-danger"), "dialog severity accents (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-toast { position: fixed"), "toast fixed positioning (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-toast-bottom-right"), "toast position tokens (remainder)")
        assert(!uic.Theme.primeExtraCss.contains("p-uic-dt-row"), "datatable row re-scope retired (real thead/tbody)")
        assert(!uic.Theme.primeExtraCss.contains("p-uic-tt-row"), "treetable row re-scope retired (real thead/tbody)")
        assert(prime.contains(".p-datatable-tbody > tr > td"), "generated body-cell rules reachable through the real tbody")
        assert(
            prime.contains(".p-datatable.p-datatable-striped .p-datatable-tbody > tr.p-row-odd"),
            "generated striping reachable through the real tbody"
        )
        assert(uic.Theme.primeExtraCss.contains(".p-paginator { flex-direction: row; }"), "paginator row restorer (remainder)")
        assert(
            uic.Theme.primeExtraCss.contains(".p-datatable-inline-filter { flex-direction: row; }"),
            "filter row restorer (remainder): the extracted rule leaves the direction at the browser default"
        )
        assert(
            uic.Theme.primeExtraCss.contains(".p-uic-table-fixed { table-layout: fixed; }"),
            "the layout mode a column width needs (remainder): the extracted sheet carries the clipping, not the mode"
        )
        assert(
            uic.Theme.primeExtraCss.contains("--p-uic-dt-select-width: calc(var(--p-checkbox-width, 1.25rem) + 2rem);"),
            "the width of the table's own columns (remainder): the extracted sheet sizes them only by their content"
        )
        assert(
            uic.Theme.primeExtraCss.contains(".p-uic-dt-dragging { opacity: 0.5; }"),
            "the dragged header cell (remainder): the extracted sheet has the cursor and nothing else"
        )
        assert(!uic.Theme.css.contains(".sap"), "NO sap* class rules anywhere in the theme")
        assert(!uic.Theme.css.contains("--sap"), "NO sap tokens anywhere in the theme")
        assert(!uic.Theme.primeExtraCss.contains(".sap"), "no sap* rules in the Prime remainder")
        assert(!uic.Theme.primeExtraCss.contains("sapBtnLoadingSpin"), "sapBtnLoadingSpin keyframes deleted")
        assert(!uic.Theme.css.contains("ui5"), "no ui5 vocabulary anywhere in the theme")
    }

    "Icon renders inline SVG with the glyph path, currentColor fill, and Prime class hooks" in {
        for
            html <- renderHtml(uic.Icon(uic.Icons.check).accessibleName("Save"))
        yield
            assert(html.contains("p-icon"), "has p-icon class")
            assert(html.contains("p-uic-icon"), "has p-uic-icon class")
            assert(html.contains("<svg"), "renders an <svg>")
            assert(html.contains("<path"), "renders the glyph <path>")
            assert(html.contains(uic.Icons.check.pathData.take(24)), "path data embedded")
            assert(html.contains("currentColor"), "fill inherits text color")
            assert(html.contains("""data-uic-icon="check""""), "glyph data attr")
            assert(html.contains("""aria-label="Save""""), "accessible name exposed")
    }

    "Tag renders Prime anatomy: severity suffix, rounded, icon, and label span" in {
        for
            html    <- renderHtml(uic.Tag("Done").severity(uic.Severity.Success).rounded(true).icon(uic.Icons.check))
            primary <- renderHtml(uic.Tag("Plain"))
        yield
            assert(html.contains("p-tag"), "base class")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("p-tag-success"), "severity suffix class")
            assert(html.contains("p-tag-rounded"), "rounded modifier")
            assert(html.contains("p-tag-icon"), "icon slot class")
            assert(html.contains("<svg"), "icon rendered as SVG")
            assert(html.contains("p-tag-label"), "label span class")
            assert(html.contains("Done"), "label text rendered")
            assert(!primary.contains("p-tag-primary"), "unset severity keeps the unsuffixed base skin")
    }

    "Title renders ARIA heading semantics on the p-uic-title scale" in {
        for
            html <- renderHtml(uic.Title().level(uic.TitleLevel.H1).size(uic.TitleLevel.H3)("Overview"))
        yield
            assert(html.contains("p-uic-title"), "base class")
            assert(html.contains("""role="heading""""), "heading role")
            assert(html.contains("""aria-level="1""""), "aria level from H1")
            assert(html.contains("p-uic-title--h3"), "visual size class")
            assert(html.contains("Overview"), "text rendered")
    }

    "Text renders class hooks, line-clamp data attr, and slot" in {
        for
            html <- renderHtml(
                uic.Text().emptyIndicatorMode(uic.TextEmptyIndicatorMode.On).maxLines(3)("hello")
            )
        yield
            assert(html.contains("p-uic-text"), "base class")
            assert(html.contains("p-uic-text--empty-indicator"), "empty indicator modifier")
            assert(html.contains("p-uic-text--clamp"), "clamp modifier class")
            assert(html.contains("""data-uic-max-lines="3""""), "max-lines data attr")
            assert(html.contains("hello"), "default slot rendered")
    }

    "ProgressSpinner renders Prime anatomy: spin svg + circle, progressbar role" in {
        for
            html  <- renderHtml(uic.ProgressSpinner().accessibleName("Loading"))
            small <- renderHtml(uic.ProgressSpinner().size(uic.Size.Small))
        yield
            assert(html.contains("p-progressspinner"), "base class")
            assert(html.contains("""role="progressbar""""), "progressbar role")
            assert(html.contains("p-progressspinner-spin"), "spin svg class")
            assert(html.contains("""viewBox="25 25 50 50""""), "Prime viewBox")
            assert(html.contains("p-progressspinner-circle"), "circle class")
            assert(html.contains("<circle"), "renders a real <circle>")
            assert(html.contains("""aria-label="Loading""""), "accessible name exposed")
            assert(small.contains("p-uic-progressspinner-sm"), "size preset class")
    }

    "Panel + Card compose with Prime anatomy; toggleable panel renders the toggle button" in {
        for
            html <-
                for
                    ref <- Signal.initRef(false)
                    ui = uic.Panel().header("Details").toggleable(true).collapsed(ref)(
                        uic.Card().title("Info").subtitle("Sub")(p("Body")): UI
                    )
                    out <- UI.runRender(ui).take(1).run
                yield out.mkString
            fixed <- renderHtml(uic.Panel().header("Plain").footer(span("foot"))(p("content")))
        yield
            assert(html.contains("p-panel"), "panel base class")
            assert(html.contains("p-panel-toggleable"), "toggleable modifier")
            assert(html.contains("p-panel-header"), "panel header")
            assert(html.contains("p-panel-title"), "panel title")
            assert(html.contains("p-panel-header-actions"), "header actions container")
            assert(html.contains("p-panel-toggle-button"), "toggle button")
            assert(html.contains("""data-uic-icon="minus""""), "expanded panel shows the minus glyph")
            assert(html.contains("p-panel-content-container"), "content container")
            assert(html.contains("p-panel-content"), "content element")
            assert(html.contains("p-card"), "card base class")
            assert(html.contains("p-card-body"), "card body")
            assert(html.contains("p-card-caption"), "card caption")
            assert(html.contains("p-card-title"), "card title")
            assert(html.contains("p-card-subtitle"), "card subtitle")
            assert(html.contains("p-card-content"), "card content")
            assert(html.contains("Body"), "content rendered")
            assert(!fixed.contains("p-panel-toggleable"), "fixed default has no toggleable class")
            assert(!fixed.contains("p-panel-toggle-button"), "fixed default has no toggle button")
            assert(fixed.contains("p-panel-footer"), "panel footer slot rendered")
    }

    "Card renders the caption row extensions and footer slot" in {
        for
            html <- renderHtml(
                uic.Card()
                    .title("Jane")
                    .additionalText("+12%")
                    .headerAvatar(uic.Avatar().initials("JD"))
                    .headerAction(uic.Button("Go"))
                    .footer(span("footer-slot"))(p("Body"))
            )
        yield
            assert(html.contains("p-uic-card-caption-row"), "caption row wrapper")
            assert(html.contains("p-uic-card-avatar"), "avatar slot")
            assert(html.contains("p-uic-card-additional"), "additional text slot")
            assert(html.contains("p-uic-card-action"), "action slot")
            assert(html.contains("+12%"), "additional text rendered")
            assert(html.contains("p-card-footer"), "footer slot")
            assert(html.contains("footer-slot"), "footer content rendered")
    }

    "CheckBox binds two-way: Prime anatomy, checked class + icon from ref, change reg" in {
        for
            html <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(uic.CheckBox("Accept").checked(ref).invalid(true)).take(1).run
                yield out.mkString
            mixed <- renderHtml(uic.CheckBox("Some").indeterminate(true))
        yield
            assert(html.contains("p-checkbox"), "base class hook")
            assert(html.contains("p-checkbox-checked"), "checked modifier class from ref's initial value")
            assert(html.contains("p-checkbox-input"), "hidden native input class")
            assert(html.contains("p-checkbox-box"), "visual box element")
            assert(html.contains("p-checkbox-icon"), "check icon rendered while checked")
            assert(html.contains("""data-uic-icon="check""""), "check glyph")
            assert(html.contains("""type="checkbox""""), "composes native checkbox input")
            assert(html.contains(" checked"), "native checked attr set from ref")
            assert(html.contains("p-invalid"), "invalid modifier class")
            assert(html.contains("change"), "registers change for two-way binding")
            assert(html.contains("Accept"), "label text rendered")
            assert(mixed.contains("""aria-checked="mixed""""), "indeterminate exposes aria-checked=mixed")
            assert(mixed.contains("""data-uic-icon="minus""""), "indeterminate shows the minus glyph")
    }

    "RadioButton renders Prime anatomy, group name, change reg" in {
        for
            html <-
                for
                    ref <- Signal.initRef(false)
                    out <- UI.runRender(uic.RadioButton("Option A").name("choice").checked(ref)).take(1).run
                yield out.mkString
        yield
            assert(html.contains("p-radiobutton"), "base class hook")
            assert(!html.contains("p-radiobutton-checked"), "unchecked ref leaves the checked class off")
            assert(html.contains("p-radiobutton-input"), "hidden native input class")
            assert(html.contains("p-radiobutton-box"), "visual box element")
            assert(html.contains("p-radiobutton-icon"), "dot icon element always present")
            assert(html.contains("""type="radio""""), "composes native radio input")
            assert(html.contains("""name="choice""""), "group name propagated")
            assert(html.contains("change"), "registers change")
            assert(html.contains("Option A"), "label text rendered")
    }

    "ToggleSwitch renders switch role, Prime slider/handle anatomy" in {
        for
            html <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(uic.ToggleSwitch().checked(ref)).take(1).run
                yield out.mkString
        yield
            assert(html.contains("p-toggleswitch"), "base class hook")
            assert(html.contains("p-toggleswitch-checked"), "checked modifier class from ref")
            assert(html.contains("""role="switch""""), "switch role on the native input")
            assert(html.contains("""aria-checked="true""""), "aria-checked reflects ref")
            assert(html.contains("""type="checkbox""""), "composes native checkbox input")
            assert(html.contains("p-toggleswitch-input"), "hidden native input class")
            assert(html.contains("p-toggleswitch-slider"), "slider element rendered")
            assert(html.contains("p-toggleswitch-handle"), "handle element rendered")
            assert(html.contains("change"), "registers change")
    }

    "TextArea binds two-way: Prime classes, value from ref, placeholder, data-rows" in {
        for
            html <-
                for
                    ref <- Signal.initRef("hello")
                    out <- UI.runRender(uic.TextArea().value(ref).placeholder("Notes").rows(4)).take(1).run
                yield out.mkString
            resizing <- renderHtml(uic.TextArea().autoResize(true).invalid(true).invalidMessage("Too long"))
        yield
            assert(html.contains("p-textarea"), "base class hook")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("<textarea"), "renders a real <textarea>")
            assert(html.contains("""placeholder="Notes""""), "placeholder set")
            assert(html.contains("""data-rows="4""""), "rows exposed as data-rows")
            assert(html.contains("hello"), "ref value rendered as content")
            assert(html.contains("change"), "registers change for two-way binding")
            assert(resizing.contains("p-uic-autoresize"), "autoResize emits the field-sizing class")
            assert(resizing.contains("p-textarea-resizable"), "autoResize emits Prime's resizable class")
            assert(resizing.contains("p-invalid"), "invalid modifier class")
            assert(resizing.contains("p-uic-invalid-message"), "invalidMessage renders the message row")
    }

    "Select renders Prime's div-trigger anatomy; the closed projection shows the bound label" in {
        for
            html <-
                for
                    ref <- Signal.initRef("b")
                    out <- UI.runRender(
                        uic.Select[(String, String)]()
                            .options(Seq("a" -> "Apple", "b" -> "Banana"))(_._2)
                            .optionKey(_._1)
                            .value(ref)
                    ).take(1).run
                yield out.mkString
            placeholder <-
                for
                    ref <- Signal.initRef("")
                    out <- UI.runRender(
                        uic.Select[String]().options(Seq("Small", "Large")).placeholder("Pick one").value(ref)
                    ).take(1).run
                yield out.mkString
            invalid <- renderHtml(
                uic.Select[String]().options(Seq("x")).invalid(true).invalidMessage("Required").size(uic.Size.Small).fluid(true)
            )
            named <- renderHtml(uic.Select[String]().options(Seq("x")).name("country"))
        yield
            assert(html.contains("p-select"), "root field class")
            assert(html.contains("p-component"), "p-component class")
            assert(!html.contains("<select"), "the native <select> fallback is retired (real floating panel now)")
            assert(html.contains("""class="p-select-label""""), "the trigger label span carries Prime's label class")
            assert(html.contains("Banana"), "closed trigger shows the bound option's label projection")
            assert(!html.contains("Apple"), "unselected options do not render while closed")
            assert(html.contains("""aria-haspopup="listbox""""), "trigger advertises the listbox popup")
            assert(html.contains("""aria-expanded="false""""), "closed trigger reads collapsed")
            assert(html.contains("""tabindex="0""""), "trigger is focusable (keyboard open + focus restore target)")
            assert(html.contains("p-select-dropdown"), "chevron dropdown affordance")
            assert(html.contains("""data-uic-icon="chevron-down""""), "chevron glyph")
            assert(placeholder.contains("Pick one"), "placeholder text rendered while value empty")
            assert(placeholder.contains("p-placeholder"), "placeholder skin class on the label")
            assert(invalid.contains("p-invalid"), "invalid class on the field")
            assert(invalid.contains("p-uic-invalid-message"), "invalidMessage row")
            assert(invalid.contains("p-select-sm"), "small size class")
            assert(invalid.contains("p-select-fluid"), "fluid class")
            assert(named.contains("""data-kyo-prop-name="country""""), "name(...) emits the hidden form carrier")
            assert(named.contains("""type="hidden""""), "form carrier is a hidden input")
    }

    "Select (open, wired) renders the floating panel: overlay skin, options, filter, checkmark, clear" in {
        def openHtml(sel: uic.Select[(String, String)], current: String, hi: Int = -1): String < Async =
            for
                vref <- Signal.initRef(current)
                oref <- Signal.initRef(true)
                href <- Signal.initRef(hi)
                qref <- Signal.initRef("")
                out  <- UI.runRender(sel.value(vref).open(oref).wired(oref, href, qref)).take(1).run
            yield out.mkString
        val base = uic.Select[(String, String)]()
            .options(Seq("a" -> "Apple", "b" -> "Banana"))(_._2)
            .optionKey(_._1)

        // filterQuery binds the header to an app-owned ref: the panel filters against
        // its live value, and the header renders without a separate filterable(true).
        // Nothing is selected here, so the trigger label contributes no option text and
        // the assertions below see only the panel rows.
        def appQueryHtml(query: String): String < Async =
            for
                vref <- Signal.initRef("")
                oref <- Signal.initRef(true)
                href <- Signal.initRef(-1)
                qref <- Signal.initRef(query)
                out  <- UI.runRender(base.filterQuery(qref).value(vref).open(oref).wired(oref, href, qref)).take(1).run
            yield out.mkString
        for

            open      <- openHtml(base, "b")
            highlight <- openHtml(base, "b", hi = 0)
            featured  <- openHtml(base.filterable(true).checkmark(true).showClear(true), "b")
            disabled  <- openHtml(base.optionDisabled(_._1 == "a"), "b")
            appQuery  <- appQueryHtml("ap")
        yield

            assert(open.contains("p-select-open"), "open: root modifier class")
            assert(open.contains("p-uic-overlay-anchor"), "open: anchor glue class for the panel geometry")
            assert(open.contains("""aria-expanded="true""""), "open: trigger reads expanded")
            assert(open.contains("p-uic-overlay-backdrop"), "open: outside-click backdrop (Overlay primitive)")
            assert(open.contains("p-uic-overlay-panel"), "open: overlay panel geometry class")
            assert(open.contains("p-select-overlay"), "open: Prime's panel skin class")
            // Focus stays on the trigger, which is what makes the trigger the combobox and lets
            // its `aria-activedescendant` be read at all.
            assert(!open.contains("""data-kyo-focus-auto="1""""), "open: the panel seeds no focus")
            assert(!open.contains("""data-kyo-focus-trap="1""""), "open: and traps none either")
            assert(open.contains("p-select-list-container"), "open: scrollable list container")
            assert(open.contains("""class="p-select-list""""), "open: Prime's option list")
            assert(open.contains("""role="listbox""""), "open: listbox role")
            assert(open.contains("p-select-option"), "open: option rows")
            assert(open.contains("p-select-option-selected"), "open: bound value's row marked selected")
            assert(open.contains("""aria-selected="true""""), "open: aria-selected on the picked row")
            assert(open.contains("Apple"), "open: all options render in the panel")
            assert(open.contains("p-select-option-label"), "open: option label span")
            assert(!open.contains("p-select-header"), "open: no filter header without filterable(true)")
            assert(!open.contains("p-focus"), "open: no highlight before keyboard navigation")
            assert(highlight.contains("p-focus"), "highlight ref stamps Prime's .p-focus row")
            assert(featured.contains("p-select-header"), "filterable(true): Prime's header slot")
            assert(featured.contains("p-select-filter"), "filterable(true): the filter input")
            // A filter header takes focus into the panel, which makes IT the combobox: the
            // announcement is read off the focused element, so it has to be the one that carries
            // the role, the popup reference and the highlight.
            assert(featured.contains("""role="combobox""""), "the filter input is the combobox once there is one")
            assert(featured.contains("""data-kyo-focus-auto="1""""), "and it is what focus is seeded onto")
            assert(featured.contains("p-select-option-check-icon"), "checkmark(true): check glyph on the selected row")
            assert(featured.contains("p-select-option-blank-icon"), "checkmark(true): blank slot on unselected rows")
            assert(featured.contains("p-select-clear-icon"), "showClear(true): clear affordance while a value is set")
            assert(disabled.contains("p-disabled"), "optionDisabled rows carry .p-disabled")
            assert(disabled.contains("""aria-disabled="true""""), "optionDisabled rows carry aria-disabled")
            assert(appQuery.contains("p-select-header"), "filterQuery: the header renders without filterable(true)")
            assert(appQuery.contains("""value="ap""""), "filterQuery: the input carries the app-owned query")
            assert(appQuery.contains("Apple"), "filterQuery: the matching option stays")
            assert(!appQuery.contains("Banana"), "filterQuery: the panel filters against the app-owned query")
        end for
    }

    "Select optionGroups renders the ARIA group anatomy; the highlight index stays flat across headers" in {
        def openHtml(sel: uic.Select[String], hi: Int = -1, query: String = ""): String < Async =
            for
                vref <- Signal.initRef("")
                oref <- Signal.initRef(true)
                href <- Signal.initRef(hi)
                qref <- Signal.initRef(query)
                out  <- UI.runRender(sel.value(vref).open(oref).wired(oref, href, qref)).take(1).run
            yield out.mkString
        // Two groups and one loose option, so the flat option order is Apple 0,
        // Banana 1, Carrot 2, Date 3 while four headers and rows render between them.
        val base = uic.Select[String]().optionGroups(Seq(
            uic.OptionItem.group("Fruit")("Apple", "Banana"),
            uic.OptionItem.group("Vegetable")("Carrot"),
            uic.OptionItem.item("Date")
        ))(identity)
        for
            open     <- openHtml(base)
            thirdHi  <- openHtml(base, hi = 2)
            filtered <- openHtml(base.filterable(true), query = "app")
            flat     <- openHtml(uic.Select[String]().options(Seq("Apple"))(identity))
        yield
            assert(open.contains("p-uic-option-group"), "group wrapper class")
            assert(open.contains("""role="group""""), "the group is a real ARIA group, not a styled sibling")
            assert(open.contains("""aria-label="Fruit""""), "the group carries its label as its accessible name")
            assert(open.contains("p-uic-option-group-list"), "nested list holding the header and the options")
            assert(open.contains("""role="none""""), "the nested list drops out of the accessibility tree")
            assert(open.contains("p-select-option-group"), "Prime's header class stays on an li element")
            assert(open.contains("""aria-hidden="true""""), "the header is hidden; the group already announces the same text")
            assert(open.contains("Vegetable"), "every group header renders")
            assert(open.contains("Date"), "a loose OptionItem.item renders beside the groups")
            assert(!flat.contains("p-uic-option-group"), "an ungrouped select renders no group wrapper at all")
            val focusAt = thirdHi.indexOf("p-focus")
            assert(
                focusAt > thirdHi.indexOf("Banana") && focusAt < thirdHi.indexOf("Carrot"),
                "hi=2 highlights Carrot: the index counts pickable options, not rendered rows"
            )
            assert(filtered.contains("Apple"), "filter: the matching option stays")
            assert(filtered.contains("""aria-label="Fruit""""), "filter: a surviving option keeps its group")
            assert(!filtered.contains("Vegetable"), "filter: a group the query emptied loses its header with it")
            assert(!filtered.contains("Carrot"), "filter: the emptied group's options are gone")
        end for
    }

    "Overlay renders backdrop + anchored panel while open; closed renders nothing" in {
        def overlay(open: Boolean, f: uic.Overlay => uic.Overlay): String < Async =
            for
                ref <- Signal.initRef(open)
                out <- UI.runRender(f(uic.Overlay(ref))(span("panel-content"))).take(1).run
            yield out.mkString
        for
            open   <- overlay(true, identity)
            closed <- overlay(false, identity)
            topEnd <- overlay(true, _.anchor(uic.OverlayAnchor.TopEnd).matchWidth(false))
            capped <- overlay(true, _.maxHeight(240))
            bare   <- overlay(true, _.dismissOnOutsideClick(false).dismissOnEscape(false).seedFocus(false))
            locked <- overlay(true, _.scroll(uic.Overlay.Scroll.Lock))

            // With a trigger the overlay owns the anchor box, so the glue class the
            // geometry depends on cannot be left off by the caller.
            triggered <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(uic.Overlay(ref).trigger(button("Open"))(span("panel-content"))).take(1).run
                yield out.mkString
            triggeredClosed <-
                for
                    ref <- Signal.initRef(false)
                    out <- UI.runRender(uic.Overlay(ref).trigger(button("Open"))(span("panel-content"))).take(1).run
                yield out.mkString
        yield
            assert(open.contains("p-uic-overlay-backdrop"), "open: transparent outside-click backdrop")
            assert(open.contains("""data-kyo-ev="click,wheel""""), "open: backdrop registers dismiss click + Scroll.Close wheel")
            assert(
                locked.contains("""data-kyo-ev="click""""),
                "Scroll.Lock: backdrop keeps only the dismiss click (wheel swallowed natively)"
            )
            assert(open.contains("p-uic-overlay-panel"), "open: panel class")
            assert(open.contains("p-uic-overlay-bottom-start"), "default anchor: bottom-start")
            assert(open.contains("p-uic-overlay-match-width"), "default: min-width matches the anchor")
            assert(open.contains("""data-kyo-focus-auto="1""""), "default: panel seeds focus")
            assert(open.contains("""data-kyo-focus-restore="1""""), "default: focus restore on close")
            assert(open.contains("""tabindex="-1""""), "panel is a focusable seed target")
            assert(open.contains("""data-kyo-focus-trap="1""""), "panel traps Tab")
            assert(open.contains("""data-kyo-ev="keydown""""), "panel registers Escape")
            assert(open.contains("""data-kyo-stop="1""""), "panel consumes its own keydown (per-level Escape)")
            assert(open.contains("panel-content"), "panel children render")
            assert(!closed.contains("p-uic-overlay-panel"), "closed: nothing rendered")
            assert(!bare.contains("data-kyo-stop"), "dismissOnEscape(false): Escape-inert panel stays event-transparent")
            assert(topEnd.contains("p-uic-overlay-top-end"), "anchor variant class")
            assert(!topEnd.contains("p-uic-overlay-match-width"), "matchWidth(false) omits the width class")
            assert(capped.contains("max-height"), "maxHeight caps the panel")
            assert(capped.contains("overflow-y"), "maxHeight makes the panel scroll")
            assert(!bare.contains("p-uic-overlay-backdrop"), "dismissOnOutsideClick(false): no backdrop")
            assert(!bare.contains("data-kyo-focus-auto"), "seedFocus(false): no seed attribute")
            assert(!bare.contains("data-kyo-ev=\"keydown\""), "dismissOnEscape(false) + no host keys: no keydown")
            assert(triggered.contains("p-uic-overlay-anchor"), "trigger: the overlay stamps its own anchor container")
            assert(triggered.contains("Open"), "trigger: the trigger renders inside that container")
            assert(triggered.contains("p-uic-overlay-panel"), "trigger: the panel renders next to the trigger")
            assert(triggeredClosed.contains("p-uic-overlay-anchor"), "trigger: the anchor container survives a closed panel")
            assert(!triggeredClosed.contains("p-uic-overlay-panel"), "trigger: closed still renders no panel")
            // Without a trigger the primitive stays as it was: backdrop + panel only, for
            // the components that own their own root and stamp the class there.
            assert(!open.contains("p-uic-overlay-anchor"), "no trigger: the overlay adds no container of its own")
        end for
    }

    "Tooltip renders Prime anatomy inside the hover wrapper — zero state, zero round-trips" in {
        for
            html   <- renderHtml(uic.Tooltip("Save your work")(uic.Button("Save")))
            bottom <- renderHtml(uic.Tooltip("Below").position(uic.TooltipPosition.Bottom)(span("target")))
            left   <- renderHtml(uic.Tooltip("L").position(uic.TooltipPosition.Left)(span("t")))
            right  <- renderHtml(uic.Tooltip("R").position(uic.TooltipPosition.Right)(span("t")))
            rich   <- renderHtml(uic.Tooltip(span.cssClass("rich")("Formatted"))(span("t")))
        yield
            assert(html.contains("p-uic-tooltip"), "relative hover wrapper (kyo glue)")
            assert(html.contains("p-tooltip"), "Prime's tooltip box class")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("p-tooltip-top"), "Top is the default position")
            assert(html.contains("p-tooltip-arrow"), "arrow element")
            assert(html.contains("p-tooltip-text"), "text element")
            assert(html.contains("""role="tooltip""""), "tooltip role")
            assert(html.contains("Save your work"), "tooltip text rendered")
            assert(html.contains("p-button"), "target child renders inside the wrapper")
            assert(!html.contains("data-kyo-ev"), "CSS-driven: no event registration at all")
            assert(bottom.contains("p-tooltip-bottom"), "Bottom position class")
            assert(left.contains("p-tooltip-left"), "Left position class")
            assert(right.contains("p-tooltip-right"), "Right position class")
            assert(rich.contains("""class="rich""""), "UI content overload renders arbitrary markup")
    }

    "Popover (open) renders Prime panel + content on the Overlay; trigger toggles; flipped above" in {
        def popover(open: Boolean, f: uic.Popover => uic.Popover): String < Async =
            for
                ref <- Signal.initRef(open)
                out <- UI.runRender(f(uic.Popover(ref))(p("popover-content"))).take(1).run
            yield out.mkString
        for
            open    <- popover(true, _.trigger(span("Open me")))
            closed  <- popover(false, _.trigger(span("Open me")))
            bare    <- popover(true, identity)
            flipped <- popover(true, _.anchor(uic.OverlayAnchor.TopStart))
            pinned  <- popover(true, _.dismissable(false))
            noSeed  <- popover(true, _.seedFocus(false))
        yield
            assert(open.contains("p-uic-popover-anchor"), "trigger: anchor wrapper (position glue)")
            assert(open.contains("p-uic-popover-trigger"), "trigger: toggle wrapper")
            assert(open.contains("Open me"), "trigger content rendered")
            assert(open.contains("""data-kyo-ev="click""""), "trigger registers the toggle click")
            assert(open.contains("p-uic-overlay-backdrop"), "open: outside-click backdrop (Overlay primitive)")
            assert(open.contains("p-uic-overlay-panel"), "open: overlay panel geometry class")
            assert(open.contains("p-popover"), "open: Prime's popover skin class (arrow via the sheet's :before/:after)")
            assert(open.contains("p-popover-content"), "open: Prime's content element")
            assert(open.contains("popover-content"), "open: panel children render")
            assert(open.contains("""data-kyo-focus-auto="1""""), "open: panel seeds focus (default)")
            assert(open.contains("""data-kyo-focus-restore="1""""), "open: focus returns to the trigger on close")
            assert(!open.contains("p-uic-overlay-match-width"), "popover sizes to content (no matchWidth)")
            assert(!open.contains("p-popover-flipped"), "below by default: no flipped class")
            assert(!closed.contains("p-popover"), "closed: no panel")
            assert(closed.contains("p-uic-popover-trigger"), "closed: trigger still renders")
            assert(!bare.contains("p-uic-popover-anchor"), "no trigger: bare floating panel (caller anchors)")
            assert(flipped.contains("p-popover-flipped"), "Top anchor stamps Prime's flipped class (arrow below)")
            assert(flipped.contains("p-uic-overlay-top-start"), "Top anchor geometry class")
            assert(!pinned.contains("p-uic-overlay-backdrop"), "dismissable(false): no backdrop")
            assert(!noSeed.contains("data-kyo-focus-auto"), "seedFocus(false): no seed attribute")
        end for
    }

    "AutoComplete (open, wired) renders the FLOATING suggestion panel; focus stays in the field" in {
        def openHtml(
            ac: uic.AutoComplete[String],
            text: String,
            open: Boolean = true,
            hi: Int = -1,
            all: Boolean = false
        ): String < Async =
            for
                vref <- Signal.initRef(text)
                oref <- Signal.initRef(open)
                href <- Signal.initRef(hi)
                aref <- Signal.initRef(all)
                out  <- UI.runRender(ac.value(vref).wired(oref, href, aref)).take(1).run
            yield out.mkString
        val base = uic.AutoComplete[String]().options(Seq("Apple", "Banana"))
        for
            open      <- openHtml(base, "ap")
            closed    <- openHtml(base, "ap", open = false)
            empty     <- openHtml(uic.AutoComplete[String]().options(Seq("Apple")).emptyContent("No match"), "zz")
            gated     <- openHtml(base.minQueryLength(3), "ap")
            fullList  <- openHtml(base.dropdown(true), "zz", all = true)
            highlight <- openHtml(base, "ap", hi = 0)
            statics <-
                for
                    ref <- Signal.initRef("x")
                    out <- UI.runRender(
                        uic.AutoComplete[String]().options(Seq("Apple")).loading(true).showClear(true).value(ref)
                    ).take(1).run
                yield out.mkString
            templated <-
                for
                    vref <- Signal.initRef("Ber")
                    oref <- Signal.initRef(true)
                    href <- Signal.initRef(-1)
                    aref <- Signal.initRef(false)
                    out <- UI.runRender(
                        uic.AutoComplete[(String, String)]()
                            .options(Seq("Berlin" -> "BER"))(_._1)
                            .itemTemplate((c, code) => fragment(span(c), span.cssClass("code")(code)))
                            .value(vref)
                            .wired(oref, href, aref)
                    ).take(1).run
                yield out.mkString
        yield
            assert(open.contains("p-autocomplete"), "root class hook")
            assert(open.contains("p-autocomplete-input"), "Prime input class")
            assert(open.contains("p-inputtext"), "inputtext skin on the field")
            assert(open.contains("""value="ap""""), "input value from ref")
            assert(open.contains("""aria-expanded="true""""), "open: field reads expanded")
            assert(open.contains("p-uic-overlay-backdrop"), "open: outside-click backdrop (Overlay primitive)")
            assert(open.contains("p-uic-overlay-panel"), "open: overlay panel geometry class")
            assert(open.contains("p-autocomplete-overlay"), "open: Prime's panel skin class")
            assert(!open.contains("data-kyo-focus-auto"), "open: NO focus seed — focus stays in the input")
            assert(open.contains("p-autocomplete-list-container"), "open: scrollable list container")
            assert(open.contains("p-autocomplete-list"), "open: suggestion list class")
            assert(open.contains("""role="listbox""""), "open: listbox role on the ul")
            assert(open.contains("p-autocomplete-option"), "open: option row class")
            assert(open.contains("""role="option""""), "open: option role on rows")
            assert(open.contains("Apple"), "open: option matching the query is shown")
            assert(!open.contains("Banana"), "open: non-matching option filtered out")
            assert(open.contains("keydown"), "open: field keyboard registered (arrows/Enter/Escape)")
            assert(!closed.contains("p-autocomplete-overlay"), "closed ref: no panel")
            assert(empty.contains("p-autocomplete-empty-message"), "empty message row when nothing matches")
            assert(empty.contains("No match"), "empty message text")
            assert(!gated.contains("p-autocomplete-overlay"), "no panel below minQueryLength")
            assert(fullList.contains("Banana"), "dropdown/show-all bypasses the filter (full list)")
            assert(fullList.contains("p-autocomplete-dropdown"), "dropdown(true): Prime's trigger button")
            assert(highlight.contains("p-focus"), "highlight ref stamps Prime's .p-focus row")
            assert(!statics.contains("p-autocomplete-overlay"), "static projection: closed anatomy (mounted placeholder)")
            assert(statics.contains("p-autocomplete-loader"), "loading spinner slot (sheet class)")
            assert(statics.contains("p-autocomplete-clear-icon"), "clear button while the text is non-empty (sheet class)")
            assert(templated.contains("""class="code""""), "itemTemplate renders custom row content")
            assert(templated.contains("BER"), "itemTemplate content rendered")
        end for
    }

    "AutoComplete optionGroups renders grouped suggestions; a query that empties a group drops its header" in {
        def openHtml(ac: uic.AutoComplete[String], text: String, hi: Int = -1): String < Async =
            for
                vref <- Signal.initRef(text)
                oref <- Signal.initRef(true)
                href <- Signal.initRef(hi)
                aref <- Signal.initRef(false)
                out  <- UI.runRender(ac.value(vref).wired(oref, href, aref)).take(1).run
            yield out.mkString
        val base = uic.AutoComplete[String]().optionGroups(Seq(
            uic.OptionItem.group("Fruit")("Apple", "Apricot"),
            uic.OptionItem.group("Vegetable")("Artichoke")
        ))(identity)
        for
            wide    <- openHtml(base, "a")
            narrow  <- openHtml(base, "ap")
            thirdHi <- openHtml(base, "a", hi = 2)
        yield
            assert(wide.contains("p-autocomplete-option-group"), "Prime's header class stays on an li element")
            assert(wide.contains("""role="group""""), "the group is a real ARIA group")
            assert(wide.contains("""aria-label="Vegetable""""), "each group carries its label as its accessible name")
            assert(wide.contains("Artichoke"), "every matching suggestion renders")
            assert(narrow.contains("Apricot"), "the narrowed query keeps its own matches")
            assert(!narrow.contains("Vegetable"), "a group the query emptied loses its header with it")
            val focusAt = thirdHi.indexOf("p-focus")
            assert(
                focusAt > thirdHi.indexOf("Apricot") && focusAt < thirdHi.indexOf("Artichoke"),
                "hi=2 highlights the third suggestion, counting straight across the group boundary"
            )
        end for
    }

    "DatePicker inline(true) renders the in-flow Prime panel; month navigation + week numbers" in {
        def picker(vref: SignalRef[String], oref: SignalRef[Boolean])(using Frame): uic.DatePicker =
            uic.DatePicker().inline(true).value(vref).open(oref)

        for
            open <-
                for
                    vref <- Signal.initRef("2024-03-15")
                    oref <- Signal.initRef(true)
                    out  <- dpHtml(picker(vref, oref), oref)
                yield out.mkString
            closed <-
                for
                    vref <- Signal.initRef("2024-03-15")
                    oref <- Signal.initRef(false)
                    out  <- dpHtml(picker(vref, oref), oref)
                yield out.mkString
            alwaysOn <-
                for
                    vref <- Signal.initRef("2024-03-15")
                    on   <- Signal.initRef(true)
                    out  <- dpHtml(uic.DatePicker().inline(true).value(vref), on)
                yield out.mkString
            withMonth <-
                for
                    vref <- Signal.initRef("2024-03-15")
                    mref <- Signal.initRef("2024-05")
                    oref <- Signal.initRef(true)
                    out  <- dpHtml(uic.DatePicker().inline(true).value(vref).month(mref), oref, Present(mref))
                yield out.mkString
            weeks <-
                for
                    oref <- Signal.initRef(true)
                    out <- UI.runRender(
                        uic.DatePicker().inline(true).referenceDate("2026-07-01").showWeek(true).open(oref)
                    ).take(1).run
                yield out.mkString
        yield
            assert(open.contains("p-datepicker"), "open: root class hook")
            assert(open.contains("p-datepicker-input"), "open: Prime input class")
            assert(open.contains("p-datepicker-dropdown"), "open: dropdown toggle button")
            assert(open.contains("""data-uic-icon="calendar""""), "open: calendar glyph on the dropdown")
            assert(open.contains("p-datepicker-panel-inline"), "open: in-flow panel variant (Prime's inline)")
            assert(!open.contains("p-uic-overlay-panel"), "open: inline hosts no Overlay")
            assert(open.contains("p-datepicker-calendar-container"), "open: calendar container")
            assert(open.contains("p-datepicker-header"), "open: header")
            assert(open.contains("p-datepicker-prev-button"), "open: prev nav button")
            assert(open.contains("p-datepicker-next-button"), "open: next nav button")
            assert(open.contains("p-datepicker-select-month"), "open: month title")
            assert(open.contains("p-datepicker-select-year"), "open: year title")
            assert(open.contains("p-datepicker-day-view"), "open: day table")
            assert(open.contains("p-datepicker-weekday"), "open: weekday header")
            assert(open.contains("p-datepicker-day-cell"), "open: day cells")
            assert(open.contains("p-datepicker-other-month"), "open: adjacent-month lead-in cells")
            assert(open.contains("March"), "open: month derived from bound value (pure, no Date.now)")
            assert(open.contains("p-datepicker-day-selected"), "open: bound day highlighted")
            assert(!open.contains("p-datepicker-weeknumber"), "open: week numbers hidden by default (Prime showWeek=false)")
            assert(alwaysOn.contains("p-datepicker-panel-inline"), "inline without an open ref: always-visible panel (Prime parity)")
            assert(withMonth.contains("May"), "month ref overrides the displayed month")
            assert(weeks.contains("p-datepicker-weeknumber"), "showWeek renders the week-number column")
            assert(weeks.contains("p-datepicker-weekheader"), "showWeek renders the week header cell")
            assert(!closed.contains("p-datepicker-panel"), "closed: no panel rendered")
        end for
    }

    "DatePicker (open) hosts the SAME panel in the floating Overlay; static projection stays closed" in {
        for
            floating <-
                for
                    vref <- Signal.initRef("2024-03-15")
                    oref <- Signal.initRef(true)
                    out  <- dpHtml(uic.DatePicker().value(vref), oref)
                yield out.mkString
            closed <-
                for
                    vref <- Signal.initRef("2024-03-15")
                    oref <- Signal.initRef(false)
                    out  <- dpHtml(uic.DatePicker().value(vref), oref)
                yield out.mkString
            selfManaged <-
                for
                    vref <- Signal.initRef("2024-03-15")
                    out  <- UI.runRender(uic.DatePicker().value(vref)).take(1).run
                yield out.mkString
        yield
            assert(floating.contains("p-uic-overlay-backdrop"), "open: outside-click backdrop (Overlay primitive)")
            assert(floating.contains("p-uic-overlay-panel"), "open: overlay panel geometry class")
            assert(floating.contains("p-datepicker-panel"), "open: the SAME panel render inside the overlay")
            assert(!floating.contains("p-datepicker-panel-inline"), "open: no inline variant class on the floating panel")
            assert(floating.contains("p-uic-overlay-anchor"), "open: anchor glue class on the root")
            assert(!floating.contains("data-kyo-focus-auto"), "open: NO focus seed — focus stays on the field")
            assert(floating.contains("p-datepicker-day-view"), "open: day grid renders in the floating panel")
            assert(floating.contains("keydown"), "open: Escape registered (field + panel)")
            assert(!closed.contains("p-datepicker-panel"), "closed ref: no panel")
            assert(!closed.contains("p-uic-overlay-backdrop"), "closed ref: no backdrop")
            assert(!selfManaged.contains("p-datepicker-panel"), "self-managed static projection: closed anatomy (mounted placeholder)")
            assert(!selfManaged.contains("disabled"), "self-managed: dropdown button enabled-looking in the placeholder")
    }

    // ---- feedback & navigation controls (Phase 02) ----

    "Link renders a real anchor with href + the p-uic-link hook; disabled drops href" in {
        for
            html     <- renderHtml(uic.Link("Docs").href("/help").icon(uic.Icons.download))
            disabled <- renderHtml(uic.Link("Off").href("/x").disabled(true))
        yield
            assert(html.contains("p-uic-link"), "base class hook")
            assert(html.contains("<a"), "renders a real <a>")
            assert(html.contains("""href="/help""""), "href rendered")
            assert(html.contains("Docs"), "default-slot text")
            assert(html.contains("p-uic-link-icon"), "leading icon slot class")
            assert(html.contains("<svg"), "icon rendered as SVG")
            assert(disabled.contains("p-disabled"), "disabled: Prime's p-disabled skin")
            assert(disabled.contains("""aria-disabled="true""""), "disabled: aria-disabled set")
            assert(!disabled.contains("href="), "disabled: href dropped")
    }

    "Message renders Prime anatomy: severity token, text span, default icon; closable adds the button" in {
        for
            html     <- renderHtml(uic.Message().severity(uic.Severity.Danger)("Failed"))
            closable <- renderHtml(uic.Message().severity(uic.Severity.Success).closable(true).onDismissed(())("OK"))
            simple   <- renderHtml(uic.Message().variant(uic.MessageVariant.Simple).size(uic.Size.Small).hideIcon(true)("Hint"))
        yield
            assert(html.contains("p-message"), "base class hook")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("p-message-error"), "Danger maps to Prime's error token")
            assert(html.contains("""role="alert""""), "alert role")
            assert(html.contains("p-message-content-wrapper"), "content wrapper")
            assert(html.contains("p-message-content"), "content element")
            assert(html.contains("p-message-text"), "text slot wrapper")
            assert(html.contains("Failed"), "message text rendered")
            assert(html.contains("p-message-icon"), "design-derived default icon rendered")
            assert(html.contains("<svg"), "leading icon rendered as SVG")
            assert(!html.contains("p-message-close-button"), "not closable by default")
            assert(closable.contains("p-message-success"), "success token")
            assert(closable.contains("p-message-close-button"), "closable renders Prime's close button")
            assert(closable.contains("p-message-close-icon"), "close icon class")
            assert(closable.contains("click"), "onDismissed registers the click")
            assert(simple.contains("p-message-simple"), "simple variant class")
            assert(simple.contains("p-message-sm"), "small size class")
            assert(!simple.contains("p-message-icon"), "hideIcon suppresses the icon")
    }

    "Avatar renders Prime anatomy: shape/size hooks, initials, image, icon variants" in {
        for
            html  <- renderHtml(uic.Avatar().initials("AL").size(uic.Size.Large).shape(uic.AvatarShape.Circle))
            icon  <- renderHtml(uic.Avatar().icon(uic.Icons.user))
            image <- renderHtml(uic.Avatar().image("https://example.com/a.png").size(uic.Size.XLarge))
        yield
            assert(html.contains("p-avatar"), "base class hook")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("p-avatar-lg"), "large size class")
            assert(html.contains("p-avatar-circle"), "circle shape class")
            assert(html.contains("p-avatar-text"), "initials span class")
            assert(html.contains("""role="img""""), "img role")
            assert(html.contains("""aria-label="AL""""), "accessible name from initials")
            assert(html.contains("AL"), "initials rendered")
            assert(icon.contains("p-avatar-icon"), "icon slot class")
            assert(icon.contains("<svg"), "icon variant renders inline SVG")
            assert(image.contains("p-avatar-image"), "image modifier class")
            assert(image.contains("p-avatar-xl"), "xlarge size class")
            assert(image.contains("<img"), "image variant renders a native <img>")
            assert(image.contains("https://example.com/a.png"), "img src rendered")
    }

    "ProgressBar renders Prime anatomy + aria; reactive ref binds value; indeterminate mode" in {
        for
            const <- renderHtml(uic.ProgressBar().value(40))
            templ <- renderHtml(uic.ProgressBar().value(30).valueTemplate(v => s"$v of 100"))
            plain <- renderHtml(uic.ProgressBar().value(40).showValue(false))
            indet <- renderHtml(uic.ProgressBar().mode(uic.ProgressBarMode.Indeterminate))
            reactive <-
                for
                    ref <- Signal.initRef(75)
                    out <- UI.runRender(uic.ProgressBar().value(ref)).take(1).run
                yield out.mkString
        yield
            assert(const.contains("p-progressbar"), "base class hook")
            assert(const.contains("p-component"), "p-component class")
            assert(const.contains("p-progressbar-determinate"), "determinate mode class")
            assert(const.contains("""role="progressbar""""), "progressbar role")
            assert(const.contains("""aria-valuenow="40""""), "aria-valuenow from const value")
            assert(const.contains("""aria-valuemin="0""""), "aria-valuemin")
            assert(const.contains("""aria-valuemax="100""""), "aria-valuemax")
            assert(const.contains("p-progressbar-value"), "value bar element")
            assert(const.contains("p-progressbar-label"), "label element")
            assert(const.contains("40%"), "bar width and label reflect the value")
            assert(templ.contains("30 of 100"), "valueTemplate formats the label")
            assert(!plain.contains("p-progressbar-label"), "showValue(false) drops the label")
            assert(indet.contains("p-progressbar-indeterminate"), "indeterminate mode class")
            assert(!indet.contains("p-progressbar-label"), "indeterminate renders no label")
            assert(reactive.contains("""aria-valuenow="75""""), "aria-valuenow derived from ref")
            assert(reactive.contains("75%"), "bar width derived from ref value")
    }

    "Toast (open) renders Prime message anatomy with severity + position; (closed) renders nothing" in {
        def toast(ref: SignalRef[Boolean])(using Frame): UI =
            uic.Toast()
                .open(ref)
                .position(uic.OverlayPosition.BottomRight)
                .severity(uic.Severity.Success)
                .summary("Saved")
                .detail("Changes stored.")
                .closable(true)
                .duration(5000)
        for
            open <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(toast(ref)).take(1).run
                yield out.mkString
            closed <-
                for
                    ref <- Signal.initRef(false)
                    out <- UI.runRender(toast(ref)).take(1).run
                yield out.mkString
            danger <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(uic.Toast().open(ref).severity(uic.Severity.Danger).summary("Failed")).take(1).run
                yield out.mkString
        yield
            assert(open.contains("p-toast"), "open: region class hook")
            assert(open.contains("p-toast-bottom-right"), "open: position token class")
            assert(open.contains("p-toast-message"), "open: message element")
            assert(open.contains("p-toast-message-success"), "open: severity token class")
            assert(open.contains("p-toast-message-content"), "open: content element")
            assert(open.contains("p-toast-message-icon"), "open: severity icon")
            assert(open.contains("""data-uic-icon="check""""), "open: success glyph")
            assert(open.contains("p-toast-message-text"), "open: text slot")
            assert(open.contains("p-toast-summary"), "open: summary span")
            assert(open.contains("Saved"), "open: summary text")
            assert(open.contains("p-toast-detail"), "open: detail element")
            assert(open.contains("Changes stored."), "open: detail text")
            assert(open.contains("p-toast-close-button"), "open: close button (closable)")
            assert(open.contains("p-toast-close-icon"), "open: close icon")
            assert(open.contains("""data-uic-duration="5000""""), "open: duration data hook (pure — no timer)")
            assert(open.contains("""role="alert""""), "open: alert role")
            assert(danger.contains("p-toast-message-error"), "Danger maps to Prime's error token")
            assert(danger.contains("p-toast-top-right"), "default position is Prime's top-right")
            assert(!danger.contains("p-toast-close-button"), "not closable by default")
            assert(!closed.contains("p-toast"), "closed: nothing rendered")
        end for
    }

    "Breadcrumb renders Prime anatomy: nav landmark, item links, chevron separators, home item" in {
        for
            html <- renderHtml(
                uic.Breadcrumb().home(uic.Icons.home, "/").item("Reports", "/reports").item("Q2")
            )
        yield
            assert(html.contains("p-breadcrumb"), "base class hook")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("<nav"), "nav landmark element")
            assert(html.contains("""aria-label="Breadcrumb""""), "breadcrumb nav label")
            assert(html.contains("<ol"), "ordered list")
            assert(html.contains("p-breadcrumb-list"), "list class")
            assert(html.contains("p-breadcrumb-home-item"), "home item class")
            assert(html.contains("p-breadcrumb-item-link"), "item link class")
            assert(html.contains("p-breadcrumb-item-label"), "item label span")
            assert(html.contains("""href="/reports""""), "linked crumb renders href")
            assert(html.contains("p-breadcrumb-separator"), "separator between items")
            assert(html.contains("""data-uic-icon="chevron-right""""), "chevron separator glyph")
            assert(html.contains("""data-uic-icon="home""""), "home icon glyph")
            assert(html.contains("""aria-current="page""""), "final crumb marked current")
            assert(html.contains("Q2"), "current-page crumb text")
    }

    "Breadcrumb marks only the last crumb current, however many carry no link" in {
        for
            html <- renderHtml(uic.Breadcrumb().home(uic.Icons.home, "/").item("Structure").item("DataTable"))
        yield
            // A trail can pass through something that is not a page: the demo's own group crumb
            // names a shelf in its navigation and has nothing to open.
            assert(html.split("""aria-current="page"""", -1).length - 1 == 1, "exactly one current crumb")
            // The mark opens the crumb it belongs to, so between the two labels it can only be
            // the last one's.
            val between = html.substring(html.indexOf("Structure"), html.indexOf("DataTable"))
            assert(between.contains("""aria-current="page""""), "and it is the last crumb that carries it")
            assert(!html.contains("""href="Structure""""), "the intermediate crumb is not turned into a link either")
    }

    "Toolbar renders Prime anatomy: role + start/center/end sections" in {
        for
            html <- renderHtml(
                uic.Toolbar().start(uic.Button("A")).center(span("mid")).end(uic.Button("B"))
            )
        yield
            assert(html.contains("p-toolbar"), "base class hook")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("""role="toolbar""""), "toolbar role")
            assert(html.contains("p-toolbar-start"), "start section")
            assert(html.contains("p-toolbar-center"), "center section")
            assert(html.contains("p-toolbar-end"), "end section")
            assert(html.contains("A"), "start content rendered")
            assert(html.contains("mid"), "center content rendered")
            assert(html.contains("B"), "end content rendered")
    }

    // ---- structural / composite controls (Phase 03) ----

    "Listbox renders Prime anatomy with aria-selected reflecting a bound Set ref" in {
        for
            html <-
                for
                    ref <- Signal.initRef(Set("b"))
                    ui = uic.Listbox()
                        .selectionMode(uic.SelectionMode.Multiple)
                        .item("Apple", "a")
                        .item("Banana", "b", icon = Present(uic.Icons.check))
                        .value(ref)
                    out <- UI.runRender(ui).take(1).run
                yield out.mkString
            checked <- renderHtml(uic.Listbox().checkmark(true).item("Solo", "s"))
            empty   <- renderHtml(uic.Listbox().emptyContent("Nothing here"))
        yield
            assert(html.contains("p-listbox"), "base class hook")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("p-listbox-list-container"), "list container element")
            assert(html.contains("p-listbox-list"), "list class")
            assert(html.contains("<ul"), "renders a real <ul>")
            assert(html.contains("""role="listbox""""), "listbox role on the ul")
            assert(html.contains("p-listbox-option"), "option row class")
            assert(html.contains("""role="option""""), "option role on rows")
            assert(html.contains("""aria-selected="true""""), "bound item marked selected from the Set ref")
            assert(html.contains("""aria-selected="false""""), "unbound item not selected")
            assert(html.contains("p-listbox-option-selected"), "selected modifier on the bound row")
            assert(html.contains("p-uic-option-icon"), "leading icon slot class")
            assert(html.contains("<svg"), "leading icon rendered as SVG")
            assert(html.contains("Apple"), "first item text")
            assert(html.contains("Banana"), "second item text")
            assert(checked.contains("p-listbox-option-blank-icon"), "checkmark renders the blank placeholder on unselected rows")
            assert(empty.contains("p-listbox-empty-message"), "empty message row class")
            assert(empty.contains("Nothing here"), "empty message text")
    }

    "Listbox filterQuery renders Prime's header input bound to the query ref and filters options" in {
        for
            html <-
                for
                    query <- Signal.initRef("ap")
                    ui = uic.Listbox()
                        .filterQuery(query)
                        .item("Apple", "a")
                        .item("Banana", "b")
                    out <- UI.runRender(ui).take(1).run
                yield out.mkString
        yield
            assert(html.contains("p-listbox-header"), "filter header element")
            assert(html.contains("p-listbox-filter"), "filter input class")
            assert(html.contains("p-inputtext"), "filter input carries the inputtext skin")
            assert(html.contains("""value="ap""""), "input value from the query ref")
            assert(html.contains("Apple"), "matching option shown")
            assert(!html.contains("Banana"), "non-matching option filtered out")
    }

    "Listbox filterable(true) renders the same header over a query it allocates itself" in {
        for
            self  <- renderHtml(uic.Listbox().filterable(true).item("Apple", "a").item("Banana", "b"))
            plain <- renderHtml(uic.Listbox().item("Apple", "a").item("Banana", "b"))

            wired <-
                for
                    q   <- Signal.initRef("ap")
                    out <- UI.runRender(uic.Listbox().filterQuery(q).filterable(true).item("Apple", "a").item("Banana", "b")).take(1).run
                yield out.mkString
        yield
            // The static projection is the header, inert: the query ref only exists once the
            // mount publishes, exactly like Select's open/highlight state.
            assert(self.contains("p-listbox-header"), "filterable: header renders without an app-owned ref")
            assert(self.contains("p-listbox-filter"), "filterable: Prime's filter input class")
            assert(self.contains("Apple") && self.contains("Banana"), "filterable: nothing filtered before a query")
            assert(!plain.contains("p-listbox-header"), "no filtering asked for: no header at all")
            assert(!wired.contains("Banana"), "an app-owned query still filters when filterable is also set")
    }

    "Listbox optionGroups renders grouped options; the filter drops a group it empties" in {
        for
            html <- renderHtml(
                uic.Listbox().optionGroups(
                    uic.OptionItem.group("Fruit")(uic.ListItem.of("Apple", "a"), uic.ListItem.of("Banana", "b")),
                    uic.OptionItem.item(uic.ListItem.of("Date", "d"))
                )
            )
            filtered <-
                for
                    qref <- Signal.initRef("app")
                    ui = uic.Listbox().optionGroups(
                        uic.OptionItem.group("Fruit")(uic.ListItem.of("Apple", "a")),
                        uic.OptionItem.group("Vegetable")(uic.ListItem.of("Carrot", "c"))
                    ).filterQuery(qref)
                    out <- UI.runRender(ui).take(1).run
                yield out.mkString
        yield
            assert(html.contains("p-listbox-option-group"), "Prime's header class stays on an li element")
            assert(html.contains("""role="group""""), "the group is a real ARIA group")
            assert(html.contains("""aria-label="Fruit""""), "the group carries its label as its accessible name")
            assert(html.contains("p-uic-option-group-list"), "nested list holding the header and the options")
            assert(html.contains("""role="none""""), "the nested list drops out of the accessibility tree")
            assert(html.contains("Date"), "a loose OptionItem.item renders beside the group")
            assert(filtered.contains("Apple"), "filter: the matching option stays")
            assert(!filtered.contains("Vegetable"), "filter: a group the query emptied loses its header with it")
            assert(!filtered.contains("Carrot"), "filter: the emptied group's options are gone")
        end for
    }

    "Listbox roving: the list is the only tab stop and the only focus target" in {
        def occurrences(html: String, needle: String): Int = java.util.regex.Pattern.quote(needle).r.findAllIn(html).size
        def listbox(using Frame) = uic.Listbox()
            .selectionMode(uic.SelectionMode.Single)
            .item("Apple", "a")
            .item("Banana", "b")
        for
            hi    <- Signal.initRef(-1)
            roved <- renderHtml(listbox.resolved(Set("a"), "", Present(hi)))
            rows  <- renderHtml(listbox.resolved(Set("a"), ""))
        yield
            assert(occurrences(roved, """tabindex="0"""") == 1, "roving: one tab stop, the list")
            // A row with tabindex="-1" stays click-focusable: the click parked the real focus on
            // it, the next key press promoted it to :focus-visible, and the browser's own ring
            // then sat on that row while the highlight moved away from it.
            assert(occurrences(roved, """tabindex="-1"""") == 0, "roving: no row is a focus target")
            assert(roved.contains("focus") && roved.contains("blur"), "roving: the list registers focus and blur")
            assert(occurrences(rows, """tabindex="0"""") == 2, "no highlight to rove: the rows are the tab stops")
            assert(!rows.contains("""tabindex="-1""""), "no highlight to rove: and none of them is roved")
        end for
    }

    "Tree roving: the list owns focus alone, and hands it to a host that says so" in {
        def tree(using Frame) = uic.Tree()
            .nodes(uic.TreeNode("src", "src", children = List(uic.TreeNode("main", "main"))))
            .selectionMode(uic.SelectionMode.Single)
        for
            hi   <- Signal.initRef(0)
            own  <- renderHtml(tree.resolved(Set("src"), Set.empty, Present(uic.Roving(hi, 0, "t"))))
            host <- renderHtml(tree.resolved(Set("src"), Set.empty, Present(uic.Roving(hi, 0, "t", ownsFocus = false))))
        yield
            assert(own.contains("""tabindex="0""""), "the tree list is the tab stop")
            assert(own.contains("p-focus"), "and the focused row wears the ring Prime draws")
            assert(own.contains("focus") && own.contains("blur"), "it registers focus and blur to keep that row honest")
            // A hosted tree owns no part of the focus story: TreeSelect keeps focus on its trigger,
            // wears the tree's key handler there, and announces the highlight from there. A tab
            // stop here would be a second focus owner inside the host's own panel.
            assert(!host.contains("""tabindex="0""""), "hosted: not a tab stop")
            assert(!host.contains("blur"), "hosted: focus and blur belong to the host that holds them")
            assert(!host.contains("activedescendant"), "hosted: and the announcement belongs there too")
        end for
    }

    "DataTable computes sort + filter + pagination + selection server-side into Prime anatomy" in {
        final case class Product(id: String, name: String, price: Int)
        val products = List(
            Product("p1", "Bamboo Watch", 65),
            Product("p2", "Black Watch", 72),
            Product("p3", "Blue Band", 79),
            Product("p4", "Gold Ring", 40)
        )
        def tableOf(
            sort: SignalRef[List[uic.SortKey]],
            query: SignalRef[String],
            page: SignalRef[Int],
            sel: SignalRef[Set[String]],
            exp: SignalRef[Set[String]]
        )(using Frame): UI =
            uic.DataTable[Product]()
                .rows(products)
                .rowKey(_.id)
                .columns(
                    uic.Column[Product]("Name")(_.name).sortBy(_.name),
                    uic.Column[Product]("Price").body(p => span(s"$$${p.price}")).sortBy(_.price).align(uic.ColumnAlign.End)
                )
                .sort(sort)
                .globalFilter(query)
                .paginate(2)(page)
                .selectionMode(uic.SelectionMode.Checkbox)
                .selected(sel)
                .expanded(exp)
                .rowExpansionTemplate(prod => p(s"Details for ${prod.name}"))
                .stripedRows(true)
                .showGridlines(true)
                .emptyContent("Nothing found")

        def render(
            sortV: List[uic.SortKey],
            queryV: String,
            pageV: Int,
            selV: Set[String],
            expV: Set[String]
        ): String < Async =
            for
                sort  <- Signal.initRef(sortV)
                query <- Signal.initRef(queryV)
                page  <- Signal.initRef(pageV)
                sel   <- Signal.initRef(selV)
                exp   <- Signal.initRef(expV)
                out   <- UI.runRender(tableOf(sort, query, page, sel, exp)).take(1).run
            yield out.mkString
        for

            base     <- render(Nil, "", 0, Set("p2"), Set("p1"))
            sortedD  <- render(List(uic.SortKey("Price", uic.SortDirection.Descending)), "", 0, Set.empty, Set.empty)
            filtered <- render(Nil, "watch", 0, Set.empty, Set.empty)
            page2    <- render(Nil, "", 1, Set.empty, Set.empty)
            empty    <- render(Nil, "zzz", 0, Set.empty, Set.empty)
        yield

            assert(base.contains("p-datatable"), "root class hook")
            assert(base.contains("p-component"), "p-component class")
            assert(base.contains("p-datatable-hoverable"), "selectable table is hoverable")
            assert(base.contains("p-datatable-striped"), "striped modifier")
            assert(base.contains("p-datatable-gridlines"), "gridlines modifier")
            assert(base.contains("p-datatable-table-container"), "table container")
            assert(base.contains("p-datatable-table"), "table class")
            assert(base.contains("<table"), "renders a real <table>")
            assert(base.contains("p-datatable-header-cell"), "header cell class")
            assert(base.contains("p-datatable-column-header-content"), "header content wrapper")
            assert(base.contains("p-datatable-column-title"), "column title span")
            assert(base.contains("p-datatable-sortable-column"), "sortable column class")
            assert(base.contains("p-datatable-sort-icon"), "sort icon slot")
            assert(base.contains("""data-uic-icon="sort-alt""""), "unsorted glyph")
            assert(groupTag("thead", "p-datatable-thead").findFirstIn(base).isDefined, "header rows sit in a real thead")
            assert(groupTag("tbody", "p-datatable-tbody").findFirstIn(base).isDefined, "body rows sit in a real tbody")
            assert(base.contains("p-row-odd"), "odd-row class for striping")
            assert(base.contains("p-datatable-row-selected"), "selected row class from the Set ref")
            assert(base.contains("p-checkbox"), "Checkbox mode renders Prime's checkbox anatomy")
            assert(base.contains("p-checkbox-checked"), "selected row's checkbox is checked")
            assert(base.contains("p-datatable-row-toggle-button"), "expander button column")
            assert(base.contains("p-datatable-row-expansion"), "expansion row for the expanded key")
            assert(base.contains("Details for Bamboo Watch"), "expansion template content")
            assert(base.contains("p-uic-dt-end"), "column alignment class")
            assert(base.contains("p-paginator"), "embedded paginator")
            assert(base.contains("p-paginator-page-selected"), "current page highlighted")
            assert(base.contains("Bamboo Watch"), "page 1 row rendered")
            assert(!base.contains("Gold Ring"), "page 2 row not rendered on page 1")
            assert(sortedD.contains("""aria-sort="descending""""), "descending aria-sort")
            assert(sortedD.contains("""data-uic-icon="sort-amount-down""""), "descending glyph")
            assert((sortedD.indexOf("Blue Band") < sortedD.indexOf("Black Watch")), "rows sorted by price descending")
            assert(filtered.contains("Bamboo Watch"), "filter keeps matching rows")
            assert(!filtered.contains("Blue Band"), "filter drops non-matching rows")
            assert(page2.contains("Blue Band"), "page 2 shows the next slice")
            assert(!page2.contains("Bamboo Watch"), "page 2 hides the first slice")
            assert(empty.contains("p-datatable-empty-message"), "empty message row")
            assert(empty.contains("Nothing found"), "empty message text")
        end for
    }

    // A lazily loaded table is the same anatomy with the three passes taken out: it renders
    // the page it was handed and paginates over a total it could not have counted itself.
    "DataTable renders a lazily loaded page verbatim and paginates over the total it was given" in {
        final case class Row(id: String, name: String) derives CanEqual
        val page = List(Row("3", "Carol"), Row("4", "Alice"))
        for
            sort  <- Signal.initRef(List(uic.SortKey("Name", uic.SortDirection.Ascending)))
            query <- Signal.initRef("zzz")
            at    <- Signal.initRef(1)
            base = uic.DataTable[Row]()
                .rows(page)
                .rowKey(_.id)
                .columns(
                    uic.Column[Row]("Name")(_.name).sortable(true),
                    uic.Column[Row]("Id")(_.id)
                )
                .sort(sort)
                .globalFilter(query)
                .paginate(2)(at)
                .emptyContent("Nothing found")
            lazily <- renderHtml(base.lazyRows(9).render)
            owned  <- renderHtml(base.render)
        yield
            assert(lazily.indexOf("Carol") < lazily.indexOf("Alice"), "the page renders in the order it arrived")
            assert(lazily.contains("""aria-label="Page 5""""), "nine rows of two make five pages")
            assert(lazily.contains("""aria-current="page""""), "and the bound page is the current one")
            assert(lazily.contains("p-datatable-sortable-column"), "a column that says it sorts still offers it")
            assert(lazily.contains("""data-uic-icon="sort-amount-up-alt""""), "and shows the direction the spec asks for")
            assert(owned.contains("p-datatable-empty-message"), "the same table reading the query itself empties instead")
            assert(!owned.contains("""aria-label="Page 5""""), "and counts the pages off what it kept")
        end for
    }

    "DataTable pins frozen rows in a second row group that holds under the header" in {
        final case class Row(id: String, name: String) derives CanEqual
        for
            err <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
            top <- Signal.initRef(Present(41): Maybe[Int])
            table = uic.DataTable[Row]()
                .rows(List(Row("1", "Alice"), Row("2", "Bob")))
                .rowKey(_.id)
                .columns(uic.Column[Row]("Name")(_.name))
                .scrollHeight("240px")
                .frozenRows(List(Row("9", "Total")))
            out <- renderHtml(table.wired("t", Map.empty, err, _ => (), headTop = Present(top)))
        yield
            assert(out.contains("p-datatable-scrollable-table"), "the rule that makes a row group sticky is scoped to one")
            assert(
                out.contains("""class="p-datatable-tbody p-datatable-frozen-tbody""""),
                "the frozen group carries the body class too, or its cells lose every rule keyed on it"
            )
            assert(out.indexOf("p-datatable-frozen-tbody") < out.indexOf(">Alice<"), "and it comes before the body")
            assert(out.contains("top: 41px"), "holding at the height the header was measured at")
            assert(out.contains("""id="t-head""""), "which is the element the observation names")
            assert(out.contains(">Total<") && out.contains(">Alice<"), "both groups render their own rows")
        end for
    }

    "DataTable windows its rows inside Prime's scroller, between two spacer rows" in {
        final case class Row(id: String, name: String) derives CanEqual
        val rows = (1 to 40).toList.map(i => Row(i.toString, s"R$i"))
        for
            err    <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
            scroll <- Signal.initRef(0.0)
            table = uic.DataTable[Row]()
                .rows(rows)
                .rowKey(_.id)
                .columns(uic.Column[Row]("Name")(_.name))
                .scrollHeight("200px")
                .scrollRows(40, overscan = 0)
            out <- renderHtml(table.wired("t", Map.empty, err, _ => (), scroll = Present(scroll)))
        yield
            // Prime's own nesting for a virtually scrolled table, which the extracted
            // sheet names beside the plain one.
            assert(
                out.contains("p-datatable-table-container") &&
                    out.indexOf("p-virtualscroller") > out.indexOf("p-datatable-table-container") &&
                    out.indexOf("<table") > out.indexOf("p-virtualscroller"),
                "the scroller sits between the container and the table"
            )
            assert(out.contains("p-uic-vs-viewport"), "and it is the element that scrolls")
            assert(out.contains("height: 200px"), "at the height the caller gave it")
            assert(out.contains("p-datatable-scrollable"), "which is what pins the header to its edge")
            assert(out.contains("p-datatable-virtualscroller-spacer"), "Prime's spacer row holds what is not drawn")
            assert(out.contains("height: 1360px"), "thirty-four rows of forty behind the six that are")
            // The group is as tall as the whole list, which the spacers add up to anyway.
            // It is what keeps the table from collapsing under the scroll position while
            // the rows are rewritten, since they are rewritten in document order.
            assert(out.contains("height: 1600px"), "and the body says the height of all forty")
            assert(out.contains(">R6<") && !out.contains(">R7<"), "the window reaches six rows and stops")
            assert(out.contains("""data-uic-vs-first="0""""), "window start attr")
            assert(out.contains("""data-uic-vs-count="6""""), "window size attr")
            assert(!out.contains("p-paginator"), "a windowed table scrolls instead of paginating")
        end for
    }

    "DataTable ranks multi-sort columns with a badge and gives the checkbox column a select-all header" in {
        final case class Row(id: String, a: String, b: String)
        val rows = List(Row("r1", "x", "p"), Row("r2", "y", "q"))
        def render(sortV: List[uic.SortKey], selV: Set[String])(using Frame): String < Async =
            for
                sort <- Signal.initRef(sortV)
                sel  <- Signal.initRef(selV)
                out <- UI.runRender(
                    uic.DataTable[Row]()
                        .rows(rows)
                        .rowKey(_.id)
                        .columns(
                            uic.Column[Row]("A")(_.a).sortBy(_.a),
                            uic.Column[Row]("B")(_.b).sortBy(_.b)
                        )
                        .sort(sort)
                        .selectionMode(uic.SelectionMode.Checkbox)
                        .selected(sel)
                ).take(1).run
            yield out.mkString
        for
            unsorted <- render(Nil, Set.empty)
            single   <- render(List(uic.SortKey.ascending("A")), Set.empty)
            multi    <- render(List(uic.SortKey.ascending("B"), uic.SortKey("A", uic.SortDirection.Descending)), Set.empty)
            none     <- render(Nil, Set.empty)
            some     <- render(Nil, Set("r1"))
            all      <- render(Nil, Set("r1", "r2"))
        yield
            assert(!unsorted.contains("p-datatable-sort-badge"), "no badge while nothing is sorted")
            // One sorted column needs no ordinal: the icon already says everything.
            assert(!single.contains("p-datatable-sort-badge"), "no badge on a single sort key")
            assert(multi.contains("p-datatable-sort-badge"), "multi-sort ranks the columns")
            assert(multi.contains("p-badge"), "the rank rides Prime's badge skin")
            // Column A renders first and carries rank 2, because the spec's first
            // entry (B) is the primary key.
            assert(multi.indexOf(">2<") < multi.indexOf(">1<"), "the ordinal follows the spec order, not the column order")
            // Prime's select-all is binary: a partial selection reads unchecked.
            assert(none.contains("p-checkbox"), "checkbox column stamps a select-all header")
            assert(!none.contains("p-checkbox-checked"), "nothing selected: header unchecked")
            assert("p-checkbox-checked".r.findAllIn(some).size == 1, "one row selected: only that row's box is checked")
            assert("p-checkbox-checked".r.findAllIn(all).size == 3, "all rows selected: both rows plus the header")
        end for
    }

    "an unsorted sort key holds its slot: no sorted skin, no rank, but the columns behind it keep theirs" in {
        final case class Row(id: String, a: String, b: String)
        val rows = List(Row("r1", "y", "q"), Row("r2", "x", "p"))
        def render(sortV: List[uic.SortKey])(using Frame): String < Async =
            for
                sort <- Signal.initRef(sortV)
                out <- UI.runRender(
                    uic.DataTable[Row]()
                        .rows(rows)
                        .rowKey(_.id)
                        .columns(
                            uic.column("A")(_.a).sortBy(_.a),
                            uic.column("B")(_.b).sortBy(_.b)
                        )
                        .sort(sort)
                ).take(1).run
            yield out.mkString
        for
            both <- render(List(uic.SortKey.ascending("A"), uic.SortKey.ascending("B")))
            held <- render(List(uic.SortKey("A", uic.SortDirection.Unsorted), uic.SortKey.ascending("B")))
        yield
            assert("p-datatable-sort-badge".r.findAllIn(both).size == 2, "two sorting keys, two ranks")
            // A holds slot 0 while unsorted, so B is the only sorting key: no ranks at all,
            // and A renders as a plain sortable header rather than a sorted one.
            assert(!held.contains("p-datatable-sort-badge"), "one sorting key needs no ordinal")
            assert("p-datatable-column-sorted".r.findAllIn(held).size == 1, "only B wears the sorted skin")
            assert(held.contains("""data-uic-icon="sort-alt""""), "A falls back to the neutral glyph")
            assert(held.indexOf("Row") < 0 && held.indexOf(">x<") < held.indexOf(">y<"), "rows follow B ascending")
        end for
    }

    "emptyContent takes arbitrary UI, in the same slot the text occupies" in {
        def emptyState(using Frame): UI =
            uic.FlexBox().vertical(true).gap(8)(
                uic.Icon(uic.Icons.inbox),
                span("No products yet"),
                uic.Button("Add the first one")
            )
        for
            tableText <- renderHtml(uic.DataTable[String]().columns(uic.column("Name")(identity)).emptyContent("Nothing"))
            tableUi   <- renderHtml(uic.DataTable[String]().columns(uic.column("Name")(identity)).emptyContent(emptyState))
            viewUi    <- renderHtml(uic.DataView[String]().emptyContent(emptyState))
            listUi    <- renderHtml(uic.Listbox().emptyContent(emptyState))
            treeUi    <- renderHtml(uic.Tree().emptyContent(emptyState))
        yield
            assert(tableText.contains("Nothing"), "the String form still renders text")
            // The UI form must land INSIDE the same wrapper, not beside the table: that
            // is the whole reason it belongs to the component rather than the caller.
            assert(tableUi.contains("p-datatable-empty-message"), "table keeps Prime's empty row")
            assert(tableUi.contains("No products yet"), "and hosts the UI inside it")
            assert(tableUi.contains("p-button"), "including interactive content")
            assert(viewUi.contains("p-dataview-empty-message"), "DataView keeps its empty wrapper")
            assert(viewUi.contains("No products yet"), "and hosts the UI")
            assert(listUi.contains("p-listbox-empty-message"), "Listbox keeps its empty row")
            assert(listUi.contains("No products yet"), "and hosts the UI")
            assert(treeUi.contains("p-tree-empty-message"), "Tree keeps its empty row")
            assert(treeUi.contains("No products yet"), "and hosts the UI")
        end for
    }

    "DataTable header/footer slots, column footers, loading mask and scroll height render Prime anatomy" in {
        final case class Item(id: String, name: String, price: Int)
        val items = List(Item("a", "Alpha", 10), Item("b", "Beta", 20), Item("c", "Gamma", 30))
        def render(query: String, table: uic.DataTable[Item])(using Frame): String < Async =
            for
                q   <- Signal.initRef(query)
                out <- UI.runRender(table.globalFilter(q).render).take(1).run
            yield out.mkString

        val base = uic.DataTable[Item]()
            .rows(items)
            .rowKey(_.id)
            .columns(
                uic.Column[Item]("Name")(_.name).footer("Total"),
                uic.Column[Item]("Price")(_.price.toString)
                    .align(uic.ColumnAlign.End)
                    .footer(rs => span(rs.map(_.price).sum.toString))
            )
        for
            all      <- render("", base.header(p("toolbar")).footer(p("3 items")))
            filtered <- render("alpha", base)
            busy     <- render("", base.loading(true))
            idle     <- render("", base.loading(false))
            scrolled <- render("", base.scrollHeight("240px"))
        yield
            assert(groupTag("tfoot", "p-datatable-tfoot").findFirstIn(all).isDefined, "columns with a footer add a tfoot")
            assert(all.contains("p-datatable-column-footer"), "footer cell content class")
            assert(all.contains(">Total<"), "static column footer label")
            assert(all.contains(">60<"), "computed column footer aggregates every row")
            assert(all.contains("class=\"p-datatable-header\""), "header slot wrapper")
            assert(all.contains("toolbar"), "header slot content")
            assert(all.contains("class=\"p-datatable-footer\""), "footer slot wrapper")
            assert(all.contains("3 items"), "footer slot content")
            // The table owns filtering, so the aggregate has to follow the filter
            // rather than the caller's unfiltered list.
            assert(filtered.contains(">10<"), "computed footer aggregates the FILTERED rows")
            assert(!filtered.contains(">60<"), "filtered-out rows leave the aggregate")
            assert(!filtered.contains("class=\"p-datatable-header\""), "no header slot without one bound")
            assert(busy.contains("p-datatable-mask"), "loading renders Prime's mask")
            assert(busy.contains("p-overlay-mask"), "mask dims through the shared overlay skin")
            assert(busy.contains("p-progressspinner"), "mask holds the spinner")
            assert(!idle.contains("p-datatable-mask"), "loading(false) renders no mask")
            assert(scrolled.contains("p-datatable-scrollable"), "scroll height marks the root scrollable")
            assert(scrolled.contains("p-datatable-scrollable-table"), "and the table, which pins the row groups")
            assert(scrolled.contains("max-height: 240px"), "container capped at the given length")
            assert(!all.contains("p-datatable-scrollable"), "no scroll classes without a scroll height")
        end for
    }

    "DataTable nests grouping levels, and a group is identified by its path, not its key" in {
        final case class Item(id: String, category: String, brand: String, name: String, price: Int)
        val items = List(
            Item("1", "Watches", "Rolex", "Sub", 10),
            Item("2", "Watches", "Rolex", "GMT", 20),
            Item("3", "Bands", "Rolex", "Jubilee", 7),
            Item("4", "Bands", "Casio", "Strap", 5)
        )
        def occurrences(html: String, needle: String): Int = needle.r.findAllIn(html).size

        val base = uic.DataTable[Item]()
            .rows(items)
            .rowKey(_.id)
            .columns(uic.column("Name")(_.name), uic.column("Price")(_.price.toString))
        for
            flat   <- renderHtml(base.render)
            one    <- renderHtml(base.groupBy(uic.group(_.category)).render)
            nested <- renderHtml(base.groupBy(uic.group(_.category), uic.group(_.brand)).render)
            pathed <- renderHtml(
                base.groupBy(
                    uic.group(_.category),
                    uic.group(_.brand).header((path, rows) => span(s"${path.keys.mkString("/")}:${rows.size}"))
                ).render
            )
            summed <- renderHtml(
                base.groupBy(
                    uic.group(_.category).footer((path, rows) => span(s"${path.key} total ${rows.map(_.price).sum}")),
                    uic.group(_.brand).showHeader(false).footer((_, rows) => span(s"sub ${rows.map(_.price).sum}"))
                ).render
            )
            // Watches and Bands are open, and Rolex is open UNDER WATCHES ONLY. The Rolex
            // sitting under Bands carries the same key and a different path, so it stays
            // shut: that is the whole reason the ref holds paths.
            open <-
                for
                    ref <- Signal.initRef(Set(
                        uic.GroupPath("Watches"),
                        uic.GroupPath("Bands"),
                        uic.GroupPath("Watches", "Rolex")
                    ))
                    out <- UI.runRender(
                        base.groupBy(uic.group(_.category), uic.group(_.brand)).expandedGroups(ref).render
                    ).take(1).run
                yield out.mkString
        yield
            val headerRow = groupTag("tr", "p-datatable-row-group-header")
            val footerRow = groupTag("tr", "p-datatable-row-group-footer")
            assert(headerRow.findAllIn(flat).isEmpty, "no group rows without a level")
            assert(headerRow.findAllIn(one).size == 2, "one header row per run of the single level")
            assert(one.contains("colspan=\"2\""), "the header cell spans the whole grid")
            // Two categories, and three brand runs, because Rolex is split across them.
            assert(headerRow.findAllIn(nested).size == 5, "a header row per group at every level")
            assert(nested.indexOf(">Watches<") < nested.indexOf(">Rolex<"), "the outer level heads its groups")
            assert(occurrences(nested, ">Rolex<") == 2, "one Rolex group per parent, not one overall")
            assert(pathed.contains(">Watches/Rolex:2<"), "the header template sees the whole path and its rows")
            assert(pathed.contains(">Bands/Rolex:1<"), "including for the same key under another parent")
            // Levels close innermost first, so a level's summary sits inside its parent's.
            assert(footerRow.findAllIn(summed).size == 5, "a summary row per group at every level")
            assert(headerRow.findAllIn(summed).size == 2, "showHeader(false) drops that level's header rows")
            assert(summed.contains(">sub 30<") && summed.contains(">Watches total 30<"), "each level aggregates itself")
            assert(summed.indexOf(">sub 30<") < summed.indexOf(">Watches total 30<"), "the inner summary closes first")
            assert(occurrences(open, "p-datatable-row-toggle-button") == 5, "every header row carries a toggle")
            assert(open.contains(">Sub<") && open.contains(">GMT<"), "the open path renders its rows")
            assert(!open.contains(">Jubilee<"), "the same key under another parent stays shut")
            assert(!open.contains(">Strap<"), "and so does a sibling that was never opened")
            assert(headerRow.findAllIn(open).size == 5, "collapsing hides rows, not the headers they hang off")
        end for
    }

    "a merge key is any comparable type, and a group key labels itself through toString" in {
        final case class Item(id: String, year: Int, tier: Boolean, name: String)
        val items = List(
            Item("1", 2024, true, "Alpha"),
            Item("2", 2024, true, "Beta"),
            Item("3", 2024, false, "Gamma"),
            Item("4", 2025, false, "Delta")
        )
        def occurrences(html: String, needle: String): Int = needle.r.findAllIn(html).size

        for
            // An Int key, and a tuple of two fields: neither reaches a String on the way.
            byYear <- renderHtml(
                uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                    uic.column("Year")(_.year.toString).rowSpan(_.year),
                    uic.column("Name")(_.name)
                ).render
            )
            byBoth <- renderHtml(
                uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                    uic.column("Year")(_.year.toString).rowSpan(i => (i.year, i.tier)),
                    uic.column("Name")(_.name)
                ).render
            )
            grouped <- renderHtml(
                uic.DataTable[Item]().rows(items).rowKey(_.id)
                    .columns(uic.column("Name")(_.name))
                    .groupBy(uic.group(_.year), uic.group(_.tier)).render
            )
        yield
            assert(byYear.contains("rowspan=\"3\""), "an Int key merges its run")
            assert(occurrences(byYear, ">2024<") == 1, "as one cell for the three rows")
            // The tuple splits 2024 in two, which a key that ignored `tier` would not.
            assert(byBoth.contains("rowspan=\"2\""), "a tuple key merges by both fields")
            assert(!byBoth.contains("rowspan=\"3\""), "so the run stops where either changes")
            assert(occurrences(byBoth, ">2024<") == 2, "and 2024 is emitted once per tier")
            // A level renders and identifies its key by toString, so a non-String key needs
            // no projection of its own and a String key passes through untouched.
            assert(grouped.contains(">2024<") && grouped.contains(">2025<"), "the Int level labels itself")
            assert(grouped.contains(">true<") && grouped.contains(">false<"), "and so does the Boolean one")
            assert(groupTag("tr", "p-datatable-row-group-header").findAllIn(grouped).size == 5, "two levels of groups")
        end for
    }

    "a rowSpan column merges its runs, clipped by the merged columns left of it and by the group" in {
        final case class Item(id: String, category: String, brand: String, name: String)
        val items = List(
            Item("1", "Watches", "Rolex", "Sub"),
            Item("2", "Watches", "Rolex", "GMT"),
            Item("3", "Bands", "Rolex", "Jubilee"),
            Item("4", "Bands", "Casio", "Strap")
        )
        def occurrences(html: String, needle: String): Int = needle.r.findAllIn(html).size

        for
            // Brand alone would merge all three Rolex rows into one cell crossing the
            // Category boundary, which is exactly the overlap the clipping prevents.
            merged <- renderHtml(
                uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                    uic.column("Category")(_.category).rowSpan,
                    uic.column("Brand")(_.brand).rowSpan,
                    uic.column("Name")(_.name)
                ).render
            )
            loose <- renderHtml(
                uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                    uic.column("Category")(_.category),
                    uic.column("Brand")(_.brand).rowSpan,
                    uic.column("Name")(_.name)
                ).render
            )
            grouped <- renderHtml(
                uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                    uic.column("Brand")(_.brand).rowSpan,
                    uic.column("Name")(_.name)
                ).groupBy(uic.group(_.category)).render
            )
            templated <- renderHtml(
                uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                    uic.column("Brand").body(i => span(i.brand)).rowSpan(_.brand),
                    uic.column("Name")(_.name)
                ).render
            )
            expanded <-
                for
                    ref <- Signal.initRef(Set("1"))
                    out <- UI.runRender(
                        uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                            uic.column("Category")(_.category).rowSpan,
                            uic.column("Name")(_.name)
                        ).rowExpansionTemplate(i => span(s"detail ${i.name}")).expanded(ref).render
                    ).take(1).run
                yield out.mkString
        yield
            // Category twice over two rows, and Brand once, for Rolex inside Watches.
            assert(occurrences(merged, "rowspan=\"2\"") == 3, "each run spans exactly its own rows")
            assert(!merged.contains("rowspan=\"3\""), "no span crosses the column left of it")
            assert(occurrences(merged, ">Rolex<") == 2, "Rolex merges within a category, not across two")
            assert(occurrences(merged, ">Casio<") == 1, "a run of one row is emitted plainly")
            assert(!merged.contains("rowspan=\"1\""), "and states no span it does not have")
            assert(occurrences(merged, ">Sub<") == 1, "unmarked columns still render per row")
            // Nothing to the left is merged here, so the three Rolex rows are one run.
            assert(loose.contains("rowspan=\"3\""), "an unclipped run spans every row it covers")
            assert(occurrences(loose, ">Rolex<") == 1, "which is one cell for all three")
            assert(occurrences(grouped, ">Rolex<") == 2, "a group clips a run the same way a merged column does")
            assert(templated.contains("rowspan=\"3\""), "an explicit key merges a body-only column")
            // The span counts table ROWS, so the expansion row of an expanded row joins it.
            assert(expanded.contains("rowspan=\"3\""), "an expanded row's expansion row joins the span")
            assert(expanded.contains("p-datatable-row-expansion"), "the expansion row is there")
        end for
    }

    "a headerGroup gives the table a multi-row header whose spans are read off the tree" in {
        final case class Sale(id: String, code: String, q1: String, q2: String, total: String)
        val sales                                          = List(Sale("1", "A-1", "10", "20", "30"), Sale("2", "A-2", "5", "6", "11"))
        def occurrences(html: String, needle: String): Int = needle.r.findAllIn(html).size
        def twoYears(using Frame) = uic.DataTable[Sale]().rows(sales).rowKey(_.id).columns(
            uic.headerGroup("2024")(uic.column("Q1")(_.q1).sortBy(_.q1)),
            uic.headerGroup("2025")(uic.column("Q1")(_.q2).sortBy(_.q2))
        )

        for
            grouped <- renderHtml(
                uic.DataTable[Sale]().rows(sales).rowKey(_.id).columns(
                    uic.column("Code")(_.code),
                    uic.headerGroup("Revenue")(
                        uic.headerGroup("2025")(uic.column("Q1")(_.q1), uic.column("Q2")(_.q2)),
                        uic.column("Total")(_.total)
                    )
                ).render
            )
            flat <- renderHtml(
                uic.DataTable[Sale]().rows(sales).rowKey(_.id).columns(uic.column("Code")(_.code)).render
            )
            checked <-
                for
                    ref <- Signal.initRef(Set.empty[String])
                    out <- UI.runRender(
                        uic.DataTable[Sale]().rows(sales).rowKey(_.id).columns(
                            uic.headerGroup("Revenue")(uic.column("Q1")(_.q1), uic.column("Q2")(_.q2))
                        ).selectionMode(uic.SelectionMode.Checkbox).selected(ref).render
                    ).take(1).run
                yield out.mkString
            empty <- renderHtml(
                uic.DataTable[Sale]().rows(sales).rowKey(_.id).columns(
                    uic.headerGroup("Nothing")(),
                    uic.column("Code")(_.code)
                ).render
            )
            // The same header under two groups is two paths, so a qualified spec picks one
            // of them and neither needs renaming.
            qualified <-
                for
                    ref <- Signal.initRef(List(uic.SortKey.ascending("2025", "Q1")))
                    out <- UI.runRender(twoYears.sort(ref).render).take(1).run
                yield out.mkString
            unqualified <-
                for
                    ref <- Signal.initRef(List(uic.SortKey.ascending("Q1")))
                    out <- UI.runRender(twoYears.sort(ref).render).take(1).run
                yield out.mkString
            samePath <-
                for
                    ref <- Signal.initRef(List.empty[uic.SortKey])
                    out <- UI.runRender(
                        uic.DataTable[Sale]().rows(sales).rowKey(_.id).columns(
                            uic.headerGroup("2025")(
                                uic.column("Q1")(_.q1).sortBy(_.q1),
                                uic.column("Q1")(_.q2).sortBy(_.q2)
                            )
                        ).sort(ref).render
                    ).take(1).run
                yield out.mkString
        yield
            // Three levels deep: Code and Revenue, then 2025 and Total, then Q1 and Q2.
            assert(occurrences(grouped, "<tr") == 2 + 3, "one header row per level, plus the two data rows")
            assert(grouped.contains("colspan=\"3\""), "Revenue spans the three columns under it")
            assert(grouped.contains("colspan=\"2\""), "and 2025 the two under itself")
            assert(occurrences(grouped, "rowspan=\"3\"") == 1, "Code reaches the bottom of the header")
            assert(occurrences(grouped, "rowspan=\"2\"") == 1, "and Total from its own level down")
            assert(occurrences(grouped, "<td") == 8, "the body still has one cell per leaf column")
            // A group states no span it does not have, and a flat header states none at all.
            assert(!grouped.contains("colspan=\"1\"") && !grouped.contains("rowspan=\"1\""))
            assert(!flat.contains("colspan=") && !flat.contains("rowspan="), "a one-row header spans nothing")
            // The checkbox column sits beside the leaves, not beside the group, so it has to
            // reach down the full header the way an ungrouped column does.
            assert(checked.contains("rowspan=\"2\""), "the select-all cell reaches down the header")
            assert(empty.contains(">Code<") && !empty.contains(">Nothing<"), "an empty group renders no cell")
            assert(empty.contains("p-uic-key-error") && empty.contains("Nothing"), "and is reported instead")
            // Both headers read "Q1", so which one the spec picked shows in which th carries
            // the sorted class: the second, whose path is 2025 / Q1.
            val firstQ1  = qualified.indexOf(">Q1<")
            val secondQ1 = qualified.indexOf(">Q1<", firstQ1 + 1)
            val sortedTh = qualified.indexOf("p-datatable-column-sorted")
            assert(occurrences(qualified, "p-datatable-column-sorted") == 1, "one column sorts")
            assert(sortedTh > firstQ1 && sortedTh < secondQ1, "and it is the one the path names")
            assert(!qualified.contains("p-uic-key-error"), "and a shared header under two groups is no fault")
            assert(unqualified.contains("p-uic-key-error"), "a path missing its group labels matches nothing")
            assert(samePath.contains("p-uic-key-error"), "two columns reachable by one path are reported")
        end for
    }

    "Column.sortable gates the affordance without hiding the sort the spec applied" in {
        final case class Row(id: String, name: String, note: String)
        val rows                                           = List(Row("1", "B", "x"), Row("2", "A", "y"))
        def occurrences(html: String, needle: String): Int = needle.r.findAllIn(html).size

        for
            open <-
                for
                    ref <- Signal.initRef(List(uic.SortKey.ascending("Name")))
                    out <- UI.runRender(
                        uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                            uic.column("Name")(_.name).sortBy(_.name)
                        ).sort(ref).render
                    ).take(1).run
                yield out.mkString
            locked <-
                for
                    ref <- Signal.initRef(List(uic.SortKey.ascending("Name")))
                    out <- UI.runRender(
                        uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                            uic.column("Name")(_.name).sortBy(_.name).sortable(false)
                        ).sort(ref).render
                    ).take(1).run
                yield out.mkString
            idle <-
                for
                    ref <- Signal.initRef(List.empty[uic.SortKey])
                    out <- UI.runRender(
                        uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                            uic.column("Name")(_.name).sortBy(_.name).sortable(false)
                        ).sort(ref).render
                    ).take(1).run
                yield out.mkString
            reactive <-
                for
                    flag <- Signal.initRef(false)
                    ref  <- Signal.initRef(List.empty[uic.SortKey])
                    out <- UI.runRender(
                        uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                            uic.column("Name")(_.name).sortBy(_.name).sortable(flag)
                        ).sort(ref).render
                    ).take(1).run
                yield out.mkString
            noOrdering <-
                for
                    ref <- Signal.initRef(List.empty[uic.SortKey])
                    out <- UI.runRender(
                        uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                            uic.column("Note")(_.note).sortable(true)
                        ).sort(ref).render
                    ).take(1).run
                yield out.mkString
            tree <-
                for
                    ref <- Signal.initRef(List.empty[uic.SortKey])
                    out <- UI.runRender(
                        uic.TreeTable[Row]().nodes(uic.TreeTableNode(rows.head)).columns(
                            uic.column("Name")(_.name).sortBy(_.name).sortable(false)
                        ).sort(ref).render
                    ).take(1).run
                yield out.mkString
        yield
            // Unset, a column with an ordering is what it always was.
            assert(open.contains("p-datatable-sortable-column"), "an ordering alone still makes the header live")
            assert(occurrences(open, "tabindex=\"0\"") == 1, "and gives it a tab stop")
            assert(open.contains("p-datatable-column-sorted") && open.contains("p-datatable-sort-icon"))
            // sortable(false) drops the affordance and keeps the state, which is the pair
            // that lets a table sort by a column the reader may not re-sort.
            assert(!locked.contains("p-datatable-sortable-column"), "the affordance goes")
            assert(!locked.contains("tabindex="), "including the tab stop")
            assert(locked.contains("p-datatable-column-sorted"), "the sorted state stays")
            assert(locked.contains("aria-sort=\"ascending\""), "and so does what it says to a reader")
            assert(locked.contains("p-datatable-sort-icon"), "the direction icon is state, not affordance")
            assert(locked.indexOf(">A<") < locked.indexOf(">B<"), "and the spec still sorts the rows")
            // Nothing to show and nothing to offer: no icon at all.
            assert(!idle.contains("p-datatable-sort-icon"), "an inert unsorted column shows no icon")
            assert(!idle.contains("p-datatable-sortable-column"))
            // The reactive form resolves to its current value before the table builds.
            assert(!reactive.contains("p-datatable-sortable-column"), "a signal reading false is inert")
            assert(noOrdering.contains("p-uic-key-error"), "sortable(true) with no ordering is reported")
            assert(!tree.contains("p-treetable-sortable-column"), "TreeTable honors the same flag")
        end for
    }

    "Column.visible narrows the table to what the reader can see, without losing what it sorts by" in {
        final case class Row(id: String, name: String, note: String, code: String)
        val rows                                           = List(Row("1", "B", "x", "c1"), Row("2", "A", "y", "c2"))
        def occurrences(html: String, needle: String): Int = needle.r.findAllIn(html).size

        def render(t: Frame ?=> uic.DataTable[Row])(using Frame) =
            UI.runRender(t.render).take(1).run.map(_.mkString)

        for
            all <- render(uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Note")(_.note),
                uic.column("Code")(_.code)
            ))
            flagged <- render(uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name).visible(true),
                uic.column("Note")(_.note).visible(true),
                uic.column("Code")(_.code).visible(true)
            ))
            hidden <- render(uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Note")(_.note).visible(false),
                uic.column("Code")(_.code)
            ))
            grouped <- render(uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.headerGroup("Detail")(
                    uic.column("Note")(_.note).visible(false),
                    uic.column("Code")(_.code).visible(false)
                )
            ))
            footers <- render(uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Note")(_.note).footer("total").visible(false)
            ))
            empty <- render(uic.DataTable[Row]().rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Note")(_.note).visible(false),
                uic.column("Code")(_.code)
            ))
            filtered <-
                for
                    q <- Signal.initRef("x")
                    out <- render(uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                        uic.column("Name")(_.name),
                        uic.column("Note")(_.note).visible(false)
                    ).globalFilter(q))
                yield out
            sorted <-
                for
                    spec <- Signal.initRef(List(uic.SortKey(List("Note"), uic.SortDirection.Descending)))
                    out <- render(uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                        uic.column("Name")(_.name),
                        uic.column("Note")(_.note).sortBy(_.note).visible(false)
                    ).sort(spec))
                yield out
            tree <- UI.runRender(
                uic.TreeTable[Row]().nodes(uic.TreeTableNode(rows.head)).columns(
                    uic.column("Name")(_.name).visible(false),
                    uic.column("Code")(_.code)
                ).render
            ).take(1).run.map(_.mkString)
        yield
            // The flag written out as true is the default written down, so it may not
            // change one byte of what the table renders.
            assert(flagged == all, "visible(true) renders what an unflagged column renders")
            assert(hidden != all)
            // A hidden column contributes no cell anywhere, so the table is exactly as
            // wide as the columns left and every count follows.
            assert(!hidden.contains(">Note<"), "no header cell")
            assert(!hidden.contains(">x<") && !hidden.contains(">y<"), "and no body cells")
            assert(occurrences(hidden, "<th[ >]") == 2 && occurrences(all, "<th[ >]") == 3)
            assert(occurrences(hidden, "<td") == 4, "two rows of two columns")
            assert(empty.contains("colspan=\"2\""), "the empty message spans the columns that are there")
            // A header cell over nothing spans nothing, so the group leaves with its columns.
            assert(!grouped.contains(">Detail<"), "a group whose columns are all hidden goes with them")
            assert(grouped.contains(">Name<") && occurrences(grouped, "<tr") == 3, "one header row left, plus two rows")
            assert(!grouped.contains("p-uic-key-error"), "and it is not reported as an empty group")
            assert(!footers.contains("p-datatable-tfoot"), "the only footer was on a hidden column")
            // The filter matches what is ON THE SCREEN: the query hits the hidden column's
            // text alone, so it matches nothing.
            assert(!filtered.contains(">B<") && !filtered.contains(">A<"), "a hidden column is not searched")
            // The spec is the caller's, so hiding a column never reshuffles the rows: it
            // still sorts, and it is still not reported as a column the table cannot sort.
            assert(sorted.indexOf(">A<") < sorted.indexOf(">B<"), "the hidden column still sorts")
            assert(!sorted.contains("p-uic-key-error"), "and the spec that names it is not called unknown")
            assert(!tree.contains(">B<"), "TreeTable honors the same flag")
            assert(tree.contains("p-treetable-node-toggle-button"), "and the toggler moves onto the column that is left")
        end for
    }

    "per-column filters render Prime's inline filter row, and the mode menu it opens" in {
        final case class Row(id: String, name: String, price: Int) derives CanEqual
        val rows                                           = List(Row("1", "Bamboo", 65), Row("2", "Black", 72))
        def occurrences(html: String, needle: String): Int = java.util.regex.Pattern.quote(needle).r.findAllIn(html).size

        def table(using Frame) = uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
            uic.column("Name")(_.name).filterBy,
            uic.column("Note")(_ => "n"),
            uic.column("Price")(_.price.toString).filterBy(_.price)
        )

        for
            plain <- UI.runRender(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name)).render
            ).take(1).run.map(_.mkString)
            unbound <- UI.runRender(table.render).take(1).run.map(_.mkString)
            live <-
                for
                    specs <- Signal.initRef(Map.empty[List[String], uic.ColumnFilter])
                    err   <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
                    menu  <- Signal.initRef(false)
                    out <- UI.runRender(
                        table.columnFilters(specs).wired("t", Map.empty, err, _ => (), Map(List("Name") -> menu))
                    ).take(1).run
                yield out.mkString
            filtering <-
                for
                    specs <- Signal.initRef(Map(
                        List("Name")  -> uic.ColumnFilter("Bam", uic.MatchMode.StartsWith),
                        List("Price") -> uic.ColumnFilter("nope", uic.MatchMode.Equals)
                    ))
                    err  <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
                    menu <- Signal.initRef(false)
                    out <- UI.runRender(
                        table.columnFilters(specs).wired("t", Map.empty, err, _ => (), Map(List("Name") -> menu))
                    ).take(1).run
                yield out.mkString
            opened <-
                for
                    specs <- Signal.initRef(Map(List("Name") -> uic.ColumnFilter("Bam", uic.MatchMode.StartsWith)))
                    err   <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
                    menu  <- Signal.initRef(true)
                    out <- UI.runRender(
                        table.columnFilters(specs).wired("t", Map.empty, err, _ => (), Map(List("Name") -> menu))
                    ).take(1).run
                yield out.mkString
        yield
            // Nothing to filter, nothing bound: the header is what it always was.
            assert(!plain.contains("p-datatable-inline-filter"))
            assert(!unbound.contains("p-datatable-inline-filter"), "a pipeline with no state bound renders no row")
            assert(unbound.contains("p-uic-key-error"), "and says so")
            // One inline filter per filterable column, and an empty header cell over the
            // column that carries no pipeline.
            assert(occurrences(live, "p-datatable-inline-filter") == 2)
            assert(occurrences(live, "p-datatable-filter-element-container") == 2)
            assert(occurrences(live, "p-datatable-column-filter-button") == 2, "both columns offer more than one mode")
            assert(
                occurrences(live, "p-datatable-header-cell\"></th>") == 1,
                "the column that carries no pipeline keeps an empty cell, so the row still lines up"
            )
            assert(!live.contains("p-uic-key-error"), "a bound filter row reports nothing")
            // The funnel says whether its column is narrowing the table without being opened.
            assert(live.contains(uic.Icons.filter.pathData) && !live.contains(uic.Icons.filterFill.pathData))
            assert(filtering.contains(uic.Icons.filterFill.pathData), "a filtering column carries the filled funnel")
            assert(filtering.contains("value=\"Bam\""), "the input shows what was typed")
            assert(filtering.contains("p-invalid"), "and a query the column cannot read marks its own input")
            assert(filtering.contains(">Bamboo<") && !filtering.contains(">Black<"), "the rows the filter left")
            // The mode menu is the column's own list, with the current one marked.
            assert(!live.contains("p-datatable-filter-constraint-list"), "closed, it renders nothing")
            assert(opened.contains("p-datatable-filter-overlay"))
            assert(occurrences(opened, "p-datatable-filter-constraint\"") == 5, "six modes, one of them selected")
            assert(opened.contains("p-datatable-filter-constraint p-datatable-filter-constraint-selected"))
            assert(opened.contains(">Starts with<") && opened.contains(">Not contains<"))
            assert(!opened.contains(">Less than<"), "a text column offers no comparison")
        end for
    }

    "column widths render as a colgroup, and a bound map puts a handle on every boundary" in {
        final case class Row(id: String, name: String, price: Int) derives CanEqual
        val rows                                           = List(Row("1", "Bamboo", 65), Row("2", "Black", 72))
        def occurrences(html: String, needle: String): Int = java.util.regex.Pattern.quote(needle).r.findAllIn(html).size

        def table(using Frame) = uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
            uic.column("Name")(_.name).width(220),
            uic.column("Note")(_ => "n"),
            uic.column("Price")(_.price.toString)
        )

        for
            plain <- UI.runRender(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name)).render
            ).take(1).run.map(_.mkString)
            authored <- UI.runRender(table.render).take(1).run.map(_.mkString)
            live <-
                for
                    widths <- Signal.initRef(Map.empty[List[String], Double])
                    err    <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
                    out <- UI.runRender(
                        table.columnWidths(widths).wired("t", Map.empty, err, _ => ())
                    ).take(1).run
                yield out.mkString
            dragged <-
                for
                    widths <- Signal.initRef(Map(List("Name") -> 300.0, List("Note") -> 90.0))
                    err    <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
                    out <- UI.runRender(
                        table.columnWidths(widths).wired("t", Map.empty, err, _ => ())
                    ).take(1).run
                yield out.mkString
            pinned <-
                for
                    widths <- Signal.initRef(Map.empty[List[String], Double])
                    err    <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
                    out <- UI.runRender(
                        uic.DataTable[Row]().rows(rows).rowKey(_.id).selectionMode(uic.SelectionMode.Checkbox).columns(
                            uic.column("Name")(_.name),
                            uic.column("Price")(_.price.toString).resizable(false)
                        ).columnWidths(widths).wired("t", Map.empty, err, _ => ())
                    ).take(1).run
                yield out.mkString
            tree <- UI.runRender(
                uic.TreeTable[Row]().nodes(uic.TreeTableNode(rows.head)).columns(
                    uic.column("Name")(_.name).width(140),
                    uic.column("Price")(_.price.toString)
                ).render
            ).take(1).run.map(_.mkString)
        yield
            // A table nobody sized renders what it always rendered.
            assert(!plain.contains("<colgroup") && !plain.contains("p-uic-table-fixed"))
            assert(!plain.contains("p-datatable-column-resizer"))
            // One col per column, sized or not, and the layout mode that makes a width mean
            // what it says.
            assert(occurrences(authored, "<colgroup") == 1)
            assert(occurrences(authored, "<col ") == 3, "one per column, sized or not")
            assert(authored.contains("width: 220px"))
            assert(authored.contains("p-uic-table-fixed"))
            assert(!authored.contains("p-datatable-column-resizer"), "an authored width is not an invitation to drag it")
            assert(!authored.contains("p-datatable-resizable-table"))
            // Bound: Prime's resizable table, a handle per boundary, and an id on every
            // header cell, the last one included, since it is measured as a neighbour.
            assert(live.contains("p-datatable-resizable-table") && live.contains("p-datatable-resizable-table-fit"))
            assert(occurrences(live, "p-datatable-column-resizer") == 2, "three columns are two boundaries")
            assert(occurrences(live, "p-datatable-resizable-column") == 2, "and the last column carries none")
            assert(live.contains("id=\"t-h0\"") && live.contains("id=\"t-h2\""))
            // What the reader dragged wins over what the caller authored.
            assert(dragged.contains("width: 300px") && dragged.contains("width: 90px"))
            assert(!dragged.contains("width: 220px"), "the bound width replaces the authored one")
            // The checkbox column is a column of the table too, so the list has to count it
            // or every width would land one column to the left.
            assert(occurrences(pinned, "<col ") == 3, "the selection column gets a col of its own")
            assert(!pinned.contains("p-datatable-column-resizer"), "a pinned neighbour takes the only boundary away")
            assert(tree.contains("<colgroup") && tree.contains("width: 140px"), "a hierarchy sizes its columns the same way")
            assert(tree.contains("p-uic-table-fixed"))
        end for
    }

    "a reorderable header carries Prime's grip, and a drag renders the line it would land on" in {
        final case class Row(id: String, name: String, price: Int) derives CanEqual
        val rows                                           = List(Row("1", "Bamboo", 65), Row("2", "Black", 72))
        def occurrences(html: String, needle: String): Int = java.util.regex.Pattern.quote(needle).r.findAllIn(html).size

        def table(using Frame) = uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
            uic.column("Name")(_.name),
            uic.column("Note")(_ => "n"),
            uic.column("Price")(_.price.toString)
        )

        def wire(t: uic.DataTable[Row], drag: Maybe[uic.ColumnDrag] = Absent)(using Frame) =
            for
                order <- Signal.initRef(List.empty[List[String]])
                err   <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
                move  <- Signal.initRef(drag)
                out <- UI.runRender(
                    t.columnOrder(order).wired(
                        "t",
                        Map.empty,
                        err,
                        _ => (),
                        Map.empty,
                        (_: Seq[String]) => Chunk.empty[UI.Rect],
                        Absent,
                        Present(move)
                    )
                ).take(1).run
            yield out.mkString

        for
            plain <- UI.runRender(table.render).take(1).run.map(_.mkString)
            live  <- wire(table)
            grouped <- wire(uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.headerGroup("G")(uic.column("Note")(_ => "n"), uic.column("Price")(_.price.toString)),
                uic.column("Extra")(_ => "e")
            ))
            pinned <- wire(uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                uic.column("Name")(_.name),
                uic.column("Note")(_ => "n").reorderable(false)
            ))
            dragging <- wire(table, Present(uic.ColumnDrag(0, 1, 2, 0, List(0, 100, 200, 300), List(2, 3), moved = true)))
            ordered <-
                for
                    order <- Signal.initRef(List(List("Price")))
                    out   <- UI.runRender(table.columnOrder(order).render).take(1).run
                yield out.mkString
        yield
            // A table nobody bound an order to renders what it always rendered.
            assert(!plain.contains("p-datatable-reorderable-column"))
            // Three columns that may all trade places: every header cell is a grip, and
            // every one of them carries the id the grab measures it by.
            assert(occurrences(live, "p-datatable-reorderable-column") == 3)
            assert(live.contains("id=\"t-h0\"") && live.contains("id=\"t-h2\""), "measured without a width in sight")
            // A header of more than one row does not tile the same columns in each of them,
            // so a cell that counted its own row from zero would answer for the wrong
            // column and two cells would carry the same id.
            assert(
                List("t-h0", "t-h1", "t-h2", "t-h3").forall(i => occurrences(grouped, s"id=\"$i\"") == 1),
                "one id per column, whatever row its header cell sits in"
            )
            // A pinned column leaves its neighbour nowhere to go, so neither is a grip.
            assert(!pinned.contains("p-datatable-reorderable-column"))
            // The line sits on the cell the drop would land in front of, and the cell being
            // carried is the dimmed one.
            assert(dragging.contains("p-uic-dt-dragging"))
            assert(occurrences(dragging, "p-uic-dt-drop-before") == 1)
            assert(!dragging.contains("p-uic-dt-drop-after"), "the drop is not past the last column")
            // A seeded order is the order the header renders in, before anyone drags.
            assert(ordered.indexOf(">Price<") < ordered.indexOf(">Name<"))
        end for
    }

    "a frozen column renders Prime's sticky cell, at the offset the widths add up to" in {
        final case class Row(id: String, name: String, price: Int) derives CanEqual
        val rows                                           = List(Row("1", "Bamboo", 65), Row("2", "Black", 72))
        def occurrences(html: String, needle: String): Int = java.util.regex.Pattern.quote(needle).r.findAllIn(html).size

        for
            plain <- UI.runRender(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                    uic.column("Name")(_.name).width(220)
                ).render
            ).take(1).run.map(_.mkString)
            start <- UI.runRender(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                    uic.column("Name")(_.name).width(220).frozen(true),
                    uic.column("Price")(_.price.toString).width(120).frozen(true),
                    uic.column("Note")(_ => "n")
                ).render
            ).take(1).run.map(_.mkString)
            withLead <- UI.runRender(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).selectionMode(uic.SelectionMode.Checkbox).columns(
                    uic.column("Name")(_.name).width(220).frozen(true),
                    uic.column("Note")(_ => "n")
                ).render
            ).take(1).run.map(_.mkString)
            end <- UI.runRender(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                    uic.column("Name")(_.name),
                    uic.column("Price")(_.price.toString).width(150).frozen(uic.FrozenEdge.End),
                    uic.column("Note")(_ => "n").width(100).frozen(uic.FrozenEdge.End)
                ).render
            ).take(1).run.map(_.mkString)
        yield
            // A table that freezes nothing stays out of the scroll container: freezing is
            // what asks for one, since a column can only be frozen against something moving.
            assert(!plain.contains("p-datatable-scrollable") && !plain.contains("p-datatable-frozen-column"))
            assert(start.contains("p-datatable-scrollable") && start.contains("p-datatable-scrollable-table"))
            // Two rows of data plus the header row, for each of the two frozen columns.
            assert(occurrences(start, "p-datatable-frozen-column") == 6)
            assert(start.contains("left: 0"), "the first one sits on the edge")
            assert(start.contains("left: 220px"), "and the second stands off it by the first one's width")
            assert(!start.contains("left: 340px"), "the free column carries no offset of its own")
            // The checkbox column stands between the frozen one and the edge, so it holds
            // too, and its width is the same variable in the col and in the offset.
            assert(withLead.contains("width: calc(var(--p-uic-dt-select-width))"))
            assert(withLead.contains("left: calc(var(--p-uic-dt-select-width))"))
            assert(!withLead.contains("left: 220px"), "nothing is frozen past the sized column")
            assert(end.contains("right: 0") && end.contains("right: 100px"), "the trailing edge counts backwards")
            assert(!end.contains("left: "), "and a column held against one edge names only that one")
        end for
    }

    "a row the selection predicate rejects renders as a row nothing can pick" in {
        final case class Row(id: String, name: String) derives CanEqual
        val rows                                           = List(Row("1", "Bamboo"), Row("2", "Black"))
        def occurrences(html: String, needle: String): Int = java.util.regex.Pattern.quote(needle).r.findAllIn(html).size
        for
            ref <- Signal.initRef(Set.empty[String])
            open <- UI.runRender(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                    .selectionMode(uic.SelectionMode.Checkbox).selected(ref).render
            ).take(1).run.map(_.mkString)
            limited <- UI.runRender(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                    .selectionMode(uic.SelectionMode.Checkbox).selected(ref)
                    .selectableWhen(_.id != "2").render
            ).take(1).run.map(_.mkString)
        yield
            assert(
                occurrences(open, "p-checkbox p-component") == 3,
                "the select-all box and one per row"
            )
            assert(!open.contains("p-checkbox p-component p-disabled"))
            assert(
                occurrences(limited, "p-checkbox p-component p-disabled") == 1,
                "and with a predicate exactly the rejected one wears Prime's disabled class"
            )
        end for
    }

    "a rejected row keeps the pointer that says it cannot be picked off it" in {
        final case class Row(id: String, name: String) derives CanEqual
        val rows                                           = List(Row("1", "Bamboo"), Row("2", "Black"))
        def occurrences(html: String, needle: String): Int = java.util.regex.Pattern.quote(needle).r.findAllIn(html).size
        for
            ref <- Signal.initRef(Set.empty[String])
            all <- UI.runRender(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                    .selectionMode(uic.SelectionMode.Multiple).selected(ref).render
            ).take(1).run.map(_.mkString)
            some <- UI.runRender(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                    .selectionMode(uic.SelectionMode.Multiple).selected(ref)
                    .selectableWhen(_.id != "2").render
            ).take(1).run.map(_.mkString)
        yield
            assert(occurrences(all, "p-datatable-selectable-row") == 2)
            assert(occurrences(some, "p-datatable-selectable-row") == 1)
            assert(some.contains("aria-selected"), "the rejected row still says where it stands")
        end for
    }

    "the caller's row classes render after the table's own" in {
        final case class Row(id: String, name: String, price: Int) derives CanEqual
        val rows = List(Row("1", "Bamboo", 10), Row("2", "Black", 20))
        for
            out <- renderHtml(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name))
                    .stripedRows(true)
                    .rowClasses(r => if r.price > 15 then Seq("dear") else Nil).render
            )
        yield
            assert(out.contains("""class="p-row-odd dear""""), "appended, with Prime's own left in place")
            assert(!out.contains("""class="p-row-even dear""""), "and only on the row the caller named")
        end for
    }

    "an expanding table states its own width and keeps the last column's handle" in {
        final case class Row(id: String, name: String, price: Int) derives CanEqual
        val rows                                           = List(Row("1", "Bamboo", 65))
        def occurrences(html: String, needle: String): Int = java.util.regex.Pattern.quote(needle).r.findAllIn(html).size
        def table(using Frame) = uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
            uic.column("Name")(_.name).width(220),
            uic.column("Price")(_.price.toString).width(120)
        )
        for
            expand <-
                for
                    widths <- Signal.initRef(Map.empty[List[String], Double])
                    err    <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
                    out <- UI.runRender(
                        table.columnWidths(widths).columnResizeMode(uic.ColumnResizeMode.Expand)
                            .wired("t", Map.empty, err, _ => ())
                    ).take(1).run
                yield out.mkString
            fit <-
                for
                    widths <- Signal.initRef(Map.empty[List[String], Double])
                    err    <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
                    out    <- UI.runRender(table.columnWidths(widths).wired("t", Map.empty, err, _ => ())).take(1).run
                yield out.mkString
        yield
            assert(expand.contains("p-datatable-resizable-table"))
            assert(!expand.contains("p-datatable-resizable-table-fit"), "Prime's -fit is what hides the last handle")
            assert(occurrences(expand, "p-datatable-column-resizer") == 2, "a handle per column, not per boundary")
            assert(expand.contains("width: 340px"), "the table is as wide as its columns add up to")
            assert(
                expand.contains("p-datatable-scrollable") && expand.contains("p-datatable-scrollable-table"),
                "outgrowing the container only means anything where the container scrolls"
            )
            assert(fit.contains("p-datatable-resizable-table-fit"))
            assert(occurrences(fit, "p-datatable-column-resizer") == 1, "two columns are one boundary")
            assert(!fit.contains("p-datatable-scrollable"), "a fitting table never outgrows what it was given")
        end for
    }

    "reorderable rows render Prime's grip column at the leading edge" in {
        final case class Row(id: String, name: String) derives CanEqual
        val rows                                           = List(Row("1", "Bamboo"), Row("2", "Black"))
        def occurrences(html: String, needle: String): Int = java.util.regex.Pattern.quote(needle).r.findAllIn(html).size
        for
            plain <- renderHtml(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name).width(200)).render
            )
            grips <-
                for
                    bound <- Signal.initRef[Seq[Row]](rows)
                    err   <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
                    out <- UI.runRender(
                        uic.DataTable[Row]().rows(bound).rowKey(_.id)
                            .columns(uic.column("Name")(_.name).width(200))
                            .reorderableRows(true)
                            .wired("t", Map.empty, err, _ => ())
                    ).take(1).run
                yield out.mkString
        yield
            assert(!plain.contains("p-datatable-reorderable-row-handle"))
            assert(occurrences(grips, "p-datatable-reorderable-row-handle") == 2, "one grip per data row")
            assert(grips.contains("""data-uic-icon="bars""""), "Prime's own grip glyph")
            assert(
                grips.contains("width: calc(var(--p-uic-dt-handle-width))"),
                "the grip column is sized like the other leading ones, or it takes an equal share"
            )
            assert(occurrences(grips, "<col ") == 2, "the grip column gets a col of its own")
            assert(occurrences(grips, "<th") == 2, "and a header cell, or the body would be one column wider")
        end for
    }

    "a selected cell carries the kyo class and says so to a reader" in {
        final case class Row(id: String, name: String, price: Int) derives CanEqual
        val rows                                           = List(Row("1", "Bamboo", 65), Row("2", "Black", 72))
        def occurrences(html: String, needle: String): Int = java.util.regex.Pattern.quote(needle).r.findAllIn(html).size
        for
            cells <- Signal.initRef(Set(uic.CellPath("2", List("Price"))))
            out <- UI.runRender(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
                    uic.column("Name")(_.name),
                    uic.column("Price")(_.price.toString)
                ).selectionMode(uic.SelectionMode.Multiple).selectedCells(cells).render
            ).take(1).run.map(_.mkString)
            plain <- renderHtml(
                uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(uic.column("Name")(_.name)).render
            )
        yield
            assert(occurrences(out, "p-uic-dt-cell-selected") == 1, "exactly the cell the set names")
            assert(occurrences(out, """aria-selected="false"""") == 3, "and the other three say they are not")
            assert(!out.contains("p-datatable-row-selected"), "a cell selection is not a row selection")
            assert(!plain.contains("aria-selected"), "a table that picks nothing says nothing")
        end for
    }

    "the filter menu renders Prime's popover anatomy: the operator, the rules, the bar" in {
        final case class Row(id: String, name: String, price: Int) derives CanEqual
        val rows                                           = List(Row("1", "Bamboo", 65))
        def occurrences(html: String, needle: String): Int = java.util.regex.Pattern.quote(needle).r.findAllIn(html).size
        def table(using Frame) = uic.DataTable[Row]().rows(rows).rowKey(_.id).columns(
            uic.column("Name")(_.name).filterBy,
            uic.column("Price")(_.price.toString)
        )
        for
            closed <-
                for
                    filters <- Signal.initRef(Map.empty[List[String], uic.ColumnFilter])
                    err     <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
                    open    <- Signal.initRef(false)
                    draft   <- Signal.initRef(uic.ColumnFilter("", uic.MatchMode.Contains))
                    out <- UI.runRender(
                        table.columnFilters(filters).filterDisplay(uic.FilterDisplay.Menu)
                            .wired(
                                "t",
                                Map.empty,
                                err,
                                _ => (),
                                Map(List("Name") -> open),
                                filterDrafts = Map(List("Name") -> draft)
                            )
                    ).take(1).run
                yield out.mkString
            opened <-
                for
                    filters <- Signal.initRef(Map.empty[List[String], uic.ColumnFilter])
                    err     <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
                    open    <- Signal.initRef(true)
                    draft <- Signal.initRef(uic.ColumnFilter(
                        List(uic.FilterRule("a", uic.MatchMode.Contains), uic.FilterRule("b", uic.MatchMode.EndsWith)),
                        uic.FilterOperator.Or
                    ))
                    out <- UI.runRender(
                        table.columnFilters(filters).filterDisplay(uic.FilterDisplay.Menu)
                            .wired(
                                "t",
                                Map.empty,
                                err,
                                _ => (),
                                Map(List("Name") -> open),
                                filterDrafts = Map(List("Name") -> draft)
                            )
                    ).take(1).run
                yield out.mkString
            row <-
                for
                    filters <- Signal.initRef(Map.empty[List[String], uic.ColumnFilter])
                    err     <- Signal.initRef(Absent: Maybe[(uic.CellPath, uic.form.FieldError)])
                    out     <- UI.runRender(table.columnFilters(filters).wired("t", Map.empty, err, _ => ())).take(1).run
                yield out.mkString
        yield
            // Closed: the funnel sits in the header cell, pushed to the trailing edge, and
            // only the filterable column has one.
            assert(occurrences(closed, "p-datatable-popover-filter") == 1)
            assert(closed.contains("p-datatable-column-filter-button"))
            assert(!closed.contains("p-datatable-filter-overlay-popover"), "the panel is not rendered while it is shut")
            assert(!closed.contains("p-datatable-inline-filter"), "and there is no filter row")
            // Open: Prime's popover, the operator over two rules, and the bar.
            assert(opened.contains("p-datatable-filter-overlay-popover"))
            assert(opened.contains("p-datatable-filter-operator-dropdown"))
            assert(occurrences(opened, "p-datatable-filter-rule") >= 2, "the list and one rule per condition")
            assert(occurrences(opened, "p-datatable-filter-remove-rule-button") == 2, "one per rule, since there are two")
            assert(opened.contains("p-datatable-filter-add-rule-button"))
            assert(opened.contains("p-datatable-filter-buttonbar"))
            assert(opened.contains("Match Any"), "the operator shows which join is running")
            // The row display is the other shape, and it is what a table renders by default.
            assert(row.contains("p-datatable-inline-filter") && !row.contains("p-datatable-popover-filter"))
        end for
    }

    "editing renders the column's own editor over the table's draft, and reports what it refuses" in {
        final case class Item(id: String, name: String, price: Int) derives CanEqual
        val items                                          = List(Item("1", "A", 10), Item("2", "B", 20))
        def occurrences(html: String, needle: String): Int = needle.r.findAllIn(html).size

        def table(using Frame) = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
            uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)),
            uic.column("Price")(_.price.toString).editable(_.price)((i, v) => i.copy(price = v)),
            uic.column("Note")(_ => "n")
        ).onCellValueChanged(_ => ())

        /** The editing anatomy lives behind a mount, so a top-down render shows the
          * placeholder. `wired` is the seam that publishes it.
          */
        def wired(t: uic.DataTable[Item], cell: Maybe[uic.CellPath] = Absent, rows: Set[String] = Set.empty)(using Frame) =
            for
                drafts <- Signal.initRef("name-draft")
                price  <- Signal.initRef("99")
                err    <- Signal.initRef(Absent: Maybe[(uic.CellPath, kyo.uic.form.FieldError)])
                cellR  <- Signal.initRef(cell)
                rowsR  <- Signal.initRef(rows)
                out <- UI.runRender(
                    t.editingCell(cellR).wired("t", Map(List("Name") -> drafts, List("Price") -> price), err, _ => ())
                ).take(1).run
            yield out.mkString

        for
            placeholder <-
                for
                    ref <- Signal.initRef(Absent: Maybe[uic.CellPath])
                    out <- UI.runRender(table.editingCell(ref).render).take(1).run
                yield out.mkString
            idle    <- wired(table)
            open    <- wired(table, cell = Present(uic.CellPath("2", List("Name"))))
            numeric <- wired(table, cell = Present(uic.CellPath("1", List("Price"))))
            rowOpen <-
                for
                    drafts <- Signal.initRef("name-draft")
                    price  <- Signal.initRef("99")
                    err    <- Signal.initRef(Absent: Maybe[(uic.CellPath, kyo.uic.form.FieldError)])
                    rowsR  <- Signal.initRef(Set("1"))
                    out <- UI.runRender(
                        table.editingRows(rowsR).wired("t", Map(List("Name") -> drafts, List("Price") -> price), err, _ => ())
                    ).take(1).run
                yield out.mkString
            refused <-
                for
                    drafts <- Signal.initRef("nope")
                    price  <- Signal.initRef("nope")
                    err <- Signal.initRef(
                        Maybe((
                            uic.CellPath("1", List("Price")),
                            kyo.uic.form.FieldError(
                                "integer",
                                Map.empty,
                                Present("nope is not a whole number")
                            )
                        ))
                    )
                    cellR <- Signal.initRef(Present(uic.CellPath("1", List("Price"))): Maybe[uic.CellPath])
                    out <- UI.runRender(
                        table.editingCell(cellR).wired("t", Map(List("Name") -> drafts, List("Price") -> price), err, _ => ())
                    ).take(1).run
                yield out.mkString
            perRow <-
                for
                    drafts <- Signal.initRef("d")
                    err    <- Signal.initRef(Absent: Maybe[(uic.CellPath, kyo.uic.form.FieldError)])
                    cellR  <- Signal.initRef(Absent: Maybe[uic.CellPath])
                    out <- UI.runRender(
                        uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                            uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)).editableWhen(_.id == "1")
                        ).onCellValueChanged(_ => ()).editingCell(cellR).wired("t", Map(List("Name") -> drafts), err, _ => ())
                    ).take(1).run
                yield out.mkString
            noEditable <-
                for
                    ref <- Signal.initRef(Absent: Maybe[uic.CellPath])
                    out <- UI.runRender(
                        uic.DataTable[Item]().rows(items).rowKey(_.id)
                            .columns(uic.column("Name")(_.name)).editingCell(ref).render
                    ).take(1).run
                yield out.mkString
            nowhereToSave <-
                for
                    ref <- Signal.initRef(Absent: Maybe[uic.CellPath])
                    out <- UI.runRender(
                        uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                            uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v))
                        ).editingCell(ref).render
                    ).take(1).run
                yield out.mkString
            twoSources <-
                for
                    rowsR <- Signal.initRef[Seq[Item]](items)
                    ref   <- Signal.initRef(Absent: Maybe[uic.CellPath])
                    out   <- UI.runRender(table.rows(rowsR).editingCell(ref).render).take(1).run
                yield out.mkString
            bothModes <-
                for
                    rows <- Signal.initRef(Set.empty[String])
                    cell <- Signal.initRef(Absent: Maybe[uic.CellPath])
                    out  <- UI.runRender(table.editingRows(rows).editingCell(cell).render).take(1).run
                yield out.mkString
            noKey <-
                for
                    ref <- Signal.initRef(Absent: Maybe[uic.CellPath])
                    out <- UI.runRender(
                        uic.DataTable[Item]().rows(items).columns(
                            uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v))
                        ).onCellValueChanged(_ => ()).editingCell(ref).render
                    ).take(1).run
                yield out.mkString
        yield
            // The placeholder is the table without its editing state: same rows, no editor.
            assert(placeholder.contains("p-datatable-table"), "the placeholder is still the table")
            assert(!placeholder.contains("p-editable-column"), "and carries no editing affordance")
            // Cell mode marks what can be opened, one cell at a time is open.
            assert(occurrences(idle, "p-editable-column") == 4, "two editable columns over two rows")
            assert(!idle.contains("p-cell-editing"), "and nothing is open")
            assert(!idle.contains("p-datatable-row-editor-init"), "cell mode adds no editor column")
            assert(occurrences(open, "p-cell-editing") == 1, "exactly one cell is open")
            // The open cell shows the column's editor over the draft, not the row's text.
            assert(open.contains("p-inputtext"), "the text column's editor is Prime's input")
            assert(open.contains("""value="name-draft""""), "opened on the draft the table seeded")
            assert(open.contains("""data-kyo-focus-auto="1""""), "and it takes focus, since it is new")
            assert(numeric.contains("p-inputnumber"), "the Int column's editor is Prime's number field")
            assert(numeric.contains("""value="99""""), "over its own draft")
            // Row mode opens every editable column of the row and keeps the button column.
            assert(occurrences(rowOpen, "p-datatable-editing-row") == 1)
            assert(occurrences(rowOpen, "p-inputtext") >= 1 && rowOpen.contains("p-inputnumber"), "both editors open")
            assert(rowOpen.contains("p-datatable-row-editor-save") && rowOpen.contains("p-datatable-row-editor-cancel"))
            assert(occurrences(rowOpen, "p-datatable-row-editor-init") == 1, "the other row still rests")
            // A refused commit keeps the cell open and says why.
            assert(refused.contains("p-cell-editing") && refused.contains("p-invalid"), "the cell stays open, marked")
            assert(refused.contains("nope is not a whole number"), "with the error's own message")
            // editableWhen gates the affordance per row.
            assert(occurrences(perRow, "p-editable-column") == 1, "only the row the predicate admits can open")
            // What a binding cannot do, it says.
            assert(noEditable.contains("p-uic-key-error"), "editing bound with no editable column is reported")
            assert(nowhereToSave.contains("p-uic-key-error"), "and an editable column the table cannot save from")
            assert(twoSources.contains("p-uic-key-error"), "and two row lists")
            assert(bothModes.contains("p-uic-key-error"), "and binding both modes at once")
            assert(noKey.contains("p-uic-key-error"), "editing is keyed by rowKey, so it needs one")
        end for
    }

    "cell navigation makes the editable cells the tab stops, and leaves a plain table alone" in {
        final case class Item(id: String, name: String, note: String) derives CanEqual
        val items                                          = List(Item("1", "A", "n1"), Item("2", "B", "n2"))
        def occurrences(html: String, needle: String): Int = needle.r.findAllIn(html).size

        def navTable(using Frame) = uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
            uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)),
            uic.column("Note")(_.note)
        ).onCellValueChanged(_ => ())

        for
            nav <-
                for
                    draft <- Signal.initRef("d")
                    err   <- Signal.initRef(Absent: Maybe[(uic.CellPath, kyo.uic.form.FieldError)])
                    cell  <- Signal.initRef(Absent: Maybe[uic.CellPath])
                    out <- UI.runRender(
                        navTable.editingCell(cell).wired("t", Map(List("Name") -> draft), err, _ => ())
                    ).take(1).run
                yield out.mkString
            // A column nobody may land on is stepped over, and never a tab stop.
            locked <-
                for
                    draft <- Signal.initRef("d")
                    err   <- Signal.initRef(Absent: Maybe[(uic.CellPath, kyo.uic.form.FieldError)])
                    cell  <- Signal.initRef(Absent: Maybe[uic.CellPath])
                    out <- UI.runRender(
                        uic.DataTable[Item]().rows(items).rowKey(_.id).columns(
                            uic.column("Name")(_.name).editable(_.name)((i, v) => i.copy(name = v)),
                            uic.column("Note")(_.note).navigable(false)
                        ).onCellValueChanged(_ => ()).editingCell(cell)
                            .wired("t", Map(List("Name") -> draft), err, _ => ())
                    ).take(1).run
                yield out.mkString
            plain <- renderHtml(
                uic.DataTable[Item]().rows(items).rowKey(_.id)
                    .columns(uic.column("Name")(_.name), uic.column("Note")(_.note))
            )
            optedOut <-
                for
                    draft <- Signal.initRef("d")
                    err   <- Signal.initRef(Absent: Maybe[(uic.CellPath, kyo.uic.form.FieldError)])
                    cell  <- Signal.initRef(Absent: Maybe[uic.CellPath])
                    out <- UI.runRender(
                        navTable.cellNavigation(false).editingCell(cell)
                            .wired("t", Map(List("Name") -> draft), err, _ => ())
                    ).take(1).run
                yield out.mkString
        yield
            // Every cell is addressable, and the editable ones are the tab stops. The DOM
            // is row-major, so that IS "the next editable cell, wrapping into the row
            // below", with nothing prevented to make it so.
            assert(nav.contains("""id="t-c0-0"""") && nav.contains("""id="t-c1-1""""), "cells are addressed by position")
            assert(occurrences(nav, """tabindex="0"""") == 2, "one tab stop per editable cell")
            assert(occurrences(nav, """tabindex="-1"""") == 2, "and the rest are reachable only by the cursor")
            assert(nav.contains("p-uic-dt-nav") && nav.contains("data-kyo-scroll-keys"), "the arrows do not scroll the page")
            assert(occurrences(locked, """tabindex="0"""") == 2, "a locked column changes no tab stop")
            assert(!locked.contains("""id="t-c0-1""""), "and carries no cursor address at all")
            // A table that does not navigate renders what it always rendered.
            assert(!plain.contains("tabindex") && !plain.contains("p-uic-dt-nav"), "a plain table is untouched")
            assert(!optedOut.contains("p-uic-dt-nav"), "and so is one that opts out")
            assert(optedOut.contains("p-editable-column"), "which still edits, by mouse")
        end for
    }

    "Tabs renders Prime's compound anatomy and shows only the selected tab's content" in {
        for
            html <-
                for
                    ref <- Signal.initRef("t2")
                    ui = uic.Tabs()
                        .tab("First", "t1")(p("first-content"))
                        .tab("Second", "t2")(p("second-content"))
                        .selected(ref)
                    out <- UI.runRender(ui).take(1).run
                yield out.mkString
            counted <- renderHtml(uic.Tabs().tabs(uic.Tab("Inbox", UI.empty, "in", additionalText = Present("12"))))
        yield
            assert(html.contains("p-tabs"), "base class hook")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("p-tablist"), "tablist element")
            assert(html.contains("p-tablist-content"), "tablist content element")
            assert(html.contains("p-tablist-tab-list"), "tab-list element")
            assert(html.contains("""role="tablist""""), "tablist role")
            assert(html.contains("<button"), "tabs are real <button>s")
            assert(html.contains("""role="tab""""), "tab role on each tab")
            assert(html.contains("p-tab-active"), "active tab modifier class")
            assert(html.contains("p-tablist-active-bar"), "ink bar rendered in the active tab")
            assert(html.contains("p-tabpanels"), "tabpanels element")
            assert(html.contains("p-tabpanel"), "tabpanel element")
            assert(html.contains("""role="tabpanel""""), "tabpanel role on the content panel")
            assert(html.contains("""aria-selected="true""""), "active tab marked aria-selected")
            assert(html.contains("second-content"), "selected tab's content is shown")
            assert(!html.contains("first-content"), "unselected tab's content is not rendered")
            assert(counted.contains("p-uic-tab-count"), "additionalText renders the count badge")
            assert(counted.contains("12"), "count text rendered")
    }

    "Tree renders Prime's nested anatomy with aria-expanded reflecting the bound expansion set" in {
        for
            html <-
                for
                    exp <- Signal.initRef(Set("root"))
                    sel <- Signal.initRef(Set("a"))
                    ui = uic.Tree()
                        .selectionMode(uic.SelectionMode.Multiple)
                        .nodes(
                            uic.TreeNode(
                                "Root",
                                "root",
                                children = List(
                                    uic.TreeNode("Child A", "a"),
                                    uic.TreeNode("Child B", "b")
                                )
                            )
                        )
                        .expanded(exp)
                        .selected(sel)
                    out <- UI.runRender(ui).take(1).run
                yield out.mkString
            empty <- renderHtml(uic.Tree().emptyContent("No nodes"))
        yield
            assert(html.contains("p-tree"), "base class hook")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("p-tree-selectable"), "selectable modifier while a mode is set")
            assert(html.contains("p-tree-root-children"), "root children list class")
            assert(html.contains("""role="tree""""), "tree role on the root ul")
            assert(html.contains("p-tree-node"), "node class")
            assert(html.contains("""role="treeitem""""), "treeitem role on nodes")
            assert(html.contains("p-tree-node-content"), "node content element")
            assert(html.contains("p-tree-node-toggle-button"), "toggle button element")
            assert(html.contains("""data-uic-icon="chevron-down""""), "expanded parent shows the down chevron")
            assert(html.contains("""data-uic-icon="chevron-right""""), "collapsed/leaf rows show the right chevron")
            assert(html.contains("p-tree-node-label"), "label span class")
            assert(html.contains("p-tree-node-children"), "expanded parent nests the child list")
            assert(html.contains("""role="group""""), "expanded parent renders a nested group")
            assert(html.contains("""aria-expanded="true""""), "root's aria-expanded reflects the bound set")
            assert(html.contains("p-tree-node-leaf"), "leaves carry the leaf modifier")
            assert(html.contains("p-tree-node-selected"), "selected node content modifier")
            assert(html.contains("p-tree-node-checkbox"), "multi-select renders the Prime checkbox")
            assert(html.contains("p-checkbox-checked"), "selected node's checkbox is checked")
            assert(html.contains("Child A"), "children rendered while parent is expanded")
            assert(empty.contains("p-tree-empty-message"), "empty message row class")
            assert(empty.contains("No nodes"), "empty message text")
    }

    "Tree Checkbox mode cascades selection and renders the derived tri-state" in {
        val leafA = uic.TreeNode("Child A", "a")
        val leafB = uic.TreeNode("Child B", "b")
        val root  = uic.TreeNode("Root", "root", children = List(leafA, leafB))
        val tree  = uic.Tree().selectionMode(uic.SelectionMode.Checkbox).nodes(root)

        // Cascade: check the parent, then uncheck one leaf, then re-check it.
        val checkedAll = tree.cascadeToggle("root", Set.empty)
        val oneOff     = tree.cascadeToggle("a", checkedAll)
        val reAdded    = tree.cascadeToggle("a", oneOff)

        def html(sel: Set[String]): String < Async =
            for
                exp <- Signal.initRef(Set("root"))
                s   <- Signal.initRef(sel)
                out <- UI.runRender(tree.expanded(exp).selected(s)).take(1).run
            yield out.mkString
        for
            partialHtml <- html(oneOff)
            fullHtml    <- html(checkedAll)
        yield

            assert((checkedAll == Set("root", "a", "b")), s"checking a parent cascades to its subtree: $checkedAll")
            assert((oneOff == Set("b")), s"unchecking a leaf drops the parent, keeps the sibling: $oneOff")
            assert(tree.isPartiallyChecked(root, oneOff), "the partially-checked parent is indeterminate")
            assert(!tree.isFullyChecked(root, oneOff), "the partially-checked parent is not fully checked")
            assert((reAdded == Set("root", "a", "b")), s"re-checking the sibling re-adds the parent: $reAdded")
            assert(tree.isFullyChecked(root, reAdded), "the parent is fully checked once all leaves are")
            assert(partialHtml.contains("""aria-checked="mixed""""), "indeterminate parent advertises aria-checked=mixed")
            assert(partialHtml.contains("""data-uic-icon="minus""""), "indeterminate parent shows the minus glyph")
            assert(fullHtml.contains("p-checkbox-checked"), "fully-checked node's box carries the checked class")
            assert(fullHtml.contains("""aria-checked="true""""), "fully-checked node advertises aria-checked=true")
        end for
    }

    // ---- Divider / Avatar / Chip / MeterGroup / Skeleton / Fieldset / ScrollPanel ----

    "Skeleton renders Prime anatomy: base classes, aria-hidden, inline dimensions, shape/animation modifiers" in {
        for
            html   <- renderHtml(uic.Skeleton().width("10rem").height("4rem"))
            circle <- renderHtml(uic.Skeleton().shape(uic.SkeletonShape.Circle).size("4rem"))
            frozen <- renderHtml(uic.Skeleton().animation(false).borderRadius("16px"))
        yield
            assert(html.contains("p-skeleton"), "base class")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("""aria-hidden="true""""), "purely visual placeholder is aria-hidden")
            assert(html.contains("width: calc(10rem)"), "inline width from the CSS string")
            assert(html.contains("height: calc(4rem)"), "inline height from the CSS string")
            assert(!html.contains("p-skeleton-circle"), "rectangle default has no circle class")
            assert(circle.contains("p-skeleton-circle"), "circle shape modifier")
            assert(circle.contains("width: calc(4rem)"), "size sets the width")
            assert(circle.contains("height: calc(4rem)"), "size sets the height")
            assert(frozen.contains("p-skeleton-animation-none"), "animation(false) freezes the shimmer")
            assert(frozen.contains("border-radius: 16px"), "inline border radius")
            assert(!html.contains("p-skeleton-animation-none"), "animation defaults on")
    }

    "Divider renders Prime anatomy: layout/line/align classes, separator role, content slot" in {
        for
            plain    <- renderHtml(uic.Divider())
            centered <- renderHtml(uic.Divider().align(uic.DividerAlign.Center).lineStyle(uic.DividerLineStyle.Dashed)(span("OR")))
            vertical <- renderHtml(uic.Divider().layout(uic.DividerLayout.Vertical).lineStyle(uic.DividerLineStyle.Dotted))
        yield
            assert(plain.contains("p-divider"), "base class")
            assert(plain.contains("p-component"), "p-component class")
            assert(plain.contains("p-divider-horizontal"), "horizontal default")
            assert(plain.contains("p-divider-solid"), "solid default")
            assert(plain.contains("p-divider-left"), "unset horizontal align carries Prime's -left class quirk")
            assert(plain.contains("justify-content: center"), "unset align centers via Prime's inline style")
            assert(plain.contains("""role="separator""""), "separator role")
            assert(plain.contains("""aria-orientation="horizontal""""), "orientation exposed")
            assert(!plain.contains("p-divider-content"), "no content slot without children")
            assert(centered.contains("p-divider-center"), "center align class")
            assert(centered.contains("p-divider-dashed"), "dashed line class")
            assert(centered.contains("p-divider-content"), "children render in the content slot")
            assert(centered.contains("OR"), "content rendered")
            assert(vertical.contains("p-divider-vertical"), "vertical layout class")
            assert(vertical.contains("p-divider-dotted"), "dotted line class")
            assert(vertical.contains("""aria-orientation="vertical""""), "vertical orientation")
    }

    "Fieldset renders Prime anatomy; toggleable renders the toggle button bound to the collapse ref" in {
        for
            fixed <- renderHtml(uic.Fieldset().legend("Header")(p("content")))
            toggleable <-
                for
                    ref <- Signal.initRef(false)
                    out <- UI.runRender(uic.Fieldset().legend("Details").toggleable(true).collapsed(ref)(p("body"))).take(1).run
                yield out.mkString
            collapsed <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(uic.Fieldset().legend("Details").toggleable(true).collapsed(ref)(p("body"))).take(1).run
                yield out.mkString
        yield
            assert(fixed.contains("p-fieldset"), "base class")
            assert(fixed.contains("p-component"), "p-component class")
            assert(fixed.contains("""role="group""""), "group role on the div root (no fieldset factory)")
            assert(fixed.contains("p-fieldset-legend"), "legend element")
            assert(fixed.contains("p-fieldset-legend-label"), "legend label span")
            assert(fixed.contains("Header"), "legend text rendered")
            assert(fixed.contains("p-fieldset-content-container"), "content container")
            assert(fixed.contains("p-fieldset-content-wrapper"), "content wrapper")
            assert(fixed.contains("p-fieldset-content"), "content element")
            assert(!fixed.contains("p-fieldset-toggleable"), "fixed default has no toggleable class")
            assert(!fixed.contains("p-fieldset-toggle-button"), "fixed default has no toggle button")
            assert(toggleable.contains("p-fieldset-toggleable"), "toggleable modifier")
            assert(toggleable.contains("p-fieldset-toggle-button"), "toggle button")
            assert(toggleable.contains("p-fieldset-toggle-icon"), "toggle icon slot")
            assert(toggleable.contains("""data-uic-icon="minus""""), "expanded fieldset shows the minus glyph")
            assert(toggleable.contains("""aria-expanded="true""""), "expanded state exposed")
            assert(toggleable.contains("click"), "toggle registers the click")
            assert(collapsed.contains("""data-uic-icon="plus""""), "collapsed fieldset shows the plus glyph")
            assert(collapsed.contains("p-uic-collapsed"), "collapsed ref hides the content container via the collapse grid")
    }

    "Badge renders Prime anatomy: dot/circle automatics, severity + size suffixes; OverlayBadge wraps child + badge" in {
        for
            counter <- renderHtml(uic.Badge("22").severity(uic.Severity.Danger).size(uic.Size.Large))
            single  <- renderHtml(uic.Badge("4"))
            dot     <- renderHtml(uic.Badge().severity(uic.Severity.Success))
            overlay <- renderHtml(uic.OverlayBadge(uic.Avatar().initials("A"))(uic.Badge("2")))
        yield
            assert(counter.contains("p-badge"), "base class")
            assert(counter.contains("p-component"), "p-component class")
            assert(counter.contains("p-badge-danger"), "severity suffix class")
            assert(counter.contains("p-badge-lg"), "size suffix class")
            assert(counter.contains("22"), "value text rendered")
            assert(!counter.contains("p-badge-circle"), "multi-char value is the pill, not the circle")
            assert(single.contains("p-badge-circle"), "single-char value auto-circles (Prime semantics)")
            assert(!single.contains("p-badge-primary"), "unset severity keeps the unsuffixed base skin")
            assert(dot.contains("p-badge-dot"), "value-less badge renders the dot")
            assert(dot.contains("p-badge-success"), "dot keeps the severity suffix")
            assert(overlay.contains("p-overlaybadge"), "overlay wrapper class")
            assert(overlay.contains("p-avatar"), "wrapped child rendered")
            assert(overlay.contains("p-badge"), "overlaid badge rendered")
            assert(overlay.contains("p-badge-circle"), "overlaid single-char badge circles")
    }

    "Chip renders Prime anatomy: label/icon/image variants; removable renders the remove affordance" in {
        for
            iconChip  <- renderHtml(uic.Chip("Amy").icon(uic.Icons.user))
            imageChip <- renderHtml(uic.Chip("Amy").image("/amy.png"))
            removable <- renderHtml(uic.Chip("Xuxue").removable(true).onRemove(()))
        yield
            assert(iconChip.contains("p-chip"), "base class")
            assert(iconChip.contains("p-component"), "p-component class")
            assert(iconChip.contains("p-chip-icon"), "icon slot class")
            assert(iconChip.contains("<svg"), "icon rendered as SVG")
            assert(iconChip.contains("p-chip-label"), "label element")
            assert(iconChip.contains("Amy"), "label text rendered")
            assert(!iconChip.contains("p-chip-remove-icon"), "no remove affordance by default")
            assert(imageChip.contains("p-chip-image"), "image class")
            assert(imageChip.contains("<img"), "renders a real <img>")
            assert(imageChip.contains("""src="/amy.png""""), "image src rendered")
            assert(!imageChip.contains("p-chip-icon"), "image wins over icon (Prime precedence)")
            assert(removable.contains("p-chip-remove-icon"), "remove affordance class")
            assert(removable.contains("<button"), "remove affordance is a real <button> (a11y deviation)")
            assert(removable.contains("""data-uic-icon="times-circle""""), "Prime's times-circle remove glyph")
            assert(removable.contains("click"), "onRemove registers the click")
    }

    "AvatarGroup wraps avatars in the Prime group container" in {
        for
            html <- renderHtml(
                uic.AvatarGroup(
                    uic.Avatar().initials("A"),
                    uic.Avatar().initials("B")
                )(span.cssClass("extra-slot")("+2"))
            )
        yield
            assert(html.contains("p-avatar-group"), "group class")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("p-avatar"), "avatars rendered inside")
            assert(html.contains(">A<"), "first avatar rendered")
            assert(html.contains(">B<"), "second avatar rendered")
            assert(html.contains("extra-slot"), "extra children rendered after the avatars")
    }

    "MeterGroup renders Prime anatomy: meter role, scaled inline segments, label list with markers" in {
        for
            html <- renderHtml(
                uic.MeterGroup()
                    .meter("Apps", 16)
                    .meter("Messages", 8, "var(--p-cyan-500)")
                    .meter("Empty", 0)
                    .max(200)
            )
        yield
            assert(html.contains("p-metergroup"), "base class")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("p-metergroup-horizontal"), "horizontal orientation class")
            assert(html.contains("""role="meter""""), "meter role")
            assert(html.contains("""aria-valuemax="200""""), "max exposed")
            assert(html.contains("""aria-valuenow="12""""), "valuenow is the rounded total percent")
            assert(html.contains("p-metergroup-meters"), "meters container")
            assert(html.contains("p-metergroup-meter"), "meter segments")
            assert(html.contains("width: 8%"), "segment width scaled against max")
            assert(html.contains("var(--p-cyan-500)"), "explicit var() color applied")
            assert(html.contains("var(--p-primary-color)"), "default palette color applied")
            assert(html.contains("<ol"), "label list is a real <ol>")
            assert(html.contains("p-metergroup-label-list"), "label list class")
            assert(html.contains("p-metergroup-label-list-horizontal"), "horizontal label list class")
            assert(html.contains("p-metergroup-label"), "label rows")
            assert(html.contains("p-metergroup-label-marker"), "color markers")
            assert(html.contains("p-metergroup-label-text"), "label text span")
            assert(html.contains("Apps (8%)"), "label text carries the rounded percent")
            assert(html.contains("Empty (0%)"), "zero meter still lists its label")
    }

    "Inplace renders display XOR content from the bound ref; closable adds the close button" in {
        def inplace(ref: SignalRef[Boolean])(using Frame): UI =
            uic.Inplace().display(span("view")).content(span("edit")).active(ref).closable(true)
        for
            inactive <-
                for
                    ref <- Signal.initRef(false)
                    out <- UI.runRender(inplace(ref)).take(1).run
                yield out.mkString
            active <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(inplace(ref)).take(1).run
                yield out.mkString
        yield
            assert(inactive.contains("p-inplace"), "base class")
            assert(inactive.contains("p-component"), "p-component class")
            assert(inactive.contains("""aria-live="polite""""), "polite live region (Prime semantics)")
            assert(inactive.contains("p-inplace-display"), "inactive: display side rendered")
            assert(inactive.contains("""role="button""""), "inactive: display is an activatable button")
            assert(inactive.contains("""tabindex="0""""), "inactive: display focusable")
            assert(inactive.contains("view"), "inactive: display slot rendered")
            assert(inactive.contains("click"), "inactive: activation click registered")
            assert(!inactive.contains("p-inplace-content"), "inactive: content side absent (XOR)")
            assert(active.contains("p-inplace-content"), "active: content side rendered")
            assert(active.contains("edit"), "active: content slot rendered")
            assert(!active.contains("p-inplace-display"), "active: display side absent (XOR)")
            assert(active.contains("p-button-icon-only"), "active: closable renders the icon-only close button")
            assert(active.contains("""data-uic-icon="times""""), "active: times glyph on the close button")
        end for
    }

    "ScrollPanel renders Prime anatomy with inline viewport dimensions" in {
        for
            html <- renderHtml(uic.ScrollPanel(p("long content")).width("100%").height("12rem"))
        yield
            assert(html.contains("p-scrollpanel"), "base class")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("p-scrollpanel-content-container"), "content container")
            assert(html.contains("p-scrollpanel-content"), "content element")
            assert(html.contains("long content"), "content rendered")
            assert(html.contains("width: 100%"), "inline width")
            assert(html.contains("height: calc(12rem)"), "inline height")
    }

    // ---- Timeline / Rating / SelectButton / InputGroup / Stepper / ToggleButton / IconField ----

    "Rating renders Prime anatomy: options, active split from ref, cancel semantics markers" in {
        for
            html <-
                for
                    ref <- Signal.initRef(3)
                    out <- UI.runRender(uic.Rating().value(ref)).take(1).run
                yield out.mkString
            readonly <- renderHtml(uic.Rating().value(2).readonly(true))
            disabled <- renderHtml(uic.Rating().value(1).disabled(true))
            ten      <- renderHtml(uic.Rating().stars(10))
        yield
            assert(html.contains("p-rating"), "base class")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("p-hidden-accessible"), "hidden radio container (PrimeVue anatomy)")
            assert(html.contains("""type="radio""""), "hidden native radios")
            assert(html.contains("p-rating-option"), "option elements")
            assert(
                (html.sliding("p-rating-option-active".length).count(_ == "p-rating-option-active") == 3),
                "3 active options from the ref"
            )
            assert(html.contains("p-rating-on-icon"), "filled icon class on active options")
            assert(html.contains("p-rating-off-icon"), "outline icon class beyond the value")
            assert(html.contains("""data-uic-icon="star-fill""""), "filled star glyph")
            assert(html.contains("""data-uic-icon="star""""), "outline star glyph")
            assert(html.contains("click"), "options register the click")
            assert(readonly.contains("p-readonly"), "readonly modifier class")
            assert(!readonly.contains("click"), "readonly options stop reacting")
            assert(disabled.contains("p-disabled"), "disabled modifier class")
            assert(!disabled.contains("click"), "disabled options stop reacting")
            assert((ten.sliding("p-rating-option".length).count(_ == "p-rating-option") >= 10), "stars(10) renders 10 options")
    }

    "ToggleButton binds two-way: checked class + on/off label + icons + size" in {
        for
            checked <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(uic.ToggleButton().checked(ref).onLabel("On").offLabel("Off")).take(1).run
                yield out.mkString
            off      <- renderHtml(uic.ToggleButton())
            icons    <- renderHtml(uic.ToggleButton().checked(true).onIcon(uic.Icons.check).offIcon(uic.Icons.times))
            small    <- renderHtml(uic.ToggleButton().size(uic.Size.Small).invalid(true))
            disabled <- renderHtml(uic.ToggleButton().disabled(true))
        yield
            assert(checked.contains("p-togglebutton"), "base class")
            assert(checked.contains("p-component"), "p-component class")
            assert(checked.contains("p-togglebutton-checked"), "checked modifier from ref")
            assert(checked.contains("<button"), "renders a real <button>")
            assert(checked.contains("""aria-pressed="true""""), "aria-pressed reflects the value")
            assert(checked.contains("p-togglebutton-content"), "content span")
            assert(checked.contains("p-togglebutton-label"), "label span")
            assert(checked.contains(">On<"), "checked shows the onLabel")
            assert(checked.contains("click"), "registers the toggle click")
            assert(off.contains(">No<"), "unchecked default offLabel is Prime's \"No\"")
            assert(!off.contains("p-togglebutton-checked"), "unchecked has no checked class")
            assert(icons.contains("p-togglebutton-icon"), "icon slot class")
            assert(icons.contains("""data-uic-icon="check""""), "checked shows the onIcon")
            assert(!icons.contains("""data-uic-icon="times""""), "offIcon not rendered while checked")
            assert(small.contains("p-togglebutton-sm"), "small size class")
            assert(small.contains("p-inputfield-sm"), "small inputfield class (Prime pairs them)")
            assert(small.contains("p-invalid"), "invalid modifier class")
            assert(disabled.contains("disabled"), "disabled blocks the native button")
    }

    "SelectButton renders fused ToggleButtons with typed options; single + multiple from refs" in {
        for
            single <-
                for
                    ref <- Signal.initRef("b")
                    out <- UI.runRender(
                        uic.SelectButton[(String, String)]()
                            .options(Seq("a" -> "Apple", "b" -> "Banana"))(_._2)
                            .optionKey(_._1)
                            .value(ref)
                    ).take(1).run
                yield out.mkString
            multi <-
                for
                    ref <- Signal.initRef(Set("S", "L"))
                    out <- UI.runRender(
                        uic.SelectButton[String]().options(Seq("S", "M", "L")).multiple(true).value(ref)
                    ).take(1).run
                yield out.mkString
            invalid <- renderHtml(uic.SelectButton[String]().options(Seq("x")).invalid(true))
        yield
            assert(single.contains("p-selectbutton"), "base class")
            assert(single.contains("p-component"), "p-component class")
            // A choice of ONE is a radio group, and its options are radios: `aria-checked` is the
            // state that role has, and a toggle button's `aria-pressed` would be a second, different
            // story about the same control. Several independent choices stay a group of toggles.
            assert(single.contains("""role="radiogroup""""), "single: radio group role")
            assert(single.contains("""role="radio""""), "single: the options are radios")
            assert(single.contains("""aria-checked="true""""), "single: and report the checked state of one")
            assert(!single.contains("aria-pressed"), "single: without also reporting a pressed state")
            assert(multi.contains("""role="group""""), "multiple: a group of independent toggles")
            assert(multi.contains("""aria-pressed="true""""), "multiple: which report being pressed")
            assert(!multi.contains("""role="radio""""), "multiple: and are not radios")
            assert(single.contains("p-togglebutton"), "options render as ToggleButtons")
            assert(
                (single.sliding("p-togglebutton-checked".length).count(_ == "p-togglebutton-checked") == 1),
                "exactly the bound option is checked"
            )
            assert(single.contains("Apple"), "first option label")
            assert(single.contains("Banana"), "second option label")
            assert(
                (multi.sliding("p-togglebutton-checked".length).count(_ == "p-togglebutton-checked") == 2),
                "multiple: both bound options checked"
            )
            assert(invalid.contains("p-invalid"), "invalid class on the group")
    }

    "the seven newly-bindable controls render the invalid state and the message row" in {
        // Prime's `.p-invalid` skin plus the kyo message row: the same two marks every
        // other control already carried, now on the ones that had neither.
        def marks(html: String, what: String): Unit =
            assert(html.contains("p-invalid"), s"$what: invalid class")
            assert(html.contains("""aria-invalid="true""""), s"$what: aria-invalid")
            assert(html.contains("p-uic-invalid-message"), s"$what: message row")
            assert(html.contains("Required"), s"$what: the message text")
        end marks

        for
            toggle       <- renderHtml(uic.ToggleButton().invalid(true).invalidMessage("Required"))
            slider       <- renderHtml(uic.Slider().invalid(true).invalidMessage("Required"))
            knob         <- renderHtml(uic.Knob().invalid(true).invalidMessage("Required"))
            rating       <- renderHtml(uic.Rating().invalid(true).invalidMessage("Required"))
            colorPicker  <- renderHtml(uic.ColorPicker().inline(true).invalid(true).invalidMessage("Required"))
            fileUpload   <- renderHtml(uic.FileUpload().invalid(true).invalidMessage("Required"))
            selectButton <- renderHtml(uic.SelectButton[String]().options(Seq("A", "B")).invalid(true).invalidMessage("Required"))
            listbox      <- renderHtml(uic.Listbox().item("A", "a").invalid(true).invalidMessage("Required"))
            clean        <- renderHtml(uic.Slider())
            sliderId     <- renderHtml(uic.Slider().id("vol"))
            knobId       <- renderHtml(uic.Knob().id("gain"))
            toggleId     <- renderHtml(uic.ToggleButton().id("live"))
            fileId       <- renderHtml(uic.FileUpload().id("cv"))
        yield
            marks(toggle, "ToggleButton")
            marks(slider, "Slider")
            marks(knob, "Knob")
            marks(rating, "Rating")
            marks(colorPicker, "ColorPicker")
            marks(fileUpload, "FileUpload")
            marks(selectButton, "SelectButton")
            marks(listbox, "Listbox")
            // Valid controls stay clean: the marks are not unconditional decoration.
            assert(!clean.contains("p-invalid"), "a valid Slider carries no invalid class")
            assert(!clean.contains("p-uic-invalid-message"), "a valid Slider renders no message row")
            // id() reaches the focusable element, which is what focus-first-invalid needs.
            assert(sliderId.contains("""id="vol""""), "Slider id lands on the range input")
            assert(knobId.contains("""id="gain""""), "Knob id lands on the dial")
            assert(toggleId.contains("""id="live""""), "ToggleButton id lands on the button")
            assert(fileId.contains("""id="cv""""), "FileUpload id lands on the native input")
        end for
    }

    "integer() constrains Slider and Knob to whole numbers" in {
        for
            fractional <- renderHtml(uic.Slider().min(0).max(10).step(0.5).value(3.7))
            whole      <- renderHtml(uic.Slider().min(0).max(10).step(0.5).integer(true).value(3.7))
        yield
            assert(fractional.contains("3.7"), "without the constraint the fractional value renders as given")
            assert(!whole.contains("3.7"), "integer(true) rounds the rendered value")
            assert(whole.contains("4"), "3.7 rounds to 4")
            // The native step follows, so the browser cannot produce a fraction either.
            assert(fractional.contains("0.5"), "the declared step is used as-is")
            assert(!whole.contains("0.5"), "integer(true) lifts a fractional step to a whole one")
    }

    "FileUpload binds the picked files as its value, and the label follows that ref" in {
        val payload = UI.FilePayload("cv.pdf", 1024L, "application/pdf", "…")
        for
            bound <-
                for
                    ref <- Signal.initRef(Seq(payload))
                    out <- UI.runRender(uic.FileUpload().value(ref)).take(1).run
                yield out.mkString
            empty <-
                for
                    ref <- Signal.initRef(Seq.empty[UI.FilePayload])
                    out <- UI.runRender(uic.FileUpload().value(ref)).take(1).run
                yield out.mkString
        yield
            assert(bound.contains("cv.pdf"), "the bound files drive the chosen-file label")
            assert(bound.contains("p-fileupload-filename"), "a picked file gets Prime's filename class")
            assert(bound.contains("""data-kyo-ev"""), "the picker registers its select handler for the write-back")
            assert(empty.contains("No file chosen"), "an empty bound value shows the empty state")
            assert(!empty.contains("p-fileupload-filename"), "an empty bound value is not a filename")
        end for
    }

    "a colliding option key is reported loudly instead of silently picking the wrong option" in {

        // The tree-shaped picker defaults its key to the label the same way, so it reports
        // the same collision — from the node ids, which is where it lands there.
        final case class Dept(name: String, subs: List[Dept])
        def treeHtml(depts: Seq[Dept]): String < Async =
            for
                vref <- Signal.initRef(Set.empty[String])
                oref <- Signal.initRef(true)
                eref <- Signal.initRef(Set.empty[String])
                href <- Signal.initRef(-1)
                ts = uic.TreeSelect().options(depts)(_.name)(_.subs).value(vref)
                out <- UI.runRender(ts.open(oref).wired(oref, eref, href, "ts")).take(1).run
            yield out.mkString
        for
            // Two options, one label, no optionKey: the derived keys collide, so a pick
            // would apply to whichever matched first. That is the exact failure that only
            // shows up with the data that triggers it.
            dupSelect <-
                for
                    vref <- Signal.initRef("")
                    oref <- Signal.initRef(true)
                    href <- Signal.initRef(-1)
                    qref <- Signal.initRef("")
                    sel = uic.Select[(String, String)]().options(Seq("a" -> "Ada", "b" -> "Ada"))(_._2)
                    out <- UI.runRender(sel.value(vref).open(oref).wired(oref, href, qref)).take(1).run
                yield out.mkString
            keyedSelect <-
                for
                    vref <- Signal.initRef("")
                    oref <- Signal.initRef(true)
                    href <- Signal.initRef(-1)
                    qref <- Signal.initRef("")
                    sel = uic.Select[(String, String)]().options(Seq("a" -> "Ada", "b" -> "Ada"))(_._2).optionKey(_._1)
                    out <- UI.runRender(sel.value(vref).open(oref).wired(oref, href, qref)).take(1).run
                yield out.mkString

            dupMulti <-
                for
                    vref <- Signal.initRef(Set.empty[String])
                    oref <- Signal.initRef(true)
                    href <- Signal.initRef(-1)
                    qref <- Signal.initRef("")
                    ms = uic.MultiSelect[(String, String)]().options(Seq("a" -> "Ada", "b" -> "Ada"))(_._2)
                    out <- UI.runRender(ms.value(vref).open(oref).wired(oref, href, qref)).take(1).run
                yield out.mkString

            dupButtons <- renderHtml(uic.SelectButton[String]().options(Seq("S", "S", "L")))
            okButtons  <- renderHtml(uic.SelectButton[String]().options(Seq("S", "M", "L")))
            dupTree    <- treeHtml(Seq(Dept("Ops", List(Dept("Ops", Nil)))))
            okTree     <- treeHtml(Seq(Dept("Ops", List(Dept("Field", Nil)))))
        yield
            assert(dupSelect.contains("p-uic-key-error"), "Select: duplicate derived keys render the diagnostic card")
            assert(dupSelect.contains("optionKey"), "Select: the card names the setter to reach for")
            assert(dupSelect.contains("Ada"), "Select: the card names the offending key")
            assert(dupSelect.contains("""role="alert""""), "the card announces itself to assistive tech")
            assert(!keyedSelect.contains("p-uic-key-error"), "Select: a distinct optionKey clears the diagnostic")
            assert(dupMulti.contains("p-uic-key-error"), "MultiSelect: duplicate derived keys render the card")
            assert(dupButtons.contains("p-uic-key-error"), "SelectButton: duplicate derived keys render the card")
            assert(!okButtons.contains("p-uic-key-error"), "SelectButton: distinct options render no card")
            assert(dupTree.contains("p-uic-key-error"), "TreeSelect: duplicate node ids render the card")
            assert(!okTree.contains("p-uic-key-error"), "TreeSelect: distinct node ids render no card")
        end for
    }

    "a DataTable that binds identity without a rowKey says so instead of keying by position" in {
        final case class Row(id: String, name: String)
        val rows = Seq(Row("r1", "Ada"), Row("r2", "Bob"))
        def table(f: uic.DataTable[Row] => uic.DataTable[Row]): String < Async =
            for
                sel <- Signal.initRef(Set.empty[String])
                base = uic.DataTable[Row]().rows(rows).columns(uic.Column[Row]("Name")(_.name))
                out <- UI.runRender(f(base).selected(sel).selectionMode(uic.SelectionMode.Single)).take(1).run
            yield out.mkString
        for
            unkeyed <- table(identity)
            keyed   <- table(_.rowKey(_.id))
            // No identity bound at all: position is a fine key for a read-only table, so
            // nothing is reported.
            readOnly <- renderHtml(uic.DataTable[Row]().rows(rows).columns(uic.Column[Row]("Name")(_.name)))
        yield
            assert(unkeyed.contains("p-uic-key-error"), "selection bound without rowKey renders the diagnostic card")
            assert(unkeyed.contains("rowKey"), "the card names the setter to reach for")
            assert(unkeyed.contains("p-datatable-table-container"), "the table still renders alongside the card")
            assert(!keyed.contains("p-uic-key-error"), "a rowKey clears the diagnostic")
            assert(!readOnly.contains("p-uic-key-error"), "a table that consumes no identity is not nagged")
        end for
    }

    "InputGroup renders addons + fields in order; IconField pins InputIcons around the input" in {
        for
            group <- renderHtml(
                uic.InputGroup()(
                    uic.InputGroup.addon(span("www.")),
                    uic.Input().placeholder("Site"),
                    uic.InputGroup.addon(span(".com"))
                )
            )
            field    <- renderHtml(uic.IconField(uic.Input().placeholder("Search")).iconStart(uic.Icons.search))
            endField <- renderHtml(uic.IconField(uic.Input()).iconEnd(uic.Icons.spinner))
        yield
            assert(group.contains("p-inputgroup"), "group class")
            assert(group.contains("p-inputgroupaddon"), "addon class")
            assert(group.contains("www."), "leading addon content")
            assert(group.contains(".com"), "trailing addon content")
            assert(group.contains("p-inputtext"), "field child rendered")
            assert((group.indexOf("www.") < group.indexOf("p-inputtext")), "addon precedes the field")
            assert(field.contains("p-iconfield"), "iconfield class")
            assert(field.contains("p-inputicon"), "inputicon span")
            assert(field.contains("""data-uic-icon="search""""), "start icon glyph")
            assert((field.indexOf("p-inputicon") < field.indexOf("p-inputtext")), "start icon precedes the input (position-keyed CSS)")
            assert((endField.indexOf("p-inputicon") > endField.indexOf("p-inputtext")), "end icon follows the input")
    }

    "FloatLabel floats via CSS + p-filled from the bound value; IftaLabel renders the infield label" in {
        def wrapped(v: String): String < Async =
            for
                ref <- Signal.initRef(v)
                out <- UI.runRender(uic.FloatLabel(uic.Input().id("uname").value(ref), "Username").forId("uname")).take(1).run
            yield out.mkString
        for
            filled <- wrapped("Ada")
            empty  <- wrapped("")
            onVar  <- renderHtml(uic.FloatLabel(uic.Input(), "Email").variant(uic.FloatLabelVariant.On))
            ifta   <- renderHtml(uic.IftaLabel(uic.Input().value("x"), "Username"))

            // The other three FloatLabel hosts render through IftaLabel as well: Prime's
            // iftalabel sheet carries the textarea and .p-inputwrapper selectors for them.
            iftaTextArea <- renderHtml(uic.IftaLabel(uic.TextArea().value("note"), "Notes"))
            iftaSelect   <- renderHtml(uic.IftaLabel(uic.Select[String]().options(Seq("kg", "lb")), "Unit"))
            iftaAuto     <- renderHtml(uic.IftaLabel(uic.AutoComplete[String]().options(Seq("Berlin")), "City"))
        yield
            assert(filled.contains("p-floatlabel"), "root class")
            assert(filled.contains("p-floatlabel-over"), "over is the default variant")
            assert(filled.contains("<label"), "renders a real <label>")
            assert(filled.contains("""for="uname""""), "label for binding")
            assert(filled.contains("""id="uname""""), "input id rendered")
            assert(filled.contains("Username"), "label text")
            assert(filled.contains("p-filled"), "non-empty bound value stamps p-filled (CSS float hook)")
            assert(!empty.contains("p-filled"), "empty bound value leaves p-filled off")
            assert(onVar.contains("p-floatlabel-on"), "on variant class")
            assert(ifta.contains("p-iftalabel"), "iftalabel root class")
            assert(ifta.contains("p-filled"), "iftalabel stamps p-filled too")
            assert(iftaTextArea.contains("p-iftalabel"), "TextArea host wraps in the iftalabel root")
            assert(iftaTextArea.contains("<textarea"), "TextArea host renders its own field")
            assert(iftaTextArea.contains("Notes"), "TextArea host renders the label text")
            assert(iftaSelect.contains("p-iftalabel"), "Select host wraps in the iftalabel root")
            assert(iftaSelect.contains("p-inputwrapper"), "Select host keeps Prime's wrapper hook")
            assert(iftaAuto.contains("p-iftalabel"), "AutoComplete host wraps in the iftalabel root")
            assert(iftaAuto.contains("p-autocomplete"), "AutoComplete host renders its own field")
        end for
    }

    "Timeline renders opposite/separator/marker/connector/content per event; last event no connector" in {
        for
            html <- renderHtml(
                uic.Timeline[(String, String)]()
                    .events(Seq("Ordered" -> "15/10", "Shipped" -> "16/10", "Delivered" -> "17/10"))
                    .content(e => span(e._1))
                    .opposite(e => span(e._2))
                    .align(uic.TimelineAlign.Alternate)
            )
            horizontal <- renderHtml(
                uic.Timeline[String]().events(Seq(
                    "2024",
                    "2025"
                )).content(span(_)).layout(uic.TimelineLayout.Horizontal).align(uic.TimelineAlign.Top)
            )
        yield
            def count(s: String, sub: String) = s.sliding(sub.length).count(_ == sub)
            assert(html.contains("p-timeline"), "base class")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("p-timeline-vertical"), "vertical default layout class")
            assert(html.contains("p-timeline-alternate"), "alternate align class")
            assert((count(html, "p-timeline-event-separator") == 3), "one separator per event")
            assert((count(html, "p-timeline-event-marker") == 3), "one marker per event")
            assert((count(html, "p-timeline-event-connector") == 2), "last event renders NO connector")
            assert(html.contains("p-timeline-event-opposite"), "opposite slot")
            assert(html.contains("p-timeline-event-content"), "content slot")
            assert(html.contains("Ordered"), "content rendered")
            assert(html.contains("15/10"), "opposite rendered")
            assert(horizontal.contains("p-timeline-horizontal"), "horizontal layout class")
            assert(horizontal.contains("p-timeline-top"), "top align class")
    }

    "Paginator (standalone) renders Prime nav anatomy bound to the page ref" in {
        def paginator(p: Int): String < Async =
            for
                ref <- Signal.initRef(p)
                out <- UI.runRender(uic.Paginator().totalRecords(25).rows(10).page(ref)).take(1).run
            yield out.mkString
        for
            first <- paginator(0)
            last  <- paginator(2)
        yield
            assert(first.contains("p-paginator"), "base class")
            assert(first.contains("p-component"), "p-component class")
            assert(first.contains("""role="navigation""""), "navigation role")
            assert(first.contains("p-paginator-first"), "first button")
            assert(first.contains("p-paginator-prev"), "prev button")
            assert(first.contains("p-paginator-pages"), "pages container")
            assert(first.contains("p-paginator-page-selected"), "current page highlighted")
            assert(first.contains("p-paginator-next"), "next button")
            assert(first.contains("p-paginator-last"), "last button")
            assert(first.contains(">3<"), "25 records / 10 rows = 3 pages")
            assert(first.contains("p-disabled"), "backward nav disabled on page 1")
            assert((first.sliding("p-disabled".length).count(_ == "p-disabled") == 2), "exactly first+prev disabled on page 1")
            assert((last.sliding("p-disabled".length).count(_ == "p-disabled") == 2), "exactly next+last disabled on the last page")
            assert(last.contains("""aria-current="page""""), "aria-current on the selected page")
        end for
    }

    "a paginator template picks the elements and the order they render in" in {
        def rendered(f: uic.Paginator => uic.Paginator): String < Async =
            for
                ref <- Signal.initRef(1)
                out <- UI.runRender(f(uic.Paginator().totalRecords(25).rows(10).page(ref))).take(1).run
            yield out.mkString
        for
            plain <- rendered(identity)
            reversed <- rendered(
                _.template(
                    uic.PaginatorElement.CurrentPageReport,
                    uic.PaginatorElement.PageLinks,
                    uic.PaginatorElement.PrevPageLink,
                    uic.PaginatorElement.NextPageLink
                ).currentPageReport("{currentPage} of {totalPages}")
            )
            bare    <- rendered(_.template(uic.PaginatorElement.CurrentPageReport))
            dropped <- rendered(_.template(uic.PaginatorElement.PageLinks).jumpToPageInput(true))
            jump    <- rendered(_.template(uic.PaginatorElement.JumpToPageDropdown))
        yield
            assert(plain.contains("p-paginator-first") && plain.contains("p-paginator-last"))
            assert(!plain.contains("p-paginator-current"), "an element nothing configured stays out")
            // Named, so rendered, and in the order named rather than Prime's.
            assert(reversed.contains("p-paginator-current"))
            assert(reversed.indexOf("p-paginator-current") < reversed.indexOf("p-paginator-pages"))
            assert(reversed.indexOf("p-paginator-pages") < reversed.indexOf("p-paginator-prev"))
            assert(!reversed.contains("p-paginator-first"), "an element the template does not name does not render")
            assert(!reversed.contains("p-paginator-last"))
            // Named with nothing behind it, and the flag the template overrules.
            assert(bare.contains("p-uic-key-error") && bare.contains("currentPageReport"))
            assert(!dropped.contains("p-paginator-jtp-input"), "the template is the layout")
            assert(dropped.contains("p-uic-key-error") && dropped.contains("JumpToPageInput"))
            assert(
                jump.contains("p-paginator-jtp-dropdown") && jump.contains(">2<"),
                "a Select over the pages, showing the one the reader is on"
            )
        end for
    }

    "DataView renders layout class, header/content/footer, slices pages through the embedded Paginator" in {
        final case class P(name: String, price: Int)
        val items = List(P("Bamboo Watch", 65), P("Black Watch", 72), P("Blue Band", 79))
        def view(page: Int): String < Async =
            for
                ref <- Signal.initRef(page)
                out <- UI.runRender(
                    uic.DataView[P]()
                        .items(items)
                        .itemTemplate(p => div(span(p.name), span(s"$$${p.price}")))
                        .header(span("Products"))
                        .footer(span("3 total"))
                        .paginate(2)(ref)
                ).take(1).run
            yield out.mkString
        for
            page1 <- view(0)
            page2 <- view(1)
            grid  <- renderHtml(uic.DataView[P]().items(items).gridItemTemplate(p => div(span(p.name))).layout(uic.DataViewLayout.Grid))
            empty <- renderHtml(uic.DataView[String]().emptyContent("Nothing here"))
        yield
            assert(page1.contains("p-dataview"), "base class")
            assert(page1.contains("p-component"), "p-component class")
            assert(page1.contains("p-dataview-list"), "list layout class (default)")
            assert(page1.contains("p-dataview-header"), "header slot")
            assert(page1.contains("p-dataview-content"), "content element")
            assert(page1.contains("p-dataview-footer"), "footer slot")
            assert(page1.contains("p-paginator"), "embedded paginator")
            assert(page1.contains("p-dataview-paginator-bottom"), "dataview paginator position class")
            assert(page1.contains("Bamboo Watch"), "page 1 item rendered")
            assert(!page1.contains("Blue Band"), "page 2 item not on page 1")
            assert(page2.contains("Blue Band"), "page 2 shows the next slice")
            assert(!page2.contains("Bamboo Watch"), "page 2 hides the first slice")
            assert(grid.contains("p-dataview-grid"), "grid layout class")
            assert(empty.contains("p-dataview-empty-message"), "empty message element")
            assert(empty.contains("Nothing here"), "empty message text")
        end for
    }

    "Stepper renders steplist + active panel; linear disables forward headers" in {
        def stepper(active: Int, linear: Boolean): String < Async =
            for
                ref <- Signal.initRef(active)
                out <- UI.runRender(
                    uic.Stepper()
                        .active(ref)
                        .linear(linear)
                        .step("Personal")(p("personal-content"))
                        .step("Payment")(p("payment-content"))
                        .step("Review")(p("review-content"))
                ).take(1).run
            yield out.mkString
        for
            first  <- stepper(0, false)
            second <- stepper(1, false)
            linear <- stepper(0, true)
        yield
            def count(s: String, sub: String) = s.sliding(sub.length).count(_ == sub)
            assert(first.contains("p-stepper"), "base class")
            assert(first.contains("p-component"), "p-component class")
            assert(first.contains("p-steplist"), "step list element")
            assert((count(first, """class="p-step """) + count(first, """class="p-step"""") >= 3), "three steps")
            assert(first.contains("p-step-active"), "active step modifier")
            assert(first.contains("p-step-header"), "header button")
            assert(first.contains("""role="tab""""), "tab role on headers")
            assert(first.contains("p-step-number"), "number span")
            assert(first.contains("p-step-title"), "title span")
            assert(first.contains("Personal"), "title text")
            assert((count(first, "p-stepper-separator") == 2), "separator between steps, none after the last")
            assert(first.contains("p-steppanels"), "panels container")
            assert(first.contains("p-steppanel"), "panel element")
            assert(first.contains("personal-content"), "active step's content rendered")
            assert(!first.contains("payment-content"), "inactive step's content not rendered")
            assert(second.contains("payment-content"), "ref switches the panel")
            assert(second.contains("""aria-current="step""""), "active step exposes aria-current")
            assert(linear.contains("p-stepper-readonly"), "linear renders the readonly modifier")
            assert(linear.contains("disabled"), "linear disables forward headers")
            assert((count(linear, "p-disabled") == 2), "both forward steps blocked while step 1 is active")
        end for
    }

    // ---- deepened DatePicker / Paginator / Stepper / MeterGroup ----

    "DatePicker drills into Prime's month/year views through the currentView ref" in {
        def picker(view: uic.DatePickerView): String < Async =
            for
                vref  <- Signal.initRef("2026-07-15")
                mref  <- Signal.initRef("2026-07")
                cvref <- Signal.initRef(view)
                oref  <- Signal.initRef(true)
                out   <- dpHtml(uic.DatePicker().value(vref).month(mref).currentView(cvref), oref, Present(mref))
            yield out.mkString
        for
            date   <- picker(uic.DatePickerView.Date)
            monthV <- picker(uic.DatePickerView.Month)
            yearV  <- picker(uic.DatePickerView.Year)
            noRefs <-
                for
                    oref <- Signal.initRef(true)
                    out  <- dpHtml(uic.DatePicker().referenceDate("2026-07-01"), oref)
                yield out.mkString
        yield
            assert(
                date.contains(
                    """class="p-datepicker-select-month" aria-label="Choose Month" data-kyo-prop-type="button" data-kyo-ev="click""""
                ),
                "date view: month title is a clickable BUTTON"
            )
            assert(
                date.contains(
                    """class="p-datepicker-select-year" aria-label="Choose Year" data-kyo-prop-type="button" data-kyo-ev="click""""
                ),
                "date view: year title is a clickable BUTTON"
            )
            assert(date.contains("p-datepicker-day-view"), "date view: day grid")
            assert(!date.contains("p-datepicker-month-view"), "date view: no month grid")
            assert(monthV.contains("p-datepicker-month-view"), "month view: month grid container")
            assert(monthV.contains("p-datepicker-month-selected"), "month view: bound value's month selected")
            assert(monthV.contains("Jul"), "month view: short month names")
            assert(!monthV.contains("p-datepicker-day-view"), "month view: no day grid")
            assert(!monthV.contains("p-datepicker-select-month"), "month view: title shows only the year button")
            assert(yearV.contains("p-datepicker-year-view"), "year view: year grid container")
            assert(yearV.contains("p-datepicker-year-selected"), "year view: bound value's year selected")
            assert(yearV.contains("p-datepicker-decade"), "year view: decade title span")
            assert(yearV.contains("2020 - 2029"), "year view: decade range text")
            assert(!yearV.contains("p-datepicker-month-view"), "year view: no month grid")
            assert(
                noRefs.contains("""aria-label="Choose Month" data-kyo-prop-type="button" data-kyo-ev="click""""),
                "no currentView ref: the mount mints one, so the title button is live rather than disabled"
            )
        end for
    }

    "DatePicker view(Month|Year) picks ISO-prefix values from the granularity grids" in {
        for
            monthPicker <-
                for
                    vref <- Signal.initRef("2026-07")
                    oref <- Signal.initRef(true)
                    out  <- dpHtml(uic.DatePicker().value(vref).view(uic.DatePickerView.Month), oref)
                yield out.mkString
            yearPicker <-
                for
                    vref <- Signal.initRef("2026")
                    oref <- Signal.initRef(true)
                    out  <- dpHtml(uic.DatePicker().value(vref).view(uic.DatePickerView.Year), oref)
                yield out.mkString
        yield
            assert(monthPicker.contains("p-datepicker-month-view"), "view(Month): starts in the month grid")
            assert(monthPicker.contains("p-datepicker-month-selected"), "view(Month): YYYY-MM value selects its month")
            assert(monthPicker.contains("click"), "view(Month): month cells are clickable without drill-down refs")
            assert(!monthPicker.contains("p-datepicker-day-view"), "view(Month): no day grid")
            assert(yearPicker.contains("p-datepicker-year-view"), "view(Year): starts in the year grid")
            assert(yearPicker.contains("p-datepicker-year-selected"), "view(Year): YYYY value selects its year")
            assert(yearPicker.contains("2026"), "view(Year): year cell text")
    }

    "DatePicker multiple + range selection render Prime's day state classes" in {
        def count(s: String, sub: String) = s.sliding(sub.length).count(_ == sub)
        // The day the keyboard is on carries `p-focus` after its state class, so a count that
        // anchored on the closing quote stopped seeing whichever selected day the cursor sits on.
        def selectedDays(s: String) =
            count(s, "p-datepicker-day-selected") - count(s, "p-datepicker-day-selected-range")
        for
            multi <-
                for
                    sel  <- Signal.initRef(Set("2026-07-03", "2026-07-10"))
                    oref <- Signal.initRef(true)
                    out  <- dpHtml(uic.DatePicker().values(sel), oref)
                yield out.mkString
            range <-
                for
                    s    <- Signal.initRef("2026-07-06")
                    e    <- Signal.initRef("2026-07-09")
                    oref <- Signal.initRef(true)
                    out  <- dpHtml(uic.DatePicker().range(s, e), oref)
                yield out.mkString
            rangeOpen <-
                for
                    s    <- Signal.initRef("2026-07-06")
                    e    <- Signal.initRef("")
                    oref <- Signal.initRef(true)
                    out  <- dpHtml(uic.DatePicker().range(s, e), oref)
                yield out.mkString
        yield
            assert(selectedDays(multi) == 2, "multiple: both set members selected")
            assert(multi.contains("2026-07-03, 2026-07-10"), "multiple: field shows the joined display text")
            assert(selectedDays(range) == 2, "range: both endpoints selected")
            assert((count(range, "p-datepicker-day-selected-range") == 2), "range: exactly the two in-between days carry the range class")
            assert(range.contains("2026-07-06 - 2026-07-09"), "range: field shows the start - end display text")
            assert(selectedDays(rangeOpen) == 1, "open range: only the start selected")
            assert((count(rangeOpen, "p-datepicker-day-selected-range") == 0), "open range: no in-range days yet")
        end for
    }

    "DatePicker time picker renders Prime anatomy; timeOnly drops the calendar" in {
        for
            dateTime <-
                for
                    vref <- Signal.initRef("2026-07-16T14:30")
                    oref <- Signal.initRef(true)
                    out  <- dpHtml(uic.DatePicker().value(vref).showTime(true), oref)
                yield out.mkString
            twelve <-
                for
                    vref <- Signal.initRef("2026-07-16T14:30")
                    oref <- Signal.initRef(true)
                    out  <- dpHtml(uic.DatePicker().value(vref).showTime(true).hourFormat(uic.HourFormat.H12), oref)
                yield out.mkString
            clock <-
                for
                    vref <- Signal.initRef("09:15")
                    oref <- Signal.initRef(true)
                    out  <- dpHtml(uic.DatePicker().value(vref).timeOnly(true), oref)
                yield out.mkString
            empty <-
                for
                    vref <- Signal.initRef("")
                    oref <- Signal.initRef(true)
                    out  <- dpHtml(uic.DatePicker().value(vref).showTime(true).referenceDate("2026-07-01"), oref)
                yield out.mkString
        yield
            assert(dateTime.contains("p-datepicker-time-picker"), "time picker container")
            assert(dateTime.contains("p-datepicker-hour-picker"), "hour column")
            assert(dateTime.contains("p-datepicker-minute-picker"), "minute column")
            assert(dateTime.contains("p-datepicker-increment-button"), "increment buttons")
            assert(dateTime.contains("p-datepicker-decrement-button"), "decrement buttons")
            assert(dateTime.contains("""data-uic-icon="chevron-up""""), "chevron-up glyph")
            assert(dateTime.contains(">14<"), "24h hour display")
            assert(dateTime.contains(">30<"), "minute display")
            assert(dateTime.contains("p-datepicker-day-view"), "showTime keeps the day grid")
            assert(!dateTime.contains("p-datepicker-ampm-picker"), "24h: no AM/PM column")
            assert(!dateTime.contains("p-datepicker-timeonly"), "showTime alone is not timeonly")
            assert(twelve.contains("p-datepicker-ampm-picker"), "12h: AM/PM column")
            assert(twelve.contains(">02<"), "12h: 14 renders as 02")
            assert(twelve.contains(">PM<"), "12h: PM label")
            assert(clock.contains("p-datepicker-timeonly"), "timeOnly: panel modifier class")
            assert(clock.contains(">09<"), "timeOnly: hour from the HH:MM value")
            assert(!clock.contains("p-datepicker-calendar-container"), "timeOnly: no calendar")
            assert(
                empty.contains("""aria-label="Next Hour" data-kyo-prop-type="button" type="submit" disabled"""),
                "empty value + showTime: spin buttons disabled (no clock to default to)"
            )
    }

    "DatePicker button bar renders Clear always, Today only with an explicit today(iso)" in {
        def picker(withToday: Boolean): String < Async =
            for
                vref <- Signal.initRef("2026-07-15")
                oref <- Signal.initRef(true)
                base = uic.DatePicker().value(vref).showButtonBar(true).open(oref)
                out <- dpHtml(if withToday then base.today("2026-07-16") else base, oref)
            yield out
        for
            withToday <- picker(true)
            without   <- picker(false)
        yield
            assert(withToday.contains("p-datepicker-buttonbar"), "button bar container")
            assert(withToday.contains("p-datepicker-today-button"), "today button with today(iso)")
            assert(withToday.contains(">Today<"), "today label")
            assert(withToday.contains("p-datepicker-clear-button"), "clear button")
            assert(withToday.contains(">Clear<"), "clear label")
            assert(withToday.contains("p-button-sm"), "Prime's small text-button skin")
            assert(without.contains("p-datepicker-buttonbar"), "bar renders without today too")
            assert(!without.contains("p-datepicker-today-button"), "no Today button without an explicit today(iso) — the render is pure")
        end for
    }

    "Paginator windows page links to pageLinkSize with PrimeVue's boundary math" in {
        def paginator(p: Int, linkSize: Int): String < Async =
            for
                ref <- Signal.initRef(p)
                out <- UI.runRender(uic.Paginator().totalRecords(120).rows(10).pageLinkSize(linkSize).page(ref)).take(1).run
            yield out.mkString
        for
            first <- paginator(0, 5)
            mid   <- paginator(5, 5)
            last  <- paginator(11, 5)
            all   <- paginator(0, 12)
        yield
            def count(s: String, sub: String) = s.sliding(sub.length).count(_ == sub)
            assert(
                (count(first, "p-paginator-page\"") + count(first, "p-paginator-page p-paginator-page-selected") == 5),
                "first page: exactly 5 links"
            )
            assert(first.contains(">5<"), "first window ends at 5")
            assert(!first.contains(">6<"), "first window excludes 6")
            assert(mid.contains(">4<"), "mid window starts at 4")
            assert(mid.contains(">8<"), "mid window ends at 8")
            assert(!mid.contains(">3<"), "mid window excludes 3")
            assert(!mid.contains(">9<"), "mid window excludes 9")
            assert(last.contains(">8<"), "last window pulled back to 8")
            assert(last.contains(">12<"), "last window ends at 12")
            assert(!last.contains(">7<"), "last window excludes 7")
            assert(all.contains(">1<") && all.contains(">12<"), "a covering pageLinkSize renders all pages")
        end for
    }

    "Paginator renders the rpp dropdown, current-page report, and jump-to-page slots" in {
        for
            html <-
                for
                    page <- Signal.initRef(1)
                    rows <- Signal.initRef(10)
                    out <- UI.runRender(
                        uic.Paginator()
                            .totalRecords(25)
                            .rows(rows)
                            .page(page)
                            .rowsPerPageOptions(Seq(5, 10, 20))
                            .currentPageReport("Showing {first} to {last} of {totalRecords} ({currentPage}/{totalPages}, {rows} rows)")
                            .jumpToPageInput(true)
                    ).take(1).run
                yield out.mkString
            unbound <- renderHtml(
                uic.Paginator().totalRecords(25).rows(10).rowsPerPageOptions(Seq(5, 10))
            )
        yield
            assert(html.contains("p-paginator-rpp-dropdown"), "rpp dropdown class on the Select root")
            assert(html.contains("p-select"), "rpp renders our Select anatomy")
            assert(html.contains("""p-select-label">10<"""), "current rows value shows on the closed trigger")
            assert(html.contains("p-paginator-current"), "current-page report span")
            assert(html.contains("Showing 11 to 20 of 25 (2/3, 10 rows)"), "report placeholders substituted")
            assert(html.contains("p-paginator-jtp-input"), "jump-to-page wrapper class")
            assert(html.contains("p-inputtext"), "jtp wraps our Input")
            assert(html.contains("""value="2""""), "jtp shows the 1-based current page")
            assert(
                (html.indexOf("p-paginator-rpp-dropdown") < html.indexOf("p-paginator-current")),
                "Prime's default order: rpp before report"
            )
            assert(
                (html.indexOf("p-paginator-current") < html.indexOf("p-paginator-jtp-input")),
                "Prime's default order: report before jtp"
            )
            assert(unbound.contains("p-paginator-rpp-dropdown"), "rpp renders without a rows ref too")
            assert(unbound.contains("p-disabled"), "rpp without a rows(SignalRef) binding renders disabled")
    }

    "Stepper renders the vertical StepItem composition with the inline active panel" in {
        def stepper(active: Int): String < Async =
            for
                ref <- Signal.initRef(active)
                out <- UI.runRender(
                    uic.Stepper()
                        .vertical(true)
                        .active(ref)
                        .step("Personal")(p("personal-content"))
                        .step("Payment")(p("payment-content"))
                        .step("Review")(p("review-content"))
                ).take(1).run
            yield out.mkString
        for
            first <- stepper(0)
            last  <- stepper(2)
        yield
            def count(s: String, sub: String) = s.sliding(sub.length).count(_ == sub)
            assert((count(first, "p-stepitem") >= 3), "one stepitem wrapper per step")
            assert(first.contains("p-stepitem-active"), "active stepitem modifier")
            assert(!first.contains("p-steplist"), "vertical: no steplist row")
            assert(!first.contains("p-steppanels"), "vertical: no separate panels container")
            assert(first.contains("p-steppanel-active"), "active panel modifier (vertical-only in Prime)")
            assert(first.contains("p-steppanel-content-wrapper"), "panel content wrapper")
            assert(first.contains("p-steppanel-content"), "panel content element")
            assert(first.contains("personal-content"), "active step's content inline under its header")
            assert(!first.contains("payment-content"), "inactive steps render no panel")
            assert((count(first, "p-stepper-separator") == 1), "the active (non-last) panel carries the vertical separator")
            assert((count(last, "p-stepper-separator") == 0), "the last step's panel has no separator")
            assert(last.contains("review-content"), "ref switches the inline panel")
        end for
    }

    "Stepper value-keyed steps bind the active VALUE; per-step disabled blocks its header" in {
        for
            html <-
                for
                    ref <- Signal.initRef("payment")
                    out <- UI.runRender(
                        uic.Stepper()
                            .value(ref)
                            .step("Personal", value = Present("personal"))(p("personal-content"))
                            .step("Payment", value = Present("payment"))(p("payment-content"))
                            .step("Review", value = Present("review"))(p("review-content"))
                    ).take(1).run
                yield out.mkString
            hardDisabled <-
                for
                    ref <- Signal.initRef(0)
                    out <- UI.runRender(
                        uic.Stepper()
                            .active(ref)
                            .step("One")(p("c1"))
                            .step("Two", disabled = true)(p("c2"))
                            .step("Three")(p("c3"))
                    ).take(1).run
                yield out.mkString
        yield
            def count(s: String, sub: String) = s.sliding(sub.length).count(_ == sub)
            assert(html.contains("payment-content"), "value ref selects the matching step's panel")
            assert(!html.contains("personal-content"), "other panels not rendered")
            assert(html.contains("""aria-current="step""""), "active step exposes aria-current")
            assert((count(hardDisabled, "p-disabled") == 1), "exactly the hard-disabled step is blocked outside linear mode")
            assert(hardDisabled.contains("disabled"), "disabled step's header button is a native disabled button")
    }

    "MeterGroup renders vertical orientation, label position/orientation, icons, and templates" in {
        for
            vertical <- renderHtml(
                uic.MeterGroup()
                    .orientation(uic.Orientation.Vertical)
                    .labelOrientation(uic.Orientation.Vertical)
                    .meter("Apps", 30)
            )
            startLabels <- renderHtml(
                uic.MeterGroup().labelPosition(uic.LabelPosition.Start).meter("Apps", 30)
            )
            icons <- renderHtml(
                uic.MeterGroup().meter("Apps", 16, "var(--p-cyan-500)", uic.Icons.check)
            )
            templated <- renderHtml(
                uic.MeterGroup()
                    .meter("Storage", 40)
                    .startTemplate(span.cssClass("custom-start")("used"))
                    .endTemplate(span.cssClass("custom-end")("of 100 GB"))
                    .labelTemplate((m, pc) => span.cssClass("custom-label")(s"${m.labelText}: ${math.round(pc)}%"))
            )
            meterTpl <- renderHtml(
                uic.MeterGroup().meter("Zero", 0).meterTemplate((m, pc) => span.cssClass("custom-meter")(m.labelText))
            )
        yield
            assert(vertical.contains("p-metergroup-vertical"), "vertical orientation class")
            assert(!vertical.contains("p-metergroup-horizontal"), "vertical drops the horizontal class")
            assert(vertical.contains("height: 30%"), "vertical segments size by height")
            assert(vertical.contains("p-metergroup-label-list-vertical"), "vertical label list class")
            assert(startLabels.contains("p-metergroup-label-list-horizontal"), "labelOrientation defaults horizontal")
            assert(
                (startLabels.indexOf("p-metergroup-label-list") < startLabels.indexOf("p-metergroup-meters")),
                "labelPosition(Start) puts the labels before the meters"
            )
            assert(icons.contains("p-metergroup-label-icon"), "per-meter icon slot class")
            assert(icons.contains("""data-uic-icon="check""""), "icon glyph rendered")
            assert(icons.contains("color: var(--p-cyan-500)"), "icon tinted with the meter color")
            assert(!icons.contains("p-metergroup-label-marker"), "icon replaces the color marker (Prime semantics)")
            assert(templated.contains("custom-start"), "start template before the meters")
            assert(templated.contains("custom-end"), "end template after the meters")
            assert((templated.indexOf("custom-start") < templated.indexOf("p-metergroup-meters")), "start template order")
            assert((templated.indexOf("p-metergroup-meters") < templated.indexOf("custom-end")), "end template order")
            assert(templated.contains("custom-label"), "label template replaces the row content")
            assert(templated.contains("Storage: 40%"), "label template receives meter + percent")
            assert(!templated.contains("p-metergroup-label-marker"), "label template suppresses the default marker")
            assert(meterTpl.contains("custom-meter"), "meter template replaces the segment")
            assert(meterTpl.contains(">Zero<"), "meter template renders for 0% meters too (skipping is its call)")
    }

    "Theme carries the kyo remainder for the deepened DatePicker / Stepper / MeterGroup" in {
        assert(
            uic.Theme.primeExtraCss.contains(".p-datepicker-buttonbar, .p-datepicker-time-picker,"),
            "datepicker buttonbar/time-picker row restorers (remainder)"
        )
        assert(
            uic.Theme.primeExtraCss.contains(".p-datepicker-month-view, .p-datepicker-year-view { display: block; }"),
            "month/year grid block containers (remainder)"
        )
        assert(
            uic.Theme.primeExtraCss.contains("button.p-datepicker-select-month, button.p-datepicker-select-year { font: inherit; }"),
            "title button font inherit (remainder)"
        )
        assert(uic.Theme.primeExtraCss.contains(".p-steppanel-content { display: block; }"), "vertical steppanel content block (remainder)")
        assert(
            uic.Theme.primeExtraCss.contains("ol.p-metergroup-label-list-vertical { flex-direction: column; }"),
            "vertical metergroup label list (remainder)"
        )
    }

    "Theme carries the extracted CSS + kyo remainder for Timeline / Rating / SelectButton / Stepper" in {
        val prime = uic.Theme.primeCss
        assert(prime.contains(".p-rating"), "extracted rating CSS present")
        assert(prime.contains(".p-togglebutton"), "extracted togglebutton CSS present")
        assert(prime.contains(".p-selectbutton"), "extracted selectbutton CSS present")
        assert(prime.contains(".p-inputgroup"), "extracted inputgroup CSS present")
        assert(prime.contains(".p-iconfield"), "extracted iconfield CSS present")
        assert(prime.contains(".p-floatlabel"), "extracted floatlabel CSS present")
        assert(prime.contains(".p-iftalabel"), "extracted iftalabel CSS present")
        assert(prime.contains(".p-timeline"), "extracted timeline CSS present")
        assert(prime.contains(".p-dataview"), "extracted dataview CSS present")
        assert(prime.contains(".p-steplist"), "extracted stepper CSS present")
        assert(
            uic.Theme.primeExtraCss.contains(".p-timeline-event, .p-steplist, .p-step { flex-direction: row; }"),
            "timeline/stepper row restorers (remainder)"
        )
        assert(
            uic.Theme.primeExtraCss.contains("span.p-rating-icon, span.p-togglebutton-icon"),
            "rating/togglebutton glyph spans (remainder)"
        )
        assert(uic.Theme.primeExtraCss.contains("span.p-inputicon"), "inputicon token box (remainder)")
        assert(uic.Theme.primeExtraCss.contains("button.p-step-header { font: inherit; }"), "step header font inherit (remainder)")
    }

    "Theme carries the extracted CSS + kyo remainder for Divider / Avatar / Chip / MeterGroup / Skeleton" in {
        val prime = uic.Theme.primeCss
        assert(prime.contains(".p-skeleton"), "extracted skeleton CSS present")
        assert(prime.contains(".p-divider-horizontal"), "extracted divider CSS present")
        assert(prime.contains(".p-fieldset"), "extracted fieldset CSS present")
        assert(prime.contains(".p-badge"), "extracted badge CSS present")
        assert(prime.contains(".p-overlaybadge"), "extracted overlaybadge CSS present")
        assert(prime.contains(".p-chip"), "extracted chip CSS present")
        assert(prime.contains(".p-avatar-group"), "avatar-group rules present (avatar sheet)")
        assert(prime.contains(".p-metergroup"), "extracted metergroup CSS present")
        assert(prime.contains(".p-inplace-display"), "extracted inplace CSS present")
        assert(prime.contains(".p-scrollpanel-content"), "extracted scrollpanel CSS present")
        assert(
            uic.Theme.primeExtraCss.contains(".p-metergroup-meters, ol.p-metergroup-label-list { flex-direction: row; }"),
            "metergroup row restorers (remainder)"
        )
        assert(
            uic.Theme.primeExtraCss.contains(".p-skeleton { position: relative; }"),
            "skeleton position (remainder — Prime inline style)"
        )
        assert(uic.Theme.primeExtraCss.contains(".p-overlaybadge { display: inline-flex"), "overlaybadge hug (remainder)")
        assert(uic.Theme.primeExtraCss.contains("button.p-chip-remove-icon"), "chip remove button chrome strip (remainder)")
        assert(uic.Theme.primeExtraCss.contains("button.p-fieldset-toggle-button"), "fieldset toggle font inherit (remainder)")
        assert(uic.Theme.primeExtraCss.contains("scrollbar-width: thin"), "scrollpanel native thin scrollbars (remainder)")
        assert(uic.Theme.primeExtraCss.contains("::-webkit-scrollbar-thumb"), "scrollpanel webkit thumb (remainder)")
    }

    // ---- PrimeReact/PrimeVue feature-gap closure ----

    "Rating renders hidden per-option radios (form participation) + custom on/off icons" in {
        def count(s: String, sub: String) = s.sliding(sub.length).count(_ == sub)
        for
            named    <- renderHtml(uic.Rating().value(2).name("score"))
            hearts   <- renderHtml(uic.Rating().value(1).stars(2).onIcon(uic.Icons.heartFill).offIcon(uic.Icons.heart))
            readonly <- renderHtml(uic.Rating().value(2).readonly(true))
            unnamed  <- renderHtml(uic.Rating().value(1))
            minted   <- renderHtml(uic.Rating().value(1).wired("kyo-uic-7"))
        yield
            assert((count(named, "p-hidden-accessible") == 5), "one hidden container per option")
            assert((count(named, """type="radio"""") == 5), "one native radio per option")
            assert((count(named, """name="score"""") == 5), "name(...) groups all radios")
            assert(named.contains("""value="2""""), "radios carry their star value")
            assert(named.contains("checked"), "the current value's radio is checked")
            assert(named.contains("""aria-label="1 star""""), "first star aria-label")
            assert(named.contains("""aria-label="2 stars""""), "plural star aria-label")
            // The name is what the browser groups the radios by, and the grouping is what makes
            // this ONE tab stop with arrows between the stars. A caller's own name wins; the
            // mount mints one otherwise, and only the static projection, which has no mount to
            // mint in, is left without.
            assert(!unnamed.contains("name="), "the static projection has no uniqueness source to mint from")
            assert((count(minted, """name="kyo-uic-7"""") == 5), "the mount's minted name groups them all")
            assert(named.contains("""role="radiogroup""""), "the box around the radios says what it is")
            assert(hearts.contains("""data-uic-icon="heart-fill""""), "onIcon overrides the filled glyph")
            assert(hearts.contains("""data-uic-icon="heart""""), "offIcon overrides the outline glyph")
            assert(!hearts.contains("""data-uic-icon="star"""), "no star glyphs once overridden")
            assert(hearts.contains("p-rating-on-icon"), "override keeps the on-icon class")
            assert(hearts.contains("p-rating-off-icon"), "override keeps the off-icon class")
            // readonly keeps the stars reachable: only `disabled` takes a control out of the tab
            // order. The client declines the browser's own toggle instead, and the GROUP carries
            // `aria-readonly`, since a radio has no readonly state of its own to report.
            assert(!readonly.contains(" disabled"), "readonly no longer disables the hidden radios")
            assert(readonly.contains("data-kyo-inert"), "the browser's toggle is declined instead")
            assert(tagWithClass(readonly, "p-rating").contains("""aria-readonly="true""""), "the GROUP reports it")
            assert(!readonly.contains("""type="radio" aria-readonly"""), "not the radios, which have no such state")
        end for
    }

    "ToggleButton fluid spans + readonly blocks toggling without the disabled look" in {
        for
            fluid <- renderHtml(uic.ToggleButton().checked(true).fluid(true))
            readonly <-
                for
                    ref <- Signal.initRef(false)
                    out <- UI.runRender(uic.ToggleButton().checked(ref).readonly(true)).take(1).run
                yield out.mkString
        yield
            assert(fluid.contains("p-togglebutton-fluid"), "fluid modifier class")
            assert(!readonly.contains("click"), "readonly registers no toggle click")
            assert(!readonly.contains(" disabled"), "readonly is not natively disabled (it keeps the normal look)")
            // `role="button"` has no readonly state to report, so the true thing it can say is
            // that it cannot be operated — while staying focusable, which native disabled is not.
            assert(readonly.contains("""aria-disabled="true""""), "readonly says it cannot be operated")
            assert(readonly.contains("p-togglebutton"), "readonly keeps the base anatomy")
    }

    "SelectButton itemTemplate renders arbitrary UI per option; label stays the aria name" in {
        for
            html <-
                for
                    ref <- Signal.initRef("kg")
                    out <- UI.runRender(
                        uic.SelectButton[(String, String)]()
                            .options(Seq("kg" -> "Kilogram", "lb" -> "Pound"))(_._2)
                            .optionKey(_._1)
                            .itemTemplate(o => span.cssClass("opt-tpl")(o._2.toUpperCase))
                            .value(ref)
                    ).take(1).run
                yield out.mkString
        yield
            assert(html.contains("opt-tpl"), "template UI rendered inside the buttons")
            assert(html.contains("KILOGRAM"), "template output rendered")
            assert(!html.contains("p-togglebutton-label"), "template replaces the default label span")
            assert(html.contains("p-togglebutton-content"), "template renders inside the content span")
            assert(html.contains("""aria-label="Kilogram""""), "label projection stays the accessible name")
            assert(html.contains("p-togglebutton-checked"), "selection still binds through the key")
    }

    "IconField hosts TextArea and Select via the stamped padding classes" in {
        for
            ta <- renderHtml(uic.IconField(uic.TextArea().placeholder("Notes")).iconStart(uic.Icons.search))
            sel <- renderHtml(
                uic.IconField(uic.Select[String]().options(Seq("A", "B"))).iconStart(uic.Icons.user).iconEnd(uic.Icons.chevronDown)
            )
        yield
            assert(ta.contains("p-iconfield"), "textarea host: iconfield root")
            assert(ta.contains("p-textarea"), "textarea host: field rendered")
            assert(ta.contains("p-uic-iconfield-start"), "textarea host: start padding class stamped")
            assert((ta.indexOf("p-inputicon") < ta.indexOf("p-textarea")), "textarea host: icon precedes the field")
            assert(sel.contains("p-iconfield"), "select host: iconfield root")
            assert(sel.contains("p-select"), "select host: field rendered")
            assert(sel.contains("p-uic-iconfield-start"), "select host: start class stamped on the root")
            assert(sel.contains("p-uic-iconfield-end"), "select host: end class stamped on the root")
            assert(sel.contains("""data-uic-icon="user""""), "select host: start glyph rendered")
    }

    "FloatLabel hosts TextArea/Select/AutoComplete: p-filled and p-inputwrapper-filled track the value" in {
        def taWrapped(v: String): String < Async =
            for
                ref <- Signal.initRef(v)
                out <- UI.runRender(uic.FloatLabel(uic.TextArea().id("msg").value(ref), "Message").forId("msg")).take(1).run
            yield out.mkString
        def selWrapped(v: String): String < Async =
            for
                ref <- Signal.initRef(v)
                out <- UI.runRender(
                    uic.FloatLabel(uic.Select[String]().options(Seq("kg", "lb")).id("unit").value(ref), "Unit").forId("unit")
                ).take(1).run
            yield out.mkString
        for
            taFilled  <- taWrapped("Hello")
            taEmpty   <- taWrapped("")
            selFilled <- selWrapped("kg")
            selEmpty  <- selWrapped("")
            acFilled <-
                for
                    ref <- Signal.initRef("Berlin")
                    out <- UI.runRender(
                        uic.FloatLabel(uic.AutoComplete[String]().options(Seq("Berlin")).id("city").value(ref), "City").forId("city")
                    ).take(1).run
                yield out.mkString
        yield
            assert(taFilled.contains("p-floatlabel"), "textarea host: floatlabel root")
            assert(taFilled.contains("p-filled"), "textarea host: non-empty value stamps p-filled")
            assert(!taEmpty.contains("p-filled"), "textarea host: empty value leaves p-filled off")
            assert(taFilled.contains("""for="msg""""), "textarea host: label for binding")
            assert(taFilled.contains("""id="msg""""), "textarea host: field id rendered")
            assert(selFilled.contains("p-inputwrapper"), "select host: Prime wrapper class")
            assert(selFilled.contains("p-inputwrapper-filled"), "select host: selected value stamps -filled")
            assert(!selEmpty.contains("p-inputwrapper-filled"), "select host: empty selection leaves -filled off")
            assert(selEmpty.contains("p-placeholder"), "select host: auto empty placeholder keeps the closed field blank")
            assert(selFilled.contains("""id="unit""""), "select host: native select id rendered")
            assert(acFilled.contains("p-autocomplete"), "autocomplete host: field rendered")
            assert(acFilled.contains("p-filled"), "autocomplete host: non-empty value stamps p-filled on the inner input")
            assert(acFilled.contains("""id="city""""), "autocomplete host: inner input id rendered")
        end for
    }

    "ToggleSwitch handleIcon renders the per-state glyph inside the handle" in {
        for
            on    <- renderHtml(uic.ToggleSwitch().checked(true).handleIcon(uic.Icons.check, uic.Icons.times))
            off   <- renderHtml(uic.ToggleSwitch().checked(false).handleIcon(uic.Icons.check, uic.Icons.times))
            plain <- renderHtml(uic.ToggleSwitch().checked(true))
        yield
            assert(on.contains("p-toggleswitch-handle"), "handle element")
            assert(on.contains("p-uic-toggleswitch-handle-icon"), "handle icon sizing class")
            assert(on.contains("""data-uic-icon="check""""), "checked state renders the checked glyph")
            assert(!on.contains("""data-uic-icon="times""""), "unchecked glyph absent while on")
            assert(off.contains("""data-uic-icon="times""""), "unchecked state renders the unchecked glyph")
            assert((on.indexOf("p-toggleswitch-handle") < on.indexOf("data-uic-icon")), "glyph renders inside the handle")
            assert(!plain.contains("data-uic-icon"), "no glyph without handleIcon (Prime's empty handle)")
    }

    "Chip removeIcon overrides the remove glyph, keeping the button anatomy" in {
        for
            html <- renderHtml(uic.Chip("Tag").removable(true).removeIcon(uic.Icons.times).onRemove(()))
        yield
            assert(html.contains("p-chip-remove-icon"), "remove affordance class")
            assert(html.contains("<button"), "remove affordance stays a real <button>")
            assert(html.contains("""data-uic-icon="times""""), "custom glyph rendered")
            assert(!html.contains("""data-uic-icon="times-circle""""), "default glyph replaced")
    }

    "Inplace disabled renders an inert display; onOpen/onClose wire the interactions" in {
        def inplace(disabled: Boolean)(ref: SignalRef[Boolean])(using Frame): UI =
            uic.Inplace()
                .display(span("view"))
                .content(span("edit"))
                .active(ref)
                .closable(true)
                .disabled(disabled)
                .onOpen(())
                .onClose(())
        for
            disabledHtml <-
                for
                    ref <- Signal.initRef(false)
                    out <- UI.runRender(inplace(true)(ref)).take(1).run
                yield out.mkString
            openable <-
                for
                    ref <- Signal.initRef(false)
                    out <- UI.runRender(inplace(false)(ref)).take(1).run
                yield out.mkString
            closable <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(inplace(false)(ref)).take(1).run
                yield out.mkString
        yield
            assert(disabledHtml.contains("p-disabled"), "disabled: display carries p-disabled")
            assert(!disabledHtml.contains("click"), "disabled: no activation registered")
            assert(openable.contains("p-inplace-display"), "enabled: display side rendered")
            assert(openable.contains("click"), "enabled: activation (ref write + onOpen) registered")
            assert(closable.contains("p-inplace-content"), "active: content side rendered")
            assert(closable.contains("click"), "active: close (ref write + onClose) registered")
        end for
    }

    "DataView loading renders the sheet's overlay + ProgressSpinner over the content" in {
        for
            loading <- renderHtml(
                uic.DataView[String]().items(Seq("a", "b")).itemTemplate(s => div(span(s))).loading(true)
            )
            idle <- renderHtml(
                uic.DataView[String]().items(Seq("a")).itemTemplate(s => div(span(s)))
            )
        yield
            assert(loading.contains("p-dataview-loading"), "root loading modifier (overlay anchor)")
            assert(loading.contains("p-dataview-loading-overlay"), "sheet overlay class")
            assert(loading.contains("p-overlay-mask"), "dimming mask composed")
            assert(!loading.contains("p-overlay-mask-enter-active"), "no transient enter class (paints transparent when permanent)")
            assert(loading.contains("p-progressspinner"), "ProgressSpinner composed")
            assert(loading.contains("p-dataview-content"), "content still rendered under the overlay")
            assert(!idle.contains("p-dataview-loading"), "idle: no loading modifier")
            assert(!idle.contains("p-dataview-loading-overlay"), "idle: no overlay")
    }

    "Theme carries the kyo remainder for IconField / FloatLabel / TextArea / Select / DataView" in {
        val extra = uic.Theme.primeExtraCss
        assert(extra.contains(".p-hidden-accessible"), "hidden-accessible base helper (remainder)")
        assert(extra.contains(".p-iconfield .p-textarea.p-uic-iconfield-start"), "iconfield textarea padding (remainder)")
        assert(extra.contains(".p-iconfield .p-select.p-uic-iconfield-start .p-select-label"), "iconfield select padding (remainder)")
        assert(
            extra.contains(".p-floatlabel:has(.p-select:focus-within) label"),
            "floatlabel select focus float via :focus-within (remainder)"
        )
        assert(extra.contains("span.p-uic-toggleswitch-handle-icon"), "toggleswitch handle icon sizing (remainder)")
        assert(extra.contains(".p-dataview-loading { position: relative;"), "dataview loading anchor (remainder)")
        assert(uic.Theme.primeCss.contains(".p-dataview-loading-overlay"), "extracted dataview loading overlay present")
        assert(uic.Theme.primeCss.contains(".p-togglebutton-fluid"), "extracted togglebutton fluid rule present")
    }

    "Theme carries the kyo remainder for the Overlay primitive + Select panel" in {
        val extra = uic.Theme.primeExtraCss
        assert(extra.contains(".p-uic-overlay-anchor { position: relative; }"), "anchor glue class")
        assert(extra.contains(".p-uic-overlay-backdrop"), "backdrop geometry")
        assert(extra.contains(".p-uic-overlay-panel"), "panel geometry")
        assert(extra.contains(".p-uic-overlay-bottom-start"), "bottom-start anchor rule")
        assert(extra.contains(".p-uic-overlay-bottom-end"), "bottom-end anchor rule")
        assert(extra.contains(".p-uic-overlay-top-start"), "top-start anchor rule")
        assert(extra.contains(".p-uic-overlay-top-end"), "top-end anchor rule")
        assert(extra.contains(".p-uic-overlay-match-width { min-width: 100%; }"), "matchWidth rule")
        assert(extra.contains(".p-select:not(.p-disabled):focus-within"), "select focus ring via :focus-within")
        assert(
            extra.contains(".p-select-overlay .p-select-list-container { max-height: 14rem; }"),
            "list scroll cap (Prime scrollHeight default)"
        )
        assert(extra.contains("button.p-select-clear-icon"), "clear button chrome strip")
        assert(uic.Theme.primeCss.contains(".p-select-overlay"), "extracted select overlay skin present")
        assert(uic.Theme.primeCss.contains(".p-select-option"), "extracted select option rules present")
        assert(uic.Theme.primeCss.contains(".p-select-filter"), "extracted select filter rule present")
    }

    "Theme carries the kyo remainder for Tooltip/Popover + the floating AutoComplete/DatePicker panels" in {
        val extra = uic.Theme.primeExtraCss
        assert(extra.contains(".p-uic-tooltip { position: relative;"), "tooltip hover wrapper (remainder)")
        assert(
            extra.contains(".p-uic-tooltip:hover > .p-tooltip, .p-uic-tooltip:focus-within > .p-tooltip { display: block; }"),
            "hover/focus-within show rule (remainder)"
        )
        assert(extra.contains(".p-uic-tooltip > .p-tooltip-top"), "tooltip top placement (remainder)")
        assert(
            extra.contains(".p-tooltip-left .p-tooltip-arrow { right: 0; top: 50%; }"),
            "tooltip arrow placement (remainder — Prime inline styles)"
        )
        assert(extra.contains(".p-popover { --p-popover-arrow-left: 0px; }"), "popover arrow-left fix (remainder — Prime sets it from JS)")
        assert(
            extra.contains(".p-uic-overlay-panel.p-popover { margin-top: var(--p-popover-gutter); }"),
            "popover arrow gutter over the overlay geometry (remainder)"
        )
        assert(extra.contains(".p-uic-popover-anchor"), "popover anchor hug (remainder)")
        assert(extra.contains(".p-autocomplete > span[data-kyo-reactive]"), "autocomplete reactive-span field sizing (remainder)")
        assert(
            extra.contains(".p-autocomplete-overlay .p-autocomplete-list-container { display: block; max-height: 14rem; }"),
            "autocomplete list scroll cap (Prime scrollHeight default)"
        )
        assert(extra.contains("button.p-autocomplete-clear-icon"), "autocomplete clear button chrome strip (remainder)")
        assert(extra.contains("button.p-autocomplete-dropdown { font: inherit; }"), "autocomplete dropdown font inherit (remainder)")
        assert(
            extra.contains(".p-datepicker-panel-inline { margin-block-start: 0.25rem; }"),
            "inline datepicker panel gap scoped to the inline variant (remainder)"
        )
        assert(uic.Theme.primeCss.contains(".p-tooltip-arrow"), "extracted tooltip sheet present")
        assert(uic.Theme.primeCss.contains(".p-popover:after"), "extracted popover arrow rules present")
        assert(uic.Theme.primeCss.contains(".p-autocomplete-overlay"), "extracted autocomplete overlay skin present")
        assert(uic.Theme.primeCss.contains(".p-autocomplete-dropdown"), "extracted autocomplete dropdown rules present")
    }

    // ---- the menu family ----

    private val menuItems = Seq(
        uic.MenuItem("New").icon(uic.Icons.plus).onSelect(()),
        uic.MenuItem("Open").url("/open"),
        uic.MenuItem.separator,
        uic.MenuItem("Quit").disabled(true)
    )

    "Menu (inline + wired) renders Prime anatomy: list, rows, separator, section label, focus row" in {
        def wiredHtml(m: uic.Menu, hi: Int = -1): String < Async =
            for
                href <- Signal.initRef(hi)
                out  <- UI.runRender(m.wired(href)).take(1).run
            yield out.mkString
        val base = uic.Menu().items(menuItems*)
        for
            html      <- wiredHtml(base)
            highlight <- wiredHtml(base, hi = 0)
            ided      <- wiredHtml(uic.Menu().id("m1").items(menuItems*), hi = 0)
            grouped <- wiredHtml(
                uic.Menu().items(uic.MenuItem("Documents").items(uic.MenuItem("New").icon(uic.Icons.plus)))
            )
        yield
            assert(html.contains("p-menu"), "root class")
            assert(html.contains("p-component"), "p-component class")
            assert(html.contains("p-menu-list"), "list ul")
            assert(html.contains("""role="menu""""), "menu role")
            assert(html.contains("p-menu-item"), "item rows")
            assert(html.contains("p-menu-item-content"), "item content div")
            assert(html.contains("p-menu-item-link"), "item link anchor")
            assert(html.contains("p-menu-item-label"), "item label span")
            assert(html.contains("p-menu-item-icon"), "item icon slot")
            assert(html.contains("""data-uic-icon="plus""""), "icon glyph rendered")
            assert(html.contains("p-menu-separator"), "separator row")
            assert(html.contains("""role="separator""""), "separator role")
            assert(html.contains("""href="/open""""), "url item renders a real href")
            assert(html.contains("p-disabled"), "disabled row carries p-disabled")
            assert(html.contains("""aria-disabled="true""""), "disabled row carries aria-disabled")
            assert(html.contains("keydown"), "inline list registers the keyboard (data-kyo-ev)")
            assert(html.contains("click"), "actionable rows register click")
            assert(!html.contains("p-focus"), "no highlight before keyboard navigation")
            assert(!html.contains("p-uic-overlay-panel"), "inline: no overlay machinery")
            assert(highlight.contains("p-focus"), "highlight ref stamps Prime's .p-focus row")
            assert(html.contains("""tabindex="-1""""), "roving: item links carry tabindex=-1 (out of Tab order)")
            assert(!html.contains("""p-menu-item-link" tabindex="0""""), "roving: links are NOT their own tab stops")
            assert(ided.contains("""aria-activedescendant="m1-active""""), "id wires aria-activedescendant to the focused row")
            assert(ided.contains("""id="m1-active""""), "the focused row carries the referenced id")
            assert(grouped.contains("p-menu-submenu-label"), "grouped items flatten to a section heading")
            assert(grouped.contains("Documents"), "section heading text")
        end for
    }

    "Menu (popup, wired) rides the Overlay: p-menu-overlay skin, focus seeding, Escape/outside dismiss" in {
        def popupHtml(open: Boolean): String < Async =
            for
                oref <- Signal.initRef(open)
                href <- Signal.initRef(-1)
                out  <- UI.runRender(uic.Menu().items(menuItems*).popup(oref).wired(href)).take(1).run
            yield out.mkString
        for
            open   <- popupHtml(true)
            closed <- popupHtml(false)
        yield
            assert(open.contains("p-uic-overlay-backdrop"), "open: outside-click backdrop (Overlay primitive)")
            assert(open.contains("p-uic-overlay-panel"), "open: overlay panel geometry class")
            assert(open.contains("p-menu-overlay"), "open: Prime's popup skin class")
            assert(open.contains("p-menu-list"), "open: list inside the panel")
            assert(open.contains("""data-kyo-focus-auto="1""""), "open: panel seeds focus (keyboard without prior click)")
            assert(open.contains("""data-kyo-focus-restore="1""""), "open: focus returns to the trigger on close")
            assert(open.contains("keydown"), "open: panel registers Escape + arrow navigation")
            assert(!open.contains("p-uic-overlay-match-width"), "popup sizes to content (no matchWidth)")
            assert(!closed.contains("p-menu-list"), "closed: nothing rendered")
        end for
    }

    "Menubar (wired) renders the root bar; open items stamp active + nested submenu Overlays" in {
        val mb = uic.Menubar()
            .start(span("LOGO"))
            .end(span("END"))
            .items(
                uic.MenuItem("File").items(
                    uic.MenuItem("New").icon(uic.Icons.plus).onSelect(()),
                    uic.MenuItem("Recent").items(uic.MenuItem("a.txt"))
                ),
                uic.MenuItem("Home").icon(uic.Icons.home).onSelect(())
            )
        def wiredHtml(openPaths: List[List[Int]], focus: List[Int] = Nil, setId: Boolean = false): String < Async =
            for
                refs <- Kyo.foreach(mb.submenuPaths)(p => Signal.initRef(openPaths.contains(p)).map(p -> _))
                fref <- Signal.initRef(focus)
                out  <- UI.runRender((if setId then mb.id("mb1") else mb).wired(refs.toList, fref)).take(1).run
            yield out.mkString
        for
            closed      <- wiredHtml(Nil)
            open        <- wiredHtml(List(List(0)))
            nested      <- wiredHtml(List(List(0), List(0, 1)))
            highlighted <- wiredHtml(Nil, focus = List(1), setId = true)
        yield
            assert(closed.contains("p-menubar"), "root class")
            assert(closed.contains("p-menubar-root-list"), "root list")
            assert(closed.contains("""role="menubar""""), "menubar role")
            assert(closed.contains("p-menubar-start"), "start slot")
            assert(closed.contains("p-menubar-end"), "end slot")
            assert(closed.contains("p-menubar-item-link"), "item link anchor")
            assert(closed.contains("p-menubar-submenu-icon"), "submenu glyph on parent rows")
            assert(closed.contains("""data-uic-icon="angle-down""""), "root parent glyph is angle-down")
            assert(closed.contains("p-uic-overlay-anchor"), "parent rows carry the anchor glue")
            assert(!closed.contains("p-menubar-submenu\""), "closed: no submenu panel")
            assert(!closed.contains("p-menubar-item-active"), "closed: no active row")
            assert(closed.contains("""tabindex="-1""""), "roving: item links out of the Tab order")
            assert(closed.contains("keydown"), "root list registers the keyboard")
            assert(open.contains("p-menubar-item-active"), "open: active root row")
            assert(open.contains("p-menubar-submenu"), "open: submenu panel skin")
            assert(open.contains("p-uic-overlay-panel"), "open: submenu rides the Overlay")
            assert(open.contains("p-uic-overlay-bottom-start"), "open: root submenu opens BELOW")
            assert(open.contains("p-uic-overlay-backdrop"), "open: per-level outside-click backdrop")
            assert(!open.contains("""data-kyo-focus-auto="1""""), "roving: submenu does NOT seed focus (root list keeps the keys)")
            assert(open.contains("""data-uic-icon="angle-right""""), "nested parent glyph is angle-right")
            assert(nested.contains("p-uic-overlay-right-start"), "nested submenu opens to the SIDE")
            assert(nested.contains("a.txt"), "nested submenu content renders")
            assert(highlighted.contains("p-focus"), "highlight stamps Prime's .p-focus row")
            assert(highlighted.contains("""aria-activedescendant="mb1-active""""), "id wires activedescendant")
            assert(highlighted.contains("""id="mb1-active""""), "focused row carries the referenced id")
        end for
    }

    "TieredMenu (wired) renders side-nested submenus; popup mode rides the Overlay" in {
        val tm = uic.TieredMenu().items(
            uic.MenuItem("File").items(uic.MenuItem("New").onSelect(())),
            uic.MenuItem.separator,
            uic.MenuItem("Quit").onSelect(())
        )
        def wiredHtml(
            openPaths: List[List[Int]],
            popup: Maybe[Boolean] = Absent,
            focus: List[Int] = Nil,
            setId: Boolean = false
        ): String < Async =
            for
                oref <- Signal.initRef(popup.getOrElse(false))
                m0 = popup match
                    case Present(_) => tm.popup(oref)
                    case Absent     => tm
                m = if setId then m0.id("tm1") else m0
                refs <- Kyo.foreach(m.submenuPaths)(p => Signal.initRef(openPaths.contains(p)).map(p -> _))
                fref <- Signal.initRef(focus)
                out  <- UI.runRender(m.wired(refs.toList, fref)).take(1).run
            yield out.mkString
        for
            closed      <- wiredHtml(Nil)
            open        <- wiredHtml(List(List(0)))
            popupOpen   <- wiredHtml(Nil, popup = Present(true))
            highlighted <- wiredHtml(List(List(0)), focus = List(0), setId = true)
        yield
            assert(closed.contains("p-tieredmenu"), "root class")
            assert(closed.contains("p-tieredmenu-root-list"), "root list")
            assert(closed.contains("p-tieredmenu-item-link"), "item link anchor")
            assert(closed.contains("p-tieredmenu-separator"), "separator row")
            assert(closed.contains("""data-uic-icon="angle-right""""), "parent glyph is angle-right")
            assert(closed.contains("""tabindex="-1""""), "roving: item links out of the Tab order")
            assert(closed.contains("""role="menu""""), "root list is role=menu")
            assert(closed.contains("keydown"), "inline list registers the keyboard")
            assert(open.contains("p-tieredmenu-item-active"), "open: active row")
            assert(open.contains("p-tieredmenu-submenu"), "open: submenu panel skin")
            assert(open.contains("p-uic-overlay-right-start"), "open: submenu opens to the SIDE (root level too)")
            assert(!open.contains("""data-kyo-focus-auto="1""""), "roving: an inline submenu panel does NOT seed focus")
            assert(highlighted.contains("p-focus"), "highlight stamps Prime's .p-focus row")
            assert(highlighted.contains("""aria-activedescendant="tm1-active""""), "id wires activedescendant")
            assert(highlighted.contains("""id="tm1-active""""), "focused row carries the referenced id")
            assert(popupOpen.contains("p-tieredmenu-overlay"), "popup: Prime's overlay skin")
            assert(popupOpen.contains("p-uic-overlay-panel"), "popup: rides the Overlay")
            assert(popupOpen.contains("""data-kyo-focus-auto="1""""), "popup: panel seeds focus")
        end for
    }

    "MegaMenu (wired) renders the root bar and the active item's grid panel" in {
        val mm = uic.MegaMenu()
            .items(
                uic.MegaMenuItem("Furniture")
                    .icon(uic.Icons.box)
                    .column(
                        uic.MenuGroup("Living Room").items(uic.MenuItem("Accessories").onSelect(())),
                        uic.MenuGroup("Kitchen").items(uic.MenuItem("Bar stools"))
                    )
                    .column(uic.MenuGroup("Bedroom").items(uic.MenuItem("Beds"))),
                uic.MegaMenuItem("Contact").onSelect(())
            )
        def wiredHtml(
            openPaths: List[List[Int]],
            vertical: Boolean = false,
            focus: List[Int] = Nil,
            setId: Boolean = false
        ): String < Async =
            val m0 = if vertical then mm.orientation(uic.Orientation.Vertical) else mm
            val m  = if setId then m0.id("mg1") else m0
            for
                refs <- Kyo.foreach(m.panelPaths)(p => Signal.initRef(openPaths.contains(p)).map(p -> _))
                fref <- Signal.initRef(focus)
                out  <- UI.runRender(m.wired(refs.toList, fref)).take(1).run
            yield out.mkString
            end for
        end wiredHtml
        for
            closed    <- wiredHtml(Nil)
            open      <- wiredHtml(List(List(0)))
            vertical  <- wiredHtml(List(List(0)), vertical = true)
            itemFocus <- wiredHtml(List(List(0)), focus = List(0, 0, 1), setId = true) // "Bar stools"
            rootFocus <- wiredHtml(Nil, focus = List(1))                               // "Contact" root
        yield
            assert(closed.contains("p-megamenu"), "root class")
            assert(closed.contains("p-megamenu-horizontal"), "horizontal orientation modifier (default)")
            assert(closed.contains("p-megamenu-root-list"), "root list")
            assert(closed.contains("""role="menubar""""), "menubar role")
            assert(closed.contains("p-megamenu-submenu-icon"), "submenu glyph on panel rows")
            assert(closed.contains("""tabindex="-1""""), "roving: item links out of the Tab order")
            assert(closed.contains("keydown"), "root list registers the keyboard")
            assert(!closed.contains("p-megamenu-overlay"), "closed: no panel")
            assert(open.contains("p-megamenu-item-active"), "open: active root row")
            assert(open.contains("p-megamenu-overlay"), "open: panel skin")
            assert(open.contains("p-megamenu-grid"), "open: column grid")
            assert(open.contains("p-megamenu-col-6"), "open: two columns split the raster (col-6)")
            assert(open.contains("p-megamenu-submenu-label"), "open: group headings")
            assert(open.contains("Living Room"), "open: group heading text")
            assert(open.contains("p-megamenu-submenu"), "open: group lists")
            assert(open.contains("p-uic-overlay-backdrop"), "open: outside-click backdrop")
            assert(!open.contains("""data-kyo-focus-auto="1""""), "roving: the panel does NOT seed focus (root list keeps the keys)")
            assert(itemFocus.contains("p-focus"), "panel item highlight stamps .p-focus")
            assert(itemFocus.contains("""aria-activedescendant="mg1-active""""), "id wires activedescendant")
            assert(itemFocus.contains("""id="mg1-active""""), "focused panel item carries the referenced id")
            assert(rootFocus.contains("p-focus"), "root-bar highlight stamps .p-focus")
            assert(vertical.contains("p-megamenu-vertical"), "vertical orientation modifier")
            assert(vertical.contains("p-uic-overlay-right-start"), "vertical: panel opens to the side")
        end for
    }

    "SplitButton (wired) renders both segments and the popup Menu panel" in {
        def wiredHtml(sb: uic.SplitButton, open: Boolean): String < Async =
            for
                oref <- Signal.initRef(open)
                href <- Signal.initRef(-1)
                out  <- UI.runRender(sb.wired(oref, href)).take(1).run
            yield out.mkString
        val base = uic.SplitButton("Save").items(
            uic.MenuItem("Update").onSelect(()),
            uic.MenuItem.separator,
            uic.MenuItem("Delete").onSelect(())
        )
        for
            closed <- wiredHtml(base, open = false)
            open   <- wiredHtml(base, open = true)
            styled <- wiredHtml(
                base.severity(uic.Severity.Danger).size(uic.Size.Small).rounded(true).raised(true).fluid(true),
                open = false
            )
            disabled <- wiredHtml(base.disabled(true), open = false)
        yield
            assert(closed.contains("p-splitbutton"), "root class")
            assert(closed.contains("p-splitbutton-button"), "primary segment class")
            assert(closed.contains("p-splitbutton-dropdown"), "dropdown segment class")
            assert(closed.contains("p-button-icon-only"), "dropdown segment is icon-only Button anatomy")
            assert(closed.contains("""data-uic-icon="chevron-down""""), "dropdown chevron glyph")
            assert(closed.contains("""aria-haspopup="menu""""), "dropdown announces the popup")
            assert(closed.contains("""aria-expanded="false""""), "closed: aria-expanded false")
            assert(!closed.contains("p-menu-list"), "closed: no panel")
            assert(open.contains("""aria-expanded="true""""), "open: aria-expanded true")
            assert(open.contains("p-menu-overlay"), "open: popup Menu panel skin")
            assert(open.contains("p-menu-list"), "open: menu rows render")
            assert(open.contains("p-menu-separator"), "open: separator renders")
            assert(styled.contains("p-button-danger"), "severity passes to both segments")
            assert(styled.contains("p-button-sm"), "size passes through")
            assert(styled.contains("p-splitbutton-rounded"), "rounded modifier")
            assert(styled.contains("p-splitbutton-raised"), "raised modifier")
            assert(styled.contains("p-splitbutton-fluid"), "fluid modifier")
            assert(disabled.contains("disabled"), "disabled reaches the native buttons")
        end for
    }

    "SpeedDial (wired) renders the toggle + linear action fan; open stamps p-speeddial-open" in {
        def wiredHtml(sd: uic.SpeedDial, open: Boolean): String < Async =
            for
                oref <- Signal.initRef(open)
                out  <- UI.runRender(sd.wired(oref, "sd", _ => ())).take(1).run
            yield out.mkString
        val base = uic.SpeedDial().items(
            uic.MenuItem("Add").icon(uic.Icons.pencil).onSelect(()),
            uic.MenuItem("Delete").icon(uic.Icons.trash).onSelect(())
        )
        for
            closed <- wiredHtml(base, open = false)
            open   <- wiredHtml(base, open = true)
            down   <- wiredHtml(base.direction(uic.SpeedDialDirection.Down), open = false)
        yield
            assert(closed.contains("p-speeddial"), "root class")
            assert(closed.contains("p-speeddial-up"), "Up is the default direction")
            assert(closed.contains("p-speeddial-button"), "toggle button class")
            assert(closed.contains("p-speeddial-rotate"), "toggle rotate class (sheet rotates the plus glyph)")
            assert(closed.contains("""data-uic-icon="plus""""), "toggle plus glyph")
            assert(closed.contains("p-speeddial-list"), "action list")
            assert(closed.contains("""aria-controls="sd-list""""), "the toggle names the menu it opens")
            assert(!closed.contains("p-speeddial-item"), "closed: the fan renders no actions to tab through")
            assert(!closed.contains("p-speeddial-open"), "closed: no open modifier")
            assert(closed.contains("""aria-expanded="false""""), "closed: aria-expanded false")
            assert(open.contains("p-speeddial-item"), "open: action rows")
            assert(open.contains("p-button-rounded"), "actions are rounded icon Buttons")
            assert(open.contains("p-button-secondary"), "actions carry the secondary severity")
            assert(open.contains("p-button-sm"), "actions are small (Prime's 32px fan buttons)")
            assert(open.contains("""aria-label="Add""""), "action label becomes the accessible name")
            assert(open.contains("""title="Add""""), "action label becomes the native tooltip")
            assert(open.contains("""role="menuitem""""), "an action in a role=menu is a menuitem")
            assert(!open.contains("""tabindex="0""""), "open: no action is a tab stop, the dial's one tab stop is the toggle")
            assert(open.contains("""tabindex="-1""""), "an action is focusable, but only by opening the fan")
            assert(open.contains("p-speeddial-open"), "open: open modifier (sheet scales the fan in)")
            assert(open.contains("""aria-expanded="true""""), "open: aria-expanded true")
            assert(open.contains("""data-kyo-focus-auto="1""""), "open: the fan seeds focus onto its first action")
            assert(open.contains("""data-kyo-focus-restore="1""""), "and hands it back to the toggle on close")
            assert(open.contains("data-kyo-scroll-keys"), "the arrows the fan answers do not scroll the page")
            assert(down.contains("p-speeddial-down"), "direction modifier class")
        end for
    }

    "Theme carries the kyo remainder for the menu family" in {
        val extra = uic.Theme.primeExtraCss
        assert(extra.contains(".p-uic-overlay-right-start"), "side anchor rule (nested submenus)")
        assert(extra.contains(".p-uic-overlay-left-start"), "left side anchor rule")
        assert(extra.contains(".p-uic-overlay-panel.p-menubar-submenu"), "menubar submenu panel restorer")
        assert(
            extra.contains(".p-uic-overlay-panel.p-tieredmenu-overlay { will-change: auto; }"),
            "tieredmenu popup will-change neutralized (backdrop trap)"
        )
        assert(extra.contains(".p-speeddial-up { flex-direction: column-reverse;"), "speeddial direction layout (Prime inline styles)")
        assert(extra.contains("span.p-menubar-submenu-icon"), "submenu glyph sizing")
        assert(uic.Theme.primeCss.contains(".p-menu-overlay"), "extracted menu sheet present")
        assert(uic.Theme.primeCss.contains(".p-menubar-submenu"), "extracted menubar sheet present")
        assert(uic.Theme.primeCss.contains(".p-tieredmenu-submenu"), "extracted tieredmenu sheet present")
        assert(uic.Theme.primeCss.contains(".p-megamenu-grid"), "extracted megamenu sheet present")
        assert(uic.Theme.primeCss.contains(".p-splitbutton-dropdown.p-button"), "extracted splitbutton sheet present")
        assert(uic.Theme.primeCss.contains(".p-speeddial-open .p-speeddial-item"), "extracted speeddial sheet present")
    }

    // ==== MultiSelect / CascadeSelect / TreeSelect / Drawer ====

    "MultiSelect (closed) renders Prime's trigger: label modes, chips, clear, form carrier" in {
        def closedHtml(ms: uic.MultiSelect[String], sel: Set[String]): String < Async =
            for
                ref <- Signal.initRef(sel)
                out <- UI.runRender(ms.value(ref)).take(1).run
            yield out.mkString
        val base = uic.MultiSelect[String]().options(Seq("Apple", "Banana", "Cherry"))

        for
            comma       <- closedHtml(base, Set("Apple", "Cherry"))
            placeholder <- closedHtml(base.placeholder("Pick fruit"), Set.empty)
            overMax     <- closedHtml(base.maxSelectedLabels(1).selectedItemsLabel("{0} picked"), Set("Apple", "Banana"))
            chips       <- closedHtml(base.display(uic.MultiSelectDisplay.Chip), Set("Apple", "Banana"))
            named       <- closedHtml(base.name("fruit"), Set("Apple", "Cherry"))
            styled <- closedHtml(
                base.invalid(true).invalidMessage("Required").size(uic.Size.Small).fluid(true).variant(uic.FieldVariant.Filled),
                Set.empty
            )
        yield
            assert(comma.contains("p-multiselect"), "root field class")
            assert(comma.contains("p-component"), "p-component class")
            assert(comma.contains("p-inputwrapper"), "Prime stamps p-inputwrapper on the multiselect root")
            assert(comma.contains("p-inputwrapper-filled"), "non-empty selection fills the wrapper")
            assert(comma.contains("p-multiselect-label-container"), "label container div")
            assert(comma.contains("""class="p-multiselect-label""""), "label div carries Prime's label class")
            assert(comma.contains("Apple, Cherry"), "comma display joins the selected labels in options order")
            assert(!comma.contains("Banana"), "unselected options do not render while closed")
            assert(comma.contains("""aria-haspopup="listbox""""), "trigger advertises the listbox popup")
            assert(comma.contains("""aria-expanded="false""""), "closed trigger reads collapsed")
            assert(comma.contains("""tabindex="0""""), "trigger is focusable")
            assert(comma.contains("p-multiselect-dropdown"), "chevron dropdown affordance")
            assert(comma.contains("""data-uic-icon="chevron-down""""), "chevron glyph")
            assert(placeholder.contains("Pick fruit"), "placeholder text while the set is empty")
            assert(placeholder.contains("p-placeholder"), "placeholder skin class")
            assert(!placeholder.contains("p-inputwrapper-filled"), "empty selection does not fill the wrapper")
            assert(overMax.contains("2 picked"), "beyond maxSelectedLabels the selectedItemsLabel summary shows")
            assert(chips.contains("p-multiselect-display-chip"), "chip display root modifier")
            assert(chips.contains("p-multiselect-chip-item"), "chip item wrapper span")
            assert(chips.contains("p-multiselect-chip"), "contextual chip class")
            assert(chips.contains("p-chip-remove-icon"), "chips carry the remove affordance")
            assert(named.contains("""data-kyo-prop-name="fruit""""), "name(...) emits the hidden form carrier")
            assert(named.contains("Apple,Cherry"), "carrier holds the comma-joined keys")
            assert(styled.contains("p-invalid"), "invalid class")
            assert(styled.contains("p-uic-invalid-message"), "invalidMessage row")
            assert(styled.contains("p-multiselect-sm"), "small size class")
            assert(styled.contains("p-multiselect-fluid"), "fluid class")
            assert(styled.contains("p-variant-filled"), "filled variant class")
        end for
    }

    "MultiSelect (open, wired) renders the panel: header select-all + filter, checkbox rows" in {
        def openHtml(ms: uic.MultiSelect[String], sel: Set[String], hi: Int = -1): String < Async =
            for
                vref <- Signal.initRef(sel)
                oref <- Signal.initRef(true)
                href <- Signal.initRef(hi)
                qref <- Signal.initRef("")
                out  <- UI.runRender(ms.value(vref).open(oref).wired(oref, href, qref)).take(1).run
            yield out.mkString
        val base = uic.MultiSelect[String]().options(Seq("Apple", "Banana"))

        for
            open      <- openHtml(base, Set("Apple"))
            highlight <- openHtml(base, Set("Apple"), hi = 1)
            allPicked <- openHtml(base, Set("Apple", "Banana"))
            featured  <- openHtml(base.filterable(true).showClear(true), Set("Apple"))
            noToggle  <- openHtml(base.showToggleAll(false), Set("Apple"))
            disabled  <- openHtml(base.optionDisabled(_ == "Banana"), Set.empty)
            hilite    <- openHtml(base.highlightOnSelect(true), Set("Apple"))
        yield

            assert(open.contains("p-multiselect-open"), "open: root modifier class")
            assert(open.contains("p-uic-overlay-anchor"), "open: anchor glue class")
            assert(open.contains("""aria-expanded="true""""), "open: trigger reads expanded")
            assert(open.contains("p-uic-overlay-backdrop"), "open: outside-click backdrop")
            assert(open.contains("p-multiselect-overlay"), "open: Prime's panel skin class")
            // Focus stays on the trigger, which is what makes the trigger the combobox.
            assert(!open.contains("""data-kyo-focus-auto="1""""), "open: the panel seeds no focus")
            assert(!open.contains("""data-kyo-focus-trap="1""""), "open: and traps none either")
            assert(featured.contains("""data-kyo-focus-auto="1""""), "a filter header takes focus into the panel")
            assert(open.contains("p-multiselect-header"), "open: header with the select-all checkbox")
            assert(open.contains("p-checkbox-input"), "header select-all is the REAL Checkbox anatomy")
            assert(open.contains("change"), "header select-all registers its change handler")
            assert(open.contains("p-multiselect-list-container"), "open: scrollable list container")
            assert(open.contains("""role="listbox""""), "open: listbox role")
            assert(open.contains("""aria-multiselectable="true""""), "open: multiselectable list")
            assert(open.contains("p-multiselect-option"), "open: option rows")
            // PrimeVue's highlightOnSelect defaults to FALSE: the checked box alone
            // marks a selected row (verified against live PrimeVue 4.5).
            assert(!open.contains("p-multiselect-option-selected"), "open: no highlighted row skin by default")
            assert(hilite.contains("p-multiselect-option-selected"), "highlightOnSelect(true): selected row skin")
            assert(open.contains("""aria-selected="true""""), "open: aria-selected on the picked row")
            assert(open.contains("p-checkbox-box"), "open: rows carry the Prime checkbox anatomy")
            assert(open.contains("""data-uic-icon="check""""), "open: selected row's box shows the check glyph")
            assert(!open.contains("p-focus"), "open: no highlight before keyboard navigation")
            assert(highlight.contains("p-focus"), "highlight ref stamps Prime's .p-focus row")
            // Every checkbox (header select-all + both rows) reads checked — 3 boxes.
            assert(("p-checkbox-checked".r.findAllIn(allPicked).size == 3), "all visible selected: select-all AND both rows checked")
            assert(featured.contains("p-multiselect-filter-container"), "filter(true): IconField filter container")
            assert(featured.contains("p-multiselect-filter"), "filter(true): the filter input")
            // A filter header takes focus into the panel, which makes IT the combobox: the
            // announcement is read off the focused element, so it has to be the one that carries
            // the role, the popup reference and the highlight.
            assert(featured.contains("""role="combobox""""), "the filter input is the combobox once there is one")
            assert(featured.contains("""data-kyo-focus-auto="1""""), "and it is what focus is seeded onto")
            assert(featured.contains("p-multiselect-clear-icon"), "showClear(true): clear affordance")
            assert(!noToggle.contains("p-checkbox-input"), "showToggleAll(false): no header checkbox (rows are inert anatomy)")
            assert(disabled.contains("p-disabled"), "optionDisabled rows carry .p-disabled")
            assert(disabled.contains("""aria-disabled="true""""), "optionDisabled rows carry aria-disabled")
        end for
    }

    "MultiSelect optionGroups renders grouped rows; select-all keeps counting across every group" in {
        def openHtml(ms: uic.MultiSelect[String], sel: Set[String]): String < Async =
            for
                vref <- Signal.initRef(sel)
                oref <- Signal.initRef(true)
                href <- Signal.initRef(-1)
                qref <- Signal.initRef("")
                out  <- UI.runRender(ms.value(vref).open(oref).wired(oref, href, qref)).take(1).run
            yield out.mkString
        val base = uic.MultiSelect[String]().optionGroups(Seq(
            uic.OptionItem.group("Fruit")("Apple", "Banana"),
            uic.OptionItem.group("Vegetable")("Carrot")
        ))(identity)
        for
            none    <- openHtml(base, Set.empty)
            partial <- openHtml(base, Set("Apple", "Banana"))
            every   <- openHtml(base, Set("Apple", "Banana", "Carrot"))
        yield
            assert(none.contains("p-multiselect-option-group"), "Prime's header class stays on an li element")
            assert(none.contains("""role="group""""), "the group is a real ARIA group")
            assert(none.contains("""aria-label="Vegetable""""), "each group carries its label as its accessible name")
            assert(none.contains("p-uic-option-group-list"), "nested list holding the header and the options")
            assert("p-checkbox-checked".r.findAllIn(none).size == 0, "nothing picked: no box reads checked")
            assert(
                "p-checkbox-checked".r.findAllIn(partial).size == 2,
                "one group fully picked: its two rows check, select-all stays open across the other group"
            )
            assert(
                "p-checkbox-checked".r.findAllIn(every).size == 4,
                "every group picked: select-all and all three rows check"
            )
        end for
    }

    "CascadeSelect announces the option the highlight is on, wherever the nesting has taken it" in {
        // One id travels with the highlight rather than one id per option: the rows live across
        // nested panels, and `aria-activedescendant` names exactly one of them. The TRIGGER carries
        // it, however deep the chain goes, because the trigger is the one element that ever holds
        // focus and the attribute is read off the focused element or off nothing at all.
        def html(focusAt: List[Int], openPaths: List[List[Int]]): String < Async =
            val cs = uic.CascadeSelect[String]()
                .options(
                    Seq(
                        uic.CascadeItem.group("Germany")(uic.CascadeItem.leaf("Berlin")),
                        uic.CascadeItem.leaf("Zurich")
                    )
                )(identity)
                .id("cs")
            for
                vref <- Signal.initRef("")
                oref <- Signal.initRef(true)
                refs <- Kyo.foreach(cs.value(vref).groupPaths)(p => Signal.initRef(openPaths.contains(p)).map(p -> _))
                fref <- Signal.initRef(focusAt)
                out  <- UI.runRender(cs.value(vref).open(oref).wired(oref, refs.toList, fref, Present("cs"))).take(1).run
            yield out.mkString
            end for
        end html
        for
            root   <- html(List(1), Nil)
            nested <- html(List(0, 0), List(List(0)))
            none   <- html(Nil, Nil)
        yield
            assert(
                tagWithClass(root, "p-cascadeselect").contains("""aria-activedescendant="cs-active""""),
                "the trigger, which is the combobox, carries the announcement"
            )
            assert(
                !tagWithClass(root, "p-cascadeselect-list").contains("activedescendant"),
                "and the list, which never holds focus, does not"
            )
            assert(root.contains("""id="cs-active""""), "and the highlighted row answers to that id")
            assert(
                tagWithClass(nested, "p-cascadeselect").contains("""aria-activedescendant="cs-active""""),
                "still the trigger, one level down"
            )
            assert(!none.contains("activedescendant"), "nothing highlighted announces nothing")
        end for
    }

    "CascadeSelect (wired) renders the trigger + nested group panel chain" in {
        def html(cs: uic.CascadeSelect[String], current: String, rootOpen: Boolean, openPaths: List[List[Int]]): String < Async =
            for
                vref <- Signal.initRef(current)
                oref <- Signal.initRef(rootOpen)
                refs <- Kyo.foreach(cs.value(vref).groupPaths)(p => Signal.initRef(openPaths.contains(p)).map(p -> _))
                fref <- Signal.initRef(List.empty[Int])
                out  <- UI.runRender(cs.value(vref).open(oref).wired(oref, refs.toList, fref)).take(1).run
            yield out.mkString
        val base = uic.CascadeSelect[String]()
            .options(
                Seq(
                    uic.CascadeItem.group("Germany")(
                        uic.CascadeItem.group("Bavaria")(uic.CascadeItem.leaf("Munich")),
                        uic.CascadeItem.leaf("Berlin")
                    ),
                    uic.CascadeItem.leaf("Zurich")
                )
            )(identity)
        for

            closed <- html(base.placeholder("Select a city"), "", rootOpen = false, Nil)
            // Zurich is a ROOT-level leaf: its row is visible without any group open
            // (a selected leaf inside a closed group renders only its trigger label).
            open   <- html(base, "Zurich", rootOpen = true, Nil)
            nested <- html(base, "", rootOpen = true, List(List(0), List(0, 0)))
        yield

            assert(closed.contains("p-cascadeselect"), "root field class")
            assert(closed.contains("p-inputwrapper"), "Prime stamps p-inputwrapper on the cascadeselect root")
            assert(closed.contains("""class="p-cascadeselect-label p-placeholder""""), "trigger label span with placeholder skin")
            assert(closed.contains("Select a city"), "placeholder text")
            assert(closed.contains("p-cascadeselect-dropdown"), "chevron dropdown affordance")
            assert(closed.contains("""aria-haspopup="tree""""), "trigger advertises the tree popup")
            assert(!closed.contains("p-cascadeselect-overlay"), "closed: no panel")
            assert(open.contains("p-cascadeselect-open"), "open: root modifier class")
            assert(open.contains("Zurich"), "trigger shows the selected leaf's label")
            assert(open.contains("p-cascadeselect-overlay"), "open: Prime's panel skin")
            assert(open.contains("p-cascadeselect-list-container"), "open: list container")
            assert(open.contains("""role="tree""""), "open: root list is the tree")
            assert(open.contains("p-cascadeselect-option-group"), "group rows marked")
            assert(open.contains("p-cascadeselect-group-icon-container"), "group icon container span")
            assert(open.contains("""data-uic-icon="angle-right""""), "group angle glyph")
            assert(open.contains("p-cascadeselect-option-selected"), "selected leaf row marked")
            assert(open.contains("p-cascadeselect-option-text"), "option text spans")
            assert(!open.contains("p-cascadeselect-option-active"), "no group open: no active row")
            assert(!open.contains("p-cascadeselect-option-list"), "no group open: no sub-panel")
            assert(nested.contains("p-cascadeselect-option-active"), "nested: open groups stamp the active row")
            assert(nested.contains("p-cascadeselect-option-list"), "nested: sub-panel class chain")
            assert(nested.contains("p-uic-overlay-right-start"), "nested: side anchor geometry")
            assert(nested.contains("""role="group""""), "nested lists carry the group role")
            assert(nested.contains("Munich"), "deep leaf renders in the open chain")
        end for
    }

    "TreeSelect (wired) hosts the REAL Tree in the floating panel" in {
        def html(ts: uic.TreeSelect, sel: Set[String], exp: Set[String], open: Boolean): String < Async =
            for
                vref <- Signal.initRef(sel)
                oref <- Signal.initRef(open)
                eref <- Signal.initRef(exp)
                href <- Signal.initRef(-1)
                out  <- UI.runRender(ts.value(vref).open(oref).expanded(eref).wired(oref, eref, href, "ts")).take(1).run
            yield out.mkString
        val base = uic.TreeSelect().nodes(
            uic.TreeNode(
                "src",
                "src",
                children = List(uic.TreeNode("App.scala", "app"), uic.TreeNode("Theme.scala", "theme"))
            )
        )

        for
            closed   <- html(base.placeholder("Select a file"), Set.empty, Set.empty, open = false)
            open     <- html(base, Set("app"), Set("src"), open = true)
            checkbox <- html(base.selectionMode(uic.SelectionMode.Checkbox), Set("app"), Set("src"), open = true)
        yield

            assert(closed.contains("p-treeselect"), "root field class")
            assert(closed.contains("p-inputwrapper"), "Prime stamps p-inputwrapper on the treeselect root")
            assert(closed.contains("p-treeselect-label-container"), "label container div")
            assert(closed.contains("p-placeholder"), "placeholder skin while empty")
            assert(closed.contains("Select a file"), "placeholder text")
            assert(closed.contains("""aria-haspopup="tree""""), "trigger advertises the tree popup")
            assert(!closed.contains("p-treeselect-overlay"), "closed: no panel")
            assert(open.contains("p-treeselect-open"), "open: root modifier class")
            assert(open.contains("App.scala"), "trigger shows the selected node's label")
            assert(open.contains("p-treeselect-overlay"), "open: Prime's panel skin")
            assert(open.contains("p-treeselect-tree-container"), "open: tree container")
            assert(open.contains("""class="p-tree p-component p-tree-selectable""""), "open: the REAL uic.Tree renders inside")
            assert(open.contains("p-tree-node-content"), "open: tree node anatomy")
            assert(open.contains("p-tree-node-selected"), "open: bound id marks its node selected")
            assert(open.contains("""aria-expanded="true""""), "expanded set reaches the hosted tree")
            assert(!open.contains("""data-kyo-focus-auto="1""""), "open: the panel seeds no focus, the trigger keeps it")
            assert(checkbox.contains("p-tree-node-checkbox"), "Checkbox mode renders Tree's per-node checkbox column")
        end for
    }

    "Drawer (open) renders mask + docked panel; positions + full; (closed) renders nothing" in {
        def drawer(open: Boolean, f: uic.Drawer => uic.Drawer): String < Async =
            for
                ref <- Signal.initRef(open)
                out <- UI.runRender(f(uic.Drawer().open(ref).header("Menu"))(p("body"))).take(1).run
            yield out.mkString
        for
            open     <- drawer(true, identity)
            closed   <- drawer(false, identity)
            right    <- drawer(true, _.position(uic.DrawerPosition.Right))
            full     <- drawer(true, _.position(uic.DrawerPosition.Full))
            footered <- drawer(true, _.footer(span("actions")))
            bare     <- drawer(true, _.dismissable(false).showCloseIcon(false).preventInitialFocus(true).preventFocusRestore(true))
        yield

            assert(open.contains("p-drawer-mask"), "open: drawer mask class")
            assert(open.contains("p-overlay-mask"), "open: Prime overlay mask backdrop (modal)")
            assert(!open.contains("p-overlay-mask-enter-active"), "open: no transient enter class (paints transparent when permanent)")
            assert(open.contains("p-drawer-open"), "open: mask open modifier")
            assert(open.contains("p-drawer-left"), "Left is the default position")
            assert(open.contains("""role="dialog""""), "open: dialog role")
            assert(open.contains("""aria-modal="true""""), "open: modal aria")
            assert(open.contains("""class="p-drawer p-component""""), "open: panel classes")
            assert(open.contains("p-drawer-header"), "open: header element")
            assert(open.contains("p-drawer-title"), "open: title div")
            assert(open.contains("Menu"), "open: header text")
            assert(open.contains("p-drawer-close-button"), "open: Prime close button")
            assert(open.contains("p-button-icon-only"), "open: close button is icon-only Button anatomy")
            assert(open.contains("p-drawer-content"), "open: content element")
            assert(open.contains("""data-kyo-focus-auto="1""""), "open: panel seeds focus")
            assert(open.contains("""data-kyo-focus-restore="1""""), "open: focus returns to the opener")
            assert(open.contains("""data-kyo-focus-trap="1""""), "open: panel traps Tab")
            assert(open.contains("""data-kyo-ev="keydown""""), "open: panel registers Escape")
            assert(right.contains("p-drawer-right"), "position modifier on the mask")
            assert(full.contains("p-drawer-full"), "full position modifier")
            assert(footered.contains("p-drawer-footer"), "footer element")
            assert(!bare.contains("data-kyo-focus-auto"), "preventInitialFocus omits the seed attribute")
            assert(!bare.contains("p-drawer-close-button"), "showCloseIcon(false): no close button")
            assert(!closed.contains("p-drawer-mask"), "closed: no mask rendered")
        end for
    }

    "Theme carries the kyo remainder for multiselect/cascadeselect/treeselect/drawer" in {
        val extra = uic.Theme.primeExtraCss
        assert(extra.contains(".p-multiselect-overlay .p-multiselect-list-container"), "multiselect list container block + scroll cap")
        assert(extra.contains(".p-treeselect-overlay .p-treeselect-tree-container"), "treeselect tree container block + scroll cap")
        assert(
            extra.contains(".p-uic-overlay-panel.p-cascadeselect-option-list { display: block; margin: 0; }"),
            "cascade sub-panel display restorer (sheet display:none)"
        )
        assert(extra.contains("li.p-cascadeselect-option { flex-direction: column;"), "cascade row stacks content over the sub-panel")
        assert(extra.contains("button.p-multiselect-clear-icon"), "multiselect clear button chrome strip")
        assert(extra.contains(".p-drawer-mask { position: fixed;"), "drawer mask positioning (Prime inline styles)")
        assert(extra.contains(".p-drawer-mask.p-drawer-right { justify-content: flex-end;"), "drawer positional alignment")
        assert(extra.contains("span.p-multiselect-dropdown-icon"), "multiselect/cascadeselect/treeselect glyph sizing")
        assert(uic.Theme.primeCss.contains(".p-multiselect-option"), "extracted multiselect sheet present")
        assert(uic.Theme.primeCss.contains(".p-cascadeselect-option-list"), "extracted cascadeselect sheet present")
        assert(uic.Theme.primeCss.contains(".p-treeselect-overlay .p-tree"), "extracted treeselect sheet reaches the hosted tree")
        assert(uic.Theme.primeCss.contains(".p-drawer-left .p-drawer"), "extracted drawer sheet present")
    }

    "ContextMenu (wired) wraps its target, registers contextmenu, and opens the Prime panel" in {
        def html(cm: uic.ContextMenu, open: Boolean, focus: List[Int], openPaths: List[List[Int]]): String < Async =
            for
                oref <- Signal.initRef(open)
                fref <- Signal.initRef(focus)
                refs <- Kyo.foreach(cm.submenuPaths)(p => Signal.initRef(openPaths.contains(p)).map(p -> _))
                out  <- UI.runRender(cm.wired(oref, fref, refs.toList)).take(1).run
            yield out.mkString
        val base = uic.ContextMenu(
            Seq(
                uic.MenuItem("Copy").icon(uic.Icons.copy),
                uic.MenuItem("Paste"),
                uic.MenuItem.separator,
                uic.MenuItem("Share").items(uic.MenuItem("Email"), uic.MenuItem("Link"))
            )
        )(div.cssClass("target-region")(span("right-click me")))

        for
            closed  <- html(base, open = false, focus = Nil, Nil)
            open    <- html(base, open = true, focus = Nil, Nil)
            focused <- html(base.id("cm1"), open = true, focus = List(0), Nil)
            nested  <- html(base, open = true, focus = Nil, List(List(3)))
        yield

            assert(closed.contains("p-uic-contextmenu-target"), "target wrapper class")
            assert(closed.contains("p-uic-overlay-anchor"), "target is the overlay anchor")
            assert(closed.contains("contextmenu"), "target registers the contextmenu event")
            assert(closed.contains("""aria-haspopup="menu""""), "target advertises the menu popup")
            assert(closed.contains("target-region"), "target children render")
            assert(!closed.contains("p-contextmenu-root-list"), "closed: no panel")
            assert(open.contains("p-uic-overlay-panel"), "open: panel rides the Overlay primitive")
            assert(open.contains("p-contextmenu"), "open: Prime's contextmenu skin")
            assert(open.contains("p-contextmenu-root-list"), "open: root list")
            assert(open.contains("""role="menu""""), "open: menu role")
            assert(open.contains("p-contextmenu-item-link"), "open: item link anatomy")
            assert(open.contains("""data-uic-icon="copy""""), "open: item icon glyph")
            assert(open.contains("p-contextmenu-separator"), "open: separator row")
            assert(open.contains("""data-uic-icon="angle-right""""), "open: submenu glyph on the parent row")
            assert(open.contains("""data-kyo-focus-auto="1""""), "open: panel seeds focus")
            assert(open.contains("""data-kyo-stop="1""""), "open: panel consumes its own keydown (per-level Escape)")
            assert(open.contains("""tabindex="-1""""), "roving: item links out of the Tab order")
            assert(!open.contains("p-focus"), "open: no highlight before keyboard navigation")
            assert(!open.contains("p-uic-overlay-right-start"), "open: nested panel closed by default")
            assert(focused.contains("p-focus"), "highlight ref stamps Prime's .p-focus row")
            assert(focused.contains("""aria-activedescendant="cm1-active""""), "id wires activedescendant")
            assert(focused.contains("""id="cm1-active""""), "focused row carries the referenced id")
            assert(nested.contains("p-contextmenu-item-active"), "nested: open parent row stamps active")
            assert(nested.contains("p-contextmenu-submenu"), "nested: submenu panel class")
            assert(nested.contains("p-uic-overlay-right-start"), "nested: side anchor geometry")
            assert(
                (nested.split("""data-kyo-focus-auto="1"""").length - 1 == 1),
                "roving: only the root panel seeds focus, not the nested submenu"
            )
            assert(nested.contains("Email"), "nested: submenu items render")
        end for
    }

    "ToastService region stacks queued messages in one Prime toast region" in {
        import uic.ToastService.Queued
        def html(queued: Seq[Queued], pos: uic.OverlayPosition = uic.OverlayPosition.BottomRight): String < Async =
            renderHtml(uic.ToastService.body(queued, pos, _ => ()))
        for
            two <- html(
                Seq(
                    Queued(1, uic.ToastMessage(uic.Severity.Success, Present("Saved"), Present("Changes stored."), Present(3.seconds))),
                    Queued(2, uic.ToastMessage(uic.Severity.Danger, Present("Error"), Present("Request failed."), Absent, closable = true))
                )
            )
            sticky <- html(Seq(Queued(7, uic.ToastMessage(summary = Present("Sticky"), closable = false))))
            top    <- html(Seq(Queued(1, uic.ToastMessage())), uic.OverlayPosition.TopRight)
            empty  <- html(Nil)
        yield
            assert(two.contains("p-toast p-component"), "region root classes")
            assert(two.contains("p-toast-bottom-right"), "BottomRight is the service default position")
            assert(("p-toast-message-content".r.findAllIn(two).size == 2), "two queued messages stack in ONE region")
            assert(two.contains("p-toast-message-success"), "severity tint per message (success)")
            assert(two.contains("p-toast-message-error"), "severity tint per message (Danger -> error)")
            assert(two.contains("Saved"), "summary text")
            assert(two.contains("Request failed."), "detail text")
            assert(two.contains("""data-uic-life="3000""""), "life emitted as data-uic-life ms")
            assert(two.contains("""data-uic-toast-id="1""""), "service-assigned id on the message")
            assert(two.contains("p-toast-close-button"), "closable default: close button")
            assert(two.contains("""role="alert""""), "region announces itself")
            assert(!sticky.contains("p-toast-close-button"), "closable(false): no close button")
            assert(!sticky.contains("data-uic-life"), "no life: no auto-dismiss attr")
            assert(top.contains("p-toast-top-right"), "position token class")
            assert(!empty.contains("p-toast"), "empty queue renders nothing")
        end for
    }

    "ConfirmDialog rides the Dialog machinery with Prime's confirmdialog anatomy" in {
        def confirm(open: Boolean, f: uic.ConfirmDialog => uic.ConfirmDialog): String < Async =
            for
                ref <- Signal.initRef(open)
                out <- UI.runRender(
                    f(
                        uic.ConfirmDialog(ref)
                            .header("Delete file?")
                            .message("This cannot be undone.")
                            .icon(uic.Icons.exclamationTriangle)
                    )
                ).take(1).run
            yield out.mkString
        for
            open    <- confirm(true, identity)
            closed  <- confirm(false, identity)
            labeled <- confirm(true, _.acceptLabel("Delete").rejectLabel("Keep").acceptSeverity(uic.Severity.Danger))
        yield

            assert(open.contains("p-confirmdialog"), "open: confirmdialog skin on the dialog box")
            assert(open.contains("p-dialog"), "open: rides the Dialog anatomy")
            assert(open.contains("p-overlay-mask"), "open: modal mask")
            assert(open.contains("""role="alertdialog""""), "open: alertdialog role")
            assert(open.contains("p-confirmdialog-icon"), "open: leading icon slot")
            assert(open.contains("""data-uic-icon="exclamation-triangle""""), "open: icon glyph")
            assert(open.contains("p-confirmdialog-message"), "open: message span")
            assert(open.contains("This cannot be undone."), "open: message text")
            assert(open.contains("Delete file?"), "open: header text")
            assert(open.contains("p-dialog-footer"), "open: footer slot")
            assert(open.contains("p-confirmdialog-accept-button"), "open: accept button hook")
            assert(open.contains("p-confirmdialog-reject-button"), "open: reject button hook")
            assert(open.contains("Yes"), "accept default label")
            assert(open.contains("No"), "reject default label")
            assert(open.contains("p-button-text"), "reject is Prime's text variant")
            assert(open.contains("p-button-secondary"), "reject is secondary")
            assert(open.contains("""data-kyo-focus-auto="1""""), "open: box seeds focus (Escape = reject works immediately)")
            assert(open.contains("""data-kyo-focus-restore="1""""), "open: focus returns to the opener")
            assert(labeled.contains("Delete"), "acceptLabel")
            assert(labeled.contains("Keep"), "rejectLabel")
            assert(labeled.contains("p-button-danger"), "acceptSeverity renders the severity skin")
            assert(!closed.contains("p-confirmdialog"), "closed: nothing rendered")
        end for
    }

    "Theme carries the kyo remainder for contextmenu/toast service/confirmdialog" in {
        val extra = uic.Theme.primeExtraCss
        assert(extra.contains("li.p-contextmenu-item { flex-direction: column;"), "contextmenu row stacks content over the sub-panel")
        assert(extra.contains("a.p-contextmenu-item-link { flex-direction: row; }"), "contextmenu link row restorer")
        assert(
            extra.contains(".p-uic-overlay-panel.p-contextmenu-submenu { display: flex; flex-direction: column; margin: 0; }"),
            "contextmenu submenu panel restorer (flush, flex column)"
        )
        assert(extra.contains("span.p-contextmenu-submenu-icon"), "contextmenu submenu glyph sizing")
        assert(extra.contains(".p-confirmdialog .p-dialog-content { flex-direction: row; }"), "confirmdialog content row restorer")
        assert(extra.contains("span.p-confirmdialog-icon"), "confirmdialog icon glyph sizing")
        assert(uic.Theme.primeCss.contains(".p-contextmenu-root-list"), "extracted contextmenu sheet present")
        assert(uic.Theme.primeCss.contains(".p-confirmdialog-icon"), "extracted confirmdialog sheet present")
    }

    // ==== InputNumber / Password / InputOtp / Slider / Knob ====

    "InputNumber renders a real number field in Prime's wrapper + spin buttons per layout" in {
        for
            bound <-
                for
                    ref <- Signal.initRef(5.0)
                    out <- UI.runRender(uic.InputNumber().value(ref).min(0).max(10).step(0.5)).take(1).run
                yield out.mkString
            stacked    <- renderHtml(uic.InputNumber().value(3).showButtons(true))
            horizontal <- renderHtml(uic.InputNumber().value(3).showButtons(true).buttonLayout(uic.InputNumberButtonLayout.Horizontal))
            adorned    <- renderHtml(uic.InputNumber().value(42).prefix("$").suffix(" kg"))
            invalid    <- renderHtml(uic.InputNumber().value(1).invalid(true).invalidMessage("Out of range"))
            disabled   <- renderHtml(uic.InputNumber().value(1).showButtons(true).disabled(true))
            fluid      <- renderHtml(uic.InputNumber().value(1).fluid(true))
        yield

            assert(bound.contains("p-inputnumber"), "wrapper root class")
            assert(bound.contains("p-inputwrapper"), "inputwrapper hook (FloatLabel contract)")
            assert(bound.contains("p-inputwrapper-filled"), "bound value marks the wrapper filled")
            assert(bound.contains("p-inputnumber-input"), "field carries the inputnumber input class")
            assert(bound.contains("p-inputtext"), "field carries the shared text-field skin")
            assert(bound.contains("""type="number""""), "a REAL native number input")
            assert(bound.contains("""value="5""""), "ref value rendered (integral without .0)")
            assert(bound.contains("""min="0""""), "native min")
            assert(bound.contains("""max="10""""), "native max")
            assert(bound.contains("""step="0.5""""), "native step")
            assert(bound.contains("change"), "registers change for the numeric write-back")
            assert(!bound.contains("p-inputnumber-stacked"), "no layout class without showButtons")
            assert(stacked.contains("p-inputnumber-stacked"), "showButtons defaults to the stacked layout")
            assert(stacked.contains("p-inputnumber-button-group"), "stacked: buttons docked in the group")
            assert(stacked.contains("p-inputnumber-increment-button"), "increment button hook")
            assert(stacked.contains("p-inputnumber-decrement-button"), "decrement button hook")
            assert(stacked.contains("""data-uic-icon="angle-up""""), "increment glyph")
            assert(stacked.contains("""data-uic-icon="angle-down""""), "decrement glyph")
            assert(horizontal.contains("p-inputnumber-horizontal"), "horizontal layout class")
            assert(!horizontal.contains("p-inputnumber-button-group"), "horizontal: no stacked group")
            assert(adorned.contains("p-uic-inputnumber-prefix"), "static prefix adornment (documented deviation)")
            assert(adorned.contains("p-uic-inputnumber-suffix"), "static suffix adornment")
            assert(invalid.contains("p-invalid"), "invalid class")
            assert(invalid.contains("""aria-invalid="true""""), "aria-invalid")
            assert(invalid.contains("Out of range"), "invalid message row")
            assert(disabled.contains("disabled"), "disabled reaches the field and the buttons")
            assert(fluid.contains("p-inputnumber-fluid"), "fluid wrapper class")
    }

    "Password renders the masked field; wired seam toggles mask + inline strength meter" in {
        def wired(revealed: Boolean, f: uic.Password => uic.Password): String < Async =
            for
                rev <- Signal.initRef(revealed)
                out <- UI.runRender(f(uic.Password().toggleMask(true)).wired(rev)).take(1).run
            yield out.mkString
        for
            plain    <- renderHtml(uic.Password().value("secret1"))
            masked   <- wired(false, identity)
            revealed <- wired(true, identity)
            weak <-
                for
                    ref <- Signal.initRef("abc")
                    out <- UI.runRender(uic.Password().value(ref).feedback(true)).take(1).run
                yield out.mkString
            strong <-
                for
                    ref <- Signal.initRef("Str0ngPass")
                    out <- UI.runRender(uic.Password().value(ref).feedback(true)).take(1).run
                yield out.mkString
            prompt <-
                for
                    ref <- Signal.initRef("")
                    out <- UI.runRender(uic.Password().value(ref).feedback(true).promptLabel("Type one")).take(1).run
                yield out.mkString
        yield

            assert(plain.contains("p-password"), "wrapper root class")
            assert(plain.contains("p-inputwrapper"), "inputwrapper hook")
            assert(plain.contains("p-password-input"), "field carries the password input class")
            assert(plain.contains("""type="password""""), "masked native field")
            assert(!plain.contains("p-password-toggle-mask-icon"), "no eye toggle unless toggleMask")
            assert(masked.contains("p-password-toggle-mask-icon"), "toggleMask: eye affordance")
            assert(masked.contains("p-password-unmask-icon"), "masked state shows the unmask (eye) affordance")
            assert(masked.contains("""data-uic-icon="eye""""), "masked: eye glyph")
            assert(masked.contains("""type="password""""), "masked: password type")
            assert(revealed.contains("p-password-mask-icon"), "revealed state shows the mask (eye-slash) affordance")
            assert(revealed.contains("""data-uic-icon="eye-slash""""), "revealed: eye-slash glyph")
            assert(revealed.contains("""type="text""""), "revealed: text type")
            assert(weak.contains("p-password-content"), "feedback: meter block")
            assert(weak.contains("p-password-meter"), "feedback: meter track")
            assert(weak.contains("p-password-meter-label"), "feedback: meter fill")
            assert(weak.contains("p-password-meter-weak"), "3 plain chars rate weak (Prime's default rules)")
            assert(weak.contains("Weak"), "weak label text")
            assert(strong.contains("p-password-meter-strong"), "3 classes + length 8 rate strong")
            assert(strong.contains("Strong"), "strong label text")
            assert(prompt.contains("Type one"), "empty value shows the prompt label")
            assert(!plain.contains("p-password-meter"), "no meter unless feedback")
        end for
    }

    "InputOtp renders N one-char cells bound to one code; mask/integerOnly variants" in {
        for
            bound <-
                for
                    ref <- Signal.initRef("12")
                    out <- UI.runRender(uic.InputOtp().value(ref)).take(1).run
                yield out.mkString
            six     <- renderHtml(uic.InputOtp().value("123456").length(6))
            masked  <- renderHtml(uic.InputOtp().mask(true))
            invalid <- renderHtml(uic.InputOtp().invalid(true).invalidMessage("Wrong code"))
        yield

            assert(bound.contains("p-inputotp"), "root class")
            assert(("p-inputotp-input".r.findAllIn(bound).size == 4), "Prime default: 4 cells")
            assert(bound.contains("""value="1""""), "cell 0 shows the first character")
            assert(bound.contains("""value="2""""), "cell 1 shows the second character")
            assert(bound.contains("input"), "cells register the per-cell write-back")
            assert(bound.contains("OTP character 1"), "per-position accessible labels")
            assert(("p-inputotp-input".r.findAllIn(six).size == 6), "length(6) renders 6 cells")
            assert(six.contains("""value="6""""), "constant code fills the last cell")
            assert(masked.contains("""type="password""""), "mask: password-type cells")
            assert(invalid.contains("p-invalid"), "invalid cells")
            assert(invalid.contains("Wrong code"), "invalid message row")
    }

    "Slider renders Prime's track/range/handle driven by the value + the invisible native input" in {
        for
            bound <-
                for
                    ref <- Signal.initRef(30.0)
                    out <- UI.runRender(uic.Slider().value(ref)).take(1).run
                yield out.mkString
            vertical <- renderHtml(uic.Slider().value(40).orientation(uic.Orientation.Vertical))
            disabled <- renderHtml(uic.Slider().value(10).disabled(true))
            stepped  <- renderHtml(uic.Slider().value(2).min(0).max(4).step(2))
        yield

            assert(bound.contains("p-slider"), "root class")
            assert(bound.contains("p-slider-horizontal"), "horizontal is the default orientation")
            assert(bound.contains("p-slider-range"), "range fill element")
            assert(bound.contains("width: 30%"), "fill width computed from the bound value")
            assert(bound.contains("p-slider-handle"), "handle element")
            assert(bound.contains("left: 30%"), "handle position computed from the bound value")
            assert(bound.contains("p-uic-slider-native"), "invisible native range input overlays the track")
            assert(bound.contains("""type="range""""), "a REAL native range input")
            assert(bound.contains("change"), "registers change for the write-back")
            assert(vertical.contains("p-slider-vertical"), "vertical orientation class")
            assert(vertical.contains("height: 40%"), "vertical: fill height from the value")
            assert(vertical.contains("bottom: 40%"), "vertical: handle from the bottom")
            assert(disabled.contains("p-disabled"), "disabled root class")
            assert(disabled.contains("disabled"), "native input disabled")
            assert(stepped.contains("""step="2""""), "native step")
            assert(stepped.contains("width: 50%"), "fill percent respects min/max")
    }

    "Knob renders Prime's svg dial: arcs from the dial math, center text, slider semantics" in {
        for
            bound <-
                for
                    ref <- Signal.initRef(60.0)
                    out <- UI.runRender(uic.Knob().value(ref)).take(1).run
                yield out.mkString
            sized    <- renderHtml(uic.Knob().value(25).size(150).strokeWidth(8))
            noText   <- renderHtml(uic.Knob().value(10).showValue(false))
            template <- renderHtml(uic.Knob().value(10).valueTemplate(v => s"${v.toInt}%"))
            disabled <- renderHtml(uic.Knob().value(10).disabled(true))
        yield

            assert(bound.contains("p-knob"), "root class")
            assert(bound.contains("p-knob-range"), "background arc class")
            assert(bound.contains("p-knob-value"), "value arc class")
            assert(bound.contains("p-knob-text"), "center label class")
            assert(bound.contains(uic.Knob.rangePath), "background arc uses the full 240-degree dial path")
            assert(bound.contains(uic.Knob.valuePath(60.0, 0.0, 100.0)), "value arc path computed from the bound value")
            assert(bound.contains(">60</text>"), "center label shows the value")
            assert(bound.contains("""role="slider""""), "slider role on the dial")
            assert(bound.contains("""aria-valuenow="60""""), "aria-valuenow")
            assert(bound.contains("""tabindex="0""""), "focusable dial")
            assert(bound.contains("keydown"), "registers the keyboard interaction")
            assert(bound.contains("var(--p-knob-value-background)"), "value arc colored by the knob token")
            assert(sized.contains("""width="150""""), "size(px) sizes the svg")
            assert(sized.contains("""stroke-width="8""""), "strokeWidth reaches the arcs")
            assert(!noText.contains("p-knob-text"), "showValue(false) drops the label")
            assert(template.contains(">10%</text>"), "valueTemplate formats the label")
            assert(disabled.contains("p-disabled"), "disabled root class")
            assert(!disabled.contains("""tabindex="0""""), "disabled dial is unfocusable")
    }

    "Theme carries the kyo remainder for inputnumber/password/otp/slider/knob" in {
        val extra = uic.Theme.primeExtraCss
        assert(extra.contains(".p-password, .p-inputotp { flex-direction: row; }"), "password/otp row restorers")
        assert(extra.contains(".p-uic-inputnumber-prefix"), "inputnumber adornment extension")
        assert(extra.contains(".p-uic-password-feedback"), "inline password meter extension")
        assert(extra.contains("span.p-password-toggle-mask-icon"), "password toggle glyph sizing")
        assert(extra.contains(".p-uic-slider-native"), "slider native-input overlay extension")
        assert(extra.contains(".p-knob { display: inline-flex; }"), "knob root display")
        assert(uic.Theme.primeCss.contains(".p-inputnumber-button"), "extracted inputnumber sheet present")
        assert(uic.Theme.primeCss.contains(".p-password-meter"), "extracted password sheet present")
        assert(uic.Theme.primeCss.contains(".p-inputotp-input"), "extracted inputotp sheet present")
        assert(uic.Theme.primeCss.contains(".p-slider-handle"), "extracted slider sheet present")
        assert(uic.Theme.primeCss.contains(".p-knob-range"), "extracted knob sheet present")
    }

    // ---- the container/data block ----

    "Accordion renders Prime's panel anatomy; the ref TYPE picks single vs multiple mode" in {
        def panels(acc: uic.Accordion): uic.Accordion =
            acc.panels(
                uic.AccordionPanel("Header I", p("Content I"), "p1"),
                uic.AccordionPanel("Header II", p("Content II"), "p2", disabled = true)
            )
        for
            single <-
                for
                    ref <- Signal.initRef("p1")
                    out <- UI.runRender(panels(uic.Accordion().value(ref))).take(1).run
                yield out.mkString
            multi <-
                for
                    ref <- Signal.initRef(Set("p1", "p2"))
                    out <- UI.runRender(panels(uic.Accordion().value(ref))).take(1).run
                yield out.mkString
            static <- renderHtml(panels(uic.Accordion()))
        yield
            assert(single.contains("p-accordion"), "root class")
            assert(single.contains("p-accordionpanel"), "panel class")
            assert(single.contains("p-accordionpanel-active"), "bound open panel is active")
            assert(single.contains("p-accordionheader"), "header button class")
            assert(single.contains("<button"), "header is a real <button>")
            assert(single.contains("p-accordionheader-toggle-icon"), "toggle icon slot")
            assert(single.contains("""data-uic-icon="chevron-up""""), "active header shows chevron-up")
            assert(single.contains("""data-uic-icon="chevron-down""""), "collapsed header shows chevron-down")
            assert(single.contains("p-accordioncontent-wrapper"), "content wrapper anatomy")
            assert(single.contains("p-accordioncontent-content"), "content inner anatomy")
            assert(single.contains("Content I"), "active content rendered")
            assert(!single.contains("Content II"), "collapsed content not rendered")
            assert(single.contains("p-disabled"), "disabled panel class")
            assert(single.contains("""aria-expanded="true""""), "aria-expanded on the open header")
            assert(single.contains("click"), "headers register clicks")
            assert(multi.contains("Content I") && multi.contains("Content II"), "multiple mode opens both")
            assert(!static.contains("p-accordionpanel-active"), "no ref: fully collapsed")
            assert(!static.contains("click"), "no ref: inert headers")
        end for
    }

    "OrderList embeds a multiple Listbox beside Prime's four secondary move buttons" in {
        for
            (withSel, noSel, roving) <-
                for
                    items <- Signal.initRef(Seq("Bamboo Watch", "Black Watch", "Blue Band"))
                    sel   <- Signal.initRef(Set("Black Watch"))
                    empty <- Signal.initRef(Set.empty[String])
                    hi    <- Signal.initRef(1)
                    a     <- UI.runRender(uic.OrderList[String]().items(items)(identity).selected(sel)).take(1).run
                    b     <- UI.runRender(uic.OrderList[String]().items(items)(identity).selected(empty)).take(1).run
                    c <- UI.runRender(
                        uic.OrderList[String]().items(items)(identity).selected(sel)
                            .wired(Present(uic.ListReorder.Cursor(hi, "ol")))
                    ).take(1).run
                yield (a.mkString, b.mkString, c.mkString)
        yield
            assert(withSel.contains("p-orderlist"), "root class")
            assert(withSel.contains("p-orderlist-controls"), "controls rail")
            assert(withSel.contains("""data-uic-icon="angle-up""""), "move-up glyph")
            assert(withSel.contains("""data-uic-icon="angle-double-up""""), "move-top glyph")
            assert(withSel.contains("""data-uic-icon="angle-down""""), "move-down glyph")
            assert(withSel.contains("""data-uic-icon="angle-double-down""""), "move-bottom glyph")
            assert(withSel.contains("p-button-secondary"), "Prime's secondary move buttons")
            assert(withSel.contains("p-listbox"), "embedded listbox")
            assert(withSel.contains("p-listbox-option-selected"), "selection reaches the rows")
            assert(withSel.contains("""aria-multiselectable="true""""), "multiple selection mode")
            assert(noSel.contains("disabled"), "empty selection disables the move buttons")
            // The live tree comes from a mount, and a golden render shows a mount only as its
            // placeholder, so the roving shape goes through the `wired` seam.
            assert(roving.contains("""aria-activedescendant="ol-option-1""""), "the list announces the row it holds")
            assert(roving.contains("""id="ol-option-1""""), "which is a row that carries that id")
            assert(
                roving.sliding(12).count(_ == """tabindex="0"""") == 1,
                "one tab stop for the whole control: the list, not a row each"
            )
    }

    "PickList renders two Listbox columns, transfer controls, and reorder rails" in {
        for
            html <-
                for
                    src    <- Signal.initRef(Seq("San Francisco", "London"))
                    tgt    <- Signal.initRef(Seq("Paris"))
                    srcSel <- Signal.initRef(Set("London"))
                    tgtSel <- Signal.initRef(Set.empty[String])
                    out <- UI.runRender(
                        uic.PickList[String]().sourceItems(src)(identity).targetItems(tgt).sourceSelected(srcSel).targetSelected(tgtSel)
                    ).take(1).run
                yield out.mkString
            noRails <-
                for
                    src <- Signal.initRef(Seq("A"))
                    tgt <- Signal.initRef(Seq.empty[String])
                    out <- UI.runRender(
                        uic.PickList[String]().sourceItems(src)(
                            identity
                        ).targetItems(tgt).showSourceControls(false).showTargetControls(false)
                    ).take(1).run
                yield out.mkString
            roving <-
                for
                    src    <- Signal.initRef(Seq("San Francisco", "London"))
                    tgt    <- Signal.initRef(Seq("Paris"))
                    srcSel <- Signal.initRef(Set("London"))
                    tgtSel <- Signal.initRef(Set.empty[String])
                    srcHi  <- Signal.initRef(1)
                    tgtHi  <- Signal.initRef(-1)
                    out <- UI.runRender(
                        uic.PickList[String]().sourceItems(src)(identity).targetItems(tgt).sourceSelected(srcSel)
                            .targetSelected(tgtSel)
                            .wired(
                                Present(uic.ListReorder.Cursor(srcHi, "pl-src")),
                                Present(uic.ListReorder.Cursor(tgtHi, "pl-tgt"))
                            )
                    ).take(1).run
                yield out.mkString
        yield
            assert(html.contains("p-picklist"), "root class")
            assert(html.contains("p-picklist-source-controls"), "source reorder rail")
            assert(html.contains("p-picklist-target-controls"), "target reorder rail")
            assert(html.contains("p-picklist-transfer-controls"), "transfer rail")
            assert(html.contains("p-picklist-source-list-container"), "source column container")
            assert(html.contains("p-picklist-target-list-container"), "target column container")
            assert(html.contains("""data-uic-icon="angle-right""""), "move-to-target glyph")
            assert(html.contains("""data-uic-icon="angle-double-right""""), "move-all-to-target glyph")
            assert(html.contains("""data-uic-icon="angle-left""""), "move-to-source glyph")
            assert(html.contains("""data-uic-icon="angle-double-left""""), "move-all-to-source glyph")
            assert(html.contains("San Francisco") && html.contains("Paris"), "both columns render their items")
            assert(!noRails.contains("p-picklist-source-controls"), "showSourceControls(false) drops the rail")
            assert(!noRails.contains("p-picklist-target-controls"), "showTargetControls(false) drops the rail")
            // The live tree comes from a mount, and a golden render shows a mount only as its
            // placeholder, so the roving shape goes through the `wired` seam.
            assert(roving.contains("""aria-activedescendant="pl-src-option-1""""), "the source column announces its row")
            assert(!roving.contains("aria-activedescendant=\"pl-tgt"), "the column with no highlight announces none")
            assert(
                roving.sliding(12).count(_ == """tabindex="0"""") == 2,
                "one tab stop per column, and none for a row"
            )
    }

    "Carousel renders the visible window, secondary text nav buttons, and indicator dots" in {
        def carousel(ref: SignalRef[Int])(using Frame) =
            uic.Carousel[String]()
                .items(Seq("A", "B", "C", "D", "E"))(s => span(s))
                .numVisible(2)
                .numScroll(2)
                .page(ref)
        for
            page0 <-
                for
                    ref <- Signal.initRef(0)
                    out <- UI.runRender(carousel(ref)).take(1).run
                yield out.mkString
            page1 <-
                for
                    ref <- Signal.initRef(1)
                    out <- UI.runRender(carousel(ref)).take(1).run
                yield out.mkString
            vertical <-
                for
                    ref <- Signal.initRef(0)
                    out <- UI.runRender(
                        uic.Carousel[String]().items(Seq("A", "B"))(s => span(s)).page(ref).vertical(true)
                    ).take(1).run
                yield out.mkString
        yield
            assert(page0.contains("p-carousel"), "root class")
            assert(page0.contains("p-carousel-horizontal"), "horizontal default")
            assert(page0.contains("p-carousel-content-container"), "content container")
            assert(page0.contains("p-carousel-viewport"), "viewport")
            assert(page0.contains("p-carousel-item-list"), "item list")
            assert(page0.contains("p-carousel-item-active"), "visible items are active")
            assert(page0.contains("flex-basis: 50%"), "items sized 100/numVisible (Prime's inline sizing)")
            assert(page0.contains(">A</span>") && page0.contains(">B</span>"), "page 0 shows the first window")
            assert(!page0.contains(">C</span>"), "page 0 hides the rest (server-honest window)")
            assert(page1.contains(">C</span>") && page1.contains(">D</span>"), "page 1 shows the stepped window")
            assert(page0.contains("p-carousel-prev-button"), "prev button")
            assert(page0.contains("p-carousel-next-button"), "next button")
            assert(page0.contains("p-button-text"), "nav buttons are Prime's text variant")
            assert(page0.contains("p-button-secondary"), "nav buttons are secondary")
            assert(page0.contains("p-carousel-indicator-list"), "indicator list")
            assert(page0.contains("p-carousel-indicator-active"), "active indicator")
            assert(page0.contains("""aria-current="page""""), "active dot exposes aria-current")
            assert(vertical.contains("p-carousel-vertical"), "vertical modifier")
            assert(vertical.contains("""data-uic-icon="chevron-up""""), "vertical uses chevron-up/-down navigators")
            assert(vertical.contains("height: 300px"), "vertical viewport gets Prime's default 300px height")
        end for
    }

    "Galleria renders preview + caption + thumbnail strip bound to activeIndex" in {
        def items = Seq(
            uic.GalleriaItem(
                "/img/a.jpg",
                "Image A",
                thumbnailSrc = Present("/img/a-t.jpg"),
                title = Present("Title A"),
                subtitle = Present("Sub A")
            ),
            uic.GalleriaItem("/img/b.jpg", "Image B")
        )
        for
            html <-
                for
                    ref <- Signal.initRef(0)
                    out <- UI.runRender(
                        uic.Galleria().items(items*).activeIndex(ref).showItemNavigators(true).showIndicators(true)
                    ).take(1).run
                yield out.mkString
            bare <- renderHtml(uic.Galleria().items(items*))
        yield
            assert(html.contains("p-galleria"), "root class")
            assert(html.contains("p-galleria-content"), "content wrapper")
            assert(html.contains("p-galleria-items-container"), "items container")
            assert(html.contains("p-galleria-item"), "item stage")
            assert(html.contains("""src="/img/a.jpg""""), "active image renders")
            assert(html.contains("p-galleria-caption"), "caption block from title/subtitle")
            assert(html.contains("Title A") && html.contains("Sub A"), "caption texts")
            assert(html.contains("p-galleria-prev-button") && html.contains("p-galleria-next-button"), "item navigators (enabled)")
            assert(html.contains("p-galleria-nav-button"), "nav button base class")
            assert(html.contains("p-galleria-indicator-list"), "indicators (enabled)")
            assert(html.contains("p-galleria-thumbnails"), "thumbnail strip (default on)")
            assert(html.contains("p-galleria-thumbnails-viewport"), "thumbnail viewport")
            assert(html.contains("p-galleria-thumbnail-item-current"), "current thumbnail marked")
            assert(html.contains("""src="/img/a-t.jpg""""), "thumbnailSrc used for the strip")
            assert(html.contains("p-galleria-thumbnail-nav-button"), "thumbnail navigators")
            assert(!bare.contains("p-galleria-prev-button"), "item navigators default OFF (Prime)")
            assert(!bare.contains("p-galleria-indicator-list"), "indicators default OFF (Prime)")
        end for
    }

    "FileUpload (basic) renders the label-for choose button over the hidden native input" in {
        for
            html <- renderHtml(
                uic.FileUpload().inputId("up1").accept(FileAccept.Extension(".txt")).onSelect(_ => ())
            )
            disabled <- renderHtml(uic.FileUpload().inputId("up2").disabled(true))
        yield
            assert(html.contains("p-fileupload"), "root class")
            assert(html.contains("p-fileupload-basic"), "basic mode class")
            assert(html.contains("p-fileupload-basic-content"), "content row")
            assert(html.contains("p-fileupload-choose-button"), "choose button class")
            assert(html.contains("<label"), "choose affordance is a real <label> (native activation)")
            assert(html.contains("""for="up1""""), "label targets the input id")
            assert(html.contains("p-button"), "label wears Prime's Button classes")
            assert(html.contains("""data-uic-icon="plus""""), "Prime's plus choose icon")
            assert(html.contains("Choose"), "default choose label")
            assert(html.contains("p-fileupload-filelabel"), "file label span")
            assert(html.contains("No file chosen"), "default file label text")
            assert(html.contains("""type="file""""), "real native file input")
            assert(html.contains("""id="up1""""), "input carries the id")
            assert(html.contains(""".txt"""), "accept filter rendered")
            assert(html.contains("change"), "onSelect registers the change event")
            assert(disabled.contains("p-disabled"), "disabled dims the choose label")
            assert(disabled.contains("disabled"), "disabled locks the input")
    }

    "TreeTable renders grouped header/body rows, depth-indented togglers, selection + sort" in {
        final case class F(name: String, size: String)
        def nodes = Seq(
            uic.TreeTableNode(F("Applications", "200mb"), List(uic.TreeTableNode(F("Scala", "25mb")))),
            uic.TreeTableNode(F("Cloud", "20mb"))
        )
        def cols = Seq(
            uic.Column[F]("Name")(_.name).sortBy(_.name),
            uic.Column[F]("Size")(_.size)
        )
        for
            expanded <-
                for
                    exp  <- Signal.initRef(Set("Applications"))
                    sel  <- Signal.initRef(Set("Scala"))
                    sort <- Signal.initRef(List.empty[uic.SortKey])
                    out <- UI.runRender(
                        uic.TreeTable[F]()
                            .nodes(nodes*)
                            .columns(cols*)
                            .rowKey(_.name)
                            .expanded(exp)
                            .selected(sel)
                            .sort(sort)
                            .selectionMode(uic.SelectionMode.Single)
                    ).take(1).run
                yield out.mkString
            collapsed <-
                for
                    exp <- Signal.initRef(Set.empty[String])
                    out <- UI.runRender(uic.TreeTable[F]().nodes(nodes*).columns(cols*).rowKey(_.name).expanded(exp)).take(1).run
                yield out.mkString
        yield
            assert(expanded.contains("p-treetable"), "root class")
            assert(expanded.contains("p-treetable-table-container"), "table container")
            assert(expanded.contains("p-treetable-table"), "table class")
            assert(expanded.contains("""role="treegrid""""), "treegrid role")
            assert(groupTag("thead", "p-treetable-thead").findFirstIn(expanded).isDefined, "header rows sit in a real thead")
            assert(expanded.contains("p-treetable-header-cell"), "header cell class")
            assert(expanded.contains("p-treetable-column-title"), "column title span")
            assert(expanded.contains("p-treetable-sortable-column"), "sortable header")
            assert(expanded.contains("""data-uic-icon="sort-alt""""), "unsorted icon")
            assert(groupTag("tbody", "p-treetable-tbody").findFirstIn(expanded).isDefined, "body rows sit in a real tbody")
            assert(expanded.contains("p-treetable-body-cell-content"), "cell content wrapper")
            assert(expanded.contains("p-treetable-node-toggle-button"), "toggler button")
            assert(expanded.contains("""data-uic-icon="chevron-down""""), "expanded toggler chevron")
            assert(expanded.contains("margin: 0 0 0 calc(1rem)"), "child toggler indented one level")
            assert(expanded.contains("p-uic-tt-toggle-hidden"), "leaf togglers hidden but present")
            assert(expanded.contains("Scala"), "expanded child row rendered")
            assert(expanded.contains("p-treetable-row-selected"), "selected row class")
            assert(expanded.contains("p-treetable-selectable-row"), "selectable rows")
            assert(expanded.contains("p-treetable-hoverable"), "hoverable with a selection mode")
            assert(expanded.contains("""aria-level="2""""), "child row aria-level")
            assert(!collapsed.contains("Scala"), "collapsed children not rendered")
        end for
    }

    "OrganizationChart renders nested tables with node boxes, connector rows, and toggles" in {
        def chart = uic.OrgChartNode(
            "CEO",
            "ceo",
            children = List(
                uic.OrgChartNode("CFO", "cfo"),
                uic.OrgChartNode("CTO", "cto", children = List(uic.OrgChartNode("Dev", "dev")))
            )
        )
        // Expansion is keyed like Tree/TreeTable: the set holds the EXPANDED ids.
        for
            expanded <-
                for
                    exp <- Signal.initRef(Set("ceo", "cto"))
                    sel <- Signal.initRef(Set("cto"))
                    out <- UI.runRender(
                        uic.OrganizationChart()
                            .node(chart)
                            .selectionMode(uic.SelectionMode.Single)
                            .expanded(exp)
                            .selected(sel)
                    ).take(1).run
                yield out.mkString
            collapsed <-
                for
                    exp <- Signal.initRef(Set.empty[String])
                    out <- UI.runRender(uic.OrganizationChart().node(chart).expanded(exp)).take(1).run
                yield out.mkString
            // No bound ref means no toggles at all — Prime's collapsible=false, without a
            // separate flag to discover.
            static <- renderHtml(uic.OrganizationChart().node(chart))
        yield
            assert(expanded.contains("p-organizationchart"), "root class")
            assert(expanded.contains("p-organizationchart-table"), "table-based layout")
            assert(expanded.contains("<tbody"), "every nested table puts its rows in a real row group")
            assert(expanded.contains("p-organizationchart-node"), "node box class")
            assert(expanded.contains("p-organizationchart-node-selectable"), "selectable node class")
            assert(expanded.contains("p-organizationchart-node-selected"), "selected node class")
            assert(expanded.contains("p-organizationchart-node-toggle-button"), "toggle anchor")
            assert(expanded.contains("<a "), "toggle is an anchor like Prime")
            assert(expanded.contains("p-organizationchart-connector-down"), "vertical connector line")
            assert(expanded.contains("p-organizationchart-connectors"), "connector rows")
            assert(expanded.contains("p-organizationchart-connector-left"), "left line cells")
            assert(expanded.contains("p-organizationchart-connector-right"), "right line cells")
            assert(expanded.contains("p-organizationchart-connector-top"), "top border on inner line cells")
            assert(expanded.contains("p-organizationchart-node-children"), "children row")
            assert(expanded.contains("""colspan="4""""), "node cell spans 2×children")
            assert(expanded.contains("""data-uic-icon="chevron-down""""), "expanded toggle chevron-down")
            assert(!expanded.contains("p-uic-oc-hidden"), "expanded: no hidden rows")
            assert(collapsed.contains("p-uic-oc-hidden"), "collapsed subtree hides via visibility class")
            assert(collapsed.contains("CFO"), "collapsed children stay in the DOM (Prime's visibility model)")
            assert(collapsed.contains("""data-uic-icon="chevron-up""""), "collapsed toggle chevron-up")
            assert(static.contains("p-organizationchart"), "an unbound chart still renders")
            assert(!static.contains("p-organizationchart-node-toggle-button"), "no ref, no toggles")
            // A static chart has no toggle to open a subtree with, so it shows the whole
            // hierarchy. Keying expansion by the EXPANDED ids makes the unbound default the
            // dangerous direction: an empty set would keep every descendant in the DOM but
            // visibility-hidden, leaving the root alone on screen with no way to open it.
            assert(static.contains("CFO") && static.contains("Dev"), "unbound: descendants in the DOM")
            assert(!static.contains("p-uic-oc-hidden"), "unbound: nothing hidden, the hierarchy is visible")
        end for
    }

    "Terminal renders welcome, history rows, and the bare prompt input wired to the handler" in {
        for
            html <-
                for
                    cmds <- Signal.initRef(Seq(uic.TerminalCommand("date", "2026-07-17")))
                    out <- UI.runRender(
                        uic.Terminal()
                            .welcomeMessage("Welcome to PrimeOne")
                            .prompt("kyo $")
                            .commands(cmds)
                            .commandHandler(cmd => s"Unknown command: $cmd")
                    ).take(1).run
                yield out.mkString
            inert <- renderHtml(uic.Terminal().welcomeMessage("W"))
        yield
            assert(html.contains("p-terminal"), "root class")
            assert(html.contains("p-terminal-welcome-message"), "welcome message")
            assert(html.contains("Welcome to PrimeOne"), "welcome text")
            assert(html.contains("p-terminal-command-list"), "history list")
            assert(html.contains("p-terminal-command"), "history row")
            assert(html.contains("p-terminal-prompt-label"), "prompt label")
            assert(html.contains("kyo $"), "custom prompt text")
            assert(html.contains("p-terminal-command-value"), "command value span")
            assert(html.contains(">date</span>"), "history command text")
            assert(html.contains("p-terminal-command-response"), "response block")
            assert(html.contains("2026-07-17"), "response text")
            assert(html.contains("p-terminal-prompt"), "prompt row")
            assert(html.contains("p-terminal-prompt-value"), "bare prompt input")
            assert(html.contains("change"), "handler registers the change commit")
            assert(!inert.contains("change"), "no handler/history: input inert")
    }

    "Theme carries the kyo remainder for orderlist/picklist/carousel/galleria/fileupload/treetable/orgchart/terminal" in {
        val extra = uic.Theme.primeExtraCss
        assert(
            extra.contains(".p-orderlist, .p-picklist, .p-fileupload-basic-content, .p-terminal-prompt,"),
            "orderlist/picklist/fileupload/terminal row restorers"
        )
        assert(extra.contains(".p-terminal-command { display: block; }"), "terminal history rows block")
        assert(extra.contains(".p-carousel-item { display: block; }"), "carousel item block")
        assert(extra.contains("label.p-button { flex-direction: row; }"), "label-as-button row restorer")
        assert(
            extra.contains(".p-treetable-tbody > tr.p-treetable-empty-message > td"),
            "treetable empty-message row (the sheet has no rules for the template slot)"
        )
        assert(extra.contains(".p-uic-tt-toggle-hidden"), "leaf toggler visibility class")
        assert(extra.contains(".p-uic-oc-hidden"), "orgchart collapsed visibility class")
        assert(!extra.contains(".p-organizationchart-table td"), "orgchart cell re-scope retired (real tbody)")
        assert(
            uic.Theme.primeCss.contains(".p-organizationchart-table > tbody > tr > td"),
            "generated orgchart cell rule reachable through the real tbody"
        )
        assert(extra.contains(".p-treetable-table-container { overflow: auto; }"), "treetable inlineStyles expressed")
        assert(uic.Theme.primeCss.contains(".p-accordionheader"), "extracted accordion sheet present")
        assert(uic.Theme.primeCss.contains(".p-orderlist-controls"), "extracted orderlist sheet present")
        assert(uic.Theme.primeCss.contains(".p-picklist-list-container"), "extracted picklist sheet present")
        assert(uic.Theme.primeCss.contains(".p-carousel-indicator-button"), "extracted carousel sheet present")
        assert(uic.Theme.primeCss.contains(".p-galleria-thumbnail-item"), "extracted galleria sheet present")
        assert(uic.Theme.primeCss.contains(".p-fileupload-basic-content"), "extracted fileupload sheet present")
        assert(uic.Theme.primeCss.contains(".p-treetable-node-toggle-button"), "extracted treetable sheet present")
        assert(uic.Theme.primeCss.contains(".p-organizationchart-node"), "extracted orgchart sheet present")
        assert(uic.Theme.primeCss.contains(".p-terminal-prompt-value"), "extracted terminal sheet present")
    }

    // ==== ColorPicker / InputMask / VirtualScroller (+ InputOtp retrofit) ====

    "ColorPicker (inline) renders Prime's plane + hue anatomy, hue-tinted, pointer-wired" in {
        for
            inline <-
                for
                    ref <- Signal.initRef("#ff0000")
                    out <- UI.runRender(uic.ColorPicker(ref).inline(true)).take(1).run
                yield out.mkString
            overlayStat <- renderHtml(uic.ColorPicker().value("#00ff00"))
            disabled    <- renderHtml(uic.ColorPicker().value("#0000ff").inline(true).disabled(true))
        yield

            assert(inline.contains("p-colorpicker"), "root class")
            assert(inline.contains("p-colorpicker-panel-inline"), "inline panel variant")
            assert(inline.contains("p-colorpicker-color-selector"), "2D selector")
            assert(inline.contains("p-colorpicker-color-background"), "saturation/brightness plane")
            assert(inline.contains("p-colorpicker-color-handle"), "plane handle")
            assert(inline.contains("p-colorpicker-hue"), "hue bar")
            assert(inline.contains("p-colorpicker-hue-handle"), "hue handle")
            assert(inline.contains("pointerdown"), "plane/hue register pointerdown")
            assert(inline.contains("pointermove"), "plane/hue register pointermove")
            // red (#ff0000) → saturation 1, brightness 1: handle at left 100%, top 0%
            assert(inline.contains("left: 100%"), "saturation handle at full for pure red")
            // overlay mode: a preview swatch trigger renders (mounted region → placeholder is the closed swatch)
            assert(overlayStat.contains("p-colorpicker-preview"), "overlay preview swatch")
            assert(disabled.contains("p-disabled"), "disabled root")
            assert(!disabled.contains("pointerdown"), "disabled drops pointer wiring")
    }

    "ColorPicker HSB↔hex math is exact on the primaries and round-trips" in {
        assert(uic.ColorPicker.hexOf(0, 1, 1) == "#ff0000", "hue 0 = red")
        assert(uic.ColorPicker.hexOf(120, 1, 1) == "#00ff00", "hue 120 = green")
        assert(uic.ColorPicker.hexOf(240, 1, 1) == "#0000ff", "hue 240 = blue")
        assert(uic.ColorPicker.hexOf(0, 0, 1) == "#ffffff", "saturation 0, value 1 = white")
        assert(uic.ColorPicker.hexOf(0, 0, 0) == "#000000", "value 0 = black")
        assert(uic.ColorPicker.normalizeHex("f00") == "#ff0000", "3-digit hex expands")
        assert(uic.ColorPicker.normalizeHex("3B82F6") == "#3b82f6", "no-# uppercase normalizes")
        val (h, s, v) = uic.ColorPicker.hsvOf("#00ff00")
        assert(h == 120.0 && s == 1.0 && v == 1.0, "green parses back to hue 120")
    }

    "InputMask wraps InputText and carries the client-local mask token" in {
        for
            phone <-
                for
                    ref <- Signal.initRef("")
                    out <- UI.runRender(uic.InputMask("(999) 999-9999").value(ref).placeholder("(999) 999-9999")).take(1).run
                yield out.mkString
            ssn <- renderHtml(uic.InputMask("999-99-9999").value("123-45-6789").invalid(true).invalidMessage("bad"))
        yield

            assert(phone.contains("p-inputtext"), "IS an InputText")
            assert(phone.contains("p-inputmask"), "carries Prime's p-inputmask class")
            assert(phone.contains("""data-kyo-mask="(999) 999-9999""""), "emits the mask token")
            assert(phone.contains("change"), "no explicit change wiring, but two-way ref registers change")
            assert(ssn.contains("""data-kyo-mask="999-99-9999""""), "ssn mask token")
            assert(ssn.contains("""value="123-45-6789""""), "constant value")
            assert(ssn.contains("p-invalid"), "invalid model inherited")
            assert(ssn.contains("bad"), "invalid message inherited")
    }

    "InputOtp integerOnly now carries the client-local inputFilter (retrofit)" in {
        for
            intOnly <- renderHtml(uic.InputOtp().integerOnly(true))
            plain   <- renderHtml(uic.InputOtp())
        yield
            // Wire token is "digits": the component's own `inputFilter("int")` vocabulary is translated by
            // Input.asInputFilter onto kyo-ui's InputFilter.Digits, which serializes as "digits". The old
            // expectation predates that enum and asserted the pre-upstream fork token.
            assert(("""data-kyo-filter="digits"""".r.findAllIn(intOnly).size == 4), "every cell filters to digits")
            assert(!plain.contains("data-kyo-filter"), "default OTP has no filter")
    }

    "VirtualScroller windows a 10k list to a handful of rows; absolute rows in a full-height spacer; native-scroll-wired" in {
        val items = (0 until 10000).toList
        def vs    = uic.VirtualScroller(items).itemSize(40).height(200)(i => uic.Text()(s"row $i"))
        // Static placeholder (mounted region) = the window at scroll 0.
        for
            stat <- renderHtml(vs)
            // The wired seam carries the native scroll-position handler.
            wired <-
                for
                    r   <- Signal.initRef(0.0)
                    out <- UI.runRender(vs.wired(r)).take(1).run
                yield out.mkString
        yield

            assert(stat.contains("p-virtualscroller"), "viewport class")
            assert(stat.contains("p-uic-vs-viewport"), "native-scroll viewport class")
            assert(stat.contains("p-virtualscroller-spacer"), "scroll-extent spacer (Prime anatomy)")
            assert(stat.contains("p-uic-vs-item"), "row wrapper class")
            assert(stat.contains(">row 0<"), "first row rendered")
            assert(stat.contains(">row 8<"), "row within the window rendered")
            assert(!stat.contains(">row 50<"), "far rows NOT rendered (windowed)")
            assert(!stat.contains(">row 9999<"), "tail NOT rendered")
            // At scroll 0: firstIdx = max(0, 0 - overscan) = 0, lastIdx = 0 + ceil(200/40) + 2·overscan + 1 = 5 + 6 + 1 = 12.
            assert(("p-uic-vs-item".r.findAllIn(stat).size == 12), "exactly ceil(200/40)+2·overscan+1 = 12 rows")
            assert(stat.contains("""data-uic-vs-first="0""""), "window start attr")
            assert(stat.contains("""data-uic-vs-count="12""""), "window size attr")
            assert(wired.contains("""data-kyo-ev="scroll""""), "wired viewport registers the native scroll handler")
        end for
    }

    // The same anatomy over a RowSource, where no sequence is held: what arrived is drawn
    // at the offset the source published, and what has not is a slot of the row's height.
    "VirtualScroller over a source draws what arrived and leaves a Skeleton slot for what has not" in {
        val vs = uic.VirtualScroller(Seq.empty[Int]).itemSize(40).height(200).overscan(0)(i => uic.Text()(s"row $i"))
        for
            out <- renderHtml(vs.sourceWindow(0.0, uic.RowSource.Window(1, Seq(7, 8)), uic.Total.Known(50)))
        yield
            assert(out.contains("p-virtualscroller-spacer"), "Prime's scroll-extent spacer")
            assert(out.contains("2000px"), "fifty rows of forty, whether or not they are loaded")
            assert(out.contains(">row 7<") && out.contains(">row 8<"), "the rows that arrived")
            assert(out.contains("p-skeleton"), "and a slot where the source has not reached")
            assert(("p-uic-vs-item".r.findAllIn(out).size == 6), "five rows fit the viewport, plus the reach")
            assert(out.contains("""data-uic-vs-first="0""""), "window start attr")
            assert(out.contains("""data-uic-vs-count="6""""), "window size attr")
        end for
    }

    "enter/leave transitions: Dialog/Toast/Overlay/Message carry both, driven by Prime's keyframe classes" in {
        for
            dialog <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(uic.Dialog().open(ref).header("Hi")(p("x"))).take(1).run
                yield out.mkString
            toast <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(uic.Toast().open(ref).summary("Saved")).take(1).run
                yield out.mkString
            overlay <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(uic.Overlay(ref)(span("panel"))).take(1).run
                yield out.mkString
            overlayOff <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(uic.Overlay(ref).animate(false)(span("panel"))).take(1).run
                yield out.mkString
            message <- renderHtml(uic.Message().severity(uic.Severity.Success)(span("done")))
        yield

            // Dialog: box scale-enter, mask fade-enter + Prime's mask-leave keyframe
            assert(dialog.contains("""data-kyo-enter="p-uic-enter-scale""""), "dialog box scale enter")
            assert(dialog.contains("""data-kyo-enter="p-uic-enter-fade""""), "dialog mask fade enter")
            assert(dialog.contains("""data-kyo-leave="p-overlay-mask-leave-active""""), "dialog mask uses Prime's leave keyframe")
            assert(toast.contains("""data-kyo-leave="p-toast-message-leave-active""""), "toast uses Prime's leave keyframe")
            assert(overlay.contains("""data-kyo-enter="p-uic-enter-fade""""), "overlay panel fade enter")
            assert(
                overlay.contains("""data-kyo-leave="p-anchored-overlay-leave-active""""),
                "overlay panel uses Prime's anchored leave keyframe"
            )
            assert(!overlayOff.contains("data-kyo-enter"), "animate(false) drops the enter wiring")
            assert(!overlayOff.contains("data-kyo-leave"), "animate(false) drops the leave wiring")
            assert(message.contains("""data-kyo-leave="p-message-leave-active""""), "message uses Prime's leave keyframe")
            assert(uic.Theme.primeExtraCss.contains(".p-uic-enter-scale { opacity: 0; transform: scale(0.92); }"), "scale enter from-state")
            assert(
                uic.Theme.primeExtraCss.contains(".p-overlay-mask { transition: opacity 150ms ease; }"),
                "mask carries the enter transition"
            )
    }

    "Theme carries the kyo remainder + the colorpicker/virtualscroller sheets" in {
        val extra = uic.Theme.primeExtraCss
        assert(extra.contains(".p-colorpicker-color-background { pointer-events: none; }"), "surface/handle pointer-events off")
        assert(
            extra.contains(".p-colorpicker-hue > span[data-kyo-reactive] { pointer-events: none; display: contents; }"),
            "reactive wrapper pointer-transparent"
        )
        assert(extra.contains(".p-uic-overlay-panel .p-colorpicker-panel { position: static; }"), "overlay-panel skin reset")
        assert(extra.contains(".p-uic-vs-viewport { overflow: auto; overscroll-behavior: contain; }"), "vs native-scroll viewport")
        assert(uic.Theme.primeCss.contains(".p-colorpicker-color-background"), "extracted colorpicker sheet present")
        assert(uic.Theme.primeCss.contains(".p-virtualscroller-loader"), "extracted virtualscroller sheet present")
    }

    // ==== flip/shift, OTP auto-advance, auto-scroll, drag, metadata ====

    "Overlay.flipAnchor flips the vertical side only on the overflowing edge; shiftFor keeps it inside" in {
        // rect overflowing the bottom edge → Bottom* flips to Top*
        val overBottom = UI.Rect(x = 10, y = 500, width = 100, height = 200, viewportWidth = 1000, viewportHeight = 600)
        val fits       = UI.Rect(x = 10, y = 50, width = 100, height = 200, viewportWidth = 1000, viewportHeight = 600)
        val overTop    = UI.Rect(x = 10, y = -5, width = 100, height = 200, viewportWidth = 1000, viewportHeight = 600)
        val overRight  = UI.Rect(x = 950, y = 50, width = 100, height = 200, viewportWidth = 1000, viewportHeight = 600)
        val overLeft   = UI.Rect(x = -10, y = 50, width = 100, height = 200, viewportWidth = 1000, viewportHeight = 600)
        assert(
            (uic.Overlay.flipAnchor(uic.OverlayAnchor.BottomStart, overBottom) == uic.OverlayAnchor.TopStart),
            "BottomStart→TopStart when it overflows the bottom"
        )
        assert((uic.Overlay.flipAnchor(uic.OverlayAnchor.BottomEnd, overBottom) == uic.OverlayAnchor.TopEnd), "BottomEnd→TopEnd")
        assert((uic.Overlay.flipAnchor(uic.OverlayAnchor.BottomStart, fits) == uic.OverlayAnchor.BottomStart), "no flip when it fits")
        assert(
            (uic.Overlay.flipAnchor(uic.OverlayAnchor.TopStart, overTop) == uic.OverlayAnchor.BottomStart),
            "TopStart→BottomStart when it overflows the top"
        )
        assert((uic.Overlay.shiftFor(overRight) < 0.0), "right overflow shifts the panel left")
        assert((uic.Overlay.shiftFor(overLeft) > 0.0), "left overflow shifts the panel right")
        assert((uic.Overlay.shiftFor(fits) == 0.0), "no shift when it fits")
    }

    "Overlay.geometryFor resolves flip + shift into ONE in-place Style: both vertical edges always set, x-overflow → negative margin-left" in {
        // geometryFor is patched onto the panel via bindStyleById (which MERGES, never
        // clears), so it must set BOTH top and bottom every time — assert the serialized form.
        def geom(rect: UI.Rect): String < Async =
            renderHtml(div.style(uic.Overlay.geometryFor(uic.OverlayAnchor.BottomStart, rect)))
        val overBottom = UI.Rect(x = 10, y = 500, width = 100, height = 200, viewportWidth = 1000, viewportHeight = 600)
        val fits       = UI.Rect(x = 10, y = 50, width = 100, height = 200, viewportWidth = 1000, viewportHeight = 600)
        val overRight  = UI.Rect(x = 950, y = 50, width = 100, height = 200, viewportWidth = 1000, viewportHeight = 600)
        for
            flipped <- geom(overBottom)
            below   <- geom(fits)
            shifted <- geom(overRight)
        yield
            assert(flipped.contains("bottom: 100%"), "bottom overflow flips up: bottom:100%")
            assert(flipped.contains("top: auto"), "bottom overflow flips up: top:auto (leaving edge reset)")
            assert(below.contains("top: 100%"), "fits: stays below with top:100%")
            assert(below.contains("bottom: auto"), "fits: bottom:auto (leaving edge reset)")
            assert(below.contains("margin: 0 0 0 0"), "no x-overflow: zero margin-left shift")
            assert(shifted.contains("margin: 0 0 0 -"), "x-overflow: negative margin-left pulls it inside")
        end for
    }

    "Overlay(autoFlip default true) open still renders the anchored backdrop + panel placeholder" in {
        def overlay(flip: Boolean): String < Async =
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.Overlay(ref).autoFlip(flip)(span("panel-content"))).take(1).run
            yield out.mkString
        for
            on  <- overlay(true)
            off <- overlay(false)
        yield
            assert(on.contains("p-uic-overlay-backdrop"), "autoFlip on: backdrop present (mounted placeholder = declared anchor)")
            assert(on.contains("p-uic-overlay-panel"), "autoFlip on: panel present")
            assert(on.contains("p-uic-overlay-bottom-start"), "autoFlip on: declared anchor geometry in the placeholder")
            assert(off.contains("p-uic-overlay-panel"), "autoFlip off: panel present")
            assert(off.contains("panel-content"), "autoFlip off: children rendered")
        end for
    }

    "InputOtp.wired stamps a self id on every cell (auto-advance target) and registers input" in {
        for
            html <-
                for
                    ref <- Signal.initRef("12")
                    out <- UI.runRender(uic.InputOtp().value(ref).wired(ref, List("otp0", "otp1", "otp2", "otp3"), _ => ())).take(1).run
                yield out.mkString
        yield
            assert(html.contains("""id="otp0""""), "cell 0 carries its self id")
            assert(html.contains("""id="otp1""""), "cell 1 carries its self id")
            assert(html.contains("""id="otp3""""), "last cell carries its self id")
            assert(html.contains("input"), "cells still register the write-back that triggers the advance")
    }

    "Terminal.wired stamps the newest history row so it can be scrolled into view" in {
        for
            html <-
                for
                    ref <- Signal.initRef(Seq(uic.TerminalCommand("a", "1"), uic.TerminalCommand("b", "2")))
                    out <- UI.runRender(
                        uic.Terminal().commands(ref).commandHandler(_ => "ok").wired(ref, "lastrow", _ => ())
                    ).take(1).run
                yield out.mkString
        yield
            assert(("""id="lastrow"""".r.findAllIn(html).size == 1), "exactly the newest row carries the scroll id")
            assert(html.contains("p-terminal-command"), "history rows rendered")
    }

    "Carousel.wired renders ALL items in the id-stamped stable strip and wires swipe on it" in {
        for
            html <-
                for
                    ref   <- Signal.initRef(0)
                    start <- Signal.initRef(0.0)
                    out <- UI.runRender(
                        uic.Carousel[String]().items(Seq("A", "B", "C"))(s => span(s)).page(ref).wired(ref, "strip", start)
                    ).take(1).run
                yield out.mkString
        yield
            assert(html.contains("""id="strip""""), "the stable strip carries the bindStyle id")
            assert(html.contains("p-uic-carousel-track"), "the strip carries the transition class")
            assert(
                html.contains(">A</span>") && html.contains(">B</span>") && html.contains(">C</span>"),
                "the stable strip renders ALL items (not just the window)"
            )
            assert(html.contains("pointerdown"), "swipe registers pointerdown on the strip")
            assert(html.contains("pointerup"), "swipe registers pointerup on the strip")
    }

    "Carousel.translateFraction slides the strip by the clamped window start" in {
        assert((uic.Carousel.translateFraction(0, 1, 1, 5) == 0.0), "page 0 -> 0%")
        assert((uic.Carousel.translateFraction(1, 1, 1, 5) == -20.0), "page 1 (1 scroll / 1 visible over 5) -> -20%")
        assert((uic.Carousel.translateFraction(10, 1, 1, 5) == -80.0), "over-scrolled page clamps to the last window -> -80%")
        assert((uic.Carousel.translateFraction(1, 3, 3, 6) == -50.0), "page 1 (3 scroll / 3 visible over 6) -> -50%")
        assert((uic.Carousel.translateFraction(0, 1, 1, 0) == 0.0), "empty strip -> 0%")
    }

    "Knob.valueFromPointer maps the dial geometry back to a value (inverse of the arc math)" in {
        val vMin   = uic.Knob.valueFromPointer(30.0, 85.0, 100.0, 100.0, 0.0, 100.0, 1.0)  // lower-left ≈ min
        val vTop   = uic.Knob.valueFromPointer(50.0, 10.0, 100.0, 100.0, 0.0, 100.0, 1.0)  // top ≈ mid
        val vRight = uic.Knob.valueFromPointer(84.6, 70.0, 100.0, 100.0, 0.0, 100.0, 1.0)  // right ≈ high
        val vSnap  = uic.Knob.valueFromPointer(50.0, 10.0, 100.0, 100.0, 0.0, 100.0, 10.0) // snapped to step 10
        assert((math.abs(vMin - 0.0) < 2.0), s"lower-left maps near min (got $vMin)")
        assert((math.abs(vTop - 50.0) < 2.0), s"top maps near mid (got $vTop)")
        assert((math.abs(vRight - 90.0) < 3.0), s"right side maps high (got $vRight)")
        assert((vSnap % 10.0 == 0.0), s"snaps to the step (got $vSnap)")
    }

    "FileUpload wires onFileSelect metadata (fileselect event) and multiple" in {
        for
            single <- renderHtml(uic.FileUpload().inputId("f1").onSelect(_ => ()))
            multi  <- renderHtml(uic.FileUpload().inputId("f2").multiple(true).onSelect(_ => ()))
        yield
            assert(single.contains("fileselect"), "onFileSelect registers the fileselect metadata event")
            assert(single.contains("p-fileupload-filelabel"), "empty-state file label present")
            assert(multi.contains("multiple"), "multiple(true) renders the native multiple attribute")
    }

    "Dialog(draggable/resizable) placeholder renders a proper box; theme carries the drag glue" in {
        for
            drag <-
                for
                    ref <- Signal.initRef(true)
                    out <- UI.runRender(uic.Dialog().open(ref).header("Move me").draggable(true).resizable(true)(p("body"))).take(1).run
                yield out.mkString
        yield
            val extra = uic.Theme.primeExtraCss
            assert(drag.contains("p-dialog-mask"), "mask present")
            assert(drag.contains("p-dialog"), "box present in the placeholder")
            assert(drag.contains("p-dialog-header"), "header present")
            assert(extra.contains(".p-uic-dialog-draggable-header"), "draggable header cursor glue")
            assert(extra.contains(".p-uic-dialog-movable"), "movable box no-transition glue")
            assert(extra.contains(".p-uic-dialog-resize-handle"), "resize handle glue")
            assert(extra.contains(".p-uic-knob-dial { pointer-events: none; }"), "knob dial pointer-events glue")
    }
    // ---- focusable means operable ----

    /** Elements the browser activates from the keyboard on its own, so a kyo key handler on top
      * would be the second activation rather than the first. An anchor qualifies only with an
      * `href`: without one the browser gives it neither focus nor Enter.
      */
    private def nativelyOperable(tag: String, attrs: String): Boolean =
        tag match
            case "button" | "input" | "select" | "textarea" | "summary" => true
            case "a"                                                    => attrs.contains("href=\"")
            case _                                                      => false

    private val openTag = """<([a-zA-Z][a-zA-Z0-9]*)\s([^>]*?)/?>""".r

    /** The first opening tag in `html` whose class list contains exactly `cls`.
      *
      * A whole-document `contains` cannot tell which element carries an attribute, and for
      * `aria-activedescendant` that is the entire question: the same string on the focused element
      * and on an unfocused list one level down mean announced and not announced.
      */
    private def tagWithClass(html: String, cls: String): String =
        openTag.findAllMatchIn(html).find { m =>
            """\sclass="([^"]*)"""".r.findFirstMatchIn(m.group(2)).exists(_.group(1).split(" ").contains(cls))
        }.map(_.group(0)).getOrElse(throw new AssertionError(s"no element with class $cls"))
    private val evAttr = """data-kyo-ev="([^"]*)"""".r

    /** Every element in `html` that takes the Tab key and acts on a click, but answers no key.
      *
      * That combination is a tab stop a keyboard cannot operate: the reader arrives on it, presses
      * the two keys that work everywhere else, and nothing happens. The list is returned whole
      * rather than as a first hit, since the interesting question is which components have it.
      */
    private def deadTabStops(html: String): Seq[String] =
        openTag.findAllMatchIn(html).flatMap { m =>
            val tag   = m.group(1).toLowerCase
            val attrs = m.group(2)
            val ev    = evAttr.findFirstMatchIn(attrs).map(_.group(1)).getOrElse("").split(",").toSet
            val dead  = attrs.contains("tabindex=\"0\"") && ev.contains("click") && !ev.contains("keydown")
            if dead && !nativelyOperable(tag, attrs) then Some(s"<$tag> ${m.group(0).take(120)}") else None
        }.toSeq

    // The sample below grows with the campaign that introduced this leaf: a component joins it as
    // its keyboard lands, so the invariant is always green and always covers what has been done.
    "every focusable element that acts on a click also acts on a key" in {
        final case class Row(id: String, name: String, note: String) derives CanEqual
        val rows = List(Row("r1", "Ada", "first"), Row("r2", "Grace", "second"))
        for
            icon    <- renderHtml(uic.Icon(uic.Icons.check).accessibleName("Save").onClick(()))
            inplace <- renderHtml(uic.Inplace().display(span("View")).content(p("Full")).closable(true))
            org <- Signal.initRef(Set("ceo")).map { expanded =>
                uic.OrganizationChart()
                    .node(uic.OrgChartNode("CEO", "ceo", children = List(uic.OrgChartNode("CTO", "cto"))))
                    .expanded(expanded)
            }.flatMap(o => renderHtml(o.render))
            table <- Signal.initRef(List.empty[uic.SortKey]).map { sort =>
                uic.DataTable[Row]().rows(rows).rowKey(_.id)
                    .columns(uic.column("Name")(_.name).sortBy(_.name))
                    .selectionMode(uic.SelectionMode.Single)
                    .sort(sort)
            }.map(_.render).flatMap(renderHtml)
            treeSort <- Signal.initRef(List.empty[uic.SortKey])
            tree <- renderHtml(
                uic.TreeTable[Row]().nodes(uic.TreeTableNode(rows.head))
                    .columns(uic.column("Name")(_.name).sortBy(_.name))
                    .selectionMode(uic.SelectionMode.Single)
                    .sort(treeSort)
                    .wired("tt", _ => ())
            )
            // The shapes that already get it right, so the invariant is proven to have teeth
            // in both directions rather than only where it currently bites.
            // The roving-tabindex family, rendered through the `wired` seam: their key handlers
            // live in a mount, and a golden render shows a mount only as its placeholder.
            tabs <- renderHtml(
                uic.Tabs().tabs(uic.Tab("One", p("1"), "one"), uic.Tab("Two", p("2"), "two"))
                    .wired(List("k0", "k1"), _ => ())
            )
            carPage  <- Signal.initRef(0)
            carStart <- Signal.initRef(0.0)
            carousel <- renderHtml(
                uic.Carousel[String]().items(Seq("a", "b"))(x => p(x)).wired(carPage, "car", carStart, _ => ())
            )
            galAt <- Signal.initRef(0)
            galleria <- renderHtml(
                uic.Galleria().items(uic.GalleriaItem("/a.png", "A"), uic.GalleriaItem("/b.png", "B"))
                    .showIndicators(true).showThumbnails(true).activeIndex(galAt).wired("g", _ => ())
            )
            stepAt <- Signal.initRef(0)
            stepper <- renderHtml(
                uic.Stepper().step("One")(p("1")).step("Two")(p("2")).active(stepAt).wired(List("s0", "s1"), _ => ())
            )
            // A `role="group"` of toggle buttons is NOT a composite widget: it asks for no roving
            // and no arrows, and Prime leaves every button its own tab stop. Rendered here so the
            // invariant proves that shape stays reachable rather than being roved by mistake.
            pick      <- Signal.initRef("a")
            segmented <- renderHtml(uic.SelectButton[String]().options(Seq("a", "b")).value(pick).render)
            fanOpen   <- Signal.initRef(true)
            speedDial <- renderHtml(
                uic.SpeedDial().items(
                    uic.MenuItem("Add").icon(uic.Icons.pencil).onSelect(()),
                    uic.MenuItem("Delete").icon(uic.Icons.trash).onSelect(())
                ).wired(fanOpen, "sd", _ => ())
            )
            cpValue  <- Signal.initRef("#ff0000")
            cpOpen   <- Signal.initRef(true)
            cpOpened <- Signal.initRef("#ff0000")
            colorPick <- renderHtml(
                uic.ColorPicker().value(cpValue).wired(uic.ColorPicker.Panel(cpOpen, cpOpened), Present(cpValue), "#ff0000")
            )
            card   <- renderHtml(uic.Card().title("Info").onHeaderClick(())(p("Body")))
            avatar <- renderHtml(uic.Avatar().initials("AL").onClick(()))
        yield
            val named = List(
                "Icon"              -> icon,
                "Inplace"           -> inplace,
                "OrganizationChart" -> org,
                "DataTable"         -> table,
                "TreeTable"         -> tree,
                "Tabs"              -> tabs,
                "Carousel"          -> carousel,
                "Galleria"          -> galleria,
                "Stepper"           -> stepper,
                "SelectButton"      -> segmented,
                "SpeedDial"         -> speedDial,
                "ColorPicker"       -> colorPick,
                "Card"              -> card,
                "Avatar"            -> avatar
            )
            val offenders = named.flatMap((name, html) => deadTabStops(html).map(el => s"$name: $el"))
            assert(offenders.isEmpty, s"these tab stops answer no key:\n${offenders.mkString("\n")}")
            assert(card.nonEmpty && avatar.nonEmpty, "the two known-good shapes rendered at all")
        end for
    }

    // ---- a highlight is only announced if the element it names says what it is ----

    /** The roles `aria-activedescendant` may legitimately name: a thing in a collection. */
    private val descendantRoles =
        Set("menuitem", "menuitemcheckbox", "menuitemradio", "option", "treeitem", "row", "gridcell", "tab")

    private val idAttr   = """\sid="([^"]*)"""".r
    private val roleAttr = """\srole="([^"]*)"""".r

    /** Every `aria-activedescendant` in `html` whose target is missing, or is there but says
      * nothing about itself.
      *
      * The attribute is a promise that a reader who cannot see the highlight will be told what it
      * landed on. A target with `role="presentation"` is the exact shape that breaks the promise
      * while looking wired up: the id resolves, the highlight moves, and what gets announced is
      * an element that has declared itself to be nothing.
      */
    private def danglingActiveDescendants(html: String): Seq[String] =
        """aria-activedescendant="([^"]*)"""".r.findAllMatchIn(html).map(_.group(1)).distinct.flatMap { target =>
            openTag.findAllMatchIn(html).find(m => idAttr.findFirstMatchIn(m.group(2)).exists(_.group(1) == target)) match
                case None => Some(s"$target: names no element")
                case Some(m) =>
                    roleAttr.findFirstMatchIn(m.group(2)).map(_.group(1)) match
                        case Some(r) if descendantRoles.contains(r) => None
                        case Some(r)                                => Some(s"$target: names a role=$r element")
                        case None                                   => Some(s"$target: names an element with no role")
        }.toSeq

    /** Every `aria-activedescendant` in `html` that sits on an element nothing can focus.
      *
      * The attribute is read off the element with DOM focus, or off nothing at all. So the half of
      * the promise the target check cannot see is the CARRIER: on a list inside a panel that the
      * reader never lands on, the id resolves, the role is right, and the announcement still never
      * happens. A carrier qualifies by being a tab stop, by being seeded focus, or by being a text
      * box, which the browser focuses on its own.
      */
    private def strandedAnnouncements(html: String): Seq[String] =
        openTag.findAllMatchIn(html).flatMap { m =>
            val attrs = m.group(2)
            if !attrs.contains("aria-activedescendant=") then None
            else
                val focusable =
                    attrs.contains("""tabindex="0"""") ||
                        attrs.contains("""data-kyo-focus-auto="1"""") ||
                        m.group(1).equalsIgnoreCase("input")
                if focusable then None else Some(s"<${m.group(1)}> ${m.group(0).take(140)}")
            end if
        }.toSeq

    "a highlight names an element that says what it is" in {
        val bar = uic.Menubar().items(
            uic.MenuItem("File").items(uic.MenuItem("New").onSelect(()), uic.MenuItem("Recent").items(uic.MenuItem("a.txt"))),
            uic.MenuItem("Home").icon(uic.Icons.home).onSelect(())
        )
        val tiered = uic.TieredMenu().id("tm").items(menuItems*)
        val mega = uic.MegaMenu().id("mm").items(
            uic.MegaMenuItem("Shop").column(uic.MenuGroup("Men").items(uic.MenuItem("Shirts").onSelect(())))
        )
        val ctx = uic.ContextMenu().id("cm").items(menuItems*)
        val cascade = uic.CascadeSelect[String]()
            .options(Seq(uic.CascadeItem.group("Germany")(uic.CascadeItem.leaf("Berlin"))))(identity)
        def paths(ps: List[List[Int]], open: List[List[Int]]) =
            Kyo.foreach(ps)(p => Signal.initRef(open.contains(p)).map(p -> _)).map(_.toList)
        for
            menuHi <- Signal.initRef(0)
            menu   <- renderHtml(uic.Menu().id("m1").items(menuItems*).wired(menuHi))
            // The POPUP forms too: they are where the panel used to take the focus the list needed.
            menuPopOpen <- Signal.initRef(true)
            menuPopHi   <- Signal.initRef(0)
            menuPopup   <- renderHtml(uic.Menu().id("m2").popup(menuPopOpen).items(menuItems*).wired(menuPopHi))
            barRefs     <- paths(bar.submenuPaths, List(List(0)))
            barHi       <- Signal.initRef(List(0, 0))
            menubar     <- renderHtml(bar.id("mb").wired(barRefs, barHi))
            tieredRefs  <- paths(tiered.submenuPaths, Nil)
            tieredHi    <- Signal.initRef(List(0))
            tieredHtml  <- renderHtml(tiered.wired(tieredRefs, tieredHi))
            tmPopOpen   <- Signal.initRef(true)
            tmPopRefs   <- paths(tiered.submenuPaths, Nil)
            tmPopHi     <- Signal.initRef(List(0))
            tieredPop   <- renderHtml(tiered.id("tmp").popup(tmPopOpen).wired(tmPopRefs, tmPopHi))
            megaRefs    <- paths(mega.panelPaths, List(List(0)))
            megaHi      <- Signal.initRef(List(0))
            megaHtml    <- renderHtml(mega.wired(megaRefs, megaHi))
            ctxOpen     <- Signal.initRef(true)
            ctxHi       <- Signal.initRef(List(0))
            ctxRefs     <- paths(ctx.submenuPaths, Nil)
            ctxHtml     <- renderHtml(ctx.wired(ctxOpen, ctxHi, ctxRefs))
            tsOpen      <- Signal.initRef(true)
            tsExp       <- Signal.initRef(Set.empty[String])
            tsHi        <- Signal.initRef(0)
            treeSel <- renderHtml(
                uic.TreeSelect().nodes(uic.TreeNode("Root", "r")).wired(tsOpen, tsExp, tsHi, "ts")
            )
            dpOpen   <- Signal.initRef(true)
            dpMonth  <- Signal.initRef("2026-07")
            dpCursor <- Signal.initRef("2026-07-15")
            dpValue  <- Signal.initRef("2026-07-15")
            dpView   <- Signal.initRef(uic.DatePickerView.Date)
            dpSeed   <- Signal.initRef(false)
            dpUndo   <- Signal.initRef(Maybe.empty[uic.DatePicker.Restore])
            datePicker <- renderHtml(
                uic.DatePicker().value(dpValue).wired(uic.DatePicker.Refs(dpOpen, dpMonth, dpCursor, dpView, dpSeed, dpUndo), "dp", _ => ())
            )
            selOpen  <- Signal.initRef(true)
            selHi    <- Signal.initRef(1)
            selQuery <- Signal.initRef("")
            selValue <- Signal.initRef("")
            select <- renderHtml(
                uic.Select[String]().id("sel").options(Seq("a", "b"))(identity).optionKey(identity)
                    .value(selValue).wired(selOpen, selHi, selQuery, Present("sel"))
            )
            msOpen  <- Signal.initRef(true)
            msHi    <- Signal.initRef(1)
            msQuery <- Signal.initRef("")
            msValue <- Signal.initRef(Set.empty[String])
            multi <- renderHtml(
                uic.MultiSelect[String]().id("ms").options(Seq("a", "b"))(identity).optionKey(identity)
                    .value(msValue).wired(msOpen, msHi, msQuery, Present("ms"))
            )
            acOpen <- Signal.initRef(true)
            acHi   <- Signal.initRef(1)
            acAll  <- Signal.initRef(true)
            acText <- Signal.initRef("")
            auto <- renderHtml(
                uic.AutoComplete[String]().id("ac").options(List("a", "b"))(identity).optionKey(identity)
                    .value(acText).wired(acOpen, acHi, acAll, Present("ac"))
            )
            csValue <- Signal.initRef("")
            csOpen  <- Signal.initRef(true)
            csRefs  <- paths(cascade.value(csValue).groupPaths, Nil)
            csHi    <- Signal.initRef(List(0))
            csHtml  <- renderHtml(cascade.value(csValue).open(csOpen).wired(csOpen, csRefs, csHi, Present("cs")))
        yield
            val named = List(
                "Menu"               -> menu,
                "Menubar"            -> menubar,
                "Menu (popup)"       -> menuPopup,
                "TieredMenu"         -> tieredHtml,
                "TieredMenu (popup)" -> tieredPop,
                "MegaMenu"           -> megaHtml,
                "ContextMenu"        -> ctxHtml,
                "TreeSelect"         -> treeSel,
                "DatePicker"         -> datePicker,
                "Select"             -> select,
                "MultiSelect"        -> multi,
                "AutoComplete"       -> auto,
                "CascadeSelect"      -> csHtml
            )
            val offenders = named.flatMap((n, h) => danglingActiveDescendants(h).map(d => s"$n: $d"))
            assert(offenders.isEmpty, s"these highlights announce nothing:\n${offenders.mkString("\n")}")
            val stranded = named.flatMap((n, h) => strandedAnnouncements(h).map(d => s"$n: $d"))
            assert(stranded.isEmpty, s"these highlights sit where no focus ever lands:\n${stranded.mkString("\n")}")
            // And the invariant has teeth: each sample really does carry a highlight to check.
            val missing = named.collect { case (n, h) if !h.contains("aria-activedescendant") => n }
            assert(missing.isEmpty, s"no highlight rendered at all in: ${missing.mkString(", ")}")
            assert(select.contains("""role="combobox""""), "a field that opens a list says it is a combobox")
            assert(multi.contains("""role="combobox""""))
            assert(auto.contains("""role="combobox""""))
            assert(csHtml.contains("""role="combobox""""))
            assert(treeSel.contains("""role="combobox""""))
        end for
    }

end GoldenRenderTest
