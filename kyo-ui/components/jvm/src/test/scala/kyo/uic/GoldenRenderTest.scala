package kyo.uic.test

import kyo.*
import kyo.UI.*
import kyo.uic
import kyo.uic.UicTest
import scala.language.implicitConversions

/** Golden SSR-render assertions (JVM-only: kyo effects are discharged with
  * `KyoApp.Unsafe.runAndBlock`, which cannot block on JS). Renders components
  * through kyo-ui's real `HtmlRenderer` and asserts the wire-level contract the
  * theme and the client depend on: class hooks, data attributes, event
  * registration (`data-kyo-ev`), and two-way value binding.
  */
class GoldenRenderTest extends UicTest:
    import AllowUnsafe.embrace.danger

    private def run[A](v: A < (Async & Abort[Throwable])): A =
        KyoApp.Unsafe.runAndBlock(Duration.Infinity)(Abort.run[Throwable](v)) match
            case Result.Success(Result.Success(a)) => a
            case other                             => sys.error(s"effect failed: $other")

    private def renderHtml(ui: UI): String =
        run(UI.runRender(ui).take(1).run.map(_.mkString))

    "Button emits Prime anatomy: class hooks, severity suffix, label span, click registration" in {
        val html = renderHtml(
            uic.Button("Save").severity(uic.Severity.Danger).icon(uic.Icons.check).onClick(())
        )
        val primary = renderHtml(uic.Button("OK"))
        val loading = renderHtml(uic.Button("Busy").loading(true))
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
        val html = run {
            for
                ref <- Signal.initRef("Ada")
                out <- UI.runRender(uic.Input().placeholder("Your name").value(ref)).take(1).run
            yield out.mkString
        }
        val invalid = renderHtml(uic.Input().value("x").invalid(true).invalidMessage("This value is invalid"))
        assert(html.contains("p-inputtext"), "has p-inputtext class")
        assert(html.contains("p-component"), "has p-component class")
        assert(html.contains("""placeholder="Your name""""), "has placeholder")
        assert(html.contains("""value="Ada""""), "renders the ref's current value")
        assert(html.contains("change"), "registers change for two-way binding")
        assert(invalid.contains("p-invalid"), "invalid(true) renders .p-invalid")
        assert(invalid.contains("""aria-invalid="true""""), "invalid(true) sets aria-invalid")
        assert(invalid.contains("p-uic-invalid-message"), "invalidMessage renders the message row")
        assert(invalid.contains("This value is invalid"), "message text rendered")
    }

    "Dialog (open) renders Prime mask + anatomy; (closed) renders no mask" in {
        def dialog(ref: SignalRef[Boolean])(using Frame): UI =
            uic.Dialog()
                .open(ref)
                .header("Confirm")
                .severity(uic.Severity.Danger)
                .footer(span("actions"))(p("Sure?"))

        val open = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(dialog(ref)).take(1).run
            yield out.mkString
        }
        val closed = run {
            for
                ref <- Signal.initRef(false)
                out <- UI.runRender(dialog(ref)).take(1).run
            yield out.mkString
        }
        val maximized = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.Dialog().open(ref).header("Big").maximized(true)(p("x"))).take(1).run
            yield out.mkString
        }
        val noFocus = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(
                    uic.Dialog().open(ref).header("Quiet").preventInitialFocus(true).preventFocusRestore(true)(p("x"))
                ).take(1).run
            yield out.mkString
        }
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
        assert(uic.Theme.primeExtraCss.contains(".p-tablist, .p-tablist-tab-list, .p-breadcrumb-list"), "wave-C row restorers (remainder)")
        assert(uic.Theme.primeExtraCss.contains("li.p-tree-node { flex-direction: column"), "tree node column restorer (remainder)")
        assert(uic.Theme.primeExtraCss.contains("p-icon-spin"), "loading spinner keyframes (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-invalid-message"), "invalidMessage row CSS (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-label"), "label CSS (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-title--h1"), "title rem scale (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-text--clamp"), "text line-clamp (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-avatar-badge"), "avatar badge overlay (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-card-caption-row"), "card caption row (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-progressspinner-sm"), "spinner size presets (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-flex"), "flex primitive (remainder)")
        // Wave D closed the migration: extracted CSS for the last components is
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
        assert(!uic.Theme.primeExtraCss.contains("select.p-select-label"), "native-select chrome strip retired (wave I)")
        assert(uic.Theme.primeExtraCss.contains(".p-autocomplete { flex-direction: row"), "autocomplete row restorer + anchor (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-datepicker { flex-wrap: wrap; }"), "inline datepicker panel wrap (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-dialog-mask { display: flex"), "dialog mask centering (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-uic-dialog-danger"), "dialog severity accents (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-toast { position: fixed"), "toast fixed positioning (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-toast-bottom-right"), "toast position tokens (remainder)")
        assert(uic.Theme.primeExtraCss.contains("tr.p-uic-dt-row"), "datatable tbody re-scope (remainder — no thead/tbody factories)")
        assert(uic.Theme.primeExtraCss.contains(".p-datatable-striped tr.p-uic-dt-row.p-row-odd"), "datatable striped re-scope (remainder)")
        assert(uic.Theme.primeExtraCss.contains(".p-paginator { flex-direction: row; }"), "wave-D row restorers (remainder)")
        assert(!uic.Theme.css.contains(".sap"), "NO sap* class rules anywhere in the theme")
        assert(!uic.Theme.css.contains("--sap"), "NO sap tokens anywhere in the theme")
        assert(!uic.Theme.primeExtraCss.contains(".sap"), "no sap* rules in the Prime remainder")
        assert(!uic.Theme.primeExtraCss.contains("sapBtnLoadingSpin"), "sapBtnLoadingSpin keyframes deleted")
        assert(!uic.Theme.css.contains("ui5"), "no ui5 vocabulary anywhere in the theme")
    }

    "Icon renders inline SVG with the glyph path, currentColor fill, and Prime class hooks" in {
        val html = renderHtml(uic.Icon(uic.Icons.check).accessibleName("Save"))
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
        val html    = renderHtml(uic.Tag("Done").severity(uic.Severity.Success).rounded(true).icon(uic.Icons.check))
        val primary = renderHtml(uic.Tag("Plain"))
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
        val html = renderHtml(uic.Title().level(uic.TitleLevel.H1).size(uic.TitleLevel.H3)("Overview"))
        assert(html.contains("p-uic-title"), "base class")
        assert(html.contains("""role="heading""""), "heading role")
        assert(html.contains("""aria-level="1""""), "aria level from H1")
        assert(html.contains("p-uic-title--h3"), "visual size class")
        assert(html.contains("Overview"), "text rendered")
    }

    "Text renders class hooks, line-clamp data attr, and slot" in {
        val html = renderHtml(
            uic.Text().emptyIndicatorMode(uic.TextEmptyIndicatorMode.On).maxLines(3)("hello")
        )
        assert(html.contains("p-uic-text"), "base class")
        assert(html.contains("p-uic-text--empty-indicator"), "empty indicator modifier")
        assert(html.contains("p-uic-text--clamp"), "clamp modifier class")
        assert(html.contains("""data-uic-max-lines="3""""), "max-lines data attr")
        assert(html.contains("hello"), "default slot rendered")
    }

    "ProgressSpinner renders Prime anatomy: spin svg + circle, progressbar role" in {
        val html  = renderHtml(uic.ProgressSpinner().accessibleName("Loading"))
        val small = renderHtml(uic.ProgressSpinner().size(uic.Size.Small))
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
        val html = run {
            for
                ref <- Signal.initRef(false)
                ui = uic.Panel().header("Details").toggleable(true).collapsed(ref)(
                    uic.Card().title("Info").subtitle("Sub")(p("Body")): UI
                )
                out <- UI.runRender(ui).take(1).run
            yield out.mkString
        }
        val fixed = renderHtml(uic.Panel().header("Plain").footer(span("foot"))(p("content")))
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
        val html = renderHtml(
            uic.Card()
                .title("Jane")
                .additionalText("+12%")
                .headerAvatar(uic.Avatar().initials("JD"))
                .headerAction(uic.Button("Go"))
                .footer(span("footer-slot"))(p("Body"))
        )
        assert(html.contains("p-uic-card-caption-row"), "caption row wrapper")
        assert(html.contains("p-uic-card-avatar"), "avatar slot")
        assert(html.contains("p-uic-card-additional"), "additional text slot")
        assert(html.contains("p-uic-card-action"), "action slot")
        assert(html.contains("+12%"), "additional text rendered")
        assert(html.contains("p-card-footer"), "footer slot")
        assert(html.contains("footer-slot"), "footer content rendered")
    }

    "CheckBox binds two-way: Prime anatomy, checked class + icon from ref, change reg" in {
        val html = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.CheckBox("Accept").checked(ref).invalid(true)).take(1).run
            yield out.mkString
        }
        val mixed = renderHtml(uic.CheckBox("Some").indeterminate(true))
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
        val html = run {
            for
                ref <- Signal.initRef(false)
                out <- UI.runRender(uic.RadioButton("Option A").name("choice").checked(ref)).take(1).run
            yield out.mkString
        }
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
        val html = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.ToggleSwitch().checked(ref)).take(1).run
            yield out.mkString
        }
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
        val html = run {
            for
                ref <- Signal.initRef("hello")
                out <- UI.runRender(uic.TextArea().value(ref).placeholder("Notes").rows(4)).take(1).run
            yield out.mkString
        }
        val resizing = renderHtml(uic.TextArea().autoResize(true).invalid(true).invalidMessage("Too long"))
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
        val html = run {
            for
                ref <- Signal.initRef("b")
                out <- UI.runRender(
                    uic.Select[(String, String)]()
                        .options(Seq("a" -> "Apple", "b" -> "Banana"))(_._2)
                        .optionKey(_._1)
                        .value(ref)
                ).take(1).run
            yield out.mkString
        }
        val placeholder = run {
            for
                ref <- Signal.initRef("")
                out <- UI.runRender(
                    uic.Select[String]().options(Seq("Small", "Large")).placeholder("Pick one").value(ref)
                ).take(1).run
            yield out.mkString
        }
        val invalid = renderHtml(
            uic.Select[String]().options(Seq("x")).invalid(true).invalidMessage("Required").size(uic.Size.Small).fluid(true)
        )
        val named = renderHtml(uic.Select[String]().options(Seq("x")).name("country"))
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
        def openHtml(sel: uic.Select[(String, String)], current: String, hi: Int = -1): String = run {
            for
                vref <- Signal.initRef(current)
                oref <- Signal.initRef(true)
                href <- Signal.initRef(hi)
                qref <- Signal.initRef("")
                out  <- UI.runRender(sel.value(vref).open(oref).wired(oref, href, qref)).take(1).run
            yield out.mkString
        }
        val base = uic.Select[(String, String)]()
            .options(Seq("a" -> "Apple", "b" -> "Banana"))(_._2)
            .optionKey(_._1)

        val open      = openHtml(base, "b")
        val highlight = openHtml(base, "b", hi = 0)
        val featured  = openHtml(base.filterable(true).checkmark(true).showClear(true), "b")
        val disabled  = openHtml(base.optionDisabled(_._1 == "a"), "b")

        assert(open.contains("p-select-open"), "open: root modifier class")
        assert(open.contains("p-uic-overlay-anchor"), "open: anchor glue class for the panel geometry")
        assert(open.contains("""aria-expanded="true""""), "open: trigger reads expanded")
        assert(open.contains("p-uic-overlay-backdrop"), "open: outside-click backdrop (Overlay primitive)")
        assert(open.contains("p-uic-overlay-panel"), "open: overlay panel geometry class")
        assert(open.contains("p-select-overlay"), "open: Prime's panel skin class")
        assert(open.contains("""data-kyo-focus-auto="1""""), "open: panel seeds focus (keyboard without prior click)")
        assert(open.contains("""data-kyo-focus-restore="1""""), "open: focus returns to the trigger on close")
        assert(open.contains("""data-kyo-focus-trap="1""""), "open: panel traps Tab")
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
        assert(featured.contains("""role="searchbox""""), "filter input announces itself")
        assert(featured.contains("p-select-option-check-icon"), "checkmark(true): check glyph on the selected row")
        assert(featured.contains("p-select-option-blank-icon"), "checkmark(true): blank slot on unselected rows")
        assert(featured.contains("p-select-clear-icon"), "showClear(true): clear affordance while a value is set")
        assert(disabled.contains("p-disabled"), "optionDisabled rows carry .p-disabled")
        assert(disabled.contains("""aria-disabled="true""""), "optionDisabled rows carry aria-disabled")

        // filterQuery binds the header to an app-owned ref: the panel filters against
        // its live value, and the header renders without a separate filterable(true).
        // Nothing is selected here, so the trigger label contributes no option text and
        // the assertions below see only the panel rows.
        def appQueryHtml(query: String): String = run {
            for
                vref <- Signal.initRef("")
                oref <- Signal.initRef(true)
                href <- Signal.initRef(-1)
                qref <- Signal.initRef(query)
                out  <- UI.runRender(base.filterQuery(qref).value(vref).open(oref).wired(oref, href, qref)).take(1).run
            yield out.mkString
        }
        val appQuery = appQueryHtml("ap")
        assert(appQuery.contains("p-select-header"), "filterQuery: the header renders without filterable(true)")
        assert(appQuery.contains("""value="ap""""), "filterQuery: the input carries the app-owned query")
        assert(appQuery.contains("Apple"), "filterQuery: the matching option stays")
        assert(!appQuery.contains("Banana"), "filterQuery: the panel filters against the app-owned query")
    }

    "Overlay renders backdrop + anchored panel while open; closed renders nothing" in {
        def overlay(open: Boolean, f: uic.Overlay => uic.Overlay): String = run {
            for
                ref <- Signal.initRef(open)
                out <- UI.runRender(f(uic.Overlay(ref))(span("panel-content"))).take(1).run
            yield out.mkString
        }
        val open   = overlay(true, identity)
        val closed = overlay(false, identity)
        val topEnd = overlay(true, _.anchor(uic.OverlayAnchor.TopEnd).matchWidth(false))
        val capped = overlay(true, _.maxHeight(240))
        val bare   = overlay(true, _.dismissOnOutsideClick(false).dismissOnEscape(false).seedFocus(false))
        val locked = overlay(true, _.scroll(uic.Overlay.Scroll.Lock))
        assert(open.contains("p-uic-overlay-backdrop"), "open: transparent outside-click backdrop")
        assert(open.contains("""data-kyo-ev="click,wheel""""), "open: backdrop registers dismiss click + Scroll.Close wheel")
        assert(locked.contains("""data-kyo-ev="click""""), "Scroll.Lock: backdrop keeps only the dismiss click (wheel swallowed natively)")
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

        // With a trigger the overlay owns the anchor box, so the glue class the
        // geometry depends on cannot be left off by the caller.
        val triggered = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.Overlay(ref).trigger(button("Open"))(span("panel-content"))).take(1).run
            yield out.mkString
        }
        val triggeredClosed = run {
            for
                ref <- Signal.initRef(false)
                out <- UI.runRender(uic.Overlay(ref).trigger(button("Open"))(span("panel-content"))).take(1).run
            yield out.mkString
        }
        assert(triggered.contains("p-uic-overlay-anchor"), "trigger: the overlay stamps its own anchor container")
        assert(triggered.contains("Open"), "trigger: the trigger renders inside that container")
        assert(triggered.contains("p-uic-overlay-panel"), "trigger: the panel renders next to the trigger")
        assert(triggeredClosed.contains("p-uic-overlay-anchor"), "trigger: the anchor container survives a closed panel")
        assert(!triggeredClosed.contains("p-uic-overlay-panel"), "trigger: closed still renders no panel")
        // Without a trigger the primitive stays as it was: backdrop + panel only, for
        // the components that own their own root and stamp the class there.
        assert(!open.contains("p-uic-overlay-anchor"), "no trigger: the overlay adds no container of its own")
    }

    "Tooltip renders Prime anatomy inside the hover wrapper — zero state, zero round-trips" in {
        val html   = renderHtml(uic.Tooltip("Save your work")(uic.Button("Save")))
        val bottom = renderHtml(uic.Tooltip("Below").position(uic.TooltipPosition.Bottom)(span("target")))
        val left   = renderHtml(uic.Tooltip("L").position(uic.TooltipPosition.Left)(span("t")))
        val right  = renderHtml(uic.Tooltip("R").position(uic.TooltipPosition.Right)(span("t")))
        val rich   = renderHtml(uic.Tooltip(span.cssClass("rich")("Formatted"))(span("t")))
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
        def popover(open: Boolean, f: uic.Popover => uic.Popover): String = run {
            for
                ref <- Signal.initRef(open)
                out <- UI.runRender(f(uic.Popover(ref))(p("popover-content"))).take(1).run
            yield out.mkString
        }
        val open    = popover(true, _.trigger(span("Open me")))
        val closed  = popover(false, _.trigger(span("Open me")))
        val bare    = popover(true, identity)
        val flipped = popover(true, _.anchor(uic.OverlayAnchor.TopStart))
        val pinned  = popover(true, _.dismissable(false))
        val noSeed  = popover(true, _.seedFocus(false))
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
    }

    "AutoComplete (open, wired) renders the FLOATING suggestion panel; focus stays in the field" in {
        def openHtml(
            ac: uic.AutoComplete[String],
            text: String,
            open: Boolean = true,
            hi: Int = -1,
            all: Boolean = false
        ): String = run {
            for
                vref <- Signal.initRef(text)
                oref <- Signal.initRef(open)
                href <- Signal.initRef(hi)
                aref <- Signal.initRef(all)
                out  <- UI.runRender(ac.value(vref).wired(oref, href, aref)).take(1).run
            yield out.mkString
        }
        val base = uic.AutoComplete[String]().options(Seq("Apple", "Banana"))

        val open      = openHtml(base, "ap")
        val closed    = openHtml(base, "ap", open = false)
        val empty     = openHtml(uic.AutoComplete[String]().options(Seq("Apple")).emptyMessage("No match"), "zz")
        val gated     = openHtml(base.minQueryLength(3), "ap")
        val fullList  = openHtml(base.dropdown(true), "zz", all = true)
        val highlight = openHtml(base, "ap", hi = 0)
        val statics = run {
            for
                ref <- Signal.initRef("x")
                out <- UI.runRender(
                    uic.AutoComplete[String]().options(Seq("Apple")).loading(true).showClear(true).value(ref)
                ).take(1).run
            yield out.mkString
        }
        val templated = run {
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
        }
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
    }

    "DatePicker inline(true) renders the in-flow Prime panel; month navigation + week numbers" in {
        def picker(vref: SignalRef[String], oref: SignalRef[Boolean])(using Frame): UI =
            uic.DatePicker().inline(true).value(vref).open(oref)

        val open = run {
            for
                vref <- Signal.initRef("2024-03-15")
                oref <- Signal.initRef(true)
                out  <- UI.runRender(picker(vref, oref)).take(1).run
            yield out.mkString
        }
        val closed = run {
            for
                vref <- Signal.initRef("2024-03-15")
                oref <- Signal.initRef(false)
                out  <- UI.runRender(picker(vref, oref)).take(1).run
            yield out.mkString
        }
        val alwaysOn = run {
            for
                vref <- Signal.initRef("2024-03-15")
                out  <- UI.runRender(uic.DatePicker().inline(true).value(vref)).take(1).run
            yield out.mkString
        }
        val withMonth = run {
            for
                vref <- Signal.initRef("2024-03-15")
                mref <- Signal.initRef("2024-05")
                oref <- Signal.initRef(true)
                out  <- UI.runRender(uic.DatePicker().inline(true).value(vref).month(mref).open(oref)).take(1).run
            yield out.mkString
        }
        val weeks = run {
            for
                oref <- Signal.initRef(true)
                out <- UI.runRender(
                    uic.DatePicker().inline(true).referenceDate("2026-07-01").showWeek(true).open(oref)
                ).take(1).run
            yield out.mkString
        }
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
    }

    "DatePicker (open) hosts the SAME panel in the floating Overlay; static projection stays closed" in {
        val floating = run {
            for
                vref <- Signal.initRef("2024-03-15")
                oref <- Signal.initRef(true)
                out  <- UI.runRender(uic.DatePicker().value(vref).open(oref)).take(1).run
            yield out.mkString
        }
        val closed = run {
            for
                vref <- Signal.initRef("2024-03-15")
                oref <- Signal.initRef(false)
                out  <- UI.runRender(uic.DatePicker().value(vref).open(oref)).take(1).run
            yield out.mkString
        }
        val selfManaged = run {
            for
                vref <- Signal.initRef("2024-03-15")
                out  <- UI.runRender(uic.DatePicker().value(vref)).take(1).run
            yield out.mkString
        }
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
        val html     = renderHtml(uic.Link("Docs").href("/help").icon(uic.Icons.download))
        val disabled = renderHtml(uic.Link("Off").href("/x").disabled(true))
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
        val html     = renderHtml(uic.Message().severity(uic.Severity.Danger)("Failed"))
        val closable = renderHtml(uic.Message().severity(uic.Severity.Success).closable(true).onDismissed(())("OK"))
        val simple   = renderHtml(uic.Message().variant(uic.MessageVariant.Simple).size(uic.Size.Small).hideIcon(true)("Hint"))
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
        val html  = renderHtml(uic.Avatar().initials("AL").size(uic.Size.Large).shape(uic.AvatarShape.Circle))
        val icon  = renderHtml(uic.Avatar().icon(uic.Icons.user))
        val image = renderHtml(uic.Avatar().image("https://example.com/a.png").size(uic.Size.XLarge))
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
        val const = renderHtml(uic.ProgressBar().value(40))
        val templ = renderHtml(uic.ProgressBar().value(30).valueTemplate(v => s"$v of 100"))
        val plain = renderHtml(uic.ProgressBar().value(40).showValue(false))
        val indet = renderHtml(uic.ProgressBar().mode(uic.ProgressBarMode.Indeterminate))
        val reactive = run {
            for
                ref <- Signal.initRef(75)
                out <- UI.runRender(uic.ProgressBar().value(ref)).take(1).run
            yield out.mkString
        }
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
        val open = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(toast(ref)).take(1).run
            yield out.mkString
        }
        val closed = run {
            for
                ref <- Signal.initRef(false)
                out <- UI.runRender(toast(ref)).take(1).run
            yield out.mkString
        }
        val danger = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.Toast().open(ref).severity(uic.Severity.Danger).summary("Failed")).take(1).run
            yield out.mkString
        }
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
    }

    "Breadcrumb renders Prime anatomy: nav landmark, item links, chevron separators, home item" in {
        val html = renderHtml(
            uic.Breadcrumb().home(uic.Icons.home, "/").item("Reports", "/reports").item("Q2")
        )
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

    "Toolbar renders Prime anatomy: role + start/center/end sections" in {
        val html = renderHtml(
            uic.Toolbar().start(uic.Button("A")).center(span("mid")).end(uic.Button("B"))
        )
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
        val html = run {
            for
                ref <- Signal.initRef(Set("b"))
                ui = uic.Listbox()
                    .selectionMode(uic.SelectionMode.Multiple)
                    .item("Apple", "a")
                    .item("Banana", "b", icon = Present(uic.Icons.check))
                    .value(ref)
                out <- UI.runRender(ui).take(1).run
            yield out.mkString
        }
        val checked = renderHtml(uic.Listbox().checkmark(true).item("Solo", "s"))
        val empty   = renderHtml(uic.Listbox().emptyMessage("Nothing here"))
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
        val html = run {
            for
                query <- Signal.initRef("ap")
                ui = uic.Listbox()
                    .filterQuery(query)
                    .item("Apple", "a")
                    .item("Banana", "b")
                out <- UI.runRender(ui).take(1).run
            yield out.mkString
        }
        assert(html.contains("p-listbox-header"), "filter header element")
        assert(html.contains("p-listbox-filter"), "filter input class")
        assert(html.contains("p-inputtext"), "filter input carries the inputtext skin")
        assert(html.contains("""value="ap""""), "input value from the query ref")
        assert(html.contains("Apple"), "matching option shown")
        assert(!html.contains("Banana"), "non-matching option filtered out")
    }

    "Listbox filterable(true) renders the same header over a query it allocates itself" in {
        val self  = renderHtml(uic.Listbox().filterable(true).item("Apple", "a").item("Banana", "b"))
        val plain = renderHtml(uic.Listbox().item("Apple", "a").item("Banana", "b"))
        // The static projection is the header, inert: the query ref only exists once the
        // mount publishes, exactly like Select's open/highlight state.
        assert(self.contains("p-listbox-header"), "filterable: header renders without an app-owned ref")
        assert(self.contains("p-listbox-filter"), "filterable: Prime's filter input class")
        assert(self.contains("Apple") && self.contains("Banana"), "filterable: nothing filtered before a query")
        assert(!plain.contains("p-listbox-header"), "no filtering asked for: no header at all")

        val wired = run {
            for
                q   <- Signal.initRef("ap")
                out <- UI.runRender(uic.Listbox().filterQuery(q).filterable(true).item("Apple", "a").item("Banana", "b")).take(1).run
            yield out.mkString
        }
        assert(!wired.contains("Banana"), "an app-owned query still filters when filterable is also set")
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
            sort: SignalRef[List[(String, Boolean)]],
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
                .emptyMessage("Nothing found")

        def render(
            sortV: List[(String, Boolean)],
            queryV: String,
            pageV: Int,
            selV: Set[String],
            expV: Set[String]
        ): String = run {
            for
                sort  <- Signal.initRef(sortV)
                query <- Signal.initRef(queryV)
                page  <- Signal.initRef(pageV)
                sel   <- Signal.initRef(selV)
                exp   <- Signal.initRef(expV)
                out   <- UI.runRender(tableOf(sort, query, page, sel, exp)).take(1).run
            yield out.mkString
        }

        val base     = render(Nil, "", 0, Set("p2"), Set("p1"))
        val sortedD  = render(List("Price" -> false), "", 0, Set.empty, Set.empty)
        val filtered = render(Nil, "watch", 0, Set.empty, Set.empty)
        val page2    = render(Nil, "", 1, Set.empty, Set.empty)
        val empty    = render(Nil, "zzz", 0, Set.empty, Set.empty)

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
        assert(base.contains("p-uic-dt-row"), "body row re-scope class (no tbody factory)")
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
    }

    "Tabs renders Prime's compound anatomy and shows only the selected tab's content" in {
        val html = run {
            for
                ref <- Signal.initRef("t2")
                ui = uic.Tabs()
                    .tab("First", "t1")(p("first-content"))
                    .tab("Second", "t2")(p("second-content"))
                    .selected(ref)
                out <- UI.runRender(ui).take(1).run
            yield out.mkString
        }
        val counted = renderHtml(uic.Tabs().tabs(uic.Tab("Inbox", UI.empty, "in", additionalText = Present("12"))))
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
        val html = run {
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
        }
        val empty = renderHtml(uic.Tree().emptyMessage("No nodes"))
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

        def html(sel: Set[String]): String = run {
            for
                exp <- Signal.initRef(Set("root"))
                s   <- Signal.initRef(sel)
                out <- UI.runRender(tree.expanded(exp).selected(s)).take(1).run
            yield out.mkString
        }
        val partialHtml = html(oneOff)
        val fullHtml    = html(checkedAll)

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
    }

    // ---- wave E: new Tier-1 components ----

    "Skeleton renders Prime anatomy: base classes, aria-hidden, inline dimensions, shape/animation modifiers" in {
        val html   = renderHtml(uic.Skeleton().width("10rem").height("4rem"))
        val circle = renderHtml(uic.Skeleton().shape(uic.SkeletonShape.Circle).size("4rem"))
        val frozen = renderHtml(uic.Skeleton().animation(false).borderRadius("16px"))
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
        val plain    = renderHtml(uic.Divider())
        val centered = renderHtml(uic.Divider().align(uic.DividerAlign.Center).lineStyle(uic.DividerLineStyle.Dashed)(span("OR")))
        val vertical = renderHtml(uic.Divider().layout(uic.DividerLayout.Vertical).lineStyle(uic.DividerLineStyle.Dotted))
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
        val fixed = renderHtml(uic.Fieldset().legend("Header")(p("content")))
        val toggleable = run {
            for
                ref <- Signal.initRef(false)
                out <- UI.runRender(uic.Fieldset().legend("Details").toggleable(true).collapsed(ref)(p("body"))).take(1).run
            yield out.mkString
        }
        val collapsed = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.Fieldset().legend("Details").toggleable(true).collapsed(ref)(p("body"))).take(1).run
            yield out.mkString
        }
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
        val counter = renderHtml(uic.Badge("22").severity(uic.Severity.Danger).size(uic.Size.Large))
        val single  = renderHtml(uic.Badge("4"))
        val dot     = renderHtml(uic.Badge().severity(uic.Severity.Success))
        val overlay = renderHtml(uic.OverlayBadge(uic.Avatar().initials("A"))(uic.Badge("2")))
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
        val iconChip  = renderHtml(uic.Chip("Amy").icon(uic.Icons.user))
        val imageChip = renderHtml(uic.Chip("Amy").image("/amy.png"))
        val removable = renderHtml(uic.Chip("Xuxue").removable(true).onRemove(()))
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
        val html = renderHtml(
            uic.AvatarGroup(
                uic.Avatar().initials("A"),
                uic.Avatar().initials("B")
            )(span.cssClass("extra-slot")("+2"))
        )
        assert(html.contains("p-avatar-group"), "group class")
        assert(html.contains("p-component"), "p-component class")
        assert(html.contains("p-avatar"), "avatars rendered inside")
        assert(html.contains(">A<"), "first avatar rendered")
        assert(html.contains(">B<"), "second avatar rendered")
        assert(html.contains("extra-slot"), "extra children rendered after the avatars")
    }

    "MeterGroup renders Prime anatomy: meter role, scaled inline segments, label list with markers" in {
        val html = renderHtml(
            uic.MeterGroup()
                .meter("Apps", 16)
                .meter("Messages", 8, "var(--p-cyan-500)")
                .meter("Empty", 0)
                .max(200)
        )
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
        val inactive = run {
            for
                ref <- Signal.initRef(false)
                out <- UI.runRender(inplace(ref)).take(1).run
            yield out.mkString
        }
        val active = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(inplace(ref)).take(1).run
            yield out.mkString
        }
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
    }

    "ScrollPanel renders Prime anatomy with inline viewport dimensions" in {
        val html = renderHtml(uic.ScrollPanel(p("long content")).width("100%").height("12rem"))
        assert(html.contains("p-scrollpanel"), "base class")
        assert(html.contains("p-component"), "p-component class")
        assert(html.contains("p-scrollpanel-content-container"), "content container")
        assert(html.contains("p-scrollpanel-content"), "content element")
        assert(html.contains("long content"), "content rendered")
        assert(html.contains("width: 100%"), "inline width")
        assert(html.contains("height: calc(12rem)"), "inline height")
    }

    // ---- wave F: the final ten Tier-1 components ----

    "Rating renders Prime anatomy: options, active split from ref, cancel semantics markers" in {
        val html = run {
            for
                ref <- Signal.initRef(3)
                out <- UI.runRender(uic.Rating().value(ref)).take(1).run
            yield out.mkString
        }
        val readonly = renderHtml(uic.Rating().value(2).readonly(true))
        val disabled = renderHtml(uic.Rating().value(1).disabled(true))
        val ten      = renderHtml(uic.Rating().stars(10))
        assert(html.contains("p-rating"), "base class")
        assert(html.contains("p-component"), "p-component class")
        assert(html.contains("p-hidden-accessible"), "hidden radio container (PrimeVue anatomy)")
        assert(html.contains("""type="radio""""), "hidden native radios")
        assert(html.contains("p-rating-option"), "option elements")
        assert((html.sliding("p-rating-option-active".length).count(_ == "p-rating-option-active") == 3), "3 active options from the ref")
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
        val checked = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.ToggleButton().checked(ref).onLabel("On").offLabel("Off")).take(1).run
            yield out.mkString
        }
        val off      = renderHtml(uic.ToggleButton())
        val icons    = renderHtml(uic.ToggleButton().checked(true).onIcon(uic.Icons.check).offIcon(uic.Icons.times))
        val small    = renderHtml(uic.ToggleButton().size(uic.Size.Small).invalid(true))
        val disabled = renderHtml(uic.ToggleButton().disabled(true))
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
        val single = run {
            for
                ref <- Signal.initRef("b")
                out <- UI.runRender(
                    uic.SelectButton[(String, String)]()
                        .options(Seq("a" -> "Apple", "b" -> "Banana"))(_._2)
                        .optionKey(_._1)
                        .value(ref)
                ).take(1).run
            yield out.mkString
        }
        val multi = run {
            for
                ref <- Signal.initRef(Set("S", "L"))
                out <- UI.runRender(
                    uic.SelectButton[String]().options(Seq("S", "M", "L")).multiple(true).value(ref)
                ).take(1).run
            yield out.mkString
        }
        val invalid = renderHtml(uic.SelectButton[String]().options(Seq("x")).invalid(true))
        assert(single.contains("p-selectbutton"), "base class")
        assert(single.contains("p-component"), "p-component class")
        assert(single.contains("""role="group""""), "group role")
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
        // Prime's `.p-invalid` skin plus the kyo message row — the same two marks every
        // other control already carried, now on the ones that had neither.
        def marks(html: String, what: String): Unit =
            assert(html.contains("p-invalid"), s"$what: invalid class")
            assert(html.contains("""aria-invalid="true""""), s"$what: aria-invalid")
            assert(html.contains("p-uic-invalid-message"), s"$what: message row")
            assert(html.contains("Required"), s"$what: the message text")
        end marks

        marks(renderHtml(uic.ToggleButton().invalid(true).invalidMessage("Required")), "ToggleButton")
        marks(renderHtml(uic.Slider().invalid(true).invalidMessage("Required")), "Slider")
        marks(renderHtml(uic.Knob().invalid(true).invalidMessage("Required")), "Knob")
        marks(renderHtml(uic.Rating().invalid(true).invalidMessage("Required")), "Rating")
        marks(renderHtml(uic.ColorPicker().inline(true).invalid(true).invalidMessage("Required")), "ColorPicker")
        marks(renderHtml(uic.FileUpload().invalid(true).invalidMessage("Required")), "FileUpload")
        marks(
            renderHtml(uic.SelectButton[String]().options(Seq("A", "B")).invalid(true).invalidMessage("Required")),
            "SelectButton"
        )
        marks(renderHtml(uic.Listbox().item("A", "a").invalid(true).invalidMessage("Required")), "Listbox")
        // Valid controls stay clean — the marks are not unconditional decoration.
        val clean = renderHtml(uic.Slider())
        assert(!clean.contains("p-invalid"), "a valid Slider carries no invalid class")
        assert(!clean.contains("p-uic-invalid-message"), "a valid Slider renders no message row")

        // id() reaches the focusable element, which is what focus-first-invalid needs.
        assert(renderHtml(uic.Slider().id("vol")).contains("""id="vol""""), "Slider id lands on the range input")
        assert(renderHtml(uic.Knob().id("gain")).contains("""id="gain""""), "Knob id lands on the dial")
        assert(renderHtml(uic.ToggleButton().id("live")).contains("""id="live""""), "ToggleButton id lands on the button")
        assert(renderHtml(uic.FileUpload().id("cv")).contains("""id="cv""""), "FileUpload id lands on the native input")
    }

    "integer() constrains Slider and Knob to whole numbers" in {
        val fractional = renderHtml(uic.Slider().min(0).max(10).step(0.5).value(3.7))
        val whole      = renderHtml(uic.Slider().min(0).max(10).step(0.5).integer(true).value(3.7))
        assert(fractional.contains("3.7"), "without the constraint the fractional value renders as given")
        assert(!whole.contains("3.7"), "integer(true) rounds the rendered value")
        assert(whole.contains("4"), "3.7 rounds to 4")
        // The native step follows, so the browser cannot produce a fraction either.
        assert(fractional.contains("0.5"), "the declared step is used as-is")
        assert(!whole.contains("0.5"), "integer(true) lifts a fractional step to a whole one")
    }

    "FileUpload binds the picked files as its value, and the label follows that ref" in {
        val payload = UI.FilePayload("cv.pdf", 1024L, "application/pdf", "…")
        val bound = run {
            for
                ref <- Signal.initRef(Seq(payload))
                out <- UI.runRender(uic.FileUpload().value(ref)).take(1).run
            yield out.mkString
        }
        val empty = run {
            for
                ref <- Signal.initRef(Seq.empty[UI.FilePayload])
                out <- UI.runRender(uic.FileUpload().value(ref)).take(1).run
            yield out.mkString
        }
        assert(bound.contains("cv.pdf"), "the bound files drive the chosen-file label")
        assert(bound.contains("p-fileupload-filename"), "a picked file gets Prime's filename class")
        assert(bound.contains("""data-kyo-ev"""), "the picker registers its select handler for the write-back")
        assert(empty.contains("No file chosen"), "an empty bound value shows the empty state")
        assert(!empty.contains("p-fileupload-filename"), "an empty bound value is not a filename")
    }

    "a colliding option key is reported loudly instead of silently picking the wrong option" in {
        // Two options, one label, no optionKey: the derived keys collide, so a pick
        // would apply to whichever matched first. That is the exact failure that only
        // shows up with the data that triggers it.
        val dupSelect = run {
            for
                vref <- Signal.initRef("")
                oref <- Signal.initRef(true)
                href <- Signal.initRef(-1)
                qref <- Signal.initRef("")
                sel = uic.Select[(String, String)]().options(Seq("a" -> "Ada", "b" -> "Ada"))(_._2)
                out <- UI.runRender(sel.value(vref).open(oref).wired(oref, href, qref)).take(1).run
            yield out.mkString
        }
        val keyedSelect = run {
            for
                vref <- Signal.initRef("")
                oref <- Signal.initRef(true)
                href <- Signal.initRef(-1)
                qref <- Signal.initRef("")
                sel = uic.Select[(String, String)]().options(Seq("a" -> "Ada", "b" -> "Ada"))(_._2).optionKey(_._1)
                out <- UI.runRender(sel.value(vref).open(oref).wired(oref, href, qref)).take(1).run
            yield out.mkString
        }
        assert(dupSelect.contains("p-uic-key-error"), "Select: duplicate derived keys render the diagnostic card")
        assert(dupSelect.contains("optionKey"), "Select: the card names the setter to reach for")
        assert(dupSelect.contains("Ada"), "Select: the card names the offending key")
        assert(dupSelect.contains("""role="alert""""), "the card announces itself to assistive tech")
        assert(!keyedSelect.contains("p-uic-key-error"), "Select: a distinct optionKey clears the diagnostic")

        val dupMulti = run {
            for
                vref <- Signal.initRef(Set.empty[String])
                oref <- Signal.initRef(true)
                href <- Signal.initRef(-1)
                qref <- Signal.initRef("")
                ms = uic.MultiSelect[(String, String)]().options(Seq("a" -> "Ada", "b" -> "Ada"))(_._2)
                out <- UI.runRender(ms.value(vref).open(oref).wired(oref, href, qref)).take(1).run
            yield out.mkString
        }
        assert(dupMulti.contains("p-uic-key-error"), "MultiSelect: duplicate derived keys render the card")

        val dupButtons = renderHtml(uic.SelectButton[String]().options(Seq("S", "S", "L")))
        val okButtons  = renderHtml(uic.SelectButton[String]().options(Seq("S", "M", "L")))
        assert(dupButtons.contains("p-uic-key-error"), "SelectButton: duplicate derived keys render the card")
        assert(!okButtons.contains("p-uic-key-error"), "SelectButton: distinct options render no card")

        // The tree-shaped picker defaults its key to the label the same way, so it reports
        // the same collision — from the node ids, which is where it lands there.
        final case class Dept(name: String, subs: List[Dept])
        def treeHtml(depts: Seq[Dept]): String = run {
            for
                vref <- Signal.initRef(Set.empty[String])
                oref <- Signal.initRef(true)
                eref <- Signal.initRef(Set.empty[String])
                ts = uic.TreeSelect().options(depts)(_.name)(_.subs).value(vref)
                out <- UI.runRender(ts.open(oref).wired(oref, eref)).take(1).run
            yield out.mkString
        }
        val dupTree = treeHtml(Seq(Dept("Ops", List(Dept("Ops", Nil)))))
        val okTree  = treeHtml(Seq(Dept("Ops", List(Dept("Field", Nil)))))
        assert(dupTree.contains("p-uic-key-error"), "TreeSelect: duplicate node ids render the card")
        assert(!okTree.contains("p-uic-key-error"), "TreeSelect: distinct node ids render no card")
    }

    "a DataTable that binds identity without a rowKey says so instead of keying by position" in {
        final case class Row(id: String, name: String)
        val rows = Seq(Row("r1", "Ada"), Row("r2", "Bob"))
        def table(f: uic.DataTable[Row] => uic.DataTable[Row]): String = run {
            for
                sel <- Signal.initRef(Set.empty[String])
                base = uic.DataTable[Row]().rows(rows).columns(uic.Column[Row]("Name")(_.name))
                out <- UI.runRender(f(base).selected(sel).selectionMode(uic.SelectionMode.Single)).take(1).run
            yield out.mkString
        }
        val unkeyed = table(identity)
        val keyed   = table(_.rowKey(_.id))
        // No identity bound at all: position is a fine key for a read-only table, so
        // nothing is reported.
        val readOnly = renderHtml(uic.DataTable[Row]().rows(rows).columns(uic.Column[Row]("Name")(_.name)))
        assert(unkeyed.contains("p-uic-key-error"), "selection bound without rowKey renders the diagnostic card")
        assert(unkeyed.contains("rowKey"), "the card names the setter to reach for")
        assert(unkeyed.contains("p-datatable-table-container"), "the table still renders alongside the card")
        assert(!keyed.contains("p-uic-key-error"), "a rowKey clears the diagnostic")
        assert(!readOnly.contains("p-uic-key-error"), "a table that consumes no identity is not nagged")
    }

    "InputGroup renders addons + fields in order; IconField pins InputIcons around the input" in {
        val group = renderHtml(
            uic.InputGroup()(
                uic.InputGroup.addon(span("www.")),
                uic.Input().placeholder("Site"),
                uic.InputGroup.addon(span(".com"))
            )
        )
        val field    = renderHtml(uic.IconField(uic.Input().placeholder("Search")).iconStart(uic.Icons.search))
        val endField = renderHtml(uic.IconField(uic.Input()).iconEnd(uic.Icons.spinner))
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
        def wrapped(v: String): String = run {
            for
                ref <- Signal.initRef(v)
                out <- UI.runRender(uic.FloatLabel(uic.Input().id("uname").value(ref), "Username").forId("uname")).take(1).run
            yield out.mkString
        }
        val filled = wrapped("Ada")
        val empty  = wrapped("")
        val onVar  = renderHtml(uic.FloatLabel(uic.Input(), "Email").variant(uic.FloatLabelVariant.On))
        val ifta   = renderHtml(uic.IftaLabel(uic.Input().value("x"), "Username"))
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

        // The other three FloatLabel hosts render through IftaLabel as well: Prime's
        // iftalabel sheet carries the textarea and .p-inputwrapper selectors for them.
        val iftaTextArea = renderHtml(uic.IftaLabel(uic.TextArea().value("note"), "Notes"))
        val iftaSelect   = renderHtml(uic.IftaLabel(uic.Select[String]().options(Seq("kg", "lb")), "Unit"))
        val iftaAuto     = renderHtml(uic.IftaLabel(uic.AutoComplete[String]().options(Seq("Berlin")), "City"))
        assert(iftaTextArea.contains("p-iftalabel"), "TextArea host wraps in the iftalabel root")
        assert(iftaTextArea.contains("<textarea"), "TextArea host renders its own field")
        assert(iftaTextArea.contains("Notes"), "TextArea host renders the label text")
        assert(iftaSelect.contains("p-iftalabel"), "Select host wraps in the iftalabel root")
        assert(iftaSelect.contains("p-inputwrapper"), "Select host keeps Prime's wrapper hook")
        assert(iftaAuto.contains("p-iftalabel"), "AutoComplete host wraps in the iftalabel root")
        assert(iftaAuto.contains("p-autocomplete"), "AutoComplete host renders its own field")
    }

    "Timeline renders opposite/separator/marker/connector/content per event; last event no connector" in {
        val html = renderHtml(
            uic.Timeline[(String, String)]()
                .events(Seq("Ordered" -> "15/10", "Shipped" -> "16/10", "Delivered" -> "17/10"))
                .content(e => span(e._1))
                .opposite(e => span(e._2))
                .align(uic.TimelineAlign.Alternate)
        )
        val horizontal = renderHtml(
            uic.Timeline[String]().events(Seq(
                "2024",
                "2025"
            )).content(span(_)).layout(uic.TimelineLayout.Horizontal).align(uic.TimelineAlign.Top)
        )
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
        def paginator(p: Int): String = run {
            for
                ref <- Signal.initRef(p)
                out <- UI.runRender(uic.Paginator().totalRecords(25).rows(10).page(ref)).take(1).run
            yield out.mkString
        }
        val first = paginator(0)
        val last  = paginator(2)
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
    }

    "DataView renders layout class, header/content/footer, slices pages through the embedded Paginator" in {
        final case class P(name: String, price: Int)
        val items = List(P("Bamboo Watch", 65), P("Black Watch", 72), P("Blue Band", 79))
        def view(page: Int): String = run {
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
        }
        val page1 = view(0)
        val page2 = view(1)
        val grid  = renderHtml(uic.DataView[P]().items(items).gridItemTemplate(p => div(span(p.name))).layout(uic.DataViewLayout.Grid))
        val empty = renderHtml(uic.DataView[String]().emptyMessage("Nothing here"))
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
    }

    "Stepper renders steplist + active panel; linear disables forward headers" in {
        def stepper(active: Int, linear: Boolean): String = run {
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
        }
        val first                         = stepper(0, false)
        val second                        = stepper(1, false)
        val linear                        = stepper(0, true)
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
    }

    // ---- wave G: deepened DatePicker / Paginator / Stepper / MeterGroup ----

    "DatePicker drills into Prime's month/year views through the currentView ref" in {
        def picker(view: uic.DatePickerView): String = run {
            for
                vref  <- Signal.initRef("2026-07-15")
                mref  <- Signal.initRef("2026-07")
                cvref <- Signal.initRef(view)
                oref  <- Signal.initRef(true)
                out   <- UI.runRender(uic.DatePicker().value(vref).month(mref).currentView(cvref).open(oref)).take(1).run
            yield out.mkString
        }
        val date   = picker(uic.DatePickerView.Date)
        val monthV = picker(uic.DatePickerView.Month)
        val yearV  = picker(uic.DatePickerView.Year)
        val noRefs = run {
            for
                oref <- Signal.initRef(true)
                out  <- UI.runRender(uic.DatePicker().referenceDate("2026-07-01").open(oref)).take(1).run
            yield out.mkString
        }
        assert(
            date.contains(
                """class="p-datepicker-select-month" aria-label="Choose Month" data-kyo-prop-type="button" data-kyo-ev="click""""
            ),
            "date view: month title is a clickable BUTTON"
        )
        assert(
            date.contains("""class="p-datepicker-select-year" aria-label="Choose Year" data-kyo-prop-type="button" data-kyo-ev="click""""),
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
            noRefs.contains("""aria-label="Choose Month" data-kyo-prop-type="button" type="submit" disabled"""),
            "no currentView ref: title buttons render disabled (Prime's switchViewButtonDisabled)"
        )
    }

    "DatePicker view(Month|Year) picks ISO-prefix values from the granularity grids" in {
        val monthPicker = run {
            for
                vref <- Signal.initRef("2026-07")
                oref <- Signal.initRef(true)
                out  <- UI.runRender(uic.DatePicker().value(vref).view(uic.DatePickerView.Month).open(oref)).take(1).run
            yield out.mkString
        }
        val yearPicker = run {
            for
                vref <- Signal.initRef("2026")
                oref <- Signal.initRef(true)
                out  <- UI.runRender(uic.DatePicker().value(vref).view(uic.DatePickerView.Year).open(oref)).take(1).run
            yield out.mkString
        }
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
        val multi = run {
            for
                sel  <- Signal.initRef(Set("2026-07-03", "2026-07-10"))
                oref <- Signal.initRef(true)
                out  <- UI.runRender(uic.DatePicker().values(sel).open(oref)).take(1).run
            yield out.mkString
        }
        val range = run {
            for
                s    <- Signal.initRef("2026-07-06")
                e    <- Signal.initRef("2026-07-09")
                oref <- Signal.initRef(true)
                out  <- UI.runRender(uic.DatePicker().range(s, e).open(oref)).take(1).run
            yield out.mkString
        }
        val rangeOpen = run {
            for
                s    <- Signal.initRef("2026-07-06")
                e    <- Signal.initRef("")
                oref <- Signal.initRef(true)
                out  <- UI.runRender(uic.DatePicker().range(s, e).open(oref)).take(1).run
            yield out.mkString
        }
        assert((count(multi, "p-datepicker-day-selected\"") == 2), "multiple: both set members selected")
        assert(multi.contains("2026-07-03, 2026-07-10"), "multiple: field shows the joined display text")
        assert((count(range, "p-datepicker-day-selected\"") == 2), "range: both endpoints selected")
        assert((count(range, "p-datepicker-day-selected-range") == 2), "range: exactly the two in-between days carry the range class")
        assert(range.contains("2026-07-06 - 2026-07-09"), "range: field shows the start - end display text")
        assert((count(rangeOpen, "p-datepicker-day-selected\"") == 1), "open range: only the start selected")
        assert((count(rangeOpen, "p-datepicker-day-selected-range") == 0), "open range: no in-range days yet")
    }

    "DatePicker time picker renders Prime anatomy; timeOnly drops the calendar" in {
        val dateTime = run {
            for
                vref <- Signal.initRef("2026-07-16T14:30")
                oref <- Signal.initRef(true)
                out  <- UI.runRender(uic.DatePicker().value(vref).showTime(true).open(oref)).take(1).run
            yield out.mkString
        }
        val twelve = run {
            for
                vref <- Signal.initRef("2026-07-16T14:30")
                oref <- Signal.initRef(true)
                out  <- UI.runRender(uic.DatePicker().value(vref).showTime(true).hourFormat(uic.HourFormat.H12).open(oref)).take(1).run
            yield out.mkString
        }
        val clock = run {
            for
                vref <- Signal.initRef("09:15")
                oref <- Signal.initRef(true)
                out  <- UI.runRender(uic.DatePicker().value(vref).timeOnly(true).open(oref)).take(1).run
            yield out.mkString
        }
        val empty = run {
            for
                vref <- Signal.initRef("")
                oref <- Signal.initRef(true)
                out  <- UI.runRender(uic.DatePicker().value(vref).showTime(true).referenceDate("2026-07-01").open(oref)).take(1).run
            yield out.mkString
        }
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
        def picker(withToday: Boolean): String = run {
            for
                vref <- Signal.initRef("2026-07-15")
                oref <- Signal.initRef(true)
                base = uic.DatePicker().value(vref).showButtonBar(true).open(oref)
                out <- UI.runRender(if withToday then base.today("2026-07-16") else base).take(1).run
            yield out.mkString
        }
        val withToday = picker(true)
        val without   = picker(false)
        assert(withToday.contains("p-datepicker-buttonbar"), "button bar container")
        assert(withToday.contains("p-datepicker-today-button"), "today button with today(iso)")
        assert(withToday.contains(">Today<"), "today label")
        assert(withToday.contains("p-datepicker-clear-button"), "clear button")
        assert(withToday.contains(">Clear<"), "clear label")
        assert(withToday.contains("p-button-sm"), "Prime's small text-button skin")
        assert(without.contains("p-datepicker-buttonbar"), "bar renders without today too")
        assert(!without.contains("p-datepicker-today-button"), "no Today button without an explicit today(iso) — the render is pure")
    }

    "Paginator windows page links to pageLinkSize with PrimeVue's boundary math" in {
        def paginator(p: Int, linkSize: Int): String = run {
            for
                ref <- Signal.initRef(p)
                out <- UI.runRender(uic.Paginator().totalRecords(120).rows(10).pageLinkSize(linkSize).page(ref)).take(1).run
            yield out.mkString
        }
        val first                         = paginator(0, 5)
        val mid                           = paginator(5, 5)
        val last                          = paginator(11, 5)
        val all                           = paginator(0, 12)
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
    }

    "Paginator renders the rpp dropdown, current-page report, and jump-to-page slots" in {
        val html = run {
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
        }
        val unbound = renderHtml(
            uic.Paginator().totalRecords(25).rows(10).rowsPerPageOptions(Seq(5, 10))
        )
        assert(html.contains("p-paginator-rpp-dropdown"), "rpp dropdown class on the Select root")
        assert(html.contains("p-select"), "rpp renders our Select anatomy")
        assert(html.contains("""p-select-label">10<"""), "current rows value shows on the closed trigger")
        assert(html.contains("p-paginator-current"), "current-page report span")
        assert(html.contains("Showing 11 to 20 of 25 (2/3, 10 rows)"), "report placeholders substituted")
        assert(html.contains("p-paginator-jtp-input"), "jump-to-page wrapper class")
        assert(html.contains("p-inputtext"), "jtp wraps our Input")
        assert(html.contains("""value="2""""), "jtp shows the 1-based current page")
        assert((html.indexOf("p-paginator-rpp-dropdown") < html.indexOf("p-paginator-current")), "Prime's default order: rpp before report")
        assert((html.indexOf("p-paginator-current") < html.indexOf("p-paginator-jtp-input")), "Prime's default order: report before jtp")
        assert(unbound.contains("p-paginator-rpp-dropdown"), "rpp renders without a rows ref too")
        assert(unbound.contains("p-disabled"), "rpp without a rows(SignalRef) binding renders disabled")
    }

    "Stepper renders the vertical StepItem composition with the inline active panel" in {
        def stepper(active: Int): String = run {
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
        }
        val first                         = stepper(0)
        val last                          = stepper(2)
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
    }

    "Stepper value-keyed steps bind the active VALUE; per-step disabled blocks its header" in {
        val html = run {
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
        }
        val hardDisabled = run {
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
        }
        def count(s: String, sub: String) = s.sliding(sub.length).count(_ == sub)
        assert(html.contains("payment-content"), "value ref selects the matching step's panel")
        assert(!html.contains("personal-content"), "other panels not rendered")
        assert(html.contains("""aria-current="step""""), "active step exposes aria-current")
        assert((count(hardDisabled, "p-disabled") == 1), "exactly the hard-disabled step is blocked outside linear mode")
        assert(hardDisabled.contains("disabled"), "disabled step's header button is a native disabled button")
    }

    "MeterGroup renders vertical orientation, label position/orientation, icons, and templates" in {
        val vertical = renderHtml(
            uic.MeterGroup()
                .orientation(uic.Orientation.Vertical)
                .labelOrientation(uic.Orientation.Vertical)
                .meter("Apps", 30)
        )
        val startLabels = renderHtml(
            uic.MeterGroup().labelPosition(uic.LabelPosition.Start).meter("Apps", 30)
        )
        val icons = renderHtml(
            uic.MeterGroup().meter("Apps", 16, "var(--p-cyan-500)", uic.Icons.check)
        )
        val templated = renderHtml(
            uic.MeterGroup()
                .meter("Storage", 40)
                .startTemplate(span.cssClass("custom-start")("used"))
                .endTemplate(span.cssClass("custom-end")("of 100 GB"))
                .labelTemplate((m, pc) => span.cssClass("custom-label")(s"${m.label}: ${math.round(pc)}%"))
        )
        val meterTpl = renderHtml(
            uic.MeterGroup().meter("Zero", 0).meterTemplate((m, pc) => span.cssClass("custom-meter")(m.label))
        )
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

    "Theme carries the wave-G kyo remainder" in {
        assert(uic.Theme.primeExtraCss.contains(".p-datepicker-buttonbar, .p-datepicker-time-picker,"), "wave-G row restorers (remainder)")
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

    "Theme carries the wave-F extracted CSS + kyo remainder" in {
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
            "wave-F row restorers (remainder)"
        )
        assert(uic.Theme.primeExtraCss.contains("span.p-rating-icon, span.p-togglebutton-icon"), "wave-F glyph spans (remainder)")
        assert(uic.Theme.primeExtraCss.contains("span.p-inputicon"), "inputicon token box (remainder)")
        assert(uic.Theme.primeExtraCss.contains("button.p-step-header { font: inherit; }"), "step header font inherit (remainder)")
    }

    "Theme carries the wave-E extracted CSS + kyo remainder" in {
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
            "wave-E row restorers (remainder)"
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

    // ---- wave H: PrimeReact/PrimeVue feature-gap closure ----

    "Rating renders hidden per-option radios (form participation) + custom on/off icons" in {
        val named                         = renderHtml(uic.Rating().value(2).name("score"))
        val hearts                        = renderHtml(uic.Rating().value(1).stars(2).onIcon(uic.Icons.heartFill).offIcon(uic.Icons.heart))
        val readonly                      = renderHtml(uic.Rating().value(2).readonly(true))
        def count(s: String, sub: String) = s.sliding(sub.length).count(_ == sub)
        assert((count(named, "p-hidden-accessible") == 5), "one hidden container per option")
        assert((count(named, """type="radio"""") == 5), "one native radio per option")
        assert((count(named, """name="score"""") == 5), "name(...) groups all radios")
        assert(named.contains("""value="2""""), "radios carry their star value")
        assert(named.contains("checked"), "the current value's radio is checked")
        assert(named.contains("""aria-label="1 star""""), "first star aria-label")
        assert(named.contains("""aria-label="2 stars""""), "plural star aria-label")
        assert(!renderHtml(uic.Rating().value(1)).contains("name="), "no name attribute unless set (no uniqueness source server-side)")
        assert(hearts.contains("""data-uic-icon="heart-fill""""), "onIcon overrides the filled glyph")
        assert(hearts.contains("""data-uic-icon="heart""""), "offIcon overrides the outline glyph")
        assert(!hearts.contains("""data-uic-icon="star"""), "no star glyphs once overridden")
        assert(hearts.contains("p-rating-on-icon"), "override keeps the on-icon class")
        assert(hearts.contains("p-rating-off-icon"), "override keeps the off-icon class")
        assert(readonly.contains("disabled"), "readonly disables the hidden radios")
        assert(readonly.contains("""aria-readonly="true""""), "readonly radios expose aria-readonly")
    }

    "ToggleButton fluid spans + readonly blocks toggling without the disabled look" in {
        val fluid = renderHtml(uic.ToggleButton().checked(true).fluid(true))
        val readonly = run {
            for
                ref <- Signal.initRef(false)
                out <- UI.runRender(uic.ToggleButton().checked(ref).readonly(true)).take(1).run
            yield out.mkString
        }
        assert(fluid.contains("p-togglebutton-fluid"), "fluid modifier class")
        assert(!readonly.contains("click"), "readonly registers no toggle click")
        assert(!readonly.contains("disabled"), "readonly is not disabled (keeps the normal look)")
        assert(readonly.contains("p-togglebutton"), "readonly keeps the base anatomy")
    }

    "SelectButton itemTemplate renders arbitrary UI per option; label stays the aria name" in {
        val html = run {
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
        }
        assert(html.contains("opt-tpl"), "template UI rendered inside the buttons")
        assert(html.contains("KILOGRAM"), "template output rendered")
        assert(!html.contains("p-togglebutton-label"), "template replaces the default label span")
        assert(html.contains("p-togglebutton-content"), "template renders inside the content span")
        assert(html.contains("""aria-label="Kilogram""""), "label projection stays the accessible name")
        assert(html.contains("p-togglebutton-checked"), "selection still binds through the key")
    }

    "IconField hosts TextArea and Select via the stamped padding classes" in {
        val ta = renderHtml(uic.IconField(uic.TextArea().placeholder("Notes")).iconStart(uic.Icons.search))
        val sel = renderHtml(
            uic.IconField(uic.Select[String]().options(Seq("A", "B"))).iconStart(uic.Icons.user).iconEnd(uic.Icons.chevronDown)
        )
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
        def taWrapped(v: String): String = run {
            for
                ref <- Signal.initRef(v)
                out <- UI.runRender(uic.FloatLabel(uic.TextArea().id("msg").value(ref), "Message").forId("msg")).take(1).run
            yield out.mkString
        }
        def selWrapped(v: String): String = run {
            for
                ref <- Signal.initRef(v)
                out <- UI.runRender(
                    uic.FloatLabel(uic.Select[String]().options(Seq("kg", "lb")).id("unit").value(ref), "Unit").forId("unit")
                ).take(1).run
            yield out.mkString
        }
        val taFilled  = taWrapped("Hello")
        val taEmpty   = taWrapped("")
        val selFilled = selWrapped("kg")
        val selEmpty  = selWrapped("")
        val acFilled = run {
            for
                ref <- Signal.initRef("Berlin")
                out <- UI.runRender(
                    uic.FloatLabel(uic.AutoComplete[String]().options(Seq("Berlin")).id("city").value(ref), "City").forId("city")
                ).take(1).run
            yield out.mkString
        }
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
    }

    "ToggleSwitch handleIcon renders the per-state glyph inside the handle" in {
        val on    = renderHtml(uic.ToggleSwitch().checked(true).handleIcon(uic.Icons.check, uic.Icons.times))
        val off   = renderHtml(uic.ToggleSwitch().checked(false).handleIcon(uic.Icons.check, uic.Icons.times))
        val plain = renderHtml(uic.ToggleSwitch().checked(true))
        assert(on.contains("p-toggleswitch-handle"), "handle element")
        assert(on.contains("p-uic-toggleswitch-handle-icon"), "handle icon sizing class")
        assert(on.contains("""data-uic-icon="check""""), "checked state renders the checked glyph")
        assert(!on.contains("""data-uic-icon="times""""), "unchecked glyph absent while on")
        assert(off.contains("""data-uic-icon="times""""), "unchecked state renders the unchecked glyph")
        assert((on.indexOf("p-toggleswitch-handle") < on.indexOf("data-uic-icon")), "glyph renders inside the handle")
        assert(!plain.contains("data-uic-icon"), "no glyph without handleIcon (Prime's empty handle)")
    }

    "Chip removeIcon overrides the remove glyph, keeping the button anatomy" in {
        val html = renderHtml(uic.Chip("Tag").removable(true).removeIcon(uic.Icons.times).onRemove(()))
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
        val disabledHtml = run {
            for
                ref <- Signal.initRef(false)
                out <- UI.runRender(inplace(true)(ref)).take(1).run
            yield out.mkString
        }
        val openable = run {
            for
                ref <- Signal.initRef(false)
                out <- UI.runRender(inplace(false)(ref)).take(1).run
            yield out.mkString
        }
        val closable = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(inplace(false)(ref)).take(1).run
            yield out.mkString
        }
        assert(disabledHtml.contains("p-disabled"), "disabled: display carries p-disabled")
        assert(!disabledHtml.contains("click"), "disabled: no activation registered")
        assert(openable.contains("p-inplace-display"), "enabled: display side rendered")
        assert(openable.contains("click"), "enabled: activation (ref write + onOpen) registered")
        assert(closable.contains("p-inplace-content"), "active: content side rendered")
        assert(closable.contains("click"), "active: close (ref write + onClose) registered")
    }

    "DataView loading renders the sheet's overlay + ProgressSpinner over the content" in {
        val loading = renderHtml(
            uic.DataView[String]().items(Seq("a", "b")).itemTemplate(s => div(span(s))).loading(true)
        )
        val idle = renderHtml(
            uic.DataView[String]().items(Seq("a")).itemTemplate(s => div(span(s)))
        )
        assert(loading.contains("p-dataview-loading"), "root loading modifier (overlay anchor)")
        assert(loading.contains("p-dataview-loading-overlay"), "sheet overlay class")
        assert(loading.contains("p-overlay-mask"), "dimming mask composed")
        assert(!loading.contains("p-overlay-mask-enter-active"), "no transient enter class (paints transparent when permanent)")
        assert(loading.contains("p-progressspinner"), "ProgressSpinner composed")
        assert(loading.contains("p-dataview-content"), "content still rendered under the overlay")
        assert(!idle.contains("p-dataview-loading"), "idle: no loading modifier")
        assert(!idle.contains("p-dataview-loading-overlay"), "idle: no overlay")
    }

    "Theme carries the wave-H kyo remainder" in {
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

    "Theme carries the wave-I kyo remainder (Overlay primitive + Select panel)" in {
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

    "Theme carries the wave-J kyo remainder (Tooltip/Popover + floating AutoComplete/DatePicker)" in {
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

    // ---- wave K: the menu family ----

    private val menuItems = Seq(
        uic.MenuItem("New").icon(uic.Icons.plus).onSelect(()),
        uic.MenuItem("Open").url("/open"),
        uic.MenuItem.separator,
        uic.MenuItem("Quit").disabled(true)
    )

    "Menu (inline + wired) renders Prime anatomy: list, rows, separator, section label, focus row" in {
        def wiredHtml(m: uic.Menu, hi: Int = -1): String = run {
            for
                href <- Signal.initRef(hi)
                out  <- UI.runRender(m.wired(href)).take(1).run
            yield out.mkString
        }
        val base      = uic.Menu().items(menuItems*)
        val html      = wiredHtml(base)
        val highlight = wiredHtml(base, hi = 0)
        val ided      = wiredHtml(uic.Menu().id("m1").items(menuItems*), hi = 0)
        val grouped = wiredHtml(
            uic.Menu().items(uic.MenuItem("Documents").items(uic.MenuItem("New").icon(uic.Icons.plus)))
        )
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
    }

    "Menu (popup, wired) rides the Overlay: p-menu-overlay skin, focus seeding, Escape/outside dismiss" in {
        def popupHtml(open: Boolean): String = run {
            for
                oref <- Signal.initRef(open)
                href <- Signal.initRef(-1)
                out  <- UI.runRender(uic.Menu().items(menuItems*).popup(oref).wired(href)).take(1).run
            yield out.mkString
        }
        val open   = popupHtml(true)
        val closed = popupHtml(false)
        assert(open.contains("p-uic-overlay-backdrop"), "open: outside-click backdrop (Overlay primitive)")
        assert(open.contains("p-uic-overlay-panel"), "open: overlay panel geometry class")
        assert(open.contains("p-menu-overlay"), "open: Prime's popup skin class")
        assert(open.contains("p-menu-list"), "open: list inside the panel")
        assert(open.contains("""data-kyo-focus-auto="1""""), "open: panel seeds focus (keyboard without prior click)")
        assert(open.contains("""data-kyo-focus-restore="1""""), "open: focus returns to the trigger on close")
        assert(open.contains("keydown"), "open: panel registers Escape + arrow navigation")
        assert(!open.contains("p-uic-overlay-match-width"), "popup sizes to content (no matchWidth)")
        assert(!closed.contains("p-menu-list"), "closed: nothing rendered")
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
        def wiredHtml(openPaths: List[List[Int]], focus: List[Int] = Nil, setId: Boolean = false): String = run {
            for
                refs <- Kyo.foreach(mb.submenuPaths)(p => Signal.initRef(openPaths.contains(p)).map(p -> _))
                fref <- Signal.initRef(focus)
                out  <- UI.runRender((if setId then mb.id("mb1") else mb).wired(refs.toList, fref)).take(1).run
            yield out.mkString
        }
        val closed      = wiredHtml(Nil)
        val open        = wiredHtml(List(List(0)))
        val nested      = wiredHtml(List(List(0), List(0, 1)))
        val highlighted = wiredHtml(Nil, focus = List(1), setId = true)
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
        ): String = run {
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
        }
        val closed      = wiredHtml(Nil)
        val open        = wiredHtml(List(List(0)))
        val popupOpen   = wiredHtml(Nil, popup = Present(true))
        val highlighted = wiredHtml(List(List(0)), focus = List(0), setId = true)
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
        ): String = run {
            val m0 = if vertical then mm.orientation(uic.Orientation.Vertical) else mm
            val m  = if setId then m0.id("mg1") else m0
            for
                refs <- Kyo.foreach(m.panelPaths)(p => Signal.initRef(openPaths.contains(p)).map(p -> _))
                fref <- Signal.initRef(focus)
                out  <- UI.runRender(m.wired(refs.toList, fref)).take(1).run
            yield out.mkString
            end for
        }
        val closed    = wiredHtml(Nil)
        val open      = wiredHtml(List(List(0)))
        val vertical  = wiredHtml(List(List(0)), vertical = true)
        val itemFocus = wiredHtml(List(List(0)), focus = List(0, 0, 1), setId = true) // "Bar stools"
        val rootFocus = wiredHtml(Nil, focus = List(1))                               // "Contact" root
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
    }

    "SplitButton (wired) renders both segments and the popup Menu panel" in {
        def wiredHtml(sb: uic.SplitButton, open: Boolean): String = run {
            for
                oref <- Signal.initRef(open)
                href <- Signal.initRef(-1)
                out  <- UI.runRender(sb.wired(oref, href)).take(1).run
            yield out.mkString
        }
        val base = uic.SplitButton("Save").items(
            uic.MenuItem("Update").onSelect(()),
            uic.MenuItem.separator,
            uic.MenuItem("Delete").onSelect(())
        )
        val closed = wiredHtml(base, open = false)
        val open   = wiredHtml(base, open = true)
        val styled = wiredHtml(
            base.severity(uic.Severity.Danger).size(uic.Size.Small).rounded(true).raised(true).fluid(true),
            open = false
        )
        val disabled = wiredHtml(base.disabled(true), open = false)
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
    }

    "SpeedDial (wired) renders the toggle + linear action fan; open stamps p-speeddial-open" in {
        def wiredHtml(sd: uic.SpeedDial, open: Boolean): String = run {
            for
                oref <- Signal.initRef(open)
                out  <- UI.runRender(sd.wired(oref)).take(1).run
            yield out.mkString
        }
        val base = uic.SpeedDial().items(
            uic.MenuItem("Add").icon(uic.Icons.pencil).onSelect(()),
            uic.MenuItem("Delete").icon(uic.Icons.trash).onSelect(())
        )
        val closed = wiredHtml(base, open = false)
        val open   = wiredHtml(base, open = true)
        val down   = wiredHtml(base.direction(uic.SpeedDialDirection.Down), open = false)
        assert(closed.contains("p-speeddial"), "root class")
        assert(closed.contains("p-speeddial-up"), "Up is the default direction")
        assert(closed.contains("p-speeddial-button"), "toggle button class")
        assert(closed.contains("p-speeddial-rotate"), "toggle rotate class (sheet rotates the plus glyph)")
        assert(closed.contains("""data-uic-icon="plus""""), "toggle plus glyph")
        assert(closed.contains("p-speeddial-list"), "action list")
        assert(closed.contains("p-speeddial-item"), "action rows")
        assert(closed.contains("p-button-rounded"), "actions are rounded icon Buttons")
        assert(closed.contains("p-button-secondary"), "actions carry the secondary severity")
        assert(closed.contains("p-button-sm"), "actions are small (Prime's 32px fan buttons)")
        assert(closed.contains("""aria-label="Add""""), "action label becomes the accessible name")
        assert(closed.contains("""title="Add""""), "action label becomes the native tooltip")
        assert(!closed.contains("p-speeddial-open"), "closed: no open modifier")
        assert(closed.contains("""aria-expanded="false""""), "closed: aria-expanded false")
        assert(open.contains("p-speeddial-open"), "open: open modifier (sheet scales the fan in)")
        assert(open.contains("""aria-expanded="true""""), "open: aria-expanded true")
        assert(down.contains("p-speeddial-down"), "direction modifier class")
    }

    "Theme carries the wave-K kyo remainder (menu family)" in {
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

    // ==== wave L: MultiSelect / CascadeSelect / TreeSelect / Drawer ====

    "MultiSelect (closed) renders Prime's trigger: label modes, chips, clear, form carrier" in {
        def closedHtml(ms: uic.MultiSelect[String], sel: Set[String]): String = run {
            for
                ref <- Signal.initRef(sel)
                out <- UI.runRender(ms.value(ref)).take(1).run
            yield out.mkString
        }
        val base = uic.MultiSelect[String]().options(Seq("Apple", "Banana", "Cherry"))

        val comma       = closedHtml(base, Set("Apple", "Cherry"))
        val placeholder = closedHtml(base.placeholder("Pick fruit"), Set.empty)
        val overMax     = closedHtml(base.maxSelectedLabels(1).selectedItemsLabel("{0} picked"), Set("Apple", "Banana"))
        val chips       = closedHtml(base.display(uic.MultiSelectDisplay.Chip), Set("Apple", "Banana"))
        val named       = closedHtml(base.name("fruit"), Set("Apple", "Cherry"))
        val styled = closedHtml(
            base.invalid(true).invalidMessage("Required").size(uic.Size.Small).fluid(true).variant(uic.FieldVariant.Filled),
            Set.empty
        )
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
    }

    "MultiSelect (open, wired) renders the panel: header select-all + filter, checkbox rows" in {
        def openHtml(ms: uic.MultiSelect[String], sel: Set[String], hi: Int = -1): String = run {
            for
                vref <- Signal.initRef(sel)
                oref <- Signal.initRef(true)
                href <- Signal.initRef(hi)
                qref <- Signal.initRef("")
                out  <- UI.runRender(ms.value(vref).open(oref).wired(oref, href, qref)).take(1).run
            yield out.mkString
        }
        val base = uic.MultiSelect[String]().options(Seq("Apple", "Banana"))

        val open      = openHtml(base, Set("Apple"))
        val highlight = openHtml(base, Set("Apple"), hi = 1)
        val allPicked = openHtml(base, Set("Apple", "Banana"))
        val featured  = openHtml(base.filterable(true).showClear(true), Set("Apple"))
        val noToggle  = openHtml(base.showToggleAll(false), Set("Apple"))
        val disabled  = openHtml(base.optionDisabled(_ == "Banana"), Set.empty)
        val hilite    = openHtml(base.highlightOnSelect(true), Set("Apple"))

        assert(open.contains("p-multiselect-open"), "open: root modifier class")
        assert(open.contains("p-uic-overlay-anchor"), "open: anchor glue class")
        assert(open.contains("""aria-expanded="true""""), "open: trigger reads expanded")
        assert(open.contains("p-uic-overlay-backdrop"), "open: outside-click backdrop")
        assert(open.contains("p-multiselect-overlay"), "open: Prime's panel skin class")
        assert(open.contains("""data-kyo-focus-auto="1""""), "open: panel seeds focus")
        assert(open.contains("""data-kyo-focus-trap="1""""), "open: panel traps Tab")
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
        assert(featured.contains("""role="searchbox""""), "filter input announces itself")
        assert(featured.contains("p-multiselect-clear-icon"), "showClear(true): clear affordance")
        assert(!noToggle.contains("p-checkbox-input"), "showToggleAll(false): no header checkbox (rows are inert anatomy)")
        assert(disabled.contains("p-disabled"), "optionDisabled rows carry .p-disabled")
        assert(disabled.contains("""aria-disabled="true""""), "optionDisabled rows carry aria-disabled")
    }

    "CascadeSelect (wired) renders the trigger + nested group panel chain" in {
        def html(cs: uic.CascadeSelect[String], current: String, rootOpen: Boolean, openPaths: List[List[Int]]): String =
            run {
                for
                    vref <- Signal.initRef(current)
                    oref <- Signal.initRef(rootOpen)
                    refs <- Kyo.foreach(cs.value(vref).groupPaths)(p => Signal.initRef(openPaths.contains(p)).map(p -> _))
                    out  <- UI.runRender(cs.value(vref).open(oref).wired(oref, refs.toList)).take(1).run
                yield out.mkString
            }
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

        val closed = html(base.placeholder("Select a city"), "", rootOpen = false, Nil)
        // Zurich is a ROOT-level leaf: its row is visible without any group open
        // (a selected leaf inside a closed group renders only its trigger label).
        val open   = html(base, "Zurich", rootOpen = true, Nil)
        val nested = html(base, "", rootOpen = true, List(List(0), List(0, 0)))

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
    }

    "TreeSelect (wired) hosts the REAL Tree in the floating panel" in {
        def html(ts: uic.TreeSelect, sel: Set[String], exp: Set[String], open: Boolean): String = run {
            for
                vref <- Signal.initRef(sel)
                oref <- Signal.initRef(open)
                eref <- Signal.initRef(exp)
                out  <- UI.runRender(ts.value(vref).open(oref).expanded(eref).wired(oref, eref)).take(1).run
            yield out.mkString
        }
        val base = uic.TreeSelect().nodes(
            uic.TreeNode(
                "src",
                "src",
                children = List(uic.TreeNode("App.scala", "app"), uic.TreeNode("Theme.scala", "theme"))
            )
        )

        val closed   = html(base.placeholder("Select a file"), Set.empty, Set.empty, open = false)
        val open     = html(base, Set("app"), Set("src"), open = true)
        val checkbox = html(base.selectionMode(uic.SelectionMode.Checkbox), Set("app"), Set("src"), open = true)

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
        assert(open.contains("""data-kyo-focus-auto="1""""), "open: panel seeds focus")
        assert(checkbox.contains("p-tree-node-checkbox"), "Checkbox mode renders Tree's per-node checkbox column")
    }

    "Drawer (open) renders mask + docked panel; positions + full; (closed) renders nothing" in {
        def drawer(open: Boolean, f: uic.Drawer => uic.Drawer): String = run {
            for
                ref <- Signal.initRef(open)
                out <- UI.runRender(f(uic.Drawer().open(ref).header("Menu"))(p("body"))).take(1).run
            yield out.mkString
        }
        val open     = drawer(true, identity)
        val closed   = drawer(false, identity)
        val right    = drawer(true, _.position(uic.DrawerPosition.Right))
        val full     = drawer(true, _.position(uic.DrawerPosition.Full))
        val footered = drawer(true, _.footer(span("actions")))
        val bare     = drawer(true, _.dismissable(false).showCloseIcon(false).preventInitialFocus(true).preventFocusRestore(true))

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
    }

    "Theme carries the wave-L kyo remainder (multiselect/cascadeselect/treeselect/drawer)" in {
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
        assert(extra.contains("span.p-multiselect-dropdown-icon"), "wave-L glyph sizing")
        assert(uic.Theme.primeCss.contains(".p-multiselect-option"), "extracted multiselect sheet present")
        assert(uic.Theme.primeCss.contains(".p-cascadeselect-option-list"), "extracted cascadeselect sheet present")
        assert(uic.Theme.primeCss.contains(".p-treeselect-overlay .p-tree"), "extracted treeselect sheet reaches the hosted tree")
        assert(uic.Theme.primeCss.contains(".p-drawer-left .p-drawer"), "extracted drawer sheet present")
    }

    "ContextMenu (wired) wraps its target, registers contextmenu, and opens the Prime panel" in {
        def html(cm: uic.ContextMenu, open: Boolean, focus: List[Int], openPaths: List[List[Int]]): String = run {
            for
                oref <- Signal.initRef(open)
                fref <- Signal.initRef(focus)
                refs <- Kyo.foreach(cm.submenuPaths)(p => Signal.initRef(openPaths.contains(p)).map(p -> _))
                out  <- UI.runRender(cm.wired(oref, fref, refs.toList)).take(1).run
            yield out.mkString
        }
        val base = uic.ContextMenu(
            Seq(
                uic.MenuItem("Copy").icon(uic.Icons.copy),
                uic.MenuItem("Paste"),
                uic.MenuItem.separator,
                uic.MenuItem("Share").items(uic.MenuItem("Email"), uic.MenuItem("Link"))
            )
        )(div.cssClass("target-region")(span("right-click me")))

        val closed  = html(base, open = false, focus = Nil, Nil)
        val open    = html(base, open = true, focus = Nil, Nil)
        val focused = html(base.id("cm1"), open = true, focus = List(0), Nil)
        val nested  = html(base, open = true, focus = Nil, List(List(3)))

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
    }

    "ToastService region stacks queued messages in one Prime toast region" in {
        import uic.ToastService.Queued
        def html(queued: Seq[Queued], pos: uic.OverlayPosition = uic.OverlayPosition.BottomRight): String =
            renderHtml(uic.ToastService.body(queued, pos, _ => ()))
        val two = html(
            Seq(
                Queued(1, uic.ToastMessage(uic.Severity.Success, Present("Saved"), Present("Changes stored."), Present(3.seconds))),
                Queued(2, uic.ToastMessage(uic.Severity.Danger, Present("Error"), Present("Request failed."), Absent, closable = true))
            )
        )
        val sticky = html(Seq(Queued(7, uic.ToastMessage(summary = Present("Sticky"), closable = false))))
        val top    = html(Seq(Queued(1, uic.ToastMessage())), uic.OverlayPosition.TopRight)
        val empty  = html(Nil)

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
    }

    "ConfirmDialog rides the Dialog machinery with Prime's confirmdialog anatomy" in {
        def confirm(open: Boolean, f: uic.ConfirmDialog => uic.ConfirmDialog): String = run {
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
        }
        val open    = confirm(true, identity)
        val closed  = confirm(false, identity)
        val labeled = confirm(true, _.acceptLabel("Delete").rejectLabel("Keep").acceptSeverity(uic.Severity.Danger))

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
    }

    "Theme carries the wave-M kyo remainder (contextmenu/toast service/confirmdialog)" in {
        val extra = uic.Theme.primeExtraCss
        assert(extra.contains("li.p-contextmenu-item { flex-direction: column;"), "contextmenu row stacks content over the sub-panel")
        assert(extra.contains("a.p-contextmenu-item-link { flex-direction: row; }"), "contextmenu link row restorer")
        assert(
            extra.contains(".p-uic-overlay-panel.p-contextmenu-submenu { display: flex; flex-direction: column; margin: 0; }"),
            "contextmenu submenu panel restorer (flush, flex column)"
        )
        assert(extra.contains("span.p-contextmenu-submenu-icon"), "wave-M glyph sizing")
        assert(extra.contains(".p-confirmdialog .p-dialog-content { flex-direction: row; }"), "confirmdialog content row restorer")
        assert(extra.contains("span.p-confirmdialog-icon"), "confirmdialog icon glyph sizing")
        assert(uic.Theme.primeCss.contains(".p-contextmenu-root-list"), "extracted contextmenu sheet present")
        assert(uic.Theme.primeCss.contains(".p-confirmdialog-icon"), "extracted confirmdialog sheet present")
    }

    // ==== wave N: InputNumber / Password / InputOtp / Slider / Knob ====

    "InputNumber renders a real number field in Prime's wrapper + spin buttons per layout" in {
        val bound = run {
            for
                ref <- Signal.initRef(5.0)
                out <- UI.runRender(uic.InputNumber().value(ref).min(0).max(10).step(0.5)).take(1).run
            yield out.mkString
        }
        val stacked    = renderHtml(uic.InputNumber().value(3).showButtons(true))
        val horizontal = renderHtml(uic.InputNumber().value(3).showButtons(true).buttonLayout(uic.InputNumberButtonLayout.Horizontal))
        val adorned    = renderHtml(uic.InputNumber().value(42).prefix("$").suffix(" kg"))
        val invalid    = renderHtml(uic.InputNumber().value(1).invalid(true).invalidMessage("Out of range"))
        val disabled   = renderHtml(uic.InputNumber().value(1).showButtons(true).disabled(true))
        val fluid      = renderHtml(uic.InputNumber().value(1).fluid(true))

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
        def wired(revealed: Boolean, f: uic.Password => uic.Password): String = run {
            for
                rev <- Signal.initRef(revealed)
                out <- UI.runRender(f(uic.Password().toggleMask(true)).wired(rev)).take(1).run
            yield out.mkString
        }
        val plain    = renderHtml(uic.Password().value("secret1"))
        val masked   = wired(false, identity)
        val revealed = wired(true, identity)
        val weak = run {
            for
                ref <- Signal.initRef("abc")
                out <- UI.runRender(uic.Password().value(ref).feedback(true)).take(1).run
            yield out.mkString
        }
        val strong = run {
            for
                ref <- Signal.initRef("Str0ngPass")
                out <- UI.runRender(uic.Password().value(ref).feedback(true)).take(1).run
            yield out.mkString
        }
        val prompt = run {
            for
                ref <- Signal.initRef("")
                out <- UI.runRender(uic.Password().value(ref).feedback(true).promptLabel("Type one")).take(1).run
            yield out.mkString
        }

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
    }

    "InputOtp renders N one-char cells bound to one code; mask/integerOnly variants" in {
        val bound = run {
            for
                ref <- Signal.initRef("12")
                out <- UI.runRender(uic.InputOtp().value(ref)).take(1).run
            yield out.mkString
        }
        val six     = renderHtml(uic.InputOtp().value("123456").length(6))
        val masked  = renderHtml(uic.InputOtp().mask(true))
        val invalid = renderHtml(uic.InputOtp().invalid(true).invalidMessage("Wrong code"))

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
        val bound = run {
            for
                ref <- Signal.initRef(30.0)
                out <- UI.runRender(uic.Slider().value(ref)).take(1).run
            yield out.mkString
        }
        val vertical = renderHtml(uic.Slider().value(40).orientation(uic.Orientation.Vertical))
        val disabled = renderHtml(uic.Slider().value(10).disabled(true))
        val stepped  = renderHtml(uic.Slider().value(2).min(0).max(4).step(2))

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
        val bound = run {
            for
                ref <- Signal.initRef(60.0)
                out <- UI.runRender(uic.Knob().value(ref)).take(1).run
            yield out.mkString
        }
        val sized    = renderHtml(uic.Knob().value(25).size(150).strokeWidth(8))
        val noText   = renderHtml(uic.Knob().value(10).showValue(false))
        val template = renderHtml(uic.Knob().value(10).valueTemplate(v => s"${v.toInt}%"))
        val disabled = renderHtml(uic.Knob().value(10).disabled(true))

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

    "Theme carries the wave-N kyo remainder (inputnumber/password/otp/slider/knob)" in {
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

    // ---- wave O: the container/data block ----

    "Accordion renders Prime's panel anatomy; the ref TYPE picks single vs multiple mode" in {
        def panels(acc: uic.Accordion): uic.Accordion =
            acc.panels(
                uic.AccordionPanel("Header I", p("Content I"), "p1"),
                uic.AccordionPanel("Header II", p("Content II"), "p2", disabled = true)
            )
        val single = run {
            for
                ref <- Signal.initRef("p1")
                out <- UI.runRender(panels(uic.Accordion().value(ref))).take(1).run
            yield out.mkString
        }
        val multi = run {
            for
                ref <- Signal.initRef(Set("p1", "p2"))
                out <- UI.runRender(panels(uic.Accordion().value(ref))).take(1).run
            yield out.mkString
        }
        val static = renderHtml(panels(uic.Accordion()))
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
    }

    "OrderList embeds a multiple Listbox beside Prime's four secondary move buttons" in {
        val (withSel, noSel) = run {
            for
                items <- Signal.initRef(Seq("Bamboo Watch", "Black Watch", "Blue Band"))
                sel   <- Signal.initRef(Set("Black Watch"))
                empty <- Signal.initRef(Set.empty[String])
                a     <- UI.runRender(uic.OrderList[String]().items(items)(identity).selected(sel)).take(1).run
                b     <- UI.runRender(uic.OrderList[String]().items(items)(identity).selected(empty)).take(1).run
            yield (a.mkString, b.mkString)
        }
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
    }

    "PickList renders two Listbox columns, transfer controls, and reorder rails" in {
        val html = run {
            for
                src    <- Signal.initRef(Seq("San Francisco", "London"))
                tgt    <- Signal.initRef(Seq("Paris"))
                srcSel <- Signal.initRef(Set("London"))
                tgtSel <- Signal.initRef(Set.empty[String])
                out <- UI.runRender(
                    uic.PickList[String]().sourceItems(src)(identity).targetItems(tgt).sourceSelected(srcSel).targetSelected(tgtSel)
                ).take(1).run
            yield out.mkString
        }
        val noRails = run {
            for
                src <- Signal.initRef(Seq("A"))
                tgt <- Signal.initRef(Seq.empty[String])
                out <- UI.runRender(
                    uic.PickList[String]().sourceItems(src)(identity).targetItems(tgt).showSourceControls(false).showTargetControls(false)
                ).take(1).run
            yield out.mkString
        }
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
    }

    "Carousel renders the visible window, secondary text nav buttons, and indicator dots" in {
        def carousel(ref: SignalRef[Int])(using Frame) =
            uic.Carousel[String]()
                .items(Seq("A", "B", "C", "D", "E"))(s => span(s))
                .numVisible(2)
                .numScroll(2)
                .page(ref)
        val page0 = run {
            for
                ref <- Signal.initRef(0)
                out <- UI.runRender(carousel(ref)).take(1).run
            yield out.mkString
        }
        val page1 = run {
            for
                ref <- Signal.initRef(1)
                out <- UI.runRender(carousel(ref)).take(1).run
            yield out.mkString
        }
        val vertical = run {
            for
                ref <- Signal.initRef(0)
                out <- UI.runRender(
                    uic.Carousel[String]().items(Seq("A", "B"))(s => span(s)).page(ref).vertical(true)
                ).take(1).run
            yield out.mkString
        }
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
        val html = run {
            for
                ref <- Signal.initRef(0)
                out <- UI.runRender(
                    uic.Galleria().items(items*).activeIndex(ref).showItemNavigators(true).showIndicators(true)
                ).take(1).run
            yield out.mkString
        }
        val bare = renderHtml(uic.Galleria().items(items*))
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
    }

    "FileUpload (basic) renders the label-for choose button over the hidden native input" in {
        val html = renderHtml(
            uic.FileUpload().inputId("up1").accept(FileAccept.Extension(".txt")).onSelect(_ => ())
        )
        val disabled = renderHtml(uic.FileUpload().inputId("up2").disabled(true))
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

    "TreeTable renders stamped header/body rows, depth-indented togglers, selection + sort" in {
        final case class F(name: String, size: String)
        def nodes = Seq(
            uic.TreeTableNode(F("Applications", "200mb"), List(uic.TreeTableNode(F("Scala", "25mb")))),
            uic.TreeTableNode(F("Cloud", "20mb"))
        )
        def cols = Seq(
            uic.Column[F]("Name")(_.name).sortBy(_.name),
            uic.Column[F]("Size")(_.size)
        )
        val expanded = run {
            for
                exp  <- Signal.initRef(Set("Applications"))
                sel  <- Signal.initRef(Set("Scala"))
                sort <- Signal.initRef(List.empty[(String, Boolean)])
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
        }
        val collapsed = run {
            for
                exp <- Signal.initRef(Set.empty[String])
                out <- UI.runRender(uic.TreeTable[F]().nodes(nodes*).columns(cols*).rowKey(_.name).expanded(exp)).take(1).run
            yield out.mkString
        }
        assert(expanded.contains("p-treetable"), "root class")
        assert(expanded.contains("p-treetable-table-container"), "table container")
        assert(expanded.contains("p-treetable-table"), "table class")
        assert(expanded.contains("""role="treegrid""""), "treegrid role")
        assert(expanded.contains("p-uic-tt-header-row"), "stamped header row (no thead factory)")
        assert(expanded.contains("p-treetable-header-cell"), "header cell class")
        assert(expanded.contains("p-treetable-column-title"), "column title span")
        assert(expanded.contains("p-treetable-sortable-column"), "sortable header")
        assert(expanded.contains("""data-uic-icon="sort-alt""""), "unsorted icon")
        assert(expanded.contains("p-uic-tt-row"), "stamped body rows")
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
        val expanded = run {
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
        }
        val collapsed = run {
            for
                exp <- Signal.initRef(Set.empty[String])
                out <- UI.runRender(uic.OrganizationChart().node(chart).expanded(exp)).take(1).run
            yield out.mkString
        }
        // No bound ref means no toggles at all — Prime's collapsible=false, without a
        // separate flag to discover.
        val static = renderHtml(uic.OrganizationChart().node(chart))
        assert(expanded.contains("p-organizationchart"), "root class")
        assert(expanded.contains("p-organizationchart-table"), "table-based layout")
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
    }

    "Terminal renders welcome, history rows, and the bare prompt input wired to the handler" in {
        val html = run {
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
        }
        val inert = renderHtml(uic.Terminal().welcomeMessage("W"))
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

    "Theme carries the wave-O kyo remainder (orderlist/picklist/carousel/galleria/fileupload/treetable/orgchart/terminal)" in {
        val extra = uic.Theme.primeExtraCss
        assert(extra.contains(".p-orderlist, .p-picklist, .p-fileupload-basic-content, .p-terminal-prompt,"), "wave-O row restorers")
        assert(extra.contains(".p-terminal-command { display: block; }"), "terminal history rows block")
        assert(extra.contains(".p-carousel-item { display: block; }"), "carousel item block")
        assert(extra.contains("label.p-button { flex-direction: row; }"), "label-as-button row restorer")
        assert(extra.contains(".p-treetable-table tr.p-uic-tt-row"), "treetable stamped-row re-scope")
        assert(extra.contains(".p-uic-tt-toggle-hidden"), "leaf toggler visibility class")
        assert(extra.contains(".p-uic-oc-hidden"), "orgchart collapsed visibility class")
        assert(extra.contains(".p-organizationchart-table td"), "orgchart cell re-scope")
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

    // ==== wave R: ColorPicker / InputMask / VirtualScroller (+ InputOtp retrofit) ====

    "ColorPicker (inline) renders Prime's plane + hue anatomy, hue-tinted, pointer-wired" in {
        val inline = run {
            for
                ref <- Signal.initRef("#ff0000")
                out <- UI.runRender(uic.ColorPicker(ref).inline(true)).take(1).run
            yield out.mkString
        }
        val overlayStat = renderHtml(uic.ColorPicker().value("#00ff00"))
        val disabled    = renderHtml(uic.ColorPicker().value("#0000ff").inline(true).disabled(true))

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
        val phone = run {
            for
                ref <- Signal.initRef("")
                out <- UI.runRender(uic.InputMask("(999) 999-9999").value(ref).placeholder("(999) 999-9999")).take(1).run
            yield out.mkString
        }
        val ssn = renderHtml(uic.InputMask("999-99-9999").value("123-45-6789").invalid(true).invalidMessage("bad"))

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
        val intOnly = renderHtml(uic.InputOtp().integerOnly(true))
        val plain   = renderHtml(uic.InputOtp())
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
        val stat = renderHtml(vs)
        // The wired seam carries the native scroll-position handler.
        val wired = run {
            for
                r   <- Signal.initRef(0.0)
                out <- UI.runRender(vs.wired(r)).take(1).run
            yield out.mkString
        }

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
    }

    "wave-P transitions: Dialog/Toast/Overlay/Message carry enter+leave, driven by Prime's keyframe classes" in {
        val dialog = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.Dialog().open(ref).header("Hi")(p("x"))).take(1).run
            yield out.mkString
        }
        val toast = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.Toast().open(ref).summary("Saved")).take(1).run
            yield out.mkString
        }
        val overlay = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.Overlay(ref)(span("panel"))).take(1).run
            yield out.mkString
        }
        val overlayOff = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.Overlay(ref).animate(false)(span("panel"))).take(1).run
            yield out.mkString
        }
        val message = renderHtml(uic.Message().severity(uic.Severity.Success)(span("done")))

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
        assert(uic.Theme.primeExtraCss.contains(".p-overlay-mask { transition: opacity 150ms ease; }"), "mask carries the enter transition")
    }

    "Theme carries the wave-R kyo remainder + colorpicker/virtualscroller sheets" in {
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

    // ==== Wave R2: flip/shift, OTP auto-advance, auto-scroll, drag, metadata ====

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
        def geom(rect: UI.Rect): String =
            renderHtml(div.style(uic.Overlay.geometryFor(uic.OverlayAnchor.BottomStart, rect)))
        val overBottom = UI.Rect(x = 10, y = 500, width = 100, height = 200, viewportWidth = 1000, viewportHeight = 600)
        val fits       = UI.Rect(x = 10, y = 50, width = 100, height = 200, viewportWidth = 1000, viewportHeight = 600)
        val overRight  = UI.Rect(x = 950, y = 50, width = 100, height = 200, viewportWidth = 1000, viewportHeight = 600)
        val flipped    = geom(overBottom)
        val below      = geom(fits)
        val shifted    = geom(overRight)
        assert(flipped.contains("bottom: 100%"), "bottom overflow flips up: bottom:100%")
        assert(flipped.contains("top: auto"), "bottom overflow flips up: top:auto (leaving edge reset)")
        assert(below.contains("top: 100%"), "fits: stays below with top:100%")
        assert(below.contains("bottom: auto"), "fits: bottom:auto (leaving edge reset)")
        assert(below.contains("margin: 0 0 0 0"), "no x-overflow: zero margin-left shift")
        assert(shifted.contains("margin: 0 0 0 -"), "x-overflow: negative margin-left pulls it inside")
    }

    "Overlay(autoFlip default true) open still renders the anchored backdrop + panel placeholder" in {
        def overlay(flip: Boolean): String = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.Overlay(ref).autoFlip(flip)(span("panel-content"))).take(1).run
            yield out.mkString
        }
        val on  = overlay(true)
        val off = overlay(false)
        assert(on.contains("p-uic-overlay-backdrop"), "autoFlip on: backdrop present (mounted placeholder = declared anchor)")
        assert(on.contains("p-uic-overlay-panel"), "autoFlip on: panel present")
        assert(on.contains("p-uic-overlay-bottom-start"), "autoFlip on: declared anchor geometry in the placeholder")
        assert(off.contains("p-uic-overlay-panel"), "autoFlip off: panel present")
        assert(off.contains("panel-content"), "autoFlip off: children rendered")
    }

    "InputOtp.wired stamps a self id on every cell (auto-advance target) and registers input" in {
        val html = run {
            for
                ref <- Signal.initRef("12")
                out <- UI.runRender(uic.InputOtp().value(ref).wired(ref, List("otp0", "otp1", "otp2", "otp3"), _ => ())).take(1).run
            yield out.mkString
        }
        assert(html.contains("""id="otp0""""), "cell 0 carries its self id")
        assert(html.contains("""id="otp1""""), "cell 1 carries its self id")
        assert(html.contains("""id="otp3""""), "last cell carries its self id")
        assert(html.contains("input"), "cells still register the write-back that triggers the advance")
    }

    "Terminal.wired stamps the newest history row so it can be scrolled into view" in {
        val html = run {
            for
                ref <- Signal.initRef(Seq(uic.TerminalCommand("a", "1"), uic.TerminalCommand("b", "2")))
                out <- UI.runRender(
                    uic.Terminal().commands(ref).commandHandler(_ => "ok").wired(ref, "lastrow", _ => ())
                ).take(1).run
            yield out.mkString
        }
        assert(("""id="lastrow"""".r.findAllIn(html).size == 1), "exactly the newest row carries the scroll id")
        assert(html.contains("p-terminal-command"), "history rows rendered")
    }

    "Carousel.wired renders ALL items in the id-stamped stable strip and wires swipe on it" in {
        val html = run {
            for
                ref   <- Signal.initRef(0)
                start <- Signal.initRef(0.0)
                out <- UI.runRender(
                    uic.Carousel[String]().items(Seq("A", "B", "C"))(s => span(s)).page(ref).wired(ref, "strip", start)
                ).take(1).run
            yield out.mkString
        }
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
        val single = renderHtml(uic.FileUpload().inputId("f1").onSelect(_ => ()))
        val multi  = renderHtml(uic.FileUpload().inputId("f2").multiple(true).onSelect(_ => ()))
        assert(single.contains("fileselect"), "onFileSelect registers the fileselect metadata event")
        assert(single.contains("p-fileupload-filelabel"), "empty-state file label present")
        assert(multi.contains("multiple"), "multiple(true) renders the native multiple attribute")
    }

    "Dialog(draggable/resizable) placeholder renders a proper box; theme carries the drag glue" in {
        val drag = run {
            for
                ref <- Signal.initRef(true)
                out <- UI.runRender(uic.Dialog().open(ref).header("Move me").draggable(true).resizable(true)(p("body"))).take(1).run
            yield out.mkString
        }
        val extra = uic.Theme.primeExtraCss
        assert(drag.contains("p-dialog-mask"), "mask present")
        assert(drag.contains("p-dialog"), "box present in the placeholder")
        assert(drag.contains("p-dialog-header"), "header present")
        assert(extra.contains(".p-uic-dialog-draggable-header"), "draggable header cursor glue")
        assert(extra.contains(".p-uic-dialog-movable"), "movable box no-transition glue")
        assert(extra.contains(".p-uic-dialog-resize-handle"), "resize handle glue")
        assert(extra.contains(".p-uic-knob-dial { pointer-events: none; }"), "knob dial pointer-events glue")
    }
end GoldenRenderTest
