package kyo.uic.test

import kyo.uic.UicTest

/** Machine-checked IDE-discoverability contract: each component's setter surface
  * is gated by its concrete return type, so options that don't apply to a
  * component must not exist on it — invalid code fails to COMPILE, which is the
  * same information autocomplete uses to show only valid options after `.`.
  */
class DiscoverabilityTest extends UicTest:

    // `inline val` + constant-folded `+` keeps every argument to the inline `typeCheck`/
    // `typeCheckFailure` a compile-time constant, as they require.
    private inline val preamble =
        "import kyo.*\nimport kyo.UI.*\nimport kyo.uic\nimport scala.language.implicitConversions\n"

    "Button has no placeholder (not a text input)" in {
        typeCheckFailure(preamble + """def x = uic.Button("a").placeholder("b")""")
    }

    "Input has placeholder" in {
        typeCheck(preamble + """def x = uic.Input().placeholder("b")""")
    }

    "text slots accept a reactive Signal[String] as well as a constant String" in {
        // Button/Label/Tag labels + Input placeholder each take a Signal[String] overload
        // (for locale-driven I18n.t leaves) alongside the constant-String form.
        typeCheck(preamble + """def x(s: Signal[String]): uic.Button = uic.Button(s)""")
        typeCheck(preamble + """def x(s: Signal[String]): uic.Label = uic.Label(s)""")
        typeCheck(preamble + """def x(s: Signal[String]): uic.Tag = uic.Tag(s)""")
        typeCheck(preamble + """def x(s: Signal[String]): uic.Input = uic.Input().placeholder(s)""")
        // the constant-String forms keep working and keep the concrete type
        typeCheck(preamble + """def x: uic.Button = uic.Button("Save")""")
        typeCheck(preamble + """def x: uic.Tag = uic.Tag("Done")""")
        // a Signal placeholder still chains into the concrete Input type
        typeCheck(preamble + """def x(s: Signal[String]): uic.Input = uic.Input().placeholder(s).disabled(true)""")
    }

    "Card title/subtitle take a reactive Signal[String] alongside the constant String" in {
        typeCheck(preamble + """def x(s: Signal[String]): uic.Card = uic.Card().title(s).subtitle(s)""")
        typeCheck(preamble + """def x: uic.Card = uic.Card().title("Players").subtitle("2 joined")""")
    }

    "Button carries an id and a reactive disabled (Signal[Boolean]) alongside the constant" in {
        typeCheck(preamble + """def x: uic.Button = uic.Button("Save").id("save-btn")""")
        typeCheck(preamble + """def x(busy: Signal[Boolean]): uic.Button = uic.Button("Save").disabled(busy)""")
        typeCheck(preamble + """def x: uic.Button = uic.Button("Save").disabled(true)""")
        typeCheck(
            preamble + """def x(busy: Signal[Boolean]): uic.Button = uic.Button("Save").id("s").severity(uic.Severity.Secondary).disabled(busy)"""
        )
    }

    "Button renders as an SPA anchor via .href, and onClick accepts Abort[Throwable] & Async" in {
        typeCheck(preamble + """def x: uic.Button = uic.Button("Open").severity(uic.Severity.Secondary).href("/lobbies/1")""")
        // onClick mirrors kyo-ui's native event effect: an aborting handler is accepted...
        typeCheck(preamble + """def x(eff: Any < (Abort[Throwable] & Async))(using Frame): uic.Button = uic.Button("Go").onClick(eff)""")
        typeCheck(
            preamble + """def x(eff: Any < (Async & Abort[java.io.IOException]))(using Frame): uic.Button = uic.Button("Go").onClick(eff)"""
        )
        // ...and a pure Async handler still fits
        typeCheck(preamble + """def x(eff: Any < Async)(using Frame): uic.Button = uic.Button("Go").onClick(eff)""")
    }

    "Button has severity and variant, not the retired design" in {
        typeCheck(preamble + """def x = uic.Button("a").severity(uic.Severity.Danger).variant(uic.ButtonVariant.Outlined)""")
        typeCheckFailure(preamble + """def x = uic.Button("a").design(uic.ButtonDesign.Emphasized)""")
        typeCheckFailure(preamble + """def x = uic.Input().severity(uic.Severity.Danger)""")
    }

    "Label has no onClick (not interactive)" in {
        typeCheckFailure(preamble + """def x(a: Any < Async) = uic.Label("a").onClick(a)""")
    }

    "wrappingType is retired on migrated components (Label/CheckBox/RadioButton)" in {
        typeCheckFailure(preamble + """def x = uic.Label("a").wrappingType(uic.WrappingType.Normal)""")
        typeCheckFailure(preamble + """def x = uic.CheckBox("a").wrappingType(uic.WrappingType.Normal)""")
        typeCheckFailure(preamble + """def x = uic.RadioButton("a").wrappingType(uic.WrappingType.Normal)""")
    }

    "Dialog opens only via SignalRef (no plain Boolean)" in {
        typeCheckFailure(preamble + """def x = uic.Dialog().open(true)""")
    }

    "chained setters keep the concrete type" in {
        typeCheck(preamble + """def x: uic.Button = uic.Button("a").severity(uic.Severity.Success).icon(uic.Icons.check).disabled(true)""")
    }

    "components place directly into kyo containers" in {
        typeCheck(preamble + """def x(using Frame): UI = div(uic.Button("a"), uic.Input(), span("raw"))""")
    }

    "Tag exposes Prime's surface: severity/rounded/icon, not the retired ui5 design" in {
        typeCheck(preamble + """def x = uic.Tag("Done").severity(uic.Severity.Success).rounded(true).icon(uic.Icons.check)""")
        typeCheckFailure(preamble + """def x = uic.Tag().design(uic.TagDesign.Positive)""")
        typeCheckFailure(preamble + """def x = uic.Tag().interactive(true)""")
        typeCheckFailure(preamble + """def x = uic.Tag().wrappingType(uic.WrappingType.Normal)""")
        typeCheckFailure(preamble + """def x = uic.Tag().placeholder("nope")""")
    }

    "Icon takes only IconGlyph (no bare string); IconDesign is retired" in {
        typeCheckFailure(preamble + """def x = uic.Icon("save")""")
        typeCheckFailure(preamble + """def x = uic.Icon(uic.Icons.check).design(uic.IconDesign.Positive)""")
    }

    "ProgressSpinner replaces BusyIndicator; Prime's spinner has no active/delay/text" in {
        typeCheck(preamble + """def x = uic.ProgressSpinner().size(uic.Size.Large).accessibleName("Loading")""")
        typeCheckFailure(preamble + """def x = uic.BusyIndicator()""")
        typeCheckFailure(preamble + """def x = uic.ProgressSpinner().active(true)""")
        typeCheckFailure(preamble + """def x = uic.ProgressSpinner().delay(500)""")
        typeCheckFailure(preamble + """def x = uic.ProgressSpinner().text("Loading…")""")
    }

    "CalendarLegend is retired" in {
        typeCheckFailure(preamble + """def x = uic.CalendarLegend()""")
        typeCheckFailure(preamble + """def x = uic.CalendarLegendItem()""")
    }

    "Title/Text keep level/size/maxLines; wrappingType is retired" in {
        typeCheck(preamble + """def x: uic.Title = uic.Title().level(uic.TitleLevel.H2).size(uic.TitleLevel.H4).wrap(false)""")
        typeCheck(preamble + """def x: uic.Text = uic.Text().maxLines(2).emptyIndicatorMode(uic.TextEmptyIndicatorMode.On)""")
        typeCheckFailure(preamble + """def x = uic.Title().wrappingType(uic.WrappingType.None)""")
    }

    "CheckBox exposes its own options, not a text input's placeholder" in {
        typeCheck(preamble + """def x = uic.CheckBox("a").checked(true).disabled(true).invalid(true).size(uic.Size.Large)""")
        typeCheckFailure(preamble + """def x = uic.CheckBox("a").placeholder("b")""")
    }

    "ToggleSwitch replaces Switch; Prime has no state text or design" in {
        typeCheck(preamble + """def x = uic.ToggleSwitch().checked(true).disabled(true).invalid(true)""")
        typeCheckFailure(preamble + """def x = uic.Switch()""")
        typeCheckFailure(preamble + """def x = uic.ToggleSwitch().textOn("On")""")
        typeCheckFailure(preamble + """def x = uic.ToggleSwitch().design(uic.SwitchDesign.Graphical)""")
    }

    "CheckBox has indeterminate; RadioButton does not (tri-state is checkbox-only)" in {
        typeCheck(preamble + """def x = uic.CheckBox("a").indeterminate(true).name("form-field")""")
        typeCheck(preamble + """def x = uic.RadioButton("a").name("grp")""")
        typeCheckFailure(preamble + """def x = uic.RadioButton("a").indeterminate(true)""")
    }

    "Select carries TYPED options: options(Seq)(label) + optionKey; value only via SignalRef" in {
        typeCheck(preamble + """def x(r: SignalRef[String]): uic.Select[String] = uic.Select[String]().options(Seq("A", "B")).value(r)""")
        typeCheck(
            preamble + """def x(r: SignalRef[String]) = uic.Select[(String, String)]().options(Seq("a" -> "A"))(_._2).optionKey(_._1).optionDisabled(_ => false).value(r)"""
        )
        typeCheckFailure(preamble + """def x = uic.Select[String]().value("a")""")
        typeCheckFailure(preamble + """def x(r: SignalRef[String]) = uic.Select[String]().selected(r)""")
        typeCheckFailure(preamble + """def x = uic.Select[String]().option("a", "A")""")
        typeCheckFailure(preamble + """def x = uic.SelectOption("a", "A")""")
    }

    "AutoComplete replaces ComboBox: typed options, itemTemplate, minQueryLength, showClear" in {
        typeCheck(
            preamble + """def x(r: SignalRef[String]): uic.AutoComplete[String] = uic.AutoComplete[String]().options(Seq("A")).value(r).filter(uic.FilterMode.Contains).minQueryLength(2).showClear(true)"""
        )
        typeCheck(
            preamble + """def x(f: (String, String) => Any < Async)(using Frame) = uic.AutoComplete[(String, String)]().options(Seq("a" -> "A"))(_._2).itemTemplate(o => span(o._1)).onSelect(o => f(o._1, o._2))"""
        )
        typeCheckFailure(preamble + """def x = uic.ComboBox()""")
        typeCheckFailure(preamble + """def x = uic.ComboBoxFilter.Contains""")
        typeCheckFailure(preamble + """def x = uic.AutoComplete[String]().option("a", "A")""")
        typeCheckFailure(preamble + """def x(r: SignalRef[String]) = uic.AutoComplete[String]().selected(r)""")
    }

    "AutoComplete has the dropdown trigger; DatePicker has inline (typed Boolean)" in {
        typeCheck(preamble + """def x = uic.AutoComplete[String]().options(Seq("A")).dropdown(true)""")
        typeCheck(preamble + """def x: uic.DatePicker = uic.DatePicker().inline(true)""")
        typeCheckFailure(preamble + """def x = uic.DatePicker().inline("yes")""")
    }

    "Tooltip is CSS-driven: content + typed position, no open ref and no JS timer props" in {
        typeCheck(preamble + """def x(using Frame) = uic.Tooltip("hint").position(uic.TooltipPosition.Bottom)(span("t"))""")
        typeCheck(preamble + """def x(using Frame) = uic.Tooltip(span("rich"): UI)(span("t"))""")
        typeCheckFailure(preamble + """def x(r: SignalRef[Boolean]) = uic.Tooltip("hint").open(r)""")
        typeCheckFailure(preamble + """def x = uic.Tooltip("hint").showDelay(100)""")
        typeCheckFailure(preamble + """def x = uic.Tooltip("hint").autoHide(false)""")
    }

    "Popover opens only via SignalRef; dismissable/seedFocus/anchor; no JS-only props" in {
        typeCheck(
            preamble + """def x(r: SignalRef[Boolean])(using Frame): uic.Popover = uic.Popover(r).trigger(span("open")).dismissable(false).seedFocus(false).anchor(uic.OverlayAnchor.TopEnd)"""
        )
        typeCheckFailure(preamble + """def x = uic.Popover(true)""")
        typeCheckFailure(preamble + """def x(r: SignalRef[Boolean]) = uic.Popover(r).showCloseIcon(true)""")
    }

    "ValueState is retired everywhere; Dialog accents via severity, DatePicker validates via invalid" in {
        typeCheckFailure(preamble + """def x = uic.ValueState.Negative""")
        typeCheckFailure(preamble + """def x = uic.Dialog().state(uic.ValueState.Negative)""")
        typeCheck(preamble + """def x = uic.Dialog().severity(uic.Severity.Danger).maximized(true)""")
        typeCheckFailure(preamble + """def x = uic.Dialog().stretch(true)""")
        typeCheck(preamble + """def x = uic.DatePicker().invalid(true).invalidMessage("m").showWeek(true)""")
        typeCheckFailure(preamble + """def x = uic.DatePicker().valueState(uic.ValueState.Negative)""")
        typeCheckFailure(preamble + """def x = uic.DatePicker().hideWeekNumbers(true)""")
    }

    "TextArea has rows/autoResize; Input does not have rows" in {
        typeCheck(preamble + """def x = uic.TextArea().rows(4).autoResize(true)""")
        typeCheckFailure(preamble + """def x = uic.Input().rows(4)""")
        typeCheckFailure(preamble + """def x = uic.TextArea().growing(true)""")
    }

    "migrated form fields validate via invalid/invalidMessage, not valueState" in {
        typeCheck(preamble + """def x = uic.Input().invalid(true).invalidMessage("m").variant(uic.FieldVariant.Filled).fluid(true)""")
        typeCheck(preamble + """def x = uic.TextArea().invalid(true).invalidMessage("m").size(uic.Size.Small)""")
        typeCheckFailure(preamble + """def x = uic.Input().valueState(uic.ValueState.Negative)""")
        typeCheckFailure(preamble + """def x = uic.TextArea().valueState(uic.ValueState.Negative)""")
        typeCheckFailure(preamble + """def x = uic.CheckBox("a").valueState(uic.ValueState.Negative)""")
        typeCheckFailure(preamble + """def x = uic.RadioButton("a").valueState(uic.ValueState.Negative)""")
    }

    "DatePicker opens only via SignalRef[Boolean] (no plain Boolean)" in {
        typeCheck(preamble + """def x(o: SignalRef[Boolean]) = uic.DatePicker().open(o)""")
        typeCheckFailure(preamble + """def x = uic.DatePicker().open(true)""")
    }

    "new controls' chained setters keep the concrete type" in {
        typeCheck(preamble + """def x: uic.CheckBox = uic.CheckBox("a").disabled(true).invalid(true)""")
        typeCheck(preamble + """def x: uic.DatePicker = uic.DatePicker().placeholder("p").disabled(true).referenceDate("2024-01-01")""")
    }

    "Link keeps href/wrap/icon; the retired ui5 designs and interactiveAreaSize are gone" in {
        typeCheck(preamble + """def x = uic.Link().href("/h").disabled(true).wrap(true).icon(uic.Icons.check).endIcon(uic.Icons.check)""")
        typeCheckFailure(preamble + """def x = uic.Link().value("b")""")
        typeCheckFailure(preamble + """def x = uic.Link().design(uic.LinkDesign.Emphasized)""")
        typeCheckFailure(preamble + """def x = uic.Link().wrapping(true)""")
        typeCheckFailure(preamble + """def x = uic.Link().interactiveAreaSize(uic.InteractiveAreaSize.Large)""")
    }

    "Avatar has no href (not a link)" in {
        typeCheck(preamble + """def x = uic.Avatar().initials("AL").size(uic.Size.Large).shape(uic.AvatarShape.Square)""")
        typeCheckFailure(preamble + """def x = uic.Avatar().href("/x")""")
    }

    "ProgressBar.value accepts Int or SignalRef[Int]; ui5's valueState/disabled are retired" in {
        typeCheck(preamble + """def x = uic.ProgressBar().value(40)""")
        typeCheck(preamble + """def x(r: SignalRef[Int]) = uic.ProgressBar().value(r)""")
        typeCheckFailure(preamble + """def x = uic.ProgressBar().value("x")""")
        typeCheckFailure(preamble + """def x = uic.ProgressBar().valueState(uic.ValueState.Negative)""")
        typeCheckFailure(preamble + """def x = uic.ProgressBar().disabled(true)""")
        typeCheckFailure(preamble + """def x = uic.ProgressBar().hideValue(true)""")
        typeCheckFailure(preamble + """def x = uic.ProgressIndicator()""")
    }

    "Message replaces MessageStrip: severity/closable, no ui5 design or colorScheme" in {
        typeCheck(
            preamble + """def x = uic.Message().severity(uic.Severity.Warn).variant(uic.MessageVariant.Outlined).size(uic.Size.Small).closable(true)"""
        )
        typeCheckFailure(preamble + """def x = uic.MessageStrip()""")
        typeCheckFailure(preamble + """def x = uic.Message().design(uic.MessageStripDesign.Positive)""")
        typeCheckFailure(preamble + """def x = uic.Message().colorScheme(3)""")
        typeCheckFailure(preamble + """def x = uic.Message().hideCloseButton(true)""")
    }

    "Panel is toggleable (not ui5-fixed); sticky/noAnimation are retired" in {
        typeCheck(
            preamble + """def x(r: SignalRef[Boolean])(using Frame) = uic.Panel().header("H").toggleable(true).collapsed(r).footer(span("f"))"""
        )
        typeCheckFailure(preamble + """def x = uic.Panel().fixed(true)""")
        typeCheckFailure(preamble + """def x = uic.Panel().stickyHeader(true)""")
        typeCheckFailure(preamble + """def x = uic.Panel().noAnimation(true)""")
    }

    "Avatar sizes via Size (AvatarSize retired); image is native" in {
        typeCheck(preamble + """def x = uic.Avatar().initials("AL").size(uic.Size.Large).shape(uic.AvatarShape.Circle).image("/a.png")""")
        typeCheckFailure(preamble + """def x = uic.Avatar().size(uic.AvatarSize.L)""")
        typeCheckFailure(preamble + """def x = uic.Avatar().colorScheme("Accent1")""")
    }

    "Size.XLarge reaches only the components the PrimeOne sheet gives an -xl rule" in {
        // Avatar (.p-avatar-xl) and Badge (.p-badge-xl) are the whole set; their setters
        // widen to ExtendedSize.
        typeCheck(preamble + """def x: uic.Avatar = uic.Avatar().initials("AL").size(uic.Size.XLarge)""")
        typeCheck(preamble + """def x: uic.Badge = uic.Badge("3").size(uic.Size.XLarge)""")
        // Everything else takes the standard three-case Size, so an extra-large request
        // that would silently render as Large does not compile.
        typeCheckFailure(preamble + """def x = uic.Button("Save").size(uic.Size.XLarge)""")
        typeCheckFailure(preamble + """def x = uic.Input().size(uic.Size.XLarge)""")
        typeCheckFailure(preamble + """def x = uic.Message().size(uic.Size.XLarge)""")
        typeCheckFailure(preamble + """def x = uic.CheckBox("a").size(uic.Size.XLarge)""")
        typeCheckFailure(preamble + """def x = uic.DataTable[String]().size(uic.Size.XLarge)""")
        typeCheckFailure(preamble + """def x = uic.ProgressSpinner().size(uic.Size.XLarge)""")
        // The three standard cases keep reaching both scales.
        typeCheck(preamble + """def x: uic.Avatar = uic.Avatar().size(uic.Size.Small)""")
        typeCheck(preamble + """def x: uic.Button = uic.Button("Save").size(uic.Size.Large)""")
    }

    "Toolbar absorbs Bar: start/center/end sections, no spacer/design; Bar is retired" in {
        typeCheck(preamble + """def x(using Frame): uic.Toolbar = uic.Toolbar().start(span("a")).center(span("b")).end(span("c"))""")
        typeCheckFailure(preamble + """def x = uic.Bar()""")
        typeCheckFailure(preamble + """def x = uic.Toolbar().spacer()""")
        typeCheckFailure(preamble + """def x = uic.Toolbar().design(uic.ToolbarDesign.Solid)""")
        typeCheckFailure(preamble + """def x = uic.Toolbar().content(span("a"))""")
    }

    "Toast opens only via SignalRef[Boolean]; positions via OverlayPosition (ToastPlacement retired)" in {
        typeCheck(
            preamble + """def x(o: SignalRef[Boolean]) = uic.Toast().open(o).position(uic.OverlayPosition.BottomRight).severity(uic.Severity.Success).summary("s").detail("d").closable(true).duration(2000)"""
        )
        typeCheckFailure(preamble + """def x = uic.Toast().open(true)""")
        typeCheckFailure(preamble + """def x = uic.ToastPlacement.TopEnd""")
        typeCheckFailure(preamble + """def x(o: SignalRef[Boolean]) = uic.Toast().open(o).placement(uic.OverlayPosition.TopRight)""")
    }

    "feedback/nav controls' chained setters keep the concrete type" in {
        typeCheck(
            preamble + """def x: uic.ProgressBar = uic.ProgressBar().value(40).mode(uic.ProgressBarMode.Indeterminate).showValue(false)"""
        )
        typeCheck(preamble + """def x(using Frame): uic.Toolbar = uic.Toolbar().start(span("a")).end(span("b")).accessibleName("bar")""")
        typeCheck(
            preamble + """def x: uic.Breadcrumb = uic.Breadcrumb().home(uic.Icons.home, "/").item("H", "/").item("Now").separator(uic.Icons.angleRight)"""
        )
        typeCheck(preamble + """def x: uic.Message = uic.Message().severity(uic.Severity.Success).closable(true)""")
    }

    // ---- structural / composite controls (Phase 03) ----

    "Breadcrumb replaces Breadcrumbs; ui5 designs and string separators are retired" in {
        typeCheckFailure(preamble + """def x = uic.Breadcrumbs()""")
        typeCheckFailure(preamble + """def x = uic.Breadcrumb().separator(">")""")
        typeCheckFailure(preamble + """def x = uic.Breadcrumb().separators(uic.BreadcrumbsSeparator.GreaterThan)""")
        typeCheckFailure(preamble + """def x = uic.Breadcrumb().design(uic.BreadcrumbsDesign.NoCurrentPage)""")
    }

    "Listbox replaces ItemList: items/selection, no Table columns, no Tree nodes, no ui5 riches" in {
        typeCheck(
            preamble + """def x = uic.Listbox().item("A", "a").selectionMode(uic.SelectionMode.Multiple).checkmark(true).emptyMessage("m")"""
        )
        typeCheckFailure(preamble + """def x = uic.ItemList()""")
        typeCheckFailure(preamble + """def x = uic.Listbox().columns()""")
        typeCheckFailure(preamble + """def x = uic.Listbox().nodes()""")
        typeCheckFailure(preamble + """def x = uic.Listbox().headerText("h")""")
        typeCheckFailure(preamble + """def x = uic.Listbox().separators(uic.ListSeparators.Inner)""")
        typeCheckFailure(preamble + """def x = uic.Listbox().indent(true)""")
        typeCheckFailure(preamble + """def x = uic.Listbox().onItemDelete((_: String) => ())""")
        typeCheckFailure(preamble + """def x = uic.ListItem("A", id = "a", itemType = uic.ListItemType.Navigation)""")
    }

    "Listbox.filter takes a SignalRef[String] (server-side filtering), not a Boolean" in {
        typeCheck(preamble + """def x(q: SignalRef[String]) = uic.Listbox().filter(q).item("A", "a")""")
        typeCheckFailure(preamble + """def x = uic.Listbox().filter(true)""")
    }

    "DataTable replaces Table: typed rows/columns; the old UI-cell API is retired" in {
        typeCheck(
            preamble +
                """final case class P(id: String, name: String, price: Int)
def x(sort: SignalRef[List[(String, Boolean)]], q: SignalRef[String], pg: SignalRef[Int], sel: SignalRef[Set[String]])(using Frame) =
  uic.DataTable[P]()
    .rows(Seq(P("1", "A", 1)))
    .rowKey(_.id)
    .columns(
      uic.Column[P]("Name")(_.name).sortBy(_.name),
      uic.Column[P]("Price").body(p => span(p.price.toString)).sortBy(_.price).align(uic.ColumnAlign.End)
    )
    .sort(sort).globalFilter(q).paginate(10)(pg)
    .selectionMode(uic.SelectionMode.Checkbox).selected(sel)
    .stripedRows(true).showGridlines(true).size(uic.Size.Small)"""
        )
        typeCheckFailure(preamble + """def x = uic.Table()""")
        typeCheckFailure(preamble + """def x = uic.TableColumn("H")""")
        typeCheckFailure(preamble + """def x = uic.TableRow(Nil, "id")""")
        typeCheckFailure(preamble + """def x = uic.TableCell()""")
        typeCheckFailure(preamble + """def x = uic.ListSelectionMode.Multi""")
        typeCheckFailure(preamble + """def x = uic.SortOrder.Ascending""")
        typeCheckFailure(preamble + """def x = uic.DataTable[String]().row(span("a"))""")
        typeCheckFailure(preamble + """def x = uic.DataTable[String]().nodes()""")
        typeCheckFailure(preamble + """def x = uic.DataTable[String]().item("A", "a")""")
    }

    "Tree exposes nodes/expansion on the shared SelectionMode; the ui5 riches are retired" in {
        typeCheck(
            preamble + """def x = uic.Tree().nodes(uic.TreeNode("R", "r")).selectionMode(uic.SelectionMode.Multiple).emptyMessage("m")"""
        )
        typeCheckFailure(preamble + """def x = uic.Tree().columns()""")
        typeCheckFailure(preamble + """def x = uic.Tree().item("A", "a")""")
        typeCheckFailure(preamble + """def x = uic.Tree().headerText("h")""")
        typeCheckFailure(preamble + """def x = uic.Tree().selectionMode(uic.ListSelectionMode.Multi)""")
        typeCheckFailure(preamble + """def x = uic.TreeNode("R", "r", additionalText = Present("x"))""")
        typeCheckFailure(preamble + """def x = uic.TreeNode("R", "r", navigated = true)""")
    }

    "Tabs replaces TabContainer; the ui5 designs/layouts/collapsed are retired" in {
        typeCheck(
            preamble + """def x(r: SignalRef[String])(using Frame) = uic.Tabs().tabs(uic.Tab("T", span("c"), "t", additionalText = Present("3"))).selected(r)"""
        )
        typeCheckFailure(preamble + """def x = uic.TabContainer()""")
        typeCheckFailure(preamble + """def x = uic.Tabs().collapsed(true)""")
        typeCheckFailure(preamble + """def x = uic.Tabs().tabLayout(uic.TabLayout.Inline)""")
        typeCheckFailure(preamble + """def x = uic.Tabs().headerBackgroundDesign(uic.BackgroundDesign.Transparent)""")
        typeCheckFailure(preamble + """def x(using Frame) = uic.Tab("T", span("c"), "t", design = uic.SemanticColor.Negative)""")
    }

    "Tabs.selected accepts only SignalRef[String]; Listbox.selected only SignalRef[Set[String]]" in {
        typeCheck(preamble + """def x(r: SignalRef[String]) = uic.Tabs().selected(r)""")
        typeCheckFailure(preamble + """def x(r: SignalRef[Set[String]]) = uic.Tabs().selected(r)""")
        typeCheck(preamble + """def x(r: SignalRef[Set[String]]) = uic.Listbox().selected(r)""")
        typeCheckFailure(preamble + """def x(r: SignalRef[String]) = uic.Listbox().selected(r)""")
    }

    "Tree binds two Set refs (expanded + selected); Listbox selection is not a bare String" in {
        typeCheck(preamble + """def x(r: SignalRef[Set[String]]) = uic.Tree().expanded(r).selected(r)""")
        typeCheckFailure(preamble + """def x(r: SignalRef[Boolean]) = uic.Tree().expanded(r)""")
        typeCheckFailure(preamble + """def x = uic.Listbox().selected(Set("a"))""")
    }

    "structural controls' chained setters keep the concrete type" in {
        typeCheck(preamble + """def x: uic.Listbox = uic.Listbox().item("A", "a").selectionMode(uic.SelectionMode.Single)""")
        typeCheck(
            preamble + """def x: uic.DataTable[String] = uic.DataTable[String]().rows(Seq("a")).columns(uic.Column[String]("H")(identity))"""
        )
        typeCheck(preamble + """def x(r: SignalRef[String]): uic.Tabs = uic.Tabs().selected(r)""")
        typeCheck(preamble + """def x(r: SignalRef[Set[String]]): uic.Tree = uic.Tree().expanded(r).selected(r)""")
    }

    // ---- wave E: new Tier-1 components ----

    "wave-E components' chained setters keep the concrete type" in {
        typeCheck(
            preamble + """def x: uic.Skeleton = uic.Skeleton().shape(uic.SkeletonShape.Circle).size("4rem").borderRadius("16px").animation(false)"""
        )
        typeCheck(
            preamble + """def x: uic.Divider = uic.Divider().layout(uic.DividerLayout.Vertical).lineStyle(uic.DividerLineStyle.Dashed).align(uic.DividerAlign.Center)"""
        )
        typeCheck(preamble + """def x(r: SignalRef[Boolean]): uic.Fieldset = uic.Fieldset().legend("L").toggleable(true).collapsed(r)""")
        typeCheck(preamble + """def x: uic.Badge = uic.Badge("3").severity(uic.Severity.Danger).size(uic.Size.XLarge)""")
        typeCheck(
            preamble + """def x(using Frame): uic.OverlayBadge = uic.OverlayBadge(uic.Avatar().initials("A")).badge(uic.Badge("2"))"""
        )
        typeCheck(preamble + """def x(using Frame): uic.Chip = uic.Chip("Apple").icon(uic.Icons.check).removable(true).onRemove(())""")
        typeCheck(preamble + """def x: uic.AvatarGroup = uic.AvatarGroup(uic.Avatar().initials("A"), uic.Avatar().initials("B"))""")
        typeCheck(
            preamble + """def x: uic.MeterGroup = uic.MeterGroup().meter("Apps", 16).meter("Messages", 8, "var(--p-cyan-500)").max(200)"""
        )
        typeCheck(
            preamble + """def x(r: SignalRef[Boolean])(using Frame): uic.Inplace = uic.Inplace().display(span("view")).content(span("edit")).active(r).closable(true)"""
        )
        typeCheck(preamble + """def x: uic.ScrollPanel = uic.ScrollPanel().width("100%").height("12rem")""")
    }

    // ---- wave F: the final ten Tier-1 components ----

    "wave-F components' chained setters keep the concrete type" in {
        typeCheck(preamble + """def x(r: SignalRef[Int]): uic.Rating = uic.Rating().value(r).stars(10).readonly(true)""")
        typeCheck(
            preamble + """def x(r: SignalRef[Boolean]): uic.ToggleButton = uic.ToggleButton().checked(r).onLabel("On").offLabel("Off").onIcon(uic.Icons.check).offIcon(uic.Icons.times).size(uic.Size.Small).invalid(true)"""
        )
        typeCheck(
            preamble + """def x(r: SignalRef[String]): uic.SelectButton[(String, String)] = uic.SelectButton[(String, String)]().options(Seq("a" -> "A"))(_._2).optionKey(_._1).optionDisabled(_ => false).value(r).allowEmpty(false).accessibleName("Language").onChange(_ => ())"""
        )
        typeCheck(
            preamble + """def x(r: SignalRef[Set[String]]): uic.SelectButton[String] = uic.SelectButton[String]().options(Seq("S", "M")).multiple(true).values(r)"""
        )
        typeCheck(preamble + """def x(using Frame): uic.InputGroup = uic.InputGroup()(uic.InputGroup.addon(span("$")), uic.Input())""")
        typeCheck(preamble + """def x: uic.IconField = uic.IconField(uic.Input()).iconStart(uic.Icons.search).iconEnd(uic.Icons.spinner)""")
        typeCheck(
            preamble + """def x: uic.FloatLabel = uic.FloatLabel(uic.Input().id("u"), "Username").variant(uic.FloatLabelVariant.In).forId("u")"""
        )
        typeCheck(preamble + """def x: uic.IftaLabel = uic.IftaLabel(uic.Input(), "Username").forId("u")""")
        typeCheck(
            preamble + """def x(using Frame): uic.Timeline[String] = uic.Timeline[String]().events(Seq("a")).content(span(_)).opposite(span(_)).marker(_ => span("m")).align(uic.TimelineAlign.Alternate).layout(uic.TimelineLayout.Horizontal)"""
        )
        typeCheck(
            preamble + """def x(r: SignalRef[Int]): uic.Paginator = uic.Paginator().totalRecords(120).rows(10).page(r).onPage(_ => ())"""
        )
        typeCheck(
            preamble + """def x(r: SignalRef[Int])(using Frame): uic.DataView[String] = uic.DataView[String]().items(Seq("a")).itemTemplate(s => span(s)).gridItemTemplate(s => span(s)).layout(uic.DataViewLayout.Grid).header(span("h")).footer(span("f")).paginate(5)(r).emptyMessage("none")"""
        )
        typeCheck(
            preamble + """def x(r: SignalRef[Int])(using Frame): uic.Stepper = uic.Stepper().active(r).linear(true).step("One")(p("c1")).step("Two")(p("c2"))"""
        )
    }

    "wave-F selection state binds only via SignalRef (no bare values)" in {
        typeCheckFailure(preamble + """def x = uic.SelectButton[String]().options(Seq("a")).value("a")""")
        typeCheckFailure(preamble + """def x = uic.SelectButton[String]().options(Seq("a")).values(Set("a"))""")
        typeCheckFailure(preamble + """def x = uic.Paginator().page(0)""")
        typeCheckFailure(preamble + """def x = uic.Stepper().active(0)""")
    }

    "wave-F components expose only their own options" in {
        typeCheckFailure(preamble + """def x = uic.Rating().placeholder("p")""")
        typeCheckFailure(preamble + """def x = uic.Rating().severity(uic.Severity.Danger)""")
        typeCheckFailure(preamble + """def x = uic.ToggleButton().options(Seq("a"))""")
        typeCheckFailure(preamble + """def x = uic.SelectButton[String]().option("a", "A")""")
        typeCheckFailure(preamble + """def x = uic.InputGroup().iconStart(uic.Icons.search)""")
        typeCheckFailure(preamble + """def x = uic.IconField(uic.Input()).addon(span("x"))""")
        typeCheckFailure(preamble + """def x = uic.FloatLabel(uic.Input(), "L").iconStart(uic.Icons.search)""")
        typeCheckFailure(preamble + """def x = uic.Timeline[String]().rows(Seq("a"))""")
        typeCheckFailure(preamble + """def x = uic.DataView[String]().columns()""")
        typeCheckFailure(preamble + """def x = uic.Stepper().tab("T", "t")""")
    }

    // ---- wave G: deepened DatePicker / Paginator / Stepper / MeterGroup ----

    "wave-G deepened surfaces typecheck" in {
        typeCheck(preamble + """def x(v: SignalRef[Set[String]]): uic.DatePicker = uic.DatePicker().values(v)""")
        typeCheck(preamble + """def x(s: SignalRef[String], e: SignalRef[String]): uic.DatePicker = uic.DatePicker().range(s, e)""")
        typeCheck(
            preamble + """def x(cv: SignalRef[uic.DatePickerView]): uic.DatePicker = uic.DatePicker().view(uic.DatePickerView.Month).currentView(cv)"""
        )
        typeCheck(
            preamble + """def x: uic.DatePicker = uic.DatePicker().showTime(true).hourFormat(uic.HourFormat.H12).showButtonBar(true).today("2026-07-16")"""
        )
        typeCheck(preamble + """def x: uic.DatePicker = uic.DatePicker().timeOnly(true)""")
        typeCheck(
            preamble + """def x(pg: SignalRef[Int], rows: SignalRef[Int]): uic.Paginator = uic.Paginator().totalRecords(120).rows(rows).page(pg).pageLinkSize(3).rowsPerPageOptions(Seq(5, 10)).currentPageReport("{currentPage} of {totalPages}").jumpToPageInput(true)"""
        )
        typeCheck(
            preamble + """def x(v: SignalRef[String])(using Frame): uic.Stepper = uic.Stepper().vertical(true).value(v).step("One", value = Present("one"))(p("c1")).step("Two", disabled = true)(p("c2"))"""
        )
        typeCheck(
            preamble + """def x: uic.MeterGroup = uic.MeterGroup().orientation(uic.Orientation.Vertical).labelOrientation(uic.Orientation.Vertical).labelPosition(uic.LabelPosition.Start).meter("A", 10, uic.Icons.check).meter("B", 10, "var(--p-cyan-500)", uic.Icons.user)"""
        )
        typeCheck(
            preamble + """def x(using Frame): uic.MeterGroup = uic.MeterGroup().meter("A", 10).startTemplate(span("s")).endTemplate(span("e")).meterTemplate((m, pc) => span(m.label)).labelTemplate((m, pc) => span(s"${m.label} ${math.round(pc)}%"))"""
        )
    }

    "wave-G surfaces stay typed: no string views/orientations, refs not bare values" in {
        typeCheckFailure(preamble + """def x = uic.DatePicker().view("month")""")
        typeCheckFailure(preamble + """def x = uic.DatePicker().values(Set("2026-01-01"))""")
        typeCheckFailure(preamble + """def x = uic.DatePicker().range("2026-01-01", "2026-01-05")""")
        typeCheckFailure(preamble + """def x = uic.DatePicker().hourFormat("12")""")
        typeCheckFailure(preamble + """def x = uic.Paginator().rowsPerPageOptions(Seq("10"))""")
        typeCheckFailure(preamble + """def x(v: SignalRef[Int]) = uic.Stepper().value(v)""")
        typeCheckFailure(preamble + """def x = uic.MeterGroup().orientation("vertical")""")
        typeCheckFailure(preamble + """def x = uic.MeterGroup().labelPosition("start")""")
    }

    // ---- wave H: PrimeReact/PrimeVue feature-gap closure ----

    "wave-H deepened surfaces typecheck" in {
        typeCheck(
            preamble + """def x(r: SignalRef[Int]): uic.Rating = uic.Rating().value(r).name("score").onIcon(uic.Icons.heartFill).offIcon(uic.Icons.heart)"""
        )
        typeCheck(preamble + """def x: uic.ToggleButton = uic.ToggleButton().fluid(true).readonly(true)""")
        typeCheck(
            preamble + """def x(r: SignalRef[String])(using Frame): uic.SelectButton[(String, String)] = uic.SelectButton[(String, String)]().options(Seq("a" -> "A"))(_._2).optionKey(_._1).itemTemplate(o => span(o._2)).value(r)"""
        )
        typeCheck(preamble + """def x: uic.IconField = uic.IconField(uic.TextArea().placeholder("Notes")).iconStart(uic.Icons.search)""")
        typeCheck(preamble + """def x: uic.IconField = uic.IconField(uic.Select[String]().options(Seq("A"))).iconStart(uic.Icons.user)""")
        typeCheck(
            preamble + """def x(r: SignalRef[String]): uic.FloatLabel = uic.FloatLabel(uic.TextArea().id("m").value(r), "Message").forId("m")"""
        )
        typeCheck(
            preamble + """def x(r: SignalRef[String]): uic.FloatLabel = uic.FloatLabel(uic.Select[String]().options(Seq("A")).id("u").value(r), "Unit").forId("u")"""
        )
        typeCheck(
            preamble + """def x(r: SignalRef[String]): uic.FloatLabel = uic.FloatLabel(uic.AutoComplete[String]().options(Seq("A")).id("c").value(r), "City").variant(uic.FloatLabelVariant.In).forId("c")"""
        )
        typeCheck(preamble + """def x: uic.ToggleSwitch = uic.ToggleSwitch().checked(true).handleIcon(uic.Icons.check, uic.Icons.times)""")
        typeCheck(preamble + """def x(using Frame): uic.Chip = uic.Chip("Tag").removable(true).removeIcon(uic.Icons.times).onRemove(())""")
        typeCheck(
            preamble + """def x(r: SignalRef[Boolean])(using Frame): uic.Inplace = uic.Inplace().display(span("v")).content(span("e")).active(r).disabled(true).onOpen(()).onClose(())"""
        )
        typeCheck(
            preamble + """def x(using Frame): uic.DataView[String] = uic.DataView[String]().items(Seq("a")).itemTemplate(s => span(s)).loading(true)"""
        )
    }

    "wave-H surfaces stay typed: glyphs not strings, sorting stays a Seq concern" in {
        typeCheckFailure(preamble + """def x = uic.Rating().onIcon("pi pi-heart")""")
        typeCheckFailure(preamble + """def x = uic.ToggleSwitch().handleIcon("check", "times")""")
        typeCheckFailure(preamble + """def x = uic.Chip("a").removeIcon("pi pi-times")""")
        typeCheckFailure(preamble + """def x = uic.DataView[String]().sortField("name")""")
        typeCheckFailure(preamble + """def x = uic.DataView[String]().sortOrder(1)""")
        typeCheckFailure(preamble + """def x = uic.IconField(uic.Button("x"))""")
        typeCheckFailure(preamble + """def x = uic.FloatLabel(uic.CheckBox("a"), "L")""")
    }

    // ---- wave I: Overlay primitive + Select floating panel ----

    "Overlay opens only via SignalRef; chained setters keep the concrete type" in {
        typeCheck(
            preamble + """def x(o: SignalRef[Boolean])(using Frame): uic.Overlay = uic.Overlay(o).anchor(uic.OverlayAnchor.TopEnd).matchWidth(false).maxHeight("20rem").dismissOnOutsideClick(false).dismissOnEscape(false).seedFocus(false)(span("content"))"""
        )
        typeCheck(preamble + """def x(o: SignalRef[Boolean]): uic.Overlay = uic.Overlay(o).maxHeight(240)""")
        typeCheckFailure(preamble + """def x = uic.Overlay(true)""")
        typeCheckFailure(preamble + """def x(o: SignalRef[Boolean]) = uic.Overlay(o).placeholder("p")""")
        typeCheckFailure(preamble + """def x(o: SignalRef[Boolean]) = uic.Overlay(o).severity(uic.Severity.Danger)""")
    }

    "Select panel surface: filter/showClear/checkmark are Booleans, open only via SignalRef" in {
        typeCheck(
            preamble + """def x(r: SignalRef[String], o: SignalRef[Boolean]): uic.Select[String] = uic.Select[String]().options(Seq("A")).value(r).open(o).filter(true).showClear(true).checkmark(true).emptyMessage("none")"""
        )
        typeCheckFailure(preamble + """def x = uic.Select[String]().open(true)""")
        typeCheckFailure(preamble + """def x(q: SignalRef[String]) = uic.Select[String]().filter(q)""")
    }

    "wave-E components expose only their own options" in {
        typeCheckFailure(preamble + """def x = uic.Skeleton().severity(uic.Severity.Danger)""")
        typeCheckFailure(preamble + """def x = uic.Divider().placeholder("p")""")
        typeCheckFailure(preamble + """def x = uic.Fieldset().collapsed(true)""")
        typeCheckFailure(preamble + """def x = uic.Badge().rounded(true)""")
        typeCheckFailure(preamble + """def x = uic.Chip("a").severity(uic.Severity.Info)""")
        typeCheckFailure(preamble + """def x = uic.Inplace().active(true)""")
        typeCheckFailure(preamble + """def x = uic.MeterGroup().value(40)""")
        typeCheckFailure(preamble + """def x = uic.ScrollPanel().onClick(())""")
    }

    // ---- wave K: the menu family ----

    "MenuItem is the shared typed model: icon/disabled/url/onSelect/items + the separator row" in {
        typeCheck(
            preamble + """def x(using Frame): uic.MenuItem = uic.MenuItem("Save").icon(uic.Icons.check).disabled(true).url("/save").onSelect(()).items(uic.MenuItem("Nested"))"""
        )
        typeCheck(preamble + """def x: uic.MenuItem = uic.MenuItem.separator""")
        typeCheckFailure(preamble + """def x = uic.MenuItem("a").severity(uic.Severity.Danger)""")
        typeCheckFailure(preamble + """def x = uic.MenuItem("a").command(() => ())""")
    }

    "Menu/TieredMenu popup only via SignalRef; items are MenuItem (no string rows)" in {
        typeCheck(
            preamble + """def x(o: SignalRef[Boolean])(using Frame): uic.Menu = uic.Menu().items(uic.MenuItem("New"), uic.MenuItem.separator).popup(o)"""
        )
        typeCheck(
            preamble + """def x(o: SignalRef[Boolean])(using Frame): uic.TieredMenu = uic.TieredMenu().items(uic.MenuItem("File").items(uic.MenuItem("New"))).popup(o)"""
        )
        typeCheckFailure(preamble + """def x = uic.Menu().popup(true)""")
        typeCheckFailure(preamble + """def x = uic.TieredMenu().popup(true)""")
        typeCheckFailure(preamble + """def x = uic.Menu().items("New")""")
        typeCheckFailure(preamble + """def x = uic.Menu().item("New", "n")""")
    }

    "Menubar has start/end slots and MenuItem rows; MegaMenu takes MegaMenuItem columns of MenuGroups" in {
        typeCheck(
            preamble + """def x(using Frame): uic.Menubar = uic.Menubar().start(span("logo")).end(span("x")).items(uic.MenuItem("File").items(uic.MenuItem("New")))"""
        )
        typeCheck(
            preamble + """def x(using Frame): uic.MegaMenu = uic.MegaMenu().orientation(uic.Orientation.Vertical).items(uic.MegaMenuItem("Furniture").icon(uic.Icons.box).column(uic.MenuGroup("Living Room").items(uic.MenuItem("Sofa"))))"""
        )
        typeCheckFailure(preamble + """def x = uic.MegaMenu().items(uic.MenuItem("x"))""")
        typeCheckFailure(preamble + """def x = uic.Menubar().items(uic.MegaMenuItem("x"))""")
        typeCheckFailure(preamble + """def x = uic.Menubar().model(Seq())""")
    }

    "SplitButton passes Button styling through; SpeedDial is typed on direction and refs" in {
        typeCheck(
            preamble + """def x(o: SignalRef[Boolean])(using Frame): uic.SplitButton = uic.SplitButton("Save").severity(uic.Severity.Danger).variant(uic.ButtonVariant.Outlined).size(uic.Size.Small).rounded(true).raised(true).fluid(true).icon(uic.Icons.check).onClick(()).items(uic.MenuItem("Update")).open(o)"""
        )
        typeCheck(
            preamble + """def x(o: SignalRef[Boolean])(using Frame): uic.SpeedDial = uic.SpeedDial().items(uic.MenuItem("Add").icon(uic.Icons.plus).onSelect(())).direction(uic.SpeedDialDirection.Down).open(o).disabled(true).accessibleName("Actions")"""
        )
        typeCheckFailure(preamble + """def x = uic.SplitButton("a").open(true)""")
        typeCheckFailure(preamble + """def x = uic.SplitButton("a").placeholder("p")""")
        typeCheckFailure(preamble + """def x = uic.SpeedDial().direction("up")""")
        typeCheckFailure(preamble + """def x = uic.SpeedDial().open(true)""")
        typeCheckFailure(preamble + """def x = uic.SpeedDial().mask(true)""")
    }

    // ---- wave L: MultiSelect / CascadeSelect / TreeSelect / Drawer ----

    "MultiSelect binds a Set ref, keeps the panel surface; single-value binding is rejected" in {
        typeCheck(
            preamble + """def x(r: SignalRef[Set[String]], o: SignalRef[Boolean]): uic.MultiSelect[String] = uic.MultiSelect[String]().options(Seq("A", "B")).value(r).open(o).filter(true).showClear(true).showToggleAll(false).highlightOnSelect(true).display(uic.MultiSelectDisplay.Chip).maxSelectedLabels(3).selectedItemsLabel("{0} picked").emptyMessage("none")"""
        )
        typeCheck(
            preamble + """def x(r: SignalRef[Set[String]]) = uic.MultiSelect[(String, String)]().options(Seq("a" -> "A"))(_._2).optionKey(_._1).optionDisabled(_ => false).value(r).onChange(_ => ())"""
        )
        typeCheckFailure(preamble + """def x(r: SignalRef[String]) = uic.MultiSelect[String]().value(r)""")
        typeCheckFailure(preamble + """def x = uic.MultiSelect[String]().value(Set("a"))""")
        typeCheckFailure(preamble + """def x = uic.MultiSelect[String]().open(true)""")
        typeCheckFailure(preamble + """def x = uic.MultiSelect[String]().checkmark(true)""")
    }

    "CascadeSelect takes a typed CascadeItem tree; flat options and bare values are rejected" in {
        typeCheck(
            preamble + """def x(r: SignalRef[String], o: SignalRef[Boolean]): uic.CascadeSelect[String] = uic.CascadeSelect[String]().options(Seq(uic.CascadeItem.group("G")(uic.CascadeItem.leaf("a"))))(identity).optionKey(identity).value(r).open(o).placeholder("p")"""
        )
        typeCheckFailure(preamble + """def x = uic.CascadeSelect[String]().options(Seq("A", "B"))""")
        typeCheckFailure(preamble + """def x = uic.CascadeSelect[String]().value("a")""")
        typeCheckFailure(preamble + """def x(r: SignalRef[String]) = uic.CascadeSelect[String]().filter(true)""")
    }

    "TreeSelect reuses TreeNode + SelectionMode and binds Set refs" in {
        typeCheck(
            preamble + """def x(r: SignalRef[Set[String]], e: SignalRef[Set[String]], o: SignalRef[Boolean]): uic.TreeSelect = uic.TreeSelect().nodes(uic.TreeNode("R", "r")).value(r).expanded(e).open(o).selectionMode(uic.SelectionMode.Checkbox).placeholder("p").emptyMessage("m")"""
        )
        typeCheckFailure(preamble + """def x(r: SignalRef[String]) = uic.TreeSelect().value(r)""")
        typeCheckFailure(preamble + """def x = uic.TreeSelect().open(true)""")
        typeCheckFailure(preamble + """def x = uic.TreeSelect().options(Seq("A"))""")
    }

    "Drawer opens only via SignalRef; position is typed; Overlay-only props are rejected" in {
        typeCheck(
            preamble + """def x(o: SignalRef[Boolean])(using Frame): uic.Drawer = uic.Drawer().open(o).position(uic.DrawerPosition.Right).header("H").footer(span("f")).dismissable(false).showCloseIcon(false).onClose(())(p("content"))"""
        )
        typeCheckFailure(preamble + """def x = uic.Drawer().open(true)""")
        typeCheckFailure(preamble + """def x(o: SignalRef[Boolean]) = uic.Drawer().open(o).position("left")""")
        typeCheckFailure(preamble + """def x(o: SignalRef[Boolean]) = uic.Drawer().open(o).anchor(uic.OverlayAnchor.TopEnd)""")
        typeCheckFailure(preamble + """def x(o: SignalRef[Boolean]) = uic.Drawer().open(o).maximized(true)""")
    }

    "ContextMenu wraps a target over typed MenuItems; open state is internal (no open/popup setter)" in {
        typeCheck(
            preamble + """def x(using Frame): uic.ContextMenu = uic.ContextMenu(Seq(uic.MenuItem("Copy").icon(uic.Icons.copy))).items(uic.MenuItem.separator)(div(span("target")))"""
        )
        typeCheckFailure(preamble + """def x(o: SignalRef[Boolean]) = uic.ContextMenu().open(o)""")
        typeCheckFailure(preamble + """def x(o: SignalRef[Boolean]) = uic.ContextMenu().popup(o)""")
        typeCheckFailure(preamble + """def x = uic.ContextMenu().placeholder("p")""")
    }

    "ToastService is an Env service: typed messages in, Env-typed region out; no visibility ref" in {
        typeCheck(
            preamble + """def x(svc: uic.ToastService)(using Frame): Unit < Async = svc.add(uic.ToastMessage(uic.Severity.Success, Present("S"), Present("D"), Present(3.seconds)))"""
        )
        typeCheck(preamble + """def x(using Frame): UI < Env[uic.ToastService] = uic.ToastService.render(uic.OverlayPosition.TopRight)""")
        typeCheck(preamble + """def x(using Frame): Layer[uic.ToastService, Sync] = uic.ToastService.layer""")
        typeCheckFailure(preamble + """def x(svc: uic.ToastService)(using Frame) = svc.add("just a string")""")
        typeCheckFailure(
            preamble + """def x(o: SignalRef[Boolean])(using Frame) = uic.ToastService.render(uic.OverlayPosition.TopRight).open(o)"""
        )
    }

    "ConfirmDialog opens only via SignalRef; Dialog-internals stay hidden" in {
        typeCheck(
            preamble + """def x(o: SignalRef[Boolean], a: Any < Async)(using Frame): uic.ConfirmDialog = uic.ConfirmDialog(o).header("H").message("M").icon(uic.Icons.exclamationTriangle).acceptLabel("Del").rejectLabel("Keep").acceptSeverity(uic.Severity.Danger).onAccept(a).onReject(a)"""
        )
        typeCheckFailure(preamble + """def x = uic.ConfirmDialog().open(true)""")
        typeCheckFailure(preamble + """def x(o: SignalRef[Boolean]) = uic.ConfirmDialog(o).maximized(true)""")
        typeCheckFailure(preamble + """def x(o: SignalRef[Boolean]) = uic.ConfirmDialog(o).footer(span("f"))""")
    }

    // ---- wave N: the form-input block ----

    "InputNumber is Double-typed with typed button layout; Prime's live formatting props are absent" in {
        typeCheck(
            preamble + """def x(r: SignalRef[Double])(using Frame): uic.InputNumber = uic.InputNumber().value(r).min(0).max(10).step(0.5).showButtons(true).buttonLayout(uic.InputNumberButtonLayout.Horizontal).prefix("$").suffix(" kg").fluid(true).invalid(true).onChange(_ => ())"""
        )
        typeCheckFailure(preamble + """def x(r: SignalRef[String]) = uic.InputNumber().value(r)""")
        typeCheckFailure(preamble + """def x = uic.InputNumber().buttonLayout("stacked")""")
        typeCheckFailure(preamble + """def x = uic.InputNumber().mode("currency")""")
        typeCheckFailure(preamble + """def x = uic.InputNumber().locale("de-DE")""")
        typeCheckFailure(preamble + """def x = uic.InputNumber().useGrouping(true)""")
    }

    "Password has toggleMask/feedback/meter labels; the overlay props are absent (inline meter)" in {
        typeCheck(
            preamble + """def x(r: SignalRef[String])(using Frame): uic.Password = uic.Password().value(r).toggleMask(true).feedback(true).promptLabel("Type").weakLabel("W").mediumLabel("M").strongLabel("S").placeholder("Password").fluid(true).invalid(true)"""
        )
        typeCheckFailure(preamble + """def x(r: SignalRef[Double]) = uic.Password().value(r)""")
        typeCheckFailure(preamble + """def x = uic.Password().appendTo("body")""")
        typeCheckFailure(preamble + """def x = uic.Password().overlayVisible(true)""")
        typeCheckFailure(preamble + """def x = uic.Password().severity(uic.Severity.Danger)""")
    }

    "InputOtp binds ONE string code over N cells; per-cell values are not addressable" in {
        typeCheck(
            preamble + """def x(r: SignalRef[String])(using Frame): uic.InputOtp = uic.InputOtp().value(r).length(6).integerOnly(true).mask(true).size(uic.Size.Large).invalid(true).onChange(_ => ())"""
        )
        typeCheckFailure(preamble + """def x(r: SignalRef[List[String]]) = uic.InputOtp().value(r)""")
        typeCheckFailure(preamble + """def x = uic.InputOtp().cellValue(0, "1")""")
        typeCheckFailure(preamble + """def x = uic.InputOtp().placeholder("0")""")
    }

    "Slider is Double-typed with typed orientation; the dual-handle range mode is deferred (absent)" in {
        typeCheck(
            preamble + """def x(r: SignalRef[Double])(using Frame): uic.Slider = uic.Slider().value(r).min(0).max(1).step(0.1).orientation(uic.Orientation.Vertical).disabled(true).accessibleName("Volume").onChange(_ => ())"""
        )
        typeCheckFailure(preamble + """def x(r: SignalRef[Int]) = uic.Slider().value(r)""")
        typeCheckFailure(preamble + """def x = uic.Slider().range(true)""")
        typeCheckFailure(preamble + """def x = uic.Slider().orientation("vertical")""")
    }

    "Knob draws from a Double value with dial options; pointer-drag props are absent (deferred)" in {
        typeCheck(
            preamble + """def x(r: SignalRef[Double])(using Frame): uic.Knob = uic.Knob().value(r).min(0).max(40).step(2).size(150).strokeWidth(8).showValue(false).valueTemplate(v => s"$v%").valueColor("var(--p-primary-color)").rangeColor("#dfe7ef").textColor("var(--p-text-color)").readonly(true).onChange(_ => ())"""
        )
        typeCheckFailure(preamble + """def x(r: SignalRef[String]) = uic.Knob().value(r)""")
        typeCheckFailure(preamble + """def x = uic.Knob().onDrag(() => ())""")
        typeCheckFailure(preamble + """def x = uic.Knob().placeholder("p")""")
    }

    // ---- wave O: the container/data block ----

    "Accordion: the value ref TYPE picks the mode (String = single, Set = multiple); no bare Boolean" in {
        typeCheck(
            preamble + """def x(r: SignalRef[String])(using Frame): uic.Accordion = uic.Accordion().panel("H", "h")(p("c")).value(r).onToggle(_ => ())"""
        )
        typeCheck(
            preamble + """def x(r: SignalRef[Set[String]])(using Frame): uic.Accordion = uic.Accordion().panels(uic.AccordionPanel("H", p("c"), "h", disabled = true)).value(r)"""
        )
        typeCheckFailure(preamble + """def x = uic.Accordion().value("h")""")
        typeCheckFailure(preamble + """def x = uic.Accordion().multiple(true)""")
        typeCheckFailure(preamble + """def x = uic.Accordion().placeholder("p")""")
    }

    "OrderList reorders a bound Seq ref with typed items; the selection drives the moves" in {
        typeCheck(
            preamble + """def x(items: SignalRef[Seq[String]], sel: SignalRef[Set[String]])(using Frame): uic.OrderList[String] = uic.OrderList[String]().items(items)(identity).itemKey(identity).itemTemplate(s => span(s)).selected(sel).disabled(false).accessibleName("List")"""
        )
        typeCheckFailure(preamble + """def x = uic.OrderList[String]().items(Seq("A"))""")
        typeCheckFailure(
            preamble + """def x(items: SignalRef[Seq[String]]): uic.OrderList[String] = uic.OrderList[String]().items(items)"""
        )
        typeCheckFailure(preamble + """def x = uic.OrderList[String]().onReorder(() => ())""")
    }

    "PickList binds two Seq columns + two selections; per-column rails are toggleable" in {
        typeCheck(
            preamble + """def x(s: SignalRef[Seq[String]], t: SignalRef[Seq[String]], ss: SignalRef[Set[String]], ts: SignalRef[Set[String]])(using Frame): uic.PickList[String] = uic.PickList[String]().source(s)(identity).target(t).itemKey(identity).itemTemplate(x => span(x)).sourceSelected(ss).targetSelected(ts).showSourceControls(false).showTargetControls(false).disabled(false)"""
        )
        typeCheckFailure(preamble + """def x = uic.PickList[String]().source(Seq("A"))""")
        typeCheckFailure(preamble + """def x(t: SignalRef[Seq[String]]) = uic.PickList[String]().target(t)(identity)""")
    }

    "Carousel pages a typed item window via an Int ref; JS-era transition props are absent" in {
        typeCheck(
            preamble + """def x(r: SignalRef[Int])(using Frame): uic.Carousel[String] = uic.Carousel[String]().items(Seq("A"))(s => span(s)).page(r).numVisible(3).numScroll(3).circular(true).vertical(true).verticalViewHeight("330px").showNavigators(false).showIndicators(false).autoplay(3000).onPageChange(_ => ())"""
        )
        typeCheckFailure(preamble + """def x = uic.Carousel[String]().page(0)""")
        typeCheckFailure(preamble + """def x = uic.Carousel[String]().autoplayInterval(3000)""")
        typeCheckFailure(preamble + """def x: uic.Carousel[String] = uic.Carousel[String]().items(Seq("A"))""")
    }

    "Galleria binds activeIndex via an Int ref over typed GalleriaItems; fullscreen is deferred" in {
        typeCheck(
            preamble + """def x(r: SignalRef[Int])(using Frame): uic.Galleria = uic.Galleria().items(uic.GalleriaItem("/a.jpg", "A", title = Present("T"))).activeIndex(r).circular(true).showThumbnails(false).showIndicators(true).showItemNavigators(true).showThumbnailNavigators(false).itemTemplate(i => span(i.alt)).thumbnailTemplate(i => span(i.alt)).onItemChange(_ => ())"""
        )
        typeCheckFailure(preamble + """def x = uic.Galleria().activeIndex(0)""")
        typeCheckFailure(preamble + """def x = uic.Galleria().fullScreen(true)""")
        typeCheckFailure(preamble + """def x = uic.Galleria().items("a.jpg")""")
    }

    "FileUpload is basic-mode only: FilePayload-metadata onSelect + multiple; Prime's URL/auto machinery is absent" in {
        typeCheck(
            preamble + """def x(using Frame): uic.FileUpload = uic.FileUpload().inputId("up").chooseLabel("Pick").fileLabel("None yet").accept(FileAccept.Extension(".csv"), FileAccept.Pdf).multiple(true).disabled(false).onSelect(files => ())"""
        )
        typeCheckFailure(preamble + """def x = uic.FileUpload().mode("advanced")""")
        typeCheckFailure(preamble + """def x = uic.FileUpload().url("/api/upload")""")
        typeCheckFailure(preamble + """def x = uic.FileUpload().auto(true)""")
        typeCheckFailure(preamble + """def x = uic.FileUpload().onSelect((name: String, content: String) => ())""")
    }

    "TreeTable reuses the DataTable Column carrier over recursive typed nodes; pagination is deferred" in {
        typeCheck(
            preamble + """def x(e: SignalRef[Set[String]], s: SignalRef[Set[String]], o: SignalRef[List[(String, Boolean)]])(using Frame): uic.TreeTable[String] = uic.TreeTable[String]().nodes(uic.TreeTableNode("root", List(uic.TreeTableNode("leaf")))).columns(uic.Column[String]("Name")(identity).sortBy(identity)).rowKey(identity).expanded(e).selected(s).sort(o).selectionMode(uic.SelectionMode.Multiple).showGridlines(true).size(uic.Size.Small).emptyMessage("Empty").onNodeToggle(_ => ()).onRowClick(_ => ()).accessibleName("Files")"""
        )
        typeCheckFailure(preamble + """def x(p: SignalRef[Int]) = uic.TreeTable[String]().paginate(5)(p)""")
        typeCheckFailure(preamble + """def x = uic.TreeTable[String]().rows(Seq("a"))""")
    }

    "OrganizationChart collapses via a collapsed-keys Set ref (empty = fully expanded)" in {
        typeCheck(
            preamble + """def x(c: SignalRef[Set[String]], s: SignalRef[Set[String]])(using Frame): uic.OrganizationChart = uic.OrganizationChart().node(uic.OrgChartNode("CEO", "ceo", children = List(uic.OrgChartNode("CTO", "cto")), template = Present(span("x")), className = Present("hl"))).collapsible(true).collapsed(c).selectionMode(uic.SelectionMode.Single).selected(s).onNodeToggle(_ => ()).onNodeClick(_ => ())"""
        )
        typeCheckFailure(preamble + """def x(e: SignalRef[Set[String]]) = uic.OrganizationChart().expanded(e)""")
        typeCheckFailure(preamble + """def x = uic.OrganizationChart().collapsed(Set("a"))""")
        typeCheckFailure(preamble + """def x = uic.OrganizationChart().nodes(uic.OrgChartNode("a", "a"))""")
    }

    "Terminal takes an effect-typed command handler and a history Seq ref; no event bus" in {
        typeCheck(
            preamble + """def x(h: SignalRef[Seq[uic.TerminalCommand]])(using Frame): uic.Terminal = uic.Terminal().welcomeMessage("Hi").prompt("$ ").commands(h).commandHandler(cmd => s"ran $cmd").accessibleName("Shell")"""
        )
        typeCheck(
            preamble + """def x(h: SignalRef[Seq[uic.TerminalCommand]], eff: String => String < Async)(using Frame): uic.Terminal = uic.Terminal().commands(h).commandHandler(eff)"""
        )
        typeCheckFailure(preamble + """def x = uic.Terminal().commands(Seq(uic.TerminalCommand("a", "b")))""")
        typeCheckFailure(preamble + """def x = uic.TerminalService""")
    }

    // ---- wave R: ColorPicker / InputMask / VirtualScroller + Input filter/mask ----

    "Input exposes the client-local inputFilter/inputMask and keeps the concrete type" in {
        typeCheck(preamble + """def x: uic.Input = uic.Input().inputFilter("int").inputMask("(999) 999-9999").placeholder("p")""")
        typeCheck(preamble + """def x(r: SignalRef[String])(using Frame): uic.Input = uic.Input().value(r).inputFilter("decimal")""")
        typeCheckFailure(preamble + """def x = uic.Button("a").inputMask("999")""")
    }

    "ColorPicker binds a hex String ref with inline/overlay + drag options; no raw HSB setters" in {
        typeCheck(
            preamble + """def x(r: SignalRef[String])(using Frame): uic.ColorPicker = uic.ColorPicker(r).inline(true).disabled(true).anchor(uic.OverlayAnchor.TopStart).accessibleName("Color").onChange(_ => ())"""
        )
        typeCheck(preamble + """def x: uic.ColorPicker = uic.ColorPicker().value("#3b82f6")""")
        typeCheckFailure(preamble + """def x(r: SignalRef[Int]) = uic.ColorPicker().value(r)""")
        typeCheckFailure(preamble + """def x = uic.ColorPicker().hue(120)""")
        typeCheckFailure(preamble + """def x = uic.ColorPicker().placeholder("p")""")
    }

    "InputMask is a typed InputText wrapper over a mask template; slotChar/autoClear are absent (deferred)" in {
        typeCheck(
            preamble + """def x(r: SignalRef[String])(using Frame): uic.InputMask = uic.InputMask("(999) 999-9999").value(r).placeholder("phone").size(uic.Size.Large).variant(uic.FieldVariant.Filled).fluid(true).invalid(true).invalidMessage("bad").disabled(true).onChange(_ => ()).onComplete(_ => ())"""
        )
        typeCheck(preamble + """def x: uic.InputMask = uic.InputMask().mask("99/99/9999").value("12/31/2026")""")
        typeCheckFailure(preamble + """def x = uic.InputMask("999").slotChar("_")""")
        typeCheckFailure(preamble + """def x = uic.InputMask("999").autoClear(true)""")
    }

    "VirtualScroller is items-typed with a fixed itemSize/height window; lazy-load props are absent (deferred)" in {
        typeCheck(
            preamble + """def x(using Frame): uic.VirtualScroller[Int] = uic.VirtualScroller((0 until 10000).toList).itemSize(40).height(200).overscan(4)(i => span(s"row $i"))"""
        )
        typeCheckFailure(preamble + """def x = uic.VirtualScroller(List(1)).lazyLoad(true)""")
        typeCheckFailure(preamble + """def x = uic.VirtualScroller(List(1)).scrollHeight("14rem")""")
    }

    "Wave R2 feature setters are discoverable; deferred-only props stay absent" in {
        // Overlay flip/shift
        typeCheck(preamble + """def x(r: SignalRef[Boolean])(using Frame): uic.Overlay = uic.Overlay(r).autoFlip(false)""")
        // Dialog draggable + resizable
        typeCheck(preamble + """def x: uic.Dialog = uic.Dialog().draggable(true).resizable(true)""")
        // Knob keeps keyboard; pointer-drag is behavioral (no new prop) — the value binding stands
        typeCheck(preamble + """def x(r: SignalRef[Double])(using Frame): uic.Knob = uic.Knob().value(r).min(0).max(100)""")
        // Carousel swipe is behavioral on the bound page; no responsiveOptions prop (deferred)
        typeCheckFailure(preamble + """def x = uic.Carousel[Int]().responsiveOptions(Nil)""")
        // FileUpload: onSelect now delivers FilePayload metadata; multiple present
        typeCheck(preamble + """def x(using Frame): uic.FileUpload = uic.FileUpload().multiple(true).onSelect(fs => ())""")
        typeCheck(
            preamble + """def x(using Frame): uic.FileUpload = uic.FileUpload().onSelect((fs: Seq[UI.FilePayload]) => fs.headOption.map(_.name))"""
        )
        // the old content-String onSelect signature is gone
        typeCheckFailure(preamble + """def x(using Frame): uic.FileUpload = uic.FileUpload().onSelect((s: String) => ())""")
    }
end DiscoverabilityTest
