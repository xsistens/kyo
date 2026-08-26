package kyo.uic.test

import kyo.*
import kyo.UI.*
import kyo.uic
import scala.language.implicitConversions

/** Compile-time showcase: exercises every integration seam the library promises.
  * This object typechecking IS the test — it proves the downstream API shape:
  * qualified `uic.X` access, chained setters, auto-lift into kyo containers,
  * two-way signal binding, and mixing uic components with raw kyo elements.
  */
object ShowcaseCompile:

    def demoPage(nameRef: SignalRef[String], save: Any < Async)(using Frame): UI =
        UI.main.cssClass("demo")(
            h1("kyo-ui-components showcase"),

            // uic component directly inside a kyo container (Node => HtmlChildVal lift)
            uic.Button("Save")
                .severity(uic.Severity.Primary)
                .icon(uic.Icons.check)
                .onClick(save),

            // two-way bound input + label, mixed with raw kyo elements
            div.cssClass("field")(
                uic.Label("Name").forId("name").required(true),
                uic.Input().placeholder("Your name").value(nameRef)
            ),

            // uic component inside fragment(...) (Node => UI lift)
            fragment(
                uic.Button("Cancel").variant(uic.ButtonVariant.Text): UI
            ),

            // reactive subtree containing a uic component
            nameRef.render(n => div(uic.Button(s"Hello $n"): UI)),

            // disabled + variadic extra children
            uic.Button().icon(uic.Icons.trash).disabled(true)(span("extra"))
        )

    def layoutPage(collapsedRef: SignalRef[Boolean], logout: Any < Async)(using Frame): UI =
        div(
            // toolbar with three sections, nesting uic + kyo children
            uic.Toolbar()
                .start(uic.Icon(uic.Icons.bars).accessibleName("Menu"))
                .center(span("kyo-ui-components"))
                .end(uic.Button("Logout").variant(uic.ButtonVariant.Text).onClick(logout)),

            // flex layout with kyo enum lambdas + toggleable panel + card with footer
            uic.FlexBox().vertical(true).gap(16).align(_.stretch).justify(_.start)(
                uic.Panel().header("Details").toggleable(true).collapsed(collapsedRef).footer(span("2 items"))(
                    uic.Card().title("Info").subtitle("Static").footer(uic.Button("Open"))(p("Body text"))
                ),
                uic.Icon(uic.Icons.check).size(24)
            )
        )

    def formPage(
        agree: SignalRef[Boolean],
        choice: SignalRef[Boolean],
        live: SignalRef[Boolean],
        notes: SignalRef[String],
        country: SignalRef[String],
        city: SignalRef[String],
        date: SignalRef[String],
        calendarOpen: SignalRef[Boolean],
        submit: Any < Async
    )(using Frame): UI =
        UI.main.cssClass("form-demo")(
            h1("Form controls"),

            // toggles with two-way signal binding, mixed with a raw kyo element
            div.cssClass("field")(
                uic.CheckBox("I agree").checked(agree).invalid(true).invalidMessage("Please confirm").onChange(_ => submit),
                span("(raw kyo element)")
            ),
            uic.RadioButton("Option A").name("group").checked(choice),
            uic.ToggleSwitch().checked(live).onChange(_ => submit),

            // multi-line + option-list controls (typed options: Seq[A] + label projection)
            uic.TextArea().value(notes).placeholder("Notes").rows(3),
            uic.Select[(String, String)]()
                .options(Seq("de" -> "Germany", "us" -> "USA"))(_._2)
                .optionKey(_._1)
                .value(country),
            uic.AutoComplete[String]()
                .options(Seq("Berlin", "Munich"))
                .value(city)
                .placeholder("City")
                .showClear(true),
            uic.DatePicker().value(date).open(calendarOpen).placeholder("YYYY-MM-DD"),

            // uic component inside fragment(...) (Node => UI lift)
            fragment(uic.ToggleSwitch().checked(live): UI),

            // reactive subtree containing a uic component (auto-lift under a signal)
            live.render(b => div(uic.ToggleSwitch().checked(b): UI))
        )

    def feedbackPage(
        progress: SignalRef[Int],
        toastOpen: SignalRef[Boolean],
        dismiss: Any < Async
    )(using Frame): UI =
        UI.main.cssClass("feedback-demo")(
            // breadcrumb nav trail with the Prime home item and a custom separator glyph
            uic.Breadcrumb()
                .home(uic.Icons.home, "/")
                .item("Reports", "/reports")
                .item("Q2 2026")
                .separator(uic.Icons.angleRight),

            // toolbar sections, mixing uic components and raw kyo elements
            uic.Toolbar()
                .start(uic.Avatar().initials("AL"), span("Ada Lovelace"))
                .end(uic.Link("Help").href("/help").icon(uic.Icons.externalLink)),

            // semantic banner with an icon and a close action
            uic.Message()
                .severity(uic.Severity.Warn)
                .icon(uic.Icons.exclamationTriangle)
                .closable(true)
                .onDismissed(dismiss)("Your session expires soon."),

            // reactive progress bound two-way to a signal, mixed with a raw kyo element
            div.cssClass("field")(
                span("Upload"),
                uic.ProgressBar().value(progress).showValue(true)
            ),

            // reactive subtree containing a uic component (auto-lift under a signal)
            progress.render(p => div(uic.ProgressBar().value(p): UI)),

            // signal-gated toast overlay toggled by a link
            uic.Link("Show toast").onClick(toastOpen.set(true)),
            uic.Toast()
                .open(toastOpen)
                .position(uic.OverlayPosition.BottomRight)
                .severity(uic.Severity.Success)
                .summary("Done")
                .detail("Everything saved.")
                .duration(2500)
        )

    def structuralPage(
        listSel: SignalRef[Set[String]],
        listQuery: SignalRef[String],
        tableSel: SignalRef[Set[String]],
        activeTab: SignalRef[String],
        treeExpanded: SignalRef[Set[String]],
        treeSel: SignalRef[Set[String]],
        onPick: String => Any < Async
    )(using Frame): UI =
        UI.main.cssClass("structural-demo")(
            // grouped listbox: OptionItem rows carrying the untyped ListItem
            uic.Listbox()
                .selectionMode(uic.SelectionMode.Single)
                .optionGroups(
                    uic.OptionItem.group("Mail")(uic.ListItem.of("Inbox", "g-inbox"), uic.ListItem.of("Sent", "g-sent")),
                    uic.OptionItem.item(uic.ListItem.of("Trash", "g-trash"))
                ),

            // listbox with multi-select bound two-way, checkmarks, a filter, and an item handler
            uic.Listbox()
                .selectionMode(uic.SelectionMode.Multiple)
                .checkmark(true)
                .filterQuery(listQuery)
                .emptyContent("No matches")
                .item("Inbox", "inbox", icon = Present(uic.Icons.inbox))
                .item("Drafts", "drafts")
                .items(uic.ListItem(uic.TextValue.Const("Sent"), id = "sent", additionalText = Present("12")))
                .value(listSel)
                .onItemClick(onPick),

            // typed data table: text + template columns, single-select bound two-way
            uic.DataTable[(String, String)]()
                .rows(Seq("ada" -> "Active", "bob" -> "Away"))
                .rowKey(_._1)
                .columns(
                    uic.Column[(String, String)]("Name")(_._1).sortBy(_._1),
                    uic.Column[(String, String)]("Status").body(r => uic.Tag(r._2).severity(uic.Severity.Success))
                )
                .selectionMode(uic.SelectionMode.Single)
                .selected(tableSel)
                .onRowClick(onPick),

            // tabs with the active tab bound to a String ref, panels nesting uic + kyo
            uic.Tabs()
                .tab("Overview", "ov")(p("Overview content"), (uic.Button("Refresh"): UI))
                .tab("Details", "dt")(uic.Card().title("Info")(p("Details")))
                .selected(activeTab)
                .onTabSelect(onPick),

            // recursive tree with two-way expansion + selection Set refs and node icons
            uic.Tree()
                .selectionMode(uic.SelectionMode.Multiple)
                .nodes(
                    uic.TreeNode(
                        "src",
                        "src",
                        icon = Present(uic.Icons.folder),
                        children = List(
                            uic.TreeNode("main", "main", children = List(uic.TreeNode("App.scala", "app"))),
                            uic.TreeNode("test", "test")
                        )
                    )
                )
                .expanded(treeExpanded)
                .selected(treeSel)
                .onNodeToggle(onPick),

            // reactive subtree containing a structural component (auto-lift under a signal)
            activeTab.render(id => div(uic.Tabs().tab("Solo", id)(span(id)).selected(activeTab): UI))
        )

    def typographyPage(using Frame): UI =
        UI.main.cssClass("typography-demo")(
            // Text: clamp + empty indicator, default slot
            uic.Text().emptyIndicatorMode(uic.TextEmptyIndicatorMode.On).maxLines(2)("Body text"),

            // Title: semantic level decoupled from the visual size
            uic.Title().level(uic.TitleLevel.H2).size(uic.TitleLevel.H4).wrap(false)("Heading"),

            // ProgressSpinner auto-lifted straight into the kyo container
            uic.ProgressSpinner().size(uic.Size.Large).accessibleName("Loading"),

            // Tag mixed with raw kyo (Node => UI lift)
            div(uic.Tag("Beta").severity(uic.Severity.Info).rounded(true): UI),

            // component inside fragment(...) (Node => UI lift)
            fragment(uic.ProgressSpinner(): UI),

            // Message with an explicit icon
            uic.Message().severity(uic.Severity.Success).icon(uic.Icons.checkCircle)("Saved.")
        )

    def tier1Page(
        fieldsetCollapsed: SignalRef[Boolean],
        inplaceActive: SignalRef[Boolean],
        dismissTag: Any < Async
    )(using Frame): UI =
        UI.main.cssClass("tier1-demo")(
            // skeleton placeholders: shapes, dimensions, animation toggle
            uic.Skeleton().width("10rem").height("4rem"),
            uic.Skeleton().shape(uic.SkeletonShape.Circle).size("4rem").animation(false),

            // divider with centered content between raw kyo blocks
            p("above"),
            uic.Divider().align(uic.DividerAlign.Center).lineStyle(uic.DividerLineStyle.Dashed)(span("OR")),
            p("below"),

            // toggleable fieldset bound two-way to a ref
            uic.Fieldset().legend("Details").toggleable(true).collapsed(fieldsetCollapsed)(
                p("Fieldset body")
            ),

            // badges: value/dot/severity/size + the overlay on an avatar
            uic.Badge("3"),
            uic.Badge().severity(uic.Severity.Danger),
            uic.Badge("22").severity(uic.Severity.Warn).size(uic.Size.Large),
            uic.OverlayBadge(uic.Avatar().initials("JD"))(uic.Badge("2")),

            // chips: icon/image variants, removable with an effect
            uic.Chip("Apple").icon(uic.Icons.check),
            uic.Chip("Amy").image("/amy.png"),
            uic.Chip("Xuxue").removable(true).onRemove(dismissTag),

            // avatar stack
            uic.AvatarGroup(
                uic.Avatar().initials("A").shape(uic.AvatarShape.Circle),
                uic.Avatar().initials("B").shape(uic.AvatarShape.Circle)
            ),

            // meter group scaled against a custom max, mixing palette + explicit colors
            uic.MeterGroup()
                .meter("Apps", 16)
                .meter("Messages", 8, "var(--p-cyan-500)")
                .max(200),

            // inplace bound two-way, closable content
            uic.Inplace()
                .display(span("View Content"))
                .content(p("Full content"))
                .active(inplaceActive)
                .closable(true),

            // natively scrolling panel with a fixed viewport
            uic.ScrollPanel(p("scrolling content")).width("100%").height("12rem")
        )

    def optionControlPage(
        score: SignalRef[Int],
        muted: SignalRef[Boolean],
        unit: SignalRef[String],
        toppings: SignalRef[Set[String]],
        username: SignalRef[String],
        pageIdx: SignalRef[Int],
        wizardStep: SignalRef[Int],
        notify: Int => Any < Async
    )(using Frame): UI =
        UI.main.cssClass("option-control-demo")(
            // rating bound two-way, with the cancel-on-same-value semantics
            uic.Rating().value(score).stars(5).onChange(notify),
            uic.Rating().value(4).readonly(true),

            // toggle button with state labels + icons
            uic.ToggleButton().checked(muted).onLabel("Muted").offLabel("Live").onIcon(uic.Icons.volumeOff).offIcon(uic.Icons.volumeUp),

            // typed select buttons: single + multiple
            uic.SelectButton[(String, String)]()
                .options(Seq("kg" -> "Kilogram", "lb" -> "Pound"))(_._2)
                .optionKey(_._1)
                .value(unit)
                .allowEmpty(false),
            uic.SelectButton[String]().options(Seq("Cheese", "Ham", "Olives")).multiple(true).value(toppings),

            // input group composing addons with existing fields
            uic.InputGroup()(
                uic.InputGroup.addon(uic.Icon(uic.Icons.user)),
                uic.Input().placeholder("Username").value(username),
                uic.InputGroup.addon(uic.Button("Go"))
            ),

            // icon field + float/ifta labels around the same Input builder
            uic.IconField(uic.Input().placeholder("Search")).iconStart(uic.Icons.search),
            uic.FloatLabel(uic.Input().id("fl-user").value(username), "Username").forId("fl-user"),
            uic.IftaLabel(uic.Input().id("il-user").value(username), "Username").forId("il-user"),

            // typed timeline
            uic.Timeline[(String, String)]()
                .events(Seq("Ordered" -> "15/10", "Shipped" -> "16/10"))
                .content(e => span(e._1))
                .opposite(e => span(e._2))
                .marker(_ => uic.Icon(uic.Icons.check): UI),

            // standalone paginator + a paginated data view sharing the page ref idea
            uic.Paginator().totalRecords(120).rows(10).page(pageIdx).onPage(notify),
            uic.DataView[String]()
                .items(Seq("Bamboo Watch", "Black Watch", "Blue Band"))
                .itemTemplate(s => div(span(s)))
                .header(span("Products"))
                .paginate(2)(pageIdx),

            // linear wizard bound to a step ref
            uic.Stepper()
                .active(wizardStep)
                .linear(true)
                .step("Personal")(p("Step 1"))
                .step("Payment")(p("Step 2"))
                .step("Review")(p("Step 3"))
        )

    def deepenedControlPage(
        date: SignalRef[String],
        monthValue: SignalRef[String],
        datetime: SignalRef[String],
        clock: SignalRef[String],
        picked: SignalRef[Set[String]],
        rangeStart: SignalRef[String],
        rangeEnd: SignalRef[String],
        displayedMonth: SignalRef[String],
        calView: SignalRef[uic.DatePickerView],
        calOpen: SignalRef[Boolean],
        pageIdx: SignalRef[Int],
        pageRows: SignalRef[Int],
        wizardValue: SignalRef[String],
        notify: Int => Any < Async
    )(using Frame): UI =
        UI.main.cssClass("deepened-control-demo")(
            // month/year drill-down (currentView ref) + button bar with an explicit today
            uic.DatePicker()
                .value(date)
                .month(displayedMonth)
                .currentView(calView)
                .open(calOpen)
                .showButtonBar(true)
                .today("2026-07-16"),

            // granularity views: month/year value models stay ISO-prefix strings
            uic.DatePicker().value(monthValue).view(uic.DatePickerView.Month).open(calOpen),

            // multiple + range selection (ISO string collections / start-end pair)
            uic.DatePicker().values(picked).open(calOpen),
            uic.DatePicker().range(rangeStart, rangeEnd).open(calOpen),

            // time picking: datetime, 12-hour clock, time-only
            uic.DatePicker().value(datetime).showTime(true).hourFormat(uic.HourFormat.H12).open(calOpen),
            uic.DatePicker().value(clock).timeOnly(true).open(calOpen),

            // paginator with windowed links and all three template slots
            uic.Paginator()
                .totalRecords(120)
                .rows(pageRows)
                .page(pageIdx)
                .pageLinkSize(5)
                .rowsPerPageOptions(Seq(10, 20, 30))
                .currentPageReport("Showing {first} to {last} of {totalRecords}")
                .jumpToPageInput(true)
                .onPage(notify),

            // vertical, value-keyed stepper with a hard-disabled step
            uic.Stepper()
                .vertical(true)
                .value(wizardValue)
                .step("Personal", value = Present("personal"))(p("Step 1"))
                .step("Payment", value = Present("payment"))(p("Step 2"))
                .step("Review", value = Present("review"), disabled = true)(p("Step 3")),

            // meter group: orientation, label position/orientation, icons, templates
            uic.MeterGroup()
                .orientation(uic.Orientation.Vertical)
                .labelOrientation(uic.Orientation.Vertical)
                .labelPosition(uic.LabelPosition.Start)
                .meter("Apps", 24, uic.Icons.user)
                .meter("Messages", 16, "var(--p-cyan-500)", uic.Icons.inbox)
                .startTemplate(span("used"))
                .endTemplate(span("of 100 GB"))
                .meterTemplate((m, pc) => span.cssClass("seg")(m.label))
                .labelTemplate((m, pc) => span(s"${m.label} — ${math.round(pc)}%"))
        )

    def featureGapPage(
        score: SignalRef[Int],
        muted: SignalRef[Boolean],
        unit: SignalRef[String],
        query: SignalRef[String],
        city: SignalRef[String],
        editing: SignalRef[Boolean],
        notify: Any < Async
    )(using Frame): UI =
        UI.main.cssClass("feature-gap-demo")(
            // rating with form-participating hidden radios + custom glyphs
            uic.Rating().value(score).name("score").onIcon(uic.Icons.heartFill).offIcon(uic.Icons.heart),

            // toggle button: full width + readonly (no state writes on click)
            uic.ToggleButton().checked(muted).fluid(true),
            uic.ToggleButton().checked(true).readonly(true),

            // select button with arbitrary option UI (label projection stays aria/key)
            uic.SelectButton[(String, String)]()
                .options(Seq("kg" -> "Kilogram", "lb" -> "Pound"))(_._2)
                .optionKey(_._1)
                .itemTemplate(o => span(o._2.toUpperCase))
                .value(unit),

            // icon fields around TextArea and Select hosts
            uic.IconField(uic.TextArea().placeholder("Notes").value(query)).iconStart(uic.Icons.search),
            uic.IconField(uic.Select[String]().options(Seq("A", "B")).value(unit)).iconStart(uic.Icons.user),

            // float labels on the three new hosts
            uic.FloatLabel(uic.TextArea().id("fl-msg").value(query), "Message").forId("fl-msg"),
            uic.FloatLabel(uic.Select[String]().options(Seq("kg", "lb")).id("fl-unit").value(unit), "Unit").forId("fl-unit"),
            uic.FloatLabel(uic.AutoComplete[String]().options(Seq("Berlin", "Bern")).id("fl-city").value(city), "City").forId("fl-city"),

            // toggle switch with per-state handle glyphs
            uic.ToggleSwitch().checked(muted).handleIcon(uic.Icons.check, uic.Icons.times),

            // chip with a custom remove glyph
            uic.Chip("Tag").removable(true).removeIcon(uic.Icons.times).onRemove(notify),

            // inplace: disabled guard + open/close callbacks around the two-way ref
            uic.Inplace()
                .display(span("Edit name"))
                .content(uic.Input().value(query))
                .active(editing)
                .closable(true)
                .onOpen(notify)
                .onClose(notify),
            uic.Inplace().display(span("Locked")).content(span("never")).active(editing).disabled(true),

            // data view busy overlay (sorting stays a Seq concern: sortBy before items)
            uic.DataView[String]()
                .items(Seq("Bamboo Watch", "Black Watch").sortBy(identity))
                .itemTemplate(s => div(span(s)))
                .loading(true)
        )

    def overlayPage(
        unit: SignalRef[String],
        panelOpen: SignalRef[Boolean]
    )(using Frame): UI =
        UI.main.cssClass("overlay-demo")(
            // the floating-panel Select: filter header, clear affordance, checkmark column
            uic.Select[(String, String)]()
                .options(Seq("kg" -> "Kilogram", "lb" -> "Pound"))(_._2)
                .optionKey(_._1)
                .value(unit)
                .filterable(true)
                .showClear(true)
                .checkmark(true)
                .placeholder("Unit"),

            // panel visibility exposed two-way (self-managed without it)
            uic.Select[String]().options(Seq("A", "B")).value(unit).open(panelOpen),

            // grouped options: the same OptionItem rows across the flat pickers,
            // composing with the filter and the key projection as plain options do
            uic.Select[(String, String)]()
                .optionGroups(Seq(
                    uic.OptionItem.group("Metric")("kg"   -> "Kilogram", "g" -> "Gram"),
                    uic.OptionItem.group("Imperial")("lb" -> "Pound"),
                    uic.OptionItem.item("ct"              -> "Carat")
                ))(_._2)
                .optionKey(_._1)
                .value(unit)
                .filterable(true)
                .placeholder("Unit"),

            // the raw Overlay primitive on a custom relative anchor
            div.cssClass("p-uic-overlay-anchor")(
                uic.Button("Toggle").onClick(panelOpen.set(true)),
                uic.Overlay(panelOpen)
                    .anchor(uic.OverlayAnchor.BottomEnd)
                    .maxHeight("16rem")(
                        div(span("panel content"))
                    ): UI
            )
        )

    def menuPage(
        menuOpen: SignalRef[Boolean],
        dialOpen: SignalRef[Boolean],
        save: Any < Async
    )(using Frame): UI =
        UI.main.cssClass("menu-demo")(
            // the shared typed item model: icon + effect + url + separator + nesting
            uic.Menu().items(
                uic.MenuItem("New").icon(uic.Icons.plus).onSelect(save),
                uic.MenuItem("Open").url("/open"),
                uic.MenuItem.separator,
                uic.MenuItem("Documents").items(uic.MenuItem("Report Q3"))
            ),

            // popup menu on a caller-anchored trigger (two-way visibility)
            div.cssClass("p-uic-overlay-anchor")(
                uic.Button("Options").onClick(menuOpen.set(true)),
                uic.Menu().items(uic.MenuItem("Refresh").onSelect(save)).popup(menuOpen): UI
            ),

            // horizontal bar with nested submenus + logo/end slots
            uic.Menubar()
                .start(span("LOGO"))
                .end(uic.Button("Login").size(uic.Size.Small))
                .items(
                    uic.MenuItem("File").items(
                        uic.MenuItem("New").icon(uic.Icons.plus).onSelect(save),
                        uic.MenuItem("Recent").items(uic.MenuItem("a.txt").onSelect(save))
                    )
                ),

            // vertical tiered menu with side-nested submenus
            uic.TieredMenu().items(
                uic.MenuItem("Edit").items(uic.MenuItem("Undo").onSelect(save))
            ),

            // mega menu: one panel of grouped columns per root item
            uic.MegaMenu().items(
                uic.MegaMenuItem("Furniture")
                    .icon(uic.Icons.box)
                    .column(uic.MenuGroup("Living Room").items(uic.MenuItem("Sofa").onSelect(save)))
                    .column(uic.MenuGroup("Bedroom").items(uic.MenuItem("Bed")))
            ),

            // split button: primary action + attached popup menu, styling passthrough
            uic.SplitButton("Save")
                .icon(uic.Icons.check)
                .severity(uic.Severity.Success)
                .onClick(save)
                .items(uic.MenuItem("Save as copy").onSelect(save), uic.MenuItem.separator, uic.MenuItem("Discard")),

            // speed dial: linear action fan bound two-way
            uic.SpeedDial()
                .direction(uic.SpeedDialDirection.Right)
                .open(dialOpen)
                .items(uic.MenuItem("Add").icon(uic.Icons.plus).onSelect(save))
        )

    def dialogPage(openRef: SignalRef[Boolean], confirm: Any < Async)(using Frame): UI =
        div(
            uic.Button("Open dialog").onClick(openRef.set(true)),
            uic.Dialog()
                .open(openRef)
                .header("Confirm action")
                .closeOnBackdrop(true)
                .footer(
                    uic.FlexBox().gap(8)(
                        uic.Button("Cancel").variant(uic.ButtonVariant.Text).onClick(openRef.set(false)),
                        uic.Button("OK").onClick(confirm)
                    )
                )(p("Are you sure?"))
        )

    // ---- MultiSelect / CascadeSelect / TreeSelect / Drawer ----
    def advancedPickerPage(
        fruits: SignalRef[Set[String]],
        city: SignalRef[String],
        files: SignalRef[Set[String]],
        drawerOpen: SignalRef[Boolean],
        notify: Any < Async
    )(using Frame): UI =
        div(
            // multi-value select: Set-keyed binding, chips + select-all + filter
            uic.MultiSelect[(String, String)]()
                .options(Seq("ap" -> "Apple", "ba" -> "Banana"))(_._2)
                .optionKey(_._1)
                .value(fruits)
                .display(uic.MultiSelectDisplay.Chip)
                .maxSelectedLabels(3)
                .selectedItemsLabel("{0} fruits")
                .filterable(true)
                .showClear(true)
                .placeholder("Fruits")
                .onChange(_ => notify),

            // nested typed option tree with side sub-panels
            uic.CascadeSelect[String]()
                .options(
                    Seq(uic.CascadeItem.group("Germany")(uic.CascadeItem.leaf("Berlin"), uic.CascadeItem.leaf("Munich")))
                )(identity)
                .value(city)
                .placeholder("City"),

            // Select-style trigger hosting the real Tree in the panel
            uic.TreeSelect()
                .nodes(uic.TreeNode("src", "src", children = List(uic.TreeNode("App.scala", "app"))))
                .value(files)
                .selectionMode(uic.SelectionMode.Checkbox)
                .placeholder("Files"),

            // edge-docked modal panel (Dialog's sibling)
            uic.Button("Open drawer").onClick(drawerOpen.set(true)),
            uic.Drawer()
                .open(drawerOpen)
                .position(uic.DrawerPosition.Right)
                .header("Settings")
                .footer(uic.Button("Apply").onClick(drawerOpen.set(false)))
                .onClose(notify)(p("Drawer content"))
        )

    // ---- ContextMenu / ToastService / ConfirmDialog ----
    def overlayServicePage(
        svc: uic.ToastService,
        confirmOpen: SignalRef[Boolean],
        notify: Any < Async
    )(using Frame): UI =
        div(
            // right-click target wrapped by the context menu; nested items + effects
            uic.ContextMenu(
                Seq(
                    uic.MenuItem("Copy").icon(uic.Icons.copy).onSelect(notify),
                    uic.MenuItem.separator,
                    uic.MenuItem("Share").items(uic.MenuItem("Email").onSelect(notify))
                )
            )(div(span("Right-click this region"))),

            // toast service resolved at build time; handlers close over the instance
            uic.Button("Toast").onClick(svc.add(uic.Severity.Success, "Saved", "Changes stored.")),
            uic.Button("Sticky").onClick(
                svc.add(uic.ToastMessage(uic.Severity.Warn, Present("Sticky"), Absent, Absent))
            ),
            uic.ToastService.render(svc, uic.OverlayPosition.BottomRight),

            // confirm dialog: declarative ref + accept/reject callbacks
            uic.Button("Delete…").onClick(confirmOpen.set(true)),
            uic.ConfirmDialog(confirmOpen)
                .header("Delete file?")
                .message("This cannot be undone.")
                .icon(uic.Icons.exclamationTriangle)
                .acceptLabel("Delete")
                .rejectLabel("Keep")
                .acceptSeverity(uic.Severity.Danger)
                .onAccept(notify)
                .onReject(notify)
        )

    /** The Env-channel resolution the demo/app root uses: the service arrives via
      * `Env.get` at BUILD time and the layer is provided once at the root.
      */
    def waveMEnvWiring(confirmOpen: SignalRef[Boolean], notify: Any < Async)(using
        Frame
    ): UI < (Async & Env[uic.ToastService]) =
        Env.get[uic.ToastService].map(svc => overlayServicePage(svc, confirmOpen, notify))

    /** The form-input block: Double-typed numeric fields, the masked
      * password with inline strength feedback, the one-string OTP row, the
      * native-range-backed slider and the svg dial.
      */
    def formInputPage(
        amount: SignalRef[Double],
        secret: SignalRef[String],
        code: SignalRef[String],
        volume: SignalRef[Double],
        notify: Any < Async
    )(using Frame): UI =
        div(
            // number field: native min/max/step + stacked spin buttons + adornments
            uic.InputNumber()
                .value(amount)
                .min(0)
                .max(100)
                .step(0.5)
                .showButtons(true)
                .buttonLayout(uic.InputNumberButtonLayout.Stacked)
                .suffix(" kg")
                .onChange(_ => notify),

            // password: eye toggle + Prime's default strength rules inline
            uic.Password()
                .value(secret)
                .toggleMask(true)
                .feedback(true)
                .promptLabel("Enter a password")
                .placeholder("Password"),

            // otp: one bound code over six digit-only masked cells
            uic.InputOtp().value(code).length(6).integerOnly(true).mask(true).onChange(_ => notify),

            // slider + knob share the Double value vocabulary
            uic.Slider().value(volume).min(0).max(100).step(5).accessibleName("Volume"),
            uic.Knob().value(volume).size(120).valueTemplate(v => s"${v.toInt}%").onChange(_ => notify)
        )

    /** The container/data block: panel accordion, list shufflers, media
      * paging, the label-for file chooser, hierarchical tables, the org tree, and
      * the effect-handled terminal.
      */
    def containerDataPage(
        openPanel: SignalRef[String],
        products: SignalRef[Seq[String]],
        productSel: SignalRef[Set[String]],
        targetList: SignalRef[Seq[String]],
        targetSel: SignalRef[Set[String]],
        pageIx: SignalRef[Int],
        activeImage: SignalRef[Int],
        expandedRows: SignalRef[Set[String]],
        expandedNodes: SignalRef[Set[String]],
        history: SignalRef[Seq[uic.TerminalCommand]],
        notify: Any < Async
    )(using Frame): UI =
        div(
            // accordion: ref type picks the mode (String ref = one open panel)
            uic.Accordion()
                .panel("Header I", "h1")(p("Content I"))
                .panel("Header II", "h2")(p("Content II"))
                .value(openPanel)
                .onToggle(_ => notify),

            // orderlist: the Seq ref IS the model; the selection drives the moves
            uic.OrderList[String]()
                .items(products)(identity)
                .selected(productSel)
                .itemTemplate(s => span(s)),

            // picklist: two columns + transfers, sharing the item vocabulary
            uic.PickList[String]()
                .sourceItems(products)(identity)
                .targetItems(targetList)
                .sourceSelected(productSel)
                .targetSelected(targetSel),

            // carousel: server-honest visible window over a typed page ref
            uic.Carousel[String]()
                .items(Seq("A", "B", "C"))(s => span(s))
                .page(pageIx)
                .numVisible(2)
                .circular(true),

            // galleria: preview + thumbnails bound to the active index
            uic.Galleria()
                .items(uic.GalleriaItem("/img/a.jpg", "A", title = Present("Alps")))
                .activeIndex(activeImage)
                .showItemNavigators(true),

            // fileupload (basic): the content string arrives in the handler
            uic.FileUpload()
                .inputId("container-data-upload")
                .accept(FileAccept.Extension(".txt"))
                .onSelect(_ => notify),

            // treetable: DataTable columns over recursive typed nodes
            uic.TreeTable[(String, String)]()
                .nodes(uic.TreeTableNode(("src", "dir"), List(uic.TreeTableNode(("main.scala", "file")))))
                .columns(uic.Column[(String, String)]("Name")(_._1), uic.Column[(String, String)]("Type")(_._2))
                .rowKey(_._1)
                .expanded(expandedRows)
                .selectionMode(uic.SelectionMode.Single),

            // orgchart: expanded-keys ref (Tree's polarity), toggle buttons, selectable boxes
            uic.OrganizationChart()
                .node(uic.OrgChartNode("CEO", "ceo", children = List(uic.OrgChartNode("CTO", "cto"))))
                .expanded(expandedNodes),

            // terminal: effect-typed handler + bound history (the server-push showcase)
            uic.Terminal()
                .welcomeMessage("kyo shell")
                .prompt("$")
                .commands(history)
                .commandHandler(cmd => s"unknown: $cmd")
        )
end ShowcaseCompile
