# kyo-ui-components

kyo-ui-components is a component library for kyo-ui: 83 components in package `kyo.uic`, wearing PrimeOne's own `.p-*` markup so Prime's real stylesheets apply to them verbatim. A component is a plain immutable builder value, not a `kyo.UI` node. You construct one (`uic.Button("Save")`), chain setters that each return the same concrete type, and drop it straight into a kyo-ui container; an implicit lift renders it at placement, the same way kyo-ui lifts a bare `String` into a text node. Because the setters are typed to the component, autocomplete after `.` shows exactly the options that component has and nothing else, and options it does not have fail to compile.

Everything reactive is kyo-ui's own vocabulary, unchanged. A text, boolean, or severity slot takes a constant or a `Signal[A]`; a value slot takes a `SignalRef[A]` and binds two way; a panel's visibility is a `SignalRef[Boolean]` you own. Rendering stays server-honest: state lives in signals, behaviour is computed from those signals at render time, and the only client-side machinery is the contract kyo-ui already ships (focus seeding, in-place attribute and class patching, pointer and scroll reporting). Two things you supply yourself: `uic.Theme.css` on the page (nothing is injected for you), and `import scala.language.implicitConversions` for the placement lift.

The module cross-builds for every platform kyo-ui does: the JVM, Scala Native, and both of Scala.js' linker backends, JS and WebAssembly. There is no per-platform component API, because there is no per-platform source: every component lives in shared source and renders identically on all four, and the test suite runs on all four to keep it that way. Off the JVM the build pulls `scala-java-time` so `DateCodec` has a `java.time` to work against, along with the timezone, locale, and currency databases: those types exist without them but carry no data, so resolving one throws at run time, invisible to both compile and link.

<!-- doctest:setup
```scala
import kyo.*
import kyo.UI.*
import kyo.uic
import kyo.uic.form.*
import scala.language.implicitConversions

case class Product(id: String, name: String, category: String, price: Double, inStock: Boolean) derives CanEqual
case class Category(id: String, label: String, children: List[Category]) derives CanEqual

val catalog: Seq[Product] = Seq(
    Product("p1", "Bamboo Watch", "Accessories", 65.0, true),
    Product("p2", "Black Watch", "Accessories", 72.5, false),
    Product("p3", "Blue Band", "Fitness", 12.0, true)
)

val categories: Seq[Category] = Seq(
    Category("acc", "Accessories", Nil),
    Category("fit", "Fitness", List(Category("yoga", "Yoga", Nil)))
)
```
-->

```scala
import kyo.*
import kyo.UI.*
import kyo.uic
import scala.language.implicitConversions

val search: UI < Async =
    for query <- Signal.initRef("")
    yield div(
        uic.Input().placeholder("Search the catalog").value(query),
        uic.Button("Go").icon(uic.Icons.search)
    )
```

Every example below assumes those four imports, so they are not repeated per block. The validation layer covered under [Validated forms](#validated-forms) adds one more, `import kyo.uic.form.*`, which its examples do show.

## Placing a component

A component is not a `UI`. The kyo HTML AST is `sealed` and cannot be extended from outside kyo-ui, so `uic.Button` is a plain case class implementing `Node`, and it renders itself into a `UI` at the moment you place it. That indirection is invisible in practice, because two implicit conversions do the lifting: `Node.nodeToChild` puts a component into a container's child list, and `Node.nodeToUI` produces a bare `UI` where one is expected. Both need `scala.language.implicitConversions` in scope.

The consequence you feel is that uic components and raw kyo elements are the same kind of thing at a call site. A `uic.Button` sits inside a `div` next to a `span`, and a `p` sits inside a `uic.Card` next to another component.

```scala
val productCard: UI =
    uic.Card()
        .title("Bamboo Watch")
        .subtitle("Accessories")
        .footer(uic.Button("Add to cart").icon(uic.Icons.shoppingCart))(
            p("A wooden watch with a fabric strap."),
            span("65.00 EUR")
        )
```

Most components carry a default child slot, `apply(cs: UI*)`, which is what the trailing `(...)` above fills. Components without meaningful children (`uic.Input`, `uic.Select`) simply do not declare it, so passing children to them is a compile error rather than a silently ignored argument.

### The two conversions, and the one place they cannot reach

`Node => UI => HtmlChildVal` would be two implicit hops, and Scala performs only one. That is why there are two separate conversions rather than one. It is also why a position whose expected type is a bare `UI` sometimes needs help: `UI.fragment[C <: UI](cs: C*)` infers `C` from its arguments, and a `Node` argument gives it nothing to infer from. Ascribe there.

```scala
val cancelRow: UI = fragment(uic.Button("Cancel").variant(uic.ButtonVariant.Text): UI)
```

An implicit conversion needs an expected type to fire against, and a `val` holding a comprehension has none. `val rows = for p <- catalog yield uic.Card().title(p.name)` infers `List[Card]`, and the placement a line later fails, pointing at the collection rather than at the missing conversion. No extra conversion can rescue that position, so project explicitly with `toUI`.

```scala
val cards: UI =
    val rows = for p <- catalog yield uic.Card().title(p.name).toUI
    fragment(rows*)
```

> **Note:** the ascription is only needed where the expected type is generic or absent. A container's child slot (`div(uic.Button("Save"))`), a `UI`-typed setter (`.footer(uic.Button("Open"))`), and a `UI*` varargs slot (`.start(uic.Icon(uic.Icons.bars))`) all lift without one.

### Every setter returns the component's own type

`Node` fixes the return type through an abstract `type Self`, and each component's setters are declared to return their own concrete class. `uic.Button.severity` returns `Button`, not `Node`, so the chain never widens and never loses the options further down it.

```scala
val deleteButton: uic.Button =
    uic.Button("Delete")
        .severity(uic.Severity.Danger)
        .variant(uic.ButtonVariant.Outlined)
        .icon(uic.Icons.trash)
        .size(uic.Size.Small)
```

The builder is a `case class` with `copy`-based setters, so it is immutable and shareable: build a base once and specialize it in several places without any risk of one call mutating another's configuration.

```scala
val base: uic.Button      = uic.Button().severity(uic.Severity.Secondary).size(uic.Size.Small)
val edit: uic.Button      = base.icon(uic.Icons.pencil).accessibleName("Edit")
val duplicate: uic.Button = base.icon(uic.Icons.copy).accessibleName("Duplicate")
val toolbarRow: UI        = div(edit, duplicate)
```

### Why the import is `import kyo.uic`, not a wildcard

The idiomatic style is to import the package and qualify every use. `uic.Button`, `uic.Severity.Danger`, `uic.Icons.trash`. The namespace carries the origin, which matters here more than usual: kyo-ui itself defines `input`, `select`, `label`, and `form`, and a wildcard import would put 83 component names next to them with nothing at the call site to say which layer a name came from.

## Binding state

There is no new reactive primitive in this module. kyo-ui's `Signal[A]` (read-only, re-renders on change) and `SignalRef[A]` (read-write, two-way) are the whole state story, and each slot declares which of the two it takes. Reading a setter's signature tells you what it can do: a slot that takes a `Signal` can display change, a slot that takes a `SignalRef` can also be written by the user.

Text, boolean, and severity slots each come in two overloads, a constant and a `Signal`. Passing the signal form patches the rendered attribute or text node in place rather than rebuilding the component.

```scala
val stockBadge: UI < Async =
    for inStock <- Signal.initRef(true)
    yield div(
        uic.Tag(inStock.map(b => if b then "In stock" else "Sold out"))
            .severity(inStock.map(b => if b then uic.Severity.Success else uic.Severity.Danger)),
        uic.Button("Toggle").onClick(inStock.updateAndGet(!_))
    )
```

Interactive state binds only through a `SignalRef`, never through a bare value. `value`, `checked`, `selected`, `expanded`, `page`, `active`, and `open` all take a ref, and the component writes into it as the user interacts. There is no `onChange`-only path where the component holds private state you cannot read.

```scala
val filters: UI < Async =
    for
        query    <- Signal.initRef("")
        onlyLeft <- Signal.initRef(false)
        picked   <- Signal.initRef(Set.empty[String])
    yield div(
        uic.Input().placeholder("Name contains").value(query),
        uic.CheckBox("Only in stock").checked(onlyLeft),
        uic.MultiSelect[Category]().options(categories)(_.label).optionKey(_.id).value(picked)
    )
```

Constant, `Signal`, and `SignalRef` are three different intents and it is worth being deliberate about which you reach for. Use the constant overload when the value is fixed at build time, which keeps the component out of any reactive boundary. Use the `Signal` overload when the value is computed from state the component does not own (a `disabled` derived from a form's validity). Use the `SignalRef` overload for anything the user edits, because that is the only form that gives the component write access.

Event handlers are typed `Any < Async`, the same shape kyo-ui uses. The return value is discarded, so any effectful expression goes in directly, and a handler can suspend, call kyo-http, or write another ref.

```scala
val saveRow: UI < Async =
    for saving <- Signal.initRef(false)
    yield uic.Button("Save")
        .loading(saving)
        .onClick(saving.set(true).andThen(Console.printLine("saving")).andThen(saving.set(false))): UI
```

## The shared vocabulary

Prime's own props are inconsistent across components: a button's accent is `danger`, a message's is `error`, and sizes are spelled differently again. kyo-ui-components exposes one harmonized enum set across all 83 components and maps each case to the right `.p-*` suffix privately. You learn `uic.Severity` once.

`Severity` has `Primary`, `Secondary`, `Success`, `Info`, `Warn`, `Danger`, `Help`, and `Contrast`. `Size` has `Small`, `Normal`, `Large`, and `XLarge`. Alongside those sit `SelectionMode`, `ButtonVariant`, `FieldVariant`, `Orientation`, `LabelPosition`, `OverlayPosition`, `OverlayAnchor`, `ColumnAlign`, `TitleLevel`, and `TextEmptyIndicatorMode`, plus per-component enums (`DatePickerView`, `MultiSelectDisplay`, `DrawerPosition`, `SkeletonShape`, and so on) that only one family needs.

"No accent" is `Maybe[Severity]` at the component's own boundary, expressed by simply not calling the setter. There is no `Severity.None` case, because validity is a separate axis (`invalid` plus `invalidMessage`), exactly as in Prime's `.p-invalid` model.

```scala
val statuses: UI =
    div(
        uic.Tag("New").severity(uic.Severity.Info),
        uic.Tag("Discontinued").severity(uic.Severity.Danger),
        uic.Tag("Draft")
    )
```

### The discoverability contract

Each component declares only the cases it means, and the *cases* are typed as narrowly as the setters. `Size` has three values; `Size.XLarge` is a fourth that is deliberately not one of them, so `uic.Avatar.size` and `uic.Badge.size` take it (the design system defines an extra-large avatar and badge) and every other `size` refuses it at compile time rather than quietly rendering large. In the same way, a setter a component does not have does not exist on its type at all, so it does not compile and does not show up in autocomplete. Discovering a component's surface is a matter of typing `.` after it.

### Icons are typed glyph values

An icon is never a CSS class string. `IconGlyph` is a value carrying the glyph's name, its raw SVG path data, and its viewBox, and it is rendered inline as an `<svg>` with `fill=currentColor`. Two sets ship: `uic.Icons`, the 309 PrimeIcons, and `uic.FioriIcons`, the 705-glyph SAP-icons-v5 set kept for migrations. Each glyph is a separate `def`, so Scala.js method-level dead-code elimination strips the path strings of every icon you did not reference.

```scala
val glyph: uic.IconGlyph = uic.Icons.shoppingCart

val iconRow: UI =
    div(
        uic.Icon(uic.Icons.filter).size(20).accessibleName("Filter"),
        uic.Button("Refresh").icon(uic.Icons.refresh),
        uic.Chip("Accessories").icon(uic.Icons.tag)
    )
```

> **Note:** an `Icon` is decorative by default and renders `aria-hidden`. Giving it an `accessibleName` makes it an exposed image; giving it an `onClick` makes it a focusable button. The three states are chosen by which setters you call, so a purely ornamental glyph never lands in the accessibility tree by accident.

## Showing status and progress

A page is mostly not controls. It is the small marks that say what state a thing is in, how far along a job is, and how the reader should read what is in front of them. These components are the first consumers of `Severity`, which is why they sit here: a `Tag`, a `Badge`, and a `Message` all take the same eight cases and each renders them in its own skin.

### Marks on a value

`Tag` labels a value, `Badge` counts it, and `Chip` stands for something the user picked and can drop again. All three take a constant or a `Signal[String]` for their text, so a status that changes patches in place rather than rebuilding.

```scala
val marks: UI =
    div(
        uic.Tag("In stock").severity(uic.Severity.Success).icon(uic.Icons.check),
        uic.Tag("Discontinued").severity(uic.Severity.Danger).rounded(true),
        uic.Badge("3").severity(uic.Severity.Info),
        uic.Badge().severity(uic.Severity.Danger),
        uic.Chip("Accessories").icon(uic.Icons.tag).removable(true).onRemove(Console.printLine("dropped"))
    )
```

A `Badge` with no value is the status dot, and a single-character value takes the circular disc automatically. A reactive value never takes it, because its length is unknown at render time. `OverlayBadge` pins a badge to the top-end corner of whatever it wraps, which is the counter-on-an-icon shape.

```scala
val cart: UI =
    uic.OverlayBadge(uic.Icon(uic.Icons.shoppingCart).size(24))(uic.Badge("2").severity(uic.Severity.Danger))
```

> **Note:** `Severity.Primary` and `Severity.Help` render as the unsuffixed base skin on `Badge` and `Tag`. Prime's badge and tag vocabulary has no case for them, so they look identical to passing no severity at all. Every other case is distinct on those two. `Message` folds both onto its `info` skin for the same reason.

### Messages the page owns

`Message` sits in the flow of the page rather than floating over it, which makes it the shape for a result that belongs to a region: an import summary above the table it describes, a warning above the fieldset it qualifies. It defaults to `Severity.Info` and picks its leading glyph from the severity, which `icon` overrides and `hideIcon` suppresses.

```scala
val importResult: UI < Async =
    for failed <- Signal.initRef(true)
    yield div(
        uic.Message().severity(uic.Severity.Warn)("Two rows were skipped."),
        when(failed)(
            uic.Message()
                .severity(uic.Severity.Danger)
                .variant(uic.MessageVariant.Outlined)
                .closable(true)
                .onDismissed(failed.set(false))("The import failed."): UI
        )
    )
```

> **Unlike** the modal surfaces under [Floating panels](#floating-panels), which own a visibility ref and write `false` into it before running your effect, `Message.onDismissed` and `Chip.onRemove` change no visibility at all. They run the effect and leave the element exactly where it was, which is why the message above is wrapped in `when(failed)`: the page owns whether it is on screen. The names say which contract you are in: `onClose` belongs to the surfaces that own a `SignalRef[Boolean]` and close themselves, and an inline element that owns no ref does not have it.

### Work in progress

A running job shows up in one of three ways, and which one you pick follows from what you know about it. `ProgressBar` in its default determinate mode wants a percentage; the same bar in `Indeterminate` mode is for work whose end you cannot predict; `ProgressSpinner` is the same statement in the space of an icon.

```scala
val importing: UI < Async =
    for done <- Signal.initRef(0)
    yield div(
        uic.ProgressBar().value(done).valueTemplate(v => s"$v of 100 rows"),
        uic.ProgressBar().mode(uic.ProgressBarMode.Indeterminate),
        uic.ProgressSpinner().size(uic.Size.Small).accessibleName("Loading the catalog")
    )
```

`ProgressBar.value` has three overloads and the third is the one most work wants. `value(Int)` is a constant, `value(SignalRef[Int])` binds a ref you also write from elsewhere, and `value(Signal[Int])` tracks a derived signal (`loaded.combineLatest(total).map(pct)`), which is what progress usually is: computed from other state, never edited.

`Skeleton` covers the moment before there is any content to report on, holding the shape the content will take.

```scala
val placeholder: UI =
    uic.Card()(
        uic.Skeleton().shape(uic.SkeletonShape.Circle).size("3rem"),
        uic.Skeleton().width("60%").height("1.5rem"),
        uic.Skeleton().height("1rem").animation(false)
    )
```

`MeterGroup` is the segmented bar for a whole that divides into parts: stock by category, spend by account, disk by mount. Each meter scales against the group's `max`, and a segment that rounds to 0% draws nothing while still listing its label.

```scala
val stockMix: UI =
    uic.MeterGroup()
        .meter("Accessories", 65.0, uic.Icons.tag)
        .meter("Fitness", 20.0)
        .max(100.0)
        .labelOrientation(uic.Orientation.Vertical)
```

### People

`Avatar` renders initials, a glyph, or an image, in that precedence, and `AvatarGroup` stacks several of them with Prime's overlap. An avatar is announced as an image by default; `interactive(true)`, or an `onClick` which implies it, makes it a focusable button with Enter and Space activation.

```scala
val reviewers: UI =
    div(
        uic.Avatar()
            .initials("AL")
            .shape(uic.AvatarShape.Circle)
            .size(uic.Size.XLarge)
            .badge(uic.Badge().severity(uic.Severity.Success)),
        uic.AvatarGroup(
            uic.Avatar().initials("AL").shape(uic.AvatarShape.Circle),
            uic.Avatar().icon(uic.Icons.user).shape(uic.AvatarShape.Circle),
            uic.Avatar().initials("+3").shape(uic.AvatarShape.Circle)
        )
    )
```

### Text, headings, and links

Three components carry plain content rather than state, and they exist so that ordinary prose picks up the same tokens as everything around it. `Title` keeps its semantic level and its visual size as separate settings, so a heading can sit at the right place in the document outline while looking like a smaller one. `Text` is the body span, with a line clamp and a dash for the empty case. `Link` composes kyo-ui's own `<a>`, so `href`, focus, and click behaviour are the native ones under Prime's link skin.

```scala
val productHeader: UI =
    div(
        uic.Title().level(uic.TitleLevel.H2).size(uic.TitleLevel.H4)("Bamboo Watch"),
        uic.Text().maxLines(2)("A wooden watch with a fabric strap."),
        uic.Text().emptyIndicatorMode(uic.TextEmptyIndicatorMode.On)(),
        uic.Link("Full specification").href("/products/p1").endIcon(uic.Icons.externalLink).target("_blank")
    )
```

> **Note:** a disabled `Link` drops its `href` and its click handler, leaves the tab order, and exposes `aria-disabled` alongside the `.p-disabled` skin. An anchor has no native `disabled` attribute, so the state has to be assembled from those four pieces, and assembling only the skin leaves a link that looks dead and still navigates.

## Installing the theme

Nothing injects the stylesheet for you. Without `uic.Theme.css` on the page, every component renders correct `.p-*` markup and every one of them is completely unstyled. That sheet is Prime's own CSS, extracted from the MIT `@primeuix` packages at build time and frozen into generated Scala sources, and `uic.Theme.css` is the whole of it as one `String`: design tokens (`generated.Tokens`), the per-component `.p-*` rules (`generated.ComponentCss`), and a small kyo-specific remainder (`Theme.primeExtraCss`) covering the pieces Prime implements in JS or in slots.

```scala
val head: PageHead = PageHead(title = "Catalog", css = uic.Theme.css)

val page: Stream[String, Async] = UI.runRenderPage(head)(div(uic.Button("Save")))
```

For a client-side mount, put the same string in the host page's `<style>` block. `Theme.primeCss`, `Theme.primeTokensCss`, and `Theme.primeExtraCss` expose the layers individually if you are assembling the page CSS yourself.

### Presets and the dark scheme

Four presets ship: Aura (the default, declared on `:root`), Material, Lara, and Nora. You select one with a `data-theme` attribute on any ancestor, and the dark scheme with `data-scheme="dark"`. The two compose, because the preset diffs and the dark set are separate token blocks.

```scala
val themed: UI =
    div.data("theme", "material").data("scheme", "dark")(
        uic.Card().title("Bamboo Watch")(p("65.00 EUR"))
    )
```

The Aura base token set is re-declared on every theme and scheme scope, not only on `:root`. Component tokens are `var()` chains, and CSS substitutes a `var()` at the element that declares the property, so a `data-scheme="dark"` scope below `<html>` only re-derives those chains if the base set is declared on the scope element too.

### The one class you can still stamp yourself

Floating panels position against the nearest positioned ancestor, so a panel's anchor element must be `position: relative`. The sheet provides `p-uic-overlay-anchor` for exactly that, and the failure when it is missing is silent and visual: the panel positions against some far-away ancestor.

Hand a component its trigger and it owns the anchor for you. Components with a built-in trigger (`Select`, `MultiSelect`, `DatePicker`) stamp it; `Popover` and `Overlay` stamp it as soon as you give them a `trigger`.

```scala
val anchored: UI < Async =
    for open <- Signal.initRef(false)
    yield uic.Overlay(open)
        .trigger(uic.Button("Options").onClick(open.set(true)))(
            uic.Menu().items(uic.MenuItem("Refresh")).popup(open): UI
        ): UI
```

Attaching a panel to a container you build yourself is still allowed, and then the class is yours to stamp.

```scala
val handAnchored: UI < Async =
    for open <- Signal.initRef(false)
    yield div.cssClass("p-uic-overlay-anchor")(
        uic.Button("Options").onClick(open.set(true)),
        uic.Menu().items(uic.MenuItem("Refresh")).popup(open): UI
    )
```

## Entering values

Every form control in the library follows one shape: a two-way `value` (or `checked`) binding, and an `invalid` plus `invalidMessage` pair for validity, each in a constant and a reactive form. That set is not a convention to remember but the `FormControl` trait, so a control that holds a user-supplied value carries all of it or is not one. `size` and `variant` ride along wherever the design system defines them, which is the field-shaped controls. This section is the controls alone; the separate validation layer that computes `invalid` for you is [Validated forms](#validated-forms) below.

### Text entry

Most of what a user types is a string, and the differences between the controls that collect one are about the shape the string arrives in, not about the binding. `Input`, `TextArea`, `Password`, `InputMask`, and `InputOtp` all bind a `SignalRef[String]`.

```scala
val productName: UI < Async =
    for name <- Signal.initRef("")
    yield div(
        uic.Label("Name").forId("name").required(true),
        uic.Input().id("name").placeholder("Bamboo Watch").value(name).maxLength(80)
    )
```

`Password` adds an eye toggle and Prime's inline strength feedback; `InputOtp` spreads one bound string over N masked cells; `InputMask` formats against a fixed pattern as the user types, where `9` is a digit, `a` a letter, `*` either, and every other character a literal the field inserts for you.

```scala
val credentials: UI < Async =
    for
        secret <- Signal.initRef("")
        code   <- Signal.initRef("")
    yield div(
        uic.Password().value(secret).toggleMask(true).feedback(true).promptLabel("Enter a password"),
        uic.InputOtp().value(code).length(6).integerOnly(true).mask(true)
    )

val skuField: UI < Async =
    for sku <- Signal.initRef("")
    yield uic.InputMask("aaa-9999").value(sku).placeholder("acc-0001"): UI
```

`InputNumber` is the one text-shaped control whose value is a `Double` rather than a `String`. It carries the native `min`, `max`, and `step`, optional spin buttons, and prefix/suffix adornments.

```scala
val priceField: UI < Async =
    for price <- Signal.initRef(0.0)
    yield uic.InputNumber()
        .value(price)
        .min(0)
        .max(10000)
        .step(0.5)
        .showButtons(true)
        .buttonLayout(uic.InputNumberButtonLayout.Stacked)
        .suffix(" EUR"): UI
```

### Booleans and toggles

`CheckBox`, `RadioButton`, and `ToggleSwitch` bind a `SignalRef[Boolean]` through `checked`. `ToggleButton` is the pressed-state button variant, with separate labels and glyphs per state.

```scala
val availability: UI < Async =
    for
        inStock <- Signal.initRef(true)
        listed  <- Signal.initRef(false)
    yield div(
        uic.CheckBox("In stock").checked(inStock),
        uic.ToggleSwitch().checked(listed).handleIcon(uic.Icons.check, uic.Icons.times),
        uic.ToggleButton().checked(listed).onLabel("Listed").offLabel("Hidden")
    )
```

### Numbers, colour, and rating

`Slider` and `Knob` share a `Double` vocabulary; `Rating` binds an `Int`; `ColorPicker` binds a hex `String`.

```scala
val presentation: UI < Async =
    for
        weight <- Signal.initRef(50.0)
        stars  <- Signal.initRef(4)
        accent <- Signal.initRef("#4E46E0")
    yield div(
        uic.Slider().value(weight).min(0).max(100).step(5).accessibleName("Weight"),
        uic.Knob().value(weight).size(120).valueTemplate(v => s"${v.toInt}%"),
        uic.Rating().value(stars).stars(5),
        uic.ColorPicker(accent)
    )
```

> **Note:** a `Slider`'s visual fill follows a pointer drag only on release, because the native `change` event commits at pointer-up. Keyboard stepping commits per keypress and does track live. `Knob` and `Rating` commit per interaction.

> **Caution:** `ColorPicker` is one of the two components with an explicit latency cost. Dragging its colour plane is one server round-trip per animation frame, which is crisp locally and visibly stepped over a slow link. The other is `VirtualScroller`, covered under [Displaying data](#displaying-data).

### Dates are ISO-prefix strings

The whole date surface binds `String`, not a `Date` object, and the string is always an ISO prefix: `YYYY-MM-DD` normally, `YYYY-MM-DDTHH:MM` with `showTime`, `HH:MM` with `timeOnly`, `YYYY-MM` under `view(Month)`, and `YYYY` under `view(Year)`.

```scala
val restock: UI < Async =
    for
        date <- Signal.initRef("2026-08-16")
        open <- Signal.initRef(false)
    yield uic.DatePicker()
        .value(date)
        .open(open)
        .showButtonBar(true)
        .today("2026-08-16")
        .placeholder("YYYY-MM-DD"): UI
```

> **Note:** `today(iso)` is explicit, and deliberately so. A pure render never reads a clock, so the component cannot know which day to circle unless you tell it. The same picker also covers multiple selection (`values(SignalRef[Set[String]])`), ranges (`range(startRef, endRef)`), and time (`showTime`, `timeOnly`, `hourFormat`), all on the same string model.

### File selection

`FileUpload` is basic mode only: it hands your handler a `Seq[UI.FilePayload]`, each carrying the file's name, size, MIME type, and content, and does nothing else. There is no upload URL and no `auto` machinery, because the file goes wherever your handler sends it. Those same payloads are also its bound `value`, which is what makes a size cap or an extension check an ordinary validation rule.

```scala
val importer: UI =
    uic.FileUpload()
        .inputId("catalog-import")
        .accept(FileAccept.Extension(".csv"))
        .onSelect(files => Kyo.foreach(files)(f => Console.printLine(s"picked ${f.name} (${f.size} bytes)")))

val importField: UI < Async =
    for picked <- Signal.initRef(Seq.empty[UI.FilePayload])
    yield uic.FileUpload().inputId("catalog-import-2").value(picked): UI
```

> **Caution:** the choose affordance is a real `<label for>`, so `inputId(...)` must be a stable, page-unique id. Without it the native file dialog never opens.

### Decorating a control

Three different mechanisms sit near each other here, and it is worth keeping them apart. `FloatLabel`, `IftaLabel`, and `IconField` are single-host WRAPPERS: each takes one control builder as a value and keeps its concrete type, which is what lets them stamp classes onto it (`.p-filled`, the icon paddings). `InputGroup` is a CONTAINER of already-built children in visual order; Prime's CSS keys on child position, so the group needs no handle on any child. `Label` is neither — it stands beside a field and links to it by `forId`.

```scala
val decorated: UI < Async =
    for name <- Signal.initRef("")
    yield div(
        uic.IconField(uic.Input().placeholder("Search")).iconStart(uic.Icons.search),
        uic.FloatLabel(uic.Input().id("fl-name").value(name), "Product name").forId("fl-name"),
        uic.InputGroup()(
            uic.InputGroup.addon(uic.Icon(uic.Icons.tag)),
            uic.Input().placeholder("SKU"),
            uic.InputGroup.addon(uic.Button("Check"))
        )
    )
```

`FloatLabel` and `IftaLabel` accept the same four hosts — `Input`, `TextArea`, `Select`, `AutoComplete` — because they are one idea with different label placement; `IconField` accepts `Input`, `TextArea`, and `Select`. A host they do not accept is a compile error, not a silently unwrapped control.

### Validity before the validation layer

Every control carries `invalid` and `invalidMessage`, both with a constant and a `Signal` overload. That pair is Prime's `.p-invalid` model: `invalid` stamps the red state and `invalidMessage` renders the message row beneath the field. You can drive them by hand from any signal you already have, which is the whole story for a form too small to want a validation layer.

```scala
val handRolled: UI < Async =
    for name <- Signal.initRef("")
    yield
        val missing: Signal[Boolean]       = name.map(_.trim.isEmpty)
        val message: Signal[Maybe[String]] = missing.map(m => if m then Present("Name is required") else Absent)
        uic.Input().value(name).invalid(missing).invalidMessage(message): UI
```

## Choosing from options

Every picker shares one typed shape: a `Seq[A]` of options, a label projection `A => String`, an optional key projection `A => String`, and a bound selection. The selection binds as the *keys*, not as `A`, because keys are what a control can persist and round-trip. `TreeSelect` takes the same shape plus one projection more, `children: A => Seq[A]`, which is the whole difference between a tree-shaped picker and a flat one.

```scala
val categoryPicker: UI < Async =
    for chosen <- Signal.initRef("acc")
    yield uic.Select[Category]()
        .options(categories)(_.label)
        .optionKey(_.id)
        .value(chosen)
        .placeholder("Category")
        .showClear(true): UI
```

> **Caution:** `optionKey` defaults to the label projection, so two options that render the same text collapse onto one selection key. Set it whenever the label is not unique, which is most of the time for a real domain. The same default applies to `MultiSelect`, `SelectButton`, and `CascadeSelect`. This one used to fail silently on exactly the data that triggers it; now the panel renders a loud `.p-uic-key-error` card naming the colliding keys, so a duplicate-label data set shows up the first time it is rendered rather than the first time someone picks the wrong row.

### Grouping options

`Select`, `MultiSelect`, `AutoComplete` and `Listbox` also take their options as `OptionItem` rows, where `OptionItem.group` heads a labelled block and `OptionItem.item` stands beside them ungrouped. It is a separate method rather than an `options` overload, because both would erase to the same signature.

```scala
val groupedPicker: UI < Async =
    for chosen <- Signal.initRef("acc")
    yield uic.Select[Product]()
        .optionGroups(Seq(
            uic.OptionItem.group("In stock")(catalog.filter(_.inStock)*),
            uic.OptionItem.group("Backordered")(catalog.filterNot(_.inStock)*)
        ))(_.name)
        .optionKey(_.id)
        .value(chosen)
        .filterable(true): UI
```

Grouping is a panel concern only. The bound value, the key projection and the keyboard all address the same flat option sequence they would without it, so the highlight walks straight across a header rather than landing on one, and a filter that empties a group takes its header with it. Rows render in the order you pass them and are never reordered or merged: two groups sharing a label stay two groups.

The group is a real `li[role=group]` carrying its label as its accessible name, with the header and the options in a nested list that drops back out of the accessibility tree. Prime renders the header as a flat sibling of the options it labels, which looks right but tells a screen reader nothing; this version measures identically and does not.

### The family

Which picker you reach for follows from three questions the shared shape leaves open: one selection or many, floating panel or always visible, flat options or a nested tree. `Select[A]` picks one option from a floating panel. `MultiSelect[A]` picks many, binding a `SignalRef[Set[String]]` and displaying the selection as chips or as a summary label. `SelectButton[A]` is the same choice rendered as a button group, single or multiple. `AutoComplete[A]` is a text field that matches its options against what you type. `CascadeSelect[A]` walks a nested option tree through side panels. `TreeSelect` puts a real `Tree` in the panel.

```scala
val pickers: UI < Async =
    for
        one    <- Signal.initRef("acc")
        many   <- Signal.initRef(Set("acc"))
        typed  <- Signal.initRef("")
        nested <- Signal.initRef("")
    yield div(
        uic.MultiSelect[Category]()
            .options(categories)(_.label)
            .optionKey(_.id)
            .value(many)
            .display(uic.MultiSelectDisplay.Chip)
            .maxSelectedLabels(3)
            .filterable(true),
        uic.SelectButton[Category]().options(categories)(_.label).optionKey(_.id).value(one).allowEmpty(false),
        uic.AutoComplete[Product]().options(catalog)(_.name).optionKey(_.id).value(typed).minQueryLength(2),
        uic.CascadeSelect[String]()
            .options(Seq(uic.CascadeItem.group("Accessories")(uic.CascadeItem.leaf("Watches"))))(identity)
            .value(nested)
    )
```

`TreeSelect` takes typed options like the rest, with `children` supplying the structure the flat pickers do not need.

```scala
final case class Dept(id: String, name: String, subs: List[Dept])

val treePicker: UI < Async =
    for chosen <- Signal.initRef(Set("acc"))
    yield uic.TreeSelect()
        .options(Seq(Dept("acc", "Accessories", Nil), Dept("fit", "Fitness", List(Dept("yoga", "Yoga", Nil)))))(_.name, _.id)(_.subs)
        .value(chosen)
        .selectionMode(uic.SelectionMode.Multiple)
        .placeholder("Category"): UI
```

A `TreeNode` also carries an icon, a tooltip, and its own accessible name, which no `A => String` projection supplies. When you need those, hand the nodes over directly instead — the same relation `Listbox.items` has to `Listbox.item`.

```scala
val handAuthoredTree: UI < Async =
    for chosen <- Signal.initRef(Set("acc"))
    yield uic.TreeSelect()
        .nodes(
            uic.TreeNode("Accessories", "acc", icon = Present(uic.Icons.tag)),
            uic.TreeNode("Fitness", "fit", children = List(uic.TreeNode("Yoga", "yoga")))
        )
        .value(chosen)
        .placeholder("Category"): UI
```

`Listbox` is the family's inline member: the same option model, always visible, no floating panel. It is the right choice when the options are the page rather than a control on it.

```scala
val inlineList: UI < Async =
    for
        picked <- Signal.initRef(Set.empty[String])
        query  <- Signal.initRef("")
    yield uic.Listbox()
        .selectionMode(uic.SelectionMode.Multiple)
        .checkmark(true)
        .filterQuery(query)
        .item("Accessories", "acc", icon = Present(uic.Icons.tag))
        .item("Fitness", "fit")
        .value(picked)
        .emptyMessage("No matches"): UI
```

Its bound selection is spelled `value`, like every other picker's, because a Listbox is the always-visible member of the family rather than a different concept — and for the same reason it is a form control, so `uic.Listbox().bind(field)` works on a `Set[String]` field exactly as `MultiSelect` does.

Filtering has one word per concept across the whole family, and each component offers the ones it can mean. `filterable(Boolean)` renders the header filter over a query the component allocates itself; `filterQuery(SignalRef[String])` renders the same header over a query *you* own, which is what you want when the option list is fetched per keystroke. `Listbox`, `Select`, and `MultiSelect` all have both. `AutoComplete` has neither, because it filters by construction; what it has instead is `filterMode(FilterMode)`, the matching strategy — `StartsWithPerTerm`, `StartsWith`, `Contains`, or `None` when you pre-filter server-side.

## Floating panels

One primitive sits underneath everything that floats. `Overlay` is a backdrop plus a panel bound to a `SignalRef[Boolean]`, and every dialog, drawer, popover, menu panel, and select panel is built from it. That is why dismissal, focus, nesting, and geometry behave identically across the family: there is only one implementation of them.

```scala
val rawOverlay: UI < Async =
    for open <- Signal.initRef(false)
    yield div.cssClass("p-uic-overlay-anchor")(
        uic.Button("Show panel").onClick(open.set(true)),
        uic.Overlay(open)
            .anchor(uic.OverlayAnchor.BottomEnd)
            .matchWidth(true)
            .maxHeight("16rem")
            .dismissOnOutsideClick(true)
            .dismissOnEscape(true)
            .seedFocus(true)(
                div(p("Panel content"))
            ): UI
    )
```

Outside-click dismissal and Escape are both per level, so a menu opened from inside a dialog closes only the menu on the first Escape and only the dialog on the second. `autoFlip` measures the panel after render and flips it to the opposite side when it would overflow the viewport; `scroll` chooses between `Overlay.Scroll.Close` and `Overlay.Scroll.Lock` for what a page scroll does to an open panel.

> **Caution:** `portal(false)` (the default) renders the panel where it is declared. A `transform`, `filter`, or `will-change` on any ancestor then becomes the containing block for `position: fixed`, which traps the overlay's full-viewport backdrop inside that ancestor and stops outside-click dismissal working outside its box. Turn `portal(true)` on when the panel lives under a transformed ancestor.

> **Note:** the host-gated `renderOpen` path that menu-family submenus use keeps the declared anchor deliberately: no `autoFlip`, no `portal`. Only the standard `render` path measures the panel and re-homes it.

### Modal surfaces

Some panels are an interruption: the page behind them is not to be used until the reader deals with what is in front. `Dialog`, `ConfirmDialog`, and `Drawer` are the three that behave that way. They seed focus into the panel on open, trap Tab inside it while open, and restore focus to the trigger on close.

```scala
val deleteFlow: UI < Async =
    for confirm <- Signal.initRef(false)
    yield div(
        uic.Button("Delete").severity(uic.Severity.Danger).onClick(confirm.set(true)),
        uic.ConfirmDialog(confirm)
            .header("Delete product?")
            .message("This cannot be undone.")
            .icon(uic.Icons.exclamationTriangle)
            .acceptLabel("Delete")
            .rejectLabel("Keep")
            .acceptSeverity(uic.Severity.Danger)
            .onAccept(Console.printLine("deleted"))
    )
```

`Dialog` is the general form, taking your own header, footer, and body; `Drawer` is the same thing docked to an edge.

```scala
val editorDrawer: UI < Async =
    for open <- Signal.initRef(false)
    yield div(
        uic.Button("Edit").onClick(open.set(true)),
        uic.Drawer()
            .open(open)
            .position(uic.DrawerPosition.Right)
            .header("Edit product")
            .footer(uic.Button("Apply").onClick(open.set(false)))(
                p("Editor body")
            )
    )
```

> **Note:** `Dialog.onClose` and `Drawer.onClose` fire only when the component dismisses *itself*: the close button, Escape, a backdrop click, or a mask click. Each of those writes `false` into your ref first and then runs the effect. Writing `false` into the ref yourself closes the panel silently, which is what you want when the close is already part of a larger action you are performing.

The inline dismissible elements take the opposite convention, and the contrast is worked through under [Messages the page owns](#messages-the-page-owns): `Message.onDismissed` and `Chip.onRemove` change no visibility at all, because neither owns a ref to write into — which is why neither has an `onClose` at all.

### Popover, tooltip, context menu

`Popover` is a non-modal panel with an optional `trigger`, which is the shape to reach for when the panel should not take focus away from the page. `ContextMenu` wraps a region and opens on right-click.

```scala
val rowActions: UI < Async =
    for open <- Signal.initRef(false)
    yield div(
        uic.Popover(open)
            .trigger(uic.Button("Details").onClick(open.set(true)))
            .anchor(uic.OverlayAnchor.BottomStart)(
                p("65.00 EUR, 12 in stock")
            ),
        uic.ContextMenu(
            Seq(
                uic.MenuItem("Copy").icon(uic.Icons.copy),
                uic.MenuItem.separator,
                uic.MenuItem("Delete").icon(uic.Icons.trash)
            )
        )(div(span("Right-click a row")))
    )
```

> **Note:** the context menu anchors to the wrapped region, not to the pointer. kyo-ui's `MouseEvent` carries no coordinates, so there is nowhere to read a click point from. Prime opens at the click position; this one opens at the region.

`Tooltip` wraps its target and shows a box on hover or focus.

```scala
val hinted: UI = uic.Tooltip("Removes the product").position(uic.TooltipPosition.Bottom)(uic.Button("Delete"))
```

> **Unlike** Prime's tooltip, which is a JS directive that mounts and positions a box on pointer events with timers, this one is pure CSS: the box is always in the DOM and `:hover` / `:focus-within` reveal it. That buys zero state and zero round-trips, and costs the JS-only affordances. There is no `showDelay`, `hideDelay`, or `autoHide`, the box is not interactive, and it does not flip at viewport edges. Pick a position that fits.

### Toasts

A single `uic.Toast` is a declarative overlay bound to your own `SignalRef[Boolean]`, exactly like a dialog.

```scala
val savedToast: UI < Async =
    for shown <- Signal.initRef(false)
    yield div(
        uic.Button("Save").onClick(shown.set(true)),
        uic.Toast()
            .open(shown)
            .position(uic.OverlayPosition.BottomRight)
            .severity(uic.Severity.Success)
            .summary("Saved")
            .detail("Catalog updated.")
            .duration(2500)
    )
```

> **Caution:** a single `Toast` runs no timer. `duration(ms)` is emitted as a `data-uic-duration` attribute for the client runtime to act on; on the server side, timed dismissal is a fiber you fork yourself. The alternative is `ToastService`, which owns that fiber.

`ToastService` is the queued form: a store of messages provided as an `Env` service, rendering a stack and dismissing each message on its own schedule.

```scala
val toasted: UI < (Async & Env[uic.ToastService]) =
    uic.ToastService.use { toasts =>
        div(
            uic.Button("Save").onClick(toasts.add(uic.Severity.Success, "Saved", "Catalog updated.")),
            toasts.renderRegion(uic.OverlayPosition.BottomRight)
        ): UI
    }

val toastLayer: Layer[uic.ToastService, Sync] = uic.ToastService.layer
```

`use` resolves the service once, here, and hands the instance to the block, so everything the block builds — handlers included — closes over that instance. That matters because *where* you resolve it decides whether it works: kyo-ui event handlers erase their effect row and run on the event-drain fiber, which carries the root `Env` context in the browser transport but not under server-push dispatch, so an `Env.get` deferred *into* a handler passes local development and fails in the deployment that matters. `uic.Button.onClick` refuses such a handler outright — its parameter is `Any < Async`, so an `Env` effect cannot ride along — but a raw kyo element's `onClick` accepts it, so `use` is the habit to keep.

## Menus and navigation

Seven components render menus, and they all consume one typed item model. `MenuItem` carries a label, an optional icon, an optional url, an `onSelect` effect, a `disabled` flag, and nested `items`. `MenuItem.separator` is a rule. `MegaMenu` adds one layer over that model: its roots are `MegaMenuItem`s, and each `column(groups*)` call appends one column of titled `MenuGroup`s to the panel that root opens. Learning the model once covers `Menu`, `Menubar`, `TieredMenu`, `MegaMenu`, `ContextMenu`, `SplitButton`, and `SpeedDial`.

```scala
val fileMenu: Seq[uic.MenuItem] =
    Seq(
        uic.MenuItem("New").icon(uic.Icons.plus),
        uic.MenuItem("Open").url("/open"),
        uic.MenuItem.separator,
        uic.MenuItem("Recent").items(uic.MenuItem("catalog.csv"), uic.MenuItem("prices.csv"))
    )

val bar: UI = uic.Menubar().start(span("CATALOG")).items(uic.MenuItem("File").items(fileMenu*))
```

`Menu` renders inline by default and becomes a popup when you give it a visibility ref via `popup`. `TieredMenu` is the vertical form with side-nested submenus, `MegaMenu` opens one panel of grouped columns per root item, and `SplitButton` is a primary action with an attached menu.

```scala
val saveSplit: UI =
    uic.SplitButton("Save")
        .icon(uic.Icons.check)
        .severity(uic.Severity.Success)
        .items(uic.MenuItem("Save as copy"), uic.MenuItem.separator, uic.MenuItem("Discard"))
```

`SpeedDial` is the same items rendered as a fan of rounded icon buttons behind one toggle, which is the floating-action shape. `direction` is the setting that matters: `Up` is the default, and `Down`, `Left`, and `Right` pick which way the fan travels, so the toggle can sit in any corner and still open into the page.

```scala
val quickActions: UI =
    uic.SpeedDial()
        .items(uic.MenuItem("Add product").icon(uic.Icons.plus), uic.MenuItem("Export").icon(uic.Icons.download))
        .direction(uic.SpeedDialDirection.Right)
        .accessibleName("Quick actions")
```

> **Note:** submenus open on click across the whole family, never on hover. A hover-open submenu would be a server round-trip per pointer movement, which is not a cost worth paying for a menu. Prime's hover styling from the stylesheet is untouched, so the family still looks the way you expect; only the open trigger differs. Keyboard navigation follows WAI-ARIA in full, including arrow keys into and out of submenus.

### Navigation that is not a menu

`Breadcrumb` is a trail, `Tabs` is a panel switcher bound to a `SignalRef[String]`, `Stepper` is a linear wizard, and `Paginator` is a page control you can also use standalone from any table.

```scala
val navChrome: UI < Async =
    for
        active <- Signal.initRef("overview")
        page   <- Signal.initRef(0)
    yield div(
        uic.Breadcrumb().home(uic.Icons.home, "/").item("Catalog", "/catalog").item("Bamboo Watch"),
        uic.Tabs()
            .tab("Overview", "overview")(p("Overview content"))
            .tab("Pricing", "pricing")(p("Pricing content"))
            .selected(active),
        uic.Paginator().totalRecords(120).rows(10).page(page).pageLinkSize(5).jumpToPageInput(true)
    )
```

`Stepper` accepts two bindings. `active(SignalRef[Int])` is index-based and is the compatibility path; `value(SignalRef[String])` keys each step by an explicit id and survives steps being inserted or reordered.

```scala
val wizard: UI < Async =
    for step <- Signal.initRef("details")
    yield uic.Stepper()
        .value(step)
        .linear(true)
        .step("Details", value = Present("details"))(p("Step 1"))
        .step("Pricing", value = Present("pricing"))(p("Step 2"))
        .step("Review", value = Present("review"))(p("Step 3")): UI
```

> **Note:** both refs may be bound at once, and `value` wins. Prefer `value` for anything but a migration.

## Displaying data

`DataTable[A]` takes typed rows and a list of `Column[A]`, and every derived view (sorting, filtering, paging, selection, expansion) is a pure function of that data plus the refs you bind. Nothing is stored inside the component.

A column is built one of two ways, and the difference matters. `Column[A]("Name")(_.name)` carries a text projection: it renders that text and the global filter can match against it. `Column[A]("Price").body(...)` carries a template instead: it renders whatever UI you return.

```scala
val productTable: UI < Async =
    for
        query    <- Signal.initRef("")
        page     <- Signal.initRef(0)
        selected <- Signal.initRef(Set.empty[String])
    yield div(
        uic.Input().placeholder("Filter").value(query),
        uic.DataTable[Product]()
            .rows(catalog)
            .rowKey(_.id)
            .columns(
                uic.Column[Product]("Name")(_.name).sortBy(_.name),
                uic.Column[Product]("Category")(_.category).sortBy(_.category),
                uic.Column[Product]("Price")(p => f"${p.price}%.2f").sortBy(_.price).align(uic.ColumnAlign.End),
                uic.Column[Product]("Stock").body(p =>
                    uic.Tag(if p.inStock then "In stock" else "Sold out")
                        .severity(if p.inStock then uic.Severity.Success else uic.Severity.Danger)
                )
            )
            .globalFilter(query)
            .paginate(10)(page)
            .selectionMode(uic.SelectionMode.Checkbox)
            .selected(selected)
            .stripedRows(true)
    )
```

> **Caution:** a column built without a text projection is invisible to `globalFilter`, which matches only against text projections. The Stock column above is filterable by nothing; if you want "sold out" to be a searchable term, give the column a text projection *and* a `body`.

> **Caution:** `rowKey` is what selection, expansion, and the `onRowClick` payload key on. Its fallback is the row's position in the original list, which survives sorting and filtering but not a change to the data: reorder the rows and every selection re-associates with a different record. Because the table cannot tell a static list from a live one, binding any of those three without a `rowKey` renders a loud `.p-uic-key-error` card above the table instead of shipping that failure to production data.

Sorting is opt-in per column through `sortBy`, and the header click cycle (ascending, descending, unsorted) needs a `sort` ref on the table to persist. Row expansion pairs `expanded(ref)` with `rowExpansionTemplate`.

### Recursive shapes

`TreeTable[A]` is `DataTable`'s columns over recursive nodes, `Tree` is the plain hierarchy, and `OrganizationChart` is the top-down box diagram. All three bind selection as a `SignalRef[Set[String]]` of node keys, and all three bind their open state through `expanded(ref)` in the same currency and the same polarity, so one ref moves between them without inverting.

```scala
val hierarchy: UI < Async =
    for
        expanded <- Signal.initRef(Set("acc"))
        picked   <- Signal.initRef(Set.empty[String])
    yield div(
        uic.Tree()
            .selectionMode(uic.SelectionMode.Multiple)
            .nodes(
                uic.TreeNode(
                    "Accessories",
                    "acc",
                    icon = Present(uic.Icons.folder),
                    children = List(uic.TreeNode("Watches", "watches"))
                )
            )
            .expanded(expanded)
            .selected(picked),
        uic.TreeTable[Product]()
            .nodes(uic.TreeTableNode(catalog(0), List(uic.TreeTableNode(catalog(1)))))
            .columns(uic.Column[Product]("Name")(_.name), uic.Column[Product]("Category")(_.category))
            .rowKey(_.id)
            .expanded(expanded)
    )
```

Prime models the chart's open state collapse-keyed (`collapsedKeys`); `OrganizationChart` absorbs that inversion rather than exposing it, so `expanded(ref)` means here exactly what it means on `Tree`. Binding the ref is also what makes the chart interactive: with one, every node that has children renders its toggle; without one the chart is static and fully open, since there would be no toggle to open it with.

```scala
val orgChart: UI < Async =
    for
        open   <- Signal.initRef(Set("root", "fit"))
        picked <- Signal.initRef(Set.empty[String])
    yield uic.OrganizationChart()
        .node(
            uic.OrgChartNode(
                "Catalog",
                "root",
                children = List(
                    uic.OrgChartNode("Accessories", "acc"),
                    uic.OrgChartNode("Fitness", "fit", children = List(uic.OrgChartNode("Yoga", "yoga")))
                )
            )
        )
        .expanded(open)
        .selectionMode(uic.SelectionMode.Single)
        .selected(picked): UI
```

### The other views over a list

A table is one shape over a sequence of rows, and it is the wrong one as soon as the rows are cards, or the user is meant to reorder them, or they are moments in time. The same `Seq[Product]` feeds all of these. `DataView[A]` renders the rows through your own item template with list and grid layouts. `Carousel[A]` and `Timeline[A]` are the media and chronology shapes over the same typed rows.

`OrderList[A]` and `PickList[A]` are the two that let the user edit the sequence, so what they bind is a `SignalRef[Seq[A]]` rather than a plain `Seq[A]`. They differ only in how many columns they have. `OrderList[A]` is one column and one binding, `items(ref)(label)`, and its move buttons write the reordered `Seq` back.

```scala
val views: UI < Async =
    for
        page     <- Signal.initRef(0)
        ordering <- Signal.initRef(catalog)
        picked   <- Signal.initRef(Set.empty[String])
    yield div(
        uic.DataView[Product]()
            .items(catalog)
            .itemTemplate(p => div(span(p.name), span(f"${p.price}%.2f")))
            .header(span("Products"))
            .paginate(2)(page),
        uic.OrderList[Product]().items(ordering)(_.name).itemKey(_.id).selected(picked),
        uic.Carousel[Product]().items(catalog)(p => div(span(p.name))).page(page).numVisible(2).circular(true),
        uic.Timeline[Product]().events(catalog).content(p => span(p.name)).opposite(p => span(p.category))
    )
```

`PickList[A]` is two of that: `sourceItems(ref)(label)` and `targetItems(ref)` each bind their own `SignalRef[Seq[A]]`, and a transfer appends the moved rows to the other column's ref. The names are deliberately `OrderList.items` twice over, so a PickList reads as two of the same thing rather than as a different model. `sourceSelected` and `targetSelected` are the optional selection refs, one per column, and the label projection is declared once on `sourceItems` because it renders both columns.

```scala
val transfer: UI < Async =
    for
        available <- Signal.initRef(catalog)
        chosen    <- Signal.initRef(Seq.empty[Product])
        srcPicked <- Signal.initRef(Set.empty[String])
    yield uic.PickList[Product]()
        .sourceItems(available)(_.name)
        .targetItems(chosen)
        .itemKey(_.id)
        .sourceSelected(srcPicked)
        .showTargetControls(false): UI
```

`Galleria` is the one view here that is not generic over your rows. Its model is a fixed `GalleriaItem`: an image `src`, its `alt` text, an optional `thumbnailSrc`, and an optional `title`/`subtitle` caption pair. That fixed shape is what buys it the thumbnail strip, which renders each item's `thumbnailSrc` (falling back to `src`), and the caption, which renders the `title`/`subtitle` pair. `Carousel[A]` has neither, because neither can be read off an arbitrary `A`. Indicator dots and prev/next navigators are positional, derived from the item count rather than from the item type, so both components have them under the same names, `showIndicators` and `showItemNavigators`. Their DEFAULTS differ: on by default in `Carousel`, off in `Galleria`. That split is Prime's own between the two components, inherited so the out-of-the-box look matches the design system rather than a local convention. `activeIndex(ref)` binds the current image, and `itemTemplate` / `thumbnailTemplate` replace the default `img` on either side.

```scala
val gallery: UI < Async =
    for active <- Signal.initRef(0)
    yield uic.Galleria()
        .items(
            uic.GalleriaItem("/img/bamboo-watch.jpg", alt = "Bamboo Watch", title = Present("Bamboo Watch")),
            uic.GalleriaItem(
                "/img/black-watch.jpg",
                alt = "Black Watch",
                thumbnailSrc = Present("/img/black-watch-thumb.jpg")
            )
        )
        .activeIndex(active)
        .showIndicators(true)
        .showItemNavigators(true)
        .circular(true): UI
```

`VirtualScroller[A]` windows a long list to the rows in view, and `Terminal` is a command line whose handler is an ordinary effect.

```scala
val longList: UI = uic.VirtualScroller(catalog).itemSize(48).height(400)(p => div(span(p.name)))

val shell: UI < Async =
    for history <- Signal.initRef(Seq.empty[uic.TerminalCommand])
    yield uic.Terminal()
        .welcomeMessage("catalog shell")
        .prompt("$")
        .commands(history)
        .commandHandler(cmd => s"unknown: $cmd"): UI
```

> **Caution:** `VirtualScroller` is the second component with an explicit latency cost. Every scroll round-trips to the server to recompute the visible window. It is the right tool for a list too long to render whole, and the wrong one over a link where a round-trip is perceptible.

## Layout and containers

`Card`, `Panel`, `Fieldset`, `Accordion`, and `Inplace` are the structural components, and three of them animate a collapse: `Panel`, `Fieldset`, and `Accordion`. `Panel` and `Fieldset` bind their collapsed state to a `SignalRef[Boolean]` you own, so an app that persists "which sections were open" needs nothing extra.

```scala
val sections: UI < Async =
    for
        collapsed <- Signal.initRef(false)
        editing   <- Signal.initRef(false)
    yield div(
        uic.Panel().header("Details").toggleable(true).collapsed(collapsed).footer(span("3 items"))(
            uic.Card().title("Bamboo Watch").subtitle("Accessories")(p("65.00 EUR"))
        ),
        uic.Inplace()
            .display(span("Click to edit the name"))
            .content(uic.Input().placeholder("Product name"))
            .active(editing)
            .closable(true)
    )
```

`Accordion` has no `multiple` flag. The *type* of the bound ref picks the mode: `value(SignalRef[String])` keeps one panel open at a time, `value(SignalRef[Set[String]])` allows many. The illegal state ("multiple = false but two panels open") is therefore unrepresentable rather than defended against at runtime.

```scala
val singleOpen: UI < Async =
    for open <- Signal.initRef("shipping")
    yield uic.Accordion()
        .panel("Shipping", "shipping")(p("Ships in 2 days"))
        .panel("Returns", "returns")(p("30 day window"))
        .value(open): UI

val manyOpen: UI < Async =
    for open <- Signal.initRef(Set("shipping", "returns"))
    yield uic.Accordion()
        .panel("Shipping", "shipping")(p("Ships in 2 days"))
        .panel("Returns", "returns")(p("30 day window"))
        .value(open): UI
```

`Divider`, `Toolbar`, `FlexBox`, and `ScrollPanel` are the non-collapsing structural pieces. `FlexBox` exposes kyo-ui's own `Style.Alignment` and `Style.Justification` enums through lambdas, so the layout vocabulary is the framework's rather than a second one.

```scala
val layoutChrome: UI =
    div(
        uic.Toolbar()
            .start(uic.Avatar().initials("AL"), span("Ada Lovelace"))
            .end(uic.Button("Log out").variant(uic.ButtonVariant.Text)),
        uic.FlexBox().vertical(true).gap(16).align(_.stretch).justify(_.start)(
            p("above"),
            uic.Divider().align(uic.DividerAlign.Center).lineStyle(uic.DividerLineStyle.Dashed)(span("OR")),
            p("below")
        ),
        uic.ScrollPanel(p("long content")).width("100%").height("12rem")
    )
```

## Validated forms

Validation is a separate, opt-in layer in `kyo.uic.form`. Nothing above this section knows it exists: the controls already carry `invalid` and `invalidMessage`, and the form layer is a thing that computes them for you and coordinates submit. Three ideas carry it. Errors are data, never text. *When a rule re-computes* and *when its failure is shown* are independent knobs. And binding a field onto a control preserves the control's concrete type, so the form layer never hands you back an opaque wrapper.

A form is a mount region: `Form.mountedWith` allocates the state, runs your builder with the live `Form` scope, and returns an ordinary `UI`.

```scala
import kyo.uic.form.*

val nameForm: UI =
    Form.mountedWith(ErrorTranslator.default) { scope =>
        for name <- scope.field(Validator.required() and Validator.minLength(2), Activation.Blur)
        yield div(
            uic.Label("Product name").forId(name.domId).required(true),
            uic.Input().bind(name).placeholder("Bamboo Watch"),
            uic.Button("Save").onClick(scope.submit(Console.printLine("saved")))
        )
    }
```

`Form.mounted` and `Form.mountedWith` differ only in where the `ErrorTranslator` comes from. `mountedWith(translator)` takes it directly and returns a plain `UI`, which is the no-i18n path; `mounted()` resolves it from `Env` at build time and returns `UI < Env[ErrorTranslator]`, so an i18n-aware app installs one translator at the root and every form downstream picks it up.

```scala
val translated: UI < Env[ErrorTranslator] =
    Form.mounted() { scope =>
        for name <- scope.field(Validator.required(), Activation.Blur)
        yield div(uic.Input().bind(name))
    }

val translatorLayer: Layer[ErrorTranslator, Any] =
    ErrorTranslator.layer(ErrorTranslator.englishMap(Map("required" -> "This field is required")))
```

### Errors are data

A validation failure has to survive a locale change, a summary that renders it somewhere else on the page, and a test that asserts on it, so it never arrives as a sentence. A `FieldError` is a flat `code`, a `Map[String, String]` of args for interpolation, and an optional literal `fallback`. The validation core never turns a code into a message; an `ErrorTranslator` does, returning a `Signal[String]` so a locale change re-renders every message already on screen.

```scala
val tooLong: FieldError = FieldError("max-length", Map("max" -> "80"))

val errorTable: ErrorTranslator =
    ErrorTranslator.englishMap(
        Map(
            "required"   -> "Required",
            "min-length" -> "Too short",
            "max-length" -> "Too long"
        )
    )
```

`Validator[A]` maps a value to `Maybe[FieldError]` in `Async`. `Absent` is valid, `Present(e)` is invalid: failure as an error value, not as an `Abort`. The built-ins cover `required`, `minLength`, `maxLength`, `email`, `pattern`, `url`, `min`, `max`, and `matchesField`, and `and` chains them with an ordered short-circuit so a field shows exactly one message at a time.

```scala
val nameRules: Validator[String] = Validator.required() and Validator.minLength(2) and Validator.maxLength(80)

val skuRules: Validator[String] = Validator.required("sku-required") and Validator.pattern("[a-z0-9-]+".r)

val positive: Validator[Double] = Validator.satisfy[Double]("must-be-positive")(_ > 0)
```

An async rule (a uniqueness check against your backend, say) folds its own failure channel into a `Maybe[FieldError]` before it becomes a `Validator`, which is what keeps this layer free of any transport dependency.

```scala
def skuAvailable(check: String => Boolean < Async): Validator[String] =
    Validator.async(sku => check(sku).map(ok => if ok then Absent else Present(FieldError("sku-taken"))))
```

### Activation and Reveal are different questions

`Activation` says when a rule is re-computed. `Reveal` says when a failure is displayed. They are orthogonal, and conflating them is the usual source of forms that either nag while you type or stay silent until it is too late.

`Activation.Field` values combine, and a field validates on the union of the ones it declares. `Change` re-checks on every value change, `Blur` on focus loss, `Submit` at submit. `Reveal` picks the display gate: `WhenTouched` (the field default, so an untouched field is quiet), `OnSubmit`, `Immediate`, or `Manual` (never auto-shown, but still feeding `isValid` and still blocking submit).

```scala
val quietUntilSubmit: UI =
    Form.mountedWith(ErrorTranslator.default) { scope =>
        for
            name <- scope.field(Validator.required() and Validator.minLength(2), Activation.Change, Activation.Blur)
            _ = name.revealWhen(Reveal.OnSubmit)
        yield div(uic.Input().bind(name))
    }
```

That form re-validates as the user types (so a submit gate can open the instant the data becomes valid) while showing nothing until the first submit.

### Binding a field onto a control

`.bind(field)` wires the field's value ref, its minted DOM id, its gated message, and its blur trigger onto a control, and returns the same concrete control type. `uic.Input().bind(f)` is an `Input`, so setters keep chaining on either side of the call.

```scala
val bound: UI =
    Form.mountedWith(ErrorTranslator.default) { scope =>
        for
            name  <- scope.field(Validator.required() and Validator.minLength(2), Activation.Blur)
            stock <- scope.field(true, Validator.all[Boolean](), Activation.Change)
        yield div(
            uic.Input().bind(name).placeholder("Bamboo Watch").fluid(true),
            uic.CheckBox("In stock").bind(stock)
        )
    }
```

`bind` compiles only against a control implementing the matching `FormControl` trait, and the field's value type picks which one. Text fields bind to `Input`, `TextArea`, `Password`, `InputMask`, `InputOtp`, `Select`, `AutoComplete`, `CascadeSelect`, `DatePicker`, `ColorPicker`, and a single-select `SelectButton`. Boolean fields bind to `CheckBox`, `RadioButton`, `ToggleSwitch`, and `ToggleButton`. Number fields bind to `InputNumber`, `Slider`, `Knob`, and `Rating`. Multi-selection fields (`Set[String]`) bind to `MultiSelect`, `TreeSelect`, `Listbox`, and a `multiple(true)` `SelectButton`. A file field (`Seq[UI.FilePayload]`) binds to `FileUpload`, which is what lets a rule check a size cap or an extension against the metadata the picker handed over.

The rule behind that list is that every control holding a user-supplied value is a `FormControl`, and `FormControl` declares the whole validation vocabulary — `invalid` and `invalidMessage` in both their constant and reactive forms, plus `id`. A control cannot carry half of it. `size` and `variant` are deliberately not part of the contract: the PrimeOne sheet defines `.p-*-sm`/`-lg` and `.p-variant-filled` only for the field-shaped controls, so requiring them would mean inventing CSS Prime does not ship for a `Slider` or a `Rating`.

`bindControlOnly` is the same wiring minus the message row: it sets `invalid` but leaves the message for you to place, which is what you want when the message belongs in a summary or beside the label rather than under the field.

### Typed values: dates and numbers

`dateField` and `numberField` are typed facades over the `String` and `Double` a control actually holds. The underlying primitive value stays the single source of truth (a `SignalRef` has no bidirectional map, so a second typed ref could not stay honest), and the typed value is derived through a codec.

```scala
import java.time.LocalDate

val typedFields: UI =
    Form.mountedWith(ErrorTranslator.default) { scope =>
        for
            price <- scope.numberField(0.0, Validator.min(0.0) and Validator.max(10000.0), Activation.Blur)
            restock <- scope.dateField[LocalDate](
                Absent,
                Validator.satisfy[LocalDate]("too-early")(_.isAfter(LocalDate.of(2020, 1, 1))),
                Activation.Blur
            )
        yield div(
            uic.InputNumber().bind(price).suffix(" EUR"),
            uic.DatePicker().bind(restock).placeholder("YYYY-MM-DD")
        )
    }
```

`NumberCodec` ships `int`, `long`, and `double`; the whole-valued ones report `integer = true`, and `bind` flips the control's integer keystroke mask on for them, so a fraction cannot be entered into an `Int` field. `DateCodec.local` handles `LocalDate` and `DateCodec.zoned(zone, at)` builds a `ZonedDateTime` codec. `DateField.value` is a `Signal[Maybe[A]]`, because a picker can be empty; `NumberField.value` is a total `Signal[A]`, because an `InputNumber` always holds a number. Rules run against the typed value, and are skipped while a date picker is empty.

### Submitting

Submit is the moment every field stops being quiet, including the ones the user never reached. `Form.submit(onValid)` opens the display gate for every field, validates the whole subtree in parallel including untouched fields, runs your effect only if everything holds, and moves focus (and scroll) to the first invalid field otherwise.

```scala
val gated: UI =
    Form.mountedWith(ErrorTranslator.default) { scope =>
        for
            name  <- scope.field(Validator.required() and Validator.minLength(2), Activation.Change, Activation.Blur)
            gate  <- scope.submitDisabled
            dirty <- scope.isDirty
        yield div(
            uic.Input().bind(name),
            uic.Button("Save")
                .ariaDisabled(gate)
                .onClick(scope.submit(Console.printLine("saved").andThen(scope.markPristine))),
            uic.Button("Discard").variant(uic.ButtonVariant.Text).ariaDisabled(dirty.map(!_)).onClick(scope.reset)
        )
    }
```

That choice of setter is not left to the reader. `submitDisabled` returns a `SubmitGate`, a distinct type from `Signal[Boolean]`, and `ariaDisabled` is the only setter it fits. The reason is an accessibility trap that is easy to walk into and hard to notice: a natively disabled button leaves the tab order, and the reactive re-enable lands a tick after the Tab keypress, so focus jumps straight past the button the user was heading for. `ariaDisabled` announces the state without removing the element from the tab order, and routing the click through `form.submit` keeps a click on a disabled-looking button harmless (submit revalidates and blocks). Take `gate.signal` when you want the raw boolean for something that is not disabling a control, such as a spinner or a hint.

`submitDisabled` is the usual gate: blocked while submitting or while any field error is currently displayed, but open for a pristine form, so the first click runs submit-reveal.

> **Note:** `submitDisabled` deliberately ignores form-level `raise` errors. Those can only be cleared by re-submitting, so gating on them would disable the very button that produced them. They surface in the error summary instead. Field-level server errors do gate, because they are value-scoped and vanish on the first edit.

`isValid(onlyVisible = false)` is the raw view, reflecting the client and server verdict even for untouched fields, which is what you want for a button that should light up the moment the data becomes valid. `isValid(onlyVisible = true)` is the gated view. `isDirty` compares each field against its live baseline, `markPristine` moves that baseline to the current values (after a successful save), and `reset` returns to it.

### Server verdicts

`FormField.setError` pushes an error from outside the rule chain, which is how a server-side validation result lands on a field. It is recorded against the value it judged.

```scala
def onSave(name: FormField[String], taken: Boolean): Unit < Sync =
    if taken then name.setError(Present(FieldError("name-taken"))) else name.setError(Absent)
```

> **Note:** a server error is scoped to the value it was set against, so the first edit hides it. A stale server verdict can never outlive its value, and hiding it re-enables the submit button, which is the only way to re-check a server-side rule at all.

### Cross-field and form-level rules

Three shapes cover the cases, and which one you reach for follows from what the rule reads.

`field.matches(other)` is the password-confirmation pattern: it appends `Validator.matchesField` *and* wires `dependsOn(other)` so the field re-validates as soon as the other one changes. Forgetting the dependency is the usual footgun, and this call makes it impossible.

`field.addRule` plus `field.dependsOn(sigs*)` is the general form for a rule that reads a sibling without an equality check.

`scope.satisfy(code)(predicate)` attaches a rule to the whole scope, in the same `satisfy(code)(predicate)` shape a field rule uses. The predicate's *type* selects when it runs, which keeps the illegal combinations unrepresentable. A `Signal[Boolean]` is a reactive invariant: re-evaluated whenever its inputs change, blocking submit and making `isValid` false while it holds `false`. A `=> Boolean < Async` thunk is a submit-pull check: evaluated once per submit, so it may suspend on a round-trip.

```scala
val crossField: UI =
    Form.mountedWith(ErrorTranslator.default) { scope =>
        for
            listPrice <- scope.numberField(0.0, Validator.min(0.0), Activation.Blur)
            salePrice <- scope.numberField(0.0, Validator.min(0.0), Activation.Blur)
            _ = scope.satisfy("sale-above-list")(
                listPrice.value.combineLatest(salePrice.value).map((list, sale) => sale <= list)
            )
            summary <- Form.errorSummary(scope)
        yield div(
            uic.InputNumber().bind(listPrice).suffix(" EUR"),
            uic.InputNumber().bind(salePrice).suffix(" EUR"),
            summary,
            uic.Button("Save").onClick(scope.submit(Console.printLine("saved")))
        )
    }
```

`scope.raise(e)` pushes a form-level error with no field attached, which is where "invalid credentials" from a submit handler belongs. `scope.anchor(id)` names an element for the summary to jump focus to for those errors.

### Nesting and repetition

Everything so far assumed a form whose fields are known when you write it. A sub-form reused in three places and a row the user adds five of are not, and both come back to the same move: a scope inside a scope. `scope.child()` creates one. A reusable sub-form owns its own fields and its own summary, and its errors bubble into the parent's aggregate, so summaries can sit at the root and at sub-forms at the same time.

`scope.fieldArray` and `scope.fieldArrayOf` handle repeated rows. Each row is its own sub-scope built by your function, which receives the row's `Form` and a `FieldArray.Row` control carrying `remove`, `moveUp`, `moveDown`, and `index`. `fieldArrayOf` additionally returns a typed model per row, which is what gives cross-row rules something to read.

```scala
val lineItems: UI =
    Form.mountedWith(ErrorTranslator.default) { scope =>
        for
            rows <- scope.fieldArrayOf[FormField[String]](1) { (rowScope, row) =>
                for sku <- rowScope.field(Validator.required() and Validator.pattern("[a-z0-9-]+".r), Activation.Blur)
                yield (
                    div(
                        uic.Input().bind(sku).placeholder("SKU"),
                        uic.Button("Remove").variant(uic.ButtonVariant.Text).onClick(row.remove)
                    ): UI,
                    sku
                )
            }
            _ = rows.satisfy("duplicate-sku")(current =>
                Kyo.foreach(current)(_.value.current).map(skus => skus.distinct.size == skus.size)
            )
            summary <- Form.errorSummary(scope)
        yield div(
            rows.render,
            uic.Button("Add line").icon(uic.Icons.plus).onClick(rows.add),
            summary,
            uic.Button("Save").onClick(scope.submit(Console.printLine("saved")))
        )
    }
```

`rows.render` is a keyed list, so adding, removing, and moving rows preserve the DOM (and therefore the focus and caret) of the rows that stayed. Cross-row `satisfy` reads the live rows, so a deleted row never counts toward a check.

A row builder runs with `Async` only and no `Scope`, because rows are added later, from a handler, long after the mount body ran. `Activation.Change` works there anyway: the array wires each row's observer on an unscoped fiber and owns it, cancelling it when the row is removed and when the form unmounts. `dependsOn` is the one thing a row cannot do, and its type says so rather than leaving you to find out — it returns `Unit < (Async & Scope)`, so it does not compile inside a row at all. Cross-field re-validation within a row falls back to `addRule`.

### Summaries

A form long enough to scroll pushes its first error off screen, and a rule attached to the whole scope has no field to sit under in the first place. Both want one list at the top. `Form.errorSummary(scope)` is an opinionated default: it appears only after the first submit, renders each field error as a button that jumps focus to its field, and renders form-level errors as plain rows. It is a helper over the data a form already exposes, not a method on the form, because a summary is a view and views are use-case specific.

For anything bespoke, build from `scope.errorEntries`, a `Signal[Chunk[Form.ErrorEntry]]` where each entry carries the error and the id of its field (`Absent` for a form-level error). Pair it with `scope.submitCount` for the display gate and `scope.focus(id)` for the jump.

## Putting it together

The pieces above compose into an ordinary product editor: a validated form, a picker over the category list, a modal confirmation, and a toast, all sharing state through signals you own.

```scala
import kyo.uic.form.*

val productEditor: UI < Async =
    for
        confirmDelete <- Signal.initRef(false)
        saved         <- Signal.initRef(false)
        category      <- Signal.initRef("acc")
    yield div(
        uic.Toolbar()
            .start(uic.Title().level(uic.TitleLevel.H2)("Edit product"))
            .end(uic.Button("Delete").severity(uic.Severity.Danger).icon(uic.Icons.trash).onClick(confirmDelete.set(true))),
        Form.mountedWith(ErrorTranslator.default) { scope =>
            for
                name    <- scope.field(Validator.required() and Validator.minLength(2), Activation.Blur)
                price   <- scope.numberField(0.0, Validator.min(0.0), Activation.Blur)
                inStock <- scope.field(true, Validator.all[Boolean](), Activation.Change)
                gate    <- scope.submitDisabled
                summary <- Form.errorSummary(scope)
            yield uic.Card().title("Details")(
                uic.FlexBox().vertical(true).gap(12)(
                    uic.Label("Name").forId(name.domId).required(true),
                    uic.Input().bind(name).fluid(true),
                    uic.InputNumber().bind(price).suffix(" EUR"),
                    uic.Select[Category]().options(categories)(_.label).optionKey(_.id).value(category),
                    uic.CheckBox("In stock").bind(inStock),
                    summary,
                    uic.Button("Save")
                        .icon(uic.Icons.check)
                        .ariaDisabled(gate)
                        .onClick(scope.submit(scope.markPristine.andThen(saved.set(true))))
                )
            ): UI
        },
        uic.ConfirmDialog(confirmDelete)
            .header("Delete product?")
            .message("This cannot be undone.")
            .acceptLabel("Delete")
            .acceptSeverity(uic.Severity.Danger)
            .onAccept(Console.printLine("deleted")),
        uic.Toast().open(saved).severity(uic.Severity.Success).summary("Saved").position(uic.OverlayPosition.BottomRight)
    )
```

## Server-honest rendering

The library targets kyo-ui's server-push transport as a first-class deployment, where every interaction is a round-trip. Several components therefore behave differently from their Prime originals, and the differences are design decisions rather than gaps. Knowing them up front means you can predict behaviour instead of discovering it.

**No per-interaction round-trips where a hover would cause them.** Menu submenus open on click across the whole family. The hover styling from Prime's sheet is untouched, so the family looks unchanged; only the open trigger moved.

**No JS-only affordance faked.** `Tooltip` is CSS, so it has no delays, is not interactive, and does not flip at viewport edges. `ScrollPanel` scrolls natively rather than simulating a scrollbar. `ContextMenu` anchors to the wrapped region, because kyo's `MouseEvent` carries no coordinates to open at.

**Two components carry an explicit latency cost**, and both are documented at their own sections: dragging `ColorPicker`'s colour plane is a round-trip per animation frame, and scrolling `VirtualScroller` round-trips to recompute the window.

**Render is pure.** No clock, no randomness, nothing scheduled while a component is being projected into a `UI`. `DatePicker.today(iso)` is an explicit parameter because a pure render cannot read a clock.

Fibers follow one rule from there. A component may own one spawned inside its own `UI.mounted`, because the mount's `Scope` is that component's lifetime and unmount cancels it — `Carousel`'s autoplay is the one that does. A timer that has to outlive any single render belongs to a service instead, which forks it unscoped and owns the cancellation: that is why a single `Toast` emits its `duration` as a data attribute and runs no timer of its own, while `ToastService` owns its dismissal fibers.

**The honest deferrals**, per family, are option groups in the picker family, virtual scrolling inside picker panels, typeahead in the menu family, range mode on `Slider`, advanced mode on `FileUpload`, pagination on `TreeTable`, and fullscreen on `Galleria`. Each is absent rather than half-present, so nothing silently degrades. The full record, component by component with the reason for each, is [PARITY.md](PARITY.md), which also names the one PrimeReact component this library does not have.
