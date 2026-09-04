# kyo-ui-components

kyo-ui-components is a component library for kyo-ui: 83 components in package `kyo.uic`, wearing PrimeOne's own `.p-*` markup so Prime's real stylesheets apply to them verbatim. A component is a plain immutable builder value, not a `kyo.UI` node. You construct one (`uic.Button("Save")`), chain setters that each return the same concrete type, and drop it straight into a kyo-ui container; an implicit lift renders it at placement, the same way kyo-ui lifts a bare `String` into a text node. Because the setters are typed to the component, autocomplete after `.` shows exactly the options that component has and nothing else, and options it does not have fail to compile.

Everything reactive is kyo-ui's own vocabulary, unchanged. Every slot is one setter taking `A | Signal[A]`: a constant renders once, a `Signal[A]` patches in place, and on an editable slot a `SignalRef[A]` binds two way; a panel's visibility is a `SignalRef[Boolean]` you own. Rendering stays server-honest: state lives in signals, behaviour is computed from those signals at render time, and the only client-side machinery is the contract kyo-ui already ships (focus seeding, in-place attribute and class patching, pointer and scroll reporting). Two things you supply yourself: `uic.Theme.css` on the page (nothing is injected for you), and `import scala.language.implicitConversions` for the placement lift.

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

def lookUp(text: String, spec: List[uic.SortKey], offset: Int, limit: Int): (Seq[Product], uic.Total) < Async =
    val found = catalog.filter(_.name.contains(text))
    (found.slice(offset, offset + limit), uic.Total.Known(found.size))
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

There is no new reactive primitive in this module. kyo-ui's `Signal[A]` (read-only, re-renders on change) and `SignalRef[A]` (read-write, two-way) are the whole state story.

Every slot is ONE setter taking `A | Signal[A]`, and what you pass decides what it does. A constant renders once. A `Signal[A]` patches the rendered attribute or text node in place rather than rebuilding the component. On a slot the user can edit, a `SignalRef[A]` binds two way: the component writes back into the ref as the user interacts.

```scala
val stockBadge: UI < Async =
    for inStock <- Signal.initRef(true)
    yield div(
        uic.Tag(inStock.map(b => if b then "In stock" else "Sold out"))
            .severity(inStock.map(b => if b then uic.Severity.Success else uic.Severity.Danger)),
        uic.Button("Toggle").onClick(inStock.updateAndGet(!_))
    )
```

Interactive state binds through a `SignalRef`. `value`, `checked`, `selected`, `expanded`, `page`, `active`, and `open` all accept one, and the component writes into it as the user interacts. There is no `onChange`-only path where the component holds private state you cannot read.

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

Constant, `Signal`, and `SignalRef` are three different intents behind one setter, and it is worth being deliberate about which you reach for. Pass a constant when the value is fixed at build time, which keeps the component out of any reactive boundary. Pass a `Signal` when the value is computed from state the component does not own (a `disabled` derived from a form's validity). Pass a `SignalRef` for anything the user edits, because that is the only form that gives the component write access.

The distinction is drawn on what you actually hand over, not on how it is typed at the call site. A `SignalRef` is a `Signal`, so ascribing one as `Signal[A]` still binds two way; `ref.readOnly` is how you hand a control the values of a ref while keeping the writes to yourself.

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

`Tag` labels a value, `Badge` counts it, and `Chip` stands for something the user picked and can drop again. All three take a `String | Signal[String]` for their text, so a status that changes patches in place rather than rebuilding.

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

`ProgressBar.value` takes `Int | Signal[Int]`, and the signal form is the one most work wants: progress is computed from other state and never edited, so a derived signal (`loaded.zip(total).map(pct)`) is the natural binding.

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

Nothing injects the stylesheet for you. Without `uic.Theme.css` on the page, every component renders correct `.p-*` markup and every one of them is completely unstyled. That sheet is Prime's own CSS, extracted from the MIT `@primeuix` packages at build time and frozen into generated Scala sources, and `uic.Theme.css` is the whole of it as one `String`: design tokens (`uic.Tokens`), the per-component `.p-*` rules (`uic.ComponentCss`), and a small kyo-specific remainder (`Theme.primeExtraCss`) covering the pieces Prime implements in JS or in slots.

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

### Your own preset

A shipped preset is a block of token values keyed by `data-theme`, and a fifth one is the same thing with your palette in it. `Theme.preset` emits it in that shape:

```scala
val spotify: Stylesheet = uic.Theme.preset(
    "spotify",
    Seq(
        "p-primary-400" -> "#1ed760",
        "p-primary-500" -> "#1db954",
        "p-surface-900" -> "#121212",
        "p-surface-950" -> "#000000"
    )
)

val sheet: String = uic.Theme.css + "\n" + spotify.render
```

Token names carry no leading `--`, the way `uic.Tokens` stores them and `Stylesheet.scopedVars` renders them. The result is a `Stylesheet`, so it composes with `++` and can be injected live with `UI.runStylesheet` instead of rendered into a `<style>`. Emit it **after** `Theme.css`: at equal specificity the later declaration wins.

Two mechanics decide whether your values actually land, and both are about *where*:

- **Re-pointing the ramps is enough, because everything else is a `var()` chain.** `--p-content-background` is `var(--p-surface-900)`, the primary button reads from the primary ramp, and so on down the graph — so you do not have to name the semantic tokens. But that only works because your block matches the same element the library's own blocks do, since CSS substitutes a `var()` at the element that *declares* the property. The ramps meant as a brand surface are `p-primary-*` (50…950) and `p-surface-*` (0…950).
- **The `data-theme` and `data-scheme` attributes must sit on the element your tokens are declared against.** Stamping them on a shell `div` works. Declaring your own tokens on `:root` while theming a `div` does not: the base set is re-declared on that `div`, and a `:root` declaration never reaches it.

`preset` always emits a second, paired block on `[data-theme="spotify"][data-scheme="dark"]` carrying your light tokens plus whatever you pass as `dark`. `dark` is a delta, so name only what actually differs in the dark scheme.

Two things that block buys. It is the only place a preset can hold values that differ by scheme at all. And at (0,2,0) it out-specifies the library's own `[data-scheme="dark"]` block (0,1,0), so your preset lands whatever order the sheets end up in — which matters because the library's dark set really does declare tokens a brand re-points (the whole `p-surface-*` ramp, `--p-content-background`, `--p-text-color`; the `p-primary-*` ramp it leaves alone). Emitted after `Theme.css` as above, your plain block already wins on source order at equal specificity; the paired block is what makes the order stop mattering.

To start from a shipped palette rather than from nothing, `Theme.tokens` hands you the set as it EFFECTIVELY resolves for a preset and scheme — the same blocks the sheet emits, folded in the same order:

```scala
val derived = uic.Theme.tokens(uic.Theme.Preset.Aura, uic.Theme.Scheme.Dark) ++
    Seq("p-primary-400" -> "#1ed760")
```

**That convenience has a price, and it is the one the next section is about.** `Theme.tokens` folds the same blocks `Theme.css` emits, so it reaches all four presets and pulls every token set into your bundle — about 765 KB of it. That is fine when you use `Theme.css` anyway. If you are on the slim path, start from the set you actually want instead, which costs nothing extra:

```scala
val derived = uic.Tokens.auraDark ++ Seq("p-primary-400" -> "#1ed760")
```

The difference is only that `Tokens.auraDark` is the dark OVERRIDES rather than the effective dark set; re-pointing the ramps is what a brand does, and the ramps are in `auraLight`, so in practice you pass `Tokens.auraLight` as `light` and your ramps ride on top of it.

### Shipping only the CSS you use

`Theme.css` is every preset in both schemes plus all 75 component sheets, whatever your page places. That is the right default — nothing can be missing — and it renders to about 684 KB of CSS out of roughly 1.15 MB of Scala string constants (`Tokens` 765 KB of that, `ComponentCss` 328 KB, the kyo remainder 62 KB). In a Scala.js bundle it costs more than the CSS it renders to, because the token sets live as arrays of pairs rather than as text.

`Theme.cssFor` is how you pay for less, and there are two rungs. **Take the first one; it is almost all of the win and it costs you nothing.**

**Rung one — one preset instead of four.** Name every sheet, but only the token sets you actually select with `data-theme`/`data-scheme`:

```scala
val css: String = uic.Theme.cssFor(uic.Tokens.auraLight, uic.Tokens.auraDark, uic.ComponentCss.all)
```

Nothing can go missing here: `ComponentCss.all` is still every sheet. The only thing you give up is the ability to switch to Material, Lara or Nora at runtime, which most apps never do.

**Rung two — only the sheets you place.** List them:

```scala
val css: String = uic.Theme.cssFor(
    uic.Tokens.auraLight,
    uic.Tokens.auraDark,
    uic.ComponentCss.button,
    uic.ComponentCss.datatable,
    uic.ComponentCss.slider
)
```

Either way the token sets land on the same two selectors the full sheet uses, `ComponentCss.base` and the kyo remainder are always included, and a preset built with `Theme.preset` composes on top exactly as it does with `Theme.css`.

**What each rung is worth**, measured on a real app (Scala.js dev bundle, optimizer off, bytes of the esbuild output; one library version, one line of app source different):

| sheet | bundle bytes | saved |
|---|---|---|
| `Theme.css` | 71 761 142 | — |
| rung one: one preset, all 74 sheets | 70 990 107 | 771 035 |
| rung two: one preset, the 22 sheets that app places | 70 750 513 | 1 010 629 |

**The tokens are 76 % of the saving and the sheets are 24 %.** Rung one is one line and carries no risk. Rung two buys the last quarter in exchange for a list you have to keep correct, which is the rest of this section.

**There is a floor, and it is the remainder.** `Theme.primeExtraCss` is one flat 66.5 KB document of kyo's own glue, and it is keyed on component classes: **62 of the 74 component names appear in it or in `base`**, so a page that places no table still ships the table's slot rules, and one that places no gallery still ships Galleria's. Those rules are inert without the component's own sheet — this costs bytes, not correctness — but it means the floor is component-specific rather than neutral. Two named sheets plus one preset measure about 243 KB against the full sheet's 684 KB, a factor of 2.8, with the token pairs and those 66.5 KB making up most of what is left. Splitting the remainder per component would lower the floor, and the numbers above say how much that is worth: it sits inside the 24 % that rung two buys, so it is a few per cent of the whole. That is why it has not been done.

**Why it takes sheets rather than names.** Scala.js eliminates dead code per *method*. A `cssFor(parts: Part*)` — an enum, a `Map`, a `byName` lookup — references every branch the moment it is reachable, so it would ship all 1.15 MB while looking like an optimisation. Passing the sheets themselves means the bundle keeps exactly the `Tokens` and `ComponentCss` defs your call site mentions. This is the same rule `Icons` follows for its 309 glyphs, which is why that object has no `all` either, and it is why `Tokens`' accessors are `def`s rather than `val`s: a `val` in an object is built by the static initialiser and would retain all eight token sets the moment anything touched `Tokens` at all.

**The list is yours to keep correct**, and this is the real cost of the slim path. A component whose sheet you did not name renders its `.p-*` markup completely unstyled, and nothing warns you — not the compiler, not the renderer, not a console message. It looks like a layout bug. Adding a `uic.Chip` to a page means adding `uic.ComponentCss.chip` to this list in the same change. If you cannot hold that discipline, use `Theme.css`; it is correct by construction and the whole point of it being the default.

A practical way to derive the list rather than guess it: render the app, collect every `class` attribute in the result, and map the `p-<name>` prefixes onto sheet names. That also catches the components your components place internally — a `Select` brings an overlay, a `DataTable` brings a paginator — which reading your own source will not tell you.

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

Every form control in the library follows one shape: a `value` (or `checked`) binding that accepts a ref for two-way editing, plus `invalid` and `invalidMessage` for validity. That set is not a convention to remember but the `FormControl` trait, which now DEFINES those setters rather than declaring them, so a control that holds a user-supplied value carries all of it, spelled the same way, or is not one. `size` and `variant` ride along wherever the design system defines them, which is the field-shaped controls. This section is the controls alone; the separate validation layer that computes `invalid` for you is [Validated forms](#validated-forms) below.

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

Every control carries `invalid` and `invalidMessage`. `invalid` takes `Boolean | Signal[Boolean]`; `invalidMessage` keeps two setters, because a constant message is always shown while the reactive one is a `Signal[Maybe[String]]` that can also clear the row. That pair is Prime's `.p-invalid` model: `invalid` stamps the red state and `invalidMessage` renders the message row beneath the field. You can drive them by hand from any signal you already have, which is the whole story for a form too small to want a validation layer.

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
        .emptyContent("No matches"): UI
```

Its bound selection is spelled `value`, like every other picker's, because a Listbox is the always-visible member of the family rather than a different concept — and for the same reason it is a form control, so `uic.Listbox().bind(field)` works on a `Set[String]` field exactly as `MultiSelect` does.

Every component that can run out of rows takes the same `emptyContent`, in three forms: a `String`, a `Signal[String]` for a locale-driven line, or arbitrary `UI`. The UI form matters more than it sounds. An empty state is rarely a sentence; it is an icon over a line of explanation and a button that creates the first record, and a component that only accepted a `String` would push that layout outside itself, where it no longer sits in the list, table body or panel the emptiness belongs to.

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

> **Note:** the context menu opens at the pointer, reading the click position off `UI.MouseEvent.position` and placing the panel there through `Overlay.pointerAnchor`. The panel portals to the body, so a scroll container around the region cannot clip it, and it turns back over the pointer near a viewport edge. A right-click while the menu is open closes it rather than moving it: the outside-click backdrop covers the viewport, so that click never reaches the row under it, and a moved panel would leave `DataTable.contextMenuRow` pointing at the row of the first click.

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

`SpeedDial` is the same items rendered as a fan of rounded icon buttons behind one toggle, which is the floating-action shape. `direction` is the setting that matters: `Up` is the default, and `Down`, `Left`, and `Right` pick which way the fan travels, so the toggle can sit in any corner and still open into the page. It also picks the fan's keyboard axis: the arrow pointing into the fan opens it from the toggle, the same pair of arrows then walks the actions, and Escape closes the fan and hands focus back to the toggle.

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

`Paginator.template(...)` says which of its elements render and in which order. Prime names the same set in a space-separated string, where a misspelt name renders nothing and reports nothing; here they are `PaginatorElement` values, so the compiler catches a name that is not one of them and the order is the order of the list. The list REPLACES the layout rather than adding to it: an element it does not name does not render, `jumpToPageInput` included, and one it names twice renders twice. What an element needs in order to have something to show stays where it was, so `CurrentPageReport` still reads `currentPageReport` and the two dropdowns still read `rowsPerPageOptions`; an element named with nothing behind it renders a card.

```scala
val compact: UI < Async =
    for page <- Signal.initRef(0)
    yield uic.Paginator()
        .totalRecords(120)
        .rows(10)
        .page(page)
        .currentPageReport("{first} to {last} of {totalRecords}")
        .template(
            uic.PaginatorElement.CurrentPageReport,
            uic.PaginatorElement.PrevPageLink,
            uic.PaginatorElement.JumpToPageDropdown,
            uic.PaginatorElement.NextPageLink
        ): UI
```

A `DataTable` reaches the same setters through `paginator(f)`, a function over the paginator it renders below its rows. Prime mirrors a handful of the paginator's props onto the table and a caller reaching for one it did not mirror has nowhere to go; here the paginator is a value, so one name covers all of them. The four the table owns are applied after `f` and cannot be overridden from there: the record count, the page size, the current page and the ref it writes to, since a paginator disagreeing with the rows above it would page a list nobody is looking at.

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

A column is built one of two ways, and the difference matters. `column("Name")(_.name)` carries a text projection: it renders that text and the global filter can match against it. `column("Price").body(...)` carries a template instead: it renders whatever UI you return.

`column` carries no type argument because it reads the row type from the table it is passed to. That works only inside a `columns(...)` call; a column list built somewhere else names its row type once, `Column[Product]("Name")(_.name)`, and splats in as `columns(sharedCols*)`.

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
                uic.column("Name")(_.name).sortBy(_.name),
                uic.column("Category")(_.category).sortBy(_.category),
                uic.column("Price")(p => f"${p.price}%.2f").sortBy(_.price).align(uic.ColumnAlign.End),
                uic.column("Stock").body(p =>
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

Sorting is opt-in per column through `sortBy`, and needs a `sort` ref on the table to persist. The ref holds an ordered `List[SortKey]`, each entry naming its column by path (its header, preceded by the labels of any `headerGroup`s around it), and the first entry that is actually sorting is the primary key. The plain click and the modifier click do different jobs, and the plain one changes with the size of the spec. While a single column sorts it owns the whole cycle, ascending, descending, off, because there is no priority order to damage and no reason to make clearing one sort reach for a modifier. Once several columns sort it only reverses the clicked one, in place: a spec built up over several clicks must not lose a key because a header was clicked once too often, so switching a column off moves to Ctrl or Cmd. A plain click on a column that is not sorting makes it the single key the reader controls either way. Ctrl or Cmd click is the multi-key control: it adds a column, or advances one already in the spec.

`sortBy` says HOW a column sorts; `Column.sortable` says whether the reader may change it. They are separate questions, and unset the second follows the first, so a column with an ordering is interactive exactly as it always was. `sortable(false)` takes the affordance away and leaves the state: no click, no tab stop, no sort affordance, but the sorted class, `aria-sort`, the direction icon and the rank badge all stay, because a spec that names the column still sorts the rows and hiding that would misreport what the reader is looking at. That pair is what the flag is for: a table sorted by a column the reader may not re-sort.

```scala
val lockedSort: UI < Async =
    for sort <- Signal.initRef(List(uic.SortKey.ascending("Category")))
    yield uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(
            uic.column("Category")(_.category).sortBy(_.category).sortable(false),
            uic.column("Name")(_.name).sortBy(_.name)
        )
        .sort(sort): UI
```

One rule ties the flag to the click transitions: a click clears only the entries the reader could have cleared themselves. A plain click on a free column would otherwise collapse the whole spec and take a locked entry with it, which would hand the reader the reset the flag exists to deny them. So locked entries keep their slots, the trailing-`Unsorted` pruning stops at one, and "several columns sort" counts the entries the reader can act on, since a locked column beside their single key would otherwise take the plain click's third state away for a reason nothing on screen explains. The same protection covers a spec entry that names no column of the table: no header, no click, not the click's to remove.

It also takes a `Signal[Boolean]`, which is what suspends re-sorting while a mutation is in flight. A reactive flag is resolved before the table builds rather than patched into the header cell, since the flag reaches the cell's class, tab stop, icon and click handler at once and the table already re-renders whenever a bound signal moves. `sortable(true)` on a column with no ordering has nothing to sort by and says so in a card.

The advance happens *in place*, which is the part that matters. `SortDirection` has three cases, not two: ascending, descending, and unsorted, and only the modifier click can reach the third. A column switched off keeps its slot in the priority order rather than leaving the list, so the next Ctrl click on that same header brings it back at the rank it had. Without the third case, switching off the second of three keys promotes the third, and putting things back means clearing the third, re-adding the second, and re-adding the third. `removableSort(false)` drops the third case and leaves the two-state cycle, which is Prime's default. Once two or more columns sort, each sorted header shows its rank, because the direction icons alone cannot say which key wins. Row expansion pairs `expanded(ref)` with `rowExpansionTemplate`.

In `SelectionMode.Checkbox` the checkbox column's header is the select-all. It is binary, matching Prime: a partial selection reads unchecked. It covers every row that survived the global filter rather than the page in view, and it adds or removes those keys instead of replacing the selection, so narrowing the filter, selecting all, and widening it again does not quietly drop what was selected before.

`selectedCells(ref)` picks CELLS instead of rows, Prime's `cellSelection`, bound as a set of `CellPath`, which is a row key paired with a column path. Binding it is the switch, and `selectionMode` says what a click means: `Single` replaces the set, `Multiple` toggles the cell in it. A cell has no identity until a row key and a column path are put together, which is why this is a second binding rather than a mode over the row one. One click can mean one thing, so binding more than one of cell editing, row selection and cell selection is a card: editing keeps the click, then row selection, and cell selection is off while either is bound. `selectableWhen` covers cells too, which is what Prime's Cell Selection disabled section does with `isDataSelectable`.

```scala
val picking: UI < Async =
    for cells <- Signal.initRef(Set.empty[uic.CellPath])
    yield uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(uic.column("Name")(_.name), uic.column("Category")(_.category))
        .selectionMode(uic.SelectionMode.Multiple)
        .selectedCells(cells): UI
```

`selectableWhen(p)` keeps rows out of the selection without keeping them out of the table. A rejected row renders as it always did and still takes an `onRowClick`, but every path into the selection set is closed to it: the click writes nothing, the row loses the pointer cursor that says it can be picked, its checkbox carries Prime's disabled class and no handler at all, and the select-all steps over it. The predicate is consulted where the selection is written rather than once at render, so a row the caller starts rejecting cannot stay selected on the strength of having been selectable a moment ago.

```scala
val onlyInStock: UI < Async =
    for picked <- Signal.initRef(Set.empty[String])
    yield uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(
            uic.column("Name")(_.name),
            uic.column("Stock")(p => if p.inStock then "yes" else "no")
        )
        .selectionMode(uic.SelectionMode.Checkbox)
        .selected(picked)
        .selectableWhen(_.inStock): UI
```

`rowClasses(f)` is the row half of conditional style, Prime's `rowClassName`. Prime takes a string or an object of class to condition; here it is the list a class attribute already is, so a condition is the caller's own `if` and an empty list is no class. They append after the table's own, which leaves striping, selection and the editing state saying what they say. A cell is styled through `Column.body`, which is any UI, so the two halves need no shared vocabulary. Frozen rows are data rows and carry them too.

```scala
val flagged: UI =
    uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(uic.column("Name")(_.name), uic.column("Price")(p => f"${p.price}%.2f"))
        .rowClasses(p => if p.price > 100 then Seq("expensive") else Nil)
```

Filtering has a second form beside the global query, and it is per column. `Column.filterBy` gives a column its own filter over the value it names, and the `CellType` in scope decides how that filter reads: a type that compares (every provided number, anything through `CellType.ordered`) gets `=`, `<`, `>` and their negations and reads the query as a VALUE, everything else is matched as text, with contains, starts-with and the rest over what the type formats the value as. `filterBy` on its own filters by the column's text projection, which is the projection nine times in ten. `columnFilters(ref)` binds the filters, keyed by the same path the sort spec names a column by, and gives the table Prime's filter row: one input per filterable column, with the mode menu behind the funnel beside it.

```scala
val perColumn: UI < Async =
    for filters <- Signal.initRef(Map.empty[List[String], uic.ColumnFilter])
    yield uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(
            uic.column("Name")(_.name).filterBy,
            uic.column("Category")(_.category).filterBy,
            uic.column("Price")(p => f"${p.price}%.2f").filterBy(_.price).align(uic.ColumnAlign.End)
        )
        .columnFilters(filters): UI
```

That is what the type buys: `filterBy(_.price)` answers "less than 50" over the number, where a text match would put 100 before 50 and match 5 against 15. Every bound filter has to pass, and the global query with them. A query the column cannot read as a value of its type filters nothing and marks its own input instead of emptying the table behind a typo, and a mode a column never offered is refused the same way, which only a hand-seeded map can reach. A hidden column does not filter, for the same reason the global query does not search it: an input the reader cannot see is one they cannot clear. That is the opposite of the sort spec, which keeps sorting by a hidden column, and for the reason that tells them apart, a filter takes rows away and a sort only moves them.

`filterDisplay(FilterDisplay.Menu)` moves that editing off the row and into Prime's filter popover: a funnel in each filterable header cell, and behind it several conditions on that one column, joined by Match All or Match Any, with the buttons to add one, remove one, clear them and apply them. A `ColumnFilter` is a list of `FilterRule` and an operator for that reason; `ColumnFilter(query, mode)` builds the single rule a row display holds, and a seeded filter carrying more under a row display shows the first and says so in a card.

The menu edits a DRAFT and applies it on the button. That is the one behavioural difference between the two displays, and it is the reason Prime has the button at all: a table is not re-filtered on the way to the second condition. Opening the funnel seeds the draft from what is applied, so the panel shows the filter the table is running rather than whatever was last abandoned in it; closing without applying leaves the table as it was; Clear takes the column out of the map and empties the draft, since a Clear that left the conditions standing would be one Apply away from coming back. One condition this column cannot read makes the whole filter unusable rather than being dropped from the join: a condition silently ignored would widen the result under Match Any and narrow it under Match All, and either way the table would be answering a question nobody asked.

```scala
val menuFiltered: UI < Async =
    for filters <- Signal.initRef(Map(
            List("Price") -> uic.ColumnFilter(
                List(
                    uic.FilterRule("10", uic.MatchMode.Greater),
                    uic.FilterRule("100", uic.MatchMode.Less)
                ),
                uic.FilterOperator.And
            )
        ))
    yield uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(
            uic.column("Name")(_.name).filterBy,
            uic.column("Price")(p => f"${p.price}%.2f").filterBy(_.price)
        )
        .columnFilters(filters)
        .filterDisplay(uic.FilterDisplay.Menu): UI
```

Which columns a table has is its shape; which of them the reader sees is each column's own business, so `Column.visible` carries it, as a constant or as a signal a toggle writes. A hidden column contributes no header cell, no body cells and no footer cell, so the table is exactly as wide as what is on the screen, every colspan follows, and a `headerGroup` whose columns are all hidden goes with them.

```scala
val hideable: UI < Async =
    for shown <- Signal.initRef(Set("Category"))
    yield uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(
            uic.column("Name")(_.name),
            uic.column("Category")(_.category).visible(shown.map(_.contains("Category"))),
            uic.column("Price")(p => f"${p.price}%.2f").visible(shown.map(_.contains("Price")))
        ): UI
```

What hiding does not do is take the column out of the table. The sort spec keeps sorting by it, so hiding a column never reshuffles the rows under the reader and the card that names an unsortable spec entry does not start firing at one. The global filter is the other way round, and deliberately: a query matches what is on the screen, so a hidden column's text is not searched. Where the keyboard grid is on, it renumbers with the header, since the cursor addresses cells by position and a position has to mean what the reader sees.

How wide a column is, is the column's own: `Column.width(px)` says it. The width reaches the column through a `col` element rather than through its cells, which is the only place that can carry one, since a cell sizes the row it is in and a grouped header's cell spans several columns at once. It is authoritative rather than a suggestion: a value too long for its column is clipped instead of widening it, and the columns with no width share what is left.

`columnWidths(ref)` hands the widths to the reader, keyed by the same path the sort spec and the column filters use. Every boundary between two resizable columns grows a drag handle, and what the reader drags is written straight back into the map, so the widths outlive the table that rendered them.

```scala
val resizable: UI < Async =
    for widths <- Signal.initRef(Map(List("Name") -> 260.0))
    yield uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(
            uic.column("Name")(_.name),
            uic.column("Category")(_.category),
            uic.column("Price")(p => f"${p.price}%.2f").resizable(false)
        )
        .columnWidths(widths): UI
```

A drag moves one BOUNDARY rather than sizing one column: the two columns it sits between trade width and their total stays where it was, so the table never grows past the space it was given and no column the reader is not touching moves. That is also why a handle needs a resizable column on both sides of it, and why the last column never carries one: what is to its right is the edge of the table, which has no width to trade. The widths the reader is given to drag are MEASURED when they grab, not read out of the map, so a column the caller never sized is as draggable as one that was.

`columnResizeMode(ColumnResizeMode.Expand)` changes what a drag moves: one COLUMN rather than the boundary beside it. Only the grabbed column changes, the table grows or shrinks by the same amount and scrolls sideways in its container, and the last column becomes draggable too, since it no longer needs a neighbour to trade with. It costs one thing the fit mode does not: an expanding table has to state its own width, or the browser hands what the columns leave over back to them and the drag lands where it started. That width is the sum of the columns, so every visible column needs one, from `Column.width` or from the bound map; a column without one is named in a card and the table resizes to fit instead.

```scala
val expanding: UI < Async =
    for widths <- Signal.initRef(Map.empty[List[String], Double])
    yield uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(
            uic.column("Name")(_.name).width(260),
            uic.column("Category")(_.category).width(160),
            uic.column("Price")(p => f"${p.price}%.2f").width(120)
        )
        .columnWidths(widths)
        .columnResizeMode(uic.ColumnResizeMode.Expand): UI
```

`state` and `restore` bring a reader back where they were. Prime persists the same set itself, under a `stateKey` in local or session storage, because in Prime that state lives inside the component and a caller has no other way to reach it. Here every field is already a ref the caller bound, so `state` bundles them and `restore` writes them back, and where the value is kept between visits stays the application's: a cookie, a URL, a profile row, or nowhere. A field whose ref is not bound reads as its default and is not restored, since there is nowhere for it to go, and nothing is validated on the way in because a restored spec naming a column the table no longer has, a width for one it never had, or a page past the end are each already something the table reports or clamps at render.

```scala
val resumable: UI < Async =
    for
        sort <- Signal.initRef(List.empty[uic.SortKey])
        page <- Signal.initRef(0)
        table = uic.DataTable[Product]()
            .rows(catalog)
            .rowKey(_.id)
            .columns(uic.column("Name")(_.name).sortBy(_.name))
            .sort(sort)
            .paginate(10)(page)
        _ <- table.restore(uic.TableState(sort = List(uic.SortKey.ascending("Name")), page = 2))
        _ <- table.state
    yield table: UI
```

`csv` is the table as data. `csv(rows)` is pure and takes the rows to write, which is what makes an export of the selection one line; `csv` on its own reads the bound sort and filters and gives back the rows the reader is looking at, in the order they are looking at them, across every page rather than the one on the screen, which is what Prime's `exportCSV()` exports too. The heading of a column is `Column.exportHeader` or its header, the field is `Column.exportAs` or its text projection, and `Column.exportable(false)` leaves it out entirely. `exportAs` is Prime's table-wide `exportFunction` put on the column the way the rest of this class is, and it is what a column rendering only a `body` needs: what a cell shows is a `UI`, and there is no honest way to read a string out of one. Fields are quoted the way RFC 4180 asks, and `csvSeparator` changes what counts. The columns come out in the order the reader put them in, since that is as much a part of what they are looking at as the sort is; a column `Column.visible` is hiding still exports, because visibility is about the screen and `exportable` is about the export.

```scala
val exportable: UI < Async =
    for
        query <- Signal.initRef("")
        table = uic.DataTable[Product]()
            .rows(catalog)
            .rowKey(_.id)
            .columns(
                uic.column("Name")(_.name),
                uic.column("Price")(p => f"${p.price}%.2f").exportAs(_.price.toString),
                uic.column("Stock").body(p => uic.Tag(if p.inStock then "yes" else "no")).exportable(false)
            )
            .globalFilter(query)
        text <- table.csv
    yield div(
        a.href(UI.Href.External(
            "data",
            s"text/csv;charset=utf-8;base64,${Base64.encode(Span.from(text.getBytes("UTF-8")))}"
        )).download("catalog.csv")("Download CSV"),
        table
    )
```

Getting the string to the reader is the app's, not the table's. `a.download(name)` over a `data:` URL is the shortest way and is what the example does, base64 rather than percent-encoded so nothing in the data has to be escaped; an endpoint that serves the bytes is the better one once the table is large, since a `data:` URL carries the whole file in the markup and is rebuilt on every render that changes it.

`contextMenuRow(ref)` says which row a right-click landed on, Prime's `contextMenuSelection`. The row is marked with Prime's own `.p-datatable-contextmenu-row-selected`, which is a second and separate mark from the selection: the reader is acting ON one row without changing what is selected. Declaring it also suppresses the browser's menu over the rows, which is what a context menu of one's own has to do. The menu itself is `ContextMenu`, which wraps the table, and the ref is the caller's to clear when it closes, since the table cannot see a panel it does not render. `onRowContextMenu` is the notification beside it, and it carries what the click was aimed at: a `RowContext` of the row under the pointer and the rows selected at that moment. Both halves, because a menu over a table asks one question with two answers behind it. A reader who right-clicks inside their selection means the selection and one who right-clicks outside it means that row, which is `selected.contains(row)` and nothing more. The table takes no position on the rule and changes nothing about the selection.

```scala
val withMenu: UI < Async =
    for
        onRow <- Signal.initRef(Absent: Maybe[String])
        table = uic.DataTable[Product]()
            .rows(catalog)
            .rowKey(_.id)
            .columns(uic.column("Name")(_.name))
            .contextMenuRow(onRow)
    yield uic.ContextMenu()
        .items(uic.MenuItem("Copy"), uic.MenuItem("Delete"))(table): UI
```

`reorderableRows(true)` lets the reader drag a row somewhere else. It adds Prime's grip column at the leading edge, where Prime has the caller place a `rowReorder` column of their own, and shows where a drop would land as a line on the row it would land beside, where Prime positions two floating arrows measured in JavaScript on every move. What a drop rewrites is the row LIST, so it needs somewhere to write: a bound `rows(ref)` the table stores the new list into, or `onRowReorder`, which hands it over with the indices counted in the list rather than in the page.

It also needs the rows on the screen to be the rows in the list, in that order. While a sort spec, a global filter, a column filter or a row grouping is deciding the order, over a lazily loaded window where the order is the query's, or over a windowed body, the grips render but carry no drag, and a card names which of them it is. Paging is fine: a page is a contiguous slice, so the row on screen and the row in the list are the same row at a known offset.

```scala
val ordering: UI < Async =
    for shelf <- Signal.initRef[Seq[Product]](catalog)
    yield uic.DataTable[Product]()
        .rows(shelf)
        .rowKey(_.id)
        .columns(uic.column("Name")(_.name), uic.column("Category")(_.category))
        .reorderableRows(true): UI
```

The geometry a drag needs is measured once, on the grab, in a single round trip for the whole page: moving a row changes no height, so the rows the reader picked up from are the rows they let go over.

Rows the reader has to keep in sight are `frozenRows(rs)`, Prime's `frozenValue`: a running total, the record being compared against, the one they pinned. They render in a row group of their own above the scrolling one and hold under the header. They are a list of their OWN and not a subset of the body's, which is what lets them be a summary rather than a duplicate; a row that is in both is a card, since two rows with one key are two rows the table cannot tell apart.

Where they hold is the height of the header, which is the one number here that nothing can be told and nothing can compute: it is whatever the header cells came out as. The table observes it and writes the offset, so a header that rewraps on a resize moves the frozen rows with it, where PrimeVue measures once in a lifecycle hook. Until the first measurement lands they sit at the top of the body in flow, which is where they belong at rest, so there is nothing to flash.

A column the reader has to keep in sight while the rest scrolls past is `Column.frozen(true)`, or `frozen(FrozenEdge.End)` against the trailing edge. Declaring one puts the table in a scroll container, since a column can only be frozen against something that moves; `scrollHeight` adds a cap on the height, and freezing on its own scrolls sideways, which is what a table wider than its page needs.

```scala
val frozenColumns: UI < Async =
    for widths <- Signal.initRef(Map.empty[List[String], Double])
    yield uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(
            uic.column("Name")(_.name).width(260).frozen(true),
            uic.column("Category")(_.category).width(220),
            uic.column("Price")(p => f"${p.price}%.2f").width(160).align(uic.ColumnAlign.End)
        )
        .columnWidths(widths): UI
```

A frozen column has to be somewhere, so it holds at the distance the columns between it and the edge take up, and the table adds that up from their widths. Two rules follow. A frozen column needs a `width`, and the frozen ones have to REACH the edge they hold against: a free column left between them and it would carry the frozen one away as it scrolled. A table that breaks either freezes nothing at all and says which column it was, because an offset computed from a width that is not there is a column parked over the middle of the table, which is worse than one that scrolls. The checkbox, expander and row-editor columns are between a frozen column and the edge too, so they hold with it and their width counts toward the offset; that width is a CSS variable (`--p-uic-dt-select-width` and its two neighbours) rather than a number, so what the `col` is given and what the offset counts are one quantity a theme can move. Prime computes the same two properties in JavaScript, measuring the previous cell on every render; here the widths are already known, so the offsets are written once and a resize drag moves them with the column that changed.

Where the columns are is `columnOrder(ref)`, a list of column paths from left to right, and with it bound the reader may drag a header cell somewhere else. `Column.reorderable(false)` pins one column where it was authored, the same per-column axis `sortable` and `resizable` already are, where Prime has one `reorderableColumns` flag for the whole table.

```scala
val reorderable: UI < Async =
    for order <- Signal.initRef(List(List("Price")))
    yield uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(
            uic.column("Name")(_.name),
            uic.column("Category")(_.category),
            uic.column("Price")(p => f"${p.price}%.2f").reorderable(false)
        )
        .columnOrder(order): UI
```

A column the order does not name keeps its authored place behind the ones it does, so the seeded `List(List("Price"))` above means "Price first" rather than "the rest is undefined". What a drag writes is the whole list, hidden columns included, so a column `Column.visible` is hiding keeps its place while the reader moves the others and comes back where it was. Where a cell may be dropped follows from the table rather than from a rule of its own: it moves among its own siblings, since the header is a tree and one cell cannot sit in two groups at once, and a drop that would leave a frozen column adrift or carry a pinned one along is not offered at all. A header group drags as one, which Prime does not do: with a ColumnGroup its reordering is off entirely. The line showing where the column would land is a border on the cell beside it, where Prime positions two floating arrows in JavaScript, and the state is written only when the answer changes, so a drag across a wide table re-renders once per boundary crossed rather than once per frame.

Rows the table did not filter, sort or page are `lazyRows(total)`. Binding the total is what says so, since locally the total IS the row count: a table handed one has been handed something it could not have worked out, so it renders what it was given verbatim and paginates over the total rather than over what it holds.

Nothing else changes. The header still sorts, the filter row still takes queries, the paginator still steps, and every one of them still writes into the ref bound to it. That is why there is no load event to bind, and none was added: the refs already are one.

The total is a `Total`, not a number, because plenty of sources cannot answer with one. A `count(*)` beside the page gives `Total.Known(n)` and the paginator counts the pages out; a cursor API or a search index gives `Total.Unknown(hasMore)` and the paginator grows one page at a time. A source fills in `Unknown`'s second field, the floor under the length, from how far it has served, and never lets it fall: without that, paging back would shrink the page list, and a scrollbar sized off it would jump. Modelling the second case is what keeps a caller from inventing a number, and a fabricated total is not the smaller wrong: it puts pages in the paginator that no query will ever fill.

`RowSource` is the other half, and it is what a table binds in practice. It owns the fetching: a block cache keyed by the query, a prefetch of the neighbouring blocks, and the busy flag that stays down when a range was already there. `source(...)` binds its rows, its total, its busy flag and its paginator in one call.

```scala
val lazyTable: UI =
    UI.mounted {
        for
            text <- Signal.initRef("")
            sort <- Signal.initRef(List.empty[uic.SortKey])
            source <- uic.RowSource.init(text.combineLatest(sort), pageSize = 25) {
                (key, offset, limit) => lookUp(key._1, key._2, offset, limit)
            }
        yield uic.DataTable[Product]()
            .rowKey(_.id)
            .columns(
                uic.column("Name")(_.name).sortable(true),
                uic.column("Category")(_.category).sortable(true),
                uic.column("Price")(p => f"${p.price}%.2f").align(uic.ColumnAlign.End)
            )
            .sort(sort)
            .source(source): UI
    }
```

The first argument is everything besides the range that decides which rows match: the search text, the sort spec, the filters. It is part of the cache key, so a sort click retires the old query's blocks without invalidating anything, and it is why the source needs no notion of what a sort even is.

Rows are fetched in fixed-size BLOCKS rather than in pages, because a viewport asks for rows 37 to 52 and that lies across any page boundary; a page is then a range that happens to start on a block boundary. `Config(blockSize, buffer, maxBlocks, expireAfterWrite)` is the whole policy: the block size defaults to the page size, so one page is one request, and setting it larger serves several pages from one.

One rule inside is worth knowing, because it is what makes the buffer safe rather than merely fast: a fetch is never interrupted, only the waiting for it. `Signal.observe` closes its per-value scope the moment the value changes, so a fetch forked into that scope would be killed exactly when the reader arrives on the page it was fetching. Every fetch is therefore forked unscoped and only awaited from inside that scope, where an interruption costs nothing.

A source lives on the scope of the `UI.mounted` that made it, so its feed stops when the node does, and a fetch that fails surfaces on the node rather than leaving stale rows standing. For anything the source does not fit, `rows(ref)`, `lazyRows(signal)` and `loading(signal)` are still there to bind by hand.

A table can also scroll instead of paginating. `scrollRows(itemSize)`, beside the `scrollHeight` that gives it a viewport, renders only the rows in view plus a few on either side; over a source it is the scroll that writes the demand, so nothing is held and the list costs the blocks the reader actually looked at.

```scala
val infiniteTable: UI =
    UI.mounted {
        for
            text <- Signal.initRef("")
            source <- uic.RowSource.init(text, pageSize = 40) {
                (t, offset, limit) => lookUp(t, Nil, offset, limit)
            }
        yield uic.DataTable[Product]()
            .rowKey(_.id)
            .columns(
                uic.column("Name")(_.name),
                uic.column("Price")(p => f"${p.price}%.2f").align(uic.ColumnAlign.End)
            )
            .source(source)
            .scrollHeight("400px")
            .scrollRows(46): UI
    }
```

`itemSize` reaches the row as a height, which the browser reads as a floor: pick it at least as tall as the row renders on its own and every row is exactly it. How far it reaches comes from the `Total`: a known count spans the whole list, and an unknown one spans what has loaded plus one screen while anything follows, which is infinite scrolling without a mode for it. A row of the window that has not arrived yet is a row of `Skeleton` cells the same height, so the geometry never moves under the reader and the waiting is shown per row rather than behind a mask over the whole table.

Two things follow from placing rows by counting them. The rows really have to be `itemSize` tall, and three features render rows that are not: `groupBy` and a level's summary row, `rowExpansionTemplate`, and `Column.rowSpan`. Each is named in a card and leaves the table rendering every row, which is slower and right. And the keyboard cursor is off while the rows are windowed, since it addresses cells by their position among the rendered ones.

A column of such a table sorts once it SAYS it does, with `sortable(true)` or with a `sortBy` whose ordering then goes unread. The default cannot be yes, or every column would offer a sort nobody asked for, the Price column above among them. What the table holds is also all it can name, so a select-all covers the page it was given, a `Column.footer` aggregate sums that page, and a `groupBy` run stops at the page's edges. Two mistakes it can still see it reports: more rows than one page holds, and more rows than the total says exist. Neither of those shows in the table itself, which is why each is a card.

Around the rows sit four pieces of chrome. `header(ui)` and `footer(ui)` are free slots, above the table and below the paginator, which is where a filter box or a record count goes. `Column.footer` is a different thing: it renders a real `tfoot` row aligned to the column grid, and its computed form `footer(rows => ...)` receives the rows that survived the global filter, across every page rather than the visible one. That distinction is load-bearing, because the table owns filtering: a column total computed by the caller from its own list would disagree with what the reader is looking at. `loading(flag)` covers the table with a spinner mask, and `scrollHeight("240px")` caps the container and pins the header row group to its top edge while the body scrolls under it. Adding `scrollRows` moves that height onto Prime's own scroller inside the container, which is then the one element that scrolls.

`flexScroll(true)` is the other way to size that viewport: it takes the height from the PARENT instead of stating one, which is what a table filling a panel or a split pane needs. Prime says the same thing with a magic `scrollHeight="flex"`; here it is its own switch, because the two are different questions and only one of them has an answer the table can read before layout. That is also what it costs: `scrollRows` cannot window against a height nobody has computed yet, and says so in a card. `frozenRows` is fine with it, since a row holds against a scroll container and not against a number. Given both a length and the flex flag, the length is what the table scrolls and the flag is reported.

A header of more than one row is a `headerGroup`: a label written *around* the columns it spans, nested as deep as it needs to be. Prime writes the header cells beside the column list and has the caller put `colSpan` and `rowSpan` on each of them; here the leaves ARE the columns, so a group is as wide as what it holds, a column beside a group reaches down to the bottom of the header, and there is no second list to fall out of step with the first. Everything else stays on the leaves: a group carries a label and nothing more, so sorting, footers, filtering and merging keep working exactly as they do without one.

```scala
val groupedHeader: UI < Async =
    for sort <- Signal.initRef(List(uic.SortKey.ascending("Stock", "Retail", "Price")))
    yield uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(
            uic.column("Name")(_.name).sortBy(_.name),
            uic.headerGroup("Stock")(
                uic.headerGroup("Retail")(
                    uic.column("Price")(p => f"${p.price}%.2f").sortBy(_.price).align(uic.ColumnAlign.End),
                    uic.column("Category")(_.category)
                ),
                uic.column("Available")(_.inStock.toString)
            )
        )
        .sort(sort): UI
```

A grouped column is addressed by its **path**: the labels of the groups around it, outermost first, then its own header. That is why `SortKey` carries a list of parts and not one joined string, so no separator ever becomes part of the API:

```scala
uic.SortKey.ascending("Category")                 // a column in no group
uic.SortKey.ascending("Stock", "Retail", "Price") // one inside two groups
```

A column in no group has a path of one part, so every spec written before groups existed means exactly what it meant. It also means a `Q1` under two year groups is two different columns to the spec, which is what lets the group carry the disambiguation instead of the cell label.

Two things stay reported at render time in a `.p-uic-key-error` card, because neither is a type error: a group holding no column occupies nothing and would otherwise vanish without a word, and a spec entry that matches no sortable column does nothing at all, which covers a mistyped part, a column that never got a `sortBy`, and a path missing the group labels above it. That last one is the cost of the path: wrap an existing column in a group and a stored spec stops matching it, where the bare header used to survive. It stops matching loudly.

Editing puts the whole round trip on the column. `editable(read)(write)` says where a cell's value comes from and where an edited one goes, and the `CellType` in scope supplies the rest: the editor, the formatter that fills it, and the parser that reads it back. Those three travel together on purpose, since an editor over labels a parser has never heard of would compile and then refuse every commit.

That is the difference from a table that owns only the editing STATE. Nothing downstream has to ask which column an edit belongs to, or dispatch on a path to find the field it maps to, or keep a draft ref per column: the table seeds the draft from `read`, the editor writes into it, and the commit goes back through `write`. Where a value comes from and where it goes is written once, and checked against the row type.

`editingCell(ref)` binds one `CellPath`, a row crossed with a column path (the same path the sort spec names a column by), and clicking a cell of an editable column moves the editor there. `editingRows(ref)` binds a `Set` of `rowKey` ids and adds Prime's editor-button column at the trailing edge, opening every editable cell of a row at once. Enter commits and Escape discards in both.

Where a commit LANDS is the second binding. `rows(ref)` over a `SignalRef[Seq[A]]` lets the table store the new row itself; with the plain `rows(Seq)` it computes the row and hands it to `onCellValueChanged` instead, which is the mode for rows the table cannot reach. A column that is editable while the table can do neither is reported in a card, because the commit would be computed and dropped.

```scala
val editableTable: UI < Async =
    for
        rows <- Signal.initRef(catalog)
        cell <- Signal.initRef(Absent: Maybe[uic.CellPath])
    yield uic.DataTable[Product]()
        .rows(rows)
        .rowKey(_.id)
        .columns(
            uic.column("Name")(_.name).editable(_.name)((p, v) => p.copy(name = v)),
            uic.column("Price")(_.price.toString)
                .editableAs(uic.CellType.double.validate(Validator.min(1.0)))(_.price)((p, v) => p.copy(price = v)),
            uic.column("In stock")(_.inStock.toString).editable(_.inStock)((p, v) => p.copy(inStock = v))
        )
        .editingCell(cell): UI
```

`editableAs` is the same thing with the cell type given explicitly: a domain type through `CellType.of(values)(label)`, which derives both halves from one label function, or a type carrying its own rules. Rules are `kyo.uic.form.Validator`, the ones a form field already uses, and they run over the PARSED value. Text that will not parse and a value a rule refuses are the same outcome: the cell stays open on what the reader typed, marked invalid, with the message under the editor. That message is the one thing a `valueSetter` returning a boolean cannot carry.

`editableWhen(p)` gates a column per row, and `Column.onValueChanged` is the column-scoped twin of `onCellValueChanged`. Editing is keyed by `rowKey`, so it joins selection and expansion in requiring one, and binding both modes at once is a card, since a cell inside an edited row would be open for two reasons with two ways out.

A table that binds an editing state owns state no caller supplies (one draft per editable column, and the error a refused commit left standing), so it renders through a mount: the static projection is the same table with its editors closed and its affordances inert, and the live one arrives when the transport attaches. A table that does not edit is untouched by this.

Editing comes with a keyboard. `cellNavigation` is on as soon as a column is editable, and the cursor is the focused `td` itself, not a ref: arrows move it, Home and End go to the ends of a row (of the grid under ctrl or cmd), the Page keys move it a page, Enter and F2 open the cell it is on, and any printable key opens it on that character. Space selects the row where selection is bound, and Escape closes an editor without writing.

Tab needs no interception, which is the one piece of this worth explaining. The tab stops ARE the editable cells (`tabindex="0"` on those, `-1` on the rest), and the DOM is row-major, so the browser's own tab order already is "the next editable cell, wrapping into the row below". The handler only has to agree with it: it commits what Tab left and opens what Tab arrived at. That matters because a kyo handler is asynchronous and cannot decline a browser default in time; the arrows, which have no such order to borrow, are suppressed declaratively instead (`preventScrollKeys`, so the page underneath does not scroll).

An editor that closes hands focus back to the cell it was in, which is what keeps the cursor alive: the editor IS the focused element while it is open, so without that the next arrow key would go nowhere. Not after Tab, where the browser is already moving focus, and not after a refused commit, where the reader is still in the editor. A row closed by its save or cancel button lands on the row's first editable cell rather than on the button that replaced the one just pressed: that element is new, and a self-command resolves its id the moment it arrives, which can be before the insert.

`Column.navigable(false)` takes a column out of the cursor's path: the arrows step over it and it is never a tab stop, while the mouse, the sorting and the rendering are unaffected. `cellNavigation(false)` turns the whole thing off, and a table that opts out (or never edits) renders exactly the markup and the tab order it did before any of this existed.

`groupBy` takes one nested level per argument, outermost first, and each level presents every run of *consecutive* rows sharing its key as a group. Consecutive is the whole contract: grouping reads the order the table is about to render in, it does not impose one, so it composes with the sort spec instead of competing with it for authority over row order. Pair it with a sort that leads with the same projections, or with rows that already arrive ordered; group by one key while sorting by another and the same key legitimately heads several runs, which is what the row order says.

A level is a value, built by `uic.group(key)` inside the `groupBy(...)` call, so it reads its row type from the table the same way `uic.column` does. It carries its own `header`, its own `footer`, and `showHeader(false)` for a level that only scopes a summary row. Both templates receive a `GroupPath`: `path.key` is that level's own key and `path.keys` the whole chain from the outermost level down. Levels close innermost first, so a level's summary row sits inside its parent's.

The key may be of any type, and it labels itself through `toString`, so `group(_.year)` needs no projection of its own and `group(_.category)` is unchanged, `String`'s `toString` being the identity. That label is also the group's identity: two groups that read the same ARE the same group, which is already what lets a key heading several runs share one collapse state. Project to the text you want shown and you have set both at once.

`expandedGroups(ref)` binds a `Set[GroupPath]` of EXPANDED groups, so an empty set starts everything closed. The currency is a path and not a key, because below the outermost level a key is not an identity: the same brand sits under every category that sells one, and collapsing it in one place must leave the others alone. Collapsing hides everything nested inside a group, its own summary included, and keeps its slice of the page, since `paginate` slices rows before they are grouped.

```scala
val groupedTable: UI < Async =
    for
        sort <- Signal.initRef(List(uic.SortKey.ascending("Category")))
        open <- Signal.initRef(Set(uic.GroupPath("Accessories")))
    yield uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(
            uic.column("Category")(_.category).sortBy(_.category),
            uic.column("Name")(_.name).sortBy(_.name),
            uic.column("Price")(p => f"${p.price}%.2f").align(uic.ColumnAlign.End)
        )
        .sort(sort)
        .groupBy(
            uic.group(_.category).header((path, rows) => span(s"${path.key} (${rows.size})")),
            uic.group(_.inStock.toString).footer((_, rows) => span(f"${rows.map(_.price).sum}%.2f"))
        )
        .expandedGroups(open): UI
```

The other way to show a key is `Column.rowSpan`, which merges that column's cells across their run into one spanning cell. It is a marker on the column rather than a name pointing at one, so it cannot select a column the table does not have.

Bare, it merges by the column's own text projection, which is the key nine times in ten and would otherwise be written twice on one line. A column rendering only a `body` template has no text to merge by, and rather than merging nothing it does not compile: the column's kind carries whether a text projection is there. `rowSpan(key)` names the key instead, which is what a body-only column takes and what a text column reaches for when it should merge by something other than what it shows.

That key is never rendered, only compared, so unlike a group key it is any type this build's strict equality will compare: an id, a tuple of two fields, an opaque type with a derived `CanEqual`. Routing it through a `String` would allocate on every comparison and merge two keys whose `toString` happened to agree.

> **Caution:** `footer` and `rowSpan(key)` drop the text projection from the column's kind, since neither needs it. Reach for the bare `rowSpan` before them, or pass the key explicitly.

Runs are clipped by every merged column to the *left* and by the innermost group a row sits in. That is load-bearing rather than tidy: a full-width group header row inside a merged run would overlap the span and break the table, and two merged columns whose runs crossed would do the same. Clipping makes both impossible, and it makes column order the outer-to-inner order, which is how a merged table reads anyway.

```scala
val mergedTable: UI < Async =
    for sort <- Signal.initRef(List(uic.SortKey.ascending("Category")))
    yield uic.DataTable[Product]()
        .rows(catalog)
        .rowKey(_.id)
        .columns(
            uic.column("Category")(_.category).sortBy(_.category).rowSpan,
            uic.column("Name")(_.name).sortBy(_.name)
        )
        .sort(sort)
        .showGridlines(true): UI
```

A `headerGroup` is a `DataTable` shape only: a `TreeTable` renders one header row over a hierarchy, and passing it a group is a compile error. `Column` itself is shared with `TreeTable`, and two of its options are not: `footer` fills a `tfoot` a TreeTable does not render, and `rowSpan` merges runs of equal cells, which rows at different depths do not form. Both return a `Column[A, FlatOnly]`, and `TreeTable.columns` takes only `AnyTable` columns, so passing one is a compile error naming the reason rather than an option quietly dropped at render. That second type argument is the only place the distinction shows: `Seq[uic.Column[R, uic.AnyTable]]` for a reusable list every table takes, `Seq[uic.Column[R, uic.FlatOnly]]` for one that merges.

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
            .columns(uic.column("Name")(_.name), uic.column("Category")(_.category))
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

`DataTable.scrollRows` is the same windowing inside a table, over the same kind of source. Reach for `VirtualScroller` where the rows are not a grid: a feed, a log, a picker.

Handed a `RowSource` instead of a sequence, it holds no list at all: it writes the visible range into the source on every scroll and draws what the source published, so a list of any length costs the blocks the reader actually looked at. Rows inside the window that have not arrived are drawn as `Skeleton` slots of the same height, which is what keeps the geometry from jumping while they load.

```scala
val lazyList: UI =
    UI.mounted {
        for
            query <- Signal.initRef("")
            source <- uic.RowSource.init(query, pageSize = 40) { (q, offset, limit) =>
                lookUp(q, Nil, offset, limit)
            }
        yield uic.VirtualScroller(source).itemSize(48).height(400)(p => div(span(p.name))): UI
    }
```

The rows are placed at the offset the source PUBLISHED, not at the range the viewport asked for, and those two differ for exactly as long as a fetch is in flight. How far the scrollbar reaches comes from the `Total`: a known count spans the whole list, and an unknown one spans the furthest the source has served plus one screen while anything follows, which is infinite scrolling without a mode of its own. A block that reports nothing after it makes the length exact, so the scrollbar settles the moment the reader reaches the end.

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

Validation is a separate, opt-in layer in `kyo.uic.form`. Nothing above this section knows it exists: the controls already carry `invalid` and `invalidMessage`, and the form layer is a thing that computes them for you and coordinates submit. Four ideas carry it. Errors are data, never text. *When a rule re-computes* and *when its failure is shown* are independent knobs. A field is declared by a spec with a default for every slot, so the common declaration says nothing but the starting value. And binding a field onto a control preserves the control's concrete type, so the form layer never hands you back an opaque wrapper.

A form is a mount region: `Form.mountedWith` allocates the state, runs your builder with the live `Form` scope, and returns an ordinary `UI`.

`scope.field(initial)` returns a `FieldSpec`, and `.declare` turns it into the live handle. Everything decided at declaration lives on that spec — `rules`, `on`, `domId`, `debounce`, `revealWhen`, `focusable` — each with the default most fields want, so `scope.field(true).declare` is a complete declaration. What stays on the handle is what can only be done afterwards: `addRule` and `matches`, which need a second field that does not exist yet when the first is declared.

```scala
import kyo.uic.form.*

val nameForm: UI =
    Form.mountedWith(ErrorTranslator.default) { scope =>
        for name <- scope.field.rules(Validator.required() and Validator.minLength(2)).declare
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
        for name <- scope.field.rules(Validator.required()).declare
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

`Activation.Field` values combine, and a field validates on the union of the ones it declares. `Change` re-checks on every value change, `Blur` on focus loss. `Reveal` picks the display gate: `WhenTouched` (the default, so an untouched field is quiet), `OnSubmit`, `Immediate`, or `Manual` (never auto-shown, but still feeding `isValid` and still blocking submit).

The defaults are `Blur + Change` with `WhenTouched`, which is why the examples above declare neither: nothing is said while the reader first types, the verdict appears when they leave the field, and it stays live from then on. `Activation.Submit` is the opt-*out* rather than a third trigger — every field is validated at submit whatever it declares, so declaring `Submit` alone means "never re-check while the reader is in the field", which is what an expensive async rule wants.

```scala
val quietUntilSubmit: UI =
    Form.mountedWith(ErrorTranslator.default) { scope =>
        for
            name <- scope.field
                .rules(Validator.required() and Validator.minLength(2))
                .revealWhen(Reveal.OnSubmit)
                .declare
        yield div(uic.Input().bind(name))
    }
```

That form re-validates as the user types (so a submit gate can open the instant the data becomes valid) while showing nothing until the first submit.

### Binding a field onto a control

`.bind(field)` wires the field's value ref, its DOM id, its gated message, and its blur trigger onto a control, and returns the same concrete control type. `uic.Input().bind(f)` is an `Input`, so setters keep chaining on either side of the call.

The id is the form's slot, not the control's: `focusFirstInvalid` and the error summary both reach a field through it, so `bind` stamps the field's own and a `.id(...)` already on the control is replaced. Choose it at declaration instead — `scope.field.domId("login-username")` — which is also what makes a field addressable from outside the form at all: a hand-written `<label for>`, an `aria-describedby` on a sibling hint, a deep link like `/settings#email`, an end-to-end selector. Left alone, the id is minted (`kyo-uic-7`), unique and stable within a render and referenceable only from inside via `field.domId`. A form reports what it can be certain of here — an id `bind` displaced, two fields claiming one id, a `<label for>` pointing at nothing — as an inline card at mount, because every one of those used to fail by moving focus nowhere and saying nothing.

```scala
val bound: UI =
    Form.mountedWith(ErrorTranslator.default) { scope =>
        for
            name  <- scope.field.rules(Validator.required() and Validator.minLength(2)).declare
            stock <- scope.field(true).declare
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
            price <- scope.numberField(0.0).rules(Validator.min(0.0) and Validator.max(10000.0)).declare
            restock <- scope.dateField[LocalDate](Absent)
                .rules(Validator.satisfy[LocalDate]("too-early")(_.isAfter(LocalDate.of(2020, 1, 1))))
                .declare
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
            name  <- scope.field.rules(Validator.required() and Validator.minLength(2)).declare
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
            listPrice <- scope.numberField(0.0).rules(Validator.min(0.0)).declare
            salePrice <- scope.numberField(0.0).rules(Validator.min(0.0)).declare
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
                for sku <- rowScope.field.rules(Validator.required() and Validator.pattern("[a-z0-9-]+".r)).declare
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
                name    <- scope.field.rules(Validator.required() and Validator.minLength(2)).declare
                price   <- scope.numberField(0.0).rules(Validator.min(0.0)).declare
                inStock <- scope.field(true).declare
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
