package kyo

import kyo.internal.Devtools
import kyo.internal.HtmlRenderer
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.Reducible
import kyo.internal.UICommands
import kyo.internal.UIExchange
import kyo.internal.UIServer
import scala.annotation.targetName
import scala.language.implicitConversions

/** An immutable description of a DOM subtree.
  *
  * A `UI` is a pure value, not a live widget. Building one allocates plain case classes and runs no effects; the same value can be rendered
  * repeatedly and shared freely. There are three kinds of node:
  *
  *   - **Elements** (`Div`, `Button`, `Input`, ...): the HTML tags, each an [[kyo.UI.Ast.Element]] case class carrying its attributes and
  *     children.
  *   - **Text** (`Text`): a literal text node; a bare `String` child auto-lifts to one.
  *   - **Reactive nodes** (`Reactive`, `Foreach`): subtrees driven by a [[kyo.Signal]]; they re-render when the signal emits.
  *
  * Elements are built with a fluent style. Each attribute/event setter returns the element's own concrete type (`Self`), so chains stay
  * typed and container factories can restrict their children at the type level. Reactivity enters through setters and child slots that
  * accept a `Signal[A]` (or `SignalRef[A]` for two-way binding) in place of a plain `A`; those overloads return `UI` because they cross
  * the reactive boundary.
  *
  * The same `UI` value runs three ways, changing only the runner:
  *
  *   - `runMount` (Scala.js): mounts to the live DOM as a single-page app.
  *   - `runHandlers`: exposes it as a server-push pair (SSR page GET and a WebSocket diff channel).
  *   - `runRender`: emits a `Stream[String, Async]` of full-page HTML for SSR, tests, or custom transports.
  *
  * @see
  *   [[kyo.UI.div]], [[kyo.UI.button]], [[kyo.UI.input]] and the other factory constructors for building elements
  * @see
  *   [[kyo.UI.when]], [[kyo.UI.render]] and [[kyo.Signal]] for reactive nodes
  * @see
  *   [[kyo.UI.runHandlers]], [[kyo.UI.runRender]] for running a UI value
  * @see
  *   [[kyo.UI.Ast]] for the case-class AST that every factory returns
  */
sealed abstract class UI:
    private[kyo] def frame: Frame

/** Companion of [[kyo.UI]]: factory constructors, the [[kyo.UI.Ast]] node types, and the runners.
  *
  * Import `kyo.UI.*` to bring the element factories (`div`, `button`, `input`, ...), the reactive combinators (`when`, the `Signal`
  * extensions), and the implicit `String`/`Signal` lifts into scope.
  *
  * @see
  *   [[kyo.UI]] for the conceptual overview of the UI tree
  */
object UI:

    given CanEqual[UI, UI] = CanEqual.derived

    import Ast.*

    /** Auto-lifts a bare `String` into a [[kyo.UI.Ast.Text]] node, so string literals can be passed directly wherever a `UI` child is
      * expected. Returns `Text` (not the wider `UI`) so that the static type satisfies `HtmlContent` in container `apply` overloads.
      */
    implicit def stringToUI(v: String)(using Frame): Text = Text(v)

    /** Auto-lifts a `Signal[String]` into a reactive text node that re-renders whenever the signal emits. */
    implicit def signalStringToUI(v: Signal[String])(using Frame): Reactive[Text] =
        Reactive[Text](v.map(s => Text(s)), Present(Reactive.Source.Lifted(v)))

    /** Auto-lifts a `Signal[A]` of UI values into a reactive node that swaps in the latest emitted subtree on every emission. */
    implicit def signalUIToUI[A <: UI](v: Signal[A])(using Frame, CanEqual[A, A]): Reactive[A] = Reactive[A](v.map(x => x: UI))

    // ---- Signal extensions ----

    extension [A](signal: Signal[A])
        /** Projects a signal into a reactive subtree: `f` maps each emitted value to a `C`, and the region re-renders whenever the signal
          * emits a value different from the last one it rendered.
          *
          * The node keeps `signal` and `f` beside the projected `Signal[UI]` ([[kyo.UI.Ast.Reactive.Source]]) so that the comparison
          * happens on the value. Comparing the projection instead cannot work: `f` allocates fresh handler closures on every call, so two
          * trees built from the same value are never equal, and a source that re-emits an unchanged value would repaint. `f` must
          * therefore be a pure function of its value: one that reads mutable state on the side is not refreshed by an emission that
          * carries the same value.
          */
        def render[C <: UI](f: A => C)(using Frame): Reactive[C] =
            val project: A => UI = a => f(a): UI
            Reactive[C](signal.map(project), Present(Reactive.Source.Projected(signal, project)))
    end extension

    extension [A](signal: Signal[Chunk[A]])
        /** Renders one child per element of the collection signal, re-rendering when the chunk changes. Elements are reconciled by
          * position; for stable identity across reorders and insertions use [[foreachKeyed]].
          */
        def foreach[C <: UI](render: A => C)(using Frame): Foreach[A, C] = Foreach[A, C](signal, Absent, (_, a) => render(a))

        /** Like [[foreach]] but `render` also receives each element's index. */
        def foreachIndexed[C <: UI](render: (Int, A) => C)(using Frame): Foreach[A, C] =
            Foreach[A, C](signal, Absent, render, indexed = true)

        /** Renders one child per element, identifying elements by `key` so reorders and insertions reuse the matching existing nodes
          * instead of re-rendering positionally. Rows whose key persists across list changes keep their live subscriptions and are
          * neither re-rendered nor re-walked while their item value is unchanged (value equality).
          */
        def foreachKeyed[C <: UI](key: A => String)(render: A => C)(using Frame): Foreach[A, C] =
            Foreach[A, C](signal, Present(key), (_, a) => render(a))

        /** Keyed [[foreachKeyed]] whose `render` also receives each element's index. Because the index is baked into each row's
          * content, rows are re-rendered on every list change (no cross-emission row reuse).
          */
        def foreachKeyedIndexed[C <: UI](key: A => String)(render: (Int, A) => C)(using Frame): Foreach[A, C] =
            Foreach[A, C](signal, Present(key), render, indexed = true)
    end extension

    // ---- Props ----

    /** A value a component reads that may change while it is shown: a constant, or a signal.
      *
      * A setter takes one, so a caller writes `.label("Save")` or `.label(name)` with no wrapper, and a default reads
      * `label: Prop[String] = ""`. A component reads it where it is used, the way Solid reads a prop: a constant is a plain
      * value and costs no subscription, a signal draws in one region of its own. The operations in [[Prop$ Prop]] keep that
      * split, so code that reads a prop is written once for both cases:
      *
      * {{{
      * Prop.render(busy)(on => if on then spinner else UI.empty)      // a region only where busy is a signal
      * Prop.map(Prop.zip(disabled, busy))(_ || _)                     // a constant where both are
      * }}}
      *
      * Drawing goes through `render`, never through `map` into a `Signal[UI]`: a region made by `render` compares the value it
      * was drawn from, while two trees built from one value are never equal (each holds fresh handler closures). A two-way
      * binding is a [[kyo.SignalRef]] in the prop, found again by its runtime class with `writable`.
      *
      * A text or element child takes a prop directly (`span(label)`), through the conversions on
      * [[kyo.UI.Ast.HtmlChildVal]].
      *
      * The signal comes first in the union, and that order is load-bearing. When a parameter typed `Prop[A]` takes an argument, the
      * compiler solves `A` against the union's branches in order and keeps the first unless a later one constrains less; `Signal` is
      * invariant, so neither does. Written `A | Signal[A]`, a bare signal, or a union whose alias was lost (a pattern binder), would
      * make `A` the signal or the whole union, and the function after it would receive that. Signal first, `A` is the element in
      * every case. `PropTest` ("the element type comes from the argument") holds each case, since this rests on how the compiler
      * breaks the tie rather than on the language spec.
      */
    type Prop[A] = Signal[A] | A

    /** The operations of a [[Prop]], as functions over it, not extensions: a prop is any value as well, so an extension on it
      * would apply to every value and hide the extensions a value already has (kyo's own `map` on `A < S` among them).
      */
    object Prop:

        /** The prop as a signal: a signal as it is, a constant as a constant signal. For combining with other signals; a
          * component that only reads the prop uses [[render]] or [[map]], which make no signal for a constant.
          */
        def signal[A](p: Prop[A])(using CanEqual[A, A], Frame): Signal[A] = p match
            case s: Signal[?] => s.asInstanceOf[Signal[A]]
            case a            => Signal.initConst(a.asInstanceOf[A])

        /** Drawn from the prop: a constant draws once with no region, a signal in one region compared by its value. */
        def render[A](p: Prop[A])(f: A => UI)(using Frame): UI = p match
            case s: Signal[?] => UI.render(s.asInstanceOf[Signal[A]])(f) // by name: here `render` is this method
            case a            => f(a.asInstanceOf[A])

        /** The prop through `f`, for a value that is not drawn: a constant gives a constant, a signal a mapped signal. */
        def map[A, B](p: Prop[A])(f: A => B)(using CanEqual[B, B], Frame): Prop[B] = p match
            case s: Signal[?] => s.asInstanceOf[Signal[A]].map(f)
            case a            => f(a.asInstanceOf[A])

        /** The prop's value now, for a handler that reads it when its event comes: a constant as it is, with no signal made
          * for it, a signal its current value.
          */
        def current[A](p: Prop[A])(using Frame): A < Sync = p match
            case s: Signal[?] => s.asInstanceOf[Signal[A]].current
            case a            => a.asInstanceOf[A]

        /** Both props as one: two constants stay a constant, anything else is one combined signal, never one region inside
          * another.
          */
        def zip[A, B](p: Prop[A], q: Prop[B])(using CanEqual[A, A], CanEqual[B, B], Frame): Prop[(A, B)] = (p, q) match
            case (_: Signal[?], _) | (_, _: Signal[?]) => signal(p).combineLatest(signal(q))
            case (a, b)                                => (a.asInstanceOf[A], b.asInstanceOf[B])

        /** The ref a reader's change is written into, when the prop binds two way: a [[kyo.SignalRef]] by its runtime class.
          * Any other signal, and a constant, binds one way. `ref.readOnly` is how a caller opts out.
          */
        def writable[A](p: Prop[A]): Maybe[SignalRef[A]] = p match
            case r: SignalRef[?] => Present(r.asInstanceOf[SignalRef[A]])
            case _               => Absent
    end Prop

    /** The props of one instance of a [[kyo.UI.mountedWith]] node, as its draw reads them. The draw runs once per instance, and
      * each place in it reads the field it shows:
      *
      * {{{
      * UI.mountedWith(props) { live =>
      *     button(live(_.label)).onClick(live.call(_.onClick).flatMap(f => f()))
      * }
      * }}}
      *
      * Where the engine can hand the instance new props, a keyed node in a region that can run again, `runs` holds the props of
      * the latest run there, and `apply` follows them: a field that is a constant on every run reads as a signal then, since the
      * next run may bring another one. Anywhere else no run can follow the first, so `apply` gives the field as the node was
      * called with it, and a constant stays a constant with no subscription. `call` reads a field when an event comes, so a
      * callback is the latest run's either way and costs nothing until then.
      */
    final class Live[P] private[kyo] (first: P, runsRef: Maybe[SignalRef[kyo.internal.Owning.Run[P]]], owning: kyo.internal.Owning[P]):

        /** Props with no ownable prop resolved: a node drawn in place, which a host binds itself. */
        private[kyo] def this(first: P, runsRef: Maybe[SignalRef[kyo.internal.Owning.Run[P]]]) =
            this(first, runsRef, kyo.internal.Owning.none[P])

        // The props as the draw reads them where no run can follow the first: resolved once, here.
        private val shown: P = if runsRef.isEmpty then owning.resolve(first) else first

        // The latest run, where the engine can hand this instance new props: its props as given and as resolved.
        private val runs: Maybe[Signal[kyo.internal.Owning.Run[P]]] = runsRef

        /** Hands this instance the props of a new run: an ownable prop the run stops binding hands its value to the instance first,
          * then the new props, resolved once here, are what every field follows. Equal props change nothing.
          */
        private[kyo] def offer(next: P)(using CanEqual[P, P], Frame): Unit < Sync =
            runsRef match
                case Present(ref) =>
                    ref.get.map { run =>
                        if run.props == next then Kyo.unit
                        else owning.carry(run.props, next).andThen(ref.set(kyo.internal.Owning.Run(next, owning.resolve(next))))
                    }
                case Absent => Kyo.unit
        end offer

        /** The field `field` picks, following the latest run where one can follow, else as the node was called with it. A run that
          * leaves the field as it was (the same constant, the same signal) changes nothing for it.
          */
        def apply[A](field: P => Prop[A])(using CanEqual[A, A], Frame): Prop[A] =
            runs match
                case Present(signal) =>
                    // The field's own identity: a constant compares as its value, a signal as the instance it is.
                    given CanEqual[Prop[A], Prop[A]] = CanEqual.derived
                    signal.map(run => field(run.resolved)).switchMap(Prop.signal(_))
                case Absent => field(shown)

        /** An optional field as one prop of an option: `Absent` where the run sets none, the field's value where it does, so one
          * read gives `Maybe[A]` (`props(_.label).read` for a `label: Maybe[Prop[String]]`), and a signal inside it is followed
          * like a signal anywhere.
          */
        @targetName("applyMaybe")
        def apply[A](field: P => Maybe[Prop[A]])(using CanEqual[A, A], Frame): Prop[Maybe[A]] =
            def flat(m: Maybe[Prop[A]]): Prop[Maybe[A]] = m match
                case Present(a) => Prop.map(a)(Present(_))
                case _          => Absent
            runs match
                case Present(signal) =>
                    // As in `apply`: a field the run leaves as it was changes nothing for it.
                    given CanEqual[Maybe[Prop[A]], Maybe[Prop[A]]] = CanEqual.derived
                    signal.map(run => field(run.resolved)).switchMap(m => Prop.signal(flat(m)))
                case Absent => flat(field(shown))
            end match
        end apply

        /** The field `field` picks, read now: from the latest run where one can follow, else as the node was called with it. */
        def call[A](field: P => A)(using Frame): A < Sync =
            runs match
                case Present(signal) => signal.current.map(run => field(run.resolved))
                case Absent          => field(shown)
    end Live

    /** A prop a component instance holds itself where the caller binds none: a checked state, a value, an open panel.
      *
      * A component declares one in its props with where its own value starts, and reads it like any other prop: the engine binds
      * the instance's own ref wherever a run leaves it unbound, made once per instance, and a run that stops binding it hands the
      * value it showed to that ref first, so the instance carries on from it. A caller's binding (a ref two way, a signal one way, a
      * constant) is the prop wherever the caller gives one.
      */
    trait Ownable[A]:
        /** What the caller bound, `Absent` where it bound nothing. */
        def bound: Maybe[Prop[A]]

        /** Where the instance's own value starts, `Absent` where the component keeps none of its own for this prop. */
        def start: Maybe[A]

        /** This prop bound as the caller says: a ref two way, any other signal one way, a constant. The engine binds the instance's
          * own ref with it in place of nothing, as it hands the props to a draw.
          */
        def bind(v: Prop[A]): Ownable[A]

        /** This prop with `f` hearing every change the reader makes, in place of whoever heard it before. `f` runs as given. */
        def withHearer(f: A => Action): Ownable[A]
    end Ownable

    // ---- Factory constructors ----

    /** The empty UI: an [[kyo.UI.Ast.Fragment]] with no children, rendering nothing. Useful as a neutral element or a conditional
      * "render nothing" branch. Returns `Fragment[Nothing]`, which an HTML container accepts because `AsHtmlChild[Fragment[Nothing]]`
      * resolves (the `Fragment` given needs `AsHtmlChild[Nothing]`, which the base `[T <: HtmlContent]` given supplies since
      * `Nothing <: HtmlContent`).
      */
    def empty(using Frame): Fragment[Nothing] = Fragment[Nothing](Chunk.empty[UI])

    def div(using Frame): Div                     = Div()
    def p(using Frame): P                         = P()
    def span(using Frame): SpanElement            = SpanElement()
    def ul(using Frame): Ul                       = Ul()
    def ol(using Frame): Ol                       = Ol()
    def li(using Frame): Li                       = Li()
    def nav(using Frame): Nav                     = Nav()
    def header(using Frame): Header               = Header()
    def footer(using Frame): Footer               = Footer()
    def section(using Frame): Section             = Section()
    def main(using Frame): Main                   = Main()
    def label(using Frame): Label                 = Label()
    def pre(using Frame): Pre                     = Pre()
    def blockquote(using Frame): Blockquote       = Blockquote()
    def code(using Frame): Code                   = Code()
    def table(using Frame): Table                 = Table()
    def colgroup(using Frame): Colgroup           = Colgroup()
    def col(using Frame): Col                     = Col()
    def thead(using Frame): Thead                 = Thead()
    def tbody(using Frame): Tbody                 = Tbody()
    def tfoot(using Frame): Tfoot                 = Tfoot()
    def tr(using Frame): Tr                       = Tr()
    def td(using Frame): Td                       = Td()
    def th(using Frame): Th                       = Th()
    def h1(using Frame): H1                       = H1()
    def h2(using Frame): H2                       = H2()
    def h3(using Frame): H3                       = H3()
    def h4(using Frame): H4                       = H4()
    def h5(using Frame): H5                       = H5()
    def h6(using Frame): H6                       = H6()
    def hr(using Frame): Hr                       = Hr()
    def br(using Frame): Br                       = Br()
    def button(using Frame): Button               = Button()
    def checkbox(using Frame): Checkbox           = Checkbox()
    def radio(using Frame): Radio                 = Radio()
    def a(using Frame): Anchor                    = Anchor()
    def form(using Frame): Form                   = Form()
    def fieldset(using Frame): Fieldset           = Fieldset()
    def legend(using Frame): Legend               = Legend()
    def select(using Frame): Select               = Select()
    def option(using Frame): Opt                  = Opt()
    def input(using Frame): Input                 = Input()
    def passwordInput(using Frame): PasswordInput = PasswordInput()
    def emailInput(using Frame): EmailInput       = EmailInput()
    def telInput(using Frame): TelInput           = TelInput()
    def urlInput(using Frame): UrlInput           = UrlInput()
    def searchInput(using Frame): SearchInput     = SearchInput()
    def numberInput(using Frame): NumberInput     = NumberInput()
    def dateInput(using Frame): DateInput         = DateInput()
    def timeInput(using Frame): TimeInput         = TimeInput()
    def colorInput(using Frame): ColorInput       = ColorInput()
    def rangeInput(using Frame): RangeInput       = RangeInput()
    def fileInput(using Frame): FileInput         = FileInput()
    def hiddenInput(using Frame): HiddenInput     = HiddenInput()
    def textarea(using Frame): Textarea           = Textarea()

    /** A custom dropdown rendered as a `div`-based overlay (not a native `<select>`), styleable like any other element. Each option is a
      * `(value, label)` pair.
      */
    def dropdown(options: (String, String)*)(using Frame): Dropdown = Dropdown(options = Chunk.from(options))

    /** An image element. `src` selects the source kind (URL, path, or inline data) and `alt` is the accessibility text. */
    def img(src: ImgSrc, alt: String)(using Frame): Img = Img(src = Present(src), alt = Present(alt))

    /** An inline frame embedding the document at `src` (a URL or `data:` URI). Give it a size with `.style` and a `.title` for accessibility. */
    def iframe(src: String)(using Frame): Iframe = Iframe(src = Present(src))

    /** Groups children into a single UI without introducing a wrapper element; the children render as siblings in the parent.
      * The type parameter `C` is inferred from the children, so `fragment(div, span)` produces `Fragment[Div | SpanElement]`,
      * which an HTML container accepts because `AsHtmlChild[Fragment[Div | SpanElement]]` resolves (the `Fragment` given on top
      * of the union given). `fragment(circle, rect)` produces a `Fragment` that fits SVG containers. A mixed `fragment(div, circle)`
      * fits neither (correct: no `AsHtmlChild` witness for the SVG side and no SVG witness for the HTML side).
      */
    def fragment[C <: UI](cs: C*)(using Frame): Fragment[C] = Fragment[C](Chunk.from(cs))

    /** Several children with no container around them, for a container's argument list: `div(kids*)`. A helper that draws a part of
      * an element's content hands it back as `Children`, and another `UI.children` takes it as one part.
      */
    type Children = Seq[HtmlChildVal]

    object Children:
        /** One part of [[UI.children]]: a child, a `Maybe` of one, a `Seq` of them, or [[Children]]. Each is made implicitly and
          * checked like a literal child, on its static element type, so an SVG node does not fit an HTML container in any form.
          */
        final class Part private[kyo] (private[kyo] val values: Children) extends AnyVal

        object Part:
            implicit def one[T <: UI](t: T)(using AsHtmlChild[T]): Part             = new Part(Seq(new HtmlChildVal(t)))
            implicit def optional[T <: UI](m: Maybe[T])(using AsHtmlChild[T]): Part =
                new Part(m.fold(Seq.empty[HtmlChildVal])(t => Seq(new HtmlChildVal(t))))
            implicit def many[T <: UI](xs: Seq[T])(using AsHtmlChild[T]): Part = new Part(xs.map(new HtmlChildVal(_)))
            implicit def drawn(cs: Children): Part                             = new Part(cs)
        end Part
    end Children

    /** The children in `parts`, in order, for a container's argument list: `div(UI.children(icon, label, rows)*)`. An implicit
      * conversion does not reach into a splat, so a list needs this. An absent part adds nothing, a list adds its elements. The tree
      * gets no extra level, unlike `fragment`, so the children keep the paths they would have as literal children.
      */
    def children(parts: Children.Part*): Children =
        if parts.sizeIs == 1 then parts.head.values else parts.flatMap(_.values)

    /** Inline HTML passthrough: emits `html` verbatim into the rendered output without any escaping.
      *
      * This is the framework's single deliberate escape hatch for trusted HTML content that cannot be expressed through the normal `UI`
      * element API. Use it only for content you control and trust completely: strings from external sources, user input, or any untrusted
      * origin must NOT be passed here as doing so creates an XSS vulnerability. There is no sanitization.
      *
      * Intended for the bounded case of inline HTML snippets (such as `<img>` or `<a><img></a>` nodes found in module README files) that
      * the Markdown transpiler encounters and must pass through verbatim. The article body itself is always a real `UI` subtree; this node
      * appears only at the leaf positions where raw HTML snippets exist in the source.
      *
      * The rendered bytes are byte-identical to `html`. Contrast with `UI.text`, which HTML-escapes its argument:
      * `UI.text("<b>x</b>")` renders `&lt;b&gt;x&lt;/b&gt;`; `UI.rawHtml("<b>x</b>")` renders `<b>x</b>`.
      *
      * @param html
      *   Trusted HTML string to emit verbatim. Must come from a controlled, trusted source.
      * @see
      *   [[kyo.UI.Ast.RawHtml]] for the AST node this factory returns
      * @see
      *   `kyo.UI.text` for the safe alternative that HTML-escapes its argument
      */
    def rawHtml(html: String)(using Frame): UI = Ast.RawHtml(html)

    /** Build a `<script type="...">` data island carrying a verbatim JSON body and an optional `id`.
      *
      * A data island is an inert structured-data block read back by a script (the JSON-LD
      * SEO block, the SSG-seeded boot payload), NOT a reactive element. It returns a
      * [[kyo.UI.DataIsland]], which does NOT extend [[kyo.UI]], so the type system rejects
      * placing it as a live tree child: it can be placed ONLY through the [[kyo.UI.PageHead]]
      * `jsonLd` (head) / `dataIslands` (body-end) fields. `scriptType` is
      * `application/ld+json` for the JSON-LD block and `application/json` for a data island;
      * `id` is `Present` for an addressable island (read back by `querySelector("#id")`) and
      * `Absent` for the JSON-LD block.
      *
      * The `json` body is stored verbatim at construction and escaped only at render time
      * (`<` becomes `<`, `>` becomes `>`), so a literal `</script>` substring in
      * any field renders as inert text that cannot close the element. This factory owns the
      * one escape; callers pass trusted JSON and never pre-escape it.
      *
      * @param scriptType
      *   the `type` attribute (`application/ld+json` or `application/json`)
      * @param id
      *   the element `id` (`Present` for an addressable island, `Absent` for the JSON-LD block)
      * @param json
      *   the verbatim JSON body (trusted; escaped at render time)
      * @see
      *   [[kyo.UI.PageHead.jsonLd]], [[kyo.UI.PageHead.dataIslands]] for the fields that place it
      * @see
      *   [[kyo.UI.rawHtml]] for the verbatim-HTML leaf
      */
    def dataIsland(scriptType: String, id: Maybe[String], json: String)(using Frame): UI.DataIsland =
        UI.DataIsland(scriptType, id, json)

    /** A `<script type="..." id="...?">json</script>` document-level payload, built by
      * [[kyo.UI.dataIsland]] and carried on [[kyo.UI.PageHead]] (the `jsonLd` head field and the
      * `dataIslands` body-end field).
      *
      * It does NOT extend [[kyo.UI]]: a data island is a `PageHead` payload, not a renderable UI
      * node. Because it is not a `UI`, the type system rejects placing it as a live tree child,
      * and the renderer never sees it as a tree node; it is rendered only from the `PageHead`
      * fields that carry it, as an inert, position-stable block with no `data-kyo-path`, no
      * attributes, and no event handler. The `json` body is verbatim at construction and escaped
      * at render time. `derives CanEqual` because [[kyo.UI.PageHead]] carries these values and
      * itself `derives CanEqual`.
      */
    final case class DataIsland(scriptType: String, id: Maybe[String], json: String)
        derives CanEqual

    /** Conditional rendering: shows `body` while `condition` is true and an empty node otherwise, re-evaluating when the signal emits. */
    def when[C <: UI](condition: Signal[Boolean])(body: => C)(using Frame): Reactive[C] =
        condition.render(v => if v then body: UI else UI.empty).asInstanceOf[Reactive[C]]

    /** What a mounted node shows between losing its instance (a key change, or a re-render of a keyless node) and the new instance
      * publishing.
      *
      *   - `KeepLatest` (the default) keeps what the node last showed as an inert snapshot: rendered once from the signals' values at the
      *     moment of the switch, with no live regions and no handlers.
      *   - `Placeholder` shows the placeholder again, the honest choice for interactive content where a frozen form would swallow input.
      *
      * A node's first mount always shows the placeholder, since there is nothing to keep yet.
      */
    enum PendingPolicy derives CanEqual:
        case KeepLatest, Placeholder

    /** Which path a mounted node's failure came from.
      *
      *   - `Effect` and `Panic`: the mount effect aborted or panicked, so the node never produced content. Offering a retry (a key swap)
      *     makes sense.
      *   - `Fork`: the node had published content, and a fiber the effect started with [[kyo.UI.fork]] died afterwards. What was on
      *     screen came from a feed that no longer runs.
      *   - `Handler`: an event handler in the node's content failed, and an `onFailure` rule escalated it.
      *   - `Duplicate`: one render pass showed the node's identity (its `.keyed` call, key and list rows) in more than one place. None
      *     of those places gets an instance and the effect does not run; the message names every place.
      */
    enum MountErrorSource derives CanEqual:
        case Effect, Panic, Fork, Handler, Duplicate

    /** The failure each place of a keyed identity shown more than once in one render pass reports, see
      * [[kyo.UI.MountErrorSource.Duplicate]]. The message names the places and the `UI.mounted` call.
      */
    final class DuplicateMountException(message: String) extends Exception(message) with scala.util.control.NoStackTrace

    /** The effect row of every event handler: it may suspend, and it may fail. A failure it leaves unhandled goes to the
      * [[kyo.UI.Reaction]] the nearest `onFailure` rule picks for it.
      */
    type Action = Any < (Async & Abort[Any])

    /** What an unhandled failure does, as an `onFailure` rule picks it.
      *
      *   - `Notify`: the content stays, and the failure goes to the nearest boundary's [[kyo.UI.Boundary.onNotice]] sink, or to the log
      *     when there is none.
      *   - `Escalate`: the enclosing [[kyo.UI.mounted]] node fails and shows its error view. Outside any mount it is `Notify`.
      *   - `Run`: runs `action`, a redirect say. When the action fails in turn, that failure is notified.
      */
    enum Reaction derives CanEqual:
        case Notify
        case Escalate
        case Run(action: Unit < (Async & Abort[Any]))
    end Reaction

    /** A failure that is not a `Throwable`, wrapped where a `Throwable` is expected: a [[kyo.UI.MountError.error]], an
      * [[kyo.UI.Boundary.onNotice]] sink. [[kyo.UI.Failed]] takes the value back out.
      */
    final case class FailureException(failure: Any)(using Frame) extends KyoException("Unhandled failure", String.valueOf(failure))

    /** The failure a `Throwable` stands for: the value a [[kyo.UI.FailureException]] wraps, otherwise the `Throwable` itself. */
    object Failed:
        def unapply(t: Throwable): Some[Any] =
            t match
                case FailureException(failure) => Some(failure)
                case other                     => Some(other)
    end Failed

    /** A mounted node's failure, with the context the `Throwable` alone does not carry.
      *
      * @param error
      *   the failure. An `Abort` failure that is not a `Throwable` arrives wrapped.
      * @param source
      *   which of the node's failure paths produced it, see [[kyo.UI.MountErrorSource]]
      * @param message
      *   a rendering-ready one-liner: the `Throwable`'s message, else its `toString`
      * @param frame
      *   the frame of the [[kyo.UI.mounted]] call, which points at the node that failed, not at the failure
      * @param path
      *   the node's render path, the identity the wire protocol addresses it by
      * @param key
      *   the node's [[kyo.UI.MountedOps.keyed]] key, if it has one
      */
    final case class MountError(
        error: Throwable,
        source: MountErrorSource,
        message: String,
        frame: Frame,
        path: Seq[String],
        key: Maybe[Any]
    ):
        /** The failure as it was raised: the `Abort` value, or the `Throwable` of a panic. [[kyo.UI.MountError.Failed]] matches on it. */
        def failure: Any = error match
            case Failed(raised) => raised
    end MountError

    object MountError:
        /** Matches a [[kyo.UI.MountError]] on the failure as it was raised: `case MountError.Failed(ApiError.NotFound) => ...`. */
        object Failed:
            def unapply(e: MountError): Some[Any] = Some(e.failure)
    end MountError

    /** Embeds a component that holds state of its own: what `draw` allocates lives as long as the node's instance.
      *
      * `draw` runs in the render pass, so what it returns is on the page from the first paint: a static rendering shows it, a live client
      * shows it with no placeholder in between, and it takes events from that paint on. There is nothing to wait for, so the node has no
      * `.placeholder`. A pass that shows the node again shows the same instance when it is [[kyo.UI.MountedOps.keyed]] or
      * [[kyo.UI.MountedOps.keyedOnSite]]; a node without a key gets a fresh instance whenever its enclosing reactive region re-renders.
      * In what a drawn component draws, with no region between, its place is its identity: it lives as long as that component's instance,
      * however often a region around the component runs. A static rendering draws too, and keeps nothing.
      *
      * Inside `draw`, [[kyo.UI.id]] names an element so that the static rendering and the live one agree, and [[kyo.UI.whileMounted]] starts
      * what has to run on a live page: a feed, an observer, a timer.
      *
      * A panic in `draw` renders through [[kyo.UI.MountedOps.onError]] (by default an inline error node).
      */
    @targetName("mountedDrawn")
    def mounted(draw: UI < Sync)(using Frame): Drawn =
        Drawn(draw, key = Absent, errorRender = Absent)

    /** [[mounted]] for a component whose configuration, `props`, can change while its instance lives. `draw` runs once per instance and
      * reads the props through a [[Live]]: a keyed instance that a region running again keeps is handed the props of the latest run there,
      * and what reads them follows, while what `draw` allocates (open state, a highlight, an id) is made once and stays. An instance no run
      * can reach again, one without a key or one in no region that runs again, reads its props as it was called with them, so a constant
      * prop costs it no subscription. Props compare by `==`: equal props reach no one.
      */
    def mountedWith[P](props: P)(draw: Live[P] => UI < Sync)(using CanEqual[P, P], scala.deriving.Mirror.ProductOf[P], Frame): Drawn =
        Drawn(UI.empty, key = Absent, errorRender = Absent, props = Present(new kyo.internal.PropsDraw(props, draw)))

    /** Embeds a component that waits for its UI: loading data, a round trip, anything that suspends.
      *
      * The effect runs when the node attaches on a live page, on a fiber, inside a `Scope` that lives as long as the node's instance, so
      * whatever the effect acquires or forks is released when the instance goes away. Until it publishes, the node shows its
      * placeholder, in a static rendering too. An `Abort` or panic renders through [[kyo.UI.MountedOps.onError]] (by default an inline
      * error node) while the enclosing region stays alive.
      *
      * Identity decides when the effect runs again. A node without a key gets a fresh instance whenever its enclosing reactive region
      * re-renders; in what a drawn component draws, with no region between, its place keeps it for as long as that component's instance
      * lives. A node with [[kyo.UI.MountedOps.keyed]] keeps its instance across re-renders of every region around it, as long as
      * each re-render shows it from the same `.keyed` call with an equal key in the same list row; that identity shown twice at once is
      * reported as a duplicate. A node with [[kyo.UI.MountedOps.keyedOnSite]] keeps it as long as each re-render shows it from the same
      * site in the same list row, as the same call in that order where one site is called more than once in its region.
      *
      * The effect must return promptly: allocate and fork, never park. Anything long-lived belongs on a forked fiber, which the
      * instance's `Scope` owns; fork it with [[kyo.UI.fork]] so its failure reaches the node. An effect typed wider than what it does
      * counts as waiting: the overload goes by the type.
      */
    @targetName("mountedWaiting")
    def mounted(effect: UI < (Abort[Any] & Async & Scope))(using Frame): Mounted =
        Mounted(effect, key = Absent, placeholderUI = Absent, errorRender = Absent, pendingPolicy = PendingPolicy.KeepLatest)

    /** An element id for the mount being drawn. An id is an address, not a lifetime: a keyed node's comes from its identity (the instance
      * that holds it, where its `.keyed` was written, its key and the keys of the list rows around it, and for a node keyed on its site
      * from the second call of that site in its region on, the order of the call), never from its place, so no sibling before it changes it; a node without a key keeps its place as base, so a new instance a re-render shows there gets the
      * same id and a morph keeps the element. Either way it is the same in a static rendering and on the live page, on the JVM and in
      * JavaScript, and needs no session. Each call in one draw names another element. Only inside a [[kyo.UI.mounted]] effect; anywhere
      * else it panics.
      */
    def id(using Frame): String < Sync =
        kyo.internal.DrawContext.current.use {
            case Present(context) => Sync.Unsafe.defer(context.nextId())
            case Absent           => Abort.panic(UIException("UI.id is only available while a UI.mounted effect runs"))
        }

    /** Runs `effect` on the live page for as long as the instance of the mount being drawn lives: on the client or in the session, after
      * its content is on the page, never in a static rendering. It runs on a fiber in the instance's `Scope`, so what it acquires or forks is
      * released when the instance ends, which for a mount without a key is the next run of its region, and an element hidden with
      * `.hidden(true)` keeps it running; a failure goes to the `onFailure` rules around the node, and when none matches it switches the node to its error
      * rendering, as a [[kyo.UI.fork]] does. The effect may run for as long as the node is shown, and several run side by side.
      *
      * Only while a [[kyo.UI.mounted]] effect runs; anywhere else, and from a fiber that effect forked once it has returned, it panics.
      */
    def whileMounted(effect: => Any < (Abort[Any] & Async & Scope))(using Frame): Unit < Sync =
        kyo.internal.DrawContext.current.use {
            case Present(context) if context.add(Sync.defer(effect)) => Kyo.unit
            case _ => Abort.panic(UIException("UI.whileMounted is only available while a UI.mounted effect runs"))
        }

    /** Where a [[kyo.UI.MountedOps.keyed]] call is written, which is part of a keyed instance's identity.
      *
      * It is fixed at compile time from the source position of the `.keyed` call, so a `Frame` passed down through the methods that
      * build the tree does not merge two calls. A helper that applies `.keyed` itself to effects its callers supply can take a
      * `(using UI.MountSite)`, which makes each of its callers a place of its own. A component that keys itself on where it was made
      * takes one the same way and hands it to [[kyo.UI.MountedOps.keyedOnSite]].
      */
    type MountSite = kyo.internal.MountSites.MountSite

    /** Runs `v` as one change for the keyed mounts of the page it runs in: whatever its writes move between regions is decided once it has
      * ended.
      *
      * A keyed instance that moves from one region to another keeps itself when the regions have seen every write of the change; the page
      * decides only then whether it moved, went away, or is shown in two places. Without `batch` a change is what the regions have not
      * seen yet: a region that is behind on a write is waited for, so writes that land before the regions run belong together. In the
      * browser that covers a handler's writes up to its first suspension, since no region runs while the handler does. On the JVM a
      * region can run on another thread between two writes of one handler, so a handler that writes more than once to move something
      * needs `batch`, and so does any code that writes on both sides of a suspension, on either platform: a fiber a mount effect forked,
      * a feed, a timer. Without it the first write is decided on its own, so the instance is released and the second write starts it
      * again.
      *
      * The decision waits for every region of the page to have seen the writes. A region over a signal without an exact read (one built
      * with `Signal.initRaw`, or a combinator over one) cannot tell, and is waited for until it reads again, which its repair timer brings
      * within one repair interval, so on such a page a move or a release can take that long to be decided.
      *
      * A `batch` must not wait for a release its own writes cause, such as a keyed node it hides going away: what its writes drop is
      * released only once it has ended, so the wait never returns. Write inside the `batch` and wait after it. Outside a mounted page
      * `batch` is `v`.
      */
    def batch[A, S](v: => A < S)(using Frame): A < (S & Sync) =
        kyo.internal.MountSession.current.use {
            case Present(session) => session.within(v)
            case Absent           => v
        }

    /** Forks a fiber whose failure the enclosing [[kyo.UI.mounted]] node reports: the `Fiber.init` to use inside a mount effect.
      *
      * A mount effect returns once it has set things up, so a feed it forked keeps running after the node shows its content. Forked with
      * `Fiber.init`, a feed that dies leaves that content on screen with nothing behind it. Forked with `UI.fork`, its first terminal
      * failure goes to the `onFailure` rules around the node, and when none matches it switches the node to its error rendering, with
      * [[kyo.UI.MountError.source]] set to `Fork`.
      *
      * An interrupt that reaches the fiber is a cancel and reports nothing: one you send it, one passed on from a fiber that waits on it
      * and is interrupted, and the node's release. An interrupt the fiber's own work ends with, without one reaching the fiber (it waited
      * on a promise that someone else's interrupt completed), is reported like any failure, unless the node is being released.
      *
      * This reaches `UI.fork` calls made from fibers the effect already forked with `UI.fork`. A plain `Fiber.init` does not take part,
      * which is the way to fork a fiber whose failure the effect handles itself, and outside a mount effect `UI.fork` is `Fiber.init`.
      * The switch replaces the node's content. To keep the content and report the failure elsewhere, give it a `Notify` rule, or report
      * it from a fiber that keeps running with [[kyo.UI.report]].
      */
    def fork[E, A, S, S2](
        using isolate: Isolate[S, Sync, S2]
    )(
        v: => A < (Abort[E] & Async & S)
    )(
        using
        reduce: Reducible[Abort[E]],
        frame: Frame
    ): Fiber[A, reduce.SReduced & S2] < (Sync & S & Scope) =
        kyo.internal.MountSupervision.local.use {
            case Absent               => Fiber.init[E, A, S, S2](v)
            case Present(supervision) =>
                Fiber.init[E, A, S, S2](v).map { fiber =>
                    for
                        cancelled <- AtomicBoolean.init(false)
                        _         <- fiber.onComplete(result => cancelled.get.map(supervision.report(result, _)))
                        // Registered after the report: a fiber runs its newest callback first, so the report sees the cancel.
                        _ <- fiber.onInterrupt(_ => cancelled.set(true))
                    yield fiber
                }
        }

    /** Reports a failure that should be surfaced without replacing any content: a background feed that died while its last
      * data is still worth showing, a refetch that failed over data already on screen.
      *
      * Goes to the nearest enclosing boundary's [[kyo.UI.Boundary.onNotice]] sink, and is logged when there is none. A failure that
      * is not a `Throwable` reaches the sink as a [[kyo.UI.FailureException]].
      */
    def notify(failure: Any)(using Frame): Unit < Async =
        kyo.internal.Failures.local.use(kyo.internal.Failures.notify(_, failure))

    /** Applies the `onFailure` rules around the caller to `failure`, without failing the caller: the way a feed or a poll that keeps
      * running reports a failed step. A failure the rules do not match takes `otherwise`.
      */
    def report(failure: Any, otherwise: Reaction)(using Frame): Unit < Async =
        kyo.internal.Failures.local.use(kyo.internal.Failures.handle(_, failure, otherwise, MountErrorSource.Fork))

    /** Runs `action` here and hands a failure it leaves to the `onFailure` rules around the caller, as a handler's failure would go,
      * instead of raising it: the caller goes on. For one step of several that should each run, such as one row of a batch.
      *
      * With no rule matching, the failure is notified; a rule that escalates fails the enclosing mount with
      * [[kyo.UI.MountErrorSource.Handler]].
      */
    def attempt(action: => Action)(using Frame): Unit < Async =
        kyo.internal.Failures.recover(())(Sync.defer(action).unit)

    /** Runs `action` on a fiber of its own, so the handler that calls it returns at once.
      *
      * A page runs its handlers one after another, so a handler that waits on a network round-trip holds every event behind it. This
      * starts the wait instead of awaiting it. The fiber carries the `onFailure` rules that were in effect where it started, so a
      * failure meets them as [[attempt]] would. The fiber is not tied to the element: it runs to its end, the way a request that
      * already left would.
      */
    def background(action: => Action)(using Frame): Unit < Sync =
        // The handler has no Scope to bind the fiber to, and the request it runs should finish even if the element goes.
        Fiber.initUnscoped(attempt(action)).unit

    /** What kind of update an observed [[kyo.UI.RenderEvent]] was, with the numbers only that kind has.
      *
      * The distinction is the whole point of reporting these separately: a `Repaint` renders a subtree and
      * hands the backend HTML to reconcile, while a `Channel` write is one `setAttribute` and a `Text` write
      * is one `Text.data` assignment. Counting all three as "a render" would put the expensive case and the
      * two cheap ones on the same axis, and a reader acting on that would optimize the wrong thing.
      */
    enum RenderKind derives CanEqual:
        /** A reactive region re-rendered its subtree. */
        case Repaint

        /** A keyed list emission, answered per row: `changedRows` of `totalRows` were actually repainted, the
          * rest reused wholesale (see the `changed` flag the region reports per row).
          */
        case ListPatch(changedRows: Int, totalRows: Int)

        /** One reactive attribute or class patched in place, with no region re-render at all. */
        case Channel(name: String)

        /** A `Signal[String]` bound straight to a text node, bypassing the region fiber entirely. */
        case Text

        /** A region that ran again drew something whose state the reader loses with every run, see [[kyo.UI.Loss]]. Reported once per
          * place and run, beside the repaint itself, and never for a region's first draw.
          */
        case Lost(loss: Loss)
    end RenderKind

    /** What a region that runs again takes from the reader, at the place where its content says so.
      *
      * A run draws its content again over what is on the page, and the engine keeps every element it can: focus, a caret, the text
      * typed into a field with no `value` and the tick of a box with no `checked` all survive it. Two things do not, because the
      * content itself says to start them over, and the engine reports each to a [[kyo.UI.devtools]] sink as a
      * [[kyo.UI.RenderKind.Lost]]:
      *
      *   - a native field whose `value`, `checked` or `selected` the content states as a constant: every run puts it back, so what the
      *     reader typed or ticked is gone the moment anything else in the region changes, and kept as long as nothing does. A field
      *     that follows a `Signal`, or binds a `SignalRef`, is compared after each edit instead, and is never reported;
      *   - a mount with no key: every run starts a new instance, which draws again and allocates its state anew. A keyed mount keeps
      *     its instance.
      *
      * The fix is the same for both: what the region does not have to draw again goes outside it, a field's value through a signal
      * and a mount with a key.
      *
      * @param frame
      *   where the field or the mount was written
      */
    enum Loss derives CanEqual:
        /** A native field (`input`, `textarea`, `select`, `checkbox`, `radio`, an `option`'s `selected`) states `attribute` as a
          * constant. `element` names it as a selector would: its tag, then its id or its classes.
          */
        case StatedValue(element: String, attribute: String, frame: Frame)

        /** A mount with no key. */
        case UnkeyedMount(frame: Frame)
    end Loss

    /** Why a region rendered. */
    enum RenderCause derives CanEqual:
        /** The region was just created: the initial mount, or a repaint of an enclosing region rebuilt it.
          * Its own signal has said nothing yet.
          */
        case Created

        /** The region's own signal emitted. */
        case Signal
    end RenderCause

    /** One update the engine applied, as reported to a [[kyo.UI.devtools]] sink.
      *
      * @param path
      *   the region's render path, the same identity the wire protocol addresses it by and the same string
      *   the rendered elements carry as `data-kyo-path`
      * @param regionId
      *   the id in the region's DOM comment markers (`<!--kyo-rs:ID-->`), `Absent` for an SVG region, which
      *   is addressed by `path` instead
      * @param frame
      *   the source position of the AST node that created the region: file, line, enclosing method and the
      *   source snippet. Always user code: the `Frame` macro refuses to expand inside the `kyo` package
      * @param wasted
      *   the render produced no DOM change. The most actionable number here: work was done and nothing came
      *   of it
      * @param duration
      *   walk plus paint, measured around the render itself; zero for the two cheap kinds
      * @param bytes
      *   size of the rendered payload that reached the DOM; `0` when nothing was sent or not measured
      */
    final case class RenderEvent(
        path: Seq[String],
        regionId: Maybe[String],
        frame: Frame,
        kind: RenderKind,
        cause: RenderCause,
        wasted: Boolean,
        duration: Duration,
        bytes: Int
    )

    /** Installs a sink that observes every update the engine applies, for render devtools.
      *
      * One wrapper at the root turns it on for the whole tree; no component is touched, and there is nothing
      * to opt into per component, because the engine reports from the only places an update can originate:
      *
      * {{{
      * UI.devtools(store.record)(UI.runMount(shell, "#app"))
      * }}}
      *
      * The sink rides an inheritable `Local`, so it reaches the region fibers, mount effects and channel callbacks below the
      * installation point. With no sink installed the
      * engine allocates nothing on any render path; the cost of the feature when off is a `Local` read per
      * paint.
      *
      * The same one line works around `runHandlers`, although kyo-http runs the sessions on fibers that do not
      * descend from this caller: the handlers take the sink in
      * effect where they are built and install it on every session they serve. Nothing outside the call
      * reports to the sink, so other mounts in the same process keep the free off path.
      *
      * The sink is called on the fiber that did the work and must not block: aggregate in memory and let some
      * other fiber drain it.
      */
    def devtools[A, S](sink: RenderEvent => Unit)(v: A < S)(using Frame): A < S =
        Devtools.sink.let(Present(sink))(v)

    /** Builder modifiers for [[kyo.UI.Ast.Mounted]] and [[kyo.UI.Ast.Drawn]]. */
    object MountedOps:
        extension (d: Drawn)
            /** Keeps the node's instance, and with it what its draw allocated, across re-renders of the reactive regions around it. The
              * identity is what [[kyo.UI.MountedOps.keyed]] on a waiting node says: the `.keyed` call, the key and the rows of the enclosing
              * lists, and the same identity shown twice at once is reported as a duplicate.
              */
            @scala.annotation.targetName("keyedDrawn")
            def keyed[K](key: K)(using CanEqual[K, K], MountSite): Drawn =
                given Frame = d.frame
                d.copy(key = Present(key), site = summon[MountSite], keyOrigin = KeyOrigin.Caller)
            end keyed

            /** Keeps the node's instance, and with it what its draw allocated, keyed on `made`, where the component was made, instead of on
              * a key its caller chose. What that identity is, and how calls of one site in one range are told apart by their order, is what
              * [[kyo.UI.MountedOps.keyedOnSite]] on a waiting node says.
              */
            @scala.annotation.targetName("keyedOnSiteDrawn")
            def keyedOnSite(made: MountSite)(using MountSite): Drawn =
                given Frame = d.frame
                d.copy(key = Present(made), site = summon[MountSite], keyOrigin = KeyOrigin.Site)
            end keyedOnSite

            /** Renders the node's failure: a panic in its draw, or a [[kyo.UI.whileMounted]] effect or [[kyo.UI.fork]] fiber dying once it is
              * shown. Defaults to a minimal inline error node. The failure is also logged.
              */
            @scala.annotation.targetName("onErrorDrawn")
            def onError(f: MountError => UI): Drawn =
                given Frame = d.frame
                d.copy(errorRender = Present(f))
            end onError
        end extension

        extension (m: Mounted)
            /** Keeps the node's instance (its `Scope`, resources and content) across re-renders of the reactive regions around it.
              *
              * The instance is found by its identity: the `.keyed` call, the key, and the rows of the enclosing lists, up to the
              * enclosing mount instance or the session root. It lives as long as a region of that content shows the identity, and is
              * torn down, before any new instance starts, once none does. Its position, its siblings, the regions around it and the
              * `Frame` they were built under are not part of it: it survives siblings and regions appearing or disappearing around it,
              * and a move from one region to another when one render pass covers both. The key is the caller's, so the same identity
              * shown more than once at a time, in one region or in several, is a mistake: none of its places gets an instance, and each
              * shows a [[kyo.UI.MountErrorSource.Duplicate]] error naming all of them, since only the data can say which instance belongs
              * where. Give each place a key of its own, such as the id of the data it shows. A component that keys itself on where it
              * was made uses [[kyo.UI.MountedOps.keyedOnSite]] instead, whose calls in one region are told apart by their order.
              *
              * A failure is final for the instance, and a move keeps it: a failed instance that moves shows its error view in its new
              * place, rendered by that node's [[kyo.UI.MountedOps.onError]], and its effect does not run again. This holds for an interrupt that ends the
              * effect while the move is still undecided, such as another mount's release interrupting a promise the effect awaits:
              * whether the instance moved or went away is decided once every region has seen the writes, and the interrupt is silent
              * only if the instance went away. If it moved, the interrupt is a [[kyo.UI.MountErrorSource.Panic]] failure in its new place.
              * To start the effect again, change the key: the failed instance is released and a new one starts.
              *
              * Inputs that should update a live instance belong in `Signal` parameters. A plain value the effect captures belongs in the
              * key, or the instance keeps the value it was created with.
              */
            def keyed[K](key: K)(using CanEqual[K, K], MountSite): Mounted =
                given Frame = m.frame
                m.copy(key = Present(key), site = summon[MountSite], keyOrigin = KeyOrigin.Caller)

            /** Keeps the node's instance across re-renders as [[kyo.UI.MountedOps.keyed]] does, keyed on `made`, where the component was
              * made, which it takes as a `(using UI.MountSite)` from its caller, instead of on a key the caller chose: a component made at
              * two places is two instances, and its caller writes no key.
              *
              * Nothing tells apart the calls of one site in one range (a region's value, or the content of an instance) but their order,
              * as Compose tells apart calls without a key: one call in a `map` is one site, its first call is the first instance, its
              * second the second, and so on. When the first of three calls goes away, the two left are the first and second call now, so
              * they show the first two instances, and the third instance is released: the state stays at its place and the data moves
              * through it. Calls written on different lines are different sites, so one of them going away moves nothing. Where the state
              * has to follow the data, key the node with the data's id through [[kyo.UI.MountedOps.keyed]], or render it with
              * `foreachKeyed`.
              */
            def keyedOnSite(made: MountSite)(using MountSite): Mounted =
                given Frame = m.frame
                m.copy(key = Present(made), site = summon[MountSite], keyOrigin = KeyOrigin.Site)

            /** Shown while the effect is pending on the first mount, and on every later re-mount under `PendingPolicy.Placeholder`.
              * Defaults to an empty node.
              */
            def placeholder(ui: UI): Mounted =
                given Frame = m.frame
                m.copy(placeholderUI = Present(ui))

            /** Renders the node's failure: the effect aborting or panicking, or a [[kyo.UI.fork]] fiber dying after the node published.
              * Defaults to a minimal inline error node. The failure is also logged.
              */
            def onError(f: MountError => UI): Mounted =
                given Frame = m.frame
                m.copy(errorRender = Present(f))

            /** What the node shows while a new instance is pending, see [[kyo.UI.PendingPolicy]]. Defaults to `KeepLatest`. */
            def whilePending(p: PendingPolicy): Mounted =
                given Frame = m.frame
                m.copy(pendingPolicy = p)
        end extension
    end MountedOps
    export MountedOps.*

    /** Sets how the [[kyo.UI.mounted]] nodes inside a subtree load and fail. While the mounts it catches load for the first time, including
      * the mounts in their content, the boundary shows its placeholder once, in its own place, and then reveals its content at once. A
      * failed mount counts as loaded and shows the error view in its own place. The innermost boundary catches a mount; an error view that
      * does not match a failure leaves it to the next boundary around it. A mount's own `.placeholder`, `.onError` and `.whilePending` are a
      * boundary around that mount alone.
      */
    def boundary: Boundary = Boundary(Absent, Absent, Absent)

    /** The settings of a [[kyo.UI.boundary]], applied to a subtree with `apply`. Each setter returns a copy.
      *
      *   - `.placeholder` is what makes a boundary wait. Without one it holds nothing back, and only its other settings apply. A boundary
      *     with a placeholder and no mounts inside shows its content at once.
      *   - `.whilePending` decides what a re-render that brings new mounts shows while they load: under `KeepLatest`, the default, the whole
      *     old content stays, live for reading and taking no events; under `Placeholder`, the placeholder comes back. A re-render whose mounts
      *     are all kept shows at once.
      *   - `.onError` draws a failed mount in that mount's own place, also when it fails after publishing, sits in a region below the boundary
      *     or in the content of another mount. A failure it does not match goes on to the boundary around this one, and one that nothing
      *     matches gets the built-in error node.
      *   - `.onFailure` and `.onNotice` are for what is not a mount's load: a failing event handler, [[kyo.UI.fork]] fiber or
      *     [[kyo.UI.report]], and where a `Notify` goes.
      *
      * A mount's own settings come first. A mount with its own `.placeholder` does not hold the boundary around it, and its own `.onError` wins
      * over the boundary's, which still catches its siblings. A mount that appears after the boundary revealed does not bring the placeholder
      * back: it loads in its own place and the rest stays live.
      */
    final case class Boundary(
        placeholderView: Maybe[UI],
        errorView: Maybe[PartialFunction[MountError, UI]],
        pending: Maybe[PendingPolicy],
        failureRules: Maybe[PartialFunction[Any, Reaction]] = Absent,
        noticeSink: Maybe[Throwable => Unit < Async] = Absent
    ):

        /** Shown while the mounts it catches load, in its own place. Without one the boundary does not wait. See [[whilePending]] for what a
          * later re-render shows.
          */
        def placeholder(ui: UI): Boundary = copy(placeholderView = Present(ui))

        /** Draws a failed mount in that mount's place. A failure the function does not match goes to the boundary around this one. For what
          * fails outside a mount's load, see [[onFailure]].
          */
        def onError(pf: PartialFunction[MountError, UI]): Boundary = copy(errorView = Present(pf))

        /** What a re-render bringing new mounts shows while they load, see [[kyo.UI.PendingPolicy]]. It needs a [[placeholder]] to wait at all. */
        def whilePending(p: PendingPolicy): Boundary = copy(pending = Present(p))

        /** Picks the [[kyo.UI.Reaction]] for a failure inside the boundary that nothing closer handles: an event handler's, a
          * [[kyo.UI.fork]] fiber's, a [[kyo.UI.report]]. A failure the rules do not match goes to the boundary around this one. For a mount
          * that fails, see [[onError]].
          */
        def onFailure(rules: PartialFunction[Any, Reaction]): Boundary = copy(failureRules = Present(rules))

        /** Where a `Notify` inside the boundary goes, a toast or a banner say. The innermost boundary with a sink takes it. */
        def onNotice(sink: Throwable => Unit < Async): Boundary = copy(noticeSink = Present(sink))

        def apply(ui: UI)(using Frame): UI = Ast.Bounded(this, ui, Absent)

        /** Whether the boundary holds its content back while the mounts it catches load. */
        private[kyo] def waits: Boolean = placeholderView.nonEmpty
    end Boundary

    /** Server-push: returns HTTP handlers (SSR page GET and a WebSocket route) for this UI at the given path. Compose with other handlers
      * via HttpServer.init.
      *
      * The page GET is pure SSR: it evaluates the UI, renders the initial HTML, and returns. It creates no session, forks no fibers, and
      * sets no cookie. Each WebSocket connection owns its own subscription tree for the duration of the connection; per-connection state
      * resets when the client disconnects.
      */
    def runHandlers(basePath: String)(ui: => UI < Async)(using Frame): Seq[HttpHandler[?, ?, ?]] < Sync =
        UIServer.handlers(basePath)(ui)

    /** Server-push for an app of many pages: every path under `basePath` is a page, and `ui` builds the page for its [[Request]]. The
      * served document carries `head` (title, meta, links, the app's css), and a link to another path under `basePath` is an ordinary
      * page load, so back, forward and reload land on the page of the URL.
      *
      * The GET evaluates `ui` for the request it serves. The page's client opens the session socket with its own path and query, and the
      * session evaluates `ui` again for that same request, so the SSR page and the live session agree. Paths under `basePath/_kyo` are the
      * framework's.
      */
    def runHandlers(basePath: String, head: PageHead)(ui: Request => UI < Async)(using Frame): Seq[HttpHandler[?, ?, ?]] < Sync =
        UIServer.handlers(basePath, head)(ui)

    /** The page a request-aware [[runHandlers]] is serving: `path` is relative to the base path (`/` for the base itself, never ending in
      * `/` otherwise) and `query` holds the first value of each query parameter, decoded.
      */
    final case class Request(path: String, query: Map[String, String]) derives CanEqual

    /** Scrolls the element with `id` to the start of the client viewport, smoothly, from an event handler or an effect a mount or
      * element runs on the live page. Every scroller around the element moves, the page included, so a component scrolling inside
      * itself uses [[kyo.UI.Commands.scrollIntoViewId]], which goes to the nearest edge.
      *
      * The command rides the session it runs in: under [[runHandlers]] it travels the same
      * WebSocket as the reactive updates and the connected browser scrolls the element; under a
      * browser mount (`UI.runMount`) it scrolls the local document directly. Outside any session
      * (plain SSR, [[runRender]], render-only tests) there is no client to scroll and the call is a
      * no-op.
      *
      * Fire-and-forget: an `id` with no matching element on the client is silently ignored, with no
      * round trip or retry. The target must therefore already exist on the client when the command
      * arrives; a scroll aimed at an element that only appears in a later re-render is dropped, since
      * the command does not wait for pending updates beyond the frame it lands in.
      */
    def scrollIntoView(id: String)(using Frame): Unit < Async =
        UICommands.scrollIntoView(id)

    /** An element geometry rectangle plus the current viewport size: the reply to a [[kyo.UI.Commands.requestMeasureById]].
      * `x`/`y`/`width`/`height` are the element's `getBoundingClientRect` (viewport coordinates);
      * `viewportWidth`/`viewportHeight` are `window.innerWidth`/`innerHeight`. All in CSS pixels.
      */
    final case class Rect(
        x: Double,
        y: Double,
        width: Double,
        height: Double,
        viewportWidth: Double,
        viewportHeight: Double
    ) derives CanEqual

    /** Session-scoped imperative channel provided by every live kyo-ui runner (`runMount` / `runHandlers`).
      *
      * The normal way a kyo-ui app changes the page is declarative: edit a [[kyo.Signal]], the reactive region re-renders.
      * `Commands` is the escape hatch for the two things a render diff cannot express: sending a whitelisted imperative DOM
      * command to an element ([[commandById]]: `focus`, `scrollIntoView`) and asking the client for an element's live
      * geometry ([[requestMeasureById]]). A component addresses its own elements by an id it mints with [[freshId]] and
      * stamps with `.id(...)`. Obtain it inside any event handler or `UI.mounted` effect via [[kyo.UI.commands]]; the
      * runner installs it per session.
      *
      * Both ride the same transport that carries render ops: the server emits an op, the client applies it (server-push:
      * over the WebSocket; SPA: straight to the local DOM). A measure additionally flows back as an inbound reply the
      * runner routes here, so the honest pattern is "await a measure, derive a Signal, re-render with correct geometry".
      * Example: an overlay that flips above its anchor when it would overflow the viewport: on open, measure the panel and
      * bind the returned `rect`, so that if `rect.y + rect.height > rect.viewportHeight`, it sets a `flip` Signal the panel
      * reads and re-renders from.
      *
      * A command sent from a `UI.mounted` effect, or a fiber it forked, goes out once that mount's own content is on the page,
      * behind the paints that bring it there and after any boundary holding it back has revealed it, so an effect can address the
      * element it yields. If that content never shows, the command is dropped and a measure waiting for it answers `Absent`. A
      * measure sent while the effect still runs goes out at once, since an effect that waits for it before yielding can only be
      * measuring something already on the page, and so does a command from an event handler.
      */
    final class Commands private[kyo] (
        private[kyo] val emit: internal.HtmlOp => Unit < Async,
        // Pending one-shot measure round-trips, keyed by id. `requestMeasureById` suspends on the Promise; the transport
        // completes it with the reply: the kyo-native request/reply shape where the effect RETURNS the value.
        private[kyo] val pendingById: AtomicRef[Map[String, Promise[Maybe[Rect], Any]]],
        // PERSISTENT viewport-observer signals, keyed by id. Unlike `pendingById` (one-shot: the Promise is consumed on
        // the first MeasureById reply), an entry here is KEPT across replies: each reply pushes the new Rect into the
        // SignalRef so a scroll/resize stream keeps updating it. Removed only by observeViewportById's scope finalizer.
        // An id can sit in both maps at once (an observed element measured on demand); deliverMeasureById serves both.
        private[kyo] val observers: AtomicRef[Map[String, Signal.SignalRef[Maybe[Rect]]]],
        private[kyo] val idCounter: AtomicInt
    ):
        // ---- Self-addressing: id minting + id-addressed command/measure ----

        /** Mint a session-unique element id (`"kyo-uic-N"`, N monotonically increasing from 1). Deterministic within a
          * session and unique across the page (no clock/random). The self-addressing pattern: a reusable component mints
          * an id here, stamps it on one of its own elements (`.id(theId)`: id is a settable prop), and then drives
          * [[focusId]] / [[scrollIntoViewId]] / [[requestMeasureById]] at that id.
          */
        def freshId(using Frame): String < Sync = idCounter.incrementAndGet.map(n => s"kyo-uic-$n")

        /** Send a whitelisted imperative `verb` (`"focus"`, `"scrollIntoView"`) to the element with DOM id `id` (resolved
          * client-side via `document.getElementById`). Unknown verbs are ignored by the client (forward-compatible).
          */
        def commandById(id: String, verb: String)(using Frame): Unit < Async =
            sendAbout(internal.HtmlOp.CommandById(id, verb))

        /** `commandById(id, "focus")`: move DOM focus to the element with id `id`. */
        def focusId(id: String)(using Frame): Unit < Async = commandById(id, "focus")

        /** `commandById(id, "scrollIntoView")`: scroll the element with id `id` into view (`{block:"nearest"}`). */
        def scrollIntoViewId(id: String)(using Frame): Unit < Async = commandById(id, "scrollIntoView")

        /** Measure the element with DOM id `id` and RETURN its [[kyo.UI.Rect]] (element rect + viewport), `Absent` when no
          * element on the page carries that id. Suspends until the client's reply is routed here. A second request for the
          * same id before the first reply supersedes it (last wins).
          */
        def requestMeasureById(id: String)(using Frame): Maybe[Rect] < Async =
            Promise.init[Maybe[Rect], Any].map { p =>
                pendingById.getAndUpdate(_.updated(id, p))
                    .andThen(sendMeasure(id))
                    .andThen(p.get)
            }

        /** Batched twin of [[requestMeasureById]]: measures several elements in ONE round trip, in the order of `ids`.
          *
          * The single-id form couples the request to the wait, so measuring n elements with it costs n round trips: each
          * emit is followed by a suspension that only resumes on its own reply, and the next request is not sent until it
          * does. Here every Promise is registered and every op emitted BEFORE anything is awaited, so the requests leave
          * together and the replies come back together. That is what makes geometry over a list affordable: a drag that
          * has to know where every rendered row sits pays one round trip rather than one per row.
          *
          * A repeated id is requested once and answered once, with every position in the result holding that same rect.
          */
        def requestMeasureByIds(ids: Seq[String])(using Frame): Chunk[Maybe[Rect]] < Async =
            val wanted = ids.distinct
            Kyo.foreach(wanted)(_ => Promise.init[Maybe[Rect], Any]).map { promises =>
                pendingById.getAndUpdate(_ ++ wanted.zip(promises))
                    .andThen(Kyo.foreachDiscard(wanted)(sendMeasure))
                    .andThen(Kyo.foreach(promises)(_.get))
                    .map(rects => Chunk.from(ids.map(id => rects(wanted.indexOf(id)))))
            }
        end requestMeasureByIds

        /** Transport hook: deliver `rect` for `id`. Serves BOTH maps: a PERSISTENT [[observers]] SignalRef (viewport
          * observation) is updated and KEPT (the stream continues), and a one-shot [[pendingById]] Promise is completed
          * and dropped.
          *
          * Both, not whichever comes first: an id can legitimately sit in both maps, since observing an element does not
          * stop a caller from asking what it measures right now, and the two are registered by different call sites that
          * cannot see each other. Serving only the observer left such a request unanswered, and an unanswered request
          * never returns.
          */
        private[kyo] def deliverMeasureById(id: String, rect: Rect)(using Frame): Unit < Async =
            observers.get.map { obs =>
                val observed: Unit < Sync = obs.get(id) match
                    case Some(ref) => ref.set(Present(rect))
                    case None      => ()
                observed.andThen(
                    pendingById.getAndUpdate(_.removed(id)).map { m =>
                        m.get(id) match
                            case Some(p) => p.completeDiscard(Result.succeed(Present(rect)))
                            case None    => ()
                    }
                )
            }

        /** Transport hook: no element carries `id`, so a pending [[requestMeasureById]] for it answers `Absent`. */
        private[kyo] def deliverMeasureMissing(id: String)(using Frame): Unit < Sync =
            pendingById.getAndUpdate(_.removed(id)).map { m =>
                m.get(id) match
                    case Some(p) => p.completeDiscard(Result.succeed(Absent))
                    case None    => ()
            }

        // ---- in-place reactive attribute patching + viewport observation ----

        /** Bind a class on the element with DOM id `id` to `on`: fork a mount-lifetime fiber that observes the signal and
          * emits [[internal.HtmlOp.SetClassById]]`(id, className, value)` on every emission (including the current value
          * at subscribe). The client toggles the class WITHOUT replacing the element, so CSS transitions on that class
          * fire, the whole point of this channel over a `Reactive` re-render. The fiber (and thus the subscription) is
          * bound to the current [[kyo.Scope]] and cancelled on scope close. Idempotent with any baked-in initial class.
          */
        def bindClassById(id: String, className: String, on: Signal[Boolean])(using Frame): Unit < (Async & Scope) =
            Fiber.init(on.observe(v => sendAbout(internal.HtmlOp.SetClassById(id, className, v)))).unit

        /** Bind the inline style of the element with DOM id `id` to `style`: fork a mount-lifetime fiber that observes the
          * signal and emits [[internal.HtmlOp.SetStyleById]] with the serialized declaration string on every emission
          * (including the current value at subscribe). The client MERGES the declarations over the element's existing
          * inline props (no full `style=""` clobber), so transitions fire. Cancelled on scope close.
          */
        def bindStyleById(id: String, style: Signal[Style])(using Frame): Unit < (Async & Scope) =
            Fiber.init(style.observe(v => sendAbout(internal.HtmlOp.SetStyleById(id, internal.CssStyleRenderer.render(v))))).unit

        /** Bind attribute `name` on the element with DOM id `id` to `value`, emitting [[internal.HtmlOp.SetAttrById]]
          * on every emission (current value included). The client sets the attribute WITHOUT replacing the element, so
          * it keeps its DOM position, needed for a reactive attribute (e.g. `aria-expanded`) on an element that must
          * stay a direct child for a CSS `>` child-combinator, which a `Reactive` re-render's wrapper would break.
          * Cancelled on scope close.
          */
        def bindAttrById(id: String, name: String, value: Signal[String])(using Frame): Unit < (Async & Scope) =
            Fiber.init(value.observe(v => sendAbout(internal.HtmlOp.SetAttrById(id, name, v)))).unit

        /** Continuously observe the viewport geometry of the element with DOM id `id`, RETURNING a [[kyo.Signal]] that holds
          * the latest [[kyo.UI.Rect]] (`Absent` until the first measurement lands). Registers a backing SignalRef in the
          * persistent [[observers]] map, emits [[internal.HtmlOp.ObserveViewportById]] (the client measures now and attaches
          * scroll+resize listeners that each reply with a `MeasureById(id)`), and registers a [[kyo.Scope]] finalizer that
          * emits [[internal.HtmlOp.UnobserveViewportById]] AND drops the ref. Each reply is pushed into the signal by
          * [[deliverMeasureById]] for as long as the scope is open: the kyo-native shape for a continuous stream is a
          * `Signal` the caller maps or renders, not a callback (e.g. `observeViewportById(id).map(sig => bindStyleById(id,
          * sig.map(toStyle)))`).
          */
        def observeViewportById(id: String)(using Frame): Signal[Maybe[Rect]] < (Async & Scope) =
            // The start may wait for its element (see sendAbout) and runs on a fiber of its own, so the stop must never overtake it:
            // a start still waiting never goes out, one that is out is stopped, and one caught while sending stops itself once sent.
            val unobserve = emit(internal.HtmlOp.UnobserveViewportById(id))
            for
                ref   <- Signal.initRef[Maybe[Rect]](Absent)
                phase <- AtomicInt.init(0) // 0 waiting, 1 sending, 2 out, 3 stopped
                start = phase.compareAndSet(0, 1).map { sending =>
                    if !sending then Kyo.unit
                    else
                        emit(internal.HtmlOp.ObserveViewportById(id))
                            .andThen(phase.compareAndSet(1, 2))
                            .map(out => if out then Kyo.unit else unobserve)
                }
                _ <- observers.getAndUpdate(_.updated(id, ref))
                _ <- sendWhenShown(internal.HeldCommand(start, Kyo.unit))
                _ <- Scope.ensure(
                    phase.getAndSet(3).map(was => if was == 2 then unobserve else Kyo.unit)
                        .andThen(observers.getAndUpdate(_.removed(id)))
                )
            yield ref
            end for
        end observeViewportById

        /** Sends `op`, which addresses an element by id. From a `UI.mounted` effect, or a fiber it forked, it goes out once the content
          * that mount shows is on the page, behind the paints that bring it there, and is dropped with that content if it never shows;
          * `missing` then answers it the way a page without the element would. From anywhere else it goes out at once.
          */
        private def sendAbout(op: internal.HtmlOp)(using Frame): Unit < Async =
            sendWhenShown(internal.HeldCommand(emit(op), Kyo.unit))

        /** A measure its caller waits for; dropped, it answers `Absent`. */
        private def sendMeasure(id: String)(using Frame): Unit < Async =
            sendWhenShown(internal.HeldCommand(
                emit(internal.HtmlOp.RequestMeasureById(id)),
                deliverMeasureMissing(id),
                answered = true
            ))

        private def sendWhenShown(command: internal.HeldCommand)(using Frame): Unit < Async =
            internal.MountSupervision.local.use {
                case Present(supervision) => supervision.afterShown.send(command)
                case Absent               => command.send
            }
    end Commands

    private[kyo] object Commands:
        /** Create a session channel over `emit` (the per-transport op sink: `ws.put` server-side, direct DOM apply in the SPA). */
        def init(emit: internal.HtmlOp => Unit < Async)(using Frame): Commands < Sync =
            for
                pendingById <- AtomicRef.init(Map.empty[String, Promise[Maybe[Rect], Any]])
                observers   <- AtomicRef.init(Map.empty[String, Signal.SignalRef[Maybe[Rect]]])
                idCounter   <- AtomicInt.init(0)
            yield new Commands(emit, pendingById, observers, idCounter)
    end Commands

    /** The session's [[kyo.UI.Commands]] channel (imperative commands + measure requests). Available inside any event handler
      * or `UI.mounted` effect, which the runner starts with the session's channel installed; called anywhere else it panics.
      */
    def commands(using Frame): Commands < Sync =
        UICommands.commands.use {
            case Present(commands) => commands
            case Absent            =>
                Abort.panic(UIException("UI.commands is only available inside an event handler or a UI.mounted effect of a running UI"))
        }

    /** Read-only stream of the full rendered HTML. Emits whenever any signal changes. First emission is the initial render. Useful for
      * testing, SSR, export, or custom transports.
      */
    def runRender(ui: UI)(using Frame): Stream[String, Async] =
        Stream[String, Async] {
            for initialHtml <- HtmlRenderer.render(ui, Seq.empty)
            yield Emit.valueWith(Chunk(initialHtml)) {
                Channel.use[Unit](256) { channel =>
                    val exchange =
                        new UIExchange:
                            // The stream re-renders the whole page per change, so which nodes moved is not
                            // information it can use: `previous` is ignored here on purpose.
                            def onChange(
                                region: ReactiveRegion,
                                path: Seq[String],
                                contentContext: ReactiveRegion.RegionIdentity,
                                parentContext: ReactiveRegion.ParentContext,
                                previous: Maybe[UI],
                                changedUI: UI
                            )(using Frame): Unit < Async =
                                // runPartial drops only a Closed (the consumer stopped draining); a Panic propagates.
                                Abort.runPartial[Closed](channel.put(())).unit
                            override def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async =
                                // Plain-HTML transport has no in-place patch; re-emit full HTML like onChange.
                                Abort.runPartial[Closed](channel.put(())).unit
                            override def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async =
                                Abort.runPartial[Closed](channel.put(())).unit
                            override def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async =
                                Abort.runPartial[Closed](channel.put(())).unit
                            override def onStylePatch(path: Seq[String], set: Seq[(String, String)], removed: Seq[String])(using
                                Frame
                            ): Unit < Async =
                                Abort.runPartial[Closed](channel.put(())).unit
                    // Scope.run owns the root region Fiber.init: when the stream consumer stops draining,
                    // the consume loop ends, the Scope closes, and the subscription tree cascade-tears-down.
                    Scope.run {
                        for
                            root <- ReactiveUI.normalize(ui, Seq.empty)
                            _    <- ReactiveUI.subscribe(root, exchange)
                        yield Loop.foreach {
                            // runPartial captures only the Closed failure (the channel closed when the consumer stopped
                            // draining / on teardown -> end the stream); a Panic propagates rather than being silently
                            // turned into a clean stream end.
                            Abort.runPartial[Closed](channel.take).map {
                                case Result.Success(_) =>
                                    HtmlRenderer.render(ui, Seq.empty).map { html =>
                                        Emit.valueWith(Chunk(html))(Loop.continue)
                                    }
                                case Result.Failure(_) => Loop.done
                            }
                        }
                        end for
                    }
                }
            }
        }

    /** The base CSS reset kyo-ui relies on for correct layout: `box-sizing: border-box`, the
      * flex-column / flex-row element defaults, `[data-kyo-reactive] { display: contents }`, the
      * list/heading/anchor/table normalizations, and `[hidden] { display: none }`.
      *
      * Every kyo-ui runner injects this automatically (`runMount` via the DOM stylesheet,
      * `runHandlers` and the page runner via the document `<head>`), so a normal app never names
      * it. It is public for one case: a static site that hand-builds its own HTML document and
      * wants the framework reset in its `<head>`. Emit it BEFORE any custom stylesheet so custom
      * rules win at equal specificity (CSS is last-declaration-wins). `UI.runRenderPage` already
      * does this ordering for you; reach for `UI.baseCss` directly only when you assemble the
      * document yourself.
      */
    val baseCss: String = HtmlRenderer.baseCss

    /** Configuration for the `<head>` of a full HTML document produced by [[kyo.UI.runRenderPage]].
      *
      * `title` becomes the `<title>` (attribute-escaped). `meta` is a sequence of
      * `(name, content)` pairs each emitted as `<meta name="..." content="...">` (both escaped),
      * for SEO/OpenGraph/viewport tags; the renderer always prepends `charset=utf-8` and a
      * responsive `viewport` so a minimal `PageHead` is still a valid document. `links` is a
      * sequence of `(rel, href)` pairs each emitted as `<link rel="..." href="...">` (escaped),
      * for canonical, icons, preconnect, and stylesheet links such as web fonts. `css` is extra
      * stylesheet text emitted in a `<style>` block AFTER [[kyo.UI.baseCss]] so site rules win at
      * equal specificity. `moduleScript` is an optional `<script type="module" src="...">` ESModule
      * reference (a custom Scala.js bundle); `Absent` emits no script. `jsonLd` is an optional
      * head-level data island (a `<script type="application/ld+json">` structured-data
      * block) rendered immediately before `</head>`; `Absent` (the default) emits none.
      * `dataIslands` is an ordered list of body-end data islands (`<script
      * type="application/json" id="...">` blocks) rendered immediately before `</body>`;
      * empty (the default) emits none. Both carry [[kyo.UI.DataIsland]] values built by
      * [[kyo.UI.dataIsland]]; the renderer escapes their JSON bodies so a `</script>`
      * substring cannot close the element early.
      */
    final case class PageHead(
        title: String,
        meta: Seq[(String, String)] = Seq.empty,
        links: Seq[(String, String)] = Seq.empty,
        css: String = "",
        moduleScript: Maybe[String] = Absent,
        jsonLd: Maybe[UI.DataIsland] = Absent,
        dataIslands: Seq[UI.DataIsland] = Seq.empty
    ) derives CanEqual

    /** Read-only stream of a COMPLETE HTML document (`<!DOCTYPE html>` ... `</html>`) for static-site
      * generation and SSR. Like [[kyo.UI.runRender]], the first emission is the initial render and each
      * later emission is a full re-render whenever any signal changes; unlike `runRender` (which emits
      * a body FRAGMENT), this wraps the fragment in a document with a configurable head and an
      * optional module `<script>`.
      *
      * The head is built from `head`: charset + viewport are always present, then the `meta`/`links`
      * pairs, then a single `<style>` block containing [[kyo.UI.baseCss]] strictly before `head.css` so
      * custom rules override the framework reset. The body holds the rendered UI fragment; the
      * optional module script is emitted last, before `</html>`, so the page paints before the bundle
      * loads. Take the first emission for a one-shot SSG snapshot.
      *
      * This is the SSG counterpart to [[kyo.UI.runHandlers]] (which serves a server-push page with the
      * SSE client baked in): `runRenderPage` produces a static document that links YOUR bundle, with
      * no kyo-ui server-push client injected.
      */
    def runRenderPage(head: PageHead)(ui: UI)(using Frame): Stream[String, Async] =
        runRender(ui).map(fragment => HtmlRenderer.page(head, fragment))

    /** The base CSS plus a stylesheet, ready to drop into [[kyo.UI.PageHead.css]]: returns
      * `sheet.render` (the stylesheet alone). [[kyo.UI.runRenderPage]] already prepends [[kyo.UI.baseCss]],
      * so pass `sheet.render` as `PageHead.css` directly; this helper is the named, discoverable
      * bridge from a `Stylesheet` value to the page head, parallel to how `baseCss` is the bridge for
      * the reset.
      */
    def stylesheetCss(sheet: Stylesheet)(using Frame): String = sheet.render

    /** A property value that is a constant, a signal the element follows, or a reactive [[kyo.SignalRef]] it writes back to.
      *
      * The `value`/`checked` setters store their argument as a `Bound`: `Bound.Const` for a one-shot value, `Bound.Follow` for a read-only
      * signal, `Bound.Ref` for a `SignalRef` that establishes two-way binding. Renderers and backends match on the three cases to decide
      * whether to wire input/change listeners back to the signal.
      *
      * @tparam A
      *   the type of the underlying property value
      */
    sealed trait Bound[+A]

    object Bound:
        final case class Const[A](value: A) extends Bound[A]

        /** One-way: the element shows `signal` in place, and an edit its owner did not take is put back. */
        final case class Follow[A](signal: Signal[A]) extends Bound[A]

        final case class Ref[A](ref: SignalRef[A]) extends Bound[A]

        /** A `SignalRef` binds two way, any other `Signal` one way, anything else is a constant. The choice is made on the runtime class, so
          * `ref: Signal[A]` still binds two way; `ref.readOnly` is the opt-out.
          */
        def apply[A](v: A | Signal[A]): Bound[A] = v match
            case r: SignalRef[A] @unchecked => Ref(r)
            case s: Signal[A] @unchecked    => Follow(s)
            case a: A @unchecked            => Const(a)
    end Bound

    /** The typed identity of a key in a [[kyo.UI.KeyboardEvent]].
      *
      * Named cases cover the non-printable keys (`Enter`, `Escape`, the arrows, function keys, ...). Printable keys arrive as `Char`
      * carrying the character, and anything the mapping does not recognize falls back to `Unknown` carrying the raw DOM `key` string, so no
      * keystroke is ever dropped. Use [[kyo.UI.Keyboard.fromString]] to map a browser `key` value to a `Keyboard`.
      *
      * @see
      *   [[kyo.UI.Keyboard.fromString]] for parsing a DOM `key` string
      * @see
      *   [[kyo.UI.KeyboardEvent]] for the event payload that carries it
      */
    enum Keyboard derives CanEqual:
        case Enter, Tab, Escape, Backspace, Delete, Space
        case Home, End, Insert, PageUp, PageDown
        case ArrowUp, ArrowDown, ArrowLeft, ArrowRight
        case F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12
        case Char(c: scala.Char)
        case Unknown(raw: String)

        /** The character produced by this key, present for `Char` and `Space`, `Absent` for every other case. */
        def charValue: Maybe[String] = this match
            case Char(c) => Present(c.toString)
            case Space   => Present(" ")
            case _       => Absent
    end Keyboard

    object Keyboard:
        /** Maps a DOM `KeyboardEvent.key` string to a [[kyo.UI.Keyboard]], falling back to `Char` for single characters and `Unknown` otherwise. */
        def fromString(s: String): Keyboard = s match
            case "Enter"            => Enter
            case "Tab"              => Tab
            case "Escape"           => Escape
            case "Backspace"        => Backspace
            case "Delete"           => Delete
            case " "                => Space
            case "Home"             => Home
            case "End"              => End
            case "Insert"           => Insert
            case "PageUp"           => PageUp
            case "PageDown"         => PageDown
            case "ArrowUp"          => ArrowUp
            case "ArrowDown"        => ArrowDown
            case "ArrowLeft"        => ArrowLeft
            case "ArrowRight"       => ArrowRight
            case "F1"               => F1
            case "F2"               => F2
            case "F3"               => F3
            case "F4"               => F4
            case "F5"               => F5
            case "F6"               => F6
            case "F7"               => F7
            case "F8"               => F8
            case "F9"               => F9
            case "F10"              => F10
            case "F11"              => F11
            case "F12"              => F12
            case s if s.length == 1 => Char(s.charAt(0))
            case s                  => Unknown(s)
    end Keyboard

    /** Target attribute for anchor elements.
      *
      * Each case maps to the corresponding HTML `target` attribute string value:
      *   - `Self` → `"_self"` (default browser behaviour: current frame)
      *   - `Blank` → `"_blank"` (new tab / window)
      *   - `Parent` → `"_parent"` (parent frame)
      *   - `Top` → `"_top"` (full window, escaping all frames)
      *
      * The mapping is performed by `HtmlRenderer` and is not visible to callers. PascalCase names are used deliberately to follow Scala
      * enum conventions; the HTML wire-format strings are an encoding detail.
      */
    enum Target derives CanEqual:
        case Self, Blank, Parent, Top
    end Target

    /** A typed anchor destination, replacing a stringly-typed `href` attribute.
      *
      * `Absolute` carries a parsed [[kyo.HttpUrl]], `Path` a same-origin path, `Fragment` an in-page `#id`, and `External` an arbitrary
      * scheme (`mailto`, `tel`, ...). Each case renders to the corresponding `href` string.
      */
    enum Href derives CanEqual:
        case Absolute(url: HttpUrl)
        case Path(value: String)
        case Fragment(id: String)
        case External(scheme: String, value: String)
    end Href

    object Href:
        /** The `href` string this destination renders to. The ONE definition: both the renderer and the
          * reactive `href` channel go through it, so a signal-driven href cannot drift from a static one.
          */
        private[kyo] def attrValue(h: Href): String = h match
            case Href.Absolute(url)       => url.full
            case Href.Path(p)             => p
            case Href.Fragment(id)        => s"#$id"
            case Href.External(scheme, v) => s"$scheme:$v"
    end Href

    /** A typed image source for an `img` `src` attribute.
      *
      * `Absolute` carries a parsed [[kyo.HttpUrl]], `Path` a same-origin path, and `Data` an inline `data:` URI (MIME type plus payload).
      */
    enum ImgSrc derives CanEqual:
        case Absolute(url: HttpUrl)
        case Path(value: String)
        case Data(mime: String, payload: String)
    end ImgSrc

    object ImgSrc:
        /** The `src` string this source renders to; single owner, as [[Href.attrValue]]. */
        private[kyo] def attrValue(s: ImgSrc): String = s match
            case ImgSrc.Absolute(url)       => url.full
            case ImgSrc.Path(p)             => p
            case ImgSrc.Data(mime, payload) => s"data:$mime;base64,$payload"
    end ImgSrc

    /** A supported raster/vector image format, used by [[kyo.UI.FileAccept.Image]] to build an image-extension accept filter. */
    enum ImageExt derives CanEqual:
        case Png, Jpeg, Webp, Gif, Svg, Avif
    end ImageExt

    /** A typed entry for a file input's `accept` filter, replacing the stringly-typed list of MIME types and extensions.
      *
      * The wildcard cases (`AnyImage`, `AnyVideo`, `AnyAudio`, `Pdf`) cover the common groups, `Image` builds an extension filter from a
      * known [[kyo.UI.ImageExt]], and `Extension`/`MediaType` are the explicit escapes for arbitrary extensions or MIME types. Each case
      * renders to one `accept` token.
      */
    enum FileAccept derives CanEqual:
        case AnyImage                // "image/*"
        case AnyVideo                // "video/*"
        case AnyAudio                // "audio/*"
        case Pdf                     // "application/pdf"
        case Image(ext: ImageExt)    // ".png", ".jpg", etc.
        case Extension(ext: String)  // ".csv", ".docx", explicit string escape for arbitrary extensions
        case MediaType(mime: String) // "application/foo", explicit escape for arbitrary MIME types
    end FileAccept

    /** A typed set of characters a text input accepts, replacing a stringly-typed pattern.
      *
      * `Digits` and `Decimal` name the two numeric constraints, and `Allowed` is the explicit escape for an arbitrary
      * character set. A stringly-typed pattern could not express both: a set spelled `"int"` was indistinguishable
      * from the digits-only keyword, and a typo like `"integer"` silently degraded to "allow i, n, t, e, g and r".
      *
      * The filter is applied in the browser as the reader types, so it is a typing convenience rather than a
      * validation boundary: any client can submit any value, and server-side checks still apply.
      *
      * @see
      *   `kyo.UI.ConstrainedInput.inputFilter` for the setter
      */
    enum InputFilter derives CanEqual:
        case Digits                 // "digits"
        case Decimal                // "decimal", digits plus at most one `.` or `,`
        case Allowed(chars: String) // "chars:" ++ chars, explicit escape for an arbitrary set
    end InputFilter

    /** The `type` a free-text field shows, for the field whose type follows a signal ([[kyo.UI.ConstrainedInput.textType]]).
      *
      * A field with one fixed type is built by its own factory (`UI.input`, `UI.passwordInput`, `UI.emailInput`, ...), which
      * writes that type. This enum is for a field that changes type while it stays on the page, the way a password field
      * that reveals its text does: the client switches the attribute on the element it has, so what the reader typed, the
      * caret and focus stay.
      *
      * Only the free-text types are here. A `number` field reads its value differently, so switching to or from it would
      * change what the field holds; it keeps its own factory.
      *
      * @see
      *   `kyo.UI.ConstrainedInput.textType` for the setter
      */
    enum TextType(val attr: String) derives CanEqual:
        case Text     extends TextType("text")
        case Password extends TextType("password")
        case Email    extends TextType("email")
        case Tel      extends TextType("tel")
        case Url      extends TextType("url")
        case Search   extends TextType("search")
    end TextType

    /** The ctrl/alt/shift/meta chord held down when a mouse or keyboard event fired.
      *
      * Carried by [[kyo.UI.MouseEvent]] and [[kyo.UI.KeyboardEvent]] so handlers can branch on modified clicks/keystrokes (for example
      * shift-click range selection). [[kyo.UI.Modifiers.none]] is the no-key-held value.
      */
    final case class Modifiers(
        ctrl: Boolean = false,
        alt: Boolean = false,
        shift: Boolean = false,
        meta: Boolean = false
    ) derives CanEqual, Schema

    object Modifiers:
        /** The chord with no modifier keys held. */
        val none: Modifiers = Modifiers()

    /** One selected file delivered to an `onFileSelect` handler, and the wire payload of `UIEvent.FileSelect`.
      *
      * `name` is the file name, `size` its byte length, `mimeType` the browser-reported MIME type (empty string when the
      * browser reports none), and `content` the file's text content (read via `FileReader.readAsText`). Public in the `UI`
      * object and `derives Schema` so it doubles as the on-wire representation, mirroring how [[kyo.UI.Modifiers]] is both a
      * public payload type and a wire field.
      */
    final case class FilePayload(
        name: String,
        size: Long,
        mimeType: String,
        content: String
    ) derives CanEqual, Schema

    /** A pointer position in viewport coordinates: CSS pixels from the top-left of the visible page, the frame
      * `getBoundingClientRect` and [[kyo.UI.Rect]] already speak, so a point and a measured box can be compared without a
      * conversion.
      *
      * Carried by [[kyo.UI.MouseEvent.position]] and by the drag protocol, which names it [[kyo.Drag.Point]]: one pointer
      * position, one type, whichever event delivered it.
      */
    final case class Point(x: Double, y: Double) derives CanEqual, Schema

    /** The typed payload delivered to a typed `onClick`/`onClickSelf`/`onContextMenu`/`onFocus`/`onBlur` handler.
      *
      * `targetId` is the `id` of the element the event fired on (`Absent` when that element has no id), and `modifiers` is the
      * [[kyo.UI.Modifiers]] chord at the time of the event.
      *
      * `position` is where the pointer was, in viewport coordinates. It is `Absent` for the events that have no pointer to
      * report: `focus`, `blur` and `submit` are not mouse events in the browser either and carry no coordinates, so the
      * `Maybe` says whether the event answered it rather than reporting a `(0, 0)` no one clicked. A menu that opens where
      * the reader right-clicked reads it; a handler that only wants to know that a click happened ignores it.
      *
      * `onControl` says the click passed through an element that is a control of its own on its way to this handler: an
      * anchor with an href, a button, or a form field strictly below the element being dispatched to. An element that makes
      * a whole region clickable needs it to leave the controls inside that region alone: a table row that selects on click
      * must not also select when the reader presses the row's edit button or follows a link in a cell. Without it such a
      * region cannot tell the two apart, because a control that navigates natively declares no handler and therefore cannot
      * consume the click with `stopPropagation` either.
      *
      * It is always `false` on the element the click landed on, so a control's own handler still runs.
      */
    final case class MouseEvent(
        targetId: Maybe[String],
        modifiers: Modifiers,
        position: Maybe[Point] = Absent,
        onControl: Boolean = false
    ) derives CanEqual

    /** The typed payload delivered to an `onKeyDown`/`onKeyUp` handler.
      *
      * `key` is the typed [[kyo.UI.Keyboard]] identity, `modifiers` the [[kyo.UI.Modifiers]] chord, and `targetId` the `id` of the element
      * the event fired on (`Absent` when that element has no id).
      */
    final case class KeyboardEvent(
        key: Keyboard,
        modifiers: Modifiers,
        targetId: Maybe[String]
    ) derives CanEqual

    /** The typed payload delivered to an `onScroll` handler.
      *
      * `deltaX` and `deltaY` are the wheel deltas in pixels (positive = scroll right/down). `targetId` is the `id` of the element the event
      * fired on (`Absent` when that element has no id), and `modifiers` records any held keys (ctrl-wheel zoom is the common case).
      *
      * @see
      *   [[kyo.UI.Ast.Interactive.onScroll]] for the setter that registers a handler receiving this payload.
      */
    final case class WheelEvent(
        deltaX: Double,
        deltaY: Double,
        targetId: Maybe[String],
        modifiers: Modifiers
    ) derives CanEqual

    /** The payload delivered to an `onPointerDown`/`onPointerMove`/`onPointerUp` handler, and the wire payload of the
      * corresponding `UIEvent` cases.
      *
      * `x`/`y` are the pointer position RELATIVE to the target element's top-left; `rectX`/`rectY`/`rectW`/`rectH` are the target's
      * `getBoundingClientRect` (viewport coordinates), so a drag handler can normalize the pointer into the element's 0..1 local space
      * (`(x/rectW, y/rectH)`) without a second round-trip. `buttons` is the pressed-button bitmask (1 = primary). `targetId` is the `id`
      * of the element the pointer event fired on (`Absent` when it has none). Public and `derives Schema` so it doubles as the on-wire
      * representation, mirroring how [[kyo.UI.FilePayload]] is both a public payload type and a wire field.
      *
      * On `pointerdown` over an element declaring `onPointerDown` the client calls `setPointerCapture`, so subsequent `pointermove`s
      * keep streaming to the same element even outside its bounds (a real drag session); moves are coalesced to at most one per animation
      * frame and stop on `pointerup` (which releases the capture). Only an active capture session streams moves: an idle mouse-move
      * over the page posts nothing.
      */
    final case class PointerEvent(
        x: Double,
        y: Double,
        rectX: Double,
        rectY: Double,
        rectW: Double,
        rectH: Double,
        buttons: Int,
        targetId: Maybe[String]
    ) derives CanEqual, Schema

    /** The payload delivered to an `onScrollPosition` handler: the element's resulting native scroll offset
      * (`scrollTop`/`scrollLeft`) after a scroll gesture, plus the scrolled element's `id` (`Absent` when it has
      * none). Unlike [[WheelEvent]] (a per-notch mouse-wheel delta), this reports the browser-owned scroll position
      * and fires for every scroll input (wheel, scrollbar drag, touch, keyboard).
      */
    final case class ScrollPositionEvent(
        scrollTop: Double,
        scrollLeft: Double,
        targetId: Maybe[String]
    ) derives CanEqual

    /** The case-class abstract syntax tree that every [[kyo.UI]] factory returns.
      *
      * Every node a factory builds is a value under `Ast`: the element case classes (`Div`, `Button`, `Input`, ...), the text node
      * `Text`, the reactive nodes `Reactive`/`Foreach`, and `Fragment`. Because they are plain case classes, you can pattern-match on a
      * `UI` for tests, transforms, or to write a custom rendering backend.
      *
      * @see
      *   [[kyo.UI.Ast.Element]] for the element base trait and its capability taxonomy
      */
    object Ast:

        // ---- Element base trait ----

        /** The base trait for every HTML element node, and the root of kyo-ui's capability taxonomy.
          *
          * An `Element` carries its `attrs`, its `children`, and a `Self` type fixed to its own concrete class so that every setter
          * returns the same element type and chains stay precisely typed. Setters that take a `Signal` instead return `UI`, since they
          * cross the reactive boundary.
          *
          * Capabilities an element supports are expressed as mixed-in marker/setter traits, which is how invalid HTML is made to not
          * compile (`.checked` exists only on a [[kyo.UI.Ast.BooleanInput]], `.disabled` only on a [[kyo.UI.Ast.HasDisabled]], and so
          * on):
          *
          *   - [[kyo.UI.Ast.Interactive]]: event handlers (`onClick`, `onKeyDown`, ...) and `tabIndex`/focus grouping.
          *   - [[kyo.UI.Ast.Block]] / [[kyo.UI.Ast.Inline]]: the element's layout flow category.
          *   - [[kyo.UI.Ast.Void]]: elements that cannot have children (`hr`, `input`, ...).
          *   - [[kyo.UI.Ast.Focusable]]: can receive keyboard focus.
          *   - [[kyo.UI.Ast.HasDisabled]]: exposes a `disabled` setter.
          *   - [[kyo.UI.Ast.TextInput]] / [[kyo.UI.Ast.PickerInput]] / [[kyo.UI.Ast.BooleanInput]]: the three form-input shapes (free text,
          *     picker with a `String` value, boolean `checked`).
          *   - [[kyo.UI.Ast.Activatable]] / [[kyo.UI.Ast.Clickable]]: semantic activation/click affordances (button, anchor).
          *
          * @see
          *   [[kyo.UI.Ast]] for the surrounding AST and the non-element node types
          */
        sealed trait Element extends UI:
            type Self <: Element
            private[kyo] def frame: Frame
            def attrs: Attrs
            def children: Chunk[UI]
            private[kyo] def withAttrs(a: Attrs): Self

            /** The element with `f` applied where `cond` holds, and as it is where not, so a setter that depends on a condition
              * stays in the chain: `button.when(disabled)(_.disabled(true)).when(blurs)(_.onBlur(blur))`.
              *
              * Inside a `UI.reactive` block `f` is a lambda, so a `.read` in it is refused; read into a `val` first, or pass the
              * prop to the setter, which takes it as a channel.
              */
            def when(cond: Boolean)(f: Self => Self): Self =
                // Unsafe: every element is its own `Self` (each case class fixes `type Self` to itself).
                val self = this.asInstanceOf[Self]
                if cond then f(self) else self
            end when

            /** The element with `f` applied to the value where `v` holds one, and as it is where not, so an optional setting
              * stays in the chain: `el.when(id)(_.id(_)).when(name)(_.aria("label", _))`.
              */
            def when[A](v: Maybe[A])(f: (Self, A) => Self): Self =
                // Unsafe: every element is its own `Self` (each case class fixes `type Self` to itself).
                val self = this.asInstanceOf[Self]
                v.fold(self)(a => f(self, a))
            end when

            /** The element with the setters `f` makes on a [[kyo.UI.Ast.Toggle]] applied while `cond` holds, constant or reactive
              * through one setter: `el.when(fluid)(_.cssClass("p-fluid"))`, `el.when(checked)(_.cssClass("on").aria("pressed", "true"))`.
              *
              * A `Boolean` applies them as the element's own setters would, or leaves the element as it is, so it is the bytes of
              * `when(cond: Boolean)` with the same setters (a plain `Boolean` picks that overload anyway). A `Signal[Boolean]`
              * binds each one to a channel, so a change patches the element in place and draws nothing again. That is why `f`
              * only has the setters with a channel: classes, `aria`, `data`, `role`, `hidden`, `silent`, and `disabled`,
              * `readOnly` and `required` where the element has them; a handler or a style needs `when(Boolean)` in a region.
              *
              * Inside a `UI.reactive` block a read handed to `when(Boolean)` with such setters is taken as this form's channel,
              * by the same rule as any setter that takes a `Prop`.
              */
            def when(cond: Prop[Boolean])(f: kyo.UI.Ast.Toggle[Self] => kyo.UI.Ast.Toggle[Self]): Self =
                // Unsafe: every element is its own `Self` (each case class fixes `type Self` to itself), and a toggle keeps the
                // element's own class through its setters.
                f(Toggle.of[Self](this, cond)).element.asInstanceOf[Self]
            end when

            /** Shared bodies of the declarative reactive-channel setters (aria/title/placeholder/href/src →
              * reactiveAttrs; disabled/readOnly/hidden → reactiveBoolAttrs; cssClass → reactiveClasses, or reactiveClassNames for a name).
              * `inline` so each compiles to the bare `withAttrs(attrs.copy(...))`, adding no dispatch of its own.
              *
              * WHICH SETTER GETS A CHANNEL. A `Signal`-typed setter is a channel (patching in place, returning
              * `Self`, chainable) exactly when its value maps to ONE plain HTML attribute whose written value
              * IS the live state. Everything else wraps the element in a `Reactive` and re-renders it, because
              * a channel could not express it:
              *
              *   - `style`: a [[kyo.Style]] may carry pseudo-state rules, which become a generated class plus an
              *     injected stylesheet rule rather than an inline value, and `uiStyle ++ v` merges rather than
              *     replaces. Not one attribute. Its INLINE part is a channel of its own, `inlineStyle`, which
              *     patches the declarations of the element's `style` attribute (see there).
              *   - `checked` / `selected`: the ATTRIBUTE is the control's default, the live state is the DOM
              *     property. Patching the attribute would leave a live control where it stood.
              *   - `indeterminate`: a DOM property with no attribute at all (see `applyJsProps`).
              *
              * Keep new setters on this rule.
              *
              * PRECEDENCE: where a channel exists it IS the value, in either setter order: the static field is
              * consulted by neither the renderer nor event dispatch. Not a preference: it is the only rule
              * consistent with the client, whose channel patch sets and removes the attribute on every emission
              * regardless of what the initial HTML carried.
              */
            private[kyo] inline def withReactiveAttr(name: String, sig: Signal[String]): Self =
                withAttrs(attrs.copy(reactiveAttrs = attrs.reactiveAttrs.updated(name, sig)))
            private[kyo] inline def withReactiveBoolAttr(name: String, sig: Signal[Boolean]): Self =
                withAttrs(attrs.copy(reactiveBoolAttrs = attrs.reactiveBoolAttrs.updated(name, sig)))
            private[kyo] inline def withReactiveClass(name: String, on: Signal[Boolean]): Self =
                withAttrs(attrs.copy(reactiveClasses = attrs.reactiveClasses.updated(name, on)))
            private[kyo] inline def withReactiveClassName(name: Signal[Maybe[String]]): Self =
                withAttrs(attrs.copy(reactiveClassNames = attrs.reactiveClassNames.append(name)))
            private[kyo] inline def withReactiveStyle(sig: Signal[Style]): Self =
                withAttrs(attrs.copy(reactiveStyle = Present(sig)))

            // Identity & visibility
            def id(v: String): Self = withAttrs(attrs.copy(identifier = Present(v)))

            /** Runs `effect` while this element is mounted on a live page: on the client or in the session, once the element is on the page,
              * never in a static rendering. It runs on a fiber in a `Scope` of its own, so what it acquires or forks is released with it, and a
              * failure goes to the `onFailure` rules around the element and otherwise fails the mount around it, as a [[kyo.UI.fork]] does.
              *
              * It follows the rule of an unkeyed [[kyo.UI.mounted]]: it belongs to the current run of the region around it, and the next run
              * of that region has new content, so it starts again with it, and with the values that run captured. An effect that has to
              * survive its region running again says so with [[whileMountedKeyed]]. The element's [[id]] is no key.
              */
            def whileMounted(effect: => Any < (Abort[Any] & Async & Scope))(using frame: Frame, site: UI.MountSite): Self =
                withAttrs(attrs.copy(whileMounted = attrs.whileMounted.append(Sync.defer(effect)), whileMountedSite = site))

            /** [[whileMounted]] kept across runs of the region around the element, identified as a `.keyed` mount is: by this call, `key`
              * and the keys of the rows of the lists around it, never by its place among its siblings. It survives a move and the region running
              * again, and a new key starts it again. A value the effect captures stays what it was when it started; what should reach a running
              * effect belongs in a `Signal`.
              */
            def whileMountedKeyed[K](key: K)(effect: => Any < (Abort[Any] & Async & Scope))(
                using
                CanEqual[K, K],
                Frame,
                UI.MountSite
            ): Self =
                withAttrs(attrs.copy(
                    whileMounted = attrs.whileMounted.append(Sync.defer(effect)),
                    whileMountedKey = Present(key),
                    whileMountedSite = summon[UI.MountSite]
                ))

            /** `hidden`, constant or reactive through one setter: a `Boolean` sets it statically, a `Signal[Boolean]` toggles the
              * attribute IN PLACE on emission, so showing and hiding keeps the subtree (its focus, caret, scroll position and running
              * transitions) instead of repainting it. One setter, so a read in a `UI.reactive` block passed here is a channel.
              */
            def hidden(v: Boolean | Signal[Boolean]): Self = v match
                case s: Signal[Boolean] @unchecked => withReactiveBoolAttr("hidden", s)
                case b: Boolean                    => withAttrs(attrs.copy(hidden = Present(b)))

            /** Silences the element: while it holds, the element forwards no event to the server and none of its handlers runs, so an
              * event on it goes on to the nearest ancestor that takes it, exactly as if the element declared no handler. An element is
              * also silent while it is `disabled`, so a disabled control needs nothing more; the descendants of a silent element are not
              * silenced by it.
              *
              * A `Boolean` decides it at render: a silent element writes no `data-kyo-ev` at all, the bytes of the same element with no
              * handler. A `Signal[Boolean]` writes the attribute for the signal's current value and patches it IN PLACE on emission,
              * removing it while silent and putting it back while not, with nothing drawn again. Dispatch reads the value when an event
              * arrives, so an event sent before the patch reached the page reaches nothing.
              *
              * The handlers stay on the element and run again once it is no longer silent. Where a signal is bound it is the value, in
              * either setter order, as for `disabled`. An `onAttached` hook is no event handler and still starts.
              */
            def silent(v: Boolean | Signal[Boolean]): Self = v match
                case s: Signal[Boolean] @unchecked => withAttrs(attrs.copy(silentSignal = Present(s)))
                case b: Boolean                    => withAttrs(attrs.copy(silent = b))

            // Visual
            def style(v: Style): Self               = withAttrs(attrs.copy(uiStyle = attrs.uiStyle ++ v))
            def style(f: Style.type => Style): Self = style(f(Style))

            /** Reactive `style`: re-renders when the signal emits. A pseudo-state rule (`hover`, `focus`, ...) can only follow
              * a signal this way, since it lives in a generated class that a re-render picks; for a style made of inline
              * declarations alone, [[inlineStyle]] patches them in place instead.
              */
            def style(v: Signal[Style]): Reactive[Self] =
                given Frame = frame
                Reactive[Self](v.map(s => this.style(s): UI))

            /** The element's INLINE style as a channel: the declarations of each value are patched onto the element's
              * `style` attribute in place, setting the ones the value has and removing the ones the previous value had and
              * this one lacks, so the element and everything inside it (a typed field, a mount, a running transition) stay
              * as they are. The render writes the current value inline, so an element drawn again carries it.
              *
              * Why a setter of its own rather than `style(Signal[Style])`: whether a value carries pseudo-state rules is
              * only known per emission, and those need a generated class, which only a re-render applies. This setter
              * says the style is inline declarations, which is what a measured size or offset is; pseudo-state rules in
              * its values are not applied (bind them with `style(Signal)`, or statically). A property this channel sets
              * belongs to it: the static `style` of the same property is overridden while the channel holds it, and gone
              * once the channel drops it.
              */
            def inlineStyle(v: Signal[Style]): Self = withReactiveStyle(v)

            // ARIA attributes
            /** Sets a single `aria-*` attribute (the `name` is the suffix after `aria-`), constant or reactive through one setter: a
              * `String` is a plain attribute, a `Signal[String]` patches `aria-$name` IN PLACE on emission, with no re-render (for a
              * locale-driven accessible name or a state that must change without re-mounting). One setter, so a read in a
              * `UI.reactive` block passed here is a channel and not a read of the block.
              */
            def aria(name: String, v: Prop[String]): Self = v match
                case s: Signal[String] @unchecked => withReactiveAttr(s"aria-$name", s)
                case t: String                    => withAttrs(attrs.copy(ariaAttrs = attrs.ariaAttrs.updated(name, t)))

            /** `aria-*` from an optional prop: `Present` sets it as [[aria]] does, constant or reactive, and `Absent` sets nothing, so
              * an optional accessible name stays in the chain: `el.aria("label", access.name)`.
              */
            @targetName("ariaMaybe")
            def aria(name: String, v: Maybe[Prop[String]]): Self =
                // Unsafe: every element is its own `Self` (each case class fixes `type Self` to itself).
                v.fold(this.asInstanceOf[Self])(aria(name, _))

            /** An `aria-*` state that is said only while it holds: `aria-$name="true"` while `on`, and no attribute while not, where
              * the absent attribute and `"false"` mean the same (`aria-disabled`, `aria-busy`, `aria-invalid`). A `Boolean` writes it
              * or leaves it out; a `Signal[Boolean]` sets and removes it in place on emission, with no re-render. Where `"false"`
              * says something of its own (`aria-pressed`, `aria-expanded`), use [[aria]] with the value.
              */
            def ariaFlag(name: String, on: Boolean | Signal[Boolean]): Self = on match
                case b: Boolean =>
                    if b then aria(name, "true") else withAttrs(attrs.copy(ariaAttrs = attrs.ariaAttrs - name))
                case s: Signal[Boolean] @unchecked =>
                    given CanEqual[Maybe[String], Maybe[String]] = CanEqual.derived
                    val said: Signal[Maybe[String]]              = s.map(b => if b then Present("true") else Absent)(using summon, frame)
                    withAttrs(attrs.copy(
                        ariaAttrs = attrs.ariaAttrs - name,
                        reactiveMaybeAttrs = attrs.reactiveMaybeAttrs.updated(s"aria-$name", said)
                    ))

            /** Sets several `aria-*` attributes at once from name/value pairs. */
            def aria(pairs: (String, String)*): Self =
                withAttrs(attrs.copy(ariaAttrs = attrs.ariaAttrs ++ pairs))

            /** The native `title` (tooltip), constant or reactive through one setter: a constant is the element's `title`, and a
              * signal is the `title` attribute's channel, patched in place on emission with no re-render.
              */
            def title(v: Prop[String]): Self = v match
                case s: Signal[String] @unchecked => withReactiveAttr("title", s)
                case t: String                    => jsProp("title", t)

            /** `title` from an optional prop: `Present` sets it as [[title]] does, constant or reactive, and `Absent` sets nothing,
              * so an optional tooltip stays in the chain: `el.title(tooltip)`.
              */
            @targetName("titleMaybe")
            def title(v: Maybe[Prop[String]]): Self =
                // Unsafe: every element is its own `Self` (each case class fixes `type Self` to itself).
                v.fold(this.asInstanceOf[Self])(title(_))

            /** Sets the WAI-ARIA `role` attribute (emitted as the bare `role="..."` HTML attribute). */
            def role(v: String): Self =
                withAttrs(attrs.copy(role = Present(v)))

            // Data attributes (kyo- prefix is reserved)
            /** Sets a single `data-*` attribute (the `name` is the suffix after `data-`).
              *
              * @throws java.lang.IllegalArgumentException
              *   if `name` starts with `kyo-`; that prefix is reserved by the framework
              */
            def data(name: String, value: String): Self =
                if name.startsWith("kyo-") then
                    throw new IllegalArgumentException(
                        s"data-* names must not start with 'kyo-'; the kyo- prefix is reserved by the framework. Got: kyo-${name.stripPrefix("kyo-")}"
                    )
                end if
                withAttrs(attrs.copy(dataAttrs = attrs.dataAttrs.updated(name, value)))
            end data

            /** Sets several `data-*` attributes at once from name/value pairs.
              *
              * @throws java.lang.IllegalArgumentException
              *   if any name starts with `kyo-`; that prefix is reserved by the framework
              */
            def data(pairs: (String, String)*): Self =
                pairs.foreach { case (n, _) =>
                    if n.startsWith("kyo-") then
                        throw new IllegalArgumentException(
                            s"data-* names must not start with 'kyo-'; the kyo- prefix is reserved by the framework. Got: kyo-${n.stripPrefix("kyo-")}"
                        )
                }
                withAttrs(attrs.copy(dataAttrs = attrs.dataAttrs ++ pairs))
            end data

            /** The element's CSS classes (emitted as `class="..."`), set through the returned [[kyo.UI.Ast.CssClass]]:
              * `el.cssClass("p-button", "p-component")` adds classes, and the other forms there add a class that follows a
              * prop. A class is the stable hook a [[kyo.Stylesheet]] class rule ([[kyo.Selector.cls]]) targets, so
              * document-level rules (`@media`, shared component styles, `:hover` against a named class) apply to it. For
              * per-element one-off styling use [[style]] instead; the two coexist on one element.
              */
            // The full path, not `CssClass[Self]`: seen from an element declared outside `Ast` (the `Svg` ones), the short form
            // types as `Ast.this.CssClass`, and the compiler then finds no extension `apply` on it (scalac 3.9.0).
            def cssClass: kyo.UI.Ast.CssClass[Self] = this

            /** The element with `name` added after its classes: the body of `cssClass(name)`. */
            private[kyo] def addClass(name: String): Self = withAttrs(attrs.copy(cssClasses = attrs.cssClasses :+ name))

            /** The element with `names` added after its classes, in order: the body of `cssClass(names*)`. */
            private[kyo] def addClasses(names: Seq[String]): Self = withAttrs(attrs.copy(cssClasses = attrs.cssClasses ++ names))

            /** Declarative ENTER transition (client-local): when a patch inserts this element newly into the DOM (its
              * `data-kyo-path` was not present before, or at initial mount), the client adds the space-separated `classes`
              * after insertion, forces a reflow, then removes them next frame, so a CSS transition runs from the enter-from
              * state the classes describe. An echo re-render of an already-present element does not replay. Emits
              * `data-kyo-enter="..."`.
              */
            def enterTransition(classes: String): Self = withAttrs(attrs.copy(enterTransition = Present(classes)))

            /** Declarative LEAVE transition (client-local): when a Replace/Remove removes this element, the client captures
              * its `getBoundingClientRect` before applying the patch (nothing crosses the wire), then appends a visual GHOST
              * (deep clone, `position:fixed` at the captured rect, `pointer-events:none`) to `document.body` with these
              * classes added next frame; the ghost is removed on `transitionend`/`animationend` or a 1s safety timeout. Only
              * the outermost removed leave-element per patch spawns a ghost, and only when the element actually left the
              * document: a subtree the patch preserved keeps its live node and never plays a leave. A mount cell's OWN
              * republish never ghosts either, since the engine re-running a mount effect because an enclosing region
              * repainted is bookkeeping, not leaving; the leave plays when an ENCLOSING patch removes the content (the
              * gate closing). The ghost is visual-only: its identifying `data-kyo-*`/`id` attributes are stripped from
              * the clone. Emits `data-kyo-leave="..."`.
              */
            def leaveTransition(classes: String): Self = withAttrs(attrs.copy(leaveTransition = Present(classes)))

            /** The body of `cssClass(name, on)`: a `Boolean` adds `name` or leaves the element as it is, a signal binds it to the
              * element's class channel.
              */
            private[kyo] def toggleClass(name: String, on: Boolean | Signal[Boolean]): Self = on match
                case b: Boolean                    => if b then addClass(name) else withAttrs(attrs)
                case s: Signal[Boolean] @unchecked => withReactiveClass(name, s)

            /** The body of `cssClass.matching`: a constant adds the class its case names (or none), a signal binds the
              * element's class-name channel to the class of the case it holds.
              */
            private[kyo] def matchClass[A](v: Prop[A], f: A => String | Maybe[String]): Self = v match
                case sig: Signal[A] @unchecked =>
                    given CanEqual[Maybe[String], Maybe[String]] = CanEqual.derived
                    withReactiveClassName(sig.map(a => CssClass.named(f(a)))(using summon, frame))
                case a =>
                    // Unsafe: a prop that is no signal is the value itself, and every element is its own `Self`.
                    CssClass.named(f(a.asInstanceOf[A])).fold(this.asInstanceOf[Self])(addClass)

            /** Declarative portal (client-local): after a patch inserts this element, the client physically moves it to
              * `document.body`, leaving an inert placeholder (`data-kyo-portal-slot="<path>"`, hidden) at the logical
              * position. The element keeps its `data-kyo-path`, so it stays the document's only carrier of that path:
              * server-side dispatch (which bubbles over the logical tree), every path- and id-addressed op (`bind*`,
              * `requestMeasureById`, `commandById`) and focus restore keep working unchanged. On the next re-render of an
              * enclosing region the morph matches the incoming element against the placeholder and reconciles the body
              * twin in place, so content updates flow and no inline duplicate is materialized. When the element
              * disappears from the server render, the placeholder is removed by the normal keyed reconciliation and the
              * client removes the body twin with it, playing its [[leaveTransition]] ghost if it declared one.
              *
              * Use for floating panels (overlays, menus, toasts) that must escape ancestor `overflow` clipping and
              * `transform`/`filter` containing blocks. Stacking (z-index) stays the consumer's CSS. Client-only:
              * SSR/SSG render the element inline at its logical position. Emits `data-kyo-portal="1"`.
              */
            def portal(v: Boolean): Self = withAttrs(attrs.copy(portal = Present(v)))

            // Internal JS property setter (used by Checkbox.indeterminate, etc.)
            private[kyo] def jsProp(name: String, value: String): Self =
                withAttrs(attrs.copy(jsProps = attrs.jsProps.updated(name, value)))
        end Element

        /** The class setters of an element, what [[kyo.UI.Ast.Element.cssClass]] returns: each one returns the element (`S`, its
          * own type), so a chain stays an element chain.
          *
          * {{{
          * el.cssClass("p-cascadeselect", "p-component", "p-inputwrapper")
          * el.cssClass.matching(p.size) {
          *     case Size.Small  => "p-button-sm"
          *     case Size.Large  => "p-button-lg"
          *     case Size.Normal => Absent
          * }
          * }}}
          *
          * `cssClass(names*)` adds the classes after the ones the element has, in the order given, so repeated calls space-join.
          * A name the element already carries is written again; a class list is a set to the browser, so it changes nothing.
          *
          * `matching(prop)(f)` adds the class of the case a prop holds, for a prop over a closed set of cases (a size, a
          * severity, a variant): `f` names the class of each case, or `Absent` for a case that has none. A constant writes that
          * class statically, the bytes of `cssClass(name)` or of nothing. A signal is one class-name channel: the render writes
          * the class of the case it holds, after the element's fixed classes, and a change takes the old class off and puts the
          * new one on in place, with nothing drawn again. `f` is a match over the cases, so under the build's `-Werror` a case it
          * leaves out does not compile.
          *
          * It is the element itself under another type, so `el.cssClass` allocates nothing, and each setter is a static forward
          * to the element's own method. The setters are not `inline`: `UI.reactive` reads a toggle's `cssClass` call as written
          * to rebuild it (see `Element.when`), and an inline expansion keeps no call it could read. Every setter that writes a
          * class lives here, so the class vocabulary is one place: a later setter (a prefix or a suffix over a prop, say) is a
          * method of this type rather than an overload of `cssClass` on every element.
          *
          * @tparam S
          *   the element type the setters return
          */
        opaque type CssClass[S] = Element

        object CssClass:
            extension [S](self: CssClass[S])

                /** Adds the class `name` after the element's classes. */
                def apply(name: String): S =
                    // Unsafe: `self` is the element whose `cssClass` built it, and `S` is that element's own `Self`.
                    self.addClass(name).asInstanceOf[S]

                /** Adds the classes `names` after the element's classes, in the order given. */
                def apply(names: String*): S =
                    // Unsafe: `self` is the element whose `cssClass` built it, and `S` is that element's own `Self`.
                    self.addClasses(names).asInstanceOf[S]

                /** Constant OR reactive class through one union setter. A `Boolean` is static (byte-identical to
                  * `cssClass(name)` resp. omitting it). A `Signal[Boolean]` toggles `name` IN PLACE on emission
                  * (`classList.toggle`, so CSS transitions fire) with no re-render, still chainable. Declarative twin of
                  * `UI.Commands.bindClassById`.
                  */
                def apply(name: String, on: Boolean | Signal[Boolean]): S =
                    // Unsafe: `self` is the element whose `cssClass` built it, and `S` is that element's own `Self`.
                    self.toggleClass(name, on).asInstanceOf[S]

                /** The class of the case `v` holds, as `f` names it, or none where `f` gives `Absent`. A constant is a static
                  * class; a signal is the element's class-name channel, which swaps the class in place (two `classList.toggle`s,
                  * so CSS transitions fire) with no re-render. The channel owns the names it writes: a fixed class of the same
                  * name is taken off with it.
                  */
                def matching[A](v: Prop[A])(f: A => String | Maybe[String]): S =
                    // Unsafe: `self` is the element whose `cssClass` built it, and `S` is that element's own `Self`.
                    self.matchClass(v, f).asInstanceOf[S]
            end extension

            /** The class a case names: a name is that class, a `Maybe` is what it holds. A `Present` name is the bare string at
              * run time, so it takes the first case, which gives it back as it was.
              */
            private[kyo] def named(v: String | Maybe[String]): Maybe[String] = v match
                case s: String => Present(s)
                // Unsafe: what is no string here is the `Maybe` the union names.
                case m => m.asInstanceOf[Maybe[String]]
        end CssClass

        /** What [[kyo.UI.Ast.Element.when]] over a `Prop[Boolean]` hands its function: the setters of the element that have a channel,
          * each applied while the condition holds.
          *
          * {{{
          * el.when(fluid)(_.cssClass("p-password-fluid"))
          * el.when(checked)(_.cssClass("p-togglebutton-checked").aria("pressed", "true"))
          * }}}
          *
          * Over a constant each setter is the element's own: `true` applies it, `false` leaves the element as it is. Over a signal
          * each one is bound to the channel of what it sets, and the element is patched in place as the signal moves, with
          * nothing drawn again:
          *
          *   - a class goes on while the signal holds (the class channel); one the element already carries stays on in both
          *     states, and the toggled classes are written after the element's fixed ones, sorted by name;
          *   - `aria`, `data` and `role` are the attribute's channel, the toggle's value while the signal holds and what the
          *     element had (its value, or no attribute) while it does not;
          *   - `hidden`, `disabled`, `readOnly` and `required` are the boolean attribute's channel, and `silent` the element's
          *     silence channel, with the element's own value as the off state.
          *
          * Where the element already has a channel of that name that a setter bound, the channel is the value in both states,
          * as it is for a constant (the precedence rule above `withReactiveAttr`), and the toggle sets nothing. Where an earlier
          * toggle bound it, the two compose as written: the later toggle's value while its signal holds, else the earlier's.
          *
          * Nothing else is offered, since nothing else can change in place: a handler or a style under a condition needs
          * `when(cond: Boolean)` inside a region.
          *
          * @tparam S
          *   the element type the toggle builds, which `when` hands back
          */
        final class Toggle[S] private (private[kyo] val element: Element, cond: Prop[Boolean]):

            /** The classes `names` while the condition holds. */
            def cssClass(names: String*): Toggle[S] = cond match
                case s: Signal[Boolean] @unchecked => next(names.foldLeft(element)(Toggle.classChannel(_, _, s)))
                case on: Boolean                   => if on then next(element.addClasses(names)) else this

            /** `aria-$name="value"` while the condition holds. */
            def aria(name: String, value: String): Toggle[S] =
                attr(
                    s"aria-$name",
                    Maybe.fromOption(element.attrs.ariaAttrs.get(name)),
                    value,
                    a => a.copy(ariaAttrs = a.ariaAttrs - name)
                )(
                    element.aria(name, value)
                )

            /** `data-$name="value"` while the condition holds.
              *
              * @throws java.lang.IllegalArgumentException
              *   if `name` starts with `kyo-`; that prefix is reserved by the framework
              */
            def data(name: String, value: String): Toggle[S] =
                // The element's own setter refuses a reserved name, whichever way the condition goes.
                val held = element.data(name, value)
                attr(
                    s"data-$name",
                    Maybe.fromOption(element.attrs.dataAttrs.get(name)),
                    value,
                    a => a.copy(dataAttrs = a.dataAttrs - name)
                )(held)
            end data

            /** The `role` while the condition holds. */
            def role(value: String): Toggle[S] =
                attr("role", element.attrs.role, value, _.copy(role = Absent))(element.role(value))

            /** `hidden` set to `v` while the condition holds. */
            def hidden(v: Boolean): Toggle[S] =
                flag("hidden", element.attrs.hidden.getOrElse(false), v)(element.hidden(v))

            /** The element silent (`v`) or listening while the condition holds; see [[kyo.UI.Ast.Element.silent]]. */
            def silent(v: Boolean): Toggle[S] = cond match
                case s: Signal[Boolean] @unchecked => next(Toggle.silenceChannel(element, v, s))
                case on: Boolean                   => if on then next(element.silent(v)) else this

            /** `disabled` set to `v` while the condition holds, on an element that has it. */
            def disabled(v: Boolean)(using ev: S <:< HasDisabled): Toggle[S] =
                // Unsafe: the toggle's element is its `S`, which the evidence says is a `HasDisabled`.
                val el = ev(element.asInstanceOf[S])
                flag("disabled", el.disabled.getOrElse(false), v)(el.withDisabled(v))
            end disabled

            /** `readonly` set to `v` while the condition holds, on a text field. */
            def readOnly(v: Boolean)(using ev: S <:< TextInput): Toggle[S] =
                // Unsafe: the toggle's element is its `S`, which the evidence says is a `TextInput`.
                val el = ev(element.asInstanceOf[S])
                flag("readonly", el.readOnly.getOrElse(false), v)(el.withReadOnly(v))
            end readOnly

            /** `required` set to `v` while the condition holds, on a text field. */
            def required(v: Boolean)(using ev: S <:< TextInput): Toggle[S] =
                // Unsafe: the toggle's element is its `S`, which the evidence says is a `TextInput`.
                val el = ev(element.asInstanceOf[S])
                flag("required", el.required.getOrElse(false), v)(el.withRequired(v))
            end required

            private def next(el: Element): Toggle[S] = new Toggle(el, cond)

            private def attr(name: String, off: Maybe[String], value: String, strip: Attrs => Attrs)(held: => Element): Toggle[S] =
                cond match
                    case s: Signal[Boolean] @unchecked => next(Toggle.attrChannel(element, name, off, value, strip, s))
                    case on: Boolean                   => if on then next(held) else this

            private def flag(name: String, off: Boolean, v: Boolean)(held: => Element): Toggle[S] = cond match
                case s: Signal[Boolean] @unchecked => next(Toggle.boolChannel(element, name, off, v, s))
                case on: Boolean                   => if on then next(held) else this
        end Toggle

        object Toggle:
            private[kyo] def of[S](el: Element, cond: Prop[Boolean]): Toggle[S] = new Toggle(el, cond)

            private given CanEqual[Maybe[String], Maybe[String]] = CanEqual.derived

            // Each channel below is one the toggle binds, named in `Attrs.toggled`, so a later toggle on the same name composes
            // with it; a channel some setter bound is the value, and the toggle leaves it.

            /** The class `name` on while `s` holds: nothing where the element carries it already, or-ed with a channel a toggle bound. */
            private[kyo] def classChannel(el: Element, name: String, s: Signal[Boolean]): Element =
                given Frame = el.frame
                val a       = el.attrs
                if a.cssClasses.contains(name) then el
                else
                    a.reactiveClasses.get(name) match
                        case Some(was) =>
                            el.withAttrs(a.copy(reactiveClasses =
                                a.reactiveClasses.updated(name, s.combineLatest(was).map((on, w) => on || w))
                            ))
                        case None => el.withAttrs(a.copy(reactiveClasses = a.reactiveClasses.updated(name, s)))
                end if
            end classChannel

            /** The attribute `name` at `value` while `s` holds, and `off` (what the element had) while not. */
            private[kyo] def attrChannel(
                el: Element,
                name: String,
                off: Maybe[String],
                value: String,
                strip: Attrs => Attrs,
                s: Signal[Boolean]
            ): Element =
                given Frame = el.frame
                val a       = el.attrs
                if a.toggled.contains(name) then
                    val was = a.reactiveMaybeAttrs(name)
                    el.withAttrs(a.copy(reactiveMaybeAttrs =
                        a.reactiveMaybeAttrs.updated(name, s.combineLatest(was).map((on, w) => if on then Present(value) else w))
                    ))
                else if a.reactiveAttrs.contains(name) || a.reactiveMaybeAttrs.contains(name) || off == Present(value) then el
                else
                    val stripped = strip(a)
                    el.withAttrs(stripped.copy(
                        reactiveMaybeAttrs = stripped.reactiveMaybeAttrs.updated(name, s.map(on => if on then Present(value) else off)),
                        toggled = stripped.toggled + name
                    ))
                end if
            end attrChannel

            /** The boolean attribute `name` at `v` while `s` holds, and `off` (the element's own value) while not. */
            private[kyo] def boolChannel(el: Element, name: String, off: Boolean, v: Boolean, s: Signal[Boolean]): Element =
                given Frame = el.frame
                val a       = el.attrs
                if a.toggled.contains(name) then
                    val was = a.reactiveBoolAttrs(name)
                    el.withAttrs(a.copy(reactiveBoolAttrs =
                        a.reactiveBoolAttrs.updated(name, s.combineLatest(was).map((on, w) => if on then v else w))
                    ))
                else if a.reactiveBoolAttrs.contains(name) || off == v then el
                else
                    el.withAttrs(a.copy(
                        reactiveBoolAttrs = a.reactiveBoolAttrs.updated(name, if v then s else s.map(!_)),
                        toggled = a.toggled + name
                    ))
                end if
            end boolChannel

            /** The element silent (`v`) while `s` holds, and as silent as it was while not. */
            private[kyo] def silenceChannel(el: Element, v: Boolean, s: Signal[Boolean]): Element =
                given Frame = el.frame
                val a       = el.attrs
                a.silentSignal match
                    case Present(was) if a.toggled.contains(silentKey) =>
                        el.withAttrs(a.copy(silentSignal = Present(s.combineLatest(was).map((on, w) => if on then v else w))))
                    case Present(_)              => el
                    case Absent if a.silent == v => el
                    case Absent                  =>
                        el.withAttrs(a.copy(silentSignal = Present(if v then s else s.map(!_)), toggled = a.toggled + silentKey))
                end match
            end silenceChannel

            /** The name `Attrs.toggled` keeps for the silence channel, which has no attribute of its own. */
            private val silentKey = "data-kyo-ev"
        end Toggle

        // ---- Interactive trait (event handlers + tabIndex) ----

        /** Capability trait for elements that accept event handlers (`onClick`, `onKeyDown`, ...) and `tabIndex`/focus-group settings. */
        sealed trait Interactive extends Element:
            def tabIndex(v: Int): Self       = withAttrs(attrs.copy(tabIndex = Present(v)))
            def focusTrap(v: Boolean): Self  = withAttrs(attrs.copy(focusTrap = Present(v)))
            def focusGroup(id: String): Self = withAttrs(attrs.copy(focusGroup = Present(id)))

            /** Declarative focus seeding: when a re-render inserts this element newly into the DOM (its `data-kyo-path` was
              * not present before), the client calls `.focus()` on it once; re-rendering an already-visible element (same
              * path) does not re-seed. Seeding runs after focus/caret restore, so opening a panel wins over
              * restore-to-trigger; on initial load any focus-auto element is seeded (like native autofocus). Does not make
              * the element focusable, pair with `tabIndex(-1)` for non-natively-focusable elements. The previously focused
              * element is recorded for [[focusRestore]].
              */
            def focusAuto(v: Boolean): Self = withAttrs(attrs.copy(focusAuto = Present(v)))

            /** Declarative focus return: when this focus-seeded element (see [[focusAuto]]) leaves the document, the client
              * focuses whatever was focused just before seeding (located by `data-kyo-path`, skipped silently if gone). Only
              * meaningful with `focusAuto(true)`; typical use is a modal that returns focus to its trigger on close.
              */
            def focusRestore(v: Boolean): Self = withAttrs(attrs.copy(focusRestore = Present(v)))

            /** Declarative scroll-into-view: whenever a patch leaves this element carrying the flag and the element that
              * carried it before was a different one (or none), the client scrolls it into view with `block: "nearest"`,
              * which moves the nearest scrollable ancestor by the least it can and leaves it alone when the element is
              * already visible.
              *
              * This is what a roving highlight needs and cannot get any other way. A moving DOM focus is scrolled into
              * view by the browser itself, but a highlight is a class plus `aria-activedescendant` on a container that
              * never moves, so nothing follows it: in a list capped at a few rows the highlight walks out of the panel,
              * and in a long page list it walks off the screen. Marking the highlighted row with this flag is one line
              * per host and covers the two moments that matter, the highlight moving and a panel opening onto a row that
              * was never on screen, the second of which a `scrollIntoView` command cannot: the row does not exist yet
              * when the handler runs.
              *
              * Unlike [[focusAuto]] it re-fires as the flag moves between elements, since that is the movement it
              * follows; a re-render that leaves the flag where it was scrolls nothing.
              *
              * The first paint is the one place the two scrollers differ. A flag the DOCUMENT would have to scroll to is
              * left alone, because the document's position is what the browser restores on reload and on back. A flag
              * inside a scroll container of the page's own (a list opened on its selected row, a navigation rail opened
              * on a deep link) is brought into view, and only that container moves.
              */
            def scrollAuto(v: Boolean): Self = withAttrs(attrs.copy(scrollAuto = Present(v)))

            /** Opt-in per-level event consumption: when the dispatch walk (innermost target first, then ancestors) reaches
              * this element AND it declared a handler for the event's type, the event stops here so handlers on elements
              * above do not fire. Only consumes event types this element actually handles; others pass through, and the
              * default (unset/`false`) keeps bubble-through. Applies to bubbling events (click, keydown, keyup, hover,
              * unhover, scroll). Typical use: nested overlays where Escape closes only the innermost layer.
              */
            def stopPropagation(v: Boolean): Self = withAttrs(attrs.copy(stopPropagation = Present(v)))

            /** Picks the [[kyo.UI.Reaction]] for a failure the handlers of this element, or of any element inside it, leave
              * unhandled. A failure the rules do not match goes on outward: to the elements around this one, then to the enclosing
              * boundaries' rules, and finally to `Notify`.
              */
            def onFailure(rules: PartialFunction[Any, Reaction]): Self = withAttrs(attrs.copy(onFailure = Present(rules)))

            /** Runs `action` on click, ignoring the event payload. */
            def onClick(action: => Action): Self = withAttrs(attrs.copy(onClick = Present(Sync.defer(action)(using frame))))

            /** Runs `f` on click, receiving the [[kyo.UI.MouseEvent]] payload (target id, modifier chord). */
            def onClick(f: MouseEvent => Action): Self = withAttrs(attrs.copy(onClickEvt = Present(f)))

            /** Runs `action` only when the click lands on this element itself, not on a descendant; ignores the event payload. */
            def onClickSelf(action: => Action): Self = withAttrs(attrs.copy(onClickSelf = Present(Sync.defer(action)(using frame))))

            /** Runs `f` only when the click lands on this element itself, not on a descendant; receives the [[kyo.UI.MouseEvent]] payload. */
            def onClickSelf(f: MouseEvent => Action): Self = withAttrs(attrs.copy(onClickSelfEvt = Present(f)))

            /** Runs `action` on right-click (the `contextmenu` event), ignoring the event payload. When the right-click lands on
              * this element or a descendant, the client suppresses the browser's native context menu (`preventDefault`); a
              * right-click on elements with no context-menu handler in their ancestor chain keeps the native menu.
              */
            def onContextMenu(action: => Action): Self =
                withAttrs(attrs.copy(onContextMenu = Present(Sync.defer(action)(using frame))))

            /** Runs `f` on right-click (the `contextmenu` event), receiving the [[kyo.UI.MouseEvent]] payload (target id, modifier
              * chord). Same native-menu suppression as the payload-free overload.
              */
            def onContextMenu(f: MouseEvent => Action): Self = withAttrs(attrs.copy(onContextMenuEvt = Present(f)))

            /** Runs `f` on key-down, receiving the [[kyo.UI.KeyboardEvent]] payload (typed key, modifier chord, target id). */
            def onKeyDown(f: KeyboardEvent => Action): Self = withAttrs(attrs.copy(onKeyDown = Present(f)))

            /** Runs `f` on key-down only when the key is pressed on this element itself, not on a descendant: the key a
              * control inside it receives is that control's. The keyboard twin of [[onClickSelf]], for an element that is a
              * control of its own around content that may hold others (a `role="button"` wrapper).
              */
            def onKeyDownSelf(f: KeyboardEvent => Action): Self = withAttrs(attrs.copy(onKeyDownSelf = Present(f)))
            def onKeyUp(f: KeyboardEvent => Action): Self       = withAttrs(attrs.copy(onKeyUp = Present(f)))
            def onFocus(action: => Action): Self                = withAttrs(attrs.copy(onFocus = Present(Sync.defer(action)(using frame))))
            def onFocus(f: MouseEvent => Action): Self          = withAttrs(attrs.copy(onFocusEvt = Present(f)))

            /** Runs `action` when this element loses focus, ignoring the event payload. A text field's own text on blur is
              * the two-parameter `onBlur((e, text) => ...)` (on [[kyo.UI.Ast.TextInput]]), which carries it.
              */
            def onBlur(action: => Action): Self = withAttrs(attrs.copy(onBlur = Present(Sync.defer(action)(using frame))))

            /** Runs `f` when this element loses focus, receiving the [[kyo.UI.MouseEvent]] payload (target id; no position, since
              * a blur has no pointer). A text field's own text on blur is the two-parameter `onBlur((e, text) => ...)` (on
              * [[kyo.UI.Ast.TextInput]]).
              */
            def onBlur(f: MouseEvent => Action): Self = withAttrs(attrs.copy(onBlurEvt = Present(f)))

            /** Runs `action` when the pointer enters this element. */
            def onHover(action: => Action): Self = withAttrs(attrs.copy(onHover = Present(Sync.defer(action)(using frame))))

            /** Runs `f` when the pointer enters this element, receiving the [[kyo.UI.MouseEvent]] payload. */
            def onHover(f: MouseEvent => Action): Self = withAttrs(attrs.copy(onHoverEvt = Present(f)))

            /** Runs `action` when the pointer leaves this element. */
            def onUnhover(action: => Action): Self = withAttrs(attrs.copy(onUnhover = Present(Sync.defer(action)(using frame))))

            /** Runs `f` when the pointer leaves this element, receiving the [[kyo.UI.MouseEvent]] payload. */
            def onUnhover(f: MouseEvent => Action): Self = withAttrs(attrs.copy(onUnhoverEvt = Present(f)))

            /** Runs `action` when the mouse wheel is used over this element. */
            def onScroll(action: => Action): Self = withAttrs(attrs.copy(onScroll = Present(Sync.defer(action)(using frame))))

            /** Runs `f` when the mouse wheel is used over this element, receiving the [[kyo.UI.WheelEvent]] payload. */
            def onScroll(f: WheelEvent => Action): Self = withAttrs(attrs.copy(onScrollEvt = Present(f)))

            /** Declares this element as a drag source with stable transfer metadata. */
            def dragSource(source: Drag.Source): Self = withAttrs(attrs.copy(dragSource = Present(source)))

            /** Declares this element as a drop target with stable acceptance metadata. */
            def dropTarget(target: Drag.Target): Self = withAttrs(attrs.copy(dropTarget = Present(target)))

            /** Marks this element as an activation handle for an ancestor drag source.
              *
              * Emits the reserved `data-kyo-drag-handle="true"` marker without adding presentation
              * or focus behavior. Authors remain responsible for making keyboard handles focusable.
              */
            def dragHandle: Self = withAttrs(attrs.copy(dataAttrs = attrs.dataAttrs.updated("kyo-drag-handle", "true")))

            /** Runs `action` when a drag session starts, ignoring the event payload. */
            def onDragStart(action: => Action): Self =
                withAttrs(attrs.copy(onDragStart = Present(Sync.defer(action)(using frame))))

            /** Runs `f` when a drag session starts, receiving the normalized drag event. */
            def onDragStart(f: Drag.Event => Action): Self = withAttrs(attrs.copy(onDragStartEvt = Present(f)))

            /** Runs `action` when a drag session ends, ignoring the event payload. */
            def onDragEnd(action: => Action): Self =
                withAttrs(attrs.copy(onDragEnd = Present(Sync.defer(action)(using frame))))

            /** Runs `f` when a drag session ends, receiving its final state and cancellation flag. */
            def onDragEnd(f: Drag.End => Action): Self = withAttrs(attrs.copy(onDragEndEvt = Present(f)))

            /** Runs `action` when a drag enters this target, ignoring the event payload. */
            def onDragEnter(action: => Action): Self =
                withAttrs(attrs.copy(onDragEnter = Present(Sync.defer(action)(using frame))))

            /** Runs `f` when a drag enters this target, receiving the normalized drag event. */
            def onDragEnter(f: Drag.Event => Action): Self = withAttrs(attrs.copy(onDragEnterEvt = Present(f)))

            /** Runs `action` when a drag leaves this target, ignoring the event payload. */
            def onDragLeave(action: => Action): Self =
                withAttrs(attrs.copy(onDragLeave = Present(Sync.defer(action)(using frame))))

            /** Runs `f` when a drag leaves this target, receiving the normalized drag event. */
            def onDragLeave(f: Drag.Event => Action): Self = withAttrs(attrs.copy(onDragLeaveEvt = Present(f)))

            /** Runs `action` while a drag moves over this target, ignoring the event payload. */
            def onDragOver(action: => Action): Self =
                withAttrs(attrs.copy(onDragOver = Present(Sync.defer(action)(using frame))))

            /** Runs `f` while a drag moves over this target, receiving the normalized drag event. */
            def onDragOver(f: Drag.Event => Action): Self = withAttrs(attrs.copy(onDragOverEvt = Present(f)))

            /** Runs `action` for a drop and accepts it after the action completes successfully. */
            def onDrop(action: => Action): Self =
                given Frame = frame
                withAttrs(attrs.copy(onDrop = Present(Sync.defer(action)(using frame).andThen(Drag.Decision.Accept))))

            /** Runs `f` for a drop and uses its decision as the drop result. */
            def onDrop(f: Drag.Event => Drag.Decision < (Async & Abort[Any])): Self = withAttrs(attrs.copy(onDropEvt = Present(f)))

            /** Runs `action` for a sortable move and accepts it after the action completes successfully. */
            def onSortMove(action: => Action): Self =
                given Frame = frame
                withAttrs(attrs.copy(onSortMove = Present(Sync.defer(action)(using frame).andThen(Drag.Decision.Accept))))

            /** Runs `f` for a sortable move and uses its decision as the move result. */
            def onSortMove(f: Drag.Move => Drag.Decision < (Async & Abort[Any])): Self = withAttrs(attrs.copy(onSortMoveEvt = Present(f)))

            /** Marks this element (and its subtree) as a keyboard-navigation region whose page-scrolling keys are
              * suppressed. While focus is on this element or a descendant, the client calls `preventDefault()`
              * synchronously for the navigation keys so the browser does not ALSO scroll the page; the keydown is
              * still forwarded, so this element's own `onKeyDown` (e.g. moving an option highlight) runs unchanged.
              *
              * Vertical keys (`ArrowUp`/`ArrowDown`/`PageUp`/`PageDown`) are suppressed unless the focused target
              * consumes them itself (`textarea`, `select`, contenteditable — there the browser default is caret line
              * movement or an option change, not a page scroll). A single-line input stays suppressed for vertical keys:
              * that is the combobox case where `ArrowDown` drives the listbox highlight. Horizontal/edge keys
              * (`ArrowLeft`/`ArrowRight`/`Home`/`End`) are suppressed only when the focused target is NOT a text-editable
              * field, so caret movement inside a filter input keeps working. Space is suppressed for the same reason the
              * arrows are, since a list that is one tab stop has no native control to consume it, and left alone wherever
              * it would type or activate (a text field, a `button`, a `summary`); a link does not consume it either, so
              * Space on an anchor inside the region is suppressed too.
              *
              * The rule itself is `kyo.internal.KeyPolicy`, which both clients answer from.
              *
              * Declarative by necessity: a kyo-ui handler runs asynchronously (and remotely on the server-push
              * transport), so it cannot decline the browser default in time. Both transports therefore read the emitted
              * `data-kyo-scroll-keys` attribute in the browser before posting the event — the framework's equivalent of
              * the imperative `event.preventDefault()` a component library calls in every arrow-key handler.
              */
            def preventScrollKeys: Self = withAttrs(attrs.copy(dataAttrs = attrs.dataAttrs.updated("kyo-scroll-keys", "1")))

            /** Marks this element (and its subtree) inert to its own activation: the client declines the browser's
              * default for a click and for the keys that change a native control's value, while leaving the element
              * focusable and leaving every event still forwarded.
              *
              * This is what makes a readonly control expressible. HTML's `readonly` attribute does not apply to a
              * checkbox, a radio or a button, so a library that wants "focusable, reads its value, refuses to change
              * it" has two options: mark it natively `disabled`, which takes it out of the tab order and makes
              * readonly indistinguishable from disabled to a keyboard or a screen reader, or decline the default. A
              * kyo handler cannot decline it: it runs asynchronously, and remotely on the server-push transport, so
              * the browser has already toggled the box by the time it is asked. Declaring it here moves the decision
              * to the client, which is the same reason [[preventScrollKeys]] is declarative.
              *
              * Space and the four arrows are suppressed (a checkbox toggles, a radio group walks, a range's thumb
              * moves); Enter is not, because it changes no native value and submitting the form around a readonly
              * field is still the reader's to do. The rule itself is `kyo.internal.KeyPolicy`, which both clients
              * answer from.
              */
            def preventActivation: Self = withAttrs(attrs.copy(dataAttrs = attrs.dataAttrs.updated("kyo-inert", "1")))

            /** Runs `f` on pointer-down over this element, receiving the [[kyo.UI.PointerEvent]] payload (local x/y, target rect,
              * button mask). Declaring this starts a drag session: the client calls `setPointerCapture` on pointer-down, so the
              * subsequent [[onPointerMove]] stream keeps flowing even when the pointer leaves the element, until [[onPointerUp]]
              * releases the capture. Emits the `pointerdown` token in `data-kyo-ev`; bubbles like `onClick`.
              */
            def onPointerDown(f: PointerEvent => Action): Self = withAttrs(attrs.copy(onPointerDown = Present(f)))

            /** Runs `f` on each pointer-move during an active drag session started by a pointer-down on a declaring element,
              * receiving the [[kyo.UI.PointerEvent]] payload. Moves are rAF-coalesced (at most one per animation frame, latest
              * coordinates win), so a rapid drag delivers fewer handler calls than raw DOM move events. Emits the `pointermove` token.
              */
            def onPointerMove(f: PointerEvent => Action): Self = withAttrs(attrs.copy(onPointerMove = Present(f)))

            /** Runs `f` on pointer-up ending a drag session, receiving the [[kyo.UI.PointerEvent]] payload. The client releases the
              * pointer capture before posting. Emits the `pointerup` token; bubbles like `onClick`.
              */
            def onPointerUp(f: PointerEvent => Action): Self = withAttrs(attrs.copy(onPointerUp = Present(f)))

            /** Runs `f` when files are selected on a descendant file input, receiving the full list of
              * [[kyo.UI.FilePayload]] (name, size, MIME type, and text content of every selected file). The client reads all
              * selected files (a `FileReader` per file, aggregated once every read completes) and posts a single `FileSelect`
              * event; it bubbles like `Change`. Declaring this emits the `fileselect` token in `data-kyo-ev`. A file input
              * with only `onChange` still receives the text content of one file.
              */
            def onFileSelect(f: Seq[FilePayload] => Action): Self = withAttrs(attrs.copy(onFileSelect = Present(f)))

            /** Runs `f` when this element is natively scrolled (a real `overflow:auto`/`scroll` viewport), receiving the
              * [[kyo.UI.ScrollPositionEvent]] payload (`scrollTop`/`scrollLeft`). Unlike [[onScroll]] (a per-notch wheel
              * delta), this fires for all scroll input (wheel, scrollbar drag, touch, keyboard) because the browser owns
              * the scroll and reports the resulting position; the client rAF-coalesces bursts to one event per frame.
              * Intended for server-authoritative virtual scrolling: the browser scrolls a full-height spacer natively and
              * the server repositions the rendered window from the reported `scrollTop`. Emits the `scroll` token in
              * `data-kyo-ev`.
              */
            def onScrollPosition(f: ScrollPositionEvent => Action): Self = withAttrs(attrs.copy(onScrollPos = Present(f)))
        end Interactive

        // ---- Layout traits ----

        /** Content-model marker for elements that are valid HTML children: all `Block` and `Inline` layout elements, `Text` nodes,
          * and the SVG root (`Svg.Root`). HTML containers restrict their `apply` overloads to this bound so that bare SVG primitives
          * (which extend `SvgElement` but NOT `HtmlContent`) are rejected at compile time.
          */
        sealed trait HtmlContent extends UI

        /** Typeclass witnessing that `T` is a valid child of an HTML container (content-model safety).
          *
          * Recursive givens cover: direct `HtmlContent` values, `Reactive[C]`/`Foreach[A,C]`/`Fragment[C]` where `C` has an
          * `AsHtmlChild` witness (so a nested `Fragment[Fragment[...]]` and a `foreach`/`when` returning a `Fragment` are
          * accepted), and union types `A | B` where both sides have witnesses. A lowest-priority
          * `AsHtmlChild[UI]` given (in `AsHtmlChildLowestPriority`) accepts bare `UI` values as children; it resolves only
          * when no higher-priority given matches, so named `HtmlContent` subtypes, wrappers, and fragments all resolve first.
          * `AsHtmlChild[Svg.Circle]` has no given (SVG primitives do not extend `HtmlContent`), so SVG nodes are rejected.
          * Each child is checked individually at the call site via the `HtmlChildVal`
          * implicit conversion, which avoids the LUB-widening that varargs would otherwise cause for heterogeneous arg lists.
          *
          * The `if cond then elem else UI.empty` idiom infers the branch union `A | Fragment[Nothing]`. The general `A | B`
          * given does not decompose a union (implicit search binds both `A` and `B` to the whole union, which then fails), so a
          * dedicated `emptyOr` given resolves the "render this element or render nothing" shape directly when it is passed as a
          * direct container child, e.g. `div(if cond then span(...) else UI.empty)`. It is placed in the lower-priority
          * [[kyo.UI.Ast.AsHtmlChildLowPriority]] base so it does not compete with the direct `Fragment` given when the type is a
          * bare `Fragment[Nothing]`. `A` still needs its own `AsHtmlChild` witness, so `Svg.Circle | Fragment[Nothing]` is rejected.
          *
          * For a reactive body the union must be pinned explicitly: `signal.render[Elem | Fragment[Nothing]](v => if v then elem
          * else UI.empty)`, because `render`'s content type `C` is inferred independently of the container and would otherwise
          * widen to `UI` (which has no witness). The simpler "show element while true, else nothing" case needs no union at all:
          * `UI.when(signal)(elem)` renders `elem` when true and `UI.empty` otherwise.
          *
          * Known limitation: the general `A | B` union given does NOT recurse through the structural givens for a
          * disjunct, so a single `fragment(...)` whose children mix a nested structural child with a plain sibling (element type
          * e.g. `Fragment[SpanElement] | SpanElement`) is rejected. The fix is to convert per-element at the container instead,
          * e.g. `div(fragment(fragment(a, b)), c)`; all realistic patterns compile this way.
          */
        trait AsHtmlChild[T]

        /** Lowest-priority `AsHtmlChild` given: allows bare `UI` values as children. This is the fallback
          * for callers whose helpers return `UI` (rather than a specific `HtmlContent` subtype); the
          * typed givens in `AsHtmlChild` and `AsHtmlChildLowPriority` take precedence for all named types.
          * Correctness is maintained by `HtmlRenderer`, which handles every `UI` variant at runtime.
          */
        sealed trait AsHtmlChildLowestPriority:
            given AsHtmlChild_UI: AsHtmlChild[UI] = new AsHtmlChild[UI] {}

        /** Lower-priority `AsHtmlChild` givens. Holds the `emptyOr` given for the `A | Fragment[Nothing]` branch union so it does
          * not create an ambiguity with the direct `Fragment` given when the static type is a bare `Fragment[Nothing]`.
          */
        sealed trait AsHtmlChildLowPriority extends AsHtmlChildLowestPriority:
            given emptyOr[A <: UI](using AsHtmlChild[A]): AsHtmlChild[A | Fragment[Nothing]] = new AsHtmlChild[A | Fragment[Nothing]] {}
            // RawHtml extends UI (not HtmlContent); given here at low priority so the
            // HtmlContent-based given in AsHtmlChild takes precedence for HtmlContent subtypes.
            given AsHtmlChild_RawHtml: AsHtmlChild[RawHtml] = new AsHtmlChild[RawHtml] {}
        end AsHtmlChildLowPriority

        object AsHtmlChild extends AsHtmlChildLowPriority:
            given [T <: HtmlContent]: AsHtmlChild[T]                               = new AsHtmlChild[T] {}
            given [C <: UI](using AsHtmlChild[C]): AsHtmlChild[Reactive[C]]        = new AsHtmlChild[Reactive[C]] {}
            given [A, C <: UI](using AsHtmlChild[C]): AsHtmlChild[Foreach[A, C]]   = new AsHtmlChild[Foreach[A, C]] {}
            given [C <: UI](using AsHtmlChild[C]): AsHtmlChild[Fragment[C]]        = new AsHtmlChild[Fragment[C]] {}
            given [A, B](using AsHtmlChild[A], AsHtmlChild[B]): AsHtmlChild[A | B] = new AsHtmlChild[A | B] {}
        end AsHtmlChild

        /** Witness that a value has passed the `AsHtmlChild` check, wrapping the underlying `UI` value. Because the wrapper is
          * stored in the `HtmlChildVal*` vararg array (a generic position), the `AnyVal` is boxed there, so each argument
          * allocates one short-lived wrapper at the call site. The wrappers are unwrapped immediately into the container's
          * `Chunk[UI]` children and not retained, so this is a per-argument allocation on the cold construction path, not on
          * the hot render path. HTML containers accept `HtmlChildVal*` so that each argument is checked individually at the
          * call site rather than via a single inferred `T`, which avoids LUB-widening issues for heterogeneous lists like
          * `(Button, Reactive[Span])`.
          */
        final class HtmlChildVal private[kyo] (private[kyo] val value: UI) extends AnyVal

        object HtmlChildVal:
            /** Implicit conversion: any value `T` with a `AsHtmlChild[T]` witness becomes a `HtmlChildVal`. */
            implicit def lift[T <: UI](t: T)(using AsHtmlChild[T]): HtmlChildVal = new HtmlChildVal(t)

            /** Direct implicit conversion from `String`: bridges the `stringToUI` auto-lift so that bare string literals
              * can be passed directly to HTML container `apply` overloads without a double-implicit chain.
              */
            implicit def liftString(s: String)(using Frame): HtmlChildVal = new HtmlChildVal(Text(s))

            /** Direct implicit conversion from a `Signal[A]` where A is an HTML-valid UI type: bridges the
              * `signalUIToUI`/`signalStringToUI` auto-lifts without requiring a double-implicit chain.
              * Checks that `AsHtmlChild[A]` exists so bare `Signal[SvgElement]` is still rejected.
              */
            implicit def liftSignal[A <: UI](sig: Signal[A])(using Frame, AsHtmlChild[A]): HtmlChildVal =
                new HtmlChildVal(Reactive[A](sig.map(x => x: UI)))

            /** Direct implicit conversion from a `Signal[String]` (including `SignalRef[String]`): produces a reactive
              * text node so string signals can be passed directly to HTML container `apply` overloads.
              */
            implicit def liftSignalString(sig: Signal[String])(using Frame): HtmlChildVal =
                new HtmlChildVal(Reactive[Text](sig.map(s => Text(s)), Present(Reactive.Source.Lifted(sig))))

            /** A text [[kyo.UI.Prop]] as a child: a constant is a text node, a signal a reactive one, as `liftString` and
              * `liftSignalString` make them. A plain `String` or `Signal[String]` still takes those, which are more specific.
              */
            implicit def liftPropString(p: Prop[String])(using Frame): HtmlChildVal = p match
                case s: Signal[?] => liftSignalString(s.asInstanceOf[Signal[String]])
                case s            => liftString(s.asInstanceOf[String])

            /** A [[kyo.UI.Prop]] of an element type as a child: a constant is the element, a signal a reactive node, as `lift` and
              * `liftSignal` make them.
              */
            implicit def liftPropUI[A <: UI](p: Prop[A])(using Frame, AsHtmlChild[A]): HtmlChildVal = p match
                case s: Signal[?] => liftSignal(s.asInstanceOf[Signal[A]])
                case a            => new HtmlChildVal(a.asInstanceOf[A])
        end HtmlChildVal

        /** Layout marker for block-flow elements (`div`, `p`, headings, ...). */
        sealed trait Block extends Element with HtmlContent

        /** Layout marker for inline-flow elements (`span`, `a`, inputs, ...). */
        sealed trait Inline extends Element with HtmlContent

        // ---- SVG cross-file extension bridge ----

        /** Sanctioned cross-file extension point for the SVG AST (defined in `Svg.scala`).
          *
          * `Element`, `Interactive`, `Block`, `Inline`, and `HtmlContent` are `sealed`, so the HTML
          * AST stays closed and exhaustive over `UI`. SVG nodes live in a separate file, so they
          * cannot extend a sealed trait directly; `SvgNode`, `SvgInteractiveNode`, and `SvgRootNode`
          * are the only non-sealed descendants and form the bridge. `Svg.SvgElement` extends `SvgNode`, which makes every SVG
          * element an `Element` (so it reuses the path/event/reactive engine) without making it
          * `HtmlContent`: a bare SVG primitive is therefore not a valid HTML child (`div(Svg.circle)`
          * does not compile).
          */
        trait SvgNode extends Element

        /** Bridge for SVG nodes that also accept event handlers (`SvgNode` plus `Interactive`).
          * SVG elements that carry events mix this in instead of `Interactive` directly, since
          * `Interactive` is sealed.
          */
        trait SvgInteractiveNode extends SvgNode with Interactive

        /** Bridge for the `<svg>` root only: an `SvgInteractiveNode` that is also `Inline`
          * `HtmlContent`, so `Svg.Root` (and only the root) embeds in an HTML container
          * (`div(Svg.svg(...))` compiles; `div(Svg.circle(...))` does not).
          */
        trait SvgRootNode extends SvgInteractiveNode with Inline with HtmlContent

        // ---- Void trait (elements that cannot have children) ----

        /** Capability trait for void elements that cannot have children (`hr`, `br`, `input`, ...); fixes `children` to empty. */
        sealed trait Void extends Element:
            final def children: Chunk[UI] = Chunk.empty

        // ---- Capability traits ----

        /** Capability marker for elements that can receive keyboard focus. */
        sealed trait Focusable extends Element

        /** Capability trait for elements exposing a `disabled` setter: one setter over `Boolean | Signal[Boolean]`, so a read in a `UI.reactive` block passed to it is a channel. */
        sealed trait HasDisabled extends Element:
            def disabled: Maybe[Boolean]
            private[kyo] def withDisabled(v: Boolean): Self

            /** Constant OR reactive disabled through one union setter. A `Boolean` sets it statically; a
              * `Signal[Boolean]` toggles `disabled` IN PLACE on emission (setAttribute("")/removeAttribute)
              * via the boolean-attribute channel with no re-render, still chainable.
              */
            def disabled(v: Boolean | Signal[Boolean]): Self = v match
                case b: Boolean                    => withDisabled(b)
                case s: Signal[Boolean] @unchecked => withReactiveBoolAttr("disabled", s)
        end HasDisabled

        /** Capability trait for free-text form inputs: a `String` `value` (constant or `SignalRef`), placeholder, read-only, `onInput`/`onChange`, and an `onBlur` that carries the text. */
        sealed trait TextInput extends Focusable with HasDisabled:
            def value: Maybe[Bound[String]]
            def placeholder: Maybe[String]
            def readOnly: Maybe[Boolean]
            private[kyo] def withReadOnly(v: Boolean): Self
            def required: Maybe[Boolean]
            private[kyo] def withRequired(v: Boolean): Self
            def onInput: Maybe[String => Action]
            def onChange: Maybe[String => Action]

            /** The handler the two-parameter `onBlur` stores. */
            def onBlurText: Maybe[(MouseEvent, String) => Action]

            /** Runs `f` when the field loses focus, with the [[kyo.UI.MouseEvent]] payload and the text the field holds at that
              * moment: what the reader typed, whether or not anything binds the field, the way `onInput` and `onChange` carry
              * it. The client reads the text off the element as focus leaves, so an unbound field and one started from a
              * constant report the reader's edit rather than the text the page was built with.
              *
              * An overload of the element's `onBlur` told apart by the lambda's arity: `onBlur(e => ...)` is the
              * [[kyo.UI.MouseEvent]] form and `onBlur((e, text) => ...)` this one, with no type ascription needed. It runs
              * after the payload-free and the [[kyo.UI.MouseEvent]] forms. A transport that has no text to send for the blur
              * reports the field's `SignalRef` value, else its constant, else the empty text.
              */
            def onBlur(f: (MouseEvent, String) => Action): Self

            /** Puts the class `name` on this field while it holds text, and takes it off while it is empty, in the client:
              * as the reader types, as a patch writes the field's value, and after every patch. Nothing goes to the server
              * and nothing renders again, so the class follows the text the browser keeps for a field nothing binds, and
              * the reader's edit of a field started from a constant, which no render knows about.
              *
              * The static render carries the class already when the field's starting text (its constant, or what its
              * `SignalRef` holds) is not empty, so a page shown before the client attaches is right. Emits
              * `data-kyo-filled`, which both clients read through `kyo.internal.FilledClass`.
              *
              * Declarative for the reason [[preventScrollKeys]] is: a handler would learn of the text only after a round
              * trip, and of an unbound field's text not at all.
              */
            def filledClass(name: String): Self =
                withAttrs(attrs.copy(dataAttrs = attrs.dataAttrs.updated(kyo.internal.FilledClass.classKey, name)))

            /** [[filledClass]] with the class put on the closest ancestor that `closest` (a CSS selector) matches, for a
              * stylesheet that marks the wrapper around a field (Prime's `.p-inputwrapper-filled`). It sits beside a class on
              * the field itself, so a field can mark both. The static render cannot reach an ancestor it has already
              * written, so the component that draws the wrapper stamps the starting class on it from the starting text; the
              * client keeps it right from there. Emits `data-kyo-filled-up` and `data-kyo-filled-on`.
              */
            def filledClass(name: String, closest: String): Self =
                withAttrs(attrs.copy(dataAttrs =
                    attrs.dataAttrs.updated(kyo.internal.FilledClass.upKey, name).updated(kyo.internal.FilledClass.targetKey, closest)
                ))

            /** A constant is the field's starting text. A read-only `Signal` is what the field shows, in place, and an edit its owner does not take is put back: at the commit, and at each keystroke when `onInput` is set. A `SignalRef` binds two way: typing writes into the ref, and ref changes update the field. */
            def value(v: String | Signal[String]): Self

            /** Runs `f` with the text the field holds after each keystroke, paste or drop, once any typing constraint has
              * filtered it. A field bound to a `SignalRef` has the ref written first.
              */
            def onInput(f: String => Action): Self

            /** Runs `f` with the text the field holds when the reader commits it: focus leaving after an edit, or Enter. A field
              * bound to a `SignalRef` has the ref written first.
              */
            def onChange(f: String => Action): Self

            private[kyo] def withPlaceholder(v: String): Self

            /** The text shown while the field is empty, constant or reactive through one setter: a `String` is the
              * `placeholder` attribute, and a `Signal[String]` is that attribute's channel, patched IN PLACE on emission with
              * no re-render of the field. One setter, so a read in a `UI.reactive` block passed here is a channel and not a
              * read of the block. Where both are set, the signal's channel is the attribute, in either order (the precedence
              * rule above `withReactiveAttr`).
              */
            def placeholder(v: Prop[String]): Self = v match
                case s: Signal[String] @unchecked => withReactiveAttr("placeholder", s)
                case t: String                    => withPlaceholder(t)

            /** `placeholder` from an optional prop: `Present` sets it as [[placeholder]] does, constant or reactive, and
              * `Absent` sets nothing, so an optional placeholder stays in the chain: `field.placeholder(hint)`.
              */
            @targetName("placeholderMaybe")
            def placeholder(v: Maybe[Prop[String]]): Self =
                // Unsafe: every element is its own `Self` (each case class fixes `type Self` to itself).
                v.fold(this.asInstanceOf[Self])(placeholder(_))

            /** Constant OR reactive readonly through one union setter: a `Boolean` sets it statically; a
              * `Signal[Boolean]` toggles the `readonly` attribute IN PLACE on emission via the boolean-attribute
              * channel: no re-render, so caret/focus survive.
              */
            def readOnly(v: Boolean | Signal[Boolean]): Self = v match
                case b: Boolean                    => withReadOnly(b)
                case s: Signal[Boolean] @unchecked => withReactiveBoolAttr("readonly", s)

            /** Native `required` (the field takes part in the form's constraint validation), constant or reactive through one union
              * setter: a `Boolean` writes the attribute statically; a `Signal[Boolean]` sets and removes `required` IN PLACE on
              * emission via the boolean-attribute channel, with no re-render, so caret and focus survive. It says nothing to
              * assistive technology beyond what the browser derives from it; a component that wants `aria-required` writes it.
              */
            def required(v: Boolean | Signal[Boolean]): Self = v match
                case b: Boolean                    => withRequired(b)
                case s: Signal[Boolean] @unchecked => withReactiveBoolAttr("required", s)
        end TextInput

        /** Capability trait for text inputs that accept declarative client-local typing constraints.
          *
          * Every free-text input mixes this in except `NumberInput`. A `number` field constrains its content through
          * `type`, `min`, `max` and `step` instead, its `value` reads as the empty string while the content is not a
          * valid number, and a mask's own literals (`(`, `)`, `-`, space) are never valid there. Leaving the trait off
          * that one element keeps the combination from compiling rather than failing at runtime.
          *
          * Both constraints are enforced in the browser as the reader types, so they are typing conveniences rather
          * than validation boundaries: any client can submit any value, and server-side checks still apply.
          *
          * A composition (an IME, a dead key, mobile autocorrect) is let through and its finished text corrected on
          * `compositionend`, rather than being cancelled mid-composition, which is what cancelling the composition
          * event would do.
          */
        sealed trait ConstrainedInput extends TextInput:
            def inputFilter: Maybe[InputFilter]
            def inputMask: Maybe[String]

            /** Restricts the characters the field accepts, rejecting the rest as they are typed, pasted or dropped.
              *
              * The rejected character never enters the field, so the reader sees no flicker and the caret does not
              * jump, and the server only observes filtered text through the normal input and change events. Emits
              * `data-kyo-filter`, read by a document-level `beforeinput` capture listener.
              *
              * @see
              *   [[kyo.UI.InputFilter]] for the available character sets
              */
            def inputFilter(v: InputFilter): Self

            /** Formats the field progressively against a fixed pattern as the reader types.
              *
              * Tokens are `9` for a digit, `a` for an ASCII letter and `*` for either; every other character is a
              * literal the mask inserts on its own, as in `"(999) 999-9999"`. A backslash escapes the next character,
              * so `"+4\\9 999"` keeps a literal `9` in the country code rather than opening an input position there.
              * Each position accepts only its own character class, backspace steps back over literals, and the caret
              * follows the insertion. Formatting stops at the first position the value cannot fill, so a partial entry
              * renders without trailing literals.
              *
              * The classes are ASCII only: `a` and `*` reject accented and non-Latin letters. The server receives the
              * formatted value through the normal input and change events. Emits `data-kyo-mask`.
              *
              * The mask governs display, not only typing: the initial value and a value bound to a [[kyo.SignalRef]]
              * are formatted as they render, so a masked field never shows an unformatted value whoever set it. A
              * value the mask cannot hold is truncated at its capacity.
              */
            def inputMask(v: String): Self

            /** The constant type set with [[textType]], in place of the factory's own; `Absent` where the factory's stands. */
            def textType: Maybe[TextType]

            private[kyo] def withTextType(v: TextType): Self

            /** The field's `type`. A constant is written where the factory writes its own, so a field of any free-text factory
              * given a constant reads as the factory of that type would draw it. A signal is patched IN PLACE on emission: the
              * client switches the attribute on the element it has, with no re-render, so the text the reader typed, the caret
              * and focus stay. The type the factory wrote gives way to it, in the static render as in the client, so the field
              * carries one `type`, at the value the signal holds.
              *
              * A signal is for a field that changes type on the page, such as a password field that reveals its text. A
              * component whose type is a prop hands it on as it is, constant or signal, and keeps one factory.
              */
            def textType(v: Prop[TextType]): Self =
                given Frame = frame
                v match
                    case sig: Signal[TextType] @unchecked => withReactiveAttr("type", sig.map(_.attr))
                    case t: TextType                      => withTextType(t)
            end textType
        end ConstrainedInput

        /** Capability trait for picker inputs that carry a `String` `value` and an `onChange` but no free-text typing (`select`, date/time/color, ...). */
        sealed trait PickerInput extends Focusable with HasDisabled:
            def value: Maybe[Bound[String]]
            def onChange: Maybe[String => Action]

        end PickerInput

        /** Capability trait for boolean inputs that carry a `checked` flag (constant or `SignalRef`) and a `Boolean` `onChange` (`checkbox`, `radio`). */
        sealed trait BooleanInput extends Focusable with HasDisabled:
            def checked: Maybe[Bound[Boolean]]
            def onChange: Maybe[Boolean => Action]

            /** A constant is fixed unless a handler takes the toggle. A read-only `Signal` is what the control shows, in place, and a toggle its owner does not take is put back. A `SignalRef` binds two way: toggling writes back into the ref, and ref changes update the control. */
            def checked(v: Boolean | Signal[Boolean]): Self
        end BooleanInput

        /** Capability marker for elements with semantic activation (button, anchor): can be triggered by keyboard as well as click. */
        sealed trait Activatable extends Element

        /** Capability marker for elements whose primary semantic is being clicked (button, anchor). */
        sealed trait Clickable extends Element

        final private[kyo] case class Attrs(
            identifier: Maybe[String] = Absent,
            hidden: Maybe[Boolean] = Absent,
            tabIndex: Maybe[Int] = Absent,
            focusTrap: Maybe[Boolean] = Absent,
            focusGroup: Maybe[String] = Absent,
            focusAuto: Maybe[Boolean] = Absent,
            focusRestore: Maybe[Boolean] = Absent,
            scrollAuto: Maybe[Boolean] = Absent,
            stopPropagation: Maybe[Boolean] = Absent,
            enterTransition: Maybe[String] = Absent,
            leaveTransition: Maybe[String] = Absent,
            // Client-local portal: the client re-homes the element to document.body behind a placeholder slot.
            portal: Maybe[Boolean] = Absent,
            uiStyle: Style = Style.empty,
            onClick: Maybe[Action] = Absent,
            onClickEvt: Maybe[MouseEvent => Action] = Absent,
            onClickSelf: Maybe[Action] = Absent,
            onClickSelfEvt: Maybe[MouseEvent => Action] = Absent,
            onContextMenu: Maybe[Action] = Absent,
            onContextMenuEvt: Maybe[MouseEvent => Action] = Absent,
            onKeyDown: Maybe[KeyboardEvent => Action] = Absent,
            onKeyDownSelf: Maybe[KeyboardEvent => Action] = Absent,
            onKeyUp: Maybe[KeyboardEvent => Action] = Absent,
            onFocus: Maybe[Action] = Absent,
            onFocusEvt: Maybe[MouseEvent => Action] = Absent,
            onBlur: Maybe[Action] = Absent,
            onBlurEvt: Maybe[MouseEvent => Action] = Absent,
            onHover: Maybe[Action] = Absent,
            onHoverEvt: Maybe[MouseEvent => Action] = Absent,
            onUnhover: Maybe[Action] = Absent,
            onUnhoverEvt: Maybe[MouseEvent => Action] = Absent,
            onScroll: Maybe[Action] = Absent,
            onScrollEvt: Maybe[WheelEvent => Action] = Absent,
            dragSource: Maybe[Drag.Source] = Absent,
            dropTarget: Maybe[Drag.Target] = Absent,
            onDragStart: Maybe[Action] = Absent,
            onDragStartEvt: Maybe[Drag.Event => Action] = Absent,
            onDragEnd: Maybe[Action] = Absent,
            onDragEndEvt: Maybe[Drag.End => Action] = Absent,
            onDragEnter: Maybe[Action] = Absent,
            onDragEnterEvt: Maybe[Drag.Event => Action] = Absent,
            onDragLeave: Maybe[Action] = Absent,
            onDragLeaveEvt: Maybe[Drag.Event => Action] = Absent,
            onDragOver: Maybe[Action] = Absent,
            onDragOverEvt: Maybe[Drag.Event => Action] = Absent,
            onDrop: Maybe[Drag.Decision < (Async & Abort[Any])] = Absent,
            onDropEvt: Maybe[Drag.Event => Drag.Decision < (Async & Abort[Any])] = Absent,
            onSortMove: Maybe[Drag.Decision < (Async & Abort[Any])] = Absent,
            onSortMoveEvt: Maybe[Drag.Move => Drag.Decision < (Async & Abort[Any])] = Absent,
            onFailure: Maybe[PartialFunction[Any, Reaction]] = Absent,
            onPointerDown: Maybe[PointerEvent => Action] = Absent,
            onPointerMove: Maybe[PointerEvent => Action] = Absent,
            onPointerUp: Maybe[PointerEvent => Action] = Absent,
            onFileSelect: Maybe[Seq[UI.FilePayload] => Action] = Absent,
            onScrollPos: Maybe[ScrollPositionEvent => Action] = Absent,
            // The browser mount's element-in-the-document hook. The element is typed `Any` here because the shared
            // AST names no DOM type; the typed setter lives in the Scala.js sources (kyo.onAttached).
            onAttached: Maybe[Any => Any < (Async & Scope & Abort[Any])] = Absent,
            // What runs while the element is on a live page, see Element.whileMounted.
            whileMounted: Chunk[Any < (Abort[Any] & Async & Scope)] = Chunk.empty,
            whileMountedKey: Maybe[Any] = Absent,
            whileMountedSite: UI.MountSite = kyo.internal.MountSites.MountSite.none,
            ariaAttrs: Map[String, String] = Map.empty,
            dataAttrs: Map[String, String] = Map.empty,
            jsProps: Map[String, String] = Map.empty,
            cssClasses: Chunk[String] = Chunk.empty,
            role: Maybe[String] = Absent,
            // Declarative reactive channels: the element renders each signal's current value at SSR, then an
            // observer (started in ReactiveUI.subscribeScoped) patches it IN PLACE on emission (no re-render).
            // reactiveAttrs: fully-qualified name -> value (aria/placeholder/title(sig)); reactiveBoolAttrs:
            // present while true (disabled/readOnly(sig)); reactiveClasses: classList.toggle (cssClass(name, sig)).
            reactiveAttrs: Map[String, Signal[String]] = Map.empty,
            reactiveBoolAttrs: Map[String, Signal[Boolean]] = Map.empty,
            // Attributes a signal may leave out (ariaFlag(sig)): the value while Present, no attribute while Absent.
            reactiveMaybeAttrs: Map[String, Signal[Maybe[String]]] = Map.empty,
            reactiveClasses: Map[String, Signal[Boolean]] = Map.empty,
            // Classes whose name a signal holds (cssClass.matching(sig)), written after the toggled ones, in the order they were set.
            reactiveClassNames: Chunk[Signal[Maybe[String]]] = Chunk.empty,
            // The inline-style channel (inlineStyle(sig)): its declarations are written inline at render and patched with
            // HtmlOp.SetStyleByPath on emission.
            reactiveStyle: Maybe[Signal[Style]] = Absent,
            // Element.silent: no event forwarded and no handler run while it holds. A signal is the channel of `data-kyo-ev` (see
            // kyo.internal.Channels.of); where it is bound the constant is not consulted.
            silent: Boolean = false,
            silentSignal: Maybe[Signal[Boolean]] = Absent,
            // The channels a `when` over a signal bound (by attribute name; the silence channel as `data-kyo-ev`), which a later
            // toggle on the same name composes with; a channel a setter bound is the value, and a toggle leaves it (UI.Ast.Toggle).
            toggled: Set[String] = Set.empty
        ):
            /** Whether any reactive channel is bound here. Every element of a render asks, so it allocates nothing; [[kyo.internal.Channels.of]] is the channels themselves. */
            def hasChannels: Boolean =
                reactiveAttrs.nonEmpty || reactiveMaybeAttrs.nonEmpty || reactiveBoolAttrs.nonEmpty || reactiveClasses.nonEmpty ||
                    reactiveClassNames.nonEmpty ||
                    reactiveStyle.nonEmpty || silentSignal.nonEmpty
        end Attrs

        // ---- Non-element AST cases ----

        /** A literal text node; a bare `String` child auto-lifts to one. */
        case class Text(value: String)(using val frame: Frame) extends UI with HtmlContent

        /** Verbatim inline HTML passthrough node. The `value` string is emitted byte-for-byte into the rendered output with no HTML
          * escaping. This is the AST counterpart to [[kyo.UI.rawHtml]].
          *
          * Security contract: `value` must come from a trusted, controlled source. There is no sanitization; passing user-supplied or
          * externally-sourced strings here creates an XSS vulnerability. Use [[kyo.UI.Ast.Text]] (which escapes its argument) for any
          * content that is not fully trusted.
          *
          * @param value
          *   Trusted HTML string; rendered verbatim, no escaping applied.
          */
        case class RawHtml(value: String)(using val frame: Frame) extends UI

        /** A subtree driven by a `Signal[UI]`: re-renders the bound region whenever the signal emits a new value. The type parameter `C`
          * is a phantom bound that records the content type at the construction site; it erases to `UI` at runtime.
          *
          * `source` is the signal before the projection to UI, kept as an accelerator (see [[Reactive.Source]]). `signal` stays the
          * node's content for the first paint, for event dispatch and for the server transport, and a node built without a source (a
          * lifted `Signal[UI]`, where no value exists before the UI) is compared as UI.
          */
        case class Reactive[C <: UI](signal: Signal[UI], source: Maybe[Reactive.Source[?]] = Absent)(using val frame: Frame) extends UI

        object Reactive:
            /** The signal a region was projected from, with the projection that turns one of its values into UI. The same pair
              * [[kyo.UI.Ast.Foreach]] keeps for its rows.
              *
              * Why the node cannot make do with `signal` alone: the only comparison available there is UI against UI, and a subtree built
              * from closures is never equal to the last one, so every emission repaints, including the ones a projection made
              * irrelevant. `a.map(slice).render(f)` observed in UI space compares the root's values (projections compose down the chain,
              * see `Signal.map`) against an image that is never equal; observed here it compares `slice`, which is a value.
              */
            sealed trait Source[A]:
                def signal: Signal[A]
                def render: A => UI

                /** The value the source holds now and what it paints for it, for a pass that holds the thread. */
                private[kyo] def paintedNow(using AllowUnsafe, Frame, kyo.internal.SignalNow.Locals): Painted =
                    val value = kyo.internal.SignalNow(signal)
                    Painted(value, render(value))

                /** Observes the source in value space: an emission equal to the last one never reaches the projection. The passes and
                  * rests go to `stamp` (see `Signal.observeStamped`). A first emission of the value the enclosing paint painted hands on what
                  * that paint had, so the region can tell it is the same: two trees built from closures are never equal, and a repaint
                  * would draw again what that paint drew.
                  */
                private[kyo] def observe[S](stamp: Signal.ReadStamp, painted: Maybe[Painted])(paint: UI => Unit < (S & Async & Scope))(using
                    Frame
                ): Unit < (S & Async) =
                    // One region's own loop reads and clears it.
                    var first = painted
                    signal.observeStamped(stamp) { v =>
                        val shown = first match
                            case Present(p) if p.value.equals(v) => p.ui
                            case _                               => render(v)
                        first = Absent
                        paint(shown)
                    }
                end observe
            end Source

            /** One value of a [[Source]] with the content a paint showed for it. */
            final private[kyo] case class Painted(value: Any, ui: UI)

            object Source:
                /** A region built by `Signal.render`. */
                final case class Projected[A](signal: Signal[A], render: A => UI) extends Source[A]

                /** A region lifted from a `Signal[String]`. Its content is one text node whatever the signal emits, which is knowable at the
                  * lift site and only guessable afterwards, so the engine binds it straight to the backend's text write; a backend that
                  * offers no such write re-renders the region.
                  */
                final case class Lifted(signal: Signal[String])(using Frame) extends Source[String]:
                    def render: String => UI = s => Text(s)
            end Source
        end Reactive

        /** A keyed list driven by a `Signal[Chunk[A]]`: renders one child per element, reconciling by `key` when present. The type
          * parameter `C` is a phantom bound that records the content type at the construction site; it erases to `UI` at runtime.
          *
          * @param indexed
          *   true for the `*Indexed` variants: the index is baked into row content, so a row cannot be reused across emissions
          *   even when its key and item are unchanged
          */
        case class Foreach[A, C <: UI](
            signal: Signal[Chunk[A]],
            key: Maybe[A => String],
            render: (Int, A) => UI,
            indexed: Boolean = false
        )(using val frame: Frame) extends UI:
            /** Apply a polymorphic continuation with the typed members of this Foreach.
              *
              * Callers that match on `Foreach[?, ?]` (existential) lose the type parameters. This method re-introduces A in a single
              * cast that is sound because the Foreach instance carries its own signal/render/key all constructed with the same A.
              */
            private[kyo] def applyTyped[B](k: [T] => (Signal[Chunk[T]], Maybe[T => String], (Int, T) => UI) => B): B =
                k[A](signal, key, render)
        end Foreach

        /** A transparent grouping of children with no wrapping element; renders its children inline into the parent. The type parameter
          * `C` is a phantom bound that records the content type at the construction site; it erases to `UI` at runtime.
          */
        case class Fragment[C <: UI](children: Chunk[UI])(using val frame: Frame) extends UI

        private[kyo] case class KeyedChild[C <: UI](key: String, child: UI)(using val frame: Frame) extends UI

        /** A subtree under a [[kyo.UI.boundary]]. `display`, when present, is what a static rendering shows in place of the boundary's
          * own choice between its placeholder and `child`: the live state the engine resolved for a reveal or a snapshot.
          */
        private[kyo] case class Bounded(settings: UI.Boundary, child: UI, display: Maybe[UI])(using val frame: Frame) extends UI
            with HtmlContent

        /** Where the key of a [[MountNode]] comes from, which decides what one identity shown more than once in one range is.
          *
          * `Caller`: the caller chose the key with [[kyo.UI.MountedOps.keyed]]. Two calls in one range with the same identity are a mistake
          * the page reports as [[kyo.UI.MountErrorSource.Duplicate]], since only the data can say which instance belongs where.
          *
          * `Site`: the component keyed itself on where it was made, with [[kyo.UI.MountedOps.keyedOnSite]]. The caller chose nothing, so
          * calls of one site in one range, such as one call in a `map`, are told apart by their order, as Compose tells apart calls
          * without a key.
          */
        enum KeyOrigin derives CanEqual:
            case Caller, Site
        end KeyOrigin

        /** What [[Mounted]] and [[Drawn]] have in common: a place with an instance of its own, found by its [[key]] from [[site]], the
          * key coming from where [[keyOrigin]] says.
          */
        sealed trait MountNode extends UI with HtmlContent:
            def key: Maybe[Any]
            def errorRender: Maybe[UI.MountError => UI]
            def site: UI.MountSite
            def keyOrigin: KeyOrigin
            def frame: Frame
        end MountNode

        /** A component that waits for its content, built by [[kyo.UI.mounted]]. Its static rendering is its placeholder; the live content
          * belongs to the instance the renderer runs. `site` is where [[kyo.UI.MountedOps.keyed]] was called, and means nothing without a key.
          */
        case class Mounted(
            effect: UI < (Abort[Any] & Async & Scope),
            key: Maybe[Any],
            placeholderUI: Maybe[UI],
            errorRender: Maybe[UI.MountError => UI],
            pendingPolicy: UI.PendingPolicy,
            site: UI.MountSite = kyo.internal.MountSites.MountSite.none,
            keyOrigin: KeyOrigin = KeyOrigin.Caller
        )(using val frame: Frame) extends MountNode

        /** A component whose content `draw` produces in the render pass, built by [[kyo.UI.mounted]]. `display`, when present, is what a
          * static rendering shows instead of drawing: the live state the engine resolved for a reveal or a snapshot.
          */
        case class Drawn(
            draw: UI < Sync,
            key: Maybe[Any],
            errorRender: Maybe[UI.MountError => UI],
            site: UI.MountSite = kyo.internal.MountSites.MountSite.none,
            display: Maybe[UI] = Absent,
            // The props of a node built by `UI.mountedWith`, which its instance reads through a signal of its own.
            props: Maybe[kyo.internal.PropsDraw[?]] = Absent,
            keyOrigin: KeyOrigin = KeyOrigin.Caller
        )(using val frame: Frame) extends MountNode

        // ====== Block containers ======

        final case class Div(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Div
            def withAttrs(a: Attrs): Div      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Div = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Div

        final case class P(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = P
            def withAttrs(a: Attrs): P      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): P = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end P

        final case class Section(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Section
            def withAttrs(a: Attrs): Section      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Section = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Section

        final case class Main(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Main
            def withAttrs(a: Attrs): Main      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Main = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Main

        final case class Header(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Header
            def withAttrs(a: Attrs): Header      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Header = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Header

        final case class Footer(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Footer
            def withAttrs(a: Attrs): Footer      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Footer = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Footer

        final case class Pre(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Pre
            def withAttrs(a: Attrs): Pre      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Pre = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Pre

        final case class Blockquote(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Blockquote
            def withAttrs(a: Attrs): Blockquote      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Blockquote = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Blockquote

        final case class Code(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Code
            def withAttrs(a: Attrs): Code      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Code = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Code

        final case class Ul(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Ul
            def withAttrs(a: Attrs): Ul      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Ul = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Ul

        final case class Ol(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Ol
            def withAttrs(a: Attrs): Ol      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Ol = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Ol

        final case class Table(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Table
            def withAttrs(a: Attrs): Table      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Table = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Table

        /** Column definitions for a table: the one place a per-column width belongs, since a
          * `<col>` sizes its whole column at once while a width written on a cell sizes only the
          * row it is in. A header spanning several columns cannot carry a per-column width at all,
          * which is what makes this the only structure that works under a grouped header.
          */
        final case class Colgroup(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame)
            extends Block:
            type Self = Colgroup
            def withAttrs(a: Attrs): Colgroup      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Colgroup = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Colgroup

        /** One column of a [[Colgroup]], matched to a table column by position. Void. A `width` here is the column's, and a
          * `background` paints behind every cell of it.
          */
        final case class Col(attrs: Attrs = Attrs())(using val frame: Frame) extends Block with Void:
            type Self = Col
            def withAttrs(a: Attrs): Col = copy(attrs = a)

        /** Table header row group. The parser never synthesizes a `<thead>`: header rows written as direct
          * `<table>` children become part of the implied body group, so `thead`-scoped selectors (and the
          * sticky-header layout that rides on them) never match. Wrap the header rows in `UI.thead`.
          */
        final case class Thead(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Thead
            def withAttrs(a: Attrs): Thead      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Thead = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Thead

        /** Explicit table row group for authored table structure and styling. Reactive rows placed directly under
          * [[Table]] receive their own legal row-group host automatically.
          */
        final case class Tbody(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Tbody
            def withAttrs(a: Attrs): Tbody      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Tbody = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Tbody

        /** Table footer row group, the counterpart of [[Thead]] for summary rows. A footer row written as a
          * direct `<table>` child joins the implied body group instead, so it renders where it is written rather
          * than as the table's footer, and `tfoot`-scoped selectors miss it.
          */
        final case class Tfoot(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Tfoot
            def withAttrs(a: Attrs): Tfoot      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Tfoot = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Tfoot

        // ====== Headings (Block) ======

        final case class H1(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = H1
            def withAttrs(a: Attrs): H1      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): H1 = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end H1

        final case class H2(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = H2
            def withAttrs(a: Attrs): H2      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): H2 = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end H2

        final case class H3(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = H3
            def withAttrs(a: Attrs): H3      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): H3 = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end H3

        final case class H4(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = H4
            def withAttrs(a: Attrs): H4      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): H4 = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end H4

        final case class H5(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = H5
            def withAttrs(a: Attrs): H5      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): H5 = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end H5

        final case class H6(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = H6
            def withAttrs(a: Attrs): H6      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): H6 = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end H6

        // ====== Inline containers ======

        final case class SpanElement(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Inline
            with Interactive:
            type Self = SpanElement
            def withAttrs(a: Attrs): SpanElement      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): SpanElement = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end SpanElement

        final case class Nav(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Inline
            with Interactive:
            type Self = Nav
            def withAttrs(a: Attrs): Nav      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Nav = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Nav

        final case class Li(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Inline
            with Interactive:
            type Self = Li
            def withAttrs(a: Attrs): Li      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Li = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Li

        final case class Tr(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Inline
            with Interactive:
            type Self = Tr
            def withAttrs(a: Attrs): Tr      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Tr = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Tr

        // ====== Specialized Block elements ======

        final case class Form(
            attrs: Attrs = Attrs(),
            children: Chunk[UI] = Chunk.empty,
            onSubmit: Maybe[Action] = Absent,
            onSubmitEvt: Maybe[MouseEvent => Action] = Absent
        )(using val frame: Frame) extends Block with Interactive:
            type Self = Form
            def withAttrs(a: Attrs): Form      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Form = copy(children = children ++ Chunk.from(cs.map(_.value)))

            /** Runs `action` on form submit, ignoring the event payload. */
            def onSubmit(action: => Action): Form = copy(onSubmit = Present(Sync.defer(action)(using frame)))

            /** Runs `f` on form submit, receiving the [[kyo.UI.MouseEvent]] payload. */
            def onSubmit(f: MouseEvent => Action): Form = copy(onSubmitEvt = Present(f))
        end Form

        /** `<fieldset>`: a named group of controls.
          *
          * The group's accessible name comes from a [[Legend]] as its first child. That is a structural relationship, so a legend whose
          * text is a signal names the group as its text changes, which an `aria-label` on a `div` with `role="group"` cannot do. A
          * `disabled` fieldset also disables the controls inside it.
          */
        final case class Fieldset(
            attrs: Attrs = Attrs(),
            children: Chunk[UI] = Chunk.empty,
            disabled: Maybe[Boolean] = Absent
        )(using val frame: Frame) extends Block with Interactive with HasDisabled:
            type Self = Fieldset
            def withAttrs(a: Attrs): Fieldset      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Fieldset = copy(children = children ++ Chunk.from(cs.map(_.value)))

            /** Disables every control inside the group, however deeply nested, without the controls declaring anything themselves. */
            private[kyo] def withDisabled(v: Boolean): Fieldset = copy(disabled = Present(v))
        end Fieldset

        /** `<legend>`: the caption of the [[Fieldset]] it is the first child of. An ordinary block element whose meaning comes from that
          * position.
          */
        final case class Legend(attrs: Attrs = Attrs(), children: Chunk[UI] = Chunk.empty)(using val frame: Frame) extends Block
            with Interactive:
            type Self = Legend
            def withAttrs(a: Attrs): Legend      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Legend = copy(children = children ++ Chunk.from(cs.map(_.value)))
        end Legend

        final case class Textarea(
            attrs: Attrs = Attrs(),
            textInputAttrs: TextInputAttrs = TextInputAttrs()
        )(using val frame: Frame) extends Block with Interactive with ConstrainedInput with Void:
            type Self = Textarea
            def withAttrs(a: Attrs): Textarea = copy(attrs = a)

            def value: Maybe[Bound[String]]                       = textInputAttrs.value
            def placeholder: Maybe[String]                        = textInputAttrs.placeholder
            def readOnly: Maybe[Boolean]                          = textInputAttrs.readOnly
            def required: Maybe[Boolean]                          = textInputAttrs.required
            def disabled: Maybe[Boolean]                          = textInputAttrs.disabled
            def onInput: Maybe[String => Action]                  = textInputAttrs.onInput
            def onChange: Maybe[String => Action]                 = textInputAttrs.onChange
            def onBlurText: Maybe[(MouseEvent, String) => Action] = textInputAttrs.onBlurText
            def inputFilter: Maybe[InputFilter]                   = textInputAttrs.inputFilter
            def inputMask: Maybe[String]                          = textInputAttrs.inputMask

            def value(v: String | Signal[String]): Textarea         = copy(textInputAttrs = textInputAttrs.copy(value = Present(Bound(v))))
            private[kyo] def withPlaceholder(v: String): Textarea   = copy(textInputAttrs = textInputAttrs.copy(placeholder = Present(v)))
            private[kyo] def withReadOnly(v: Boolean): Textarea     = copy(textInputAttrs = textInputAttrs.copy(readOnly = Present(v)))
            private[kyo] def withRequired(v: Boolean): Textarea     = copy(textInputAttrs = textInputAttrs.copy(required = Present(v)))
            private[kyo] def withDisabled(v: Boolean): Textarea     = copy(textInputAttrs = textInputAttrs.copy(disabled = Present(v)))
            def onInput(f: String => Action): Textarea              = copy(textInputAttrs = textInputAttrs.copy(onInput = Present(f)))
            def onChange(f: String => Action): Textarea             = copy(textInputAttrs = textInputAttrs.copy(onChange = Present(f)))
            def onBlur(f: (MouseEvent, String) => Action): Textarea = copy(textInputAttrs = textInputAttrs.copy(onBlurText = Present(f)))
            def inputFilter(v: InputFilter): Textarea               = copy(textInputAttrs = textInputAttrs.copy(inputFilter = Present(v)))
            def inputMask(v: String): Textarea                      = copy(textInputAttrs = textInputAttrs.copy(inputMask = Present(v)))
            // A textarea has no `type`: the constant is kept, and the renderer writes none for it.
            def textType: Maybe[TextType]                        = textInputAttrs.textType
            private[kyo] def withTextType(v: TextType): Textarea = copy(textInputAttrs = textInputAttrs.copy(textType = Present(v)))
        end Textarea

        final case class Select(
            attrs: Attrs = Attrs(),
            children: Chunk[UI] = Chunk.empty,
            value: Maybe[Bound[String]] = Absent,
            disabled: Maybe[Boolean] = Absent,
            onChange: Maybe[String => Action] = Absent
        )(using val frame: Frame) extends Block with Interactive with PickerInput:
            type Self = Select
            def withAttrs(a: Attrs): Select = copy(attrs = a)

            def apply(cs: HtmlChildVal*): Select              = copy(children = children ++ Chunk.from(cs.map(_.value)))
            def value(v: String | Signal[String]): Select     = copy(value = Present(Bound(v)))
            private[kyo] def withDisabled(v: Boolean): Select = copy(disabled = Present(v))
            def onChange(f: String => Action): Select         = copy(onChange = Present(f))
        end Select

        final case class Hr(attrs: Attrs = Attrs())(using val frame: Frame) extends Block with Void:
            type Self = Hr
            def withAttrs(a: Attrs): Hr = copy(attrs = a)

        final case class Br(attrs: Attrs = Attrs())(using val frame: Frame) extends Block with Void:
            type Self = Br
            def withAttrs(a: Attrs): Br = copy(attrs = a)

        final case class Td(
            attrs: Attrs = Attrs(),
            children: Chunk[UI] = Chunk.empty,
            colspan: Maybe[Int] = Absent,
            rowspan: Maybe[Int] = Absent
        )(using val frame: Frame) extends Block with Interactive:
            type Self = Td
            def withAttrs(a: Attrs): Td      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Td = copy(children = children ++ Chunk.from(cs.map(_.value)))
            def colspan(v: Int): Td          = copy(colspan = Present(math.max(1, v)))
            def rowspan(v: Int): Td          = copy(rowspan = Present(math.max(1, v)))
        end Td

        final case class Th(
            attrs: Attrs = Attrs(),
            children: Chunk[UI] = Chunk.empty,
            colspan: Maybe[Int] = Absent,
            rowspan: Maybe[Int] = Absent
        )(using val frame: Frame) extends Block with Interactive:
            type Self = Th
            def withAttrs(a: Attrs): Th      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Th = copy(children = children ++ Chunk.from(cs.map(_.value)))
            def colspan(v: Int): Th          = copy(colspan = Present(math.max(1, v)))
            def rowspan(v: Int): Th          = copy(rowspan = Present(math.max(1, v)))
        end Th

        final case class Label(
            attrs: Attrs = Attrs(),
            children: Chunk[UI] = Chunk.empty,
            forId: Maybe[String] = Absent
        )(using val frame: Frame) extends Block with Interactive:
            type Self = Label
            def withAttrs(a: Attrs): Label      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Label = copy(children = children ++ Chunk.from(cs.map(_.value)))
            def forId(v: String): Label         = copy(forId = Present(v))
            def `for`(v: String): Label         = forId(v)
        end Label

        final case class Opt(
            attrs: Attrs = Attrs(),
            children: Chunk[UI] = Chunk.empty,
            value: Maybe[String] = Absent,
            selected: Maybe[Boolean] = Absent
        )(using val frame: Frame) extends Block:
            type Self = Opt
            def withAttrs(a: Attrs): Opt                     = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Opt                = copy(children = children ++ Chunk.from(cs.map(_.value)))
            def value(v: String): Opt                        = copy(value = Present(v))
            def selected(v: Boolean): Opt                    = copy(selected = Present(v))
            def selected(v: Signal[Boolean]): Reactive[Self] =
                given Frame = frame
                Reactive[Self](v.map(b => this.selected(b): UI))
        end Opt

        // ====== Specialized Inline elements ======

        final case class Button(
            attrs: Attrs = Attrs(),
            children: Chunk[UI] = Chunk.empty,
            disabled: Maybe[Boolean] = Absent
        )(using val frame: Frame) extends Inline with Interactive with Focusable with HasDisabled with Activatable with Clickable:
            type Self = Button
            def withAttrs(a: Attrs): Button = copy(attrs = a)

            def apply(cs: HtmlChildVal*): Button              = copy(children = children ++ Chunk.from(cs.map(_.value)))
            private[kyo] def withDisabled(v: Boolean): Button = copy(disabled = Present(v))
        end Button

        // ---- Boolean inputs (checked + onChange: Boolean) ----

        final case class Checkbox(
            attrs: Attrs = Attrs(),
            checked: Maybe[Bound[Boolean]] = Absent,
            disabled: Maybe[Boolean] = Absent,
            onChange: Maybe[Boolean => Action] = Absent
        )(using val frame: Frame) extends Inline with Interactive with BooleanInput with Void:
            type Self = Checkbox
            def withAttrs(a: Attrs): Checkbox = copy(attrs = a)

            def checked(v: Boolean | Signal[Boolean]): Checkbox = copy(checked = Present(Bound(v)))
            private[kyo] def withDisabled(v: Boolean): Checkbox = copy(disabled = Present(v))
            def onChange(f: Boolean => Action): Checkbox        = copy(onChange = Present(f))

            /** Sets the DOM `indeterminate` property, a visual tri-state distinct from `checked` (the checkbox shows a dash, not a tick). */
            def indeterminate(v: Boolean): Checkbox =
                if v then jsProp("indeterminate", "true")
                else withAttrs(attrs.copy(jsProps = attrs.jsProps - "indeterminate"))

            /** Reactive `indeterminate`: re-renders when the signal emits. */
            def indeterminate(v: Signal[Boolean])(using Frame, CanEqual[Boolean, Boolean]): Reactive[Self] =
                given Frame = frame
                Reactive[Self](v.map(b => this.indeterminate(b): UI))
        end Checkbox

        final case class Radio(
            attrs: Attrs = Attrs(),
            checked: Maybe[Bound[Boolean]] = Absent,
            disabled: Maybe[Boolean] = Absent,
            onChange: Maybe[Boolean => Action] = Absent,
            name: Maybe[String] = Absent
        )(using val frame: Frame) extends Inline with Interactive with BooleanInput with Void:
            type Self = Radio
            def withAttrs(a: Attrs): Radio = copy(attrs = a)

            def checked(v: Boolean | Signal[Boolean]): Radio = copy(checked = Present(Bound(v)))
            private[kyo] def withDisabled(v: Boolean): Radio = copy(disabled = Present(v))
            def onChange(f: Boolean => Action): Radio        = copy(onChange = Present(f))
            def name(v: String): Radio                       = copy(name = Present(v))
        end Radio

        // ---- Text inputs (value + placeholder + onInput + onChange: String) ----

        final private[kyo] case class TextInputAttrs(
            value: Maybe[Bound[String]] = Absent,
            placeholder: Maybe[String] = Absent,
            readOnly: Maybe[Boolean] = Absent,
            required: Maybe[Boolean] = Absent,
            disabled: Maybe[Boolean] = Absent,
            onInput: Maybe[String => Action] = Absent,
            onChange: Maybe[String => Action] = Absent,
            onBlurText: Maybe[(MouseEvent, String) => Action] = Absent,
            inputFilter: Maybe[InputFilter] = Absent,
            inputMask: Maybe[String] = Absent,
            textType: Maybe[TextType] = Absent
        )

        final case class Input(
            attrs: Attrs = Attrs(),
            textInputAttrs: TextInputAttrs = TextInputAttrs()
        )(using val frame: Frame) extends Inline with Interactive with ConstrainedInput with Void:
            type Self = Input
            def withAttrs(a: Attrs): Input = copy(attrs = a)

            def value: Maybe[Bound[String]]                       = textInputAttrs.value
            def placeholder: Maybe[String]                        = textInputAttrs.placeholder
            def readOnly: Maybe[Boolean]                          = textInputAttrs.readOnly
            def required: Maybe[Boolean]                          = textInputAttrs.required
            def disabled: Maybe[Boolean]                          = textInputAttrs.disabled
            def onInput: Maybe[String => Action]                  = textInputAttrs.onInput
            def onChange: Maybe[String => Action]                 = textInputAttrs.onChange
            def onBlurText: Maybe[(MouseEvent, String) => Action] = textInputAttrs.onBlurText
            def inputFilter: Maybe[InputFilter]                   = textInputAttrs.inputFilter
            def inputMask: Maybe[String]                          = textInputAttrs.inputMask

            def value(v: String | Signal[String]): Input         = copy(textInputAttrs = textInputAttrs.copy(value = Present(Bound(v))))
            private[kyo] def withPlaceholder(v: String): Input   = copy(textInputAttrs = textInputAttrs.copy(placeholder = Present(v)))
            private[kyo] def withReadOnly(v: Boolean): Input     = copy(textInputAttrs = textInputAttrs.copy(readOnly = Present(v)))
            private[kyo] def withRequired(v: Boolean): Input     = copy(textInputAttrs = textInputAttrs.copy(required = Present(v)))
            private[kyo] def withDisabled(v: Boolean): Input     = copy(textInputAttrs = textInputAttrs.copy(disabled = Present(v)))
            def onInput(f: String => Action): Input              = copy(textInputAttrs = textInputAttrs.copy(onInput = Present(f)))
            def onChange(f: String => Action): Input             = copy(textInputAttrs = textInputAttrs.copy(onChange = Present(f)))
            def onBlur(f: (MouseEvent, String) => Action): Input = copy(textInputAttrs = textInputAttrs.copy(onBlurText = Present(f)))
            def inputFilter(v: InputFilter): Input               = copy(textInputAttrs = textInputAttrs.copy(inputFilter = Present(v)))
            def inputMask(v: String): Input                      = copy(textInputAttrs = textInputAttrs.copy(inputMask = Present(v)))
            def textType: Maybe[TextType]                        = textInputAttrs.textType
            private[kyo] def withTextType(v: TextType): Input    = copy(textInputAttrs = textInputAttrs.copy(textType = Present(v)))
        end Input

        final case class PasswordInput(
            attrs: Attrs = Attrs(),
            textInputAttrs: TextInputAttrs = TextInputAttrs()
        )(using val frame: Frame) extends Inline with Interactive with ConstrainedInput with Void:
            type Self = PasswordInput
            def withAttrs(a: Attrs): PasswordInput = copy(attrs = a)

            def value: Maybe[Bound[String]]                       = textInputAttrs.value
            def placeholder: Maybe[String]                        = textInputAttrs.placeholder
            def readOnly: Maybe[Boolean]                          = textInputAttrs.readOnly
            def required: Maybe[Boolean]                          = textInputAttrs.required
            def disabled: Maybe[Boolean]                          = textInputAttrs.disabled
            def onInput: Maybe[String => Action]                  = textInputAttrs.onInput
            def onChange: Maybe[String => Action]                 = textInputAttrs.onChange
            def onBlurText: Maybe[(MouseEvent, String) => Action] = textInputAttrs.onBlurText
            def inputFilter: Maybe[InputFilter]                   = textInputAttrs.inputFilter
            def inputMask: Maybe[String]                          = textInputAttrs.inputMask

            def value(v: String | Signal[String]): PasswordInput = copy(textInputAttrs = textInputAttrs.copy(value = Present(Bound(v))))
            private[kyo] def withPlaceholder(v: String): PasswordInput =
                copy(textInputAttrs = textInputAttrs.copy(placeholder = Present(v)))
            private[kyo] def withReadOnly(v: Boolean): PasswordInput     = copy(textInputAttrs = textInputAttrs.copy(readOnly = Present(v)))
            private[kyo] def withRequired(v: Boolean): PasswordInput     = copy(textInputAttrs = textInputAttrs.copy(required = Present(v)))
            private[kyo] def withDisabled(v: Boolean): PasswordInput     = copy(textInputAttrs = textInputAttrs.copy(disabled = Present(v)))
            def onInput(f: String => Action): PasswordInput              = copy(textInputAttrs = textInputAttrs.copy(onInput = Present(f)))
            def onChange(f: String => Action): PasswordInput             = copy(textInputAttrs = textInputAttrs.copy(onChange = Present(f)))
            def onBlur(f: (MouseEvent, String) => Action): PasswordInput =
                copy(textInputAttrs = textInputAttrs.copy(onBlurText = Present(f)))
            def inputFilter(v: InputFilter): PasswordInput            = copy(textInputAttrs = textInputAttrs.copy(inputFilter = Present(v)))
            def inputMask(v: String): PasswordInput                   = copy(textInputAttrs = textInputAttrs.copy(inputMask = Present(v)))
            def textType: Maybe[TextType]                             = textInputAttrs.textType
            private[kyo] def withTextType(v: TextType): PasswordInput = copy(textInputAttrs = textInputAttrs.copy(textType = Present(v)))
        end PasswordInput

        final case class EmailInput(
            attrs: Attrs = Attrs(),
            textInputAttrs: TextInputAttrs = TextInputAttrs()
        )(using val frame: Frame) extends Inline with Interactive with ConstrainedInput with Void:
            type Self = EmailInput
            def withAttrs(a: Attrs): EmailInput = copy(attrs = a)

            def value: Maybe[Bound[String]]                       = textInputAttrs.value
            def placeholder: Maybe[String]                        = textInputAttrs.placeholder
            def readOnly: Maybe[Boolean]                          = textInputAttrs.readOnly
            def required: Maybe[Boolean]                          = textInputAttrs.required
            def disabled: Maybe[Boolean]                          = textInputAttrs.disabled
            def onInput: Maybe[String => Action]                  = textInputAttrs.onInput
            def onChange: Maybe[String => Action]                 = textInputAttrs.onChange
            def onBlurText: Maybe[(MouseEvent, String) => Action] = textInputAttrs.onBlurText
            def inputFilter: Maybe[InputFilter]                   = textInputAttrs.inputFilter
            def inputMask: Maybe[String]                          = textInputAttrs.inputMask

            def value(v: String | Signal[String]): EmailInput       = copy(textInputAttrs = textInputAttrs.copy(value = Present(Bound(v))))
            private[kyo] def withPlaceholder(v: String): EmailInput = copy(textInputAttrs = textInputAttrs.copy(placeholder = Present(v)))
            private[kyo] def withReadOnly(v: Boolean): EmailInput   = copy(textInputAttrs = textInputAttrs.copy(readOnly = Present(v)))
            private[kyo] def withRequired(v: Boolean): EmailInput   = copy(textInputAttrs = textInputAttrs.copy(required = Present(v)))
            private[kyo] def withDisabled(v: Boolean): EmailInput   = copy(textInputAttrs = textInputAttrs.copy(disabled = Present(v)))
            def onInput(f: String => Action): EmailInput            = copy(textInputAttrs = textInputAttrs.copy(onInput = Present(f)))
            def onChange(f: String => Action): EmailInput           = copy(textInputAttrs = textInputAttrs.copy(onChange = Present(f)))
            def onBlur(f: (MouseEvent, String) => Action): EmailInput = copy(textInputAttrs = textInputAttrs.copy(onBlurText = Present(f)))
            def inputFilter(v: InputFilter): EmailInput               = copy(textInputAttrs = textInputAttrs.copy(inputFilter = Present(v)))
            def inputMask(v: String): EmailInput                      = copy(textInputAttrs = textInputAttrs.copy(inputMask = Present(v)))
            def textType: Maybe[TextType]                             = textInputAttrs.textType
            private[kyo] def withTextType(v: TextType): EmailInput    = copy(textInputAttrs = textInputAttrs.copy(textType = Present(v)))
        end EmailInput

        final case class TelInput(
            attrs: Attrs = Attrs(),
            textInputAttrs: TextInputAttrs = TextInputAttrs()
        )(using val frame: Frame) extends Inline with Interactive with ConstrainedInput with Void:
            type Self = TelInput
            def withAttrs(a: Attrs): TelInput = copy(attrs = a)

            def value: Maybe[Bound[String]]                       = textInputAttrs.value
            def placeholder: Maybe[String]                        = textInputAttrs.placeholder
            def readOnly: Maybe[Boolean]                          = textInputAttrs.readOnly
            def required: Maybe[Boolean]                          = textInputAttrs.required
            def disabled: Maybe[Boolean]                          = textInputAttrs.disabled
            def onInput: Maybe[String => Action]                  = textInputAttrs.onInput
            def onChange: Maybe[String => Action]                 = textInputAttrs.onChange
            def onBlurText: Maybe[(MouseEvent, String) => Action] = textInputAttrs.onBlurText
            def inputFilter: Maybe[InputFilter]                   = textInputAttrs.inputFilter
            def inputMask: Maybe[String]                          = textInputAttrs.inputMask

            def value(v: String | Signal[String]): TelInput         = copy(textInputAttrs = textInputAttrs.copy(value = Present(Bound(v))))
            private[kyo] def withPlaceholder(v: String): TelInput   = copy(textInputAttrs = textInputAttrs.copy(placeholder = Present(v)))
            private[kyo] def withReadOnly(v: Boolean): TelInput     = copy(textInputAttrs = textInputAttrs.copy(readOnly = Present(v)))
            private[kyo] def withRequired(v: Boolean): TelInput     = copy(textInputAttrs = textInputAttrs.copy(required = Present(v)))
            private[kyo] def withDisabled(v: Boolean): TelInput     = copy(textInputAttrs = textInputAttrs.copy(disabled = Present(v)))
            def onInput(f: String => Action): TelInput              = copy(textInputAttrs = textInputAttrs.copy(onInput = Present(f)))
            def onChange(f: String => Action): TelInput             = copy(textInputAttrs = textInputAttrs.copy(onChange = Present(f)))
            def onBlur(f: (MouseEvent, String) => Action): TelInput = copy(textInputAttrs = textInputAttrs.copy(onBlurText = Present(f)))
            def inputFilter(v: InputFilter): TelInput               = copy(textInputAttrs = textInputAttrs.copy(inputFilter = Present(v)))
            def inputMask(v: String): TelInput                      = copy(textInputAttrs = textInputAttrs.copy(inputMask = Present(v)))
            def textType: Maybe[TextType]                           = textInputAttrs.textType
            private[kyo] def withTextType(v: TextType): TelInput    = copy(textInputAttrs = textInputAttrs.copy(textType = Present(v)))
        end TelInput

        final case class UrlInput(
            attrs: Attrs = Attrs(),
            textInputAttrs: TextInputAttrs = TextInputAttrs()
        )(using val frame: Frame) extends Inline with Interactive with ConstrainedInput with Void:
            type Self = UrlInput
            def withAttrs(a: Attrs): UrlInput = copy(attrs = a)

            def value: Maybe[Bound[String]]                       = textInputAttrs.value
            def placeholder: Maybe[String]                        = textInputAttrs.placeholder
            def readOnly: Maybe[Boolean]                          = textInputAttrs.readOnly
            def required: Maybe[Boolean]                          = textInputAttrs.required
            def disabled: Maybe[Boolean]                          = textInputAttrs.disabled
            def onInput: Maybe[String => Action]                  = textInputAttrs.onInput
            def onChange: Maybe[String => Action]                 = textInputAttrs.onChange
            def onBlurText: Maybe[(MouseEvent, String) => Action] = textInputAttrs.onBlurText
            def inputFilter: Maybe[InputFilter]                   = textInputAttrs.inputFilter
            def inputMask: Maybe[String]                          = textInputAttrs.inputMask

            def value(v: String | Signal[String]): UrlInput         = copy(textInputAttrs = textInputAttrs.copy(value = Present(Bound(v))))
            private[kyo] def withPlaceholder(v: String): UrlInput   = copy(textInputAttrs = textInputAttrs.copy(placeholder = Present(v)))
            private[kyo] def withReadOnly(v: Boolean): UrlInput     = copy(textInputAttrs = textInputAttrs.copy(readOnly = Present(v)))
            private[kyo] def withRequired(v: Boolean): UrlInput     = copy(textInputAttrs = textInputAttrs.copy(required = Present(v)))
            private[kyo] def withDisabled(v: Boolean): UrlInput     = copy(textInputAttrs = textInputAttrs.copy(disabled = Present(v)))
            def onInput(f: String => Action): UrlInput              = copy(textInputAttrs = textInputAttrs.copy(onInput = Present(f)))
            def onChange(f: String => Action): UrlInput             = copy(textInputAttrs = textInputAttrs.copy(onChange = Present(f)))
            def onBlur(f: (MouseEvent, String) => Action): UrlInput = copy(textInputAttrs = textInputAttrs.copy(onBlurText = Present(f)))
            def inputFilter(v: InputFilter): UrlInput               = copy(textInputAttrs = textInputAttrs.copy(inputFilter = Present(v)))
            def inputMask(v: String): UrlInput                      = copy(textInputAttrs = textInputAttrs.copy(inputMask = Present(v)))
            def textType: Maybe[TextType]                           = textInputAttrs.textType
            private[kyo] def withTextType(v: TextType): UrlInput    = copy(textInputAttrs = textInputAttrs.copy(textType = Present(v)))
        end UrlInput

        final case class SearchInput(
            attrs: Attrs = Attrs(),
            textInputAttrs: TextInputAttrs = TextInputAttrs()
        )(using val frame: Frame) extends Inline with Interactive with ConstrainedInput with Void:
            type Self = SearchInput
            def withAttrs(a: Attrs): SearchInput = copy(attrs = a)

            def value: Maybe[Bound[String]]                       = textInputAttrs.value
            def placeholder: Maybe[String]                        = textInputAttrs.placeholder
            def readOnly: Maybe[Boolean]                          = textInputAttrs.readOnly
            def required: Maybe[Boolean]                          = textInputAttrs.required
            def disabled: Maybe[Boolean]                          = textInputAttrs.disabled
            def onInput: Maybe[String => Action]                  = textInputAttrs.onInput
            def onChange: Maybe[String => Action]                 = textInputAttrs.onChange
            def onBlurText: Maybe[(MouseEvent, String) => Action] = textInputAttrs.onBlurText
            def inputFilter: Maybe[InputFilter]                   = textInputAttrs.inputFilter
            def inputMask: Maybe[String]                          = textInputAttrs.inputMask

            def value(v: String | Signal[String]): SearchInput       = copy(textInputAttrs = textInputAttrs.copy(value = Present(Bound(v))))
            private[kyo] def withPlaceholder(v: String): SearchInput = copy(textInputAttrs = textInputAttrs.copy(placeholder = Present(v)))
            private[kyo] def withReadOnly(v: Boolean): SearchInput   = copy(textInputAttrs = textInputAttrs.copy(readOnly = Present(v)))
            private[kyo] def withRequired(v: Boolean): SearchInput   = copy(textInputAttrs = textInputAttrs.copy(required = Present(v)))
            private[kyo] def withDisabled(v: Boolean): SearchInput   = copy(textInputAttrs = textInputAttrs.copy(disabled = Present(v)))
            def onInput(f: String => Action): SearchInput            = copy(textInputAttrs = textInputAttrs.copy(onInput = Present(f)))
            def onChange(f: String => Action): SearchInput           = copy(textInputAttrs = textInputAttrs.copy(onChange = Present(f)))
            def onBlur(f: (MouseEvent, String) => Action): SearchInput = copy(textInputAttrs = textInputAttrs.copy(onBlurText = Present(f)))
            def inputFilter(v: InputFilter): SearchInput            = copy(textInputAttrs = textInputAttrs.copy(inputFilter = Present(v)))
            def inputMask(v: String): SearchInput                   = copy(textInputAttrs = textInputAttrs.copy(inputMask = Present(v)))
            def textType: Maybe[TextType]                           = textInputAttrs.textType
            private[kyo] def withTextType(v: TextType): SearchInput = copy(textInputAttrs = textInputAttrs.copy(textType = Present(v)))
        end SearchInput

        final case class NumberInput(
            attrs: Attrs = Attrs(),
            textInputAttrs: TextInputAttrs = TextInputAttrs(),
            min: Maybe[Double] = Absent,
            max: Maybe[Double] = Absent,
            step: Maybe[Double] = Absent,
            onChangeNumeric: Maybe[Double => Action] = Absent
        )(using val frame: Frame) extends Inline with Interactive with TextInput with Void:
            type Self = NumberInput
            def withAttrs(a: Attrs): NumberInput = copy(attrs = a)

            def value: Maybe[Bound[String]]                       = textInputAttrs.value
            def placeholder: Maybe[String]                        = textInputAttrs.placeholder
            def readOnly: Maybe[Boolean]                          = textInputAttrs.readOnly
            def required: Maybe[Boolean]                          = textInputAttrs.required
            def disabled: Maybe[Boolean]                          = textInputAttrs.disabled
            def onInput: Maybe[String => Action]                  = textInputAttrs.onInput
            def onChange: Maybe[String => Action]                 = textInputAttrs.onChange
            def onBlurText: Maybe[(MouseEvent, String) => Action] = textInputAttrs.onBlurText

            def value(v: String | Signal[String]): NumberInput       = copy(textInputAttrs = textInputAttrs.copy(value = Present(Bound(v))))
            private[kyo] def withPlaceholder(v: String): NumberInput = copy(textInputAttrs = textInputAttrs.copy(placeholder = Present(v)))
            private[kyo] def withReadOnly(v: Boolean): NumberInput   = copy(textInputAttrs = textInputAttrs.copy(readOnly = Present(v)))
            private[kyo] def withRequired(v: Boolean): NumberInput   = copy(textInputAttrs = textInputAttrs.copy(required = Present(v)))
            private[kyo] def withDisabled(v: Boolean): NumberInput   = copy(textInputAttrs = textInputAttrs.copy(disabled = Present(v)))
            def onInput(f: String => Action): NumberInput            = copy(textInputAttrs = textInputAttrs.copy(onInput = Present(f)))
            def onChange(f: String => Action): NumberInput           = copy(textInputAttrs = textInputAttrs.copy(onChange = Present(f)))
            def onBlur(f: (MouseEvent, String) => Action): NumberInput = copy(textInputAttrs = textInputAttrs.copy(onBlurText = Present(f)))
            def min(v: Double): NumberInput                            = copy(min = Present(v))
            def max(v: Double): NumberInput                            = copy(max = Present(v))
            def step(v: Double): NumberInput                           = copy(step = Present(if v <= 0 then 1.0 else v))
            def onChangeNumeric(f: Double => Action): NumberInput      = copy(onChangeNumeric = Present(f))
        end NumberInput

        // ---- Picker inputs (value + onChange: String, no placeholder/onInput) ----

        final case class DateInput(
            attrs: Attrs = Attrs(),
            value: Maybe[Bound[String]] = Absent,
            disabled: Maybe[Boolean] = Absent,
            onChange: Maybe[String => Action] = Absent
        )(using val frame: Frame) extends Inline with Interactive with PickerInput with Void:
            type Self = DateInput
            def withAttrs(a: Attrs): DateInput                   = copy(attrs = a)
            def value(v: String | Signal[String]): DateInput     = copy(value = Present(Bound(v)))
            private[kyo] def withDisabled(v: Boolean): DateInput = copy(disabled = Present(v))
            def onChange(f: String => Action): DateInput         = copy(onChange = Present(f))
        end DateInput

        final case class TimeInput(
            attrs: Attrs = Attrs(),
            value: Maybe[Bound[String]] = Absent,
            disabled: Maybe[Boolean] = Absent,
            onChange: Maybe[String => Action] = Absent
        )(using val frame: Frame) extends Inline with Interactive with PickerInput with Void:
            type Self = TimeInput
            def withAttrs(a: Attrs): TimeInput                   = copy(attrs = a)
            def value(v: String | Signal[String]): TimeInput     = copy(value = Present(Bound(v)))
            private[kyo] def withDisabled(v: Boolean): TimeInput = copy(disabled = Present(v))
            def onChange(f: String => Action): TimeInput         = copy(onChange = Present(f))
        end TimeInput

        final case class ColorInput(
            attrs: Attrs = Attrs(),
            value: Maybe[Bound[String]] = Absent,
            disabled: Maybe[Boolean] = Absent,
            onChange: Maybe[String => Action] = Absent
        )(using val frame: Frame) extends Inline with Interactive with PickerInput with Void:
            type Self = ColorInput
            def withAttrs(a: Attrs): ColorInput                   = copy(attrs = a)
            def value(v: String | Signal[String]): ColorInput     = copy(value = Present(Bound(v)))
            private[kyo] def withDisabled(v: Boolean): ColorInput = copy(disabled = Present(v))
            def onChange(f: String => Action): ColorInput         = copy(onChange = Present(f))
        end ColorInput

        // ---- Special inputs ----

        final case class RangeInput(
            attrs: Attrs = Attrs(),
            value: Maybe[Bound[Double]] = Absent,
            min: Maybe[Double] = Absent,
            max: Maybe[Double] = Absent,
            step: Maybe[Double] = Absent,
            disabled: Maybe[Boolean] = Absent,
            onChange: Maybe[Double => Action] = Absent
        )(using val frame: Frame) extends Inline with Interactive with Focusable with HasDisabled with Void:
            type Self = RangeInput
            def withAttrs(a: Attrs): RangeInput                   = copy(attrs = a)
            def value(v: Double | Signal[Double]): RangeInput     = copy(value = Present(Bound(v)))
            def min(v: Double): RangeInput                        = copy(min = Present(v))
            def max(v: Double): RangeInput                        = copy(max = Present(v))
            def step(v: Double): RangeInput                       = copy(step = Present(if v <= 0 then 1.0 else v))
            private[kyo] def withDisabled(v: Boolean): RangeInput = copy(disabled = Present(v))
            def onChange(f: Double => Action): RangeInput         = copy(onChange = Present(f))
        end RangeInput

        final case class FileInput(
            attrs: Attrs = Attrs(),
            accept: Maybe[Chunk[FileAccept]] = Absent,
            disabled: Maybe[Boolean] = Absent,
            multiple: Maybe[Boolean] = Absent,
            onChange: Maybe[String => Action] = Absent
        )(using val frame: Frame) extends Inline with Interactive with Focusable with HasDisabled with Void:
            type Self = FileInput
            def withAttrs(a: Attrs): FileInput                   = copy(attrs = a)
            def accept(vs: FileAccept*): FileInput               = copy(accept = Present(Chunk.from(vs)))
            private[kyo] def withDisabled(v: Boolean): FileInput = copy(disabled = Present(v))

            /** Allow selecting more than one file (renders the HTML `multiple` attribute). */
            def multiple(v: Boolean): FileInput          = copy(multiple = Present(v))
            def onChange(f: String => Action): FileInput = copy(onChange = Present(f))
        end FileInput

        final case class HiddenInput(
            attrs: Attrs = Attrs(),
            value: Maybe[Bound[String]] = Absent
        )(using val frame: Frame) extends Inline with Void:
            type Self = HiddenInput
            def withAttrs(a: Attrs): HiddenInput               = copy(attrs = a)
            def value(v: String | Signal[String]): HiddenInput = copy(value = Present(Bound(v)))
        end HiddenInput

        final case class Anchor(
            attrs: Attrs = Attrs(),
            children: Chunk[UI] = Chunk.empty,
            href: Maybe[Href] = Absent,
            target: Maybe[Target] = Absent,
            download: Maybe[String] = Absent
        )(using val frame: Frame) extends Inline with Interactive with Focusable with Activatable with Clickable:
            type Self = Anchor
            def withAttrs(a: Attrs): Anchor      = copy(attrs = a)
            def apply(cs: HtmlChildVal*): Anchor = copy(children = children ++ Chunk.from(cs.map(_.value)))

            /** Where the link points, constant or reactive through one setter: a `Signal[Href]` is patched in place, so the link
              * keeps its node (and any focus on it).
              */
            def href(v: Href | Signal[Href]): Anchor = v match
                case s: Signal[Href] @unchecked =>
                    given Frame = frame
                    withReactiveAttr("href", s.map(Href.attrValue))
                case h: Href => copy(href = Present(h))

            /** Saves what `href` points at under `filename` instead of navigating to it (the HTML `download` attribute). Browsers
              * honor it for same-origin, `data:` and `blob:` URLs; a cross-origin href navigates instead. An empty `filename` leaves the
              * name to the browser.
              *
              * A clicked download anchor keeps the browser's default: neither a kyo click handler nor client-side routing turns it
              * into an in-page action.
              */
            def download(filename: String): Anchor    = copy(download = Present(filename))
            def href(v: Href, target: Target): Anchor = copy(href = Present(v), target = Present(target))
            def target(v: Target): Anchor             = copy(target = Present(v))
        end Anchor

        final case class Img(
            attrs: Attrs = Attrs(),
            src: Maybe[ImgSrc] = Absent,
            alt: Maybe[String] = Absent
        )(using val frame: Frame) extends Inline with Void:
            type Self = Img
            def withAttrs(a: Attrs): Img = copy(attrs = a)

            /** The image, constant or reactive through one setter: a `Signal[ImgSrc]` is patched in place, so swapping the image
              * does not replace the element.
              */
            def src(v: ImgSrc | Signal[ImgSrc]): Img = v match
                case s: Signal[ImgSrc] @unchecked =>
                    given Frame = frame
                    withReactiveAttr("src", s.map(ImgSrc.attrValue))
                case i: ImgSrc => copy(src = Present(i))
            def alt(v: String): Img = copy(alt = Present(v))
        end Img

        /** An inline frame embedding another document at `src`. `title` names the frame for assistive technology. Size it with `.style`. */
        final case class Iframe(
            attrs: Attrs = Attrs(),
            src: Maybe[String] = Absent,
            frameTitle: Maybe[String] = Absent
        )(using val frame: Frame) extends Block with Void:
            type Self = Iframe
            def withAttrs(a: Attrs): Iframe = copy(attrs = a)

            /** The framed document, constant or reactive through one setter: a `Signal[String]` is patched in place, since
              * re-rendering an iframe would reload the framed document.
              */
            def src(v: String | Signal[String]): Iframe = v match
                case s: Signal[String] @unchecked => withReactiveAttr("src", s)
                case t: String                    => copy(src = Present(t))
            def title(v: String): Iframe = copy(frameTitle = Present(v))
        end Iframe

        // ---- Custom dropdown (div-based overlay, NOT native <select>) ----

        final case class Dropdown(
            attrs: Attrs = Attrs(),
            options: Chunk[(String, String)] = Chunk.empty,
            value: Maybe[Bound[String]] = Absent,
            disabled: Maybe[Boolean] = Absent,
            onChange: Maybe[String => Action] = Absent
        )(using val frame: Frame) extends Block with Interactive with PickerInput:
            type Self = Dropdown
            def children: Chunk[UI]                             = Chunk.empty
            def withAttrs(a: Attrs): Dropdown                   = copy(attrs = a)
            def value(v: String): Dropdown                      = copy(value = Present(Bound.Const(v)))
            def value(v: SignalRef[String]): Dropdown           = copy(value = Present(Bound.Ref(v)))
            private[kyo] def withDisabled(v: Boolean): Dropdown = copy(disabled = Present(v))
            def onChange(f: String => Action): Dropdown         = copy(onChange = Present(f))
        end Dropdown

    end Ast

end UI
