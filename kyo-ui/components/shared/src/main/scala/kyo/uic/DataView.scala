package kyo.uic

import kyo.*
import kyo.UI.*

/** DataView presentation mode. */
enum DataViewLayout derives CanEqual:
    case List, Grid

    private[uic] def token: String = this match
        case DataViewLayout.List => "list"
        case DataViewLayout.Grid => "grid"
end DataViewLayout

/** DataView — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * DataView anatomy: `div.p-dataview.p-component.p-dataview-list|-grid` >
  * [`div.p-dataview-header`] + `div.p-dataview-content` + [the standalone
  * [[Paginator]] stamped `p-dataview-paginator-bottom`] +
  * [`div.p-dataview-footer`]), so the extracted `@primeuix` dataview CSS
  * applies verbatim.
  *
  * Items are TYPED: `items(Seq[A])` supplies the data, `itemTemplate` renders
  * each item in the list layout, `gridItemTemplate` (falling back to
  * `itemTemplate`) in the grid layout — item layout inside the content is
  * template-owned, exactly like Prime. `layout` is a static enum; apps that
  * want a live list/grid switcher re-render with the other value (pair it
  * with a [[SelectButton]]). `paginate(rows)(pageRef)` slices the items and
  * embeds the paginator bound two-way to the 0-based page ref.
  *
  * A list that GROWS — a stream of pages, a search that refines — supplies the
  * data as `items(Signal[Chunk[A]])` instead. The rows then become a kyo-ui list
  * region reconciled per emission, so an appended page renders the new cards and
  * leaves the ones already on screen alone; the build-time `Seq` form rebuilds
  * every card on every emission, because the whole component is re-created to
  * carry the new data. [[itemKey]] moves that reconciliation from position to
  * identity, which is what a reorder or a mid-list insertion needs.
  *
  * `loading(true)` renders PrimeReact's busy state: the sheet's
  * `.p-dataview-loading-overlay` (composed with `.p-overlay-mask` for the
  * dimmed backdrop) over the content, holding a [[ProgressSpinner]] — the
  * root's `position: relative` anchor is a `.p-uic-*` remainder rule keyed on
  * `.p-dataview-loading` (PrimeReact's root modifier).
  *
  * Sorting is deliberately NOT a component prop (Prime's `sortField`/
  * `sortOrder` re-order internally): sort the `Seq` before `items(...)` — the
  * template owns row layout either way, and a plain `sortBy` keeps the data
  * flow visible.
  */
final case class DataView[A] private (
    itemsV: List[A] = Nil,
    itemsSigV: Maybe[Signal[Chunk[A]]] = Absent,
    itemKeyF: Maybe[A => String] = Absent,
    itemF: Maybe[A => UI] = Absent,
    gridItemF: Maybe[A => UI] = Absent,
    layoutV: DataViewLayout = DataViewLayout.List,
    headerV: Maybe[UI] = Absent,
    footerV: Maybe[UI] = Absent,
    pageSizeV: Maybe[Int] = Absent,
    pageRef: Maybe[SignalRef[Int]] = Absent,
    emptyContentV: Maybe[EmptyContent] = Absent,
    loadingV: Maybe[BoolValue] = Absent
) extends Node, HasEmptyContent:
    type Self = DataView[A]

    /** Appends data items, fixed at build time. Clears a previously set items SIGNAL: the two sources are
      * exclusive and the last one set wins, because a component that silently merged a static block with a
      * live one would have no answer for what an emission does to the static rows.
      */
    def items(is: Seq[A]): DataView[A] = copy(itemsV = itemsV ++ is.toList, itemsSigV = Absent)

    /** Reactive items: the list is carried as a signal and the rows are reconciled per emission through
      * kyo-ui's list region (`Signal.foreach`), instead of the whole content being rebuilt.
      *
      * This is the form for data that arrives over time — a stream of pages, a query that refines. With the
      * build-time [[items(Seq)]] form an emission means rebuilding the component, so every card is
      * re-rendered even though only the tail changed; here an appended page renders the appended rows and
      * leaves the rows already on screen untouched. Without [[itemKey]] rows are matched by POSITION, which
      * is right for appends and wrong for reorders and mid-list insertions.
      *
      * Two limits worth knowing before reaching for it. Clears a previously set static list (the sources are
      * exclusive, last one wins), and a [[paginate]]d DataView falls back to the build-time path — the page
      * slice is build-time arithmetic over the whole list, so the rows cannot be a list region at all; the
      * signal then drives a plain region around the component, which is the same cost as rebuilding.
      */
    def items(sig: Signal[Chunk[A]]): DataView[A] = copy(itemsSigV = Present(sig), itemsV = Nil)

    /** A stable key per row, which switches reactive reconciliation from position to identity
      * (`Signal.foreachKeyed`): a row whose key survives a list change keeps its live subscriptions and its
      * DOM instead of being re-rendered into the position it moved to.
      *
      * Only the [[items(Signal)]] source reconciles, so this has no effect on a static list or on a
      * paginated one. The key must be an actual identity for the data — kyo-ui warns on duplicates, and a
      * key that collides applies a row's update to the wrong record.
      */
    def itemKey(f: A => String): DataView[A] = copy(itemKeyF = Present(f))

    /** Item template for the list layout (and the grid fallback). */
    def itemTemplate(f: A => UI): DataView[A] = copy(itemF = Present(f))

    /** Item template for the grid layout (falls back to [[itemTemplate]]). */
    def gridItemTemplate(f: A => UI): DataView[A] = copy(gridItemF = Present(f))

    /** Presentation mode: `List` (default) or `Grid`. */
    def layout(v: DataViewLayout): DataView[A] = copy(layoutV = v)

    /** Header slot (`div.p-dataview-header`). */
    def header(ui: UI): DataView[A] = copy(headerV = Present(ui))

    /** Footer slot (`div.p-dataview-footer`). */
    def footer(ui: UI): DataView[A] = copy(footerV = Present(ui))

    /** Slices the items into pages of `size` and embeds the [[Paginator]];
      * `ref` holds the 0-based page index (clamped at render).
      */
    def paginate(size: Int)(ref: SignalRef[Int]): DataView[A] =
        copy(pageSizeV = Present(math.max(1, size)), pageRef = Present(ref))

    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): DataView[A] = copy(emptyContentV = v)

    /** Busy state: a spinner overlay (`.p-dataview-loading-overlay`) dims the content while data is being
      * fetched (PrimeReact's `loading`). Bind a `Signal[Boolean]` to the data-fetch in-flight signal: the
      * overlay toggles in its own sub-region and the `.p-dataview-loading` class swaps IN PLACE via the
      * class channel, with no re-render of the data list.
      */
    def loading(v: Boolean | Signal[Boolean]): DataView[A] = copy(loadingV = Present(ReactiveValue(v)))

    /** The paginated fallback for a reactive source.
      *
      * A page slice is arithmetic over the WHOLE list (`totalRecords`, the clamped page count, the
      * `slice`), all of it build-time work the row region cannot do. So a paginated reactive DataView
      * resolves its signal into a plain region and renders the build-time path underneath — an honest
      * whole-component rebuild per emission rather than a keyed list that is not keyed. The paginator's own
      * page region nests inside it, which is the one place in this component where two regions stack: the
      * inner one is re-created by every emission of the outer, so it cannot outlive the list it sliced.
      */
    private[uic] def render(using Frame): UI =
        (itemsSigV, pageSizeV) match
            case (Present(sig), Present(_)) =>
                sig.render(items => copy(itemsSigV = Absent, itemsV = items.toList).render)
            case _ =>
                pageRef match
                    case Present(ref) => ref.render(body)
                    case Absent       => body(0)

    private def body(page: Int)(using Frame): UI =
        val (paged, paginatorUI) = pageSizeV match
            case Present(size) =>
                val totalPages = math.max(1, (itemsV.size + size - 1) / size)
                val cur        = math.min(math.max(page, 0), totalPages - 1)
                var pag = Paginator()
                    .totalRecords(itemsV.size)
                    .rows(size)
                    .currentPage(cur)
                    .hostClass("p-dataview-paginator-bottom")
                pageRef.foreach(ref => pag = pag.page(ref))
                (itemsV.slice(cur * size, cur * size + size), List(pag.render))
            case Absent => (itemsV, Nil)

        // Grid prefers its own template; both layouts fall back to the other's.
        val template: Maybe[A => UI] = layoutV match
            case DataViewLayout.Grid => gridItemF.orElse(itemF)
            case DataViewLayout.List => itemF.orElse(gridItemF)

        def emptyMessage: UI =
            EmptyContent.render(emptyContentV, "No records found")(c => div.cssClass("p-dataview-empty-message")(c))

        val contentChildren: List[UI] = itemsSigV match
            case Present(sig) =>
                // The rows are a list region; the empty state gets its OWN region over the narrowest
                // projection there is (`isEmpty`), so `emptyContent` survives without putting the rows
                // behind a region that repaints them whenever the list changes length.
                val rows: List[UI] = template.toList.map { f =>
                    itemKeyF match
                        case Present(key) => sig.foreachKeyed(key)(f)
                        case Absent       => sig.foreach(f)
                }
                rows :+ sig.map(_.isEmpty).render(isEmpty => if isEmpty then emptyMessage else UI.empty)
            case Absent =>
                if paged.isEmpty then List(emptyMessage)
                else template.toList.flatMap(f => paged.map(f))

        val headerSlot: List[UI] = headerV.toList.map(h => div.cssClass("p-dataview-header")(toChild(h)))
        val contentEl: UI        = div.cssClass("p-dataview-content")(contentChildren.map(toChild)*)
        val footerSlot: List[UI] = footerV.toList.map(f => div.cssClass("p-dataview-footer")(toChild(f)))

        // PrimeReact's busy state: the sheet's absolute overlay composed with the
        // dimming .p-overlay-mask (position resolves to absolute — the dataview rule
        // wins over the mask's fixed), holding our ProgressSpinner.
        def overlayDiv: UI =
            div
                .cssClass("p-dataview-loading-overlay")
                .cssClass("p-overlay-mask")(
                    toChild(ProgressSpinner().size(Size.Small).accessibleName("Loading").render)
                )
        val loadingOverlay: List[UI] = loadingV match
            case Present(BoolValue.Const(true))  => List(overlayDiv)
            case Present(BoolValue.Dyn(sig))     => List(sig.render(b => if b then overlayDiv else UI.empty))
            case Present(BoolValue.Const(false)) => Nil
            case Absent                          => Nil

        var el = div
            .cssClass("p-dataview")
            .cssClass("p-component")
            .cssClass(s"p-dataview-${layoutV.token}")
        el = loadingV.foldFlag(el)(el.cssClass("p-dataview-loading", _))
        el(
            (loadingOverlay ++ headerSlot ++ (contentEl :: paginatorUI) ++ footerSlot).map(toChild)*
        )
    end body
end DataView

object DataView:
    def apply[A](): DataView[A] = new DataView[A]()
