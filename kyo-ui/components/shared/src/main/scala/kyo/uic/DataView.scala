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

/** DataView: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * DataView anatomy: `div.p-dataview.p-component.p-dataview-list|-grid` >
  * [`div.p-dataview-header`] + `div.p-dataview-content` + [the standalone
  * [[Paginator]] stamped `p-dataview-paginator-bottom`] +
  * [`div.p-dataview-footer`]), so the extracted `@primeuix` dataview CSS
  * applies verbatim.
  *
  * Items are TYPED: `items(Seq[A])` supplies the data, `itemTemplate` renders
  * each item in the list layout, `gridItemTemplate` (falling back to
  * `itemTemplate`) in the grid layout. Item layout inside the content is
  * template-owned, exactly like Prime. `layout` is a static enum; apps that
  * want a live list/grid switcher re-render with the other value (pair it
  * with a [[SelectButton]]). `paginate(rows)(page)` slices the items and
  * embeds the paginator over the 0-based page: a ref binds two way, a signal or
  * a constant one way, with the reader's pick reported to `onPageChange`.
  *
  * A list that GROWS (a stream of pages, a search that refines) supplies the
  * data as `items(Signal[Chunk[A]])` instead. The rows then become a kyo-ui list
  * region reconciled per emission, so an appended page renders the new cards and
  * leaves the ones already on screen alone; the build-time `Seq` form rebuilds
  * every card on every emission, because the whole component is re-created to
  * carry the new data. [[itemKey]] moves that reconciliation from position to
  * identity, which is what a reorder or a mid-list insertion needs.
  *
  * `loading(true)` renders PrimeReact's busy state: the sheet's
  * `.p-dataview-loading-overlay` (composed with `.p-overlay-mask` for the
  * dimmed backdrop) over the content, holding a [[ProgressSpinner]]; the
  * root's `position: relative` anchor is a `.p-uic-*` remainder rule keyed on
  * `.p-dataview-loading` (PrimeReact's root modifier).
  *
  * Sorting is deliberately NOT a component prop (Prime's `sortField`/
  * `sortOrder` re-order internally): sort the `Seq` before `items(...)`: the
  * template owns row layout either way, and a plain `sortBy` keeps the data
  * flow visible.
  */
final class DataView[A] private[uic] (private[uic] val s: DataView.State[A]) extends Node, HasEmptyContent, Stated:
    import s.*

    type Self = DataView[A]

    /** Appends data items, fixed at build time. Clears a previously set items SIGNAL: the two sources are
      * exclusive and the last one set wins, because a component that silently merged a static block with a
      * live one would have no answer for what an emission does to the static rows.
      */
    def items(is: Seq[A]): DataView[A] = new DataView(s.copy(itemsV = itemsV ++ is.toList, itemsSigV = Absent))

    /** Reactive items: the list is carried as a signal and the rows are reconciled per emission through
      * kyo-ui's list region (`Signal.foreach`), instead of the whole content being rebuilt.
      *
      * This is the form for data that arrives over time: a stream of pages, a query that refines. With the
      * build-time [[items(Seq)]] form an emission means rebuilding the component, so every card is
      * re-rendered even though only the tail changed; here an appended page renders the appended rows and
      * leaves the rows already on screen untouched. Without [[itemKey]] rows are matched by POSITION, which
      * is right for appends and wrong for reorders and mid-list insertions.
      *
      * Two limits worth knowing before reaching for it. Clears a previously set static list (the sources are
      * exclusive, last one wins), and a [[paginate]]d DataView falls back to the build-time path: the page
      * slice is build-time arithmetic over the whole list, so the rows cannot be a list region at all; the
      * signal then drives a plain region around the component, which is the same cost as rebuilding.
      */
    def items(sig: Signal[Chunk[A]]): DataView[A] = new DataView(s.copy(itemsSigV = Present(sig), itemsV = Nil))

    /** A stable key per row, which switches reactive reconciliation from position to identity
      * (`Signal.foreachKeyed`): a row whose key survives a list change keeps its live subscriptions and its
      * DOM instead of being re-rendered into the position it moved to.
      *
      * Only the [[items(Signal)]] source reconciles, so this has no effect on a static list or on a
      * paginated one. The key must be an actual identity for the data: kyo-ui warns on duplicates, and a
      * key that collides applies a row's update to the wrong record.
      */
    def itemKey(f: A => String): DataView[A] = new DataView(s.copy(itemKeyF = Present(f)))

    /** Item template for the list layout (and the grid fallback). */
    def itemTemplate(f: A => UI): DataView[A] = new DataView(s.copy(itemF = Present(f)))

    /** Item template for the grid layout (falls back to [[itemTemplate]]). */
    def gridItemTemplate(f: A => UI): DataView[A] = new DataView(s.copy(gridItemF = Present(f)))

    /** Presentation mode: `List` (default) or `Grid`. */
    def layout(v: DataViewLayout): DataView[A] = new DataView(s.copy(layoutV = v))

    /** Header slot (`div.p-dataview-header`). */
    def header(ui: UI): DataView[A] = new DataView(s.copy(headerV = Present(ui)))

    /** Footer slot (`div.p-dataview-footer`). */
    def footer(ui: UI): DataView[A] = new DataView(s.copy(footerV = Present(ui)))

    /** Slices the items into pages of `size` and embeds the [[Paginator]]; `page` is the 0-based page index (clamped at
      * render). A `SignalRef` binds two way: a page click writes it. A plain `Signal` or a constant binds one way: the view
      * shows that page and hands the reader's pick to [[onPageChange]] without writing.
      */
    def paginate(size: Int)(page: Int | Signal[Int]): DataView[A] =
        new DataView(s.copy(pageSizeV = Present(math.max(1, size)), pageSlot = pageSlot.bind(page)))

    /** Slices the items into pages of `size` with the page the view's own, starting on the first: [[paginate]] for a page
      * nobody else reads. [[onPageChange]] still hears every pick.
      */
    def paginate(size: Int): DataView[A] = new DataView(s.copy(pageSizeV = Present(math.max(1, size))))

    /** Fired with the page the reader asked for, after a two-way [[paginate]] page is written. */
    def onPageChange(f: Int => UI.Action)(using Frame): DataView[A] = new DataView(s.copy(pageSlot = pageSlot.hear(f)))

    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): DataView[A] = new DataView(s.copy(emptyContentV = v))

    /** Busy state: a spinner overlay (`.p-dataview-loading-overlay`) dims the content while data is being
      * fetched (PrimeReact's `loading`). Bind a `Signal[Boolean]` to the data-fetch in-flight signal: the
      * overlay toggles in its own sub-region and the `.p-dataview-loading` class swaps IN PLACE via the
      * class channel, with no re-render of the data list.
      */
    def loading(v: Boolean | Signal[Boolean]): DataView[A] = new DataView(s.copy(loadingV = Present(ReactiveValue(v))))

    private given CanEqual[Chunk[A], Chunk[A]] = CanEqual.derived

    private[uic] def render(using Frame): UI =
        // Paginated with nothing bound, the page is the view's own.
        Slot.owning(
            pageSlot,
            start = 0,
            on = ReactiveValue.Const(pageSizeV.isDefined)
        )(s => new DataView(this.s.copy(pageSlot = s)).drawn)

    /** Only the rows follow the page and the items. The header, the footer and the paginator are built once: each may
      * hold a control with a mount of its own (a sort SelectButton in the header, the paginator's dropdowns), and a
      * region around it would start that mount again on every page change. The paginator reads the page from the ref
      * and the total from the items, both in place.
      */
    private def drawn(using Frame): UI =
        val page: ReactiveValue[Int] = pageSlot.reading(0)

        // A list region; the empty state gets its OWN region over the narrowest projection there is (`isEmpty`), so
        // `emptyContent` survives without putting the rows behind a region that repaints them whenever the list changes
        // length.
        def listed(sig: Signal[Chunk[A]]): UI =
            val rows: List[UI] = template.toList.map { f =>
                itemKeyF match
                    case Present(key) => sig.foreachKeyed(key)(f)
                    case Absent       => sig.foreach(f)
            }
            div.cssClass("p-dataview-content")(
                UI.children(rows :+ sig.map(_.isEmpty).render(isEmpty => if isEmpty then emptyMessage else UI.empty))*
            )
        end listed

        val content: UI = itemsSigV.fold(
            pageSizeV.fold(contentEl(itemsV))(size => page.render(p => contentEl(slice(itemsV, p, size))))
        ) { sig =>
            pageSizeV.fold(listed(sig))(size =>
                ReactiveValue.Dyn(sig).and(page).render((items, p) => contentEl(slice(items.toList, p, size)))
            )
        }

        val paginatorUI: List[UI] = pageSizeV.toList.map { size =>
            var pag = Paginator()
                .totalRecords(itemsSigV.fold(itemsV.size: Int | Signal[Int])(_.map(_.size)))
                .rows(size)
                .hostClass("p-dataview-paginator-bottom")
            pageSlot.binding.foreach(p => pag = pag.hostPage(p, pageSlot.hearer))
            pag.render
        }

        val headerSlot: List[UI] = headerV.toList.map(h => div.cssClass("p-dataview-header")(h))
        val footerSlot: List[UI] = footerV.toList.map(f => div.cssClass("p-dataview-footer")(f))

        // PrimeReact's busy state: the sheet's absolute overlay composed with the
        // dimming .p-overlay-mask (position resolves to absolute: the dataview rule
        // wins over the mask's fixed), holding our ProgressSpinner.
        def overlayDiv: UI =
            div
                .cssClass("p-dataview-loading-overlay")
                .cssClass("p-overlay-mask")(
                    ProgressSpinner().size(Size.Small).accessibleName("Loading").render
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
            UI.children(loadingOverlay ++ headerSlot ++ (content :: paginatorUI) ++ footerSlot)*
        )
    end drawn

    /** The rows of page `page` (clamped into the page count) of `items`, `size` to a page. */
    private def slice(items: List[A], page: Int, size: Int): List[A] =
        val totalPages = math.max(1, (items.size + size - 1) / size)
        val cur        = math.min(math.max(page, 0), totalPages - 1)
        items.slice(cur * size, cur * size + size)
    end slice

    // Grid prefers its own template; both layouts fall back to the other's.
    private def template: Maybe[A => UI] = layoutV match
        case DataViewLayout.Grid => gridItemF.orElse(itemF)
        case DataViewLayout.List => itemF.orElse(gridItemF)

    private def emptyMessage(using Frame): UI =
        EmptyContent.render(emptyContentV, "No records found")(c => div.cssClass("p-dataview-empty-message")(c))

    private def contentEl(rows: List[A])(using Frame): UI =
        val children: List[UI] =
            if rows.isEmpty then List(emptyMessage)
            else template.toList.flatMap(f => rows.map(f))
        div.cssClass("p-dataview-content")(UI.children(children)*)
    end contentEl
end DataView

object DataView:
    final private[uic] case class State[A](
        itemsV: List[A] = Nil,
        itemsSigV: Maybe[Signal[Chunk[A]]] = Absent,
        itemKeyF: Maybe[A => String] = Absent,
        itemF: Maybe[A => UI] = Absent,
        gridItemF: Maybe[A => UI] = Absent,
        layoutV: DataViewLayout = DataViewLayout.List,
        headerV: Maybe[UI] = Absent,
        footerV: Maybe[UI] = Absent,
        pageSizeV: Maybe[Int] = Absent,
        pageSlot: Slot[Int] = Slot(),
        emptyContentV: Maybe[EmptyContent] = Absent,
        loadingV: Maybe[BoolValue] = Absent
    )

    def apply[A](): DataView[A] = new DataView[A](State())
end DataView
