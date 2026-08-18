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
    itemF: Maybe[A => UI] = Absent,
    gridItemF: Maybe[A => UI] = Absent,
    layoutV: DataViewLayout = DataViewLayout.List,
    headerV: Maybe[UI] = Absent,
    footerV: Maybe[UI] = Absent,
    pageSizeV: Maybe[Int] = Absent,
    pageRef: Maybe[SignalRef[Int]] = Absent,
    emptyMessageV: Maybe[TextValue] = Absent,
    loadingV: Maybe[BoolValue] = Absent
) extends Node:
    type Self = DataView[A]

    /** Appends data items. */
    def items(is: Seq[A]): DataView[A] = copy(itemsV = itemsV ++ is.toList)

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

    /** Text shown in the content area when there are no items. */
    def emptyMessage(v: String): DataView[A] = copy(emptyMessageV = Present(TextValue.Const(v)))

    /** Reactive variant — re-renders the empty message in place on signal emission. */
    def emptyMessage(sig: Signal[String]): DataView[A] = copy(emptyMessageV = Present(TextValue.Dyn(sig)))

    /** Busy state: a spinner overlay (`.p-dataview-loading-overlay`) dims the
      * content while data is being fetched (PrimeReact's `loading`).
      */
    def loading(v: Boolean): DataView[A] = copy(loadingV = Present(BoolValue.Const(v)))

    /** Reactive busy state — bind to the data-fetch in-flight signal; the dimming overlay toggles in
      * its own sub-region and the `.p-dataview-loading` class swaps IN PLACE via the class channel
      * (no re-render of the data list).
      */
    def loading(sig: Signal[Boolean]): DataView[A] = copy(loadingV = Present(BoolValue.Dyn(sig)))

    private[uic] def render(using Frame): UI =
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

        val contentChildren: List[UI] =
            if paged.isEmpty then
                List(emptyMessageV.getOrElse(TextValue.Const("No records found")) match
                    case TextValue.Const(t) => div.cssClass("p-dataview-empty-message")(t): UI
                    case TextValue.Dyn(s)   => s.render(t => div.cssClass("p-dataview-empty-message")(t)))
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
