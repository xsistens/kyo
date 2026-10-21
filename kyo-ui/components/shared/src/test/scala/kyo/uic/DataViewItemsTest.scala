package kyo.uic.test

import kyo.*
import kyo.UI.*
import kyo.uic
import kyo.uic.UicTest
import scala.language.implicitConversions

/** Which of DataView's two item sources a build lands on, and what the rows become.
  *
  * `items(Seq)` is build-time data: the rows are plain children, and new data means a new component, so
  * every row is re-rendered. `items(Signal[Chunk])` makes the rows a kyo-ui list region — a
  * `UI.Ast.Foreach` — which reconciles per emission and leaves untouched rows alone; `itemKey` moves that
  * reconciliation from position to identity by filling the node's `key`.
  *
  * Both sources render the same markup for the same data, so the golden suite cannot tell them apart.
  * What is asserted here is therefore the FORM of the built tree: whether a `Foreach` is there, and
  * whether it carries a key. The third case is the honest limit — a paginated DataView slices at build
  * time, so a reactive source falls back to the rebuild path and no list region exists at all.
  */
class DataViewItemsTest extends UicTest:

    private def renderHtml(ui: UI)(using Frame): String < Async =
        UI.runRender(ui).take(1).run.map(_.mkString)

    private def view(using Frame): uic.DataView[String] =
        uic.DataView[String]().itemTemplate(s => span.cssClass("row")(s))

    "a reactive item source renders the rows as a list region" in {
        for
            ref <- Signal.initRef(Chunk("a", "b", "c"))
            tree = view.items(ref).toUI
            lists <- foreachNodes(tree)
            rows  <- elementsWithClass(tree, "row")
            html  <- renderHtml(tree)
        yield
            assert(lists.size == 1, s"the content holds exactly one list region, found ${lists.size}")
            assert(lists.head.key.isEmpty, "without itemKey the rows reconcile by position")
            assert(rows.size == 3, s"the region renders one row per item, found ${rows.size}")
            assert(html.contains("p-dataview-content"), "still Prime's content element")
            assert(html.contains("a") && html.contains("b") && html.contains("c"), "renders the current items")
    }

    "itemKey switches the list region from position to identity" in {
        for
            ref <- Signal.initRef(Chunk("a", "b"))
            tree = view.items(ref).itemKey(identity).toUI
            lists <- foreachNodes(tree)
        yield
            assert(lists.size == 1, "still exactly one list region")
            assert(lists.head.key.isDefined, "itemKey fills the region's key function")
    }

    "a build-time item source stays plain children — no list region" in {
        for
            tree  <- Kyo.lift(view.items(Seq("a", "b")).toUI)
            lists <- foreachNodes(tree)
            rows  <- elementsWithClass(tree, "row")
        yield
            assert(lists.isEmpty, s"a static list needs no region, found ${lists.size}")
            assert(rows.size == 2, "and renders one row per item as before")
    }

    "a paginated reactive source falls back to the build-time path" in {
        for
            ref  <- Signal.initRef(Chunk("a", "b", "c"))
            page <- Signal.initRef(0)
            tree = view.items(ref).itemKey(identity).paginate(2)(page).toUI
            lists <- foreachNodes(tree)
            rows  <- elementsWithClass(tree, "row")
            html  <- renderHtml(tree)
        yield
            // The documented limit: the page slice is arithmetic over the whole list, which a row region
            // cannot do, so the signal drives a plain region and itemKey has nothing to key.
            assert(lists.isEmpty, s"no list region under a paginator, found ${lists.size}")
            assert(rows.size == 2, s"the first page holds the page size, found ${rows.size}")
            assert(html.contains("p-dataview-paginator-bottom"), "and the paginator is still embedded")
    }

    "the two sources are exclusive: the last one set wins" in {
        for
            ref <- Signal.initRef(Chunk("live"))
            // signal after seq
            overriddenStatic = view.items(Seq("static")).items(ref).toUI
            // seq after signal
            overriddenSignal = view.items(ref).items(Seq("static")).toUI
            liveLists   <- foreachNodes(overriddenStatic)
            liveRows    <- elementsWithClass(overriddenStatic, "row")
            staticLists <- foreachNodes(overriddenSignal)
            staticRows  <- elementsWithClass(overriddenSignal, "row")
        yield
            assert(liveLists.size == 1, "items(Signal) after items(Seq) leaves a list region")
            assert(liveRows.size == 1, "and only the signal's items, not the dropped static ones")
            assert(staticLists.isEmpty, "items(Seq) after items(Signal) drops the signal")
            assert(staticRows.size == 1, "and renders only the static items")
    }

    "emptyContent survives a reactive source, in its own region over isEmpty" in {
        for
            ref <- Signal.initRef(Chunk.empty[String])
            tree = view.items(ref).emptyContent("Nothing yet").toUI
            lists <- foreachNodes(tree)
            html  <- renderHtml(tree)
            rows  <- elementsWithClass(tree, "row")
        yield
            assert(lists.size == 1, "the list region is built even while the list is empty")
            assert(rows.isEmpty, "an empty list renders no rows")
            assert(html.contains("p-dataview-empty-message"), "Prime's empty-message element is rendered")
            assert(html.contains("Nothing yet"), "with the caller's own empty text")
    }

    "a non-empty reactive source renders no empty message" in {
        for
            ref <- Signal.initRef(Chunk("a"))
            tree = view.items(ref).emptyContent("Nothing yet").toUI
            html <- renderHtml(tree)
        yield
            assert(!html.contains("p-dataview-empty-message"), "the empty region renders nothing while rows exist")
            assert(!html.contains("Nothing yet"), "and none of its text")
    }

    "the other slots are untouched by a reactive source" in {
        for
            ref <- Signal.initRef(Chunk("a"))
            tree = view.items(ref).header(span("H")).footer(span("F")).loading(true).toUI
            html <- renderHtml(tree)
        yield
            assert(html.contains("p-dataview-header"), "header slot")
            assert(html.contains("p-dataview-footer"), "footer slot")
            assert(html.contains("p-dataview-loading-overlay"), "loading overlay")
            assert(html.contains("p-dataview-list"), "layout class")
    }
end DataViewItemsTest
