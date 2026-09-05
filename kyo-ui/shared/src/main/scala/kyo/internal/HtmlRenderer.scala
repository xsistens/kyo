package kyo.internal

import kyo.*
import kyo.Svg
import kyo.UI.*
import kyo.UI.Ast.*

private[kyo] object HtmlRenderer:

    // className -> rule CSS text, accumulated during a renderWithCss traversal. Threaded through
    // renderTo/renderCommonAttrs/renderDropdown* the same way `sb` carries the HTML, so collection
    // costs nothing on the render(...) path (cssRules stays Absent there).
    private type CssCollector = scala.collection.mutable.LinkedHashMap[String, String]

    /** Render a UI tree to HTML with data-kyo-path attributes. */
    def render(ui: UI, path: Seq[String])(using Frame): String < Sync =
        val sb = new StringBuilder
        // Every descendant extends the path with `:+`, and `Seq.empty` is a List, whose `:+` copies the
        // whole prefix per node. Normalizing once at the entry makes those appends Vector appends; paths
        // are only ever built, compared and joined, and Seq equality holds across both.
        val root = path.toVector
        renderTo(
            sb,
            ui,
            root,
            ReactiveRegion.RegionIdentity.root(root),
            ReactiveRegion.Namespace.Html,
            parentContext = ReactiveRegion.ParentContext.Other,
            boundaryMode = ReactiveRegion.BoundaryMode.Emit
        ).andThen(sb.toString)
    end render

    private[kyo] def renderRegion(ui: UI, path: Seq[String])(using Frame): String < Sync =
        renderRegion(ui, path, ReactiveRegion.RegionIdentity.root(path))

    private[kyo] def renderRegion(ui: UI, path: Seq[String], context: ReactiveRegion.RegionIdentity)(using Frame): String < Sync =
        renderRegion(
            ui,
            path,
            context,
            ReactiveRegion.from(context, svgContext = false),
            ReactiveRegion.ParentContext.Other,
            ReactiveRegion.BoundaryMode.Suppress
        )

    /** Whether `ui` paints as exactly ONE element carrying its own `data-kyo-path`, which is what makes a row
      * addressable by key.
      *
      * A structural command names rows BY KEY, so a row that paints as several roots or as none has nothing for a
      * key to name: a fragment paints its children under sub-paths, text and raw HTML carry no path at all, and a
      * region paints a marker pair whose identity is its range id rather than the row path. Answering this without
      * rendering is what lets the gate sit before the op is chosen, which a wire needs: the in-process path can
      * look at the live DOM and change its mind, but once the untouched rows are left out of a frame the client
      * has nothing to rebuild them from.
      *
      * A total match with no `case _`: a new UI node has to be answered here as well as in [[renderTo]], and a
      * default would answer it silently and wrongly.
      */
    private[kyo] def paintsAsKeyedRoot(ui: UI): Boolean =
        ui match
            case KeyedChild(_, child) => paintsAsKeyedRoot(child)
            case _: Element           => true
            case _: Fragment[?]       => false
            case _: Text              => false
            case _: RawHtml           => false
            case _: Reactive[?]       => false
            case _: Foreach[?, ?]     => false
            case _: Mounted           => false
    end paintsAsKeyedRoot

    /** Render ONE keyed row of a list region, exactly as the region's own render would have placed it.
      *
      * A list patch renders the rows that changed and nothing else, so it needs the row alone rather than the
      * region around it: same path (`path :+ key`), same identity (`context.child(key)`), same namespace and
      * parent context the enclosing host imposes, and no boundary markers of the region's own. What comes back is
      * therefore byte-identical to the slice the whole-list render would have produced for that row, which is
      * what lets the reconciliation match it against the live row by key.
      */
    private[kyo] def renderRow(
        ui: UI,
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity,
        host: ReactiveRegion.RenderHost
    )(using Frame): String < Sync =
        renderRowInto(ui, path, context, host, Absent).map(_._1)

    /** [[renderRow]] plus the pseudo-state rules the row introduced, for the transport that has to ship them. */
    private[kyo] def renderRowWithCss(
        ui: UI,
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity,
        host: ReactiveRegion.RenderHost
    )(using Frame): (String, Chunk[(String, String)]) < Sync =
        val css = new CssCollector
        renderRowInto(ui, path, context, host, Present(css)).map((html, _) => (html, Chunk.from(css)))
    end renderRowWithCss

    private def renderRowInto(
        ui: UI,
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity,
        host: ReactiveRegion.RenderHost,
        cssRules: Maybe[CssCollector]
    )(using Frame): (String, Unit) < Sync =
        val sb = new StringBuilder
        val (namespace, parentContext) = host match
            case _: ReactiveRegion.RenderHost.HtmlTableBody =>
                (ReactiveRegion.Namespace.Html, ReactiveRegion.ParentContext.Other)
            case other => (ReactiveRegion.namespace(other), ReactiveRegion.contentParent(other))
        renderTo(
            sb,
            ui,
            path.toVector,
            context,
            namespace,
            cssRules,
            parentContext,
            ReactiveRegion.BoundaryMode.Emit
        ).andThen((sb.toString, ()))
    end renderRowInto

    private[kyo] def renderRegion(
        ui: UI,
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity,
        region: ReactiveRegion,
        parentContext: ReactiveRegion.ParentContext,
        boundaryMode: ReactiveRegion.BoundaryMode
    )(using Frame): String < Sync =
        val sb   = new StringBuilder
        val host = ReactiveRegion.renderHost(region, parentContext, ReactiveRegion.tableContent(ui))
        renderHostedContent(sb, ui, path, context, host, boundaryMode).andThen(sb.toString)
    end renderRegion

    /** Render a UI tree to HTML, additionally collecting the CSS rule(s) for every pseudo-state
      * (hover/focus/active/disabled) [[kyo.Style]] encountered along the way.
      *
      * Used by the server-push runtime (`kyo.internal.UIServer`), which has no inline-style channel
      * for pseudo-states: an inline `style="..."` attribute cannot express `:hover` etc., so the
      * collected rules must be carried in a real stylesheet alongside the HTML instead. Each entry is
      * `(stableClass, ruleCss)`; an element whose pseudo-state style produces the SAME rule text as one
      * already collected shares its class rather than generating a new one (see
      * [[kyo.internal.CssStyleRenderer.pseudoStateClass]]), so the result has one entry per distinct
      * rule, in first-encountered order.
      */
    private[kyo] def renderWithCss(ui: UI, path: Seq[String])(using Frame): (String, Chunk[(String, String)]) < Sync =
        val sb   = new StringBuilder
        val css  = new CssCollector
        val root = path.toVector
        renderTo(
            sb,
            ui,
            root,
            ReactiveRegion.RegionIdentity.root(root),
            ReactiveRegion.Namespace.Html,
            cssRules = Present(css),
            parentContext = ReactiveRegion.ParentContext.Other,
            boundaryMode = ReactiveRegion.BoundaryMode.Emit
        )
            .andThen((sb.toString, Chunk.from(css)))
    end renderWithCss

    private[kyo] def renderRegionWithCss(ui: UI, path: Seq[String])(using
        Frame
    ): (String, Chunk[(String, String)]) < Sync =
        renderRegionWithCss(ui, path, ReactiveRegion.RegionIdentity.root(path))

    private[kyo] def renderRegionWithCss(
        ui: UI,
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity
    )(using Frame): (String, Chunk[(String, String)]) < Sync =
        renderRegionWithCss(
            ui,
            path,
            context,
            ReactiveRegion.from(context, svgContext = false),
            ReactiveRegion.ParentContext.Other,
            ReactiveRegion.BoundaryMode.Suppress
        )

    private[kyo] def renderRegionWithCss(
        ui: UI,
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity,
        region: ReactiveRegion,
        parentContext: ReactiveRegion.ParentContext,
        boundaryMode: ReactiveRegion.BoundaryMode
    )(using Frame): (String, Chunk[(String, String)]) < Sync =
        val sb       = new StringBuilder
        val css      = new CssCollector
        val host     = ReactiveRegion.renderHost(region, parentContext, ReactiveRegion.tableContent(ui))
        val rendered = renderHostedContent(sb, ui, path, context, host, boundaryMode, Present(css))
        rendered.andThen((sb.toString, Chunk.from(css)))
    end renderRegionWithCss

    private def renderHostedContent(
        sb: StringBuilder,
        ui: UI,
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity,
        host: ReactiveRegion.RenderHost,
        boundaryMode: ReactiveRegion.BoundaryMode,
        cssRules: Maybe[CssCollector] = Absent
    )(using Frame): Unit < Sync =
        host match
            case ReactiveRegion.RenderHost.HtmlTableBody(id) =>
                w(sb, s"<tbody data-kyo-range-host=\"$id\">")
                renderTo(
                    sb,
                    ui,
                    path,
                    context,
                    ReactiveRegion.Namespace.Html,
                    cssRules,
                    ReactiveRegion.ParentContext.Other,
                    boundaryMode
                ).andThen(w(sb, "</tbody>"))
            case host =>
                renderTo(
                    sb,
                    ui,
                    path,
                    context,
                    ReactiveRegion.namespace(host),
                    cssRules,
                    ReactiveRegion.contentParent(host),
                    boundaryMode
                )
    end renderHostedContent

    private[kyo] def wrapReactiveRegion(region: ReactiveRegion, innerHtml: String): String =
        region match
            case ReactiveRegion.HtmlRange(id) => s"<!--kyo-rs:$id-->$innerHtml<!--kyo-re:$id-->"
            case ReactiveRegion.SvgElement(path) =>
                s"""<g data-kyo-path="${pathAttr(path)}" data-kyo-reactive>$innerHtml</g>"""

    private def openSvgRegion(path: Seq[String]): String =
        s"""<g data-kyo-path="${pathAttr(path)}" data-kyo-reactive>"""

    /** Wrap body HTML in a full page with inline JS client.
      *
      * The devtools overlay rides along in a second `<script>` when one is registered, AFTER the client: the
      * client is what receives the stats frames and hands them over, so the overlay has to be defined by the
      * time the socket opens. Nothing is emitted when no overlay is registered, which is every page in a build
      * that does not depend on kyo-ui-devtools.
      */
    def renderPage(title: String, body: String, css: String, basePath: String): String =
        val devtools = Devtools.pageScript match
            case Present(script) => s"\n<script>$script</script>"
            case Absent          => ""
        s"""<!DOCTYPE html>
           |<html>
           |<head>
           |<meta charset="UTF-8">
           |<title>${esc(title)}</title>
           |<style>$baseCss$css</style>
           |</head>
           |<body>$body
           |<script>${clientJs(jsStr(basePath))}</script>$devtools
           |</body>
           |</html>""".stripMargin
    end renderPage

    /** Wrap body HTML in a complete static HTML document with a configurable head (for SSG/SSR).
      *
      * Unlike `renderPage` (which injects the SSE client JS for server-push), this helper emits a
      * clean static document with an optional module script: the caller's bundle or nothing. The
      * `baseCss` reset is always emitted before `head.css` so framework defaults can be overridden.
      * Called by `UI.runRenderPage`.
      */
    private[kyo] def page(head: UI.PageHead, body: String): String =
        val metaTags = head.meta.map((n, c) => s"""<meta name="${esc(n)}" content="${esc(c)}">""").mkString
        val linkTags = head.links.map((r, h) => s"""<link rel="${esc(r)}" href="${esc(h)}">""").mkString
        val script = head.moduleScript match
            case Present(src) => s"""<script type="module" src="${esc(src)}"></script>"""
            case Absent       => ""
        val ldBlock = head.jsonLd match
            case Present(di) => renderDataIsland(di)
            case Absent      => ""
        val islands = head.dataIslands.map(renderDataIsland).mkString
        s"""<!DOCTYPE html>
           |<html lang="en">
           |<head>
           |<meta charset="utf-8">
           |<meta name="viewport" content="width=device-width, initial-scale=1">
           |<title>${esc(head.title)}</title>
           |$metaTags$linkTags
           |<style>$baseCss${head.css}</style>
           |$ldBlock</head>
           |<body>$body$islands</body>
           |$script
           |</html>""".stripMargin
    end page

    // Render a data island as `<script type="..."[ id="..."]>ESCAPED-JSON</script>`. The type
    // and id attributes use the HTML-entity escape (`esc`); the JSON body uses the JS-unicode
    // escape (`escScript`) so a `</script>` substring renders as `</script>`, inert
    // text the consumer's JSON.parse still reads, rather than the HTML-entity form `&lt;` that
    // would change the bytes and break JSON.parse on read-back.
    private def renderDataIsland(di: UI.DataIsland): String =
        val idAttr = di.id match
            case Present(v) => s""" id="${esc(v)}""""
            case Absent     => ""
        s"""<script type="${esc(di.scriptType)}"$idAttr>${escScript(di.json)}</script>"""
    end renderDataIsland

    // The single owner of the data-island body escape: a literal "</script>" in the JSON body
    // would close the element early, so "<"/">" become their JSON unicode escapes. This is the
    // JS-unicode form ("<"/">"), NOT the HTML-entity esc(...) form, because the body
    // is JSON read back by JSON.parse, not HTML re-parsed.
    private def escScript(json: String): String =
        json.replace("<", "\\u003c").replace(">", "\\u003e")

    // ---- Core rendering ----

    private def renderTo(
        sb: StringBuilder,
        ui: UI,
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity,
        namespace: ReactiveRegion.Namespace,
        cssRules: Maybe[CssCollector] = Absent,
        parentContext: ReactiveRegion.ParentContext,
        boundaryMode: ReactiveRegion.BoundaryMode
    )(using
        Frame
    ): Unit < Sync =
        ui match
            case dd: Dropdown =>
                renderBoundElementBoundary(sb, dd, context, namespace, parentContext, boundaryMode) {
                    renderDropdown(sb, dd, path, cssRules)
                }
            case elem: Element =>
                val tag  = tagName(elem)
                val void = elem.isInstanceOf[Void]
                renderBoundElementBoundary(sb, elem, context, namespace, parentContext, boundaryMode) {
                    // Reactive classes currently true, folded into the class list so SSR is correct. Empty when
                    // none are bound, so the `class` attribute is byte-identical there.
                    def openTag(extraClasses: Seq[String]): Unit =
                        w(sb, s"""<$tag data-kyo-path="${pathAttr(path)}"""")
                        renderCommonAttrs(
                            sb,
                            if extraClasses.isEmpty then elem.attrs
                            else elem.attrs.copy(cssClasses = elem.attrs.cssClasses ++ extraClasses),
                            elem.isInstanceOf[Svg.SvgElement],
                            cssRules
                        )
                        renderEventAttr(sb, elem)
                    end openTag
                    // Reading a reactive class is an effect, so only an element that binds one suspends before its
                    // tag is written. Every other element keeps the open tag on the eager path, which is what makes
                    // an attribute the renderer rejects (an oversized drag config, say) fail where the render is
                    // built rather than where it is later run.
                    val opened: Unit < Sync =
                        if elem.attrs.reactiveClasses.isEmpty then openTag(Seq.empty)
                        else reactiveTrueClasses(elem.attrs).map(openTag)
                    for
                        _ <- opened
                        _ <- renderElementAttrs(sb, elem)
                        _ <- renderReactiveAttrs(sb, elem.attrs)
                        _ <- renderReactiveBoolAttrs(sb, elem.attrs)
                    yield
                        if void then
                            w(sb, " />")
                            elem match
                                case ta: Textarea =>
                                    sb.delete(sb.length - 3, sb.length)
                                    w(sb, ">")
                                    renderTextareaValue(sb, ta).andThen(w(sb, "</textarea>"))
                                case _: Iframe =>
                                    // iframe is not a void element: it needs an explicit closing tag.
                                    sb.delete(sb.length - 3, sb.length)
                                    w(sb, "></iframe>")
                                case _ => ()
                            end match
                        else
                            w(sb, ">")
                            // ForeignObject bridges back to HTML, so reset svg context to false. It MUST be
                            // matched before SvgElement (ForeignObject IS an SvgElement).
                            val childNamespace = elem match
                                case _: Svg.ForeignObject => ReactiveRegion.Namespace.Html
                                case _: Svg.SvgElement    => ReactiveRegion.Namespace.Svg
                                case _                    => namespace
                            val childParentContext = elem match
                                case _: Table => ReactiveRegion.ParentContext.HtmlTable
                                case _        => ReactiveRegion.ParentContext.Other
                            val textChild: Unit < Sync = elem match
                                case t: Svg.Title => w(sb, esc(t.text)); Kyo.unit
                                case d: Svg.Desc  => w(sb, esc(d.text)); Kyo.unit
                                case _            => Kyo.unit
                            textChild.andThen(
                                // foreachIndexedDiscard takes the Chunk and the index directly: `toSeq.zipWithIndex`
                                // built one tuple per child and a Seq, which foreachDiscard then copied back into a
                                // Chunk, three allocations per node on a render that runs once per node.
                                Kyo.foreachIndexedDiscard(elem.children) { (i, child) =>
                                    renderTo(
                                        sb,
                                        child,
                                        path :+ i.toString,
                                        context.child(i.toString),
                                        childNamespace,
                                        cssRules,
                                        childParentContext,
                                        ReactiveRegion.BoundaryMode.Emit
                                    )
                                }.andThen(w(sb, s"</$tag>"))
                            )
                        end if
                    end for
                }

            case UI.Ast.RawHtml(value) =>
                w(sb, value)

            case UI.Ast.Text(value) =>
                w(sb, esc(value))

            case Fragment(children) =>
                // Use key for KeyedChild, index for everything else. This matches the path scheme
                // walkStatic uses, so server-side event routing aligns with rendered data-kyo-path.
                Kyo.foreachIndexedDiscard(children) { (i, child) =>
                    val childPath = child match
                        case kc: KeyedChild[?] => path :+ kc.key
                        case _                 => path :+ i.toString
                    val childContext = child match
                        case kc: KeyedChild[?] => context.child(kc.key)
                        case _                 => context.child(i.toString)
                    renderTo(sb, child, childPath, childContext, namespace, cssRules, parentContext, ReactiveRegion.BoundaryMode.Emit)
                }

            case KeyedChild(_, child) =>
                renderTo(sb, child, path, context, namespace, cssRules, parentContext, boundaryMode)

            case r: Reactive[?] =>
                val region = ReactiveRegion.from(context, namespace)
                for current <- r.signal.current(using r.frame)
                yield
                    val host = ReactiveRegion.renderHost(region, parentContext, ReactiveRegion.tableContent(current))
                    openInitialHost(sb, host)
                    renderTo(
                        sb,
                        current,
                        path,
                        context.transparent,
                        ReactiveRegion.namespace(host),
                        cssRules,
                        ReactiveRegion.contentParent(host),
                        ReactiveRegion.BoundaryMode.Emit
                    ).andThen(closeInitialHost(sb, host))
                end for

            case fe: Foreach[?, ?] @unchecked =>
                val region = ReactiveRegion.from(context, namespace)
                fe.applyTyped {
                    [T] =>
                        (signal, keyFn, renderFn) =>
                            for items <- signal.current(using fe.frame)
                            yield
                                val rendered = items.toSeq.zipWithIndex.map { (item, i) =>
                                    val key = keyFn match
                                        case Present(f) => f(item)
                                        case Absent     => i.toString
                                    (key, renderFn(i, item))
                                }
                                val content = ReactiveRegion.tableContent(rendered.iterator.map(_._2))
                                val host    = ReactiveRegion.renderHost(region, parentContext, content)
                                openInitialHost(sb, host)
                                Kyo.foreachDiscard(rendered) { (key, child) =>
                                    renderTo(
                                        sb,
                                        child,
                                        path :+ key,
                                        context.child(key),
                                        ReactiveRegion.namespace(host),
                                        cssRules,
                                        ReactiveRegion.contentParent(host),
                                        ReactiveRegion.BoundaryMode.Emit
                                    )
                                }.andThen(closeInitialHost(sb, host))
                            end for
                }

            case m: Mounted =>
                // Static/SSG projection of a mount is its placeholder; live, ReactiveUI.subscribeMounted patches
                // the region when the node's cell publishes (synchronously, before paint, for an adopted keyed instance).
                val region      = ReactiveRegion.from(context, namespace)
                val placeholder = m.placeholderUI.getOrElse(UI.empty(using m.frame))
                val host        = ReactiveRegion.renderHost(region, parentContext, ReactiveRegion.tableContent(placeholder))
                openInitialHost(sb, host, ReactiveRegion.mountSlotFlags(m.key))
                renderTo(
                    sb,
                    placeholder,
                    path,
                    context.transparent,
                    ReactiveRegion.namespace(host),
                    cssRules,
                    ReactiveRegion.contentParent(host),
                    ReactiveRegion.BoundaryMode.Emit
                ).andThen(closeInitialHost(sb, host))
    end renderTo

    private def renderBoundElementBoundary(
        sb: StringBuilder,
        elem: Element,
        context: ReactiveRegion.RegionIdentity,
        namespace: ReactiveRegion.Namespace,
        parentContext: ReactiveRegion.ParentContext,
        boundaryMode: ReactiveRegion.BoundaryMode
    )(render: => Unit < Sync)(using Frame): Unit < Sync =
        val host = (namespace, boundaryMode, ReactiveUI.collectSignalRef(elem)) match
            case (ReactiveRegion.Namespace.Html, ReactiveRegion.BoundaryMode.Emit, Present(_)) =>
                Present(
                    ReactiveRegion.renderHost(
                        ReactiveRegion.HtmlRange(ReactiveRegion.htmlId(context)),
                        parentContext,
                        ReactiveRegion.tableContent(elem)
                    )
                )
            case _ => Absent
        host.foreach(openInitialHost(sb, _))
        render.andThen(host.foreach(closeInitialHost(sb, _)))
    end renderBoundElementBoundary

    /** Open a region's host. `flags` is the marker's flag section, empty for every region but a mount slot (see
      * [[ReactiveRegion.mountSlotFlags]]); an SVG region has an element rather than markers and carries none.
      */
    private def openInitialHost(sb: StringBuilder, host: ReactiveRegion.RenderHost, flags: String = ""): Unit =
        host match
            case ReactiveRegion.RenderHost.HtmlComments(id, _) => w(sb, s"<!--kyo-rs:$id$flags-->")
            case ReactiveRegion.RenderHost.HtmlTableBody(id) =>
                w(sb, s"<tbody data-kyo-range-host=\"$id\"><!--kyo-rs:$id$flags-->")
            case ReactiveRegion.RenderHost.SvgGroup(path) => w(sb, openSvgRegion(path))

    private def closeInitialHost(sb: StringBuilder, host: ReactiveRegion.RenderHost): Unit =
        host match
            case ReactiveRegion.RenderHost.HtmlComments(id, _) => w(sb, s"<!--kyo-re:$id-->")
            case ReactiveRegion.RenderHost.HtmlTableBody(id)   => w(sb, s"<!--kyo-re:$id--></tbody>")
            case _: ReactiveRegion.RenderHost.SvgGroup         => w(sb, "</g>")

    private def renderTextareaValue(sb: StringBuilder, ta: Textarea)(using Frame): Unit < Sync =
        ta.value match
            case Present(Bound.Const(s)) => w(sb, esc(masked(ta.inputMask, s)))
            case Present(Bound.Ref(ref)) =>
                for str <- ref.get
                yield w(sb, esc(masked(ta.inputMask, str)))
            case _ => ()

    // ---- Dropdown (custom div-based overlay) ----

    private def renderDropdown(sb: StringBuilder, dd: Dropdown, path: Seq[String], cssRules: Maybe[CssCollector])(using
        Frame
    ): Unit < Sync =
        val baseId = dd.attrs.identifier.getOrElse("")
        // Read current selected value for initial highlight
        val currentValueEffect: Unit < Sync = dd.value match
            case Present(Bound.Ref(ref)) =>
                ref.get.map { currentVal =>
                    renderDropdownWithValue(sb, dd, path, baseId, currentVal, cssRules)
                }
            case Present(Bound.Const(s)) =>
                renderDropdownWithValue(sb, dd, path, baseId, s, cssRules)
            case _ =>
                renderDropdownWithValue(sb, dd, path, baseId, "", cssRules)
        currentValueEffect
    end renderDropdown

    private def renderDropdownWithValue(
        sb: StringBuilder,
        dd: Dropdown,
        path: Seq[String],
        baseId: String,
        currentVal: String,
        cssRules: Maybe[CssCollector]
    )(using
        Frame
    ): Unit =
        val pathStr     = pathAttr(path)
        val idAttr      = if baseId.nonEmpty then s""" id="${esc(baseId)}"""" else ""
        val ddAttr      = if baseId.nonEmpty then s""" data-kyo-dropdown="${esc(baseId)}"""" else " data-kyo-dropdown"
        val disAttr     = if dd.disabled.getOrElse(false) then " data-kyo-disabled" else ""
        val hidAttr     = if dd.attrs.hidden.getOrElse(false) then " hidden" else ""
        val tabAttr     = dd.attrs.tabIndex.map(n => s""" tabindex="$n"""").getOrElse("")
        val pseudoClass = registerPseudoClass(cssRules, dd.attrs.uiStyle)
        // A generated pseudoClass already carries the base props in its own rule (see
        // registerPseudoClass); rendering them inline too would shadow the pseudo-state override.
        val styleStr = if pseudoClass.nonEmpty then ""
        else
            val styleAttr = CssStyleRenderer.render(dd.attrs.uiStyle)
            if styleAttr.nonEmpty then s""" style="${esc(styleAttr)}"""" else ""
        // The dropdown wrapper carries its cssClasses (the same `.cssClass(...)` hook every other
        // element honors), plus the generated pseudoClass when present, so callers can style the
        // trigger container via a class selector.
        val classes = pseudoClass match
            case Present(cls) => dd.attrs.cssClasses :+ cls
            case Absent       => dd.attrs.cssClasses
        val clsStr = if classes.nonEmpty then s""" class="${esc(classes.mkString(" "))}"""" else ""
        // Determine initial trigger label
        val firstLabel    = dd.options.headMaybe.map(_._1).getOrElse("")
        val currentLabel  = Maybe.fromOption(dd.options.toSeq.find(_._2 == currentVal)).map(_._1).getOrElse(firstLabel)
        val triggerLabel  = esc(if currentLabel.nonEmpty then s"$currentLabel ▾" else "▾")
        val triggerId     = if baseId.nonEmpty then s""" id="${esc(baseId + "-trigger")}"""" else ""
        val optionsId     = if baseId.nonEmpty then s""" id="${esc(baseId + "-options")}"""" else ""
        val triggerDdAttr = if baseId.nonEmpty then s""" data-kyo-dropdown-trigger="${esc(baseId)}"""" else ""
        val optionsDdAttr = if baseId.nonEmpty then s""" data-kyo-dropdown-options="${esc(baseId)}"""" else ""
        // Wrapper div
        w(
            sb,
            s"""<div data-kyo-path="$pathStr"$idAttr$clsStr$ddAttr data-kyo-ev="click,keydown,change"$hidAttr$disAttr$tabAttr$styleStr>"""
        )
        // Trigger button
        w(sb, s"""<button$triggerId type="button"$triggerDdAttr tabindex="0">$triggerLabel</button>""")
        // Options container (hidden by default)
        w(sb, s"""<div$optionsId$optionsDdAttr hidden>""")
        dd.options.toSeq.zipWithIndex.foreach { case ((label, value), idx) =>
            val hlAttr = if value == currentVal && currentVal.nonEmpty then """ data-kyo-dropdown-hl="true"""" else ""
            w(sb, s"""<div data-kyo-dropdown-opt="$idx" data-kyo-dropdown-val="${esc(value)}"$hlAttr>${esc(label)}</div>""")
        }
        w(sb, "</div>")
        w(sb, "</div>")
    end renderDropdownWithValue

    // ---- Tag names ----

    private def tagName(elem: Element): String = elem match
        case _: Div            => "div"
        case _: P              => "p"
        case _: Section        => "section"
        case _: Main           => "main"
        case _: Header         => "header"
        case _: Footer         => "footer"
        case _: Pre            => "pre"
        case _: Blockquote     => "blockquote"
        case _: Code           => "code"
        case _: Ul             => "ul"
        case _: Ol             => "ol"
        case _: Table          => "table"
        case _: Colgroup       => "colgroup"
        case _: Col            => "col"
        case _: Thead          => "thead"
        case _: Tbody          => "tbody"
        case _: Tfoot          => "tfoot"
        case _: H1             => "h1"
        case _: H2             => "h2"
        case _: H3             => "h3"
        case _: H4             => "h4"
        case _: H5             => "h5"
        case _: H6             => "h6"
        case _: Hr             => "hr"
        case _: Br             => "br"
        case _: SpanElement    => "span"
        case _: Nav            => "nav"
        case _: Li             => "li"
        case _: Tr             => "tr"
        case _: Td             => "td"
        case _: Th             => "th"
        case _: Label          => "label"
        case _: Form           => "form"
        case _: Fieldset       => "fieldset"
        case _: Legend         => "legend"
        case _: Textarea       => "textarea"
        case _: Select         => "select"
        case _: Opt            => "option"
        case _: Button         => "button"
        case _: Anchor         => "a"
        case _: Img            => "img"
        case _: Iframe         => "iframe"
        case _: Input          => "input"
        case _: PasswordInput  => "input"
        case _: EmailInput     => "input"
        case _: TelInput       => "input"
        case _: UrlInput       => "input"
        case _: SearchInput    => "input"
        case _: NumberInput    => "input"
        case _: Checkbox       => "input"
        case _: Radio          => "input"
        case _: DateInput      => "input"
        case _: TimeInput      => "input"
        case _: ColorInput     => "input"
        case _: RangeInput     => "input"
        case _: FileInput      => "input"
        case _: HiddenInput    => "input"
        case _: Dropdown       => "div"
        case e: Svg.SvgElement => svgTagName(e)
        // SvgNode/SvgRootNode are the sanctioned non-sealed cross-file bridge for the SVG AST
        // (see UI.Ast.SvgNode); every in-tree SVG node extends Svg.SvgElement, matched above, so
        // this arm only covers the abstract bridge type. It is unreachable for any node the
        // framework produces; an instance here means an out-of-tree extension of the bridge.
        case e: SvgNode =>
            throw new IllegalStateException(s"SvgNode must extend Svg.SvgElement: ${e.getClass.getName}")

    // ---- Common attributes ----

    /** Registers `style`'s pseudo-state rule (if any) into `cssRules` and returns its generated class,
      * for an element whose render call is collecting CSS (the server-push path). Returns `Absent`
      * when `cssRules` is `Absent` (the plain `render(...)` path, which keeps today's inline-style
      * behavior unchanged) or `style` carries no pseudo-state prop. Deduped by class: an identical
      * pseudo-state style anywhere else in the tree reuses the same entry rather than appending a
      * duplicate rule.
      *
      * When this returns `Present`, the rule already carries the element's BASE props too (see
      * [[kyo.internal.CssStyleRenderer.pseudoStateClass]]), so the caller must render NO inline style
      * for `style` in that case, or the inline declaration would out-specificity the class rule and
      * the pseudo-state would never visibly apply.
      */
    private def registerPseudoClass(cssRules: Maybe[CssCollector], style: Style): Maybe[String] =
        cssRules.flatMap { rules =>
            CssStyleRenderer.pseudoStateClass(style).map { case (cls, rule) =>
                if !rules.contains(cls) then rules(cls) = rule
                cls
            }
        }

    private def renderCommonAttrs(sb: StringBuilder, elem: Element, cssRules: Maybe[CssCollector] = Absent)(using Frame): Unit =
        renderCommonAttrs(sb, elem.attrs, elem.isInstanceOf[Svg.SvgElement], cssRules)

    /** The `attrs` overload lets a caller render an element with a MODIFIED attribute set (the reactive
      * classes currently true are folded into the class list) without rebuilding the element itself.
      */
    private def renderCommonAttrs(sb: StringBuilder, attrs: Attrs, isSvg: Boolean, cssRules: Maybe[CssCollector])(using Frame): Unit =
        attrs.identifier.foreach(id => w(sb, s""" id="${esc(id)}""""))
        val pseudoClass = registerPseudoClass(cssRules, attrs.uiStyle)
        val classes = pseudoClass match
            case Present(cls) => attrs.cssClasses :+ cls
            case Absent       => attrs.cssClasses
        if classes.nonEmpty then w(sb, s""" class="${esc(classes.mkString(" "))}"""")
        // Skipped when a `.hidden(Signal)` channel owns the attribute; see renderElementAttrs on precedence.
        if !attrs.reactiveBoolAttrs.contains("hidden") then attrs.hidden.foreach(v => if v then w(sb, " hidden"))
        attrs.tabIndex.foreach(n => w(sb, s""" tabindex="$n""""))
        attrs.focusTrap.foreach(v => if v then w(sb, """ data-kyo-focus-trap="1""""))
        attrs.focusGroup.foreach(id => w(sb, s""" data-kyo-focus-group="${esc(id)}""""))
        attrs.focusAuto.foreach(v => if v then w(sb, """ data-kyo-focus-auto="1""""))
        attrs.focusRestore.foreach(v => if v then w(sb, """ data-kyo-focus-restore="1""""))
        attrs.dragSource.foreach { source =>
            w(sb, s""" data-kyo-drag-source="${esc(encodedDragSource(source))}"""")
            w(sb, s""" data-kyo-drag-source-key="${esc(source.key)}"""")
            val nativeActivation = source.activation == Drag.Activation.Native || source.activation == Drag.Activation.Both
            if nativeActivation && !isSvg then w(sb, """ draggable="true"""")
        }
        attrs.dropTarget.foreach { target =>
            w(sb, s""" data-kyo-drop-target="${esc(encodedDropTarget(target))}"""")
            w(sb, s""" data-kyo-drop-target-key="${esc(target.key)}"""")
        }
        // A source and target can coexist on one sortable element. The source owns the single DOM
        // routing key in that case, while the complete target key remains present in its JSON metadata.
        attrs.dragSource match
            case Present(source) => w(sb, s""" data-kyo-drag-key="${esc(source.key)}"""")
            case Absent          => attrs.dropTarget.foreach(target => w(sb, s""" data-kyo-drag-key="${esc(target.key)}""""))
        attrs.scrollAuto.foreach(v => if v then w(sb, """ data-kyo-scroll-auto="1""""))
        // Marker only: stop-propagation is decided server-side in ReactiveUI.dispatchToElement; the client never reads this.
        attrs.stopPropagation.foreach(v => if v then w(sb, """ data-kyo-stop="1""""))
        // enter/leave transition class lists (read client-side by the patch-application code).
        attrs.enterTransition.foreach(c => w(sb, s""" data-kyo-enter="${esc(c)}""""))
        attrs.leaveTransition.foreach(c => w(sb, s""" data-kyo-leave="${esc(c)}""""))
        // Portal marker (read client-side by the patch-application code): the client re-homes this element to
        // document.body behind a data-kyo-portal-slot placeholder; SSR/SSG keep it inline.
        attrs.portal.foreach(v => if v then w(sb, """ data-kyo-portal="1""""))
        // A generated pseudoClass already carries the base props in its own rule (see
        // registerPseudoClass); rendering them inline too would shadow the pseudo-state override.
        if pseudoClass.isEmpty then
            val css = CssStyleRenderer.render(attrs.uiStyle)
            if css.nonEmpty then w(sb, s""" style="${esc(css)}"""")
        // The emptiness gates match reactiveTrueClasses below: sorting for deterministic output costs a
        // Seq copy and a sort per element, and on a plain element every one of these maps is empty.
        if attrs.ariaAttrs.nonEmpty then
            attrs.ariaAttrs.toSeq.sortBy(_._1).foreach { case (name, value) =>
                w(sb, s""" aria-$name="${esc(value)}"""")
            }
        end if
        attrs.role.foreach(r => w(sb, s""" role="${esc(r)}""""))
        if attrs.dataAttrs.nonEmpty then
            attrs.dataAttrs.toSeq.sortBy(_._1).foreach { case (name, value) =>
                w(sb, s""" data-$name="${esc(value)}"""")
            }
        end if
        if attrs.jsProps.nonEmpty then
            attrs.jsProps.toSeq.sortBy(_._1).foreach { case (name, value) =>
                w(sb, s""" data-kyo-prop-$name="${esc(value)}"""")
            }
        end if
    end renderCommonAttrs

    private def encodedDragSource(source: Drag.Source)(using Frame): String =
        DragProtocol.encodedSourceConfig(source, DragProtocol.Limits.default) match
            case Result.Success(encoded) => encoded
            case failure                 => throw new IllegalArgumentException(s"Invalid drag source metadata: $failure")

    private def encodedDropTarget(target: Drag.Target)(using Frame): String =
        DragProtocol.encodedTargetConfig(target, DragProtocol.Limits.default) match
            case Result.Success(encoded) => encoded
            case failure                 => throw new IllegalArgumentException(s"Invalid drop target metadata: $failure")

    // ---- Element-specific attributes ----

    /** The typing constraints, read client-side by the `beforeinput` capture listener.
      *
      * Only [[kyo.UI.Ast.ConstrainedInput]] carries these, so they are emitted here rather than with the universal
      * attributes: a `div` or a chart datum has no use for them.
      */
    private def renderInputConstraints(sb: StringBuilder, ci: ConstrainedInput): Unit =
        ci.inputFilter.foreach(f => w(sb, s""" data-kyo-filter="${esc(InputMasking.filterWire(f))}""""))
        ci.inputMask.foreach(m => w(sb, s""" data-kyo-mask="${esc(m)}""""))
    end renderInputConstraints

    /** Emits each reactive attribute's current value as an ordinary `name="value"` so SSR carries it; the client
      * then patches it in place (HtmlOp.SetAttrByPath). Sorted for deterministic output.
      */
    private def renderReactiveAttrs(sb: StringBuilder, attrs: Attrs)(using Frame): Unit < Sync =
        if attrs.reactiveAttrs.isEmpty then Kyo.unit
        else
            Kyo.foreachDiscard(attrs.reactiveAttrs.toSeq.sortBy(_._1)) { case (name, sig) =>
                sig.current.map(v => w(sb, s""" $name="${esc(v)}""""))
            }

    /** Emits each reactive boolean attribute as a bare present attribute while its signal is true (mirrors
      * `boolAttr`); the client toggles it in place (HtmlOp.SetBoolAttrByPath). Sorted for deterministic output.
      */
    private def renderReactiveBoolAttrs(sb: StringBuilder, attrs: Attrs)(using Frame): Unit < Sync =
        if attrs.reactiveBoolAttrs.isEmpty then Kyo.unit
        else
            Kyo.foreachDiscard(attrs.reactiveBoolAttrs.toSeq.sortBy(_._1)) { case (name, sig) =>
                sig.current.map(v => if v then w(sb, s" $name"))
            }

    /** The reactive classes whose signal is currently true, for folding into the SSR class list; the client
      * toggles them in place afterwards (HtmlOp.SetClassByPath). Empty (and cheap) when none are bound.
      */
    private def reactiveTrueClasses(attrs: Attrs)(using Frame): Seq[String] < Sync =
        if attrs.reactiveClasses.isEmpty then Seq.empty
        else
            Kyo.foreach(attrs.reactiveClasses.toSeq.sortBy(_._1)) { case (name, sig) =>
                sig.current.map(v => if v then name else "")
            }.map(_.filter(_.nonEmpty))

    private def renderElementAttrs(sb: StringBuilder, elem: Element)(using Frame): Unit < Sync =
        // WHERE A CHANNEL EXISTS, THE CHANNEL IS THE VALUE. Shadows the file-level `boolAttr` for the whole
        // method, so a static value is dropped for any name a `Signal`-typed setter also bound. This is not a
        // preference — it is what the client already does at runtime: the channel patches its attribute on
        // every emission and REMOVES it on false, whatever the initial HTML said. Writing both here would
        // paint an attribute the first patch then contradicts, and would leave `.disabled(true).disabled(sig)`
        // meaning something different from `.disabled(sig).disabled(true)`.
        def boolAttr(sb: StringBuilder, name: String, value: Maybe[Boolean]): Unit =
            if !elem.attrs.reactiveBoolAttrs.contains(name) then HtmlRenderer.boolAttr(sb, name, value)
        def owned(name: String): Boolean = elem.attrs.reactiveAttrs.contains(name)
        elem match
            case ci: ConstrainedInput => renderInputConstraints(sb, ci)
            case _                    => ()
        elem match
            case b: Button =>
                w(sb, " type=\"submit\"")
                boolAttr(sb, "disabled", b.disabled)
            case fs: Fieldset =>
                boolAttr(sb, "disabled", fs.disabled)
            case cb: Checkbox =>
                w(sb, " type=\"checkbox\"")
                boolAttr(sb, "disabled", cb.disabled)
                renderCheckedAttr(sb, cb.checked)
            case r: Radio =>
                w(sb, " type=\"radio\"")
                boolAttr(sb, "disabled", r.disabled)
                renderCheckedAttr(sb, r.checked).andThen {
                    r.name.foreach(n => w(sb, s""" name="${esc(n)}""""))
                }
            case i: Input =>
                w(sb, " type=\"text\"");
                renderValueAttr(sb, i.value, i.inputMask)
                    .andThen {
                        boolAttr(sb, "disabled", i.disabled); boolAttr(sb, "readonly", i.readOnly);
                        i.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
                    }
            case p: PasswordInput =>
                w(sb, " type=\"password\"");
                renderValueAttr(sb, p.value, p.inputMask)
                    .andThen {
                        boolAttr(sb, "disabled", p.disabled); boolAttr(sb, "readonly", p.readOnly);
                        p.placeholder.foreach(p2 => w(sb, s""" placeholder="${esc(p2)}""""))
                    }
            case e: EmailInput =>
                w(sb, " type=\"email\"");
                renderValueAttr(sb, e.value, e.inputMask)
                    .andThen {
                        boolAttr(sb, "disabled", e.disabled); boolAttr(sb, "readonly", e.readOnly);
                        e.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
                    }
            case t: TelInput =>
                w(sb, " type=\"tel\"");
                renderValueAttr(sb, t.value, t.inputMask)
                    .andThen {
                        boolAttr(sb, "disabled", t.disabled); boolAttr(sb, "readonly", t.readOnly);
                        t.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
                    }
            case u: UrlInput =>
                w(sb, " type=\"url\"");
                renderValueAttr(sb, u.value, u.inputMask)
                    .andThen {
                        boolAttr(sb, "disabled", u.disabled); boolAttr(sb, "readonly", u.readOnly);
                        u.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
                    }
            case s: SearchInput =>
                w(sb, " type=\"search\"");
                renderValueAttr(sb, s.value, s.inputMask)
                    .andThen {
                        boolAttr(sb, "disabled", s.disabled); boolAttr(sb, "readonly", s.readOnly);
                        s.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
                    }
            case n: NumberInput =>
                w(sb, " type=\"number\"")
                renderValueAttr(sb, n.value).andThen {
                    boolAttr(sb, "disabled", n.disabled); boolAttr(sb, "readonly", n.readOnly)
                    n.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
                    n.min.foreach(v => w(sb, s""" min="${fmtD(v)}""""))
                    n.max.foreach(v => w(sb, s""" max="${fmtD(v)}""""))
                    n.step.foreach(v => w(sb, s""" step="${fmtD(v)}""""))
                }
            case d: DateInput  => w(sb, " type=\"date\""); renderPickerAttrs(sb, d)
            case t: TimeInput  => w(sb, " type=\"time\""); renderPickerAttrs(sb, t)
            case c: ColorInput => w(sb, " type=\"color\""); renderPickerAttrs(sb, c)
            case r: RangeInput =>
                w(sb, " type=\"range\"")
                boolAttr(sb, "disabled", r.disabled)
                val rv: Unit < Sync = r.value match
                    case Present(Bound.Const(d)) => w(sb, s""" value="${fmtD(d)}"""")
                    case Present(Bound.Ref(ref)) =>
                        for d <- ref.get
                        yield w(sb, s""" value="${fmtD(d)}"""")
                    case _ => ()
                rv.andThen {
                    r.min.foreach(v => w(sb, s""" min="${fmtD(v)}""""))
                    r.max.foreach(v => w(sb, s""" max="${fmtD(v)}""""))
                    r.step.foreach(v => w(sb, s""" step="${fmtD(v)}""""))
                }
            case f: FileInput =>
                w(sb, " type=\"file\"")
                boolAttr(sb, "disabled", f.disabled)
                boolAttr(sb, "multiple", f.multiple)
                f.accept.foreach { accepts =>
                    val value = accepts.map {
                        case FileAccept.AnyImage             => "image/*"
                        case FileAccept.AnyVideo             => "video/*"
                        case FileAccept.AnyAudio             => "audio/*"
                        case FileAccept.Pdf                  => "application/pdf"
                        case FileAccept.Image(ImageExt.Png)  => ".png"
                        case FileAccept.Image(ImageExt.Jpeg) => ".jpg"
                        case FileAccept.Image(ImageExt.Webp) => ".webp"
                        case FileAccept.Image(ImageExt.Gif)  => ".gif"
                        case FileAccept.Image(ImageExt.Svg)  => ".svg"
                        case FileAccept.Image(ImageExt.Avif) => ".avif"
                        case FileAccept.Extension(ext)       => ext
                        case FileAccept.MediaType(mime)      => mime
                    }.mkString(",")
                    w(sb, s""" accept="${esc(value)}"""")
                }
            case h: HiddenInput =>
                w(sb, " type=\"hidden\"")
                renderValueAttr(sb, h.value)
            case ta: Textarea =>
                boolAttr(sb, "disabled", ta.disabled)
                boolAttr(sb, "readonly", ta.readOnly)
                ta.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
            case sel: Select =>
                boolAttr(sb, "disabled", sel.disabled)
                val selValue: Unit < Sync = sel.value match
                    case Present(Bound.Const(s)) => w(sb, s""" value="${esc(s)}"""")
                    case Present(Bound.Ref(ref)) =>
                        for s <- ref.get
                        yield w(sb, s""" value="${esc(s)}"""")
                    case _ =>
                        // Fall back to first Opt child with selected(true)
                        val selected = Maybe.fromOption(sel.children.toSeq.collectFirst {
                            case opt: Opt if opt.selected == Present(true) =>
                                opt.value.getOrElse("")
                        })
                        selected match
                            case Present(v) if v.nonEmpty => w(sb, s""" value="${esc(v)}"""")
                            case _                        => ()
                selValue
            case opt: Opt =>
                opt.value.foreach(v => w(sb, s""" value="${esc(v)}""""))
                boolAttr(sb, "selected", opt.selected)
            case a: Anchor =>
                if !owned("href") then a.href.foreach(href => w(sb, s""" href="${esc(Href.attrValue(href))}""""))
                a.download.foreach(name => w(sb, s""" download="${esc(name)}""""))
                a.target.foreach { t =>
                    val tv = t match
                        case Target.Self   => "_self"
                        case Target.Blank  => "_blank"
                        case Target.Parent => "_parent"
                        case Target.Top    => "_top"
                    w(sb, s""" target="$tv"""")
                }
            case img: Img =>
                if !owned("src") then img.src.foreach(src => w(sb, s""" src="${esc(ImgSrc.attrValue(src))}""""))
                img.alt.foreach(a => w(sb, s""" alt="${esc(a)}""""))
            case f: Iframe =>
                if !owned("src") then f.src.foreach(s => w(sb, s""" src="${esc(s)}""""))
                f.frameTitle.foreach(t => w(sb, s""" title="${esc(t)}""""))
            case td: Td =>
                td.colspan.foreach(n => w(sb, s""" colspan="$n""""))
                td.rowspan.foreach(n => w(sb, s""" rowspan="$n""""))
            case th: Th =>
                th.colspan.foreach(n => w(sb, s""" colspan="$n""""))
                th.rowspan.foreach(n => w(sb, s""" rowspan="$n""""))
            case lbl: Label =>
                lbl.forId.foreach(f => w(sb, s""" for="${esc(f)}""""))
            case e: Svg.SvgElement => renderSvgAttrs(sb, e)
            case _                 => ()
        end match
    end renderElementAttrs

    private def boolAttr(sb: StringBuilder, name: String, value: Maybe[Boolean]): Unit =
        value.foreach(v => if v then w(sb, s" $name"))

    private def renderCheckedAttr(sb: StringBuilder, value: Maybe[Bound[Boolean]])(using Frame): Unit < Sync =
        value match
            case Present(Bound.Const(b)) => if b then w(sb, " checked")
            case Present(Bound.Ref(ref)) =>
                for b <- ref.get
                yield if b then w(sb, " checked")
            case _ => ()

    /** Render a value attribute, reading SignalRef if needed. */
    /** Formats a value about to be displayed through the element's mask, if it carries one.
      *
      * A mask is a display format, so a masked field has to show a masked value whoever set it. Client-side
      * enforcement only ever sees typing: a value bound to a [[SignalRef]], a server-side transform of what was
      * typed, and the initial render all reach the field without a `beforeinput` event. Formatting here covers
      * every one of them at once, in both transports and on initial and reactive-range renders, which no amount
      * of client-side patching would.
      *
      * A value the mask already formatted comes back unchanged, so the keystroke echo of a two-way binding still
      * compares equal and leaves the caret where it is.
      */
    private def masked(mask: Maybe[String], value: String): String =
        mask.fold(value)(InputMasking.maskNormalize(_, value))

    private def renderValueAttr(sb: StringBuilder, value: Maybe[Bound[String]], mask: Maybe[String] = Absent)(using
        Frame
    ): Unit < Sync =
        value match
            case Present(Bound.Const(s)) => w(sb, s""" value="${esc(masked(mask, s))}"""")
            case Present(Bound.Ref(ref)) =>
                for s <- ref.get
                yield w(sb, s""" value="${esc(masked(mask, s))}"""")
            case _ => ()

    private def renderPickerAttrs(sb: StringBuilder, pi: PickerInput)(using Frame): Unit < Sync =
        boolAttr(sb, "disabled", pi.disabled)
        renderValueAttr(sb, pi.value)

    // ---- Event attributes ----

    private def hasSignalRefValue(value: Maybe[Bound[?]]): Boolean = value match
        case Present(_: Bound.Ref[?]) => true
        case _                        => false

    private def renderEventAttr(sb: StringBuilder, elem: Element): Unit =
        val events = Seq.newBuilder[String]
        val attrs  = elem.attrs
        if attrs.onClick.nonEmpty || attrs.onClickEvt.nonEmpty ||
            attrs.onClickSelf.nonEmpty || attrs.onClickSelfEvt.nonEmpty
        then events += "click"
        if attrs.onContextMenu.nonEmpty || attrs.onContextMenuEvt.nonEmpty then events += "contextmenu"
        if attrs.onFocus.nonEmpty || attrs.onFocusEvt.nonEmpty then events += "focus"
        if attrs.onBlur.nonEmpty || attrs.onBlurEvt.nonEmpty then events += "blur"
        if attrs.onKeyDown.nonEmpty then events += "keydown"
        if attrs.onKeyUp.nonEmpty then events += "keyup"
        if attrs.onHover.nonEmpty || attrs.onHoverEvt.nonEmpty then events += "mouseover"
        if attrs.onUnhover.nonEmpty || attrs.onUnhoverEvt.nonEmpty then events += "mouseout"
        if attrs.onScroll.nonEmpty || attrs.onScrollEvt.nonEmpty then events += "wheel"
        if attrs.onDragStart.nonEmpty || attrs.onDragStartEvt.nonEmpty then events += "dragstart"
        if attrs.onDragEnd.nonEmpty || attrs.onDragEndEvt.nonEmpty then events += "dragend"
        if attrs.onDragEnter.nonEmpty || attrs.onDragEnterEvt.nonEmpty then events += "dragenter"
        if attrs.onDragLeave.nonEmpty || attrs.onDragLeaveEvt.nonEmpty then events += "dragleave"
        if attrs.onDragOver.nonEmpty || attrs.onDragOverEvt.nonEmpty then events += "dragover"
        if attrs.onDrop.nonEmpty || attrs.onDropEvt.nonEmpty then events += "drop"
        if attrs.onSortMove.nonEmpty || attrs.onSortMoveEvt.nonEmpty then events += "sortmove"
        if attrs.onScrollPos.nonEmpty then events += "scroll"
        // "input" event: when handler is set OR when .value(SignalRef) auto-binding is in use
        elem match
            case ti: TextInput if ti.onInput.nonEmpty || hasSignalRefValue(ti.value) => events += "input"
            case _                                                                   =>
        // "change" event: when handler is set OR when .value/.checked(SignalRef) auto-binding is in use
        elem match
            case ti: TextInput if ti.onChange.nonEmpty || hasSignalRefValue(ti.value)          => events += "change"
            case pi: PickerInput if pi.onChange.nonEmpty || hasSignalRefValue(pi.value)        => events += "change"
            case bi: BooleanInput if bi.onChange.nonEmpty || hasSignalRefValue(bi.checked)     => events += "change"
            case ni: NumberInput if ni.onChangeNumeric.nonEmpty || hasSignalRefValue(ni.value) => events += "change"
            case ri: RangeInput if ri.onChange.nonEmpty || hasSignalRefValue(ri.value)         => events += "change"
            case fi: FileInput if fi.onChange.nonEmpty || attrs.onFileSelect.nonEmpty          => events += "change"
            case sel: Select if hasSignalRefValue(sel.value)                                   => events += "change"
            case _                                                                             =>
        end match
        elem match
            case f: Form if f.onSubmit.nonEmpty || f.onSubmitEvt.nonEmpty => events += "submit"
            case _                                                        =>
        if attrs.onPointerDown.nonEmpty then events += "pointerdown"
        if attrs.onPointerMove.nonEmpty then events += "pointermove"
        if attrs.onPointerUp.nonEmpty then events += "pointerup"
        // fileselect marker so the client's change branch reads all files instead of the legacy single-file path.
        if attrs.onFileSelect.nonEmpty then events += "fileselect"
        val ev = events.result()
        if ev.nonEmpty then w(sb, s""" data-kyo-ev="${ev.mkString(",")}"""")
    end renderEventAttr

    // ---- Helpers ----

    private[kyo] val baseCss =
        """*, *::before, *::after { box-sizing: border-box; }
          |body { font-family: system-ui, -apple-system, sans-serif; margin: 0; padding: 0; }
          |div, section, main, header, footer, form, article, aside, p, ul, ol, pre, code, h1, h2, h3, h4, h5, h6, label { display: flex; flex-direction: column; }
          |nav, li, span, button, a { display: flex; flex-direction: row; align-items: center; }
          |[data-kyo-reactive] { display: contents; }
          |ul, ol { list-style: none; padding: 0; margin: 0; }
          |h1, h2, h3, h4, h5, h6, p { margin: 0; }
          |a { color: inherit; text-decoration: none; }
          |table { border-collapse: collapse; width: 100%; }
          |[hidden] { display: none !important; }
          |""".stripMargin

    private def pathAttr(path: Seq[String]): String = path.mkString(".")

    private def fmtD(v: Double): String = NumberFormat.double(v)

    private inline def w(sb: StringBuilder, s: String): Unit =
        sb.append(s); ()

    private def esc(s: String): String =
        val sb = new StringBuilder(s.length)
        s.foreach {
            case '&'  => sb.append("&amp;")
            case '<'  => sb.append("&lt;")
            case '>'  => sb.append("&gt;")
            case '"'  => sb.append("&quot;")
            case '\'' => sb.append("&#39;")
            case c    => sb.append(c)
        }
        sb.toString
    end esc

    // Escape a string for safe embedding inside a JS double-quoted string literal within a
    // <script> element. Must handle both JS parse hazards and the HTML parser's raw-text
    // model: </script> (or </Script> etc.) ends the script element regardless of JS context.
    //
    // Rules applied, in order:
    //   \  -> \\   (backslash first, before any escape that produces \)
    //   "  -> \"   (closing double-quote)
    //   '  -> \'   (single-quote, safe-by-default)
    //  \r  -> \r   (CR, JS line terminator)
    //  \n  -> \n   (LF, JS line terminator)
    // U+2028 -> U+2028  (LINE SEPARATOR, JS line terminator)
    // U+2029 -> U+2029  (PARAGRAPH SEPARATOR, JS line terminator)
    //  </  -> <\/  (prevents </script> from closing the element; < alone is harmless in JS)
    private[kyo] def jsStr(s: String): String =
        val sb = new StringBuilder(s.length)
        @scala.annotation.tailrec
        def loop(i: Int): Unit =
            if i < s.length then
                s.charAt(i) match
                    case '\\' =>
                        sb.append("\\\\")
                        loop(i + 1)
                    case '"' =>
                        sb.append("\\\"")
                        loop(i + 1)
                    case '\'' =>
                        sb.append("\\'")
                        loop(i + 1)
                    case '\r' =>
                        sb.append("\\r")
                        loop(i + 1)
                    case '\n' =>
                        sb.append("\\n")
                        loop(i + 1)
                    case ' ' =>
                        sb.append("\\u2028")
                        loop(i + 1)
                    case ' ' =>
                        sb.append("\\u2029")
                        loop(i + 1)
                    case '<' if i + 1 < s.length && s.charAt(i + 1) == '/' =>
                        sb.append("<\\/")
                        loop(i + 2)
                    case c =>
                        sb.append(c)
                        loop(i + 1)
        loop(0)
        sb.toString
    end jsStr

    // ---- Client JS ----

    /** The pure half of the client-side input filter and mask: the JavaScript mirror of [[InputMasking]].
      *
      * Kept out of [[clientJs]] as a named fragment for two reasons. It is the part that has an exact Scala
      * counterpart, so having it standalone lets `InputMaskingJsParityTest` load it into a page and drive the same
      * case table through both implementations, which is what keeps the two from drifting. And unlike [[clientJs]]
      * this is not an interpolated string, so a backslash written here is the backslash the browser sees; inlined,
      * the mask parser's escape had to be doubled, and getting that wrong produced an unterminated JavaScript
      * literal that took down the whole client script.
      *
      * Only DOM-free functions belong here. Everything that touches an element stays in [[clientJs]].
      */
    private[kyo] val inputMaskJs: String =
        """// Mirrors kyo.internal.InputMasking.filterStr: keep the two in step.
          |// An unrecognized wire value admits everything, so a page cached from an older build stays usable.
          |function kyoFilterStr(pat,str,cur){
          |  var isChars=pat.indexOf("chars:")===0;var allowed=isChars?pat.slice(6):"";
          |  if(!isChars&&pat!=="digits"&&pat!=="decimal")return str;
          |  var out="";var hasSep=(pat==="decimal")&&(cur.indexOf(".")>=0||cur.indexOf(",")>=0);
          |  for(var i=0;i<str.length;i++){var ch=str.charAt(i);
          |    if(isChars){if(allowed.indexOf(ch)>=0)out+=ch;}
          |    else if(pat==="digits"){if(ch>="0"&&ch<="9")out+=ch;}
          |    else if(ch>="0"&&ch<="9")out+=ch;
          |    else if((ch==="."||ch===",")&&!hasSep){out+=ch;hasSep=true;}}
          |  return out;
          |}
          |// Mirrors kyo.internal.InputMasking.parseMask: the mask is parsed once into positions, {k:class} or
          |// {l:literal}, so the backslash escape is handled in one place rather than in every function.
          |function kyoMaskParse(mask){var ts=[];for(var i=0;i<mask.length;){var c=mask.charAt(i);
          |  if(c==="\\"&&i+1<mask.length){ts.push({l:mask.charAt(i+1)});i+=2;}
          |  else{if(c==="9"||c==="a"||c==="*")ts.push({k:c});else ts.push({l:c});i++;}}
          |  return ts;}
          |function kyoMaskClassAt(ts,idx){var c=0;for(var i=0;i<ts.length;i++){if(ts[i].k!==undefined){if(c===idx)return ts[i].k;c++;}}return null;}
          |function kyoMaskOk(cls,ch){if(cls==="9")return ch>="0"&&ch<="9";if(cls==="a")return (ch>="a"&&ch<="z")||(ch>="A"&&ch<="Z");return (ch>="0"&&ch<="9")||(ch>="a"&&ch<="z")||(ch>="A"&&ch<="Z");}
          |function kyoMaskFormat(ts,raw){var out="";var ri=0;for(var i=0;i<ts.length;i++){
          |  if(ts[i].k!==undefined){if(ri<raw.length){out+=raw.charAt(ri);ri++;}else break;}
          |  else{if(ri<raw.length)out+=ts[i].l;else break;}}
          |  return out;}
          |function kyoMaskRaw(ts,val){var raw="";var vi=0;for(var i=0;i<ts.length&&vi<val.length;i++){
          |  if(ts[i].k!==undefined){raw+=val.charAt(vi);vi++;}
          |  else{if(val.charAt(vi)===ts[i].l)vi++;else{raw+=val.charAt(vi);vi++;}}}
          |  return raw;}
          |// Mirrors kyo.internal.InputMasking.maskNormalize.
          |function kyoMaskNormalize(mask,val){var ts=kyoMaskParse(mask);return kyoMaskFormat(ts,kyoMaskRaw(ts,val));}""".stripMargin

    private[kyo] val reactiveRangesJs: String =
        """function kyoRangeId(id){
          |  if(typeof id!=="string"||!/^r[0-9a-f]*(?:n[0-9a-f]{8})?$/.test(id))return false;
          |  var suffix=id.indexOf("n",1),end=suffix<0?id.length:suffix;
          |  if(suffix>=0&&id.slice(suffix+1)==="00000000")return false;
          |  var i=1;while(i<end){if(i+8>end)return false;var n=parseInt(id.slice(i,i+8),16);i+=8;
          |    if(!isFinite(n)||i+n*4>end)return false;i+=n*4;}
          |  return i===end;
          |}
          |function kyoRangeFail(message){throw new Error("kyo-ui reactive range: "+message);}
          |// A marker payload may carry a flag section after its id (a mount slot, and the m the client stamps on once
          |// it adopts one), so the id is everything up to the first separator.
          |function kyoRangeMarker(comment,prefix){
          |  var value=comment.data;if(value.indexOf(prefix)!==0)return null;
          |  var rest=value.slice(prefix.length),sp=rest.indexOf(" "),id=sp<0?rest:rest.slice(0,sp);
          |  if(!kyoRangeId(id))kyoRangeFail("malformed id: "+id);return id;
          |}
          |function kyoRangeScan(root){
          |  var found=new Map(),seen=new Map(),open=[];
          |  var walker=document.createTreeWalker(root,128,null,false),comment;
          |  while((comment=walker.nextNode())){
          |    var start=kyoRangeMarker(comment,"kyo-rs:");
          |    if(start!==null){if(seen.has(start))kyoRangeFail("duplicate id: "+start);seen.set(start,true);open.push({id:start,node:comment});continue;}
          |    var end=kyoRangeMarker(comment,"kyo-re:");
          |    if(end!==null){if(open.length===0)kyoRangeFail("end marker has no start: "+end);
          |      var top=open[open.length-1];if(top.id!==end)kyoRangeFail("crossed ranges: expected "+top.id+", found "+end);
          |      open.pop();if(top.node.parentNode!==comment.parentNode)kyoRangeFail("anchors are not siblings: "+end);
          |      found.set(end,{start:top.node,end:comment});}
          |  }
          |  if(open.length)kyoRangeFail("start marker has no end: "+open[open.length-1].id);
          |  return found;
          |}
          |function kyoRangeRoots(start,end){
          |  var roots=[],node=start.nextSibling;while(node&&node!==end){if(node.nodeType===1)roots.push(node);node=node.nextSibling;}return roots;
          |}
          |function kyoRangeFragmentRoots(fragment){
          |  var roots=[],node=fragment.firstChild;while(node){if(node.nodeType===1)roots.push(node);node=node.nextSibling;}return roots;
          |}
          |function kyoRangeSemanticRoots(roots,id){
          |  if(roots.length!==1||roots[0].tagName!=="TBODY"||roots[0].getAttribute("data-kyo-range-host")!==id)return roots;
          |  var semantic=[],node=roots[0].firstChild;while(node){if(node.nodeType===1)semantic.push(node);node=node.nextSibling;}return semantic;
          |}
          |function kyoRangeSyncHost(host,incoming,id){for(var i=host.attributes.length-1;i>=0;i--){var name=host.attributes[i].name;if(name!=="data-kyo-range-host")host.removeAttribute(name);}
          |  for(var i=0;i<incoming.attributes.length;i++){var a=incoming.attributes[i];if(a.name!=="data-kyo-range-host")host.setAttribute(a.name,a.value);}
          |  host.setAttribute("data-kyo-range-host",id);
          |}
          |function kyoRangeContains(roots,node){for(var i=0;i<roots.length;i++)if(roots[i]===node||roots[i].contains(node))return true;return false;}
          |function kyoRangeFocusLocator(roots,active){for(var r=0;r<roots.length;r++){if(roots[r]===active||roots[r].contains(active)){
          |    var route=[],node=active;while(node!==roots[r]){var parent=node.parentElement,index=0;
          |      while(index<parent.children.length&&parent.children[index]!==node)index++;route.unshift(index);node=parent;}
          |    return {path:active.getAttribute("data-kyo-path"),root:r,route:route};}}return null;
          |}
          |function kyoRangeResolveFocus(roots,locator){if(!locator)return null;if(locator.path!==null){for(var r=0;r<roots.length;r++){
          |    if(roots[r].getAttribute("data-kyo-path")===locator.path)return roots[r];var all=roots[r].querySelectorAll("[data-kyo-path]");
          |    for(var i=0;i<all.length;i++)if(all[i].getAttribute("data-kyo-path")===locator.path)return all[i];}}
          |  var node=roots[locator.root];if(!node)return null;for(var i=0;i<locator.route.length;i++){
          |    node=node.children[locator.route[i]];if(!node)return null;}return node;
          |}
          |function kyoMergeSets(into,from){for(var key in from)if(Object.prototype.hasOwnProperty.call(from,key))into[key]=true;return into;}
          |function kyoEnterPathsRoots(roots){var set={};for(var i=0;i<roots.length;i++)kyoMergeSets(set,faEnterPaths(roots[i]));return set;}
          |function kyoFocusPathsRoots(roots){var set={};for(var i=0;i<roots.length;i++)kyoMergeSets(set,focusAutoPaths(roots[i]));return set;}
          |function kyoLeavePathsRoots(roots){var set={};for(var i=0;i<roots.length;i++){var root=roots[i],els=[];
          |  if(root.hasAttribute("data-kyo-leave"))els.push(root);var ds=root.querySelectorAll("[data-kyo-leave]");
          |  for(var j=0;j<ds.length;j++)els.push(ds[j]);for(var j=0;j<els.length;j++){var p=els[j].getAttribute("data-kyo-path");if(p!==null)set[p]=true;}}
          |  return set;
          |}
          |function kyoLeavePrepareRoots(roots,surv){var ghosts=[];for(var i=0;i<roots.length;i++)ghosts=ghosts.concat(kyoLeavePrepare(roots[i],surv));return ghosts;}
          |function kyoSeedEnterRoots(roots,oldSet){for(var i=0;i<roots.length;i++)kyoEnterSeed(roots[i],oldSet);}
          |function kyoSeedFocusRoots(roots,oldSet){
          |  var cand=[];for(var i=0;i<roots.length;i++){var root=roots[i];if(root.hasAttribute("data-kyo-focus-auto"))cand.push(root);
          |    var ds=root.querySelectorAll("[data-kyo-focus-auto]");for(var j=0;j<ds.length;j++)cand.push(ds[j]);}
          |  for(var i=0;i<cand.length;i++){var fa=cand[i].getAttribute("data-kyo-path");if(fa!==null&&!oldSet[fa]){
          |    var ae=document.activeElement,ret=(ae&&ae!==document.body&&ae.getAttribute)?ae.getAttribute("data-kyo-path"):null;
          |    __focusReturnStack.push({fa:fa,ret:ret,restore:cand[i].hasAttribute("data-kyo-focus-restore")});
          |    if(typeof cand[i].focus==="function")cand[i].focus({preventScroll:true});return;}}
          |}
          |// ---- range morph (twin of morphNode and its neighbours in DomBackend; keep the two in lockstep) ----
          |// Reconciling a re-rendered region against its live nodes instead of replacing them is what keeps node
          |// identity across a paint: focus, caret, scroll position, an imperatively bound attribute, anything a
          |// DOM-local expando holds. Replacing the range is always correct and always loses all of it, so it stays
          |// the fallback rather than the default.
          |function __kyoMorphCompatible(from,to){
          |  if(from.nodeType!==to.nodeType)return false;
          |  if(from.nodeType!==1)return true;
          |  return from.tagName===to.tagName&&from.namespaceURI===to.namespaceURI;
          |}
          |function __kyoMorphNode(from,to){
          |  if(from.nodeType!==1){if(from.nodeValue!==to.nodeValue)from.nodeValue=to.nodeValue;return;}
          |  __kyoMorphAttrs(from,to);
          |  // A focused contenteditable would lose its caret if its children were rewritten mid-edit; leave its
          |  // subtree alone (INPUT and TEXTAREA have no element children, so they need no such guard).
          |  if(!((from===document.activeElement)&&from.hasAttribute("contenteditable")))
          |    __kyoMorphRun(from,from.firstChild,null,to.firstChild,null);
          |}
          |function __kyoMorphAttrs(from,to){
          |  var tag=from.tagName;
          |  var activeInput=(from===document.activeElement)&&(tag==="INPUT"||tag==="TEXTAREA");
          |  // An attribute the imperative id-addressed channel owns is never reconciled: rendered HTML never carries
          |  // the client-set value, so reconciling would clobber it. Twin of ownedAttrs in DomBackend.
          |  var own=from.__kyoOwn||{};
          |  // A field renders its value PROPERTY, and the property stops tracking the attribute the first time the
          |  // user types, so writing the attribute alone is invisible on any field that has been typed into. The
          |  // attribute may even be unchanged (both empty) when a ref write clears it, so the property is written on
          |  // every pass. The focused field is the exception below: its own echo must not move the caret.
          |  for(var i=0;i<to.attributes.length;i++){var a=to.attributes[i];
          |    if(!own[a.name]){
          |      if(from.getAttribute(a.name)!==a.value)from.setAttribute(a.name,a.value);
          |      if(!activeInput)__kyoSyncField(from,a.name,a.value);}}
          |  for(var j=from.attributes.length-1;j>=0;j--){var n=from.attributes[j].name;
          |    if(!own[n]&&!to.hasAttribute(n))from.removeAttribute(n);}
          |  // Active-input preservation: two-way binding echoes each keystroke back as a re-render. Never overwrite
          |  // the focused field's live value (its caret) with its own echo; assign only a genuine external change.
          |  if(activeInput){var v=tag==="TEXTAREA"?to.textContent:(to.getAttribute("value")||"");
          |    if(from.value!==v)from.value=v;}
          |}
          |// Reconcile the live sibling run of `parent` from `fromStart` up to (not including) `fromEnd`, null meaning
          |// the end of the parent, toward the run starting at `toStart` inside a detached fragment. Positional, pair
          |// by pair: what did not move keeps its identity; reordering a keyed list is the two-ended pass's business.
          |// A portal element does not live in the range: the first sweep moved it under <body> and left an inert slot
          |// behind. The payload still carries it inline, so reconcile the twin where it actually lives and step over
          |// the live slot. Re-inserting it here would hand __kyoPortalSweep a second copy, which retires the twin and
          |// takes everything hanging off it along. Twin of morphPortalPair in DomBackend.
          |function __kyoMorphPortalPair(from,to){
          |  if(from.nodeType!==1||to.nodeType!==1)return false;
          |  var p=to.getAttribute("data-kyo-path");
          |  if(p===null||!to.hasAttribute("data-kyo-portal")||from.getAttribute("data-kyo-portal-slot")!==p)return false;
          |  var twin=__kyoPortalTwin(p);if(!twin)return false;
          |  __kyoMorphNode(twin,to);return true;
          |}
          |// ---- logical children ----
          |// A nested range is opened and closed by comments that are siblings of its content, so to a naive sibling
          |// walk it looks like several unrelated children. It is one: the markers carry the identity the registry is
          |// keyed by, and pairing them positionally would rewrite marker text and hand the registry a range that is
          |// no longer there. The morph therefore walks LOGICAL children, an opening marker standing for its span.
          |function __kyoSpanId(node){
          |  if(node.nodeType!==8)return null;
          |  var d=node.data;if(d.indexOf("kyo-rs:")!==0)return null;
          |  var rest=d.slice(7),sp=rest.indexOf(" ");return sp<0?rest:rest.slice(0,sp);
          |}
          |// The flag section of an opening marker (m, s, k=), "" when it carries none.
          |function __kyoSpanFlags(node){
          |  if(node.nodeType!==8)return "";
          |  var sp=node.data.indexOf(" ");return sp<0?"":node.data.slice(sp+1);
          |}
          |function __kyoHasFlag(flags,flag){return flags.length>0&&flags.split(" ").indexOf(flag)>=0;}
          |function __kyoFlagKey(flags){
          |  if(!flags.length)return null;
          |  var parts=flags.split(" ");
          |  for(var i=0;i<parts.length;i++)if(parts[i].indexOf("k=")===0)return parts[i].slice(2);
          |  return null;
          |}
          |function __kyoSpanClose(open,id){
          |  var n=open.nextSibling;
          |  while(n){if(n.nodeType===8&&n.data==="kyo-re:"+id)return n;n=n.nextSibling;}
          |  return null;
          |}
          |function __kyoLogicalNext(node){
          |  var id=__kyoSpanId(node);if(id===null)return node.nextSibling;
          |  var close=__kyoSpanClose(node,id);return close?close.nextSibling:node.nextSibling;
          |}
          |function __kyoEachSpanNode(first,f){
          |  var id=__kyoSpanId(first),last=first;
          |  if(id!==null){var close=__kyoSpanClose(first,id);if(close)last=close;}
          |  var n=first,stop=false;
          |  while(!stop&&n){var next=n.nextSibling;stop=n===last;f(n);n=next;}
          |}
          |function __kyoRemoveLogical(parent,node){__kyoEachSpanNode(node,function(n){parent.removeChild(n);});}
          |function __kyoInsertLogicalClone(parent,toNode,ref){
          |  // SMIL animations only start on a node this pass actually inserted; a reused one is already running.
          |  __kyoEachSpanNode(toNode,function(n){var fresh=document.importNode(n,true);parent.insertBefore(fresh,ref);ba(fresh);});
          |}
          |// Two spans of the same id are the same region still sitting here, so the pass recurses into their contents
          |// and never touches the live markers. Any other mismatch is replaced whole, markers and all.
          |function __kyoPatchLogical(parent,fromNode,toNode){
          |  var fid=__kyoSpanId(fromNode),tid=__kyoSpanId(toNode);
          |  var fclose=fid!==null?__kyoSpanClose(fromNode,fid):null,tclose=tid!==null?__kyoSpanClose(toNode,tid):null;
          |  if(fclose&&tclose&&fid===tid){
          |    // A mount that already owns this slot repaints its own content, so the span is opaque and its live
          |    // marker is left alone: reconciling it against the placeholder the parent rendered would morph the
          |    // instance's subtree away, and with it focus, caret and every DOM-local thing hanging off it. The key
          |    // is what makes that safe; a differing key falls through to the morph and resets the slot. The live
          |    // marker adopts the incoming key on the way out.
          |    //
          |    // A NAMED slot is opaque from the first pass, not from the second. `m` says the client adopted the
          |    // span; `s` with the same key says the same thing one beat earlier, because the render that emitted
          |    // the slot named the instance that owns it. Waiting for `m` bought exactly one destructive morph per
          |    // slot, and the regions inside the discarded subtree leave the registry with it while the instance
          |    // republishes asynchronously — a subscription emitting in between dies on an unknown range, for good.
          |    // An UNNAMED slot keeps the old rule: without a key nothing tells one instance from the next.
          |    var lf=__kyoSpanFlags(fromNode),tf=__kyoSpanFlags(toNode),slot=__kyoHasFlag(tf,"s");
          |    var sameKey=__kyoFlagKey(lf)===__kyoFlagKey(tf),named=__kyoFlagKey(tf)!==null&&sameKey;
          |    if((__kyoHasFlag(lf,"m")||(named&&__kyoHasFlag(lf,"s")))&&slot&&sameKey)return;
          |    __kyoMorphRun(parent,fromNode.nextSibling,fclose,toNode.nextSibling,tclose);
          |    if(slot){var k=__kyoFlagKey(tf);fromNode.data="kyo-rs:"+fid+(k===null?" m":" m k="+k);}
          |    return;}
          |  if(!fclose&&!tclose&&__kyoMorphCompatible(fromNode,toNode)){__kyoMorphNode(fromNode,toNode);return;}
          |  __kyoInsertLogicalClone(parent,toNode,fromNode);
          |  __kyoRemoveLogical(parent,fromNode);
          |}
          |// Reconcile the live logical run [fromStart,fromEnd) of parent toward [toStart,toEnd).
          |// A two-ended keyed pass runs first, then a single cursor over whatever it could not settle. The cursor
          |// alone can only insert IN FRONT of itself, so a key it finds behind itself has to be dragged forward past
          |// every sibling in between: swapping two rows of a thousand costs 997 moves and a full relayout. Matching
          |// both ends first relocates only the children that actually changed place.
          |function __kyoCollectLogical(start,end,nodes,keys){
          |  var scan=start;
          |  while(scan&&scan!==end){nodes.push(scan);keys.push(__kyoLogicalKey(scan));scan=__kyoLogicalNext(scan);}
          |}
          |function __kyoLogicalKey(node){
          |  if(node.nodeType===1)return node.getAttribute("data-kyo-path");
          |  var id=__kyoSpanId(node);
          |  return (id!==null&&__kyoSpanClose(node,id))?id:null;
          |}
          |function __kyoMoveLogicalBefore(parent,node,ref){__kyoEachSpanNode(node,function(n){parent.insertBefore(n,ref);});}
          |function __kyoMorphRun(parent,fromStart,fromEnd,toStart,toEnd){
          |  var fromNodes=[],fromKeys=[],toNodes=[],toKeys=[];
          |  __kyoCollectLogical(fromStart,fromEnd,fromNodes,fromKeys);
          |  __kyoCollectLogical(toStart,toEnd,toNodes,toKeys);
          |  var fromKeyed=null,toKeyed=null,i;
          |  for(i=0;i<fromKeys.length;i++)if(fromKeys[i]!==null){if(!fromKeyed)fromKeyed={};fromKeyed[fromKeys[i]]=fromNodes[i];}
          |  for(i=0;i<toKeys.length;i++)if(toKeys[i]!==null){if(!toKeyed)toKeyed={};toKeyed[toKeys[i]]=true;}
          |  // Invariant: the children still to place are exactly fromNodes[head..tail], a contiguous DOM run ending
          |  // immediately before tailBoundary. Only run boundaries are ever moved, and only out to a boundary.
          |  var head=0,tail=fromNodes.length-1,toHead=0,toTail=toNodes.length-1,tailBoundary=fromEnd,scanning=true;
          |  while(scanning&&head<=tail&&toHead<=toTail){
          |    if(fromKeys[head]===null||fromKeys[tail]===null||toKeys[toHead]===null||toKeys[toTail]===null)scanning=false;
          |    else if(fromKeys[head]===toKeys[toHead]){__kyoPatchLogical(parent,fromNodes[head],toNodes[toHead]);head++;toHead++;}
          |    else if(fromKeys[tail]===toKeys[toTail]){__kyoPatchLogical(parent,fromNodes[tail],toNodes[toTail]);tailBoundary=fromNodes[tail];tail--;toTail--;}
          |    else if(fromKeys[head]===toKeys[toTail]){
          |      if(head!==tail)__kyoMoveLogicalBefore(parent,fromNodes[head],tailBoundary);
          |      __kyoPatchLogical(parent,fromNodes[head],toNodes[toTail]);tailBoundary=fromNodes[head];head++;toTail--;}
          |    else if(fromKeys[tail]===toKeys[toHead]){
          |      if(tail!==head)__kyoMoveLogicalBefore(parent,fromNodes[tail],fromNodes[head]);
          |      __kyoPatchLogical(parent,fromNodes[tail],toNodes[toHead]);tail--;toHead++;}
          |    else scanning=false;
          |  }
          |  // Hand the unresolved middle to the cursor. The dictionaries stay whole: keys are unique among siblings.
          |  var cursorFrom=head<=tail?fromNodes[head]:tailBoundary;
          |  var cursorToEnd=(toTail+1<toNodes.length)?toNodes[toTail+1]:toEnd;
          |  var cursorTo=toHead<=toTail?toNodes[toHead]:cursorToEnd;
          |  __kyoMorphCursor(parent,cursorFrom,tailBoundary,cursorTo,cursorToEnd,fromKeyed,toKeyed);
          |}
          |// Single-cursor reconciliation of whatever the two-ended pass left over: a keyed child is pulled to the
          |// cursor by key, an unkeyed one morphs positionally against the first compatible live child.
          |function __kyoMorphCursor(parent,fromStart,fromEnd,toStart,toEnd,fromKeyed,toKeyed){
          |  var curFrom=fromStart,curTo=toStart;
          |  while(curTo&&curTo!==toEnd){
          |    var toNext=__kyoLogicalNext(curTo);
          |    if(curFrom&&curFrom!==fromEnd&&__kyoMorphPortalPair(curFrom,curTo)){curFrom=__kyoLogicalNext(curFrom);}
          |    else{
          |      var toKey=__kyoLogicalKey(curTo);
          |      if(toKey!==null){
          |        var m=fromKeyed?fromKeyed[toKey]:undefined;
          |        if(m){if(m!==curFrom)__kyoMoveLogicalBefore(parent,m,curFrom);else curFrom=__kyoLogicalNext(curFrom);
          |          __kyoPatchLogical(parent,m,curTo);}
          |        else __kyoInsertLogicalClone(parent,curTo,curFrom);
          |      }else{
          |        var handled=false,loop=true;
          |        while(loop&&curFrom&&curFrom!==fromEnd){
          |          var fromNext=__kyoLogicalNext(curFrom),fromKey=__kyoLogicalKey(curFrom);
          |          if(fromKey!==null){
          |            // A keyed live child at an unkeyed slot: keep it if the payload reuses it elsewhere (its own
          |            // slot moves it into place), else it is stale and goes.
          |            if(!toKeyed||!Object.prototype.hasOwnProperty.call(toKeyed,fromKey))__kyoRemoveLogical(parent,curFrom);
          |            curFrom=fromNext;
          |          }else if(__kyoMorphCompatible(curFrom,curTo)){__kyoMorphNode(curFrom,curTo);curFrom=fromNext;handled=true;loop=false;}
          |          else{__kyoRemoveLogical(parent,curFrom);curFrom=fromNext;}
          |        }
          |        if(!handled)__kyoInsertLogicalClone(parent,curTo,curFrom);
          |      }
          |    }
          |    curTo=toNext;
          |  }
          |  while(curFrom&&curFrom!==fromEnd){var fn=__kyoLogicalNext(curFrom);__kyoRemoveLogical(parent,curFrom);curFrom=fn;}
          |}
          |// Declined in two shapes this pass does not maintain, both of which fall back to the wholesale replacement
          |// that always works: a payload declaring nested ranges of its own, and a live range that still holds one.
          |// Either would leave the range registry describing markers the morph moved or dropped.
          |// Nested ranges reconcile as logical children, so a region containing another region morphs like any other;
          |// kyoRangeReplace re-reads the live markers afterwards and hands the registry what is actually there.
          |function kyoRangeMorph(endpoints,fragment,incoming,synthetic){
          |  if(synthetic)return false;
          |  __kyoMorphRun(endpoints.start.parentNode,endpoints.start.nextSibling,endpoints.end,fragment.firstChild,null);
          |  return true;
          |}
          |// The ranges the live content holds right now, read off the DOM: the morph keeps the markers of a range that
          |// survived and clones or drops the rest, so the registry cannot be updated from the payload the way the
          |// wholesale path does it. Twin of rescanRange in DomReactiveRegions.
          |function kyoRangeRescan(id,endpoints){
          |  var found=new Map(),open=[];
          |  function visit(node){
          |    var sid=__kyoSpanId(node);
          |    if(sid!==null)open.push({id:sid,node:node});
          |    else if(node.nodeType===8&&node.data.indexOf("kyo-re:")===0){
          |      var eid=node.data.slice(7);
          |      if(!open.length||open[open.length-1].id!==eid)kyoRangeFail("crossed ranges after morphing "+id+": found "+eid);
          |      found.set(eid,{start:open.pop().node,end:node});}
          |    var child=node.firstChild;
          |    while(child){var next=child.nextSibling;visit(child);child=next;}
          |  }
          |  var n=endpoints.start.nextSibling;
          |  while(n&&n!==endpoints.end){var next=n.nextSibling;visit(n);n=next;}
          |  if(open.length)kyoRangeFail("start marker has no end after morphing "+id+": "+open[open.length-1].id);
          |  return found;
          |}
          |// Reconcile a list region against a ROW ORDER rather than a rendered document: the payload holds only the
          |// rows that changed, every other row is named by key and stays where it is. The walk is the same
          |// two-ended keyed pass __kyoMorphRun uses, with the order standing in for the `to` side. Twin of
          |// applyListPatch in DomBackend; keep the two in lockstep.
          |//
          |// ReactiveUI refused the emission already if the rows are not addressable by key, so there is nothing to
          |// check and nothing to fall back to: a frame that left the untouched rows out gives us nothing to rebuild
          |// them from. A row the live range does not hold is therefore a protocol error, not a repaint trigger.
          |function __kyoApplyList(id,keys,changedKeys,changedHtml){
          |  if(!kyoRangeId(id))kyoRangeFail("malformed replacement id: "+id);
          |  if(!__kyoRanges)kyoRangeFail("registry is closed");
          |  var endpoints=__kyoRanges.get(id);
          |  if(!endpoints)kyoRangeFail("unknown id: "+id);
          |  if(kyoRangeMarker(endpoints.start,"kyo-rs:")!==id||endpoints.end.data!=="kyo-re:"+id)kyoRangeFail("markers are corrupted: "+id);
          |  var parent=endpoints.start.parentNode;
          |  if(!parent||parent!==endpoints.end.parentNode)kyoRangeFail("anchors are no longer siblings: "+id);
          |  var range=document.createRange();range.setStartAfter(endpoints.start);range.setEndBefore(endpoints.end);
          |  var fragment=changedHtml.length?range.createContextualFragment(changedHtml):null;
          |  var parsedKeyed={},parsed=fragment?fragment.firstChild:null;
          |  while(parsed){var pk=__kyoLogicalKey(parsed);if(pk!==null)parsedKeyed[pk]=parsed;parsed=__kyoLogicalNext(parsed);}
          |  var fromNodes=[],fromKeys=[];
          |  __kyoCollectLogical(endpoints.start.nextSibling,endpoints.end,fromNodes,fromKeys);
          |  var fromKeyed={},i;
          |  for(i=0;i<fromKeys.length;i++)if(fromKeys[i]!==null)fromKeyed[fromKeys[i]]=fromNodes[i];
          |  var changedSet={};for(i=0;i<changedKeys.length;i++)changedSet[changedKeys[i]]=true;
          |  var prefix=__kyoRangeIdPath(id);if(prefix===null)prefix="";
          |  var toKeys=[],toNodes=[],targetKeys={};
          |  for(i=0;i<keys.length;i++){
          |    var full=prefix.length?prefix+"."+keys[i]:keys[i];
          |    toKeys.push(full);targetKeys[full]=true;
          |    toNodes.push(Object.prototype.hasOwnProperty.call(changedSet,keys[i])?(parsedKeyed[full]||null):null);
          |  }
          |  // Focus spans the WHOLE region: a retained row keeps its DOM, but MOVING it is a remove and an insert,
          |  // which blurs whatever it holds, so a pure reorder is precisely the case this must survive.
          |  var oldRoots=kyoRangeRoots(endpoints.start,endpoints.end),active=document.activeElement;
          |  var inside=active&&active!==document.body&&kyoRangeContains(oldRoots,active);
          |  var activeLocator=inside?kyoRangeFocusLocator(oldRoots,active):null;
          |  var ss=inside&&typeof active.selectionStart==="number"?active.selectionStart:null;
          |  var se=inside&&typeof active.selectionEnd==="number"?active.selectionEnd:null;
          |  var disturbed=[],repainted=[];
          |  for(i=0;i<fromKeys.length;i++){
          |    var fk=fromKeys[i];
          |    if(fk===null||!Object.prototype.hasOwnProperty.call(targetKeys,fk))disturbed.push(fromNodes[i]);
          |  }
          |  for(i=0;i<toKeys.length;i++)if(toNodes[i]){var lv=fromKeyed[toKeys[i]];if(lv){repainted.push(lv);disturbed.push(lv);}}
          |  var survivors=fragment?kyoLeavePathsRoots(kyoRangeFragmentRoots(fragment)):{};
          |  var oldEnter=kyoEnterPathsRoots(__kyoLogicalElements(repainted));
          |  var oldFocus=kyoFocusPathsRoots(__kyoLogicalElements(repainted));
          |  var ghosts=kyoLeavePrepareRoots(__kyoLogicalElements(disturbed),survivors);
          |  var touched=[];
          |  function place(from,ti){var to=toNodes[ti];if(to){__kyoPatchLogical(parent,from,to);
          |    var live=fromKeyed[toKeys[ti]];if(live&&live.nodeType===1)touched.push(live);}}
          |  var head=0,tail=fromNodes.length-1,toHead=0,toTail=toKeys.length-1,tailBoundary=endpoints.end,scanning=true;
          |  while(scanning&&head<=tail&&toHead<=toTail){
          |    if(fromKeys[head]===null||fromKeys[tail]===null)scanning=false;
          |    else if(fromKeys[head]===toKeys[toHead]){place(fromNodes[head],toHead);head++;toHead++;}
          |    else if(fromKeys[tail]===toKeys[toTail]){place(fromNodes[tail],toTail);tailBoundary=fromNodes[tail];tail--;toTail--;}
          |    else if(fromKeys[head]===toKeys[toTail]){
          |      if(head!==tail)__kyoMoveLogicalBefore(parent,fromNodes[head],tailBoundary);
          |      place(fromNodes[head],toTail);tailBoundary=fromNodes[head];head++;toTail--;}
          |    else if(fromKeys[tail]===toKeys[toHead]){
          |      if(tail!==head)__kyoMoveLogicalBefore(parent,fromNodes[tail],fromNodes[head]);
          |      place(fromNodes[tail],toHead);tail--;toHead++;}
          |    else scanning=false;
          |  }
          |  var cursor=head<=tail?fromNodes[head]:tailBoundary;
          |  for(i=toHead;i<=toTail;i++){
          |    var key=toKeys[i],live2=fromKeyed[key];
          |    if(live2){if(live2!==cursor)__kyoMoveLogicalBefore(parent,live2,cursor);else cursor=__kyoLogicalNext(cursor);
          |      place(live2,i);}
          |    else if(toNodes[i]){__kyoInsertLogicalClone(parent,toNodes[i],cursor);
          |      var ins=__kyoLogicalElements([toNodes[i]]);for(var q=0;q<ins.length;q++)touched.push(ins[q]);}
          |    else kyoRangeFail("row is neither retained nor rendered: "+key);
          |  }
          |  for(i=0;i<fromKeys.length;i++){
          |    var lk=fromKeys[i];
          |    if((lk===null||!Object.prototype.hasOwnProperty.call(targetKeys,lk))&&document.contains(fromNodes[i]))
          |      __kyoRemoveLogical(parent,fromNodes[i]);
          |  }
          |  var finalRoots=kyoRangeRoots(endpoints.start,endpoints.end);
          |  for(i=0;i<finalRoots.length;i++)applyJsProps(finalRoots[i]);
          |  var restored=kyoRangeResolveFocus(finalRoots,activeLocator);
          |  if(restored){restored.focus({preventScroll:true});if(ss!==null)kyoSetCaret(restored,ss,se);}
          |  kyoSeedEnterRoots(touched,oldEnter);kyoSeedFocusRoots(touched,oldFocus);kyoSpawnGhosts(ghosts);
          |  for(i=0;i<touched.length;i++)__kyoPortalSweep(touched[i]);
          |  __kyoPortalSweep(null);sweepFocusAuto();sweepScrollAuto(true);
          |}
          |// The elements of a set of logical children, each span contributing the elements it brackets.
          |function __kyoLogicalElements(nodes){
          |  var out=[];
          |  for(var i=0;i<nodes.length;i++)__kyoEachSpanNode(nodes[i],function(n){if(n.nodeType===1)out.push(n);});
          |  return out;
          |}
          |// The topmost node above this one: the Document for anything attached, the detached subtree's own root
          |// otherwise. Two nodes share a tree exactly when this returns the same node for both. Twin of treeRoot
          |// in DomReactiveRegions.
          |function __kyoTreeRoot(node){var top=node;while(top.parentNode)top=top.parentNode;return top;}
          |// Is this live marker sitting inside a portal twin — an element the sweep re-homed to <body>?
          |// Twin of inPortalTwin in DomReactiveRegions.
          |function __kyoInPortalTwin(node){
          |  var el=node.parentNode;
          |  while(el&&el.nodeType===1){
          |    if(el.parentNode===document.body&&el.hasAttribute("data-kyo-portal"))return true;
          |    el=el.parentNode;}
          |  return false;
          |}
          |function kyoRangeReplace(id,html){
          |  if(!kyoRangeId(id))kyoRangeFail("malformed replacement id: "+id);
          |  if(!__kyoRanges)kyoRangeFail("registry is closed");var endpoints=__kyoRanges.get(id);
          |  if(!endpoints)kyoRangeFail("unknown id: "+id);
          |  if(kyoRangeMarker(endpoints.start,"kyo-rs:")!==id||endpoints.end.data!=="kyo-re:"+id)kyoRangeFail("markers are corrupted: "+id);
          |  if(!endpoints.start.parentNode||endpoints.start.parentNode!==endpoints.end.parentNode)kyoRangeFail("anchors are no longer siblings: "+id);
          |  var ordered=endpoints.start.nextSibling;while(ordered&&ordered!==endpoints.end)ordered=ordered.nextSibling;
          |  if(!ordered)kyoRangeFail("end is not after start: "+id);
          |  var range=document.createRange();range.setStartAfter(endpoints.start);range.setEndBefore(endpoints.end);
          |  var parent=endpoints.start.parentNode,synthetic=parent.tagName==="TBODY"&&parent.getAttribute("data-kyo-range-host")===id;
          |  var parser=range;if(synthetic){parser=document.createRange();parser.selectNode(parent);}
          |  var fragment=parser.createContextualFragment(html),incoming=kyoRangeScan(fragment),removed=[];
          |  // Gone from the registry's point of view: inside the range about to be patched, or stranded in a tree
          |  // this one no longer shares. The second case is a portal twin the sweep retired — its markers went with
          |  // it into a detached subtree, and the sweep moves DOM without walking the registry, so the entry
          |  // outlives the nodes it names. A dead entry is not a duplicate of the region coming back; it is the
          |  // same region's corpse, and keeping it would refuse the live one.
          |  //
          |  // The test is "same tree", not "in the document": a live portal twin under <body> shares the document
          |  // with the range being patched and therefore stays, which is what the twin check below relies on.
          |  // Twin of the `removed` computation in DomReactiveRegions.
          |  var liveRoot=__kyoTreeRoot(endpoints.start);
          |  __kyoRanges.forEach(function(pair,key){
          |    if(key!==id&&(range.intersectsNode(pair.start)||__kyoTreeRoot(pair.start)!==liveRoot))removed.push(key);});
          |  // A region registered inside a portal twin is not a second copy of itself: the payload carries the portal
          |  // element inline (the twin's inline original), and this patch either morphs the twin in place — markers,
          |  // and so the registration, untouched — or replaces the range wholesale, in which case the incoming markers
          |  // take the id over and the sweep retires the stale twin. It must NOT join `removed` either: nothing inside
          |  // the live range re-registers it, so deleting it would leave the next write to that region unknown.
          |  incoming.forEach(function(pair,key){
          |    if(__kyoRanges.has(key)&&removed.indexOf(key)<0&&!__kyoInPortalTwin(__kyoRanges.get(key).start))
          |      kyoRangeFail("duplicate id: "+key);});
          |  var oldRoots=kyoRangeRoots(endpoints.start,endpoints.end),newRoots=kyoRangeFragmentRoots(fragment),newSemanticRoots=kyoRangeSemanticRoots(newRoots,id),active=document.activeElement;
          |  // The pre-patch reads (focused node, enter and leave path sets, ghost clones of what is about to depart)
          |  // happen before either path touches the DOM, and the post-patch tail below runs for both: a morph is
          |  // still a patch, and a leave transition or a portal re-home has no business depending on which one painted.
          |  var inside=active&&active!==document.body&&kyoRangeContains(oldRoots,active);
          |  var activeLocator=inside?kyoRangeFocusLocator(oldRoots,active):null;
          |  var ss=inside&&typeof active.selectionStart==="number"?active.selectionStart:null;
          |  var se=inside&&typeof active.selectionEnd==="number"?active.selectionEnd:null;
          |  var oldEnter=kyoEnterPathsRoots(oldRoots),ghosts=kyoLeavePrepareRoots(oldRoots,kyoLeavePathsRoots(newSemanticRoots));
          |  var oldFocus=kyoFocusPathsRoots(oldRoots);
          |  var finalRoots,morphed=!synthetic&&kyoRangeMorph(endpoints,fragment,incoming,synthetic);
          |  if(morphed){finalRoots=kyoRangeRoots(endpoints.start,endpoints.end);
          |    for(var i=0;i<removed.length;i++)__kyoRanges.delete(removed[i]);
          |    kyoRangeRescan(id,endpoints).forEach(function(pair,key){__kyoRanges.set(key,pair);});
          |    for(var i=0;i<finalRoots.length;i++)applyJsProps(finalRoots[i]);
          |  }else{
          |  range.deleteContents();for(var i=0;i<removed.length;i++)__kyoRanges.delete(removed[i]);
          |  finalRoots=newSemanticRoots;if(synthetic&&newRoots.length===1&&newRoots[0].tagName==="TBODY"&&newRoots[0].getAttribute("data-kyo-range-host")===id){
          |    var incomingHost=newRoots[0];kyoRangeSyncHost(parent,incomingHost,id);while(incomingHost.firstChild)parent.insertBefore(incomingHost.firstChild,endpoints.end);finalRoots=kyoRangeRoots(endpoints.start,endpoints.end);
          |  }else if(synthetic){var table=parent.parentNode;if(!table)kyoRangeFail("table range host is detached: "+id);
          |    table.insertBefore(endpoints.start,parent);table.insertBefore(fragment,parent);table.insertBefore(endpoints.end,parent);table.removeChild(parent);
          |  }else{endpoints.end.parentNode.insertBefore(fragment,endpoints.end);if(newRoots.length&&newRoots[0].tagName==="TBODY"&&newRoots[0].getAttribute("data-kyo-range-host")===id){
          |    newRoots[0].insertBefore(endpoints.start,newRoots[0].firstChild);newRoots[0].appendChild(endpoints.end);finalRoots=kyoRangeRoots(endpoints.start,endpoints.end);}}
          |  incoming.forEach(function(pair,key){__kyoRanges.set(key,pair);});
          |  for(var i=0;i<finalRoots.length;i++){applyJsProps(finalRoots[i]);ba(finalRoots[i]);}
          |  }
          |  var restored=kyoRangeResolveFocus(finalRoots,activeLocator);if(restored){restored.focus({preventScroll:true});if(ss!==null)kyoSetCaret(restored,ss,se);}
          |  kyoSeedEnterRoots(finalRoots,oldEnter);kyoSeedFocusRoots(finalRoots,oldFocus);kyoSpawnGhosts(ghosts);
          |  // Portal upkeep over the roots this patch inserted, and retirement of a placeholder it removed
          |  // (twin of the portalSweep in DomBackend.finishRangePatch).
          |  for(var pi=0;pi<finalRoots.length;pi++)__kyoPortalSweep(finalRoots[pi]);
          |  __kyoPortalSweep(null);
          |  sweepFocusAuto();
          |  // A patch that left the flag on a different element scrolls that element into view; one that left it
          |  // where it was scrolls nothing.
          |  sweepScrollAuto(true);
          |}
          |var __kyoRanges=kyoRangeScan(document.body);
          |window.addEventListener("pagehide",function(){if(__kyoRanges){__kyoRanges.clear();__kyoRanges=null;}});
          |function kyoClientError(error){if(window.console&&console.error)console.error(error);}""".stripMargin

    /** The keys that activate a button, and the keys that follow a link, as JavaScript conditions.
      * Named here so the client script's line stays readable; the values are [[KeyPolicy]]'s.
      */
    private val jsButtonActivation = KeyPolicy.jsKeyTest(KeyPolicy.buttonActivationKeys)
    private val jsLinkActivation   = KeyPolicy.jsKeyTest(KeyPolicy.linkActivationKeys)

    /** The server-push client script. `private[kyo]` so KeyPolicyTest can hold the two copies of the
      * keyboard rules against each other; nothing outside this module builds it.
      */
    private[kyo] def clientJs(basePath: String): String =
        s"""(function(){
           |var base="$basePath";
           |var __q=[];
           |// Mirrors DomBackend.setSelection: the one place that knows the two ways a caret move can be a no-op.
           |// Elements outside input and textarea (select, contenteditable) have no setSelectionRange at all, and
           |// on input types without a text selection (email, number) it throws InvalidStateError; in both cases
           |// the value is set and only the caret stays put. Every other exception is a real failure and propagates.
           |function kyoSetCaret(t,s,e){if(typeof t.setSelectionRange!=="function")return;
           |  try{t.setSelectionRange(s,e);}catch(er){if(er.name!=="InvalidStateError")throw er;}}
           |$reactiveRangesJs
           |// A field renders its .value PROPERTY, and the property stops tracking the attribute the first time the
           |// user types. Patching the attribute alone is therefore invisible on any touched field, so mirror it onto
           |// the property. Assigning only on a real difference leaves a focused field's caret alone (the echo of the
           |// user's own keystroke compares equal). Twin of DomBackend.syncFieldProperty; keep in lockstep.
           |function __kyoSyncField(el,name,value){
           |  if(name==="value"&&(el.tagName==="INPUT"||el.tagName==="TEXTAREA")&&el.value!==value)el.value=value;
           |}
           |// Mark an attr name as owned by the imperative id-addressed channel: names live in a __kyoOwn expando dict
           |// ON the element (reclaimed with the node), which kyoRangeMorph reads to shield each owned attr from
           |// reconciliation. Mirrors markOwned in DomBackend.
           |function __kyoMark(el,n){(el.__kyoOwn||(el.__kyoOwn={}))[n]=true;}
           |var ws=new WebSocket((location.protocol===\"https:\"?\"wss:\":\"ws:\")+"//"+location.host+base+"/_kyo/ws");
           |ws.onopen=function(){__q.forEach(function(m){ws.send(m);});__q=[];};
           |${DragClientJs.script(basePath)}
           |var __dragRt=installDragRuntime(function(m){post(m);},{onClose:function(c){__dragCleanup=c;}});
           |var __dragCleanup=__dragRt.cleanup;
           |ws.onclose=function(){if(__dragCleanup){__dragCleanup();__dragCleanup=null;}};
           |ws.onmessage=function(e){
           |  var op=JSON.parse(e.data);
           |  if(op.ResolveDrag){
           |    try{__dragRt.resolve(op.ResolveDrag.sessionId,op.ResolveDrag.decision);}catch(error){kyoClientError(error);}
           |  }else if(op.ReadDropFile||op.ReadDropDirectory||op.CancelDropRead){
           |    try{__dragRt.serveDropRead(op);}catch(error){kyoClientError(error);}
           |  }else if(op.PatchList){
          |    try{__kyoApplyList(op.PatchList.regionId,op.PatchList.keys,op.PatchList.changedKeys,op.PatchList.changed);}
          |    catch(error){kyoClientError(error);}
          |  }else if(op.ReplaceRange){
           |    try{kyoRangeReplace(op.ReplaceRange.regionId,op.ReplaceRange.html);}catch(error){kyoClientError(error);}
           |  }else if(op.Replace){
           |    var p=op.Replace.path.join(".");
           |    var el=document.querySelector('[data-kyo-path="'+p+'"]');
           |    // Replace is the SVG-only, path-addressed operation. HTML boundaries use ReplaceRange above.
           |    if(el&&el.outerHTML!==op.Replace.html){
           |      var ae=document.activeElement;
           |      var ap=ae&&ae!==document.body&&ae.getAttribute?ae.getAttribute("data-kyo-path"):null;
           |      var ss=(ae&&typeof ae.selectionStart==='number')?ae.selectionStart:null;
           |      var se=(ae&&typeof ae.selectionEnd==='number')?ae.selectionEnd:null;
           |      var __en=faEnterPaths(el);
           |      var __gh=kyoLeavePrepare(el,kyoLeaveSurv(op.Replace.html));
           |      var __fa=focusAutoPaths(el);
           |      el.outerHTML=op.Replace.html;
           |      var nel=document.querySelector('[data-kyo-path="'+p+'"]');if(nel){applyJsProps(nel);ba(nel);}
           |      if(ap){var rf=document.querySelector('[data-kyo-path="'+ap+'"]');if(rf){rf.focus({preventScroll:true});if(ss!==null)kyoSetCaret(rf,ss,se);}}
           |      // Seed after focus/caret restore so a newly appeared focus-auto element wins restore-to-trigger.
           |      if(nel){kyoEnterSeed(nel,__en);seedFocusAuto(nel,__fa);}
           |      kyoSpawnGhosts(__gh);
           |      // Portal upkeep over what this replace inserted (twin of DomBackend.portalSweep).
           |      if(nel)__kyoPortalSweep(nel);
           |    }
           |    sweepFocusAuto();sweepScrollAuto(true);
           |  }else if(op.Remove){
           |    var p=op.Remove.path.join(".");
           |    var el=document.querySelector('[data-kyo-path="'+p+'"]');
           |    var __rgh=el?kyoLeavePrepare(el,{}):[];
           |    if(el)el.remove();
           |    kyoSpawnGhosts(__rgh);
           |    // The removed subtree may have held portal placeholders: retire their body twins.
           |    __kyoPortalSweep(null);
           |    sweepFocusAuto();sweepScrollAuto(true);
           |  }else if(op.DevtoolsStats){
           |    // The overlay is only there when the page carried its script; a frame that arrives without it
           |    // is dropped rather than queued, because the numbers describe a moment that has passed.
           |    if(window.__kyoDev)window.__kyoDev.push(JSON.parse(op.DevtoolsStats.payload));
           |  }else if(op.InjectCss){
           |    var s=document.createElement("style");
           |    s.textContent=op.InjectCss.css;
           |    document.head.appendChild(s);
           |  }else if(op.ScrollIntoView){
           |    // The scroll may arrive in the same batch as the Replace that introduces its target, so
           |    // resolve the id after this frame's DOM writes have applied.
           |    requestAnimationFrame(function(){
           |      var el=document.getElementById(op.ScrollIntoView.id);
           |      if(el)el.scrollIntoView({behavior:"smooth",block:"start"});
           |    });
           |  }else if(op.Command){
           |    kyoApplyVerb(__kyoResolveEl(op.Command.path.join(".")),op.Command.verb);
           |  }else if(op.CommandById){
           |    kyoApplyVerb(document.getElementById(op.CommandById.id),op.CommandById.verb);
           |  }else if(op.RequestMeasure){
           |    var rmp=op.RequestMeasure.path.join(".");
           |    var rmel=__kyoResolveEl(rmp);
           |    if(rmel){
           |      var rmr=rmel.getBoundingClientRect();
           |      post({Measure:{path:op.RequestMeasure.path,rectX:rmr.left,rectY:rmr.top,rectW:rmr.width,rectH:rmr.height,viewportW:window.innerWidth,viewportH:window.innerHeight}});
           |    }
           |  }else if(op.RequestMeasureById){
           |    // MeasureById carries `id` back; `path` is vestigial for id-routing, sent empty.
           |    var rmiel=document.getElementById(op.RequestMeasureById.id);
           |    if(rmiel){
           |      var rmir=rmiel.getBoundingClientRect();
           |      post({MeasureById:{path:[],id:op.RequestMeasureById.id,rectX:rmir.left,rectY:rmir.top,rectW:rmir.width,rectH:rmir.height,viewportW:window.innerWidth,viewportH:window.innerHeight}});
           |    }
           |  }else if(op.SetClassById){
           |    var scel=document.getElementById(op.SetClassById.id);if(scel){__kyoMark(scel,"class");scel.classList.toggle(op.SetClassById.className,op.SetClassById.on);}
           |  }else if(op.SetStyleById){
           |    var ssel=document.getElementById(op.SetStyleById.id);
           |    if(ssel){__kyoMark(ssel,"style");var ssd=op.SetStyleById.css.split(";");for(var ssi=0;ssi<ssd.length;ssi++){var ssc=ssd[ssi].trim();if(!ssc)continue;var sso=ssc.indexOf(":");if(sso>0)ssel.style.setProperty(ssc.substring(0,sso).trim(),ssc.substring(sso+1).trim());}}
           |  }else if(op.SetAttrById){
           |    // set an attribute in place (element stays put, so a CSS `>` anchored on it keeps matching).
           |    var sael=document.getElementById(op.SetAttrById.id);if(sael){__kyoMark(sael,op.SetAttrById.name);sael.setAttribute(op.SetAttrById.name,op.SetAttrById.value);__kyoSyncField(sael,op.SetAttrById.name,op.SetAttrById.value);}
           |  }else if(op.SetAttrByPath){
           |    // Regions carry no element of their own (comment markers), so the path resolves uniquely to the content element.
           |    var sapp=op.SetAttrByPath.path.join(".");var sapel=document.querySelector(__kyoPathSel(sapp));if(sapel){__kyoMark(sapel,op.SetAttrByPath.name);sapel.setAttribute(op.SetAttrByPath.name,op.SetAttrByPath.value);__kyoSyncField(sapel,op.SetAttrByPath.name,op.SetAttrByPath.value);}
           |  }else if(op.SetBoolAttrByPath){
           |    var sbpp=op.SetBoolAttrByPath.path.join(".");var sbpel=document.querySelector(__kyoPathSel(sbpp));if(sbpel){__kyoMark(sbpel,op.SetBoolAttrByPath.name);if(op.SetBoolAttrByPath.value){sbpel.setAttribute(op.SetBoolAttrByPath.name,'');}else{sbpel.removeAttribute(op.SetBoolAttrByPath.name);}}
           |  }else if(op.SetClassByPath){
           |    // Path-addressed reactive class: toggle in place so CSS transitions fire; own "class" so a morph won't reconcile it.
           |    var scpp=op.SetClassByPath.path.join(".");var scpel=document.querySelector(__kyoPathSel(scpp));if(scpel){__kyoMark(scpel,'class');scpel.classList.toggle(op.SetClassByPath.name,op.SetClassByPath.on);}
           |  }else if(op.ObserveViewportById){
           |    var vid=op.ObserveViewportById.id;
           |    window.__kyoVpObs=window.__kyoVpObs||{};
           |    if(!window.__kyoVpObs[vid]){
           |      var vh=function(){var ve=document.getElementById(vid);if(ve){var vr=ve.getBoundingClientRect();post({MeasureById:{path:[],id:vid,rectX:vr.left,rectY:vr.top,rectW:vr.width,rectH:vr.height,viewportW:window.innerWidth,viewportH:window.innerHeight}});}};
           |      window.__kyoVpObs[vid]=vh;
           |      window.addEventListener("scroll",vh,true);
           |      window.addEventListener("resize",vh);
           |      vh();
           |    }
           |  }else if(op.UnobserveViewportById){
           |    var uid=op.UnobserveViewportById.id;
           |    if(window.__kyoVpObs&&window.__kyoVpObs[uid]){
           |      var uh=window.__kyoVpObs[uid];
           |      window.removeEventListener("scroll",uh,true);
           |      window.removeEventListener("resize",uh);
           |      delete window.__kyoVpObs[uid];
           |    }
           |  }
           |};
           |// Shared verb whitelist for Command/CommandById; unknown verbs ignored (forward-compat).
           |function kyoApplyVerb(el,verb){
           |  if(!el)return;
           |  if(verb==="focus"){var fs='input,textarea,select,button,a[href],[tabindex],[contenteditable]';var ft=(el.matches&&el.matches(fs))?el:(el.querySelector?el.querySelector(fs):null);if(ft&&typeof ft.focus==="function")ft.focus();}
           |  else if(verb==="scrollIntoView"){if(typeof el.scrollIntoView==="function")el.scrollIntoView({block:"nearest"});}
           |}
           |// A reactive range's id encodes its own path (ReactiveRegion.htmlId: 8 hex digits of segment length,
           |// then 4 hex digits per UTF-16 unit, optional 'n' + depth suffix), so the path is read back out of the
           |// id rather than re-encoded here. Returns null for anything that is not a range id.
           |function __kyoRangeIdPath(id){
           |  if(!id||id.charAt(0)!=="r")return null;
           |  var end=id.indexOf("n",1);if(end<0)end=id.length;
           |  var i=1,segs=[];
           |  while(i<end){
           |    if(i+8>end)return null;
           |    var len=parseInt(id.substring(i,i+8),16);i+=8;
           |    var s="";
           |    for(var k=0;k<len;k++){if(i+4>end)return null;s+=String.fromCharCode(parseInt(id.substring(i,i+4),16));i+=4;}
           |    segs.push(s);
           |  }
           |  return segs.join(".");
           |}
           |function __kyoPathSel(p){var bs=String.fromCharCode(92),q=String.fromCharCode(34);var s=String(p).split(bs).join(bs+bs).split(q).join(bs+q);return '[data-kyo-path='+q+s+q+']';}
           |// Path-addressed command/measure target: the element carrying the path, else the first element inside
           |// the reactive range that owns the path (a range is delimited by comments, so it has no element of its own).
           |function __kyoResolveEl(p){
           |  var el=document.querySelector(__kyoPathSel(p));
           |  if(el)return el;
           |  var found=null;
           |  if(__kyoRanges)__kyoRanges.forEach(function(pair,id){
           |    if(found||__kyoRangeIdPath(id)!==p)return;
           |    var n=pair.start.nextSibling;
           |    while(n&&n!==pair.end){if(n.nodeType===1&&!found)found=n;n=n.nextSibling;}
           |  });
           |  return found;
           |}
           |function fp(el){
           |  while(el&&el!==document.body){
           |    if(el.hasAttribute("data-kyo-path"))return el;
           |    el=el.parentElement;
           |  }
           |  return null;
           |}
           |function he(el,t){
           |  // Walk up from `el` checking each ancestor for the event marker.
           |  // Bubbling events (keydown/keyup/click) are forwarded if ANY ancestor declared the handler.
           |  var n=el;
           |  while(n&&n!==document.body){
           |    var ev=n.getAttribute&&n.getAttribute("data-kyo-ev");
           |    if(ev&&ev.split(",").indexOf(t)>=0)return true;
           |    // Portal hop: a re-homed element's DOM parent is <body>; continue the walk at its LOGICAL parent (its
           |    // placeholder slot's parent) so handlers declared on logical ancestors still forward. The server
           |    // dispatch bubbles over the logical tree anyway. Twin of DomBackend.declaredInChain.
           |    if(n.hasAttribute&&n.hasAttribute("data-kyo-portal")&&n.parentNode===document.body){
           |      var pp=n.getAttribute("data-kyo-path");
           |      var sl=pp!==null?document.querySelector('[data-kyo-portal-slot="'+pp+'"]'):null;
           |      n=sl?sl.parentElement:null;
           |    }else n=n.parentElement;
           |  }
           |  return false;
           |}
           |// Send each event over the single WebSocket. ws.send preserves send order on one socket, so the
           |// explicit fetch-queue serialization is no longer needed. Events fired before the socket opens are
           |// buffered in __q and flushed on ws.onopen.
           |function post(b){
           |  var m=JSON.stringify(b);
           |  if(ws.readyState===1)ws.send(m);
           |  else __q.push(m);
           |}
           |function pa(el){
           |  var p=el.getAttribute("data-kyo-path");
           |  return p===""?[]:p.split(".");
           |}
           |// Apply data-kyo-prop-* HTML attributes as JS DOM properties then remove the attr.
           |// Mirrors DomBackend.applyJsPropsSync for the HTTP/JVM rendering path.
           |function applyJsPropsElement(el,pfx){for(var i=el.attributes.length-1;i>=0;i--){var n=el.attributes[i].name;
           |  if(n.indexOf(pfx)===0){el[n.slice(pfx.length)]=el.getAttribute(n);el.removeAttribute(n);}}
           |}
           |function applyJsProps(root){
           |  var pfx="data-kyo-prop-";applyJsPropsElement(root,pfx);var els=root.querySelectorAll("*");
           |  for(var i=0;i<els.length;i++)applyJsPropsElement(els[i],pfx);
           |}
           |// Start freshly-inserted SMIL animations. Chart transition <animate> elements use
           |// begin="indefinite" so they do not auto-play against the shared document timeline (which would
           |// snap a post-load update to its frozen end value); beginElement() starts them relative to the
           |// insertion. Deferred one frame so the SMIL engine has registered the new nodes.
           |function ba(root){
           |  if(!root||!root.querySelectorAll)return;
           |  var an=root.querySelectorAll("animate,animateTransform,animateMotion");
           |  if(!an.length)return;
           |  requestAnimationFrame(function(){for(var i=0;i<an.length;i++){try{an[i].beginElement();}catch(e){}}});
           |}
           |// ---- enter/leave transition helpers (client-local; nothing crosses the wire) ----
           |// Set of data-kyo-enter paths under root (root included): the enter elements present BEFORE a patch.
           |function faEnterPaths(root){
           |  var s={};if(!root)return s;
           |  if(root.hasAttribute&&root.hasAttribute("data-kyo-enter")){var rp=root.getAttribute("data-kyo-path");if(rp!==null)s[rp]=true;}
           |  var els=root.querySelectorAll?root.querySelectorAll("[data-kyo-enter]"):[];
           |  for(var i=0;i<els.length;i++){var ep=els[i].getAttribute("data-kyo-path");if(ep!==null)s[ep]=true;}
           |  return s;
           |}
           |// For each data-kyo-enter element under newRoot (root included) whose path is NOT in oldSet (newly appeared):
           |// add its enter classes, force a reflow (offsetWidth), then remove them next frame so the CSS transition runs.
           |function kyoEnterSeed(newRoot,oldSet){
           |  if(!newRoot)return;
           |  var cand=[];
           |  if(newRoot.hasAttribute&&newRoot.hasAttribute("data-kyo-enter"))cand.push(newRoot);
           |  var els=newRoot.querySelectorAll?newRoot.querySelectorAll("[data-kyo-enter]"):[];
           |  for(var i=0;i<els.length;i++)cand.push(els[i]);
           |  for(var j=0;j<cand.length;j++){
           |    var el=cand[j];var pth=el.getAttribute("data-kyo-path");
           |    if(pth===null||oldSet[pth])continue;
           |    var cls=el.getAttribute("data-kyo-enter").split(/\\s+/);
           |    for(var k=0;k<cls.length;k++){if(cls[k])el.classList.add(cls[k]);}
           |    void el.offsetWidth;
           |    (function(e2,cs){requestAnimationFrame(function(){for(var m=0;m<cs.length;m++){if(cs[m])e2.classList.remove(cs[m]);}});})(el,cls);
           |  }
           |}
           |// Set of paths of data-kyo-LEAVE elements in an HTML fragment (which leave-elements SURVIVE a Replace).
           |// Keyed on leave-carrying elements, NOT every data-kyo-path.
           |function kyoLeaveSurv(html){
           |  var s={};var t=document.createElement("template");t.innerHTML=html;
           |  var els=t.content.querySelectorAll("[data-kyo-leave]");
           |  for(var i=0;i<els.length;i++){var pp=els[i].getAttribute("data-kyo-path");if(pp!==null)s[pp]=true;}
           |  return s;
           |}
           |// Strip data-kyo-* and id from a subtree so a ghost clone is inert (no data-kyo-path/focus-auto selector collisions).
           |function kyoStrip(el){
           |  var all=[el];if(el.querySelectorAll){var ds=el.querySelectorAll("*");for(var i=0;i<ds.length;i++)all.push(ds[i]);}
           |  for(var j=0;j<all.length;j++){var e2=all[j];if(!e2.getAttributeNames)continue;var ns=e2.getAttributeNames();
           |    for(var k=0;k<ns.length;k++){if(ns[k].indexOf("data-kyo-")===0||ns[k]==="id")e2.removeAttribute(ns[k]);}}
           |}
           |// Prepare leave ghosts for the OUTERMOST data-kyo-leave elements under root being removed (path not in survSet).
           |// Captures rect+clone WHILE the node is still in the DOM (getBoundingClientRect on a detached node is zero).
           |function kyoLeavePrepare(root,survSet){
           |  if(!root)return [];
           |  var cand=[];
           |  if(root.getAttribute&&root.getAttribute("data-kyo-leave")!==null)cand.push(root);
           |  var els=root.querySelectorAll?root.querySelectorAll("[data-kyo-leave]"):[];
           |  for(var i=0;i<els.length;i++)cand.push(els[i]);
           |  var removed=[];
           |  for(var j=0;j<cand.length;j++){var pp=cand[j].getAttribute("data-kyo-path");if(pp===null||!survSet[pp])removed.push(cand[j]);}
           |  var out=[];
           |  for(var k=0;k<removed.length;k++){var inside=false;
           |    for(var m=0;m<removed.length;m++){if(m!==k&&removed[m].contains(removed[k])){inside=true;break;}}
           |    if(!inside)out.push(removed[k]);}
           |  var ghosts=[];
           |  for(var n=0;n<out.length;n++){
           |    var node=out[n];var rect=node.getBoundingClientRect();var leave=node.getAttribute("data-kyo-leave");
           |    var g=node.cloneNode(true);kyoStrip(g);
           |    g.style.position="fixed";g.style.left=rect.left+"px";g.style.top=rect.top+"px";
           |    g.style.width=rect.width+"px";g.style.height=rect.height+"px";g.style.margin="0";g.style.pointerEvents="none";
           |    g.setAttribute("data-kyo-ghost","1");
           |    ghosts.push({src:node,node:g,leave:leave});
           |  }
           |  return ghosts;
           |}
           |// Append prepared ghosts to <body>, add their leave classes next frame, remove on transitionend/animationend or a 1s safety.
           |// The survivor set is a PREDICTION: a preserved subtree (the opaque mount boundary) survives the patch despite not
           |// matching, so a ghost whose SOURCE is still in the document is dropped here, since a leave animation over the live
           |// element would be a false departure. Twin of DomBackend.spawnGhosts; keep in lockstep.
           |function kyoSpawnGhosts(ghosts){
           |  if(!ghosts)return;
           |  for(var i=0;i<ghosts.length;i++){(function(gh){
           |    if(gh.src&&document.contains(gh.src))return;
           |    var g=gh.node;document.body.appendChild(g);
           |    var cls=(gh.leave||"").split(/\\s+/);
           |    requestAnimationFrame(function(){for(var c=0;c<cls.length;c++){if(cls[c])g.classList.add(cls[c]);}});
           |    var done=false;
           |    function cleanup(){if(done)return;done=true;if(g.parentNode)g.parentNode.removeChild(g);}
           |    g.addEventListener("transitionend",cleanup);g.addEventListener("animationend",cleanup);
           |    setTimeout(cleanup,1000);
           |  })(ghosts[i]);}
           |}
           |// Portal upkeep after a patch (twin of DomBackend.portalSweep; keep the two in lockstep).
           |// ADOPT: every data-kyo-portal element still sitting inline under root (root included; freshly inserted by
           |// this patch) is re-homed to document.body behind an inert data-kyo-portal-slot placeholder at its logical
           |// position. Must run AFTER focus and enter seeding, both of which are subtree-scoped. Reparenting drops DOM
           |// focus, so focus and caret held inside the moved subtree are re-applied after the move; the enter
           |// transition is unaffected, since its from-state classes release on the NEXT frame.
           |// ORPHANS: a body twin whose placeholder is gone (its slot was removed or replaced) left with its region:
           |// prepare its leave ghost, remove it, then spawn, since kyoSpawnGhosts drops ghosts whose source is still
           |// connected. Document-wide by necessity: the twin sits outside every region subtree, so the regular leave
           |// sweep cannot see it.
           |// Twin of DomBackend.portalTwin: the (unique) re-homed body child carrying the path.
           |function __kyoPortalTwin(p){return document.querySelector('body > [data-kyo-path="'+p+'"][data-kyo-portal]');}
           |function __kyoPortalSweep(root){
           |  if(root&&root.querySelectorAll){
           |    var cand=[];
           |    if(root.hasAttribute&&root.hasAttribute("data-kyo-portal"))cand.push(root);
           |    var els=root.querySelectorAll("[data-kyo-portal]");
           |    for(var i=0;i<els.length;i++)cand.push(els[i]);
           |    for(var j=0;j<cand.length;j++){
           |      var el=cand[j];
           |      var p=el.getAttribute("data-kyo-path");
           |      // Only path-carrying elements can portal (the placeholder must key the slot); the parent check keeps
           |      // the sweep idempotent, since an adopted twin is a direct body child and never under a region again.
           |      if(p!==null&&el.parentNode&&el.parentNode!==document.body){
           |        var stale=__kyoPortalTwin(p);
           |        if(stale&&stale!==el)document.body.removeChild(stale);
           |        var svgNs="http://www.w3.org/2000/svg";
           |        var slot=(el.namespaceURI===svgNs)?document.createElementNS(svgNs,"g"):document.createElement("span");
           |        slot.setAttribute("data-kyo-portal-slot",p);
           |        slot.setAttribute("hidden","");
           |        var ae=document.activeElement;
           |        var had=ae&&ae!==document.body&&(el===ae||el.contains(ae));
           |        var ss=(had&&typeof ae.selectionStart==='number')?ae.selectionStart:null;
           |        var se=(had&&typeof ae.selectionEnd==='number')?ae.selectionEnd:null;
           |        el.parentNode.insertBefore(slot,el);
           |        document.body.appendChild(el);
           |        if(had){ae.focus({preventScroll:true});if(ss!==null&&typeof ae.setSelectionRange==='function'){try{ae.setSelectionRange(ss,se);}catch(e){if(e.name!=='InvalidStateError')throw e;}}}
           |      }
           |    }
           |  }
           |  var twins=document.querySelectorAll('body > [data-kyo-portal][data-kyo-path]');
           |  for(var t=0;t<twins.length;t++){
           |    var tw=twins[t];
           |    var tp=tw.getAttribute("data-kyo-path");
           |    if(!document.querySelector('[data-kyo-portal-slot="'+tp+'"]')){
           |      var tg=kyoLeavePrepare(tw,{});
           |      document.body.removeChild(tw);
           |      kyoSpawnGhosts(tg);
           |    }
           |  }
           |}
           |// Scroll-into-view for [data-kyo-scroll-auto]: the flag MOVES with a roving highlight, so the carrier
           |// set is what tells a moved flag from a re-rendered one. Mirrors DomBackend.sweepScrollAuto.
           |var __scrollAutoPaths={};
           |function sweepScrollAuto(scroll){
           |  var els=document.querySelectorAll("[data-kyo-scroll-auto]");
           |  var now={},fresh=null;
           |  for(var i=0;i<els.length;i++){
           |    var p=els[i].getAttribute("data-kyo-path");
           |    if(p===null)continue;
           |    now[p]=true;
           |    if(fresh===null&&!__scrollAutoPaths[p])fresh=els[i];
           |  }
           |  __scrollAutoPaths=now;
           |  // block:"nearest" moves the nearest scrollable ancestor by the least it can, and not at all when the
           |  // element is already visible, which is what a highlight walking within view wants.
           |  if(scroll&&fresh&&typeof fresh.scrollIntoView==='function')fresh.scrollIntoView({block:"nearest",inline:"nearest"});
           |}
           |// Focus seeding/restore for [data-kyo-focus-auto]/[data-kyo-focus-restore]; __focusReturnStack stacks {fa, ret|null, restore}.
           |// Mirrors DomBackend.focusReturnStack for the SPA transport.
           |var __focusReturnStack=[];
           |// Object-set (keyed by path) of every [data-kyo-focus-auto] path inside root, root included.
           |function focusAutoPaths(root){
           |  var s={};
           |  if(!root||!root.querySelectorAll)return s;
           |  if(root.hasAttribute&&root.hasAttribute("data-kyo-focus-auto")){var rp=root.getAttribute("data-kyo-path");if(rp!==null)s[rp]=true;}
           |  var els=root.querySelectorAll("[data-kyo-focus-auto]");
           |  for(var i=0;i<els.length;i++){var ep=els[i].getAttribute("data-kyo-path");if(ep!==null)s[ep]=true;}
           |  return s;
           |}
           |// Seed the FIRST newly-appeared focus-auto element under newRoot (path not in oldSet); record prior focus for the sweep.
           |function seedFocusAuto(newRoot,oldSet){
           |  if(!newRoot||!newRoot.querySelectorAll)return;
           |  var cand=[];
           |  if(newRoot.hasAttribute&&newRoot.hasAttribute("data-kyo-focus-auto"))cand.push(newRoot);
           |  var els=newRoot.querySelectorAll("[data-kyo-focus-auto]");
           |  for(var i=0;i<els.length;i++)cand.push(els[i]);
           |  for(var j=0;j<cand.length;j++){
           |    var fa=cand[j].getAttribute("data-kyo-path");
           |    if(fa!==null&&!oldSet[fa]){
           |      var ae=document.activeElement;
           |      var ret=(ae&&ae!==document.body&&ae.getAttribute)?ae.getAttribute("data-kyo-path"):null;
           |      __focusReturnStack.push({fa:fa,ret:ret,restore:cand[j].hasAttribute("data-kyo-focus-restore")});
           |      if(typeof cand[j].focus==='function')cand[j].focus({preventScroll:true});
           |      return;
           |    }
           |  }
           |}
           |// Unwind entries whose seeded element left the document, returning focus at most once. Mirrors DomBackend.sweepFocusAuto.
           |function sweepFocusAuto(){
           |  var restored=false;
           |  while(__focusReturnStack.length>0){
           |    var top=__focusReturnStack[__focusReturnStack.length-1];
           |    // Stop at the first seed still on screen: restoring an entry below it would move focus out of it.
           |    if(document.querySelector('[data-kyo-path="'+top.fa+'"][data-kyo-focus-auto]'))return;
           |    __focusReturnStack.pop();
           |    // At most one restore per unwind: a deeper entry belongs to a seed that closed while a newer one stayed
           |    // open, so its return target is stale and must not override the one just restored.
           |    // And only where the removal took the focus with it: the browser leaves activeElement on body when the
           |    // focused element goes, while any other element there means the reader moved focus themselves (a Tab out
           |    // of a combobox) and putting it back would undo that. Twin of DomBackend.focusWasLost.
           |    var lost=!document.activeElement||document.activeElement===document.body;
           |    if(!restored&&top.restore&&top.ret&&lost){var re=document.querySelector('[data-kyo-path="'+top.ret+'"]');if(re&&typeof re.focus==='function'){re.focus({preventScroll:true});restored=true;}}
           |  }
           |}
           |applyJsProps(document.body);ba(document.body);
           |kyoEnterSeed(document.body,{});
           |// Initial mount: everything server-rendered is new (empty old set), like native autofocus.
           |seedFocusAuto(document.body,{});
           |// Record what already carries the scroll flag without acting on it: a page that arrives with a
           |// highlight has not moved it, and scrolling on load would fight the browser's own restoration.
           |sweepScrollAuto(false);
           |// Portal adopt for the initial paint: a portal element present at load re-homes immediately.
           |__kyoPortalSweep(document.body);
           |// Dropdown helpers: close all dropdowns except the given id
           |function kyoCloseDropdown(exceptId){
           |  var all=document.querySelectorAll('[data-kyo-dropdown-options]');
           |  Array.prototype.forEach.call(all,function(el){
           |    var id=el.getAttribute('data-kyo-dropdown-options');
           |    if(id!==exceptId)el.hidden=true;
           |  });
           |}
           |// Build a mouse payload, omitting targetId when absent (null JSON would break Maybe[String] decode).
           |function mkMouse(mods,tid,pos){var m={modifiers:mods};if(tid)m.targetId=tid;if(pos)m.position=pos;return m;}
           |// Viewport coordinates of a pointer event; the events without a pointer (focus, blur, submit) pass no third argument.
           |function mkPos(e){return {x:e.clientX,y:e.clientY};}
           |// Build a keyboard payload, omitting targetId when absent.
           |function mkKbd(key,mods,tid){var k={key:key,modifiers:mods};if(tid)k.targetId=tid;return k;}
           |// onScrollPosition: rAF-coalesce bursts to one post per frame; capture-phase catches non-bubbling scroll.
           |var __scrRaf=0,__scrEl=null,__scrPath=null;
           |function __scrFlush(){__scrRaf=0;if(__scrEl){var stid=__scrEl.id?__scrEl.id:null;var sp={path:__scrPath,scrollTop:__scrEl.scrollTop,scrollLeft:__scrEl.scrollLeft};if(stid)sp.targetId=stid;post({ScrollPosition:sp});}}
           |function handle(e){
           |  var el=fp(e.target);
           |  if(!el)return;
           |  var p=pa(el),t=e.type;
           |  if(t==="click"){
           |    // preventActivation: an inert region declines the click's default too, so a readonly checkbox does
           |    // not toggle under the pointer either. The click still posts, since the component has already
           |    // dropped the handler that would have acted on it.
           |    if(e.target&&e.target.closest&&e.target.closest('[data-kyo-inert]'))e.preventDefault();
           |    // Dropdown trigger click: open/close the option list.
           |    // Skip isTrusted=false synthetic clicks (e.g. from runSpaceClickSynthesis after Space keydown).
           |    if(e.isTrusted!==false&&e.target&&e.target.getAttribute('data-kyo-dropdown-trigger')){
           |      var did=e.target.getAttribute('data-kyo-dropdown-trigger');
           |      var opts=document.querySelector('[data-kyo-dropdown-options="'+did+'"]');
           |      if(opts){
           |        var opening=opts.hidden;
           |        kyoCloseDropdown(opening?did:null);
           |        opts.hidden=!opening;
           |        if(!opts.hidden){
           |          var hlEl=opts.querySelector('[data-kyo-dropdown-hl]');
           |          if(!hlEl){var first=opts.querySelector('[data-kyo-dropdown-opt]');if(first)first.setAttribute('data-kyo-dropdown-hl','true');}
           |        }
           |      }
           |      return;
           |    }
           |    // Dropdown option click: confirm selection
           |    if(e.target&&e.target.getAttribute('data-kyo-dropdown-val')!==null){
           |      var val=e.target.getAttribute('data-kyo-dropdown-val');
           |      var wrap=e.target.closest('[data-kyo-dropdown]');
           |      if(wrap){
           |        var dOpts=document.querySelector('[data-kyo-dropdown-options="'+wrap.getAttribute('data-kyo-dropdown')+'"]');
           |        if(dOpts)dOpts.hidden=true;
           |        var wp=pa(wrap);
           |        post({Change:{path:wp,value:val}});
           |      }
           |      return;
           |    }
           |    // Prevent the browser's default navigation only where the anchor carries a kyo click handler,
           |    // so the handler rather than the href drives the action. A plain href keeps native behavior:
           |    // an in-page `#anchor` scrolls and a cross-document route is a real navigation. Preventing
           |    // every anchor kills both, which is what a navigation built from plain links runs into. Twin
           |    // of the same guard in DomBackend's click branch — including the modifier test: a
           |    // ctrl/cmd/shift/alt click is the user asking the browser for a new tab, so the default is
           |    // theirs, while the handler runs either way.
           |    var kmod=e.ctrlKey||e.metaKey||e.shiftKey||e.altKey||e.button!==0;
           |    var mid=e.target&&e.target.id?e.target.id:null;if(!kmod&&el.tagName&&el.tagName.toLowerCase()==='a'&&he(el,"click"))e.preventDefault();post({Click:{path:p,mouse:mkMouse({ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey},mid,mkPos(e))}});window._kyoClickSubmit=true;setTimeout(function(){window._kyoClickSubmit=false},0);
           |  }
           |  // Right-click: preventDefault suppresses the native menu only when a handler was declared.
           |  else if(t==="contextmenu"&&he(el,"contextmenu")){e.preventDefault();var cmid=e.target&&e.target.id?e.target.id:null;post({ContextMenu:{path:p,mouse:mkMouse({ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey},cmid,mkPos(e))}});}
           |  else if(t==="input"&&he(el,"input"))post({Input:{path:p,value:e.target.value}});
           |  else if(t==="change"&&he(el,"change")){
           |    var tgt=e.target,typ=tgt.type;
           |    if(typ==="checkbox"||typ==="radio")post({ChangeChecked:{path:p,checked:tgt.checked}});
           |    else if(typ==="number"||typ==="range")post({ChangeNumeric:{path:p,value:parseFloat(tgt.value)}});
           |    else if(typ==="file"){
           |      // fileselect token: read all files (indexed, order preserved), post one FileSelect when all complete.
           |      // Otherwise legacy: first file's text into a Change.
           |      var files=tgt.files;
           |      if(he(el,"fileselect")){
           |        var n=files.length;
           |        if(n>0){
           |          var results=new Array(n);var doneCt=0;
           |          for(var fidx=0;fidx<n;fidx++){(function(ix){
           |            var f=files[ix];var r=new FileReader();
           |            r.onload=function(){results[ix]={name:f.name,size:f.size,mimeType:f.type,content:r.result};doneCt++;if(doneCt===n)post({FileSelect:{path:p,files:results}});};
           |            r.readAsText(f);
           |          })(fidx);}
           |        }
           |      }else if(files.length>0){
           |        var r0=new FileReader();
           |        r0.onload=function(){post({Change:{path:p,value:r0.result}});};
           |        r0.readAsText(files[0]);
           |      }
           |    }
           |    else post({Change:{path:p,value:tgt.value}});
           |  }else if(t==="submit"){e.preventDefault();if(!window._kyoClickSubmit&&he(el,"submit")){var smid=e.target&&e.target.id?e.target.id:null;post({Submit:{path:p,mouse:mkMouse({ctrl:false,alt:false,shift:false,meta:false},smid)}});}}
           |  else if(t==="keydown"){
           |    // preventActivation: a region that declared itself inert declines the browser default for the keys
           |    // that change a native control's value, so a readonly checkbox stays focusable instead of being
           |    // disabled. The keydown still posts below. KeyPolicy is the rule; KeyPolicyTest holds this against it.
           |    if(e.target&&e.target.closest&&e.target.closest('[data-kyo-inert]')&&(${KeyPolicy.jsKeyTest(
              KeyPolicy.activationKeys
          )}))e.preventDefault();
           |    // preventScrollKeys: suppress native page-scroll for nav keys in a data-kyo-scroll-keys region; keydown still posts below.
           |    // Every key and tag list below is interpolated from KeyPolicy, which the SPA client calls directly,
           |    // so the two transports cannot answer this differently; KeyPolicyTest holds this text against it.
           |    if(e.target&&e.target.closest&&e.target.closest('[data-kyo-scroll-keys]')){
           |      var __sk=e.target,__ed=(/^(${KeyPolicy.jsTagTest(KeyPolicy.editableTags)})$$/.test(__sk.tagName)||__sk.isContentEditable);
           |      var __vc=(/^(${KeyPolicy.jsTagTest(KeyPolicy.verticalConsumerTags)})$$/.test(__sk.tagName)||__sk.isContentEditable);
           |      var __sa=(/^(${KeyPolicy.jsTagTest(KeyPolicy.spaceActivatedTags)})$$/.test(__sk.tagName));
           |      var __vk=(${KeyPolicy.jsKeyTest(KeyPolicy.verticalScrollKeys)});
           |      var __hk=(${KeyPolicy.jsKeyTest(KeyPolicy.edgeScrollKeys)});
           |      // Space is here for the same reason the arrows are: a list that is ONE tab stop has no
           |      // native control to consume it, so a Space that picks the highlighted row also scrolled
           |      // the page a screenful. It stays with whatever would type it or activate on it.
           |      var __spk=(e.key===" "&&!__ed&&!__sa);
           |      if((__vk&&!__vc)||(__hk&&!__ed)||__spk)e.preventDefault();
           |    }
           |    // An element with a click handler is activated TWICE once a keydown is posted at all: once
           |    // by the browser, once by the dispatcher, which emulates that activation where no browser
           |    // does it. Suppress the browser's, so it acts once and its own onKeyDown still sees the key.
           |    // A button takes Enter and Space, an anchor Enter alone (Space scrolls with a link focused).
           |    // The dispatcher answers a keydown by synthesizing the click it stands for — at the
           |    // button for keyboard activation, at the form's default button for implicit submission
           |    // — so the browser's own would be the second one. Twin of KeyPolicy.doubleActivates;
           |    // FormActivationTest holds this script against it.
           |    if(e.target&&e.target.tagName&&he(e.target,"keydown")){
           |      var __own=e.target.getAttribute("data-kyo-ev");
           |      var __ck=!!(__own&&__own.split(",").indexOf("click")>=0);
           |      var __tg=e.target.tagName;
           |      var __inf=!!(e.target.closest&&e.target.closest("form"));
           |      // An anchor keeps the click-handler gate: suppressing Enter on a plain link would take
           |      // its navigation with it, and navigation is the one thing the dispatcher does not carry.
           |      var __act=(__tg==="BUTTON")?($jsButtonActivation)
           |               :(__tg==="A")?(__ck&&($jsLinkActivation))
           |               :(__inf&&e.key==="Enter");
           |      if(__act)e.preventDefault();
           |    }
           |    // Focus-trap: when Tab is pressed inside a [data-kyo-focus-trap="1"] container,
           |    // wrap focus within the trap's focusable children instead of escaping to the page.
           |    // Escape falls through so the element's onKeyDown handler can close the modal.
           |    if(e.key==="Tab"&&e.target){
           |      var trap=e.target.closest('[data-kyo-focus-trap="1"]');
           |      if(trap){
           |        var focusables=Array.prototype.filter.call(
           |          trap.querySelectorAll('a[href],button:not([disabled]),input:not([disabled]):not([type=hidden]),select:not([disabled]),textarea:not([disabled]),[tabindex]:not([tabindex="-1"])'),
           |          function(fe){return !fe.hidden&&fe.offsetParent!==null;}
           |        );
           |        if(focusables.length>0){
           |          var ci=focusables.indexOf(document.activeElement);
           |          var dir=e.shiftKey?-1:1;
           |          var ni=((ci<0?0:ci)+dir+focusables.length)%focusables.length;
           |          var next=focusables[ni];
           |          if(next){
           |            // Reposition data-kyo-tab-prev so runTabFocusAdvance (Browser.press post-shim)
           |            // also lands on next rather than escaping the trap.
           |            var allF=Array.prototype.filter.call(
           |              document.querySelectorAll('a[href],button:not([disabled]),input:not([disabled]):not([type=hidden]),select:not([disabled]),textarea:not([disabled]),[tabindex]:not([tabindex="-1"])'),
           |              function(fe){return !fe.hidden&&fe.offsetParent!==null;}
           |            );
           |            var allPos=allF.filter(function(fe){return fe.tabIndex>0;}).sort(function(a,b){return a.tabIndex-b.tabIndex;});
           |            var allNat=allF.filter(function(fe){return fe.tabIndex<=0;});
           |            var allOrd=allPos.concat(allNat);
           |            var nextIdx=allOrd.indexOf(next);
           |            var oldMark=document.querySelector('[data-kyo-tab-prev="1"]');
           |            if(oldMark)oldMark.removeAttribute('data-kyo-tab-prev');
           |            if(nextIdx>=0){
           |              var preNext=allOrd[((nextIdx-dir+allOrd.length)%allOrd.length)];
           |              if(preNext)preNext.setAttribute('data-kyo-tab-prev','1');
           |            }
           |            next.focus();
           |            e.preventDefault();
           |            e.stopPropagation();
           |            return;
           |          }
           |        }
           |      }
           |    }
           |    // ArrowUp/Down on a focused <input type=number>: call stepUp()/stepDown() (respects step/min/max
           |    // natively), dispatch input + change events so kyo-ui's ChangeNumeric path fires, then
           |    // preventDefault to suppress the browser's native increment (which would otherwise double-step).
           |    // No return; the keydown post below still fires so onKeyDown handlers see ArrowUp/Down.
           |    if((e.key==="ArrowUp"||e.key==="ArrowDown")&&e.target&&e.target.tagName==="INPUT"&&e.target.type==="number"){
           |      if(!e.target.disabled&&!e.target.readOnly){
           |        if(e.key==="ArrowUp")e.target.stepUp();else e.target.stepDown();
           |        e.target.dispatchEvent(new Event("input",{bubbles:true}));
           |        e.target.dispatchEvent(new Event("change",{bubbles:true}));
           |        e.preventDefault();
           |      }
           |    }
           |    // Enter on a focused <select> would otherwise trigger the form's default submit;
           |    // kyo-ui treats Enter on Select as a dropdown interaction (see ReactiveUI.dispatchToElement),
           |    // so we suppress the browser default to keep TUI/browser parity.
           |    if(e.key==="Enter"&&e.target&&e.target.tagName==="SELECT")e.preventDefault();
           |    // Enter on a focused <input type=checkbox|radio>: HTML spec only activates these via Space,
           |    // not Enter. kyo-ui synthesizes Enter activation via click() for TUI/browser parity.
           |    // click() fires the native change event which the existing change handler picks up as
           |    // ChangeChecked. preventDefault stops any form submission. No return; keydown post fires.
           |    if(e.key==="Enter"&&e.target&&e.target.tagName==="INPUT"&&
           |       (e.target.type==="checkbox"||e.target.type==="radio")){
           |      if(!e.target.disabled){e.target.click();e.preventDefault();}
           |    }
           |    // Focus-group: ArrowLeft/Right cycles among siblings sharing data-kyo-focus-group.
           |    // preventDefault stops browser's native horizontal scroll-on-arrow.
           |    // No return; keydown post still fires so onKeyDown sees the event.
           |    if((e.key==="ArrowLeft"||e.key==="ArrowRight")&&e.target){
           |      var grp=e.target.getAttribute&&e.target.getAttribute("data-kyo-focus-group");
           |      if(grp){
           |        var peers=Array.prototype.slice.call(document.querySelectorAll('[data-kyo-focus-group="'+grp+'"]'));
           |        peers=peers.filter(function(pe){return !pe.disabled&&!pe.hidden&&pe.offsetParent!==null;});
           |        if(peers.length>1){
           |          var i=peers.indexOf(e.target);
           |          var dir=e.key==="ArrowRight"?1:-1;
           |          var nx=peers[((i<0?0:i)+dir+peers.length)%peers.length];
           |          if(nx&&nx!==e.target){nx.focus();e.preventDefault();}
           |        }
           |      }
           |    }
           |    // Custom dropdown (div-based): Space opens, ArrowDown/Up navigate, Enter confirms, Escape/Tab closes.
           |    // Type-ahead: single printable char jumps to next matching option.
           |    // Must run BEFORE the keydown post so early returns suppress server dispatch.
           |    var ddWrap=e.target&&e.target.closest('[data-kyo-dropdown]');
           |    if(ddWrap){
           |      var did2=ddWrap.getAttribute('data-kyo-dropdown');
           |      var opts2=did2?document.querySelector('[data-kyo-dropdown-options="'+did2+'"]'):null;
           |      var isOpen=opts2&&!opts2.hidden;
           |      var isSpaceKey=(e.key===' '||e.key==='Space'||e.keyCode===32||e.which===32);
           |      if(isSpaceKey&&!isOpen){
           |        kyoCloseDropdown(did2);opts2.hidden=false;
           |        var first2=opts2.querySelector('[data-kyo-dropdown-opt]');if(first2)first2.setAttribute('data-kyo-dropdown-hl','true');
           |        e.preventDefault();return;
           |      }
           |      if(isOpen){
           |        var items=Array.prototype.slice.call(opts2.querySelectorAll('[data-kyo-dropdown-opt]'));
           |        var hlEl2=opts2.querySelector('[data-kyo-dropdown-hl]');
           |        var hi=hlEl2?items.indexOf(hlEl2):0;
           |        if(e.key==='ArrowDown'){
           |          if(hlEl2)hlEl2.removeAttribute('data-kyo-dropdown-hl');
           |          items[(hi+1)%items.length].setAttribute('data-kyo-dropdown-hl','true');
           |          e.preventDefault();return;
           |        }
           |        if(e.key==='ArrowUp'){
           |          if(hlEl2)hlEl2.removeAttribute('data-kyo-dropdown-hl');
           |          items[((hi-1)+items.length)%items.length].setAttribute('data-kyo-dropdown-hl','true');
           |          e.preventDefault();return;
           |        }
           |        if(e.key==='Enter'){
           |          if(hlEl2){
           |            var val2=hlEl2.getAttribute('data-kyo-dropdown-val');
           |            opts2.hidden=true;
           |            post({Change:{path:pa(ddWrap),value:val2}});
           |          }
           |          e.preventDefault();return;
           |        }
           |        if(e.key==='Escape'){opts2.hidden=true;e.preventDefault();return;}
           |        if(e.key==='Tab'){opts2.hidden=true;}
           |        if(e.key.length===1){
           |          var ch=e.key.toLowerCase();
           |          var startIdx=(hi+1)%items.length;
           |          var found=null;
           |          for(var ii=0;ii<items.length&&!found;ii++){
           |            var candidate=items[(startIdx+ii)%items.length];
           |            if(candidate.textContent.trim().toLowerCase().charAt(0)===ch)found=candidate;
           |          }
           |          if(found){if(hlEl2)hlEl2.removeAttribute('data-kyo-dropdown-hl');found.setAttribute('data-kyo-dropdown-hl','true');}
           |          e.preventDefault();return;
           |        }
           |      }
           |      // Dropdown closed: suppress Enter (avoid form submit) and Space is handled above
           |      if((e.key==='Enter'||isSpaceKey)&&!isOpen&&opts2){e.preventDefault();return;}
           |    }
           |    if(he(el,"keydown")){var ktid=e.target&&e.target.id?e.target.id:null;post({KeyDown:{path:p,keyboard:mkKbd(e.key,{ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey},ktid)}});}
           |  }
           |  else if(t==="keyup"&&he(el,"keyup")){var kutid=e.target&&e.target.id?e.target.id:null;post({KeyUp:{path:p,keyboard:mkKbd(e.key,{ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey},kutid)}});}
           |  else if(t==="focus"&&he(el,"focus")){var ftid=e.target&&e.target.id?e.target.id:null;post({Focus:{path:p,mouse:mkMouse({ctrl:false,alt:false,shift:false,meta:false},ftid)}});}
           |  else if(t==="blur"&&he(el,"blur")){var btid=e.target&&e.target.id?e.target.id:null;post({Blur:{path:p,mouse:mkMouse({ctrl:false,alt:false,shift:false,meta:false},btid)}});}
           |  else if(t==="mouseover"&&he(el,"mouseover")){var hotid=e.target&&e.target.id?e.target.id:null;post({Hover:{path:p,mouse:mkMouse({ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey},hotid,mkPos(e))}});}
           |  else if(t==="mouseout"&&he(el,"mouseout")){var uhotid=e.target&&e.target.id?e.target.id:null;post({Unhover:{path:p,mouse:mkMouse({ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey},uhotid,mkPos(e))}});}
           |  // Do NOT auto-call preventDefault: leave native-scroll suppression to the handler, matching DomBackend. Server-side rendering cannot synchronously decline the event, so the default is to NOT prevent.
           |  else if(t==="wheel"&&he(el,"wheel")){var whtid=e.target&&e.target.id?e.target.id:null;var sc={path:p,deltaX:e.deltaX,deltaY:e.deltaY,modifiers:{ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey}};if(whtid)sc.targetId=whtid;post({Scroll:sc});}
           |  else if(t==="scroll"&&he(el,"scroll")){__scrEl=el;__scrPath=p;if(!__scrRaf)__scrRaf=requestAnimationFrame(__scrFlush);}
           |}
           |// ---- client-local input filter/mask (document-level beforeinput capture listener) ----
           |$inputMaskJs
           |function kyoSetVal(t,v){t.value=v;kyoSetCaret(t,v.length,v.length);t.dispatchEvent(new Event("input",{bubbles:true}));}
           |function kyoSetValAt(t,txt,s,en){var v=t.value;t.value=v.slice(0,s)+txt+v.slice(en);var np=s+txt.length;kyoSetCaret(t,np,np);t.dispatchEvent(new Event("input",{bubbles:true}));}
           |function kyoBeforeInput(e){
           |  var t=e.target;if(!t||!t.getAttribute)return;
           |  // Interactive.data lets any element carry data-kyo-filter, and everything below assumes a value
           |  // property and a text selection. Throwing from a beforeinput capture listener would break typing
           |  // for the whole page, so anything but a text field is left alone.
           |  if(t.tagName!=="INPUT"&&t.tagName!=="TEXTAREA")return;
           |  var filt=t.getAttribute("data-kyo-filter");var mask=t.getAttribute("data-kyo-mask");
           |  if(!filt&&!mask)return;
           |  var it=e.inputType||"";
           |  // insertCompositionText is deliberately absent below: preventDefault on it does not filter the input,
           |  // it aborts the composition, which breaks CJK input, dead keys and mobile autocorrect. Composition is
           |  // let through and the finished text is corrected in kyoCompositionEnd instead.
           |  if(filt){
           |    if(it.indexOf("delete")===0)return;
           |    if(it==="insertText"||it==="insertReplacementText"){
           |      var data=e.data;if(data==null)return;
           |      var f1=kyoFilterStr(filt,data,t.value);
           |      if(f1!==data){e.preventDefault();if(f1){var s=(typeof t.selectionStart==="number")?t.selectionStart:t.value.length;var en=(typeof t.selectionEnd==="number")?t.selectionEnd:s;kyoSetValAt(t,f1,s,en);}}
           |    }else if(it==="insertFromPaste"||it==="insertFromDrop"){
           |      e.preventDefault();var pasted=e.dataTransfer?e.dataTransfer.getData("text"):(e.data||"");
           |      var f2=kyoFilterStr(filt,pasted,t.value);if(f2){var s2=(typeof t.selectionStart==="number")?t.selectionStart:t.value.length;var e2=(typeof t.selectionEnd==="number")?t.selectionEnd:s2;kyoSetValAt(t,f2,s2,e2);}
           |    }
           |    return;
           |  }
           |  if(mask){
           |    var mts=kyoMaskParse(mask);
           |    if(it.indexOf("delete")===0){e.preventDefault();var raw=kyoMaskRaw(mts,t.value);raw=raw.slice(0,raw.length-1);kyoSetVal(t,kyoMaskFormat(mts,raw));return;}
           |    if(it==="insertText"||it==="insertReplacementText"||it==="insertFromPaste"||it==="insertFromDrop"){
           |      e.preventDefault();var ins=e.data;if((it==="insertFromPaste"||it==="insertFromDrop")&&e.dataTransfer)ins=e.dataTransfer.getData("text");if(ins==null)ins="";
           |      var raw2=kyoMaskRaw(mts,t.value);
           |      for(var ci=0;ci<ins.length;ci++){var cls=kyoMaskClassAt(mts,raw2.length);if(cls===null)break;var ch=ins.charAt(ci);if(kyoMaskOk(cls,ch))raw2+=ch;}
           |      kyoSetVal(t,kyoMaskFormat(mts,raw2));return;
           |    }
           |  }
           |}
           |// Corrects the whole value once a composition finishes. Mirrors DomBackend's compositionend listener:
           |// the composed text is only known when it ends, so it is filtered or formatted here rather than
           |// per keystroke. Writing back only on a change keeps a conforming composition free of a caret jump.
           |function kyoCompositionEnd(e){
           |  var t=e.target;if(!t||!t.getAttribute)return;
           |  if(t.tagName!=="INPUT"&&t.tagName!=="TEXTAREA")return;
           |  var filt=t.getAttribute("data-kyo-filter");var mask=t.getAttribute("data-kyo-mask");
           |  var v=t.value;var nv;
           |  if(filt)nv=kyoFilterStr(filt,v,"");
           |  else if(mask)nv=kyoMaskNormalize(mask,v);
           |  else return;
           |  if(nv!==v)kyoSetVal(t,nv);
           |}
           |["click","contextmenu","input","change","submit","keydown","keyup","focus","blur","mouseover","mouseout"].forEach(function(t){
           |  document.body.addEventListener(t,handle,true);
           |});
           |document.body.addEventListener("wheel",handle,{capture:true,passive:false});
           |document.body.addEventListener("beforeinput",kyoBeforeInput,true);
           |document.body.addEventListener("compositionend",kyoCompositionEnd,true);
           |// ---- pointer/drag session (setPointerCapture + rAF-coalesced move stream) ----
           |var __ptrActive=false,__ptrEl=null,__ptrPath=null,__ptrRaf=0,__ptrPendingEv=null;
           |function ptrPayload(el,ev){
           |  var r=el.getBoundingClientRect();
           |  var tid=ev.target&&ev.target.id?ev.target.id:null;
           |  var pl={x:ev.clientX-r.left,y:ev.clientY-r.top,rectX:r.left,rectY:r.top,rectW:r.width,rectH:r.height,buttons:ev.buttons};
           |  if(tid)pl.targetId=tid;
           |  return pl;
           |}
           |function ptrDown(e){
           |  var el=fp(e.target);if(!el)return;
           |  if(!he(el,"pointerdown"))return;
           |  try{if(typeof el.setPointerCapture==="function")el.setPointerCapture(e.pointerId);}catch(_e){}
           |  __ptrActive=true;__ptrEl=el;__ptrPath=pa(el);
           |  post({PointerDown:{path:__ptrPath,pointer:ptrPayload(el,e)}});
           |}
           |function ptrMove(e){
           |  // Only stream moves during an active capture/drag session; coalesce to at most one post per animation frame.
           |  if(!__ptrActive||!__ptrEl)return;
           |  __ptrPendingEv=e;
           |  if(__ptrRaf)return;
           |  __ptrRaf=requestAnimationFrame(function(){
           |    __ptrRaf=0;
           |    if(!__ptrActive||!__ptrEl||!__ptrPendingEv)return;
           |    var ev=__ptrPendingEv;__ptrPendingEv=null;
           |    post({PointerMove:{path:__ptrPath,pointer:ptrPayload(__ptrEl,ev)}});
           |  });
           |}
           |function ptrUp(e){
           |  if(!__ptrActive||!__ptrEl)return;
           |  try{if(typeof __ptrEl.releasePointerCapture==="function")__ptrEl.releasePointerCapture(e.pointerId);}catch(_e){}
           |  if(__ptrRaf){cancelAnimationFrame(__ptrRaf);__ptrRaf=0;}
           |  __ptrPendingEv=null;
           |  var el=__ptrEl,path=__ptrPath;
           |  __ptrActive=false;__ptrEl=null;__ptrPath=null;
           |  post({PointerUp:{path:path,pointer:ptrPayload(el,e)}});
           |}
           |document.body.addEventListener("pointerdown",ptrDown,true);
           |document.body.addEventListener("pointermove",ptrMove,true);
           |document.body.addEventListener("pointerup",ptrUp,true);
           |document.body.addEventListener("scroll",handle,{capture:true,passive:true});
           |})();""".stripMargin

    // ---- SVG tag and attribute rendering ----

    /** Exhaustive map from every SvgElement to its HTML/SVG tag string. NO case _ fallback:
      * a missing arm is a compile error (the kyo-ui build escalates the non-exhaustive-match
      * warning to an error for this file; see build.sbt).
      */
    private def svgTagName(e: Svg.SvgElement): String = e match
        case _: Svg.Root           => "svg"
        case _: Svg.G              => "g"
        case _: Svg.Defs           => "defs"
        case _: Svg.Symbol         => "symbol"
        case _: Svg.Switch         => "switch"
        case _: Svg.SvgAnchor      => "a"
        case _: Svg.Use            => "use"
        case _: Svg.Rect           => "rect"
        case _: Svg.Circle         => "circle"
        case _: Svg.Ellipse        => "ellipse"
        case _: Svg.Line           => "line"
        case _: Svg.Polyline       => "polyline"
        case _: Svg.Polygon        => "polygon"
        case _: Svg.Path           => "path"
        case _: Svg.Text           => "text"
        case _: Svg.TSpan          => "tspan"
        case _: Svg.TextPath       => "textPath"
        case _: Svg.LinearGradient => "linearGradient"
        case _: Svg.RadialGradient => "radialGradient"
        case _: Svg.Stop           => "stop"
        case _: Svg.Pattern        => "pattern"
        case _: Svg.ClipPath       => "clipPath"
        case _: Svg.Mask           => "mask"
        case _: Svg.Image          => "image"
        case _: Svg.ForeignObject  => "foreignObject"
        case _: Svg.Marker         => "marker"
        case _: Svg.Title          => "title"
        case _: Svg.Desc           => "desc"
        case _: Svg.Metadata       => "metadata"
        // filter family
        case _: Svg.Filter            => "filter"
        case _: Svg.FeGaussianBlur    => "feGaussianBlur"
        case _: Svg.FeOffset          => "feOffset"
        case _: Svg.FeBlend           => "feBlend"
        case _: Svg.FeColorMatrix     => "feColorMatrix"
        case _: Svg.FeFlood           => "feFlood"
        case _: Svg.FeComposite       => "feComposite"
        case _: Svg.FeMerge           => "feMerge"
        case _: Svg.FeMergeNode       => "feMergeNode"
        case _: Svg.FeImage           => "feImage"
        case _: Svg.FeTile            => "feTile"
        case _: Svg.FeMorphology      => "feMorphology"
        case _: Svg.FeTurbulence      => "feTurbulence"
        case _: Svg.FeDisplacementMap => "feDisplacementMap"
        // SMIL family
        case _: Svg.Animate          => "animate"
        case _: Svg.AnimateTransform => "animateTransform"
        case _: Svg.AnimateMotion    => "animateMotion"
        case _: Svg.SetAnim          => "set"

    private def renderSvgAttrs(sb: StringBuilder, e: Svg.SvgElement): Unit =
        val s = e.svgAttrs
        // emit the "id" attribute for definition elements. A reference-able definition element
        // (gradient/pattern/clipPath/mask/marker/filter) emits its deterministic id even when defId is
        // unset, so a raw element referenced via its *Ref/.paint handle is not a dangling url(#id).
        // Other elements emit only an explicitly-set defId (e.g. symbol via id(v)).
        e match
            case d: Svg.DefinitionElement => svgAttr(sb, "id", d.id)
            case _                        => s.defId.foreach(id => svgAttr(sb, "id", id))
        // shared presentation attributes
        renderSvgPresentation(sb, s)
        // element-specific geometry and reference slots
        e match
            case _: Svg.Root =>
                s.viewBox.foreach(v => svgAttr(sb, "viewBox", viewBox(v)))
                s.preserveAspectRatio.foreach(p => svgAttr(sb, "preserveAspectRatio", par(p)))
                s.width.foreach(c => svgAttr(sb, "width", coord(c)))
                s.height.foreach(c => svgAttr(sb, "height", coord(c)))
            case _: Svg.G    =>
            case _: Svg.Defs =>
            case _: Svg.Symbol =>
                s.viewBox.foreach(v => svgAttr(sb, "viewBox", viewBox(v)))
            case _: Svg.Switch   =>
            case _: Svg.Metadata =>
            case _: Svg.SvgAnchor =>
                s.href.foreach(h => svgAttr(sb, "href", h))
            case _: Svg.Use =>
                s.href.foreach(h => svgAttr(sb, "href", h))
                s.x.foreach(c => svgAttr(sb, "x", coord(c)))
                s.y.foreach(c => svgAttr(sb, "y", coord(c)))
                s.width.foreach(c => svgAttr(sb, "width", coord(c)))
                s.height.foreach(c => svgAttr(sb, "height", coord(c)))
            case _: Svg.Rect =>
                s.x.foreach(c => svgAttr(sb, "x", coord(c)))
                s.y.foreach(c => svgAttr(sb, "y", coord(c)))
                s.width.foreach(c => svgAttr(sb, "width", coord(c)))
                s.height.foreach(c => svgAttr(sb, "height", coord(c)))
                s.rx.foreach(c => svgAttr(sb, "rx", coord(c)))
                s.ry.foreach(c => svgAttr(sb, "ry", coord(c)))
            case _: Svg.Circle =>
                s.cx.foreach(v => svgAttr(sb, "cx", fmtD(v)))
                s.cy.foreach(v => svgAttr(sb, "cy", fmtD(v)))
                s.r.foreach(v => svgAttr(sb, "r", fmtD(v)))
            case _: Svg.Ellipse =>
                s.cx.foreach(v => svgAttr(sb, "cx", fmtD(v)))
                s.cy.foreach(v => svgAttr(sb, "cy", fmtD(v)))
                s.rx.foreach(c => svgAttr(sb, "rx", coord(c)))
                s.ry.foreach(c => svgAttr(sb, "ry", coord(c)))
            case _: Svg.Line =>
                s.x1.foreach(v => svgAttr(sb, "x1", fmtD(v)))
                s.y1.foreach(v => svgAttr(sb, "y1", fmtD(v)))
                s.x2.foreach(v => svgAttr(sb, "x2", fmtD(v)))
                s.y2.foreach(v => svgAttr(sb, "y2", fmtD(v)))
                renderMarkers(sb, s)
            case _: Svg.Polyline =>
                s.points.foreach(p => svgAttr(sb, "points", points(p)))
                renderMarkers(sb, s)
            case _: Svg.Polygon =>
                s.points.foreach(p => svgAttr(sb, "points", points(p)))
                renderMarkers(sb, s)
            case _: Svg.Path =>
                s.d.foreach(d => svgAttr(sb, "d", pathData(d)))
                renderMarkers(sb, s)
            case _: Svg.Text =>
                s.x.foreach(c => svgAttr(sb, "x", coord(c)))
                s.y.foreach(c => svgAttr(sb, "y", coord(c)))
                renderTextAttrs(sb, s)
            case _: Svg.TSpan =>
                s.x.foreach(c => svgAttr(sb, "x", coord(c)))
                s.y.foreach(c => svgAttr(sb, "y", coord(c)))
                renderTextAttrs(sb, s)
            case _: Svg.TextPath =>
                s.href.foreach(h => svgAttr(sb, "href", h))
                renderTextAttrs(sb, s)
            case _: Svg.LinearGradient =>
                s.x1.foreach(v => svgAttr(sb, "x1", fmtD(v)))
                s.y1.foreach(v => svgAttr(sb, "y1", fmtD(v)))
                s.x2.foreach(v => svgAttr(sb, "x2", fmtD(v)))
                s.y2.foreach(v => svgAttr(sb, "y2", fmtD(v)))
                s.gradientUnits.foreach(u => svgAttr(sb, "gradientUnits", units(u)))
                s.spreadMethod.foreach(m => svgAttr(sb, "spreadMethod", spread(m)))
            case _: Svg.RadialGradient =>
                s.cx.foreach(v => svgAttr(sb, "cx", fmtD(v)))
                s.cy.foreach(v => svgAttr(sb, "cy", fmtD(v)))
                s.r.foreach(v => svgAttr(sb, "r", fmtD(v)))
                s.fx.foreach(v => svgAttr(sb, "fx", fmtD(v)))
                s.fy.foreach(v => svgAttr(sb, "fy", fmtD(v)))
                s.gradientUnits.foreach(u => svgAttr(sb, "gradientUnits", units(u)))
                s.spreadMethod.foreach(m => svgAttr(sb, "spreadMethod", spread(m)))
            case _: Svg.Stop =>
                s.offset.foreach(v => svgAttr(sb, "offset", fmtD(v)))
                s.stopColor.foreach(c => svgAttr(sb, "stop-color", CssStyleRenderer.color(c)))
                s.stopOpacity.foreach(v => svgAttr(sb, "stop-opacity", fmtD(v)))
            case _: Svg.Pattern =>
                s.x.foreach(c => svgAttr(sb, "x", coord(c)))
                s.y.foreach(c => svgAttr(sb, "y", coord(c)))
                s.width.foreach(c => svgAttr(sb, "width", coord(c)))
                s.height.foreach(c => svgAttr(sb, "height", coord(c)))
                s.patternUnits.foreach(u => svgAttr(sb, "patternUnits", units(u)))
                s.viewBox.foreach(v => svgAttr(sb, "viewBox", viewBox(v)))
            case _: Svg.ClipPath =>
                s.clipPathUnits.foreach(u => svgAttr(sb, "clipPathUnits", units(u)))
            case _: Svg.Mask =>
                s.maskUnits.foreach(u => svgAttr(sb, "maskUnits", units(u)))
                s.width.foreach(c => svgAttr(sb, "width", coord(c)))
                s.height.foreach(c => svgAttr(sb, "height", coord(c)))
            case _: Svg.Image =>
                s.href.foreach(h => svgAttr(sb, "href", h))
                s.x.foreach(c => svgAttr(sb, "x", coord(c)))
                s.y.foreach(c => svgAttr(sb, "y", coord(c)))
                s.width.foreach(c => svgAttr(sb, "width", coord(c)))
                s.height.foreach(c => svgAttr(sb, "height", coord(c)))
                s.preserveAspectRatio.foreach(p => svgAttr(sb, "preserveAspectRatio", par(p)))
            case _: Svg.ForeignObject =>
                s.x.foreach(c => svgAttr(sb, "x", coord(c)))
                s.y.foreach(c => svgAttr(sb, "y", coord(c)))
                s.width.foreach(c => svgAttr(sb, "width", coord(c)))
                s.height.foreach(c => svgAttr(sb, "height", coord(c)))
            case _: Svg.Marker =>
                s.markerWidth.foreach(v => svgAttr(sb, "markerWidth", fmtD(v)))
                s.markerHeight.foreach(v => svgAttr(sb, "markerHeight", fmtD(v)))
                s.refX.foreach(v => svgAttr(sb, "refX", fmtD(v)))
                s.refY.foreach(v => svgAttr(sb, "refY", fmtD(v)))
                s.markerUnits.foreach(u => svgAttr(sb, "markerUnits", markerUnits(u)))
                s.orient.foreach(o => svgAttr(sb, "orient", o))
                s.viewBox.foreach(v => svgAttr(sb, "viewBox", viewBox(v)))
            case _: Svg.Title  =>
            case _: Svg.Desc   =>
            case _: Svg.Filter =>
                // The filter `id` is emitted by the shared DefinitionElement path above.
                s.filterX.foreach(c => svgAttr(sb, "x", coord(c)))
                s.filterY.foreach(c => svgAttr(sb, "y", coord(c)))
                s.filterWidth.foreach(c => svgAttr(sb, "width", coord(c)))
                s.filterHeight.foreach(c => svgAttr(sb, "height", coord(c)))
                s.filterUnits.foreach(u => svgAttr(sb, "filterUnits", units(u)))
            case _: Svg.FeGaussianBlur =>
                s.feIn.foreach(v => svgAttr(sb, "in", v))
                s.stdDeviation.foreach(v => svgAttr(sb, "stdDeviation", fmtD(v)))
                s.feResult.foreach(v => svgAttr(sb, "result", v))
            case _: Svg.FeOffset =>
                s.feIn.foreach(v => svgAttr(sb, "in", v))
                s.feDx.foreach(v => svgAttr(sb, "dx", fmtD(v)))
                s.feDy.foreach(v => svgAttr(sb, "dy", fmtD(v)))
                s.feResult.foreach(v => svgAttr(sb, "result", v))
            case _: Svg.FeBlend =>
                s.feIn.foreach(v => svgAttr(sb, "in", v))
                s.feIn2.foreach(v => svgAttr(sb, "in2", v))
                s.feMode.foreach(v => svgAttr(sb, "mode", blendMode(v)))
                s.feResult.foreach(v => svgAttr(sb, "result", v))
            case _: Svg.FeColorMatrix =>
                s.feIn.foreach(v => svgAttr(sb, "in", v))
                s.feColorMatrixType.foreach(v => svgAttr(sb, "type", colorMatrixType(v)))
                s.feValues.foreach(v => svgAttr(sb, "values", v))
                s.feResult.foreach(v => svgAttr(sb, "result", v))
            case _: Svg.FeFlood =>
                s.feFloodColor.foreach(c => svgAttr(sb, "flood-color", CssStyleRenderer.color(c)))
                s.feFloodOpacity.foreach(v => svgAttr(sb, "flood-opacity", fmtD(v)))
                s.feResult.foreach(v => svgAttr(sb, "result", v))
            case _: Svg.FeComposite =>
                s.feIn.foreach(v => svgAttr(sb, "in", v))
                s.feIn2.foreach(v => svgAttr(sb, "in2", v))
                s.feCompositeOperator.foreach(v => svgAttr(sb, "operator", compositeOperator(v)))
                s.feResult.foreach(v => svgAttr(sb, "result", v))
            case _: Svg.FeMerge =>
                s.feResult.foreach(v => svgAttr(sb, "result", v))
            case _: Svg.FeMergeNode =>
                s.feIn.foreach(v => svgAttr(sb, "in", v))
            case _: Svg.FeImage =>
                s.href.foreach(h => svgAttr(sb, "href", h))
                s.feResult.foreach(v => svgAttr(sb, "result", v))
            case _: Svg.FeTile =>
                s.feIn.foreach(v => svgAttr(sb, "in", v))
                s.feResult.foreach(v => svgAttr(sb, "result", v))
            case _: Svg.FeMorphology =>
                s.feIn.foreach(v => svgAttr(sb, "in", v))
                s.feMorphologyOperator.foreach(v => svgAttr(sb, "operator", morphologyOperator(v)))
                s.feResult.foreach(v => svgAttr(sb, "result", v))
            case _: Svg.FeTurbulence =>
                s.feBaseFrequency.foreach(v => svgAttr(sb, "baseFrequency", v))
                s.feTurbulenceType.foreach(v => svgAttr(sb, "type", turbulenceType(v)))
                s.feResult.foreach(v => svgAttr(sb, "result", v))
            case _: Svg.FeDisplacementMap =>
                s.feIn.foreach(v => svgAttr(sb, "in", v))
                s.feIn2.foreach(v => svgAttr(sb, "in2", v))
                s.feScale.foreach(v => svgAttr(sb, "scale", fmtD(v)))
                s.feResult.foreach(v => svgAttr(sb, "result", v))
            case _: Svg.Animate =>
                s.animAttributeName.foreach(v => svgAttr(sb, "attributeName", v))
                s.animFrom.foreach(v => svgAttr(sb, "from", v))
                s.animTo.foreach(v => svgAttr(sb, "to", v))
                s.animValues.foreach(v => svgAttr(sb, "values", v))
                s.animDur.foreach(v => svgAttr(sb, "dur", v))
                s.animCalcMode.foreach(v => svgAttr(sb, "calcMode", v))
                s.animKeyTimes.foreach(v => svgAttr(sb, "keyTimes", v))
                s.animKeySplines.foreach(v => svgAttr(sb, "keySplines", v))
                s.animRepeatCount.foreach(v => svgAttr(sb, "repeatCount", v))
                s.animBegin.foreach(v => svgAttr(sb, "begin", v))
                s.animFill.foreach(v => svgAttr(sb, "fill", animFill(v)))
            case _: Svg.AnimateTransform =>
                s.animAttributeName.foreach(v => svgAttr(sb, "attributeName", v))
                s.animType.foreach(v => svgAttr(sb, "type", transformType(v)))
                s.animFrom.foreach(v => svgAttr(sb, "from", v))
                s.animTo.foreach(v => svgAttr(sb, "to", v))
                s.animDur.foreach(v => svgAttr(sb, "dur", v))
                s.animRepeatCount.foreach(v => svgAttr(sb, "repeatCount", v))
                s.animBegin.foreach(v => svgAttr(sb, "begin", v))
                s.animFill.foreach(v => svgAttr(sb, "fill", animFill(v)))
            case _: Svg.AnimateMotion =>
                s.d.foreach(d => svgAttr(sb, "path", pathData(d)))
                s.animDur.foreach(v => svgAttr(sb, "dur", v))
                s.animRepeatCount.foreach(v => svgAttr(sb, "repeatCount", v))
                s.animFill.foreach(v => svgAttr(sb, "fill", animFill(v)))
            case _: Svg.SetAnim =>
                s.animAttributeName.foreach(v => svgAttr(sb, "attributeName", v))
                s.animTo.foreach(v => svgAttr(sb, "to", v))
                s.animBegin.foreach(v => svgAttr(sb, "begin", v))
                s.animFill.foreach(v => svgAttr(sb, "fill", animFill(v)))
        end match
    end renderSvgAttrs

    private def renderSvgPresentation(sb: StringBuilder, s: Svg.SvgAttrs): Unit =
        s.fill.foreach(p => svgAttr(sb, "fill", paint(p)))
        s.fillOpacity.foreach(v => svgAttr(sb, "fill-opacity", fmtD(v)))
        s.fillRule.foreach(r => svgAttr(sb, "fill-rule", fillRule(r)))
        s.stroke.foreach(p => svgAttr(sb, "stroke", paint(p)))
        s.strokeWidth.foreach(l => svgAttr(sb, "stroke-width", svgLength(l)))
        s.strokeOpacity.foreach(v => svgAttr(sb, "stroke-opacity", fmtD(v)))
        s.strokeLinecap.foreach(c => svgAttr(sb, "stroke-linecap", linecap(c)))
        s.strokeLinejoin.foreach(j => svgAttr(sb, "stroke-linejoin", linejoin(j)))
        s.strokeDasharray.foreach { ds =>
            svgAttr(sb, "stroke-dasharray", ds.map(fmtD).mkString(" "))
        }
        s.strokeDashoffset.foreach(l => svgAttr(sb, "stroke-dashoffset", svgLength(l)))
        s.strokeMiterlimit.foreach(v => svgAttr(sb, "stroke-miterlimit", fmtD(v)))
        s.pathLength.foreach(v => svgAttr(sb, "pathLength", fmtD(v)))
        s.opacity.foreach(v => svgAttr(sb, "opacity", fmtD(v)))
        if s.transform.nonEmpty then
            svgAttr(sb, "transform", s.transform.map(transform).mkString(" "))
        s.clipPathRef.foreach(id => svgAttr(sb, "clip-path", s"url(#$id)"))
        s.maskRef.foreach(id => svgAttr(sb, "mask", s"url(#$id)"))
        s.filterRef.foreach(id => svgAttr(sb, "filter", s"url(#$id)"))
    end renderSvgPresentation

    private def renderMarkers(sb: StringBuilder, s: Svg.SvgAttrs): Unit =
        s.markerStart.foreach(id => svgAttr(sb, "marker-start", s"url(#$id)"))
        s.markerMid.foreach(id => svgAttr(sb, "marker-mid", s"url(#$id)"))
        s.markerEnd.foreach(id => svgAttr(sb, "marker-end", s"url(#$id)"))
    end renderMarkers

    private def renderTextAttrs(sb: StringBuilder, s: Svg.SvgAttrs): Unit =
        s.textAnchor.foreach(a => svgAttr(sb, "text-anchor", textAnchor(a)))
        s.dominantBaseline.foreach(b => svgAttr(sb, "dominant-baseline", dominantBaseline(b)))
        s.fontSize.foreach(l => svgAttr(sb, "font-size", svgLength(l)))
        s.fontFamily.foreach(f => svgAttr(sb, "font-family", f))
    end renderTextAttrs

    // ---- SVG value encoders ----

    private def svgAttr(sb: StringBuilder, name: String, value: String): Unit =
        w(sb, s""" $name="${esc(value)}"""")

    private def coord(c: Svg.Coord): String = c match
        case Svg.Coord.Num(v) => fmtD(v)
        case Svg.Coord.Len(l) => svgLength(l)

    private def svgLength(l: Svg.SvgLength): String = l match
        case Svg.SvgLength.User(v) => fmtD(v)
        case Svg.SvgLength.Px(v)   => s"${fmtD(v)}px"
        case Svg.SvgLength.Pct(v)  => s"${fmtD(v)}%"
        case Svg.SvgLength.Em(v)   => s"${fmtD(v)}em"

    private def paint(p: Svg.Paint): String = p match
        case Svg.Paint.None         => "none"
        case Svg.Paint.CurrentColor => "currentColor"
        case Svg.Paint.Color(c)     => CssStyleRenderer.color(c)
        case Svg.Paint.Ref(server)  => s"url(#${server.id})"

    private def transform(t: Svg.Transform): String = t match
        case Svg.Transform.Translate(x, y) => s"translate(${fmtD(x)} ${fmtD(y)})"
        case Svg.Transform.Rotate(deg, cx, cy) =>
            cx match
                case Present(cx0) =>
                    cy match
                        case Present(cy0) => s"rotate(${fmtD(deg)} ${fmtD(cx0)} ${fmtD(cy0)})"
                        case Absent       => s"rotate(${fmtD(deg)} ${fmtD(cx0)})"
                case Absent => s"rotate(${fmtD(deg)})"
        case Svg.Transform.Scale(sx, sy) =>
            sy match
                case Present(sy0) => s"scale(${fmtD(sx)} ${fmtD(sy0)})"
                case Absent       => s"scale(${fmtD(sx)})"
        case Svg.Transform.SkewX(deg)               => s"skewX(${fmtD(deg)})"
        case Svg.Transform.SkewY(deg)               => s"skewY(${fmtD(deg)})"
        case Svg.Transform.Matrix(a, b, c, d, e, f) => s"matrix(${fmtD(a)} ${fmtD(b)} ${fmtD(c)} ${fmtD(d)} ${fmtD(e)} ${fmtD(f)})"

    private def points(p: Svg.Points): String =
        Svg.Points.pairs(p).map { case (x, y) => s"${fmtD(x)},${fmtD(y)}" }.mkString(" ")

    private def viewBox(v: Svg.ViewBox): String =
        s"${fmtD(v.minX)} ${fmtD(v.minY)} ${fmtD(v.width)} ${fmtD(v.height)}"

    private def par(p: Svg.PreserveAspectRatio): String =
        s"${align(p.align)} ${meetOrSlice(p.meetOrSlice)}"

    private def align(a: Svg.Align): String = a match
        case Svg.Align.None     => "none"
        case Svg.Align.XMinYMin => "xMinYMin"
        case Svg.Align.XMidYMin => "xMidYMin"
        case Svg.Align.XMaxYMin => "xMaxYMin"
        case Svg.Align.XMinYMid => "xMinYMid"
        case Svg.Align.XMidYMid => "xMidYMid"
        case Svg.Align.XMaxYMid => "xMaxYMid"
        case Svg.Align.XMinYMax => "xMinYMax"
        case Svg.Align.XMidYMax => "xMidYMax"
        case Svg.Align.XMaxYMax => "xMaxYMax"

    private def meetOrSlice(m: Svg.MeetOrSlice): String = m match
        case Svg.MeetOrSlice.Meet  => "meet"
        case Svg.MeetOrSlice.Slice => "slice"

    private def pathData(d: Svg.PathData): String =
        Svg.PathData.commands(d).map(pathCmd).mkString(" ")

    private def pathCmd(c: Svg.PathCommand): String = c match
        case Svg.PathCommand.MoveTo(x, y)   => s"M${fmtD(x)} ${fmtD(y)}"
        case Svg.PathCommand.MoveBy(dx, dy) => s"m${fmtD(dx)} ${fmtD(dy)}"
        case Svg.PathCommand.LineTo(x, y)   => s"L${fmtD(x)} ${fmtD(y)}"
        case Svg.PathCommand.LineBy(dx, dy) => s"l${fmtD(dx)} ${fmtD(dy)}"
        case Svg.PathCommand.HLineTo(x)     => s"H${fmtD(x)}"
        case Svg.PathCommand.HLineBy(dx)    => s"h${fmtD(dx)}"
        case Svg.PathCommand.VLineTo(y)     => s"V${fmtD(y)}"
        case Svg.PathCommand.VLineBy(dy)    => s"v${fmtD(dy)}"
        case Svg.PathCommand.CubicTo(c1x, c1y, c2x, c2y, x, y) =>
            s"C${fmtD(c1x)} ${fmtD(c1y)} ${fmtD(c2x)} ${fmtD(c2y)} ${fmtD(x)} ${fmtD(y)}"
        case Svg.PathCommand.CubicBy(c1x, c1y, c2x, c2y, dx, dy) =>
            s"c${fmtD(c1x)} ${fmtD(c1y)} ${fmtD(c2x)} ${fmtD(c2y)} ${fmtD(dx)} ${fmtD(dy)}"
        case Svg.PathCommand.SmoothCubicTo(c2x, c2y, x, y) =>
            s"S${fmtD(c2x)} ${fmtD(c2y)} ${fmtD(x)} ${fmtD(y)}"
        case Svg.PathCommand.SmoothCubicBy(c2x, c2y, dx, dy) =>
            s"s${fmtD(c2x)} ${fmtD(c2y)} ${fmtD(dx)} ${fmtD(dy)}"
        case Svg.PathCommand.QuadTo(cx, cy, x, y)   => s"Q${fmtD(cx)} ${fmtD(cy)} ${fmtD(x)} ${fmtD(y)}"
        case Svg.PathCommand.QuadBy(cx, cy, dx, dy) => s"q${fmtD(cx)} ${fmtD(cy)} ${fmtD(dx)} ${fmtD(dy)}"
        case Svg.PathCommand.SmoothQuadTo(x, y)     => s"T${fmtD(x)} ${fmtD(y)}"
        case Svg.PathCommand.SmoothQuadBy(dx, dy)   => s"t${fmtD(dx)} ${fmtD(dy)}"
        case Svg.PathCommand.ArcTo(rx, ry, xRot, largeArc, sweep, x, y) =>
            val la = if largeArc then 1 else 0
            val sw = if sweep then 1 else 0
            s"A${fmtD(rx)} ${fmtD(ry)} ${fmtD(xRot)} $la $sw ${fmtD(x)} ${fmtD(y)}"
        case Svg.PathCommand.ArcBy(rx, ry, xRot, largeArc, sweep, dx, dy) =>
            val la = if largeArc then 1 else 0
            val sw = if sweep then 1 else 0
            s"a${fmtD(rx)} ${fmtD(ry)} ${fmtD(xRot)} $la $sw ${fmtD(dx)} ${fmtD(dy)}"
        case Svg.PathCommand.Close  => "Z"
        case Svg.PathCommand.Raw(d) => d

    private def fillRule(r: Svg.FillRule): String = r match
        case Svg.FillRule.NonZero => "nonzero"
        case Svg.FillRule.EvenOdd => "evenodd"

    private def linecap(c: Svg.StrokeLinecap): String = c match
        case Svg.StrokeLinecap.Butt   => "butt"
        case Svg.StrokeLinecap.Round  => "round"
        case Svg.StrokeLinecap.Square => "square"

    private def linejoin(j: Svg.StrokeLinejoin): String = j match
        case Svg.StrokeLinejoin.Miter     => "miter"
        case Svg.StrokeLinejoin.Round     => "round"
        case Svg.StrokeLinejoin.Bevel     => "bevel"
        case Svg.StrokeLinejoin.Arcs      => "arcs"
        case Svg.StrokeLinejoin.MiterClip => "miter-clip"

    private def textAnchor(a: Svg.TextAnchor): String = a match
        case Svg.TextAnchor.Start  => "start"
        case Svg.TextAnchor.Middle => "middle"
        case Svg.TextAnchor.End    => "end"

    private def dominantBaseline(b: Svg.DominantBaseline): String = b match
        case Svg.DominantBaseline.Auto           => "auto"
        case Svg.DominantBaseline.Middle         => "middle"
        case Svg.DominantBaseline.Central        => "central"
        case Svg.DominantBaseline.Hanging        => "hanging"
        case Svg.DominantBaseline.TextBeforeEdge => "text-before-edge"
        case Svg.DominantBaseline.TextAfterEdge  => "text-after-edge"
        case Svg.DominantBaseline.Alphabetic     => "alphabetic"
        case Svg.DominantBaseline.Ideographic    => "ideographic"
        case Svg.DominantBaseline.Mathematical   => "mathematical"

    private def units(u: Svg.Units): String = u match
        case Svg.Units.UserSpaceOnUse    => "userSpaceOnUse"
        case Svg.Units.ObjectBoundingBox => "objectBoundingBox"

    private def spread(m: Svg.SpreadMethod): String = m match
        case Svg.SpreadMethod.Pad     => "pad"
        case Svg.SpreadMethod.Reflect => "reflect"
        case Svg.SpreadMethod.Repeat  => "repeat"

    private def markerUnits(u: Svg.MarkerUnits): String = u match
        case Svg.MarkerUnits.StrokeWidth    => "strokeWidth"
        case Svg.MarkerUnits.UserSpaceOnUse => "userSpaceOnUse"

    private def blendMode(m: Svg.BlendMode): String = m match
        case Svg.BlendMode.Normal     => "normal"
        case Svg.BlendMode.Multiply   => "multiply"
        case Svg.BlendMode.Screen     => "screen"
        case Svg.BlendMode.Overlay    => "overlay"
        case Svg.BlendMode.Darken     => "darken"
        case Svg.BlendMode.Lighten    => "lighten"
        case Svg.BlendMode.ColorDodge => "color-dodge"
        case Svg.BlendMode.ColorBurn  => "color-burn"
        case Svg.BlendMode.HardLight  => "hard-light"
        case Svg.BlendMode.SoftLight  => "soft-light"
        case Svg.BlendMode.Difference => "difference"
        case Svg.BlendMode.Exclusion  => "exclusion"
        case Svg.BlendMode.Hue        => "hue"
        case Svg.BlendMode.Saturation => "saturation"
        case Svg.BlendMode.Color      => "color"
        case Svg.BlendMode.Luminosity => "luminosity"

    private def colorMatrixType(t: Svg.ColorMatrixType): String = t match
        case Svg.ColorMatrixType.Matrix           => "matrix"
        case Svg.ColorMatrixType.Saturate         => "saturate"
        case Svg.ColorMatrixType.HueRotate        => "hueRotate"
        case Svg.ColorMatrixType.LuminanceToAlpha => "luminanceToAlpha"

    private def compositeOperator(o: Svg.CompositeOperator): String = o match
        case Svg.CompositeOperator.Over       => "over"
        case Svg.CompositeOperator.In         => "in"
        case Svg.CompositeOperator.Out        => "out"
        case Svg.CompositeOperator.Atop       => "atop"
        case Svg.CompositeOperator.Xor        => "xor"
        case Svg.CompositeOperator.Arithmetic => "arithmetic"

    private def morphologyOperator(o: Svg.MorphologyOperator): String = o match
        case Svg.MorphologyOperator.Erode  => "erode"
        case Svg.MorphologyOperator.Dilate => "dilate"

    private def turbulenceType(t: Svg.TurbulenceType): String = t match
        case Svg.TurbulenceType.FractalNoise => "fractalNoise"
        case Svg.TurbulenceType.Turbulence   => "turbulence"

    private def transformType(t: Svg.TransformType): String = t match
        case Svg.TransformType.Translate => "translate"
        case Svg.TransformType.Scale     => "scale"
        case Svg.TransformType.Rotate    => "rotate"
        case Svg.TransformType.SkewX     => "skewX"
        case Svg.TransformType.SkewY     => "skewY"

    private def animFill(f: Svg.AnimFill): String = f match
        case Svg.AnimFill.Freeze => "freeze"
        case Svg.AnimFill.Remove => "remove"

end HtmlRenderer
