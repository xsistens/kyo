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

    /** Render a UI tree to HTML; the elements a client addresses carry `data-kyo-path` (see [[carriesPath]]). */
    def render(ui: UI, path: Seq[String])(using Frame): String < Sync =
        // Every descendant extends the path with `:+`, and `Seq.empty` is a List, whose `:+` copies the
        // whole prefix per node. Normalizing once at the entry makes those appends Vector appends; paths
        // are only ever built, compared and joined, and Seq equality holds across both.
        val root = path.toVector
        // The page is a range of its own, as the session's root is, so the calls of one keyed site count the same in both.
        pass(ui)(DrawContext.inRange {
            val sb = new StringBuilder
            renderTo(
                sb,
                ui,
                root,
                ReactiveRegion.RegionIdentity.root(root),
                ReactiveRegion.Namespace.Html,
                parentContext = ReactiveRegion.ParentContext.Other,
                boundaryMode = ReactiveRegion.BoundaryMode.Emit
            )
            sb.toString
        })
    end render

    /** One render pass as a `Sync` effect.
      *
      * The render is a synchronous recursion whose only effects are signal reads, so it runs as one effect
      * that reads through [[SignalNow]] rather than suspending at every bound node, which would cost the pass
      * a continuation for each frame between the read and the row loop.
      *
      * A tree that binds no signal renders where the effect is built: an attribute the renderer rejects (an
      * oversized drag config, say) then fails at the call rather than at the run, which is what a caller of
      * `render` expects. Reading such a tree is pure, so the `AllowUnsafe` handed to the pass is never spent on
      * an effect, and it needs no locals to read with. A tree that binds a signal renders when the effect runs,
      * reading with the locals of the fiber that runs it.
      */
    private inline def pass[A](ui: UI)(inline body: (SignalNow.Locals, AllowUnsafe) ?=> A)(using Frame): A < Sync =
        if bindsSignal(ui) then SignalNow.Locals.take(Sync.Unsafe.defer(body(using MountDispatch.drawing)))
        else body(using SignalNow.Locals.none, AllowUnsafe.embrace.danger)

    /** The rows a list patch changed, rendered into one string under one effect. */
    private[kyo] def renderRows(
        rows: Seq[ListRow],
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity,
        host: ReactiveRegion.RenderHost
    )(using Frame): String < Sync =
        SignalNow.Locals.take(Sync.Unsafe.defer {
            given SignalNow.Locals = MountDispatch.drawing
            val sb                 = new StringBuilder
            rows.foreach { row =>
                if row.changed then renderRowInto(sb, row.ui, path :+ row.key, context.child(row.key), host, Absent)
            }
            sb.toString
        })

    /** Whether rendering `ui` reads a signal anywhere: the reactive nodes, an element with a reactive
      * attribute, bool-attribute or class channel, and a control bound to a ref. Mirrors every read
      * [[renderTo]] performs, and a mount's placeholder is read like any other tree. A drawn mount's draw is an effect like such a read.
      */
    private def bindsSignal(ui: UI): Boolean =
        ui match
            case _: Reactive[?] | _: Foreach[?, ?] | _: Drawn => true
            case el: Element                                  =>
                el.attrs.hasChannels
                || ReactiveUI.collectSignalRef(el).nonEmpty
                || Channels.follows(el)
                || (el match
                    case ta: Textarea => hasSignalRefValue(ta.value)
                    case dd: Dropdown => hasSignalRefValue(dd.value)
                    case s: Select    => hasSignalRefValue(s.value)
                    case _            => false)
                || el.children.exists(bindsSignal)
            case Fragment(children)   => children.exists(bindsSignal)
            case KeyedChild(_, child) => bindsSignal(child)
            case m: Mounted           => m.placeholderUI.exists(bindsSignal)
            // A waiting boundary reads its content's signals to learn whether it holds a mount that loads.
            case _: Bounded           => true
            case _: Text | _: RawHtml => false

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
            case _: Drawn             => false
            case _: Bounded           => false
    end paintsAsKeyedRoot

    /** Render ONE keyed row of a list region, exactly as the region's own render would have placed it, plus
      * the pseudo-state rules it introduced.
      *
      * A list patch renders the rows that changed and nothing else, so it needs the row alone rather than the
      * region around it: same path (`path :+ key`), same identity (`context.child(key)`), same namespace and
      * parent context the enclosing host imposes, and no boundary markers of the region's own. What comes back is
      * therefore byte-identical to the slice the whole-list render would have produced for that row, which is
      * what lets the reconciliation match it against the live row by key.
      */
    private[kyo] def renderRowWithCss(
        ui: UI,
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity,
        host: ReactiveRegion.RenderHost
    )(using Frame): (String, Chunk[(String, String)]) < Sync =
        pass(ui) {
            val sb  = new StringBuilder
            val css = new CssCollector
            renderRowInto(sb, ui, path, context, host, Present(css))
            (sb.toString, Chunk.from(css))
        }
    end renderRowWithCss

    private def renderRowInto(
        sb: StringBuilder,
        ui: UI,
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity,
        host: ReactiveRegion.RenderHost,
        cssRules: Maybe[CssCollector]
    )(using AllowUnsafe, Frame, SignalNow.Locals): Unit =
        val (namespace, parentContext) = host match
            case _: ReactiveRegion.RenderHost.HtmlTableBody =>
                (ReactiveRegion.Namespace.Html, ReactiveRegion.ParentContext.Other)
            case other => (ReactiveRegion.namespace(other), ReactiveRegion.contentParent(other))
        // A row is a range of its own, as the session walks a rebuilt row; its key tells its calls apart from the other rows' anyway.
        DrawContext.inRegion(renderTo(
            sb,
            ui,
            path.toVector,
            context,
            namespace,
            cssRules,
            parentContext,
            ReactiveRegion.BoundaryMode.Emit,
            keyedRoot = true
        ))
    end renderRowInto

    private[kyo] def renderRegion(
        ui: UI,
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity,
        region: ReactiveRegion,
        parentContext: ReactiveRegion.ParentContext,
        boundaryMode: ReactiveRegion.BoundaryMode
    )(using Frame): String < Sync =
        val host = ReactiveRegion.renderHost(region, parentContext, ReactiveRegion.tableContent(ui))
        pass(ui)(DrawContext.inRange {
            val sb = new StringBuilder
            renderHostedContent(sb, ui, path, context, host, boundaryMode)
            sb.toString
        })
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
        val root = path.toVector
        pass(ui)(DrawContext.inRange {
            val sb  = new StringBuilder
            val css = new CssCollector
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
            (sb.toString, Chunk.from(css))
        })
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
        val host = ReactiveRegion.renderHost(region, parentContext, ReactiveRegion.tableContent(ui))
        // What one paint shows is one range: a region's value or an instance's content.
        pass(ui)(DrawContext.inRange {
            val sb  = new StringBuilder
            val css = new CssCollector
            renderHostedContent(sb, ui, path, context, host, boundaryMode, Present(css))
            (sb.toString, Chunk.from(css))
        })
    end renderRegionWithCss

    private def renderHostedContent(
        sb: StringBuilder,
        ui: UI,
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity,
        host: ReactiveRegion.RenderHost,
        boundaryMode: ReactiveRegion.BoundaryMode,
        cssRules: Maybe[CssCollector] = Absent
    )(using AllowUnsafe, Frame, SignalNow.Locals): Unit =
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
                )
                w(sb, "</tbody>")
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
            case ReactiveRegion.HtmlRange(id)    => s"<!--kyo-rs:$id-->$innerHtml<!--kyo-re:$id-->"
            case ReactiveRegion.SvgElement(path) =>
                s"""<g data-kyo-path="${pathAttr(path)}" data-kyo-reactive>$innerHtml</g>"""

    private def openSvgRegion(path: Seq[String]): String =
        s"""<g data-kyo-path="${pathAttr(path)}" data-kyo-reactive>"""

    /** Wrap body HTML in a full page with inline JS client.
      *
      * The devtools overlay rides along in a second `<script>` when one is registered, after the client: the
      * client is what receives the stats frames and hands them over, so the overlay has to be defined by the
      * time the socket opens. Nothing is emitted when no overlay is registered, which is every page in a build
      * that does not depend on kyo-ui-devtools.
      */
    def renderPage(title: String, body: String, css: String, basePath: String, devtoolsScript: Maybe[String] = Absent): String =
        val devtools = devtoolsScript match
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
        document(head, "", body, "")

    /** A server-push page: the document [[page]] builds for `head`, with the page's pseudo-state rules after `head.css` and the
      * WebSocket client for `basePath` at the end of the body.
      */
    private[kyo] def serverPage(head: UI.PageHead, body: String, css: String, basePath: String): String =
        document(head, css, body, s"<script>${clientJs(jsStr(basePath))}</script>")

    private def document(head: UI.PageHead, css: String, body: String, client: String): String =
        val metaTags = head.meta.map((n, c) => s"""<meta name="${esc(n)}" content="${esc(c)}">""").mkString
        val linkTags = head.links.map((r, h) => s"""<link rel="${esc(r)}" href="${esc(h)}">""").mkString
        val script   = head.moduleScript match
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
           |<style>$baseCss${head.css}$css</style>
           |$ldBlock</head>
           |<body>$body$islands$client</body>
           |$script
           |</html>""".stripMargin
    end document

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
        boundaryMode: ReactiveRegion.BoundaryMode,
        keyedRoot: Boolean = false
    )(using AllowUnsafe, Frame, SignalNow.Locals): Unit =
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
                    def openTag(extraClasses: Seq[String], channelStyle: Seq[(String, String)]): Unit =
                        discard(sb.append('<').append(tag))
                        // The event attribute first: whether one was written is half of the path decision, and
                        // it is known only by walking the handlers, which this does once.
                        val events = renderEventAttr(sb, elem)
                        if events || carriesPath(elem, namespace, keyedRoot) then
                            discard(sb.append(" data-kyo-path=\""))
                            appendPath(sb, path)
                            discard(sb.append('"'))
                        end if
                        renderCommonAttrs(
                            sb,
                            if extraClasses.isEmpty then elem.attrs
                            else elem.attrs.copy(cssClasses = elem.attrs.cssClasses ++ extraClasses),
                            elem.isInstanceOf[Svg.SvgElement],
                            cssRules,
                            channelStyle
                        )
                        // Which element value this is: the browser mount starts the onAttached hook again for a rebuilt
                        // element, and leaves the content of one shown again unchanged, which the hook may own, alone.
                        if elem.attrs.onAttached.nonEmpty then
                            discard(sb.append(" data-kyo-attach=\"")
                                .append(Integer.toHexString(java.lang.System.identityHashCode(elem))).append('"'))
                    end openTag
                    openTag(reactiveTrueClasses(elem.attrs) ++ filledNow(elem), reactiveStyleNow(elem.attrs))
                    renderElementAttrs(sb, elem)
                    renderReactiveAttrs(sb, elem.attrs)
                    renderReactiveBoolAttrs(sb, elem.attrs)
                    if void then
                        w(sb, " />")
                        elem match
                            case ta: Textarea =>
                                sb.delete(sb.length - 3, sb.length)
                                // The HTML parser drops one line break right after `<textarea>`. Writing one
                                // ourselves gives it that one, so a value that starts with a line break keeps it.
                                w(sb, ">\n")
                                renderTextareaValue(sb, ta)
                                w(sb, "</textarea>")
                            case _: Iframe =>
                                // iframe is not a void element: it needs an explicit closing tag.
                                sb.delete(sb.length - 3, sb.length)
                                w(sb, "></iframe>")
                            case _ => ()
                        end match
                    else
                        w(sb, ">")
                        // ForeignObject bridges back to HTML, so reset svg context to false. It must be
                        // matched before SvgElement (ForeignObject IS an SvgElement).
                        val childNamespace = elem match
                            case _: Svg.ForeignObject => ReactiveRegion.Namespace.Html
                            case _: Svg.SvgElement    => ReactiveRegion.Namespace.Svg
                            case _                    => namespace
                        val childParentContext = elem match
                            case _: Table => ReactiveRegion.ParentContext.HtmlTable
                            case _        => ReactiveRegion.ParentContext.Other
                        elem match
                            case t: Svg.Title => w(sb, esc(t.text))
                            case d: Svg.Desc  => w(sb, esc(d.text))
                            case _            => ()
                        end match
                        var i = 0
                        elem.children.foreach { child =>
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
                            i += 1
                        }
                        discard(sb.append("</").append(tag).append('>'))
                    end if
                }

            case UI.Ast.RawHtml(value) =>
                w(sb, value)

            case UI.Ast.Text(value) =>
                escInto(sb, value)

            case Fragment(children) =>
                // Use key for KeyedChild, index for everything else. This matches the path scheme
                // walkStatic uses, so server-side event routing aligns with rendered data-kyo-path.
                var i = 0
                children.foreach { child =>
                    val childPath = child match
                        case kc: KeyedChild[?] => path :+ kc.key
                        case _                 => path :+ i.toString
                    val childContext = child match
                        case kc: KeyedChild[?] => context.child(kc.key)
                        case _                 => context.child(i.toString)
                    child match
                        case kc: KeyedChild[?] =>
                            DrawContext.inRow(kc.key)(
                                renderTo(
                                    sb,
                                    child,
                                    childPath,
                                    childContext,
                                    namespace,
                                    cssRules,
                                    parentContext,
                                    ReactiveRegion.BoundaryMode.Emit
                                )
                            )
                        case _ =>
                            renderTo(
                                sb,
                                child,
                                childPath,
                                childContext,
                                namespace,
                                cssRules,
                                parentContext,
                                ReactiveRegion.BoundaryMode.Emit
                            )
                    end match
                    i += 1
                }

            case KeyedChild(_, child) =>
                renderTo(sb, child, path, context, namespace, cssRules, parentContext, boundaryMode, keyedRoot = true)

            case r: Reactive[?] =>
                val region  = ReactiveRegion.from(context, namespace)
                val current = SignalNow(r.signal)(using r.frame, summon[SignalNow.Locals], summon[AllowUnsafe])
                val host    = ReactiveRegion.renderHost(region, parentContext, ReactiveRegion.tableContent(current))
                openInitialHost(sb, host)
                DrawContext.inRegion(renderTo(
                    sb,
                    current,
                    path,
                    context.transparent,
                    ReactiveRegion.namespace(host),
                    cssRules,
                    ReactiveRegion.contentParent(host),
                    ReactiveRegion.BoundaryMode.Emit
                ))
                closeInitialHost(sb, host)

            case fe: Foreach[?, ?] @unchecked =>
                val region = ReactiveRegion.from(context, namespace)
                fe.applyTyped {
                    [T] => (signal, keyFn, renderFn) =>
                        val items    = SignalNow(signal)(using fe.frame, summon[SignalNow.Locals], summon[AllowUnsafe])
                        val rendered = items.toSeq.zipWithIndex.map { (item, i) =>
                            val key = keyFn match
                                case Present(f) => f(item)
                                case Absent     => i.toString
                            (key, renderFn(i, item))
                        }
                        val content = ReactiveRegion.tableContent(rendered.iterator.map(_._2))
                        val host    = ReactiveRegion.renderHost(region, parentContext, content)
                        openInitialHost(sb, host)
                        val keyed = keyFn.nonEmpty
                        DrawContext.inRegion(rendered.foreach { (key, child) =>
                            DrawContext.inRow(key)(renderTo(
                                sb,
                                child,
                                path :+ key,
                                context.child(key),
                                ReactiveRegion.namespace(host),
                                cssRules,
                                ReactiveRegion.contentParent(host),
                                ReactiveRegion.BoundaryMode.Emit,
                                keyedRoot = keyed
                            ))
                        })
                        closeInitialHost(sb, host)
                }

            case m: Mounted if Boundaries.implicitOf(m).nonEmpty =>
                renderTo(sb, Boundaries.implicitOf(m).get, path, context, namespace, cssRules, parentContext, boundaryMode)

            case b: Bounded =>
                // A range of its own, so the boundary can later replace what it shows: its placeholder while it holds a mount that loads,
                // else its content, or the live state the engine resolved.
                val region  = ReactiveRegion.from(context, namespace)
                val content = b.display match
                    case Present(d) => d
                    case Absent     =>
                        if b.settings.waits &&
                            DrawContext.inRange(Boundaries.catchesNow(b.child, context.transparent, MountDispatch.drawingNow))
                        then
                            b.settings.placeholderView.get
                        else b.child
                val host = ReactiveRegion.renderHost(region, parentContext, ReactiveRegion.tableContent(content))
                // A boundary around one mount, which a mount's own `.placeholder` is, is that mount's slot: an enclosing repaint
                // leaves the instance's live content alone, as it does for a mount without one.
                val flags = b.child match
                    case m: Mounted => ReactiveRegion.mountSlotFlags(m.key, m.frame)
                    case _          => ""
                openInitialHost(sb, host, flags)
                DrawContext.inRange(renderTo(
                    sb,
                    content,
                    path,
                    context.transparent,
                    ReactiveRegion.namespace(host),
                    cssRules,
                    ReactiveRegion.contentParent(host),
                    ReactiveRegion.BoundaryMode.Emit
                ))
                closeInitialHost(sb, host)

            case d: Drawn =>
                renderDrawn(sb, d, path, context, namespace, cssRules, parentContext)

            case m: Mounted =>
                // A mount's live content belongs to the instance the renderer runs, so a static render shows its placeholder,
                // inside a range of its own that the instance's updates then replace.
                val region      = ReactiveRegion.from(context, namespace)
                val placeholder = m.placeholderUI.getOrElse(UI.empty(using m.frame))
                val host        = ReactiveRegion.renderHost(region, parentContext, ReactiveRegion.tableContent(placeholder))
                openInitialHost(sb, host, ReactiveRegion.mountSlotFlags(m.key, m.frame))
                DrawContext.inRegion(renderTo(
                    sb,
                    placeholder,
                    path,
                    context.transparent,
                    ReactiveRegion.namespace(host),
                    cssRules,
                    ReactiveRegion.contentParent(host),
                    ReactiveRegion.BoundaryMode.Emit
                ))
                closeInitialHost(sb, host)
    end renderTo

    /** A drawn mount in a range of its own, holding what it drew, or the live state the engine resolved. The draw is filed at the node's
      * place, where the subscription that follows takes it as its instance's content, so it is on the page before any instance exists.
      * A place an instance already shows is left to that instance: its own paint goes behind this one, and a client that kept the slot
      * under the same key keeps it.
      */
    private def renderDrawn(
        sb: StringBuilder,
        d: Drawn,
        path: Seq[String],
        context: ReactiveRegion.RegionIdentity,
        namespace: ReactiveRegion.Namespace,
        cssRules: Maybe[CssCollector],
        parentContext: ReactiveRegion.ParentContext
    )(using AllowUnsafe, Frame, SignalNow.Locals): Unit =
        val region                                                                                 = ReactiveRegion.from(context, namespace)
        val inner                                                                                  = context.transparent
        def inRange(hostContent: UI, flags: String, body: Maybe[UI])(using SignalNow.Locals): Unit =
            val host = ReactiveRegion.renderHost(region, parentContext, ReactiveRegion.tableContent(hostContent))
            openInitialHost(sb, host, flags)
            body.foreach(renderTo(
                sb,
                _,
                path,
                inner,
                ReactiveRegion.namespace(host),
                cssRules,
                ReactiveRegion.contentParent(host),
                ReactiveRegion.BoundaryMode.Emit
            ))
            closeInitialHost(sb, host)
        end inRange
        // Counted whether or not it draws here, as the session's walk counts every keyed node it meets.
        val occurrence = DrawContext.occurrence(summon[SignalNow.Locals].read(DrawContext.scope).rows, d)
        MountDispatch.drawingNow.noteOccurrence(inner, d, occurrence)
        d.display match
            case Present(shown) => inRange(shown, ReactiveRegion.mountSlotFlags(d.key, d.frame), Present(shown))
            case Absent         =>
                MountDispatch.drawingNow.drawnNow(inner, d, occurrence) match
                    case DrawSlot.Live(instance, _) =>
                        DrawContext.inContent(instance.children.idBase, d, inner)(
                            inRange(instance.shownNow(d), ReactiveRegion.mountSlotFlags(d.key, d.frame), Absent)
                        )
                    case DrawSlot.Fresh(draw, _) => DrawContext.inContent(draw.base, d, inner) {
                            // A failed draw shows the node's own error view here; the instance reports it with the boundaries around it.
                            val content = draw.content(MountedSpec(d, path, Nil))
                            // A walk of its own over the same content, so it counts from the start as the render does.
                            draw.painted(DrawContext.inRange(ReactiveUI.walkStatic(
                                content,
                                path,
                                namespace == ReactiveRegion.Namespace.Svg,
                                inner,
                                ReactiveUI.nestedParentContext(parentContext, content),
                                MountDispatch.drawingNow,
                                rows = 0L,
                                deepRows = Nil
                            )._1))
                            // A slot without a key is never kept, so it needs no flags at all: what a repaint brings is what it shows.
                            val flags = if d.key.isEmpty then "" else ReactiveRegion.mountSlotFlags(d.key, d.frame, fresh = true)
                            inRange(content, flags, Present(content))
                        }
        end match
    end renderDrawn

    private def renderBoundElementBoundary(
        sb: StringBuilder,
        elem: Element,
        context: ReactiveRegion.RegionIdentity,
        namespace: ReactiveRegion.Namespace,
        parentContext: ReactiveRegion.ParentContext,
        boundaryMode: ReactiveRegion.BoundaryMode
    )(render: => Unit)(using Frame): Unit =
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
        render
        host.foreach(closeInitialHost(sb, _))
    end renderBoundElementBoundary

    /** Open a region's host. `flags` is the marker's flag section, empty for every region but a mount slot (see
      * [[ReactiveRegion.mountSlotFlags]]); an SVG region has an element rather than markers and carries none.
      */
    private def openInitialHost(sb: StringBuilder, host: ReactiveRegion.RenderHost, flags: String = ""): Unit =
        host match
            case ReactiveRegion.RenderHost.HtmlComments(id, _) => w(sb, s"<!--kyo-rs:$id$flags-->")
            case ReactiveRegion.RenderHost.HtmlTableBody(id)   =>
                w(sb, s"<tbody data-kyo-range-host=\"$id\"><!--kyo-rs:$id$flags-->")
            case ReactiveRegion.RenderHost.SvgGroup(path) => w(sb, openSvgRegion(path))

    private def closeInitialHost(sb: StringBuilder, host: ReactiveRegion.RenderHost): Unit =
        host match
            case ReactiveRegion.RenderHost.HtmlComments(id, _) => w(sb, s"<!--kyo-re:$id-->")
            case ReactiveRegion.RenderHost.HtmlTableBody(id)   => w(sb, s"<!--kyo-re:$id--></tbody>")
            case _: ReactiveRegion.RenderHost.SvgGroup         => w(sb, "</g>")

    private def renderTextareaValue(sb: StringBuilder, ta: Textarea)(using AllowUnsafe, Frame, SignalNow.Locals): Unit =
        ta.value match
            case Present(Bound.Const(s))    => w(sb, esc(masked(ta.inputMask, s)))
            case Present(Bound.Ref(ref))    => w(sb, esc(masked(ta.inputMask, SignalNow(ref))))
            case Present(Bound.Follow(sig)) => w(sb, esc(masked(ta.inputMask, SignalNow(sig))))
            case _                          => ()

    // ---- Dropdown (custom div-based overlay) ----

    private def renderDropdown(sb: StringBuilder, dd: Dropdown, path: Seq[String], cssRules: Maybe[CssCollector])(using
        AllowUnsafe,
        Frame,
        SignalNow.Locals
    ): Unit =
        val baseId  = dd.attrs.identifier.getOrElse("")
        val ddStyle = reactiveStyleNow(dd.attrs)
        // Read current selected value for initial highlight
        val currentVal = dd.value match
            case Present(Bound.Ref(ref)) => SignalNow(ref)
            case Present(Bound.Const(s)) => s
            case _                       => ""
        renderDropdownWithValue(sb, dd, path, baseId, currentVal, cssRules, ddStyle)
    end renderDropdown

    private def renderDropdownWithValue(
        sb: StringBuilder,
        dd: Dropdown,
        path: Seq[String],
        baseId: String,
        currentVal: String,
        cssRules: Maybe[CssCollector],
        channelStyle: Seq[(String, String)] = Nil
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
        val styleAttr   = inlineCss(dd.attrs.uiStyle, pseudoClass.nonEmpty, channelStyle)
        val styleStr    = if styleAttr.nonEmpty then s""" style="${esc(styleAttr)}"""" else ""
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

    private[kyo] def tagName(elem: Element): String = elem match
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
        renderCommonAttrs(sb, elem.attrs, elem.isInstanceOf[Svg.SvgElement], cssRules, Nil)

    /** The `attrs` overload lets a caller render an element with a MODIFIED attribute set (the reactive
      * classes currently true are folded into the class list) without rebuilding the element itself.
      */
    private def renderCommonAttrs(
        sb: StringBuilder,
        attrs: Attrs,
        isSvg: Boolean,
        cssRules: Maybe[CssCollector],
        channelStyle: Seq[(String, String)]
    )(using Frame): Unit =
        attrs.identifier.foreach { id =>
            sb.append(" id=\"")
            escInto(sb, id)
            sb.append('"')
        }
        val pseudoClass = registerPseudoClass(cssRules, attrs.uiStyle)
        if attrs.cssClasses.nonEmpty || pseudoClass.nonEmpty then
            sb.append(" class=\"")
            var first = true
            attrs.cssClasses.foreach { cls =>
                if !first then sb.append(' ')
                first = false
                escInto(sb, cls)
            }
            pseudoClass.foreach { cls =>
                if !first then sb.append(' ')
                escInto(sb, cls)
            }
            sb.append('"')
        end if
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
        // Marker only: stop-propagation is decided server-side in EventDispatch.dispatchToElement; the client never reads this.
        attrs.stopPropagation.foreach(v => if v then w(sb, """ data-kyo-stop="1""""))
        // enter/leave transition class lists (read client-side by the patch-application code).
        attrs.enterTransition.foreach(c => w(sb, s""" data-kyo-enter="${esc(c)}""""))
        attrs.leaveTransition.foreach(c => w(sb, s""" data-kyo-leave="${esc(c)}""""))
        // Portal marker (read client-side by the patch-application code): the client re-homes this element to
        // document.body behind a data-kyo-portal-slot placeholder; SSR/SSG keep it inline.
        attrs.portal.foreach(v => if v then w(sb, """ data-kyo-portal="1""""))
        val css = inlineCss(attrs.uiStyle, pseudoClass.nonEmpty, channelStyle)
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
    private def renderReactiveAttrs(sb: StringBuilder, attrs: Attrs)(using AllowUnsafe, Frame, SignalNow.Locals): Unit =
        if attrs.reactiveAttrs.nonEmpty then
            attrs.reactiveAttrs.toSeq.sortBy(_._1).foreach { (name, sig) =>
                w(sb, s""" $name="${esc(SignalNow(sig))}"""")
            }

    /** Emits each reactive boolean attribute as a bare present attribute while its signal is true (mirrors
      * `boolAttr`); the client toggles it in place (HtmlOp.SetBoolAttrByPath). Sorted for deterministic output.
      */
    private def renderReactiveBoolAttrs(sb: StringBuilder, attrs: Attrs)(using AllowUnsafe, Frame, SignalNow.Locals): Unit =
        if attrs.reactiveBoolAttrs.nonEmpty then
            attrs.reactiveBoolAttrs.toSeq.sortBy(_._1).foreach { (name, sig) =>
                if SignalNow(sig) then w(sb, s" $name")
            }

    /** The inline-style channel's current declarations, for writing inline at render; the client patches them in place
      * afterwards (HtmlOp.SetStyleByPath). Empty when no channel is bound.
      */
    private def reactiveStyleNow(attrs: Attrs)(using AllowUnsafe, Frame, SignalNow.Locals): Seq[(String, String)] =
        attrs.reactiveStyle match
            case Present(sig) => CssStyleRenderer.declarations(SignalNow(sig))
            case Absent       => Nil

    /** The element's `style` attribute: its static declarations, then the channel's, which win where both name a
      * property. A generated pseudo-state class already carries the static base props in its own rule (see
      * registerPseudoClass), and rendering them inline too would shadow the pseudo-state override, so there only the
      * channel's declarations are inline.
      */
    private def inlineCss(static: Style, pseudo: Boolean, channel: Seq[(String, String)]): String =
        val base = if pseudo then "" else CssStyleRenderer.render(static)
        if channel.isEmpty then base
        else
            val own = channel.map((n, v) => s"$n: $v;").mkString(" ")
            if base.isEmpty then own else s"$base $own"
        end if
    end inlineCss

    /** The `filledClass` a text field carries on itself as first rendered: present while its starting text, as the field
      * shows it (masked), is not empty. The client keeps it right from there (`FilledClass`).
      */
    private def filledNow(elem: Element)(using AllowUnsafe, Frame, SignalNow.Locals): Seq[String] =
        elem match
            case ti: TextInput =>
                elem.attrs.dataAttrs.get(FilledClass.classKey) match
                    case Some(name) =>
                        val mask = ti match
                            case ci: ConstrainedInput => ci.inputMask
                            case _                    => Absent
                        val text = ti.value match
                            case Present(Bound.Const(v))    => masked(mask, v)
                            case Present(Bound.Ref(ref))    => masked(mask, SignalNow(ref))
                            case Present(Bound.Follow(sig)) => masked(mask, SignalNow(sig))
                            case _                          => ""
                        if FilledClass.filled(text) then Seq(name) else Seq.empty
                    case None => Seq.empty
            case _ => Seq.empty

    /** The reactive classes on now, for folding into the SSR class list: each toggled class whose signal is true (by name), then the
      * name each class-name channel holds (in the order they were set). The client patches them in place afterwards
      * (HtmlOp.SetClassByPath). Empty (and cheap) when none are bound.
      */
    private def reactiveTrueClasses(attrs: Attrs)(using AllowUnsafe, Frame, SignalNow.Locals): Seq[String] =
        val toggled =
            if attrs.reactiveClasses.isEmpty then Seq.empty
            else attrs.reactiveClasses.toSeq.sortBy(_._1).collect { case (name, sig) if SignalNow(sig) => name }
        if attrs.reactiveClassNames.isEmpty then toggled
        else toggled ++ attrs.reactiveClassNames.flatMap(SignalNow(_).toChunk)
    end reactiveTrueClasses

    private def renderElementAttrs(sb: StringBuilder, elem: Element)(using AllowUnsafe, Frame, SignalNow.Locals): Unit =
        // WHERE A CHANNEL EXISTS, THE CHANNEL IS THE VALUE. Shadows the file-level `boolAttr` for the whole
        // method, so a static value is dropped for any name a `Signal`-typed setter also bound. This is not a
        // preference: it is what the client already does at runtime: the channel patches its attribute on
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
                renderCheckedAttr(sb, r.checked)
                r.name.foreach(n => w(sb, s""" name="${esc(n)}""""))
            case i: Input =>
                if !owned("type") then w(sb, " type=\"text\"")
                renderValueAttr(sb, i.value, i.inputMask)
                boolAttr(sb, "disabled", i.disabled)
                boolAttr(sb, "readonly", i.readOnly)
                i.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
            case p: PasswordInput =>
                if !owned("type") then w(sb, " type=\"password\"")
                renderValueAttr(sb, p.value, p.inputMask)
                boolAttr(sb, "disabled", p.disabled)
                boolAttr(sb, "readonly", p.readOnly)
                p.placeholder.foreach(p2 => w(sb, s""" placeholder="${esc(p2)}""""))
            case e: EmailInput =>
                if !owned("type") then w(sb, " type=\"email\"")
                renderValueAttr(sb, e.value, e.inputMask)
                boolAttr(sb, "disabled", e.disabled)
                boolAttr(sb, "readonly", e.readOnly)
                e.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
            case t: TelInput =>
                if !owned("type") then w(sb, " type=\"tel\"")
                renderValueAttr(sb, t.value, t.inputMask)
                boolAttr(sb, "disabled", t.disabled)
                boolAttr(sb, "readonly", t.readOnly)
                t.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
            case u: UrlInput =>
                if !owned("type") then w(sb, " type=\"url\"")
                renderValueAttr(sb, u.value, u.inputMask)
                boolAttr(sb, "disabled", u.disabled)
                boolAttr(sb, "readonly", u.readOnly)
                u.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
            case s: SearchInput =>
                if !owned("type") then w(sb, " type=\"search\"")
                renderValueAttr(sb, s.value, s.inputMask)
                boolAttr(sb, "disabled", s.disabled)
                boolAttr(sb, "readonly", s.readOnly)
                s.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
            case n: NumberInput =>
                w(sb, " type=\"number\"")
                renderValueAttr(sb, n.value)
                boolAttr(sb, "disabled", n.disabled)
                boolAttr(sb, "readonly", n.readOnly)
                n.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
                n.min.foreach(v => w(sb, s""" min="${fmtD(v)}""""))
                n.max.foreach(v => w(sb, s""" max="${fmtD(v)}""""))
                n.step.foreach(v => w(sb, s""" step="${fmtD(v)}""""))
            case d: DateInput  => w(sb, " type=\"date\""); renderPickerAttrs(sb, d)
            case t: TimeInput  => w(sb, " type=\"time\""); renderPickerAttrs(sb, t)
            case c: ColorInput => w(sb, " type=\"color\""); renderPickerAttrs(sb, c)
            case r: RangeInput =>
                w(sb, " type=\"range\"")
                boolAttr(sb, "disabled", r.disabled)
                r.value match
                    case Present(Bound.Const(d))    => w(sb, s""" value="${fmtD(d)}"""")
                    case Present(Bound.Ref(ref))    => w(sb, s""" value="${fmtD(SignalNow(ref))}"""")
                    case Present(Bound.Follow(sig)) => w(sb, s""" value="${fmtD(SignalNow(sig))}"""")
                    case _                          => ()
                end match
                r.min.foreach(v => w(sb, s""" min="${fmtD(v)}""""))
                r.max.foreach(v => w(sb, s""" max="${fmtD(v)}""""))
                r.step.foreach(v => w(sb, s""" step="${fmtD(v)}""""))
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
                // A select has no `value` attribute in HTML: the browser ignores it and shows the first option. The
                // attribute stays for readers of the markup; the choice travels as the `value` PROPERTY, which the
                // client assigns after every paint, once the options are there.
                def bound(s: String): Unit =
                    w(sb, s""" value="${esc(s)}"""")
                    if !sel.attrs.jsProps.contains("value") then w(sb, s""" data-kyo-prop-value="${esc(s)}"""")
                sel.value match
                    case Present(Bound.Const(s))    => bound(s)
                    case Present(Bound.Ref(ref))    => bound(SignalNow(ref))
                    case Present(Bound.Follow(sig)) => bound(SignalNow(sig))
                    case _                          =>
                        // Fall back to first Opt child with selected(true)
                        val selected = Maybe.fromOption(sel.children.toSeq.collectFirst {
                            case opt: Opt if opt.selected == Present(true) =>
                                opt.value.getOrElse("")
                        })
                        selected match
                            case Present(v) if v.nonEmpty => w(sb, s""" value="${esc(v)}"""")
                            case _                        => ()
                end match
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

    /** Renders the `checked` state of a box the render binds, marked `data-kyo-bound` so the morph can tell it from a free
      * one: a bound box that is off renders no `checked`, exactly like a free one, and only the marker says that what it
      * shows is the render's to set on every pass.
      */
    private def renderCheckedAttr(sb: StringBuilder, value: Maybe[Bound[Boolean]])(using AllowUnsafe, Frame, SignalNow.Locals): Unit =
        value match
            case Present(Bound.Const(b))    => w(sb, " data-kyo-bound"); if b then w(sb, " checked")
            case Present(Bound.Ref(ref))    => w(sb, " data-kyo-bound"); if SignalNow(ref) then w(sb, " checked")
            case Present(Bound.Follow(sig)) => w(sb, " data-kyo-bound"); if SignalNow(sig) then w(sb, " checked")
            case _                          => ()

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
        AllowUnsafe,
        Frame,
        SignalNow.Locals
    ): Unit =
        value match
            case Present(Bound.Const(s))    => w(sb, s""" value="${esc(masked(mask, s))}"""")
            case Present(Bound.Ref(ref))    => w(sb, s""" value="${esc(masked(mask, SignalNow(ref)))}"""")
            case Present(Bound.Follow(sig)) => w(sb, s""" value="${esc(masked(mask, SignalNow(sig)))}"""")
            case _                          => ()

    private def renderPickerAttrs(sb: StringBuilder, pi: PickerInput)(using AllowUnsafe, Frame, SignalNow.Locals): Unit =
        boolAttr(sb, "disabled", pi.disabled)
        renderValueAttr(sb, pi.value)

    // ---- Event attributes ----

    private def hasSignalRefValue(value: Maybe[Bound[?]]): Boolean = value match
        case Present(_: Bound.Ref[?]) => true
        case _                        => false

    /** Whether the client has to forward the commit of an edit: to write a ref, or to put back what a followed signal's owner did not take. */
    private def bindsEdit(value: Maybe[Bound[?]]): Boolean = value match
        case Present(_: Bound.Ref[?] | _: Bound.Follow[?]) => true
        case _                                             => false

    /** Writes `data-kyo-ev` with every event the element's handlers and bindings make the client forward, and
      * answers whether there was one.
      */
    private def renderEventAttr(sb: StringBuilder, elem: Element): Boolean =
        // Written as they are found: the attribute opens with the first event and closes at the end, so a
        // plain element allocates nothing here.
        var first                   = true
        def add(name: String): Unit =
            discard(sb.append(if first then " data-kyo-ev=\"" else ",").append(name))
            first = false
        val attrs = elem.attrs
        if attrs.onClick.nonEmpty || attrs.onClickEvt.nonEmpty ||
            attrs.onClickSelf.nonEmpty || attrs.onClickSelfEvt.nonEmpty
        then add("click")
        if attrs.onContextMenu.nonEmpty || attrs.onContextMenuEvt.nonEmpty then add("contextmenu")
        if attrs.onFocus.nonEmpty || attrs.onFocusEvt.nonEmpty then add("focus")
        // "blurtext" marks a text field whose blur carries its text, so the client reads the text off the field only
        // where a handler takes it.
        val blurText = elem match
            case ti: TextInput => ti.onBlurText.nonEmpty
            case _             => false
        if attrs.onBlur.nonEmpty || attrs.onBlurEvt.nonEmpty || blurText then add("blur")
        if blurText then add("blurtext")
        if attrs.onKeyDown.nonEmpty || attrs.onKeyDownSelf.nonEmpty then add("keydown")
        if attrs.onKeyUp.nonEmpty then add("keyup")
        if attrs.onHover.nonEmpty || attrs.onHoverEvt.nonEmpty then add("mouseover")
        if attrs.onUnhover.nonEmpty || attrs.onUnhoverEvt.nonEmpty then add("mouseout")
        if attrs.onScroll.nonEmpty || attrs.onScrollEvt.nonEmpty then add("wheel")
        if attrs.onDragStart.nonEmpty || attrs.onDragStartEvt.nonEmpty then add("dragstart")
        if attrs.onDragEnd.nonEmpty || attrs.onDragEndEvt.nonEmpty then add("dragend")
        if attrs.onDragEnter.nonEmpty || attrs.onDragEnterEvt.nonEmpty then add("dragenter")
        if attrs.onDragLeave.nonEmpty || attrs.onDragLeaveEvt.nonEmpty then add("dragleave")
        if attrs.onDragOver.nonEmpty || attrs.onDragOverEvt.nonEmpty then add("dragover")
        if attrs.onDrop.nonEmpty || attrs.onDropEvt.nonEmpty then add("drop")
        if attrs.onSortMove.nonEmpty || attrs.onSortMoveEvt.nonEmpty then add("sortmove")
        if attrs.onScrollPos.nonEmpty then add("scroll")
        // "input" event: when handler is set OR when .value(SignalRef) auto-binding is in use
        elem match
            case ti: TextInput if ti.onInput.nonEmpty || hasSignalRefValue(ti.value) => add("input")
            case _                                                                   =>
        // "change" event: when handler is set OR when .value/.checked(SignalRef) auto-binding is in use
        elem match
            case ti: TextInput if ti.onChange.nonEmpty || bindsEdit(ti.value)          => add("change")
            case pi: PickerInput if pi.onChange.nonEmpty || bindsEdit(pi.value)        => add("change")
            case bi: BooleanInput if bi.onChange.nonEmpty || bindsEdit(bi.checked)     => add("change")
            case ni: NumberInput if ni.onChangeNumeric.nonEmpty || bindsEdit(ni.value) => add("change")
            case ri: RangeInput if ri.onChange.nonEmpty || bindsEdit(ri.value)         => add("change")
            case fi: FileInput if fi.onChange.nonEmpty || attrs.onFileSelect.nonEmpty  => add("change")
            case sel: Select if bindsEdit(sel.value)                                   => add("change")
            case _                                                                     =>
        end match
        elem match
            case f: Form if f.onSubmit.nonEmpty || f.onSubmitEvt.nonEmpty => add("submit")
            case _                                                        =>
        if attrs.onPointerDown.nonEmpty then add("pointerdown")
        if attrs.onPointerMove.nonEmpty then add("pointermove")
        if attrs.onPointerUp.nonEmpty then add("pointerup")
        // fileselect marker so the client's change branch reads all files instead of only the first.
        if attrs.onFileSelect.nonEmpty then add("fileselect")
        if !first then sb.append('"')
        !first
    end renderEventAttr

    /** Whether an element renders its `data-kyo-path`.
      *
      * The attribute is what a client reads to name an element to the server and to find one the server named,
      * and it is read on exactly these: an element that declares a handler (the event's path; reported by
      * [[renderEventAttr]] as it writes `data-kyo-ev`, so the two cannot drift apart), a control (the
      * dispatcher's target facts: disabled, submitting button, "a control below"), a keyed row root (the key the
      * reconciliation and a list patch match rows by), an element with a reactive attribute or class (patched in
      * place by path), an element the client-side machinery addresses (focus seeding and restore, focus groups,
      * enter and leave transitions, portals, drag, scroll-auto, JS props), an element the application gave an
      * `id` to, and every SVG element (a region update replaces a lowered SVG node by its path). A plain element,
      * a `td` holding text or a layout `div`, is named by nobody, and most elements of a large page are plain. An
      * event inside a plain element reports the nearest
      * carrying ancestor, which is where the dispatcher's bubble arrives anyway; the one question that asks
      * whether the click landed on that element itself travels as `MouseEventData.self`. The path-addressed
      * `UI.Commands.command` and `requestMeasure` reach a carrying element only, which is why an `id` counts: it
      * is how an application says it means to address an element that has nothing else to be addressed for.
      */
    private[kyo] def carriesPath(elem: Element, namespace: ReactiveRegion.Namespace, keyedRoot: Boolean): Boolean =
        keyedRoot ||
            namespace == ReactiveRegion.Namespace.Svg ||
            elem.isInstanceOf[Svg.SvgElement] ||
            elem.isInstanceOf[Focusable] ||
            addressedByClient(elem.attrs) ||
            Channels.follows(elem)

    private def addressedByClient(attrs: Attrs): Boolean =
        attrs.identifier.nonEmpty ||
            attrs.hasChannels || attrs.tabIndex.nonEmpty || attrs.focusTrap.nonEmpty || attrs.focusGroup.nonEmpty ||
            attrs.focusAuto.nonEmpty || attrs.focusRestore.nonEmpty ||
            attrs.dragSource.nonEmpty || attrs.dropTarget.nonEmpty || attrs.scrollAuto.nonEmpty ||
            attrs.enterTransition.nonEmpty || attrs.leaveTransition.nonEmpty || attrs.portal.nonEmpty ||
            attrs.jsProps.nonEmpty || attrs.onAttached.nonEmpty

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

    /** A render path as a `data-kyo-path` value. Keys are application data, so the joined path is escaped like any
      * other attribute value.
      */
    private def pathAttr(path: Seq[String]): String = esc(path.mkString("."))

    private def fmtD(v: Double): String = NumberFormat.double(v)

    private inline def w(sb: StringBuilder, s: String): Unit =
        sb.append(s); ()

    private def escaped(c: Char): String =
        c match
            case '&'  => "&amp;"
            case '<'  => "&lt;"
            case '>'  => "&gt;"
            case '"'  => "&quot;"
            case '\'' => "&#39;"
            case _    => null

    private def firstSpecial(s: String): Int =
        var i = 0
        val n = s.length
        while i < n && (escaped(s.charAt(i)) eq null) do i += 1
        if i < n then i else -1
    end firstSpecial

    /** `s` HTML-escaped; the string itself when it has nothing to escape, which is most text. */
    private def esc(s: String): String =
        if firstSpecial(s) < 0 then s
        else
            val sb = new StringBuilder(s.length + 8)
            escInto(sb, s)
            sb.toString

    /** Writes `s` HTML-escaped into `sb`, copying runs between special characters in one go. */
    private def escInto(sb: StringBuilder, s: String): Unit =
        val n     = s.length
        var start = 0
        var i     = 0
        while i < n do
            val rep = escaped(s.charAt(i))
            if rep ne null then
                if start < i then discard(sb.underlying.append(s, start, i))
                sb.append(rep)
                start = i + 1
            end if
            i += 1
        end while
        if start == 0 then sb.append(s)
        else if start < n then discard(sb.underlying.append(s, start, n))
    end escInto

    /** Writes the `data-kyo-path` value, segments joined by `.` and escaped like [[pathAttr]], without building
      * the joined string.
      */
    private def appendPath(sb: StringBuilder, path: Seq[String]): Unit =
        val it = path.iterator
        if it.hasNext then escInto(sb, it.next())
        while it.hasNext do
            discard(sb.append('.'))
            escInto(sb, it.next())
    end appendPath

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
        """// Twin of ReactiveRegion.isValidHtmlId: `r`, a dot and a segment per path segment (verbatim [0-9A-Za-z_],
          |// `$` + 4 hex for any other UTF-16 unit), then an optional `~` and a decimal depth without a leading zero.
          |function kyoRangeId(id){
          |  return typeof id==="string"&&/^r(?:\.(?:[0-9A-Za-z_]|\$[0-9a-f]{4})*)*(?:~[1-9][0-9]*)?$/.test(id);
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
          |// The morph and the list patch live in MorphScript: both clients run that one walk over their own hooks.
          |var __kyoMorph=(function(h){
          |""".stripMargin + MorphScript.js +
            """})({syncField:__kyoSyncField,portalTwin:__kyoPortalTwin,rewrite:function(slot,twin,change){return __kyoRangeRewrite(twin,change);},
          |  beginAnimations:ba,guardAttached:false,listBefore:__kyoListBefore,listAfter:__kyoListAfter});
          |var __kyoMorphRun=__kyoMorph.run,__kyoSpanId=__kyoMorph.spanId,__kyoLogicalNext=__kyoMorph.logicalNext,__kyoEachSpanNode=__kyoMorph.eachSpanNode;
          |// Nested ranges reconcile as logical children, so a region containing another region morphs like any other;
          |// kyoRangeReplace re-reads the live markers afterwards and hands the registry what is actually there.
          |function kyoRangeMorph(endpoints,fragment){
          |  __kyoMorphRun(endpoints.start.parentNode,endpoints.start.nextSibling,endpoints.end,fragment.firstChild,null);
          |  return true;
          |}
          |// The ranges the live content holds right now, read off the DOM: the morph keeps the markers of a range that
          |// survived and clones or drops the rest, so the registry cannot be updated from the payload the way the
          |// wholesale path does it. Twin of rescanRange in DomReactiveRegions.
          |function kyoRangeRescan(id,endpoints){
          |  var found=new Map();__kyoRangeScanRun(id,endpoints.start.nextSibling,endpoints.end,found);return found;
          |}
          |// The ranges of the sibling run [first,until) into found, nested ones included. Twin of scanSiblings in
          |// DomReactiveRegions.
          |function __kyoRangeScanRun(id,first,until,found){
          |  var open=[];
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
          |  var n=first;
          |  while(n&&n!==until){var next=n.nextSibling;visit(n);n=next;}
          |  if(open.length)kyoRangeFail("start marker has no end after morphing "+id+": "+open[open.length-1].id);
          |}
          |// Runs change, which replaces or reconciles node outside any range patch, and brings the registry in line with
          |// what stands afterwards: the ranges registered under node are retired first (a change may drop markers, whose
          |// ids are unreadable afterwards), and the ranges under the node change answers are read after it. A portal twin
          |// reconciled through its slot and a replaced SVG region are such changes. Twin of DomReactiveRegions.rewrite.
          |function __kyoRangeRewrite(node,change){
          |  var before=__kyoRanges?__kyoRangesUnder(node):[];
          |  var after=change();
          |  if(__kyoRanges){
          |    for(var i=0;i<before.length;i++)__kyoRanges.delete(before[i]);
          |    if(after){var found=new Map();__kyoRangeScanRun("rewrite",after,after.nextSibling,found);
          |      found.forEach(function(pair,key){__kyoRanges.set(key,pair);});}}
          |  return after;
          |}
          |// Drops the ranges registered under node, which is about to leave the document outside any range patch (a
          |// retired portal twin), so the registry names only markers in the document. Twin of
          |// DomReactiveRegions.forgetRangesUnder.
          |function __kyoRangeForget(node){
          |  if(!__kyoRanges)return;
          |  var ids=__kyoRangesUnder(node);for(var i=0;i<ids.length;i++)__kyoRanges.delete(ids[i]);
          |}
          |// The ids of the registered ranges whose opening marker is node or lies under it.
          |function __kyoRangesUnder(node){
          |  var out=[];
          |  function take(n){var key=__kyoSpanId(n);if(key!==null){var pair=__kyoRanges.get(key);if(pair&&pair.start===n)out.push(key);}}
          |  take(node);
          |  if(node.nodeType===1){var w=document.createTreeWalker(node,128,null,false);while(w.nextNode())take(w.currentNode);}
          |  return out;
          |}
          |// Every range id opening at or under node. Twin of collectStartIds in DomReactiveRegions.
          |function __kyoCollectStartIds(node,into){
          |  var sid=__kyoSpanId(node);if(sid!==null)into.push(sid);
          |  for(var child=node.firstChild;child;child=child.nextSibling)__kyoCollectStartIds(child,into);
          |}
          |// The server's list patch: the registry lookup and its account of the patch around the walk in MorphScript. ReactiveUI refused the
          |// emission already if the rows are not addressable by key, so there is nothing to fall back to: a frame that left the untouched
          |// rows out gives us nothing to rebuild them from, and a row the live range does not hold is a protocol error.
          |function __kyoApplyList(id,keys,changedHtml){
          |  if(!kyoRangeId(id))kyoRangeFail("malformed replacement id: "+id);
          |  if(!__kyoRanges)kyoRangeFail("registry is closed");
          |  var endpoints=__kyoRanges.get(id);
          |  if(!endpoints)kyoRangeFail("unknown id: "+id);
          |  if(kyoRangeMarker(endpoints.start,"kyo-rs:")!==id||endpoints.end.data!=="kyo-re:"+id)kyoRangeFail("markers are corrupted: "+id);
          |  var parent=endpoints.start.parentNode;
          |  if(!parent||parent!==endpoints.end.parentNode)kyoRangeFail("anchors are no longer siblings: "+id);
          |  var range=document.createRange();range.setStartAfter(endpoints.start);range.setEndBefore(endpoints.end);
          |  var fragment=changedHtml.length?range.createContextualFragment(changedHtml):null;
          |  var prefix=__kyoRangeIdPath(id);if(prefix===null)prefix="";
          |  var full=[],i;
          |  for(i=0;i<keys.length;i++)full.push(prefix.length?prefix+"."+keys[i]:keys[i]);
          |  // The registry follows the patch as DomReactiveRegions.PatchScope does: the ranges of every row leaving or repainted are retired
          |  // before anything moves (a morph may drop markers, and a dropped marker's id is unreadable afterwards), and the ranges standing
          |  // where a row was painted or inserted are read right after.
          |  var retired=[],found=new Map();
          |  var scope={
          |    retire:function(node){__kyoEachSpanNode(node,function(n){__kyoCollectStartIds(n,retired);});},
          |    placed:function(first,until){__kyoRangeScanRun(id,first,until,found);},
          |    finish:function(){for(var r=0;r<retired.length;r++)__kyoRanges.delete(retired[r]);found.forEach(function(pair,key){__kyoRanges.set(key,pair);});}
          |  };
          |  if(!__kyoMorph.applyList({parent:parent,start:endpoints.start,end:endpoints.end,fragment:fragment},scope,full,null))
          |    kyoRangeFail("row is neither retained nor rendered in "+id);
          |}
          |// What a list patch reads before anything moves. Focus spans the whole region: a retained row keeps its DOM, but moving it is a
          |// remove and an insert, which blurs whatever it holds, so a pure reorder is precisely the case this must survive.
          |function __kyoListBefore(target,disturbed,repainted){
          |  var oldRoots=kyoRangeRoots(target.start,target.end),active=document.activeElement;
          |  var inside=active&&active!==document.body&&kyoRangeContains(oldRoots,active);
          |  return {
          |    locator:inside?kyoRangeFocusLocator(oldRoots,active):null,
          |    ss:inside&&typeof active.selectionStart==="number"?active.selectionStart:null,
          |    se:inside&&typeof active.selectionEnd==="number"?active.selectionEnd:null,
          |    oldEnter:kyoEnterPathsRoots(repainted),oldFocus:kyoFocusPathsRoots(repainted),
          |    ghosts:kyoLeavePrepareRoots(disturbed,target.fragment?kyoLeavePathsRoots(kyoRangeFragmentRoots(target.fragment)):{})
          |  };
          |}
          |// What a list patch does once the rows stand: the properties, focus and caret, enter and focus seeds, ghosts, portals, sweeps.
          |function __kyoListAfter(state,target,touched){
          |  var finalRoots=kyoRangeRoots(target.start,target.end),i;
          |  for(i=0;i<finalRoots.length;i++)applyJsProps(finalRoots[i]);
          |  var restored=kyoRangeResolveFocus(finalRoots,state.locator);
          |  if(restored){restored.focus({preventScroll:true});if(state.ss!==null)kyoSetCaret(restored,state.ss,state.se);}
          |  kyoSeedEnterRoots(touched,state.oldEnter);kyoSeedFocusRoots(touched,state.oldFocus);kyoSpawnGhosts(state.ghosts);
          |  for(i=0;i<touched.length;i++)__kyoAdoptPortals(touched[i]);
          |  __kyoRetireOrphanTwins();sweepFocusAuto();sweepScrollAuto(true);
          |}
          |// The topmost node above this one: the Document for anything attached, the detached subtree's own root
          |// otherwise. Two nodes share a tree exactly when this returns the same node for both. Twin of treeRoot
          |// in DomReactiveRegions.
          |function __kyoTreeRoot(node){var top=node;while(top.parentNode)top=top.parentNode;return top;}
          |// Is this live marker sitting inside a portal twin, an element the sweep re-homed to <body>?
          |// Twin of inPortalTwin in DomReactiveRegions.
          |function __kyoInPortalTwin(node){
          |  var el=node.parentNode;
          |  while(el&&el.nodeType===1){
          |    if(el.parentNode===document.body&&el.hasAttribute("data-kyo-portal"))return true;
          |    el=el.parentNode;}
          |  return false;
          |}
          |// A new mount instance paints this range next: every mount slot in it loses its adoption (`m`, `s`), keeping its id and key,
          |// so the paint resets the slots of the instances that showed there before. Twin of DomReactiveRegions.releaseSlots.
          |function kyoRangeRelease(id){
          |  var endpoints=__kyoRanges&&__kyoRanges.get(id);if(!endpoints)return;
          |  for(var n=endpoints.start.nextSibling;n&&n!==endpoints.end;n=n.nextSibling)__kyoReleaseSlotsUnder(n);
          |}
          |function __kyoReleaseSlotsUnder(node){
          |  __kyoReleaseSlot(node);
          |  if(node.nodeType===1){var w=document.createTreeWalker(node,128,null,false);while(w.nextNode())__kyoReleaseSlot(w.currentNode);}
          |}
          |function __kyoReleaseSlot(node){
          |  if(node.nodeType!==8||node.data.indexOf("kyo-rs:")!==0)return;
          |  var space=node.data.indexOf(" ");if(space<0)return;
          |  var flags=node.data.substring(space+1).split(" "),kept=[],dropped=false;
          |  for(var i=0;i<flags.length;i++){var f=flags[i];if(f==="m"||f==="s")dropped=true;else if(f)kept.push(f);}
          |  if(dropped)node.data=node.data.substring(0,space)+(kept.length?" "+kept.join(" "):"");
          |}
          |// A range that keeps showing old content while its new content loads: its elements turn inert and busy and focus leaves them,
          |// since the server drops what they would send. Only the marks this function set are removed again.
          |function kyoRangeHold(id,held){
          |  var endpoints=__kyoRanges&&__kyoRanges.get(id);if(!endpoints)return;
          |  var roots=kyoRangeRoots(endpoints.start,endpoints.end),ae=document.activeElement;
          |  for(var i=0;i<roots.length;i++){var r=roots[i];if(r.nodeType!==1)continue;
          |    if(held){if(r.hasAttribute("inert"))continue;r.setAttribute("inert","");r.setAttribute("aria-busy","true");r.setAttribute("data-kyo-held","");
          |      if(ae&&ae!==document.body&&r.contains(ae))ae.blur();}
          |    else if(r.hasAttribute("data-kyo-held")){r.removeAttribute("inert");r.removeAttribute("aria-busy");r.removeAttribute("data-kyo-held");}}
          |}
          |// A select chooses among the options it holds when its value is assigned, so a value assigned before its
          |// options arrived selects nothing: assign it again once options inside it were painted. applyJsProps calls
          |// it after each paint. Twin of reassertEnclosingSelect in DomBackend.
          |function __kyoReassertSelect(root){var pn=root.parentNode;if(!pn||pn.nodeType!==1)return;
          |  var s=pn.closest("select");if(s&&s.__kyoProps&&s.__kyoProps.value&&s.value!==s.__kyoProps.value.value)
          |    s.value=s.__kyoProps.value.value;
          |}
          |// The ids of the registered ranges inside the range, nested at any depth, read off the comments between its
          |// anchors: the cost is the size of the range, not of the registry. An id counts only when its entry opens at
          |// the marker found. Twin of nestedIn in DomReactiveRegions.
          |function kyoRangeNested(endpoints){
          |  var found=new Set(),walker=document.createTreeWalker(endpoints.start.parentNode,128,null,false);
          |  walker.currentNode=endpoints.start;
          |  for(var node=walker.nextNode();node&&node!==endpoints.end;node=walker.nextNode()){
          |    var key=__kyoSpanId(node);if(key===null)continue;
          |    var pair=__kyoRanges.get(key);if(pair&&pair.start===node)found.add(key);}
          |  return found;
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
          |  var fragment=parser.createContextualFragment(html),incoming=kyoRangeScan(fragment),removed=kyoRangeNested(endpoints);
          |  // Gone from the registry's point of view: inside the range about to be patched, or stranded in a tree
          |  // this one no longer shares. Every change that takes registered markers out of the document outside a
          |  // range patch drops their entries as it does it (__kyoRangeForget, __kyoRangeRewrite), so a stranded
          |  // entry is not expected; the test stays as the net for one that slipped through. A dead entry is not a
          |  // duplicate of the region coming back; it is the same region's corpse, and keeping it would refuse the
          |  // live one.
          |  //
          |  // The test is "same tree", not "in the document": a live portal twin under <body> shares the document
          |  // with the range being patched and therefore stays, which is what the twin check below relies on. It is
          |  // asked only of the ids the payload brings back, the only ones that can be refused.
          |  // Twin of the `removed` computation in DomReactiveRegions.
          |  // A region registered inside a portal twin is not a second copy of itself: the payload carries the portal
          |  // element inline (the twin's inline original), and this patch either morphs the twin in place (markers,
          |  // and so the registration, untouched) or replaces the range wholesale, in which case the incoming markers
          |  // take the id over and the sweep retires the stale twin. It must not join `removed` either: nothing inside
          |  // the live range re-registers it, so deleting it would leave the next write to that region unknown.
          |  var liveRoot=null;
          |  incoming.forEach(function(pair,key){var live=__kyoRanges.get(key);if(!live||removed.has(key))return;
          |    if(liveRoot===null)liveRoot=__kyoTreeRoot(endpoints.start);
          |    if(__kyoTreeRoot(live.start)!==liveRoot)removed.add(key);
          |    else if(!__kyoInPortalTwin(live.start))kyoRangeFail("duplicate id: "+key);});
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
          |  // Morph only siblings into siblings, as DomReactiveRegions does: rows arriving in a range host move the
          |  // markers into that host below, which a morph of the live nodes into the wrapper would skip.
          |  var hostWrapped=newRoots.length===1&&newRoots[0].tagName==="TBODY"&&newRoots[0].getAttribute("data-kyo-range-host")===id;
          |  var finalRoots,morphed=!synthetic&&!hostWrapped&&kyoRangeMorph(endpoints,fragment);
          |  if(morphed){finalRoots=kyoRangeRoots(endpoints.start,endpoints.end);
          |    removed.forEach(function(key){__kyoRanges.delete(key);});
          |    kyoRangeRescan(id,endpoints).forEach(function(pair,key){__kyoRanges.set(key,pair);});
          |    for(var i=0;i<finalRoots.length;i++)applyJsProps(finalRoots[i]);
          |  }else{
          |  range.deleteContents();removed.forEach(function(key){__kyoRanges.delete(key);});
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
          |  // (twin of adoptPortals and retireOrphanTwins in DomBackend.finishRangePatch).
          |  for(var pi=0;pi<finalRoots.length;pi++)__kyoAdoptPortals(finalRoots[pi]);
          |  __kyoRetireOrphanTwins();
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
    private val jsImplicitSubmit   = KeyPolicy.jsKeyTest(KeyPolicy.implicitSubmissionKeys)
    private val jsMayActivate      = KeyPolicy.jsKeyTest(KeyPolicy.anyActivationKeys)

    /** The server-push client script. `private[kyo]` so KeyPolicyTest can hold the two copies of the
      * keyboard rules against each other; nothing outside this module builds it.
      */
    /** The client's transport, registry and path helpers. A template of its own, with no arguments, because
      * `clientJs` is one interpolated template and the JVM keeps a template's literal text as a single constant of
      * at most 64 KB; an `s` template like the outer one, so the escapes it holds read the same way there and here.
      */
    private[kyo] val clientCoreJs: String =
        s"""var ws=null,__wsRetries=0,__wsGone=false,__live=false,__liveWaiters=[];
           |// Read-only test hook on the current socket; it follows each reconnect.
           |Object.defineProperty(window,"__kyoWs",{get:function(){return ws;},configurable:true});
           |// Read-only test hook on the range registry, so a test can check that it names only markers in the document.
           |Object.defineProperty(window,"__kyoRanges",{get:function(){return __kyoRanges;},configurable:true});
           |// Test hook: resolves once the session is live, at once when it already is. The first server frame is the only
           |// signal that the page can take an interaction, and no document readiness state carries it.
           |window.__kyoLive=function(){return __live?Promise.resolve(true):new Promise(function(r){__liveWaiters.push(r);});};
           |var __dragRt=null,__dragCleanup=null;
           |// One call site on purpose. The runtime wires document-level capture listeners and a pagehide listener that only its cleanup
           |// removes, so it must be installed once per session and never over a live one; the guard at the open handler is what keeps
           |// that true across reconnects, and its own cleanup is idempotent.
           |function kyoInstallDrag(){
           |  __dragRt=installDragRuntime(function(m){post(m);},{onClose:function(c){__dragCleanup=c;}});
           |  __dragCleanup=__dragRt.cleanup;
           |}
           |kyoInstallDrag();
           |// A session is one socket, and reconnecting is not optional bookkeeping. post() buffers into __q whenever the session is not
           |// live, and __q has exactly one drain point, so a page that never reconnects silently stops delivering every later
           |// interaction while still looking healthy: the click reaches the document, nothing reaches the server, and the failure only
           |// surfaces much later as an assertion against state that never arrived. The refusal that forced this is a connect denied for
           |// want of buffer space (Windows WSAENOBUFS), which is transient, so backing off and retrying is what rides it out.
           |//
           |// LIVE means a frame has ARRIVED, not that the socket opened. Completing the upgrade proves the transport is up and nothing
           |// more: a server can accept and then end the session at once, during a restart window, and draining on open would empty the
           |// buffer into a connection nobody reads, losing exactly the events this exists to preserve. Every session announces itself
           |// with SessionReady before it renders, so this resolves even for a tree that is entirely const and never renders. It is also
           |// what makes the backoff real: resetting the counter on open would let an accept-then-close loop redial forever at the
           |// shortest interval, which is the opposite of backing off.
           |function kyoConnect(){
           |  if(__wsGone)return;
           |  // Never dial while one is already dialing or open. Two paths can call in at once: a page becomes eligible for the
           |  // back/forward cache precisely when its socket is down, which is precisely when a retry is pending, so a restore
           |  // races the resumed timer. The loser of that race would be superseded immediately and never closed, leaving a live
           |  // connection with a session attached whose frames are all dropped.
           |  if(ws&&ws.readyState<2)return;
           |  __live=false;
           |  // The page's own path and query ride the upgrade, so a request-aware session evaluates the UI for the same page the GET served.
           |  var sock=new WebSocket((location.protocol===\"https:\"?\"wss:\":\"ws:\")+"//"+location.host+base+"/_kyo/ws?${UIServer.pageParam}="+encodeURIComponent(location.pathname+location.search));
           |  ws=sock;
           |  // Each handler serves ONE socket and stands down once a newer one has replaced it, closing itself on the way out so
           |  // a superseded connection is torn down instead of lingering with a server session attached.
           |  sock.onopen=function(){
           |    if(sock!==ws){sock.close();return;}
           |    if(!__dragCleanup)kyoInstallDrag();
           |  };
           |  sock.onmessage=function(e){
           |    if(sock!==ws){sock.close();return;}
           |    if(!__live){__live=true;__wsRetries=0;var pending=__q;__q=[];for(var i=0;i<pending.length;i++)sock.send(pending[i]);
           |      var waiters=__liveWaiters;__liveWaiters=[];for(var j=0;j<waiters.length;j++)waiters[j](true);}
           |    kyoOnMessage(e);
           |    // A patch may have written a field's value or its target's class from what the server last knew.
           |    __kyoSweepFilled();
           |  };
           |  // A connect that never completes reports here and then closes, so recovery is driven from onclose alone;
           |  // this handler exists so the failure does not surface as an unhandled error.
           |  sock.onerror=function(){};
           |  sock.onclose=function(){
           |    if(sock!==ws)return;
           |    __live=false;
           |    if(__dragCleanup){__dragCleanup();__dragCleanup=null;}
           |    if(__wsGone)return;
           |    // Capped exponential backoff: quick enough that a transient refusal recovers within a page's useful
           |    // lifetime, slow enough not to hammer a server that is genuinely down. Jittered to 50-100% of each step
           |    // so pages dropped together by a server restart do not all redial at the same instant.
           |    var wait=Math.min(250*Math.pow(2,__wsRetries),5000);
           |    __wsRetries++;
           |    setTimeout(kyoConnect,wait*(0.5+Math.random()*0.5));
           |  };
           |}
           |// A page being torn down is not a lost connection: stop redialing so an unloading page does not keep calling the server it is
           |// leaving. A page becomes eligible for the back/forward cache precisely when its socket is DOWN, since an open one usually
           |// blocks it, so a restored page has to redial and rebuild the range map the hide path cleared, or it returns permanently
           |// inert, which is the exact failure this reconnect exists to prevent.
           |window.addEventListener("pagehide",function(){__wsGone=true;});
           |window.addEventListener("pageshow",function(e){
           |  if(!e.persisted)return;
           |  if(!__kyoRanges)__kyoRanges=kyoRangeScan(document.body);
           |  __wsGone=false;__wsRetries=0;kyoConnect();
           |});
           |kyoConnect();
           |function kyoOnMessage(e){
           |  var op=JSON.parse(e.data);
           |  if(op.ResolveDrag){
           |    try{__dragRt.resolve(op.ResolveDrag.sessionId,op.ResolveDrag.decision);}catch(error){kyoClientError(error);}
           |  }else if(op.ReadDropFile||op.ReadDropDirectory||op.CancelDropRead){
           |    try{__dragRt.serveDropRead(op);}catch(error){kyoClientError(error);}
           |  }else if(op.PatchList){
           |    try{__kyoApplyList(op.PatchList.regionId,op.PatchList.keys,op.PatchList.changed);}
           |    catch(error){kyoClientError(error);}
           |  }else if(op.ReplaceRange){
           |    try{kyoRangeReplace(op.ReplaceRange.regionId,op.ReplaceRange.html);}catch(error){kyoClientError(error);}
           |  }else if(op.HoldRange){
           |    try{kyoRangeHold(op.HoldRange.regionId,op.HoldRange.held);}catch(error){kyoClientError(error);}
           |  }else if(op.ReleaseSlots){
           |    try{kyoRangeRelease(op.ReleaseSlots.regionId);}catch(error){kyoClientError(error);}
           |  }else if(op.Replace){
           |    var p=op.Replace.path.join(".");
           |    var el=document.querySelector(__kyoPathSel(p));
           |    // Replace is the SVG-only, path-addressed operation. HTML boundaries use ReplaceRange above.
           |    if(el&&el.outerHTML!==op.Replace.html){
           |      var ae=document.activeElement;
           |      var ap=ae&&ae!==document.body&&ae.getAttribute?ae.getAttribute("data-kyo-path"):null;
           |      var ss=(ae&&typeof ae.selectionStart==='number')?ae.selectionStart:null;
           |      var se=(ae&&typeof ae.selectionEnd==='number')?ae.selectionEnd:null;
           |      var __en=faEnterPaths(el);
           |      var __gh=kyoLeavePrepare(el,kyoLeaveSurv(op.Replace.html));
           |      var __fa=focusAutoPaths(el);
           |      // The element may hold HTML ranges (under a foreignObject), which the replacement removes and brings anew.
           |      var nel=__kyoRangeRewrite(el,function(){el.outerHTML=op.Replace.html;return document.querySelector(__kyoPathSel(p));});
           |      if(nel){applyJsProps(nel);ba(nel);}
           |      if(ap){var rf=document.querySelector(__kyoPathSel(ap));if(rf){rf.focus({preventScroll:true});if(ss!==null)kyoSetCaret(rf,ss,se);}}
           |      // Seed after focus/caret restore so a newly appeared focus-auto element wins restore-to-trigger.
           |      if(nel){kyoEnterSeed(nel,__en);seedFocusAuto(nel,__fa);}
           |      kyoSpawnGhosts(__gh);
           |      // Portal upkeep over what this replace inserted (twin of DomBackend.adoptPortals).
           |      if(nel)__kyoAdoptPortals(nel);__kyoRetireOrphanTwins();
           |    }
           |    sweepFocusAuto();sweepScrollAuto(true);
           |  }else if(op.Remove){
           |    var p=op.Remove.path.join(".");
           |    var el=document.querySelector(__kyoPathSel(p));
           |    var __rgh=el?kyoLeavePrepare(el,{}):[];
           |    if(el){__kyoRangeForget(el);el.remove();}
           |    kyoSpawnGhosts(__rgh);
           |    // The removed subtree may have held portal placeholders: retire their body twins.
           |    __kyoRetireOrphanTwins();
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
           |  }else if(op.CommandById){
           |    kyoApplyVerb(document.getElementById(op.CommandById.id),op.CommandById.verb);
           |  }else if(op.RequestMeasureById){
           |    // MeasureById carries `id` back; `path` is vestigial for id-routing, sent empty.
           |    var rmiel=document.getElementById(op.RequestMeasureById.id);
           |    if(rmiel){
           |      var rmir=rmiel.getBoundingClientRect();
           |      post({MeasureById:{path:[],id:op.RequestMeasureById.id,rectX:rmir.left,rectY:rmir.top,rectW:rmir.width,rectH:rmir.height,viewportW:window.innerWidth,viewportH:window.innerHeight}});
           |    }else post({MeasureMissing:{path:[],id:op.RequestMeasureById.id}});
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
           |    var sapp=op.SetAttrByPath.path.join(".");var sapel=document.querySelector(__kyoPathSel(sapp));if(sapel){sapel.setAttribute(op.SetAttrByPath.name,op.SetAttrByPath.value);__kyoSyncField(sapel,op.SetAttrByPath.name,op.SetAttrByPath.value);}
           |  }else if(op.SetBoolAttrByPath){
           |    var sbpp=op.SetBoolAttrByPath.path.join(".");var sbpel=document.querySelector(__kyoPathSel(sbpp));if(sbpel){if(op.SetBoolAttrByPath.value){sbpel.setAttribute(op.SetBoolAttrByPath.name,'');}else{sbpel.removeAttribute(op.SetBoolAttrByPath.name);}if(op.SetBoolAttrByPath.name==="checked"&&sbpel.tagName==="INPUT")__kyoSyncChecked(sbpel,op.SetBoolAttrByPath.value);}
           |  }else if(op.SetClassByPath){
           |    // Path-addressed reactive class: toggle in place so CSS transitions fire. Not owned: the render carries its value.
           |    var scpp=op.SetClassByPath.path.join(".");var scpel=document.querySelector(__kyoPathSel(scpp));if(scpel){scpel.classList.toggle(op.SetClassByPath.name,op.SetClassByPath.on);}
           |  }else if(op.SetStyleByPath){
           |    // Path-addressed inline style: drop what the previous value had and this one lacks, set the rest. Not owned: the render carries it.
           |    var sspp=op.SetStyleByPath.path.join(".");var sspel=document.querySelector(__kyoPathSel(sspp));if(sspel){var ssr=op.SetStyleByPath.removed||[];for(var ssri=0;ssri<ssr.length;ssri++)sspel.style.removeProperty(ssr[ssri]);var sss=op.SetStyleByPath.set||[];for(var sssi=0;sssi<sss.length;sssi++)sspel.style.setProperty(sss[sssi].name,sss[sssi].value);}
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
           |// Verb whitelist for CommandById; unknown verbs ignored (forward-compat).
           |function kyoApplyVerb(el,verb){
           |  if(!el)return;
           |  if(verb==="focus"){var fs='input,textarea,select,button,a[href],[tabindex],[contenteditable]';var ft=(el.matches&&el.matches(fs))?el:(el.querySelector?el.querySelector(fs):null);if(ft&&typeof ft.focus==="function")ft.focus();}
           |  else if(verb==="scrollIntoView"){if(typeof el.scrollIntoView==="function")el.scrollIntoView({block:"nearest"});}
           |}
           |// A reactive range's id encodes its own path (ReactiveRegion.htmlId: `.` and the segment per path segment,
           |// a unit outside [0-9A-Za-z_] written as `$$` + 4 hex, optional `~` + depth suffix), so the path is read
           |// back out of the id rather than re-encoded here. Returns null for anything that is not a range id.
           |function __kyoRangeIdPath(id){
           |  if(!kyoRangeId(id))return null;
           |  var end=id.indexOf("~");if(end<0)end=id.length;
           |  var segs=[],i=1;
           |  while(i<end){
           |    var j=i+1,s="";
           |    while(j<end&&id.charAt(j)!=="."){
           |      if(id.charAt(j)==="$$"){s+=String.fromCharCode(parseInt(id.substring(j+1,j+5),16));j+=5;}
           |      else{s+=id.charAt(j);j++;}
           |    }
           |    segs.push(s);i=j;
           |  }
           |  return segs.join(".");
           |}
           |function __kyoAttrSel(n,v){var bs=String.fromCharCode(92),q=String.fromCharCode(34);var s=String(v).split(bs).join(bs+bs).split(q).join(bs+q);return '['+n+'='+q+s+q+']';}
           |function __kyoPathSel(p){return __kyoAttrSel('data-kyo-path',p);}
           |""".stripMargin

    /** The server-push client script for a page under `basePath`.
      *
      * Built once. The template is rendered with a placeholder for the base path and its comment lines are dropped: they explain
      * the script to whoever reads this file, and on every page that ships it they were dead weight, close to half of it gzipped.
      * A page only splices its own base path in.
      */
    private[kyo] def clientJs(basePath: String): String = clientJsParts._1 + basePath + clientJsParts._2

    private lazy val clientJsParts: (String, String) =
        val slot = "\u0000kyo-base\u0000"
        val js   = clientJsTemplate(slot).linesIterator.filterNot(_.trim.startsWith("//")).mkString("\n")
        val at   = js.indexOf(slot)
        (js.substring(0, at), js.substring(at + slot.length))
    end clientJsParts

    private def clientJsTemplate(basePath: String): String =
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
           |  if(name==="value"&&(el.tagName==="INPUT"||el.tagName==="TEXTAREA")&&el.value!==value){el.value=value;__kyoFilled(el);}
           |  else if(name==="value"&&el.tagName==="SELECT"&&el.value!==value)el.value=value;
           |}
           |// A patch of checked: a control shows its property once the reader has toggled it. The browser took the group's selection from
           |// the other radios when the reader clicked this one, so each bound radio of the group shows what its attribute says again; a free
           |// radio's tick stays. Twin of syncChecked in DomBackend.
           |function __kyoSyncChecked(el,on){
           |  el.checked=on;
           |  if(el.type==="radio"&&el.name){var rs=document.querySelectorAll("input[type=radio][data-kyo-bound]");
           |    for(var i=0;i<rs.length;i++){var r=rs[i];if(r.name===el.name&&r.form===el.form)r.checked=r.hasAttribute("checked");}}
           |}
           |// filledClass: a text field's class follows whether it holds text (kyo.internal.FilledClass, shared with DomBackend).
           |${FilledClass.js}
           |// Mark an attr name as owned by the imperative id-addressed channel: names live in a __kyoOwn expando dict
           |// ON the element (reclaimed with the node), which __kyoMorphAttrs reads to shield each owned attr from
           |// reconciliation. Mirrors markOwned in DomBackend.
           |function __kyoMark(el,n){(el.__kyoOwn||(el.__kyoOwn={}))[n]=true;}
           |${DragClientJs.script(basePath)}
           |$clientCoreJs
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
           |      var sl=pp!==null?document.querySelector(__kyoAttrSel('data-kyo-portal-slot',pp)):null;
           |      n=sl?sl.parentElement:null;
           |    }else n=n.parentElement;
           |  }
           |  return false;
           |}
           |// Send each event over the session's WebSocket. ws.send preserves send order on one socket, so the
           |// explicit fetch-queue serialization is no longer needed. Events raised before the session is live are
           |// buffered in __q and flushed when its first frame arrives, which every reconnect reaches. Both an absent
           |// socket and an open-but-unanswered one take the buffer, so nothing is handed to a connection that has
           |// not proven a session is reading it.
           |function post(b){
           |  var m=JSON.stringify(b);
           |  if(ws&&__live&&ws.readyState===1)ws.send(m);
           |  else __q.push(m);
           |}
           |function pa(el){
           |  var p=el.getAttribute("data-kyo-path");
           |  return p===""?[]:p.split(".");
           |}
           |// Apply data-kyo-prop-* HTML attributes as JS DOM properties then remove the attr.
           |// Mirrors DomBackend.applyJsPropsSync for the HTTP/JVM rendering path.
           |// Each assignment is kept on the element (__kyoProps), for __kyoReassertSelect to assign again, with the value
           |// the element had before the first one, for __kyoRestoreProps to give back.
           |function applyJsPropsElement(el,pfx){for(var i=el.attributes.length-1;i>=0;i--){var n=el.attributes[i].name;
           |  if(n.indexOf(pfx)===0){var p=n.slice(pfx.length),v=el.getAttribute(n),ps=el.__kyoProps||(el.__kyoProps={});
           |    ps[p]={base:ps[p]?ps[p].base:el[p],value:v};el[p]=v;el.removeAttribute(n);}}
           |}
           |function applyJsProps(root){
           |  var pfx="data-kyo-prop-";applyJsPropsElement(root,pfx);var els=root.querySelectorAll("*");
           |  for(var i=0;i<els.length;i++)applyJsPropsElement(els[i],pfx);
           |  __kyoReassertSelect(root);
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
           |    // At <body> a class's percentage min-size resolves against the viewport and beats the inline size (a dropdown's
           |    // min-width:100% stretched the ghost to the window edge). Only the minimum is reset: max-* stays with the
           |    // classes, so a leave that collapses through max-height still can.
           |    g.style.boxSizing="border-box";g.style.minWidth="0";g.style.minHeight="0";
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
           |// Portal upkeep after a patch (twin of DomBackend.adoptPortals; keep the two in lockstep).
           |// Adopt: every data-kyo-portal element still sitting inline under root (root included; freshly inserted by
           |// this patch) is re-homed to document.body behind an inert data-kyo-portal-slot placeholder at its logical
           |// position. Must run after focus and enter seeding, both of which are subtree-scoped. Reparenting drops DOM
           |// focus, so focus and caret held inside the moved subtree are re-applied after the move; the enter
           |// transition is unaffected, since its from-state classes release on the next frame.
           |// Orphans: a body twin whose placeholder is gone (its slot was removed or replaced) left with its region:
           |// prepare its leave ghost, remove it, then spawn, since kyoSpawnGhosts drops ghosts whose source is still
           |// connected. Document-wide by necessity: the twin sits outside every region subtree, so the regular leave
           |// sweep cannot see it.
           |// Twin of DomBackend.portalTwin: the (unique) re-homed body child carrying the path.
           |function __kyoPortalTwin(p){return document.querySelector('body > '+__kyoPathSel(p)+'[data-kyo-portal]');}
           |// Re-homes the portal elements under root to document.body behind a placeholder. Twin of DomBackend.adoptPortals.
           |var __kyoPortaled=false;
           |function __kyoAdoptPortals(root){
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
           |        if(stale&&stale!==el){__kyoRangeForget(stale);document.body.removeChild(stale);}
           |        var svgNs="http://www.w3.org/2000/svg";
           |        var slot=(el.namespaceURI===svgNs)?document.createElementNS(svgNs,"g"):document.createElement("span");
           |        slot.setAttribute("data-kyo-portal-slot",p);
           |        slot.setAttribute("hidden","");
           |        var ae=document.activeElement;
           |        var had=ae&&ae!==document.body&&(el===ae||el.contains(ae));
           |        var ss=(had&&typeof ae.selectionStart==='number')?ae.selectionStart:null;
           |        var se=(had&&typeof ae.selectionEnd==='number')?ae.selectionEnd:null;
           |        el.parentNode.insertBefore(slot,el);
           |        document.body.appendChild(el);__kyoPortaled=true;
           |        if(had){ae.focus({preventScroll:true});if(ss!==null&&typeof ae.setSelectionRange==='function'){try{ae.setSelectionRange(ss,se);}catch(e){if(e.name!=='InvalidStateError')throw e;}}}
           |      }
           |    }
           |  }
           |}
           |// Retires every body twin whose placeholder is gone; once per patch, and only once something was ever adopted, since
           |// a twin exists only after an adoption. Twin of DomBackend.retireOrphanTwins.
           |function __kyoRetireOrphanTwins(){
           |  if(!__kyoPortaled)return;
           |    var twins=document.querySelectorAll('body > [data-kyo-portal][data-kyo-path]');
           |    for(var t=0;t<twins.length;t++){
           |      var tw=twins[t];
           |      var tp=tw.getAttribute("data-kyo-path");
           |      if(!document.querySelector(__kyoAttrSel('data-kyo-portal-slot',tp))){
           |        var tg=kyoLeavePrepare(tw,{});
           |        __kyoRangeForget(tw);
           |        document.body.removeChild(tw);
           |        kyoSpawnGhosts(tg);
           |      }
           |    }
           |}
           |// Scroll-into-view for [data-kyo-scroll-auto]: the flag moves with a roving highlight, so the carrier
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
           |  if(!fresh||typeof fresh.scrollIntoView!=='function')return;
           |  if(scroll)fresh.scrollIntoView({block:"nearest",inline:"nearest"});
           |  // The first paint leaves the document where the browser restores it, and brings a flag in a scroll
           |  // container of the page's own into view, scrolling that container only. Mirrors DomBackend.sweepScrollAuto.
           |  else if(__kyoInOwnScroller(fresh))fresh.scrollIntoView({block:"nearest",inline:"nearest",container:"nearest"});
           |}
           |function __kyoInOwnScroller(el){
           |  for(var p=el.parentElement;p&&p!==document.body&&p!==document.documentElement;p=p.parentElement){
           |    var o=getComputedStyle(p).overflowY;
           |    if(o==="auto"||o==="scroll"||o==="overlay")return true;
           |  }
           |  return false;
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
           |    if(document.querySelector(__kyoPathSel(top.fa)+'[data-kyo-focus-auto]'))return;
           |    __focusReturnStack.pop();
           |    // At most one restore per unwind: a deeper entry belongs to a seed that closed while a newer one stayed
           |    // open, so its return target is stale and must not override the one just restored.
           |    // And only where the removal took the focus with it: the browser leaves activeElement on body when the
           |    // focused element goes, while any other element there means the reader moved focus themselves (a Tab out
           |    // of a combobox) and putting it back would undo that. Twin of DomBackend.focusWasLost.
           |    var lost=!document.activeElement||document.activeElement===document.body;
           |    if(!restored&&top.restore&&top.ret&&lost){var re=document.querySelector(__kyoPathSel(top.ret));if(re&&typeof re.focus==='function'){re.focus({preventScroll:true});restored=true;}}
           |  }
           |}
           |applyJsProps(document.body);ba(document.body);
           |kyoEnterSeed(document.body,{});
           |// Initial mount: everything server-rendered is new (empty old set), like native autofocus.
           |seedFocusAuto(document.body,{});
           |// Record what already carries the scroll flag without acting on it: a page that arrives with a
           |// highlight has not moved it, and scrolling on load would fight the browser's own restoration.
           |sweepScrollAuto(false);
           |// filledClass: a field the reader typed into before the script ran (or one the browser refilled) is put right, and
           |// every later keystroke, paste or autofill toggles its class where it happens.
           |__kyoSweepFilled();
           |["input","change"].forEach(function(t){document.body.addEventListener(t,function(e){var f=e.target;if(f&&f.getAttribute&&(f.getAttribute("${FilledClass.classAttr}")||f.getAttribute("${FilledClass.upAttr}")))__kyoFilled(f);},true);});
           |// Portal adopt for the initial paint: a portal element present at load re-homes immediately.
           |__kyoAdoptPortals(document.body);__kyoRetireOrphanTwins();
           |// Dropdown helpers: close all dropdowns except the given id
           |function kyoCloseDropdown(exceptId){
           |  var all=document.querySelectorAll('[data-kyo-dropdown-options]');
           |  Array.prototype.forEach.call(all,function(el){
           |    var id=el.getAttribute('data-kyo-dropdown-options');
           |    if(id!==exceptId)el.hidden=true;
           |  });
           |}
           |// Build a mouse payload, omitting targetId when absent (null JSON would break Maybe[String] decode). `self`
           |// is false only when the DOM target sat inside the element the path names (see MouseEventData.self).
           |function mkMouse(mods,tid,pos,self){var m={modifiers:mods};if(tid)m.targetId=tid;if(pos)m.position=pos;if(self===false)m.self=false;return m;}
           |// Viewport coordinates of a pointer event; the events without a pointer (focus, blur, submit) pass no third argument.
           |function mkPos(e){return {x:e.clientX,y:e.clientY};}
           |// Build a keyboard payload, omitting targetId when absent; `self` as for a mouse payload (see KeyboardEventData.self).
           |function mkKbd(key,mods,tid,self){var k={key:key,modifiers:mods};if(tid)k.targetId=tid;if(self===false)k.self=false;return k;}
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
           |    // An anchor the UI handles stays on the page; one it does not handle is a link, and the browser follows it.
           |    // A modified click (a new tab or window) and a download anchor keep their default; the handler runs either way.
           |    var kmod=e.ctrlKey||e.metaKey||e.shiftKey||e.altKey||e.button!==0;
           |    var mid=e.target&&e.target.id?e.target.id:null;if(!kmod&&el.tagName&&el.tagName.toLowerCase()==='a'&&!el.hasAttribute("download")&&he(el,"click"))e.preventDefault();post({Click:{path:p,mouse:mkMouse({ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey},mid,mkPos(e),e.target===el)}});window._kyoClickSubmit=true;setTimeout(function(){window._kyoClickSubmit=false},0);
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
           |      // Otherwise the first file's text goes out as a Change.
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
           |    // A radio group with an inert radio in it: the browser's own walk lands on the inert radio, whose selection
           |    // the inert click block declines, and sticks there. The client walks instead, to where the browser lands
           |    // when the inert radios are disabled, and an inert radio that is selected holds the group. Twin of
           |    // KeyPolicy.radioStep, which carries the rule; RadioTest holds this script to it in Chrome.
           |    if(e.target&&e.target.tagName==="INPUT"&&e.target.type==="radio"&&e.target.name&&(${KeyPolicy.jsKeyTest(
              KeyPolicy.radioStepKeys
          )})){
           |      var __rb=e.target,__ra=(__rb.form||__rb.ownerDocument).querySelectorAll('input[type="radio"]'),__rg=[],__rf=-1;
           |      for(var __ri=0;__ri<__ra.length;__ri++){var __rx=__ra[__ri];if(__rx.name===__rb.name&&__rx.form===__rb.form){if(__rx===__rb)__rf=__rg.length;__rg.push(__rx);}}
           |      var __rin=function(x){return !!x.closest('[data-kyo-inert]');};
           |      if(__rf>=0&&__rg.some(__rin)){
           |        e.preventDefault();
           |        if(!(__rin(__rb)&&__rb.checked)){
           |          var __rtl=getComputedStyle(__rb).direction==="rtl",__rk=e.key;
           |          var __rfw=__rk==="ArrowDown"||(__rk==="ArrowRight"&&!__rtl)||(__rk==="ArrowLeft"&&__rtl);
           |          var __rn=__rg.length,__rs=__rfw?1:__rn-1,__rj=(__rf+__rs)%__rn;
           |          for(var __rl=__rn-1;__rl>0;__rl--){var __ry=__rg[__rj];if(!__rin(__ry)&&!__ry.disabled){__ry.focus();__ry.click();break;}__rj=(__rj+__rs)%__rn;}
           |        }
           |      }
           |    }
           |    // preventScrollKeys: suppress native page-scroll for nav keys in a data-kyo-scroll-keys region; keydown still posts below.
           |    // Every key and tag list below is interpolated from KeyPolicy, which the SPA client calls directly,
           |    // so the two transports cannot answer this differently; KeyPolicyTest holds this text against it.
           |    if(e.target&&e.target.closest&&e.target.closest('[data-kyo-scroll-keys]')){
           |      var __sk=e.target,__ed=(/^(${KeyPolicy.jsTagTest(KeyPolicy.editableTags)})$$/.test(__sk.tagName)||__sk.isContentEditable);
           |      var __it=__sk.tagName==="INPUT"?(__sk.getAttribute("type")||"").toLowerCase():"";
           |      var __st=((${KeyPolicy.jsKeyTest(KeyPolicy.arrowStepKeys)})&&/^(${KeyPolicy.jsTagTest(
              KeyPolicy.arrowSteppingInputTypes
          )})$$/.test(__it))||((${KeyPolicy.jsKeyTest(KeyPolicy.pageStepKeys)})&&/^(${KeyPolicy.jsTagTest(
              KeyPolicy.pageSteppingInputTypes
          )})$$/.test(__it));
           |      var __vc=(/^(${KeyPolicy.jsTagTest(KeyPolicy.verticalConsumerTags)})$$/.test(__sk.tagName)||__sk.isContentEditable||__st);
           |      var __sa=(/^(${KeyPolicy.jsTagTest(KeyPolicy.spaceActivatedTags)})$$/.test(__sk.tagName));
           |      var __vk=(${KeyPolicy.jsKeyTest(KeyPolicy.verticalScrollKeys)});
           |      var __hk=(${KeyPolicy.jsKeyTest(KeyPolicy.edgeScrollKeys)});
           |      // Space is here for the same reason the arrows are: a list that is one tab stop has no
           |      // native control to consume it, so a Space that picks the highlighted row would also scroll
           |      // the page a screenful. It stays with whatever would type it or activate on it.
           |      var __spk=(e.key===" "&&!__ed&&!__sa);
           |      if((__vk&&!__vc)||(__hk&&!__ed)||__spk)e.preventDefault();
           |    }
           |    // An element with a click handler is activated twice once a keydown is posted at all: once
           |    // by the browser, once by the dispatcher, which emulates that activation where no browser
           |    // does it. Suppress the browser's, so it acts once and its own onKeyDown still sees the key.
           |    // A button takes Enter and Space, an anchor Enter alone (Space scrolls with a link focused).
           |    // The dispatcher answers a keydown by synthesizing the click it stands for (at the
           |    // button for keyboard activation, at the form's default button for implicit submission),
           |    // so the browser's own would be the second one. Twin of KeyPolicy.doubleActivates;
           |    // FormActivationTest holds this script against it.
           |    if(($jsMayActivate)&&e.target&&e.target.tagName&&he(e.target,"keydown")){
           |      var __own=e.target.getAttribute("data-kyo-ev");
           |      var __ck=!!(__own&&__own.split(",").indexOf("click")>=0);
           |      var __tg=e.target.tagName;
           |      var __inf=!!(e.target.closest&&e.target.closest("form"));
           |      // An anchor keeps the click-handler gate: suppressing Enter on a plain link would take
           |      // its navigation with it, and navigation is the one thing the dispatcher does not carry.
           |      var __act=(__tg==="BUTTON")?($jsButtonActivation)
           |               :(__tg==="A")?(__ck&&($jsLinkActivation))
           |               :(__tg==="INPUT")?(__inf&&($jsImplicitSubmit))
           |               :false;
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
           |    // kyo-ui treats Enter on Select as a dropdown interaction (see EventDispatch.dispatchToElement),
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
           |    if(he(el,"keydown")){var ktid=e.target&&e.target.id?e.target.id:null;post({KeyDown:{path:p,keyboard:mkKbd(e.key,{ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey},ktid,e.target===el)}});}
           |  }
           |  else if(t==="keyup"&&he(el,"keyup")){var kutid=e.target&&e.target.id?e.target.id:null;post({KeyUp:{path:p,keyboard:mkKbd(e.key,{ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey},kutid)}});}
           |  else if(t==="focus"&&he(el,"focus")){var ftid=e.target&&e.target.id?e.target.id:null;post({Focus:{path:p,mouse:mkMouse({ctrl:false,alt:false,shift:false,meta:false},ftid)}});}
           |  // blurtext: a text field's blur carries the text it holds as focus leaves it, read off the field itself.
           |  else if(t==="blur"&&he(el,"blur")){var btid=e.target&&e.target.id?e.target.id:null;var bl={path:p,mouse:mkMouse({ctrl:false,alt:false,shift:false,meta:false},btid)};var bt=e.target;if(bt===el&&he(el,"blurtext")&&bt.tagName&&(bt.tagName==="INPUT"||bt.tagName==="TEXTAREA"))bl.value=bt.value;post({Blur:bl});}
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
            case _: Svg.G      =>
            case _: Svg.Defs   =>
            case _: Svg.Symbol =>
                s.viewBox.foreach(v => svgAttr(sb, "viewBox", viewBox(v)))
            case _: Svg.Switch    =>
            case _: Svg.Metadata  =>
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
        case Svg.Transform.Translate(x, y)     => s"translate(${fmtD(x)} ${fmtD(y)})"
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
        case Svg.PathCommand.MoveTo(x, y)                      => s"M${fmtD(x)} ${fmtD(y)}"
        case Svg.PathCommand.MoveBy(dx, dy)                    => s"m${fmtD(dx)} ${fmtD(dy)}"
        case Svg.PathCommand.LineTo(x, y)                      => s"L${fmtD(x)} ${fmtD(y)}"
        case Svg.PathCommand.LineBy(dx, dy)                    => s"l${fmtD(dx)} ${fmtD(dy)}"
        case Svg.PathCommand.HLineTo(x)                        => s"H${fmtD(x)}"
        case Svg.PathCommand.HLineBy(dx)                       => s"h${fmtD(dx)}"
        case Svg.PathCommand.VLineTo(y)                        => s"V${fmtD(y)}"
        case Svg.PathCommand.VLineBy(dy)                       => s"v${fmtD(dy)}"
        case Svg.PathCommand.CubicTo(c1x, c1y, c2x, c2y, x, y) =>
            s"C${fmtD(c1x)} ${fmtD(c1y)} ${fmtD(c2x)} ${fmtD(c2y)} ${fmtD(x)} ${fmtD(y)}"
        case Svg.PathCommand.CubicBy(c1x, c1y, c2x, c2y, dx, dy) =>
            s"c${fmtD(c1x)} ${fmtD(c1y)} ${fmtD(c2x)} ${fmtD(c2y)} ${fmtD(dx)} ${fmtD(dy)}"
        case Svg.PathCommand.SmoothCubicTo(c2x, c2y, x, y) =>
            s"S${fmtD(c2x)} ${fmtD(c2y)} ${fmtD(x)} ${fmtD(y)}"
        case Svg.PathCommand.SmoothCubicBy(c2x, c2y, dx, dy) =>
            s"s${fmtD(c2x)} ${fmtD(c2y)} ${fmtD(dx)} ${fmtD(dy)}"
        case Svg.PathCommand.QuadTo(cx, cy, x, y)                       => s"Q${fmtD(cx)} ${fmtD(cy)} ${fmtD(x)} ${fmtD(y)}"
        case Svg.PathCommand.QuadBy(cx, cy, dx, dy)                     => s"q${fmtD(cx)} ${fmtD(cy)} ${fmtD(dx)} ${fmtD(dy)}"
        case Svg.PathCommand.SmoothQuadTo(x, y)                         => s"T${fmtD(x)} ${fmtD(y)}"
        case Svg.PathCommand.SmoothQuadBy(dx, dy)                       => s"t${fmtD(dx)} ${fmtD(dy)}"
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
