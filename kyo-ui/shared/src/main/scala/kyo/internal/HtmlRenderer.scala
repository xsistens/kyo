package kyo.internal

import kyo.*
import kyo.Svg
import kyo.UI.*
import kyo.UI.Ast.*

private[kyo] object HtmlRenderer:

    /** Render a UI tree to HTML with data-kyo-path attributes. */
    def render(ui: UI, path: Seq[String])(using Frame): String < Sync =
        val sb = new StringBuilder
        renderTo(sb, ui, path).andThen(sb.toString)

    /** Wrap a reactive region's inner HTML in the appropriate placeholder tag.
      *
      * Used by the server transport layer when pushing an updated reactive region to the client.
      * Extracts the tag-choice logic (g in SVG context, span in HTML context) into a shared, testable
      * function so tests can assert on the real production wrap rather than reconstructing the string
      * themselves.
      *
      * @param path
      *   the region's path segments (joined with "." for the data-kyo-path attribute)
      * @param svgContext
      *   true if the reactive region lives inside SVG (uses "g" tag), false for HTML (uses "span")
      * @param innerHtml
      *   the rendered inner content to wrap
      * @return
      *   the full placeholder HTML string with data-kyo-path and data-kyo-reactive attributes
      */
    private[kyo] def wrapReactiveRegion(
        path: Seq[String],
        svgContext: Boolean,
        innerHtml: String,
        mount: Boolean = false
    ): String =
        val tag = if svgContext then "g" else "span"
        // A keyless mount's content paint flags its root with data-kyo-mount so a parent region's __kyoMorph treats it
        // as opaque and never wipes the live subtree. Only ever added to a per-region Replace op (never to the initial
        // full-page render, which does not go through this method), so golden HTML stays byte-identical.
        val mountAttr = if mount then " data-kyo-mount" else ""
        s"""<$tag data-kyo-path="${path.mkString(".")}" data-kyo-reactive$mountAttr>$innerHtml</$tag>"""
    end wrapReactiveRegion

    /** Wrap body HTML in a full page with inline JS client. */
    def renderPage(title: String, body: String, css: String, basePath: String): String =
        s"""<!DOCTYPE html>
           |<html>
           |<head>
           |<meta charset="UTF-8">
           |<title>${esc(title)}</title>
           |<style>$baseCss$css</style>
           |</head>
           |<body>$body
           |<script>${clientJs(jsStr(basePath))}</script>
           |</body>
           |</html>""".stripMargin

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

    private def renderTo(sb: StringBuilder, ui: UI, path: Seq[String], svg: Boolean = false)(using Frame): Unit < Sync =
        ui match
            case dd: Dropdown =>
                renderDropdown(sb, dd, path)
            case elem: Element =>
                val tag  = tagName(elem)
                val void = elem.isInstanceOf[Void]
                for
                    // Fold declaratively-bound reactive classes that are CURRENTLY true into the rendered class
                    // list (effectful: reads each Signal). Empty for elements without reactive classes, so the
                    // `class` attribute is byte-identical there; the client toggles them in place afterwards.
                    extraClasses <- reactiveTrueClasses(elem.attrs)
                    _ = w(sb, s"""<$tag data-kyo-path="${pathAttr(path)}"""")
                    _ = renderCommonAttrs(
                        sb,
                        if extraClasses.isEmpty then elem.attrs
                        else elem.attrs.copy(cssClasses = elem.attrs.cssClasses ++ extraClasses)
                    )
                    _ = renderEventAttr(sb, elem)
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
                        val childSvg = elem match
                            case _: Svg.ForeignObject => false
                            case _: Svg.SvgElement    => true
                            case _                    => svg
                        val textChild: Unit < Sync = elem match
                            case t: Svg.Title => w(sb, esc(t.text)); Kyo.unit
                            case d: Svg.Desc  => w(sb, esc(d.text)); Kyo.unit
                            case _            => Kyo.unit
                        textChild.andThen(
                            Kyo.foreachDiscard(elem.children.toSeq.zipWithIndex) { (child, i) =>
                                renderTo(sb, child, path :+ i.toString, childSvg)
                            }.andThen(w(sb, s"</$tag>"))
                        )
                    end if
                end for

            case UI.Ast.RawHtml(value) =>
                w(sb, value)

            case UI.Ast.Text(value) =>
                w(sb, esc(value))

            case Fragment(children) =>
                // Use key for KeyedChild, index for everything else. This matches the path scheme
                // walkStatic uses, so server-side event routing aligns with rendered data-kyo-path.
                Kyo.foreachDiscard(children.toSeq.zipWithIndex) { (child, i) =>
                    val childPath = child match
                        case kc: KeyedChild[?] => path :+ kc.key
                        case _                 => path :+ i.toString
                    renderTo(sb, child, childPath, svg)
                }

            case KeyedChild(_, child) =>
                renderTo(sb, child, path, svg)

            case r: Reactive[?] =>
                // In SVG context emit a <g> placeholder (a <span> is invalid inside <svg>); the
                // closing tag matches the opening. Both carry the same data-kyo-reactive anchor.
                val tag = if svg then "g" else "span"
                w(sb, s"""<$tag data-kyo-path="${pathAttr(path)}" data-kyo-reactive>""")
                for current <- r.signal.current(using r.frame)
                yield renderTo(sb, current, contentPath(path, current), svg).andThen(w(sb, s"</$tag>"))

            case fe: Foreach[?, ?] @unchecked =>
                val tag = if svg then "g" else "span"
                w(sb, s"""<$tag data-kyo-path="${pathAttr(path)}" data-kyo-reactive>""")
                fe.applyTyped {
                    [T] =>
                        (signal, keyFn, renderFn) =>
                            for items <- signal.current(using fe.frame)
                            yield Kyo.foreachDiscard(items.toSeq.zipWithIndex) { (item, i) =>
                                val key = keyFn match
                                    case Present(f) => f(item)
                                    case Absent     => i.toString
                                renderTo(sb, renderFn(i, item), path :+ key, svg)
                            }.andThen(w(sb, s"</$tag>"))
                            end for
                }

            case m: Mounted =>
                // A mount renders as an ordinary reactive boundary whose initial content is the placeholder:
                // the subscription (ReactiveUI.subscribeMounted) patches the region as soon as the node's cell
                // publishes — for an adopted keyed instance, synchronously within the same render pass, so the
                // placeholder never reaches a paint. In a static/SSG render (no subscription) the placeholder IS
                // the node's server-side projection.
                val tag = if svg then "g" else "span"
                w(sb, s"""<$tag data-kyo-path="${pathAttr(path)}" data-kyo-reactive>""")
                val mph = m.placeholderUI.getOrElse(UI.empty(using m.frame))
                renderTo(sb, mph, contentPath(path, mph), svg)
                    .andThen(w(sb, s"</$tag>"))

            case b: Boundary =>
                // A boundary paints its fallback first; the subscription (ReactiveUI.subscribeBoundary) repaints
                // the wrapper with the child from current signal values on reveal — immediately, within the same
                // task chain, when the eager child walk started nothing pending. SSG projection = fallback.
                val tag = if svg then "g" else "span"
                w(sb, s"""<$tag data-kyo-path="${pathAttr(path)}" data-kyo-reactive>""")
                val bfb = b.fallbackUI.getOrElse(UI.empty(using b.frame))
                renderTo(sb, bfb, contentPath(path, bfb), svg)
                    .andThen(w(sb, s"</$tag>"))
    end renderTo

    private def renderTextareaValue(sb: StringBuilder, ta: Textarea)(using Frame): Unit < Sync =
        ta.value match
            case Present(Bound.Const(s)) => w(sb, esc(s))
            case Present(Bound.Ref(ref)) =>
                for str <- ref.get
                yield w(sb, esc(str))
            case _ => ()

    // ---- Dropdown (custom div-based overlay) ----

    private def renderDropdown(sb: StringBuilder, dd: Dropdown, path: Seq[String])(using Frame): Unit < Sync =
        val baseId = dd.attrs.identifier.getOrElse("")
        // Read current selected value for initial highlight
        val currentValueEffect: Unit < Sync = dd.value match
            case Present(Bound.Ref(ref)) =>
                ref.get.map { currentVal =>
                    renderDropdownWithValue(sb, dd, path, baseId, currentVal)
                }
            case Present(Bound.Const(s)) =>
                renderDropdownWithValue(sb, dd, path, baseId, s)
            case _ =>
                renderDropdownWithValue(sb, dd, path, baseId, "")
        currentValueEffect
    end renderDropdown

    private def renderDropdownWithValue(sb: StringBuilder, dd: Dropdown, path: Seq[String], baseId: String, currentVal: String)(using
        Frame
    ): Unit =
        val pathStr   = pathAttr(path)
        val idAttr    = if baseId.nonEmpty then s""" id="${esc(baseId)}"""" else ""
        val ddAttr    = if baseId.nonEmpty then s""" data-kyo-dropdown="${esc(baseId)}"""" else " data-kyo-dropdown"
        val disAttr   = if dd.disabled.getOrElse(false) then " data-kyo-disabled" else ""
        val hidAttr   = if dd.attrs.hidden.getOrElse(false) then " hidden" else ""
        val tabAttr   = dd.attrs.tabIndex.map(n => s""" tabindex="$n"""").getOrElse("")
        val styleAttr = CssStyleRenderer.render(dd.attrs.uiStyle)
        val styleStr  = if styleAttr.nonEmpty then s""" style="${esc(styleAttr)}"""" else ""
        // The dropdown wrapper carries its cssClasses (the same `.cssClass(...)` hook every other
        // element honors) so callers can style the trigger container via a class selector.
        val clsStr = if dd.attrs.cssClasses.nonEmpty then s""" class="${esc(dd.attrs.cssClasses.mkString(" "))}"""" else ""
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

    private def renderCommonAttrs(sb: StringBuilder, attrs: Attrs): Unit =
        attrs.identifier.foreach(id => w(sb, s""" id="${esc(id)}""""))
        if attrs.cssClasses.nonEmpty then w(sb, s""" class="${esc(attrs.cssClasses.mkString(" "))}"""")
        attrs.hidden.foreach(v => if v then w(sb, " hidden"))
        attrs.tabIndex.foreach(n => w(sb, s""" tabindex="$n""""))
        attrs.focusTrap.foreach(v => if v then w(sb, """ data-kyo-focus-trap="1""""))
        attrs.focusGroup.foreach(id => w(sb, s""" data-kyo-focus-group="${esc(id)}""""))
        attrs.focusAuto.foreach(v => if v then w(sb, """ data-kyo-focus-auto="1""""))
        attrs.focusRestore.foreach(v => if v then w(sb, """ data-kyo-focus-restore="1""""))
        // Marker only (inspection/tooling): the stop-propagation semantics live server-side in
        // ReactiveUI.dispatchToElement, which reads the flag from the AST — the client never branches on this.
        attrs.stopPropagation.foreach(v => if v then w(sb, """ data-kyo-stop="1""""))
        // Unit 1: enter/leave transition class lists (read client-side by the patch-application code).
        attrs.enterTransition.foreach(c => w(sb, s""" data-kyo-enter="${esc(c)}""""))
        attrs.leaveTransition.foreach(c => w(sb, s""" data-kyo-leave="${esc(c)}""""))
        val css = CssStyleRenderer.render(attrs.uiStyle)
        if css.nonEmpty then w(sb, s""" style="${esc(css)}"""")
        attrs.ariaAttrs.toSeq.sortBy(_._1).foreach { case (name, value) =>
            w(sb, s""" aria-$name="${esc(value)}"""")
        }
        attrs.role.foreach(r => w(sb, s""" role="${esc(r)}""""))
        attrs.dataAttrs.toSeq.sortBy(_._1).foreach { case (name, value) =>
            w(sb, s""" data-$name="${esc(value)}"""")
        }
        attrs.jsProps.toSeq.sortBy(_._1).foreach { case (name, value) =>
            w(sb, s""" data-kyo-prop-$name="${esc(value)}"""")
        }
        // Unit 3: input filter/mask patterns (read by the client-local beforeinput listener).
        attrs.inputFilter.foreach(p => w(sb, s""" data-kyo-filter="${esc(p)}""""))
        attrs.inputMask.foreach(m => w(sb, s""" data-kyo-mask="${esc(m)}""""))
    end renderCommonAttrs

    // ---- Element-specific attributes ----

    /** Emits the CURRENT value of each declaratively-bound reactive attribute (Attrs.reactiveAttrs) as an
      * ordinary `name="value"` attribute, so SSR / initial mount carries the correct value; the client then
      * patches it in place on subsequent emissions (HtmlOp.SetAttrByPath). Effectful — reads each Signal's
      * current value (same as the reactive-content region renderer). Sorted for deterministic output.
      */
    private def renderReactiveAttrs(sb: StringBuilder, attrs: Attrs)(using Frame): Unit < Sync =
        Kyo.foreachDiscard(attrs.reactiveAttrs.toSeq.sortBy(_._1)) { case (name, sig) =>
            sig.current.map(v => w(sb, s""" $name="${esc(v)}""""))
        }

    /** Emits each declaratively-bound reactive BOOLEAN attribute (Attrs.reactiveBoolAttrs) as a bare present
      * attribute while its signal's current value is true (mirrors `boolAttr`); the client toggles it in place
      * on subsequent emissions (HtmlOp.SetBoolAttrByPath). Sorted for deterministic output.
      */
    private def renderReactiveBoolAttrs(sb: StringBuilder, attrs: Attrs)(using Frame): Unit < Sync =
        Kyo.foreachDiscard(attrs.reactiveBoolAttrs.toSeq.sortBy(_._1)) { case (name, sig) =>
            sig.current.map(v => if v then w(sb, s" $name"))
        }

    /** The declaratively-bound reactive classes (Attrs.reactiveClasses) whose signal is CURRENTLY true — folded
      * into the element's rendered class list so SSR / initial mount is correct; the client toggles them in
      * place on subsequent emissions (HtmlOp.SetClassByPath). Empty (and cheap) when none are bound.
      */
    private def reactiveTrueClasses(attrs: Attrs)(using Frame): Seq[String] < Sync =
        if attrs.reactiveClasses.isEmpty then Seq.empty
        else
            Kyo.foreach(attrs.reactiveClasses.toSeq.sortBy(_._1)) { case (name, sig) =>
                sig.current.map(v => if v then name else "")
            }.map(_.filter(_.nonEmpty))

    private def renderElementAttrs(sb: StringBuilder, elem: Element)(using Frame): Unit < Sync =
        elem match
            case b: Button =>
                w(sb, " type=\"submit\"")
                boolAttr(sb, "disabled", b.disabled)
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
                renderValueAttr(sb, i.value)
                    .andThen {
                        boolAttr(sb, "disabled", i.disabled); boolAttr(sb, "readonly", i.readOnly);
                        i.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
                    }
            case p: PasswordInput =>
                w(sb, " type=\"password\"");
                renderValueAttr(sb, p.value)
                    .andThen {
                        boolAttr(sb, "disabled", p.disabled); boolAttr(sb, "readonly", p.readOnly);
                        p.placeholder.foreach(p2 => w(sb, s""" placeholder="${esc(p2)}""""))
                    }
            case e: EmailInput =>
                w(sb, " type=\"email\"");
                renderValueAttr(sb, e.value)
                    .andThen {
                        boolAttr(sb, "disabled", e.disabled); boolAttr(sb, "readonly", e.readOnly);
                        e.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
                    }
            case t: TelInput =>
                w(sb, " type=\"tel\"");
                renderValueAttr(sb, t.value)
                    .andThen {
                        boolAttr(sb, "disabled", t.disabled); boolAttr(sb, "readonly", t.readOnly);
                        t.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
                    }
            case u: UrlInput =>
                w(sb, " type=\"url\"");
                renderValueAttr(sb, u.value)
                    .andThen {
                        boolAttr(sb, "disabled", u.disabled); boolAttr(sb, "readonly", u.readOnly);
                        u.placeholder.foreach(p => w(sb, s""" placeholder="${esc(p)}""""))
                    }
            case s: SearchInput =>
                w(sb, " type=\"search\"");
                renderValueAttr(sb, s.value)
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
                a.href.foreach { href =>
                    val value = href match
                        case Href.Absolute(url)       => url.full
                        case Href.Path(p)             => p
                        case Href.Fragment(id)        => s"#$id"
                        case Href.External(scheme, v) => s"$scheme:$v"
                    w(sb, s""" href="${esc(value)}"""")
                }
                a.target.foreach { t =>
                    val tv = t match
                        case Target.Self   => "_self"
                        case Target.Blank  => "_blank"
                        case Target.Parent => "_parent"
                        case Target.Top    => "_top"
                    w(sb, s""" target="$tv"""")
                }
            case img: Img =>
                img.src.foreach { src =>
                    val value = src match
                        case ImgSrc.Absolute(url)       => url.full
                        case ImgSrc.Path(p)             => p
                        case ImgSrc.Data(mime, payload) => s"data:$mime;base64,$payload"
                    w(sb, s""" src="${esc(value)}"""")
                }
                img.alt.foreach(a => w(sb, s""" alt="${esc(a)}""""))
            case f: Iframe =>
                f.src.foreach(s => w(sb, s""" src="${esc(s)}""""))
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
    private def renderValueAttr(sb: StringBuilder, value: Maybe[Bound[String]])(using Frame): Unit < Sync =
        value match
            case Present(Bound.Const(s)) => w(sb, s""" value="${esc(s)}"""")
            case Present(Bound.Ref(ref)) =>
                for s <- ref.get
                yield w(sb, s""" value="${esc(s)}"""")
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
        // Unit 2: fileselect marker so the client's change branch reads all files instead of the legacy single-file path.
        if attrs.onFileSelect.nonEmpty then events += "fileselect"
        // Unit 1: pointer/drag tokens so the client's pointer listeners forward down/move/up for this element's chain.
        if attrs.onPointerDown.nonEmpty then events += "pointerdown"
        if attrs.onPointerMove.nonEmpty then events += "pointermove"
        if attrs.onPointerUp.nonEmpty then events += "pointerup"
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

    /** Reserved path segment for a reactive region whose current value is ITSELF a reactive
      * region (`Reactive`/`Foreach`/`Mounted`/`Boundary`). Nested reactive wrappers used to
      * collapse onto one `data-kyo-path`, so the patcher (first-match `querySelector` / the
      * morph's same-path keying) could only address the OUTERMOST — an inner region could not
      * patch its own node and lost the race. Appending this segment gives each nesting level a
      * distinct, stable wrapper path. It contains no '.' (the path separator) and is not a
      * decimal index, so it cannot collide with Element/Fragment child indices; it only ever
      * appears as the sole content of a reactive wrapper, so it cannot collide with
      * Foreach/KeyedChild keys.
      */
    private[kyo] val reactiveContentSegment: String = "$r"

    /** Path at which a reactive region renders its current value: a distinct sub-path when the
      * value is itself a reactive region, else `path` unchanged (flat content stays
      * byte-identical). SINGLE SOURCE OF TRUTH — `renderTo` (SSR), `ReactiveUI.walkStatic`
      * (client normalize), and both patch exchanges (`DomBackend`/`UIServer` `onChange`) MUST
      * all route content through this, or SSR and client paths diverge and hydration breaks.
      */
    private[kyo] def contentPath(path: Seq[String], current: UI): Seq[String] =
        current match
            case _: Reactive[?] | _: Foreach[?, ?] | _: Mounted | _: Boundary => path :+ reactiveContentSegment
            case _                                                            => path

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
    private def jsStr(s: String): String =
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

    private def clientJs(basePath: String): String =
        s"""(function(){
           |var base="$basePath";
           |var __q=[];
           |// Per-element ownership of specific attributes by the imperative id-addressed channel
           |// (SetClassById/SetStyleById/SetAttrById), applied out of the render pass so CSS transitions fire. The owned
           |// attr names live in a __kyoOwn expando dict ON the element, reclaimed with the node (no session-lived set
           |// that only grows) and shielding each owned attr BY NAME. __kyoMorphAttrs reads it so an owned attribute is
           |// never reconciled back to server HTML (which never carries the client-set value). Mirrors markOwned in DomBackend.
           |function __kyoMark(el,n){(el.__kyoOwn||(el.__kyoOwn={}))[n]=true;}
           |var ws=new WebSocket((location.protocol===\"https:\"?\"wss:\":\"ws:\")+"//"+location.host+base+"/_kyo/ws");
           |ws.onopen=function(){__q.forEach(function(m){ws.send(m);});__q=[];};
           |ws.onmessage=function(e){
           |  var op=JSON.parse(e.data);
           |  if(op.Replace){
           |    var p=op.Replace.path.join(".");
           |    var el=document.querySelector('[data-kyo-path="'+p+'"]');
           |    // A focused input keeps its value and caret across a Replace because __kyoMorph patches in place and
           |    // its active-input rule leaves the live .value untouched on a self-echo (the field already holds that
           |    // value), applying only a genuine external change (submit-clear, programmatic update). So the former
           |    // focused-field fast path here is redundant and has been removed — __kyoMorph handles it uniformly.
           |    if(el&&el.outerHTML!==op.Replace.html){
           |      var ae=document.activeElement;
           |      var ap=ae&&ae!==document.body&&ae.getAttribute?ae.getAttribute("data-kyo-path"):null;
           |      var ss=(ae&&typeof ae.selectionStart==='number')?ae.selectionStart:null;
           |      var se=(ae&&typeof ae.selectionEnd==='number')?ae.selectionEnd:null;
           |      var __fa=faPaths(el);
           |      // Unit 1: capture enter old-set + prepare leave ghosts (rect+clone) BEFORE the patch mutates the DOM.
           |      var __en=faEnterPaths(el);
           |      var __gh=kyoLeavePrepare(el,kyoLeaveSurv(op.Replace.html));
           |      // Morph in place toward the pushed HTML (preserves focus/caret/scroll/transitions on reused nodes);
           |      // __kyoMorph returns the region root, so no re-query for the path is needed.
           |      var nel=__kyoMorph(el,op.Replace.html);
           |      if(nel){applyJsProps(nel);ba(nel);}
           |      if(ap){var rf=lbp(ap);if(rf&&rf.hasAttribute&&rf.hasAttribute('data-kyo-reactive')){var inner=rf.querySelector('input,textarea,select,[contenteditable]');if(inner)rf=inner;}if(rf){rf.focus();if(ss!==null&&typeof rf.setSelectionRange==='function'){try{rf.setSelectionRange(ss,se);}catch(e){if(e.name!=='InvalidStateError')throw e;}}}}
           |      // Focus seeding runs AFTER the focus/caret restore so a newly-appeared focus-auto element wins
           |      // over restore-to-trigger (opening a panel moves focus into it). The morph path above never seeds.
           |      if(nel)faSeed(nel,__fa);
           |      // Unit 1: animate newly-appeared enter elements, then spawn the leave ghosts captured above.
           |      if(nel)kyoEnterSeed(nel,__en);
           |      kyoSpawnGhosts(__gh);
           |    }
           |    frSweep();
           |  }else if(op.Remove){
           |    var p=op.Remove.path.join(".");
           |    var el=document.querySelector('[data-kyo-path="'+p+'"]');
           |    // Unit 1: everything under the removed node leaves; prepare ghosts BEFORE removal, spawn after.
           |    var __rgh=el?kyoLeavePrepare(el,{}):[];
           |    if(el)el.remove();
           |    kyoSpawnGhosts(__rgh);
           |    frSweep();
           |  }else if(op.InjectCss){
           |    var s=document.createElement("style");
           |    s.textContent=op.InjectCss.css;
           |    document.head.appendChild(s);
           |  }else if(op.Command){
           |    // Unit 3: whitelisted imperative command. Unknown verbs are ignored (forward-compat).
           |    kyoApplyVerb(lbp(op.Command.path.join(".")),op.Command.verb);
           |  }else if(op.CommandById){
           |    // Self-addressing: same verb whitelist, but resolve the target by DOM id (getElementById).
           |    kyoApplyVerb(document.getElementById(op.CommandById.id),op.CommandById.verb);
           |  }else if(op.RequestMeasure){
           |    // Unit 2: measure the element and reply with a Measure event (element rect + viewport size).
           |    var rmp=op.RequestMeasure.path.join(".");
           |    var rmel=lbp(rmp);
           |    if(rmel){
           |      var rmr=rmel.getBoundingClientRect();
           |      post({Measure:{path:op.RequestMeasure.path,rectX:rmr.left,rectY:rmr.top,rectW:rmr.width,rectH:rmr.height,viewportW:window.innerWidth,viewportH:window.innerHeight}});
           |    }
           |  }else if(op.RequestMeasureById){
           |    // Self-addressing: measure by DOM id and reply with a MeasureById event carrying the id back (path is
           |    // vestigial for id-routing, sent empty). The server routes it to Commands.deliverMeasureById(id).
           |    var rmiel=document.getElementById(op.RequestMeasureById.id);
           |    if(rmiel){
           |      var rmir=rmiel.getBoundingClientRect();
           |      post({MeasureById:{path:[],id:op.RequestMeasureById.id,rectX:rmir.left,rectY:rmir.top,rectW:rmir.width,rectH:rmir.height,viewportW:window.innerWidth,viewportH:window.innerHeight}});
           |    }
           |  }else if(op.SetClassById){
           |    // Unit 8: toggle a class in place (element stays in the DOM, so CSS transitions on the class fire).
           |    var scel=document.getElementById(op.SetClassById.id);if(scel){__kyoMark(scel,"class");scel.classList.toggle(op.SetClassById.className,op.SetClassById.on);}
           |  }else if(op.SetStyleById){
           |    // Unit 8: merge inline-style declarations (setProperty each) so other inline props are not clobbered.
           |    var ssel=document.getElementById(op.SetStyleById.id);
           |    if(ssel){__kyoMark(ssel,"style");var ssd=op.SetStyleById.css.split(";");for(var ssi=0;ssi<ssd.length;ssi++){var ssc=ssd[ssi].trim();if(!ssc)continue;var sso=ssc.indexOf(":");if(sso>0)ssel.style.setProperty(ssc.substring(0,sso).trim(),ssc.substring(sso+1).trim());}}
           |  }else if(op.SetAttrById){
           |    // Unit 8: set an attribute in place (element stays put, so a CSS `>` anchored on it keeps matching).
           |    var sael=document.getElementById(op.SetAttrById.id);if(sael){__kyoMark(sael,op.SetAttrById.name);sael.setAttribute(op.SetAttrById.name,op.SetAttrById.value);}
           |  }else if(op.SetAttrByPath){
           |    // Declarative path-addressed twin of SetAttrById: patch an attribute in place on the element at data-kyo-path.
           |    var sapp=op.SetAttrByPath.path.join(".");var sapel=document.querySelector('[data-kyo-path="'+sapp+'"]:not([data-kyo-reactive])')||document.querySelector('[data-kyo-path="'+sapp+'"]');if(sapel){__kyoMark(sapel,op.SetAttrByPath.name);sapel.setAttribute(op.SetAttrByPath.name,op.SetAttrByPath.value);}
           |  }else if(op.SetBoolAttrByPath){
           |    // Presence-based boolean twin: toggle the attribute in place (present when true) on the element at data-kyo-path.
           |    var sbpp=op.SetBoolAttrByPath.path.join(".");var sbpel=document.querySelector('[data-kyo-path="'+sbpp+'"]:not([data-kyo-reactive])')||document.querySelector('[data-kyo-path="'+sbpp+'"]');if(sbpel){__kyoMark(sbpel,op.SetBoolAttrByPath.name);if(op.SetBoolAttrByPath.value){sbpel.setAttribute(op.SetBoolAttrByPath.name,'');}else{sbpel.removeAttribute(op.SetBoolAttrByPath.name);}}
           |  }else if(op.SetClassByPath){
           |    // Path-addressed reactive class: toggle in place so CSS transitions fire; own "class" so a morph won't reconcile it.
           |    var scpp=op.SetClassByPath.path.join(".");var scpel=document.querySelector('[data-kyo-path="'+scpp+'"]:not([data-kyo-reactive])')||document.querySelector('[data-kyo-path="'+scpp+'"]');if(scpel){__kyoMark(scpel,'class');scpel.classList.toggle(op.SetClassByPath.name,op.SetClassByPath.on);}
           |  }else if(op.ObserveViewportById){
           |    // Unit 8: measure now + attach capturing scroll + resize listeners that each reply with a MeasureById(id).
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
           |    // Unit 8: detach the scroll/resize listeners registered for this id.
           |    var uid=op.UnobserveViewportById.id;
           |    if(window.__kyoVpObs&&window.__kyoVpObs[uid]){
           |      var uh=window.__kyoVpObs[uid];
           |      window.removeEventListener("scroll",uh,true);
           |      window.removeEventListener("resize",uh);
           |      delete window.__kyoVpObs[uid];
           |    }
           |  }
           |};
           |// Self-addressing: shared whitelist for Command / CommandById. Unknown verbs are ignored (forward-compat).
           |function kyoApplyVerb(el,verb){
           |  if(!el)return;
           |  if(verb==="focus"){var fs='input,textarea,select,button,a[href],[tabindex],[contenteditable]';var ft=(el.matches&&el.matches(fs))?el:(el.querySelector?el.querySelector(fs):null);if(ft&&typeof ft.focus==="function")ft.focus();}
           |  else if(verb==="scrollIntoView"){if(typeof el.scrollIntoView==="function")el.scrollIntoView({block:"nearest"});}
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
           |    n=n.parentElement;
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
           |function applyJsProps(root){
           |  var pfx="data-kyo-prop-";
           |  var els=root.querySelectorAll("[data-kyo-prop-indeterminate],[data-kyo-prop-checked]");
           |  var list=[];
           |  if(root.hasAttribute&&root.getAttribute){
           |    var an=root.getAttributeNames?root.getAttributeNames():[];
           |    for(var i=0;i<an.length;i++){if(an[i].indexOf(pfx)===0){list.push(root);break;}}
           |  }
           |  for(var i=0;i<els.length;i++)list.push(els[i]);
           |  for(var j=0;j<list.length;j++){
           |    var el=list[j];
           |    var names=el.getAttributeNames?el.getAttributeNames():[];
           |    var rem=[];
           |    for(var k=0;k<names.length;k++){
           |      var n=names[k];
           |      if(n.indexOf(pfx)===0){el[n.slice(pfx.length)]=el.getAttribute(n);rem.push(n);}
           |    }
           |    for(var k=0;k<rem.length;k++)el.removeAttribute(rem[k]);
           |  }
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
           |// Focus seeding + restore for [data-kyo-focus-auto]/[data-kyo-focus-restore]. __frs is a stack of
           |// {fa: seeded element path, ret: previously focused element path or null, restore: focus-restore flag}.
           |// Mirrors DomBackend.focusReturnStack for the SPA transport.
           |var __frs=[];
           |// Set (object keyed by path) of every [data-kyo-focus-auto] path inside root, root itself included.
           |function faPaths(root){
           |  var s={};
           |  if(!root||!root.querySelectorAll)return s;
           |  if(root.hasAttribute&&root.hasAttribute("data-kyo-focus-auto")){var rp=root.getAttribute("data-kyo-path");if(rp!==null)s[rp]=true;}
           |  var els=root.querySelectorAll("[data-kyo-focus-auto]");
           |  for(var i=0;i<els.length;i++){var ep=els[i].getAttribute("data-kyo-path");if(ep!==null)s[ep]=true;}
           |  return s;
           |}
           |// Seed the FIRST focus-auto element under newRoot whose path was NOT in oldSet (i.e. it newly appeared).
           |// Records the previously focused element's path so frSweep can return focus when the seeded element leaves.
           |function faSeed(newRoot,oldSet){
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
           |      __frs.push({fa:fa,ret:ret,restore:cand[j].hasAttribute("data-kyo-focus-restore")});
           |      if(typeof cand[j].focus==='function')cand[j].focus();
           |      return;
           |    }
           |  }
           |}
           |// Locate the element at a path, preferring a non-wrapper match: reactive wrapper spans share their
           |// data-kyo-path with the first element they wrap, so a plain first-match query can return the
           |// transparent (unfocusable) wrapper instead of the real element.
           |function lbp(p){return document.querySelector('[data-kyo-path="'+p+'"]:not([data-kyo-reactive])')||document.querySelector('[data-kyo-path="'+p+'"]');}
           |// Pop stack entries whose seeded element left the document; focus the recorded previous element when
           |// the entry carried data-kyo-focus-restore (skip silently if that element is gone too).
           |function frSweep(){
           |  while(__frs.length>0){
           |    var top=__frs[__frs.length-1];
           |    if(document.querySelector('[data-kyo-path="'+top.fa+'"][data-kyo-focus-auto]'))return;
           |    __frs.pop();
           |    if(top.restore&&top.ret){var re=lbp(top.ret);if(re&&typeof re.focus==='function')re.focus();}
           |  }
           |}
           |// ---- Unit 1: enter/leave transition helpers (client-local; nothing crosses the wire) ----
           |// Set of data-kyo-enter paths under root (root included) — the enter elements present BEFORE a patch.
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
           |// Keyed on leave-carrying elements, NOT all data-kyo-path: a reactive wrapper span shares its path with
           |// the (leaving) element it wraps, so an all-path set would wrongly report the element as surviving.
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
           |// ---- DOM morphing (twin of DomBackend.morph; keep the two in lockstep) ----
           |// Patch an existing subtree in place toward new HTML instead of el.outerHTML= replacing it, so focus, caret,
           |// scroll offset and in-flight transitions on reused nodes survive a server-pushed Replace. Reconciliation is
           |// SIBLING-SCOPED and keyed on data-kyo-path (unique among siblings; the wrapper/inner nesting repeats it, so
           |// the keyed lookup is per child list, never a global id set). Objects use Object.create(null) so a path that
           |// equals a prototype name (e.g. "constructor") cannot false-match.
           |function __kyoKey(n){return (n.nodeType===1&&n.hasAttribute("data-kyo-path"))?n.getAttribute("data-kyo-path"):null;}
           |function __kyoIsMount(n){return n.nodeType===1&&n.hasAttribute("data-kyo-mount");}
           |function __kyoFirstEl(p){var c=p.firstChild;while(c&&c.nodeType!==1)c=c.nextSibling;return c;}
           |function __kyoHasElChild(el){var c=el.firstChild;while(c){if(c.nodeType===1)return true;c=c.nextSibling;}return false;}
           |function __kyoOnlyElChild(el){var f=null,n=0,c=el.firstChild;while(c){if(c.nodeType===1){n++;f=c;}c=c.nextSibling;}return n===1?f:null;}
           |// Collapse a chain of transparent same-path reactive wrappers to one (nested reactive regions at one position
           |// each emit a wrapper at the same path, and a paint carries only its own depth; left as-is a shallow paint
           |// morphed against a deep live subtree forces a tag mismatch that wipes the subtree). A mount wrapper is never
           |// collapsed (its marker is load-bearing).
           |function __kyoCollapse(el){
           |  if(el.hasAttribute("data-kyo-reactive")&&!el.hasAttribute("data-kyo-mount")){
           |    var p=el.getAttribute("data-kyo-path"),cont=true;
           |    while(cont){
           |      var ch=__kyoOnlyElChild(el);
           |      if(ch&&ch.hasAttribute("data-kyo-reactive")&&ch.getAttribute("data-kyo-path")===p&&!ch.hasAttribute("data-kyo-mount")){
           |        while(ch.firstChild)el.insertBefore(ch.firstChild,ch);
           |        el.removeChild(ch);
           |      }else cont=false;
           |    }
           |  }
           |  var c=el.firstChild;while(c){var nx=c.nextSibling;if(c.nodeType===1)__kyoCollapse(c);c=nx;}
           |}
           |function __kyoParseRoot(fromEl,html){
           |  var t=document.createElement("template");
           |  var svg=fromEl.namespaceURI==="http://www.w3.org/2000/svg"&&fromEl.tagName.toLowerCase()!=="svg";
           |  var root;
           |  if(svg){t.innerHTML="<svg>"+html+"</svg>";var o=__kyoFirstEl(t.content);root=o?__kyoFirstEl(o):null;}
           |  else{t.innerHTML=html;root=__kyoFirstEl(t.content);}
           |  if(root)__kyoCollapse(root);
           |  return root;
           |}
           |function __kyoCompat(a,b){return a.nodeType===b.nodeType&&(a.nodeType!==1||a.tagName===b.tagName);}
           |function __kyoMorphAttrs(fromEl,toEl){
           |  var own=fromEl.__kyoOwn;
           |  var tag=fromEl.tagName;
           |  var activeInput=(fromEl===document.activeElement)&&(tag==="INPUT"||tag==="TEXTAREA");
           |  var ta=toEl.attributes,i;
           |  for(i=0;i<ta.length;i++){var a=ta[i];if(!(own&&own[a.name])&&fromEl.getAttribute(a.name)!==a.value)fromEl.setAttribute(a.name,a.value);}
           |  var fa=fromEl.attributes,j;
           |  for(j=fa.length-1;j>=0;j--){var fn=fa[j].name;if(!(own&&own[fn])&&!toEl.hasAttribute(fn))fromEl.removeAttribute(fn);}
           |  if(activeInput){var nv=(tag==="TEXTAREA")?toEl.textContent:(toEl.getAttribute("value")||"");if(nv!==fromEl.value)fromEl.value=nv;}
           |}
           |function __kyoMorphChildren(fromParent,toParent){
           |  var fromKeyed=null,toKeyed=null,scan=fromParent.firstChild;
           |  while(scan){var k=__kyoKey(scan);if(k!==null){if(!fromKeyed)fromKeyed=Object.create(null);fromKeyed[k]=scan;}scan=scan.nextSibling;}
           |  scan=toParent.firstChild;
           |  while(scan){var k2=__kyoKey(scan);if(k2!==null){if(!toKeyed)toKeyed=Object.create(null);toKeyed[k2]=true;}scan=scan.nextSibling;}
           |  var curFrom=fromParent.firstChild,curTo=toParent.firstChild;
           |  while(curTo){
           |    var toNext=curTo.nextSibling,tKey=__kyoKey(curTo);
           |    if(tKey!==null){
           |      var m=(fromKeyed&&fromKeyed[tKey])?fromKeyed[tKey]:null;
           |      if(m){
           |        if(m!==curFrom)fromParent.insertBefore(m,curFrom);else curFrom=curFrom.nextSibling;
           |        // Opaque mount boundary: while the top-down slot still carries content, leave the live mount root's
           |        // subtree alone (it owns and repaints itself); an EMPTY slot means the gate closed, so morph it out.
           |        if(!(__kyoIsMount(m)&&__kyoHasElChild(curTo)))__kyoMorphNode(m,curTo);
           |      }else fromParent.insertBefore(document.importNode(curTo,true),curFrom);
           |    }else{
           |      var handled=false,loop=true;
           |      while(loop&&curFrom){
           |        var fNext=curFrom.nextSibling,fKey=__kyoKey(curFrom);
           |        if(fKey!==null){
           |          if(!toKeyed||!toKeyed[fKey])fromParent.removeChild(curFrom);
           |          curFrom=fNext;
           |        }else if(__kyoCompat(curFrom,curTo)){__kyoMorphNode(curFrom,curTo);curFrom=fNext;handled=true;loop=false;}
           |        else{fromParent.removeChild(curFrom);curFrom=fNext;}
           |      }
           |      if(!handled)fromParent.insertBefore(document.importNode(curTo,true),curFrom);
           |    }
           |    curTo=toNext;
           |  }
           |  while(curFrom){var fN=curFrom.nextSibling;fromParent.removeChild(curFrom);curFrom=fN;}
           |}
           |function __kyoMorphEl(fromEl,toEl){
           |  __kyoMorphAttrs(fromEl,toEl);
           |  var editing=(fromEl===document.activeElement)&&fromEl.hasAttribute("contenteditable");
           |  if(!editing)__kyoMorphChildren(fromEl,toEl);
           |}
           |function __kyoMorphNode(fromNode,toNode){
           |  if(toNode.nodeType!==1){if(fromNode.nodeValue!==toNode.nodeValue)fromNode.nodeValue=toNode.nodeValue;}
           |  else if(fromNode.tagName!==toNode.tagName)fromNode.parentNode.replaceChild(document.importNode(toNode,true),fromNode);
           |  else __kyoMorphEl(fromNode,toNode);
           |}
           |// Returns the region root (fromEl when identity is preserved, or the replacement on a root-tag change), so
           |// the caller reuses it instead of re-querying the document for the path.
           |function __kyoMorph(fromEl,newHtml){
           |  var toEl=__kyoParseRoot(fromEl,newHtml);
           |  if(!toEl)return fromEl;
           |  __kyoCollapse(fromEl);
           |  if(fromEl.tagName!==toEl.tagName){var rep=document.importNode(toEl,true);fromEl.parentNode.replaceChild(rep,fromEl);return rep;}
           |  __kyoMorphEl(fromEl,toEl);return fromEl;
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
           |    g.style.width=rect.width+"px";g.style.height=rect.height+"px";
           |    // Neutralize any %-based min/max size the clone's classes carry (e.g. an
           |    // overlay panel's min-width:100%) — at <body> it resolves to the viewport
           |    // and would OVERRIDE the pinned width, ballooning the ghost full-width.
           |    g.style.minWidth="0";g.style.maxWidth="none";g.style.minHeight="0";g.style.maxHeight="none";
           |    g.style.margin="0";g.style.pointerEvents="none";
           |    g.setAttribute("data-kyo-ghost","1");
           |    ghosts.push({node:g,leave:leave});
           |  }
           |  return ghosts;
           |}
           |// Append prepared ghosts to <body>, add their leave classes next frame, remove on transitionend/animationend or a 1s safety.
           |function kyoSpawnGhosts(ghosts){
           |  if(!ghosts)return;
           |  for(var i=0;i<ghosts.length;i++){(function(gh){
           |    var g=gh.node;document.body.appendChild(g);
           |    var cls=(gh.leave||"").split(/\\s+/);
           |    requestAnimationFrame(function(){for(var c=0;c<cls.length;c++){if(cls[c])g.classList.add(cls[c]);}});
           |    var done=false;
           |    function cleanup(){if(done)return;done=true;if(g.parentNode)g.parentNode.removeChild(g);}
           |    g.addEventListener("transitionend",cleanup);g.addEventListener("animationend",cleanup);
           |    setTimeout(cleanup,1000);
           |  })(ghosts[i]);}
           |}
           |applyJsProps(document.body);ba(document.body);
           |// Initial-mount seeding: the page arrived server-rendered, so any focus-auto element already present
           |// is NEW by definition (old set empty) — consistent with native autofocus.
           |faSeed(document.body,{});
           |// Unit 1: initial-mount enter seeding — any data-kyo-enter element present at load animates in (old set empty).
           |kyoEnterSeed(document.body,{});
           |// Dropdown helpers: close all dropdowns except the given id
           |function kyoCloseDropdown(exceptId){
           |  var all=document.querySelectorAll('[data-kyo-dropdown-options]');
           |  Array.prototype.forEach.call(all,function(el){
           |    var id=el.getAttribute('data-kyo-dropdown-options');
           |    if(id!==exceptId)el.hidden=true;
           |  });
           |}
           |// Build a mouse payload, omitting targetId when absent (null JSON would break Maybe[String] decode).
           |function mkMouse(mods,tid){var m={modifiers:mods};if(tid)m.targetId=tid;return m;}
           |// Build a keyboard payload, omitting targetId when absent.
           |function mkKbd(key,mods,tid){var k={key:key,modifiers:mods};if(tid)k.targetId=tid;return k;}
           |// Native scroll POSITION (onScrollPosition): rAF-coalesce bursts to one post per frame. Scroll does not
           |// bubble, but the capture-phase body listener catches descendant viewport scrolls.
           |var __scrRaf=0,__scrEl=null,__scrPath=null;
           |function __scrFlush(){__scrRaf=0;if(__scrEl){var stid=__scrEl.id?__scrEl.id:null;var sp={path:__scrPath,scrollTop:__scrEl.scrollTop,scrollLeft:__scrEl.scrollLeft};if(stid)sp.targetId=stid;post({ScrollPosition:sp});}}
           |function handle(e){
           |  var el=fp(e.target);
           |  if(!el)return;
           |  var p=pa(el),t=e.type;
           |  if(t==="click"){
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
           |    var mid=e.target&&e.target.id?e.target.id:null;if(el.tagName&&el.tagName.toLowerCase()==='a')e.preventDefault();post({Click:{path:p,mouse:mkMouse({ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey},mid)}});window._kyoClickSubmit=true;setTimeout(function(){window._kyoClickSubmit=false},0);
           |  }
           |  // Right-click: only when an ancestor declared a contextmenu handler. preventDefault suppresses the
           |  // native browser menu ONLY in that case — a bare right-click elsewhere keeps native behavior.
           |  else if(t==="contextmenu"&&he(el,"contextmenu")){e.preventDefault();var cmid=e.target&&e.target.id?e.target.id:null;post({ContextMenu:{path:p,mouse:mkMouse({ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey},cmid)}});}
           |  else if(t==="input"&&he(el,"input"))post({Input:{path:p,value:e.target.value}});
           |  else if(t==="change"&&he(el,"change")){
           |    var tgt=e.target,typ=tgt.type;
           |    if(typ==="checkbox"||typ==="radio")post({ChangeChecked:{path:p,checked:tgt.checked}});
           |    else if(typ==="number"||typ==="range")post({ChangeNumeric:{path:p,value:parseFloat(tgt.value)}});
           |    else if(typ==="file"){
           |      // Unit 2: file input. With an ancestor onFileSelect (fileselect token), read ALL files and post one
           |      // FileSelect once every FileReader completes (each writes its own index so file order is preserved).
           |      // Otherwise the legacy path reads the first file's text into a Change (byte-compatible content-only).
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
           |    // preventScrollKeys: suppress the browser's native page-scroll for navigation keys inside a region that
           |    // declared preventScrollKeys (data-kyo-scroll-keys). Vertical keys always; horizontal/edge keys only when
           |    // the target is not text-editable (so a filter input keeps caret movement). The keydown still posts below.
           |    if(e.target&&e.target.closest&&e.target.closest('[data-kyo-scroll-keys]')){
           |      var __sk=e.target,__ed=(/^(INPUT|TEXTAREA|SELECT)$$/.test(__sk.tagName)||__sk.isContentEditable);
           |      var __vk=(e.key==="ArrowUp"||e.key==="ArrowDown"||e.key==="PageUp"||e.key==="PageDown");
           |      var __hk=(e.key==="ArrowLeft"||e.key==="ArrowRight"||e.key==="Home"||e.key==="End");
           |      if(__vk||(__hk&&!__ed))e.preventDefault();
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
           |  else if(t==="mouseover"&&he(el,"mouseover")){var hotid=e.target&&e.target.id?e.target.id:null;post({Hover:{path:p,mouse:mkMouse({ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey},hotid)}});}
           |  else if(t==="mouseout"&&he(el,"mouseout")){var uhotid=e.target&&e.target.id?e.target.id:null;post({Unhover:{path:p,mouse:mkMouse({ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey},uhotid)}});}
           |  // Do NOT auto-call preventDefault: leave native-scroll suppression to the handler, matching DomBackend. Server-side rendering cannot synchronously decline the event, so the default is to NOT prevent.
           |  else if(t==="wheel"&&he(el,"wheel")){var whtid=e.target&&e.target.id?e.target.id:null;var sc={path:p,deltaX:e.deltaX,deltaY:e.deltaY,modifiers:{ctrl:e.ctrlKey,alt:e.altKey,shift:e.shiftKey,meta:e.metaKey}};if(whtid)sc.targetId=whtid;post({Scroll:sc});}
           |  else if(t==="scroll"&&he(el,"scroll")){__scrEl=el;__scrPath=p;if(!__scrRaf)__scrRaf=requestAnimationFrame(__scrFlush);}
           |}
           |// ---- Unit 3: client-local input filter/mask (document-level beforeinput capture listener) ----
           |function kyoFilterStr(pat,str,t){
           |  var out="";var hasSep=(pat==="decimal")&&(t.value.indexOf(".")>=0||t.value.indexOf(",")>=0);
           |  for(var i=0;i<str.length;i++){var ch=str.charAt(i);
           |    if(pat==="int"){if(ch>="0"&&ch<="9")out+=ch;}
           |    else if(pat==="decimal"){if(ch>="0"&&ch<="9")out+=ch;else if((ch==="."||ch===",")&&!hasSep){out+=ch;hasSep=true;}}
           |    else{if(pat.indexOf(ch)>=0)out+=ch;}}
           |  return out;
           |}
           |function kyoMaskClassAt(mask,idx){var c=0;for(var i=0;i<mask.length;i++){var mc=mask.charAt(i);if(mc==="9"||mc==="a"||mc==="*"){if(c===idx)return mc;c++;}}return null;}
           |function kyoMaskOk(cls,ch){if(cls==="9")return ch>="0"&&ch<="9";if(cls==="a")return (ch>="a"&&ch<="z")||(ch>="A"&&ch<="Z");return (ch>="0"&&ch<="9")||(ch>="a"&&ch<="z")||(ch>="A"&&ch<="Z");}
           |function kyoMaskFormat(mask,raw){var out="";var ri=0;for(var i=0;i<mask.length;i++){var mc=mask.charAt(i);
           |  if(mc==="9"||mc==="a"||mc==="*"){if(ri<raw.length){out+=raw.charAt(ri);ri++;}else break;}
           |  else{if(ri<raw.length)out+=mc;else break;}}
           |  return out;}
           |function kyoMaskRaw(mask,val){var raw="";var vi=0;for(var i=0;i<mask.length&&vi<val.length;i++){var mc=mask.charAt(i);
           |  if(mc==="9"||mc==="a"||mc==="*"){raw+=val.charAt(vi);vi++;}
           |  else{if(val.charAt(vi)===mc)vi++;else{raw+=val.charAt(vi);vi++;}}}
           |  return raw;}
           |function kyoSetVal(t,v){t.value=v;if(typeof t.setSelectionRange==="function"){try{t.setSelectionRange(v.length,v.length);}catch(er){}}t.dispatchEvent(new Event("input",{bubbles:true}));}
           |function kyoSetValAt(t,txt,s,en){var v=t.value;t.value=v.slice(0,s)+txt+v.slice(en);var np=s+txt.length;if(typeof t.setSelectionRange==="function"){try{t.setSelectionRange(np,np);}catch(er){}}t.dispatchEvent(new Event("input",{bubbles:true}));}
           |function kyoBeforeInput(e){
           |  var t=e.target;if(!t||!t.getAttribute)return;
           |  var filt=t.getAttribute("data-kyo-filter");var mask=t.getAttribute("data-kyo-mask");
           |  if(!filt&&!mask)return;
           |  var it=e.inputType||"";
           |  if(filt){
           |    if(it.indexOf("delete")===0)return;
           |    if(it==="insertText"||it==="insertReplacementText"||it==="insertCompositionText"){
           |      var data=e.data;if(data==null)return;
           |      var f1=kyoFilterStr(filt,data,t);
           |      if(f1!==data){e.preventDefault();if(f1){var s=(typeof t.selectionStart==="number")?t.selectionStart:t.value.length;var en=(typeof t.selectionEnd==="number")?t.selectionEnd:s;kyoSetValAt(t,f1,s,en);}}
           |    }else if(it==="insertFromPaste"||it==="insertFromDrop"){
           |      e.preventDefault();var pasted=e.dataTransfer?e.dataTransfer.getData("text"):(e.data||"");
           |      var f2=kyoFilterStr(filt,pasted,t);if(f2){var s2=(typeof t.selectionStart==="number")?t.selectionStart:t.value.length;var e2=(typeof t.selectionEnd==="number")?t.selectionEnd:s2;kyoSetValAt(t,f2,s2,e2);}
           |    }
           |    return;
           |  }
           |  if(mask){
           |    if(it.indexOf("delete")===0){e.preventDefault();var raw=kyoMaskRaw(mask,t.value);raw=raw.slice(0,raw.length-1);kyoSetVal(t,kyoMaskFormat(mask,raw));return;}
           |    if(it==="insertText"||it==="insertReplacementText"||it==="insertCompositionText"||it==="insertFromPaste"||it==="insertFromDrop"){
           |      e.preventDefault();var ins=e.data;if((it==="insertFromPaste"||it==="insertFromDrop")&&e.dataTransfer)ins=e.dataTransfer.getData("text");if(ins==null)ins="";
           |      var raw2=kyoMaskRaw(mask,t.value);
           |      for(var ci=0;ci<ins.length;ci++){var cls=kyoMaskClassAt(mask,raw2.length);if(cls===null)break;var ch=ins.charAt(ci);if(kyoMaskOk(cls,ch))raw2+=ch;}
           |      kyoSetVal(t,kyoMaskFormat(mask,raw2));return;
           |    }
           |  }
           |}
           |["click","contextmenu","input","change","submit","keydown","keyup","focus","blur","mouseover","mouseout"].forEach(function(t){
           |  document.body.addEventListener(t,handle,true);
           |});
           |document.body.addEventListener("wheel",handle,{capture:true,passive:false});
           |document.body.addEventListener("scroll",handle,{capture:true,passive:true});
           |document.body.addEventListener("beforeinput",kyoBeforeInput,true);
           |// ---- Unit 1: pointer/drag session (setPointerCapture + rAF-coalesced move stream) ----
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
