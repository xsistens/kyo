package kyo.internal

import kyo.*
import kyo.UI
import kyo.UI.Ast.*

/** Identifies the DOM representation of one reactive UI boundary. */
sealed private[kyo] trait ReactiveRegion derives CanEqual

private[kyo] object ReactiveRegion:

    /** Render identity separates application paths from transparent reactive nesting. */
    final case class RegionIdentity(path: Seq[String], transparentDepth: Int) derives CanEqual:
        require(transparentDepth >= 0, s"transparentDepth must be non-negative: $transparentDepth")

        def child(segment: String): RegionIdentity = RegionIdentity(path :+ segment, 0)

        def transparent: RegionIdentity =
            if transparentDepth == Int.MaxValue then
                throw new IllegalStateException("Reactive region transparent nesting exceeds Int.MaxValue")
            else copy(transparentDepth = transparentDepth + 1)
    end RegionIdentity

    object RegionIdentity:
        def root(path: Seq[String]): RegionIdentity = RegionIdentity(path, 0)

    enum ParentContext derives CanEqual:
        case HtmlTable, Other

    enum Namespace derives CanEqual:
        case Html, Svg

    enum BoundaryMode derives CanEqual:
        case Emit, Suppress

    enum TableContent derives CanEqual:
        case Rows, AuthoredSections, Transparent, Other

    enum RenderHost derives CanEqual:
        case HtmlComments(id: String, parentContext: ParentContext)
        case HtmlTableBody(id: String)
        case SvgGroup(path: Seq[String])
    end RenderHost

    def tableContent(ui: UI): TableContent =
        ui match
            case _: Thead | _: Tbody | _: Tfoot => TableContent.AuthoredSections
            case _: Tr                          => TableContent.Rows
            case _: Reactive[?]                 => TableContent.Transparent
            case _: Foreach[?, ?]               => TableContent.Transparent
            // A mount's static shape is its placeholder: that is what the renderer emits,
            // and what the parser will therefore see. Classified as `Other`, a `foreach` of
            // mounted rows would have its anchors written bare into the table, where the
            // parser is free to open an implied <tbody> for a preceding row and capture one
            // anchor but not the other, stranding the pair in different elements.
            case mounted: Mounted =>
                mounted.placeholderUI match
                    case Present(placeholder) => tableContent(placeholder)
                    case Absent               => TableContent.Transparent
            case KeyedChild(_, child)                   => tableContent(child)
            case Fragment(children) if children.isEmpty => TableContent.Other
            case Fragment(children)                     => tableContent(children)
            case _                                      => TableContent.Other
    end tableContent

    def tableContent(children: IterableOnce[UI]): TableContent =
        val iterator    = children.iterator
        var hasAuthored = false
        var hasOther    = false
        while iterator.hasNext do
            tableContent(iterator.next()) match
                case TableContent.Rows             => return TableContent.Rows
                case TableContent.AuthoredSections => hasAuthored = true
                case TableContent.Transparent      => ()
                case TableContent.Other            => hasOther = true
        end while
        if hasOther then TableContent.Other
        else if hasAuthored then TableContent.AuthoredSections
        else TableContent.Transparent
    end tableContent

    final case class HtmlRange(id: String)         extends ReactiveRegion
    final case class SvgElement(path: Seq[String]) extends ReactiveRegion

    def renderHost(region: ReactiveRegion, parentContext: ParentContext, content: TableContent): RenderHost =
        region match
            case HtmlRange(id) =>
                (parentContext, content) match
                    case (ParentContext.HtmlTable, TableContent.Rows) => RenderHost.HtmlTableBody(id)
                    case _                                            => RenderHost.HtmlComments(id, parentContext)
            case SvgElement(path) => RenderHost.SvgGroup(path)
    end renderHost

    def namespace(host: RenderHost): Namespace =
        host match
            case _: RenderHost.HtmlComments  => Namespace.Html
            case _: RenderHost.HtmlTableBody => Namespace.Html
            case _: RenderHost.SvgGroup      => Namespace.Svg
    end namespace

    def contentParent(host: RenderHost): ParentContext =
        host match
            case RenderHost.HtmlComments(_, parentContext) => parentContext
            case _: RenderHost.HtmlTableBody               => ParentContext.Other
            case _: RenderHost.SvgGroup                    => ParentContext.Other
    end contentParent

    def from(path: Seq[String], svgContext: Boolean): ReactiveRegion =
        from(RegionIdentity.root(path), svgContext)

    def from(identity: RegionIdentity, svgContext: Boolean): ReactiveRegion =
        if svgContext then SvgElement(identity.path)
        else HtmlRange(htmlId(identity))

    def from(identity: RegionIdentity, namespace: Namespace): ReactiveRegion =
        namespace match
            case Namespace.Html => HtmlRange(htmlId(identity))
            case Namespace.Svg  => SvgElement(identity.path)

    /** The id in a region's DOM comment markers, `Absent` for an SVG region, which has a real `<g>` element
      * and is addressed by its path instead. What a devtools overlay needs to find a region's node run.
      */
    def htmlIdOf(region: ReactiveRegion): Maybe[String] =
        region match
            case HtmlRange(id) => Present(id)
            case _: SvgElement => Absent

    def namespace(region: ReactiveRegion): Namespace =
        region match
            case _: HtmlRange  => Namespace.Html
            case _: SvgElement => Namespace.Svg

    def owns(region: ReactiveRegion, identity: RegionIdentity): Boolean =
        region match
            case HtmlRange(id)    => id == htmlId(identity)
            case SvgElement(path) => path == identity.path

    /** The flag section a mount's opening marker carries after its id, `""` for everything that is not a mount.
      *
      * A region is delimited by comments, so a mount placeholder has no element of its own to hang state on and its
      * marker is the only thing the client can read. `s` says the span IS a mount slot; `k` names the mount whose
      * instance owns it, hex-encoded so no key can spell a comment terminator or a separator. The live counterpart
      * is `m`, which the client stamps onto the marker once it has adopted the slot: an id never holds a space, so
      * the space before the first flag separates the two unambiguously.
      */
    private[kyo] def mountSlotFlags(key: Maybe[Any]): String =
        key match
            case Present(value) =>
                val text = value.toString
                val out  = new StringBuilder(4 + text.length * 4)
                out.append(" s k=")
                var i = 0
                while i < text.length do
                    appendHex(out, text.charAt(i).toInt)
                    i += 1
                end while
                out.toString
            case Absent => " s"

    /** The id part of a marker payload: everything up to the first flag separator. */
    private[kyo] def markerIdOf(payload: String): String =
        val space = payload.indexOf(' ')
        if space < 0 then payload else payload.substring(0, space)

    /** An id without its transparent-nesting suffix: the id of the same path at depth zero.
      *
      * `~` opens the suffix and is escaped everywhere else in an id, so the first one is the cut.
      */
    private[kyo] def baseIdOf(id: String): String =
        val nestingAt = id.indexOf('~')
        if nestingAt < 0 then id else id.substring(0, nestingAt)

    private[kyo] def htmlId(path: Seq[String]): String =
        htmlId(RegionIdentity.root(path))

    /** The id a region's DOM markers carry: `r`, then `.` and the segment for every path segment, then `~` and
      * the decimal transparent depth when it is not zero.
      *
      * A segment is written as itself where it is made of `[0-9A-Za-z_]`, which is every decimal index and most
      * keys; any other UTF-16 unit is `$` and four hex digits. That keeps the three structural characters `.`,
      * `~` and `$` out of the segment text, so the id splits without lookahead, the segment count is the dot
      * count (`r` is the root, `r.` a single empty segment), and no key can spell `--`, `>`, a quote or a space
      * into the comment or attribute the id sits in. The client twins are `__kyoRangeIdOf` and
      * `__kyoRangeIdPath` in `HtmlRenderer.clientJs`, and `kyoRangeId` validates the same grammar; keep the
      * four in lockstep.
      */
    private[kyo] def htmlId(identity: RegionIdentity): String =
        var size = 1
        identity.path.foreach(segment => size += 1 + segment.length)
        if identity.transparentDepth > 0 then size += 11
        val out = new StringBuilder(size)
        out.append('r')
        identity.path.foreach { segment =>
            out.append('.')
            appendSegment(out, segment)
        }
        if identity.transparentDepth > 0 then discard(out.append('~').append(identity.transparentDepth))
        out.toString
    end htmlId

    /** The path an [[htmlId]] encodes, `Absent` for anything that is not a well-formed id. The nesting suffix
      * is ignored: a transparent nesting level does not change the path.
      */
    private[kyo] def pathOf(id: String): Maybe[Seq[String]] =
        if !isValidHtmlId(id) then Absent
        else
            val pathEnd  = nestingStart(id)
            val segments = Chunk.newBuilder[String]
            var i        = 1
            while i < pathEnd do
                // `i` is at the dot that opens a segment.
                val start = i + 1
                var end   = start
                var plain = true
                while end < pathEnd && id.charAt(end) != '.' do
                    if id.charAt(end) == '$' then plain = false
                    end += 1
                end while
                if plain then segments.addOne(id.substring(start, end))
                else
                    val segment = new StringBuilder(end - start)
                    var j       = start
                    while j < end do
                        val c = id.charAt(j)
                        if c == '$' then
                            discard(segment.append(Integer.parseInt(id.substring(j + 1, j + 5), 16).toChar))
                            j += 5
                        else
                            discard(segment.append(c))
                            j += 1
                        end if
                    end while
                    segments.addOne(segment.toString)
                end if
                i = end
            end while
            Present(segments.result())
        end if
    end pathOf

    private[kyo] def isValidHtmlId(id: String): Boolean =
        if id.isEmpty || id.charAt(0) != 'r' then false
        else
            val n     = id.length
            var i     = 1
            var valid = true
            while valid && i < n && id.charAt(i) != '~' do
                if id.charAt(i) != '.' then valid = false
                else
                    i += 1
                    var inSegment = true
                    while valid && inSegment && i < n do
                        val c = id.charAt(i)
                        if c == '.' || c == '~' then inSegment = false
                        else if isVerbatim(c) then i += 1
                        else if c == '$' then
                            if i + 5 > n then valid = false
                            else
                                var j = 1
                                while valid && j <= 4 do
                                    valid = isHex(id.charAt(i + j))
                                    j += 1
                                end while
                                i += 5
                        else valid = false
                        end if
                    end while
            end while
            if valid && i < n then
                // At the `~`: a decimal depth with no leading zero, so one depth has one spelling.
                i += 1
                if i >= n || id.charAt(i) < '1' || id.charAt(i) > '9' then valid = false
                else
                    i += 1
                    while valid && i < n do
                        val c = id.charAt(i)
                        valid = c >= '0' && c <= '9'
                        i += 1
                    end while
                end if
            end if
            valid
        end if
    end isValidHtmlId

    private def nestingStart(id: String): Int =
        val at = id.indexOf('~')
        if at < 0 then id.length else at

    private def isVerbatim(c: Char): Boolean =
        (c >= '0' && c <= '9') ||
            (c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z') || c == '_'

    private def isHex(c: Char): Boolean = hexValue(c) >= 0

    private def hexValue(c: Char): Int =
        if c >= '0' && c <= '9' then c - '0'
        else if c >= 'a' && c <= 'f' then c - 'a' + 10
        else -1

    /** Writes `segment` with every unit outside `[0-9A-Za-z_]` escaped; a decimal index is one plain append. */
    private def appendSegment(out: StringBuilder, segment: String): Unit =
        val n     = segment.length
        var start = 0
        var i     = 0
        while i < n do
            val c = segment.charAt(i)
            if !isVerbatim(c) then
                if start < i then discard(out.underlying.append(segment, start, i))
                out.append('$')
                appendHex(out, c.toInt)
                start = i + 1
            end if
            i += 1
        end while
        if start == 0 then discard(out.append(segment))
        else if start < n then discard(out.underlying.append(segment, start, n))
    end appendSegment

    // Four hex digits for every value below 256, which covers every Latin-1 UTF-16 unit, so such a unit is written
    // as one whole string.
    private val hex4: Array[String] =
        val out = new Array[String](256)
        var v   = 0
        while v < 256 do
            val sb = new StringBuilder(4)
            appendHexSlow(sb, v)
            out(v) = sb.toString
            v += 1
        end while
        out
    end hex4

    /** Four lowercase hex digits of a UTF-16 unit. */
    private def appendHex(out: StringBuilder, value: Int): Unit =
        if value >= 0 && value < 256 then discard(out.append(hex4(value)))
        else appendHexSlow(out, value)

    private def appendHexSlow(out: StringBuilder, value: Int): Unit =
        var shift = 12
        while shift >= 0 do
            val nibble = (value >>> shift) & 0xf
            out.append(if nibble < 10 then ('0' + nibble).toChar else ('a' + nibble - 10).toChar)
            shift -= 4
        end while
    end appendHexSlow

end ReactiveRegion
