package kyo.internal

import kyo.*

/** The comment-node markers that delimit a reactive region (`Reactive`/`Foreach`/`Mounted`) in
  * rendered HTML: `<!--kyo:PATH-->content<!--/kyo:PATH-->`.
  *
  * Comment nodes are valid in every HTML parse context (table, tr, select, svg), unlike the wrapper
  * elements they replaced, which the parser foster-parented out of table contexts (span before table) or
  * dropped entirely ("in select" insertion mode). The markers carry no layout, so no `display: contents`
  * reset is needed and the region's content participates in its real parent's formatting context.
  *
  * BYTE-FORMAT CONTRACT: shared with the inline LiveView client in `HtmlRenderer.clientJs`; both
  * implementations MUST stay in lockstep:
  * {{{
  *   open  comment data = "kyo:" + escape(path) [+ " m"] [+ " s"] [+ " k=" + escape(mountKey)]
  *   close comment data = "/kyo:" + escape(path)
  *   escape: "%" -> "%25", "-" -> "%2D", " " -> "%20"   (decode in reverse; "%25" last)
  * }}}
  * Foreach keys are arbitrary user strings and land in the path, so escaping is load-bearing: with no raw
  * '-' left, no comment-terminating "-->" / "--!>" can occur inside a marker; with no raw ' ', the first
  * space unambiguously separates the path from the flags AND the flags from each other, which is why the
  * `k=` payload is escaped with the same function.
  *
  * Flags (client-paint only, never emitted by SSR/full-page renders, so golden HTML stays byte-identical):
  *   - `m` (mount root): a mount's live region; a parent morph treats the whole marker span as opaque and
  *     never wipes the live subtree.
  *   - `s` (mount slot): a `Mounted` placeholder painted by an exchange `onChange`, so a morph can tell
  *     "this slot IS a mount" from content that merely collided on the positional path key.
  *   - `k=<key>` (mount key): the identity of the mount that owns or claims this slot. `m` alone cannot
  *     decide opacity for a KEYED mount, because a key change must still reset the slot: the morph skips
  *     the span only when the live `k` equals the incoming one, and adopts the incoming `k` onto the live
  *     marker whenever it does not. Keyless mounts carry no `k` on either side, so `Absent == Absent`
  *     keeps their existing behaviour exactly.
  */
private[kyo] object RegionMarker:

    /** A parsed marker comment's data. `path` is the unescaped dot-joined path (registry key); `key` is the
      * unescaped mount key, present only on mount slots/roots that carry one.
      */
    final case class Parsed(
        path: String,
        isClose: Boolean,
        mount: Boolean,
        slot: Boolean,
        key: Maybe[String] = Absent
    ) derives CanEqual

    def open(path: Seq[String], mount: Boolean = false, slot: Boolean = false, key: Maybe[String] = Absent): String =
        s"<!--${openData(path.mkString("."), mount, slot, key)}-->"

    def close(path: Seq[String]): String =
        s"<!--${closeData(path.mkString("."))}-->"

    /** Comment data (the text between `<!--` and `-->`) of an open marker for a dot-joined path. */
    def openData(
        joinedPath: String,
        mount: Boolean = false,
        slot: Boolean = false,
        key: Maybe[String] = Absent
    ): String =
        val flags =
            (if mount then " m" else "") +
                (if slot then " s" else "") +
                key.fold("")(k => s" k=${escape(k)}")
        s"kyo:${escape(joinedPath)}$flags"
    end openData

    /** Comment data of a close marker for a dot-joined path. */
    def closeData(joinedPath: String): String =
        s"/kyo:${escape(joinedPath)}"

    /** Parse a comment node's data. `Absent` for comments that are not kyo markers. */
    def parse(data: String): Maybe[Parsed] =
        if data.startsWith("/kyo:") then Present(Parsed(unescape(data.substring(5)), isClose = true, mount = false, slot = false))
        else if data.startsWith("kyo:") then
            val rest = data.substring(4)
            val sp   = rest.indexOf(' ')
            if sp < 0 then Present(Parsed(unescape(rest), isClose = false, mount = false, slot = false))
            else
                val flags = rest.substring(sp + 1).split(' ')
                Present(Parsed(
                    unescape(rest.substring(0, sp)),
                    isClose = false,
                    mount = flags.contains("m"),
                    slot = flags.contains("s"),
                    key = Maybe.fromOption(flags.find(_.startsWith("k=")).map(f => unescape(f.substring(2))))
                ))
            end if
        else Absent

    private[kyo] def escape(path: String): String =
        if path.forall(c => c != '%' && c != '-' && c != ' ') then path
        else
            val sb = new StringBuilder(path.length + 4)
            path.foreach {
                case '%' => sb.append("%25")
                case '-' => sb.append("%2D")
                case ' ' => sb.append("%20")
                case c   => sb.append(c)
            }
            sb.toString

    private[kyo] def unescape(s: String): String =
        if !s.contains('%') then s
        else
            val sb = new StringBuilder(s.length)
            var i  = 0
            while i < s.length do
                val c = s.charAt(i)
                if c == '%' && i + 2 < s.length then
                    s.substring(i + 1, i + 3) match
                        case "25" => sb.append('%'); i += 3
                        case "2D" => sb.append('-'); i += 3
                        case "20" => sb.append(' '); i += 3
                        case _    => sb.append(c); i += 1
                else
                    sb.append(c)
                    i += 1
                end if
            end while
            sb.toString
end RegionMarker
