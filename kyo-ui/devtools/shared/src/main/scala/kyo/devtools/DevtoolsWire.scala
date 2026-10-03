package kyo.devtools

import kyo.*

/** The snapshot as the overlay reads it.
  *
  * One encoding for both runners. The browser mount could hand the overlay a live JS object and skip the
  * parse, but then the two transports would differ in the one place a divergence is invisible until a field
  * is wrong under exactly one of them — and the parse costs about a millisecond at ten snapshots a second,
  * which is not worth a second code path.
  *
  * Keys are one and two letters because under server-push every snapshot crosses a WebSocket, where the long
  * names would be most of the frame. The reader is `push` in [[DevtoolsJs]]; the two change together.
  */
object DevtoolsWire:

    /** Serialize the hottest `limit` regions.
      *
      * A cap, because the payload is per snapshot and the overlay can only draw a couple of hundred badges
      * anyway. The snapshot is already sorted hottest first, so the cut falls exactly where a reader stops
      * caring — and the panel's totals are computed before it, so the header still describes the whole page.
      */
    def snapshotJson(snapshot: DevtoolsStore.Snapshot, limit: Int = 200): String =
        val out = new StringBuilder(256 + limit * 220)
        out.append("{\"o\":")
        out.append(snapshot.overflowed)
        out.append(",\"rec\":")
        out.append(snapshot.recording)
        out.append(",\"h\":")
        ints(out, snapshot.history)
        out.append(",\"rows\":[")
        var written = 0
        val rows    = snapshot.regions
        var i       = 0
        while i < rows.size && written < limit do
            if written > 0 then out.append(',')
            region(out, rows(i))
            written += 1
            i += 1
        end while
        out.append("]}")
        out.toString()
    end snapshotJson

    private def region(out: StringBuilder, r: DevtoolsStore.RegionSnapshot): Unit =
        out.append("{\"k\":")
        string(out, r.key)
        out.append(",\"id\":")
        string(out, r.regionId.getOrElse(""))
        out.append(",\"n\":")
        string(out, r.label.display)
        out.append(",\"f\":")
        string(out, r.label.file)
        out.append(",\"l\":")
        out.append(r.label.line)
        out.append(",\"c\":")
        string(out, r.label.callee)
        out.append(",\"s\":")
        string(out, r.label.snippet)
        out.append(",\"r\":")
        out.append(round2(r.rate))
        out.append(",\"pk\":")
        out.append(round2(r.peakRate))
        out.append(",\"t\":")
        out.append(r.repaints)
        out.append(",\"cr\":")
        out.append(r.created)
        out.append(",\"w\":")
        out.append(r.wasted)
        out.append(",\"ch\":")
        out.append(r.channelWrites)
        out.append(",\"tx\":")
        out.append(r.textWrites)
        out.append(",\"rr\":")
        out.append(r.rowsRepainted)
        out.append(",\"rs\":")
        out.append(r.rowsSeen)
        out.append(",\"b\":")
        out.append(r.bytes)
        out.append(",\"i\":")
        out.append(round2(r.idleSeconds))
        out.append(",\"a\":")
        out.append(r.avgNanos)
        out.append(",\"p\":")
        out.append(r.p95Nanos)
        out.append(",\"h\":")
        ints(out, r.history)
        out.append('}')
    end region

    private def ints(out: StringBuilder, values: Chunk[Int]): Unit =
        out.append('[')
        var i = 0
        while i < values.size do
            if i > 0 then out.append(',')
            out.append(values(i))
            i += 1
        end while
        out.append(']')
    end ints

    /** Two decimals: the overlay renders one, and the difference is payload nobody reads. */
    private def round2(value: Double): String =
        if value.isNaN || value.isInfinite then "0"
        else (math.rint(value * 100.0) / 100.0).toString

    private def string(out: StringBuilder, value: String): Unit =
        out.append('"')
        var i = 0
        while i < value.length do
            val c = value.charAt(i)
            c match
                case '"'  => out.append("\\\"")
                case '\\' => out.append("\\\\")
                case '\n' => out.append("\\n")
                case '\r' => out.append("\\r")
                case '\t' => out.append("\\t")
                // Everything below space, plus U+2028/U+2029: legal inside a JSON string, but they terminate
                // a JavaScript string literal, and this payload gets inlined into a page. Compared as code
                // points rather than written as escapes, which in Scala source would depend on how the
                // compiler treats a unicode escape inside a character literal.
                case _ if c < ' ' || c.toInt == 0x2028 || c.toInt == 0x2029 =>
                    out.append("\\u")
                    val hex = Integer.toHexString(c.toInt)
                    var p   = hex.length
                    while p < 4 do
                        out.append('0')
                        p += 1
                    out.append(hex)
                case _ => out.append(c)
            end match
            i += 1
        end while
        out.append('"')
    end string

end DevtoolsWire
