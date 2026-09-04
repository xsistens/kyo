package kyo.devtools

import kyo.*

/** The encoding the overlay reads.
  *
  * Hand-rolled JSON earns a test the way a hand-rolled parser does. The failure modes are not "wrong number"
  * but "unparseable frame", and under server-push an unparseable frame is a silent dead overlay with nothing
  * in the console pointing here.
  */
class DevtoolsWireTest extends kyo.test.Test[Any]:

    private def region(
        key: String = "0",
        display: String = "Page.rows",
        file: String = "Page.scala",
        snippet: String = "",
        repaints: Int = 3,
        rate: Double = 1.5
    ): DevtoolsStore.RegionSnapshot =
        DevtoolsStore.RegionSnapshot(
            key = key,
            path = key.split('.').toSeq,
            regionId = Present("r0001"),
            label = DevtoolsStore.Label(display, file, 42, "foreachKeyed", snippet),
            repaints = repaints,
            created = 1,
            wasted = 2,
            channelWrites = 4,
            textWrites = 5,
            rowsRepainted = 6L,
            rowsSeen = 7L,
            bytes = 8L,
            rate = rate,
            peakRate = 9.0,
            idleSeconds = 0.5,
            avgNanos = 10L,
            p95Nanos = 11L,
            history = Chunk(1, 2, 3)
        )

    private def snapshot(regions: DevtoolsStore.RegionSnapshot*): DevtoolsStore.Snapshot =
        DevtoolsStore.Snapshot(Chunk.from(regions), overflowed = false, recording = true)

    "every field the overlay reads is on the wire" in {
        val json = DevtoolsWire.snapshotJson(snapshot(region()))
        // Named one by one rather than by count: a field silently dropped from the encoder is a panel column
        // that reads zero forever, and nothing about that looks broken.
        val expected = Seq(
            "\"k\":\"0\"",
            "\"id\":\"r0001\"",
            "\"n\":\"Page.rows\"",
            "\"f\":\"Page.scala\"",
            "\"l\":42",
            "\"c\":\"foreachKeyed\"",
            "\"t\":3",
            "\"cr\":1",
            "\"w\":2",
            "\"ch\":4",
            "\"tx\":5",
            "\"rr\":6",
            "\"rs\":7",
            "\"b\":8",
            "\"a\":10",
            "\"p\":11",
            "\"h\":[1,2,3]",
            "\"rec\":true",
            "\"o\":false"
        )
        val missing = expected.filterNot(json.contains)
        assert(missing.isEmpty, s"missing from the payload: $missing\n$json")
    }

    "a rate is rounded, not printed at full precision" in {
        val json = DevtoolsWire.snapshotJson(snapshot(region(rate = 1.23456789)))
        assert(json.contains("\"r\":1.23"), s"rate not rounded: $json")
    }

    "a source snippet cannot break out of the payload" in {
        // A snippet is verbatim user source: it carries quotes, backslashes and newlines as a matter of
        // course, and under server-push it travels inside a JSON frame that a `"` would end.
        val nasty = "val s = \"x\\y\"\n\ttail\u2028after"
        val json  = DevtoolsWire.snapshotJson(snapshot(region(snippet = nasty)))
        assert(json.contains("\\\""), "a quote was not escaped")
        assert(json.contains("\\\\"), "a backslash was not escaped")
        assert(json.contains("\\n") && json.contains("\\t"), "a control character was not escaped")
        assert(json.contains("\\u2028"), "the line separator was not escaped")
        assert(!json.contains(nasty), "the snippet went out raw")
        // The payload must still be one flat line: it is inlined into a script context under server-push.
        assert(!json.contains("\n"), "the payload carries a raw newline")
    }

    "the payload is capped at the hottest regions" in {
        val many = (1 to 10).map(i => region(key = i.toString, rate = i.toDouble))
        val json = DevtoolsWire.snapshotJson(snapshot(many*), limit = 3)
        assert(json.split("\\{\"k\"", -1).length - 1 == 3, s"expected 3 regions, got: $json")
        // The cut follows the snapshot's own order, so it takes the front — not an arbitrary three.
        assert(json.contains("\"k\":\"1\"") && !json.contains("\"k\":\"4\""), s"wrong regions kept: $json")
    }

    "the overlay reads every key the encoder writes" in {
        // The encoder and the overlay are the two halves of a contract with nothing in between to check it:
        // a key renamed on one side produces no error, no warning and no log line — just a panel column that
        // reads undefined, in a tool whose whole purpose is to be believed. So the halves are compared here.
        val json   = DevtoolsWire.snapshotJson(snapshot(region()))
        val script = DevtoolsJs.script
        val rowKeys = "\"([a-z]{1,2})\":".r
            .findAllMatchIn(json.substring(json.indexOf("\"rows\":")))
            .map(_.group(1))
            .toSet
        assert(rowKeys.nonEmpty, "no row keys found in the payload")
        val unread = rowKeys.filterNot(key => ("row\\." + key + "\\b").r.findFirstIn(script).isDefined)
        assert(unread.isEmpty, s"the overlay never reads: ${unread.toSeq.sorted}")
        Seq("snapshot.o", "snapshot.h", "snapshot.rows").foreach(field =>
            assert(script.contains(field), s"the overlay never reads $field")
        )
    }

    "an empty snapshot is still a frame" in {
        val json = DevtoolsWire.snapshotJson(snapshot())
        assert(json == "{\"o\":false,\"rec\":true,\"h\":[],\"rows\":[]}", json)
    }

end DevtoolsWireTest
