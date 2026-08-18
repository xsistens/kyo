package kyo.uic.form

import java.time.LocalDate
import java.time.LocalTime
import java.time.ZonedDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import kyo.*

/** Bridges a typed date value `A` to and from the native picker's ISO string seam
  * (`uic.DatePicker` binds a two-way `SignalRef[String]` of `YYYY-MM-DD`). The picker
  * stays the single source of truth; the typed value is derived through this codec, so
  * there is no second ref to keep in sync.
  *
  * `fromInput` returns `Absent` for an empty / unparsable string — an unselected
  * picker has no date, and a native `<input type=date>` only ever emits a valid ISO
  * date or the empty string, so parse failures do not arise in practice.
  *
  * On Scala.js the `java.time` API comes from `scala-java-time`; the zone-aware
  * [[zoned]] codec additionally needs the zone's rules at runtime, which the app must
  * bundle (see the demo's `sbt-tzdb` setup). [[local]] needs no timezone data.
  */
trait DateCodec[A]:
    def toInput(a: A): String
    def fromInput(s: String): Maybe[A]

    /** Equality for `A`, carried by the codec so call sites never need `CanEqual[A, A]` in scope.
      *
      * A typed field projects its value through a `Signal`, which compares images to suppress
      * redundant emissions and therefore needs the instance. Requiring it at the factory would push
      * the obligation onto every caller, and java.time types carry no instance of their own; a codec
      * author, who already knows the type, answers it once here. Mirrors how kyo-ui's `Chart.ColorKey`
      * carries its own equality. The default covers the ordinary case, where representable-as-a-value
      * already implies comparable.
      */
    private[uic] def equality: CanEqual[A, A] = CanEqual.derived
end DateCodec

object DateCodec:

    private val Iso = DateTimeFormatter.ISO_LOCAL_DATE

    /** A plain calendar date, no timezone — the honest type for a date-only picker. */
    given local: DateCodec[LocalDate] with
        def toInput(a: LocalDate): String = a.format(Iso)
        def fromInput(s: String): Maybe[LocalDate] =
            val t = s.trim
            if t.isEmpty then Absent
            else
                try Present(LocalDate.parse(t, Iso))
                catch case _: Throwable => Absent
            end if
        end fromInput
    end local

    /** Zone-aware: a picked calendar day becomes a `ZonedDateTime` at `at` (default
      * midnight) in `zone`; formatting projects any instant to that zone's local date.
      * The zone is fixed by the codec, so round-trips within it are stable — which is
      * why a picker (that can only carry a date) can back a zoned value at all.
      */
    def zoned(zone: ZoneId, at: LocalTime = LocalTime.MIDNIGHT): DateCodec[ZonedDateTime] =
        new DateCodec[ZonedDateTime]:
            def toInput(a: ZonedDateTime): String = a.withZoneSameInstant(zone).toLocalDate.format(Iso)
            def fromInput(s: String): Maybe[ZonedDateTime] =
                val t = s.trim
                if t.isEmpty then Absent
                else
                    try Present(LocalDate.parse(t, Iso).atTime(at).atZone(zone))
                    catch case _: Throwable => Absent
                end if
            end fromInput
end DateCodec
