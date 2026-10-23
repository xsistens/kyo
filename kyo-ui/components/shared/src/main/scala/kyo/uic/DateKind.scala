package kyo.uic

import java.time.DateTimeException
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.Year
import java.time.YearMonth
import java.time.ZonedDateTime
import java.time.ZoneId
import java.time.temporal.ChronoUnit
import kyo.*
import scala.annotation.implicitNotFound

/** What a [[DatePicker]] holds, and so what it shows: the value type decides the picker's mode, so a picker cannot be set up
  * to show one thing and hold another.
  *
  * | type            | the picker shows                       |
  * |-----------------|----------------------------------------|
  * | `LocalDate`     | the day grid                           |
  * | `LocalDateTime` | the day grid and the time picker       |
  * | `LocalTime`     | the time picker alone                  |
  * | `YearMonth`     | the month grid                         |
  * | `Year`          | the year grid                          |
  *
  * Each of those is a given, so `uic.DatePicker[LocalDate]()` finds its kind by the type alone. [[DateKind.zoned]] puts a
  * `ZonedDateTime` behind the day grid at a fixed zone and time of day, and is passed explicitly:
  * `uic.DatePicker(uic.DateKind.zoned(zone))`.
  *
  * The calendar works on ISO strings inside, since its date math is integer arithmetic over them and that keeps the render
  * pure on every platform. A kind is the one place a value is written as such a string and read back from one, at the
  * picker's precision (the minute, for the kinds that carry a time). Two values a kind writes as the same string are the same
  * pick, so a pick or a typed text naming the value already held leaves the app's own instance in place.
  *
  * The setters that only some kinds can honour ask for one of the capability markers below ([[DateKind.Calendar]],
  * [[DateKind.Drill]], [[DateKind.Weeks]], [[DateKind.Days]], [[DateKind.Clock]]), so a setter a kind has no use for does not
  * compile rather than being ignored.
  */
@implicitNotFound(
    "A DatePicker holds a LocalDate, LocalDateTime, LocalTime, YearMonth or Year, named by its type parameter " +
        "(uic.DatePicker[LocalDate]()), or a ZonedDateTime through an explicit kind (uic.DatePicker(uic.DateKind.zoned(zone))). " +
        "${A} is none of these."
)
sealed abstract class DateKind[A] private[uic] (private[uic] val shape: DateKind.Shape):

    /** The value as the calendar's ISO string. */
    private[uic] def toIso(a: A): String

    /** The value an ISO string (or what a reader typed) names, `Absent` for an empty or unreadable one. */
    private[uic] def fromIso(s: String): Maybe[A]

    /** The grid that picks the value, which is also the grid the picker opens on. */
    private[uic] def view: DatePickerView =
        shape match
            case DateKind.Shape.Month => DatePickerView.Month
            case DateKind.Shape.Year  => DatePickerView.Year
            case _                    => DatePickerView.Date

    /** Whether the day grid is followed by the time picker. */
    private[uic] def withTime: Boolean = shape == DateKind.Shape.DayAndTime

    /** Whether the time picker is all there is. */
    private[uic] def timeOnly: Boolean = shape == DateKind.Shape.Time
end DateKind

object DateKind:

    /** Which grids a kind shows. */
    private[uic] enum Shape derives CanEqual:
        case Day, DayAndTime, Time, Month, Year

    private def pad(n: Int, width: Int): String =
        val s = n.toString
        if s.length >= width then s else "0" * (width - s.length) + s

    private def isoDate(d: LocalDate): String = pad(d.getYear, 4) + "-" + pad(d.getMonthValue, 2) + "-" + pad(d.getDayOfMonth, 2)

    private def isoTime(t: LocalTime): String = pad(t.getHour, 2) + ":" + pad(t.getMinute, 2)

    /** Reads `s` with a `java.time` parser, which reports a malformed string by throwing; this is the one place that is
      * turned into `Absent`.
      */
    private def parse[A](s: String)(f: String => A): Maybe[A] =
        val t = s.trim
        if t.isEmpty then Absent
        else
            try Present(f(t))
            catch case _: DateTimeException => Absent
        end if
    end parse

    /** A calendar day: the day grid. */
    given date: DateKind[LocalDate] = new DateKind[LocalDate](Shape.Day):
        private[uic] def toIso(a: LocalDate): String          = isoDate(a)
        private[uic] def fromIso(s: String): Maybe[LocalDate] = parse(s)(LocalDate.parse)

    /** A day and a time of day, to the minute: the day grid and the time picker. */
    given dateTime: DateKind[LocalDateTime] = new DateKind[LocalDateTime](Shape.DayAndTime):
        private[uic] def toIso(a: LocalDateTime): String          = isoDate(a.toLocalDate) + "T" + isoTime(a.toLocalTime)
        private[uic] def fromIso(s: String): Maybe[LocalDateTime] =
            parse(s)(t => LocalDateTime.parse(t).truncatedTo(ChronoUnit.MINUTES))

    /** A time of day, to the minute: the time picker alone. */
    given time: DateKind[LocalTime] = new DateKind[LocalTime](Shape.Time):
        private[uic] def toIso(a: LocalTime): String          = isoTime(a)
        private[uic] def fromIso(s: String): Maybe[LocalTime] = parse(s)(t => LocalTime.parse(t).truncatedTo(ChronoUnit.MINUTES))

    /** A month of a year: the month grid. */
    given month: DateKind[YearMonth] = new DateKind[YearMonth](Shape.Month):
        private[uic] def toIso(a: YearMonth): String          = pad(a.getYear, 4) + "-" + pad(a.getMonthValue, 2)
        private[uic] def fromIso(s: String): Maybe[YearMonth] = parse(s)(YearMonth.parse)

    /** A year: the year grid. */
    given year: DateKind[Year] = new DateKind[Year](Shape.Year):
        private[uic] def toIso(a: Year): String          = pad(a.getValue, 4)
        private[uic] def fromIso(s: String): Maybe[Year] = parse(s)(t => Year.parse(t))

    /** A calendar day held as a `ZonedDateTime` at `at` in `zone`, behind the day grid.
      *
      * A value from another zone shows as `zone`'s local date. A pick of another day writes that day at `at`; a pick of the day
      * already shown writes nothing, so a held value keeps its instant (and its time of day) through the picker. On Scala.js and
      * Scala Native the zone's rules come from the tzdb the app bundles.
      */
    def zoned(zone: ZoneId, at: LocalTime = LocalTime.MIDNIGHT): DateKind[ZonedDateTime] =
        new DateKind[ZonedDateTime](Shape.Day):
            private[uic] def toIso(a: ZonedDateTime): String          = isoDate(a.withZoneSameInstant(zone).toLocalDate)
            private[uic] def fromIso(s: String): Maybe[ZonedDateTime] = parse(s)(t => LocalDate.parse(t).atTime(at).atZone(zone))

    // === capability markers =====================================================================

    /** Evidence that a picker over `A` shows a calendar to page through, which every kind but `LocalTime` does. Asked for by
      * `month` and `referenceDate`.
      */
    @implicitNotFound("A ${A} picker shows no calendar (it is the time picker alone), so there is no month to show or start from.")
    sealed trait Calendar[A]

    object Calendar:
        private def of[A]: Calendar[A] = new Calendar[A] {}
        given Calendar[LocalDate]      = of
        given Calendar[LocalDateTime]  = of
        given Calendar[YearMonth]      = of
        given Calendar[Year]           = of
        given Calendar[ZonedDateTime]  = of
    end Calendar

    /** Evidence that a picker over `A` can drill from the grid it picks in to a coarser one through the title buttons, which
      * is every kind with a calendar but `Year` (whose year grid is already the coarsest). Asked for by `currentView`.
      */
    @implicitNotFound("A ${A} picker has no grid to drill into, so it has no current view to bind.")
    sealed trait Drill[A]

    object Drill:
        private def of[A]: Drill[A] = new Drill[A] {}
        given Drill[LocalDate]      = of
        given Drill[LocalDateTime]  = of
        given Drill[YearMonth]      = of
        given Drill[ZonedDateTime]  = of
    end Drill

    /** Evidence that a picker over `A` picks in the day grid, which is the grid week numbers belong to. Asked for by
      * `showWeek`.
      */
    @implicitNotFound("A ${A} picker never shows the day grid, so it has no week numbers to show.")
    sealed trait Weeks[A]

    object Weeks:
        private def of[A]: Weeks[A] = new Weeks[A] {}
        given Weeks[LocalDate]      = of
        given Weeks[LocalDateTime]  = of
        given Weeks[ZonedDateTime]  = of
    end Weeks

    /** Evidence that one day cell picks a whole `A`, which is what a set of days or a range of them needs: `LocalDate`, and a
      * `ZonedDateTime` through [[zoned]]. Asked for by `values` and `range`.
      */
    @implicitNotFound(
        "values and range pick days, so they need a picker whose value is one day (LocalDate, or a ZonedDateTime through " +
            "DateKind.zoned). A ${A} picker picks one value."
    )
    sealed trait Days[A]

    object Days:
        private def of[A]: Days[A] = new Days[A] {}
        given Days[LocalDate]      = of
        given Days[ZonedDateTime]  = of
    end Days

    /** Evidence that a picker over `A` shows the time picker: `LocalDateTime` and `LocalTime`. Asked for by `hourFormat`. */
    @implicitNotFound("A ${A} picker has no time picker, so it has no hour format.")
    sealed trait Clock[A]

    object Clock:
        private def of[A]: Clock[A] = new Clock[A] {}
        given Clock[LocalDateTime]  = of
        given Clock[LocalTime]      = of
    end Clock

    // === equality ===============================================================================

    /** Strict equality for the value types the kinds hold, which `java.time` does not declare itself. A picker's own work
      * needs none of it; an app does, to hold a value in a `SignalRef` or a form field, and `import kyo.uic.DateKind.given`
      * brings these into scope.
      */
    given CanEqual[LocalDate, LocalDate]         = CanEqual.derived
    given CanEqual[LocalDateTime, LocalDateTime] = CanEqual.derived
    given CanEqual[LocalTime, LocalTime]         = CanEqual.derived
    given CanEqual[YearMonth, YearMonth]         = CanEqual.derived
    given CanEqual[Year, Year]                   = CanEqual.derived
    given CanEqual[ZonedDateTime, ZonedDateTime] = CanEqual.derived
end DateKind
