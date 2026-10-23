package kyo.uic

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.Year
import java.time.ZonedDateTime
import kyo.*

/** The capability markers of [[DateKind]] are sealed traits, and a tool that walks a sealed hierarchy (a derivation, a
  * settings macro) needs each to have a NAMED child. An anonymous `new Calendar[A] {}` gave it none, and the walk read the
  * trait as a child of itself. What a picker refuses without the evidence is held in `DatePickerTest`; this holds the
  * shape of the evidence itself.
  */
class DateKindTest extends UicTest:

    /** The runtime class behind a given, which is the sealed child a hierarchy walk meets. */
    private def childOf(evidence: AnyRef): Class[?] = evidence.getClass

    "every marker's evidence is one named class, shared by every kind that has it" in {
        val byMarker: List[(String, List[Class[?]])] = List(
            "Calendar" -> List(
                childOf(summon[DateKind.Calendar[LocalDate]]),
                childOf(summon[DateKind.Calendar[Year]]),
                childOf(summon[DateKind.Calendar[ZonedDateTime]])
            ),
            "Drill" -> List(childOf(summon[DateKind.Drill[LocalDate]]), childOf(summon[DateKind.Drill[ZonedDateTime]])),
            "Weeks" -> List(childOf(summon[DateKind.Weeks[LocalDate]]), childOf(summon[DateKind.Weeks[LocalDateTime]])),
            "Days"  -> List(childOf(summon[DateKind.Days[LocalDate]]), childOf(summon[DateKind.Days[ZonedDateTime]])),
            "Clock" -> List(childOf(summon[DateKind.Clock[LocalDateTime]]), childOf(summon[DateKind.Clock[LocalTime]]))
        )
        byMarker.foreach { (marker, classes) =>
            assert(classes.distinct.size == 1, s"$marker: one implementation, got ${classes.map(_.getName)}")
            val name = classes.head.getName
            assert(!name.contains("$anon"), s"$marker: the implementation is named, got $name")
            assert(name.contains(marker), s"$marker: and it lives with its marker, got $name")
        }
        // Flat: no marker's evidence satisfies another, so one given never answers two questions.
        val all = byMarker.map(_._2.head)
        assert(all.distinct.size == all.size)
        assert(!summon[DateKind.Calendar[LocalDate]].isInstanceOf[DateKind.Days[?]])
        assert(!summon[DateKind.Days[LocalDate]].isInstanceOf[DateKind.Weeks[?]])
    }

end DateKindTest
