package kyo.uic

import kyo.*

/** A severity from a signal is the element's class-name channel: Tag, Badge and Message draw no region for it, show the class of
  * the severity the signal holds, and swap it in place when it moves.
  */
class SeverityValueTest extends UicTest:

    /** The severity classes `root` carries now, among `names`. */
    private def carried(root: UI, host: String, names: Seq[String])(using Frame): Seq[String] < Sync =
        elementWithClass(root, host).map(el =>
            Kyo.foreach(names)(n => carriesClass(el, n).map(on => Maybe.when(on)(n))).map(_.flatMap(_.toChunk))
        )

    private val tagClasses     = Seq("p-tag-success", "p-tag-danger", "p-tag-info")
    private val badgeClasses   = Seq("p-badge-success", "p-badge-danger", "p-badge-info")
    private val messageClasses = Seq("p-message-success", "p-message-error", "p-message-info")

    "Tag: a signal severity is one class channel, no region, and moves the class in place" in {
        for
            sev <- Signal.initRef(Severity.Success)
            tag = uic.Tag("t").severity(sev).render
            regions <- regionsAbove(tag, "p-tag")
            success <- carried(tag, "p-tag", tagClasses)
            _       <- sev.set(Severity.Danger)
            danger  <- carried(tag, "p-tag", tagClasses)
            _       <- sev.set(Severity.Primary)
            primary <- carried(tag, "p-tag", tagClasses)
        yield
            assert(regions == 0, s"$regions regions")
            assert(success == Seq("p-tag-success") && danger == Seq("p-tag-danger") && primary.isEmpty, s"$success | $danger | $primary")
    }

    "Badge: a signal severity is one class channel, no region, and moves the class in place" in {
        for
            sev <- Signal.initRef(Severity.Success)
            badge = uic.Badge("3").severity(sev).render
            regions <- regionsAbove(badge, "p-badge")
            success <- carried(badge, "p-badge", badgeClasses)
            _       <- sev.set(Severity.Danger)
            danger  <- carried(badge, "p-badge", badgeClasses)
            _       <- sev.set(Severity.Help)
            help    <- carried(badge, "p-badge", badgeClasses)
        yield
            assert(regions == 0, s"$regions regions")
            assert(success == Seq("p-badge-success") && danger == Seq("p-badge-danger") && help.isEmpty, s"$success | $danger | $help")
    }

    "Message: a signal severity is one class channel, and the severities that share a class hold it together" in {
        for
            sev <- Signal.initRef(Severity.Danger)
            message = uic.Message().severity(sev).render
            regions <- regionsAbove(message, "p-message")
            danger  <- carried(message, "p-message", messageClasses)
            _       <- sev.set(Severity.Info)
            info    <- carried(message, "p-message", messageClasses)
            _       <- sev.set(Severity.Primary)
            primary <- carried(message, "p-message", messageClasses)
        yield
            assert(regions == 0, s"$regions regions")
            assert(
                danger == Seq("p-message-error") && info == Seq("p-message-info") && primary == Seq("p-message-info"),
                s"$danger | $info | $primary"
            )
    }

end SeverityValueTest
