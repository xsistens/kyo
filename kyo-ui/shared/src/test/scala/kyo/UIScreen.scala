package kyo

import kyo.internal.HtmlRenderer
import kyo.internal.ReactiveRegion
import kyo.internal.ReactiveUI
import kyo.internal.RecordingExchange
import kyo.internal.UIEvent

/** A page that takes every paint the way the client does, replacing the content between a range's markers, and reads the visible
  * text of the result: text inside an element with the `hidden` attribute or inside a `template` is not visible. Tests wait for a
  * state of the screen or for a step of an effect through promises, never by polling.
  */
abstract private[kyo] class UIScreen extends kyo.test.Test[Any]:

    final class Screen(initial: String) extends RecordingExchange:
        import AllowUnsafe.embrace.danger

        private val page    = AtomicRef.Unsafe.init(initial)
        private val history = AtomicRef.Unsafe.init(Chunk(Screen.visible(initial)))
        private val waiters = AtomicRef.Unsafe.init(Chunk.empty[(String => Boolean, Promise[Unit, Any])])

        override def onChange(
            region: ReactiveRegion,
            path: Seq[String],
            context: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            ui: UI
        )(using Frame): Unit < Async =
            // Rendered the way the WebSocket transport renders a region, so nested ranges carry the ids the engine addresses.
            val mode =
                if ReactiveRegion.owns(region, context) then ReactiveRegion.BoundaryMode.Suppress else ReactiveRegion.BoundaryMode.Emit
            HtmlRenderer.renderRegionWithCss(ui, path, context, region, parentContext, mode).map { (inner, _) =>
                Sync.Unsafe.defer {
                    region match
                        case ReactiveRegion.HtmlRange(id) => discard(page.getAndUpdate(Screen.splice(_, id, inner)))
                        case _                            => ()
                    val shown = Screen.visible(page.get())
                    discard(history.getAndUpdate(_.append(shown)))
                    shown
                }.map { shown =>
                    super.onChange(region, path, context, parentContext, previous, ui).andThen(Sync.Unsafe.defer {
                        var ready = Chunk.empty[Promise[Unit, Any]]
                        discard(waiters.getAndUpdate { all =>
                            val (met, rest) = all.partition(_._1(shown))
                            ready = met.map(_._2)
                            rest
                        })
                        ready
                    }).map(ready => Kyo.foreachDiscard(ready)(_.completeUnitDiscard))
                }
            }
        end onChange

        def now(using Frame): String < Sync  = Sync.Unsafe.defer(Screen.visible(page.get()))
        def html(using Frame): String < Sync = Sync.Unsafe.defer(page.get())

        /** Every state the screen went through, oldest first. */
        def states(using Frame): Chunk[String] < Sync = Sync.Unsafe.defer(history.get())

        /** How many paints replaced the range at exactly `path`, which replaces every node inside it. */
        def paintsAt(path: Seq[String])(using Frame): Int < Sync = paints.map(_.count(_.path == path))

        /** Waits until the visible text satisfies `pred`. */
        def await(label: String)(pred: String => Boolean)(using Frame, kyo.test.AssertScope): Unit < Async =
            for
                promise <- Promise.init[Unit, Any]
                _       <- Sync.Unsafe.defer(discard(waiters.getAndUpdate(_.append((pred, promise)))))
                current <- now
                _       <- if pred(current) then promise.completeUnitDiscard else Kyo.unit
                _       <- promise.get
            yield ()
    end Screen

    object Screen:
        /** Replaces what range `id` holds. A start marker may carry flags after the id (`<!--kyo-rs:r.1 s-->`). */
        def splice(page: String, id: String, inner: String): String =
            val open  = s"<!--kyo-rs:$id"
            val start = Iterator.iterate(page.indexOf(open))(at => page.indexOf(open, at + 1)).takeWhile(_ >= 0)
                .find(at => page.startsWith("-->", at + open.length) || page.startsWith(" ", at + open.length))
                .getOrElse(-1)
            val opened = if start < 0 then -1 else page.indexOf("-->", start) + 3
            val end    = if start < 0 then -1 else page.indexOf(s"<!--kyo-re:$id-->", opened)
            if start < 0 || end < 0 then page
            else page.substring(0, opened) + inner + page.substring(end)
        end splice

        private val voids    = Set("area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr")
        private val hiddenAt = """(^|\s)hidden(\s|=|/|$)""".r

        def visible(html: String): String =
            val out    = new StringBuilder
            var hidden = List.empty[Boolean]
            var i      = 0
            while i < html.length do
                if html.startsWith("<!--", i) then
                    val end = html.indexOf("-->", i)
                    i = if end < 0 then html.length else end + 3
                else if html.charAt(i) == '<' then
                    val end = html.indexOf('>', i)
                    val tag = html.substring(i + 1, if end < 0 then html.length else end)
                    i = if end < 0 then html.length else end + 1
                    if tag.startsWith("/") then hidden = hidden.drop(1)
                    else
                        val name = tag.takeWhile(c => !c.isWhitespace && c != '/').toLowerCase
                        if !tag.endsWith("/") && !voids(name) then
                            val inherited = hidden.headOption.getOrElse(false)
                            hidden = (inherited || name == "template" || hiddenAt.findFirstIn(tag).nonEmpty) :: hidden
                    end if
                else
                    val next = html.indexOf('<', i) match
                        case -1 => html.length
                        case n  => n
                    if !hidden.headOption.getOrElse(false) then discard(out.append(html.substring(i, next)).append(' '))
                    i = next
                end if
            end while
            out.toString
        end visible
    end Screen

    def mount[A](ui: UI)(f: Screen => A < (Async & Scope))(using Frame): A < Async =
        mountWith(ui)((screen, _) => f(screen))

    /** Also hands out the subscription's event handler, which events go to as if the user clicked on the page. */
    def mountWith[A](ui: UI)(f: (Screen, (Seq[String], UIEvent) => Boolean < Async) => A < (Async & Scope))(using
        Frame
    ): A < Async =
        Scope.run {
            for
                root    <- ReactiveUI.normalize(ui, Seq.empty)
                initial <- kyo.internal.MountDispatch.live.let(root.mountDispatch)(HtmlRenderer.render(ui, Seq.empty))
                screen = new Screen(initial)
                sub <- ReactiveUI.subscribe(root, screen)
                a   <- f(screen, sub.handle)
            yield a
        }

end UIScreen

private[kyo] object UIScreen:

    /** Collects the warnings a run logged. */
    final class Warnings extends Log.Unsafe:
        private val seen = new java.util.concurrent.ConcurrentLinkedQueue[String]

        private val failures = new java.util.concurrent.ConcurrentLinkedQueue[String]

        def lines: Chunk[String]  = Chunk.from(seen.toArray(Array.empty[String]))
        def errors: Chunk[String] = Chunk.from(failures.toArray(Array.empty[String]))

        def level: Log.Level                   = Log.Level.warn
        def name: String                       = "warnings"
        def withName(name: String): Log.Unsafe = this

        def trace(msg: => String)(using Frame, AllowUnsafe): Unit                  = ()
        def trace(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit = ()
        def debug(msg: => String)(using Frame, AllowUnsafe): Unit                  = ()
        def debug(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit = ()
        def info(msg: => String)(using Frame, AllowUnsafe): Unit                   = ()
        def info(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit  = ()
        def warn(msg: => String)(using Frame, AllowUnsafe): Unit                   = discard(seen.add(msg))
        def warn(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit  = discard(seen.add(msg))
        def error(msg: => String)(using Frame, AllowUnsafe): Unit                  = discard(failures.add(msg))
        def error(msg: => String, t: => Throwable)(using Frame, AllowUnsafe): Unit = discard(failures.add(msg))
    end Warnings

end UIScreen
