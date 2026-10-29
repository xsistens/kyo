package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Terminal]]'s history under every way it can be bound: unbound it is the terminal's own, a constant nobody hears is
  * fixed, a signal is followed and a commit only reported, and a ref is written before the report.
  */
class TerminalTest extends UicTest:

    private def shell(using Frame) = uic.Terminal().commandHandler(cmd => s"ran $cmd")

    private def promptOf(t: UI)(using Frame): UI.Ast.Input < Sync =
        elementWithClass(t, "p-terminal-prompt-value").map {
            case i: UI.Ast.Input => i
            case other           => throw new AssertionError(s"the prompt is not an input: $other")
        }

    private def commit(i: UI.Ast.Input, line: String)(using Frame): Any < Async =
        i.onChange match
            case Present(f) => handled(f(line))
            case Absent     => throw new AssertionError("the prompt takes no command")

    private val ls = uic.TerminalCommand("ls", "ran ls")

    "an unbound terminal with a handler keeps the history itself, so a commit shows" in {
        for
            seen  <- Signal.initRef(List.empty[Seq[uic.TerminalCommand]])
            after <- publishedAfterReady(
                shell.onCommandsChange(h => seen.updateAndGet(_ :+ h).unit).render,
                t => promptOf(t).map(_.onChange.isDefined),
                until = t => elementsWithClass(t, "p-terminal-command").map(_.nonEmpty)
            )(t => promptOf(t).map(i => commit(i, "ls")))
            s <- seen.get
        yield
            assert(after.nonEmpty, "the committed command never showed")
            assert(s == List(Seq(ls)), s"reported $s")
    }

    "a constant nobody hears is fixed: the history shows, and the prompt takes no command" in {
        for
            in   <- promptOf(shell.commands(Seq(ls)).render)
            rows <- elementsWithClass(shell.commands(Seq(ls)).render, "p-terminal-command")
            card <- elementsWithClass(shell.commands(Seq(ls)).render, "p-uic-key-error")
        yield
            assert(rows.size == 1, "the history shows")
            assert(in.onChange.isEmpty && in.readOnly.contains(true), "the prompt is read-only")
            assert(card.isEmpty, s"a fixed history is not a mistake: ${card.map(textOf)}")
    }

    "a history bound one way is followed, and a commit is reported without being written" in {
        for
            held <- Signal.initRef(Seq.empty[uic.TerminalCommand])
            seen <- Signal.initRef(Seq.empty[uic.TerminalCommand])
            t    <- publishedAfterReady(
                shell.commands(held.readOnly).onCommandsChange(seen.set).render,
                t => promptOf(t).map(_.onChange.isDefined)
            )()
                .map(_.getOrElse(throw new AssertionError("never mounted")))
            in   <- promptOf(t)
            _    <- commit(in, "ls")
            h    <- held.get
            s    <- seen.get
            deaf <- elementsWithClass(shell.commands(held.readOnly).render, "p-uic-key-error")
        yield
            assert(h.isEmpty && s == Seq(ls), s"held $h, reported $s")
            assert(deaf.size == 1 && textOf(deaf.head).contains("onCommandsChange"), s"nobody hears: ${deaf.map(textOf)}")
        end for
    }

    "a ref is written before the terminal reports it" in {
        for
            held <- Signal.initRef(Seq.empty[uic.TerminalCommand])
            seen <- Signal.initRef((Seq.empty[uic.TerminalCommand], Seq.empty[uic.TerminalCommand]))
            t    <- publishedAfterReady(
                shell.commands(held).onCommandsChange(h => held.get.map(now => seen.set((h, now)))).render,
                t => promptOf(t).map(_.onChange.isDefined)
            )().map(_.getOrElse(throw new AssertionError("never mounted")))
            in <- promptOf(t)
            _  <- commit(in, "ls")
            s  <- seen.get
        yield assert(s == (Seq(ls), Seq(ls)), s"reported, held: $s")
    }

end TerminalTest
