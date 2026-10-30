package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Terminal]]'s history holds to rule A ([[ReactiveValueContract]]): unbound it is the terminal's own, a constant nobody
  * hears is fixed, a signal is followed and a commit only reported, and a ref is written before the report. A terminal
  * takes a command only with a handler, and wires its prompt in a mount, so every case starts from one with a handler
  * and acts on what the mount published. The cases after it are what only a terminal does.
  */
class TerminalTest extends ReactiveValueContract:

    // A history compares by its commands, which are plain data.
    private given CanEqual[Seq[uic.TerminalCommand], Seq[uic.TerminalCommand]] = CanEqual.derived

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

    /** The history the newest terminal in `t` shows, row by row. */
    private def history(t: UI)(using Frame): Seq[uic.TerminalCommand] < Sync =
        for
            root <- elementWithClass(t, "p-terminal")
            rows <- elementsWithClass(root, "p-terminal-command")
            cmds <- Kyo.foreach(rows)(row =>
                for
                    cmd  <- elementWithClass(row, "p-terminal-command-value")
                    resp <- elementWithClass(row, "p-terminal-command-response")
                yield uic.TerminalCommand(textOf(cmd), textOf(resp))
            )
        yield cmds.toSeq

    private val ls = uic.TerminalCommand("ls", "ran ls")

    holds(new Slot[uic.Terminal, Seq[uic.TerminalCommand]](
        name = "commands",
        bare = Present(shell),
        bind = shell.commands(_),
        hear = (c, f) => c.onCommandsChange(f),
        event = "onCommandsChange",
        render = _.render,
        start = Seq.empty,
        next = Seq(ls),
        control = promptOf(_),
        shows = history(_),
        gesture = {
            case i: UI.Ast.Input => commit(i, "ls")
            case other           => throw new AssertionError(s"the prompt is not an input: $other")
        },
        answers = {
            case i: UI.Ast.Input => i.onChange.isDefined
            case _               => false
        },
        saysFixed = {
            case i: UI.Ast.Input => i.readOnly.contains(true)
            case _               => false
        },
        // The page as first rendered shows the history with a prompt that takes nothing yet; the mount wires it.
        ready = Present(t => promptOf(t).map(_.onChange.isDefined))
    ))

    "an unbound terminal somebody hears keeps the history itself and reports the commit as well" in {
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

    "a fixed history shows its commands" in {
        history(shell.commands(Seq(ls)).render).map(h => assert(h == Seq(ls), s"shows $h"))
    }

end TerminalTest
