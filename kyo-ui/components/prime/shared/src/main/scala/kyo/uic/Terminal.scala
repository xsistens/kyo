package kyo.uic

import kyo.*
import kyo.UI.*

/** One executed command of a [[Terminal]]: the typed `command` and the handler's
  * `response`.
  */
final case class TerminalCommand(command: String, response: String) derives CanEqual

/** Terminal: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Terminal anatomy: `div.p-terminal.p-component` > the optional
  * `div.p-terminal-welcome-message` > `div.p-terminal-command-list` of
  * `div.p-terminal-command` rows (`span.p-terminal-prompt-label` +
  * `span.p-terminal-command-value` + `div.p-terminal-command-response`) >
  * `div.p-terminal-prompt` with the prompt label and the bare
  * `input.p-terminal-prompt-value`), so the extracted `@primeuix` terminal CSS
  * applies verbatim.
  *
  * The HISTORY is the model: `commands` binds a `Seq[TerminalCommand]` (a
  * constant, a signal or a ref), and without a binding a terminal with a handler
  * keeps it itself. Committing a line (Enter, the input's
  * native change commit) runs `commandHandler(cmd): String < Async` (a real
  * effect, evaluated wherever the UI runs; in server-push transports that is the
  * SERVER, which makes Terminal the natural server-push showcase) and asks for
  * the `TerminalCommand` appended, reported to `onCommandsChange`; the re-render
  * clears the input. Prime's
  * TerminalService event bus is replaced by the effect-typed handler: same
  * contract, no global bus.
  *
  * Auto-scroll (self-addressing): the prompt, the last thing in the terminal, carries an
  * id (from `UI.id` in the `UI.mounted` region), and after each append a `whileMounted`
  * on the newest history row sends `scrollIntoViewId` for it, so a long transcript ends
  * on the latest output and the prompt below it, as Prime's client scroll does. That
  * command scrolls to the nearest edge, so the page around the terminal stays where it
  * is while the terminal itself is in view; `UI.scrollIntoView` scrolls to the block's
  * start, and moved the page with every line. A static render runs no effect.
  */
final class Terminal private[uic] (private[uic] val p: Terminal.Props) extends Node, HasAccessibleName, HasProps:
    import p.*

    type Self = Terminal

    /** Text of the `div.p-terminal-welcome-message` above the history. A `Signal[String]` re-renders it in
      * place on emission.
      */
    def welcomeMessage(v: String | Signal[String]): Terminal = new Terminal(p.copy(welcomeMessageV = Present(ReactiveValue(v))))

    /** The prompt label (default `$`), shown before the input and each history row. */
    def prompt(v: String): Terminal = new Terminal(p.copy(promptV = v))

    /** Binds the command history. A `SignalRef` binds two way: a committed command is appended to it, then
      * [[onCommandsChange]] hears the history, and external writes re-render the list (write `Seq.empty` to clear the
      * screen). A plain `Signal` binds one way: the list follows it and a commit is only reported. A constant nobody
      * hears is fixed: the history shows, and the prompt takes no command. Unbound, a terminal with a [[commandHandler]]
      * keeps the history itself. A signal nothing hears is reported in a card.
      */
    def commands(v: Seq[TerminalCommand] | Signal[Seq[TerminalCommand]]): Terminal =
        new Terminal(p.copy(commandsSlot = commandsSlot.bind(v)))

    /** Fired with the history the reader's commit asks for (the history with the new command appended), after a
      * two-way [[commands]] is written.
      */
    def onCommandsChange(f: Seq[TerminalCommand] => UI.Action)(using Frame): Terminal =
        new Terminal(p.copy(commandsSlot = commandsSlot.hear(f)))

    /** A constant nobody hears is fixed: the history shows, and the prompt takes no command. */
    private def fixed: Boolean = commandsSlot.fixed

    // A history compares by its commands, which are plain data.
    private given CanEqual[Seq[TerminalCommand], Seq[TerminalCommand]] = CanEqual.derived

    /** The command interpreter, an effect: the committed line in, the response
      * text out. Runs where the UI runs (server-side under server-push).
      */
    def commandHandler(f: String => String < Async): Terminal = new Terminal(p.copy(handlerF = Present(f)))

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new Terminal(p.copy(access = v))
    private[uic] def render(using Frame): UI                   =
        // Without a handler nothing can be committed, so the terminal only shows its history.
        val static: UI = commandsSlot.reading(Seq.empty).render(h => body(h, Absent, Absent))
        handlerF match
            case Absent     => static
            case Present(_) =>
                // Auto-scroll needs a stable id for the prompt, drawn once here, which the reactive body reads.
                val terminal = UI.mounted {
                    for
                        endId <- UI.id
                        // Unbound, the terminal keeps the history itself.
                        history <- commandsSlot.owned(Seq.empty[TerminalCommand])
                        // Counts the repaints a one-way history asks for, so a commit the owner declines clears the prompt.
                        paints <- Signal.initRef(0)
                    yield new Terminal(p.copy(commandsSlot = history)).wiredOver(
                        endId,
                        id => UI.commands.map(_.scrollIntoViewId(id)),
                        Present(paints)
                    )
                }
                // A history bound one way that nothing hears: every commit reaches nobody.
                Diagnostics.before(terminal)("Terminal")(Diagnostic.unheard(commandsSlot, Terminal.commandsName))
        end match
    end render

    /** The scrolling subscription the mount publishes: the golden seam.
      * `scrollTo` scrolls the element with the given id into view (production: `scrollIntoViewId`; tests
      * inject a fake to observe the seam).
      */
    private[uic] def wired(ref: SignalRef[Seq[TerminalCommand]], endId: String, scrollTo: String => UI.Action)(using Frame): UI =
        new Terminal(p.copy(commandsSlot = commandsSlot.bind(ref))).wiredOver(endId, scrollTo, Absent)

    /** [[wired]] over the history however it is bound. */
    private def wiredOver(
        endId: String,
        scrollTo: String => UI.Action,
        paints: Maybe[SignalRef[Int]]
    )(using Frame): UI =
        val painted            = ReactiveValue.orElse(paints, 0)
        val repaint: UI.Action = paints.fold[UI.Action](())(_.updateAndGet(_ + 1).unit)
        // The paint count is read and dropped: it is there so a repaint draws the prompt empty again.
        commandsSlot.reading(Seq.empty).and(painted).render((h, _) => body(h, Present(endId), Present(scrollTo), repaint))
    end wiredOver

    private def body(
        history: Seq[TerminalCommand],
        endId: Maybe[String],
        scrollTo: Maybe[String => UI.Action],
        repaint: UI.Action = ()
    )(using Frame): UI =
        val welcome: List[UI] =
            welcomeMessageV.toList.map {
                case TextValue.Const(t) => div.cssClass("p-terminal-welcome-message")(t): UI
                case TextValue.Dyn(s)   => div.cssClass("p-terminal-welcome-message")(s)
            }

        val lastIdx        = history.size - 1
        val rows: List[UI] = history.toList.zipWithIndex.map { (c, i) =>
            var row = div.cssClass("p-terminal-command")
            // The newest row brings the terminal's end into view once it is actually in the DOM. Doing this from the append handler
            // races the re-render (the scroll op can reach the client before the row is inserted); an effect on the freshly inserted last
            // row runs AFTER the insertion, so the scroll always lands below the real newest row. It belongs to the row's place, so the
            // next append, which makes another row the last, scrolls again.
            if i == lastIdx then
                (endId, scrollTo) match
                    case (Present(id), Present(f)) => row = row.whileMounted(f(id))
                    case _                         => ()
            end if
            row(
                UI.children(span.cssClass("p-terminal-prompt-label")(promptV) ::
                    span.cssClass("p-terminal-command-value")(c.command) ::
                    div.cssClass("p-terminal-command-response").aria("live" -> "polite")(c.response) :: Nil)*
            )
        }
        val list: UI = div.cssClass("p-terminal-command-list")(UI.children(rows)*)

        // The input value is fixed at "": committing a line appends to the bound
        // history ref, and the resulting re-render recreates the input empty.
        var field = input
            .cssClass("p-terminal-prompt-value")
            .value("")
            .aria("label" -> "Terminal command")
        if fixed then field = field.readOnly(true)
        else if handlerF.isDefined && commandsSlot.binding.isDefined then field = field.onChange(line => execute(line, repaint))
        var promptDiv = div.cssClass("p-terminal-prompt")
        endId.foreach(id => promptDiv = promptDiv.id(id))
        val promptRow: UI = promptDiv(
            span.cssClass("p-terminal-prompt-label")(promptV),
            field
        )

        var root = div.cssClass("p-terminal").cssClass("p-component")
        access.name.foreach(v => root = root.aria("label" -> v))
        root(UI.children((welcome :+ list :+ promptRow))*)
    end body

    /** Runs the handler effect on the committed line and appends the executed
      * command to the bound history. The newest row brings the terminal's end into view via
      * its own effect (see [[body]]), reliable because it fires AFTER
      * the row is inserted, unlike scrolling from here (which races the render).
      */
    private def execute(line: String, repaint: UI.Action)(using Frame): UI.Action =
        val cmd = line.trim
        if cmd.isEmpty then ()
        else
            handlerF match
                case Present(h) =>
                    for
                        response <- h(cmd)
                        _        <- commandsSlot.update(_ :+ TerminalCommand(cmd, response))
                        // One way, the owner may not take the command: the prompt is drawn empty again either way.
                        _ <- if commandsSlot.binding.isDefined && commandsSlot.writable.isEmpty then repaint else (): UI.Action
                    yield ()
                case Absent => ()
        end if
    end execute
end Terminal

object Terminal:
    final private[uic] case class Props(
        welcomeMessageV: Maybe[TextValue] = Absent,
        promptV: String = "$",
        commandsSlot: Controlled[Seq[TerminalCommand]] = Controlled[Seq[TerminalCommand]](),
        handlerF: Maybe[String => String < Async] = Absent,
        access: Accessibility = Accessibility.none
    )

    private val commandsName =
        Controlled.Name("Terminal", "the history", "a command", "onCommandsChange", consequence = "the reader's commit reaches nobody")
    def apply(): Terminal = new Terminal(Props())
end Terminal
