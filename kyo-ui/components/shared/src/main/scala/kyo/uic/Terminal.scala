package kyo.uic

import kyo.*
import kyo.UI.*

/** One executed command of a [[Terminal]]: the typed `command` and the handler's
  * `response`.
  */
final case class TerminalCommand(command: String, response: String) derives CanEqual

/** Terminal — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * Terminal anatomy: `div.p-terminal.p-component` > the optional
  * `div.p-terminal-welcome-message` > `div.p-terminal-command-list` of
  * `div.p-terminal-command` rows (`span.p-terminal-prompt-label` +
  * `span.p-terminal-command-value` + `div.p-terminal-command-response`) >
  * `div.p-terminal-prompt` with the prompt label and the bare
  * `input.p-terminal-prompt-value`), so the extracted `@primeuix` terminal CSS
  * applies verbatim.
  *
  * The HISTORY is the model: `commands` binds a
  * `SignalRef[Seq[TerminalCommand]]`. Committing a line (Enter — the input's
  * native change commit) runs `commandHandler(cmd): String < Async` — a real
  * effect, evaluated wherever the UI runs (in server-push transports that is the
  * SERVER, which makes Terminal the natural server-push showcase) — and appends
  * the `TerminalCommand` to the ref; the re-render clears the input. Prime's
  * TerminalService event bus is replaced by the effect-typed handler — same
  * contract, no global bus.
  *
  * Auto-scroll (mounted7 self-addressing): the newest history row carries a
  * session-unique id (minted via `Commands.freshId` in the `UI.mounted` region);
  * after each append a post-insertion mount on that row drives the Local
  * fire-and-forget `UI.scrollIntoView(newestId)`, so a long transcript keeps the
  * latest output in view exactly as Prime's client scroll does. Fire-and-forget is
  * the right door here: a dropped auto-scroll self-corrects on the next append and
  * needs no reply — unlike Form's submit-time reveal, which couples scroll with
  * focus and uses the guaranteed `Commands` channel. The static projection (before
  * the transport attaches) renders no id and does not scroll.
  */
final case class Terminal private (
    welcomeMessageV: Maybe[TextValue] = Absent,
    promptV: String = "$",
    commandsRef: Maybe[SignalRef[Seq[TerminalCommand]]] = Absent,
    handlerF: Maybe[String => String < Async] = Absent,
    accessibleNameV: Maybe[TextValue] = Absent
) extends Node:
    type Self = Terminal

    /** Text of the `div.p-terminal-welcome-message` above the history. */
    def welcomeMessage(v: String): Terminal = copy(welcomeMessageV = Present(TextValue.Const(v)))

    /** Reactive welcome message tracking `sig` — re-renders in place on emission. */
    def welcomeMessage(sig: Signal[String]): Terminal = copy(welcomeMessageV = Present(TextValue.Dyn(sig)))

    /** The prompt label (default `$`), shown before the input and each history row. */
    def prompt(v: String): Terminal = copy(promptV = v)

    /** Binds the command history two-way: committed commands append here, external
      * writes re-render the list (write `Seq.empty` to clear the screen).
      */
    def commands(ref: SignalRef[Seq[TerminalCommand]]): Terminal = copy(commandsRef = Present(ref))

    /** The command interpreter — an effect: the committed line in, the response
      * text out. Runs where the UI runs (server-side under server-push).
      */
    def commandHandler(f: String => String < Async): Terminal = copy(handlerF = Present(f))

    /** `aria-label` for the terminal region. */
    def accessibleName(v: String): Terminal = copy(accessibleNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Terminal = copy(accessibleNameV = Present(TextValue.Dyn(sig)))

    private[uic] def render(using Frame): UI =
        (commandsRef, handlerF) match
            case (Present(r), Present(_)) =>
                // Auto-scroll needs a stable id for the newest row — mint one from the
                // session `Commands` (`freshId`) once in a mounted effect, then the
                // reactive body reads it. The scroll ITSELF goes through the Local
                // fire-and-forget door (`UI.scrollIntoView`), not `Commands.scrollIntoViewId`:
                // a missed auto-scroll is harmless (the next append re-scrolls) and the op
                // needs no return channel or ordering with any other command, so it wants
                // the lost-op-is-fine transport — see the A+ two-door contract on UI.Commands.
                // The post-insertion mount below already guarantees the row exists when the
                // command lands, which is exactly the discipline the fire-and-forget door needs.
                UI.mounted {
                    for
                        cmds   <- UI.commands
                        lastId <- cmds.freshId
                    yield wired(r, lastId, id => UI.scrollIntoView(id))
                }.placeholder(r.render(h => body(h, Absent, Absent)))
            case (Present(r), _) => r.render(h => body(h, Absent, Absent))
            case _               => body(Seq.empty, Absent, Absent)

    /** The scrolling subscription the mount publishes — the golden seam.
      * `scrollTo` scrolls the row with the given id into view (production: the Local
      * fire-and-forget `UI.scrollIntoView`; tests inject a fake to observe the seam).
      */
    private[uic] def wired(ref: SignalRef[Seq[TerminalCommand]], lastId: String, scrollTo: String => Any < Async)(using Frame): UI =
        ref.render(h => body(h, Present(lastId), Present(scrollTo)))

    private def body(history: Seq[TerminalCommand], lastId: Maybe[String], scrollTo: Maybe[String => Any < Async])(using Frame): UI =
        val welcome: List[UI] =
            welcomeMessageV.toList.map {
                case TextValue.Const(t) => div.cssClass("p-terminal-welcome-message")(t): UI
                case TextValue.Dyn(s)   => s.render(t => div.cssClass("p-terminal-welcome-message")(t))
            }

        val lastIdx = history.size - 1
        val rows: List[UI] = history.toList.zipWithIndex.map { (c, i) =>
            var row = div.cssClass("p-terminal-command")
            // Stamp the newest row + give it an invisible mount that scrolls IT into
            // view once it is actually in the DOM. Doing this from the append handler
            // races the re-render (the scroll op can reach the client before the row
            // is inserted); a mount on the freshly inserted last row fires AFTER the
            // insertion, so the scroll always lands on the real newest row.
            val scrollMount: List[UI] =
                if i == lastIdx then
                    (lastId, scrollTo) match
                        case (Present(id), Present(f)) =>
                            List(UI.mounted(f(id).andThen(UI.empty)).placeholder(UI.empty))
                        case _ => Nil
                else Nil
            if i == lastIdx then lastId.foreach(id => row = row.id(id))
            row(
                (span.cssClass("p-terminal-prompt-label")(promptV) ::
                    span.cssClass("p-terminal-command-value")(c.command) ::
                    div.cssClass("p-terminal-command-response").aria("live", "polite")(c.response) ::
                    scrollMount).map(toChild)*
            )
        }
        val list: UI = div.cssClass("p-terminal-command-list")(rows.map(toChild)*)

        // The input value is fixed at "" — committing a line appends to the bound
        // history ref, and the resulting re-render recreates the input empty.
        var field = input
            .cssClass("p-terminal-prompt-value")
            .value("")
            .aria("label", "Terminal command")
        if handlerF.isDefined && commandsRef.isDefined then field = field.onChange(execute)
        val promptRow: UI = div.cssClass("p-terminal-prompt")(
            span.cssClass("p-terminal-prompt-label")(promptV),
            toChild(field)
        )

        var root = div.cssClass("p-terminal").cssClass("p-component")
        accessibleNameV match
            case Present(TextValue.Const(v)) => root = root.aria("label", v)
            case Present(TextValue.Dyn(s))   => root = root.aria("label", s)
            case Absent                      => ()
        end match
        root(((welcome :+ list :+ promptRow)).map(toChild)*)
    end body

    /** Runs the handler effect on the committed line and appends the executed
      * command to the bound history. The newest row scrolls itself into view via
      * its own on-mount effect (see [[body]]) — reliable because it fires AFTER
      * the row is inserted, unlike scrolling from here (which races the render).
      */
    private def execute(line: String)(using Frame): Any < Async =
        val cmd = line.trim
        if cmd.isEmpty then ()
        else
            (handlerF, commandsRef) match
                case (Present(h), Present(ref)) =>
                    for
                        response <- h(cmd)
                        _        <- ref.getAndUpdate(_ :+ TerminalCommand(cmd, response))
                    yield ()
                case _ => ()
        end if
    end execute
end Terminal

object Terminal:
    def apply(): Terminal = new Terminal()
