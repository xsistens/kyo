package kyo.internal

import kyo.*

/** Server → client operation. */
sealed private[kyo] trait HtmlOp derives CanEqual

private[kyo] object HtmlOp:
    case class Replace(path: Seq[String], html: String) extends HtmlOp derives Schema
    case class Remove(path: Seq[String])                extends HtmlOp derives Schema
    case class InjectCss(css: String)                   extends HtmlOp derives Schema
    // Unit 2 (measure round-trip): instructs the client to measure the element at `path` and reply with a
    // UIEvent.Measure. Emitted imperatively via UI.Commands.requestMeasure, outside the normal render diff.
    case class RequestMeasure(path: Seq[String]) extends HtmlOp derives Schema
    // Unit 3 (imperative command): a whitelisted verb the client applies to the element at `path`. v1 verbs:
    // "focus" (element.focus()) and "scrollIntoView" (element.scrollIntoView({block:"nearest"})). Unknown verbs
    // are ignored by the client (forward-compat). Emitted imperatively via UI.Commands.command.
    case class Command(path: Seq[String], verb: String) extends HtmlOp derives Schema
    // Self-addressing: id-addressed twins of Command / RequestMeasure. The client resolves the target with
    // document.getElementById(id) instead of the data-kyo-path querySelector, so a reusable component that minted
    // an id via UI.Commands.freshId and stamped it on its own element (id is a settable prop) can drive commands
    // at itself without ever learning its structural render path. Same verb whitelist as Command.
    case class CommandById(id: String, verb: String) extends HtmlOp derives Schema
    // Id-addressed twin of RequestMeasure (see above). The client measures getElementById(id) and replies with a
    // UIEvent.MeasureById carrying the same id, which the transport routes to UI.Commands.deliverMeasureById.
    case class RequestMeasureById(id: String) extends HtmlOp derives Schema
end HtmlOp
