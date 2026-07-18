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
    // Unit 8 (in-place reactive patching): toggle a class on getElementById(id) WITHOUT replacing the element, so CSS
    // transitions on that class fire (a full outerHTML replace loses the from-state). Client: el.classList.toggle(
    // className, on) — the boolean-force form, so the op is idempotent with any baked-in initial class. Emitted by
    // UI.Commands.bindClassById, which drives it from a Signal[Boolean].
    case class SetClassById(id: String, className: String, on: Boolean) extends HtmlOp derives Schema
    // Unit 8: merge an inline-style declaration string ("prop:val;prop:val") into getElementById(id) WITHOUT replacing
    // the element. Client parses each `prop:val` and setProperty's it, so it MERGES over other inline props rather than
    // clobbering them (unlike a full style="" replace). `css` is a serialized kyo.Style (CssStyleRenderer.render).
    // Emitted by UI.Commands.bindStyleById from a Signal[Style].
    case class SetStyleById(id: String, css: String) extends HtmlOp derives Schema
    // Unit 8 (viewport observation): begin continuously measuring getElementById(id). The client measures now and
    // attaches capturing scroll + resize listeners keyed by id; each fires a UIEvent.MeasureById(id) reply, routed to a
    // PERSISTENT sink in UI.Commands.observers (kept, not consumed). Emitted by UI.Commands.observeViewportById.
    case class ObserveViewportById(id: String) extends HtmlOp derives Schema
    // Unit 8: stop the observation started by ObserveViewportById(id) — the client detaches the scroll/resize listeners
    // registered for id. Emitted by observeViewportById's scope finalizer (which also drops the server-side sink).
    case class UnobserveViewportById(id: String) extends HtmlOp derives Schema
end HtmlOp
