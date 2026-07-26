package kyo.internal

import kyo.*

/** Server → client operation. */
sealed private[kyo] trait HtmlOp derives CanEqual

private[kyo] object HtmlOp:

    // --- Rendering operations ---

    case class Replace(path: Seq[String], html: String)     extends HtmlOp derives Schema
    case class ReplaceRange(regionId: String, html: String) extends HtmlOp derives Schema
    case class Remove(path: Seq[String])                    extends HtmlOp derives Schema
    case class InjectCss(css: String)                       extends HtmlOp derives Schema
    case class ScrollIntoView(id: String)                   extends HtmlOp derives Schema

    // Contracts for these imperative ops live on UI.Commands (requestMeasure / command / *ById).
    case class RequestMeasure(path: Seq[String])        extends HtmlOp derives Schema
    case class Command(path: Seq[String], verb: String) extends HtmlOp derives Schema
    // Id-addressed twins: the client resolves the target by getElementById(id) instead of the data-kyo-path querySelector.
    case class CommandById(id: String, verb: String) extends HtmlOp derives Schema
    case class RequestMeasureById(id: String)        extends HtmlOp derives Schema

    // classList.toggle(className, on) on getElementById(id) without replacing the element (so CSS transitions fire).
    // The boolean-force form makes it idempotent with any baked-in initial class.
    case class SetClassById(id: String, className: String, on: Boolean) extends HtmlOp derives Schema
    // setProperty each declaration of the serialized `css` onto getElementById(id), so it merges over other inline
    // props rather than clobbering them (unlike a full style="" replace).
    case class SetStyleById(id: String, css: String) extends HtmlOp derives Schema
    // set an attribute on getElementById(id) WITHOUT replacing the element, so a reactive attribute (e.g.
    // aria-expanded) tracks a signal in place; a Reactive wrapper would insert a data-kyo-reactive node and
    // break a CSS `>` child-combinator anchored on that element. Emitted by UI.Commands.bindAttrById.
    case class SetAttrById(id: String, name: String, value: String) extends HtmlOp derives Schema
    // Declarative, path-addressed (data-kyo-path) twin of SetAttrById/SetClassById: patch the attribute/class in
    // place (no re-render, no re-mount). The client marks ownership (the __kyoOwn shield) so a parent re-render's
    // morph won't reconcile the live value back. Emitted by the observers started in ReactiveUI.subscribeScoped
    // from Attrs.reactiveAttrs / reactiveBoolAttrs / reactiveClasses; SetClassByPath uses classList.toggle so CSS
    // transitions fire.
    case class SetAttrByPath(path: Seq[String], name: String, value: String)      extends HtmlOp derives Schema
    case class SetBoolAttrByPath(path: Seq[String], name: String, value: Boolean) extends HtmlOp derives Schema
    case class SetClassByPath(path: Seq[String], name: String, on: Boolean)       extends HtmlOp derives Schema
    // Attach capturing scroll+resize listeners to getElementById(id); each reply routes to a PERSISTENT sink in
    // UI.Commands.observers (kept, not consumed like RequestMeasureById).
    case class ObserveViewportById(id: String) extends HtmlOp derives Schema
    // Detach the scroll/resize listeners started by ObserveViewportById(id).
    case class UnobserveViewportById(id: String) extends HtmlOp derives Schema

    // --- Drag operations ---

    /** Requests a bounded byte range from a dropped browser file. */
    final case class ReadDropFile(requestId: String, token: String, offset: ByteSize, maxSize: ByteSize)
        extends HtmlOp derives Schema

    /** Requests a bounded page of metadata from a dropped browser directory. */
    final case class ReadDropDirectory(requestId: String, token: String, cursor: Maybe[String], maxEntries: Int)
        extends HtmlOp derives Schema

    /** Cancels an outstanding dropped file or directory read. */
    final case class CancelDropRead(requestId: String) extends HtmlOp derives Schema

    /** Resolves a browser drag session with the server's acceptance decision. */
    final case class ResolveDrag(sessionId: String, decision: Drag.Decision) extends HtmlOp derives Schema
end HtmlOp
