package kyo.internal

import kyo.*

/** Mouse event payload on the wire. Reconstructed into UI.MouseEvent on the server. */
final private[kyo] case class MouseEventData(
    modifiers: UI.Modifiers,
    targetId: Maybe[String]
) derives CanEqual, Schema

/** Keyboard event payload on the wire. Reconstructed into UI.KeyboardEvent on the server. */
final private[kyo] case class KeyboardEventData(
    key: String,
    modifiers: UI.Modifiers,
    targetId: Maybe[String]
) derives CanEqual, Schema

/** Client -> server event. Typed per event kind. */
private[kyo] enum UIEvent derives CanEqual, Schema:
    def path: Seq[String]
    case Click(path: Seq[String], mouse: MouseEventData)
    case ClickSelf(path: Seq[String], mouse: MouseEventData)
    case ContextMenu(path: Seq[String], mouse: MouseEventData)
    case Input(path: Seq[String], value: String)
    case Change(path: Seq[String], value: String)
    // Unit 2: full file-metadata selection (name/size/MIME/content per file). Payload is UI.FilePayload,
    // which derives Schema, so it rides the wire directly (mirroring how UI.Modifiers is used above).
    case FileSelect(path: Seq[String], files: Seq[UI.FilePayload])
    case ChangeChecked(path: Seq[String], checked: Boolean)
    case ChangeNumeric(path: Seq[String], value: Double)
    case Submit(path: Seq[String], mouse: MouseEventData)
    case KeyDown(path: Seq[String], keyboard: KeyboardEventData)
    case KeyUp(path: Seq[String], keyboard: KeyboardEventData)
    case Focus(path: Seq[String], mouse: MouseEventData)
    case Blur(path: Seq[String], mouse: MouseEventData)
    case Scroll(path: Seq[String], deltaX: Double, deltaY: Double, modifiers: UI.Modifiers, targetId: Maybe[String])
    case Hover(path: Seq[String], mouse: MouseEventData)
    case Unhover(path: Seq[String], mouse: MouseEventData)
    // Unit 1: pointer/drag events. `pointer` is UI.PointerEvent, which derives Schema, so (like UI.FilePayload
    // for FileSelect) it rides the wire directly. Coordinates are pre-normalized by the client (position relative
    // to the target plus its bounding rect) so the server can map to 0..1 without another round-trip. Bubble like Click.
    case PointerDown(path: Seq[String], pointer: UI.PointerEvent)
    case PointerMove(path: Seq[String], pointer: UI.PointerEvent)
    case PointerUp(path: Seq[String], pointer: UI.PointerEvent)
    // Unit 2: the client's reply to HtmlOp.RequestMeasure — the measured element rect plus the viewport size.
    // Dispatched inbound like any event, but routed by the transport to the session's UI.Commands (not the
    // element handler tree): it resolves a pending requestMeasure rather than an onXxx handler.
    case Measure(
        path: Seq[String],
        rectX: Double,
        rectY: Double,
        rectW: Double,
        rectH: Double,
        viewportW: Double,
        viewportH: Double
    )
    // Self-addressing: the client's reply to HtmlOp.RequestMeasureById. Carries the `id` (not a render path), so the
    // transport can resolve it against the session's id-keyed pending map via UI.Commands.deliverMeasureById. Routed
    // by type in UIServer.dispatchEvent exactly like Measure — so `path` is never read for this case; it exists only
    // to satisfy the enum's abstract `path` member and the client always sends it empty.
    case MeasureById(
        path: Seq[String],
        id: String,
        rectX: Double,
        rectY: Double,
        rectW: Double,
        rectH: Double,
        viewportW: Double,
        viewportH: Double
    )
end UIEvent
