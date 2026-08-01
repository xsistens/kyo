package kyo

/** Base type for kyo-ui errors, so callers can distinguish UI failures from arbitrary `KyoException`s. */
class UIException(message: => String = "", cause: String | Throwable = "")(using Frame) extends KyoException(message, cause)

/** Two [[kyo.UI.mounted]] nodes claimed the same key in one render pass: a caller bug, not a mount failure. The second slot never
  * mounts; it renders through the session's [[kyo.UI.MountErrorPolicy]] carrying this failure, so the app decides how the bug shows
  * up in the page.
  */
class DuplicateMountKey(val key: Any)(using Frame) extends UIException(DuplicateMountKey.text(key))

object DuplicateMountKey:
    /** The undecorated one-liner, used both as the exception's message and as [[kyo.UI.MountError.message]]. The
      * distinction matters: `KyoException.getMessage` is the frame-decorated DEVELOPMENT rendering, which belongs in
      * a log and not in a page.
      */
    private[kyo] def text(key: Any): String = s"kyo-ui: duplicate mounted key '$key'"
end DuplicateMountKey
