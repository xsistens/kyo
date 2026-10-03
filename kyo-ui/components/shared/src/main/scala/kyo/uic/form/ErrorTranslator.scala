package kyo.uic.form

import kyo.*

/** Turns a code+args [[FieldError]] into reactive display text. Returns
  * `Signal[String]` because translation may re-render on a locale change (an app's
  * i18n `t(code, args)` typically returns a `Signal[String]`).
  *
  * Provided once at the app root as an `Env` service (mirroring `ToastService`);
  * `Form.mounted` resolves it at build time. A form with no i18n uses [[default]].
  */
trait ErrorTranslator:
    def translate(err: FieldError)(using Frame): Signal[String]

object ErrorTranslator:

    /** Zero-i18n default: show the literal `fallback`, else the raw `code`. */
    val default: ErrorTranslator = new ErrorTranslator:
        def translate(err: FieldError)(using Frame): Signal[String] =
            Signal.initConst(err.fallback.getOrElse(err.code))

    /** A static message table (works with no i18n at all): `code -> message`,
      * falling back to the error's own `fallback`, then the raw `code`.
      */
    def englishMap(table: Map[String, String]): ErrorTranslator = new ErrorTranslator:
        def translate(err: FieldError)(using Frame): Signal[String] =
            Signal.initConst(table.getOrElse(err.code, err.fallback.getOrElse(err.code)))

    /** Wrap an instance as a root `Env` layer, à la `ToastService.layer`. */
    def layer(instance: ErrorTranslator)(using Frame): Layer[ErrorTranslator, Any] =
        Layer(instance)
end ErrorTranslator
