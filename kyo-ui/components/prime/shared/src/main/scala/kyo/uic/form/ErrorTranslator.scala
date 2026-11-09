package kyo.uic.form

import kyo.*

/** Turns a code+args [[FieldError]] into reactive display text. Returns
  * `Signal[String]` because translation may re-render on a locale change (an app's
  * i18n `t(code, args)` typically returns a `Signal[String]`).
  *
  * Provided once at the app root as an `Env` service (mirroring `ToastService`);
  * `Form.mounted` resolves it at build time. A form with no i18n uses [[ErrorTranslator.default]].
  *
  * The precedence every translator here keeps, and an app's own should keep too: the translator's word for the
  * code, else the error's own `fallback` (a rule's [[Rule.message]], a server's text), else the raw code. The
  * translator goes first because it is the one place an app can replace every visible string at once, which a
  * localized app needs; a rule's text is what shows wherever the translator has no word for its code.
  */
trait ErrorTranslator:
    def translate(err: FieldError)(using Frame): Signal[String]

object ErrorTranslator:

    /** Zero-i18n default: show the literal `fallback`, else the raw `code`. A rule with a [[Rule.message]] needs nothing
      * more than this.
      */
    val default: ErrorTranslator = new ErrorTranslator:
        def translate(err: FieldError)(using Frame): Signal[String] =
            Signal.initConst(err.fallback.getOrElse(err.code))

    /** A static message table (works with no i18n at all): `code -> message`,
      * falling back to the error's own `fallback`, then the raw `code`.
      *
      * An entry speaks for its code wherever the code fails, including a rule that carries its own
      * [[Rule.message]]. To keep a table for the default codes and still give one field its own text, give that
      * rule its own code (`Validator.required("comment-empty").message(...)`).
      */
    def englishMap(table: Map[String, String]): ErrorTranslator = new ErrorTranslator:
        def translate(err: FieldError)(using Frame): Signal[String] =
            Signal.initConst(table.getOrElse(err.code, err.fallback.getOrElse(err.code)))

    /** Wrap an instance as a root `Env` layer, à la `ToastService.layer`. */
    def layer(instance: ErrorTranslator)(using Frame): Layer[ErrorTranslator, Any] =
        Layer(instance)
end ErrorTranslator
