package kyo.uic.form

import kyo.*

/** A validation failure as data, never as text. `code` is a flat key (e.g.
  * "required", "min-length", "username-taken"); `args` feed interpolation
  * ("min" -> "5"); `fallback` is a literal shown verbatim when no translator
  * resolves the code. The validation core never turns a `code` into a message —
  * an app-supplied [[ErrorTranslator]] does, optionally through i18n.
  *
  * `derives CanEqual` is load-bearing: the reactive error is a
  * `Signal[Maybe[FieldError]]`, and every `Signal[A]` requires `CanEqual[A, A]`.
  */
final case class FieldError(
    code: String,
    args: Map[String, String] = Map.empty,
    fallback: Maybe[String] = Absent
) derives CanEqual
