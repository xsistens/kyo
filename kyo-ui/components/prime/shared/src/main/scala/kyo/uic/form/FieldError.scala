package kyo.uic.form

import kyo.*

/** A validation failure as data, never as text. `code` is a flat key (e.g.
  * "required", "min-length", "username-taken"); `args` feed interpolation
  * ("min" -> "5"); `fallback` is a literal shown verbatim when no translator
  * resolves the code. The validation core never turns a `code` into a message;
  * an app-supplied [[ErrorTranslator]] does, optionally through i18n.
  *
  * Text that needs no code at all (a server's own sentence, a rule's
  * [[Rule.message]]) rides in `fallback`; [[FieldError.message]] builds the
  * former under the one code reserved for it.
  *
  * `derives CanEqual` is load-bearing: the reactive error is a
  * `Signal[Maybe[FieldError]]`, and every `Signal[A]` requires `CanEqual[A, A]`.
  */
final case class FieldError(
    code: String,
    args: Map[String, String] = Map.empty,
    fallback: Maybe[String] = Absent
) derives CanEqual

object FieldError:

    /** The code of an error that is nothing but its text ([[message]]). A translator table should not carry it: an
      * entry here would replace every such text with one sentence.
      */
    val messageCode: String = "message"

    /** An error that is its text: what a server said, or what a submit handler wants the reader to read, with no code to
      * invent for it. [[ErrorTranslator.default]] and [[ErrorTranslator.englishMap]] show `text` as it is.
      * [[Form.raise]] and [[FormField.setError]] take the text directly and build this.
      */
    def message(text: String): FieldError = FieldError(messageCode, fallback = Present(text))
end FieldError
