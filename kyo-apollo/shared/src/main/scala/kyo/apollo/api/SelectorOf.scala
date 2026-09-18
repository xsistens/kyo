package kyo.apollo.api

/** The generated selector object of `Origin`, as a type-class the library's
  * entry points summon: `Fragment.embedded[Image](_.url, _.vibrantColor)` hands
  * each lambda `Image` itself, so it needs `Image.type` from nothing but the
  * marker `Image`. Codegen emits one instance beside every phantom marker
  * (`given SelectorOf.Aux[Image, Image.type] = SelectorOf(Image)`), in the
  * marker's companion, so it is auto-summoned with no import.
  *
  * `S` is a type member rather than a second type parameter so a caller names
  * only `Origin` and the selector type follows from the instance (`sel.S`).
  */
trait SelectorOf[Origin]:
    type S
    def selector: S

object SelectorOf:
    type Aux[Origin, S0] = SelectorOf[Origin] { type S = S0 }

    def apply[Origin, S0](s: S0): Aux[Origin, S0] =
        new SelectorOf[Origin]:
            type S = S0
            val selector: S0 = s
end SelectorOf
