package kyo.apollo.network

/** Tests the portable [[Uuid]] generator: canonical RFC-4122 v4 rendering and
  * distinctness across constructions (the reason `java.util.UUID` could not be
  * used on Scala.js).
  */
class UuidSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val v4 =
        "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$".r

    "Uuid" - {

        "random() renders a canonical 8-4-4-4-12 v4 UUID" in {
            val id = Uuid.random().value
            assert(v4.matches(id), s"not a canonical v4 UUID: $id")
        }

        "successive UUIDs differ" in {
            assert(Uuid.random() != Uuid.random())
        }
    }
end UuidSpec
