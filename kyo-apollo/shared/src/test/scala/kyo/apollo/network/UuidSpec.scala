package kyo.apollo.network

import kyo.*

/** Tests the portable [[Uuid]] generator: canonical RFC-4122 v4 rendering,
  * distinctness across mints (the reason `java.util.UUID` could not be used on
  * Scala.js), and that minting is an effect over the ambient [[kyo.Random]].
  */
class UuidSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val v4 =
        "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$".r

    "Uuid" - {

        "random renders a canonical 8-4-4-4-12 v4 UUID" in {
            Uuid.random.map(id => assert(v4.matches(id.value), s"not a canonical v4 UUID: $id"))
        }

        "successive UUIDs differ" in {
            Kyo.zip(Uuid.random, Uuid.random).map((a, b) => assert(a != b))
        }

        "Random.withSeed makes the minted sequence reproducible" in {
            val mintTwo = Kyo.zip(Uuid.random, Uuid.random)
            Kyo.zip(Random.withSeed(42)(mintTwo), Random.withSeed(42)(mintTwo)).map { (first, second) =>
                assert(first == second)
                assert(first._1 != first._2)
            }
        }
    }
end UuidSpec
