package kyo.apollo.network

import scala.util.Random

/** A minimal RFC-4122 version-4 UUID, portable to Scala.js.
  *
  * `java.util.UUID.randomUUID()` cannot be used here: the Scala.js linker
  * rejects it because it references `java.security.SecureRandom`, which has no
  * JS implementation. Requests therefore carry this small opaque value type as
  * their correlation id, mirroring apollo-kotlin's `Uuid` / `uuid4()`. It is
  * rendered as the canonical `8-4-4-4-12` lowercase-hex string.
  *
  * Randomness comes from [[scala.util.Random]], whose no-arg seed Scala.js
  * derives from `Math.random()`, so ids differ across constructions.
  */
final case class Uuid(value: String):
    override def toString: String = value

object Uuid:

    private val rng = new Random()

    /** A fresh random v4 UUID. */
    def random(): Uuid =
        val bytes = new Array[Byte](16)
        rng.nextBytes(bytes)
        // Pin the version (4) and IETF variant bits per RFC 4122 §4.4.
        bytes(6) = ((bytes(6) & 0x0f) | 0x40).toByte
        bytes(8) = ((bytes(8) & 0x3f) | 0x80).toByte
        val hex = bytes.map(b => f"${b & 0xff}%02x").mkString
        Uuid(
            s"${hex.substring(0, 8)}-${hex.substring(8, 12)}-${hex.substring(12, 16)}-" +
                s"${hex.substring(16, 20)}-${hex.substring(20, 32)}"
        )
    end random
end Uuid
