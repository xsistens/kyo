package kyo.apollo.network

import kyo.<
import kyo.Frame
import kyo.Random
import kyo.Sync

/** A minimal RFC-4122 version-4 UUID, portable to Scala.js.
  *
  * `java.util.UUID.randomUUID()` cannot be used here: the Scala.js linker
  * rejects it because it references `java.security.SecureRandom`, which has no
  * JS implementation. Requests therefore carry this small opaque value type as
  * their correlation id, mirroring apollo-kotlin's `Uuid` / `uuid4()`. It is
  * rendered as the canonical `8-4-4-4-12` lowercase-hex string.
  */
final case class Uuid(value: String):
    override def toString: String = value

object Uuid:

    /** A fresh random v4 UUID, drawn from the ambient [[kyo.Random]]. Minting is an
      * effect: it happens when the computation runs, not when it is built, and
      * `Random.withSeed` makes the ids a computation mints reproducible.
      */
    def random(using Frame): Uuid < Sync =
        Random.nextBytes(16).map(fromBytes)

    /** The v4 rendering of 16 random bytes, with the version (4) and IETF variant
      * bits pinned per RFC 4122 §4.4.
      */
    private def fromBytes(random: Seq[Byte]): Uuid =
        val bytes = random.toArray
        bytes(6) = ((bytes(6) & 0x0f) | 0x40).toByte
        bytes(8) = ((bytes(8) & 0x3f) | 0x80).toByte
        val hex = bytes.map(b => f"${b & 0xff}%02x").mkString
        Uuid(
            s"${hex.substring(0, 8)}-${hex.substring(8, 12)}-${hex.substring(12, 16)}-" +
                s"${hex.substring(16, 20)}-${hex.substring(20, 32)}"
        )
    end fromBytes
end Uuid
