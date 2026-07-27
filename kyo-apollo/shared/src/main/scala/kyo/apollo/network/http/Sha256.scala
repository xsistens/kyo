package kyo.apollo.network.http

import java.nio.charset.StandardCharsets

/** A small, portable SHA-256 (FIPS 180-4), used to derive the Automatic
  * Persisted Queries `persistedQuery.sha256Hash` from an operation document.
  *
  * Hand-rolled because `java.security.MessageDigest` does not link under
  * Scala.js. This is the faithful analog of apollo-kotlin's runtime APQ hashing
  * (`operation.document().encodeUtf8().sha256().hex()`): the composer hashes the
  * document text on demand rather than relying on a codegen-precomputed id.
  * Verified against the standard `""` and `"abc"` vectors in the test-suite.
  */
object Sha256:

    // First 32 bits of the fractional parts of the cube roots of the first 64
    // primes — the SHA-256 round constants.
    private val K: Array[Int] = Array(
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
        0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
        0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
        0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
        0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
    )

    /** The lowercase-hex SHA-256 digest of `input`'s UTF-8 bytes. */
    def hex(input: String): String =
        digest(input.getBytes(StandardCharsets.UTF_8))
            .map(b => f"${b & 0xff}%02x")
            .mkString

    /** The 32-byte SHA-256 digest of `message`. */
    def digest(message: Array[Byte]): Array[Byte] =
        // Initial hash values: first 32 bits of the fractional parts of the square
        // roots of the first 8 primes.
        var h0 = 0x6a09e667; var h1 = 0xbb67ae85; var h2 = 0x3c6ef372
        var h3 = 0xa54ff53a; var h4 = 0x510e527f; var h5 = 0x9b05688c
        var h6 = 0x1f83d9ab; var h7 = 0x5be0cd19

        val padded = pad(message)
        val w      = new Array[Int](64)

        var block = 0
        while block < padded.length do
            var i = 0
            while i < 16 do
                val j = block + i * 4
                w(i) = ((padded(j) & 0xff) << 24) | ((padded(j + 1) & 0xff) << 16) |
                    ((padded(j + 2) & 0xff) << 8) | (padded(j + 3) & 0xff)
                i += 1
            end while
            while i < 64 do
                val s0 = rotr(w(i - 15), 7) ^ rotr(w(i - 15), 18) ^ (w(i - 15) >>> 3)
                val s1 = rotr(w(i - 2), 17) ^ rotr(w(i - 2), 19) ^ (w(i - 2) >>> 10)
                w(i) = w(i - 16) + s0 + w(i - 7) + s1
                i += 1
            end while

            var a = h0; var b = h1; var c = h2; var d  = h3
            var e = h4; var f = h5; var g = h6; var hh = h7

            var t = 0
            while t < 64 do
                val s1    = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25)
                val ch    = (e & f) ^ (~e & g)
                val temp1 = hh + s1 + ch + K(t) + w(t)
                val s0    = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22)
                val maj   = (a & b) ^ (a & c) ^ (b & c)
                val temp2 = s0 + maj
                hh = g; g = f; f = e; e = d + temp1
                d = c; c = b; b = a; a = temp1 + temp2
                t += 1
            end while

            h0 += a; h1 += b; h2 += c; h3 += d
            h4 += e; h5 += f; h6 += g; h7 += hh
            block += 64
        end while

        val out = new Array[Byte](32)
        val hs  = Array(h0, h1, h2, h3, h4, h5, h6, h7)
        var k   = 0
        while k < 8 do
            out(k * 4) = (hs(k) >>> 24).toByte
            out(k * 4 + 1) = (hs(k) >>> 16).toByte
            out(k * 4 + 2) = (hs(k) >>> 8).toByte
            out(k * 4 + 3) = hs(k).toByte
            k += 1
        end while
        out
    end digest

    private def rotr(x: Int, n: Int): Int = (x >>> n) | (x << (32 - n))

    /** Append the `0x80` terminator, zero padding, and the 64-bit big-endian bit
      * length so the result is a whole number of 512-bit blocks.
      */
    private def pad(message: Array[Byte]): Array[Byte] =
        val bitLength = message.length.toLong * 8
        // One 0x80 byte, then zeros up to 56 mod 64, then 8 length bytes.
        val padLength = ((56 - (message.length + 1) % 64) + 64) % 64
        val result    = new Array[Byte](message.length + 1 + padLength + 8)
        System.arraycopy(message, 0, result, 0, message.length)
        result(message.length) = 0x80.toByte
        var i = 0
        while i < 8 do
            result(result.length - 1 - i) = (bitLength >>> (8 * i)).toByte
            i += 1
        result
    end pad
end Sha256
