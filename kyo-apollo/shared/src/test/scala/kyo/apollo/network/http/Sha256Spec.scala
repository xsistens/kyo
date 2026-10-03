package kyo.apollo.network.http

/** Verifies the hand-rolled [[Sha256]] against the canonical FIPS 180-4 vectors,
  * so the APQ `sha256Hash` the composer emits is trustworthy.
  */
class Sha256Spec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    "Sha256" - {

        "empty string" in {
            assert(
                Sha256.hex("") ==
                    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
            )
        }

        "\"abc\"" in {
            assert(
                Sha256.hex("abc") ==
                    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
            )
        }

        "multi-block message (exceeds one 512-bit block)" in {
            assert(
                Sha256.hex(
                    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
                ) ==
                    "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
            )
        }

        "hash is stable across calls" in {
            assert(Sha256.hex("query { hero }") == Sha256.hex("query { hero }"))
        }
    }
end Sha256Spec
