package kyo

class ChunkBuilderTest extends kyo.test.Test[Any]:

    "ChunkBuilder" - {
        "empty" in {
            val builder = ChunkBuilder.init[Int]
            assert(builder.result() == Chunk.empty[Int])
        }

        "non-empty" in {
            val builder = ChunkBuilder.init[Int]
            builder.addOne(1)
            builder.addOne(2)
            builder.addOne(3)
            assert(builder.result() == Chunk(1, 2, 3))
        }

        "clear" in {
            val builder = ChunkBuilder.init[Boolean]
            builder.addOne(true)
            builder.clear()
            assert(builder.knownSize == 0)
            builder.addOne(true)
            assert(builder.result() == Chunk(true))
        }

        "reusable" in {
            val builder = ChunkBuilder.init[Int]
            builder.addOne(1)
            builder.addOne(2)
            assert(builder.result() == Chunk(1, 2))
            builder.addOne(3)
            assert(builder.result() == Chunk(3))
        }

        "hint" in {
            val builder = ChunkBuilder.init[Int]
            (0 until 1000).foreach(builder.addOne)
            assert(builder.result().length == 1000)
        }

        "knownSize" in {
            val builder = ChunkBuilder.init[Int]
            assert(builder.knownSize == 0)
            (0 until 10).foreach(builder.addOne)
            assert(builder.knownSize == 10)
        }

        "toString" in {
            assert(ChunkBuilder.init[Int].toString() == "ChunkBuilder(size = 0)")
            val builder = ChunkBuilder.init[Int]
            builder.sizeHint(1)
            assert(builder.toString() == "ChunkBuilder(size = 0)")
            val hinted = ChunkBuilder.init[Int]
            hinted.addOne(1)
            hinted.sizeHint(2)
            assert(hinted.toString() == "ChunkBuilder(size = 1)")
        }

        "a released buffer keeps no element reachable" - {
            "after result" in {
                val builder = ChunkBuilder.init[String]
                (0 until 100).foreach(i => builder.addOne(i.toString))
                assert(builder.result().size == 100)
                assert(ChunkBuilder.pooledReferences() == 0)
            }

            "after clear and a smaller result" in {
                val builder = ChunkBuilder.init[String]
                (0 until 100).foreach(i => builder.addOne(i.toString))
                builder.clear()
                builder.addOne("a")
                assert(builder.result() == Chunk("a"))
                assert(ChunkBuilder.pooledReferences() == 0)
            }

            "after Chunk.foreach over appended elements" in {
                val chunk = Chunk("a").append("b").append("c")
                var seen  = List.empty[String]
                chunk.foreach(s => seen = seen :+ s)
                assert(seen == List("a", "b", "c"))
                assert(ChunkBuilder.pooledReferences() == 0)
            }
        }

        "null elements" in {
            val builder = ChunkBuilder.init[String]
            builder.addOne("a")
            builder.addOne(null)
            builder.addOne("c")
            assert(builder.result() == Chunk("a", null, "c"))
        }
    }
end ChunkBuilderTest
