package kyo

import kyo.internal.DomReactiveRegions
import org.scalajs.dom

class DomReactiveRegionsTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    private val One   = "r000000010031"
    private val Two   = "r000000010032"
    private val Outer = "r00000001006f"
    private val Old   = "r000000010064"
    private val New   = "r00000001006e"

    private def host(html: String): dom.Element =
        val root = dom.document.createElement("div")
        root.innerHTML = html
        root
    end host

    private def initResult(html: String)(using Frame): Result[Any, DomReactiveRegions] < Async =
        Fiber.initUnscoped(Scope.run(DomReactiveRegions.init(host(html)))).map(_.getResult)

    private def assertPanicContains[E, A](result: Result[E, A], expected: String)(using kyo.test.AssertScope): Unit =
        result match
            case Result.Panic(error) => assert(error.getMessage.contains(expected))
            case other               => fail(s"Expected panic containing '$expected', got: $other")

    "initial scan records nested live ranges" in {
        val root = host(s"<!--kyo-rs:$One--><div><!--kyo-rs:$Two-->x<!--kyo-re:$Two--></div><!--kyo-re:$One-->")
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                size    <- regions.size
            yield assert(size == 2)
        }
    }

    "initial scan rejects duplicate ids" in {
        initResult(s"<!--kyo-rs:$One--><!--kyo-re:$One--><!--kyo-rs:$One--><!--kyo-re:$One-->")
            .map(result => assertPanicContains(result, s"Duplicate reactive range id: $One"))
    }

    "initial scan rejects malformed ids" in {
        initResult("<!--kyo-rs:r1--><!--kyo-re:r1-->")
            .map(result => assertPanicContains(result, "Malformed reactive range id: r1"))
    }

    "initial scan rejects missing ends" in {
        initResult(s"<!--kyo-rs:$One--><span>x</span>")
            .map(result => assertPanicContains(result, s"Reactive range start marker has no end: $One"))
    }

    "initial scan rejects crossed pairs" in {
        initResult(s"<!--kyo-rs:$One--><!--kyo-rs:$Two--><!--kyo-re:$One--><!--kyo-re:$Two-->")
            .map(result => assertPanicContains(result, s"Crossed reactive ranges: expected $Two, found $One"))
    }

    "initial scan rejects non-sibling anchors" in {
        initResult(s"<!--kyo-rs:$One--><span><!--kyo-re:$One--></span>")
            .map(result => assertPanicContains(result, s"Reactive range anchors are not siblings: $One"))
    }

    "replacement uses the actual table parent context and retains anchors" in {
        val root = host(s"<table><tbody><!--kyo-rs:$One--><tr id='old'><td>old</td></tr><!--kyo-re:$One--></tbody></table>")
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                _       <- regions.replace(One, "<tr id='new'><td>new</td></tr>")
                size    <- regions.size
            yield
                val tbody = root.querySelector("tbody")
                assert(tbody.children.length == 1)
                assert(tbody.children(0).asInstanceOf[dom.Element].tagName == "TR")
                assert(root.querySelector("#new") != null)
                assert(root.innerHTML.contains(s"kyo-rs:$One"))
                assert(root.innerHTML.contains(s"kyo-re:$One"))
                assert(size == 1)
        }
    }

    "table range transitions between rows and an authored section without losing attributes" in {
        val root = host(
            s"<table><tbody data-kyo-range-host='$One'><!--kyo-rs:$One--><tr id='row'><td>row</td></tr><!--kyo-re:$One--></tbody></table>"
        )
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                _ <- regions.replace(
                    One,
                    "<tbody id='authored' class='section'><tr id='section-row'><td>section</td></tr></tbody>"
                )
                authored <- Sync.defer {
                    val section = root.querySelector("table > tbody")
                    val walker  = dom.document.createTreeWalker(root, 128, null, false)
                    val anchors = Iterator
                        .continually(walker.nextNode())
                        .takeWhile(_ != null)
                        .filter(_.nodeValue.endsWith(One))
                        .map(_.parentNode)
                        .toSeq
                    (
                        id = section.id,
                        className = section.getAttribute("class"),
                        rangeHost = section.getAttribute("data-kyo-range-host"),
                        anchorParents = anchors
                    )
                }
                _ <- regions.replaceWith(
                    One,
                    s"<tbody data-kyo-range-host='$One'><tr id='new-row'><td>new</td></tr></tbody>"
                    // `tryMorph` declines, so this exercises the wholesale path — which is
                    // the one that juggles the anchors between a synthetic host and its
                    // table, and the only one that can produce the assertions below.
                )(_ => false) { (oldRoots, newRoots) =>
                    assert(oldRoots.map(_.id) == Seq("authored"))
                    assert(newRoots.map(_.id) == Seq("new-row"))
                } { (_, insertedRoots, morphed) =>
                    assert(!morphed)
                    assert(insertedRoots.map(_.id) == Seq("new-row"))
                }
            yield
                assert(authored.id == "authored")
                assert(authored.className == "section")
                assert(authored.rangeHost == null)
                assert(authored.anchorParents.size == 2)
                assert(authored.anchorParents.forall(_ eq root.querySelector("table")))
                assert(root.querySelector("#section-row") == null)
                val rows = root.querySelectorAll("table > tbody")
                assert(rows.length == 1)
                val current = rows(0).asInstanceOf[dom.Element]
                assert(current.id == "")
                assert(!current.hasAttribute("class"))
                assert(current.getAttribute("data-kyo-range-host") == One)
                assert(current.querySelector("#new-row") != null)
                assert(current.innerHTML.contains(s"kyo-rs:$One"))
                assert(current.innerHTML.contains(s"kyo-re:$One"))
        }
    }

    "table range moves anchors out for multiple authored sections and back into a row host" in {
        val root = host(
            s"<table><tbody data-kyo-range-host='$One'><!--kyo-rs:$One--><tr><td>row</td></tr><!--kyo-re:$One--></tbody></table>"
        )
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                _ <- regions.replace(
                    One,
                    "<tbody id='first'><tr><td>one</td></tr></tbody><tbody id='second'><tr><td>two</td></tr></tbody>"
                )
                authoredCount <- Sync.defer(root.querySelectorAll("table > tbody").length)
                _ <- regions.replace(
                    One,
                    s"<tbody data-kyo-range-host='$One'><tr id='returned'><td>row</td></tr></tbody>"
                )
            yield
                assert(authoredCount == 2)
                val table = root.querySelector("table")
                assert(table.children.length == 1)
                val current = table.children(0).asInstanceOf[dom.Element]
                assert(current.getAttribute("data-kyo-range-host") == One)
                assert(current.querySelector("#returned") != null)
                assert(current.innerHTML.contains(s"kyo-rs:$One"))
                assert(current.innerHTML.contains(s"kyo-re:$One"))
        }
    }

    "replacement uses the actual select parent context" in {
        val root = host(s"<select><!--kyo-rs:$One--><option id='old'>old</option><!--kyo-re:$One--></select>")
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                _       <- regions.replace(One, "<option id='new'>new</option>")
            yield
                val select = root.querySelector("select")
                assert(select.children.length == 1)
                assert(select.children(0).asInstanceOf[dom.Element].tagName == "OPTION")
                assert(root.querySelector("#new") != null)
        }
    }

    "outer replacement unregisters removed nested ranges and registers new nested ranges" in {
        val root = host(
            s"<!--kyo-rs:$Outer--><div><!--kyo-rs:$Old-->old<!--kyo-re:$Old--></div><!--kyo-re:$Outer-->"
        )
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                _ <- regions.replace(
                    Outer,
                    s"<section><!--kyo-rs:$New-->new<!--kyo-re:$New--></section>"
                )
                size     <- regions.size
                hasOuter <- regions.contains(Outer)
                hasOld   <- regions.contains(Old)
                hasNew   <- regions.contains(New)
            yield
                assert(size == 2)
                assert(hasOuter)
                assert(!hasOld)
                assert(hasNew)
        }
    }

    "malformed incoming markers reject before mutating the live range" in {
        val root   = host(s"<!--kyo-rs:$One--><span id='kept'>kept</span><!--kyo-re:$One-->")
        val before = root.innerHTML
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                result  <- Fiber.initUnscoped(regions.replace(One, s"<!--kyo-rs:$Two--><b>broken</b>")).map(_.getResult)
                size    <- regions.size
            yield
                result match
                    case Result.Panic(error) => assert(error.getMessage.contains(s"Reactive range start marker has no end: $Two"))
                    case other               => fail(s"Expected malformed replacement panic, got: $other")
                assert(root.innerHTML == before)
                assert(size == 1)
        }
    }

    "unknown replacement reports the exact diagnostic without mutation" in {
        val root   = host(s"<!--kyo-rs:$One--><span id='kept-unknown'>kept</span><!--kyo-re:$One-->")
        val before = root.innerHTML
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                result  <- Fiber.initUnscoped(regions.replace(Two, "changed")).map(_.getResult)
            yield
                // KyoException.getMessage decorates the diagnostic with environment-aware formatting, so the
                // full diagnostic (including the region id) is asserted as a substring, never by equality.
                assertPanicContains(result, s"Unknown reactive range: $Two")
                assert(root.innerHTML == before)
        }
    }

    "replacement rejects corrupted and reordered live anchors before mutation" in {
        val corrupted = host(s"<!--kyo-rs:$One--><span>kept</span><!--kyo-re:$One-->")
        val reordered = host(s"<!--kyo-rs:$One--><span>kept</span><!--kyo-re:$One-->")
        Scope.run {
            for
                corruptedRegions <- DomReactiveRegions.init(corrupted)
                corruptedEnd = corrupted.lastChild.asInstanceOf[dom.Comment]
                _                <- Sync.defer(corruptedEnd.data = s"kyo-re:$Two")
                corruptedBefore  <- Sync.defer(corrupted.innerHTML)
                corruptedResult  <- Fiber.initUnscoped(corruptedRegions.replace(One, "new")).map(_.getResult)
                reorderedRegions <- DomReactiveRegions.init(reordered)
                reorderedStart = reordered.firstChild
                reorderedEnd   = reordered.lastChild
                _               <- Sync.defer(discard(reordered.insertBefore(reorderedEnd, reorderedStart)))
                reorderedBefore <- Sync.defer(reordered.innerHTML)
                reorderedResult <- Fiber.initUnscoped(reorderedRegions.replace(One, "new")).map(_.getResult)
            yield
                assertPanicContains(corruptedResult, "Reactive range markers are corrupted")
                assertPanicContains(reorderedResult, "Reactive range end is not after its start")
                assert(corrupted.innerHTML == corruptedBefore)
                assert(reordered.innerHTML == reorderedBefore)
        }
    }

    "scope close clears the registry and later replacement rejects" in {
        val root = host(s"<!--kyo-rs:$One-->x<!--kyo-re:$One-->")
        for
            saved <- AtomicRef.init(Absent: Maybe[DomReactiveRegions])
            _ <- Scope.run {
                DomReactiveRegions.init(root).map(regions => saved.set(Present(regions)))
            }
            regions <- saved.get.map(_.get)
            size    <- regions.size
            result  <- Fiber.initUnscoped(regions.replace(One, "later")).map(_.getResult)
        yield
            assert(size == 0)
            result match
                case Result.Panic(error) => assert(error.getMessage.contains("Reactive range registry is closed"))
                case other               => fail(s"Expected closed-registry panic, got: $other")
        end for
    }

    // Path-addressed lookups. A range id ENCODES its path, so the registry finds the range of a path by encoding
    // the path once and reading the table, never by decoding every id it holds.

    private def idOf(path: String*): String = kyo.internal.ReactiveRegion.htmlId(path)

    "a text write finds its range among a thousand and keeps the text node" in {
        val html = (0 until 1000).map { i =>
            val id = idOf("0", i.toString, "1")
            s"<p><!--kyo-rs:$id-->v$i<!--kyo-re:$id--></p>"
        }.mkString
        val root = host(html)
        Scope.run {
            DomReactiveRegions.init(root).map { regions =>
                val target = root.children(617).childNodes(1)
                val wrote  = regions.setTextAt(Seq("0", "617", "1"), "changed")
                val missed = regions.setTextAt(Seq("0", "1000", "1"), "nobody")
                assert(wrote)
                assert(!missed)
                assert(root.children(617).childNodes(1) eq target)
                assert(root.children(617).textContent == "changed")
                assert(root.children(616).textContent == "v616")
                assert(root.children(618).textContent == "v618")
            }
        }
    }

    "a text write under transparent nesting lands in the innermost range" in {
        val inner = One + "n00000001"
        val root  = host(s"<!--kyo-rs:$One--><!--kyo-rs:$inner-->x<!--kyo-re:$inner--><!--kyo-re:$One-->")
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                wrote = regions.setTextAt(Seq("1"), "y")
                size <- regions.size
                // The outer range is still patchable, which it is not once its inner markers were deleted.
                _ <- regions.replace(inner, "z")
            yield
                assert(wrote)
                assert(size == 2)
                assert(root.textContent == "z")
                assert(root.childNodes.length == 5)
        }
    }

    "a text write never takes the outer of two ranges that share a path" in {
        // Which of the two a table hands out first is an accident of hashing, so one pair proves nothing: over
        // sixty-four pairs an implementation that takes whichever comes first deletes inner markers somewhere.
        val html = (0 until 64).map { i =>
            val outer = idOf("p", i.toString)
            val inner = outer + "n00000001"
            s"<p><!--kyo-rs:$outer--><!--kyo-rs:$inner-->v<!--kyo-re:$inner--><!--kyo-re:$outer--></p>"
        }.mkString
        val root = host(html)
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                wrote = (0 until 64).forall(i => regions.setTextAt(Seq("p", i.toString), s"w$i"))
                size <- regions.size
            yield
                assert(wrote)
                assert(size == 128)
                assert((0 until 64).forall(i => root.children(i).childNodes.length == 5))
                assert((0 until 64).forall(i => root.children(i).textContent == s"w$i"))
        }
    }

    "the first element of a path resolves through either nesting level" in {
        val inner = One + "n00000001"
        val root  = host(s"<!--kyo-rs:$One--><!--kyo-rs:$inner-->t<b id='hit'></b><!--kyo-re:$inner--><!--kyo-re:$One-->")
        Scope.run {
            DomReactiveRegions.init(root).map { regions =>
                assert(regions.firstElementAt(Seq("1")).map(_.id) == Present("hit"))
                assert(regions.firstElementAt(Seq("2")).isEmpty)
            }
        }
    }

    "path lookups follow the registry through a replacement" in {
        val root = host(
            s"<!--kyo-rs:$Outer--><div><!--kyo-rs:$Old-->old<!--kyo-re:$Old--></div><!--kyo-re:$Outer-->"
        )
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                _       <- regions.replace(Outer, s"<section><!--kyo-rs:$New-->new<!--kyo-re:$New--></section>")
            yield
                assert(!regions.setTextAt(Seq("d"), "stale"))
                assert(regions.setTextAt(Seq("n"), "fresh"))
                assert(root.textContent == "fresh")
        }
    }

    "path lookups find nothing once the registry is closed" in {
        val root = host(s"<!--kyo-rs:$One-->x<!--kyo-re:$One-->")
        for
            saved   <- AtomicRef.init(Absent: Maybe[DomReactiveRegions])
            _       <- Scope.run(DomReactiveRegions.init(root).map(regions => saved.set(Present(regions))))
            regions <- saved.get.map(_.get)
        yield
            assert(!regions.setTextAt(Seq("1"), "late"))
            assert(root.textContent == "x")
        end for
    }

    "the client encodes a path to the id the server encodes it to" in {
        // `__kyoResolveEl` reads the client registry by the id `__kyoRangeIdOf` builds, so the two encoders agreeing
        // is what that lookup rests on.
        val js     = kyo.internal.HtmlRenderer.clientJs("/")
        val from   = js.indexOf("function __kyoRangeIdOf(p){")
        val until  = js.indexOf("function __kyoFirstElIn", from)
        val encode = scala.scalajs.js.eval(s"(${js.substring(from, until)})").asInstanceOf[scala.scalajs.js.Function1[String, String]]
        val paths = Seq(
            Seq.empty[String],
            Seq("0"),
            Seq("2", "0", "17", "1"),
            Seq("row-42", "k"),
            Seq("ü", "\u4e2d", "a b")
        )
        assert(from >= 0 && until > from)
        assert(paths.forall(path => encode(path.mkString(".")) == idOf(path*)))
    }

    "the path index shrinks with the ranges it indexes" in {
        // Every round retires the ranges of the round before it and registers fresh ids, one of them nested
        // transparently. An index that only ever gained entries would hold a path per round at the end.
        val root = host(s"<!--kyo-rs:$Outer--><!--kyo-re:$Outer-->")
        for
            saved <- AtomicRef.init(Absent: Maybe[DomReactiveRegions])
            counts <- Scope.run {
                for
                    regions <- DomReactiveRegions.init(root)
                    _       <- saved.set(Present(regions))
                    _ <- Kyo.foreachDiscard(0 until 200) { round =>
                        val plain = idOf("round", round.toString)
                        val outer = idOf("nested", round.toString)
                        val inner = outer + "n00000001"
                        regions.replace(
                            Outer,
                            s"<i><!--kyo-rs:$plain-->a<!--kyo-re:$plain--></i>" +
                                s"<b><!--kyo-rs:$outer--><!--kyo-rs:$inner-->b<!--kyo-re:$inner--><!--kyo-re:$outer--></b>"
                        )
                    }
                    size  <- regions.size
                    paths <- regions.indexedPaths
                yield (size, paths, regions.setTextAt(Seq("round", "0"), "stale"), regions.setTextAt(Seq("round", "199"), "live"))
            }
            regions     <- saved.get.map(_.get)
            closedSize  <- regions.size
            closedPaths <- regions.indexedPaths
        yield
            // Outer, the plain range, and the nested pair: four ranges under three paths.
            assert(counts == (4, 3, false, true))
            assert(closedSize == 0 && closedPaths == 0)
        end for
    }

    // Row-level patches. The registry follows what the reconciler reports instead of re-reading the list, so what
    // it ends up holding has to equal what a fresh read of the same DOM finds, and a row that only moved must cost
    // it nothing.

    private val ListId = idOf("l")

    private def rowHtml(i: Int, label: String): String =
        val id = idOf("l", i.toString, label)
        s"<li data-kyo-path='l.$i'><!--kyo-rs:$id-->$label<!--kyo-re:$id--></li>"

    private def listHost(rows: Int): dom.Element =
        host(s"<ul><!--kyo-rs:$ListId-->${(0 until rows).map(rowHtml(_, "a")).mkString}<!--kyo-re:$ListId--></ul>")

    private def freshIds(root: dom.Element)(using Frame): Set[String] < (Sync & Scope) =
        DomReactiveRegions.init(root).map(_.ids)

    "a row patch leaves the registry holding what a fresh read of the DOM finds" in {
        val root                     = listHost(50)
        val ul                       = root.firstElementChild
        def row(i: Int): dom.Element = ul.querySelector(s"[data-kyo-path='l.$i']")
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                removed <- regions.withRegionFragment(ListId, "") { (_, scope) =>
                    scope.retire(row(10))
                    discard(ul.removeChild(row(10)))
                    true
                }
                afterRemove  <- regions.ids
                expectRemove <- freshIds(root)
                swapped <- regions.withRegionFragment(ListId, "") { (target, _) =>
                    val (first, last) = (row(1), row(48))
                    val afterLast     = last.nextSibling
                    discard(ul.insertBefore(last, first))
                    discard(ul.insertBefore(first, afterLast))
                    true
                }
                afterSwap  <- regions.ids
                expectSwap <- freshIds(root)
                repainted <- regions.withRegionFragment(ListId, rowHtml(5, "b")) { (target, scope) =>
                    val live  = row(5)
                    val until = live.nextSibling
                    scope.retire(live)
                    val fresh = dom.document.importNode(target.fragment.firstChild, true)
                    discard(ul.insertBefore(fresh, live))
                    discard(ul.removeChild(live))
                    scope.placed(fresh, until)
                    true
                }
                afterRepaint  <- regions.ids
                expectRepaint <- freshIds(root)
                inserted <- regions.withRegionFragment(ListId, rowHtml(77, "c")) { (target, scope) =>
                    val fresh = dom.document.importNode(target.fragment.firstChild, true)
                    discard(ul.insertBefore(fresh, target.end))
                    scope.placed(fresh, target.end)
                    true
                }
                afterInsert  <- regions.ids
                expectInsert <- freshIds(root)
            yield
                assert(removed && swapped && repainted && inserted)
                assert(afterRemove == expectRemove && afterRemove.size == 50)
                assert(afterSwap == expectSwap)
                assert(afterRepaint == expectRepaint)
                assert(afterRepaint.contains(idOf("l", "5", "b")) && !afterRepaint.contains(idOf("l", "5", "a")))
                assert(afterInsert == expectInsert && afterInsert.size == 51)
                assert(regions.setTextAt(Seq("l", "48", "a"), "moved"))
                assert(!regions.setTextAt(Seq("l", "10", "a"), "gone"))
                assert(regions.setTextAt(Seq("l", "77", "c"), "new"))
        }
    }

    "a row patch costs the registry the rows it changed" in {
        val root                     = listHost(1000)
        val ul                       = root.firstElementChild
        def row(i: Int): dom.Element = ul.querySelector(s"[data-kyo-path='l.$i']")
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                before  <- regions.registryWrites
                _ <- regions.withRegionFragment(ListId, "") { (_, _) =>
                    val (first, last) = (row(1), row(998))
                    val afterLast     = last.nextSibling
                    discard(ul.insertBefore(last, first))
                    discard(ul.insertBefore(first, afterLast))
                    true
                }
                swapped <- regions.registryWrites
                _ <- regions.withRegionFragment(ListId, "") { (_, scope) =>
                    scope.retire(row(500))
                    discard(ul.removeChild(row(500)))
                    true
                }
                removed <- regions.registryWrites
                paths   <- regions.indexedPaths
            yield
                // Re-reading the list wrote two thousand entries for either patch: every row out, every row back.
                assert(swapped - before == 0)
                assert(removed - swapped == 1)
                assert(paths == 1000)
        }
    }

    "a declined row patch leaves the registry alone" in {
        val root = listHost(3)
        Scope.run {
            for
                regions <- DomReactiveRegions.init(root)
                before  <- regions.ids
                writes  <- regions.registryWrites
                took <- regions.withRegionFragment(ListId, rowHtml(9, "z")) { (_, scope) =>
                    false
                }
                after       <- regions.ids
                writesAfter <- regions.registryWrites
            yield
                assert(!took)
                assert(before == after && writes == writesAfter)
        }
    }

end DomReactiveRegionsTest
