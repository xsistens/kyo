package kyo

import kyo.internal.DomBackend
import kyo.internal.DomReactiveRegions
import kyo.internal.HtmlRenderer
import kyo.internal.ListRow
import org.scalajs.dom
import scala.scalajs.js as scalajs

/** One table, run through both clients: the browser mount's reconciler and the server-push client's script. Each case starts from the same
  * live page, with what the reader did to it, and takes the same payload; what comes out is reduced to the markup, the properties a reader
  * moves, where focus is, and which nodes kept their identity. The two have to agree, and each has to give the stated answer.
  */
class DomBackendMorphParityTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    /** What a client left: the page's markup, the properties by id, the focused id, the ids whose node survived. */
    final private case class Outcome(html: String, props: Map[String, String], active: String, retained: Set[String], ranges: Set[String])
        derives CanEqual

    private type Prepare = scalajs.Dynamic => Unit

    private def el(id: String): scalajs.Dynamic = dom.document.getElementById(id).asInstanceOf[scalajs.Dynamic]

    private val hooks =
        """var __focusReturnStack=[];
          |function faEnterPaths(){return {};}
          |function focusAutoPaths(){return {};}
          |function kyoLeavePrepare(){return [];}
          |function kyoEnterSeed(){}
          |function applyJsProps(root){var all=[root],desc=root.querySelectorAll("*");for(var i=0;i<desc.length;i++)all.push(desc[i]);
          |  for(var i=0;i<all.length;i++){var names=all[i].getAttributeNames();for(var j=0;j<names.length;j++){var name=names[j];
          |    if(name.indexOf("data-kyo-prop-")===0){var el=all[i],p=name.slice(14),v=el.getAttribute(name),ps=el.__kyoProps||(el.__kyoProps={});
          |      ps[p]={base:ps[p]?ps[p].base:el[p],value:v};el[p]=v;el.removeAttribute(name);}}}}
          |function ba(){}
          |function kyoSpawnGhosts(){}
          |function sweepFocusAuto(){}
          |function sweepScrollAuto(){}
          |function __kyoPathSel(p){return '[data-kyo-path="'+p+'"]';}
          |function __kyoPortalTwin(p){return document.querySelector('body > '+__kyoPathSel(p)+'[data-kyo-portal]');}
          |function __kyoAdoptPortals(){}
          |function __kyoRetireOrphanTwins(){}
          |function __kyoRangeIdPath(id){var parts=id.split(".");parts.shift();return parts.join(".");}
          |function __kyoSyncField(el,name,value){
          |  if(name==="value"&&(el.tagName==="INPUT"||el.tagName==="TEXTAREA")&&el.value!==value)el.value=value;
          |  else if(name==="value"&&el.tagName==="SELECT"&&el.value!==value)el.value=value;}
          |function kyoSetCaret(target,start,end){target.setSelectionRange(start,end);}""".stripMargin

    /** Puts the live page up, with every identified node marked so that a node kept through the run can be told from a new one. */
    private def setUp(live: String, prepare: Prepare): Unit =
        dom.document.body.innerHTML = s"""<div id="host">$live</div>"""
        prepare(dom.document.body.asInstanceOf[scalajs.Dynamic])
        val marked = dom.document.querySelectorAll("[id]")
        var i      = 0
        while i < marked.length do
            marked(i).asInstanceOf[scalajs.Dynamic].__kept = true
            i += 1
    end setUp

    private def outcome(ranges: Set[String]): Outcome =
        val active = dom.document.activeElement
        val all    = dom.document.querySelectorAll("[id]")
        var props  = Map.empty[String, String]
        var kept   = Set.empty[String]
        var i      = 0
        while i < all.length do
            val e  = all(i).asInstanceOf[dom.Element]
            val d  = e.asInstanceOf[scalajs.Dynamic]
            val id = e.getAttribute("id")
            if !scalajs.isUndefined(d.__kept) then kept += id
            e.tagName match
                case "INPUT" | "TEXTAREA" | "SELECT" =>
                    props += s"$id.value"   -> d.value.asInstanceOf[String]
                    props += s"$id.checked" -> d.checked.toString
                case "OPTION" => props += s"$id.selected" -> d.selected.toString
                case _        => ()
            end match
            if !scalajs.isUndefined(d.indeterminate) then props += s"$id.indeterminate" -> d.indeterminate.toString
            i += 1
        end while
        Outcome(dom.document.body.innerHTML, props, if active == null then "" else active.getAttribute("id"), kept, ranges)
    end outcome

    private def host: dom.Element = dom.document.getElementById("host")

    private def fragmentOf(html: String): dom.DocumentFragment =
        val range = dom.document.createRange()
        range.selectNodeContents(host)
        range.createContextualFragment(html)
    end fragmentOf

    private def throughScala(live: String, incoming: String, prepare: Prepare)(using Frame): Outcome =
        setUp(live, prepare)
        DomBackend.morphRun(host, host.firstChild, null, fragmentOf(incoming).firstChild, null)
        outcome(Set.empty)
    end throughScala

    private def throughScript(live: String, incoming: String, prepare: Prepare): Outcome =
        setUp(live, prepare)
        discard(scalajs.Dynamic.global.eval(hooks + "\n" + HtmlRenderer.reactiveRangesJs))
        discard(scalajs.Dynamic.global.__kyoMorphRun(host, host.firstChild, null, fragmentOf(incoming).firstChild, null))
        outcome(Set.empty)
    end throughScript

    /** One case of the reconciler. */
    private def morph(name: String, live: String, incoming: String, prepare: Prepare = _ => ())(expect: Outcome => Boolean): Unit =
        name in {
            val viaScala  = throughScala(live, incoming, prepare)
            val viaScript = throughScript(live, incoming, prepare)
            assert(viaScala == viaScript, s"the clients disagree:\nScala:  $viaScala\nscript: $viaScript")
            assert(expect(viaScala), s"not the stated answer: $viaScala")
        }

    private val box = """<input id="x" type="checkbox">"""

    morph(
        "a free box keeps a tick the render does not speak about",
        box,
        """<input id="x" type="checkbox">""",
        _ => el("x").checked = true
    )(
        _.props("x.checked") == "true"
    )
    morph(
        "a bound box takes a tick back",
        """<input id="x" type="checkbox" data-kyo-bound>""",
        """<input id="x" type="checkbox" data-kyo-bound>""",
        _ => el("x").checked = true
    )(_.props("x.checked") == "false")
    morph("a free box follows a render that turns it on", box, """<input id="x" type="checkbox" checked>""")(_.props("x.checked") == "true")
    morph(
        "a free box follows a render that turns it off, past the reader's tick",
        """<input id="x" type="checkbox" checked>""",
        """<input id="x" type="checkbox">"""
    )(_.props("x.checked") == "false")
    morph(
        "a textarea whose content moved takes it",
        """<textarea id="x">a</textarea>""",
        """<textarea id="x">b</textarea>""",
        _ => el("x").value = "typed"
    )(
        _.props("x.value") == "b"
    )
    morph(
        "a textarea whose content did not move keeps what was typed",
        """<textarea id="x">a</textarea>""",
        """<textarea id="x">a</textarea>""",
        _ => el("x").value = "typed"
    )(_.props("x.value") == "typed")
    morph(
        "a focused field is left alone by the echo of its own text",
        """<input id="x" value="a">""",
        """<input id="x" value="ab">""",
        _ =>
            discard(el("x").focus())
            el("x").value = "ab"
            discard(el("x").setSelectionRange(1, 1))
    )(o => o.props("x.value") == "ab" && o.active == "x")
    morph(
        "a focused field takes an external write",
        """<input id="x" value="a">""",
        """<input id="x" value="z">""",
        _ =>
            discard(el("x").focus())
            el("x").value = "ab"
    )(o => o.props("x.value") == "z" && o.active == "x")
    morph(
        "a field that lost focus takes the render's value",
        """<input id="x" value="a">""",
        """<input id="x" value="a">""",
        _ => el("x").value = "typed"
    )(
        _.props("x.value") == "a"
    )
    morph(
        "an attribute the id channel owns is not reconciled",
        """<div id="x" class="a"></div>""",
        """<div id="x" class="b" title="t"></div>""",
        _ =>
            el("x").__kyoOwn = scalajs.Dynamic.literal(`class` = true)
            discard(el("x").setAttribute("class", "owned"))
    )(o => o.html.contains("""class="owned"""") && o.html.contains("""title="t""""))
    morph(
        "a js property the render no longer sets goes back to what it was",
        """<input id="x" type="checkbox">""",
        """<input id="x" type="checkbox">""",
        _ =>
            el("x").__kyoProps = scalajs.Dynamic.literal(indeterminate = scalajs.Dynamic.literal(base = false, value = "true"))
            el("x").indeterminate = true
    )(_.props("x.indeterminate") == "false")
    morph(
        "a focused contenteditable keeps its subtree",
        """<div id="x" tabindex="0" contenteditable="true"><span id="i">a</span></div>""",
        """<div id="x" tabindex="0" contenteditable="true"><span id="i">b</span></div>""",
        _ => discard(el("x").focus())
    )(o => o.html.contains(">a</span>") && o.active == "x")
    morph(
        "an unfocused contenteditable takes the render",
        """<div id="x" tabindex="0" contenteditable="true"><span id="i">a</span></div>""",
        """<div id="x" tabindex="0" contenteditable="true"><span id="i">b</span></div>"""
    )(_.html.contains(">b</span>"))
    morph(
        "a keyed reorder moves the nodes and keeps every one",
        """<li id="a" data-kyo-path="0.a">a</li><li id="b" data-kyo-path="0.b">b</li><li id="c" data-kyo-path="0.c">c</li>""",
        """<li id="c" data-kyo-path="0.c">c</li><li id="a" data-kyo-path="0.a">a2</li><li id="b" data-kyo-path="0.b">b</li>"""
    )(o =>
        o.retained.contains("a") && o.retained.contains("b") && o.retained.contains("c") &&
            o.html.indexOf("""id="c"""") < o.html.indexOf("""id="a"""")
    )
    morph(
        "a keyed child the payload drops goes, and one it brings arrives",
        """<li id="a" data-kyo-path="0.a">a</li><li id="b" data-kyo-path="0.b">b</li>""",
        """<li id="b" data-kyo-path="0.b">b</li><li id="n" data-kyo-path="0.n">n</li>"""
    )(o => o.retained == Set("host", "b") && !o.html.contains("""id="a""""))
    morph(
        "a nested range reconciles as one child and keeps its markers",
        """<div id="o"><!--kyo-rs:r.1--><span id="n">a</span><!--kyo-re:r.1--></div>""",
        """<div id="o"><!--kyo-rs:r.1--><span id="n">b</span><!--kyo-re:r.1--></div>"""
    )(o => o.retained.contains("n") && o.html.contains("<!--kyo-rs:r.1--><span"))
    morph(
        "a range of another id is replaced, markers and all",
        """<!--kyo-rs:r.1--><span id="n">a</span><!--kyo-re:r.1-->""",
        """<!--kyo-rs:r.2--><span id="m">b</span><!--kyo-re:r.2-->"""
    )(o => o.html.contains("kyo-rs:r.2") && !o.html.contains("kyo-rs:r.1"))
    morph(
        "a bare run facing one incoming range is wrapped in it and keeps its nodes",
        """<span id="n">a</span>""",
        """<!--kyo-rs:r.1--><span id="n">b</span><!--kyo-re:r.1-->"""
    )(o => o.retained.contains("n") && o.html.contains("kyo-rs:r.1"))
    morph(
        "a slot its mount owns is opaque to the render around it",
        """<!--kyo-rs:r.1 m k=a--><span id="n">content</span><!--kyo-re:r.1-->""",
        """<!--kyo-rs:r.1 s k=a--><span id="n">placeholder</span><!--kyo-re:r.1-->"""
    )(o => o.html.contains(">content</span>"))
    morph(
        "a slot with another key is reset and adopts it",
        """<!--kyo-rs:r.1 m k=a--><span id="n">content</span><!--kyo-re:r.1-->""",
        """<!--kyo-rs:r.1 s k=b--><span id="n">placeholder</span><!--kyo-re:r.1-->"""
    )(o => o.html.contains(">placeholder</span>") && o.html.contains("kyo-rs:r.1 m k=b"))
    morph(
        "a span against a plain node is replaced",
        """<span id="n">a</span><p id="k">k</p>""",
        """<!--kyo-rs:r.1--><span id="n">b</span><!--kyo-re:r.1--><p id="k">k</p>"""
    )(o => o.html.contains("kyo-rs:r.1") && o.html.contains(">b</span>") && o.retained.contains("k"))
    morph("another tag replaces the node", """<span id="n">a</span>""", """<p id="n">a</p>""")(o =>
        o.html.contains("<p ") && !o.retained.contains("n")
    )
    morph("text takes the new text and stays one node", "a<b id=\"n\">x</b>", "b<b id=\"n\">x</b>")(_.html.contains("b<b"))
    morph(
        "a portal pair reconciles the twin under body and leaves the slot where it is",
        """<div data-kyo-portal-slot="0.p"></div>""",
        """<div id="twin" data-kyo-path="0.p" data-kyo-portal="true"><span id="tc">b</span></div>""",
        body =>
            val twin = dom.document.createElement("div")
            twin.setAttribute("id", "twin")
            twin.setAttribute("data-kyo-path", "0.p")
            twin.setAttribute("data-kyo-portal", "true")
            twin.innerHTML = """<span id="tc">a</span>"""
            discard(body.appendChild(twin))
    )(o => o.retained.contains("twin") && o.html.contains(">b</span>") && o.html.contains("data-kyo-portal-slot"))

    // ---- list patch ----

    /** Live rows under a range `r.0`, each `<li>` at path `0.<key>`, and rows `(key, html)` to bring: empty html for one kept as it is. */
    private def rowHtml(key: String, text: String): String = s"""<li id="$key" data-kyo-path="0.$key">$text</li>"""

    private val listStart = "<!--kyo-rs:r.0-->"
    private val listEnd   = "<!--kyo-re:r.0-->"

    private def listThroughScala(live: String, rows: Seq[(String, String)], prepare: Prepare)(using Frame): Outcome < (Sync & Scope) =
        Sync.defer(setUp(live, prepare)).andThen(DomReactiveRegions.init(host)).map { regions =>
            val changed = rows.map(_._2).mkString
            regions.withRegionFragment("r.0", changed) { (target, scope) =>
                DomBackend.listPatch(target, scope, Seq("0"), rows.map((k, h) => ListRow(k, UI.empty, h.nonEmpty)))
            }.andThen(regions.entries).map(entries => outcome(entries.keySet.toSet))
        }

    private def listThroughScript(live: String, rows: Seq[(String, String)], prepare: Prepare): Outcome =
        setUp(live, prepare)
        discard(scalajs.Dynamic.global.eval(hooks + "\n" + HtmlRenderer.reactiveRangesJs))
        discard(scalajs.Dynamic.global.__kyoApplyList("r.0", scalajs.Array(rows.map(_._1)*), rows.map(_._2).mkString))
        val ids = scalajs.Dynamic.global.eval("Array.from(__kyoRanges.keys())").asInstanceOf[scalajs.Array[String]]
        outcome(ids.toSet)
    end listThroughScript

    private def list(
        name: String,
        live: String,
        rows: Seq[(String, String)],
        prepare: Prepare = _ => ()
    )(expect: Outcome => Boolean): Unit =
        name in {
            Scope.run(listThroughScala(live, rows, prepare)).map { viaScala =>
                val viaScript = listThroughScript(live, rows, prepare)
                assert(viaScala == viaScript, s"the clients disagree:\nScala:  $viaScala\nscript: $viaScript")
                assert(expect(viaScala), s"not the stated answer: $viaScala")
            }
        }

    private val threeRows = listStart + rowHtml("a", "a") + rowHtml("b", "b") + rowHtml("c", "c") + listEnd

    list("a pure reorder repaints nothing and keeps every row", threeRows, Seq("c" -> "", "a" -> "", "b" -> ""))(o =>
        o.retained == Set("host", "a", "b", "c") && o.html.indexOf("""id="c"""") < o.html.indexOf("""id="a"""")
    )
    list("one changed row takes its new content in place", threeRows, Seq("a" -> "", "b" -> rowHtml("b", "B"), "c" -> ""))(o =>
        o.retained == Set("host", "a", "b", "c") && o.html.contains(">B</li>")
    )
    list("a removal leaves the rest", threeRows, Seq("a" -> "", "c" -> ""))(o =>
        o.retained == Set("host", "a", "c") && !o.html.contains("""id="b"""")
    )
    list("a new row lands in its place", threeRows, Seq("a" -> "", "n" -> rowHtml("n", "n"), "b" -> "", "c" -> ""))(o =>
        o.html.indexOf("""id="a"""") < o.html.indexOf("""id="n"""") && o.html.indexOf("""id="n"""") < o.html.indexOf("""id="b"""")
    )
    list(
        "a row holding a nested range keeps the registry true",
        listStart + """<li id="a" data-kyo-path="0.a"><!--kyo-rs:r.0.a.0--><span id="s">x</span><!--kyo-re:r.0.a.0--></li>""" +
            rowHtml("b", "b") + listEnd,
        Seq(
            "b" -> "",
            "a" -> """<li id="a" data-kyo-path="0.a"><!--kyo-rs:r.0.a.0--><span id="s">y</span><!--kyo-re:r.0.a.0--></li>"""
        )
    )(o => o.ranges == Set("r.0", "r.0.a.0") && o.html.contains(">y</span>") && o.retained.contains("s"))
    list(
        "a reorder keeps focus and caret in a row",
        listStart + """<li id="a" data-kyo-path="0.a"><input id="f" data-kyo-path="0.a.0" value="abc"></li>""" + rowHtml("b", "b") +
            listEnd,
        Seq("b" -> "", "a" -> ""),
        _ => discard(el("f").focus())
    )(o => o.active == "f" && o.retained.contains("f"))

end DomBackendMorphParityTest
