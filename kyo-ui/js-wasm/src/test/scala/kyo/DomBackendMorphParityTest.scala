package kyo

import kyo.internal.DomBackend
import kyo.internal.HtmlRenderer
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
        """<div id="o"><!--kyo-rs:r000000010031--><span id="n">a</span><!--kyo-re:r000000010031--></div>""",
        """<div id="o"><!--kyo-rs:r000000010031--><span id="n">b</span><!--kyo-re:r000000010031--></div>"""
    )(o => o.retained.contains("n") && o.html.contains("<!--kyo-rs:r000000010031--><span"))
    morph(
        "a range of another id is replaced, markers and all",
        """<!--kyo-rs:r000000010031--><span id="n">a</span><!--kyo-re:r000000010031-->""",
        """<!--kyo-rs:r000000010032--><span id="m">b</span><!--kyo-re:r000000010032-->"""
    )(o => o.html.contains("kyo-rs:r000000010032") && !o.html.contains("kyo-rs:r000000010031"))
    morph(
        "a slot its mount owns is opaque to the render around it",
        """<!--kyo-rs:r000000010031 m k=a--><span id="n">content</span><!--kyo-re:r000000010031-->""",
        """<!--kyo-rs:r000000010031 s k=a--><span id="n">placeholder</span><!--kyo-re:r000000010031-->"""
    )(o => o.html.contains(">content</span>"))
    morph(
        "a slot with another key is reset and adopts it",
        """<!--kyo-rs:r000000010031 m k=a--><span id="n">content</span><!--kyo-re:r000000010031-->""",
        """<!--kyo-rs:r000000010031 s k=b--><span id="n">placeholder</span><!--kyo-re:r000000010031-->"""
    )(o => o.html.contains(">placeholder</span>") && o.html.contains("kyo-rs:r000000010031 m k=b"))
    morph(
        "a span against a plain node is replaced",
        """<span id="n">a</span><p id="k">k</p>""",
        """<!--kyo-rs:r000000010031--><span id="n">b</span><!--kyo-re:r000000010031--><p id="k">k</p>"""
    )(o => o.html.contains("kyo-rs:r000000010031") && o.html.contains(">b</span>") && o.retained.contains("k"))
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

    "a repaint of a mounted page goes through the script" in {
        val script = DomBackend.morphScript
        val run    = script.run
        var runs   = 0
        script.run = scalajs.Any.fromFunction5((a: scalajs.Any, b: scalajs.Any, c: scalajs.Any, d: scalajs.Any, e: scalajs.Any) =>
            runs += 1
            run(a, b, c, d, e)
        )
        val ready = new DomTestEnv.MountReady
        for
            tick <- Signal.initRef(0)
            ui = UI.div(UI.div.id("sp-tick")(tick.map(n => UI.span(s"t:$n"): UI)))
            fiber <- Fiber.initUnscoped(Scope.run(DomBackend.mount(ui, ready)))
            _     <- assertEventually(Sync.defer(ready.installed && dom.document.getElementById("sp-tick") != null))
            _     <- tick.set(1)
            _     <- assertEventually(Sync.defer(dom.document.getElementById("sp-tick").textContent == "t:1"))
            _     <- fiber.interrupt
            _     <- fiber.getResult
            _     <- Sync.defer(script.run = run)
        yield assert(runs >= 1, "the repaint did not go through the script")
        end for
    }

end DomBackendMorphParityTest
