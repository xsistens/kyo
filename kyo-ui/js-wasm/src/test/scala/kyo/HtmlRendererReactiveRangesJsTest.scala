package kyo

import kyo.internal.HtmlRenderer
import org.scalajs.dom
import scala.scalajs.js as scalajs

class HtmlRendererReactiveRangesJsTest extends kyo.test.Test[Any]:

    DomTestEnv.install

    override def config = super.config.sequential

    private val hooks =
        """var __focusReturnStack=[],__kyoLifecycle=[];
          |function faEnterPaths(){return {};}
          |function focusAutoPaths(){return {};}
          |function kyoLeavePrepare(root){__kyoLifecycle.push("leave:"+root.id);return [];}
          |function kyoEnterSeed(root){__kyoLifecycle.push("enter:"+root.id);}
          |function applyJsProps(root){var all=[root],desc=root.querySelectorAll("*");for(var i=0;i<desc.length;i++)all.push(desc[i]);
          |  for(var i=0;i<all.length;i++){var names=all[i].getAttributeNames();for(var j=0;j<names.length;j++){var name=names[j];
          |    if(name.indexOf("data-kyo-prop-")===0){var el=all[i],p=name.slice(14),v=el.getAttribute(name),ps=el.__kyoProps||(el.__kyoProps={});
          |      ps[p]={base:ps[p]?ps[p].base:el[p],value:v};el[p]=v;el.removeAttribute(name);}}}
          |  if(typeof __kyoReassertSelect==="function")__kyoReassertSelect(root);}
          |function ba(root){__kyoLifecycle.push("animate:"+root.id);}
          |function kyoSpawnGhosts(){}
          |function sweepFocusAuto(){}
          |function sweepScrollAuto(){}
          |function __kyoPortalTwin(){return null;}
          |function __kyoAdoptPortals(){}
          |function __kyoRetireOrphanTwins(){}
          |function __kyoSyncField(el,name,value){if(name==="value"&&(el.tagName==="INPUT"||el.tagName==="TEXTAREA")&&el.value!==value)el.value=value;}
          |function kyoSetCaret(target,start,end){target.setSelectionRange(start,end);}""".stripMargin

    private def install(html: String): Unit =
        dom.document.body.innerHTML = html
        discard(scalajs.Dynamic.global.eval(hooks + "\n" + HtmlRenderer.reactiveRangesJs))

    private def evalString(source: String): String =
        scalajs.Dynamic.global.eval(source).asInstanceOf[String]

    "embedded registry replaces table rows with contextual parsing and retains anchors" in {
        install("<table><tbody><!--kyo-rs:r--><tr id='old'><td>old</td></tr><!--kyo-re:r--></tbody></table>")
        discard(scalajs.Dynamic.global.eval("kyoRangeReplace('r',\"<tr id='new'><td>new</td></tr>\")"))
        val tbody = dom.document.querySelector("tbody")
        assert(tbody.children.length == 1)
        assert(tbody.children(0).asInstanceOf[dom.Element].tagName == "TR")
        assert(dom.document.getElementById("new") != null)
        assert(evalString("String(__kyoRanges.size)") == "1")
    }

    "embedded registry preserves authored tbody attributes across row category transitions" in {
        val id = "r.1"
        install(
            s"<table><tbody data-kyo-range-host='$id'><!--kyo-rs:$id--><tr><td>row</td></tr><!--kyo-re:$id--></tbody></table>"
        )
        discard(scalajs.Dynamic.global.eval(
            s"kyoRangeReplace('$id',\"<tbody id='authored' data-state='kept'><tr id='section-row'><td>section</td></tr></tbody>\")"
        ))
        val authored = dom.document.querySelector("table > tbody")
        assert(authored.id == "authored")
        assert(authored.getAttribute("data-state") == "kept")
        assert(!authored.hasAttribute("data-kyo-range-host"))
        val tableHtml = dom.document.querySelector("table").innerHTML
        assert(tableHtml.startsWith(s"<!--kyo-rs:$id--><tbody"))
        assert(tableHtml.endsWith(s"</tbody><!--kyo-re:$id-->"))
        discard(scalajs.Dynamic.global.eval(
            s"kyoRangeReplace('$id',\"<tbody data-kyo-range-host='$id'><tr id='returned'><td>row</td></tr></tbody>\")"
        ))
        val returned = dom.document.querySelector("table > tbody")
        assert(returned.id == "")
        assert(!returned.hasAttribute("data-state"))
        assert(returned.getAttribute("data-kyo-range-host") == id)
        assert(returned.querySelector("#returned") != null)
    }

    "embedded registry moves table anchors around multiple authored sections and back" in {
        val id = "r.1"
        install(
            s"<table><tbody data-kyo-range-host='$id'><!--kyo-rs:$id--><tr><td>row</td></tr><!--kyo-re:$id--></tbody></table>"
        )
        discard(scalajs.Dynamic.global.eval(
            s"kyoRangeReplace('$id',\"<tbody id='first'></tbody><tbody id='second'></tbody>\")"
        ))
        assert(dom.document.querySelectorAll("table > tbody").length == 2)
        discard(scalajs.Dynamic.global.eval(
            s"kyoRangeReplace('$id',\"<tbody data-kyo-range-host='$id'><tr id='returned'></tr></tbody>\")"
        ))
        assert(dom.document.querySelectorAll("table > tbody").length == 1)
        assert(dom.document.querySelector("table > tbody #returned") != null)
    }

    "embedded registry rejects malformed and unknown ids without mutation" in {
        install("<!--kyo-rs:r--><span id='kept'>kept</span><!--kyo-re:r-->")
        val before    = dom.document.body.innerHTML
        val malformed = evalString("try{kyoRangeReplace('r1','changed');'no error'}catch(e){e.message}")
        val unknown   = evalString(
            "try{kyoRangeReplace('r.x','changed');'no error'}catch(e){e.message}"
        )
        assert(malformed == "kyo-ui reactive range: malformed replacement id: r1")
        assert(unknown == "kyo-ui reactive range: unknown id: r.x")
        assert(dom.document.body.innerHTML == before)
    }

    "embedded registry rejects a malformed incoming fragment without mutation" in {
        val id     = "r.1"
        val nested = "r.2"
        install(s"<!--kyo-rs:$id--><span id='kept-fragment'>kept</span><!--kyo-re:$id-->")
        val before = dom.document.body.innerHTML
        val error  = evalString(
            s"try{kyoRangeReplace('$id',\"<!--kyo-rs:$nested--><b>broken</b>\");'no error'}catch(e){e.message}"
        )
        assert(error.contains(s"start marker has no end: $nested"))
        assert(dom.document.body.innerHTML == before)
        assert(evalString("String(__kyoRanges.size)") == "1")
    }

    "embedded registry rejects corrupted live anchors without mutation" in {
        val id = "r.1"
        install(s"<!--kyo-rs:$id--><span id='kept'>kept</span><!--kyo-re:$id-->")
        discard(scalajs.Dynamic.global.eval(s"__kyoRanges.get('$id').end.data='kyo-re:r.2'"))
        val before = dom.document.body.innerHTML
        val error  = evalString(s"try{kyoRangeReplace('$id','changed');'no error'}catch(e){e.message}")
        assert(error.contains("markers are corrupted"))
        assert(dom.document.body.innerHTML == before)
    }

    "embedded range replacement restores raw focus and caret without a data path" in {
        val id = "r.1"
        install(s"<!--kyo-rs:$id--><div><input id='raw' value='one'></div><!--kyo-re:$id-->")
        discard(scalajs.Dynamic.global.eval("document.getElementById('raw').focus();document.getElementById('raw').setSelectionRange(1,1)"))
        discard(scalajs.Dynamic.global.eval(
            s"kyoRangeReplace('$id',\"<div><input id='raw' value='two'></div>\")"
        ))
        assert(dom.document.activeElement.id == "raw")
        assert(dom.document.activeElement.asInstanceOf[scalajs.Dynamic].selectionStart.asInstanceOf[Int] == 1)
    }

    "embedded table range restores raw focus relative to the reactive rows" in {
        val id = "r.1"
        install(
            s"<table><tbody data-kyo-range-host='$id'><!--kyo-rs:$id--><tr><td><input id='raw-table' value='one'></td></tr><!--kyo-re:$id--></tbody></table>"
        )
        discard(scalajs.Dynamic.global.eval(
            "document.getElementById('raw-table').focus();document.getElementById('raw-table').setSelectionRange(1,1)"
        ))
        discard(scalajs.Dynamic.global.eval(
            s"kyoRangeReplace('$id',\"<tbody data-kyo-range-host='$id'><tr><td><input id='raw-table' value='two'></td></tr></tbody>\")"
        ))
        assert(dom.document.activeElement.id == "raw-table")
        assert(dom.document.activeElement.asInstanceOf[scalajs.Dynamic].selectionStart.asInstanceOf[Int] == 1)
    }

    private def replace(id: String, html: String): Unit =
        discard(scalajs.Dynamic.global.eval(s"kyoRangeReplace('$id',\"$html\")"))

    private def live(id: String): scalajs.Dynamic = dom.document.getElementById(id).asInstanceOf[scalajs.Dynamic]

    "embedded morph unchecks a checkbox the reader ticked when the render takes the tick back" in {
        val id = "r"
        install(s"<!--kyo-rs:$id--><input id='tick' type='checkbox'><!--kyo-re:$id-->")
        val box = live("tick")
        discard(box.click())
        replace(id, "<input id='tick' type='checkbox' checked>")
        replace(id, "<input id='tick' type='checkbox'>")
        assert(live("tick") eq box)
        assert(!box.checked.asInstanceOf[Boolean], "the checkbox still shows the tick the render took back")
    }

    "embedded morph leaves the reader's tick on an unbound checkbox" in {
        val id = "r"
        install(s"<!--kyo-rs:$id--><input id='free' type='checkbox'><span>0</span><!--kyo-re:$id-->")
        val box = live("free")
        discard(box.click())
        replace(id, "<input id='free' type='checkbox'><span>1</span>")
        assert(live("free") eq box)
        assert(box.checked.asInstanceOf[Boolean], "a repaint took the reader's tick from an unbound checkbox")
    }

    "embedded morph selects the option the render selects after the reader moved away" in {
        val id                       = "r"
        def select(selected: String) =
            Seq("a", "b", "c").map(v => s"<option value='$v'${if v == selected then " selected" else ""}>$v</option>").mkString
        install(s"<!--kyo-rs:$id--><select id='opts'>${select("a")}</select><!--kyo-re:$id-->")
        val opts = live("opts")
        opts.value = "b"
        opts.value = "a"
        replace(id, s"<select id='opts'>${select("b")}</select>")
        assert(live("opts") eq opts)
        assert(opts.value.asInstanceOf[String] == "b", s"the select shows ${opts.value} after the render selected b")
    }

    "embedded morph gives back a JS property the render no longer sets" in {
        val id = "r"
        install(s"<!--kyo-rs:$id--><!--kyo-re:$id-->")
        replace(id, "<input id='mixed' type='checkbox' data-kyo-prop-indeterminate='true'>")
        val box = live("mixed")
        assert(box.indeterminate.asInstanceOf[Boolean])
        replace(id, "<input id='mixed' type='checkbox'>")
        assert(live("mixed") eq box)
        assert(!box.indeterminate.asInstanceOf[Boolean], "the checkbox kept the mixed dash the render dropped")
        replace(id, "<input id='mixed' type='checkbox' data-kyo-prop-indeterminate='true'>")
        assert(box.indeterminate.asInstanceOf[Boolean])
    }

    "embedded paint of options inside a bound select selects its value" in {
        val id = "r"
        install(s"<select id='late' data-kyo-prop-value='b'><!--kyo-rs:$id--><!--kyo-re:$id--></select>")
        discard(scalajs.Dynamic.global.eval("applyJsProps(document.body)"))
        discard(scalajs.Dynamic.global.eval(
            s"kyoRangeReplace('$id',\"<option value='a'>a</option><option value='b'>b</option><option value='c'>c</option>\")"
        ))
        val shown = dom.document.getElementById("late").asInstanceOf[scalajs.Dynamic].value.asInstanceOf[String]
        assert(shown == "b", s"the select shows $shown once its options arrived")
    }

    "embedded range replacement applies JS properties to a top-level element" in {
        val id = "r.1"
        install(s"<!--kyo-rs:$id--><!--kyo-re:$id-->")
        discard(scalajs.Dynamic.global.eval(
            s"kyoRangeReplace('$id',\"<input id='property' type='checkbox' data-kyo-prop-indeterminate='true'>\")"
        ))
        val property = dom.document.getElementById("property")
        assert(property.asInstanceOf[scalajs.Dynamic].indeterminate.asInstanceOf[Boolean])
        assert(!property.hasAttribute("data-kyo-prop-indeterminate"))
    }

    "embedded range replacement applies JS properties to nested elements" in {
        val id = "r.1"
        install(s"<!--kyo-rs:$id--><!--kyo-re:$id-->")
        discard(scalajs.Dynamic.global.eval(
            s"kyoRangeReplace('$id',\"<section><div><input id='nested-property' type='checkbox' data-kyo-prop-indeterminate='true'></div></section>\")"
        ))
        val property = dom.document.getElementById("nested-property")
        assert(property.asInstanceOf[scalajs.Dynamic].indeterminate.asInstanceOf[Boolean])
        assert(!property.hasAttribute("data-kyo-prop-indeterminate"))
    }

    "embedded registry preserves every exposed restricted parent and keyed range" in {
        val row    = "r.1"
        val cell   = "r.2"
        val list   = "r.3"
        val order  = "r.4"
        val option = "r.5"
        val keyed  = "r.6"
        install(
            s"<table><tbody><!--kyo-rs:$row--><tr id='old-row'><td>old</td></tr><!--kyo-re:$row-->" +
                s"<tr><!--kyo-rs:$cell--><td id='old-cell'>old</td><!--kyo-re:$cell--></tr></tbody></table>" +
                s"<ul><!--kyo-rs:$list--><li>old</li><!--kyo-re:$list--></ul>" +
                s"<ol><!--kyo-rs:$order--><li>old</li><!--kyo-re:$order--></ol>" +
                s"<select><!--kyo-rs:$option--><option>old</option><!--kyo-re:$option--></select>" +
                s"<div><!--kyo-rs:$keyed--><button>old</button><!--kyo-re:$keyed--></div>"
        )
        discard(scalajs.Dynamic.global.eval(s"kyoRangeReplace('$row',\"<tr id='new-row'><td>new</td></tr>\")"))
        discard(scalajs.Dynamic.global.eval(s"kyoRangeReplace('$cell',\"<th id='new-cell'>new</th>\")"))
        discard(scalajs.Dynamic.global.eval(s"kyoRangeReplace('$list',\"<li id='new-list'>new</li>\")"))
        discard(scalajs.Dynamic.global.eval(s"kyoRangeReplace('$order',\"<li id='new-order'>new</li>\")"))
        discard(scalajs.Dynamic.global.eval(s"kyoRangeReplace('$option',\"<option id='new-option'>new</option>\")"))
        discard(scalajs.Dynamic.global.eval(s"kyoRangeReplace('$keyed',\"<button id='new-keyed'>new</button>\")"))
        assert(dom.document.querySelector("table > tbody > #new-row") != null)
        assert(dom.document.querySelector("table > tbody > tr > #new-cell") != null)
        assert(dom.document.querySelector("ul > #new-list") != null)
        assert(dom.document.querySelector("ol > #new-order") != null)
        assert(dom.document.querySelector("select > #new-option") != null)
        assert(dom.document.querySelector("div > #new-keyed") != null)
        assert(evalString("String(__kyoRanges.size)") == "6")
    }

    "embedded synthetic host transition treats an authored tbody as the semantic lifecycle root" in {
        val id = "r.1"
        install(
            s"<table><tbody data-kyo-range-host='$id'><!--kyo-rs:$id--><tr id='old-row'><td>row</td></tr><!--kyo-re:$id--></tbody></table>"
        )
        discard(scalajs.Dynamic.global.eval(
            s"kyoRangeReplace('$id',\"<tbody id='semantic-host' tabindex='-1' data-kyo-path='0.0' data-kyo-focus-auto='1' data-kyo-enter='enter' data-kyo-leave='leave' data-kyo-prop-rangeprobe='applied'><tr><td>section</td></tr></tbody>\")"
        ))
        val authored = dom.document.getElementById("semantic-host")
        assert(evalString("String(document.getElementById('semantic-host').rangeprobe)") == "applied")
        assert(!authored.hasAttribute("data-kyo-prop-rangeprobe"))
        assert(dom.document.activeElement eq authored)
        assert(evalString("__kyoLifecycle.indexOf('enter:semantic-host')>=0?'yes':'no'") == "yes")
        assert(evalString("__kyoLifecycle.indexOf('animate:semantic-host')>=0?'yes':'no'") == "yes")
        discard(scalajs.Dynamic.global.eval(
            s"kyoRangeReplace('$id',\"<tbody data-kyo-range-host='$id'><tr id='returned-row' data-kyo-path='0.1' data-kyo-enter='enter' data-kyo-prop-rangeprobe='returned'><td>row</td></tr></tbody>\")"
        ))
        assert(evalString("__kyoLifecycle.indexOf('leave:semantic-host')>=0?'yes':'no'") == "yes")
        assert(evalString("String(document.getElementById('returned-row').rangeprobe)") == "returned")
        assert(evalString("__kyoLifecycle.indexOf('enter:returned-row')>=0?'yes':'no'") == "yes", evalString("__kyoLifecycle.join(',')"))
        assert(evalString("__kyoLifecycle.indexOf('animate:returned-row')>=0?'yes':'no'") == "yes")
    }

end HtmlRendererReactiveRangesJsTest
