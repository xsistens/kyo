package kyo.devtools

/** The overlay, as one self-contained browser script.
  *
  * JavaScript rather than Scala.js, and deliberately. The overlay has to run under BOTH runners, and under
  * `UI.runHandlers` the browser holds no Scala.js at all — the server sends HTML and the hand-written client
  * in `HtmlRenderer.clientJs`, which is the precedent this file follows. Written in Scala.js it would need a
  * second implementation, or a linked bundle shipped as a JVM resource, to say the same thing twice. Here the
  * page-drawing lives in one place and Scala sends it nothing but numbers.
  *
  * Everything with arithmetic in it — the rate, the percentile, the sparkline buckets — is in
  * [[DevtoolsStore]], where it can be asserted on exact values. This file resolves geometry and draws.
  *
  * '''It cannot perturb what it measures.''' The host is a fixed, pointer-transparent element appended to
  * `<body>` with a shadow root, outside the app's container and outside its stylesheet in both directions. It
  * is not built from kyo-ui, holds no signal and owns no reactive region, so the engine has nothing to report
  * about it — the observer cannot appear in its own statistics.
  *
  * '''The contract with Scala.''' `__kyoDev.install(config)` once, then `__kyoDev.push(snapshot)` at whatever
  * rate the transport allows (about 10 Hz is plenty; the eye cannot use more and the wire should not carry
  * it). `__kyoDev.onCommand` is set by the host to receive `pause`, `resume` and `reset` from the panel.
  *
  * Payload keys are one and two letters because under server-push every snapshot crosses a WebSocket: the
  * long names would be most of the frame. The reader for them is `snapshotJson` in [[DevtoolsWire]], and the
  * two must be changed together.
  */
object DevtoolsJs:

    /** The overlay script. Injected verbatim into the page by both runners. */
    val script: String =
        """(function(){
          |if(window.__kyoDev)return;
          |var HOST="__kyo-devtools";
          |// Rate thresholds, in renders per second. The scale is deliberately coarse: the question a reader
          |// asks of a badge is "is this normal, busy, or wrong", and three answers is what a glance can hold.
          |var ACTIVE=0.5,WARM=2,HOT=8;
          |// A wasted-render ratio only overrides the rate scale once there are enough renders for the ratio
          |// to mean anything; one wasted render out of one is not a finding.
          |var WASTED_RATIO=0.5,WASTED_MIN=4;
          |var TIERS={idle:"#9ca3af",active:"#60a5fa",warm:"#f59e0b",hot:"#ef4444",wasted:"#f43f5e"};
          |var opts={badges:true,flash:true,threshold:0,onlyWasted:false,maxBadges:200};
          |var rows=[],meta={o:false,h:[]},byKey={},prevTotals={},flashUntil={},labels={},frozen=false;
          |var host,root,layer,panel,pill,popover,pinned=null,frame=0,markers={},markersFresh=false,nodes={};
          |
          |function css(){return ""+
          |":host{all:initial}"+
          |"*{box-sizing:border-box;font-family:ui-monospace,SFMono-Regular,Menlo,monospace}"+
          |".badge{position:fixed;pointer-events:auto;cursor:pointer;font-size:11px;line-height:1;"+
          |"font-weight:600;padding:2px 5px;border-radius:3px;background:#0b0f17;border:1px solid;"+
          |"white-space:nowrap;transition:opacity .15s linear;letter-spacing:.02em;"+
          |// Two rings, not one: the inner dark line separates the chip from a dark app, the outer shadow
          |// from a light one. A chip that is only legible against the theme it was designed on is legible
          |// by accident, and a number nobody can read is worse than no number.
          |"box-shadow:0 0 0 1px rgba(0,0,0,.7),0 1px 4px rgba(0,0,0,.55)}"+
          |".badge:hover{opacity:1!important;background:#141c2b}"+
          |".ring{position:fixed;pointer-events:none;border-radius:2px;border-style:none;border-width:1px;"+
          |"border-color:transparent}"+
          |// Hovering the count outlines what it counts, and the outline stays while the details are open.
          |// The sibling selector is why the ring is painted immediately after its badge: it makes the
          |// pointer, not a timer or a repaint, decide when the box is shown.
          |".badge:hover + .ring,.ring.pin{border-style:solid!important;opacity:1!important}"+
          |".ring.flash{animation:kf .4s ease-out 1}"+
          |"@keyframes kf{from{opacity:1}to{opacity:.15}}"+
          |".pulse{animation:kp 2s ease-in-out infinite}"+
          |"@keyframes kp{0%,100%{opacity:.35}50%{opacity:1}}"+
          |".pop{position:fixed;pointer-events:auto;z-index:3;min-width:260px;max-width:340px;"+
          |"background:rgba(15,23,42,.97);color:#e5e7eb;border:1px solid #334155;border-radius:6px;"+
          |"padding:8px 10px;font-size:11px;line-height:1.5;box-shadow:0 8px 24px rgba(0,0,0,.45)}"+
          |".pop h1{margin:0;font-size:12px;color:#f8fafc;font-weight:600}"+
          |".pop .sub{color:#94a3b8;font-size:10px;margin-bottom:6px}"+
          |".pop table{width:100%;border-collapse:collapse}"+
          |".pop td{padding:1px 0;vertical-align:top}"+
          |".pop td.n{text-align:right;color:#f1f5f9;font-variant-numeric:tabular-nums}"+
          |".pop td.k{color:#94a3b8}"+
          |".pop .warn{color:#f43f5e}"+
          |".pop .snip{margin-top:6px;padding-top:6px;border-top:1px solid #1e293b;color:#64748b;"+
          |"white-space:pre-wrap;word-break:break-word;font-size:10px}"+
          |".pill{position:fixed;right:12px;bottom:12px;pointer-events:auto;cursor:pointer;z-index:4;"+
          |"background:rgba(15,23,42,.92);color:#cbd5e1;border:1px solid #334155;border-radius:999px;"+
          |"padding:4px 10px;font-size:11px;font-variant-numeric:tabular-nums}"+
          |".panel{position:fixed;right:12px;bottom:12px;pointer-events:auto;z-index:4;width:340px;"+
          |"max-height:60vh;overflow:auto;background:rgba(15,23,42,.97);color:#e5e7eb;border:1px solid #334155;"+
          |"border-radius:8px;font-size:11px;box-shadow:0 10px 30px rgba(0,0,0,.5)}"+
          |".panel header{display:flex;align-items:center;gap:6px;padding:6px 8px;border-bottom:1px solid #1e293b;"+
          |"position:sticky;top:0;background:rgba(15,23,42,.99);cursor:move}"+
          |".panel .grow{flex:1}"+
          |".panel button{background:#1e293b;color:#cbd5e1;border:1px solid #334155;border-radius:4px;"+
          |"padding:2px 6px;font-size:10px;cursor:pointer;font-family:inherit}"+
          |".panel button.on{background:#334155;color:#f8fafc}"+
          |".panel .rowitem{display:flex;gap:6px;align-items:center;padding:3px 8px;cursor:pointer}"+
          |".panel .rowitem:hover{background:#1e293b}"+
          |".panel .rowitem .nm{flex:1;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}"+
          |".panel .rowitem .num{color:#f1f5f9;font-variant-numeric:tabular-nums}"+
          |".panel .note{padding:4px 8px;color:#f59e0b;font-size:10px}"+
          |".spark{display:inline-block;vertical-align:middle}";}
          |
          |// ---- geometry -------------------------------------------------------------------------------
          |
          |// An HTML region is delimited by comment markers, not wrapped in an element, so its box is the union
          |// of whatever nodes currently sit between them — which is exactly what a DOM Range measures. The pair
          |// survives a repaint (only the content between them is replaced), so it is cached and the cache is
          |// rebuilt only when a lookup finds a marker that has left the document.
          |function scanMarkers(){
          |  markers={};markersFresh=true;
          |  var w=document.createTreeWalker(document.body,NodeFilter.SHOW_COMMENT,null,false),open={},n;
          |  while((n=w.nextNode())){
          |    var d=n.data;
          |    if(d.lastIndexOf("kyo-rs:",0)===0){var s=d.slice(7),sp=s.indexOf(" ");open[sp<0?s:s.slice(0,sp)]=n;}
          |    else if(d.lastIndexOf("kyo-re:",0)===0){var e=d.slice(7);if(open[e]){markers[e]=[open[e],n];delete open[e];}}
          |  }
          |}
          |function markerPair(id){
          |  var p=markers[id];
          |  if(p&&p[0].isConnected&&p[1].isConnected)return p;
          |  if(!markersFresh){scanMarkers();p=markers[id];}
          |  return p&&p[0].isConnected&&p[1].isConnected?p:null;
          |}
          |function rectOf(row){
          |  if(row.id){
          |    var p=markerPair(row.id);
          |    if(p){var r=document.createRange();r.setStartAfter(p[0]);r.setEndBefore(p[1]);
          |      var b=r.getBoundingClientRect();if(b.width||b.height)return b;}
          |  }
          |  var el=elementOf(row);
          |  return el?el.getBoundingClientRect():null;
          |}
          |// The first painted ELEMENT of a region, as opposed to its box. An HTML region's content
          |// starts right after its opening marker, but a region may open with whitespace or a text node.
          |function elementOf(row){
          |  if(row.id){
          |    var p=markerPair(row.id);
          |    if(p){var n=p[0].nextSibling;
          |      while(n&&n!==p[1]){if(n.nodeType===1)return n;n=n.nextSibling;}}
          |  }
          |  return document.querySelector('[data-kyo-path="'+cssEscape(row.k)+'"]');
          |}
          |// What to CALL a region.
          |//
          |// Not the source position, which is what one would reach for first and what does not survive
          |// contact with idiomatic kyo. A component written `def view(using Frame): UI` hands its own
          |// frame to every node it builds — that is what a `using` parameter is for — so the `Frame` on
          |// an AST node is the position of whoever threaded it in, and in an app that threads from the
          |// entry point that is one line for the whole tree. Reported as `row.f`/`row.l` still, under a
          |// label that says what it actually is, but never as the name.
          |//
          |// The DOM knows better and knows it per region: the tag it painted, plus its most telling
          |// class. A kyo-uic component leaves its own `p-<component>` class on that element, so a region
          |// that paints one reads as `div.p-datatable` — the component's real name, from the page.
          |function labelOf(row){
          |  if(labels[row.k])return labels[row.k];
          |  var el=elementOf(row);
          |  // A region with nothing painted right now (a parent replaced the subtree it lived in) has no
          |  // element to be named after; the frame label is thin but it is what is left.
          |  if(!el)return row.n||row.k;
          |  var tag=el.tagName.toLowerCase(),cls=pickClass(el);
          |  var name=cls?tag+"."+cls:tag;
          |  labels[row.k]=name;
          |  return name;
          |}
          |function pickClass(el){
          |  var list=(el.getAttribute("class")||"").split(/\s+/).filter(Boolean),i;
          |  for(i=0;i<list.length;i++)if(list[i].lastIndexOf("p-",0)===0&&list[i].length<28)return list[i];
          |  for(i=0;i<list.length;i++)if(list[i].length<28)return list[i];
          |  return null;
          |}
          |function cssEscape(s){return String(s).replace(/["\\]/g,"\\$&");}
          |
          |// ---- presentation ---------------------------------------------------------------------------
          |
          |function wastedShare(row){return row.t?row.w/row.t:0;}
          |function tierOf(row){
          |  if(row.t>=WASTED_MIN&&wastedShare(row)>WASTED_RATIO)return "wasted";
          |  if(row.r>=HOT)return "hot";
          |  if(row.r>=WARM)return "warm";
          |  if(row.r>=ACTIVE)return "active";
          |  return "idle";
          |}
          |// Quiet, not invisible. The tier is carried by COLOUR first; opacity only trims the idle end,
          |// and trimming it as far as it once went made the common case unreadable over a dark app.
          |var ALPHA={idle:.82,active:.92,warm:1,hot:1,wasted:1};
          |var RING={idle:{style:"none",width:1},active:{style:"dashed",width:1},warm:{style:"solid",width:1},
          |  hot:{style:"solid",width:2},wasted:{style:"solid",width:2}};
          |function fmtRate(r){return r<10?r.toFixed(1):String(Math.round(r));}
          |function fmtMs(ns){return ns>=1e6?(ns/1e6).toFixed(1)+" ms":ns>=1e3?(ns/1e3).toFixed(1)+" \u00b5s":ns+" ns";}
          |function fmtBytes(b){return b>=1048576?(b/1048576).toFixed(1)+" MB":b>=1024?(b/1024).toFixed(1)+" kB":b+" B";}
          |function spark(values,w,h,color){
          |  var max=1,i;for(i=0;i<values.length;i++)if(values[i]>max)max=values[i];
          |  var step=w/Math.max(1,values.length),d="",x=0;
          |  for(i=0;i<values.length;i++){var y=h-(values[i]/max)*h;d+=(i?"L":"M")+x.toFixed(1)+" "+y.toFixed(1);x+=step;}
          |  return '<svg class="spark" width="'+w+'" height="'+h+'" viewBox="0 0 '+w+' '+h+'">'+
          |    '<path d="'+d+'" fill="none" stroke="'+color+'" stroke-width="1"/></svg>';
          |}
          |
          |// ---- badges ---------------------------------------------------------------------------------
          |
          |// Only what is on screen gets a badge, and never more than the budget: a table of a thousand rows
          |// would otherwise put a thousand absolutely positioned elements over the page every frame, and the
          |// tool would be the slowest thing running. Badges too close together collapse into one that says how
          |// many are underneath, so a dense list reads as a count rather than as a smear.
          |function visible(){
          |  var out=[],i,vh=innerHeight,vw=innerWidth;
          |  for(i=0;i<rows.length&&out.length<opts.maxBadges;i++){
          |    var row=rows[i];
          |    if(row.t<opts.threshold)continue;
          |    if(opts.onlyWasted&&wastedShare(row)<=0)continue;
          |    var b=rectOf(row);
          |    if(!b)continue;
          |    if(b.bottom<0||b.top>vh||b.right<0||b.left>vw)continue;
          |    out.push({row:row,rect:b});
          |  }
          |  return out;
          |}
          |function cluster(items){
          |  var cells={},out=[],i;
          |  for(i=0;i<items.length;i++){
          |    var it=items[i],cx=Math.round(it.rect.left/18),cy=Math.round(it.rect.top/18),k=cx+":"+cy;
          |    if(cells[k]){cells[k].hidden.push(it.row);}
          |    else{cells[k]={item:it,hidden:[]};out.push(cells[k]);}
          |  }
          |  return out;
          |}
          |// Nodes are REUSED across paints, one pair per region, never rebuilt from a string.
          |//
          |// That is not an optimization, it is what makes hovering work at all. Replacing the markup twice a
          |// second takes the element out from under the pointer, and with it `:hover` — the outline would
          |// blink off on exactly the badge the reader is resting on. A badge and its ring are also adjacent
          |// siblings on purpose: `.badge:hover + .ring` is the whole hover mechanism, and it needs them so.
          |function paintBadges(){
          |  if(!opts.badges){dropBadges(function(){return true;});return;}
          |  // Every rect is read before a single style is written. Interleaving them would invalidate layout
          |  // between reads and turn one pass into one per badge.
          |  var groups=cluster(visible()),seen={},i;
          |  for(i=0;i<groups.length;i++){
          |    var g=groups[i],row=g.item.row,b=g.item.rect,tier=tierOf(row),color=TIERS[tier];
          |    var pair=nodes[row.k];
          |    if(!pair){
          |      pair={badge:document.createElement("div"),ring:document.createElement("div")};
          |      pair.badge.className="badge";
          |      pair.badge.setAttribute("data-k",row.k);
          |      layer.appendChild(pair.badge);layer.appendChild(pair.ring);
          |      nodes[row.k]=pair;
          |    }
          |    seen[row.k]=true;
          |    var flashing=opts.flash&&flashUntil[row.k]>frame,ring=RING[tier];
          |    var text=(g.hidden.length?"\u22ef "+(g.hidden.length+1):String(row.t))+(tier==="wasted"?" \u26a0":"");
          |    if(pair.badge.textContent!==text)pair.badge.textContent=text;
          |    pair.badge.style.cssText="left:"+b.left+"px;top:"+Math.max(0,b.top-2)+"px;color:"+color+
          |      ";border-color:"+color+";opacity:"+ALPHA[tier];
          |    pair.ring.className="ring"+(flashing?" flash":"")+(tier==="wasted"?" pulse":"")+
          |      (pinned&&pinned.key===row.k?" pin":"");
          |    pair.ring.style.cssText="left:"+b.left+"px;top:"+b.top+"px;width:"+b.width+"px;height:"+
          |      b.height+"px;border-color:"+color+";border-width:"+ring.width+"px;border-style:"+ring.style+
          |      ";opacity:"+(flashing?1:ALPHA[tier]*.6);
          |  }
          |  dropBadges(function(k){return !seen[k];});
          |}
          |function dropBadges(gone){
          |  for(var k in nodes)if(gone(k)){
          |    layer.removeChild(nodes[k].badge);layer.removeChild(nodes[k].ring);delete nodes[k];
          |  }
          |}
          |
          |// ---- detail ---------------------------------------------------------------------------------
          |
          |function detailHtml(row){
          |  var self=row.t-row.cr,share=wastedShare(row);
          |  function line(k,v,cls){return '<tr><td class="k">'+k+'</td><td class="n'+(cls?" "+cls:"")+'">'+v+'</td></tr>';}
          |  var rowsInfo=row.rs?line("Letzter Patch",row.rr+" von "+row.rs+" Zeilen"):"";
          |  return '<h1>'+esc(labelOf(row))+'</h1>'+
          |    '<div class="sub">'+esc(row.k)+'</div>'+
          |    spark(row.h||[],300,22,TIERS[tierOf(row)])+
          |    '<table>'+
          |      line("Renders",row.t+"  \u00b7  "+fmtRate(row.r)+"/s  \u00b7  Peak "+fmtRate(row.pk)+"/s")+
          |      line("&nbsp;&nbsp;eigene",self)+
          |      line("&nbsp;&nbsp;durch Eltern",row.cr)+
          |      line("&nbsp;&nbsp;verschwendet",row.w+" ("+Math.round(share*100)+"%)",share>WASTED_RATIO?"warn":"")+
          |      (row.ch?line("Attribut-Writes",row.ch):"")+
          |      (row.tx?line("Text-Writes",row.tx):"")+
          |      // A zero here would be read as "this render was free". It is not: under the browser mount the
          |      // engine measures with `Clock.nowMonotonic`, whose resolution there is a millisecond, and a
          |      // repaint faster than that measures as nothing. Saying so is the only honest reading.
          |      line("Dauer",row.a||row.p?"avg "+fmtMs(row.a)+"  \u00b7  p95 "+fmtMs(row.p):"unter der Uhraufl\u00f6sung")+
          |      line("Gesendet",fmtBytes(row.b))+
          |      rowsInfo+
          |      line("Zuletzt",row.i<0.05?"gerade eben":"vor "+row.i.toFixed(1)+" s")+
          |      line("Eingehängt bei",esc(row.f)+":"+row.l+(row.c?" \u00b7 "+esc(row.c):""))+
          |    '</table>'+
          |    (row.s?'<div class="snip">'+esc(row.s)+'</div>':"");
          |}
          |function esc(s){return String(s==null?"":s).replace(/[&<>"]/g,function(c){
          |  return {"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;"}[c];});}
          |function showPopover(key,anchor){
          |  var row=byKey[key];
          |  if(!row){hidePopover();return;}
          |  popover.innerHTML=detailHtml(row);
          |  popover.style.display="block";
          |  var pr=popover.getBoundingClientRect();
          |  popover.style.left=Math.min(innerWidth-pr.width-8,Math.max(8,anchor.left))+"px";
          |  popover.style.top=(anchor.bottom+6+pr.height>innerHeight?Math.max(8,anchor.top-pr.height-6):anchor.bottom+6)+"px";
          |}
          |function hidePopover(){popover.style.display="none";}
          |// The popover is anchored to the badge as it is NOW, not to where it was when it was opened: the
          |// page scrolls and the region moves, and a detail card left behind at the old coordinates points at
          |// whatever happens to be there.
          |function followPinned(){
          |  var pair=nodes[pinned.key];
          |  if(pair)showPopover(pinned.key,pair.badge.getBoundingClientRect());
          |  else hidePopover();
          |}
          |
          |// ---- panel ----------------------------------------------------------------------------------
          |
          |function paintPanel(){
          |  var total=0,wasted=0,repaints=0,i;
          |  for(i=0;i<rows.length;i++){total+=rows[i].r;wasted+=rows[i].w;repaints+=rows[i].t;}
          |  var share=repaints?Math.round(wasted/repaints*100):0;
          |  pill.textContent="\u27f3 "+fmtRate(total)+"/s \u00b7 "+share+"%"+(share>25?" \u26a0":"");
          |  if(panel.style.display==="none")return;
          |  var top=rows.slice(0,25),html=[];
          |  // Pause is local and always available: it freezes the display, which is what a reader wants when
          |  // they stop to read one, and it needs nothing from the host. Reset has to reach the counters
          |  // themselves, so it appears only where a host wired itself up to receive it — under server-push
          |  // today there is no way back up the socket, and a button that does nothing is worse than none.
          |  html.push('<header><strong>kyo-ui renders</strong><span class="grow"></span>'+
          |    '<button data-cmd="freeze" class="'+(frozen?"on":"")+'">'+(frozen?"Weiter":"Pause")+'</button>'+
          |    (window.__kyoDev.onCommand?'<button data-cmd="reset">Reset</button>':"")+
          |    '<button data-opt="badges" class="'+(opts.badges?"on":"")+'">Badges</button>'+
          |    '<button data-opt="onlyWasted" class="'+(opts.onlyWasted?"on":"")+'">\u26a0</button>'+
          |    '<button data-cmd="close">\u2715</button></header>');
          |  html.push('<div style="padding:6px 8px">'+spark(meta.h||[],320,28,TIERS.active)+'</div>');
          |  if(meta.o)html.push('<div class="note">Regionen-Limit erreicht \u2014 neue Regionen werden nicht mehr gez\u00e4hlt.</div>');
          |  for(i=0;i<top.length;i++){
          |    var row=top[i],tier=tierOf(row);
          |    html.push('<div class="rowitem" data-k="'+cssEscape(row.k)+'">'+
          |      '<span style="color:'+TIERS[tier]+'">\u25cf</span>'+
          |      '<span class="nm" title="'+esc(row.k)+'">'+esc(labelOf(row))+'</span>'+
          |      (wastedShare(row)>WASTED_RATIO&&row.t>=WASTED_MIN?'<span style="color:'+TIERS.wasted+'">\u26a0</span>':"")+
          |      '<span class="num">'+row.t+'</span>'+
          |      '<span class="num" style="color:'+TIERS[tier]+';width:44px;text-align:right">'+fmtRate(row.r)+'/s</span>'+
          |      spark(row.h||[],48,12,TIERS[tier])+'</div>');
          |  }
          |  panel.innerHTML=html.join("");
          |}
          |
          |// ---- loop -----------------------------------------------------------------------------------
          |
          |function tick(){
          |  frame++;
          |  // Half the display rate. The numbers change ten times a second at most and the geometry pass is the
          |  // only per-frame cost worth having; spending every frame on it would be the tool taxing the app it
          |  // is there to measure.
          |  if(frame%2===0){markersFresh=false;paintBadges();if(pinned)followPinned();}
          |  requestAnimationFrame(tick);
          |}
          |
          |// ---- wiring ---------------------------------------------------------------------------------
          |
          |function install(config){
          |  if(host)return;
          |  if(config)for(var k in config)if(config.hasOwnProperty(k))opts[k]=config[k];
          |  host=document.createElement("div");
          |  host.id=HOST;
          |  host.setAttribute("style","position:fixed;inset:0;pointer-events:none;z-index:2147483000");
          |  // A shadow root, so the app's stylesheet cannot reach the overlay and the overlay's cannot reach
          |  // the app. An overlay that restyles the page it is measuring is measuring a different page.
          |  root=host.attachShadow({mode:"open"});
          |  var style=document.createElement("style");style.textContent=css();root.appendChild(style);
          |  layer=document.createElement("div");root.appendChild(layer);
          |  popover=document.createElement("div");popover.className="pop";popover.style.display="none";
          |  root.appendChild(popover);
          |  pill=document.createElement("div");pill.className="pill";pill.textContent="\u27f3 0/s";
          |  root.appendChild(pill);
          |  panel=document.createElement("div");panel.className="panel";panel.style.display="none";
          |  root.appendChild(panel);
          |  pill.addEventListener("click",function(){panel.style.display="block";pill.style.display="none";paintPanel();});
          |  root.addEventListener("click",onClick);
          |  // A click anywhere in the page closes an open card. It has to be on the document, because a click
          |  // in the app never reaches the overlay's own root — and it is passive: nothing is prevented,
          |  // nothing is stopped, the app sees the click exactly as it would have.
          |  document.addEventListener("click",onDocClick,true);
          |  // Appended to <html>, not <body>. `UI.runMount(ui)` without a selector mounts by assigning
          |  // body.innerHTML, which would delete an overlay parked there — and the mount runs after this, so
          |  // the overlay would vanish on the very first paint of exactly the app it is there to watch. A
          |  // fixed-position element outside body is unusual but positions identically.
          |  document.documentElement.appendChild(host);
          |  requestAnimationFrame(tick);
          |}
          |// Opening on hover meant the card was never dismissed on purpose, only replaced by the next one it
          |// happened over — so it sat on top of the app until the pointer found another badge. A click opens
          |// it, the same click closes it, and any click elsewhere closes it too.
          |function onDocClick(e){
          |  if(!pinned)return;
          |  // The click that OPENS a card also reaches here. `composedPath` is what sees through the shadow
          |  // boundary and can tell that one apart from a click in the page.
          |  var path=e.composedPath?e.composedPath():[],i;
          |  for(i=0;i<path.length;i++)if(path[i]===host)return;
          |  pinned=null;hidePopover();paintBadges();
          |}
          |function onClick(e){
          |  var t=e.target;
          |  var cmd=t.getAttribute&&t.getAttribute("data-cmd");
          |  if(cmd==="close"){panel.style.display="none";pill.style.display="block";return;}
          |  if(cmd==="freeze"){frozen=!frozen;paintPanel();return;}
          |  if(cmd){if(window.__kyoDev.onCommand)window.__kyoDev.onCommand(cmd);return;}
          |  var opt=t.getAttribute&&t.getAttribute("data-opt");
          |  if(opt){opts[opt]=!opts[opt];paintPanel();return;}
          |  var badge=t.closest&&t.closest(".badge");
          |  if(badge){var k=badge.getAttribute("data-k");
          |    pinned=pinned&&pinned.key===k?null:{key:k};
          |    if(pinned)showPopover(k,badge.getBoundingClientRect());else hidePopover();
          |    paintBadges();return;}
          |  var item=t.closest&&t.closest(".rowitem");
          |  if(item){var key=item.getAttribute("data-k"),row=byKey[key];
          |    if(row){var b=rectOf(row);if(b){scrollTo({top:scrollY+b.top-innerHeight/3,behavior:"smooth"});
          |      flashUntil[key]=frame+60;}}
          |    return;}
          |  if(!(t.closest&&t.closest(".pop"))){pinned=null;hidePopover();paintBadges();}
          |}
          |
          |function push(snapshot){
          |  if(frozen)return;
          |  var next=snapshot.rows||[],i;
          |  meta={o:!!snapshot.o,h:snapshot.h||[]};
          |  byKey={};
          |  for(i=0;i<next.length;i++){
          |    var row=next[i];byKey[row.k]=row;
          |    // A render that happened since the last snapshot gets a flash. Comparing totals rather than
          |    // trusting a rate means a single render in an otherwise idle region still shows itself.
          |    if(prevTotals[row.k]!==undefined&&row.t>prevTotals[row.k])flashUntil[row.k]=frame+24;
          |    prevTotals[row.k]=row.t;
          |  }
          |  rows=next;
          |  paintPanel();
          |}
          |
          |function uninstall(){
          |  if(!host)return;
          |  document.removeEventListener("click",onDocClick,true);
          |  host.parentNode&&host.parentNode.removeChild(host);
          |  host=root=layer=panel=pill=popover=null;rows=[];byKey={};labels={};nodes={};pinned=null;
          |}
          |
          |window.__kyoDev={install:install,push:push,uninstall:uninstall,onCommand:null,
          |  setOption:function(k,v){opts[k]=v;paintPanel();}};
          |})();
          |""".stripMargin

end DevtoolsJs
