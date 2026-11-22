package kyo.internal

/** The one implementation of the range morph and the list patch, as JavaScript text both clients run.
  *
  * A morph is a walk over the DOM with its decisions in the walk, so it cannot be data the way [[KeyPolicy]] is, and no translation from Scala
  * to JavaScript exists in this build. The text is the single source: the body of a factory over `h`, the client's hooks, which answers what a
  * client calls. The server-push page installs it in its script; the browser mount builds it with `Function`. What differs between the
  * clients is only behind `h`: how a field's property is written, where a portal's twin is, how a rewrite tells the registry, how an
  * animation starts, whether an element's content can be an attach hook's, and what a list patch reads before it moves anything and does
  * after. The hooks are written where their client lives, in [[HtmlRenderer]] and `DomBackend`.
  *
  * `DomBackendMorphParityTest` holds a table of cases to the answers both clients have to give.
  */
private[kyo] object MorphScript:

    val js: String =
        """          |// ---- range morph ----
          |// Reconciling a re-rendered region against its live nodes instead of replacing them is what keeps node
          |// identity across a paint: focus, caret, scroll position, an imperatively bound attribute, anything a
          |// DOM-local expando holds. Replacing the range is always correct and always loses all of it, so it stays
          |// the fallback rather than the default.
          |function __kyoMorphCompatible(from,to){
          |  if(from.nodeType!==to.nodeType)return false;
          |  if(from.nodeType!==1)return true;
          |  return from.tagName===to.tagName&&from.namespaceURI===to.namespaceURI;
          |}
          |function __kyoMorphNode(from,to){
          |  if(from.nodeType!==1){if(from.nodeValue!==to.nodeValue)from.nodeValue=to.nodeValue;return;}
          |  // The same element value shown again, with an onAttached hook that may have put content of its own inside: its children are the
          |  // hook's. Only a client with such hooks asks.
          |  var attached=h.guardAttached?from.getAttribute("data-kyo-attach"):null;
          |  var unchanged=attached!==null&&attached===to.getAttribute("data-kyo-attach");
          |  __kyoMorphAttrs(from,to);
          |  // A focused contenteditable would lose its caret if its children were rewritten mid-edit; leave its
          |  // subtree alone (INPUT and TEXTAREA have no element children, so they need no such guard).
          |  if(!unchanged&&!((from===document.activeElement)&&from.hasAttribute("contenteditable")))
          |    __kyoMorphRun(from,from.firstChild,null,to.firstChild,null);
          |}
          |// Give back every JS property a previous render set and the incoming element no longer carries. applyJsProps
          |// removes the data-kyo-prop-* attribute once applied, so the attribute walk cannot see a property leave; the
          |// value before the first assignment waits in __kyoProps.
          |function __kyoRestoreProps(from,to){var ps=from.__kyoProps;if(!ps)return;
          |  for(var n in ps){if(!to.hasAttribute("data-kyo-prop-"+n)){from[n]=ps[n].base;delete ps[n];}}
          |}
          |function __kyoMorphAttrs(from,to){
          |  var tag=from.tagName;
          |  var activeInput=(from===document.activeElement)&&(tag==="INPUT"||tag==="TEXTAREA");
          |  // An attribute the imperative id-addressed channel owns is never reconciled: rendered HTML never carries
          |  // the client-set value, so reconciling would clobber it.
          |  var own=from.__kyoOwn||{};
          |  // Read before the walk moves the attribute: whether the previous render had the toggle on.
          |  var tg=tag==="INPUT"?"checked":(tag==="OPTION"?"selected":null);
          |  if(tg&&own[tg])tg=null;
          |  var wasOn=!!tg&&from.hasAttribute(tg);
          |  // A field renders its value PROPERTY, and the property stops tracking the attribute the first time the
          |  // user types, so writing the attribute alone is invisible on any field that has been typed into. The
          |  // attribute may even be unchanged (both empty) when a ref write clears it, so the property is written on
          |  // every pass. The focused field is the exception below: its own echo must not move the caret.
          |  for(var i=0;i<to.attributes.length;i++){var a=to.attributes[i];
          |    if(!own[a.name]){
          |      if(from.getAttribute(a.name)!==a.value)from.setAttribute(a.name,a.value);
          |      if(!activeInput)h.syncField(from,a.name,a.value);}}
          |  // A textarea carries its value as content, never as a value attribute, so the walk above cannot reach its
          |  // property: mirror the content instead, under the same focus rule. Only a content that moved is a value
          |  // write; an unbound textarea renders the same content on every pass and keeps what the user typed. The
          |  // children are not morphed yet, so `from` still holds the previous content.
          |  if(!activeInput&&tag==="TEXTAREA"&&from.textContent!==to.textContent)h.syncField(from,"value",to.textContent);
          |  for(var j=from.attributes.length-1;j>=0;j--){var n=from.attributes[j].name;
          |    if(!own[n]&&!to.hasAttribute(n))from.removeAttribute(n);}
          |  // A checkbox, a radio and an option show their PROPERTY once toggled by the reader, so a render that
          |  // turns the control on or off writes it. A box the render binds (data-kyo-bound) shows the render's state
          |  // on every pass, so a repaint takes back a tick the app did not accept. A free one is written only on a
          |  // change: it renders no attribute when off, and a repaint must keep its tick.
          |  if(tg){var on=to.hasAttribute(tg);if(to.hasAttribute("data-kyo-bound")?from[tg]!==on:on!==wasOn)from[tg]=on;}
          |  __kyoRestoreProps(from,to);
          |  // Active-input preservation: two-way binding echoes each keystroke back as a re-render. Never overwrite
          |  // the focused field's live value (its caret) with its own echo; assign only a genuine external change.
          |  // A bound field says so as the unfocused path reads it (an input's value attribute, a textarea's moved
          |  // content); an unbound field carries neither and keeps what the reader typed.
          |  if(activeInput){var v=tag==="TEXTAREA"?(from.textContent!==to.textContent?to.textContent:null):to.getAttribute("value");
          |    if(v!==null&&from.value!==v)from.value=v;}
          |}
          |// Reconcile the live sibling run of `parent` from `fromStart` up to (not including) `fromEnd`, null meaning
          |// the end of the parent, toward the run starting at `toStart` inside a detached fragment. Positional, pair
          |// by pair: what did not move keeps its identity; reordering a keyed list is the two-ended pass's business.
          |// A portal element does not live in the range: the first sweep moved it under <body> and left an inert slot
          |// behind. The payload still carries it inline, so reconcile the twin where it actually lives and step over
          |// the live slot. Re-inserting it here would hand __kyoAdoptPortals a second copy, which retires the twin and
          |// takes everything hanging off it along.
          |function __kyoMorphPortalPair(from,to){
          |  if(from.nodeType!==1||to.nodeType!==1)return false;
          |  var p=to.getAttribute("data-kyo-path");
          |  if(p===null||!to.hasAttribute("data-kyo-portal")||from.getAttribute("data-kyo-portal-slot")!==p)return false;
          |  var twin=h.portalTwin(p);if(!twin)return false;
          |  // The twin sits outside the range being patched, so the registry follows its reconciliation as a rewrite.
          |  h.rewrite(from,twin,function(){__kyoMorphNode(twin,to);return twin;});return true;
          |}
          |// ---- logical children ----
          |// A nested range is opened and closed by comments that are siblings of its content, so to a naive sibling
          |// walk it looks like several unrelated children. It is one: the markers carry the identity the registry is
          |// keyed by, and pairing them positionally would rewrite marker text and hand the registry a range that is
          |// no longer there. The morph therefore walks LOGICAL children, an opening marker standing for its span.
          |function __kyoSpanId(node){
          |  if(node.nodeType!==8)return null;
          |  var d=node.data;if(d.indexOf("kyo-rs:")!==0)return null;
          |  var rest=d.slice(7),sp=rest.indexOf(" ");return sp<0?rest:rest.slice(0,sp);
          |}
          |// The flag section of an opening marker (m, s, k=), "" when it carries none.
          |function __kyoSpanFlags(node){
          |  if(node.nodeType!==8)return "";
          |  var sp=node.data.indexOf(" ");return sp<0?"":node.data.slice(sp+1);
          |}
          |function __kyoHasFlag(flags,flag){return flags.length>0&&flags.split(" ").indexOf(flag)>=0;}
          |function __kyoFlagKey(flags){
          |  if(!flags.length)return null;
          |  var parts=flags.split(" ");
          |  for(var i=0;i<parts.length;i++)if(parts[i].indexOf("k=")===0)return parts[i].slice(2);
          |  return null;
          |}
          |function __kyoSpanClose(open,id){
          |  var n=open.nextSibling;
          |  while(n){if(n.nodeType===8&&n.data==="kyo-re:"+id)return n;n=n.nextSibling;}
          |  return null;
          |}
          |function __kyoLogicalNext(node){
          |  var id=__kyoSpanId(node);if(id===null)return node.nextSibling;
          |  var close=__kyoSpanClose(node,id);return close?close.nextSibling:node.nextSibling;
          |}
          |function __kyoEachSpanNode(first,f){
          |  var id=__kyoSpanId(first),last=first;
          |  if(id!==null){var close=__kyoSpanClose(first,id);if(close)last=close;}
          |  var n=first,stop=false;
          |  while(!stop&&n){var next=n.nextSibling;stop=n===last;f(n);n=next;}
          |}
          |function __kyoRemoveLogical(parent,node){__kyoEachSpanNode(node,function(n){parent.removeChild(n);});}
          |// Returns the first node it inserted, the live counterpart of toNode.
          |function __kyoInsertLogicalClone(parent,toNode,ref){
          |  // SMIL animations only start on a node this pass actually inserted; a reused one is already running.
          |  var first=null;
          |  __kyoEachSpanNode(toNode,function(n){var fresh=document.importNode(n,true);parent.insertBefore(fresh,ref);h.beginAnimations(fresh);if(first===null)first=fresh;});
          |  return first;
          |}
          |// Two spans of the same id are the same region still sitting here, so the pass recurses into their contents
          |// and never touches the live markers. Any other mismatch is replaced whole, markers and all. Returns the
          |// node that starts the slot afterwards: fromNode, or the first node of its replacement.
          |function __kyoPatchLogical(parent,fromNode,toNode){
          |  var fid=__kyoSpanId(fromNode),tid=__kyoSpanId(toNode);
          |  var fclose=fid!==null?__kyoSpanClose(fromNode,fid):null,tclose=tid!==null?__kyoSpanClose(toNode,tid):null;
          |  if(fclose&&tclose&&fid===tid){
          |    // A mount that already owns this slot repaints its own content, so the span is opaque and its live
          |    // marker is left alone: reconciling it against the placeholder the parent rendered would morph the
          |    // instance's subtree away, and with it focus, caret and every DOM-local thing hanging off it. The key
          |    // is what makes that safe; a differing key falls through to the morph and resets the slot. The live
          |    // marker adopts the incoming key on the way out.
          |    //
          |    // A NAMED slot is opaque from the first pass, not from the second. `m` says the client adopted the
          |    // span; `s` with the same key says the same thing one beat earlier, because the render that emitted
          |    // the slot named the instance that owns it. Waiting for `m` would cost one destructive morph per slot,
          |    // and the regions inside the discarded subtree leave the registry with it while the instance
          |    // republishes asynchronously: a subscription emitting in between dies on an unknown range for good.
          |    // An UNNAMED slot waits for `m`: without a key nothing tells one instance from the next.
          |    var lf=__kyoSpanFlags(fromNode),tf=__kyoSpanFlags(toNode),slot=__kyoHasFlag(tf,"s");
          |    var sameKey=__kyoFlagKey(lf)===__kyoFlagKey(tf),named=__kyoFlagKey(tf)!==null&&sameKey;
          |    if((__kyoHasFlag(lf,"m")||(named&&__kyoHasFlag(lf,"s")))&&slot&&sameKey)return fromNode;
          |    __kyoMorphRun(parent,fromNode.nextSibling,fclose,toNode.nextSibling,tclose);
          |    if(slot){var k=__kyoFlagKey(tf);fromNode.data="kyo-rs:"+fid+(k===null?" m":" m k="+k);}
          |    return fromNode;}
          |  if(!fclose&&!tclose&&__kyoMorphPortalPair(fromNode,toNode))return fromNode;
          |  if(!fclose&&!tclose&&__kyoMorphCompatible(fromNode,toNode)){__kyoMorphNode(fromNode,toNode);return fromNode;}
          |  var first=__kyoInsertLogicalClone(parent,toNode,fromNode);
          |  __kyoRemoveLogical(parent,fromNode);
          |  return first;
          |}
          |// A range whose content equals what it already shows is left as it is, which keeps a pressed click on its node. What a
          |// morph of it would still have done is adopt the mount slots inside it: the pass over a span pair stamps `m` on a
          |// slot marker (see __kyoPatchLogical), and only an adopted slot is left to its instance by the next repaint of the
          |// region around it. Content equal to the payload holds no `m` (the server writes only `s`), so every slot in it is one
          |// the morph would have stamped.
          |function __kyoAdoptSlot(node){
          |  var id=__kyoSpanId(node);if(id===null)return;
          |  var flags=__kyoSpanFlags(node);if(!__kyoHasFlag(flags,"s"))return;
          |  var k=__kyoFlagKey(flags);node.data="kyo-rs:"+id+(k===null?" m":" m k="+k);
          |}
          |function __kyoAdoptSlots(start,end){
          |  for(var n=start.nextSibling;n&&n!==end;n=n.nextSibling){
          |    if(n.nodeType===8)__kyoAdoptSlot(n);
          |    else if(n.nodeType===1){var w=document.createTreeWalker(n,128);while(w.nextNode())__kyoAdoptSlot(w.currentNode);}
          |  }
          |}
          |// Reconcile the live logical run [fromStart,fromEnd) of parent toward [toStart,toEnd).
          |// A two-ended keyed pass runs first, then a single cursor over whatever it could not settle. The cursor
          |// alone can only insert IN FRONT of itself, so a key it finds behind itself has to be dragged forward past
          |// every sibling in between: swapping two rows of a thousand costs 997 moves and a full relayout. Matching
          |// both ends first relocates only the children that actually changed place.
          |function __kyoCollectLogical(start,end,nodes,keys){
          |  var scan=start;
          |  while(scan&&scan!==end){nodes.push(scan);keys.push(__kyoLogicalKey(scan));scan=__kyoLogicalNext(scan);}
          |}
          |// A portal slot carries no path of its own and is keyed by the path of the element it stands in for.
          |function __kyoLogicalKey(node){
          |  if(node.nodeType===1){var p=node.getAttribute("data-kyo-path");return p!==null?p:node.getAttribute("data-kyo-portal-slot");}
          |  var id=__kyoSpanId(node);
          |  return (id!==null&&__kyoSpanClose(node,id))?id:null;
          |}
          |function __kyoMoveLogicalBefore(parent,node,ref){__kyoEachSpanNode(node,function(n){parent.insertBefore(n,ref);});}
          |// A mount paints its placeholder bare into its slot, and its content arrives inside a range of its own. Taken
          |// as an element against a span, that pair is replaced whole, and with it every node of a placeholder built
          |// like its content and everything hanging off them. A bare live run facing exactly one incoming range is
          |// wrapped in that range instead and reconciled against what it holds: the same markup, the nodes kept.
          |// Answers [live close, incoming close], or null when the runs are not that pair.
          |function __kyoWrapInIncomingRange(parent,fromStart,fromEnd,toStart,toEnd){
          |  if(!toStart||!fromStart||fromStart===fromEnd)return null;
          |  var tid=__kyoSpanId(toStart);if(tid===null)return null;
          |  var tclose=__kyoSpanClose(toStart,tid);if(!tclose||tclose.nextSibling!==toEnd)return null;
          |  for(var n=fromStart;n&&n!==fromEnd;n=n.nextSibling)if(__kyoSpanId(n)!==null)return null;
          |  parent.insertBefore(document.importNode(toStart,false),fromStart);
          |  return [parent.insertBefore(document.importNode(tclose,false),fromEnd),tclose];
          |}
          |function __kyoMorphRun(parent,fromStart,fromEnd,toStart,toEnd){
          |  var wrapped=__kyoWrapInIncomingRange(parent,fromStart,fromEnd,toStart,toEnd);
          |  if(wrapped){__kyoMorphRun(parent,fromStart,wrapped[0],toStart.nextSibling,wrapped[1]);return;}
          |  var fromNodes=[],fromKeys=[],toNodes=[],toKeys=[];
          |  __kyoCollectLogical(fromStart,fromEnd,fromNodes,fromKeys);
          |  __kyoCollectLogical(toStart,toEnd,toNodes,toKeys);
          |  var fromKeyed=null,toKeyed=null,i;
          |  for(i=0;i<fromKeys.length;i++)if(fromKeys[i]!==null){if(!fromKeyed)fromKeyed={};fromKeyed[fromKeys[i]]=fromNodes[i];}
          |  for(i=0;i<toKeys.length;i++)if(toKeys[i]!==null){if(!toKeyed)toKeyed={};toKeyed[toKeys[i]]=true;}
          |  // Invariant: the children still to place are exactly fromNodes[head..tail], a contiguous DOM run ending
          |  // immediately before tailBoundary. Only run boundaries are ever moved, and only out to a boundary.
          |  var head=0,tail=fromNodes.length-1,toHead=0,toTail=toNodes.length-1,tailBoundary=fromEnd,scanning=true;
          |  while(scanning&&head<=tail&&toHead<=toTail){
          |    if(fromKeys[head]===null||fromKeys[tail]===null||toKeys[toHead]===null||toKeys[toTail]===null)scanning=false;
          |    else if(fromKeys[head]===toKeys[toHead]){__kyoPatchLogical(parent,fromNodes[head],toNodes[toHead]);head++;toHead++;}
          |    else if(fromKeys[tail]===toKeys[toTail]){tailBoundary=__kyoPatchLogical(parent,fromNodes[tail],toNodes[toTail]);tail--;toTail--;}
          |    else if(fromKeys[head]===toKeys[toTail]){
          |      if(head!==tail)__kyoMoveLogicalBefore(parent,fromNodes[head],tailBoundary);
          |      tailBoundary=__kyoPatchLogical(parent,fromNodes[head],toNodes[toTail]);head++;toTail--;}
          |    else if(fromKeys[tail]===toKeys[toHead]){
          |      if(tail!==head)__kyoMoveLogicalBefore(parent,fromNodes[tail],fromNodes[head]);
          |      __kyoPatchLogical(parent,fromNodes[tail],toNodes[toHead]);tail--;toHead++;}
          |    else scanning=false;
          |  }
          |  // Hand the unresolved middle to the cursor. The dictionaries stay whole: keys are unique among siblings.
          |  var cursorFrom=head<=tail?fromNodes[head]:tailBoundary;
          |  var cursorToEnd=(toTail+1<toNodes.length)?toNodes[toTail+1]:toEnd;
          |  var cursorTo=toHead<=toTail?toNodes[toHead]:cursorToEnd;
          |  __kyoMorphCursor(parent,cursorFrom,tailBoundary,cursorTo,cursorToEnd,fromKeyed,toKeyed);
          |}
          |// Single-cursor reconciliation of whatever the two-ended pass left over: a keyed child is pulled to the
          |// cursor by key, an unkeyed one morphs positionally against the first compatible live child.
          |function __kyoMorphCursor(parent,fromStart,fromEnd,toStart,toEnd,fromKeyed,toKeyed){
          |  var curFrom=fromStart,curTo=toStart;
          |  while(curTo&&curTo!==toEnd){
          |    var toNext=__kyoLogicalNext(curTo),toKey=__kyoLogicalKey(curTo);
          |    if(toKey!==null){
          |      var m=fromKeyed?fromKeyed[toKey]:undefined;
          |      if(m){if(m!==curFrom)__kyoMoveLogicalBefore(parent,m,curFrom);else curFrom=__kyoLogicalNext(curFrom);
          |        __kyoPatchLogical(parent,m,curTo);}
          |      else __kyoInsertLogicalClone(parent,curTo,curFrom);
          |    }else{
          |      var handled=false,loop=true;
          |      while(loop&&curFrom&&curFrom!==fromEnd){
          |        var fromNext=__kyoLogicalNext(curFrom),fromKey=__kyoLogicalKey(curFrom);
          |        if(fromKey!==null){
          |          // A keyed live child at an unkeyed slot: keep it if the payload reuses it elsewhere (its own
          |          // slot moves it into place), else it is stale and goes.
          |          if(!toKeyed||!Object.prototype.hasOwnProperty.call(toKeyed,fromKey))__kyoRemoveLogical(parent,curFrom);
          |          curFrom=fromNext;
          |        }else if(__kyoMorphCompatible(curFrom,curTo)){__kyoMorphNode(curFrom,curTo);curFrom=fromNext;handled=true;loop=false;}
          |        else{__kyoRemoveLogical(parent,curFrom);curFrom=fromNext;}
          |      }
          |      if(!handled)__kyoInsertLogicalClone(parent,curTo,curFrom);
          |    }
          |    curTo=toNext;
          |  }
          |  while(curFrom&&curFrom!==fromEnd){var fn=__kyoLogicalNext(curFrom);__kyoRemoveLogical(parent,curFrom);curFrom=fn;}
          |}
          |// The elements of a set of logical children, each span contributing the elements it brackets.
          |function __kyoLogicalElements(nodes){
          |  var out=[];
          |  for(var i=0;i<nodes.length;i++)__kyoEachSpanNode(nodes[i],function(n){if(n.nodeType===1)out.push(n);});
          |  return out;
          |}
          |// Reconcile a list region against a row order rather than a rendered document: the payload holds only the rows that changed, every other
          |// row is named by key and stays where it is. The walk is the same two-ended keyed pass __kyoMorphRun uses, with the order standing in for
          |// the to side. target holds parent, start, end and the parsed fragment; scope retires ranges and reads the ones placed; keys are the rows'
          |// full keys in their new order; changed says which of them the fragment repaints, or is null where the fragment alone says so. Answers
          |// false, having changed nothing, when the rows are not individually addressable: a row painting as several roots or as none has no single
          |// key naming its DOM, and a live range that disagrees about which rows are on screen cannot be matched either.
          |function __kyoApplyListTo(target,scope,keys,changed){
          |  var parent=target.parent,fragment=target.fragment,i;
          |  var hasOwn=Object.prototype.hasOwnProperty;
          |  var parsedKeyed={},parsedCount=0,parsedSize=0,parsed=fragment?fragment.firstChild:null;
          |  while(parsed){var pk=__kyoLogicalKey(parsed);if(pk!==null){if(!hasOwn.call(parsedKeyed,pk))parsedSize++;parsedKeyed[pk]=parsed;}parsedCount++;parsed=__kyoLogicalNext(parsed);}
          |  var fromNodes=[],fromKeys=[];
          |  __kyoCollectLogical(target.start.nextSibling,target.end,fromNodes,fromKeys);
          |  var fromKeyed={};
          |  for(i=0;i<fromKeys.length;i++)if(fromKeys[i]!==null)fromKeyed[fromKeys[i]]=fromNodes[i];
          |  var toKeys=keys,toNodes=[],targetKeys={},addressable=true;
          |  if(changed){var changedCount=0;for(i=0;i<changed.length;i++)if(changed[i])changedCount++;addressable=parsedCount===changedCount&&parsedSize===changedCount;}
          |  for(i=0;addressable&&i<keys.length;i++){
          |    var key=keys[i],rendered=hasOwn.call(parsedKeyed,key)?parsedKeyed[key]:null,node=(!changed||changed[i])?rendered:null;
          |    addressable=changed?(changed[i]?node!==null:hasOwn.call(fromKeyed,key)):(node!==null||hasOwn.call(fromKeyed,key));
          |    targetKeys[key]=true;toNodes.push(node);
          |  }
          |  if(!addressable)return false;
          |  var disturbed=[],repainted=[];
          |  for(i=0;i<fromKeys.length;i++){
          |    var fk=fromKeys[i];
          |    if(fk===null||!hasOwn.call(targetKeys,fk))disturbed.push(fromNodes[i]);
          |  }
          |  for(i=0;i<toKeys.length;i++)if(toNodes[i]&&hasOwn.call(fromKeyed,toKeys[i])){var lv=fromKeyed[toKeys[i]];repainted.push(lv);disturbed.push(lv);}
          |  // Told before anything moves: the morph may drop markers, and a dropped marker's id is unreadable afterwards.
          |  for(i=0;i<disturbed.length;i++)scope.retire(disturbed[i]);
          |  var state=h.listBefore(target,__kyoLogicalElements(disturbed),__kyoLogicalElements(repainted));
          |  var touched=[];
          |  // Patches a row in place or replaces it, and answers the node that now starts it.
          |  function place(from,ti){var to=toNodes[ti];if(!to)return from;
          |    var before=from.previousSibling,until=__kyoLogicalNext(from),at=__kyoPatchLogical(parent,from,to);
          |    scope.placed(before?before.nextSibling:parent.firstChild,until);
          |    if(at!==from){var els=__kyoLogicalElements([at]);for(var q=0;q<els.length;q++)touched.push(els[q]);}
          |    else if(from.nodeType===1)touched.push(from);
          |    return at;}
          |  var head=0,tail=fromNodes.length-1,toHead=0,toTail=toKeys.length-1,tailBoundary=target.end,scanning=true;
          |  while(scanning&&head<=tail&&toHead<=toTail){
          |    if(fromKeys[head]===null||fromKeys[tail]===null)scanning=false;
          |    else if(fromKeys[head]===toKeys[toHead]){place(fromNodes[head],toHead);head++;toHead++;}
          |    else if(fromKeys[tail]===toKeys[toTail]){tailBoundary=place(fromNodes[tail],toTail);tail--;toTail--;}
          |    else if(fromKeys[head]===toKeys[toTail]){
          |      if(head!==tail)__kyoMoveLogicalBefore(parent,fromNodes[head],tailBoundary);
          |      tailBoundary=place(fromNodes[head],toTail);head++;toTail--;}
          |    else if(fromKeys[tail]===toKeys[toHead]){
          |      if(tail!==head)__kyoMoveLogicalBefore(parent,fromNodes[tail],fromNodes[head]);
          |      place(fromNodes[tail],toHead);tail--;toHead++;}
          |    else scanning=false;
          |  }
          |  var cursor=head<=tail?fromNodes[head]:tailBoundary;
          |  for(i=toHead;i<=toTail;i++){
          |    var rk=toKeys[i],live2=hasOwn.call(fromKeyed,rk)?fromKeyed[rk]:null;
          |    if(live2){if(live2!==cursor)__kyoMoveLogicalBefore(parent,live2,cursor);else cursor=__kyoLogicalNext(cursor);
          |      place(live2,i);}
          |    else if(toNodes[i]){
          |      var prior=cursor.previousSibling;
          |      var ins=__kyoLogicalElements([__kyoInsertLogicalClone(parent,toNodes[i],cursor)]);for(var q=0;q<ins.length;q++)touched.push(ins[q]);
          |      scope.placed(prior?prior.nextSibling:parent.firstChild,cursor);}
          |  }
          |  for(i=0;i<fromKeys.length;i++){
          |    var lk=fromKeys[i];
          |    if((lk===null||!hasOwn.call(targetKeys,lk))&&document.contains(fromNodes[i]))
          |      __kyoRemoveLogical(parent,fromNodes[i]);
          |  }
          |  if(scope.finish)scope.finish();
          |  h.listAfter(state,target,touched);
          |  return true;
          |}
          |return {run:__kyoMorphRun,adoptSlots:__kyoAdoptSlots,applyList:__kyoApplyListTo,spanId:__kyoSpanId,logicalNext:__kyoLogicalNext,eachSpanNode:__kyoEachSpanNode};
          |""".stripMargin

end MorphScript
