package kyo.uic.internal

import kyo.*
import kyo.uic.ReactiveScope
import scala.quoted.*

/** The expansion of `UI.reactive` (`Reactive.scala`), and what it expands to.
  *
  * A block reads props and signals with `.read`. The expansion is applicative: every read is taken out of the block and evaluated
  * once, before it, the reads are combined into one value (a chunk while every read is a constant, else one signal over the
  * distinct signals), and the block becomes a function of that value with each read replaced by its slot. A block that yields a UI
  * draws through one `render` unless its structure is only sites, and any other block maps through one `map`. Reads of one stable
  * path share a slot at compile time, and reads that reach one signal by different paths share one at run time, so the block never
  * sees two values of one source.
  *
  * A site is a UI-typed `if` nested in an element that reads: it draws its own region (`siteTerm`), so the element around it is built
  * once. A site whose condition is a key (`pct != 0`) is a keyed region over its own term, and its channels are held to the run's keys
  * (`Gate`, Option A), so a branch never shows a value its condition has left.
  *
  * Taking a read out of the block is right only where the block itself would have evaluated it, once, while it runs. A handler
  * runs later, at its event: a by-name argument or a lambda whose result is a computation that may suspend (`UI.Action`, which
  * `onClick` takes) reads what it reads when it runs, each prop's value at that moment (`Prop.current`), and a `val` of the
  * block that only reaches channels and handlers stays a channel. So the expansion refuses a read of something the block defines,
  * a read inside any other lambda, local `def` or by-name argument (each runs later, or again, or not at all, with no effect to
  * read in), and the block's scope handed on to code the expansion cannot see.
  */
// `@publicInBinary`: the inline reads and `UI.reactive` reach this object from code that expands elsewhere. Without it the compiler adds an
// accessor whose parameter is typed by the package `kyo.uic.internal`, a class no loader finds, and reflecting on the classes that hold
// those accessors fails (ReactiveReflectionTest).
@scala.annotation.publicInBinary
private[kyo] object ReactiveMacro:

    // ---- What the expansion calls at run time ----

    /** The reads as one value: a chunk of them while none is a signal, else one signal over each distinct signal, by reference. */
    def combine(reads: Seq[Any])(using Frame): UI.Prop[Chunk[Any]] =
        val signals = reads.foldLeft(Chunk.empty[Signal[Any]]) {
            case (acc, s: Signal[?]) if !acc.exists(_ eq s) => acc.append(s.asInstanceOf[Signal[Any]])
            case (acc, _)                                   => acc
        }
        if signals.isEmpty then Chunk.from(reads)
        else
            given CanEqual[Any, Any]               = CanEqual.derived
            given CanEqual[Chunk[Any], Chunk[Any]] = CanEqual.derived
            val combined                           = Signal.combineLatestAll(signals)
            // Every read a distinct signal: the combined values already are the reads, in order, so no per-emission copy.
            if signals.size == reads.size then combined
            else
                val slots = reads.map {
                    case s: Signal[?] => signals.indexWhere(_ eq s)
                    case _            => -1
                }.toArray
                val fixed = reads.toArray[Any]
                combined.map { values =>
                    val out = fixed.clone()
                    var i   = 0
                    while i < out.length do
                        val slot = slots(i)
                        if slot >= 0 then out(i) = values(slot)
                        i += 1
                    end while
                    // `out` is fresh for this emission and never touched again, so the chunk takes it without a second copy.
                    Chunk.fromNoCopy(out)
                }
            end if
        end if
    end combine

    /** A value block: a constant from constants, else the block mapped over the reads' signal. A block over one signal, or over two
      * distinct signals, maps that signal (or their `combineLatest`) once, as a hand-written block does: `combine` would combine them
      * into a chunk first, and map that again.
      */
    def value[T](reads: Seq[Any])(f: Chunk[Any] => T)(using CanEqual[T, T], Frame): UI.Prop[T] =
        given CanEqual[(Any, Any), (Any, Any)] = CanEqual.derived
        reads match
            case Seq(a: Signal[?]) =>
                UI.Prop.map[Any, T](a.asInstanceOf[Signal[Any]])(v => f(Chunk(v)))
            case Seq(a: Signal[?], b: Signal[?]) if !(a eq b) =>
                UI.Prop.map[(Any, Any), T](a.asInstanceOf[Signal[Any]].combineLatest(b.asInstanceOf[Signal[Any]]))((x, y) => f(Chunk(x, y)))
            case _ => UI.Prop.map[Chunk[Any], T](combine(reads))(f)
        end match
    end value

    /** A UI block: drawn once from constants, else one region over the reads' signal, compared by the values read. A block over one
      * signal, or over two distinct signals, draws its region over that signal (or their `combineLatest`) directly, as `value` does.
      */
    def draw(reads: Seq[Any])(f: Chunk[Any] => UI)(using Frame): UI =
        reads match
            case Seq(a: Signal[?]) =>
                UI.Prop.render[Any](a.asInstanceOf[Signal[Any]])(v => f(Chunk(v)))
            case Seq(a: Signal[?], b: Signal[?]) if !(a eq b) =>
                given CanEqual[(Any, Any), (Any, Any)] = CanEqual.derived
                UI.Prop.render[(Any, Any)](a.asInstanceOf[Signal[Any]].combineLatest(b.asInstanceOf[Signal[Any]]))(p =>
                    f(Chunk(p._1, p._2))
                )
            case _ => UI.Prop.render[Chunk[Any]](combine(reads))(f)
        end match
    end draw

    /** A UI block with keys: one region over the reads, compared by the first `compared` reads and the keys `key` gives, so a change
      * that leaves them as they were runs nothing. `f` gets the reads, then the keys, then the run's [[Gate]]. Over constants it
      * draws once, with no region.
      */
    def drawKeyed(reads: Seq[Any], compared: Int)(key: Chunk[Any] => Chunk[Any])(f: Chunk[Any] => UI)(using Frame): UI =
        def run(at: Keyed): UI = f(at.raw.concat(at.compared.drop(compared)).append(new Gate(reads, compared, key, at)))
        combine(reads) match
            case s: Signal[?] =>
                given CanEqual[Keyed, Keyed] = CanEqual.derived
                val keyed                    = s.asInstanceOf[Signal[Chunk[Any]]].map(raw => Keyed(raw, compared, key))
                UI.Prop.render[Keyed](keyed)(run)
            case values => run(Keyed(values.asInstanceOf[Chunk[Any]], compared, key))
        end match
    end drawKeyed

    /** One value of a keyed region's reads, equal to another where what the region compares is: the reads it compares as they are
      * and its keys. The region's signal skips a value equal to the one before, so a change that leaves both runs nothing.
      */
    final class Keyed private (val raw: Chunk[Any], val compared: Chunk[Any]):
        override def equals(other: Any): Boolean = other match
            case k: Keyed => compared.equals(k.compared)
            case _        => false
        override def hashCode: Int = compared.hashCode
    end Keyed

    object Keyed:
        def apply(raw: Chunk[Any], compared: Int, key: Chunk[Any] => Chunk[Any]): Keyed =
            new Keyed(raw, raw.take(compared).concat(key(raw)))

    /** What a run of a keyed region holds its channels to. A channel follows its read in place while the region's keys are the ones the
      * run was drawn for, and keeps the last value it showed while they are not: the region then draws again, and a channel never shows
      * a value of a moment whose structure the run does not show (a label reading "0%" in a run drawn for a value that is not 0).
      * A read the region does not read is not held, since no key depends on it.
      */
    final class Gate private[internal] (reads: Seq[Any], compared: Int, key: Chunk[Any] => Chunk[Any], drawn: Keyed)(using Frame):

        /** `read` held to the run's keys: a signal the region reads becomes one that moves only while the keys are the run's. */
        def hold[A](read: A): A = read match
            case s: Signal[?] =>
                val slot = reads.indexWhere(r => r.isInstanceOf[Signal[?]] && (r.asInstanceOf[AnyRef] eq s))
                if slot < 0 then read
                else
                    given CanEqual[Any, Any] = CanEqual.derived
                    // The last value the run showed. Shared by every observer of the held signal and written from the writer's
                    // stack, so an atomic cell; it starts at the run's own value of the read.
                    val last                    = new java.util.concurrent.atomic.AtomicReference[Any](drawn.raw(slot))
                    val signals                 = reads.indices.filter(i => reads(i).isInstanceOf[Signal[?]])
                    val one                     = signals.forall(i => reads(i).asInstanceOf[AnyRef] eq s)
                    val heldSignal: Signal[Any] =
                        if one then
                            // The region reads this signal alone: a map of it, so a write patches on the writer's stack.
                            val at = signals.toArray
                            s.asInstanceOf[Signal[Any]].map { v =>
                                val now = drawn.raw.toArray
                                at.foreach(i => now(i) = v)
                                if Keyed(Chunk.from(now), compared, key) == drawn then last.set(v)
                                last.get
                            }
                        else
                            combine(reads).asInstanceOf[Signal[Chunk[Any]]].map { now =>
                                if Keyed(now, compared, key) == drawn then last.set(now(slot))
                                last.get
                            }
                    heldSignal.asInstanceOf[A]
                end if
            case _ => read
    end Gate

    /** An optional prop as one prop of an option: `Absent` where it is unset, the value where it is set, a signal inside it followed
      * (as `UI.Live` reads an optional field). What a block's optional read takes out of it.
      */
    def optional[A](m: Maybe[UI.Prop[A]])(using CanEqual[A, A], Frame): UI.Prop[Maybe[A]] = m match
        case Present(a) => UI.Prop.map(a)(Present(_))
        case _          => Absent

    /** A handler's reads, taken when the handler runs: each prop's value at its event, then the handler over them. */
    def atEvent[A, S](reads: Seq[Any])(f: Chunk[Any] => A < S)(using Frame): A < (S & Sync) =
        if reads.isEmpty then f(Chunk.empty)
        else Kyo.foreach(reads)(r => UI.Prop.current(r.asInstanceOf[UI.Prop[Any]])).map(values => f(Chunk.from(values)))

    /** A branch form whose branches read, as a value: the branch its condition picks, as a prop. The branches are by-name, so only the one picked is built;
      * a signal condition switches to the branch it picks, and the
      * untaken branch has no observer. A constant condition picks at once.
      */
    def choose[A](cond: UI.Prop[Boolean])(yes: => UI.Prop[A], no: => UI.Prop[A])(using Frame): UI.Prop[A] =
        // The switch compares each picked value with the one before, as `combine` compares its reads.
        given CanEqual[A, A] = CanEqual.derived
        cond match
            case s: Signal[?] => s.asInstanceOf[Signal[Boolean]].switchMap(b => UI.Prop.signal(if b then yes else no))
            case b            => if b.asInstanceOf[Boolean] then yes else no
    end choose

    /** A branch form whose branches read, drawn: the branch its condition picks, drawn in one region over the condition. */
    def pick(cond: UI.Prop[Boolean])(yes: => UI, no: => UI)(using Frame): UI = UI.Prop.render(cond)(on => if on then yes else no)

    /** A match whose case bodies read, as a value: the case its selector picks, as a prop. `cases` builds the case the selector's value
      * takes, so only that case is built and subscribed; a signal selector switches to the case it picks.
      */
    def matched[S, A](sel: UI.Prop[S])(cases: S => UI.Prop[A])(using Frame): UI.Prop[A] =
        given CanEqual[A, A] = CanEqual.derived
        sel match
            case s: Signal[?] => s.asInstanceOf[Signal[S]].switchMap(v => UI.Prop.signal(cases(v)))
            case v            => cases(v.asInstanceOf[S])
    end matched

    /** A match whose case bodies draw: the case its selector picks, drawn in one region over the selector. */
    def pickOn[S](sel: UI.Prop[S])(cases: S => UI)(using Frame): UI = UI.Prop.render(sel)(cases)

    // ---- The expansion ----

    private val example =
        """|
           |  UI.reactive {
           |      val busy = props(_.loading).read      // read once, where the block starts
           |      button(if busy then "Saving" else "Save").disabled(busy || props(_.disabled).read)
           |  }""".stripMargin

    /** A `.read` the expansion did not take out: only reachable through the block's scope handed on to other code. */
    def strayImpl[A: Type](self: Expr[Any])(using Quotes): Expr[A] =
        import quotes.reflect.*
        report.errorAndAbort(
            s"""`.read` is taken out of a `UI.reactive` block only where the block itself is written.
               |
               |It reached code the block cannot see through the block's scope, here a helper that takes it as a
               |`using` parameter. Read the value in the block and hand the helper the value instead:
               |$example
               |""".stripMargin,
            self.asTerm.pos
        )
    end strayImpl

    def expand[T: Type](body: Expr[ReactiveScope ?=> T], frame: Expr[Frame])(using Quotes): Expr[Any] =
        import quotes.reflect.*
        given CanEqual[Symbol, Symbol] = CanEqual.derived

        def strip(t: Term): Term = t match
            case Inlined(_, Nil, inner) => strip(inner)
            case Typed(inner, _)        => strip(inner)
            case Block(Nil, inner)      => strip(inner)
            case _                      => t

        // The value a slot of a run's chunk holds, cast to the read's type.
        def slotTerm(values: Expr[Chunk[Any]], slot: Int, tpe: TypeRepr): Term = tpe.widen.asType match
            case '[t] => '{ ${ values }(${ Expr(slot) }).asInstanceOf[t] }.asTerm

        val (scopeParam, written, lambdaSym) = strip(body.asTerm) match
            case Block(List(dd @ DefDef(_, List(TermParamClause(List(param))), _, Some(rhs))), _: Closure) =>
                (param.symbol, rhs, dd.symbol)
            case other =>
                report.errorAndAbort("`UI.reactive` takes its block written in place, not a function value.", other.pos)

        val scopeClass = TypeRepr.of[ReactiveScope].typeSymbol

        // A read through the block's own scope: `scope.read[A](arg)`, as the compiler writes a given member's extension. An optional
        // read (over a `Maybe[Prop[A]]`, with its `CanEqual`) reads the prop of an option its receiver makes, `optional(arg)`.
        val optionalSym = TypeRepr.of[ReactiveMacro.type].termSymbol.methodMember("optional").headOption.getOrElse(
            report.errorAndAbort("kyo-ui has no `ReactiveMacro.optional`: the expansion of `UI.reactive` needs it.")
        )
        object Read:
            def unapply(t: Term): Option[Term] = t match
                case Apply(Apply(fn @ TypeApply(Select(q, "read"), List(a)), List(arg)), List(ce))
                    if fn.symbol.owner == scopeClass && strip(q).symbol == scopeParam =>
                    a.tpe.asType match
                        case '[a] =>
                            Some('{
                                ReactiveMacro.optional[a](${ arg.asExprOf[Maybe[UI.Prop[a]]] })(using
                                    ${ ce.asExprOf[CanEqual[a, a]] },
                                    $frame
                                )
                            }.asTerm)
                case Apply(fn, List(arg)) =>
                    def sel(f: Term): Option[Term] = f match
                        case TypeApply(inner, _)                               => sel(inner)
                        case Select(q, "read") if f.symbol.owner == scopeClass => Some(q)
                        case _                                                 => None
                    sel(fn).collect { case q if strip(q).symbol == scopeParam => arg }
                case _ => None
        end Read

        // The entry an `aria(...)` or `data(...)` pair converts to, and its parts (see the entry pass below). An Aria converts through
        // `keyed` and a Data through `dataKeyed`; `Aria.name` or `Data.name` makes the String `channel` takes, with the kind.
        val entryModule    = TypeRepr.of[UI.Ast.Entry.type].termSymbol
        val entryGiven     = List("keyed", "dataKeyed").flatMap(entryModule.methodMember(_).headOption)
        val dataKeyedGiven = entryModule.methodMember("dataKeyed").headOption
        val entryChannel   = entryModule.methodMember("channel").headOption.getOrElse(
            report.errorAndAbort("kyo-ui has no `Entry.channel`: the expansion of `UI.reactive` needs it.")
        )
        val ariaKeyModule = TypeRepr.of[kyo.Aria.type].termSymbol
        val ariaKeyName   = ariaKeyModule.methodMember("name").headOption.getOrElse(
            report.errorAndAbort("kyo-ui has no `Aria.name`: the expansion of `UI.reactive` needs it.")
        )
        val dataKeyModule = TypeRepr.of[kyo.Data.type].termSymbol
        val dataKeyName   = dataKeyModule.methodMember("name").headOption.getOrElse(
            report.errorAndAbort("kyo-ui has no `Data.name`: the expansion of `UI.reactive` needs it.")
        )
        val tuple2Module = TypeRepr.of[Tuple2.type].termSymbol
        // The name and the value of a pair written in place: `name -> value` or `(name, value)`.
        def pairOf(t: Term): Maybe[(Term, Term)] = strip(t) match
            case Apply(TypeApply(Select(Apply(_, List(name)), "->"), _), List(value))                   => Present((name, value))
            case Apply(TypeApply(Select(m, "apply"), _), List(name, value)) if m.symbol == tuple2Module => Present((name, value))
            case _                                                                                      => Absent

        // ---- `when(Boolean)` over toggle setters: the toggle form, so its condition is decided as a channel ----
        //
        // `el.when(cond)(f)` with a `Boolean` condition takes `f: Self => Self`, so a read there is the block's, and the block a
        // region over it. Where `f` only sets what a `UI.Ast.Toggle` has (classes, `aria` and `data` entries, `role`, `tabIndex`, `hidden`,
        // `silent`, `disabled`, `readOnly`, `required`, each with plain values, for an entry a value of a type with no signal
        // in it) and the condition reads (a read, or something the block defines), the call becomes `when(cond: Prop[Boolean])`
        // with `f` rebuilt on the toggle. That overload takes a `Prop`, so the data flow below decides the condition as it
        // decides any setter's: a read, or a `val` that only reaches
        // channels, is the toggle's channel, and a value the structure also needs stays the region's, where the toggle applies
        // `f` as the constant it is (the bytes of `when(Boolean)`). Any other `f` is left as written, its condition the block's.
        val elementClass = TypeRepr.of[UI.Ast.Element].typeSymbol
        val setterOwners = Set(
            elementClass,
            TypeRepr.of[UI.Ast.Interactive].typeSymbol,
            TypeRepr.of[UI.Ast.HasDisabled].typeSymbol,
            TypeRepr.of[UI.Ast.TextInput].typeSymbol
        )
        val cssClassModule = TypeRepr.of[UI.Ast.CssClass.type].termSymbol
        val stringType     = TypeRepr.of[String]
        val booleanType    = TypeRepr.of[Boolean]
        // What each toggle setter takes, by name; the element's setter of that name takes these values too. `aria` and `data`
        // take entries, each a pair converted where it is written (see `constantEntry`).
        val toggleSetters = Map(
            "role"     -> List(stringType),
            "tabIndex" -> List(TypeRepr.of[Int]),
            "hidden"   -> List(booleanType),
            "silent"   -> List(booleanType),
            "disabled" -> List(booleanType),
            "readOnly" -> List(booleanType),
            "required" -> List(booleanType)
        )
        val signalSymbol = TypeRepr.of[Signal[Any]].typeSymbol
        // Whether a value of type `t` may hold a signal: a signal, or a union or a type argument that may.
        def holdsSignal(t: TypeRepr): Boolean = t.widen.dealias match
            case OrType(a, b)         => holdsSignal(a) || holdsSignal(b)
            case AppliedType(c, args) => c.typeSymbol == signalSymbol || c.baseClasses.contains(signalSymbol) || args.exists(holdsSignal)
            case other                => other.baseClasses.contains(signalSymbol)
        // An entry argument a toggle takes as written: a pair converted in place whose value's type holds no signal. The value's
        // own type, as the conversion's is the instance's (`UI.Prop[Boolean]` for a `Boolean`).
        def constantEntry(e: Term): Boolean = strip(e) match
            case Apply(Select(Apply(TypeApply(conv, _), _), "apply"), List(pair)) if entryGiven.contains(conv.symbol) =>
                pairOf(pair).fold(!holdsSignal(pair.tpe))((_, value) => !holdsSignal(value.tpe))
            case _ => false
        def whenParams(m: Symbol): List[TypeRepr] = TypeRepr.of[UI.Ast.Element].memberType(m) match
            case MethodType(_, params, _) => params
            case _                        => Nil
        val toggleWhen = elementClass.methodMember("when").find(m =>
            whenParams(m) match
                case List(p) =>
                    p.dealias match
                        case OrType(_, _) => true
                        case _            => false
                case _ => false
        ).getOrElse(report.errorAndAbort("kyo-ui has no `when(cond: Prop[Boolean])`: the expansion of `UI.reactive` needs it."))
        def isBooleanWhen(sel: Select): Boolean =
            sel.name == "when" && sel.symbol.owner == elementClass &&
                (whenParams(sel.symbol) match
                    case List(p) => p =:= booleanType
                    case _       => false)

        // Whether `p` holds for a node of `t` or for one under it. A node that holds is not descended into.
        def anyIn(t: Tree)(p: Tree => Boolean): Boolean =
            var found = false
            new TreeTraverser:
                override def traverseTree(tree: Tree)(owner: Symbol): Unit =
                    if p(tree) then found = true else super.traverseTree(tree)(owner)
            .traverseTree(t)(lambdaSym)
            found
        end anyIn
        // Every symbol `t` defines, its definitions at any depth, traversed from `owner`.
        def definitionsIn(t: Tree, owner: Symbol): scala.collection.mutable.Set[Symbol] =
            val out = scala.collection.mutable.Set.empty[Symbol]
            new TreeTraverser:
                override def traverseTree(tree: Tree)(o: Symbol): Unit =
                    tree match
                        case d: Definition => out += d.symbol
                        case _             => ()
                    super.traverseTree(tree)(o)
                end traverseTree
            .traverseTree(t)(owner)
            out
        end definitionsIn
        // What the block defines, so a condition over it counts as one that reads.
        val defined                          = definitionsIn(written, lambdaSym)
        def readsOrDefined(t: Tree): Boolean = anyIn(t) {
            case Read(_)   => true
            case id: Ident => defined.contains(id.symbol)
            case _         => false
        }
        def refsTo(set: collection.Set[Symbol])(t: Tree): Boolean = anyIn(t) {
            case id: Ident => set.contains(id.symbol)
            case _         => false
        }

        // The arguments of a call, a varargs one spread out.
        def spread(args: List[Term]): List[Term] = args.flatMap(a =>
            strip(a) match
                case Repeated(elems, _) => elems
                case other              => List(other)
        )
        // The toggle's setter `name` on `recv` with `args`, its evidence (`S <:< HasDisabled`) found where it asks for one.
        def toggleCall(recv: Term, name: String, args: List[Term]): Maybe[Term] =
            val applied = Select.overloaded(recv, name, Nil, args)
            applied.tpe.widen match
                case mt: MethodType if mt.isImplicit =>
                    val evidence = mt.paramTypes.map(t =>
                        Implicits.search(t) match
                            case ok: ImplicitSearchSuccess => Present(ok.tree)
                            case _                         => Absent
                    )
                    if evidence.forall(_.isDefined) then Present(Apply(applied, evidence.map(_.get))) else Absent
                case _ => Present(applied)
            end match
        end toggleCall
        // `body` (a chain of setters on the lambda's parameter `p`) as the same chain on the toggle `recv`, or `Absent` where a
        // step is not a toggle setter with plain values.
        def onToggle(body: Term, p: Symbol, recv: Term): Maybe[Term] =
            def plain(args: List[Term], types: List[TypeRepr]): Boolean =
                args.size == types.size && args.zip(types).forall((a, t) => a.tpe.widen <:< t && !refsTo(Set(p))(a))
            strip(body) match
                case id: Ident if id.symbol == p => Present(recv)
                case Apply(Apply(TypeApply(Select(m, "apply"), _), List(sel @ Select(inner, "cssClass"))), args)
                    if m.symbol == cssClassModule && sel.symbol.owner == elementClass =>
                    val names = spread(args)
                    if plain(names, names.map(_ => stringType)) then onToggle(inner, p, recv).flatMap(toggleCall(_, "cssClass", names))
                    else Absent
                case Apply(sel @ Select(inner, name), args) if (name == "aria" || name == "data") && sel.symbol.owner == elementClass =>
                    val entries = spread(args)
                    if entries.forall(e => constantEntry(e) && !refsTo(Set(p))(e)) then
                        onToggle(inner, p, recv).flatMap(toggleCall(_, name, entries))
                    else Absent
                case Apply(sel @ Select(inner, name), args) if setterOwners.contains(sel.symbol.owner) =>
                    toggleSetters.get(name) match
                        case Some(types) if plain(args, types) => onToggle(inner, p, recv).flatMap(toggleCall(_, name, args))
                        case _                                 => Absent
                case _ => Absent
            end match
        end onToggle

        val toggled = new TreeMap:
            override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
                case Apply(Apply(sel @ Select(recv, "when"), List(cond)), List(f)) if isBooleanWhen(sel) && readsOrDefined(cond) =>
                    val recv1 = transformTerm(recv)(owner)
                    // The call as written, over the rewritten receiver and `f`, so a `when` inside `f` is rewritten too.
                    def asWritten: Term = Select.copy(sel)(recv1, "when").appliedTo(cond).appliedTo(transformTerm(f)(owner))
                    strip(f) match
                        case Block(List(dd @ DefDef(_, List(TermParamClause(List(p))), _, Some(body))), _: Closure) =>
                            val partial = Select(recv1, toggleWhen).appliedTo(cond)
                            partial.tpe.widen match
                                case MethodType(_, List(AppliedType(_, List(toggle, _))), _) =>
                                    // The lambda is the only place the toggle's parameter exists, so the chain is rebuilt in
                                    // it; a step the toggle has not leaves the call as written.
                                    var rebuilt = true
                                    val lambda  = Lambda(
                                        dd.symbol.owner,
                                        MethodType(List(p.name))(_ => List(toggle), _ => toggle),
                                        (o, params) =>
                                            onToggle(body, p.symbol, Ref(params.head.symbol)) match
                                                case Present(chain) => chain.changeOwner(o)
                                                case Absent         =>
                                                    rebuilt = false
                                                    Ref(params.head.symbol)
                                    )
                                    if rebuilt then partial.appliedTo(lambda) else asWritten
                                case _ => asWritten
                            end match
                        case _ => asWritten
                    end match
                case _ => super.transformTerm(tree)(owner)
        .transformTerm(written)(lambdaSym)

        // ---- `aria`/`data` entries: a value that reads is the entry's channel ----
        //
        // A pair `name -> value` reaches `aria(...)` or `data(...)` through `Entry.entry[V](using AttrValue[V]).apply(pair)`, whose
        // parameter is the pair, so a read in `value` would be the block's. Where `value` reads (a read, or something the block
        // defines) and `V` has a prop instance, the pair becomes `Entry.channel[V](name, value)`, whose `value` takes a `UI.Prop[V]`:
        // the data flow below then decides it as it decides any setter's prop argument, a channel where it only reaches channels,
        // the run's value where the structure also needs it.
        //
        // ---- A child text: a string that reads is the text's channel ----
        //
        // A `String` reaches an element's children through `HtmlChildVal.liftString`, whose parameter is the string, so a read in it
        // would be the block's. Where the string reads (a read, or something the block defines), the child becomes
        // `HtmlChildVal.liftPropString(s)`, whose parameter takes a `UI.Prop[String]`: the data flow below decides it as any setter's
        // prop argument, a text channel where it only reaches channels, the run's value where the structure also needs it. Over a
        // constant `liftPropString` is `liftString`, so the text drawn is the same.
        val childModule    = TypeRepr.of[UI.Ast.HtmlChildVal.type].termSymbol
        val liftStringSym  = childModule.methodMember("liftString").headOption
        val liftPropString = childModule.methodMember("liftPropString").headOption.getOrElse(
            report.errorAndAbort("kyo-ui has no `HtmlChildVal.liftPropString`: the expansion of `UI.reactive` needs it.")
        )
        val rhs = new TreeMap:
            override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
                case Apply(Apply(fn, List(text)), frameArgs)
                    if liftStringSym.contains(fn.symbol) && text.tpe.widen <:< stringType && readsOrDefined(text) =>
                    Ref(childModule).select(liftPropString).appliedTo(transformTerm(text)(owner)).appliedToArgs(frameArgs)
                case Apply(Select(Apply(TypeApply(conv, _), _), "apply"), List(pair)) if entryGiven.contains(conv.symbol) =>
                    pairOf(pair) match
                        case Present((name, value)) if readsOrDefined(value) =>
                            // The value's own type, not the conversion's: that one is the instance's (`UI.Prop[Boolean]` for a
                            // `Boolean`), and the prop of the value's type is what the channel takes.
                            val tpe = value.tpe.widen
                            tpe.asType match
                                case '[a] =>
                                    Implicits.search(TypeRepr.of[UI.Ast.AttrValue[UI.Prop[a]]]) match
                                        case ok: ImplicitSearchSuccess =>
                                            // A key is the String its conversion holds; `channel` takes that String and the entry's kind.
                                            val text   = transformTerm(name)(owner)
                                            val isData = dataKeyedGiven.contains(conv.symbol)
                                            val key    =
                                                if isData then Ref(dataKeyModule).select(dataKeyName).appliedTo(text)
                                                else Ref(ariaKeyModule).select(ariaKeyName).appliedTo(text)
                                            Ref(entryModule).select(entryChannel).appliedToType(tpe)
                                                .appliedTo(Literal(BooleanConstant(isData)), key, transformTerm(value)(owner))
                                                .appliedTo(ok.tree)
                                        case _ => super.transformTerm(tree)(owner)
                            end match
                        case _ => super.transformTerm(tree)(owner)
                case _ => super.transformTerm(tree)(owner)
        .transformTerm(toggled)(lambdaSym)

        // What the block defines, so no read refers to it.
        val local = definitionsIn(rhs, lambdaSym)

        // Whether `t` refers to something the block defines outside `t`: what `t` defines itself (a lambda's parameter, a val in
        // it) is taken out of the block with it.
        def refersToLocal(t: Term): Boolean =
            val own = definitionsIn(t, lambdaSym)
            anyIn(t) {
                case id: Ident => local.contains(id.symbol) && !own.contains(id.symbol)
                case _         => false
            }
        end refersToLocal

        def deferredError(at: Position, what: String): Nothing =
            report.errorAndAbort(
                s"""`.read` inside $what is not taken out of a `UI.reactive` block.
                   |
                   |The block reads every prop once, when it runs, and $what runs later, again or not at all, so a read
                   |there would see the value of the block's run instead of its own. Read into a `val` in the block and
                   |use the `val` there; a handler whose result is an effect (`onClick`'s action) reads when it runs:
                   |$example
                   |""".stripMargin,
                at
            )

        def containsRead(t: Tree): Boolean = anyIn(t) {
            case Read(_) => true
            case _       => false
        }

        // ---- Lazy branches: a branch form whose branch reads is subscribed only while it is taken ----
        //
        // An `if` (and `&&` or `||`, which the compiler writes as one) with a read in a branch is taken out of the block as a read of
        // its own: its condition and each branch are compiled as props of their own, and the branch the condition picks is the one
        // subscribed (`ReactiveMacro.choose`). A branch that uses a `val` of the block stays eager, since the block's `val` exists only
        // in the block's run. A value-typed form is read as one value; a UI-typed form at the top of the block is drawn by its
        // condition (`pick`). A UI-typed `if` or `match` nested in an element is subscribed only while its branch is taken; only a form that uses a block `val` or shares a read with the rest of the block stays eager.

        // A parameter that takes `T | Signal[T]` (a `Prop[T]`), as its `T`: an argument there may be a signal, so what it reads is
        // that argument's own channel and not a read of the block.
        def channelOf(param: TypeRepr): Maybe[TypeRepr] = param.dealias match
            case OrType(a, b) =>
                def signalOf(s: TypeRepr, other: TypeRepr): Maybe[TypeRepr] = s.dealias match
                    case AppliedType(c, List(x)) if c.typeSymbol == signalSymbol && x =:= other => Present(other)
                    case _                                                                      => Absent
                signalOf(a, b).orElse(signalOf(b, a))
            case _ => Absent

        def canEqualOf(t: TypeRepr): Maybe[Term] =
            Implicits.search(TypeRepr.of[CanEqual].appliedTo(List(t, t))) match
                case ok: ImplicitSearchSuccess => Present(ok.tree)
                case _                         => Absent

        def paramTypes(fn: Term): List[TypeRepr] = fn.tpe.widen match
            case MethodType(_, params, _) => params
            case _                        => Nil

        // Per argument of a call, the parameter's `T` where the argument may be a channel: the parameter takes a `Prop`, and the
        // argument's value can be compared (a channel compares each value with the one before).
        def propArgs(fn: Term, args: List[Term]): List[Maybe[TypeRepr]] =
            val params = paramTypes(fn)
            args.zipWithIndex.map { (a, i) =>
                params.lift(i) match
                    case Some(p) => channelOf(p).filter(x => a.tpe.widen <:< x && canEqualOf(x).isDefined)
                    case None    => Absent
            }
        end propArgs

        // ---- Handlers: deferred code that reads when it runs ----
        //
        // Code whose result is a computation that includes `Sync` (an event handler's `UI.Action`) can read a prop when it runs, so
        // its reads are taken at its start instead of the block's, and refused nowhere.
        val pendingClass = TypeRepr.of[Any < Any] match
            case AppliedType(tc, _) => tc.typeSymbol
            case other              => other.typeSymbol
        def handlerType(t: TypeRepr): Boolean = t.widen.dealias match
            case AppliedType(tc, List(_, eff)) if tc.typeSymbol == pendingClass => eff <:< TypeRepr.of[Sync]
            case _                                                              => false
        // Per argument of a call, the type a by-name parameter takes where it is a handler's: the argument is a handler.
        def handlerArgs(fn: Term, args: List[Term]): List[Maybe[TypeRepr]] =
            val params = paramTypes(fn)
            args.indices.toList.map(i =>
                params.lift(i) match
                    case Some(ByNameType(t)) if handlerType(t) => Present(t)
                    case _                                     => Absent
            )
        end handlerArgs
        def isHandlerLambda(dd: DefDef): Boolean =
            dd.name.startsWith("$anonfun") && dd.rhs.exists(r => handlerType(r.tpe))

        def isDeferred(t: Tree): Boolean = t match
            case _: DefDef                                    => true
            case vd: ValDef if vd.symbol.flags.is(Flags.Lazy) => true
            case _                                            => false

        def stablePath(t: Term): Maybe[List[Symbol]] = strip(t) match
            case id: Ident if isStable(id.symbol)   => Present(List(id.symbol))
            case Select(q, _) if isStable(t.symbol) => stablePath(q).map(_ :+ t.symbol)
            // An optional read of a stable path is one read, apart from a plain read of the same path, which yields the option itself.
            case Apply(Apply(TypeApply(fn, _), List(inner)), _) if fn.symbol == optionalSym => stablePath(inner).map(_ :+ optionalSym)
            case _                                                                          => Absent
        def isStable(s: Symbol): Boolean =
            s.flags.is(Flags.Module) || (s.isValDef && !s.flags.is(Flags.Mutable)) || s.isPackageDef
        // The stable paths of the reads in `t`, each read's path (a read without one has none to share).
        def readPaths(t: Tree): List[List[Symbol]] =
            val out = scala.collection.mutable.ListBuffer.empty[List[Symbol]]
            new TreeTraverser:
                override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                    case Read(arg) => stablePath(arg).foreach(p => out += p)
                    case _         => super.traverseTree(tree)(owner)
            .traverseTree(t)(lambdaSym)
            out.toList
        end readPaths
        // A branch reading what the condition reads takes that value from the block's own run (ISC-36), which a branch compiled apart
        // cannot do, so such a form stays the block's.
        def sharesRead(c: Term, y: Term, n: Term): Boolean =
            val branches = readPaths(y) ++ readPaths(n)
            readPaths(c).exists(branches.contains)
        // `a && b` and `a || b` on a `Boolean` are the forms `if a then b else false` and `if a then true else b`, as the compiler's
        // inlining of them would write (it does not inline them here, so they are written out).
        def lazyParts(t: Term): Maybe[(Term, Term, Term)] =
            val parts: Maybe[(Term, Term, Term)] = strip(t) match
                case If(c, y, n) => Present((c, y, n))
                case Apply(sel @ Select(a, op), List(b)) if (op == "&&" || op == "||") && sel.symbol.owner == booleanType.typeSymbol =>
                    if op == "&&" then Present((a, b, Literal(BooleanConstant(false))))
                    else Present((a, Literal(BooleanConstant(true)), b))
                case _ => Absent
            parts.filter { (c, y, n) =>
                (containsRead(y) || containsRead(n)) && !refersToLocal(c) && !refersToLocal(y) && !refersToLocal(n) && !sharesRead(c, y, n)
            }
        end lazyParts
        // A `match` whose case bodies read: the selector and each case body are props of their own, and only the case the selector
        // picks is subscribed (`ReactiveMacro.matched`, or `pickOn` where it draws). A case that uses a `val` of the block, a guard
        // that reads or uses one, and a selector that shares a read with a body stay eager, as a branch form does (ISC-29, ISC-36).
        def lazyCases(t: Term): Maybe[(Term, List[CaseDef])] = strip(t) match
            case Match(sel, cases) if cases.exists(cd => containsRead(cd.rhs)) =>
                def localOutside(x: Tree, own: collection.Set[Symbol]): Boolean = anyIn(x) {
                    case id: Ident => local.contains(id.symbol) && !own.contains(id.symbol)
                    case _         => false
                }
                val selPaths = readPaths(sel)
                val ok       = !refersToLocal(sel) && cases.forall { cd =>
                    val own = definitionsIn(cd, lambdaSym)
                    !cd.guard.exists(g => containsRead(g) || localOutside(g, own)) &&
                    !localOutside(cd.rhs, own) &&
                    readPaths(cd.rhs).forall(p => !selPaths.contains(p))
                }
                if ok then Present((sel, cases)) else Absent
            case _ => Absent
        end lazyCases
        // ---- Data flow: which of the block's `val`s only reach channels ----
        //
        // A `val` of the block that holds what a read gave is a read itself, to whoever uses it. Where every use is inside a
        // channel argument (directly, or through another such `val`), it is a prop of its own: the block computes it as a value
        // over its reads and each use reads it as a channel, so naming a read leaves the block's region as it was. One use the
        // structure needs (a condition, a value the block builds or draws, a lambda) makes it, and every `val` it is built from,
        // a read of the region, where each run has one value of it.

        // The candidates: `val`s outside channel arguments and deferred code, whose value can be compared and is not a UI.
        val candidates = scala.collection.mutable.LinkedHashMap.empty[Symbol, ValDef]
        new TreeTraverser:
            override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                case t if isDeferred(t)                                                   => ()
                case vd: ValDef if !vd.symbol.flags.is(Flags.Mutable) && vd.rhs.isDefined =>
                    val tpe = vd.tpt.tpe.widen
                    if !(tpe <:< TypeRepr.of[UI]) && canEqualOf(tpe).isDefined then candidates(vd.symbol) = vd
                    super.traverseTree(tree)(owner)
                case Apply(fn, args) =>
                    traverseTree(fn)(owner)
                    propArgs(fn, args).zip(args).foreach { (x, a) => if x.isEmpty then traverseTree(a)(owner) }
                case _ => super.traverseTree(tree)(owner)
        .traverseTree(rhs)(lambdaSym)

        // Only a `val` that holds what a read gave takes part, directly or through another such `val`.
        val reading = scala.collection.mutable.Set.empty[Symbol]
        var grew    = true
        while grew do
            grew = false
            candidates.foreach { (s, vd) =>
                if !reading.contains(s) && vd.rhs.exists(r => containsRead(r) || refsTo(reading)(r)) then
                    reading += s
                    grew = true
            }
        end while

        // A site: an `if` among the children of an element, typed UI or as the `HtmlChildVal` a child takes it in, that reads. It is a
        // region of its own over its condition (`lazyPick`), so the reads it takes are not the block's structure.
        // The argument a conversion takes: `lift[T](x)(using ...)` has it in the first list, after its given list.
        def conversionArg(t: Term): Option[Term] = strip(t) match
            case Apply(f: Apply, _) => conversionArg(f)
            case Apply(_, List(x))  => Some(x)
            case _                  => None
        // A branch as the UI it draws: a child's conversion of a UI, or a UI itself. A child that converts a String is no site.
        def drawsUI(b: Term): Boolean =
            if b.tpe.widen <:< TypeRepr.of[UI.Ast.HtmlChildVal] then conversionArg(b).exists(_.tpe.widen <:< TypeRepr.of[UI])
            else b.tpe.widen <:< TypeRepr.of[UI]
        def siteIf(t: Term): Option[Term] = strip(t) match
            case i @ If(_, y, n)
                if i.tpe.widen <:< TypeRepr.of[UI] || (i.tpe.widen <:< TypeRepr.of[UI.Ast.HtmlChildVal] && drawsUI(y) && drawsUI(n)) =>
                Some(i)
            case _ => None
        def isSite(t: Term): Boolean =
            siteIf(t).exists(i => containsRead(i) || refsTo(reading.toSet)(i))
        // The sites by identity: each `if` that is an argument of a call, or one of the elements of a varargs argument.
        val siteNodes = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Tree, java.lang.Boolean])
        new TreeTraverser:
            override def traverseTree(tree: Tree)(owner: Symbol): Unit =
                tree match
                    case Apply(_, args) =>
                        args.foreach { a =>
                            val elems = a match
                                case Typed(Repeated(es, _), _) => es
                                case other                     => List(other)
                            elems.foreach(e => if isSite(e) then discard(siteNodes.add(e)))
                        }
                    case _ => ()
                end match
                super.traverseTree(tree)(owner)
            end traverseTree
        .traverseTree(rhs)(lambdaSym)

        // ---- Keys: a condition that decides the structure through a value derived from its reads ----
        //
        // A condition of an `if`, a `match`'s selector or a guard in the structure of a UI block, whose value can be compared and
        // which reads (directly, or through `val`s it can be written out of), is a key of the region: the region compares the
        // key's value, not the reads it is computed from, so a change of the reads that leaves the key as it was runs nothing. Its
        // uses of a `val` need nothing of the region, so a `val` that otherwise only reaches channels stays a channel, and every
        // channel of the run is held to the run's keys (`ReactiveMacro.Gate`). A key that is the read itself, or that uses
        // something it cannot be written out of (a `val` defining something, a pattern's variable, a lazy branch), is not a key.
        val keyNodes = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Tree, java.lang.Boolean])
        // A reading `val` the key can be written out of: its definition defines nothing and uses only `val`s like it.
        def inlinable(s: Symbol, seen: Set[Symbol]): Boolean =
            reading.contains(s) && !seen.contains(s) &&
                candidates.get(s).flatMap(_.rhs).exists(r => definitionsIn(r, lambdaSym).isEmpty && writtenOut(r, seen + s))
        def writtenOut(t: Tree, seen: Set[Symbol]): Boolean = !anyIn(t) {
            case id: Ident if local.contains(id.symbol) => !inlinable(id.symbol, seen)
            case _                                      => false
        }
        // The read itself, through any `val`s that only name it: a key over it compares what the region compares already.
        def bareRead(t: Term): Boolean = strip(t) match
            case Read(_)                                  => true
            case id: Ident if reading.contains(id.symbol) => candidates.get(id.symbol).flatMap(_.rhs).exists(bareRead)
            case _                                        => false
        def keyEligible(t: Term): Boolean =
            TypeRepr.of[T] <:< TypeRepr.of[UI] && !(t.tpe.widen <:< TypeRepr.of[UI]) && canEqualOf(t.tpe.widen).isDefined &&
                (containsRead(t) || refsTo(reading)(t)) && definitionsIn(t, lambdaSym).isEmpty && writtenOut(t, Set.empty) &&
                !bareRead(t) && !anyIn(t) {
                    case x: Term => lazyParts(x).isDefined || lazyCases(x).isDefined
                    case _       => false
                }

        // Each use of a reading `val`: inside a channel argument or a key it needs nothing, inside another reading `val` it needs
        // what that one needs, anywhere else the region needs it.
        val structural = scala.collection.mutable.Set.empty[Symbol]
        val feeds      = scala.collection.mutable.ArrayBuffer.empty[(Symbol, Symbol)] // (used, user)
        final class Uses(inArg: Boolean, inVal: Maybe[Symbol], deferred: Boolean) extends TreeTraverser:
            override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                case t: Term if siteNodes.contains(t)         => () // a site takes its own reads
                case id: Ident if reading.contains(id.symbol) =>
                    if deferred then structural += id.symbol
                    else if inArg then ()
                    else
                        inVal match
                            case Present(user) => feeds += ((id.symbol, user))
                            case Absent        => structural += id.symbol
                case vd: ValDef if reading.contains(vd.symbol) =>
                    vd.rhs.foreach(Uses(inArg, Present(vd.symbol), deferred).traverseTree(_)(owner))
                // A handler reads what it uses when it runs: a use there needs nothing of the region.
                case dd: DefDef if isHandlerLambda(dd) => dd.rhs.foreach(Uses(inArg = true, inVal, deferred = false).traverseTree(_)(owner))
                case t if isDeferred(t)                => Uses(inArg, inVal, deferred = true).traverseTreeChildren(t)(owner)
                case Apply(fn, args) if deferred && handlerArgs(fn, args).exists(_.isDefined) =>
                    traverseTree(fn)(owner)
                    handlerArgs(fn, args).zip(args).foreach { (h, a) =>
                        if h.isDefined then Uses(inArg = true, inVal, deferred = false).traverseTree(a)(owner) else traverseTree(a)(owner)
                    }
                case Apply(fn, args) if !deferred =>
                    traverseTree(fn)(owner)
                    val handler = handlerArgs(fn, args)
                    propArgs(fn, args).zip(args).zipWithIndex.foreach { case ((x, a), i) =>
                        if x.isDefined || handler(i).isDefined then Uses(inArg = true, inVal, deferred = false).traverseTree(a)(owner)
                        else traverseTree(a)(owner)
                    }
                // A key of the structure: its uses need nothing of the region (see Keys above).
                case If(c, y, n) if !deferred && !inArg && inVal.isEmpty && keyEligible(c) =>
                    discard(keyNodes.add(c))
                    traverseTree(y)(owner)
                    traverseTree(n)(owner)
                case Match(sel, cases) if !deferred && !inArg && inVal.isEmpty =>
                    if keyEligible(sel) then discard(keyNodes.add(sel)) else traverseTree(sel)(owner)
                    cases.foreach { cd =>
                        traverseTree(cd.pattern)(owner)
                        cd.guard.foreach(g => if keyEligible(g) then discard(keyNodes.add(g)) else traverseTree(g)(owner))
                        traverseTree(cd.rhs)(owner)
                    }
                case _ => super.traverseTree(tree)(owner)
        end Uses
        Uses(inArg = false, Absent, deferred = false).traverseTree(rhs)(lambdaSym)
        grew = true
        while grew do
            grew = false
            feeds.foreach { (used, user) =>
                if structural.contains(user) && !structural.contains(used) then
                    structural += used
                    grew = true
            }
        end while
        val channelOnly: Set[Symbol] = reading.toSet -- structural

        // A channel argument: it may be a signal and it reads, directly or through a `val` that only reaches channels.
        def channels(fn: Term, args: List[Term]): List[Maybe[TypeRepr]] =
            propArgs(fn, args).zip(args).map { (x, a) => x.filter(_ => containsRead(a) || refsTo(channelOnly)(a)) }

        // The prop each channel-only `val` becomes, made where its definition is replaced (always before its uses).
        val propOf = scala.collection.mutable.Map.empty[Symbol, Symbol]

        // A read the region around already has: its value of the run, from that region's values.
        final case class Taken(values: Expr[Chunk[Any]], slot: Int)

        // Whether a handler reads: a read in it, or a channel-only `val` it uses.
        def readsInHandler(t: Tree): Boolean = containsRead(t) || refsTo(channelOnly)(t)

        // The reads of a term, hoisted to `to`: one slot per stable path (the first read of a path holds it) and one per other read.
        // Gives each read's stable path, the slot of each read, and the expression that evaluates the kept reads.
        // Inside the run of a keyed region, every read is held to the run's keys (`Gate.hold`), so a channel never shows a value of
        // a moment whose keys differ from the run's. Set while that run's term is rewritten, which is where its channels are compiled.
        var gateNow: Maybe[() => Expr[ReactiveMacro.Gate]] = Absent
        def held(read: Term): Term                         = gateNow match
            case Present(gate) =>
                read.tpe.widen.asType match
                    case '[a] => '{ ${ gate() }.hold[a](${ read.asExprOf[a] }) }.asTerm
            case Absent => read

        def slotsOf(reads: List[Term], to: Symbol, atEvent: Boolean = false): (List[Maybe[List[Symbol]]], List[Int], Expr[Seq[Any]]) =
            val keys   = reads.map(stablePath)
            val slots  = keys.zipWithIndex.map { (k, i) => k.fold(i)(path => keys.indexWhere(_.contains(path))) }
            val kept   = slots.distinct
            val slotOf = slots.map(kept.indexOf(_))
            // A handler reads each prop's value at its event, which no key holds.
            val hoisted = Expr.ofSeq(kept.map(i => (if atEvent then reads(i) else held(reads(i))).changeOwner(to).asExprOf[Any]))
            (keys, slotOf, hoisted)
        end slotsOf

        // A handler as code that reads when it runs: every read in it (and every channel-only `val` it uses, through its prop) taken
        // at its start with `Prop.current`, in tree order, one per stable path, and the handler as a function of those values. A
        // handler inside it takes its own; a read in other deferred code inside it is refused, as in the block.
        def compileHandler(term: Term, owner: Symbol, expected: TypeRepr): Term =
            val (a, eff) = expected.widen.dealias match
                case AppliedType(_, List(a, eff)) => (a, eff)
                case other                        => report.errorAndAbort(s"not a handler: ${other.show}", term.pos)
            val own   = definitionsIn(term, owner)
            val reads = scala.collection.mutable.ArrayBuffer.empty[Term]
            final class Collector(deferred: Maybe[String]) extends TreeTraverser:
                override def traverseTree(tree: Tree)(o: Symbol): Unit = tree match
                    case t @ Read(arg) =>
                        deferred.foreach(what => deferredError(t.pos, what))
                        if refsTo(own)(arg) then
                            report.errorAndAbort(
                                s"""`.read` of a value the handler defines.
                                   |
                                   |A handler reads its props when it starts, where nothing it defines exists yet. Read what the
                                   |value is built from instead.
                                   |""".stripMargin,
                                t.pos
                            )
                        end if
                        if containsRead(arg) then
                            report.errorAndAbort("`.read` inside what another `.read` reads.", t.pos)
                        reads += arg
                    case id: Ident if channelOnly.contains(id.symbol) => reads += Ref(propOf(id.symbol))
                    case Apply(fn, args)                              =>
                        traverseTree(fn)(o)
                        val byName  = paramTypes(fn).map { case ByNameType(_) => true; case _ => false }
                        val handler = handlerArgs(fn, args)
                        args.zipWithIndex.foreach { (x, i) =>
                            if handler(i).isDefined then ()
                            else if byName.lift(i).getOrElse(false) then
                                Collector(deferred.orElse(Present("a by-name argument"))).traverseTree(x)(o)
                            else traverseTree(x)(o)
                        }
                    case dd: DefDef if isHandlerLambda(dd) => ()
                    case dd: DefDef                        =>
                        val what = if dd.name.startsWith("$anonfun") then "a lambda" else s"the local `def ${dd.name}`"
                        dd.rhs.foreach(Collector(deferred.orElse(Present(what))).traverseTree(_)(dd.symbol))
                    case vd: ValDef if vd.symbol.flags.is(Flags.Lazy) =>
                        vd.rhs.foreach(Collector(deferred.orElse(Present(s"the lazy `val ${vd.name}`"))).traverseTree(_)(vd.symbol))
                    case _ => super.traverseTree(tree)(o)
            end Collector
            Collector(Absent).traverseTree(term)(owner)

            val (_, slotOf, hoisted) = slotsOf(reads.toList, owner, atEvent = true)

            (a.asType, eff.asType) match
                case ('[av], '[ev]) =>
                    val mt = MethodType(List("now"))(_ => List(TypeRepr.of[Chunk[Any]]), _ => TypeRepr.of[av < ev])
                    val f  = Lambda(
                        owner,
                        mt,
                        (lambdaOwner, params) =>
                            val values                         = Ref(params.head.symbol).asExprOf[Chunk[Any]]
                            var next                           = 0
                            def slotValue(tpe: TypeRepr): Term =
                                val slot = slotOf(next)
                                next += 1
                                slotTerm(values, slot, tpe)
                            end slotValue
                            new TreeMap:
                                override def transformStatement(tree: Statement)(o: Symbol): Statement = tree match
                                    case dd: DefDef if isHandlerLambda(dd) && dd.rhs.exists(readsInHandler) => compiledHandler(dd)
                                    case _ => super.transformStatement(tree)(o)
                                override def transformTerm(tree: Term)(o: Symbol): Term = tree match
                                    case Read(_)                                      => slotValue(tree.tpe)
                                    case id: Ident if channelOnly.contains(id.symbol) => slotValue(id.tpe)
                                    case Apply(fn, args)                              =>
                                        val handler = handlerArgs(fn, args)
                                        val args1   = args.zipWithIndex.map { (x, i) =>
                                            handler(i) match
                                                case Present(t) if readsInHandler(x) => compileHandler(x, o, t)
                                                case _                               => transformTerm(x)(o)
                                        }
                                        Apply.copy(tree)(transformTerm(fn)(o), args1)
                                    case _ => super.transformTerm(tree)(o)
                            .transformTerm(term)(owner).changeOwner(lambdaOwner)
                    ).asExprOf[Chunk[Any] => av < ev]
                    '{ ReactiveMacro.atEvent[av, ev]($hoisted)($f)(using $frame) }.asTerm.changeOwner(owner)
                case _ => report.errorAndAbort(s"not a handler: ${expected.show}", term.pos)
            end match
        end compileHandler

        // A handler lambda that reads, its body compiled as a handler (see `compileHandler`).
        def compiledHandler(dd: DefDef): DefDef = DefDef.copy(dd)(
            dd.name,
            dd.paramss,
            dd.returnTpt,
            dd.rhs.map(compileHandler(_, dd.symbol, dd.returnTpt.tpe))
        )

        // The block, a channel argument in it, or a channel-only `val`, as one value over its reads: every read in tree order (the
        // order the replacement visits them in) taken out, refusing the ones that cannot be, and the term as a function of the
        // values read. A read the region around already has is that region's value of the run. A channel argument or a
        // channel-only `val` inside is compiled the same way, on its own, and its reads are not the term's.
        // `from` owns what the term defines, `to` is where its value goes. The term is rewritten as it is and moved to `to` after, so
        // the symbols the data flow found are the ones it rewrites (moving a tree to a new owner gives its definitions new symbols).
        // `inBranch`: the term is a branch or a condition of a lazy form. Its value is switched on by `ReactiveMacro.choose`, which compares
        // with its own equality, so a type the block would not compare (no `CanEqual` given) is compared the same way there.
        def compile(
            term: Term,
            from: Symbol,
            to: Symbol,
            result: TypeRepr,
            taken: Map[List[Symbol], Taken],
            inBranch: Boolean = false
        ): Term =
            lazyParts(term) match
                case Present((c, y, n)) if result <:< TypeRepr.of[UI] => lazyPick(c, y, n, from, to, taken)
                case _                                                =>
                    lazyCases(term) match
                        case Present((sel, cases)) if result <:< TypeRepr.of[UI] => lazyPickOn(sel, cases, from, to, taken)
                        case _                                                   => compileReads(term, from, to, result, taken, inBranch)
        end compile

        def compileReads(term: Term, from: Symbol, to: Symbol, result: TypeRepr, taken: Map[List[Symbol], Taken], inBranch: Boolean): Term =
            def isTaken(arg: Term): Boolean = stablePath(arg).exists(taken.contains)
            // The lazy forms the collector took out as reads, by identity: the lambda transform replaces exactly these.
            val lazyNodes = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Tree, java.lang.Boolean])
            // A lazy form here, when none of the reads it holds is read anywhere else in this term or taken from the region around: a
            // read the rest of the term shares is one slot of the term (the block never sees two values of one source), so it stays eager.
            def lazyIn(t: Term): Maybe[(Term, Term, Term)] = lazyValue(t).filter { (c, y, n) =>
                val inside = readPaths(c) ++ readPaths(y) ++ readPaths(n)
                val whole  = readPaths(term)
                inside.forall(p => whole.count(_ == p) == inside.count(_ == p) && !taken.contains(p))
            }
            // A lazy `match` here: as `lazyIn`, its reads (selector and bodies) none of which the rest of the term or the region shares.
            def lazyMatchIn(t: Term): Maybe[(Term, List[CaseDef])] = lazyCases(t).filter { (sel, cases) =>
                val inside = readPaths(sel) ++ cases.flatMap(cd => readPaths(cd.rhs))
                val whole  = readPaths(term)
                inside.forall(p => whole.count(_ == p) == inside.count(_ == p) && !taken.contains(p))
            }
            // What this term reads: a read's argument, or the prop of a channel-only `val`.
            val reads = scala.collection.mutable.ArrayBuffer.empty[Term]
            // The keys of this region, in tree order (see Keys above): only a UI term is a region that compares them.
            val keysHere = scala.collection.mutable.ArrayBuffer.empty[Term]
            val keyed    = result <:< TypeRepr.of[UI]
            final class Collector(deferred: Maybe[String]) extends TreeTraverser:
                override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                    case t: Term if siteNodes.contains(t) && !(t eq term)             => () // a site takes its own reads
                    case t: Term if keyed && deferred.isEmpty && keyNodes.contains(t) => keysHere += t
                    case t @ Read(arg)                                                =>
                        deferred.foreach(what => deferredError(t.pos, what))
                        if refersToLocal(arg) then
                            report.errorAndAbort(
                                s"""`.read` of a value the `UI.reactive` block defines.
                                   |
                                   |Every read is taken out of the block and runs before it, where nothing the block defines exists
                                   |yet. Read what the value is built from instead:
                                   |$example
                                   |""".stripMargin,
                                t.pos
                            )
                        end if
                        if containsRead(arg) then
                            report.errorAndAbort(
                                s"""`.read` inside what another `.read` reads.
                                   |
                                   |What a read reads is taken out of the block, so a read inside it would leave the block with it.
                                   |Read the inner value into a `val` first, outside the expression the outer read reads.
                                   |""".stripMargin,
                                t.pos
                            )
                        end if
                        if !isTaken(arg) then reads += arg
                    case t: Term if deferred.isEmpty && lazyIn(t).isDefined =>
                        lazyIn(t).foreach { case (c, y, n) =>
                            discard(lazyNodes.add(t))
                            reads += lazyChoice(c, y, n, t.tpe.widen, from, to, taken)
                        }
                    case t: Term if deferred.isEmpty && !(t.tpe.widen <:< TypeRepr.of[UI]) && lazyMatchIn(t).isDefined =>
                        lazyMatchIn(t).foreach { case (sel, cases) =>
                            discard(lazyNodes.add(t))
                            reads += lazyMatchValue(sel, cases, t.tpe.widen, from, to, taken)
                        }
                    case id: Ident if channelOnly.contains(id.symbol) =>
                        reads += Ref(propOf(id.symbol))
                    case vd: ValDef if channelOnly.contains(vd.symbol) => () // its own value, compiled where it is replaced
                    case Apply(fn, args)                               =>
                        traverseTree(fn)(owner)
                        val byName  = paramTypes(fn).map { case ByNameType(_) => true; case _ => false }
                        val channel = if deferred.isEmpty then channels(fn, args) else args.map(_ => Absent)
                        val handler = handlerArgs(fn, args)
                        args.zipWithIndex.foreach { (a, i) =>
                            if channel(i).isDefined then ()      // its own value, compiled where it is replaced
                            else if handler(i).isDefined then () // a handler takes its own reads, when it runs
                            else if byName.lift(i).getOrElse(false) then
                                Collector(deferred.orElse(Present("a by-name argument"))).traverseTree(a)(owner)
                            else traverseTree(a)(owner)
                        }
                    case dd: DefDef if isHandlerLambda(dd) => () // a handler takes its own reads, when it runs
                    case dd: DefDef                        =>
                        val what = if dd.name.startsWith("$anonfun") then "a lambda" else s"the local `def ${dd.name}`"
                        dd.rhs.foreach(Collector(deferred.orElse(Present(what))).traverseTree(_)(dd.symbol))
                    case vd: ValDef if vd.symbol.flags.is(Flags.Lazy) =>
                        vd.rhs.foreach(Collector(deferred.orElse(Present(s"the lazy `val ${vd.name}`"))).traverseTree(_)(vd.symbol))
                    case _ => super.traverseTree(tree)(owner)
            end Collector
            Collector(Absent).traverseTree(term)(from)

            // Each key written out of the `val`s it uses, down to its reads, and those reads in the order the key function visits
            // them. A read the region around already has is its value of that run, and not a read of this region.
            val writtenKeys = keysHere.toList.map { k =>
                new TreeMap:
                    override def transformTerm(tree: Term)(o: Symbol): Term = tree match
                        case id: Ident if reading.contains(id.symbol) => transformTerm(candidates(id.symbol).rhs.get)(o)
                        case _                                        => super.transformTerm(tree)(o)
                .transformTerm(k)(from)
            }
            val keyReads = writtenKeys.flatMap { k =>
                val out = scala.collection.mutable.ListBuffer.empty[Term]
                new TreeTraverser:
                    override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                        case Read(arg) => if !isTaken(arg) then out += arg
                        case _         => super.traverseTree(tree)(owner)
                .traverseTree(k)(from)
                out.toList
            }

            // One slot per stable path, and one per other read: the region's own reads first, then what only its keys read.
            val (keys, slotOf, hoisted) = slotsOf(reads.toList ++ keyReads, to)
            // The slots the region compares as they are, and the number of slots it reads.
            val compared = if reads.isEmpty then 0 else slotOf.take(reads.size).max + 1
            val width    = if slotOf.isEmpty then 0 else slotOf.max + 1
            // A key node of the term, by identity, at its index among the keys.
            val keyIndex = new java.util.IdentityHashMap[Tree, Integer]
            keysHere.zipWithIndex.foreach((k, i) => discard(keyIndex.put(k, i)))

            // The keys as a function of the region's reads, in the slots the run takes them from.
            def keyFn: Expr[Chunk[Any] => Chunk[Any]] =
                val mt = MethodType(List("raw"))(_ => List(TypeRepr.of[Chunk[Any]]), _ => TypeRepr.of[Chunk[Any]])
                Lambda(
                    to,
                    mt,
                    (lambdaOwner, params) =>
                        val raw     = Ref(params.head.symbol).asExprOf[Chunk[Any]]
                        var next    = reads.size
                        val slotted = new TreeMap:
                            override def transformTerm(tree: Term)(o: Symbol): Term = tree match
                                case Read(arg) =>
                                    stablePath(arg).flatMap(p => Maybe.fromOption(taken.get(p))).fold {
                                        val slot = slotOf(next)
                                        next += 1
                                        slotTerm(raw, slot, tree.tpe)
                                    }(t => slotTerm(t.values, t.slot, tree.tpe))
                                case _ => super.transformTerm(tree)(o)
                        val values = writtenKeys.map(k => slotted.transformTerm(k)(from).asExprOf[Any])
                        '{ Chunk.from(${ Expr.ofSeq(values) }) }.asTerm.changeOwner(lambdaOwner)
                ).asExprOf[Chunk[Any] => Chunk[Any]]
            end keyFn

            // The term as a function of the read values, each channel argument and channel-only `val` in it replaced by its own
            // value, which takes what this term reads as this term's values of the run.
            def lambdaOf[R: Type]: Expr[Chunk[Any] => R] =
                val mt = MethodType(List("reads"))(_ => List(TypeRepr.of[Chunk[Any]]), _ => TypeRepr.of[R])
                Lambda(
                    to,
                    mt,
                    (lambdaOwner, params) =>
                        val values = Ref(params.head.symbol).asExprOf[Chunk[Any]]
                        // Only the reads the region compares are values of the run: what only its keys read stays a channel's.
                        val inner =
                            taken ++ keys.zip(slotOf).take(reads.size).collect { case (Present(path), slot) => path -> Taken(values, slot) }
                        var next                           = 0
                        def slotValue(tpe: TypeRepr): Term =
                            val slot = slotOf(next)
                            next += 1
                            slotTerm(values, slot, tpe)
                        end slotValue
                        // A keyed run gets its keys after its reads, and its gate after them (`ReactiveMacro.drawKeyed`).
                        val outerGate = gateNow
                        if !keysHere.isEmpty then
                            val at = width + keysHere.size
                            gateNow = Present(() => '{ ${ values }(${ Expr(at) }).asInstanceOf[ReactiveMacro.Gate] })
                        val body = new TreeMap:
                            override def transformStatement(tree: Statement)(o: Symbol): Statement = tree match
                                case dd: DefDef if isHandlerLambda(dd) && dd.rhs.exists(readsInHandler) => compiledHandler(dd)
                                case vd: ValDef if channelOnly.contains(vd.symbol)                      =>
                                    val tpe     = vd.tpt.tpe.widen
                                    val propTpe = tpe.asType match
                                        case '[t] => TypeRepr.of[UI.Prop[t]]
                                    val sym = Symbol.newVal(o, s"${vd.name}$$prop", propTpe, Flags.EmptyFlags, Symbol.noSymbol)
                                    propOf(vd.symbol) = sym
                                    ValDef(sym, Some(compile(vd.rhs.get, vd.symbol, sym, tpe, inner)))
                                case _ => super.transformStatement(tree)(o)
                            override def transformTerm(tree: Term)(o: Symbol): Term = tree match
                                // A key is the run's value of it, which the region compared.
                                // a site (also one inside a varargs argument) is its own region.
                                case _ if siteNodes.contains(tree) && !(tree eq term) => siteTerm(tree, o, o, inner)
                                case _ if keyIndex.containsKey(tree) => slotTerm(values, width + keyIndex.get(tree), tree.tpe)
                                // A lazy form is the prop the collector took out, a slot like a read.
                                case _ if lazyNodes.contains(tree) => slotValue(tree.tpe)
                                case Read(arg)                     =>
                                    val had = stablePath(arg).flatMap(p => Maybe.fromOption(taken.get(p)))
                                    had.fold(slotValue(tree.tpe))(t => slotTerm(t.values, t.slot, tree.tpe))
                                case id: Ident if channelOnly.contains(id.symbol) => slotValue(id.tpe)
                                case Apply(fn, args)                              =>
                                    val fn1     = transformTerm(fn)(o)
                                    val params  = paramTypes(fn)
                                    val channel = channels(fn, args)
                                    val handler = handlerArgs(fn, args)
                                    val args1   = args.zipWithIndex.map { (a, i) =>
                                        if isSite(a) then siteTerm(a, o, o, inner)
                                        else
                                            handler(i).filter(_ => channel(i).isEmpty && readsInHandler(a)) match
                                                case Present(t) => compileHandler(a, o, t)
                                                case _          =>
                                                    channel(i) match
                                                        case Present(x) =>
                                                            strip(a) match
                                                                // A bare read, or a bare channel-only `val`, hands on what it reads.
                                                                case Read(of)
                                                                    if !stablePath(of).exists(inner.contains) &&
                                                                        of.tpe.widen <:< params(i) =>
                                                                    held(of).changeOwner(o)
                                                                case id: Ident if channelOnly.contains(id.symbol) => Ref(propOf(id.symbol))
                                                                case _ => compile(a, o, o, x, inner)
                                                        case Absent => transformTerm(a)(o)
                                    }
                                    Apply.copy(tree)(fn1, args1)
                                case _ => super.transformTerm(tree)(o)
                        try body.transformTerm(term)(from).changeOwner(lambdaOwner)
                        finally gateNow = outerGate
                ).asExprOf[Chunk[Any] => R]
            end lambdaOf

            result.asType match
                // nothing here is structure (its reads are channels and sites), so the element is built once, over no values.
                case '[r] if result <:< TypeRepr.of[UI] && reads.isEmpty && keysHere.isEmpty =>
                    val f = lambdaOf[UI]
                    '{ ($f(Chunk.empty[Any]): UI) }.asTerm.changeOwner(to)
                case '[r] if result <:< TypeRepr.of[UI] && !keysHere.isEmpty =>
                    val f = lambdaOf[UI]
                    '{ ReactiveMacro.drawKeyed($hoisted, ${ Expr(compared) })($keyFn)($f)(using $frame) }.asTerm.changeOwner(to)
                case '[r] if result <:< TypeRepr.of[UI] =>
                    val f = lambdaOf[UI]
                    '{ ReactiveMacro.draw($hoisted)($f)(using $frame) }.asTerm.changeOwner(to)
                case '[r] =>
                    val equal: Expr[CanEqual[r, r]] =
                        if inBranch then '{ CanEqual.derived: CanEqual[r, r] }
                        else
                            canEqualOf(result).getOrElse(
                                report.errorAndAbort(
                                    s"""A `UI.reactive` block that yields a ${result.show} needs a `CanEqual[${result.show}, ${result.show}]`.
                                       |
                                       |Over a signal the block is mapped, and the mapped signal compares each value with the one before.
                                       |""".stripMargin,
                                    term.pos
                                )
                            ).asExprOf[CanEqual[r, r]]
                    val f = lambdaOf[r]
                    '{ ReactiveMacro.value[r]($hoisted)($f)(using $equal, $frame) }.asTerm.changeOwner(to)
            end match
        end compileReads

        // The lazy form as a value inside the block: a value-typed one (a UI-typed one is not read as a value: its condition draws it where it stands, `lazyPick`, and only the taken branch is subscribed).
        def lazyValue(t: Term): Maybe[(Term, Term, Term)] =
            if t.tpe.widen <:< TypeRepr.of[UI] then Absent else lazyParts(t)

        // The lazy form as a prop of its own: its condition and both branches compiled as props (building one subscribes to nothing),
        // and `ReactiveMacro.choose` observes only the one the condition picks. Its reads belong to the branches, so the block takes none.
        def lazyChoice(c: Term, y: Term, n: Term, result: TypeRepr, from: Symbol, to: Symbol, taken: Map[List[Symbol], Taken]): Term =
            val cond = compile(c, from, to, TypeRepr.of[Boolean], taken, inBranch = true)
            val yes  = compile(y, from, to, result, taken, inBranch = true)
            val no   = compile(n, from, to, result, taken, inBranch = true)
            result.asType match
                case '[r] =>
                    val cc = cond.asExprOf[UI.Prop[Boolean]]
                    val yy = yes.asExprOf[UI.Prop[r]]
                    val nn = no.asExprOf[UI.Prop[r]]
                    '{ ReactiveMacro.choose[r]($cc)($yy, $nn)(using $frame) }.asTerm.changeOwner(to)
            end match
        end lazyChoice

        // The lazy form drawn at the top of the block: the condition's prop picks the branch drawn. Both branches are built where the
        // block is (building a UI node subscribes to nothing), and the picked one is the one rendered.
        def lazyPick(c: Term, y: Term, n: Term, from: Symbol, to: Symbol, taken: Map[List[Symbol], Taken]): Term =
            val cond = compile(c, from, to, TypeRepr.of[Boolean], taken)
            val yes  = compile(y, from, to, TypeRepr.of[UI], taken)
            val no   = compile(n, from, to, TypeRepr.of[UI], taken)
            val cc   = cond.asExprOf[UI.Prop[Boolean]]
            val yy   = yes.asExprOf[UI]
            val nn   = no.asExprOf[UI]
            '{ ReactiveMacro.pick($cc)($yy, $nn)(using $frame) }.asTerm.changeOwner(to)
        end lazyPick

        // the site as its own region, drawn where it stands (`lazyPick`), its condition and branches compiled against `taken`.
        def siteTerm(t: Term, from: Symbol, to: Symbol, taken: Map[List[Symbol], Taken]): Term =
            siteRegion(t, from, to, taken).getOrElse(pickSite(t, from, to, taken))

        def pickSite(t: Term, from: Symbol, to: Symbol, taken: Map[List[Symbol], Taken]): Term =
            // A branch typed as a child is the conversion of the UI it draws: the UI is what `pick` draws.
            def ui(b: Term): Term =
                if b.tpe.widen <:< TypeRepr.of[UI.Ast.HtmlChildVal] then
                    conversionArg(b).getOrElse(report.errorAndAbort(s"not a child: ${b.show}", b.pos))
                else b
            val site  = siteIf(t).getOrElse(report.errorAndAbort(s"not a site: ${t.show}", t.pos))
            val drawn = site match
                case If(c, y, n) => lazyPick(c, ui(y), ui(n), from, to, taken)
                case other       => report.errorAndAbort(s"not an if: ${other.show}", other.pos)
            if site.tpe.widen <:< TypeRepr.of[UI] then drawn
            else '{ UI.Ast.HtmlChildVal.lift[UI](${ drawn.asExprOf[UI] }) }.asTerm.changeOwner(to)
        end pickSite

        // Option A:a UI site whose condition is a key (`pct != 0`) is a keyed region over its own term, like a block-level `if`:
        // its channels are held to the run's keys (Gate), so a branch never shows a value the condition has left.
        def siteRegion(t: Term, from: Symbol, to: Symbol, taken: Map[List[Symbol], Taken]): Maybe[Term] =
            siteIf(t) match
                case Some(i @ If(c, y, n)) if (i.tpe.widen <:< TypeRepr.of[UI] || (drawsUI(y) && drawsUI(n))) && keyEligible(c) =>
                    // The branches as the UI they draw: a child's conversion is taken off, so the region draws UI, not HtmlChildVal.
                    def drawn(b: Term): Term =
                        if b.tpe.widen <:< TypeRepr.of[UI.Ast.HtmlChildVal] then conversionArg(b).getOrElse(b) else b
                    // A block `val` the site reads is written out as its read, so the branch's channels read the region's own
                    // signal and the gate holds them (a `val`'s prop is a different signal, which the gate does not hold).
                    val writeOut = new TreeMap:
                        override def transformTerm(tree: Term)(o: Symbol): Term = tree match
                            case id: Ident if reading.contains(id.symbol) && inlinable(id.symbol, Set.empty) =>
                                transformTerm(candidates(id.symbol).rhs.get)(o)
                            case _ => super.transformTerm(tree)(o)
                    val inlined = writeOut.transformTerm(If(c, drawn(y), drawn(n)))(from) match
                        case w @ If(c2, _, _) =>
                            discard(keyNodes.add(c2))
                            w
                        case other => report.errorAndAbort(s"site not an if after inlining: ${other.show}", other.pos)
                    val region  = compileReads(inlined, from, to, TypeRepr.of[UI], taken, inBranch = false)
                    val asChild = strip(t) match
                        case If(_, _, _) if i.tpe.widen <:< TypeRepr.of[UI] => region
                        case _ => '{ UI.Ast.HtmlChildVal.lift[UI](${ region.asExprOf[UI] }) }.asTerm.changeOwner(to)
                    Present(asChild)
                case _ => Absent
            end match
        end siteRegion

        // The lazy `match` as a value inside the block: its selector as a prop, and the case its value picks as a prop built over that
        // case's pattern variables (`ReactiveMacro.matched`). A slot like a lazy `if`'s.
        def lazyMatchValue(
            sel: Term,
            cases: List[CaseDef],
            result: TypeRepr,
            from: Symbol,
            to: Symbol,
            taken: Map[List[Symbol], Taken]
        ): Term =
            val selType = sel.tpe.widen
            val selProp = compile(sel, from, to, selType, taken, inBranch = true)
            selType.asType match
                case '[s] =>
                    result.asType match
                        case '[r] =>
                            val f = Lambda(
                                to,
                                MethodType(List("selected"))(_ => List(selType), _ => TypeRepr.of[UI.Prop[r]]),
                                (lo, params) =>
                                    val arms = cases.map(cd =>
                                        CaseDef.copy(cd)(cd.pattern, cd.guard, compile(cd.rhs, from, lo, result, taken, inBranch = true))
                                    )
                                    Match(Ref(params.head.symbol), arms).changeOwner(lo)
                            ).asExprOf[s => UI.Prop[r]]
                            '{ ReactiveMacro.matched[s, r](${ selProp.asExprOf[UI.Prop[s]] })($f)(using $frame) }.asTerm.changeOwner(to)
                    end match
            end match
        end lazyMatchValue

        // The lazy `match` drawn at the top of the block: the selector's prop picks the case drawn (`ReactiveMacro.pickOn`).
        def lazyPickOn(sel: Term, cases: List[CaseDef], from: Symbol, to: Symbol, taken: Map[List[Symbol], Taken]): Term =
            val selType = sel.tpe.widen
            val selProp = compile(sel, from, to, selType, taken, inBranch = true)
            selType.asType match
                case '[s] =>
                    val f = Lambda(
                        to,
                        MethodType(List("selected"))(_ => List(selType), _ => TypeRepr.of[UI]),
                        (lo, params) =>
                            val arms =
                                cases.map(cd => CaseDef.copy(cd)(cd.pattern, cd.guard, compile(cd.rhs, from, lo, TypeRepr.of[UI], taken)))
                            Match(Ref(params.head.symbol), arms).changeOwner(lo)
                    ).asExprOf[s => UI]
                    '{ ReactiveMacro.pickOn[s](${ selProp.asExprOf[UI.Prop[s]] })($f)(using $frame) }.asTerm.changeOwner(to)
            end match
        end lazyPickOn

        // ---- What each run of a region takes from the reader (UI.Loss) ----
        //
        // A run draws the block's content again over the page, and the engine keeps every element it can. What the content itself
        // says to start over it cannot keep: a native field's default that reads starts the field over whenever a run moves it, and a
        // mount with no key starts a new instance. A binding is no loss (UI.Bound): the field shows it on every run, and kyo-ui puts back
        // an edit nobody takes. Both losses are refused where the block writes them; what the block cannot see (a component, a
        // helper, a prop whose runtime class decides) is the runtime check's, `UI.RenderKind.Lost`.
        if TypeRepr.of[T] <:< TypeRepr.of[UI] then
            // The block draws a region when the structure reads: a read, or a `val` that holds one, outside every channel argument.
            var drawsRegion = false
            new TreeTraverser:
                override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                    case t: Term if siteNodes.contains(t)              => () // a site draws its own region
                    case Read(_)                                       => drawsRegion = true
                    case id: Ident if channelOnly.contains(id.symbol)  => drawsRegion = true
                    case vd: ValDef if channelOnly.contains(vd.symbol) => ()
                    case dd: DefDef if isHandlerLambda(dd)             => () // reads when it runs, not in the region
                    case Apply(fn, args)                               =>
                        traverseTree(fn)(owner)
                        val handler = handlerArgs(fn, args)
                        channels(fn, args).zip(args).zipWithIndex.foreach { case ((c, a), i) =>
                            if c.isEmpty && handler(i).isEmpty then traverseTree(a)(owner)
                        }
                    case _ => super.traverseTree(tree)(owner)
            .traverseTree(rhs)(lambdaSym)

            if drawsRegion then
                val natives = List(
                    TypeRepr.of[UI.Ast.TextInput],
                    TypeRepr.of[UI.Ast.PickerInput],
                    TypeRepr.of[UI.Ast.BooleanInput],
                    TypeRepr.of[UI.Ast.RangeInput],
                    TypeRepr.of[UI.Ast.Opt]
                )
                // Whether `.name` on a `t` gives what the reader changes a default (UI.Bound.Default): an option's
                // `defaultSelected`, every other native field's `defaultValue` or `defaultChecked`. A binding (`value`, `checked`,
                // `selected`) is no loss: the field shows it on every run, which is what it says.
                def defaults(t: TypeRepr, name: String): Boolean =
                    if t.widen <:< TypeRepr.of[UI.Ast.Opt] then name == "defaultSelected"
                    else (name == "defaultValue" || name == "defaultChecked") && natives.exists(t.widen <:< _)
                def bindingOf(name: String): String = name match
                    case "defaultChecked"  => "checked"
                    case "defaultSelected" => "selected"
                    case _                 => "value"
                val uiModule                     = TypeRepr.of[UI.type].typeSymbol
                def isMount(s: Symbol): Boolean  = (s.name == "mounted" || s.name == "mountedWith") && s.owner == uiModule
                def isKeying(s: Symbol): Boolean = s.name == "keyed" || s.name == "keyedOnSite"

                // The call of `UI.mounted` itself, below its `using` clauses.
                def innermost(t: Term): Term = t match
                    case Apply(f: Apply, _) if f.symbol == t.symbol => innermost(f)
                    case _                                          => t
                // The mount a `.keyed` gives a key to: the receiver of the innermost application.
                // Whether `t` applies something to arguments (a value or a type list), as against naming the method.
                def applies(t: Term): Boolean = t match
                    case _: Apply | _: TypeApply => true
                    case _                       => false
                def receiver(t: Term): Maybe[Term] = t match
                    case Apply(f, List(r)) if !applies(f) => Present(r)
                    case Apply(f, _)                      => receiver(f)
                    case TypeApply(f, _)                  => receiver(f)
                    case _                                => Absent
                // Mounts found by their key: a `.keyed` call is an ancestor of the mount it keys, and the walk visits a node before its
                // children, so the key is recorded before the mount is checked.
                val keyed = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Tree, java.lang.Boolean])
                new TreeTraverser:
                    override def traverseTree(tree: Tree)(owner: Symbol): Unit =
                        tree match
                            case t: Apply if isKeying(t.symbol) => receiver(t).foreach(r => discard(keyed.add(innermost(strip(r)))))
                            case _                              => ()
                        tree match
                            case t @ Apply(Select(q, name), List(arg))
                                if defaults(q.tpe, name) && (containsRead(arg) || refsTo(reading.toSet)(arg)) =>
                                report.errorAndAbort(
                                    s"""`.$name` gives a native field a default that reads, in a `UI.reactive` block that draws a region,
                                       |and every run that moves the default draws it over what the reader typed or ticked: a
                                       |default is the reader's field from its start, so a new one starts the field over.
                                       |
                                       |Bind the field instead (`.${bindingOf(name)}(prop.read)` or a signal), which the field shows
                                       |and kyo-ui puts back after an edit, or give the default a value that does not read:
                                       |$example
                                       |""".stripMargin,
                                    t.pos
                                )
                            case t: Apply if isMount(t.symbol) && !applies(t.fun) && !keyed.contains(t) =>
                                report.errorAndAbort(
                                    s"""A mount with no key in a `UI.reactive` block that draws a region:
                                       |every run of the block starts it over, a new instance that draws again and allocates its
                                       |state anew.
                                       |
                                       |Give it a key (`.keyed(k)`), which keeps its instance across the runs, or draw it outside
                                       |the block.
                                       |""".stripMargin,
                                    t.pos
                                )
                            case _ => ()
                        end match
                        super.traverseTree(tree)(owner)
                    end traverseTree
                .traverseTree(rhs)(lambdaSym)
            end if
        end if

        val out = compile(rhs, lambdaSym, Symbol.spliceOwner, TypeRepr.of[T], Map.empty)
        new TreeTraverser:
            override def traverseTree(tree: Tree)(o: Symbol): Unit = tree match
                case id: Ident if id.symbol == scopeParam =>
                    report.errorAndAbort(
                        s"""The scope of a `UI.reactive` block is handed on.
                           |
                           |`.read` is taken out of the block only where the block itself is written, so code that takes
                           |the scope as a `using` parameter would read nothing. Read the value in the block and hand it on:
                           |$example
                           |""".stripMargin,
                        id.pos
                    )
                case _ => super.traverseTree(tree)(o)
        .traverseTree(out)(Symbol.spliceOwner)
        out.asExpr
    end expand

end ReactiveMacro
