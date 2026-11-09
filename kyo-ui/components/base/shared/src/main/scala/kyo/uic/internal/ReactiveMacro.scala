package kyo.uic.internal

import kyo.*
import kyo.uic.ReactiveScope
import scala.quoted.*

/** The expansion of `UI.reactive` (`Reactive.scala`), and what it expands to.
  *
  * A block reads props and signals with `.read`. The expansion is applicative: every read is taken out of the block and evaluated
  * once, before it, the reads are combined into one value (a chunk while every read is a constant, else one signal over the
  * distinct signals), and the block becomes a function of that value with each read replaced by its slot. A block that yields a UI
  * draws through one `render`, any other block maps through one `map`. Reads of one stable path share a slot at compile time, and
  * reads that reach one signal by different paths share one at run time, so the block never sees two values of one source.
  *
  * Taking a read out of the block is right only where the block itself would have evaluated it, once, while it runs. A handler
  * runs later, at its event: a by-name argument or a lambda whose result is a computation that may suspend (`UI.Action`, which
  * `onClick` takes) reads what it reads when it runs, each prop's value at that moment (`Prop.current`), and a `val` of the
  * block that only reaches channels and handlers stays a channel. So the expansion refuses a read of something the block defines,
  * a read inside any other lambda, local `def` or by-name argument (each runs later, or again, or not at all, with no effect to
  * read in), and the block's scope handed on to code the expansion cannot see.
  */
private[kyo] object ReactiveMacro:

    // ---- What the expansion calls at run time ----

    /** The reads as one value: a chunk of them while none is a signal, else one signal over each distinct signal, by reference. */
    def combine(reads: Seq[Any])(using Frame): Chunk[Any] | Signal[Chunk[Any]] =
        val signals = reads.foldLeft(Chunk.empty[Signal[Any]]) {
            case (acc, s: Signal[?]) if !acc.exists(_ eq s) => acc.append(s.asInstanceOf[Signal[Any]])
            case (acc, _)                                   => acc
        }
        if signals.isEmpty then Chunk.from(reads)
        else
            given CanEqual[Any, Any]               = CanEqual.derived
            given CanEqual[Chunk[Any], Chunk[Any]] = CanEqual.derived
            val slots                              = Chunk.from(reads.map {
                case s: Signal[?] => signals.indexWhere(_ eq s)
                case _            => -1
            })
            Signal.combineLatestAll(signals).map(values =>
                Chunk.from(reads.indices.map(i => if slots(i) < 0 then reads(i) else values(slots(i))))
            )
        end if
    end combine

    /** A value block: a constant from constants, else the block mapped over the reads' signal. */
    def value[T](reads: Seq[Any])(f: Chunk[Any] => T)(using CanEqual[T, T], Frame): UI.Prop[T] =
        combine(reads) match
            case s: Signal[?] => s.asInstanceOf[Signal[Chunk[Any]]].map(f)
            case c            => f(c.asInstanceOf[Chunk[Any]])

    /** A UI block: drawn once from constants, else one region over the reads' signal, compared by the values read. */
    def draw(reads: Seq[Any])(f: Chunk[Any] => UI)(using Frame): UI =
        combine(reads) match
            case s: Signal[?] => UI.render(s.asInstanceOf[Signal[Chunk[Any]]])(f)
            case c            => f(c.asInstanceOf[Chunk[Any]])

    /** A handler's reads, taken when the handler runs: each prop's value at its event, then the handler over them. */
    def atEvent[A, S](reads: Seq[Any])(f: Chunk[Any] => A < S)(using Frame): A < (S & Sync) =
        if reads.isEmpty then f(Chunk.empty)
        else Kyo.foreach(reads)(r => UI.Prop.current(r.asInstanceOf[UI.Prop[Any]])).map(values => f(Chunk.from(values)))

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

        val (scopeParam, rhs, lambdaSym) = strip(body.asTerm) match
            case Block(List(dd @ DefDef(_, List(TermParamClause(List(param))), _, Some(rhs))), _: Closure) =>
                (param.symbol, rhs, dd.symbol)
            case other =>
                report.errorAndAbort("`UI.reactive` takes its block written in place, not a function value.", other.pos)

        val scopeClass = TypeRepr.of[ReactiveScope].typeSymbol

        // A read through the block's own scope: `scope.read[A](arg)`, as the compiler writes a given member's extension.
        object Read:
            def unapply(t: Term): Option[Term] = t match
                case Apply(fn, List(arg)) =>
                    def sel(f: Term): Option[Term] = f match
                        case TypeApply(inner, _)                               => sel(inner)
                        case Select(q, "read") if f.symbol.owner == scopeClass => Some(q)
                        case _                                                 => None
                    sel(fn).collect { case q if strip(q).symbol == scopeParam => arg }
                case _ => None
        end Read

        // What the block defines, so no read refers to it.
        val local = scala.collection.mutable.Set.empty[Symbol]
        new TreeTraverser:
            override def traverseTree(tree: Tree)(owner: Symbol): Unit =
                tree match
                    case d: Definition => local += d.symbol
                    case _             => ()
                super.traverseTree(tree)(owner)
            end traverseTree
        .traverseTree(rhs)(lambdaSym)

        // Whether `t` refers to something the block defines outside `t`: what `t` defines itself (a lambda's parameter, a val in
        // it) is taken out of the block with it.
        def refersToLocal(t: Term): Boolean =
            val own = scala.collection.mutable.Set.empty[Symbol]
            new TreeTraverser:
                override def traverseTree(tree: Tree)(owner: Symbol): Unit =
                    tree match
                        case d: Definition => own += d.symbol
                        case _             => ()
                    super.traverseTree(tree)(owner)
                end traverseTree
            .traverseTree(t)(lambdaSym)
            var found = false
            new TreeTraverser:
                override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                    case id: Ident if local.contains(id.symbol) && !own.contains(id.symbol) => found = true
                    case _                                                                  => super.traverseTree(tree)(owner)
            .traverseTree(t)(lambdaSym)
            found
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

        def containsRead(t: Tree): Boolean =
            var found = false
            new TreeTraverser:
                override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                    case Read(_) => found = true
                    case _       => super.traverseTree(tree)(owner)
            .traverseTree(t)(lambdaSym)
            found
        end containsRead

        // A parameter that takes `T | Signal[T]` (a `Prop[T]`), as its `T`: an argument there may be a signal, so what it reads is
        // that argument's own channel and not a read of the block.
        val signalClass                                 = TypeRepr.of[Signal[Any]].typeSymbol
        def channelOf(param: TypeRepr): Maybe[TypeRepr] = param.dealias match
            case OrType(a, b) =>
                def signalOf(s: TypeRepr, other: TypeRepr): Maybe[TypeRepr] = s.dealias match
                    case AppliedType(c, List(x)) if c.typeSymbol == signalClass && x =:= other => Present(other)
                    case _                                                                     => Absent
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
        def handlerArgs(fn: Term, args: List[Term]): List[Boolean] =
            val params = paramTypes(fn)
            args.indices.toList.map(i =>
                params.lift(i) match
                    case Some(ByNameType(t)) => handlerType(t)
                    case _                   => false
            )
        end handlerArgs
        // The type a by-name parameter takes, for a handler argument.
        def byNameOf(fn: Term, i: Int): TypeRepr = paramTypes(fn)(i) match
            case ByNameType(t) => t
            case other         => other
        def isHandlerLambda(dd: DefDef): Boolean =
            dd.name.startsWith("$anonfun") && dd.rhs.exists(r => handlerType(r.tpe))

        def isDeferred(t: Tree): Boolean = t match
            case _: DefDef                                    => true
            case vd: ValDef if vd.symbol.flags.is(Flags.Lazy) => true
            case _                                            => false

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

        def refsTo(set: collection.Set[Symbol])(t: Tree): Boolean =
            var found = false
            new TreeTraverser:
                override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                    case id: Ident if set.contains(id.symbol) => found = true
                    case _                                    => super.traverseTree(tree)(owner)
            .traverseTree(t)(lambdaSym)
            found
        end refsTo

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

        // Each use of a reading `val`: inside a channel argument it needs nothing, inside another reading `val` it needs what that
        // one needs, anywhere else the region needs it.
        val structural = scala.collection.mutable.Set.empty[Symbol]
        val feeds      = scala.collection.mutable.ArrayBuffer.empty[(Symbol, Symbol)] // (used, user)
        final class Uses(inArg: Boolean, inVal: Maybe[Symbol], deferred: Boolean) extends TreeTraverser:
            override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
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
                case Apply(fn, args) if deferred && handlerArgs(fn, args).contains(true) =>
                    traverseTree(fn)(owner)
                    handlerArgs(fn, args).zip(args).foreach { (h, a) =>
                        if h then Uses(inArg = true, inVal, deferred = false).traverseTree(a)(owner) else traverseTree(a)(owner)
                    }
                case Apply(fn, args) if !deferred =>
                    traverseTree(fn)(owner)
                    val handler = handlerArgs(fn, args)
                    propArgs(fn, args).zip(args).zipWithIndex.foreach { case ((x, a), i) =>
                        if x.isDefined || handler(i) then Uses(inArg = true, inVal, deferred = false).traverseTree(a)(owner)
                        else traverseTree(a)(owner)
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

        def stablePath(t: Term): Maybe[List[Symbol]] = strip(t) match
            case id: Ident if isStable(id.symbol)   => Present(List(id.symbol))
            case Select(q, _) if isStable(t.symbol) => stablePath(q).map(_ :+ t.symbol)
            case _                                  => Absent
        def isStable(s: Symbol): Boolean =
            s.flags.is(Flags.Module) || (s.isValDef && !s.flags.is(Flags.Mutable)) || s.isPackageDef

        // Whether a handler reads: a read in it, or a channel-only `val` it uses.
        def readsInHandler(t: Tree): Boolean = containsRead(t) || refsTo(channelOnly)(t)

        // A handler as code that reads when it runs: every read in it (and every channel-only `val` it uses, through its prop) taken
        // at its start with `Prop.current`, in tree order, one per stable path, and the handler as a function of those values. A
        // handler inside it takes its own; a read in other deferred code inside it is refused, as in the block.
        def compileHandler(term: Term, owner: Symbol, expected: TypeRepr): Term =
            val (a, eff) = expected.widen.dealias match
                case AppliedType(_, List(a, eff)) => (a, eff)
                case other                        => report.errorAndAbort(s"not a handler: ${other.show}", term.pos)
            val own = scala.collection.mutable.Set.empty[Symbol]
            new TreeTraverser:
                override def traverseTree(tree: Tree)(o: Symbol): Unit =
                    tree match
                        case d: Definition => own += d.symbol
                        case _             => ()
                    super.traverseTree(tree)(o)
                end traverseTree
            .traverseTree(term)(owner)
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
                            if handler(i) then ()
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

            val keys    = reads.toList.map(stablePath)
            val slots   = keys.zipWithIndex.map { (k, i) => k.fold(i)(path => keys.indexWhere(_.contains(path))) }
            val kept    = slots.distinct
            val slotOf  = slots.map(kept.indexOf(_))
            val hoisted = Expr.ofSeq(kept.map(i => reads(i).changeOwner(owner).asExprOf[Any]))

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
                                tpe.widen.asType match
                                    case '[t] => '{ ${ values }(${ Expr(slot) }).asInstanceOf[t] }.asTerm
                            end slotValue
                            new TreeMap:
                                override def transformStatement(tree: Statement)(o: Symbol): Statement = tree match
                                    case dd: DefDef if isHandlerLambda(dd) && dd.rhs.exists(readsInHandler) =>
                                        DefDef.copy(dd)(
                                            dd.name,
                                            dd.paramss,
                                            dd.returnTpt,
                                            dd.rhs.map(compileHandler(_, dd.symbol, dd.returnTpt.tpe))
                                        )
                                    case _ => super.transformStatement(tree)(o)
                                override def transformTerm(tree: Term)(o: Symbol): Term = tree match
                                    case Read(_)                                      => slotValue(tree.tpe)
                                    case id: Ident if channelOnly.contains(id.symbol) => slotValue(id.tpe)
                                    case Apply(fn, args)                              =>
                                        val handler = handlerArgs(fn, args)
                                        val args1   = args.zipWithIndex.map { (x, i) =>
                                            if handler(i) && readsInHandler(x) then compileHandler(x, o, byNameOf(fn, i))
                                            else transformTerm(x)(o)
                                        }
                                        Apply.copy(tree)(transformTerm(fn)(o), args1)
                                    case _ => super.transformTerm(tree)(o)
                            .transformTerm(term)(owner).changeOwner(lambdaOwner)
                    ).asExprOf[Chunk[Any] => av < ev]
                    '{ ReactiveMacro.atEvent[av, ev]($hoisted)($f)(using $frame) }.asTerm.changeOwner(owner)
                case _ => report.errorAndAbort(s"not a handler: ${expected.show}", term.pos)
            end match
        end compileHandler

        // The block, a channel argument in it, or a channel-only `val`, as one value over its reads: every read in tree order (the
        // order the replacement visits them in) taken out, refusing the ones that cannot be, and the term as a function of the
        // values read. A read the region around already has is that region's value of the run. A channel argument or a
        // channel-only `val` inside is compiled the same way, on its own, and its reads are not the term's.
        // `from` owns what the term defines, `to` is where its value goes. The term is rewritten as it is and moved to `to` after, so
        // the symbols the data flow found are the ones it rewrites (moving a tree to a new owner gives its definitions new symbols).
        def compile(term: Term, from: Symbol, to: Symbol, result: TypeRepr, taken: Map[List[Symbol], Taken]): Term =
            def isTaken(arg: Term): Boolean = stablePath(arg).exists(taken.contains)
            // What this term reads: a read's argument, or the prop of a channel-only `val`.
            val reads = scala.collection.mutable.ArrayBuffer.empty[Term]
            final class Collector(deferred: Maybe[String]) extends TreeTraverser:
                override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                    case t @ Read(arg) =>
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
                    case id: Ident if channelOnly.contains(id.symbol) =>
                        reads += Ref(propOf(id.symbol))
                    case vd: ValDef if channelOnly.contains(vd.symbol) => () // its own value, compiled where it is replaced
                    case Apply(fn, args)                               =>
                        traverseTree(fn)(owner)
                        val byName  = paramTypes(fn).map { case ByNameType(_) => true; case _ => false }
                        val channel = if deferred.isEmpty then channels(fn, args) else args.map(_ => Absent)
                        val handler = handlerArgs(fn, args)
                        args.zipWithIndex.foreach { (a, i) =>
                            if channel(i).isDefined then () // its own value, compiled where it is replaced
                            else if handler(i) then ()      // a handler takes its own reads, when it runs
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

            // One slot per stable path, and one per other read.
            val keys    = reads.toList.map(stablePath)
            val slots   = keys.zipWithIndex.map { (k, i) => k.fold(i)(path => keys.indexWhere(_.contains(path))) }
            val kept    = slots.distinct
            val slotOf  = slots.map(kept.indexOf(_))
            val hoisted = Expr.ofSeq(kept.map(i => reads(i).changeOwner(to).asExprOf[Any]))

            // The term as a function of the read values, each channel argument and channel-only `val` in it replaced by its own
            // value, which takes what this term reads as this term's values of the run.
            def lambdaOf[R: Type]: Expr[Chunk[Any] => R] =
                val mt = MethodType(List("reads"))(_ => List(TypeRepr.of[Chunk[Any]]), _ => TypeRepr.of[R])
                Lambda(
                    to,
                    mt,
                    (lambdaOwner, params) =>
                        val values = Ref(params.head.symbol).asExprOf[Chunk[Any]]
                        val inner  = taken ++ keys.zip(slotOf).collect { case (Present(path), slot) => path -> Taken(values, slot) }
                        def valueOf(t: Taken, tpe: TypeRepr): Term = tpe.widen.asType match
                            case '[a] => '{ ${ t.values }(${ Expr(t.slot) }).asInstanceOf[a] }.asTerm
                        var next                           = 0
                        def slotValue(tpe: TypeRepr): Term =
                            val t = Taken(values, slotOf(next))
                            next += 1
                            valueOf(t, tpe)
                        end slotValue
                        new TreeMap:
                            override def transformStatement(tree: Statement)(o: Symbol): Statement = tree match
                                case dd: DefDef if isHandlerLambda(dd) && dd.rhs.exists(readsInHandler) =>
                                    DefDef.copy(dd)(
                                        dd.name,
                                        dd.paramss,
                                        dd.returnTpt,
                                        dd.rhs.map(compileHandler(_, dd.symbol, dd.returnTpt.tpe))
                                    )
                                case vd: ValDef if channelOnly.contains(vd.symbol) =>
                                    val tpe     = vd.tpt.tpe.widen
                                    val propTpe = tpe.asType match
                                        case '[t] => TypeRepr.of[UI.Prop[t]]
                                    val sym = Symbol.newVal(o, s"${vd.name}$$prop", propTpe, Flags.EmptyFlags, Symbol.noSymbol)
                                    propOf(vd.symbol) = sym
                                    ValDef(sym, Some(compile(vd.rhs.get, vd.symbol, sym, tpe, inner)))
                                case _ => super.transformStatement(tree)(o)
                            override def transformTerm(tree: Term)(o: Symbol): Term = tree match
                                case Read(arg) =>
                                    val had = stablePath(arg).flatMap(p => Maybe.fromOption(taken.get(p)))
                                    if had.isDefined then valueOf(had.get, tree.tpe) else slotValue(tree.tpe)
                                case id: Ident if channelOnly.contains(id.symbol) => slotValue(id.tpe)
                                case Apply(fn, args)                              =>
                                    val fn1     = transformTerm(fn)(o)
                                    val params  = paramTypes(fn)
                                    val channel = channels(fn, args)
                                    val handler = handlerArgs(fn, args)
                                    val args1   = args.zipWithIndex.map { (a, i) =>
                                        if channel(i).isEmpty && handler(i) && readsInHandler(a) then compileHandler(a, o, byNameOf(fn, i))
                                        else
                                            channel(i) match
                                                case Present(x) =>
                                                    strip(a) match
                                                        // A bare read, or a bare channel-only `val`, hands on what it reads.
                                                        case Read(of)
                                                            if !stablePath(of).exists(inner.contains) && of.tpe.widen <:< params(i) =>
                                                            of.changeOwner(o)
                                                        case id: Ident if channelOnly.contains(id.symbol) => Ref(propOf(id.symbol))
                                                        case _                                            => compile(a, o, o, x, inner)
                                                case Absent => transformTerm(a)(o)
                                    }
                                    Apply.copy(tree)(fn1, args1)
                                case _ => super.transformTerm(tree)(o)
                        .transformTerm(term)(from).changeOwner(lambdaOwner)
                ).asExprOf[Chunk[Any] => R]
            end lambdaOf

            result.asType match
                case '[r] if result <:< TypeRepr.of[UI] =>
                    val f = lambdaOf[UI]
                    '{ ReactiveMacro.draw($hoisted)($f)(using $frame) }.asTerm.changeOwner(to)
                case '[r] =>
                    val equal = canEqualOf(result).getOrElse(
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
        end compile

        // ---- What each run of a region takes from the reader (UI.Loss) ----
        //
        // A run draws the block's content again over the page, and the engine keeps every element it can. What the content itself
        // says to start over it cannot keep: a native field's value stated as a constant is put back by every run, and a mount with
        // no key starts a new instance. Both are refused where the block writes them; what the block cannot see (a component, a
        // helper, a prop whose runtime class decides) is the runtime check's, `UI.RenderKind.Lost`.
        if TypeRepr.of[T] <:< TypeRepr.of[UI] then
            // The block draws a region when the structure reads: a read, or a `val` that holds one, outside every channel argument.
            var drawsRegion = false
            new TreeTraverser:
                override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                    case Read(_)                                       => drawsRegion = true
                    case id: Ident if channelOnly.contains(id.symbol)  => drawsRegion = true
                    case vd: ValDef if channelOnly.contains(vd.symbol) => ()
                    case dd: DefDef if isHandlerLambda(dd)             => () // reads when it runs, not in the region
                    case Apply(fn, args)                               =>
                        traverseTree(fn)(owner)
                        val handler = handlerArgs(fn, args)
                        channels(fn, args).zip(args).zipWithIndex.foreach { case ((c, a), i) =>
                            if c.isEmpty && !handler(i) then traverseTree(a)(owner)
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
                // Whether `.name` on a `t` sets what the reader changes: an option's `value` is the value it stands for, its
                // `selected` the reader's choice; every other native field's `value` or `checked` is the reader's.
                def states(t: TypeRepr, name: String): Boolean =
                    if t.widen <:< TypeRepr.of[UI.Ast.Opt] then name == "selected"
                    else (name == "value" || name == "checked") && natives.exists(t.widen <:< _)
                def mayBeSignal(t: TypeRepr): Boolean = t.widen.dealias match
                    case OrType(a, b) => mayBeSignal(a) || mayBeSignal(b)
                    case other        => other.baseClasses.contains(signalClass)
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
                val keyed = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Tree, java.lang.Boolean])
                new TreeTraverser:
                    override def traverseTree(tree: Tree)(owner: Symbol): Unit =
                        tree match
                            case t: Apply if isKeying(t.symbol) => receiver(t).foreach(r => discard(keyed.add(innermost(strip(r)))))
                            case _                              => ()
                        super.traverseTree(tree)(owner)
                    end traverseTree
                .traverseTree(rhs)(lambdaSym)

                new TreeTraverser:
                    override def traverseTree(tree: Tree)(owner: Symbol): Unit =
                        tree match
                            case t @ Apply(Select(q, name), List(arg))
                                if states(q.tpe, name) && channels(t.fun, List(arg)).head.isEmpty && !mayBeSignal(arg.tpe) =>
                                report.errorAndAbort(
                                    s"""`.$name` states a native field's $name as a constant in a `UI.reactive` block that draws a region,
                                       |and every run of the block puts it back over what the reader typed or ticked, as soon as
                                       |anything else the block reads changes.
                                       |
                                       |Hand the field its $name as an argument that reads (`.$name(prop.read)`), which follows the
                                       |prop in place, or as a signal built outside the block, so the field is never drawn again:
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
