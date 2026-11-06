package kyo.internal

import kyo.*
import scala.quoted.*

/** The expansion of [[kyo.UI.reactive]], and what it expands to.
  *
  * A block reads props and signals with `.read`. The expansion is applicative: every read is taken out of the block and evaluated
  * once, before it, the reads are combined into one value (a chunk while every read is a constant, else one signal over the
  * distinct signals), and the block becomes a function of that value with each read replaced by its slot. A block that yields a UI
  * draws through one `render`, any other block maps through one `map`. Reads of one stable path share a slot at compile time, and
  * reads that reach one signal by different paths share one at run time, so the block never sees two values of one source.
  *
  * Taking a read out of the block is right only where the block itself would have evaluated it, once, while it runs. So the
  * expansion refuses a read of something the block defines, a read inside a lambda, a local `def` or a by-name argument (each
  * runs later, or again, or not at all), and the block's scope handed on to code the expansion cannot see.
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

    def expand[T: Type](body: Expr[UI.ReactiveScope ?=> T], frame: Expr[Frame])(using Quotes): Expr[Any] =
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

        val scopeClass = TypeRepr.of[UI.ReactiveScope].typeSymbol

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

        def refersToLocal(t: Term): Boolean =
            var found = false
            new TreeTraverser:
                override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                    case id: Ident if local.contains(id.symbol) => found = true
                    case _                                      => super.traverseTree(tree)(owner)
            .traverseTree(t)(lambdaSym)
            found
        end refersToLocal

        def deferredError(at: Position, what: String): Nothing =
            report.errorAndAbort(
                s"""`.read` inside $what is not taken out of a `UI.reactive` block.
                   |
                   |The block reads every prop once, when it runs, and $what runs later, again or not at all, so a read
                   |there would see the value of the block's run instead of its own. Read into a `val` in the block and
                   |use the `val` there, or read a callback when its event comes (`live.call`):
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

        // Every read in tree order (the order the replacement below visits them in), refusing the ones that cannot be taken out.
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
                    reads += arg
                case Apply(fn, args) =>
                    traverseTree(fn)(owner)
                    val byName = fn.tpe.widen match
                        case MethodType(_, params, _) => params.map { case ByNameType(_) => true; case _ => false }
                        case _                        => Nil
                    args.zipWithIndex.foreach { (a, i) =>
                        if byName.lift(i).getOrElse(false) then
                            Collector(deferred.orElse(Present("a by-name argument"))).traverseTree(a)(owner)
                        else traverseTree(a)(owner)
                    }
                case dd: DefDef =>
                    val what = if dd.name.startsWith("$anonfun") then "a lambda" else s"the local `def ${dd.name}`"
                    dd.rhs.foreach(Collector(deferred.orElse(Present(what))).traverseTree(_)(dd.symbol))
                case vd: ValDef if vd.symbol.flags.is(Flags.Lazy) =>
                    vd.rhs.foreach(Collector(deferred.orElse(Present(s"the lazy `val ${vd.name}`"))).traverseTree(_)(vd.symbol))
                case _ => super.traverseTree(tree)(owner)
        end Collector
        Collector(Absent).traverseTree(rhs)(lambdaSym)

        // One slot per stable path, and one per other read.
        def stablePath(t: Term): Maybe[List[Symbol]] = strip(t) match
            case id: Ident if isStable(id.symbol)   => Present(List(id.symbol))
            case Select(q, _) if isStable(t.symbol) => stablePath(q).map(_ :+ t.symbol)
            case _                                  => Absent
        def isStable(s: Symbol): Boolean =
            s.flags.is(Flags.Module) || (s.isValDef && !s.flags.is(Flags.Mutable)) || s.isPackageDef
        val keys    = reads.toList.map(stablePath)
        val slots   = keys.zipWithIndex.map { (k, i) => k.fold(i)(path => keys.indexWhere(_.contains(path))) }
        val kept    = slots.distinct
        val slotOf  = slots.map(kept.indexOf(_))
        val hoisted = Expr.ofSeq(kept.map(i => reads(i).changeOwner(Symbol.spliceOwner).asExprOf[Any]))

        // The block as a function of the read values.
        def lambdaOf[R: Type]: Expr[Chunk[Any] => R] =
            val mt = MethodType(List("reads"))(_ => List(TypeRepr.of[Chunk[Any]]), _ => TypeRepr.of[R])
            Lambda(
                Symbol.spliceOwner,
                mt,
                (owner, params) =>
                    val values = Ref(params.head.symbol).asExprOf[Chunk[Any]]
                    var next   = 0
                    val out    = new TreeMap:
                        override def transformTerm(tree: Term)(o: Symbol): Term = tree match
                            case Read(_) =>
                                val slot = Expr(slotOf(next))
                                next += 1
                                tree.tpe.widen.asType match
                                    case '[a] => '{ $values($slot).asInstanceOf[a] }.asTerm
                            case _ => super.transformTerm(tree)(o)
                    .transformTerm(rhs.changeOwner(owner))(owner)
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
                    .traverseTree(out)(owner)
                    out
            ).asExprOf[Chunk[Any] => R]
        end lambdaOf

        if TypeRepr.of[T] <:< TypeRepr.of[UI] then
            val f = lambdaOf[UI]
            '{ ReactiveMacro.draw($hoisted)($f)(using $frame) }
        else
            val equal = Expr.summon[CanEqual[T, T]].getOrElse(
                report.errorAndAbort(
                    s"""A `UI.reactive` block that yields a ${Type.show[T]} needs a `CanEqual[${Type.show[T]}, ${Type.show[T]}]`.
                       |
                       |Over a signal the block is mapped, and the mapped signal compares each value with the one before.
                       |""".stripMargin,
                    body.asTerm.pos
                )
            )
            val f = lambdaOf[T]
            '{ ReactiveMacro.value[T]($hoisted)($f)(using $equal, $frame) }
        end if
    end expand

end ReactiveMacro
