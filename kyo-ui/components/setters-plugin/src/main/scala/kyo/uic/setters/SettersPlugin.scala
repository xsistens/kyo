package kyo.uic.setters

import dotty.tools.dotc.ast.untpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.plugins.StandardPlugin
import dotty.tools.dotc.util.NoSource
import dotty.tools.dotc.util.SourceFile

/** Writes the setters of the props fields marked `@member` as real members of their component, before the typer runs, so
  * every caller in the same run and every later one sees them, and so does the presentation compiler.
  */
class SettersPlugin extends StandardPlugin:
    val name                                                    = "kyo-uic-setters"
    val description                                             = "writes @member props fields as setter members of the component class"
    override def init(options: List[String]): List[PluginPhase] = List(new SettersPhase)
end SettersPlugin

class SettersPhase extends PluginPhase:
    val phaseName                        = "kyoUicSetters"
    override val runsAfter: Set[String]  = Set("parser")
    override val runsBefore: Set[String] = Set("typer")
    given SourceFile                     = NoSource

    private def isMember(annots: List[Tree]): Boolean =
        annots.exists(_.toString.contains("Ident(member)"))

    /** The props a component's first value parameter names: `Select.Props[A]` gives (Some(Select), Props), `Props[A]` (None, Props). */
    private def propsPath(tpt: Tree): Option[(Option[Name], Name)] = tpt match
        case Ident(n)              => Some((None, n))
        case AppliedTypeTree(t, _) => propsPath(t)
        case Select(Ident(q), n)   => Some((Some(q), n))
        case _                     => None

    /** The value fields of the case class the path names: qualified by its object, else top-level. */
    private def findProps(stats: List[Tree], path: (Option[Name], Name))(using Context): List[ValDef] =
        val (owner, propsName)                       = path
        def fieldsIn(body: List[Tree]): List[ValDef] =
            body.flatMap:
                case TypeDef(n, tm: Template) if n.toString == propsName.toString =>
                    tm.constr.paramss.flatten.collect { case v: ValDef => v }
                case _ => Nil
        owner match
            case None    => fieldsIn(stats)
            case Some(o) =>
                def inObjects(ts: List[Tree]): List[ValDef] =
                    ts.flatMap:
                        case ModuleDef(n, tm: Template) if n.toString == o.toString => fieldsIn(tm.body.toList)
                        case PackageDef(_, ss)                                      => inObjects(ss)
                        case _                                                      => Nil
                inObjects(stats)
        end match
    end findProps

    private def patchClass(td: TypeDef, stats: List[Tree])(using Context): TypeDef =
        td.rhs match
            case tmpl: Template =>
                val ctorParam: Option[ValDef]          = tmpl.constr.paramss.flatten.collectFirst { case v: ValDef => v }
                val path: Option[(Option[Name], Name)] = ctorParam.flatMap(p => propsPath(p.tpt))
                (ctorParam, path) match
                    case (Some(p), Some(pathOf)) =>
                        val members = findProps(stats, pathOf).filter(f => isMember(f.mods.annotations))
                        if members.isEmpty then td
                        else
                            val sp                     = td.span
                            def at[T <: Tree](t: T): T = t.withSpan(sp)
                            val tparams                = tmpl.constr.leadingTypeParams.map(tp => Ident(tp.name))
                            val selfTpe                =
                                if tparams.isEmpty then at(Ident(td.name))
                                else at(AppliedTypeTree(at(Ident(td.name)), tparams.map(at)))
                            val pRef             = at(Ident(p.name))
                            val existing         = tmpl.body.toList.collect { case d: DefDef => d.name.toString }.toSet
                            val defs: List[Tree] = members.filterNot(f => existing.contains(f.name.toString)).map: f =>
                                val isOpt = f.tpt match
                                    case AppliedTypeTree(Ident(n), _) if n.toString == "Opt" => true
                                    case _                                                   => false
                                val paramTpe = f.tpt match
                                    case AppliedTypeTree(Ident(n), List(inner)) if n.toString == "Opt" => inner
                                    case other                                                         => other
                                val v   = at(ValDef(termName("v"), paramTpe, EmptyTree).withMods(Modifiers(Flags.Param)))
                                val arg =
                                    if isOpt then
                                        at(Apply(at(Select(at(Ident(termName("Opt"))), termName("set"))), List(at(Ident(termName("v"))))))
                                    else at(Ident(termName("v")))
                                val copy = at(Apply(at(Select(pRef, termName("copy"))), List(at(NamedArg(f.name, arg)))))
                                at(DefDef(f.name, List(List(v)), selfTpe, at(New(selfTpe, List(List(copy))))))
                            TypeDef(
                                td.name,
                                Template(tmpl.constr, tmpl.parents, tmpl.self, tmpl.body ++ defs).withSpan(tmpl.span)
                            ).withMods(td.mods).withSpan(td.span)
                        end if
                    case _ => td
                end match
            case _ => td

    private def patch(stats: List[Tree])(using Context): List[Tree] =
        stats.map:
            case td: TypeDef                        => patchClass(td, stats)
            case md @ ModuleDef(name, tm: Template) =>
                val body = tm.body.toList.map:
                    case td: TypeDef => patchClass(td, stats)
                    case other       => other
                if body.lazyZip(tm.body.toList).forall((a, b) => a eq b) then md
                else ModuleDef(name, Template(tm.constr, tm.parents, tm.self, body).withSpan(tm.span)).withMods(md.mods).withSpan(md.span)
            case other => other

    override def run(using ctx: Context): Unit =
        for unit <- ctx.run.nn.units do
            unit.untpdTree match
                case pd @ PackageDef(pid, stats) =>
                    val patched = patch(stats)
                    if patched.lazyZip(stats).exists((a, b) => !(a eq b)) then unit.untpdTree = PackageDef(pid, patched).withSpan(pd.span)
                case _ =>
end SettersPhase
