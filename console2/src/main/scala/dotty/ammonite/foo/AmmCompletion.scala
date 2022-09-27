package dotty.ammonite.foo

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names.{Name, termName}
import dotty.tools.dotc.core.Symbols.{Symbol, defn}
import dotty.tools.dotc.core.TypeError
import dotty.tools.dotc.interactive.{Completion, Interactive}
import dotty.tools.dotc.util.SourcePosition

object AmmCompletion extends AmmCompletionExtras {

  def completions(
    pos: SourcePosition,
    dependencyCompleteOpt: Option[String => (Int, Seq[String])],
    enableDeep: Boolean
  )(using Context): (Int, List[Completion]) = {
    val path = Interactive.pathTo(ctx.compilationUnit.tpdTree, pos.span)
    computeCompletions(
      pos,
      path,
      dependencyCompleteOpt,
      enableDeep
    )(using Interactive.contextOfPath(path))
  }

  def computeCompletions(
    pos: SourcePosition,
    path: List[Tree],
    dependencyCompleteOpt: Option[String => (Int, Seq[String])],
    enableDeep: Boolean
  )(using Context): (Int, List[Completion]) = {
    val mode = Completion.completionMode(path, pos)
    val prefix = Completion.completionPrefix(path, pos)
    val completer = new DeepCompleter(mode, prefix, pos)

    val hasBackTick = prefix.headOption.contains('`')

    var extra = List.empty[Completion]

    val completions = path match {
      case Select(qual, _) :: _                              => completer.selectionCompletions(qual)
      case Import(Ident(name), _) :: _
        if name.decode.toString == "$ivy" && dependencyCompleteOpt.nonEmpty =>
        val complete = dependencyCompleteOpt.get
        val (pos, completions) = complete(prefix)
        val input0 = prefix.take(pos)
        extra = extra ::: completions.distinct.toList
          .map(s => maybeBackticked(input0 + s, hasBackTick))
        Map.empty
      case Import(expr, _) :: _                              =>
        completer.directMemberCompletions(expr)
      case (_: untpd.ImportSelector) :: Import(expr, _) :: _ =>
        completer.directMemberCompletions(expr)
      case _                                                 =>
        completer.scopeCompletions ++ {
          if (enableDeep) completer.deepCompletions
          else Nil
        }
    }

    val describedCompletions = extra ++ Completion.describeCompletions(completions).map(backtick)
    val offset = Completion.completionOffset(path)

    (pos.span.start - prefix.length, describedCompletions)
  }

  class DeepCompleter(
    mode: Completion.Mode,
    prefix: String,
    pos: SourcePosition
  ) extends Completion.Completer(mode, prefix, pos):
    private def blacklisted(s: Symbol)(using Context) = {
      val blacklist = Set(
        "scala.Predef.any2stringadd.+",
        "scala.Any.##",
        "java.lang.Object.##",
        "scala.<byname>",
        "scala.<empty>",
        "scala.<repeated>",
        "scala.<repeated...>",
        "scala.Predef.StringFormat.formatted",
        "scala.Predef.Ensuring.ensuring",
        "scala.Predef.ArrowAssoc.->",
        "scala.Predef.ArrowAssoc.→",
        "java.lang.Object.synchronized",
        "java.lang.Object.ne",
        "java.lang.Object.eq",
        "java.lang.Object.wait",
        "java.lang.Object.notifyAll",
        "java.lang.Object.notify",
        "java.lang.Object.clone",
        "java.lang.Object.finalize"
      )

      blacklist(s.showFullName) ||
      s.isOneOf(GivenOrImplicit) ||
      // Cache objects, which you should probably never need to
      // access directly, and apart from that have annoyingly long names
      "cache[a-f0-9]{32}".r.findPrefixMatchOf(s.name.decode.toString).isDefined ||
      // s.isDeprecated ||
      s.name.decode.toString == "<init>" ||
      s.name.decode.toString.contains('$')
    }
    def deepCompletions(using Context): Map[Name, Seq[SingleDenotation]] = {

      val blacklistedPackages = Set("shaded")

      def allMembers(s: Symbol) =
        try s.info.allMembers
        catch {
          case _: dotty.tools.dotc.core.TypeError => Nil
        }
      def rec(t: Symbol): Seq[Symbol] =
        if (blacklistedPackages(t.name.toString))
          Nil
        else {
          val children =
            if (t.is(Package) || t.is(PackageVal) || t.is(PackageClass)) {
              allMembers(t).map(_.symbol).filter(!blacklisted(_)).filter(_ != t).flatMap(rec)
            } else Nil

          t +: children.toSeq
        }

      val syms = for {
        member <- allMembers(defn.RootClass).map(_.symbol).filter(!blacklisted(_)).toList
        sym <- rec(member)
        if sym.name.toString.startsWith(prefix)
      } yield sym

      syms.map(sym => (sym.fullName, List(sym: SingleDenotation))).toMap
    }

}

