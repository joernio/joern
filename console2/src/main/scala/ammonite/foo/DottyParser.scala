package ammonite.foo

import dotty.tools.dotc
import dotc.ast.untpd
import dotc.core.Contexts.Context
import dotc.core.Flags
import dotc.core.StdNames.nme
import dotc.parsing.Parsers.{Location, Parser}
import dotc.parsing.Tokens
import dotc.reporting.IllegalStartOfStatement
import dotc.util.SourceFile

import scala.collection.mutable

class DottyParser(source: SourceFile)(using Context) extends Parser(source) with CompatibilityParser {

  // From
  // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
  //   compiler/src/dotty/tools/dotc/parsing/Parsers.scala/#L67-L71
  extension (buf: mutable.ListBuffer[untpd.Tree])
    def +++=(x: untpd.Tree) = x match {
      case x: untpd.Thicket => buf ++= x.trees
      case x => buf += x
    }

  private val oursLocalModifierTokens = Tokens.localModifierTokens + Tokens.PRIVATE

  override def localDef(
    start: Int,
    implicitMods: untpd.Modifiers = untpd.EmptyModifiers
  ): untpd.Tree = {
    var mods = defAnnotsMods(oursLocalModifierTokens)
    for (imod <- implicitMods.mods) mods = addMod(mods, imod)
    if (mods.is(Flags.Final))
      // A final modifier means the local definition is "class-like".
      // FIXME: Deal with modifiers separately
      tmplDef(start, mods)
    else
      defOrDcl(start, mods)
  }

  // Adapted from
  // https://github.com/lampepfl/dotty/blob/3.2.0/
  //   compiler/src/dotty/tools/dotc/parsing/Parsers.scala#L4075-L4094
  // Unlike it, we accept private modifiers for top-level definitions.
  override def blockStatSeq(): List[untpd.Tree] = checkNoEscapingPlaceholders {
    val stats = new mutable.ListBuffer[untpd.Tree]
    while
      var empty = false
      if (in.token == Tokens.IMPORT)
        stats ++= compatibilityImportClause()
      else if (isExprIntro)
        stats += expr(Location.InBlock)
      else if in.token == Tokens.IMPLICIT && !in.inModifierPosition() then
        stats += closure(
          in.offset,
          Location.InBlock,
          modifiers(scala.collection.immutable.BitSet(Tokens.IMPLICIT))
        )
      else if isIdent(nme.extension) && followingIsExtension() then
        stats += extension()
      else if isDefIntro(oursLocalModifierTokens, excludedSoftModifiers = Set(nme.`opaque`)) then
        stats +++= localDef(in.offset)
      else
        empty = true
      statSepOrEnd(stats, noPrevStat = empty, altEnd = Tokens.CASE)
    do ()
    stats.toList
  }
}
