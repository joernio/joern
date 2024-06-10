package io.joern.gosrc2cpg.passes

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.astcreation.AstForPackageConstructorCreator
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass

import scala.jdk.CollectionConverters.*

class PackageCtorCreationPass(cpg: Cpg, config: Config, goGlobal: GoGlobal)
    extends ConcurrentWriterCpgPass[(String, Set[(Ast, String)])](cpg) {
  override def generateParts(): Array[(String, Set[(Ast, String)])] =
    goGlobal.pkgLevelVarAndConstantAstMap
      .keys()
      .asScala
      .map(key => (key, goGlobal.pkgLevelVarAndConstantAstMap.get(key)))
      .toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, part: (String, Set[(Ast, String)])): Unit = {
    val (packageStr, statementAsts) = part
    val packageCtorAstCreator = new AstForPackageConstructorCreator(packageStr, statementAsts)(config.schemaValidation)
    val localDiff             = packageCtorAstCreator.createAst()
    diffGraph.absorb(localDiff)
  }
}
