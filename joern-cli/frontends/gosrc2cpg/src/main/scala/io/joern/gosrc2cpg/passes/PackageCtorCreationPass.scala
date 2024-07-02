package io.joern.gosrc2cpg.passes

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.astcreation.AstForPackageConstructorCreator
import io.joern.gosrc2cpg.datastructures.{GoGlobal, PackageMemberAst}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass

import scala.jdk.CollectionConverters.*

class PackageCtorCreationPass(cpg: Cpg, config: Config, goGlobal: GoGlobal)
    extends ForkJoinParallelCpgPass[(String, Set[PackageMemberAst])](cpg) {
  override def generateParts(): Array[(String, Set[PackageMemberAst])] = {
    val parts = goGlobal.pkgLevelVarAndConstantAstMap
      .keys()
      .asScala
      .map(key => (key, goGlobal.pkgLevelVarAndConstantAstMap.get(key)))
      .toArray
    // Clearing the cache once its used.
    goGlobal.pkgLevelVarAndConstantAstMap.clear()
    parts
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, part: (String, Set[PackageMemberAst])): Unit = {
    val (packageStr, statementAsts) = part
    val packageCtorAstCreator = new AstForPackageConstructorCreator(packageStr, statementAsts)(config.schemaValidation)
    val localDiff             = packageCtorAstCreator.createAst()
    diffGraph.absorb(localDiff)
  }
}
