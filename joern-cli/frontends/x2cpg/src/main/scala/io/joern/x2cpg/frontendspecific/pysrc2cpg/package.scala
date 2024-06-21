package io.joern.x2cpg.frontendspecific

import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase

package object pysrc2cpg {

  def postProcessingPasses(cpg: Cpg, typeRecoveryConfig: XTypeRecoveryConfig): List[CpgPassBase] = {
    List(
      new ImportsPass(cpg),
      new PythonImportResolverPass(cpg),
      new DynamicTypeHintFullNamePass(cpg),
      new PythonInheritanceNamePass(cpg)
    )
      ++ new PythonTypeRecoveryPassGenerator(cpg, typeRecoveryConfig).generate()
    ++ List (
      new PythonTypeHintCallLinker(cpg),
      new NaiveCallLinker(cpg),
      // Some of passes above create new methods, so, we
      // need to run the ASTLinkerPass one more time
      new AstLinkerPass(cpg)
    )
  }
}
