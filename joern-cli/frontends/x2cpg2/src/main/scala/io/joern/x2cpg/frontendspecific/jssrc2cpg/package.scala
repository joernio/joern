package io.joern.x2cpg.frontendspecific

import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase

package object jssrc2cpg {

  def postProcessingPasses(cpg: Cpg, typeRecoveryConfig: XTypeRecoveryConfig): List[CpgPassBase] = {
    List(new JavaScriptInheritanceNamePass(cpg), new ConstClosurePass(cpg), new JavaScriptImportResolverPass(cpg))
      ++
        new JavaScriptTypeRecoveryPassGenerator(cpg, typeRecoveryConfig).generate() ++
        List(new JavaScriptTypeHintCallLinker(cpg), ObjectPropertyCallLinker(cpg), new NaiveCallLinker(cpg))
  }
}
