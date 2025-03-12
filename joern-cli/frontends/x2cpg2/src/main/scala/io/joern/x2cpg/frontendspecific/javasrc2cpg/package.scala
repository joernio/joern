package io.joern.x2cpg.frontendspecific

import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase

package object javasrc2cpg {

  object ParameterNames {
    val EnableTypeRecovery = "enable-type-recovery"
  }

  def typeRecoveryPasses(cpg: Cpg, xtypeRecoveryConfig: XTypeRecoveryConfig): List[CpgPassBase] = {
    new JavaTypeRecoveryPassGenerator(cpg, xtypeRecoveryConfig).generate() :+
      new JavaTypeHintCallLinker(cpg)
  }

}
