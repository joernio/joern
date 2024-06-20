package io.joern.x2cpg.frontendspecific

import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase

package object swiftsrc2cpg {

  def postProcessingPasses(cpg: Cpg, typeRecoveryConfig: XTypeRecoveryConfig): List[CpgPassBase] = {
    ???
  }
}
