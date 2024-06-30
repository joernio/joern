package io.joern.x2cpg.frontendspecific

import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase

package object swiftsrc2cpg {

  def postProcessingPasses(cpg: Cpg, typeRecoveryConfig: XTypeRecoveryConfig): List[CpgPassBase] = {
    List(new SwiftInheritanceNamePass(cpg), new ConstClosurePass(cpg)) ++
      new SwiftTypeRecoveryPassGenerator(cpg, typeRecoveryConfig).generate() ++ List(
        new SwiftTypeHintCallLinker(cpg),
        new NaiveCallLinker(cpg)
      )
  }
}
