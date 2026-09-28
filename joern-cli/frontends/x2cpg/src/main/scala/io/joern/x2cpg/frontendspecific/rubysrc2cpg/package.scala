package io.joern.x2cpg.frontendspecific

import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.language.*

package object rubysrc2cpg {

  def postProcessingPasses(
    cpg: Cpg,
    typeRecoveryConfig: XTypeRecoveryConfig = XTypeRecoveryConfig(iterations = 4)
  ): List[CpgPassBase] = {
    val implicitRequirePass = if (cpg.dependency.name.contains("zeitwerk")) ImplicitRequirePass(cpg) :: Nil else Nil
    implicitRequirePass ++ List(ImportsPass(cpg), RubyImportResolverPass(cpg)) ++
      new RubyTypeRecoveryPassGenerator(cpg, config = typeRecoveryConfig)
        .generate() ++ List(new RubyTypeHintCallLinker(cpg), new NaiveCallLinker(cpg), new AstLinkerPass(cpg))
  }
}
