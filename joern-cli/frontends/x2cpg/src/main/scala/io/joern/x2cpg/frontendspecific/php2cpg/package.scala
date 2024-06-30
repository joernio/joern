package io.joern.x2cpg.frontendspecific

import io.joern.x2cpg.passes.frontend.{XTypeRecoveryConfig, XTypeStubsParserConfig}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase

package object php2cpg {

  def postProcessingPasses(
    cpg: Cpg,
    typeRecoveryConfig: XTypeRecoveryConfig = XTypeRecoveryConfig(iterations = 3),
    setKnownTypesConfig: XTypeStubsParserConfig = XTypeStubsParserConfig()
  ): List[CpgPassBase] = {
    List(
      new ComposerAutoloadPass(cpg),
      new PhpTypeStubsParserPass(cpg, setKnownTypesConfig)
    ) ++ new PhpTypeRecoveryPassGenerator(cpg, typeRecoveryConfig).generate() :+ PhpTypeHintCallLinker(cpg)
  }
}
