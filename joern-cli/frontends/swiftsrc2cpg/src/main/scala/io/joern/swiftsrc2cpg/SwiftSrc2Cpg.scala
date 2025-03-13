package io.joern.swiftsrc2cpg

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.swiftsrc2cpg.passes.*
import io.joern.swiftsrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.joern.x2cpg.utils.{HashUtil, Report}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.Paths
import scala.util.Try

class SwiftSrc2Cpg extends X2CpgFrontend[Config] {

  private val report: Report = new Report()

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      FileUtil.usingTemporaryDirectory("swiftsrc2cpgOut") { tmpDir =>
        val astGenResult = new AstGenRunner(config).execute(tmpDir)
        val hash         = HashUtil.sha256(astGenResult.parsedFiles.map(file => Paths.get(file)))

        val astCreationPass = new AstCreationPass(cpg, astGenResult, config, report)(config.schemaValidation)
        astCreationPass.createAndApply()

        SwiftTypeNodePass.withRegisteredTypes(astCreationPass.typesSeen(), cpg).createAndApply()
        new BuiltinTypesPass(cpg).createAndApply()

        new ExtensionInheritancePass(cpg).createAndApply()
        new SwiftMetaDataPass(cpg, hash, config.inputPath).createAndApply()
        new DependenciesPass(cpg).createAndApply()
        new ImportsPass(cpg).createAndApply()

        report.print()
      }
    }
  }

  // This method is intended for internal use only and may be removed at any time.
  def createCpgWithAllOverlays(config: Config): Try[Cpg] = {
    val maybeCpg = createCpgWithOverlays(config)
    maybeCpg.map { cpg =>
      new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
      val typeRecoveryConfig = XTypeRecoveryConfig(config.typePropagationIterations, !config.disableDummyTypes)
      swiftsrc2cpg.postProcessingPasses(cpg, typeRecoveryConfig).foreach(_.createAndApply())
      cpg
    }
  }

}
