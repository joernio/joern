package io.joern.jssrc2cpg

import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.jssrc2cpg.passes.*
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.frontendspecific.jssrc2cpg.postProcessingPasses
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.joern.x2cpg.utils.{HashUtil, Report}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import scala.util.Try

class JsSrc2Cpg extends X2CpgFrontend[Config] {

  private val report: Report = new Report()

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      File.usingTemporaryDirectory("jssrc2cpgOut") { tmpDir =>
        val astGenResult = new AstGenRunner(config).execute(tmpDir)
        val hash         = HashUtil.sha256(astGenResult.parsedFiles.map { case (_, file) => File(file).path })

        val astCreationPass = new AstCreationPass(cpg, astGenResult, config, report)(config.schemaValidation)
        astCreationPass.createAndApply()

        JavaScriptTypeNodePass.withRegisteredTypes(astCreationPass.typesSeen(), cpg).createAndApply()
        new JavaScriptMetaDataPass(cpg, hash, config.inputPath).createAndApply()
        new DependenciesPass(cpg, config).createAndApply()
        new ConfigPass(cpg, config, report).createAndApply()
        new PrivateKeyFilePass(cpg, config, report).createAndApply()
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
      postProcessingPasses(cpg, typeRecoveryConfig).foreach(_.createAndApply())
      cpg
    }
  }

}
