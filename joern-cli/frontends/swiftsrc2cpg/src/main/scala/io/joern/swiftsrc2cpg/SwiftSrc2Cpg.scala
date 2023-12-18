package io.joern.swiftsrc2cpg

import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.swiftsrc2cpg.passes.*
import io.joern.swiftsrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.joern.x2cpg.utils.{HashUtil, Report}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import scala.util.Try

class SwiftSrc2Cpg extends X2CpgFrontend[Config] {

  private val report: Report = new Report()

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      File.usingTemporaryDirectory("swiftsrc2cpgOut") { tmpDir =>
        val astGenResult = new AstGenRunner(config).execute(tmpDir)
        val hash         = HashUtil.sha256(astGenResult.parsedFiles.map { case (_, file) => File(file).path })

        val astCreationPass = new AstCreationPass(cpg, astGenResult, config, report)(config.schemaValidation)
        astCreationPass.createAndApply()

        SwiftTypeNodePass.withRegisteredTypes(astCreationPass.typesSeen(), cpg).createAndApply()
        new SwiftMetaDataPass(cpg, hash, config.inputPath).createAndApply()
        new BuiltinTypesPass(cpg).createAndApply()
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
      SwiftSrc2Cpg.postProcessingPasses(cpg, Option(config)).foreach(_.createAndApply())
      cpg
    }
  }

}

object SwiftSrc2Cpg {

  def postProcessingPasses(cpg: Cpg, config: Option[Config] = None): List[CpgPassBase] = {
    val typeRecoveryConfig =
      config.fold(XTypeRecoveryConfig())(c => XTypeRecoveryConfig(c.typePropagationIterations, !c.disableDummyTypes))
    List(new SwiftInheritanceNamePass(cpg), new ConstClosurePass(cpg)) ++
      new SwiftTypeRecoveryPassGenerator(cpg, typeRecoveryConfig).generate() ++ List(
        new SwiftTypeHintCallLinker(cpg),
        new NaiveCallLinker(cpg)
      )
  }

}
