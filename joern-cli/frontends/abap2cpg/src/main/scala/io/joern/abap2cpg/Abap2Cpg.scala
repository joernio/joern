package io.joern.abap2cpg

import io.joern.abap2cpg.parser.AbapAstGenRunner
import io.joern.abap2cpg.passes.{AstCreationPass, RefEdgePass}
import io.joern.x2cpg.{X2Cpg, X2CpgFrontend}
import io.joern.x2cpg.passes.base.ContainsEdgePass
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import io.shiftleft.semanticcpg.utils.FileUtil

import scala.util.Try

class Abap2Cpg extends X2CpgFrontend {
  override type ConfigType = Config
  override val defaultConfig: Config = Config()

  override def createCpg(config: Config): Try[Cpg] = {
    X2Cpg.withNewEmptyCpg(config.outputPath, config) { (cpg, _) =>
      FileUtil.usingTemporaryDirectory("abap2cpgOut") { tmpDir =>
        MetaDataPass(cpg, Languages.ABAP, config.inputPath).createAndApply()

        val runner  = new AbapAstGenRunner(config)
        val result  = runner.execute(tmpDir)

        val parsed  = result.parsedFiles
        val skipped = result.skippedFiles

        if (parsed.isEmpty) {
          logger.warn("No ABAP files were parsed. Check that the input path contains .abap files.")
        }
        if (skipped.nonEmpty) {
          logger.warn(s"Skipped ${skipped.size} file(s) due to parse errors.")
        }

        new AstCreationPass(cpg, parsed, config).createAndApply()
        new ContainsEdgePass(cpg).createAndApply()
        TypeNodePass.withTypesFromCpg(cpg).createAndApply()
        new RefEdgePass(cpg).createAndApply()
      }
    }
  }

  private val logger = org.slf4j.LoggerFactory.getLogger(classOf[Abap2Cpg])
}
