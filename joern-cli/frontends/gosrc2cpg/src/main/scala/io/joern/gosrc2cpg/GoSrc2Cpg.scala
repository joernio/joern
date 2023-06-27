package io.joern.gosrc2cpg

import better.files.File
import io.joern.gosrc2cpg.passes.AstCreationPass
import io.joern.gosrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg

import scala.util.Try

class GoSrc2Cpg extends X2CpgFrontend[Config] {
  private val report: Report = new Report()

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      File.usingTemporaryDirectory("gosrc2cpgOut") { tmpDir =>
        val astGenResult = new AstGenRunner(config).execute(tmpDir)
        new AstCreationPass(cpg, astGenResult, config, report).createAndApply()
        report.print()
      }
    }
  }
}
