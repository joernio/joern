package io.joern.csharpsrc2cpg

import better.files.File
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPassBase
import io.joern.x2cpg.utils.Report
import io.joern.csharpsrc2cpg.utils.AstGenRunner

import scala.util.Try

class CSharpSrc2Cpg extends X2CpgFrontend[Config] {
  private val report: Report = new Report()

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      File.usingTemporaryDirectory("csharpsrc2cpgOut") { tmpDir =>
        report.print()
        val astGenResult = new AstGenRunner(config).execute(tmpDir)
      }
    }
  }

}

object CSharpSrc2Cpg {

  def postProcessingPasses(cpg: Cpg, config: Config): List[CpgPassBase] = List(new NaiveCallLinker(cpg))

}
