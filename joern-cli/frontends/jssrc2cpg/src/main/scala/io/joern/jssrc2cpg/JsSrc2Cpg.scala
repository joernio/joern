package io.joern.jssrc2cpg

import better.files.File
import io.joern.jssrc2cpg.passes.AstCreationPass
import io.joern.jssrc2cpg.passes.BuiltinTypesPass
import io.joern.jssrc2cpg.passes.CallLinkerPass
import io.joern.jssrc2cpg.passes.JsMetaDataPass
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.jssrc2cpg.utils.AstGenRunner.AstGenRunnerResult
import io.joern.jssrc2cpg.utils.FileUtils
import io.joern.jssrc2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend

import java.nio.file.Paths
import scala.util.Try

class JsSrc2Cpg extends X2CpgFrontend[Config] {

  private val report: Report = new Report()

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      File.usingTemporaryDirectory("jssrc2cpgOut") { tmpDir =>
        val astgenResult =
          config.inputPaths.foldLeft(AstGenRunnerResult()) { (result, input) =>
            val partialResult = AstGenRunner.execute(File(input), tmpDir)
            result.copy(
              parsedFiles = result.parsedFiles ++ partialResult.parsedFiles,
              skippedFiles = result.skippedFiles ++ partialResult.skippedFiles
            )
          }
        val hash = FileUtils.md5(astgenResult.parsedFiles.toSeq.map(f => Paths.get(f._2)))
        new AstCreationPass(cpg, astgenResult, config, report).createAndApply()
        new CallLinkerPass(cpg).createAndApply()
        new JsMetaDataPass(cpg, hash).createAndApply()
        new BuiltinTypesPass(cpg).createAndApply()
        report.print()
      }
    }
  }

}
