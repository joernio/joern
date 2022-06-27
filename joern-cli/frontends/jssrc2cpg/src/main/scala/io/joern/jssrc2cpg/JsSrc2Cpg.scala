package io.joern.jssrc2cpg

import better.files.File
import io.joern.jssrc2cpg.passes.{AstCreationPass, BuiltinTypesPass, CallLinkerPass, JsMetaDataPass, TypeNodePass}
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.jssrc2cpg.utils.AstGenRunner.AstGenRunnerResult
import io.joern.jssrc2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.utils.HashUtil

import java.nio.file.Path
import scala.util.Try

class JsSrc2Cpg extends X2CpgFrontend[Config] {

  private val report: Report = new Report()

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      File.usingTemporaryDirectory("jssrc2cpgOut") { tmpDir =>
        val partialResult = AstGenRunner.execute(File(config.inputPath), tmpDir)
        val astgenResult =
          AstGenRunnerResult(parsedFiles = partialResult.parsedFiles, skippedFiles = partialResult.skippedFiles)

        val hash = HashUtil.sha256(astgenResult.parsedFiles.map { case (_, file) => Path.of(file) }.toSeq)

        val astCreationPass = new AstCreationPass(cpg, astgenResult, config, report)
        astCreationPass.createAndApply()
        new TypeNodePass(astCreationPass.allUsedTypes(), cpg).createAndApply()
        new CallLinkerPass(cpg).createAndApply()
        new JsMetaDataPass(cpg, hash, config.inputPath).createAndApply()
        new BuiltinTypesPass(cpg).createAndApply()
        report.print()
      }
    }
  }

}
