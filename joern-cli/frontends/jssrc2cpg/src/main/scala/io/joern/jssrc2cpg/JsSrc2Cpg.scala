package io.joern.jssrc2cpg

import better.files.File
import io.joern.jssrc2cpg.passes.AstCreationPass
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.jssrc2cpg.utils.AstGenRunner.AstGenRunnerResult
import io.joern.jssrc2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.KeyPoolCreator
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.passes.IntervalKeyPool
import io.joern.x2cpg.passes.frontend.TypeNodePass

class JsSrc2Cpg {

  private val report: Report = new Report()

  def createCpg(config: Config): Cpg = {
    val keyPool         = KeyPoolCreator.obtain(2, minValue = 101)
    val metaDataKeyPool = new IntervalKeyPool(1, 100)
    val typesKeyPool    = keyPool.head
    val astKeyPool      = keyPool(1)

    val outputPath = Some(config.outputPath)
    val cpg        = newEmptyCpg(outputPath)

    File.usingTemporaryDirectory("jssrc2cpgOut") { tmpDir =>
      val astgenResult =
        config.inputPaths.foldLeft(AstGenRunnerResult()) { (result, input) =>
          val partialResult = AstGenRunner.execute(File(input), tmpDir)
          result.copy(
            parsedFiles = result.parsedFiles ++ partialResult.parsedFiles,
            skippedFiles = result.skippedFiles ++ partialResult.skippedFiles
          )
        }
      new MetaDataPass(cpg, "NEWJS", Some(metaDataKeyPool)).createAndApply()
      val astCreationPass = new AstCreationPass(cpg, astgenResult, Some(astKeyPool), config, report)
      astCreationPass.createAndApply()
      new TypeNodePass(astCreationPass.usedTypes(), cpg, Some(typesKeyPool)).createAndApply()
      report.print()
    }

    cpg
  }

}
