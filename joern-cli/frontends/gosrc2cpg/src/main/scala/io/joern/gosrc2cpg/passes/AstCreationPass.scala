package io.joern.gosrc2cpg.passes

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.utils.AstGenRunner.AstGenRunnerResult
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.{Logger, LoggerFactory}

class AstCreationPass(cpg: Cpg, astGenRunnerResult: AstGenRunnerResult, config: Config)
    extends ConcurrentWriterCpgPass[(String, String)](cpg) {
  private val logger: Logger                            = LoggerFactory.getLogger(classOf[AstCreationPass])
  override def generateParts(): Array[(String, String)] = astGenRunnerResult.parsedFiles.toArray
  override def runOnPart(builder: DiffGraphBuilder, input: (String, String)): Unit = {
    val (rootPath, jsonFilename) = input
    println(rootPath)
    println(jsonFilename)
  }
}
