package io.joern.abap2cpg.passes

import io.joern.abap2cpg.Config
import io.joern.abap2cpg.parser.AbapJsonParser
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory

import java.nio.file.{Path, Paths}
import scala.util.{Failure, Success}

class AstCreationPass(cpg: Cpg, jsonFiles: List[String], config: Config)
    extends ForkJoinParallelCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])
  private val parser = AbapJsonParser()

  override def generateParts(): Array[String] = jsonFiles.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, jsonFile: String): Unit = {
    parser.parseFile(Paths.get(jsonFile)) match {
      case Success(program) =>
        logger.debug(s"Generating CPG for: ${program.fileName}")
        val generated = CpgGenerator().generateCpg(program)
        diffGraph.absorb(generated)
      case Failure(exception) =>
        logger.warn(s"Failed to parse '$jsonFile': ${exception.getMessage}")
    }
  }
}
