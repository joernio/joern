package io.joern.rust2cpg.passes

import io.joern.rust2cpg.Config
import io.joern.rust2cpg.astcreation.AstCreator
import io.joern.rust2cpg.parser.RustJsonParser
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.util.{Try, Success, Failure}

class AstCreationPass(cpg: Cpg, parsedFiles: Seq[String], config: Config)(implicit withSchemaValidation: ValidationMode)
    extends ForkJoinParallelCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[String] = parsedFiles.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, jsonPath: String): Unit = {
    RustJsonParser.readFile(Paths.get(jsonPath)) match {
      case Success(parseResult) =>
        Try {
          val localDiff = new AstCreator(config, parseResult).createAst()
          diffGraph.absorb(localDiff)
        } match {
          case Success(_) =>
            logger.debug(s"Generated a CPG for '${parseResult.filename}'")
          case Failure(exception) =>
            logger.warn(s"Failed to generate a CPG for '${parseResult.fullPath}'", exception)
        }
      case Failure(exception) =>
        logger.warn(s"Failed to parse rust_ast_gen output '$jsonPath'", exception)
    }
  }
}
