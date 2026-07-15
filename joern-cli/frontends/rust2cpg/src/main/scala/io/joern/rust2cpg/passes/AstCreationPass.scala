package io.joern.rust2cpg.passes

import io.joern.rust2cpg.Config
import io.joern.rust2cpg.astcreation.AstCreator
import io.joern.rust2cpg.parser.RustJsonParser
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPassWithAccumulator
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.collection.mutable
import scala.util.{Try, Success, Failure}

class AstCreationPass(cpg: Cpg, parsedFiles: Seq[String], config: Config)(implicit withSchemaValidation: ValidationMode)
    extends ForkJoinParallelCpgPassWithAccumulator[String, AstCreationPass.Accumulator](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  private val accumulator = AstCreationPass.Accumulator()

  override def createAccumulator(): AstCreationPass.Accumulator = {
    AstCreationPass.Accumulator()
  }

  override def mergeAccumulator(left: AstCreationPass.Accumulator, right: AstCreationPass.Accumulator): Unit = {
    left.merge(right)
  }

  override def onAccumulatorComplete(builder: DiffGraphBuilder, completed: AstCreationPass.Accumulator): Unit = {
    accumulator.merge(completed)
  }

  def traitImplMapping(): Map[String, Set[String]] = {
    accumulator.traitImplMapping.toMap
  }

  override def generateParts(): Array[String] = parsedFiles.toArray

  override def runOnPart(
    diffGraph: DiffGraphBuilder,
    jsonPath: String,
    accumulator: AstCreationPass.Accumulator
  ): Unit = {
    RustJsonParser.readFile(Paths.get(jsonPath)) match {
      case Success(parseResult) =>
        Try {
          val localDiff = new AstCreator(config, parseResult, accumulator).createAst()
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

object AstCreationPass {
  case class Accumulator(traitImplMapping: mutable.HashMap[String, Set[String]] = mutable.HashMap.empty) {
    def registerTraitImpl(implTypeFullName: String, traitImplFullName: String): Unit = {
      traitImplMapping.updateWith(implTypeFullName) {
        case Some(existing) => Some(existing + traitImplFullName)
        case None           => Some(Set(traitImplFullName))
      }
    }

    def merge(other: Accumulator): Unit = {
      other.traitImplMapping.foreach { case (implTypeFullName, traitImplFullNames) =>
        traitImplMapping.updateWith(implTypeFullName) {
          case Some(existing) => Some(existing ++ traitImplFullNames)
          case None           => Some(traitImplFullNames)
        }
      }
    }
  }
}
