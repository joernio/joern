package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.astcreation.AstCreator
import io.joern.jssrc2cpg.parser.BabelJsonParser
import io.joern.jssrc2cpg.utils.AstGenRunner.AstGenRunnerResult
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPassWithAccumulator
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class AstCreationPass(cpg: Cpg, astGenRunnerResult: AstGenRunnerResult, config: Config, report: Report = new Report())(
  implicit withSchemaValidation: ValidationMode
) extends ForkJoinParallelCpgPassWithAccumulator[(String, String), mutable.HashSet[String]](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private var collectedTypes: Set[String] = Set.empty

  def typesSeen(): Set[String] = collectedTypes

  override def createAccumulator(): mutable.HashSet[String] = mutable.HashSet.empty[String]

  override def mergeAccumulator(left: mutable.HashSet[String], right: mutable.HashSet[String]): Unit = left ++= right

  override def onAccumulatorComplete(builder: DiffGraphBuilder, accumulator: mutable.HashSet[String]): Unit = {
    collectedTypes = accumulator.toSet
  }

  override def generateParts(): Array[(String, String)] = astGenRunnerResult.parsedFiles.toArray

  override def finish(): Unit = {
    astGenRunnerResult.skippedFiles.foreach { skippedFile =>
      val (rootPath, fileName) = skippedFile
      val filePath             = Paths.get(rootPath, fileName)
      val fileLOC = Try(IOUtils.readLinesInFile(filePath)) match {
        case Success(fileContent) => fileContent.size
        case Failure(exception) =>
          logger.warn(s"Failed to read file: '$filePath'", exception)
          -1
      }
      report.addReportInfo(fileName, fileLOC)
    }
  }

  override def runOnPart(
    diffGraph: DiffGraphBuilder,
    input: (String, String),
    usedTypes: mutable.HashSet[String]
  ): Unit = {
    val (rootPath, jsonFilename) = input
    val parseResultMaybe         = BabelJsonParser.readFile(Paths.get(rootPath), Paths.get(jsonFilename))
    val ((gotCpg, filename), duration) = TimeUtils.time {
      parseResultMaybe match {
        case Success(parseResult) =>
          report.addReportInfo(parseResult.filename, parseResult.fileLoc, parsed = true)
          Try {
            val localDiff = new AstCreator(config, usedTypes, parseResult).createAst()
            diffGraph.absorb(localDiff)
          } match {
            case Failure(exception) =>
              logger.warn(s"Failed to generate a CPG for: '${parseResult.filename}'", exception)
              (false, parseResult.filename)
            case Success(_) =>
              logger.debug(s"Generated a CPG for: '${parseResult.filename}'")
              (true, parseResult.filename)
          }
        case Failure(exception) =>
          val pathOfFailedFile = jsonFilename.replaceAll("\\.[^.]*$", "")
          logger.warn(s"Failed to read json parse result for: '$pathOfFailedFile'", exception)
          (false, pathOfFailedFile)
      }
    }
    report.updateReport(filename, cpg = gotCpg, duration)
  }

}
