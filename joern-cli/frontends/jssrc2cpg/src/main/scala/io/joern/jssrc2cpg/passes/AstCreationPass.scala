package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.astcreation.AstCreator
import io.joern.jssrc2cpg.parser.BabelJsonParser
import io.joern.x2cpg.{SourceFiles, ValidationMode}
import io.joern.x2cpg.astgen.AstGenRunner.AstGenRunnerResult
import io.joern.x2cpg.report.{Category, ScanReport}
import io.joern.x2cpg.utils.TimeUtils
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPassWithAccumulator
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class AstCreationPass(
  cpg: Cpg,
  astGenRunnerResult: AstGenRunnerResult,
  config: Config,
  report: ScanReport = new ScanReport()
)(implicit withSchemaValidation: ValidationMode)
    extends ForkJoinParallelCpgPassWithAccumulator[String, mutable.HashSet[String]](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private var collectedTypes: Set[String] = Set.empty

  def typesSeen(): Set[String] = collectedTypes

  override def createAccumulator(): mutable.HashSet[String] = mutable.HashSet.empty[String]

  override def mergeAccumulator(left: mutable.HashSet[String], right: mutable.HashSet[String]): Unit = left ++= right

  override def onAccumulatorComplete(builder: DiffGraphBuilder, accumulator: mutable.HashSet[String]): Unit = {
    collectedTypes = accumulator.toSet
  }

  override def generateParts(): Array[String] = astGenRunnerResult.parsedFiles.toArray

  override def finish(): Unit = {
    astGenRunnerResult.skippedFiles.foreach { fileName =>
      // astgen prints absolute paths, which `resolve` keeps as they are
      val filePath = Paths.get(config.inputPath).resolve(fileName)
      val fileLOC  = Try(IOUtils.readLinesInFile(filePath)).map(_.size).getOrElse(-1)
      report.addReportInfo(SourceFiles.toRelativePathIfBelow(fileName, config.inputPath), fileLOC)
    }
  }

  override def runOnPart(
    diffGraph: DiffGraphBuilder,
    jsonFilename: String,
    usedTypes: mutable.HashSet[String]
  ): Unit = {
    val parseResultMaybe               = BabelJsonParser.readFile(Paths.get(config.inputPath), Paths.get(jsonFilename))
    val ((gotCpg, filename), duration) = TimeUtils.time {
      parseResultMaybe match {
        case Success(parseResult) =>
          report.addReportInfo(parseResult.filename, parseResult.fileLoc, parsed = true)
          Try {
            val localDiff = new AstCreator(config, usedTypes, parseResult).createAst()
            diffGraph.absorb(localDiff)
          } match {
            case Failure(exception) =>
              report.warn(
                Category.FileProcessing,
                "Failed to process the file",
                Some(parseResult.filename),
                Some(exception),
                logger
              )
              (false, parseResult.filename)
            case Success(_) =>
              logger.debug(s"Processed '${parseResult.filename}'")
              (true, parseResult.filename)
          }
        case Failure(exception) =>
          val pathOfFailedFile = BabelJsonParser.stripExtension(jsonFilename)
          report.warn(
            Category.FileParse,
            "Failed to read the parse result of the AST generator",
            Some(pathOfFailedFile),
            Some(exception),
            logger
          )
          (false, pathOfFailedFile)
      }
    }
    report.updateReport(filename, cpg = gotCpg, duration)
  }

}
