package io.joern.swiftsrc2cpg.passes

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.astcreation.{AstCreator, SwiftSrcGlobal}
import io.joern.swiftsrc2cpg.parser.SwiftJsonParser
import io.joern.swiftsrc2cpg.parser.SwiftJsonParser.ParseResult
import io.joern.swiftsrc2cpg.utils.AstGenRunner.AstGenRunnerResult
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.SwiftFileLocalTypeMapping
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.utils.{Report, TimeUtils}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

class AstCreationPass(cpg: Cpg, astGenRunnerResult: AstGenRunnerResult, config: Config, report: Report = new Report())(
  implicit withSchemaValidation: ValidationMode
) extends ForkJoinParallelCpgPass[String](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private val global  = new SwiftSrcGlobal()
  private var typeMap = SwiftTypesProvider(config).map(_.retrieveMappings()).getOrElse(Map.empty)

  def typesSeen(): List[String] =
    global.usedTypes.keys().asScala.filterNot(Defines.SwiftTypes.contains).toList

  def extensionInherits(): Map[String, Set[String]] =
    global.extensionInheritMapping.asScala.view.mapValues(_.toSet).toMap

  def extensionMembers(): Map[String, List[SwiftSrcGlobal.MemberInfo]] =
    global.extensionMemberMapping.asScala.view.mapValues(_.toList).toMap

  def extensionMethodFullNameMapping(): Map[String, String] =
    global.extensionMethodFullNameMapping.asScala.toMap

  def memberPropertyMapping(): Map[String, String] =
    global.memberPropertyMapping.asScala.toMap

  override def generateParts(): Array[String] = astGenRunnerResult.parsedFiles.toArray

  override def finish(): Unit = {
    astGenRunnerResult.skippedFiles.foreach { skippedFile =>
      val filePath = Paths.get(skippedFile)
      val fileLOC = Try(IOUtils.readLinesInFile(filePath)) match {
        case Success(fileContent) => fileContent.size
        case Failure(exception) =>
          logger.warn(s"Failed to read file: '$filePath'", exception)
          -1
      }
      report.addReportInfo(skippedFile, fileLOC)
    }
  }

  private def extractFileLocalTypeMap(parseResult: ParseResult): SwiftFileLocalTypeMapping = {
    typeMap
      .collectFirst {
        // special handling for GitHub Windows and MacOS runner
        // Windows: C:\Users\RUNNER~1\AppData\Local\Temp\hash\Input\file.swift vs. C:\Users\runneradmin\AppData\Local\Temp\hash\Input\file.swift
        // macOS: /private/var/folders/y6/hash/T/Input/file.swift vs. /var/folders/y6/hash/T/Input/file.swift
        case (filename, map) if filename.replace("\\", "/").endsWith(parseResult.filename) => (filename, map)
      }
      .map { case (filename, map) =>
        typeMap = typeMap.removed(filename)
        map
      }
      .getOrElse(Map.empty)
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, input: String): Unit = {
    val ((gotCpg, filename), duration) = TimeUtils.time {
      SwiftJsonParser.readFile(Paths.get(input)) match {
        case Success(parseResult) =>
          report.addReportInfo(parseResult.filename, parseResult.loc, parsed = true)
          Try {
            val fileLocalTypesMap = extractFileLocalTypeMap(parseResult)
            val astCreator        = new AstCreator(config, global, parseResult, fileLocalTypesMap)
            val localDiff         = astCreator.createAst()
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
          logger.warn(s"Failed to read '$input'", exception)
          (false, input)
      }
    }
    report.updateReport(filename, cpg = gotCpg, duration)
  }

}
