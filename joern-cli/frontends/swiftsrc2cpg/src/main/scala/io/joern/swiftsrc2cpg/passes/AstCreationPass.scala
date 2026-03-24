package io.joern.swiftsrc2cpg.passes

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.astcreation.AstCreator
import io.joern.swiftsrc2cpg.parser.SwiftJsonParser
import io.joern.swiftsrc2cpg.parser.SwiftJsonParser.ParseResult
import io.joern.swiftsrc2cpg.utils.AstGenRunner.AstGenRunnerResult
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider
import io.joern.swiftsrc2cpg.utils.SwiftTypesProvider.SwiftFileLocalTypeMapping
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
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
) extends ForkJoinParallelCpgPassWithAccumulator[String, AstCreationPass.Accumulator](cpg) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private var typeMap = SwiftTypesProvider(config).map(_.retrieveMappings()).getOrElse(Map.empty)

  private var collectedTypes: Set[String]                                              = Set.empty
  private var collectedExtensionInherits: Map[String, Set[String]]                     = Map.empty
  private var collectedExtensionMembers: Map[String, List[AstCreationPass.MemberInfo]] = Map.empty
  private var collectedExtensionMethodFullNameMapping: Map[String, String]             = Map.empty
  private var collectedMemberPropertyMapping: Map[String, String]                      = Map.empty

  def typesSeen(): Set[String]                                          = collectedTypes
  def extensionInherits(): Map[String, Set[String]]                     = collectedExtensionInherits
  def extensionMembers(): Map[String, List[AstCreationPass.MemberInfo]] = collectedExtensionMembers
  def extensionMethodFullNameMapping(): Map[String, String]             = collectedExtensionMethodFullNameMapping
  def memberPropertyMapping(): Map[String, String]                      = collectedMemberPropertyMapping

  override def createAccumulator(): AstCreationPass.Accumulator = AstCreationPass.Accumulator(
    usedTypes = mutable.HashSet.empty,
    extensionInheritMapping = mutable.HashMap.empty,
    extensionMethodFullNameMapping = mutable.HashMap.empty,
    extensionMemberMapping = mutable.HashMap.empty,
    memberPropertyMapping = mutable.HashMap.empty
  )

  override def mergeAccumulator(left: AstCreationPass.Accumulator, right: AstCreationPass.Accumulator): Unit = {
    left.usedTypes ++= right.usedTypes

    right.extensionInheritMapping.foreach { case (key, rightSet) =>
      left.extensionInheritMapping.updateWith(key) {
        case Some(leftSet) => Some(leftSet ++= rightSet)
        case None          => Some(rightSet)
      }
    }

    right.extensionMethodFullNameMapping.foreach { case (key, value) =>
      left.extensionMethodFullNameMapping.getOrElseUpdate(key, value)
    }

    right.extensionMemberMapping.foreach { case (key, rightBuf) =>
      left.extensionMemberMapping.updateWith(key) {
        case Some(leftBuf) => Some(leftBuf ++= rightBuf)
        case None          => Some(rightBuf)
      }
    }

    right.memberPropertyMapping.foreach { case (key, value) =>
      left.memberPropertyMapping.getOrElseUpdate(key, value)
    }
  }

  override def onAccumulatorComplete(builder: DiffGraphBuilder, accumulator: AstCreationPass.Accumulator): Unit = {
    collectedTypes = accumulator.usedTypes.toSet.removedAll(Defines.SwiftTypes)
    collectedExtensionInherits = accumulator.extensionInheritMapping.view.mapValues(_.toSet).toMap
    collectedExtensionMembers = accumulator.extensionMemberMapping.view.mapValues(_.toList).toMap
    collectedExtensionMethodFullNameMapping = accumulator.extensionMethodFullNameMapping.toMap
    collectedMemberPropertyMapping = accumulator.memberPropertyMapping.toMap
  }

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

  override def runOnPart(diffGraph: DiffGraphBuilder, input: String, accumulator: AstCreationPass.Accumulator): Unit = {
    val ((gotCpg, filename), duration) = TimeUtils.time {
      SwiftJsonParser.readFile(Paths.get(input)) match {
        case Success(parseResult) =>
          report.addReportInfo(parseResult.filename, parseResult.loc, parsed = true)
          Try {
            val fileLocalTypesMap = extractFileLocalTypeMap(parseResult)
            val astCreator        = new AstCreator(config, accumulator, parseResult, fileLocalTypesMap)
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

object AstCreationPass {

  case class MemberInfo(name: String, code: String, typeFullName: String)

  /** Per-thread accumulator for data collected during AST creation. Each thread receives its own instance; the
    * framework merges them after all parts complete.
    *
    * @param usedTypes
    *   Set of type full names encountered during AST creation.
    * @param extensionInheritMapping
    *   Mapping from extension fullName to the set of names it inherits from.
    * @param extensionMethodFullNameMapping
    *   Mapping from extension method fullName (provided by the compiler) to the fullName the frontend generates for
    *   fullName uniqueness.
    * @param extensionMemberMapping
    *   Mapping from extension fullName to the members it defines as computed properties.
    * @param memberPropertyMapping
    *   Mapping from member fullName to method fullName from its computed property.
    */
  case class Accumulator(
    usedTypes: mutable.HashSet[String],
    extensionInheritMapping: mutable.HashMap[String, mutable.HashSet[String]],
    extensionMethodFullNameMapping: mutable.HashMap[String, String],
    extensionMemberMapping: mutable.HashMap[String, mutable.ArrayBuffer[MemberInfo]],
    memberPropertyMapping: mutable.HashMap[String, String]
  ) {

    def addExtensionMember(
      extensionFullName: String,
      memberName: String,
      memberCode: String,
      memberTypeFullName: String
    ): Unit = {
      val memberInfo = MemberInfo(memberName, memberCode, memberTypeFullName)
      extensionMemberMapping.updateWith(extensionFullName) {
        case Some(buf) => Some(buf += memberInfo)
        case None      => Some(mutable.ArrayBuffer(memberInfo))
      }
    }

    def addExtensionInherits(extensionFullName: String, inheritNames: Seq[String]): Unit = {
      extensionInheritMapping.updateWith(extensionFullName) {
        case Some(set) => Some(set ++= inheritNames)
        case None      => Some(mutable.HashSet.from(inheritNames))
      }
    }

    def addExtensionMethodFullName(extensionMethodFullName: String, fullName: String): Unit = {
      extensionMethodFullNameMapping.getOrElseUpdate(extensionMethodFullName, fullName)
    }

    def addMemberPropertyFullName(memberFullName: String, propertyFullName: String): Unit = {
      memberPropertyMapping.getOrElseUpdate(memberFullName, propertyFullName)
    }

    def registerType(typeFullName: String): Unit = {
      usedTypes.add(typeFullName)
    }
  }

}
