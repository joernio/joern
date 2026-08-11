package io.joern.gosrc2cpg.utils

import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, AstGenRunnerResult, SkippedFile}
import io.joern.x2cpg.astgen
import io.joern.x2cpg.astgen.AstGenRunner
import io.joern.x2cpg.utils.Environment.ArchitectureType
import io.joern.x2cpg.utils.Environment.OperatingSystemType
import io.joern.x2cpg.utils.Environment
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.{ExternalCommand, ExternalCommandResult, FileUtil}
import org.slf4j.LoggerFactory

import java.nio.file.{Path, Paths}
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object GoAstGenRunner {
  private val logger = LoggerFactory.getLogger(getClass)
  case class GoAstGenRunnerResult(
    modulePath: String = "",
    parsedModFile: Option[String] = None,
    parsedFiles: List[String] = List.empty,
    skippedFiles: List[String] = List.empty
  ) extends AstGenRunnerResult

  private object astGenMetaData extends AstGenProgramMetaData(name = "goastgen", configPrefix = "gosrc2cpg")
}

class GoAstGenRunner(config: Config, includeFileRegex: String = "")
    extends AstGenRunner(GoAstGenRunner.astGenMetaData, config) {
  import io.joern.gosrc2cpg.utils.GoAstGenRunner.*

  override val WinX86   = "windows.exe"
  override val LinuxArm = "linux-arm64"
  override val MacArm   = "macos-arm64"

  override val SupportedBinaries: Set[(OperatingSystemType, ArchitectureType)] = Set(
    Environment.OperatingSystemType.Windows -> Environment.ArchitectureType.X86,
    Environment.OperatingSystemType.Linux   -> Environment.ArchitectureType.X86,
    Environment.OperatingSystemType.Linux   -> Environment.ArchitectureType.ARMv8,
    Environment.OperatingSystemType.Mac     -> Environment.ArchitectureType.X86,
    Environment.OperatingSystemType.Mac     -> Environment.ArchitectureType.ARMv8
  )

  // goastgen writes both success and failure lines to stdout (`Converted AST for ...` / `Failed to generate AST for
  // <file> `); per-file diagnostics (`[ERROR] ...`) go to stderr but aren't reliably correlatable to a single file.
  override def skippedFiles(in: Path, runResult: ExternalCommandResult): List[SkippedFile] = {
    runResult.stdOut
      .map(_.trim)
      .collect { case s"Failed to generate AST for $filename" =>
        SkippedFile(toRelativeInputPath(filename.trim, in), "failed to generate AST (see stderr for details)")
      }
      .toList
  }

  override def fileFilter(file: String, out: Path): Boolean = {
    file.stripSuffix(".json").replace(out.toString, config.inputPath) match {
      case filePath if isIgnoredByUserConfig(filePath) => false
      case filePath if filePath.endsWith(".mod")       => false
      case _                                           => true
    }
  }

  private def filterModFile(files: List[String], out: Path): List[String] = {
    files.filter { file =>
      file.stripSuffix(".json").replace(out.toString, config.inputPath) match {
        case filePath if filePath.endsWith(".mod") => true
        case _                                     => false
      }
    }
  }

  override def runAstGenNative(in: String, out: Path, exclude: String, include: String): ExternalCommandResult = {
    val excludeCommand = if (exclude.isEmpty) Seq.empty else Seq("-exclude", exclude)
    val includeCommand = if (include.isEmpty) Seq.empty else Seq("-include-packages", include)
    ExternalCommand.run((astGenCommand +: excludeCommand) ++ includeCommand ++ Seq("-out", out.toString, in))
  }

  def executeForGo(out: Path): List[GoAstGenRunnerResult] = {
    val in = Paths.get(config.inputPath)
    logger.info(s"Running goastgen in '$config.inputPath' ...")
    val runResult = Try(runAstGenNative(config.inputPath, out, config.ignoredFilesRegex.toString(), includeFileRegex))
    val srcFiles  = astGenOutputFiles(out)
    val parsedModFile = filterModFile(srcFiles, out)
    val parsed        = filterFiles(srcFiles, out)
    runResult match {
      case Success(result) =>
        if (!isSuccess(result)) logUnsuccessfulRun(result)
        segregateByModule(config.inputPath, out.toString, parsedModFile, parsed, collectSkippedFiles(in, result))
      case Failure(exception) =>
        logger.error("\t- running goastgen failed!", exception)
        segregateByModule(config.inputPath, out.toString, parsedModFile, parsed, List.empty)
    }
  }

  /** Segregate all parsed files including go.mod files under separate modules. This will also segregate modules defined
    * inside another module
    */
  private def segregateByModule(
    inputPath: String,
    outPath: String,
    parsedModFiles: List[String],
    parsedFiles: List[String],
    skippedFiles: List[String]
  ): List[GoAstGenRunnerResult] = {
    val moduleMeta: ModuleMeta =
      ModuleMeta(inputPath, outPath, None, ListBuffer[String](), ListBuffer[String](), ListBuffer[ModuleMeta]())
    if (parsedModFiles.nonEmpty) {
      parsedModFiles
        .sortBy(_.split(UtilityConstants.fileSeparateorPattern).length)
        .foreach { modFile =>
          moduleMeta.addModFile(modFile, inputPath, outPath)
        }
      parsedFiles.foreach(moduleMeta.addParsedFile)
      skippedFiles.foreach(moduleMeta.addSkippedFile)
      moduleMeta.getOnlyChildren
    } else {
      parsedFiles.foreach(moduleMeta.addParsedFile)
      skippedFiles.foreach(moduleMeta.addSkippedFile)
      moduleMeta.getAllChildren
    }
  }

  private def getParentFolder(path: String): String = {
    val parent = Paths.get(path).getParent
    if (parent != null) parent.toString else ""
  }

  case class ModuleMeta(
    modulePath: String,
    outputModulePath: String,
    modFilePath: Option[String],
    parsedFiles: ListBuffer[String],
    skippedFiles: ListBuffer[String],
    childModules: ListBuffer[ModuleMeta]
  ) {
    def addModFile(modFile: String, inputPath: String, outPath: String): Unit = {
      childModules.collectFirst {
        case childMod if modFile.startsWith(childMod.outputModulePath) =>
          childMod.addModFile(modFile, inputPath, outPath)
      } match {
        case None =>
          val outmodpath = getParentFolder(modFile)
          childModules.addOne(
            ModuleMeta(
              outmodpath.replace(outPath, inputPath),
              outmodpath,
              Some(modFile),
              ListBuffer[String](),
              ListBuffer[String](),
              ListBuffer[ModuleMeta]()
            )
          )
        case _ =>
      }
    }

    def addParsedFile(parsedFile: String): Unit = {
      childModules.collectFirst {
        case childMod if parsedFile.startsWith(childMod.outputModulePath) =>
          childMod.addParsedFile(parsedFile)
      } match {
        case None => parsedFiles.addOne(parsedFile)
        case _    =>
      }
    }

    def addSkippedFile(skippedFile: String): Unit = {
      childModules.collectFirst {
        case childMod if skippedFile.startsWith(childMod.outputModulePath) =>
          childMod.addSkippedFile(skippedFile)
      } match {
        case None => skippedFiles.addOne(skippedFile)
        case _    =>
      }
    }

    def getOnlyChildren: List[GoAstGenRunnerResult] = {
      childModules.flatMap(_.getAllChildren).toList
    }

    def getAllChildren: List[GoAstGenRunnerResult] = {
      getOnlyChildren ++ List(
        GoAstGenRunnerResult(
          modulePath = modulePath,
          parsedModFile = modFilePath,
          parsedFiles = parsedFiles.toList,
          skippedFiles = skippedFiles.toList
        )
      )
    }
  }
}
