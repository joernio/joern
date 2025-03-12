package io.joern.gosrc2cpg.utils

import io.joern.gosrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, AstGenRunnerResult}
import io.joern.x2cpg.astgen.AstGenRunnerBase
import io.joern.x2cpg.utils.Environment.ArchitectureType.ArchitectureType
import io.joern.x2cpg.utils.Environment.OperatingSystemType.OperatingSystemType
import io.joern.x2cpg.utils.Environment
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.{ExternalCommand, FileUtil}
import org.slf4j.LoggerFactory

import java.nio.file.{Path, Paths}
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object AstGenRunner {
  private val logger = LoggerFactory.getLogger(getClass)
  case class GoAstGenRunnerResult(
    modulePath: String = "",
    parsedModFile: Option[String] = None,
    parsedFiles: List[String] = List.empty,
    skippedFiles: List[String] = List.empty
  ) extends AstGenRunnerResult
}

class AstGenRunner(config: Config, includeFileRegex: String = "") extends AstGenRunnerBase(config) {
  import io.joern.gosrc2cpg.utils.AstGenRunner.*

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

  override def skippedFiles(in: Path, astGenOut: List[String]): List[String] = {
    val skipped = astGenOut.collect {
      case out if !out.startsWith("Converted") =>
        val filename = out.substring(0, out.indexOf(" "))
        val reason   = out.substring(out.indexOf(" ") + 1)
        logger.warn(s"\t- failed to parse '${in / filename}': '$reason'")
        Option(filename)
      case out =>
        logger.debug(s"\t+ $out")
        None
    }
    skipped.flatten
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

  override def runAstGenNative(in: String, out: Path, exclude: String, include: String)(implicit
    metaData: AstGenProgramMetaData
  ): Try[Seq[String]] = {
    val excludeCommand = if (exclude.isEmpty) Seq.empty else Seq("-exclude", exclude)
    val includeCommand = if (include.isEmpty) Seq.empty else Seq("-include-packages", include)
    ExternalCommand
      .run((astGenCommand +: excludeCommand) ++ includeCommand ++ Seq("-out", out.toString, in), Some("."))
      .toTry
  }

  def executeForGo(out: Path): List[GoAstGenRunnerResult] = {
    implicit val metaData: AstGenProgramMetaData = config.astGenMetaData
    val in                                       = Paths.get(config.inputPath)
    logger.info(s"Running goastgen in '$config.inputPath' ...")
    runAstGenNative(config.inputPath, out, config.ignoredFilesRegex.toString(), includeFileRegex) match {
      case Success(result) =>
        val srcFiles = SourceFiles.determine(
          out.toString,
          Set(".json"),
          ignoredFilesRegex = Option(config.ignoredFilesRegex),
          ignoredFilesPath = Option(config.ignoredFiles)
        )
        val parsedModFile = filterModFile(srcFiles, out)
        val parsed        = filterFiles(srcFiles, out)
        val skipped       = skippedFiles(in, result.toList)
        segregateByModule(config.inputPath, out.toString, parsedModFile, parsed, skipped)
      case Failure(f) =>
        logger.error("\t- running astgen failed!", f)
        List()
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
        .foreach(modFile => {
          moduleMeta.addModFile(modFile, inputPath, outPath)
        })
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
