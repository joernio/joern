package io.joern.gosrc2cpg.utils

import better.files.File
import io.joern.gosrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, AstGenRunnerResult}
import io.joern.x2cpg.astgen.AstGenRunnerBase
import io.joern.x2cpg.utils.Environment.ArchitectureType.ArchitectureType
import io.joern.x2cpg.utils.Environment.OperatingSystemType.OperatingSystemType
import io.joern.x2cpg.utils.{Environment, ExternalCommand}
import org.slf4j.LoggerFactory

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object AstGenRunner {
  private val logger = LoggerFactory.getLogger(getClass)
  case class GoAstGenRunnerResult(
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

  override def skippedFiles(in: File, astGenOut: List[String]): List[String] = {
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

  override def fileFilter(file: String, out: File): Boolean = {
    file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
      case filePath if isIgnoredByUserConfig(filePath) => false
      case filePath if filePath.endsWith(".mod")       => false
      case _                                           => true
    }
  }

  private def filterModFile(files: List[String], out: File): List[String] = {
    files.filter { file =>
      file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
        case filePath if filePath.endsWith(".mod") => true
        case _                                     => false
      }
    }
  }

  override def runAstGenNative(in: String, out: File, exclude: String, include: String)(implicit
    metaData: AstGenProgramMetaData
  ): Try[Seq[String]] = {
    val excludeCommand = if (exclude.isEmpty) "" else s"-exclude \"$exclude\""
    val includeCommand = if (include.isEmpty) "" else s"-include \"$include\""
    ExternalCommand.run(s"$astGenCommand $excludeCommand $includeCommand -out ${out.toString()} $in", ".")
  }

  override def execute(out: File): AstGenRunnerResult = {
    implicit val metaData: AstGenProgramMetaData = config.astGenMetaData
    val in                                       = File(config.inputPath)
    logger.info(s"Running goastgen in '$config.inputPath' ...")
    runAstGenNative(config.inputPath, out, config.ignoredFilesRegex.toString(), includeFileRegex.toString()) match {
      case Success(result) =>
        val srcFiles = SourceFiles.determine(
          out.toString(),
          Set(".json"),
          ignoredFilesRegex = Option(config.ignoredFilesRegex),
          ignoredFilesPath = Option(config.ignoredFiles)
        )
        val parsedModFile = filterModFile(srcFiles, out)
        val parsed        = filterFiles(srcFiles, out)
        val skipped       = skippedFiles(in, result.toList)
        GoAstGenRunnerResult(parsedModFile.headOption, parsed, skipped)
      case Failure(f) =>
        logger.error("\t- running astgen failed!", f)
        GoAstGenRunnerResult()
    }
  }

}
