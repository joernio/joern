package io.joern.csharpsrc2cpg.utils

import io.joern.csharpsrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.AstGenRunner
import io.joern.x2cpg.astgen.AstGenRunner.AstGenProgramMetaData
import io.joern.x2cpg.report.ProblemSink
import io.shiftleft.semanticcpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.collection.mutable
import scala.util.Try

object DotNetAstGenRunner {
  private object astGenMetaData extends AstGenProgramMetaData(name = "dotnetastgen", configPrefix = "csharpsrc2cpg")
}
class DotNetAstGenRunner(config: Config, problemSink: ProblemSink = ProblemSink.LogOnly)
    extends AstGenRunner(DotNetAstGenRunner.astGenMetaData, config, problemSink) {

  private val logger = LoggerFactory.getLogger(getClass)

  // The x86 variant seems to run well enough on MacOS M-family chips, whereas the ARM build crashes
  override val MacArm: String   = MacX86
  override val WinArm: String   = WinX86
  override val LinuxArm: String = "linux-arm64"

  override def fileFilter(file: String, out: Path): Boolean = {
    file.stripSuffix(".json").replace(out.toString, config.inputPath) match {
      case filePath if isIgnoredByUserConfig(filePath) => false
      case filePath if filePath.endsWith(".csproj")    => false
      case _                                           => true
    }
  }

  /** dotnetastgen parses files in parallel and does not say which file a diagnostic belongs to: the `fail:` lines with
    * the compiler errors only name a file in the `Error(s) encountered while parsing` line that comes after them. The
    * diagnostics are therefore only reported for a file if it is the only one that failed. Otherwise, they are logged
    * without a file, and the failed files are reported without them.
    */
  override def skippedFiles(in: Path, astGenOut: List[String]): List[String] = {
    val fileDiagnostics = mutable.LinkedHashMap.empty[String, Seq[String]]
    val failedFiles     = mutable.LinkedHashSet.empty[String]
    val diagnostics     = mutable.ArrayBuffer.empty[String]

    def relative(fileName: String): String = SourceFiles.toRelativePath(fileName, in.toString)

    def addReason(file: String, reason: String): Unit = {
      fileDiagnostics.updateWith(file) {
        case Some(existingReasons) => Some(existingReasons :+ reason)
        case None                  => Some(List(reason))
      }
    }

    astGenOut.map(_.strip()).foreach {
      case s"fail: DotNetAstGen.Program[0] Error(s) encountered while parsing: $fileName" =>
        failedFiles.add(relative(fileName))
      case s"fail: DotNetAstGen.Program[0] $reason"                             => diagnostics.append(reason)
      case s"warn: DotNetAstGen.Program[0] $filename does $reason, skipping..." =>
        addReason(relative(filename), s"does $reason")
      case s"info: DotNetAstGen.Program[0] Skipping file: $fileName" => addReason(relative(fileName), "Skipped")
      case _                                                         => // ignore
    }

    val attributeDiagnostics = failedFiles.size == 1
    if (diagnostics.nonEmpty && !attributeDiagnostics) {
      logger.warn(
        s"Diagnostics of dotnetastgen that cannot be assigned to a single file:\n${diagnostics.map(diagnostic => s" - $diagnostic").mkString("\n")}"
      )
    }

    (fileDiagnostics.keysIterator ++ failedFiles.iterator.filterNot(fileDiagnostics.contains)).map { file =>
      val reasons =
        if (attributeDiagnostics && failedFiles.contains(file))
          diagnostics.toSeq ++ fileDiagnostics.getOrElse(file, Nil)
        else fileDiagnostics.getOrElse(file, Nil)
      val message =
        if (reasons.isEmpty) "Failed to parse"
        else s"Failed to parse:\n${reasons.map(reason => s" - $reason").mkString("\n")}"
      reportParseProblem(logger, file, message)
      file
    }.toList
  }

  override protected def runAstGenNative(in: String, out: Path, exclude: String, include: String): Try[Seq[String]] = {
    val excludeCommand = if (exclude.isEmpty) Seq.empty else Seq("-e", exclude)
    ExternalCommand.run(Seq(astGenCommand, "-o", out.toString(), "-i", in) ++ excludeCommand).toTry
  }

}
