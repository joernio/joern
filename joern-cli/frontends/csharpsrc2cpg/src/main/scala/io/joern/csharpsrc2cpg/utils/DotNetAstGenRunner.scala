package io.joern.csharpsrc2cpg.utils

import io.joern.csharpsrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, SkippedFile}
import io.joern.x2cpg.astgen.AstGenRunner
import io.shiftleft.semanticcpg.utils.{ExternalCommand, ExternalCommandResult}

import java.nio.file.Path
import scala.collection.mutable

object DotNetAstGenRunner {
  private object astGenMetaData extends AstGenProgramMetaData(name = "dotnetastgen", configPrefix = "csharpsrc2cpg")
}
class DotNetAstGenRunner(config: Config) extends AstGenRunner(DotNetAstGenRunner.astGenMetaData, config) {

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

  // dotnetastgen writes everything to stdout as `info:/warn:/fail: DotNetAstGen.Program[0] <msg>` lines. Per-file
  // compiler errors (`fail: <reason>`) are printed without a filename, so we buffer them and attribute them once
  // the terminating `fail: ... Error(s) encountered while parsing: <file>` line names the file. This avoids
  // misattributing errors when multiple files' "Parsing file: ..." lines are interleaved (e.g. parallel parsing).
  override def skippedFiles(in: Path, runResult: ExternalCommandResult): List[SkippedFile] = {
    val diagnosticsByFile = mutable.LinkedHashMap.empty[String, Seq[String]]
    val pendingReasons    = mutable.ListBuffer.empty[String]

    def addReasons(fileName: String, reasons: Seq[String]): Unit = {
      val relFile = SourceFiles.toRelativePath(fileName, in.toString)
      diagnosticsByFile.updateWith(relFile) {
        case Some(existing) => Some(existing ++ reasons)
        case None           => Some(reasons)
      }
    }

    runResult.stdOut.map(_.strip()).foreach {
      case s"fail: DotNetAstGen.Program[0] Error(s) encountered while parsing: $fileName" =>
        addReasons(fileName, pendingReasons.toList)
        pendingReasons.clear()
      case s"fail: DotNetAstGen.Program[0] $reason" => pendingReasons += reason
      case s"warn: DotNetAstGen.Program[0] $filename does $reason, skipping..." =>
        addReasons(filename, Seq(s"does $reason"))
      case s"info: DotNetAstGen.Program[0] Skipping file: $fileName" => addReasons(fileName, Seq("Skipped"))
      case _                                                         => // ignore
    }

    diagnosticsByFile.map { case (filename, diagnostics) => SkippedFile(filename, diagnostics.mkString("; ")) }.toList
  }

  override def runAstGenNative(in: String, out: Path, exclude: String, include: String): ExternalCommandResult = {
    val excludeCommand = if (exclude.isEmpty) Seq.empty else Seq("-e", exclude)
    ExternalCommand.run(Seq(astGenCommand, "-o", out.toString(), "-i", in) ++ excludeCommand)
  }

}
