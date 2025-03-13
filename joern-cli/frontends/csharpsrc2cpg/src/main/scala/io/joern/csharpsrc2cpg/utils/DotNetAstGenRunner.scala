package io.joern.csharpsrc2cpg.utils

import io.joern.csharpsrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, getClass}
import io.joern.x2cpg.astgen.AstGenRunnerBase
import io.shiftleft.semanticcpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.collection.mutable
import scala.util.Try

class DotNetAstGenRunner(config: Config) extends AstGenRunnerBase(config) {

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

  override def skippedFiles(in: Path, astGenOut: List[String]): List[String] = {
    val diagnosticMap = mutable.LinkedHashMap.empty[String, Seq[String]]

    def addReason(reason: String, lastFile: Option[String] = None): Unit = {
      val key = lastFile.orElse(diagnosticMap.lastOption.map(_._1))

      key.foreach { resolvedKey =>
        diagnosticMap.updateWith(resolvedKey) {
          case Some(existingReasons) => Some(existingReasons :+ reason)
          case None                  => Some(List(reason))
        }
      }
    }

    astGenOut.map(_.strip()).foreach {
      case s"info: DotNetAstGen.Program[0] Parsing file: $fileName" =>
        diagnosticMap.put(SourceFiles.toRelativePath(fileName, in.toString), Nil)
      case s"fail: DotNetAstGen.Program[0] Error(s) encountered while parsing: $_" => // ignore
      case s"fail: DotNetAstGen.Program[0] $reason"                                => addReason(reason)
      case s"warn: DotNetAstGen.Program[0] $filename does $reason, skipping..." =>
        addReason(s"does $reason", Option(filename))
      case s"info: DotNetAstGen.Program[0] Skipping file: $fileName" =>
        addReason("Skipped", Option(SourceFiles.toRelativePath(fileName, in.toString)))
      case _ => // ignore
    }

    diagnosticMap.flatMap {
      case (filename, Nil) =>
        logger.debug(s"Successfully parsed '$filename'")
        None
      case (filename, diagnostics) =>
        logger.warn(s"Failed to parse '$filename':\n${diagnostics.map(x => s" - $x").mkString("\n")}")
        Option(filename)
    }.toList
  }

  override def runAstGenNative(in: String, out: Path, exclude: String, include: String)(implicit
    metaData: AstGenProgramMetaData
  ): Try[Seq[String]] = {
    val excludeCommand = if (exclude.isEmpty) Seq.empty else Seq("-e", exclude)
    ExternalCommand.run(Seq(astGenCommand, "-o", out.toString(), "-i", in) ++ excludeCommand, Some(".")).toTry
  }

}
