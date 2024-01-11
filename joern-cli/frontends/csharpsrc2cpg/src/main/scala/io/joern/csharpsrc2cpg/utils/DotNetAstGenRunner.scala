package io.joern.csharpsrc2cpg.utils

import better.files.File
import com.typesafe.config.ConfigFactory
import io.joern.csharpsrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, getClass}
import io.joern.x2cpg.astgen.AstGenRunnerBase
import io.joern.x2cpg.utils.{Environment, ExternalCommand}
import org.slf4j.LoggerFactory
import versionsort.VersionHelper

import java.nio.file.Paths
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class DotNetAstGenRunner(config: Config) extends AstGenRunnerBase(config) {

  private val logger = LoggerFactory.getLogger(getClass)

  // The x86 variant seems to run well enough on MacOS M-family chips, whereas the ARM build crashes
  override val MacArm: String = MacX86

  override def fileFilter(file: String, out: File): Boolean = {
    file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
      case filePath if isIgnoredByUserConfig(filePath) => false
      case filePath if filePath.endsWith(".csproj")    => false
      case _                                           => true
    }
  }

  override def skippedFiles(in: File, astGenOut: List[String]): List[String] = {
    val diagnosticMap = mutable.LinkedHashMap.empty[String, Seq[String]]
    astGenOut.map(_.strip()).foreach {
      case s"info: DotNetAstGen.Program[0] Parsing file: $fileName" =>
        diagnosticMap.put(SourceFiles.toRelativePath(fileName, in.pathAsString), Nil)
      case s"fail: DotNetAstGen.Program[0] Error(s) encountered while parsing: $_" => // ignore
      case s"fail: DotNetAstGen.Program[0] $reason" =>
        val (lastFile, _) = diagnosticMap.last
        diagnosticMap.updateWith(lastFile) {
          case Some(x) => Option(x :+ reason)
          case None    => Option(reason :: Nil)
        }
      case s"info: DotNetAstGen.Program[0] Skipping file: $fileName" =>
        val reason = "Skipped"
        diagnosticMap.updateWith(SourceFiles.toRelativePath(fileName, in.pathAsString)) {
          case Some(x) => Option(x :+ reason)
          case None    => Option(reason :: Nil)
        }
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

  override def runAstGenNative(in: String, out: File, exclude: String)(implicit
    metaData: AstGenProgramMetaData
  ): Try[Seq[String]] = {
    val excludeCommand = if (exclude.isEmpty) "" else s"-e \"$exclude\""
    ExternalCommand.run(s"$astGenCommand -o ${out.toString()} -i $in $excludeCommand", ".")
  }

}
