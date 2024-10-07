package io.joern.rubysrc2cpg.parser

import better.files.File
import io.joern.rubysrc2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.astgen.AstGenRunner.AstGenProgramMetaData
import io.joern.x2cpg.astgen.AstGenRunnerBase
import io.joern.x2cpg.utils.{Environment, ExternalCommand}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.util.Try

class RubyAstGenRunner(config: Config) extends AstGenRunnerBase(config) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def fileFilter(file: String, out: File): Boolean = {
    file.stripSuffix(".json").replace(out.pathAsString, config.inputPath) match {
      case filePath if isIgnoredByUserConfig(filePath) => false
      case filePath if filePath.endsWith(".csproj")    => false
      case _                                           => true
    }
  }

  override def skippedFiles(in: File, astGenOut: List[String]): List[String] = {
    val diagnosticMap = mutable.LinkedHashMap.empty[String, Seq[String]]

    def addReason(reason: String, lastFile: Option[String] = None) = {
      val key = lastFile.getOrElse(diagnosticMap.last._1)
      diagnosticMap.updateWith(key) {
        case Some(x) => Option(x :+ reason)
        case None    => Option(reason :: Nil)
      }
    }

    astGenOut.map(_.strip()).foreach {
      case s"W, [$_]  WARN -- : $reason - $fileName"   => addReason(reason, Option(fileName))
      case s"E, [$_] ERROR -- : '$fileName' - $reason" => addReason(reason, Option(fileName))
      case s"E, [$_] ERROR -- : Failed to parse $fileName: $reason" =>
        addReason(s"Failed to parse: $reason", Option(fileName))
      case s"I, [$_]  INFO -- : Processed: $fileName -> $_" => diagnosticMap.put(fileName, Nil)
      case s"I, [$_]  INFO -- : Excluding: $fileName"       => addReason("Skipped", Option(fileName))
      case _                                                => // ignore
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

  override def executableName(x86Suffix: String, armSuffix: String)(implicit
    metaData: AstGenProgramMetaData
  ): String = {
    s"ruby_ast_gen/exe/${metaData.name}"
  }

  override def runAstGenNative(in: String, out: File, exclude: String, include: String)(implicit
    metaData: AstGenProgramMetaData
  ): Try[Seq[String]] = {
    val excludeCommand = if (exclude.isEmpty) "" else s"-e \"$exclude\""
    ExternalCommand.run(s"$astGenCommand --log info -o ${out.toString()} -i \"$in\" $excludeCommand", ".")
  }

}
