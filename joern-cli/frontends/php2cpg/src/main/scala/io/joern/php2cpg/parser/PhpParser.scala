package io.joern.php2cpg.parser

import better.files.File
import io.joern.php2cpg.Config
import io.joern.php2cpg.parser.Domain.PhpFile
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.io.Source
import scala.util.{Failure, Success, Try}

class PhpParser private (phpParserPath: String, phpIniPath: String) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private def phpParseCommand(filename: String): String = {
    val phpParserCommands = "--with-recovery --resolve-names --json-dump"
    s"php --php-ini $phpIniPath $phpParserPath $phpParserCommands $filename"
  }

  def parseFile(inputPath: String, phpIniOverride: Option[String]): Option[PhpFile] = {
    val inputFile      = File(inputPath)
    val inputFilePath  = inputFile.canonicalPath
    val inputDirectory = inputFile.parent.canonicalPath

    val command = phpParseCommand(inputFilePath)

    ExternalCommand.runMultiple(command, inputDirectory) match {
      case Success(output) =>
        processParserOutput(output, inputFilePath)

      case Failure(exception) =>
        logger.error(s"Failure running php-parser with $command", exception.getMessage())
        None
    }
  }

  private def processParserOutput(output: String, filename: String): Option[PhpFile] = {
    val maybeJson = linesToJsonValue(output.split(System.lineSeparator()).toIndexedSeq, filename)

    maybeJson.flatMap(jsonValueToPhpFile(_, filename))
  }

  private def linesToJsonValue(lines: Seq[String], filename: String): Option[ujson.Value] = {
    if (lines.exists(_.startsWith("["))) {
      val jsonString = lines.dropWhile(_.charAt(0) != '[').mkString("\n")
      Try(Option(ujson.read(jsonString))) match {
        case Success(Some(value)) => Some(value)

        case Success(None) =>
          logger.error(s"Parsing json string for $filename resulted in null return value")
          None

        case Failure(exception) =>
          logger.error(s"Parsing json string for $filename failed with exception", exception)
          None
      }

    } else {
      logger.warn(s"No JSON output for $filename")
      None
    }
  }

  private def jsonValueToPhpFile(json: ujson.Value, filename: String): Option[PhpFile] = {
    Try(Domain.fromJson(json)) match {
      case Success(phpFile) => Some(phpFile)

      case Failure(e) =>
        logger.error(s"Failed to generate intermediate AST for $filename", e)
        None
    }
  }
}

object PhpParser {
  private val logger = LoggerFactory.getLogger(this.getClass())

  val PhpParserBinEnvVar = "PHP_PARSER_BIN"

  private def defaultPhpIni: String = {
    val iniContents = Source.fromResource("php.ini").getLines().mkString(System.lineSeparator())

    val tmpIni = File.newTemporaryFile(suffix = "-php.ini").deleteOnExit()
    tmpIni.writeText(iniContents)
    tmpIni.canonicalPath
  }

  private def defaultPhpParserBin: String = {
    val dir =
      Paths.get(this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI).toAbsolutePath.toString

    val fixedDir = new java.io.File(dir.substring(0, dir.indexOf("php2cpg"))).toString

    Paths.get(fixedDir, "php2cpg", "bin", "php-parser", "php-parser.php").toAbsolutePath.toString
  }

  private def configOverrideOrDefaultPath(
    identifier: String,
    maybeOverride: Option[String],
    defaultValue: => String
  ): Option[String] = {
    val pathString = maybeOverride match {
      case Some(overridePath) if overridePath.nonEmpty =>
        logger.debug(s"Using override path for $identifier: $overridePath")
        overridePath

      case _ =>
        logger.debug(s"$identifier path not overridden. Using default: $defaultValue")
        defaultValue
    }

    File(pathString) match {
      case file if file.exists() && file.isRegularFile() => Some(file.canonicalPath)

      case _ =>
        logger.error(s"Invalid path for $identifier: $pathString")
        None
    }
  }

  private def maybePhpParserPath(config: Config): Option[String] = {
    val phpParserPathOverride =
      config.phpParserBin
        .orElse(Option(System.getenv(PhpParserBinEnvVar)))

    configOverrideOrDefaultPath("PhpParserBin", phpParserPathOverride, defaultPhpParserBin)
  }

  private def maybePhpIniPath(config: Config): Option[String] = {
    configOverrideOrDefaultPath("PhpIni", config.phpIni, defaultPhpIni)
  }

  def getParser(config: Config): Option[PhpParser] = {
    for (
      phpParserPath <- maybePhpParserPath(config);
      phpIniPath    <- maybePhpIniPath(config)
    )
      yield new PhpParser(phpParserPath, phpIniPath)
  }
}
