package io.joern.php2cpg.parser

import better.files.File
import io.joern.php2cpg.parser.Domain.PhpFile
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.io.Source
import scala.util.{Failure, Success, Try}

object PhpParser {

  private val PhpParserBinEnvVar = "PHP_PARSER_BIN"
  private val logger             = LoggerFactory.getLogger(this.getClass)

  private val ExecutablePath: String = {
    Option(System.getenv(PhpParserBinEnvVar)) match {
      case Some(phpParserPath) if phpParserPath.nonEmpty =>
        logger.debug(s"Using php-parser path from $PhpParserBinEnvVar envvar: ${phpParserPath}")
        phpParserPath

      case _ =>
        val dir =
          Paths.get(PhpParser.getClass.getProtectionDomain.getCodeSource.getLocation.toURI).toAbsolutePath.toString
        val fixedDir = new java.io.File(dir.substring(0, dir.indexOf("php2cpg"))).toString
        val phpParserPath =
          Paths.get(fixedDir, "php2cpg", "bin", "php-parser.phar").toAbsolutePath.toString
        logger.debug(s"$PhpParserBinEnvVar not set. Using default php-parser location: ${phpParserPath}")
        phpParserPath
    }
  }

  private lazy val DefaultPhpIni: String = {
    val iniContents = Source.fromResource("php.ini").getLines.mkString(System.lineSeparator())

    val tmpIni = File.newTemporaryFile(suffix = "-php.ini").deleteOnExit()
    tmpIni.writeText(iniContents)
    tmpIni.canonicalPath
  }

  private def phpParseCommand(filename: String, phpIniPath: String): String = {
    s"php --php-ini $phpIniPath $ExecutablePath --with-recovery --resolve-names --json-dump $filename"
  }

  private def getPhpIniPath(phpIniOverride: Option[String]): String = {
    phpIniOverride match {
      case None =>
        logger.debug(s"No php.ini override path provided. Using default instead.")
        DefaultPhpIni

      case Some(path) =>
        val overrideFile = File(path)
        val overridePath = overrideFile.path.toAbsolutePath.toString

        if (overrideFile.exists && overrideFile.isRegularFile) {
          logger.debug(s"Found custom php.ini to be used at $overridePath")
          overridePath
        } else {
          logger.warn(s"Could not find php.ini file at $overridePath. Using default instead.")
          DefaultPhpIni
        }
    }
  }

  def parseFile(inputPath: String, phpIniOverride: Option[String]): Option[PhpFile] = {
    val inputFile      = File(inputPath)
    val inputFilePath  = inputFile.canonicalPath
    val inputDirectory = inputFile.parent.canonicalPath
    val phpIniPath     = getPhpIniPath(phpIniOverride)

    ExternalCommand.run(phpParseCommand(inputFilePath, phpIniPath), inputDirectory, separateStdErr = true) match {
      case Success(outputLines) => processParserOutput(outputLines, inputFilePath)

      case Failure(exception) =>
        logger.error(s"php-parser failed to parse input file $inputPath", exception)
        None
    }
  }

  private def processParserOutput(lines: Seq[String], filename: String): Option[PhpFile] = {
    val maybeJson = linesToJsonValue(lines, filename)

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
