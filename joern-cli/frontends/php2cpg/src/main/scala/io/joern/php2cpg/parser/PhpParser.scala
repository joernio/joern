package io.joern.php2cpg.parser

import io.joern.php2cpg.Config
import io.joern.php2cpg.parser.Domain.PhpFile
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.{ExternalCommand, ExternalCommandResult, FileUtil}
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path, Paths}
import java.util.regex.Pattern
import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try}

private[php2cpg] case class PhpParseResult(fileName: String, parseResult: Option[PhpFile], infoLines: String)

class PhpParser private (phpParserPath: String, phpIniPath: String, disableFileContent: Boolean) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private def phpParseCommand(filenames: collection.Seq[String]): Seq[String] = {
    val phpParserCommands = Seq("--with-recovery", "--resolve-names", "--json-dump")
    Seq("php", "--php-ini", phpIniPath, phpParserPath) ++ phpParserCommands ++ filenames
  }

  def parseFiles(inputPaths: collection.Seq[String]): collection.Seq[PhpParseResult] = {
    // We need to keep a map between the input path and its canonical representation in
    // order to map back the canonical file name we get from the php parser.
    // Otherwise later on file name/path processing might get confused because the returned
    // file paths are in no relation to the input paths.
    val canonicalToInputPath = mutable.HashMap.empty[String, String]

    inputPaths.foreach { inputPath =>
      val canonicalPath = Path.of(inputPath).toFile.getCanonicalPath
      canonicalToInputPath.put(canonicalPath, inputPath)
    }

    val command = phpParseCommand(inputPaths)

    ExternalCommand
      .run(command, mergeStdErrInStdOut = true)
      .logIfFailed()
      .successOption
      .map { stdOut =>
        val asJson = linesToJsonValues(stdOut)
        val asPhpFile = asJson.map { case (filename, jsonObjectOption, infoLines) =>
          (filename, jsonToPhpFile(jsonObjectOption, filename), infoLines)
        }
        val withRemappedFileName = asPhpFile.map { case (filename, phpFileOption, infoLines) =>
          PhpParseResult(canonicalToInputPath.apply(filename), phpFileOption, infoLines)
        }
        withRemappedFileName
      }
      .getOrElse(Nil)
  }

  private def jsonToPhpFile(jsonObject: Option[ujson.Value], filename: String): Option[PhpFile] = {
    val phpFile = jsonObject.flatMap { jsonObject =>
      Try(Domain.fromJson(jsonObject)) match {
        case Success(phpFile) =>
          Some(phpFile)
        case Failure(e) =>
          logger.error(s"Failed to generate intermediate AST for $filename", e)
          None
      }
    }
    phpFile
  }

  enum PARSE_MODE {
    case PARSE_INFO, PARSE_JSON, SKIP_TRAILER, SKIP_WARNING
  }

  private def getJsonResult(
    filename: String,
    jsonLines: Array[String],
    infoLines: Array[String]
  ): collection.Seq[(String, Option[ujson.Value], String)] = {
    val result = mutable.ArrayBuffer.empty[(String, Option[ujson.Value], String)]

    val jsonString = jsonLines.mkString

    Try(Option(ujson.read(jsonString))) match {
      case Success(option) =>
        result.append((filename, option, infoLines.mkString))
        if (option.isEmpty) {
          logger.error(s"Parsing json string for $filename resulted in null return value")
        }
      case Failure(exception) =>
        result.append((filename, None, infoLines.mkString))
        logger.error(s"Parsing json string for $filename failed with exception", exception)
    }

    result
  }

  private def logWarning(lines: collection.Seq[String]): Unit = {
    if (lines.exists(_.nonEmpty)) {
      logger.warn(s"Found warning in PHP-Parser JSON output:\n${lines.mkString("\n")}")
    }
  }

  private def linesToJsonValues(
    lines: collection.Seq[String]
  ): collection.Seq[(String, Option[ujson.Value], String)] = {
    val filePrefix    = "====> File "
    val filenameRegex = Pattern.compile(s"$filePrefix(.*):")
    val result        = mutable.ArrayBuffer.empty[(String, Option[ujson.Value], String)]

    var filename     = ""
    val infoLines    = mutable.ArrayBuffer.empty[String]
    val jsonLines    = mutable.ArrayBuffer.empty[String]
    val warningLines = mutable.ArrayBuffer.empty[String]

    var mode    = PARSE_MODE.SKIP_TRAILER
    val linesIt = lines.iterator
    while (linesIt.hasNext) {
      val line = linesIt.next
      mode match {
        case PARSE_MODE.PARSE_INFO =>
          if (line != "==> JSON dump:") {
            infoLines.append(line)
          } else {
            mode = PARSE_MODE.SKIP_WARNING
          }
        case PARSE_MODE.SKIP_WARNING =>
          if (line == "[]") {
            logWarning(warningLines)
            jsonLines.append(line)
            result.appendAll(getJsonResult(filename, jsonLines.toArray, infoLines.toArray))
            mode = PARSE_MODE.SKIP_TRAILER
          } else if (line.startsWith("[")) {
            logWarning(warningLines)
            jsonLines.append(line)
            mode = PARSE_MODE.PARSE_JSON
          } else {
            warningLines.append(line)
          }
        case PARSE_MODE.PARSE_JSON =>
          jsonLines.append(line)
          if (line.startsWith("]") || line == "[]") {
            result.appendAll(getJsonResult(filename, jsonLines.toArray, infoLines.toArray))
            mode = PARSE_MODE.SKIP_TRAILER
          }
        case _ =>
      }

      if (line.startsWith(filePrefix)) {
        val matcher = filenameRegex.matcher(line)
        if (matcher.find()) {
          filename = matcher.group(1)
          infoLines.clear()
          jsonLines.clear()
          mode = PARSE_MODE.PARSE_INFO
        }
      }
    }
    result
  }
}

object PhpParser {
  private val logger             = LoggerFactory.getLogger(this.getClass)
  private val PhpParserBinEnvVar = "PHP_PARSER_BIN"

  private def createDefaultPhpIni(): Path = {
    val iniContents = Source.fromResource("php.ini").getLines().mkString(System.lineSeparator())
    val tmpIni      = FileUtil.newTemporaryFile(suffix = "-php.ini")
    Files.writeString(tmpIni, iniContents)
    tmpIni
  }

  private def defaultPhpParserBin: String = {
    val packagePath = Paths.get(this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
    ExternalCommand
      .executableDir(packagePath)
      .resolve("php-parser/php-parser.php")
      .toString
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

    Paths.get(pathString) match {
      case file if Files.exists(file) && Files.isRegularFile(file) => Some(file.absolutePathAsString)
      case _ =>
        logger.error(s"Invalid path for $identifier: $pathString")
        None
    }
  }

  private def maybePhpParserPath(config: Config): Option[String] = {
    val phpParserPathOverride = config.phpParserBin.orElse(Option(System.getenv(PhpParserBinEnvVar)))
    configOverrideOrDefaultPath("PhpParserBin", phpParserPathOverride, defaultPhpParserBin)
  }

  private def getParser(config: Config, phpIniPath: String): Option[PhpParser] = {
    maybePhpParserPath(config).map(phpParserPath => new PhpParser(phpParserPath, phpIniPath, config.disableFileContent))
  }

  private def validateIniPath(userProvidedIni: String): Option[String] = {
    Paths.get(userProvidedIni) match {
      case file if Files.exists(file) && Files.isRegularFile(file) => Some(file.absolutePathAsString)
      case _ =>
        logger.error(s"Invalid path for PhpIni: $userProvidedIni")
        None
    }
  }

  def withParser[T](config: Config)(f: PhpParser => T): Option[T] = {
    config.phpIni match {
      case Some(userProvidedIni) if userProvidedIni.nonEmpty =>
        validateIniPath(userProvidedIni).flatMap(validatedPath => getParser(config, validatedPath).map(f))
      case _ =>
        Try(FileUtil.usingTemporaryFile(suffix = "-php.ini") { tmpIni =>
          val iniContents = Source.fromResource("php.ini").getLines().mkString(System.lineSeparator())
          Files.writeString(tmpIni, iniContents)
          getParser(config, tmpIni.absolutePathAsString).map(f)
        }).toOption.flatten
    }
  }
}
