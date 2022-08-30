package io.joern.php2cpg.parser

import better.files.File
import io.joern.php2cpg.parser.Domain.PhpFile
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory
import ujson.Value.Value

import java.nio.file.Paths
import scala.util.Properties.isWin
import scala.util.{Failure, Success, Try}

object PhpParser {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val ExecutablePath: String = {
    val dir = Paths.get(PhpParser.getClass.getProtectionDomain.getCodeSource.getLocation.toURI).toAbsolutePath.toString
    val fixedDir   = new java.io.File(dir.substring(0, dir.indexOf("php2cpg"))).toString
    val executable = if (isWin) "php-parse.bat" else "php-parse"
    Paths.get(fixedDir, "php2cpg", "bin", executable).toAbsolutePath.toString
  }

  private def phpParseCommand(filename: String): String = {
    s"$ExecutablePath --with-recovery --resolve-names --json-dump $filename"
  }

  def parseFile(inputPath: String): Option[PhpFile] = {
    val inputFile      = File(inputPath)
    val inputDirectory = inputFile.parent.canonicalPath
    val filename       = inputFile.name
    ExternalCommand.run(phpParseCommand(filename), inputDirectory) match {
      case Success(outputLines) =>
        val jsonString =
          outputLines
            .dropWhile(_.charAt(0) != '[')
            .mkString("\n")

        println(jsonString)

        val jsonValue = ujson.read(jsonString)
        if (jsonValue == null) {
          logger.error(s"ujson returned null value for $filename")
          return None
        }
        Try(Domain.fromJson(jsonValue)) match {
          case Success(phpFile) => Some(phpFile)

          case Failure(e) =>
            logger.error(s"Failed to generate intermediate AST for $inputPath", e)
            None
        }

      case Failure(exception) =>
        logger.error(s"php-parser failed to parse input file $inputPath", exception)
        None
    }
  }

}
