package io.joern.php2cpg.parser

import better.files.File
import io.joern.php2cpg.parser.Domain.PhpFile
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory
import ujson.Value.Value

import java.nio.file.Paths
import scala.util.Properties.isWin
import scala.util.{Failure, Random, Success, Try}

object PhpParser {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val ExecutablePath: String = {
    val dir = Paths.get(PhpParser.getClass.getProtectionDomain.getCodeSource.getLocation.toURI).toAbsolutePath.toString
    val fixedDir = new java.io.File(dir.substring(0, dir.indexOf("php2cpg"))).toString
    Paths.get(fixedDir, "php2cpg", "bin", "vendor", "bin", "php-parse").toAbsolutePath.toString
  }

  private def phpParseCommand(filename: String): String = {
    s"$ExecutablePath --with-recovery --resolve-names --json-dump $filename"
  }

  // TODO Don't merge this
  val random = new Random()
  def getRandomElement[A](seq: Seq[A]): A =
    seq(random.nextInt(seq.length))

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

        val colors = List(
          Console.BLUE,
          Console.BLACK,
          Console.GREEN,
          Console.CYAN,
          Console.YELLOW,
          Console.MAGENTA,
        )
        println(getRandomElement(colors), filename, jsonString, Console.BLACK)

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
