package io.joern.php2cpg.parser
import better.files.File
import org.slf4j.LoggerFactory

import scala.collection.immutable.LazyList.from
import scala.io.Source
import scala.util.Using
import upickle.default.*

/** Parses the high-level symbol information of a project.
  */
class ClassParser(targetDir: File) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private lazy val classParserScript = {
    val classLoader = getClass.getClassLoader
    val f           = File.newTemporaryFile("ClassParser", ".php").deleteOnExit(swallowIOExceptions = true)
    Using(Source.fromResource("ClassParser.php").bufferedReader()) { br =>
      LazyList.continually(br.readLine()).takeWhile(_ != null).foreach(x => f.writeText(s"$x\n"))
    }
    f
  }

  private lazy val phpClassParseCommand: String = s"php ${classParserScript.pathAsString} ${targetDir.pathAsString}"

  def parse(): Try[List[ClassParserClass]] = Try {
    val inputDirectory = targetDir.parent.canonicalPath
    ExternalCommand.run(phpClassParseCommand, inputDirectory) match {
      case Success(output) =>
        read[List[ClassParserClass]](output)
      case Failure(exception) =>
        logger.error(s"Failure running `ClassParser.php` with $phpClassParseCommand", exception.getMessage)
        None
    }
  }

  implicit val classParserClassRw: ReadWriter[ClassParserClass] = readwriter[ujson.Value].bimap[ClassParserClass](
    x => ujson.Null, // no serialization
    json =>
      ClassParserClass(
        name = json("name").str,
        file = File(json("filename").str),
        modifiers = read[List[String]](json("modifiers")),
        functions = read[Map[String, ClassParserFunction]](json("functions")).map { case (name, func) =>
          func.copy(name = name)
        }.toList
      )
  )

  case class ClassParserClass(name: String, file: File, modifiers: List[String], functions: List[ClassParserFunction])

  case class ClassParserFunction(name: String = "", modifiers: List[String]) derives ReadWriter

}
