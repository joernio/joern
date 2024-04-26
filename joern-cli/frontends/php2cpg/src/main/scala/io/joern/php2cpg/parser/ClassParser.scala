package io.joern.php2cpg.parser
import better.files.File
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import scala.collection.immutable.LazyList.from
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}
import upickle.default.*

import scala.collection.mutable

/** Parses the high-level symbol information of a project.
  */
class ClassParser(targetDir: File) {

  import ClassParser.*

  private val logger = LoggerFactory.getLogger(this.getClass)

  private lazy val classParserScript = {
    val f = File.newTemporaryFile("ClassParser", ".php").deleteOnExit(swallowIOExceptions = true)
    Using(Source.fromResource("ClassParser.php")) { br =>
      f.writeText(br.getLines().mkString("\n"))
    }
    f
  }

  private lazy val phpClassParseCommand: String = s"php ${classParserScript.pathAsString} ${targetDir.pathAsString}"

  def parse(): Try[List[ClassParserClass]] = Try {
    val inputDirectory = targetDir.parent.canonicalPath
    ExternalCommand.run(phpClassParseCommand, inputDirectory).map(_.reverse) match {
      case Success(output) =>
        read[List[ClassParserClass]](output.mkString("\n"))
      case Failure(exception) =>
        logger.error(s"Failure running `ClassParser.php` with $phpClassParseCommand", exception.getMessage)
        throw exception
    }
  }

}

object ClassParser {

  implicit val classParserClassRw: ReadWriter[ClassParserClass] = readwriter[ujson.Value].bimap[ClassParserClass](
    x => ujson.Null, // no serialization
    json =>
      ClassParserClass(
        name = json("name").str,
        namespace = json("namespace").str,
        relativeFile = json("file").str,
        modifiers = json.obj.get("modifiers").map(read[List[String]](_)).getOrElse(Nil),
        functions = json.obj.get("functions") match {
          case Some(jsonFuncs) =>
            val functionMap = read[mutable.Map[String, ClassParserFunction]](jsonFuncs)
            if (!functionMap.contains("__construct")) {
              functionMap.put("__construct", ClassParserFunction("", "public" :: Nil))
            }
            functionMap.map { case (name, func) =>
              func.copy(name = name)
            }.toList
          case None => Nil
        }
      )
  )

  case class ClassParserClass(
    name: String,
    namespace: String,
    relativeFile: String,
    modifiers: List[String],
    functions: List[ClassParserFunction]
  )

  case class ClassParserFunction(name: String = "", modifiers: List[String]) derives ReadWriter

}
