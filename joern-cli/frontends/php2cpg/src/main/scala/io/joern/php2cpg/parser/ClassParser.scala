package io.joern.php2cpg.parser
import io.shiftleft.semanticcpg.utils.{ExternalCommand, FileUtil}
import FileUtil.*
import org.slf4j.LoggerFactory

import scala.io.Source
import scala.util.{Failure, Success, Try, Using}
import java.nio.file.{Files, Path}
import upickle.default.*

import scala.collection.mutable

/** Parses the high-level symbol information of a project.
  */
class ClassParser(targetDir: Path) {

  import ClassParser.*

  private val logger = LoggerFactory.getLogger(this.getClass)

  def parse(): Try[List[ClassParserClass]] = {
    val inputDirectory = targetDir.getParent.toAbsolutePath

    FileUtil.usingTemporaryFile("ClassParser", ".php") { scriptFile =>
      Using(Source.fromResource("ClassParser.php")) { br =>
        Files.writeString(scriptFile, br.getLines().mkString("\n"))
      }

      ExternalCommand
        .run(Seq("php", scriptFile.toString, targetDir.toString), Option(inputDirectory))
        .toTry
        .map(_.reverse) match {
        case Success(output) =>
          Try(read[List[ClassParserClass]](output.mkString("\n")))
        case Failure(exception) =>
          logger.error(s"Failure running `ClassParser.php` with ${scriptFile.toString}", exception.getMessage)
          Failure(exception)
      }
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
