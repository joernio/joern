package io.joern.php2cpg.parser
import io.shiftleft.semanticcpg.utils.{ExternalCommand, FileUtil}
import FileUtil.*
import org.slf4j.LoggerFactory

import scala.io.Source
import scala.util.{Failure, Success, Try, Using}
import java.nio.file.{Files, Path}
import upickle.default.*

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

      val result = ExternalCommand.run(Seq("php", scriptFile.toString, targetDir.toString), Option(inputDirectory))
      result.toTry.map(_.reverse) match {
        case Success(output) =>
          logDiagnostics(result.stdErr)
          Try(read[List[ClassParserClass]](output.mkString("\n")))
        case Failure(exception) =>
          logger.error(s"Failure running `ClassParser.php`: ${exception.getMessage}")
          Failure(exception)
      }
    }
  }

  /** Forwards the script's stderr (skipped files, excluded directories, the final summary) to the log. The script caps
    * the number of lines, so this stays short even for large dependency trees.
    */
  private def logDiagnostics(stdErr: Seq[String]): Unit = {
    stdErr.filter(_.nonEmpty).foreach(line => logger.info(s"ClassParser.php: $line"))
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
        kind = json.obj.get("kind").map(_.str).getOrElse("class"),
        functions = json.obj.get("functions") match {
          case Some(jsonFuncs) =>
            // ujson objects keep insertion order, so the order of the script output reaches the CPG unchanged.
            val declared = jsonFuncs.obj.toList.map { case (name, func) =>
              ClassParserFunction(name, func.obj.get("modifiers").map(read[List[String]](_)).getOrElse(Nil))
            }
            val kind = json.obj.get("kind").map(_.str).getOrElse("class")
            if (kind == "class" && !declared.exists(_.name == "__construct")) {
              declared :+ ClassParserFunction("__construct", "public" :: Nil)
            } else declared
          case None => Nil
        }
      )
  )

  case class ClassParserClass(
    name: String,
    namespace: String,
    relativeFile: String,
    modifiers: List[String],
    kind: String,
    functions: List[ClassParserFunction]
  ) {

    /** The full name of the type, without a leading backslash for the global namespace. */
    def fullName: String = if (namespace.isEmpty) name else s"$namespace\\$name"
  }

  case class ClassParserFunction(name: String = "", modifiers: List[String]) derives ReadWriter

}
