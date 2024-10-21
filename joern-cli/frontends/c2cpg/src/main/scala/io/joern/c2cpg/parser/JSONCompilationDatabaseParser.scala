package io.joern.c2cpg.parser

import io.joern.x2cpg.SourceFiles
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import ujson.Value

import java.nio.file.Paths
import scala.util.Try

object JSONCompilationDatabaseParser {

  private val logger = LoggerFactory.getLogger(getClass)

  /** {{{
    * 1) -D: Matches the -D flag, which is the key prefix for defining macros.
    * 2) ([A-Za-z_][A-Za-z0-9_]+): Matches a valid macro name (which must start with a letter or underscore and can be followed by letters, numbers, or underscores).
    * 3) (=(\\*".*"))?: Optionally matches = followed by either:
    *  a) A quoted string: Allows for strings in quotes.
    *  b) Any char sequence (.*") closed with a quote.
    * }}}
    */
  private val defineInCommandPattern = """-D([A-Za-z_][A-Za-z0-9_]+)(=(\\*".*"))?""".r

  /** {{{
    * 1) -I: Matches the -I flag, which indicates an include directory.
    * 2) (\S+): Matches one or more non-whitespace characters, which represent the path of the directory.
    * }}}
    */
  private val includeInCommandPattern = """-I(\S+)""".r

  case class CommandObject(directory: String, arguments: List[String], command: List[String], file: String) {

    /** @return
      *   the file path (guaranteed to be absolute)
      */
    def compiledFile(): String = SourceFiles.toAbsolutePath(file, directory)

    private def nameValuePairFromDefine(define: String): (String, String) = {
      val s = define.stripPrefix("-D")
      if (s.contains("=")) {
        val split = s.split("=")
        (split.head, split(1))
      } else {
        (s, "")
      }
    }

    private def pathFromInclude(include: String): String = include.stripPrefix("-I")

    def includes(): List[String] = {
      val includesFromArguments = arguments.filter(a => a.startsWith("-I")).map(pathFromInclude)
      val includesFromCommand = command.flatMap { c =>
        val includes = includeInCommandPattern.findAllIn(c).toList
        includes.map(pathFromInclude)
      }
      includesFromArguments ++ includesFromCommand
    }

    def defines(): List[(String, String)] = {
      val definesFromArguments = arguments.filter(a => a.startsWith("-D")).map(nameValuePairFromDefine)
      val definesFromCommand = command.flatMap { c =>
        val defines = defineInCommandPattern.findAllIn(c).toList
        defines.map(nameValuePairFromDefine)
      }
      definesFromArguments ++ definesFromCommand
    }
  }

  private def hasKey(node: Value, key: String): Boolean = Try(node(key)).isSuccess

  private def safeArguments(obj: Value): List[String] = {
    if (hasKey(obj, "arguments")) obj("arguments").arrOpt.map(_.toList.map(_.str)).getOrElse(List.empty)
    else List.empty
  }

  private def safeCommand(obj: Value): List[String] = {
    if (hasKey(obj, "command")) List(obj("command").str)
    else List.empty
  }

  def parse(compileCommandsJson: String): List[CommandObject] = {
    try {
      val jsonContent       = IOUtils.readEntireFile(Paths.get(compileCommandsJson))
      val json              = ujson.read(jsonContent)
      val allCommandObjects = json.arr.toList
      allCommandObjects.map { obj =>
        CommandObject(obj("directory").str, safeArguments(obj), safeCommand(obj), obj("file").str)
      }
    } catch {
      case t: Throwable =>
        logger.warn(s"Could not parse '$compileCommandsJson'", t)
        List.empty
    }
  }

}
