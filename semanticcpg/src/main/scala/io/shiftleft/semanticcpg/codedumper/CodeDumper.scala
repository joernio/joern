package io.shiftleft.semanticcpg.codedumper

import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.codepropertygraph.generated.nodes.{Expression, Local, Method, NewLocation}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.utils.IOUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

object CodeDumper {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def arrow(locationFullName: Option[String] = None): CharSequence = s"/* <=== ${locationFullName.getOrElse("")} */ "

  private val supportedLanguages =
    Set(
      Languages.C,
      Languages.NEWC,
      Languages.GHIDRA,
      Languages.JAVA,
      Languages.JAVASRC,
      Languages.JSSRC,
      Languages.SWIFTSRC
    )

  private def toAbsolutePath(path: String, rootPath: String): String = {
    val absolutePath = Paths.get(path) match {
      case p if p.isAbsolute            => p
      case _ if rootPath.endsWith(path) => Paths.get(rootPath)
      case p                            => Paths.get(rootPath, p.toString)
    }
    absolutePath.normalize().toString
  }

  /** Dump string representation of code at given `location`.
    */
  def dump(
    location: NewLocation,
    language: Option[String],
    rootPath: Option[String],
    highlight: Boolean,
    withArrow: Boolean = true
  ): String = {
    (location.node, language) match {
      case (None, _) =>
        logger.warn("Empty `location.node` encountered")
        ""
      case (_, None) =>
        logger.info("dump not supported; language not set in CPG")
        ""
      case (_, Some(lang)) if !supportedLanguages.contains(lang) =>
        logger.info(s"dump not supported for language '$lang'")
        ""
      case (Some(node), Some(lang)) =>
        val method: Option[Method] = node match {
          case n: Method     => Some(n)
          case n: Expression => Some(n.method)
          case n: Local      => n.method.headOption
          case _             => None
        }
        method
          .collect {
            case m: Method if m.lineNumber.isDefined && m.lineNumberEnd.isDefined =>
              val rawCode = if (lang == Languages.GHIDRA || lang == Languages.JAVA) {
                val lines = m.code.split("\n")
                lines.zipWithIndex
                  .map { case (line, lineNo) =>
                    if (lineNo == 0 && withArrow) {
                      s"$line ${arrow(Option(m.fullName))}"
                    } else
                      line
                  }
                  .mkString("\n")
              } else {
                val filename = rootPath.map(toAbsolutePath(location.filename, _)).getOrElse(location.filename)
                code(filename, m.lineNumber.get, m.lineNumberEnd.get, location.lineNumber, Option(m.fullName))
              }
              if (highlight) {
                SourceHighlighter.highlight(Source(rawCode, lang))
              } else {
                Some(rawCode)
              }
          }
          .flatten
          .getOrElse("")
    }
  }

  /** For a given `filename`, `startLine`, and `endLine`, return the corresponding code by reading it from the file. If
    * `lineToHighlight` is defined, then a line containing an arrow (as a source code comment) is included right before
    * that line.
    */
  def code(
    filename: String,
    startLine: Integer,
    endLine: Integer,
    lineToHighlight: Option[Integer] = None,
    locationFullName: Option[String] = None
  ): String = {
    Try(IOUtils.readLinesInFile(Paths.get(filename))) match {
      case Failure(exception) =>
        logger.warn(s"error reading from: '$filename'", exception)
        ""
      case Success(lines) =>
        lines
          .slice(startLine - 1, endLine)
          .zipWithIndex
          .map { case (line, lineNo) =>
            if (lineToHighlight.isDefined && lineNo == lineToHighlight.get - startLine) {
              s"$line ${arrow(locationFullName)}"
            } else {
              line
            }
          }
          .mkString("\n")
    }

  }

}
