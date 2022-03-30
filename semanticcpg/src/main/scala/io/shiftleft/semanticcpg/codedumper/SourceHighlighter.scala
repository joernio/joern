package io.shiftleft.semanticcpg.codedumper

import better.files.File
import io.shiftleft.codepropertygraph.generated.Languages
import org.slf4j.{Logger, LoggerFactory}

import scala.sys.process.Process

/** language must be one of io.shiftleft.codepropertygraph.generated.Languages TODO: generate enums instead of Strings
  * for the languages
  */
case class Source(code: String, language: String)

object SourceHighlighter {
  private val logger: Logger = LoggerFactory.getLogger(SourceHighlighter.getClass)

  def highlight(source: Source): Option[String] = {
    val langFlag = source.language match {
      case Languages.C | Languages.NEWC | Languages.GHIDRA => "-sC"
      case Languages.JAVASRC                               => "-sJava"
      case other => throw new RuntimeException(s"Attempting to call highlighter on unsupported language: $other")
    }

    val tmpSrcFile = File.newTemporaryFile("dump")
    tmpSrcFile.writeText(source.code)
    try {
      val highlightedCode = Process(Seq("source-highlight-esc.sh", tmpSrcFile.path.toString, langFlag)).!!
      Some(highlightedCode)
    } catch {
      case exception: Exception =>
        logger.info("syntax highlighting not working. Is `source-highlight` installed?", exception)
        Some(source.code)
    } finally {
      tmpSrcFile.delete()
    }
  }

}
