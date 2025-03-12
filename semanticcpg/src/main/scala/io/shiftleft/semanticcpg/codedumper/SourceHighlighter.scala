package io.shiftleft.semanticcpg.codedumper

import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.utils.{ExternalCommand, FileUtil}
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Files

/** language must be one of io.shiftleft.codepropertygraph.generated.Languages TODO: generate enums instead of Strings
  * for the languages
  */
case class Source(code: String, language: String)

object SourceHighlighter {
  private val logger: Logger = LoggerFactory.getLogger(SourceHighlighter.getClass)

  def highlight(source: Source): Option[String] = {
    val langFlag = source.language match {
      case Languages.C | Languages.NEWC | Languages.GHIDRA => "-sC"
      case Languages.JAVA | Languages.JAVASRC              => "-sJava"
      case Languages.JSSRC | Languages.JAVASCRIPT          => "-sJavascript"
      case other => throw new RuntimeException(s"Attempting to call highlighter on unsupported language: $other")
    }

    val tmpSrcFile = FileUtil.newTemporaryFile("dump")
    Files.writeString(tmpSrcFile, source.code)
    try {
      val highlightedCode = ExternalCommand
        .run(Seq("source-highlight-esc.sh", tmpSrcFile.toString, langFlag))
        .stdOut
        .mkString("\n")
      Some(highlightedCode)
    } catch {
      case exception: Exception =>
        logger.info("syntax highlighting not working. Is `source-highlight` installed?", exception)
        Some(source.code)
    } finally {
      FileUtil.delete(tmpSrcFile)
    }
  }

}
