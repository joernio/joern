package io.shiftleft.pythonparser

import io.shiftleft.pythonparser.PythonParser
import io.shiftleft.pythonparser.PythonParserConstants
import io.shiftleft.pythonparser.ast.{ErrorStatement, iast}

import java.io.{BufferedReader, ByteArrayInputStream, InputStream, Reader}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._

class PyParser {
  private var pythonParser: PythonParser = _

  def parse(code: String): iast = {
    parse(new ByteArrayInputStream(code.getBytes(StandardCharsets.UTF_8)))
  }

  def parse(inputStream: InputStream): iast = {
    pythonParser = new PythonParser(inputStream)
    // We start in INDENT_CHECK lexer state because we want to detect indentations
    // also for the first line.
    pythonParser.token_source.SwitchTo(PythonParserConstants.INDENT_CHECK)
    val module = pythonParser.module()
    module
  }

  def errors: Iterable[ErrorStatement] = {
    pythonParser.getErrors.asScala
  }
}
