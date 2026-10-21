package io.joern.pythonparser

import io.joern.pythonparser.PythonParser
import io.joern.pythonparser.PythonParserConstants
import io.joern.pythonparser.ast.{ErrorStatement, iast}

import java.io.{Reader, StringReader}
import scala.jdk.CollectionConverters.*

class PyParser {
  private var pythonParser: PythonParser = scala.compiletime.uninitialized

  def parse(code: String): iast = {
    parse(StringReader(code))
  }

  def parse(inputReader: Reader): iast = {
    pythonParser = new PythonParser(new CharStreamImpl(inputReader))
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
