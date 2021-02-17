package io.shiftleft.py2cpg

import io.shiftleft.passes.DiffGraph
import io.shiftleft.pythonparser.PyParser
import io.shiftleft.pythonparser.ast.iast

class CodeToCpg(sourceCode: String) {
  def convert(): Iterator[DiffGraph] = {
    if (sourceCode.isEmpty) {
      // For some reason the PyDev parser cannot handle empty input.
      return Iterator.empty
    }
    val astRoot = parseSourceCode()
    val pyDevAstVisitor = new PythonAstVisitor()
    astRoot.accept(pyDevAstVisitor)

    Iterator(pyDevAstVisitor.getDiffGraph)
  }

  private def parseSourceCode(): iast = {
    val parser = new PyParser()
    parser.parse(sourceCode)
  }
}
