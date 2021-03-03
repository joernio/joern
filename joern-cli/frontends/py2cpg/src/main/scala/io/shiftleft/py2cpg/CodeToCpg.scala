package io.shiftleft.py2cpg

import io.shiftleft.passes.DiffGraph
import io.shiftleft.pythonparser.PyParser
import io.shiftleft.pythonparser.ast.iast
import io.shiftleft.py2cpg.Py2Cpg.InputPair

class CodeToCpg(inputPair: InputPair) {
  def convert(): Iterator[DiffGraph] = {
    val astRoot = parseSourceCode()
    val astVisitor = new PythonAstVisitor(inputPair.file)
    astVisitor.convert(astRoot)

    Iterator(astVisitor.getDiffGraph)
  }

  private def parseSourceCode(): iast = {
    val parser = new PyParser()
    parser.parse(inputPair.content)
  }
}
