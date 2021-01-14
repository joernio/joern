package io.shiftleft.py2cpg

import io.shiftleft.passes.DiffGraph
import org.python.pydev.core.IGrammarVersionProvider
import org.python.pydev.parser.PyParser
import org.python.pydev.parser.jython.SimpleNode

class CodeToCpg(sourceCode: String) {
  def convert(): Iterator[DiffGraph] = {
    if (sourceCode.isEmpty) {
      // For some reason the PyDev parser cannot handle empty input.
      return Iterator.empty
    }
    val astRoot = parseSourceCode()
    val pyDevAstVisitor = new PyDevAstVisitor()
    astRoot.accept(pyDevAstVisitor)

    Iterator(pyDevAstVisitor.getDiffGraph)
  }

  private def parseSourceCode(): SimpleNode = {
    val python3Grammar = PyParser.createGrammar(
      true,
      IGrammarVersionProvider.LATEST_GRAMMAR_PY3_VERSION,
      sourceCode.toCharArray
    )

    val python3AstRoot = python3Grammar.file_input()

    if (python3Grammar.getErrorOnParsing == null) {
      return python3AstRoot
    } else {
      val python2Grammar = PyParser.createGrammar(
        true,
        IGrammarVersionProvider.LATEST_GRAMMAR_PY2_VERSION,
        sourceCode.toCharArray
      )

      val python2AstRoot = python2Grammar.file_input()
      if (python2Grammar.getErrorOnParsing == null) {
        return python2AstRoot
      } else {
        // If we get parse errors with python3 and python2 grammars
        // we rather take the python3 parsing result.
        return python3AstRoot
      }
    }
  }
}
