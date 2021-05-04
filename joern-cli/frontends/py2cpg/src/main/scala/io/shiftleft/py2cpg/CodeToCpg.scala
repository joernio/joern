package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.py2cpg.Py2Cpg.InputProvider
import io.shiftleft.pythonparser.PyParser

class CodeToCpg(cpg: Cpg, inputProvider: Iterable[InputProvider], keyPool: IntervalKeyPool)
    extends ParallelCpgPass[InputProvider](
      cpg,
      keyPools = Some(keyPool.split(inputProvider.size))
    ) {

  override def partIterator: Iterator[InputProvider] = inputProvider.iterator

  override def runOnPart(inputProvider: InputProvider): Iterator[DiffGraph] = {
    val inputPair = inputProvider()
    val parser = new PyParser()
    val astRoot = parser.parse(inputPair.content)
    val astVisitor = new PythonAstVisitor(inputPair.file)
    astVisitor.convert(astRoot)

    Iterator.single(astVisitor.getDiffGraph)
  }
}
