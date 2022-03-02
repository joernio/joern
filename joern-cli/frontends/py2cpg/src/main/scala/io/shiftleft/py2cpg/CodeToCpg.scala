package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import io.shiftleft.py2cpg.Py2Cpg.InputProvider
import io.shiftleft.pythonparser.PyParser
import org.slf4j.LoggerFactory

class CodeToCpg(cpg: Cpg, inputProvider: Iterable[InputProvider], keyPool: IntervalKeyPool)
    extends ParallelCpgPass[InputProvider](cpg, keyPools = Some(keyPool.split(inputProvider.size))) {
  import CodeToCpg.logger

  override def partIterator: Iterator[InputProvider] = inputProvider.iterator

  override def runOnPart(inputProvider: InputProvider): Iterator[DiffGraph] = {
    val inputPair = inputProvider()
    try {
      val parser     = new PyParser()
      val astRoot    = parser.parse(inputPair.content)
      val astVisitor = new PythonAstVisitor(inputPair.file, PythonV2AndV3)
      astVisitor.convert(astRoot)

      Iterator.single(astVisitor.getDiffGraph)
    } catch {
      case exception: Throwable =>
        logger.warn(s"Failed to convert file ${inputPair.file}", exception)
        Iterator.empty
    }
  }
}

object CodeToCpg {
  private val logger = LoggerFactory.getLogger(getClass)
}
