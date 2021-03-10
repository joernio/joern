package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool}
import io.shiftleft.semanticcpg.passes.CfgCreationPass

import scala.collection.parallel

object Py2Cpg {
  case class InputPair(content: String, file: String)
  type InputProvider = () => InputPair
}

/** Entry point for general cpg generation from python code.
  * @param inputProviders Set of functions which provide InputPairs.
  *                       The functions must be safe to call from different threads.
  * @param outputCpg Empty target cpg which will be populated.
  */
class Py2Cpg(inputProviders: Iterable[Py2Cpg.InputProvider], outputCpg: Cpg) {
  private val diffGraph = new DiffGraph.Builder()
  private val nodeBuilder = new NodeBuilder(diffGraph)

  def buildCpg(): Unit = {
    val auxKeyPool = new IntervalKeyPool(1, 100000)
    val keyPool = new IntervalKeyPool(100000, Long.MaxValue)

    nodeBuilder.metaNode(Languages.PYTHON, version = "")

    DiffGraph.Applier.applyDiff(diffGraph.build, outputCpg, keyPool = Some(auxKeyPool))

    new CodeToCpg(outputCpg, inputProviders, keyPool).createAndApply()

    // We dont need keys for the CFG pass. Thus such an empty key pool is enough.
    val cfgKeyPool = new IntervalKeyPool(0, 0)
    new CfgCreationPass(outputCpg, cfgKeyPool).createAndApply()
  }
}
