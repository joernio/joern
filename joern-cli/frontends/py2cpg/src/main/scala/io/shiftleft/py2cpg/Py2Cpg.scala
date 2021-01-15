package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool}
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
    val keyPool = new IntervalKeyPool(1, Long.MaxValue)

    nodeBuilder.metaNode(Languages.PYTHON, version = "")

    DiffGraph.Applier.applyDiff(diffGraph.build, outputCpg, keyPool = Some(keyPool))

    inputProviders.to(parallel.ParIterable).foreach { inputProviders =>
      val inputPair = inputProviders()

      val diffGraphs = new CodeToCpg(inputPair.content).convert()
      diffGraphs.foreach { diffGraph =>
        DiffGraph.Applier.applyDiff(diffGraph, outputCpg, keyPool = Some(keyPool))
      }
    }
  }
}
