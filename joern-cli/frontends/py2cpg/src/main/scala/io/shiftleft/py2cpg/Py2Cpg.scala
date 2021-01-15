package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool}

object Py2Cpg {
  case class InputPair(content: String, file: String)
  type InputProvider = () => InputPair
}

class Py2Cpg(inputProviders: Iterable[Py2Cpg.InputProvider],
             outputCpg: Cpg) {
  private val diffGraph = new DiffGraph.Builder()
  private val nodeBuilder = new NodeBuilder(diffGraph)

  def buildCpg(): Unit = {
    val keyPool = new IntervalKeyPool(1, Long.MaxValue)

    nodeBuilder.metaNode(Languages.PYTHON, version = "")


    DiffGraph.Applier.applyDiff(diffGraph.build, outputCpg, keyPool = Some(keyPool))


    inputProviders.foreach { inputProviders =>
      val inputPair = inputProviders()

      val diffGraphs = new CodeToCpg(inputPair.content).convert()
      diffGraphs.foreach { diffGraph =>
        DiffGraph.Applier.applyDiff(diffGraph, outputCpg, keyPool = Some(keyPool))
      }
    }
  }
}
