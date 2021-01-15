package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool}

object Py2Cpg {
  case class InputPair(file: String, content: String)
  type InputProvider = () => InputPair
}

class Py2Cpg(inputProviders: Iterable[Py2Cpg.InputProvider],
             outputCpg: Cpg) {

  def buildCpg(): Unit = {
    val keyPool = new IntervalKeyPool(1, Long.MaxValue)

    inputProviders.foreach { inputProviders =>
      val inputPair = inputProviders()

      val diffGraphs = new CodeToCpg(inputPair.content).convert()
      diffGraphs.foreach { diffGraph =>
        DiffGraph.Applier.applyDiff(diffGraph, outputCpg, keyPool = Some(keyPool))
      }
    }
  }
}
