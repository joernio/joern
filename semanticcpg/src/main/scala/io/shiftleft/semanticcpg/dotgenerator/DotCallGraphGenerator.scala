package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.Cpg

object DotCallGraphGenerator {

  def dotCallGraph(cpg: Cpg): Iterator[String] = {
    val callGraph = new CallGraphGenerator().generate(cpg)
    Iterator(DotSerializer.dotGraph(None, callGraph))
  }

}
