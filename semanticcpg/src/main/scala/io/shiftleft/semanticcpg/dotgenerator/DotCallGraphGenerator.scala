package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.Cpg
import overflowdb.traversal._

object DotCallGraphGenerator {

  def dotCallGraph(cpg: Cpg): Traversal[String] = {
    val callGraph = new CallGraphGenerator().generate(cpg)
    Iterator(DotSerializer.dotGraph(None, callGraph))
  }

}
