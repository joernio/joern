package io.shiftleft.dataflowengineoss.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.dotgenerator.{CdgGenerator, DotSerializer}
import overflowdb.traversal.Traversal

object DotPdgGenerator {

  def toDotPdg(traversal: Traversal[Method])(implicit semantics: Semantics): Traversal[String] =
    traversal.map(dotGraphForMethod)

  private def dotGraphForMethod(method: Method)(implicit semantics: Semantics): String = {
    val ddg = new DdgGenerator().generate(method)
    val cdg = new CdgGenerator().generate(method)
    DotSerializer.dotGraph(method, ddg.++(cdg), withEdgeTypes = true)
  }

}
