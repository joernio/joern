package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes.Method
import overflowdb.traversal.Traversal

object DotPagGenerator {

  def dotPag(traversal: Traversal[Method]): Traversal[String] =
    traversal.map(dotPag)

  def dotPag(method: Method): String = {
    val pag = new PagGenerator().generate(method)
    val cfg = new CfgGenerator().generate(method)
    DotSerializer.dotGraph(Option(method), pag ++ cfg, withEdgeTypes = true)
  }

}
