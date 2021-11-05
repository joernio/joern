package io.shiftleft.dataflowengineoss.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.dotgenerator.{AstGenerator, CdgGenerator, CfgGenerator, DotSerializer}
import overflowdb.traversal.Traversal

object DotCpg14Generator {

  def toDotCpg14(traversal: Traversal[Method])(implicit semantics: Semantics): Traversal[String] =
    traversal.map(dotGraphForMethod)

  private def dotGraphForMethod(method: Method)(implicit semantics: Semantics): String = {
    val ast = new AstGenerator().generate(method)
    val cfg = new CfgGenerator().generate(method)
    val ddg = new DdgGenerator().generate(method)
    val cdg = new CdgGenerator().generate(method)
    DotSerializer.dotGraph(method, ast ++ cfg ++ ddg ++ cdg, withEdgeTypes = true)
  }

}
