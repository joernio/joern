package io.joern.dataflowengineoss.dotgenerator

import io.joern.dataflowengineoss.DefaultSemantics
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.joern.dataflowengineoss.semanticsloader.FullNameSemantics
import io.shiftleft.semanticcpg.dotgenerator.{AstGenerator, CdgGenerator, CfgGenerator, DotSerializer}

object DotCpg14Generator {

  def toDotCpg14(traversal: Iterator[Method])(implicit
    semantics: FullNameSemantics = DefaultSemantics()
  ): Iterator[String] =
    traversal.map(dotGraphForMethod)

  private def dotGraphForMethod(method: Method)(implicit semantics: FullNameSemantics): String = {
    val ast = new AstGenerator().generate(method)
    val cfg = new CfgGenerator().generate(method)
    val ddg = new DdgGenerator().generate(method)
    val cdg = new CdgGenerator().generate(method)
    DotSerializer.dotGraph(Option(method), ast ++ cfg ++ ddg ++ cdg, withEdgeTypes = true)
  }

}
