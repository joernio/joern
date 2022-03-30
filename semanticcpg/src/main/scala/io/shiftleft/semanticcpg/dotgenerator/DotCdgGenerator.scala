package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes.Method
import overflowdb.traversal._

object DotCdgGenerator {

  def dotCdg(traversal: Traversal[Method]): Traversal[String] =
    traversal.map(dotCdg)

  def dotCdg(method: Method): String = {
    val cdg = new CdgGenerator().generate(method)
    DotSerializer.dotGraph(method, cdg)
  }

}
