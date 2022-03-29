package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes.Method
import overflowdb.traversal._

object DotCfgGenerator {

  def dotCfg(traversal: Traversal[Method]): Traversal[String] =
    traversal.map(dotCfg)

  def dotCfg(method: Method): String = {
    val cfg = new CfgGenerator().generate(method)
    DotSerializer.dotGraph(method, cfg)
  }

}
