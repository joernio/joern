package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes.Method

object DotCfgGenerator {

  def dotCfg(traversal: Iterator[Method]): Iterator[String] =
    traversal.map(dotCfg)

  def dotCfg(method: Method): String = {
    val cfg = new CfgGenerator().generate(method)
    DotSerializer.dotGraph(Option(method), cfg)
  }

}
