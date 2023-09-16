package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes.Method

object DotCdgGenerator {

  def dotCdg(traversal: Iterator[Method]): Iterator[String] =
    traversal.map(dotCdg)

  def dotCdg(method: Method): String = {
    val cdg = new CdgGenerator().generate(method)
    DotSerializer.dotGraph(Option(method), cdg)
  }

}
