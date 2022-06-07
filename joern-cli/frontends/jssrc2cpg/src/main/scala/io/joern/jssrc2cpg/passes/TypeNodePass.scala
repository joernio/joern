package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewType
import io.shiftleft.passes.SimpleCpgPass

class TypeNodePass(usedTypes: List[(String, String)], cpg: Cpg) extends SimpleCpgPass(cpg, "types") {
  override def run(diffGraph: DiffGraphBuilder): Unit = {
    diffGraph.addNode(
      NewType()
        .name("ANY")
        .fullName("ANY")
        .typeDeclFullName("ANY")
    )
    usedTypes.sortBy(_._2).foreach { case (name, fullName) =>
      val typeNode = NewType().name(name).fullName(fullName).typeDeclFullName(fullName)
      diffGraph.addNode(typeNode)
    }
  }
}
