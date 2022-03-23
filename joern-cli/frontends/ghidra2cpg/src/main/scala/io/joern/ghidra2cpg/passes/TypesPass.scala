package io.joern.ghidra2cpg.passes

import io.joern.ghidra2cpg._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.SimpleCpgPass

class TypesPass(cpg: Cpg) extends SimpleCpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    Types.types
      .foreach { typ =>
        val typeNode =
          nodes.NewType().name(typ).fullName(typ).typeDeclFullName(typ)
        diffGraph.addNode(typeNode)
      }
  }
}
