package io.joern.ghidra2cpg.passes

import ghidra.program.model.listing.Function
import io.joern.ghidra2cpg._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.ConcurrentWriterCpgPass

class TypesPass(cpg: Cpg)
  extends ConcurrentWriterCpgPass[Function](cpg) {

  override def runOnPart(diffGraphBuilder: DiffGraphBuilder, function: Function): Unit = {
    Types.types
      .foreach { typ =>
        val typeNode =
          nodes.NewType().name(typ).fullName(typ).typeDeclFullName(typ)
        diffGraphBuilder.addNode(typeNode)
      }
  }
}
