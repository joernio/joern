package io.joern.ghidra2cpg.passes

import io.joern.ghidra2cpg._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.{CpgPass, DiffGraph}

class TypesPass(cpg: Cpg) extends CpgPass(cpg) {

  override def run(): Iterator[DiffGraph] = {
    implicit val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder
    Types.types.sorted.distinct
      .foreach { typ =>
        val typeNode =
          nodes.NewType().name(typ).fullName(typ).typeDeclFullName(typ)
        diffGraph.addNode(typeNode)
      }
    Iterator(diffGraph.build())
  }
}
