package io.shiftleft.fuzzyc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.passes.{DiffGraph, ParallelCpgPass}
import io.shiftleft.semanticcpg.language._

/**
  * A pass that ensures that for any method m for which a body exists,
  * there are no more method stubs for corresponding declarations.
  * */
class StubRemovalPass(cpg: Cpg) extends ParallelCpgPass[Method](cpg) {

  private val sigToMethodWithDef = cpg.method.isNotStub.map(m => (m.signature -> true)).toMap

  override def partIterator: Iterator[Method] =
    cpg.method.isStub.toList
      .filter(m => sigToMethodWithDef.contains(m.signature))
      .iterator

  override def runOnPart(stub: Method): Iterator[DiffGraph] = {
    val diffGraph = DiffGraph.newBuilder
    stub.ast.foreach(diffGraph.removeNode)
    Iterator(diffGraph.build())
  }
}
