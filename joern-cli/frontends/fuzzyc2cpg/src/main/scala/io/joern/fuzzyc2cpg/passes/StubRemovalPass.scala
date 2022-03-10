package io.joern.fuzzyc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._

/** A pass that ensures that for any method m for which a body exists, there are no more method stubs for corresponding
  * declarations.
  */
class StubRemovalPass(cpg: Cpg) extends ConcurrentWriterCpgPass[Method](cpg) {

  private val sigToMethodWithDef = cpg.method.isNotStub.map(m => m.signature -> true).toMap

  override def generateParts(): Array[Method] =
    cpg.method.isStub.toList
      .filter(m => sigToMethodWithDef.contains(m.signature))
      .toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, stub: Method): Unit = {
    stub.ast.foreach(diffGraph.removeNode)
  }

}
