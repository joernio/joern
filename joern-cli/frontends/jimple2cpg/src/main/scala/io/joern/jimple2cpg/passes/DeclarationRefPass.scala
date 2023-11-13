package io.joern.jimple2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Declaration, Method}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language.*

/** Links declarations to their identifier nodes.
  */
class DeclarationRefPass(cpg: Cpg) extends ConcurrentWriterCpgPass[Method](cpg) {

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(builder: DiffGraphBuilder, part: Method): Unit = {
    val identifiers  = part._identifierViaContainsOut.toList
    val declarations = (part.parameter ++ part.local).collectAll[Declaration].l
    declarations.foreach(d => identifiers.nameExact(d.name).foreach(builder.addEdge(_, d, EdgeTypes.REF)))
  }

}
