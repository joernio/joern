package io.joern.jimple2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Declaration, Method}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language.*

/** Links declarations to their identifier nodes. Due to the flat AST of bytecode, we don't need to account for varying
  * scope.
  */
class DeclarationRefPass(cpg: Cpg) extends ConcurrentWriterCpgPass[Method](cpg) {

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(builder: DiffGraphBuilder, part: Method): Unit = {
    val identifiers  = part.ast.isIdentifier.toList
    val declarations = (part.parameter ++ part.block.astChildren.isLocal).collectAll[Declaration].l
    declarations.foreach(d => identifiers.nameExact(d.name).foreach(builder.addEdge(_, d, EdgeTypes.REF)))
  }

}
