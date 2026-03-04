package io.joern.swiftsrc2cpg.passes

import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.NewBinding
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

/** Adds `BINDS`/`REF` bindings from the synthetic function type to generated closure functions.
  *
  * Each closure function is exposed through the canonical apply name so dispatch can resolve closure calls via bindings
  * like regular function-type invocations.
  *
  * @param cpg
  *   graph to enrich with closure bindings
  */
class ClosureBindingsPass(cpg: Cpg) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    for {
      closureTypeDecl <- cpg.typeDecl.fullNameExact(Defines.Function).loneElementOption
      closureMethod   <- cpg.method.filter(_.name.startsWith(io.joern.x2cpg.Defines.ClosurePrefix))
    } {
      val functionBinding = NewBinding()
        .name(Defines.ClosureApplyMethodName)
        .methodFullName(closureMethod.fullName)
        .signature(closureMethod.signature)
      diffGraph.addNode(functionBinding)
      diffGraph.addEdge(closureTypeDecl, functionBinding, EdgeTypes.BINDS)
      diffGraph.addEdge(functionBinding, closureMethod, EdgeTypes.REF)
    }
  }

}
