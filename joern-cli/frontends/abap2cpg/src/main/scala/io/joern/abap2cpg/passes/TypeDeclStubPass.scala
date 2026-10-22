package io.joern.abap2cpg.passes

import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

/** Creates stub TYPE_DECL nodes for TYPE nodes that have no outgoing REF edge.
  *
  * abap2cpg emits built-in type names (ANY, STRING, NUMBER, etc.) as typeFullName values. TypeNodePass turns these into
  * TYPE nodes, but no matching TYPE_DECL is ever created. Dynamic linking requires every TYPE to have a REF edge to a
  * TYPE_DECL and crashes with NoSuchElementException otherwise.
  *
  * This pass adds the missing stubs so the downstream linker can proceed.
  */
class TypeDeclStubPass(cpg: Cpg) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    cpg.typ.filter(_._refOut.isEmpty).foreach { typ =>
      val stub = NewTypeDecl()
        .name(typ.name)
        .fullName(typ.fullName)
        .isExternal(true)
        .filename("<unknown>")
        .astParentType("NAMESPACE_BLOCK")
        .astParentFullName("<global>")
      diffGraph.addNode(stub)
      diffGraph.addEdge(typ, stub, EdgeTypes.REF)
    }
  }
}
