package io.joern.rust2cpg.passes

import io.shiftleft.codepropertygraph.generated.{Cpg, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

// For each `impl X for Y` (which becomes its own TYPE_DECL), update Y's inheritsFrom
// field to include the `impl X for Y` TYPE_DECL.
class RustTraitImplUpdatePass(cpg: Cpg, traitImplMapping: Map[String, Set[String]]) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    traitImplMapping.foreach { case (typeDeclFullName, traitImplFullNames) =>
      cpg.typeDecl.fullNameExact(typeDeclFullName).foreach { typeDecl =>
        val allInherits = (typeDecl.inheritsFromTypeFullName ++ traitImplFullNames).distinct.sorted
        diffGraph.setNodeProperty(typeDecl, PropertyNames.InheritsFromTypeFullName, allInherits)
      }
    }
  }

}
