package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.Cpg
import overflowdb.traversal._

object DotTypeHierarchyGenerator {

  def dotTypeHierarchy(cpg: Cpg): Traversal[String] = {
    val typeHierarchy = new TypeHierarchyGenerator().generate(cpg)
    Traversal(DotSerializer.dotGraph(None, typeHierarchy))
  }

}
