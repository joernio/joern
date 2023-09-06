package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.Cpg

object DotTypeHierarchyGenerator {

  def dotTypeHierarchy(cpg: Cpg): Iterator[String] = {
    val typeHierarchy = new TypeHierarchyGenerator().generate(cpg)
    Iterator(DotSerializer.dotGraph(None, typeHierarchy))
  }

}
