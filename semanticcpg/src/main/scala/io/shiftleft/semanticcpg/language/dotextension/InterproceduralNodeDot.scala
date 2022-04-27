package io.shiftleft.semanticcpg.language.dotextension

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.dotgenerator.{DotCallGraphGenerator, DotTypeHierarchyGenerator}
import overflowdb.traversal.Traversal

class InterproceduralNodeDot(val cpg: Cpg) extends AnyVal {

  def dotCallGraph: Traversal[String] = DotCallGraphGenerator.dotCallGraph(cpg)

  def dotTypeHierarchy: Traversal[String] = DotTypeHierarchyGenerator.dotTypeHierarchy(cpg)

}
