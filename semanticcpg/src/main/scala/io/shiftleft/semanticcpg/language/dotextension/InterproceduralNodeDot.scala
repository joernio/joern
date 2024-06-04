package io.shiftleft.semanticcpg.language.dotextension

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.dotgenerator.{DotCallGraphGenerator, DotTypeHierarchyGenerator}

class InterproceduralNodeDot(val cpg: Cpg) extends AnyVal {

  def dotCallGraph: Iterator[String] = DotCallGraphGenerator.dotCallGraph(cpg)

  def dotTypeHierarchy: Iterator[String] = DotTypeHierarchyGenerator.dotTypeHierarchy(cpg)

}
