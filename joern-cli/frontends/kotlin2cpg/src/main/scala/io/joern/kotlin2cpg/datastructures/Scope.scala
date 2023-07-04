package io.joern.kotlin2cpg.datastructures

import io.joern.x2cpg.datastructures.ScopeElement
import io.joern.x2cpg.datastructures.{Scope => X2CpgScope}
import io.shiftleft.codepropertygraph.generated.nodes.DeclarationNew

class Scope[I, V <: DeclarationNew, S] extends X2CpgScope[I, V, S] {
  def pushClosureScope(scopeNode: S): List[V] = {
    val captured =
      stack.foldLeft(List[V]()) { (acc, stackEntry) =>
        acc ++ stackEntry.variables.values
      }
    stack = ScopeElement[I, V, S](scopeNode) :: stack

    captured.groupBy(_.name).map { case (_, v) => v.head }.toList
  }
}
