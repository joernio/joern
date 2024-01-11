package io.joern.x2cpg.datastructures

/** Handles the scope stack for tracking identifier to variable relation.
  *
  * @tparam I
  *   Identifier type.
  * @tparam V
  *   Variable type.
  * @tparam S
  *   Scope type.
  */
class Scope[I, V, S] {
  protected var stack: List[ScopeElement[I, V, S]] = List[ScopeElement[I, V, S]]()

  def isEmpty: Boolean = {
    stack.isEmpty
  }

  def pushNewScope(scopeNode: S): Unit = {
    stack = ScopeElement[I, V, S](scopeNode) :: stack
  }

  def popScope(): Option[S] = {
    stack match {
      case Nil => None

      case head :: tail =>
        stack = tail
        Some(head.scopeNode)
    }
  }

  def addToScope(identifier: I, variable: V): S = {
    stack = stack.head.addVariable(identifier, variable) :: stack.tail
    stack.head.scopeNode
  }

  def lookupVariable(identifier: I): Option[V] = {
    stack.collectFirst {
      case scopeElement if scopeElement.variables.contains(identifier) =>
        scopeElement.variables(identifier)
    }
  }

  def size: Int = stack.size

}
