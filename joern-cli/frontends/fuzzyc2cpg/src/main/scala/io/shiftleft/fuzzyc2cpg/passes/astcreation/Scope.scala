package io.shiftleft.fuzzyc2cpg.passes.astcreation

/**
  * Handles the scope stack for tracking identifier to variable relation.
  * @tparam I Identifier type.
  * @tparam V Variable type.
  * @tparam S Scope type.
  */
class Scope[I, V, S] {
  private var stack = List[ScopeElement[I, V, S]]()

  def isEmpty: Boolean = {
    stack.isEmpty
  }

  def pushNewScope(scopeNode: S): Unit = {
    stack = ScopeElement[I, V, S](scopeNode) :: stack
  }

  def popScope(): Unit = {
    stack = stack.tail
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

}
