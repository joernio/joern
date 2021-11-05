package io.shiftleft.fuzzyc2cpg.passes.astcreation

/**
  * A single element of a scope stack.
  * @tparam I Identifier type.
  * @tparam V Variable type.
  * @tparam S Scope type.
  */
case class ScopeElement[I, V, S](scopeNode: S, variables: Map[I, V] = Map[I, V]()) {
  def addVariable(identifier: I, variable: V): ScopeElement[I, V, S] = {
    ScopeElement(scopeNode, variables + (identifier -> variable))
  }
}
