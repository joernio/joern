package io.joern.csharpsrc2cpg.datastructures

sealed trait ScopeType

case class NamespaceScope(fullName: String) extends ScopeType

case class TypeScope(fullName: String) extends ScopeType

case class MethodScope(fullName: String) extends ScopeType

object BlockScope extends ScopeType
