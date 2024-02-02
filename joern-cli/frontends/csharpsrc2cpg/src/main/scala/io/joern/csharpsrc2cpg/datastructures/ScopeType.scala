package io.joern.csharpsrc2cpg.datastructures

import io.joern.csharpsrc2cpg.parser.DotNetNodeInfo

sealed trait ScopeType

case class NamespaceScope(fullName: String) extends ScopeType

case class TypeScope(fullName: String, fields: List[FieldDecl] = List.empty) extends ScopeType

case class FieldDecl(name: String, isStatic: Boolean, isInitialized: Boolean, node: DotNetNodeInfo)

case class MethodScope(fullName: String) extends ScopeType

object BlockScope extends ScopeType
