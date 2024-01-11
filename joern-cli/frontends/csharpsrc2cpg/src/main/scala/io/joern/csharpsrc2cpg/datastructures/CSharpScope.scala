package io.joern.csharpsrc2cpg.datastructures

import io.joern.x2cpg.datastructures.{Scope, ScopeElement}
import io.shiftleft.codepropertygraph.generated.nodes.{DeclarationNew, NewLocal, NewMethodParameterIn, NewNode}

class CSharpScope extends Scope[String, DeclarationNew, ScopeType] {

  /** @return
    *   the surrounding type declaration if one exists.
    */
  def surroundingTypeDeclFullName: Option[String] = stack.collectFirst { case ScopeElement(TypeScope(fullName), _) =>
    fullName
  }

  /** @return
    *   the full name of the surrounding scope.
    */
  def surroundingScopeFullName: Option[String] = stack.collectFirst {
    case ScopeElement(NamespaceScope(fullName), _) => fullName
    case ScopeElement(MethodScope(fullName), _)    => fullName
    case ScopeElement(TypeScope(fullName), _)      => fullName
  }

  /** @return
    *   true if the scope is currently on the top-level, false if the scope is within some nested scope.
    */
  def isTopLevel: Boolean = stack
    .filterNot(x => x.scopeNode.isInstanceOf[NamespaceScope])
    .exists(x => x.scopeNode.isInstanceOf[MethodScope] || x.scopeNode.isInstanceOf[TypeScope])

  def tryResolveTypeReference(typeName: String): Option[String] = {
    // TODO: Look for a type with a matching name in scope
    None
  }

  def tryResolveMethodInvocation(typeFullName: String, callName: String): Option[String] = {
    // TODO: Use type map to resolve method full name
    None
  }

}
