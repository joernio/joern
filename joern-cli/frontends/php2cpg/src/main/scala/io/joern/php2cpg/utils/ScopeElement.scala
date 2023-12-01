package io.joern.php2cpg.utils

import io.joern.php2cpg.parser.Domain.InstanceMethodDelimiter
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewNamespaceBlock, NewNode, NewTypeDecl}

class PhpScopeElement private (val node: NewNode, scopeName: String)(implicit nextClosureName: () => String) {

  def getClosureMethodName: String = {
    s"$scopeName$InstanceMethodDelimiter${nextClosureName()}"
  }
}

object PhpScopeElement {
  def apply(method: NewMethod)(implicit nextClosureName: () => String): PhpScopeElement = {
    new PhpScopeElement(method, method.fullName)
  }

  def apply(typeDecl: NewTypeDecl)(implicit nextClosureName: () => String): PhpScopeElement = {
    new PhpScopeElement(typeDecl, typeDecl.fullName)
  }

  def apply(namespace: NewNamespaceBlock)(implicit nextClosureName: () => String): PhpScopeElement = {
    new PhpScopeElement(namespace, namespace.fullName)
  }

  def unapply(scopeElement: PhpScopeElement): Option[NewNode] = {
    Some(scopeElement.node)
  }
}
