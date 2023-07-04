package io.joern.php2cpg.utils

import io.joern.php2cpg.astcreation.AstCreator.NameConstants
import io.joern.php2cpg.parser.Domain.InstanceMethodDelimiter
import io.shiftleft.codepropertygraph.generated.nodes.{NewNode, NewMethod, NewTypeDecl, NewNamespaceBlock}
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.language._

import collection.mutable

class PhpScopeElement private (val node: NewNode, scopeName: String) {
  private lazy val closureNameIdxPool = new IntervalKeyPool(0, Long.MaxValue)

  def getClosureMethodName: String = {
    s"$scopeName$InstanceMethodDelimiter${NameConstants.Closure}${closureNameIdxPool.next()}"
  }
}

object PhpScopeElement {
  def apply(method: NewMethod): PhpScopeElement = {
    new PhpScopeElement(method, method.fullName)
  }

  def apply(typeDecl: NewTypeDecl): PhpScopeElement = {
    new PhpScopeElement(typeDecl, typeDecl.fullName)
  }

  def apply(namespace: NewNamespaceBlock): PhpScopeElement = {
    new PhpScopeElement(namespace, namespace.fullName)
  }

  def unapply(scopeElement: PhpScopeElement): Option[NewNode] = {
    Some(scopeElement.node)
  }
}
