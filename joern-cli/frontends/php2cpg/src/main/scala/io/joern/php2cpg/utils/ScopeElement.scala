package io.joern.php2cpg.utils

import io.joern.php2cpg.parser.Domain.MethodDelimiter
import io.shiftleft.codepropertygraph.generated.nodes.{
  MethodRef,
  NewBlock,
  NewMethod,
  NewMethodRef,
  NewNamespaceBlock,
  NewNode,
  NewTypeDecl
}

class PhpScopeElement private (val node: NewNode, scopeName: String, val maybeBlock: Option[NewBlock] = None)(implicit
  nextClosureName: () => String
) {
  private var tmpVarCounter   = 0
  private var tmpClassCounter = 0

  def getNextClassTmp: String = {
    val returnString = s"anon-class-${tmpClassCounter}"
    tmpClassCounter += 1

    returnString
  }

  def getNextVarTmp: String = {
    val returnString = s"tmp-${tmpVarCounter}"
    tmpVarCounter += 1

    returnString
  }

  def getName: String = scopeName

  def getClosureMethodName: String = {
    s"$scopeName$MethodDelimiter${nextClosureName()}"
  }
}

object PhpScopeElement {
  def apply(block: NewBlock, scopeName: String)(implicit nextClosureName: () => String): PhpScopeElement = {
    new PhpScopeElement(block, scopeName)
  }

  def apply(method: NewMethod, block: Option[NewBlock])(implicit nextClosureName: () => String): PhpScopeElement = {
    new PhpScopeElement(method, method.fullName, block)
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
