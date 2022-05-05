package io.joern.javasrc2cpg.util

import io.joern.javasrc2cpg.util.Scope.WildcardImportName
import io.joern.x2cpg.datastructures.Stack.Stack
import io.joern.x2cpg.datastructures.{ScopeElement, Stack, Scope => X2CpgScope}
import io.shiftleft.codepropertygraph.generated.nodes.{HasTypeFullName, NewNode, NewTypeDecl}

import scala.collection.mutable

case class NodeTypeInfo(node: NewNode with HasTypeFullName, isField: Boolean = false, isStatic: Boolean = false)

class Scope extends X2CpgScope[String, NodeTypeInfo, NewNode] {

  private var typeDeclStack: List[NewTypeDecl] = Nil

  override def pushNewScope(scopeNode: NewNode): Unit = {
    scopeNode match {
      case typeDecl: NewTypeDecl => typeDeclStack = typeDecl :: typeDeclStack
      case _                     => // Nothing to do in this case
    }

    super.pushNewScope(scopeNode)
  }

  override def popScope(): Option[NewNode] = {
    super.popScope() match {

      case Some(typeDecl: NewTypeDecl) =>
        typeDeclStack = typeDeclStack.tail
        Some(typeDecl)

      case ret => ret
    }
  }

  def addToScope(identifier: String, node: NewNode with HasTypeFullName): Unit = {
    addToScope(identifier, NodeTypeInfo(node))
  }

  def getEnclosingTypeDecl: Option[NewTypeDecl] = {
    typeDeclStack.headOption
  }

  def lookupVariableType(identifier: String): Option[String] = {
    lookupVariable(identifier).map(_.node.typeFullName)
  }

  def getWildcardType(identifier: String): Option[String] = {
    lookupVariableType(WildcardImportName) map { importName =>
      s"$importName.$identifier"
    }
  }
}

object Scope {
  val WildcardImportName: String = "*"
  def apply(): Scope             = new Scope()
}
