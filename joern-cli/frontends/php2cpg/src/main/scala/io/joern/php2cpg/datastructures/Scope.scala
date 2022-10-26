package io.joern.php2cpg.datastructures

import io.joern.php2cpg.astcreation.AstCreator.NameConstants
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.{Scope => X2CpgScope}
import io.shiftleft.codepropertygraph.generated.nodes.{NewLocal, NewMethod, NewNamespaceBlock, NewNode, NewTypeDecl}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

import scala.collection.mutable

class Scope extends X2CpgScope[String, NewNode, NewNode] {

  private var localStack: List[mutable.ArrayBuffer[NewLocal]]     = Nil
  private var constAndStaticInits: List[mutable.ArrayBuffer[Ast]] = Nil
  private var fieldInits: List[mutable.ArrayBuffer[Ast]]          = Nil

  override def pushNewScope(scopeNode: NewNode): Unit = {
    scopeNode match {
      case _: NewMethod => localStack = mutable.ArrayBuffer[NewLocal]() :: localStack
      case _: NewTypeDecl =>
        constAndStaticInits = mutable.ArrayBuffer[Ast]() :: constAndStaticInits
        fieldInits = mutable.ArrayBuffer[Ast]() :: fieldInits
      case _ => // Nothing to do here
    }
    super.pushNewScope(scopeNode)
  }

  override def popScope(): Option[NewNode] = {
    val poppedScope = super.popScope()

    poppedScope match {
      case Some(_: NewMethod) =>
        // TODO This is unsafe to catch errors for now.
        localStack = localStack.tail

      case Some(_: NewTypeDecl) =>
        // TODO This is unsafe to catch errors for now
        constAndStaticInits = constAndStaticInits.tail
        fieldInits = fieldInits.tail

      case _ => // Nothing to do here
    }

    poppedScope
  }

  override def addToScope(identifier: String, variable: NewNode): NewNode = {
    variable match {
      case local: NewLocal =>
        // TODO This is unsafe to catch errors for now.
        localStack.head.addOne(local)
      case _ => // Nothing to do here
    }
    super.addToScope(identifier, variable)
  }

  def getEnclosingNamespaceName: Option[String] =
    stack.map(_.scopeNode).collectFirst { case ns: NewNamespaceBlock => ns }.map(_.name)

  def getEnclosingTypeDeclType: Option[String] =
    stack.map(_.scopeNode).collectFirst { case td: NewTypeDecl => td }.map(_.fullName)

  def getLocalsInScope: List[NewLocal] = localStack.headOption.map(_.toList).toList.flatten

  def addConstOrStaticInitToScope(ast: Ast): Unit = {
    addInitToScope(ast, constAndStaticInits)
  }
  def getConstAndStaticInits: List[Ast] = {
    getInits(constAndStaticInits)
  }

  def addFieldInitToScope(ast: Ast): Unit = {
    addInitToScope(ast, fieldInits)
  }

  def getFieldInits: List[Ast] = {
    getInits(fieldInits)
  }

  private def addInitToScope(ast: Ast, initList: List[mutable.ArrayBuffer[Ast]]): Unit = {
    // TODO This is unsafe to catch errors for now
    initList.head.addOne(ast)
  }

  private def getInits(initList: List[mutable.ArrayBuffer[Ast]]): List[Ast] = {
    // TODO This is unsafe to catch errors for now
    val ret = initList.head.toList
    // These ASTs should only be added once to avoid aliasing issues.
    initList.head.clear()
    ret
  }
}
