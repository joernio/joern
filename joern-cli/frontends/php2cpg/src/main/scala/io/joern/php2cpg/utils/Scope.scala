package io.joern.php2cpg.utils

import io.joern.php2cpg.astcreation.AstCreator.NameConstants
import io.joern.php2cpg.utils.PhpScopeElement
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.{ScopeElement, NamespaceLikeScope, Scope as X2CpgScope}
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewNamespaceBlock, NewNode, NewTypeDecl}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory

import scala.collection.mutable

class Scope(implicit nextClosureName: () => String) extends X2CpgScope[String, NewNode, PhpScopeElement] {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private var constAndStaticInits: List[mutable.ArrayBuffer[Ast]] = Nil
  private var fieldInits: List[mutable.ArrayBuffer[Ast]]          = Nil
  private val anonymousMethods                                    = mutable.ArrayBuffer[Ast]()
  private var tmpVarCounter                                       = 0
  private var tmpClassCounter                                     = 0

  def pushNewScope(scopeNode: NewNode): Unit = {
    scopeNode match {
      case method: NewMethod =>
        super.pushNewScope(PhpScopeElement(method))

      case typeDecl: NewTypeDecl =>
        constAndStaticInits = mutable.ArrayBuffer[Ast]() :: constAndStaticInits
        fieldInits = mutable.ArrayBuffer[Ast]() :: fieldInits
        super.pushNewScope(PhpScopeElement(typeDecl))

      case namespace: NewNamespaceBlock =>
        super.pushNewScope(PhpScopeElement(namespace))

      case invalid =>
        logger.warn(s"pushNewScope called with invalid node $invalid. Ignoring!")
    }
  }

  def surroundingAstLabel: Option[String] = stack.collectFirst {
    case ScopeElement(_: NamespaceLikeScope, _) => NodeTypes.NAMESPACE_BLOCK
    case ScopeElement(scopeNode: PhpScopeElement, _) =>
      scopeNode.node match {
        case _: NewTypeDecl       => NodeTypes.TYPE_DECL
        case _: NewNamespaceBlock => NodeTypes.NAMESPACE_BLOCK
        case _: NewMethod         => NodeTypes.METHOD
      }
  }

  def surroundingScopeFulLName: Option[String] = stack.collectFirst {
    case ScopeElement(x: NamespaceLikeScope, _) => x.fullName
    case ScopeElement(scopeNode: PhpScopeElement, _) =>
      scopeNode.node match {
        case x: NewTypeDecl       => x.fullName
        case x: NewMethod         => x.fullName
        case x: NewNamespaceBlock => x.fullName
      }
  }

  override def popScope(): Option[PhpScopeElement] = {
    val scopeNode = super.popScope()

    scopeNode.map(_.node) match {
      case Some(_: NewTypeDecl) =>
        // TODO This is unsafe to catch errors for now
        constAndStaticInits = constAndStaticInits.tail
        fieldInits = fieldInits.tail

      case _ => // Nothing to do here
    }

    scopeNode
  }

  def getNewClassTmp: String = {
    stack.headOption match {
      case Some(node) =>
        s"${this.surroundingScopeFulLName.getOrElse("<global>")}@${node.scopeNode.getNextClassTmp}"
      case None =>
        logger.warn(s"Stack is empty - using global counter ")
        s"${this.surroundingScopeFulLName.getOrElse("<global>")}@${this.getNextClassTmp}"
    }
  }

  def getNewVarTmp: String = {
    stack.headOption match {
      case Some(node) =>
        s"${this.surroundingScopeFulLName.getOrElse("<global>")}@${node.scopeNode.getNextVarTmp}"
      case None =>
        s"${this.surroundingScopeFulLName.getOrElse("<global>")}@${this.getNextVarTmp}"
    }
  }

  override def addToScope(identifier: String, variable: NewNode): PhpScopeElement = {
    super.addToScope(identifier, variable)
  }

  def addAnonymousMethod(methodAst: Ast): Unit = anonymousMethods.addOne(methodAst)

  def getAndClearAnonymousMethods: List[Ast] = {
    val methods = anonymousMethods.toList
    anonymousMethods.clear()
    methods
  }

  def getEnclosingNamespaceNames: List[String] =
    stack.map(_.scopeNode.node).collect { case ns: NewNamespaceBlock => ns.name }.reverse

  def getEnclosingTypeDeclTypeName: Option[String] =
    stack.map(_.scopeNode.node).collectFirst { case td: NewTypeDecl => td }.map(_.name)

  def getEnclosingTypeDeclTypeFullName: Option[String] =
    stack.map(_.scopeNode.node).collectFirst { case td: NewTypeDecl => td }.map(_.fullName)

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

  def getScopedClosureName: String = {
    stack.headOption match {
      case Some(scopeElement) =>
        scopeElement.scopeNode.getClosureMethodName

      case None =>
        logger.warn("BUG: Attempting to get scopedClosureName, but no scope has been push. Defaulting to unscoped")
        NameConstants.Closure
    }
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

  private def getNextClassTmp: String = {
    val returnString = s"anon-class-${tmpClassCounter}"
    tmpClassCounter += 1

    returnString
  }

  private def getNextVarTmp: String = {
    val returnString = s"tmp-${tmpVarCounter}"
    tmpVarCounter += 1

    returnString
  }
}
