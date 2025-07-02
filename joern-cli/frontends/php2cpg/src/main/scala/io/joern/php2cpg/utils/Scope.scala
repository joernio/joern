package io.joern.php2cpg.utils

import flatgraph.DiffGraphBuilder
import io.joern.php2cpg.astcreation.AstCreator.NameConstants
import io.joern.php2cpg.parser.Domain.{PhpExpr, PhpNode}
import io.joern.php2cpg.parser.Domain.MetaTypeDeclExtension
import io.joern.php2cpg.passes.SymbolSummaryPass.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.{NamespaceLikeScope, ScopeElement, Scope as X2CpgScope}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory

import scala.collection.mutable

sealed case class PhpInit(originNode: PhpNode, memberNode: NewMember, value: PhpExpr) {}

class Scope(summary: Map[String, Seq[SymbolSummary]] = Map.empty)(implicit nextClosureName: () => String)
    extends X2CpgScope[String, NewNode, PhpScopeElement] {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private var constAndStaticInits: List[mutable.ArrayBuffer[PhpInit]] = Nil
  private var fieldInits: List[mutable.ArrayBuffer[PhpInit]]          = Nil
  private val anonymousMethods                                        = mutable.ArrayBuffer[Ast]()
  private var tmpVarCounter                                           = 0
  private var tmpClassCounter                                         = 0
  private var importedSymbols                                         = Map.empty[String, SymbolSummary]
  private val methodRefs                                              = mutable.ArrayBuffer[NewMethodRef]()
  private val methodRefsInAst                                         = mutable.HashSet[String]()

  def pushNewScope(
    scopeNode: NewNode,
    maybeBlock: Option[NewBlock] = None,
    methodRef: Option[NewMethodRef] = None
  ): Unit = {
    scopeNode match {
      case block: NewBlock =>
        val scopeName = stack.headOption.map(_.scopeNode.getName)
        super.pushNewScope(PhpScopeElement(block, scopeName.getOrElse("")))

      case method: NewMethod =>
        methodRef match {
          case Some(mr) => methodRefs.addOne(mr)
          case _        =>
        }

        super.pushNewScope(PhpScopeElement(method, maybeBlock))

      case typeDecl: NewTypeDecl =>
        constAndStaticInits = mutable.ArrayBuffer[PhpInit]() :: constAndStaticInits
        fieldInits = mutable.ArrayBuffer[PhpInit]() :: fieldInits
        super.pushNewScope(PhpScopeElement(typeDecl))

      case namespace: NewNamespaceBlock =>
        super.pushNewScope(PhpScopeElement(namespace))

      case invalid =>
        logger.warn(s"pushNewScope called with invalid node $invalid. Ignoring!")
    }
  }

  def surroundingAstLabel: Option[String] = stack.collectFirst {
    case ScopeElement(_: NamespaceLikeScope, _)                 => NodeTypes.NAMESPACE_BLOCK
    case ScopeElement(PhpScopeElement(x: NewNamespaceBlock), _) => NodeTypes.NAMESPACE_BLOCK
    case ScopeElement(PhpScopeElement(x: NewTypeDecl), _)       => NodeTypes.TYPE_DECL
    case ScopeElement(PhpScopeElement(x: NewMethod), _)         => NodeTypes.METHOD
  }

  def surroundingScopeFullName: Option[String] = stack.collectFirst {
    case ScopeElement(x: NamespaceLikeScope, _)                 => x.fullName
    case ScopeElement(PhpScopeElement(x: NewTypeDecl), _)       => x.fullName
    case ScopeElement(PhpScopeElement(x: NewMethod), _)         => x.fullName
    case ScopeElement(PhpScopeElement(x: NewNamespaceBlock), _) => x.fullName
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
        s"${this.surroundingScopeFullName.getOrElse("<global>")}.${node.scopeNode.getNextClassTmp}"
      case None =>
        logger.warn(s"Stack is empty - using global counter ")
        s"${this.surroundingScopeFullName.getOrElse("<global>")}.${this.getNextClassTmp}"
    }
  }

  def getNewVarTmp(varPrefix: String = ""): String = {
    stack.headOption match {
      case Some(node) =>
        s"${this.surroundingScopeFullName.getOrElse("<global>")}@$varPrefix${node.scopeNode.getNextVarTmp}"
      case None =>
        s"${this.surroundingScopeFullName.getOrElse("<global>")}@$varPrefix${this.getNextVarTmp}"
    }
  }

  override def addToScope(identifier: String, variable: NewNode): PhpScopeElement = {
    super.addToScope(identifier, variable)
  }

  def addMethodRefName(methodRefName: String): Unit     = methodRefsInAst.addOne(methodRefName)
  def containsMethodRef(methodRefName: String): Boolean = methodRefsInAst.contains(methodRefName)

  def addVariableToMethodScope(
    identifier: String,
    variable: NewNode,
    methodFullName: String
  ): Option[PhpScopeElement] = {
    stack.collectFirst { stackItem =>
      stackItem.scopeNode match {
        case _ @PhpScopeElement(method: NewMethod) if method.fullName == methodFullName =>
          val addedVarStackItem = stackItem.addVariable(identifier, variable)

          // Replace the stack item with a new one that holds the added variable
          stack = stack.map {
            case x if x.scopeNode == addedVarStackItem.scopeNode => addedVarStackItem
            case x                                               => x
          }
          stackItem.scopeNode
      }
    }
  }

  def addAnonymousMethod(methodAst: Ast): Unit = anonymousMethods.addOne(methodAst)

  def getAndClearAnonymousMethods: List[Ast] = {
    val methods = anonymousMethods.toList
    anonymousMethods.clear()
    methods
  }

  def isSurroundedByMetaclassTypeDecl: Boolean =
    stack
      .map(_.scopeNode.node)
      .collectFirst { case td: NewTypeDecl => td }
      .exists(_.name.endsWith(MetaTypeDeclExtension))

  def isTopLevel: Boolean =
    getEnclosingTypeDeclTypeName.forall(_ == NamespaceTraversal.globalNamespaceName)

  def getEnclosingNamespaceNames: List[String] =
    stack.map(_.scopeNode.node).collect { case ns: NewNamespaceBlock => ns.name }.reverse

  def getEnclosingTypeDeclTypeName: Option[String] =
    stack.map(_.scopeNode.node).collectFirst { case td: NewTypeDecl => td }.map(_.name)

  def getEnclosingTypeDeclTypeFullName: Option[String] =
    stack.map(_.scopeNode.node).collectFirst { case td: NewTypeDecl => td }.map(_.fullName)

  def getSurroundingFullName: String = {
    stack
      .map(_.scopeNode.node)
      .collectFirst {
        case td: NewTypeDecl => td.name
        case nm: NewMethod   => nm.name
      }
      .filterNot(_ == NamespaceTraversal.globalNamespaceName)
      .mkString(".")
  }

  def getSurroundingMethods: List[PhpScopeElement] =
    stack.map(_.scopeNode).collect { case nm if nm.node.isInstanceOf[NewMethod] => nm }.reverse

  def getConstAndStaticInits: List[PhpInit] = {
    getInits(constAndStaticInits)
  }

  def addConstOrStaticInitToScope(node: PhpNode, memberNode: NewMember, value: PhpExpr): Unit = {
    val initNode = PhpInit(node, memberNode, value)
    addInitToScope(initNode, constAndStaticInits)
  }

  def addFieldInitToScope(node: PhpNode, memberNode: NewMember, value: PhpExpr): Unit = {
    val initNode = PhpInit(node, memberNode, value)
    addInitToScope(initNode, fieldInits)
  }

  def getFieldInits: List[PhpInit] = {
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

  def addMethodRef(methodRef: NewMethodRef): Unit = methodRefs.addOne(methodRef)

  private def addInitToScope(init: PhpInit, initList: List[mutable.ArrayBuffer[PhpInit]]): Unit = {
    initList.head.addOne(init)
  }

  private def getInits(initList: List[mutable.ArrayBuffer[PhpInit]]): List[PhpInit] = {
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

  /** Declares imports to load into this scope.
    * @param imports
    *   the import nodes generated from parsing PhpUseStmt and PhpGroupUseStmt nodes.
    */
  def useImport(imports: Seq[NewImport]): Unit = {

    def determineUnresolvedImport(imporT: NewImport): Option[Seq[SymbolSummary]] = {
      imporT.code match {
        case s"use function $_" => imporT.importedEntity.map(x => PhpFunction(x) :: Nil)
        case s"use const $_"    => None // TODO: We don't model constants yet
        case s"use $_"          => imporT.importedEntity.map(x => PhpClass(x) :: Nil)
        case _                  => None
      }
    }

    imports.foreach { imporT =>
      imporT.importedEntity.foreach { importName =>
        summary
          .get(importName)
          .orElse(determineUnresolvedImport(imporT))
          .flatMap(_.sorted.headOption) // use ordering to set precedence
          .foreach { hit =>
            imporT.importedAs match {
              case Some(alias) =>
                importedSymbols = importedSymbols + (alias -> hit)
              case None =>
                val name = importName.split("\\\\").last
                importedSymbols = importedSymbols + (name -> hit) + (importName -> hit) // both are valid aliases
            }
          }
      }
    }
  }

  /** Declares that the following type declaration has been defined and should now be considered in scope and resolvable
    * by its name.
    * @param name
    *   the type decl name.
    * @param fullName
    *   the type decl full name.
    */
  def useTypeDecl(name: String, fullName: String): Unit = {
    summary
      .get(fullName)
      .flatMap(_.sorted.headOption)
      .foreach(hit => importedSymbols = importedSymbols + (name -> hit))
  }

  /** Declares that the following function declaration has been defined and should now be considered in scope and
    * resolvable by its name.
    *
    * @param name
    *   the function name.
    * @param fullName
    *   the function full name.
    */
  def useFunctionDecl(name: String, fullName: String): Unit = {
    summary
      .get(fullName)
      .flatMap(_.sorted.headOption)
      .foreach(hit => importedSymbols = importedSymbols + (name -> hit))
  }

  /** Attempts to resolve a simple symbol.
    *
    * Note: This will be extended to notify the caller if this identifier is instead a local variable.
    */
  def resolveIdentifier(symbol: String): Option[SymbolSummary] = {
    importedSymbols.get(symbol)
  }

  def lookupMethodRef(methodRefName: String): Option[NewMethodRef] = methodRefs.collectFirst {
    case mr if mr.methodFullName == methodRefName => mr
  }

}
