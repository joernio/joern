package io.joern.php2cpg.utils

import io.joern.php2cpg.astcreation.AstCreator.NameConstants
import io.joern.php2cpg.passes.SymbolSummaryPass.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.{NamespaceLikeScope, ScopeElement, Scope as X2CpgScope}
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.slf4j.LoggerFactory

import scala.collection.mutable

class Scope(summary: Map[String, Seq[SymbolSummary]] = Map.empty)(implicit nextClosureName: () => String)
    extends X2CpgScope[String, NewNode, PhpScopeElement] {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private var constAndStaticInits: List[mutable.ArrayBuffer[Ast]] = Nil
  private var fieldInits: List[mutable.ArrayBuffer[Ast]]          = Nil
  private val anonymousMethods                                    = mutable.ArrayBuffer[Ast]()
  private var tmpVarCounter                                       = 0
  private var tmpClassCounter                                     = 0
  private var importedSymbols                                     = Map.empty[String, SymbolSummary]

  def pushNewScope(scopeNode: NewNode): Unit = {
    scopeNode match {
      case block: NewBlock =>
        val scopeName = stack.headOption.map(_.scopeNode.getName)
        super.pushNewScope(PhpScopeElement(block, scopeName.getOrElse("")))

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
        s"${this.surroundingScopeFullName.getOrElse("<global>")}@${node.scopeNode.getNextClassTmp}"
      case None =>
        logger.warn(s"Stack is empty - using global counter ")
        s"${this.surroundingScopeFullName.getOrElse("<global>")}@${this.getNextClassTmp}"
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

  def addAnonymousMethod(methodAst: Ast): Unit = anonymousMethods.addOne(methodAst)

  def getAndClearAnonymousMethods: List[Ast] = {
    val methods = anonymousMethods.toList
    anonymousMethods.clear()
    methods
  }

  def isSurroundedByMetaclassTypeDecl: Boolean =
    stack.map(_.scopeNode.node).collectFirst { case td: NewTypeDecl if td.name.endsWith("<metaclass>") => td }.isDefined

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

  /** Declares imports to load into this scope.
    * @param imports
    *   the import nodes generated from parsing PhpUseStmt and PhpGroupUseStmt nodes.
    */
  def useImport(imports: Seq[NewImport]): Unit = {
    imports.foreach { imporT =>
      imporT.importedEntity.foreach { importName =>
        summary
          .get(importName)
          .flatMap(_.sorted.headOption) // use ordering to set precedence
          .foreach { hit =>
            imporT.importedAs match {
              case Some(alias) => importedSymbols = importedSymbols + (alias -> hit)
              case None =>
                val name = importName.split("\\\\").last
                importedSymbols = importedSymbols + (name -> hit)
            }
          }
      }
    }
  }

  /** Attempts to resolve a simple symbol.
    *
    * Note: This will be extended to notify the caller if this identifier is instead a local variable.
    */
  def resolveIdentifier(symbol: String): Option[SymbolSummary] = {
    importedSymbols.get(symbol)
  }

}
