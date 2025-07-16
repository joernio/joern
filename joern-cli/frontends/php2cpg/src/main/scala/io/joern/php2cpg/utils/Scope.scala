package io.joern.php2cpg.utils

import io.joern.php2cpg.astcreation.AstCreator.NameConstants
import io.joern.php2cpg.parser.Domain.{PhpExpr, PhpNode}
import io.joern.php2cpg.parser.Domain.MetaTypeDeclExtension
import io.joern.php2cpg.passes.SymbolSummaryPass.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.{NamespaceLikeScope, ScopeElement, TypedScopeElement, Scope as X2CpgScope}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory

import scala.collection.mutable

sealed case class PhpInit(originNode: PhpNode, memberNode: NewMember, value: PhpExpr) {}

class Scope(summary: Map[String, Seq[SymbolSummary]] = Map.empty, closureNameFn: () => String)
    extends X2CpgScope[String, NewNode, TypedScopeElement] {
  // This is a workaround for scalafmt. On 3.8.1 scalafmt fails with an error for the `given` line, but the later versions (3.8.4+)
  // are adding spaces in comments of random files which we don't want.
  private type ClosureCallBackSignature = () => String

  // allows the usage of `nextClosureName` in `trait ClosureNameCreator` in `utils/ScopeElement`
  given closureName: ClosureCallBackSignature = closureNameFn

  private val logger = LoggerFactory.getLogger(this.getClass)

  private var constAndStaticInits: List[mutable.ArrayBuffer[PhpInit]] = Nil
  private var fieldInits: List[mutable.ArrayBuffer[PhpInit]]          = Nil
  private val anonymousMethods                                        = mutable.ArrayBuffer[Ast]()
  private var tmpVarCounter                                           = 0
  private var tmpClassCounter                                         = 0
  private var importedSymbols                                         = Map.empty[String, SymbolSummary]
  private val methodRefsInAst                                         = mutable.HashMap[String, NewMethodRef]()

  override def pushNewScope(scopeNode: TypedScopeElement): Unit = {
    val mappedNode = scopeNode match {
      case block: BlockScope =>
        val blockFullName = stack.headOption
          .map { case ScopeElement(node: NamedScope, _) => node.fullName }
          .getOrElse("")
        BlockScope(block.block, blockFullName)
      case method: MethodScope =>
        method
      case typeDecl: TypeScope =>
        constAndStaticInits = mutable.ArrayBuffer[PhpInit]() :: constAndStaticInits
        fieldInits = mutable.ArrayBuffer[PhpInit]() :: fieldInits

        typeDecl
      case namespace: NamespaceScope => namespace
    }

    super.pushNewScope(mappedNode)
  }

  def getDeduplicatedMethodName(methodName: String): String =
    stack.headOption
      .collectFirst {
        case ScopeElement(node: TypeScope, _)      => node.getNextDeduplicateMethodName(methodName)
        case ScopeElement(node: MethodScope, _)    => node.getNextDeduplicateMethodName(methodName)
        case ScopeElement(node: NamespaceScope, _) => node.getNextDeduplicateMethodName(methodName)
      }
      .getOrElse(methodName)

  def getDeduplicatedClassName(className: String): String =
    stack.headOption
      .collectFirst {
        case ScopeElement(node: TypeScope, _)      => node.getNextDeduplicateClassName(className)
        case ScopeElement(node: MethodScope, _)    => node.getNextDeduplicateClassName(className)
        case ScopeElement(node: NamespaceScope, _) => node.getNextDeduplicateClassName(className)
      }
      .getOrElse(className)

  def surroundingAstLabel: Option[String] = stack.collectFirst {
    case ScopeElement(_: NamespaceLikeScope, _) => NodeTypes.NAMESPACE_BLOCK
    case ScopeElement(_: TypeLikeScope, _)      => NodeTypes.TYPE_DECL
    case ScopeElement(_: MethodLikeScope, _)    => NodeTypes.METHOD
  }

  def surroundingScopeFullName: Option[String] = stack.collectFirst {
    case ScopeElement(x: NamespaceLikeScope, _) => x.fullName
    case ScopeElement(x: TypeLikeScope, _)      => x.fullName
    case ScopeElement(x: MethodLikeScope, _)    => x.fullName
  }

  override def popScope(): Option[TypedScopeElement] = {
    val scopeNode = super.popScope()

    scopeNode match {
      case Some(_: TypeScope) =>
        // TODO This is unsafe to catch errors for now
        constAndStaticInits = constAndStaticInits.tail
        fieldInits = fieldInits.tail
      case _ => // nothing to do here
    }

    scopeNode
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

  def getNewClassTmp: String = {
    stack.headOption match {
      case Some(ScopeElement(namespace: NamespaceScope, _)) =>
        s"${this.surroundingScopeFullName.getOrElse("<global>")}.${namespace.getNextClassTmp}"
      case Some(ScopeElement(typeScope: TypeScope, _)) =>
        s"${this.surroundingScopeFullName.getOrElse("<global>")}.${typeScope.getNextClassTmp}"
      case Some(ScopeElement(methodScope: MethodScope, _)) =>
        s"${this.surroundingScopeFullName.getOrElse("<global>")}.${methodScope.getNextClassTmp}"
      case _ =>
        logger.warn(s"Stack is empty - using global counter ")
        s"${this.surroundingScopeFullName.getOrElse("<global>")}.${this.getNextClassTmp}"
    }
  }

  def getNewVarTmp(varPrefix: String = ""): String = {
    stack.headOption match {
      case Some(ScopeElement(namespace: NamespaceScope, _)) =>
        s"${this.surroundingScopeFullName.getOrElse("<global>")}@$varPrefix${namespace.getNextVarTmp}"
      case Some(ScopeElement(typeScope: TypeScope, _)) =>
        s"${this.surroundingScopeFullName.getOrElse("<global>")}@$varPrefix${typeScope.getNextVarTmp}"
      case Some(ScopeElement(methodScope: MethodScope, _)) =>
        s"${this.surroundingScopeFullName.getOrElse("<global>")}@$varPrefix${methodScope.getNextVarTmp}"
      case _ =>
        s"${this.surroundingScopeFullName.getOrElse("<global>")}@$varPrefix${this.getNextVarTmp}"
    }
  }

  def addMethodRef(methodRefName: String, methodRef: NewMethodRef): Unit = methodRefsInAst.put(methodRefName, methodRef)
  def getMethodRef(methodRefName: String): Option[NewMethodRef]          = methodRefsInAst.get(methodRefName)

  def addVariableToMethodScope(identifier: String, variable: NewNode, methodFullName: String): Option[MethodScope] = {
    stack.collectFirst {
      case el @ ScopeElement(methodScope: MethodScope, _) if methodScope.fullName == methodFullName =>
        val addedVarStackItem = el.addVariable(identifier, variable)

        // Replace the stack item with a new one that holds the added variable
        stack = stack.map {
          case x if x.scopeNode == addedVarStackItem.scopeNode => addedVarStackItem
          case x                                               => x
        }

        methodScope
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
      .map(_.scopeNode)
      .collectFirst { case TypeScope(td, _) => td }
      .exists(_.name.endsWith(MetaTypeDeclExtension))

  def isTopLevel: Boolean =
    getEnclosingTypeDeclTypeName.forall(_ == NamespaceTraversal.globalNamespaceName)

  def getEnclosingNamespaceNames: List[String] =
    stack.map(_.scopeNode).collect { case NamespaceScope(ns, _) => ns.name }.reverse

  def getEnclosingTypeDeclTypeName: Option[String] =
    stack.map(_.scopeNode).collectFirst { case TypeScope(td, _) => td }.map(_.name)

  def getEnclosingTypeDeclTypeFullName: Option[String] =
    stack.map(_.scopeNode).collectFirst { case TypeScope(td, _) => td }.map(_.fullName)

  def createMethodNameWithSurroundingInformation(methodName: String): String = {
    val namespaces =
      getEnclosingNamespaceNames.filterNot(_ == NamespaceTraversal.globalNamespaceName).reverse.mkString("\\")

    stack
      .map(_.scopeNode)
      .collectFirst {
        case NamespaceScope(nm, _) if nm.name != NamespaceTraversal.globalNamespaceName => s"${nm.name}\\$methodName"
        case TypeScope(td, _) if td.name != NamespaceTraversal.globalNamespaceName      => s"${td.fullName}.$methodName"
        case MethodScope(nm, _, _, _, _) if nm.name != NamespaceTraversal.globalNamespaceName =>
          if (namespaces.isEmpty) {
            s"${nm.fullName}.$methodName"
          } else {
            s"$namespaces\\${nm.fullName}.$methodName"
          }
      }
      .getOrElse(methodName)
  }

  def getSurroundingMethods: List[MethodScope] =
    stack.map(_.scopeNode).collect { case nm: MethodScope => nm }.reverse

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
      case Some(ScopeElement(ns: NamespaceScope, _)) => ns.getClosureMethodName()
      case Some(ScopeElement(ts: TypeScope, _))      => ts.getClosureMethodName()
      case Some(ScopeElement(ms: MethodScope, _))    => ms.getClosureMethodName()
      case _ =>
        logger.warn("BUG: Attempting to get scopedClosureName, but no scope has been push. Defaulting to unscoped")
        NameConstants.Closure
    }
  }

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
}
