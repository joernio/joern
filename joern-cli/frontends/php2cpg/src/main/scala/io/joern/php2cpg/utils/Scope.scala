package io.joern.php2cpg.utils

import io.joern.php2cpg.astcreation.AstCreator.NameConstants
import io.joern.php2cpg.passes.SymbolSummaryPass.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.{NamespaceLikeScope, ScopeElement, Scope as X2CpgScope}
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewImport,
  NewMethod,
  NewNamespaceBlock,
  NewNode,
  NewTypeDecl
}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory

import scala.collection.mutable

class Scope(summary: Seq[SymbolSummary] = Nil)(implicit nextClosureName: () => String)
    extends X2CpgScope[String, NewNode, PhpScopeElement] {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private var constAndStaticInits: List[mutable.ArrayBuffer[Ast]] = Nil
  private var fieldInits: List[mutable.ArrayBuffer[Ast]]          = Nil
  private val anonymousMethods                                    = mutable.ArrayBuffer[Ast]()
  private var tmpVarCounter                                       = 0
  private var tmpClassCounter                                     = 0
  // Builds a tree of imported symbols, which is essentially a subset of `summary`
  private var importedSymbols = Seq.empty[SymbolSummary]

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

  def useImport(imports: Seq[NewImport]): Unit = {
    summary.headOption.foreach { globalSummarySymbol =>
      imports.foreach { imporT =>
        imporT.importedEntity.foreach { importName =>
          importName.split("\\\\").toList match {
            case Nil => // ignore
            case importParts =>
              val matchingSymbol = findImport(globalSummarySymbol.name, importParts, summary.head)
              val sparseTree     = matchingSymbol.sparseTree
              importedSymbols = importedSymbols match {
                case head :: _ => head + sparseTree
                case Nil       => sparseTree :: Nil
              }
          }
        }
      }
    }
  }

  /** Attempts to resolve a fully or partially qualified symbol
    */
  def resolveImportedSymbol(symbol: String): Option[SymbolSummary] = {
    importedSymbols.headOption.flatMap(resolveSymbol(symbol, _))
  }
  
  private def resolveSymbol(symbol: String, summary: SymbolSummary): Option[SymbolSummary] = {
    summary match {
      case x if x.alias == symbol => Option(x)
      case x: HasChildren => x.children.map(resolveSymbol(symbol, _)).collectFirst {
        case Some(x) => x
      }
      case _ => Option(PhpExternal(symbol)) // TODO: Might be undesired
    }
  }

  private def findImport(worklist: Seq[String], summary: SymbolSummary): SymbolSummary =
    findImport(NamespaceTraversal.globalNamespaceName, worklist, summary)

  private def findImport(next: String, worklist: Seq[String], summary: SymbolSummary): SymbolSummary = {
    lazy val default = PhpExternal(next)

    if (worklist.isEmpty) {
      // TODO: Check if this matches PHP's import precedence
      summary match {
        case namespace @ PhpNamespace(name, _) if next == name => namespace
        case func @ PhpFunction(name) if next == name          => func
        case clazz @ PhpClass(name, _) if next == name         => clazz
        case member @ PhpMember(name) if next == name          => member
        case _                                                 => default
      }
    } else {
      summary match {
        case x: HasChildren if next == x.name =>
          x.children
            .map(findImport(worklist.head, worklist.tail, _))
            .headOption
            .getOrElse(default.copy(_parent = Option(x)))
        case _ => default
      }
    }
  }

}
