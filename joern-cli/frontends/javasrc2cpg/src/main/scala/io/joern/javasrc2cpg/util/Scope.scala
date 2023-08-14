package io.joern.javasrc2cpg.util

import io.joern.javasrc2cpg.passes.ExpectedType
import io.joern.javasrc2cpg.util.Scope.ScopeTypes.{MethodScope, NamespaceScope, ScopeType, TypeDeclScope}
import io.joern.javasrc2cpg.util.Scope.WildcardImportName
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.datastructures.{ScopeElement, Scope as X2CpgScope}
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.slf4j.LoggerFactory

import scala.collection.mutable

case class NodeTypeInfo(
  node: NewNode,
  name: String,
  typeFullName: Option[String],
  isField: Boolean = false,
  isStatic: Boolean = false
)

class Scope(implicit withSchemaValidation: ValidationMode) extends X2CpgScope[String, NodeTypeInfo, ScopeType] {

  private val logger                                        = LoggerFactory.getLogger(this.getClass)
  private var typeDeclStack: List[NewTypeDecl]              = Nil
  private var lambdaMethods: List[mutable.ArrayBuffer[Ast]] = Nil
  private var lambdaDecls: List[mutable.ArrayBuffer[Ast]]   = Nil
  private var memberInits: List[mutable.ArrayBuffer[Ast]]   = Nil

  override def pushNewScope(scope: ScopeType): Unit = {
    scope match {
      case TypeDeclScope(declNode) =>
        typeDeclStack = declNode :: typeDeclStack
        lambdaMethods = mutable.ArrayBuffer[Ast]() :: lambdaMethods
        memberInits = mutable.ArrayBuffer[Ast]() :: memberInits
      case NamespaceScope =>
        lambdaDecls = mutable.ArrayBuffer[Ast]() :: lambdaMethods
      case _ => // Nothing to do in this case
    }

    super.pushNewScope(scope)
  }

  def getCapturedVariables: List[NodeTypeInfo] = {
    stack.flatMap(_.variables.values).filter { nodeTypeInfo =>
      val node        = nodeTypeInfo.node
      val isValidType = node.isInstanceOf[NewMethodParameterIn] || node.isInstanceOf[NewLocal]
      isValidType && nodeTypeInfo.name != NameConstants.This
    }
  }

  override def popScope(): Option[ScopeType] = {
    super.popScope() match {

      case Some(TypeDeclScope(typeDecl)) =>
        typeDeclStack = typeDeclStack.tail
        lambdaMethods = lambdaMethods.tail
        memberInits = memberInits.tail
        Some(TypeDeclScope(typeDecl))

      case Some(NamespaceScope) =>
        lambdaDecls = lambdaDecls.tail
        None

      case _ => None
    }
  }

  def addToScope(node: NewNode, name: String, typeFullName: Option[String]): Unit = {
    addToScope(identifier = name, NodeTypeInfo(node, name = name, typeFullName = typeFullName))
  }

  def getEnclosingTypeDecl: Option[NewTypeDecl] = {
    typeDeclStack.headOption
  }

  def getLambdaMethodsInScope: Iterable[Ast] = {
    lambdaMethods match {
      case methodsInScope :: _ => methodsInScope

      case Nil =>
        logger.warn(s"Attempting to get lambdas from non-existent scope. Defaulting to empty list.")
        List()
    }
  }

  def addLambdaMethod(method: Ast): Unit = {
    lambdaMethods match {
      case methodsInScope :: _ => methodsInScope.addOne(method)

      case Nil =>
        logger.warn(s"Attempting to add lambda method to non-existent scope. Dropping.")
    }
  }

  def getLambdaDeclsInScope: Iterable[Ast] = {
    lambdaDecls match {
      case declsInScope :: _ => declsInScope

      case Nil =>
        logger.warn(s"Attempting to get decls from non-existent scope. Defaulting to empty list")
        List()
    }
  }

  def addLambdaDecl(ast: Ast): Unit = {
    lambdaDecls match {
      case declsInScope :: _ => declsInScope.addOne(ast)

      case Nil =>
        logger.warn(s"Attempting to add lambda type decl to non-existent scope. Dropping.")
    }
  }

  def lookupVariableType(identifier: String, wildcardFallback: Boolean = false): Option[String] = {
    lookupVariable(identifier)
      .flatMap(_.typeFullName)
      .orElse(
        Option
          .when(wildcardFallback) {
            getWildcardType(identifier)
          }
          .flatten
      )
  }

  def addMemberInitializersToScope(inits: Seq[Ast]): Unit = {
    memberInits match {
      case currMembers :: _ => currMembers.addAll(inits)
      case Nil              => logger.warn("Attempting to add static initializes without memberInits in scope.")
    }
  }

  def getMemberInitializers: Seq[Ast] = {
    memberInits match {
      case currMembers :: _ =>
        currMembers.toSeq.map { ast =>
          ast.root match {
            case Some(root: AstNodeNew) =>
              ast.subTreeCopy(root)

            case _ => Ast()
          }
        }
      case Nil =>
        logger.warn("Attemping to fetch initializers from scope with uninitialized memberInits. Inits may be missing.")
        Seq.empty
    }
  }

  private def getWildcardType(identifier: String): Option[String] = {
    lookupVariableType(WildcardImportName) map { importName =>
      s"$importName.$identifier"
    }
  }

  def getEnclosingMethodReturnType: Option[ExpectedType] = {
    stack.collectFirst { case ScopeElement(MethodScope(expectedType), _) =>
      expectedType
    }
  }
}

object Scope {
  val WildcardImportName: String = "*"

  object ScopeTypes {
    sealed trait ScopeType
    case class TypeDeclScope(declNode: NewTypeDecl)  extends ScopeType
    case class MethodScope(returnType: ExpectedType) extends ScopeType
    case object BlockScope                           extends ScopeType
    case object NamespaceScope                       extends ScopeType
  }

  def apply()(implicit withSchemaValidation: ValidationMode): Scope = new Scope()
}
