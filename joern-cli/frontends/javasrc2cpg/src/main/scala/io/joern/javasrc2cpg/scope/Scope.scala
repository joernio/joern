package io.joern.javasrc2cpg.scope

import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespace
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeParameter
import io.shiftleft.codepropertygraph.generated.nodes.NewImport
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodParameterIn
import io.shiftleft.codepropertygraph.generated.nodes.NewMember
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.nodes.DeclarationNew

import io.joern.javasrc2cpg.scope.Scope._
import io.joern.javasrc2cpg.astcreation.ExpectedType

import scala.collection.mutable
import io.joern.x2cpg.utils.ListUtils._
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import io.joern.x2cpg.Ast
import io.joern.javasrc2cpg.util.NameConstants

// TODO: Added for backwards compatibility with old scope methods, but is no longer
//  strictly necessary due to stricter scope variable classes. Refactor AstCreator
//  to make use of those and remove this instead.
case class NodeTypeInfo(
  node: NewNode,
  name: String,
  typeFullName: Option[String],
  isField: Boolean = false,
  isStatic: Boolean = false
)
class Scope {
  private var scopeStack: List[JavaScopeElement] = Nil

  def pushBlockScope(): Unit = {
    scopeStack = new BlockScope() :: scopeStack
  }

  def pushMethodScope(method: NewMethod, returnType: ExpectedType): Unit = {
    scopeStack = new MethodScope(method, returnType) :: scopeStack
  }

  def pushTypeDeclScope(typeDecl: NewTypeDecl): Unit = {
    scopeStack = new TypeDeclScope(typeDecl) :: scopeStack
  }

  def pushNamespaceScope(namespace: NewNamespaceBlock): Unit = {
    scopeStack = new NamespaceScope(namespace) :: scopeStack
  }

  def popScope(): JavaScopeElement = {
    val scope = scopeStack.head
    scopeStack = scopeStack.tail
    scope
  }

  def addParameter(parameter: NewMethodParameterIn): Unit = {
    addVariable(ScopeParameter(parameter))
  }

  def addLocal(local: NewLocal): Unit = {
    addVariable(ScopeLocal(local))
  }

  def addMember(member: NewMember, isStatic: Boolean): Unit = {
    addVariable(ScopeMember(member, isStatic))
  }

  def addStaticImport(importNode: NewImport): Unit = {
    addVariable(ScopeStaticImport(importNode))
  }

  private def addVariable(variable: ScopeVariable): Unit = {
    scopeStack.head.addVariableToScope(variable)
  }

  def addType(name: String, typeFullName: String): Unit = {
    scopeStack.head.addTypeToScope(name, typeFullName)
  }

  def addWildcardImport(prefix: String): Unit = {
    scopeStack.head.addWildcardImport(prefix)
  }

  def lookupVariable(name: String): VariableLookupResult = {
    scopeStack.takeUntil(_.lookupVariable(name).isDefined) match {
      case Nil => NotInScope

      case foundSubstack =>
        // Know the last will contain the lookup variable since that is the condition to terminate
        // the takeUntil
        val variable = foundSubstack.last.lookupVariable(name).get
        lookupResultFromFound(foundSubstack, variable)
    }
  }

  def lookupType(simpleName: String): Option[String] = {
    lookupType(simpleName, includeWildcards = true)
  }

  private def lookupType(simpleName: String, includeWildcards: Boolean): Option[String] = {
    scopeStack.iterator
      .map(_.lookupType(simpleName, includeWildcards))
      .collectFirst { case Some(typeFullName) => typeFullName }
  }

  def lookupVariableOrType(name: String): Option[String] = {
    lookupVariable(name).typeFullName.orElse(lookupType(name, includeWildcards = false))
  }

  private def lookupResultFromFound(
    foundSubstack: List[JavaScopeElement],
    variable: ScopeVariable
  ): VariableLookupResult = {
    val typeDecls = foundSubstack.collect { case td: TypeDeclScope => td }

    val isCaptureChain = typeDecls.size > 1 || (typeDecls.nonEmpty && !foundSubstack.last.isInstanceOf[TypeDeclScope])

    if (isCaptureChain) {
      CapturedVariable(typeDecls.map(_.typeDecl), variable)
    } else {
      SimpleVariable(variable)
    }
  }

  def enclosingTypeDecl: Option[NewTypeDecl] = scopeStack.collectFirst { case typeDeclScope: TypeDeclScope =>
    typeDeclScope.typeDecl
  }

  def enclosingTypeDeclFullName: Option[String] = enclosingTypeDecl.map(_.fullName)

  def enclosingMethodReturnType: Option[ExpectedType] = scopeStack.collectFirst { case methodScope: MethodScope =>
    methodScope.returnType
  }

  def addLocalDecl(decl: Ast): Unit = {
    scopeStack.collectFirst { case typeDeclContainer: TypeDeclContainer => typeDeclContainer.registerTypeDecl(decl) }
  }

  // TODO: The below section of todos are all methods that have been added for simple compatibility with the old
  //  scope. The plan is to refactor the code to handle these directly in the AstCreator to make the code easier
  //  to reason about, so these should be removed when that happens.

  // TODO: Refactor and remove this
  def addMemberInitializers(initializers: Seq[Ast]): Unit = {
    scopeStack.collectFirst { case typeDeclScope: TypeDeclScope => typeDeclScope.addMemberInitializers(initializers) }
  }

  // TODO: Refactor and remove this
  def memberInitializers: List[Ast] = {
    scopeStack
      .collectFirst { case typeDeclScope: TypeDeclScope => typeDeclScope.getMemberInitializers() }
      .getOrElse(Nil)
  }

  // TODO: Refactor and remove this
  def localDeclsInScope: List[Ast] = {
    scopeStack
      .collectFirst { case typeDeclContainer: TypeDeclContainer =>
        typeDeclContainer.registeredTypeDecls
      }
      .getOrElse(Nil)
  }

  // TODO: Refactor and remove this
  def lambdaMethodsInScope: List[Ast] = {
    scopeStack
      .collectFirst { case typeDeclContainer: TypeDeclContainer =>
        typeDeclContainer.registeredLambdaMethods
      }
      .getOrElse(Nil)
  }

  // TODO: Refactor and remove this
  def addLambdaMethod(method: Ast): Unit = {
    scopeStack.collectFirst { case typeDeclContainer: TypeDeclContainer =>
      typeDeclContainer.registerLambdaMethod(method)
    }
  }

  // TODO: Refactor and remove this
  def capturedVariables: List[ScopeVariable] = {
    scopeStack
      .flatMap(_.getVariables())
      .collect {
        case local: ScopeLocal => local

        case parameter: ScopeParameter if parameter.name != NameConstants.This => parameter
      }
  }
}

object Scope {
  type NewScopeNode    = NewBlock | NewMethod | NewTypeDecl | NewNamespaceBlock
  type NewVariableNode = NewLocal | NewMethodParameterIn | NewMember | NewImport

  sealed trait ScopeVariable {
    def node: NewVariableNode
    def typeFullName: String
    def name: String
  }
  final case class ScopeLocal(override val node: NewLocal) extends ScopeVariable {
    val typeFullName: String = node.typeFullName
    val name                 = node.name
  }
  final case class ScopeParameter(override val node: NewMethodParameterIn) extends ScopeVariable {
    val typeFullName: String = node.typeFullName
    val name                 = node.name
  }
  final case class ScopeMember(override val node: NewMember, isStatic: Boolean) extends ScopeVariable {
    val typeFullName: String = node.typeFullName
    val name                 = node.name
  }
  final case class ScopeStaticImport(override val node: NewImport) extends ScopeVariable {
    val typeFullName: String = node.importedEntity.get
    val name                 = node.importedAs.get
  }

  sealed trait VariableLookupResult {
    def typeFullName: Option[String]          = None
    def variableNode: Option[NewVariableNode] = None

    // TODO: Added for convenience, but when proper capture logic is implemented the found
    //  variable cases will have to be handled separately which would render this unnecessary.
    def getVariable(): Option[ScopeVariable] = None

    // TODO: Refactor and remove this
    def asNodeInfoOption: Option[NodeTypeInfo] = None
  }
  case object NotInScope extends VariableLookupResult
  sealed trait FoundVariable(variable: ScopeVariable) extends VariableLookupResult {
    override val typeFullName: Option[String]          = Some(variable.typeFullName)
    override val variableNode: Option[NewVariableNode] = Some(variable.node)
    override def getVariable(): Option[ScopeVariable]  = Some(variable)

    override def asNodeInfoOption: Option[NodeTypeInfo] = {
      val nodeTypeInfo = variable match {
        case ScopeMember(memberNode, isStatic) =>
          NodeTypeInfo(memberNode, memberNode.name, Some(memberNode.typeFullName), true, isStatic)

        case variable => NodeTypeInfo(variable.node, variable.name, Some(variable.typeFullName), false, false)
      }

      Some(nodeTypeInfo)
    }
  }
  final case class SimpleVariable(variable: ScopeVariable) extends FoundVariable(variable)

  final case class CapturedVariable(typeDeclChain: List[NewTypeDecl], variable: ScopeVariable)
      extends FoundVariable(variable)
}
