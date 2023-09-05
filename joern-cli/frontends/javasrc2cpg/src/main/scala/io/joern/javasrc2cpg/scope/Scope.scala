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
import io.joern.javasrc2cpg.passes.ExpectedType

import scala.collection.mutable
import io.joern.x2cpg.utils.ListUtils._
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import io.joern.x2cpg.Ast
import io.joern.javasrc2cpg.util.NodeTypeInfo

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
    scopeStack.headOption match {
      case Some(scope) =>
        scopeStack = scopeStack.tail
        scope

      case None =>
        throw new UnsupportedOperationException("Cannot pop scope element that hasn't been pushed")
    }
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

  private def addVariable(variable: ScopeVariable): Unit = {
    scopeStack.headOption match {
      case Some(scope) => scope.addVariableToScope(variable)

      case None => throw new UnsupportedOperationException("Must push a scope node before adding variables to it")
    }
  }

  def addType(name: String, typeFullName: String): Unit = {
    scopeStack.headOption match {
      case Some(scope) => scope.addTypeToScope(name, typeFullName)

      case None => throw new UnsupportedOperationException("Must push a scope node before adding types to it")
    }
  }

  def addWildcardImport(prefix: String): Unit = {
    scopeStack.headOption match {
      case Some(scope) => scope.addWildcardImport(prefix)

      case None => throw new UnsupportedOperationException("Must push a scope node before adding types to it")
    }
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
    scopeStack.iterator
      .map(_.lookupType(simpleName))
      .collectFirst { case Some(typeFullName) => typeFullName }
  }

  def lookupVariableOrType(name: String): Option[String] = {
    lookupVariable(name).typeFullName.orElse(lookupType(name))
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
  def addLocalDecl(decl: Ast): Unit = {
    scopeStack.collectFirst { case typeDeclContainer: TypeDeclContainer => typeDeclContainer.registerTypeDecl(decl) }
  }
}

object Scope {
  type NewScopeNode    = NewBlock | NewMethod | NewTypeDecl | NewNamespaceBlock
  type NewVariableNode = NewLocal | NewMethodParameterIn | NewMember

  sealed trait ScopeVariable {
    def node: NewVariableNode
    def typeFullName: String
  }
  final case class ScopeLocal(override val node: NewLocal) extends ScopeVariable {
    val typeFullName: String = node.typeFullName
  }
  final case class ScopeParameter(override val node: NewMethodParameterIn) extends ScopeVariable {
    val typeFullName: String = node.typeFullName
  }
  final case class ScopeMember(override val node: NewMember, isStatic: Boolean) extends ScopeVariable {
    val typeFullName: String = node.typeFullName
  }

  sealed trait VariableLookupResult {
    def typeFullName: Option[String]          = None
    def variableNode: Option[NewVariableNode] = None

    // TODO: Added for convenience, but when proper capture logic is implemented, much of this will be removed.
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
        case ScopeLocal(localNode) =>
          NodeTypeInfo(localNode, localNode.name, Some(localNode.typeFullName), false, false)

        case ScopeParameter(parameterNode) =>
          NodeTypeInfo(parameterNode, parameterNode.name, Some(parameterNode.typeFullName), false, false)

        case ScopeMember(memberNode, isStatic) =>
          NodeTypeInfo(memberNode, memberNode.name, Some(memberNode.typeFullName), true, isStatic)
      }

      Some(nodeTypeInfo)
    }
  }
  final case class SimpleVariable(variable: ScopeVariable) extends FoundVariable(variable)

  final case class CapturedVariable(typeDeclChain: List[NewTypeDecl], variable: ScopeVariable)
      extends FoundVariable(variable)
}
