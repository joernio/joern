package io.joern.javasrc2cpg.scope

import io.joern.javasrc2cpg.astcreation.ExpectedType
import io.joern.javasrc2cpg.scope.Scope.*
import io.joern.javasrc2cpg.scope.JavaScopeElement.*
import io.joern.javasrc2cpg.util.MultiBindingTableAdapterForJavaparser.JavaparserBindingDeclType
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.x2cpg.Ast
import io.joern.x2cpg.utils.ListUtils.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.slf4j.LoggerFactory

import scala.collection.mutable

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
  private val logger = LoggerFactory.getLogger(this.getClass)

  private var scopeStack: List[JavaScopeElement] = Nil

  def pushBlockScope(): Unit = {
    scopeStack = new BlockScope() :: scopeStack
  }

  def pushMethodScope(method: NewMethod, returnType: ExpectedType, isStatic: Boolean): Unit = {
    scopeStack = new MethodScope(method, returnType, isStatic) :: scopeStack
  }

  def pushFieldDeclScope(isStatic: Boolean, name: String): Unit = {
    scopeStack = new FieldDeclScope(isStatic, name) :: scopeStack
  }

  def pushTypeDeclScope(typeDecl: NewTypeDecl, isStatic: Boolean, methodNames: Set[String] = Set.empty): Unit = {
    val captures = getCapturesForNewScope(isStatic)
    val outerClassType = scopeStack.takeUntil(_.isInstanceOf[TypeDeclScope]) match {
      case Nil => None

      case (head: TypeDeclScope) :: Nil => Option.unless(isStatic)(head.typeDecl.fullName)

      case head :: Nil =>
        // make exhaustive match checker happy, but is impossible
        None

      case scopes =>
        Option
          .unless(isStatic || scopes.init.exists(_.isStatic)) {
            scopes.lastOption.collectFirst { case typeDeclScope: TypeDeclScope => typeDeclScope.typeDecl.fullName }
          }
          .flatten
    }
    scopeStack = new TypeDeclScope(typeDecl, isStatic, captures, outerClassType, methodNames) :: scopeStack
  }

  def pushNamespaceScope(namespace: NewNamespaceBlock): Unit = {
    scopeStack = new NamespaceScope(namespace) :: scopeStack
  }

  def popBlockScope(): BlockScope = popScope[BlockScope]()

  def popMethodScope(): MethodScope = popScope[MethodScope]()

  def popFieldDeclScope(): FieldDeclScope = popScope[FieldDeclScope]()

  def popTypeDeclScope(): TypeDeclScope = popScope[TypeDeclScope]()

  def popNamespaceScope(): NamespaceScope = popScope[NamespaceScope]()

  private def popScope[ScopeType0 <: JavaScopeElement](): ScopeType0 = {
    val scope = scopeStack.head
    scopeStack = scopeStack.tail
    scope.asInstanceOf[ScopeType0]
  }

  def addTopLevelType(name: String, typeFullName: String): Unit = {
    val scopeType = ScopeTopLevelType(typeFullName)
    scopeStack.head.addTypeToScope(name, scopeType)
  }

  def addInnerType(name: String, typeFullName: String): Unit = {
    val scopeType = ScopeInnerType(typeFullName)
    scopeStack.head.addTypeToScope(name, scopeType)
  }

  def addWildcardImport(prefix: String): Unit = {
    scopeStack.head.addWildcardImport(prefix)
  }

  def addDeclBinding(name: String, declBindingInfo: JavaparserBindingDeclType): Unit = {
    scopeStack.collectFirst { case declContainer: TypeDeclContainer =>
      declContainer.addDeclBinding(name, declBindingInfo)
    }
  }

  def getDeclBinding(name: String): Option[JavaparserBindingDeclType] = {
    scopeStack.iterator
      .collect { case declContainer: TypeDeclContainer => declContainer.getDeclBinding(name) }
      .collectFirst { case Some(result) => result }
  }

  def lookupVariable(name: String): VariableLookupResult = {
    scopeStack
      .takeUntil(_.isInstanceOf[TypeDeclScope])
      .iterator
      .map(_.lookupVariable(name))
      .collectFirst {
        case result: SimpleVariable   => result
        case result: CapturedVariable => result
      }
      .getOrElse(NotInScope)
  }

  def lookupMethodName(name: String): List[NewTypeDecl] = {
    scopeStack
      .collect { case typeDeclScope: TypeDeclScope => typeDeclScope }
      .takeUntil(_.declaredMethodNames.contains(name))
      .map(_.typeDecl)
  }

  def lookupType(simpleName: String, includeWildcards: Boolean = true): Option[String] = {
    lookupScopeType(simpleName, includeWildcards).map(_.typeFullName)
  }

  def getNextAnonymousClassIndex(): Long = {
    scopeStack.collectFirst { case counter: AnonymousClassCounter => counter.getNextAnonymousClassIndex() }.get
  }

  def lookupScopeType(simpleName: String): Option[ScopeType] = {
    lookupScopeType(simpleName, includeWildcards = false)
  }

  def lookupScopeType(simpleName: String, includeWildcards: Boolean): Option[ScopeType] = {
    scopeStack.iterator
      .map(_.lookupType(simpleName, includeWildcards))
      .collectFirst { case Some(foundType) => foundType }
  }

  def lookupVariableOrType(name: String): Option[String] = {
    lookupVariable(name).typeFullName.orElse(lookupScopeType(name, includeWildcards = false).map(_.typeFullName))
  }

  def getCapturesForNewScope(isStaticScope: Boolean): Map[String, CapturedVariable] = {
    // This will return an empty list if no enclosing type decl is found, but this is okay since
    // this means the new decl must be a top-level type decl which doesn't capture anything.
    val capturedScopes = scopeStack.takeUntil(scope => scope.isInstanceOf[TypeDeclScope]) match {
      case Nil => Nil

      // Don't capture the enclosing type decl if the new scope is static
      case scopes if isStaticScope || scopes.init.exists(_.isStatic) => scopes.init

      case scopes => scopes
    }

    val capturedVariables = capturedScopes.flatMap {
      case scope: TypeDeclScope =>
        val typeDecl = scope.typeDecl

        val capturedMembers =
          scope
            .getVariables()
            .map(variable => variable.name -> CapturedVariable(List(scope.typeDecl), variable))

        val capturedCaptures = scope.capturedVariables.map { case name -> capturedVariable =>
          name -> capturedVariable.copy(typeDeclChain = scope.typeDecl :: capturedVariable.typeDeclChain)
        }

        capturedMembers ++ capturedCaptures

      case scope =>
        scope.getVariables().map(variable => variable.name -> CapturedVariable(Nil, variable))
    }

    // Reverse the captured variables before toMap to match Java shadowing behaviour
    capturedVariables.reverse.toMap
  }

  def registerCaptureUse(capturedVariable: CapturedVariable): Unit = {
    capturedVariable.variable match {
      case _: ScopeMember =>
      // Do nothing, since members are accessed through the `outerClass` field, which is always created for non-static decls

      case variable =>
        scopeStack.collect { case td: TypeDeclScope => td }.drop(capturedVariable.typeDeclChain.size).headOption match {
          case None => logger.warn(s"Cannot register $capturedVariable as used")

          case Some(typeDeclScope) => typeDeclScope.registerCaptureUse(variable)
        }
    }
  }

  def enclosingNamespace: Option[NamespaceScope] = scopeStack.collectFirst { case scope: NamespaceScope => scope }

  def enclosingTypeDecl: Option[TypeDeclScope] = scopeStack.collectFirst { case scope: TypeDeclScope => scope }

  def enclosingMethod: Option[MethodScope] = scopeStack.collectFirst { case scope: MethodScope => scope }

  def enclosingBlock: Option[BlockScope] = scopeStack.collectFirst { case scope: BlockScope => scope }

  def enclosingMethodReturnType: Option[ExpectedType] = scopeStack.collectFirst { case methodScope: MethodScope =>
    methodScope.returnType
  }

  def addLocalDecl(decl: Ast): Unit = {
    scopeStack.collectFirst { case typeDeclContainer: TypeDeclContainer => typeDeclContainer.registerTypeDecl(decl) }
  }

  def isEnclosingScopeStatic: Boolean = scopeStack
    .collectFirst {
      case scope: TypeDeclScope  => scope.isStatic
      case scope: FieldDeclScope => scope.isStatic
      case scope: MethodScope    => scope.isStatic
    }
    .getOrElse(false)

  // TODO: The below section of todos are all methods that have been added for simple compatibility with the old
  //  scope. The plan is to refactor the code to handle these directly in the AstCreator to make the code easier
  //  to reason about, so these should be removed when that happens.

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
  def variablesInScope: List[ScopeVariable] = {
    scopeStack
      .flatMap(_.getVariables())
      .collect {
        case local: ScopeLocal => local

        case parameter: ScopeParameter if parameter.name != NameConstants.This => parameter
      }
  }

  def scopeFullName(dropScopeCount: Int = 0): Option[String] = {
    scopeStack.drop(dropScopeCount).headOption.flatMap {
      case typeDeclScope: TypeDeclScope   => Some(typeDeclScope.typeDecl.fullName)
      case namespaceScope: NamespaceScope => Some(namespaceScope.namespace.fullName)

      case methodScope: MethodScope =>
        Some(methodScope.method.fullName.stripSuffix(s":${methodScope.method.signature}"))

      case fieldDeclScope: FieldDeclScope =>
        val enclosingDeclPrefix = scopeStack.collectFirst { case typeDeclScope: TypeDeclScope =>
          typeDeclScope.typeDecl.fullName
        }
        enclosingDeclPrefix.map(prefix => s"$prefix.${fieldDeclScope.name}")

      case _: BlockScope => scopeFullName(dropScopeCount = dropScopeCount + 1)

      case _ => None
    }
  }
}

object Scope {
  type NewScopeNode    = NewBlock | NewMethod | NewTypeDecl | NewNamespaceBlock
  type NewVariableNode = NewLocal | NewMethodParameterIn | NewMember

  extension (variableNode: NewVariableNode) {
    def typeFullName: String = variableNode match {
      case node: NewLocal             => node.typeFullName
      case node: NewMethodParameterIn => node.typeFullName
      case node: NewMember            => node.typeFullName
    }
  }

  sealed trait ScopeType {
    def typeFullName: String
  }

  /** Used for top-level type declarations and imports that do not have captures to be concerned about or synthetic
    * names in the cpg
    */
  final case class ScopeTopLevelType(override val typeFullName: String) extends ScopeType

  final class ScopeInnerType(override val typeFullName: String) extends ScopeType {
    private val usedCaptures: mutable.ListBuffer[ScopeVariable] = mutable.ListBuffer()

    override def equals(other: Any): Boolean = {
      other match {
        case otherInnerType: ScopeInnerType => typeFullName == otherInnerType.typeFullName
        case _                              => false
      }
    }

    override def hashCode(): Int = typeFullName.hashCode()
  }

  object ScopeInnerType {
    def apply(typeFullName: String): ScopeInnerType = {
      new ScopeInnerType(typeFullName)
    }
  }

  sealed trait ScopeVariable {
    def node: NewVariableNode
    def typeFullName: String
    def name: String
  }
  final case class ScopeLocal(override val node: NewLocal) extends ScopeVariable {
    val typeFullName: String = node.typeFullName
    val name: String         = node.name
  }
  final case class ScopeParameter(override val node: NewMethodParameterIn) extends ScopeVariable {
    val typeFullName: String = node.typeFullName
    val name: String         = node.name
  }
  final case class ScopeMember(override val node: NewMember, isStatic: Boolean) extends ScopeVariable {
    val typeFullName: String = node.typeFullName
    val name: String         = node.name
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
