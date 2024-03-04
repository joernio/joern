package io.joern.rubysrc2cpg.datastructures

import io.joern.rubysrc2cpg.astcreation.GlobalTypes
import io.joern.x2cpg.Defines
import io.joern.x2cpg.datastructures.*
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{DeclarationNew, NewLocal, NewMethodParameterIn}

import scala.collection.mutable

class RubyScope(summary: RubyProgramSummary)
    extends Scope[String, DeclarationNew, TypedScopeElement]
    with TypedScope[RubyMethod, RubyField, RubyType](summary) {

  private val builtinMethods = GlobalTypes.builtinFunctions.map(m => RubyMethod(m, List.empty, Defines.Any)).toList

  override val typesInScope: mutable.Set[RubyType] =
    mutable.Set(RubyType(GlobalTypes.builtinPrefix, builtinMethods, List.empty))

  override val membersInScope: mutable.Set[MemberLike] = mutable.Set(builtinMethods*)

  // Ruby does not have overloading, so this can be set to true
  override protected def isOverloadedBy(method: RubyMethod, argTypes: List[String]): Boolean = true

  /** @return
    *   using the stack, will initialize a new module scope object.
    */
  def newProgramScope: Option[ProgramScope] = surroundingScopeFullName.map(ProgramScope.apply)

  override def pushNewScope(scopeNode: TypedScopeElement): Unit = {
    // Use the summary to determine if there is a constructor present
    val mappedScopeNode = scopeNode match {
      case n: NamespaceLikeScope =>
        typesInScope.addAll(summary.typesUnderNamespace(n.fullName))
        n
      case n: ProgramScope =>
        typesInScope.addAll(summary.typesUnderNamespace(n.fullName))
        n
      case _ => scopeNode
    }

    super.pushNewScope(mappedScopeNode)
  }

  /** @return
    *   the full name of the surrounding scope.
    */
  def surroundingScopeFullName: Option[String] = stack.collectFirst {
    case ScopeElement(x: NamespaceLikeScope, _) => x.fullName
    case ScopeElement(x: TypeLikeScope, _)      => x.fullName
    case ScopeElement(x: MethodLikeScope, _)    => x.fullName
  }

  def surroundingTypeFullName: Option[String] = stack.collectFirst { case ScopeElement(x: TypeLikeScope, _) =>
    x.fullName
  }

  /** @return
    *   the corresponding node label according to the scope element.
    */
  def surroundingAstLabel: Option[String] = stack.collectFirst {
    case ScopeElement(_: NamespaceLikeScope, _) => NodeTypes.NAMESPACE_BLOCK
    case ScopeElement(_: ProgramScope, _)       => NodeTypes.METHOD
    case ScopeElement(_: TypeLikeScope, _)      => NodeTypes.TYPE_DECL
    case ScopeElement(_: MethodLikeScope, _)    => NodeTypes.METHOD
  }

  /** @return
    *   true if one should still generate a default constructor for the enclosing type decl.
    */
  def shouldGenerateDefaultConstructor: Boolean = stack
    .collectFirst {
      case ScopeElement(_: ModuleScope, _)   => false
      case ScopeElement(x: TypeLikeScope, _) => !typesInScope.find(_.name == x.fullName).exists(_.hasConstructor)
      case _                                 => false
    }
    .getOrElse(false)

  /** When a singleton class is introduced into the scope, the base variable will now have the singleton's functionality
    * mixed in. This method finds base variable and appends the singleton type.
    *
    * @param singletonClassName
    *   the singleton type full name.
    * @param variableName
    *   the base variable
    */
  def pushSingletonClassDeclaration(singletonClassName: String, variableName: String): Unit = {
    lookupVariable(variableName).foreach {
      case local: NewLocal =>
        local.possibleTypes(local.possibleTypes :+ singletonClassName)
      case param: NewMethodParameterIn => param.possibleTypes(param.possibleTypes :+ singletonClassName)
      case _                           =>
    }
  }

  override def tryResolveTypeReference(typeName: String): Option[RubyType] = {
    // TODO: While we find better ways to understand how the implicit class loading works,
    //  we can approximate that all types are in scope in the mean time.
    super.tryResolveTypeReference(typeName) match {
      case None =>
        summary.namespaceToType.flatMap(_._2).collectFirst {
          case x if x.name.split("[.]").lastOption.contains(typeName) =>
            typesInScope.addOne(x)
            x
        }
      case x => x
    }
  }
}
