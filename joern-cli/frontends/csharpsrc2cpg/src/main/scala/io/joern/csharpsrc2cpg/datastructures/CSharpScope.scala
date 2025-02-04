package io.joern.csharpsrc2cpg.datastructures

import io.joern.csharpsrc2cpg.Constants
import io.joern.csharpsrc2cpg.utils.Utils
import io.joern.x2cpg.Defines
import io.joern.x2cpg.datastructures.{OverloadableScope, Scope, ScopeElement, TypedScope, TypedScopeElement}
import io.joern.x2cpg.utils.ListUtils.singleOrNone
import io.shiftleft.codepropertygraph.generated.nodes.DeclarationNew
import io.joern.x2cpg.utils.ListUtils.singleOrNone

import scala.collection.mutable
import scala.reflect.ClassTag

class CSharpScope(summary: CSharpProgramSummary)
    extends Scope[String, DeclarationNew, TypedScopeElement]
    with TypedScope[CSharpMethod, CSharpField, CSharpType](summary)
    with OverloadableScope[CSharpMethod] {

  override val typesInScope: mutable.Set[CSharpType] = mutable.Set
    .empty[CSharpType]
    .addAll(summary.findGlobalTypes)
    .addAll(summary.globalImports.flatMap(summary.namespaceToType.getOrElse(_, Set.empty)))

  /** @return
    *   the surrounding type declaration if one exists.
    */
  def surroundingTypeDeclFullName: Option[String] = stack.collectFirst {
    case ScopeElement(typeLike: TypeLikeScope, _) =>
      typeLike.fullName
  }

  def getFieldsInScope: List[FieldDecl] =
    stack.collect { case ScopeElement(TypeScope(_, fields), _) => fields }.flatten

  /** Works for `this`.field accesses or <currentType>.field accesses.
    */
  def findFieldInScope(fieldName: String): Option[FieldDecl] = {
    getFieldsInScope.find(_.name == fieldName)
  }

  override def isOverloadedBy(method: CSharpMethod, argTypes: List[String]): Boolean = {
    method.parameterTypes
      .filterNot(_._1 == Constants.This)
      .map(_._2)
      .zip(argTypes)
      .count({ case (x, y) => x != y }) == 0
  }

  /** @return
    *   the full name of the surrounding scope.
    */
  def surroundingScopeFullName: Option[String] = stack.collectFirst {
    case ScopeElement(NamespaceScope(fullName), _) => fullName
    case ScopeElement(MethodScope(fullName), _)    => fullName
    case ScopeElement(TypeScope(fullName, _), _)   => fullName
    case ScopeElement(typeLike: TypeLikeScope, _)  => typeLike.fullName
  }

  /** @return
    *   true if the scope is currently on the top-level, false if the scope is within some nested scope.
    */
  def isTopLevel: Boolean = stack
    .filterNot(x => x.scopeNode.isInstanceOf[NamespaceScope])
    .exists(x => x.scopeNode.isInstanceOf[MethodScope] || x.scopeNode.isInstanceOf[TypeLikeScope])

  override def tryResolveTypeReference(typeName: String): Option[CSharpType] = {
    if (typeName == Constants.This) {
      surroundingTypeDeclFullName.flatMap(summary.matchingTypes).headOption
    } else {
      super.tryResolveTypeReference(typeName) match
        case Some(x) => Some(x)
        case None    =>
          // typeName might be a fully-qualified name e.g. System.Console, in which case, even if we
          // don't import System (i.e. System is not in typesInScope), we should still find it if it's
          // in the type summaries and there's exactly 1 match.
          Some(typeName).filter(_.contains(".")).flatMap(summary.matchingTypes.andThen(singleOrNone))
    }
  }

  // TODO: Add inherits field in CSharpType and handle this in `pushNewScope`
  def pushTypeToScope(typeFullName: String): Unit = {
    typesInScope.addAll(summary.matchingTypes(typeFullName))
  }

  def pushField(field: FieldDecl): Unit = {
    popScope().foreach {
      case TypeScope(fullName, fields) =>
        pushNewScope(TypeScope(fullName, fields :+ field))
      case x =>
        pushField(field)
        pushNewScope(x)
    }
  }

  /** Pops the scope, adding types from the scope if necessary.
    */
  override def pushNewScope(scopeNode: TypedScopeElement): Unit = {
    scopeNode match
      case NamespaceScope(fullName) => typesInScope.addAll(summary.typesUnderNamespace(fullName))
      case TypeScope(name, _)       => typesInScope.addAll(summary.matchingTypes(name))
      case _                        =>
    super.pushNewScope(scopeNode)
  }

  /** Pops the scope, removing types from the scope if necessary.
    */
  override def popScope(): Option[TypedScopeElement] = {
    super.popScope() match
      case Some(NamespaceScope(fullName)) =>
        summary.typesUnderNamespace(fullName).foreach(typesInScope.remove)
        Some(NamespaceScope(fullName))
      case x => x
  }

  /** Returns the top of the scope, without removing it from the stack.
    */
  def peekScope(): Option[TypedScopeElement] = {
    super.popScope() match
      case None => None
      case Some(top) =>
        super.pushNewScope(top)
        Option(top)
  }

  /** Reduces [[typesInScope]] to contain only those types holding an extension method with the desired signature.
    */
  private def extensionsInScopeFor(
    extendedType: String,
    callName: String,
    argTypes: List[String]
  ): mutable.Set[CSharpType] = {
    typesInScope
      .map(t => t.copy(methods = t.methods.filter(matchingExtensionMethod(extendedType, callName, argTypes))))
      .filter(_.methods.nonEmpty)
  }

  /** Builds a predicate for matching [[CSharpMethod]] with an ad-hoc description of theirs.
    */
  private def matchingExtensionMethod(
    thisType: String,
    name: String,
    argTypes: List[String]
  ): CSharpMethod => Boolean = { m =>
    // TODO: we should also compare argTypes, however we first need to account for:
    //  a) default valued parameters in CSharpMethod, to account for different arities
    //  b) compatible/sub types, i.e. System.String should unify with System.Object.
    m.isStatic && m.name == name && m.parameterTypes.map(_._2).headOption.contains(thisType)
  }

  /** Tries to find an extension method for [[baseTypeFullName]] with the given [[callName]] and [[argTypes]] in the
    * types currently in scope.
    *
    * @param baseTypeFullName
    *   the extension method's `this` argument.
    * @param callName
    *   the method name
    * @param argTypes
    *   the method's argument types, excluding `this`
    * @return
    *   the method metadata, together with the class name where it can be found
    */
  def tryResolveExtensionMethodInvocation(
    baseTypeFullName: Option[String],
    callName: String,
    argTypes: List[String]
  ): Option[(CSharpMethod, String)] = {
    baseTypeFullName.flatMap(extensionsInScopeFor(_, callName, argTypes).headOption).map(x => (x.methods.head, x.name))
  }

  def tryResolveGetterInvocation(
    fieldIdentifierName: String,
    baseTypeFullName: Option[String]
  ): Option[CSharpMethod] = {
    val getterMethodName = Utils.composeGetterName(fieldIdentifierName)
    tryResolveMethodInvocation(getterMethodName, Nil, baseTypeFullName)
  }

  def tryResolveSetterInvocation(
    fieldIdentifierName: String,
    baseTypeFullName: Option[String]
  ): Option[CSharpMethod] = {
    val setterMethodName = Utils.composeSetterName(fieldIdentifierName)
    tryResolveMethodInvocation(setterMethodName, Defines.Any :: Nil, baseTypeFullName)
  }
}
