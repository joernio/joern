package io.joern.csharpsrc2cpg.datastructures

import io.joern.x2cpg.Defines
import io.joern.x2cpg.datastructures.{OverloadableScope, Scope, ScopeElement, TypedScope, TypedScopeElement}
import io.shiftleft.codepropertygraph.generated.nodes.DeclarationNew

import scala.collection.mutable

class CSharpScope(summary: CSharpProgramSummary)
    extends Scope[String, DeclarationNew, TypedScopeElement]
    with TypedScope[CSharpMethod, CSharpField, CSharpType](summary)
    with OverloadableScope[CSharpMethod] {

  override val typesInScope: mutable.Set[CSharpType] = mutable.Set.empty[CSharpType].addAll(summary.findGlobalTypes)

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
      .filterNot(_._1 == "this")
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
    if (typeName == "this") {
      surroundingTypeDeclFullName.flatMap(summary.matchingTypes).headOption
    } else {
      super.tryResolveTypeReference(typeName)
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

}
