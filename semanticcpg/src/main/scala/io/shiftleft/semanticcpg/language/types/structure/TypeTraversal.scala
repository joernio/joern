package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

class TypeTraversal(val traversal: Traversal[Type]) extends AnyVal {

  /** Annotations of the corresponding type declaration.
    */
  def annotation: Traversal[nodes.Annotation] =
    traversal.referencedTypeDecl.annotation

  /** Namespaces in which the corresponding type declaration is defined.
    */
  def namespace: Traversal[Namespace] =
    traversal.referencedTypeDecl.namespace

  /** Methods defined on the corresponding type declaration.
    */
  def method: Traversal[Method] =
    traversal.referencedTypeDecl.method

  /** Filter for types whos corresponding type declaration is in the analyzed jar.
    */
  def internal: Traversal[Type] =
    traversal.where(_.referencedTypeDecl.internal)

  /** Filter for types whos corresponding type declaration is not in the analyzed jar.
    */
  def external: Traversal[Type] =
    traversal.where(_.referencedTypeDecl.external)

  /** Member variables of the corresponding type declaration.
    */
  def member: Traversal[Member] =
    traversal.referencedTypeDecl.member

  /** Direct base types of the corresponding type declaration in the inheritance graph.
    */
  def baseType: Traversal[Type] =
    traversal.referencedTypeDecl.baseType

  /** Direct and transitive base types of the corresponding type declaration.
    */
  def baseTypeTransitive: Traversal[Type] =
    traversal.repeat(_.baseType)(_.emitAllButFirst)

  /** Direct derived types.
    */
  def derivedType: Traversal[Type] =
    derivedTypeDecl.referencingType

  /** Direct and transitive derived types.
    */
  def derivedTypeTransitive: Traversal[Type] =
    traversal.repeat(_.derivedType)(_.emitAllButFirst)

  /** Type declarations which derive from this type.
    */
  def derivedTypeDecl: Traversal[TypeDecl] =
    traversal.flatMap(_.inheritsFromIn)

  /** Direct alias types.
    */
  def aliasType: Traversal[Type] =
    traversal.aliasTypeDecl.referencingType

  /** Direct and transitive alias types.
    */
  def aliasTypeTransitive: Traversal[Type] =
    traversal.repeat(_.aliasType)(_.emitAllButFirst)

  def localOfType: Traversal[Local] =
    traversal.flatMap(_._localViaEvalTypeIn)

  def memberOfType: Traversal[Member] =
    traversal.flatMap(_.evalTypeIn).collectAll[Member]

  @deprecated("Please use `parameterOfType`")
  def parameter: Traversal[MethodParameterIn] = parameterOfType

  def parameterOfType: Traversal[MethodParameterIn] =
    traversal.flatMap(_.evalTypeIn).collectAll[MethodParameterIn]

  def methodReturnOfType: Traversal[MethodReturn] =
    traversal.flatMap(_.evalTypeIn).collectAll[MethodReturn]

  def expressionOfType: Traversal[Expression] = expression

  def expression: Traversal[Expression] =
    traversal.flatMap(_.evalTypeIn).collectAll[Expression]

}
