package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class TypeTraversal(val traversal: Iterator[Type]) extends AnyVal {

  /** Annotations of the corresponding type declaration.
    */
  def annotation: Iterator[Annotation] =
    traversal.referencedTypeDecl.annotation

  /** Namespaces in which the corresponding type declaration is defined.
    */
  def namespace: Iterator[Namespace] =
    traversal.referencedTypeDecl.namespace

  /** Methods defined on the corresponding type declaration.
    */
  def method: Iterator[Method] =
    traversal.referencedTypeDecl.method

  /** Filter for types whos corresponding type declaration is in the analyzed jar.
    */
  def internal: Iterator[Type] =
    traversal.where(_.referencedTypeDecl.internal)

  /** Filter for types whos corresponding type declaration is not in the analyzed jar.
    */
  def external: Iterator[Type] =
    traversal.where(_.referencedTypeDecl.external)

  /** Member variables of the corresponding type declaration.
    */
  def member: Iterator[Member] =
    traversal.referencedTypeDecl.member

  /** Direct base types of the corresponding type declaration in the inheritance graph.
    */
  def baseType: Iterator[Type] =
    traversal.referencedTypeDecl.baseType

  /** Direct and transitive base types of the corresponding type declaration.
    */
  def baseTypeTransitive: Iterator[Type] =
    traversal.repeat(_.baseType)(_.emitAllButFirst.dedup)

  /** Direct derived types.
    */
  def derivedType: Iterator[Type] =
    derivedTypeDecl.referencingType

  /** Direct and transitive derived types.
    */
  def derivedTypeTransitive: Iterator[Type] =
    traversal.repeat(_.derivedType)(_.emitAllButFirst.dedup)

  /** Type declarations which derive from this type.
    */
  def derivedTypeDecl: Iterator[TypeDecl] =
    traversal.inheritsFromIn

  /** Direct alias types.
    */
  def aliasType: Iterator[Type] =
    traversal.aliasTypeDecl.referencingType

  /** Direct and transitive alias types.
    */
  def aliasTypeTransitive: Iterator[Type] =
    traversal.repeat(_.aliasType)(_.emitAllButFirst.dedup)

  def localOfType: Iterator[Local] =
    traversal._localViaEvalTypeIn

  def memberOfType: Iterator[Member] =
    traversal.evalTypeIn.collectAll[Member]

  @deprecated("Please use `parameterOfType`")
  def parameter: Iterator[MethodParameterIn] = parameterOfType

  def parameterOfType: Iterator[MethodParameterIn] =
    traversal.evalTypeIn.collectAll[MethodParameterIn]

  def methodReturnOfType: Iterator[MethodReturn] =
    traversal.evalTypeIn.collectAll[MethodReturn]

  def expressionOfType: Iterator[Expression] = expression

  // TODO define in schema
  def expression: Iterator[Expression] =
    traversal.evalTypeIn.collectAll[Expression]

}
