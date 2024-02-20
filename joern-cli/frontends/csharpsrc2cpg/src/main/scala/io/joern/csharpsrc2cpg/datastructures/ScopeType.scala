package io.joern.csharpsrc2cpg.datastructures

import io.joern.csharpsrc2cpg.parser.DotNetNodeInfo
import io.joern.csharpsrc2cpg.astcreation.BuiltinTypes
import io.joern.csharpsrc2cpg.astcreation.BuiltinTypes.DotNetTypeMap
import io.joern.x2cpg.datastructures.{NamespaceLikeScope, TypedScopeElement}

/** Represents scope objects mapping to namespace nodes. Likened to `namespace` or `package` declarations.
  *
  * @param fullName
  *   the fully qualified name.
  */
case class NamespaceScope(fullName: String) extends NamespaceLikeScope

case class FieldDecl(
  name: String,
  typeFullName: String,
  isStatic: Boolean,
  isInitialized: Boolean,
  node: DotNetNodeInfo
) extends TypedScopeElement

/** Represents scope objects that map to a type declaration node.
  */
sealed trait TypeLikeScope extends TypedScopeElement {
  def fullName: String
}

/** A class or interface.
  *
  * @param fullName
  *   the type full name.
  */
case class TypeScope(fullName: String, fields: List[FieldDecl] = List.empty) extends TypeLikeScope

/** An enumeration type.
  *
  * @param fullName
  *   the enum full name
  * @param aliasFor
  *   the integer equivalent type that this represents
  */
case class EnumScope(fullName: String, aliasFor: String = DotNetTypeMap(BuiltinTypes.Int)) extends TypeLikeScope

/** Represents scope objects that map to a method node.
  *
  * @param fullName
  *   the method full name.
  */
case class MethodScope(fullName: String) extends TypedScopeElement

/** Represents scope objects that map to a block node.
  */
object BlockScope extends TypedScopeElement
