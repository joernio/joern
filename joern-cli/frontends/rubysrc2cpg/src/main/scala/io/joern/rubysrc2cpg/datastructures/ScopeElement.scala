package io.joern.rubysrc2cpg.datastructures

import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.datastructures.{NamespaceLikeScope, TypedScopeElement}

/** The namespace.
  * @param fullName
  *   the namespace path.
  */
case class NamespaceScope(fullName: String) extends NamespaceLikeScope

/** A type-like scope with a full name.
  */
trait TypeLikeScope extends TypedScopeElement {
  def fullName: String
}

/** A module.
  *
  * @param fileName
  *   the relative file name.
  */
case class ModuleScope(fileName: String) extends TypeLikeScope {
  override def fullName: String = s"$fileName:${Defines.Program}"
}

/** A class or interface.
  *
  * @param fullName
  *   the type full name.
  */
case class TypeScope(fullName: String) extends TypeLikeScope

/** Represents scope objects that map to a method node.
  *
  * @param fullName
  *   the method full name.
  */
case class MethodScope(fullName: String) extends TypedScopeElement

/** Represents scope objects that map to a block node.
  */
object BlockScope extends TypedScopeElement
