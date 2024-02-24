package io.joern.rubysrc2cpg.datastructures

import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.datastructures.{NamespaceLikeScope, TypedScopeElement}
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock

/** The namespace.
  * @param fullName
  *   the namespace path.
  */
case class NamespaceScope(fullName: String) extends NamespaceLikeScope

/** A type-like scope with a full name.
  */
trait TypeLikeScope extends TypedScopeElement {

  /** @return
    *   the full name of the type-like.
    */
  def fullName: String
}

/** A file-level module.
  *
  * @param fileName
  *   the relative file name.
  */
case class ProgramScope(fileName: String) extends TypeLikeScope {
  override def fullName: String = s"$fileName:${Defines.Program}"
}

/** A Ruby module/abstract class.
  * @param fullName
  *   the type full name.
  */
case class ModuleScope(fullName: String) extends TypeLikeScope

/** A class or interface.
  *
  * @param fullName
  *   the type full name.
  */
case class TypeScope(fullName: String) extends TypeLikeScope

/** Represents scope objects that map to a method node.
  */
trait MethodLikeScope extends TypedScopeElement {
  def fullName: String
}

case class MethodScope(fullName: String) extends MethodLikeScope

case class ConstructorScope(fullName: String) extends MethodLikeScope

/** Represents scope objects that map to a block node.
  */
case class BlockScope(block: NewBlock) extends TypedScopeElement
