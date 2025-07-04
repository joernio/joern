package io.joern.php2cpg.utils

import io.joern.php2cpg.parser.Domain.MethodDelimiter
import io.joern.x2cpg.datastructures.{NamespaceLikeScope, TypedScopeElement}
import io.shiftleft.codepropertygraph.generated.nodes.{
  MethodRef,
  NewBlock,
  NewMethod,
  NewMethodRef,
  NewNamespaceBlock,
  NewNode,
  NewTypeDecl
}

sealed trait AnonymousClassNameCreator {
  private var tmpClassCounter = 0

  def getNextClassTmp: String = {
    val anonClassName = s"anon-class-$tmpClassCounter"
    tmpClassCounter = tmpClassCounter + 1

    anonClassName
  }
}

sealed trait AnonymousVariableNameCreator {
  private var tmpVarCounter = 0

  def getNextVarTmp: String = {
    val tmpVarName = s"tmp-$tmpVarCounter"
    tmpVarCounter = tmpVarCounter + 1

    tmpVarName
  }
}

sealed trait ClosureNameCreator {
  def fullName: String
  def getClosureMethodName()(using nextClosureName: () => String): String = {
    s"$fullName$MethodDelimiter${nextClosureName()}"
  }
}

trait NamedScope extends TypedScopeElement {

  /** @return
    *   the full name of the type-like.
    */
  def fullName: String
}

/** The namespace.
  * @param fullName
  *   the namespace path.
  */
case class NamespaceScope(namespaceBlock: NewNamespaceBlock, fullName: String)
    extends NamedScope
    with AnonymousClassNameCreator
    with AnonymousVariableNameCreator
    with ClosureNameCreator

/** A type-like scope with a full name.
  */
trait TypeLikeScope
    extends NamedScope
    with AnonymousClassNameCreator
    with AnonymousVariableNameCreator
    with ClosureNameCreator

/** A class or interface.
  *
  * @param fullName
  *   the type full name.
  */
case class TypeScope(typeDecl: NewTypeDecl, fullName: String) extends TypeLikeScope

/** Represents scope objects that map to a method node.
  */
trait MethodLikeScope
    extends NamedScope
    with AnonymousClassNameCreator
    with AnonymousVariableNameCreator
    with ClosureNameCreator

case class MethodScope(
  methodNode: NewMethod,
  bodyNode: NewBlock,
  fullName: String,
  methodRefNode: Option[NewMethodRef] = None,
  isArrowFunc: Boolean = false
) extends MethodLikeScope

case class BlockScope(block: NewBlock, fullName: String) extends NamedScope
