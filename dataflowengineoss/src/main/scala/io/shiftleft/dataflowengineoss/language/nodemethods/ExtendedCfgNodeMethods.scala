package io.shiftleft.dataflowengineoss.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.dataflowengineoss.queryengine.{Engine, EngineContext, PathElement}
import io.shiftleft.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.language.{toExpressionMethods, _}
import io.shiftleft.semanticcpg.utils.MemberAccess
import overflowdb.traversal.{Traversal, _}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class ExtendedCfgNodeMethods[NodeType <: CfgNode](val node: NodeType) extends AnyVal {

  import ExtendedCfgNodeMethods._

  /**
    * Convert to nearest AST node
    * */
  def astNode: AstNode =
    node match {
      case n: AstNode => n
      case _          => ??? //TODO markus/fabs?
    }

  def reachableBy[NodeType <: CfgNode](sourceTravs: Traversal[NodeType]*)(
      implicit context: EngineContext): Traversal[NodeType] =
    node.start.reachableBy(sourceTravs: _*)

  def ddgIn(implicit semantics: Semantics): Traversal[CfgNode] = {
    val cache = mutable.HashMap[CfgNode, Vector[PathElement]]()
    val result = ddgIn(Vector(PathElement(node)), withInvisible = false, cache)
    cache.clear()
    result
  }

  def ddgInPathElem(withInvisible: Boolean,
                    cache: mutable.HashMap[CfgNode, Vector[PathElement]] =
                      mutable.HashMap[CfgNode, Vector[PathElement]]())(
      implicit semantics: Semantics): Traversal[PathElement] =
    ddgInPathElem(Vector(PathElement(node)), withInvisible, cache)

  def ddgInPathElem(implicit semantics: Semantics): Traversal[PathElement] = {
    val cache = mutable.HashMap[CfgNode, Vector[PathElement]]()
    val result = ddgInPathElem(Vector(PathElement(node)), withInvisible = false, cache)
    cache.clear()
    result
  }

  /**
    * Traverse back in the data dependence graph by one step, taking into account semantics
    * @param path optional list of path elements that have been expanded already
    * */
  def ddgIn(path: Vector[PathElement], withInvisible: Boolean, cache: mutable.HashMap[CfgNode, Vector[PathElement]])(
      implicit semantics: Semantics): Traversal[CfgNode] = {
    ddgInPathElem(path, withInvisible, cache).map(_.node)
  }

  /**
    * Traverse back in the data dependence graph by one step and generate corresponding PathElement,
    * taking into account semantics
    * @param path optional list of path elements that have been expanded already
    * */
  def ddgInPathElem(
      path: Vector[PathElement],
      withInvisible: Boolean,
      cache: mutable.HashMap[CfgNode, Vector[PathElement]])(implicit semantics: Semantics): Traversal[PathElement] = {
    val result = ddgInPathElemInternal(path, withInvisible, cache).to(Traversal)
    result
  }

  private def ddgInPathElemInternal(
      path: Vector[PathElement],
      withInvisible: Boolean,
      cache: mutable.HashMap[CfgNode, Vector[PathElement]])(implicit semantics: Semantics): Vector[PathElement] = {

    if (cache.contains(node)) {
      return cache(node)
    }

    val elems = Engine.expandIn(node, path)
    val result = if (withInvisible) {
      elems
    } else {
      (elems.filter(_.visible) ++ elems
        .filterNot(_.visible)
        .flatMap(x => x.node.ddgInPathElem(x +: path, withInvisible = false, cache))).distinct
    }
    cache.put(node, result)
    result
  }

  def statement: CfgNode =
    statementInternal(node, _.parentExpression.get)

  @scala.annotation.tailrec
  private def statementInternal(node: CfgNode, parentExpansion: Expression => Expression): CfgNode = {

    node match {
      case node: Identifier => parentExpansion(node)
      case node: MethodRef  => parentExpansion(node)
      case node: TypeRef    => parentExpansion(node)
      case node: Literal    => parentExpansion(node)

      case node: MethodParameterIn => node.method

      case node: MethodParameterOut =>
        node.method.methodReturn

      case node: Call if MemberAccess.isGenericMemberAccessName(node.name) =>
        parentExpansion(node)

      case node: CallRepr     => node
      case node: MethodReturn => node
      case block: Block       =>
        // Just taking the lastExpressionInBlock is not quite correct because a BLOCK could have
        // different return expressions. So we would need to expand via CFG.
        // But currently the frontends do not even put the BLOCK into the CFG so this is the best
        // we can do.
        statementInternal(lastExpressionInBlock(block).get, identity)
      case node: Expression => node
    }
  }
}

object ExtendedCfgNodeMethods {
  private def lastExpressionInBlock(block: Block): Option[Expression] =
    block._astOut.asScala
      .collect {
        case node: Expression if !node.isInstanceOf[Local] && !node.isInstanceOf[Method] => node
      }
      .toVector
      .sortBy(_.order)
      .lastOption
}
