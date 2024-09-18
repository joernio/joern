package io.joern.dataflowengineoss.language.nodemethods

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.{Engine, EngineContext, PathElement}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class ExtendedCfgNodeMethods[CfgNodeType <: CfgNode](val node: CfgNodeType) extends AnyVal {

  /** Convert to nearest AST node
    */
  def astNode: AstNode = node

  def reachableBy[NodeType](sourceTrav: Iterator[NodeType], sourceTravs: IterableOnce[NodeType]*)(implicit
    context: EngineContext
  ): Iterator[NodeType] =
    node.start.reachableBy(sourceTrav, sourceTravs*)

  def ddgIn(implicit semantics: Semantics = DefaultSemantics()): Iterator[CfgNode] = {
    val cache  = mutable.HashMap[CfgNode, Vector[PathElement]]()
    val result = ddgIn(Vector(PathElement(node)), withInvisible = false, cache)
    cache.clear()
    result
  }

  def ddgInPathElem(
    withInvisible: Boolean,
    cache: mutable.HashMap[CfgNode, Vector[PathElement]] = mutable.HashMap[CfgNode, Vector[PathElement]]()
  )(implicit semantics: Semantics): Iterator[PathElement] =
    ddgInPathElem(Vector(PathElement(node)), withInvisible, cache)

  def ddgInPathElem(implicit semantics: Semantics): Iterator[PathElement] = {
    val cache  = mutable.HashMap[CfgNode, Vector[PathElement]]()
    val result = ddgInPathElem(Vector(PathElement(node)), withInvisible = false, cache)
    cache.clear()
    result
  }

  /** Traverse back in the data dependence graph by one step, taking into account semantics
    * @param path
    *   optional list of path elements that have been expanded already
    */
  def ddgIn(path: Vector[PathElement], withInvisible: Boolean, cache: mutable.HashMap[CfgNode, Vector[PathElement]])(
    implicit semantics: Semantics
  ): Iterator[CfgNode] = {
    ddgInPathElem(path, withInvisible, cache).map(_.node.asInstanceOf[CfgNode])
  }

  /** Traverse back in the data dependence graph by one step and generate corresponding PathElement, taking into account
    * semantics
    * @param path
    *   optional list of path elements that have been expanded already
    */
  def ddgInPathElem(
    path: Vector[PathElement],
    withInvisible: Boolean,
    cache: mutable.HashMap[CfgNode, Vector[PathElement]]
  )(implicit semantics: Semantics): Iterator[PathElement] = {
    val result = ddgInPathElemInternal(path, withInvisible, cache).iterator
    result
  }

  private def ddgInPathElemInternal(
    path: Vector[PathElement],
    withInvisible: Boolean,
    cache: mutable.HashMap[CfgNode, Vector[PathElement]]
  )(implicit semantics: Semantics): Vector[PathElement] = {

    if (cache.contains(node)) {
      return cache(node)
    }

    val elems = Engine.expandIn(node, path)
    val result = if (withInvisible) {
      elems
    } else {
      (elems.filter(_.visible) ++ elems
        .filterNot(_.visible)
        .flatMap(x => x.node.asInstanceOf[CfgNode].ddgInPathElem(x +: path, withInvisible = false, cache))).distinct
    }
    cache.put(node, result)
    result
  }

}
