package io.joern.dataflowengineoss.language

import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Literal}
import io.joern.dataflowengineoss.queryengine.{Engine, EngineContext, PathElement, ReachableByResult, ResultTable}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import overflowdb.traversal._

import scala.collection.mutable

/** Base class for nodes that can occur in data flows
  */
class ExtendedCfgNode(val traversal: Traversal[CfgNode]) extends AnyVal {

  def ddgIn(implicit semantics: Semantics): Traversal[CfgNode] = {
    val cache  = mutable.HashMap[CfgNode, Vector[PathElement]]()
    val result = traversal.flatMap(x => x.ddgIn(Vector(PathElement(x)), withInvisible = false, cache))
    cache.clear()
    result
  }

  def ddgInPathElem(implicit semantics: Semantics): Traversal[PathElement] = {
    val cache  = mutable.HashMap[CfgNode, Vector[PathElement]]()
    val result = traversal.flatMap(x => x.ddgInPathElem(Vector(PathElement(x)), withInvisible = false, cache))
    cache.clear()
    result
  }

  def reachableBy[NodeType <: CfgNode](
    sourceTravs: Traversal[NodeType]*
  )(implicit context: EngineContext): Traversal[NodeType] = {
    val reachedSources = reachableByInternal(sourceTravsToList(sourceTravs)).map(_.source)
    Traversal.from(reachedSources).cast[NodeType]
  }

  def reachableByFlows[A <: CfgNode](sourceTravs: Traversal[A]*)(implicit context: EngineContext): Traversal[Path] = {
    val sources = sourceTravsToList(sourceTravs)
    val paths = reachableByInternal(sources)
      .map { result =>
        // We can get back results that start in nodes that are invisible
        // according to the semantic, e.g., arguments that are only used
        // but not defined. We filter these results here prior to returning
        val first = result.path.headOption
        if (first.isDefined && !first.get.visible && !sources.contains(first.get.node)) {
          None
        } else {
          val visiblePathElements = result.path.filter(_.visible)
          Some(Path(removeConsecutiveDuplicates(visiblePathElements.map(_.node))))
        }
      }
      .filter(_.isDefined)
      .dedup
      .flatten
    paths.to(Traversal)
  }

  def reachableByDetailed[NodeType <: CfgNode](
    sourceTravs: Traversal[NodeType]*
  )(implicit context: EngineContext): List[ReachableByResult] = {
    reachableByInternal(sourceTravsToList(sourceTravs))
  }

  private def removeConsecutiveDuplicates[T](l: Vector[T]): List[T] = {
    l.headOption.map(x => x :: l.sliding(2).collect { case Seq(a, b) if a != b => b }.toList).getOrElse(List())
  }

  private def reachableByInternal(sources: List[CfgNode])(implicit context: EngineContext): List[ReachableByResult] = {
    val sinks  = traversal.dedup.toList.sortBy(_.id)
    val engine = new Engine(context)
    val result = engine.backwards(sinks, sources)
    engine.shutdown()
    result
  }

  def sourceTravsToList[NodeType <: CfgNode](sourceTravs: Seq[Traversal[NodeType]]): List[CfgNode] = {
    sourceTravs
      .flatMap(_.toList)
      .collect { case n: CfgNode => n }
      .dedup
      .toList
      .sortBy(_.id)
  }

}
