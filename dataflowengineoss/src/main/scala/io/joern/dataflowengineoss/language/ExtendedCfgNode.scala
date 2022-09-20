package io.joern.dataflowengineoss.language

import io.joern.dataflowengineoss.language.SourceToStartingPoints.sourceTravsToStartingPoints
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Expression, Identifier, Literal, Member, TypeDecl}
import io.joern.dataflowengineoss.queryengine.{Engine, EngineContext, PathElement, ReachableByResult}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.Cpg
import overflowdb.traversal._
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable

case class StartingPointWithSource(startingPoint: CfgNode, source: CfgNode)

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

  def reachableBy[NodeType](sourceTravs: Traversal[NodeType]*)(implicit context: EngineContext): Traversal[NodeType] = {
    val reachedSources =
      reachableByInternal(sourceTravsToStartingPoints(sourceTravs)).map(_.startingPoint)
    Traversal.from(reachedSources).cast[NodeType]
  }

  def reachableByFlows[A](sourceTravs: Traversal[A]*)(implicit context: EngineContext): Traversal[Path] = {
    val sources        = sourceTravsToStartingPoints(sourceTravs)
    val startingPoints = sources.map(_.startingPoint)
    val paths = reachableByInternal(sources)
      .map { result =>
        // We can get back results that start in nodes that are invisible
        // according to the semantic, e.g., arguments that are only used
        // but not defined. We filter these results here prior to returning
        val first = result.path.headOption
        if (first.isDefined && !first.get.visible && !startingPoints.contains(first.get.node)) {
          None
        } else {
          val visiblePathElements = result.path.filter(x => startingPoints.contains(x.node) || x.visible)
          Some(Path(removeConsecutiveDuplicates(visiblePathElements.map(_.node))))
        }
      }
      .filter(_.isDefined)
      .dedup
      .flatten
    paths.to(Traversal)
  }

  def reachableByDetailed[NodeType](
    sourceTravs: Traversal[NodeType]*
  )(implicit context: EngineContext): List[ReachableByResult] = {
    reachableByInternal(sourceTravsToStartingPoints(sourceTravs))
  }

  private def removeConsecutiveDuplicates[T](l: Vector[T]): List[T] = {
    l.headOption.map(x => x :: l.sliding(2).collect { case Seq(a, b) if a != b => b }.toList).getOrElse(List())
  }

  private def reachableByInternal(
    startingPointsWithSources: List[StartingPointWithSource]
  )(implicit context: EngineContext): List[ReachableByResult] = {
    val sinks  = traversal.dedup.toList.sortBy(_.id)
    val engine = new Engine(context)
    val result = engine.backwards(sinks, startingPointsWithSources.map(_.startingPoint))

    engine.shutdown()
    val sources               = startingPointsWithSources.map(_.source)
    val startingPointToSource = startingPointsWithSources.map { x => x.startingPoint -> x.source }.toMap
    result.map { r =>
      if (sources.contains(r.startingPoint)) {
        r
      } else {
        r.copy(path = PathElement(startingPointToSource(r.startingPoint)) +: r.path)
      }
    }
  }
}
