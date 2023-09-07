package io.joern.dataflowengineoss.language

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.queryengine.*
import io.joern.dataflowengineoss.queryengine.SourcesToStartingPoints.sourceTravsToStartingPoints
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*

/** Base class for nodes that can occur in data flows
  */
class ExtendedCfgNode(val traversal: Iterator[CfgNode]) extends AnyVal {

  def ddgIn(implicit semantics: Semantics = DefaultSemantics()): Iterator[CfgNode] = {
    val cache  = mutable.HashMap[CfgNode, Vector[PathElement]]()
    val result = traversal.flatMap(x => x.ddgIn(Vector(PathElement(x)), withInvisible = false, cache))
    cache.clear()
    result
  }

  def ddgInPathElem(implicit semantics: Semantics = DefaultSemantics()): Iterator[PathElement] = {
    val cache  = mutable.HashMap[CfgNode, Vector[PathElement]]()
    val result = traversal.flatMap(x => x.ddgInPathElem(Vector(PathElement(x)), withInvisible = false, cache))
    cache.clear()
    result
  }

  def reachableBy[NodeType](sourceTrav: IterableOnce[NodeType], sourceTravs: IterableOnce[NodeType]*)(implicit
    context: EngineContext
  ): Iterator[NodeType] = {
    val sources = sourceTravsToStartingPoints(sourceTrav +: sourceTravs: _*)
    val reachedSources =
      reachableByInternal(sources).map(_.path.head.node)
    reachedSources.cast[NodeType]
  }

  def reachableByFlows[A](sourceTrav: IterableOnce[A], sourceTravs: IterableOnce[A]*)(implicit
    context: EngineContext
  ): Iterator[Path] = {
    val sources        = sourceTravsToStartingPoints(sourceTrav +: sourceTravs: _*)
    val startingPoints = sources.map(_.startingPoint)
    val paths = reachableByInternal(sources).par
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
      .toVector
    paths.iterator
  }

  def reachableByDetailed[NodeType](sourceTrav: Iterator[NodeType], sourceTravs: Iterator[NodeType]*)(implicit
    context: EngineContext
  ): Vector[TableEntry] = {
    val sources = SourcesToStartingPoints.sourceTravsToStartingPoints(sourceTrav +: sourceTravs: _*)
    reachableByInternal(sources)
  }

  private def removeConsecutiveDuplicates[T](l: Vector[T]): List[T] = {
    l.headOption.map(x => x :: l.sliding(2).collect { case Seq(a, b) if a != b => b }.toList).getOrElse(List())
  }

  private def reachableByInternal(
    startingPointsWithSources: List[StartingPointWithSource]
  )(implicit context: EngineContext): Vector[TableEntry] = {
    val sinks  = traversal.dedup.toList.sortBy(_.id)
    val engine = new Engine(context)
    val result = engine.backwards(sinks, startingPointsWithSources.map(_.startingPoint))

    engine.shutdown()
    val sources = startingPointsWithSources.map(_.source)
    val startingPointToSource = startingPointsWithSources.map { x =>
      x.startingPoint.asInstanceOf[AstNode] -> x.source
    }.toMap
    val res = result.par.map { r =>
      val startingPoint = r.path.head.node
      if (sources.contains(startingPoint) || !startingPointToSource(startingPoint).isInstanceOf[AstNode]) {
        r
      } else {
        r.copy(path = PathElement(startingPointToSource(startingPoint).asInstanceOf[AstNode]) +: r.path)
      }
    }
    res.toVector
  }

}
