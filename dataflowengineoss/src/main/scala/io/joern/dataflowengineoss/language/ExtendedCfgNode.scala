package io.joern.dataflowengineoss.language

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.queryengine.{Engine, EngineContext, PathElement, ReachableByResult}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import java.util.concurrent.{
  ForkJoinPool,
  ForkJoinTask,
  RecursiveTask,
  RejectedExecutionException,
  RejectedExecutionHandler
}
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

case class StartingPointWithSource(startingPoint: CfgNode, source: StoredNode)

/** Base class for nodes that can occur in data flows
  */
class ExtendedCfgNode(val traversal: Traversal[CfgNode]) extends AnyVal {

  def ddgIn(implicit semantics: Semantics = DefaultSemantics()): Traversal[CfgNode] = {
    val cache  = mutable.HashMap[CfgNode, Vector[PathElement]]()
    val result = traversal.flatMap(x => x.ddgIn(Vector(PathElement(x)), withInvisible = false, cache))
    cache.clear()
    result
  }

  def ddgInPathElem(implicit semantics: Semantics = DefaultSemantics()): Traversal[PathElement] = {
    val cache  = mutable.HashMap[CfgNode, Vector[PathElement]]()
    val result = traversal.flatMap(x => x.ddgInPathElem(Vector(PathElement(x)), withInvisible = false, cache))
    cache.clear()
    result
  }

  def reachableBy[NodeType](sourceTravs: Traversal[NodeType]*)(implicit context: EngineContext): Traversal[NodeType] = {
    val sources = ExtendedCfgNode.sourceTravsToStartingPoints(sourceTravs: _*)
    val reachedSources =
      reachableByInternal(sources).map(_.startingPoint)
    Traversal.from(reachedSources).cast[NodeType]
  }

  def reachableByFlows[A](sourceTravs: Traversal[A]*)(implicit context: EngineContext): Traversal[Path] = {
    val sources        = ExtendedCfgNode.sourceTravsToStartingPoints(sourceTravs: _*)
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
    val sources = ExtendedCfgNode.sourceTravsToStartingPoints(sourceTravs: _*)
    reachableByInternal(sources)
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
      if (sources.contains(r.startingPoint) || !startingPointToSource(r.startingPoint).isInstanceOf[CfgNode]) {
        r
      } else {
        r.copy(path =
          PathElement(startingPointToSource(r.startingPoint).asInstanceOf[CfgNode], r.callSiteStack.clone()) +: r.path
        )
      }
    }
  }

}

object ExtendedCfgNode {

  private val log = LoggerFactory.getLogger(ExtendedCfgNode.getClass)

  def sourceTravsToStartingPoints[NodeType](sourceTravs: Traversal[NodeType]*): List[StartingPointWithSource] = {
    val fjp = ForkJoinPool.commonPool()
    try {
      fjp.invoke(new SourceTravsToStartingPointsTask(sourceTravs: _*))
    } catch {
      case e: RejectedExecutionException =>
        log.error("Unable to execute 'SourceTravsToStartingPoints` task", e); List()
    } finally {
      fjp.shutdown()
    }
  }
}

/** The code below deals with member variables, and specifically with the situation where literals that initialize
  * static members are passed to `reachableBy` as sources. In this case, we determine the first usages of this member in
  * each method, traversing the AST from left to right. This isn't fool-proof, e.g., goto-statements would be
  * problematic, but it works quite well in practice.
  */
class SourceToStartingPoints(src: StoredNode) extends RecursiveTask[List[CfgNode]] {

  private def usages(pairs: List[(TypeDecl, Expression)]): List[CfgNode] = {
    pairs.flatMap { case (typeDecl, expression) =>
      val cpg = Cpg(typeDecl.graph())
      val usagesInSameClass =
        typeDecl.method
          .whereNot(_.nameExact(Defines.StaticInitMethodName, Defines.ConstructorMethodName))
          .flatMap { m =>
            expression match {
              case identifier: Identifier =>
                m.ast.isIdentifier.nameExact(identifier.name).takeWhile(notLeftHandOfAssignment)
              case fieldIdentifier: FieldIdentifier =>
                m.ast.isFieldIdentifier
                  .canonicalNameExact(fieldIdentifier.canonicalName)
                  .takeWhile(notLeftHandOfAssignment)
              case _ => List()
            }
          }
          .headOption
      val usagesInOtherClasses = cpg.method.flatMap { m =>
        m.fieldAccess
          .where(_.argument(1).isIdentifier.typeFullNameExact(typeDecl.fullName))
          .where { x =>
            expression match {
              case identifier: Identifier =>
                x.argument(2).isFieldIdentifier.canonicalNameExact(identifier.name)
              case fieldIdentifier: FieldIdentifier =>
                x.argument(2).isFieldIdentifier.canonicalNameExact(fieldIdentifier.canonicalName)
              case _ => List()
            }
          }
          .takeWhile(notLeftHandOfAssignment)
          .headOption
      }
      usagesInSameClass ++ usagesInOtherClasses
    }
  }

  /** For a literal, determine if it is used in the initialization of any member variables. Return list of initialized
    * members. An initialized member is either an identifier or a field-identifier.
    */
  private def literalToInitializedMembers(lit: Literal): List[Expression] = {
    lit.inAssignment
      .where(_.method.nameExact(Defines.StaticInitMethodName, Defines.ConstructorMethodName))
      .target
      .flatMap {
        case identifier: Identifier => List(identifier)
        case call: Call if call.name == Operators.fieldAccess =>
          call.ast.isFieldIdentifier.l
        case _ => List[Expression]()
      }
      .l
  }

  private def memberToInitializedMembers(member: Member): List[Expression] = {
    member.typeDecl.method
      .nameExact(Defines.StaticInitMethodName, Defines.ConstructorMethodName)
      .ast
      .flatMap { x =>
        x match {
          case identifier: Identifier if identifier.name == member.name =>
            Traversal(identifier).argumentIndex(1).where(_.inAssignment).l
          case fieldIdentifier: FieldIdentifier if fieldIdentifier.canonicalName == member.head.name =>
            Traversal(fieldIdentifier).where(_.inAssignment).l
          case _ => List[Expression]()
        }
      }
      .l
  }

  private def notLeftHandOfAssignment(x: Expression): Boolean = {
    !(x.argumentIndex == 1 && x.inAssignment.nonEmpty)
  }

  private def targetsToClassIdentifierPair(targets: List[Expression]): List[(TypeDecl, Expression)] = {
    targets.flatMap(target => target.method.typeDecl.map { typeDecl => (typeDecl, target) })
  }
  override def compute(): List[CfgNode] =
    src match {
      case lit: Literal =>
        List(lit) ++ usages(targetsToClassIdentifierPair(literalToInitializedMembers(lit)))
      case member: Member =>
        val initializedMember = memberToInitializedMembers(member)
        usages(targetsToClassIdentifierPair(initializedMember))
      case x => List(x).collect { case y: CfgNode => y }
    }
}

class SourceTravsToStartingPointsTask[NodeType](sourceTravs: Traversal[NodeType]*)
    extends RecursiveTask[List[StartingPointWithSource]] {

  private val log = LoggerFactory.getLogger(this.getClass)

  override def compute(): List[StartingPointWithSource] = {
    val sources: List[StoredNode] = sourceTravs
      .flatMap(_.toList)
      .collect { case n: StoredNode => n }
      .dedup
      .toList
      .sortBy(_.id)
    val tasks = sources.map(src => (src, new SourceToStartingPoints(src).fork()))
    tasks.flatMap { case (src, t: ForkJoinTask[List[CfgNode]]) =>
      Try(t.get()) match {
        case Failure(e)       => log.error("Unable to complete 'SourceToStartingPoints' task", e); List()
        case Success(sources) => sources.map(s => StartingPointWithSource(s, src))
      }
    }
  }
}
