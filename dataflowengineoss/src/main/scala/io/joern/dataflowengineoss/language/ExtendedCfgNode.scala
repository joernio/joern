package io.joern.dataflowengineoss.language

import io.shiftleft.codepropertygraph.generated.nodes.{
  CfgNode,
  Expression,
  FieldIdentifier,
  Identifier,
  Literal,
  Member,
  TypeDecl
}
import io.joern.dataflowengineoss.queryengine.{Engine, EngineContext, PathElement, ReachableByResult}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.Cpg
import overflowdb.traversal._
import io.shiftleft.semanticcpg.language._

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
    val reachedSources = reachableByInternal(sourceTravsToStartingPoints(sourceTravs)).map(_.source)
    Traversal.from(reachedSources).cast[NodeType]
  }

  def reachableByFlows[A <: CfgNode](sourceTravs: Traversal[A]*)(implicit context: EngineContext): Traversal[Path] = {
    val sources = sourceTravsToStartingPoints(sourceTravs)
    val paths = reachableByInternal(sources)
      .map { result =>
        // We can get back results that start in nodes that are invisible
        // according to the semantic, e.g., arguments that are only used
        // but not defined. We filter these results here prior to returning
        val first = result.path.headOption
        if (first.isDefined && !first.get.visible && !sources.contains(first.get.node)) {
          None
        } else {
          val visiblePathElements = result.path.filter(x => sources.contains(x.node) || x.visible)
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
    reachableByInternal(sourceTravsToStartingPoints(sourceTravs))
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

  def sourceTravsToStartingPoints[NodeType <: CfgNode](sourceTravs: Seq[Traversal[NodeType]]): List[CfgNode] = {
    val sources = sourceTravs
      .flatMap(_.toList)
      .collect { case n: CfgNode => n }
      .dedup
      .toList
      .sortBy(_.id)
    startingPoints(sources)
  }

  /** The code below deals with static member variables in Java, and specifically with the situation where literals that
    * initialize static members are passed to `reachableBy` as sources. In this case, we determine the first usages of
    * this member in each method, traversing the AST from left to right. This isn't fool-proof, e.g., goto-statements
    * would be problematic, but it works quite well in practice.
    */
  private def startingPoints(sources: List[CfgNode]): List[CfgNode] = {
    sources.flatMap {
      case lit: Literal =>
        List(lit) ++ usages(targetsToClassIdentifierPair(literalToInitializedMembers(lit)))
      case member: Member =>
        usages(targetsToClassIdentifierPair(memberToInitializedMembers(member)))
      case x => List(x)
    }
  }

  /** For a literal, determine if it is used in the initialization of any member variables. This is Javasrc-specific as
    * it looks for a method called "<clinit>", which represents the implicitly defined class constructor.
    */
  private def literalToInitializedMembers(lit: Literal): List[Identifier] = {
    lit.inAssignment.where(_.method.nameExact("<clinit>")).target.isIdentifier.l
  }

  private def memberToInitializedMembers(member: Member): List[Identifier] = {
    member.typeDecl.method
      .nameExact("<clinit>")
      .ast
      .isIdentifier
      .nameExact(member.name)
      .argumentIndex(1)
      .where(_.inAssignment)
      .l
  }

  private def usages(pairs: List[(TypeDecl, Identifier)]): List[CfgNode] = {
    pairs.flatMap { case (typeDecl, identifier) =>
      val cpg = Cpg(typeDecl.graph())
      val usagesInSameClass =
        typeDecl.method
          .whereNot(_.nameExact("<clinit>"))
          .flatMap { m =>
            m.ast.isIdentifier.nameExact(identifier.name).takeWhile(notLeftHandOfAssignment)
          }
          .headOption
      val usagesInOtherClasses = cpg.method.flatMap { m =>
        m.fieldAccess
          .where(_.argument(1).isIdentifier.typeFullNameExact(typeDecl.fullName))
          .where(_.argument(2).isFieldIdentifier.canonicalNameExact(identifier.name))
          .takeWhile(notLeftHandOfAssignment)
          .headOption
      }
      usagesInSameClass ++ usagesInOtherClasses
    }
  }

  private def notLeftHandOfAssignment(x: Expression): Boolean = {
    !(x.argumentIndex == 1 && x.inAssignment.nonEmpty)
  }

  private def targetsToClassIdentifierPair(targets: List[Identifier]): List[(TypeDecl, Identifier)] = {
    targets.flatMap(target => target.method.typeDecl.map { typeDecl => (typeDecl, target) })
  }

}
