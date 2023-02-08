package io.joern.dataflowengineoss.queryengine

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNode,
  Call,
  CfgNode,
  Expression,
  FieldIdentifier,
  Identifier,
  Literal,
  Member,
  Method,
  MethodReturn,
  StoredNode,
  TypeDecl
}
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import java.util.concurrent.{ForkJoinPool, ForkJoinTask, RecursiveTask, RejectedExecutionException}
import scala.util.{Failure, Success, Try}
import io.shiftleft.semanticcpg.language._

case class StartingPointWithSource(startingPoint: CfgNode, source: StoredNode)

object SourcesToStartingPoints {

  private val log = LoggerFactory.getLogger(SourcesToStartingPoints.getClass)

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

/** The code below deals with member variables, and specifically with the situation where literals that initialize
  * static members are passed to `reachableBy` as sources. In this case, we determine the first usages of this member in
  * each method, traversing the AST from left to right. This isn't fool-proof, e.g., goto-statements would be
  * problematic, but it works quite well in practice.
  */
class SourceToStartingPoints(src: StoredNode) extends RecursiveTask[List[CfgNode]] {

  private val cpg = Cpg(src.graph())

  override def compute(): List[CfgNode] =
    src match {
      case methodReturn: MethodReturn =>
        methodReturn.method.callIn.l
      case lit: Literal =>
        List(lit) ++ usages(targetsToClassIdentifierPair(literalToInitializedMembers(lit)))
      case member: Member =>
        val initializedMember = memberToInitializedMembers(member)
        usages(targetsToClassIdentifierPair(initializedMember))
      case x => List(x).collect { case y: CfgNode => y }
    }

  private def usages(pairs: List[(TypeDecl, Expression)]): List[CfgNode] = {
    pairs.flatMap { case (typeDecl, expression) =>
      val nonConstructorMethods = typeDecl.method
        .whereNot(_.nameExact(Defines.StaticInitMethodName, Defines.ConstructorMethodName))

      val usagesInSameClass =
        nonConstructorMethods.flatMap { m => usagesOfExpression(expression, m) }.headOption

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

  private def usagesOfExpression(expression: Expression, m: Method): List[Expression] = {
    expression match {
      case identifier: Identifier =>
        m.ast.isIdentifier.nameExact(identifier.name).takeWhile(notLeftHandOfAssignment).l
      case fieldIdentifier: FieldIdentifier =>
        m.ast.isFieldIdentifier
          .canonicalNameExact(fieldIdentifier.canonicalName)
          .l
          .takeWhile(notLeftHandOfAssignment)
      case _ => List()
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

  /** Classes have a static initialization method (cinit) and a non-static initialization method (init), and each member
    * should we initialized in at least one of them. This method identifies the initialization assignments for a given
    * member and returns the left-hand sides (targets) of these assignments.
    */
  private def memberToInitializedMembers(member: Member): List[Expression] = {
    val nodesInConstructors = astNodesInConstructors(member)

    nodesInConstructors.flatMap { x =>
      x match {
        case identifier: Identifier if identifier.name == member.name =>
          isTargetInAssignment(identifier)
        case fieldIdentifier: FieldIdentifier if fieldIdentifier.canonicalName == member.head.name =>
          Traversal(fieldIdentifier).where(_.inAssignment).l
        case _ => List[Expression]()
      }
    }.l
  }

  private def astNodesInConstructors(member: Member) = {
    methodsRecursively(member.typeDecl)
      .or(
        _.nameExact(Defines.StaticInitMethodName, Defines.ConstructorMethodName),
        // this is for python
        _.name(".*<body>$")
      )
      .ast
      .l
  }

  private def methodsRecursively(typeDecl: TypeDecl): List[Method] = {
    def methods(x: AstNode): List[Method] = {
      x match {
        case m: Method => m :: m.astMinusRoot.isMethod.flatMap(methods).l
        case _         => List()
      }
    }
    typeDecl.method.flatMap(methods).l
  }

  private def isTargetInAssignment(identifier: Identifier): List[Identifier] = {
    Traversal(identifier).argumentIndex(1).where(_.inAssignment).l
  }

  private def notLeftHandOfAssignment(x: Expression): Boolean = {
    !(x.argumentIndex == 1 && x.inAssignment.nonEmpty)
  }

  private def targetsToClassIdentifierPair(targets: List[Expression]): List[(TypeDecl, Expression)] = {
    targets.flatMap(target => target.method.typeDecl.map { typeDecl => (typeDecl, target) })
  }

}
