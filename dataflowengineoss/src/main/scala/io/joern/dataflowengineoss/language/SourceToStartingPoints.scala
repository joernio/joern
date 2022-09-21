package io.joern.dataflowengineoss.language

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Expression, Identifier, Literal, Member, TypeDecl}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

/** This component translates sources to starting points for data flow tracking. In most cases, a source can already be
  * a starting point. However, some nodes, such as member variables, need to be translated into suitable sources, e.g,
  * all places where the member variable is first used.
  */
object SourceToStartingPoints {

  /** For a sequence of source traversals, determine the corresponding list of starting points.
    */
  def sourceTravsToStartingPoints[NodeType](sourceTravs: Traversal[NodeType]*): List[StartingPointWithSource] = {
    val sources = sourceTravs
      .flatMap(_.toList)
      .collect { case n: CfgNode => n }
      .dedup
      .toList
      .sortBy(_.id)
    sources.flatMap { src =>
      sourceToStartingPoints(src).map(s => StartingPointWithSource(s, src))
    }
  }

  /** The code below deals with static member variables in, and specifically with the following situations:
    *
    * (a) Literals that initialize static members are used as sources.
    *
    * (b) Static members are used as sources.
    *
    * In both cases, we determine the first usages of this member in each method, traversing the AST from left to right.
    * This heuristic isn't fool-proof, e.g., goto-statements would be problematic, but it works quite well in practice.
    */
  private def sourceToStartingPoints[NodeType](src: NodeType): List[CfgNode] = {
    src match {
      case lit: Literal =>
        val initializedMembers = literalToInitializedMembers(lit)
        List(lit) ++ usages(targetsToClassIdentifierPair(initializedMembers))
      case member: Member =>
        val initializedMembers = memberToInitializedMembers(member)
        usages(targetsToClassIdentifierPair(initializedMembers))
      case x =>
        List(x).collect { case y: CfgNode => y }
    }
  }

  /** For a literal, determine if it is used in the initialization of any static member variable.
    */
  private def literalToInitializedMembers(lit: Literal): List[Identifier] = {
    lit.inAssignment.where(_.method.nameExact(Defines.StaticInitMethodName)).target.isIdentifier.l
  }

  private def memberToInitializedMembers(member: Member): List[Identifier] = {
    member.typeDecl.method
      .nameExact(Defines.StaticInitMethodName)
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
          .whereNot(_.nameExact(Defines.StaticInitMethodName))
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
