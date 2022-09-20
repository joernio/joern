package io.joern.dataflowengineoss.language

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Expression, Identifier, Literal, Member, TypeDecl}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

object SourceToStartingPoints {

  def sourceTravsToStartingPoints[NodeType](sourceTravs: Seq[Traversal[NodeType]]): List[StartingPointWithSource] = {
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

  /** The code below deals with static member variables in Java, and specifically with the situation where literals that
    * initialize static members are passed to `reachableBy` as sources. In this case, we determine the first usages of
    * this member in each method, traversing the AST from left to right. This isn't fool-proof, e.g., goto-statements
    * would be problematic, but it works quite well in practice.
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
