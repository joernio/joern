package io.joern.dataflowengineoss.queryengine

import io.joern.dataflowengineoss.globalFromLiteral
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.allAssignmentTypes
import io.shiftleft.semanticcpg.utils.MemberAccess.isFieldAccess
import org.slf4j.LoggerFactory

import java.util.concurrent.{ForkJoinPool, ForkJoinTask, RecursiveTask, RejectedExecutionException}
import scala.util.{Failure, Success, Try}

case class StartingPointWithSource(startingPoint: CfgNode, source: StoredNode)

object SourcesToStartingPoints {

  private val log = LoggerFactory.getLogger(SourcesToStartingPoints.getClass)

  def sourceTravsToStartingPoints[NodeType](sourceTravs: IterableOnce[NodeType]*): List[StartingPointWithSource] = {
    val fjp = ForkJoinPool.commonPool()
    try {
      fjp.invoke(new SourceTravsToStartingPointsTask(sourceTravs: _*)).distinct
    } catch {
      case e: RejectedExecutionException =>
        log.error("Unable to execute 'SourceTravsToStartingPoints` task", e); List()
    } finally {
      fjp.shutdown()
    }
  }

}

class SourceTravsToStartingPointsTask[NodeType](sourceTravs: IterableOnce[NodeType]*)
    extends RecursiveTask[List[StartingPointWithSource]] {

  private val log = LoggerFactory.getLogger(this.getClass)

  override def compute(): List[StartingPointWithSource] = {
    val sources: List[StoredNode] = sourceTravs
      .flatMap(_.iterator.toList)
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

  override def compute(): List[CfgNode] = sourceToStartingPoints(src)

  private def sourceToStartingPoints(src: StoredNode): List[CfgNode] = {
    src match {
      case methodReturn: MethodReturn =>
        methodReturn.method.callIn.l
      case lit: Literal =>
        val uses = usages(targetsToClassIdentifierPair(literalToInitializedMembers(lit)))
        val globals = globalFromLiteral(lit).flatMap {
          case x: Identifier if x.isModuleVariable => x :: moduleVariableToFirstUsagesAcrossProgram(x)
          case x                                   => x :: Nil
        }
        lit :: (uses ++ globals)
      case member: Member =>
        usages(targetsToClassIdentifierPair(List(member)))
      case x: Identifier =>
        val fieldAndIndexAccesses = withFieldAndIndexAccesses(x :: Nil)
        val capturedReferences = x.refsTo.capturedByMethodRef.referencedMethod.flatMap(firstUsagesForName(x.name, _)).l

        (x :: fieldAndIndexAccesses ++ capturedReferences) flatMap {
          case x: Call => sourceToStartingPoints(x) // Handle the case if this is an arg to another call
          case x       => x :: Nil
        }
      case x: Call    => x :: x._receiverIn.collectAll[CfgNode].l
      case x: CfgNode => x :: Nil
      case _          => Nil
    }
  }

  private def withFieldAndIndexAccesses(nodes: List[CfgNode]): List[CfgNode] =
    nodes.flatMap {
      case moduleVar: Identifier if moduleVar.isModuleVariable =>
        moduleVar :: moduleVariableToFirstUsagesAcrossProgram(moduleVar)
      case identifier: Identifier => identifier :: fieldAndIndexAccesses(identifier)
      case x                      => x :: Nil
    }

  private def fieldAndIndexAccesses(identifier: Identifier): List[CfgNode] =
    identifier.method._identifierViaContainsOut
      .nameExact(identifier.name)
      .inCall
      .collect { case c if isFieldAccess(c.name) => c }
      .l

  /** Finds the first usages of this module variable across all importing modules.
    *
    * TODO: This is wrapped in a try-catch because of the deprecated Ruby frontend crashing this process due to a
    * missing `.method` parent node in the contains graph.
    */
  private def moduleVariableToFirstUsagesAcrossProgram(moduleVar: Identifier): List[CfgNode] = Try {
    moduleVar.start.moduleVariables.references
      .groupBy(_.method)
      .map {
        case (sameModule, _) if moduleVar.method == sameModule => fieldAndIndexAccesses(moduleVar)
        case (_, references)                                   => references.filterNot(notLeftHandOfAssignment)
      }
      .flatMap(_.sortBy(i => (i.lineNumber, i.columnNumber)).headOption)
      .toList
  }.getOrElse(List.empty)

  private def usages(pairs: List[(TypeDecl, AstNode)]): List[CfgNode] = {
    pairs.flatMap { case (typeDecl, astNode) =>
      val nonConstructorMethods = methodsRecursively(typeDecl).iterator
        .whereNot(_.nameExact(Defines.StaticInitMethodName, Defines.ConstructorMethodName, "__init__"))
        .l

      val usagesInSameClass =
        nonConstructorMethods.flatMap { m => firstUsagesOf(astNode, m, typeDecl) }

      val usagesInOtherClasses = cpg.method.flatMap { m =>
        m.fieldAccess
          .where(_.argument(1).isIdentifier.typeFullNameExact(typeDecl.fullName))
          .where { x =>
            astNode match {
              case identifier: Identifier =>
                x.argument(2).isFieldIdentifier.canonicalNameExact(identifier.name)
              case fieldIdentifier: FieldIdentifier =>
                x.argument(2).isFieldIdentifier.canonicalNameExact(fieldIdentifier.canonicalName)
              case _ => Iterator.empty
            }
          }
          .takeWhile(notLeftHandOfAssignment)
          .headOption
      }.l
      usagesInSameClass ++ usagesInOtherClasses
    }
  }

  /** For given method, determine the first usage of the given expression.
    */
  private def firstUsagesOf(astNode: AstNode, m: Method, typeDecl: TypeDecl): List[Expression] = {
    astNode match {
      case member: Member =>
        firstUsagesForName(member.name, m)
      case identifier: Identifier =>
        firstUsagesForName(identifier.name, m)
      case fieldIdentifier: FieldIdentifier =>
        val fieldIdentifiers = m.ast.isFieldIdentifier.sortBy(x => (x.lineNumber, x.columnNumber)).l
        fieldIdentifiers
          .canonicalNameExact(fieldIdentifier.canonicalName)
          .inFieldAccess
          // TODO `isIdentifier` seems to limit us here
          .where(_.argument(1).isIdentifier.or(_.nameExact("this", "self"), _.typeFullNameExact(typeDecl.fullName)))
          .takeWhile(notLeftHandOfAssignment)
          .l
      case _ => List()
    }
  }

  private def firstUsagesForName(name: String, m: Method): List[Expression] = {
    val identifiers      = m._identifierViaContainsOut.l
    val identifierUsages = identifiers.nameExact(name).takeWhile(notLeftHandOfAssignment).l
    val fieldIdentifiers = m.fieldAccess.fieldIdentifier.sortBy(x => (x.lineNumber, x.columnNumber)).l
    val thisRefs         = Seq("this", "self") ++ m.typeDecl.name.headOption.toList
    val fieldAccessUsages = fieldIdentifiers.isFieldIdentifier
      .canonicalNameExact(name)
      .inFieldAccess
      .where(_.argument(1).codeExact(thisRefs: _*))
      .takeWhile(notLeftHandOfAssignment)
      .l
    (identifierUsages ++ fieldAccessUsages).sortBy(x => (x.lineNumber, x.columnNumber)).headOption.toList
  }

  /** For a literal, determine if it is used in the initialization of any member variables. Return list of initialized
    * members. An initialized member is either an identifier or a field-identifier.
    */
  private def literalToInitializedMembers(lit: Literal): List[CfgNode] =
    lit.inAssignment
      .or(
        _.method.nameExact(Defines.StaticInitMethodName, Defines.ConstructorMethodName, "__init__"),
        // in language such as Python, where assignments for members can be directly under a type decl
        _.method.typeDecl
      )
      .target
      .flatMap {
        case identifier: Identifier
            // If these are the same, then the parent method is the module-level type
            if identifier.method.typeDecl.fullName.contains(identifier.method.fullName) ||
              // If a member shares the name of the identifier then we consider this as a member
              lit.method.typeDecl.member.name.toSet.contains(identifier.name) =>
          identifier :: Nil
        case call: Call if isFieldAccess(call.name) => call.ast.isFieldIdentifier.l
        case _                                      => Nil
      }
      .l

  private def methodsRecursively(typeDecl: TypeDecl): List[Method] = {
    def methods(x: AstNode): List[Method] = {
      x match {
        case m: Method => m :: m.astMinusRoot.isMethod.flatMap(methods).l
        case _         => Nil
      }
    }
    typeDecl.method.flatMap(methods).l
  }

  private def isTargetInAssignment(identifier: Identifier): List[Identifier] = {
    identifier.start.argumentIndex(1).where(_.inAssignment).l
  }

  private def notLeftHandOfAssignment(x: Expression): Boolean = {
    !(x.argumentIndex == 1 && x.inCall.exists(y => allAssignmentTypes.contains(y.name)))
  }

  private def targetsToClassIdentifierPair(targets: List[AstNode]): List[(TypeDecl, AstNode)] = {
    targets.flatMap {
      case expr: Expression =>
        expr.method.typeDecl.map { typeDecl => (typeDecl, expr) }
      case member: Member =>
        member.typeDecl.map { typeDecl => (typeDecl, member) }
    }
  }

}
