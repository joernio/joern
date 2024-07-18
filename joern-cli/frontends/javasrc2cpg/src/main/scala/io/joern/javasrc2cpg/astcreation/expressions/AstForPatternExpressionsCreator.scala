package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.expr.{InstanceOfExpr, PatternExpr, RecordPatternExpr, TypePatternExpr}
import com.github.javaparser.ast.stmt.SwitchEntry
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.jartypereader.model.Model.TypeConstants
import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewIdentifier, NewTypeRef}
import io.joern.x2cpg.{Ast, Defines}
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.utils.NodeBuilders.{newFieldIdentifierNode, newOperatorCallNode}
import io.shiftleft.codepropertygraph.generated.Operators
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

trait AstForPatternExpressionsCreator { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(this.getClass)

  /** @param rootExpression
    *   in o instanceof <PatternExpr>, the rootExpression corresponds to o. For switch patterns, this is the switch
    *   condition.
    */
  private[expressions] def astForPatternMatchCondition(patternExpr: PatternExpr, rootAst: Ast): Ast = {
    patternExpr match {
      case typePatternExpr: TypePatternExpr => astForTypePatternMatchCondition(typePatternExpr, rootAst)

      case recordPatternExpr: RecordPatternExpr => astForRecordPatternMatchCondition(recordPatternExpr, rootAst)
    }
  }

  private def astForTypePatternMatchCondition(patternExpr: TypePatternExpr, rootAst: Ast): Ast = {
    val patternType = tryWithSafeStackOverflow(patternExpr.getType).toOption

    val typeName     = patternType.map(_.toString).getOrElse(TypeConstants.Any)
    val typeFullName = patternType.flatMap(typeInfoCalc.fullName).getOrElse(TypeConstants.Any)

    val instanceOfNode = newOperatorCallNode(
      Operators.instanceOf,
      s"${rootAst.rootCode.getOrElse(Defines.Unknown)} instanceof $typeName",
      Option(TypeConstants.Boolean),
      rootAst.rootLine,
      rootAst.rootColumn
    )

    val typeRef =
      typeRefNode(patternType.getOrElse(patternExpr), patternType.map(_.toString).getOrElse(""), typeFullName)

    callAst(instanceOfNode, List(rootAst, Ast(typeRef)))
  }

  private def astForRecordPatternMatchCondition(patternExpr: RecordPatternExpr, rootAst: Ast): Ast = {
    // TODO
    unknownAst(patternExpr)
  }

  def astsForPatternAssignment(patternExpr: TypePatternExpr): List[Ast] = {
    val patternType  = tryWithSafeStackOverflow(patternExpr.getType).toOption
    val typeFullName = patternType.flatMap(typeInfoCalc.fullName).getOrElse(TypeConstants.Any)

    val patternLocal = localNode(patternExpr, patternExpr.getNameAsString, code(patternExpr), typeFullName)

    val assignmentIdentifier =
      identifierNode(patternExpr, patternExpr.getNameAsString, patternExpr.getNameAsString, typeFullName)

    val assignmentRhsAst = astForTypePatternAssignmentRhs(patternExpr, patternType, typeFullName)

    val assignmentCode = s"${assignmentIdentifier.code} = ${assignmentRhsAst.rootCodeOrEmpty}"
    val assignmentCall = newOperatorCallNode(
      Operators.assignment,
      assignmentCode,
      Option(typeFullName),
      line(patternExpr),
      column(patternExpr)
    )

    Ast(patternLocal) :: callAst(assignmentCall, Ast(assignmentIdentifier) :: assignmentRhsAst :: Nil) :: Nil
  }

  private def astForTypePatternAssignmentRhs(
    patternExpr: TypePatternExpr,
    patternType: Option[Type],
    typeFullName: String
  ): Ast = {
    val parentAst = patternExpr.getParentNode.toScala match {
      case Some(instanceOfExpr: InstanceOfExpr) =>
        astsForExpression(instanceOfExpr.getExpression, ExpectedType.empty).headOption

      case Some(switchEntry: SwitchEntry) =>
        ???

      case Some(recordPatternExpr: RecordPatternExpr) =>
        ???

      case Some(other) =>
        ???

      case None =>
        ???
    }

    val typeCode   = patternType.map(_.toString()).getOrElse(Defines.Unknown)
    val parentCode = parentAst.flatMap(_.rootCode).getOrElse("")
    val castCode   = s"($typeCode) $parentCode"

    val typeRef = typeRefNode(patternExpr, typeCode, typeFullName)

    val castCall =
      newOperatorCallNode(Operators.cast, castCode, Option(typeFullName), line(patternExpr), column(patternExpr))

    callAst(castCall, Ast(typeRef) :: parentAst.toList)
  }

  private[expressions] def pushLocalsAndAssignmentsForPattern(
    patternExpr: PatternExpr,
    exprAst: Ast,
    assignmentTarget: NewIdentifier
  ): Unit = {
    findTypePatterns(patternExpr).foreach { patternExpr =>
      val patternType  = tryWithSafeStackOverflow(patternExpr.getType).toOption
      val typeFullName = patternType.flatMap(typeInfoCalc.fullName).getOrElse(TypeConstants.Any)

      val patternVariableName = patternExpr.getNameAsString
      val patternLocal        = localNode(patternExpr, patternExpr.getNameAsString, code(patternExpr), typeFullName)

      val assignmentRhsAst = astForTypePatternAssignmentRhs(patternExpr, patternType, typeFullName)

      val assignmentCode = s"${assignmentTarget.code} = ${assignmentRhsAst.rootCodeOrEmpty}"

      val assignmentCall = newOperatorCallNode(
        Operators.assignment,
        assignmentCode,
        Option(typeFullName),
        line(patternExpr),
        column(patternExpr)
      )

      val assignmentTargetAst =
        Ast(assignmentTarget).withRefEdges(
          assignmentTarget,
          scope.lookupVariable(assignmentTarget.name).getVariable().map(_.node).toList
        )

      val assignmentAst = callAst(assignmentCall, assignmentTargetAst :: assignmentRhsAst :: Nil)

      scope.enclosingMethod.foreach { methodScope =>
        methodScope.addPatternAst(patternVariableName, patternLocal, assignmentAst)
      }
    }
  }

  private def findTypePatterns(patternExpr: PatternExpr): List[TypePatternExpr] = {
    patternExpr match {
      case typePatternExpr: TypePatternExpr =>
        typePatternExpr :: Nil

      case recordPatternExpr: RecordPatternExpr =>
        recordPatternExpr.getPatternList.asScala.toList.flatMap(findTypePatterns)
    }
  }

  private def astsForRecordPatternAssignmentRhs(patternExpr: RecordPatternExpr, sourceObject: Ast): List[Ast] = {
    // TODO
    unknownAst(patternExpr) :: Nil
  }
}
