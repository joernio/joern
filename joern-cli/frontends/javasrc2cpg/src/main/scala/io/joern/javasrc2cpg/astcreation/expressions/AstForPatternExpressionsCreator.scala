package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.expr.{PatternExpr, RecordPatternExpr, TypePatternExpr}
import io.joern.javasrc2cpg.astcreation.AstCreator
import io.joern.javasrc2cpg.jartypereader.model.Model.TypeConstants
import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewTypeRef}
import io.joern.x2cpg.{Ast, Defines}
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.utils.NodeBuilders.{newFieldIdentifierNode, newOperatorCallNode}
import io.shiftleft.codepropertygraph.generated.Operators
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*

trait AstForPatternExpressionsCreator { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(this.getClass)

  /** @param rootExpression
    *   in o instanceof <PatternExpr>, the rootExpression corresponds to o. For switch patterns, this is the switch
    *   condition.
    */
  def astForPatternMatchCondition(patternExpr: PatternExpr, rootAst: Ast): Ast = {
    val instanceOfAst = patternExpr match {
      case typePatternExpr: TypePatternExpr => astForTypePatternMatchCondition(typePatternExpr, rootAst)

      case recordPatternExpr: RecordPatternExpr => astForRecordPatternMatchCondition(recordPatternExpr, rootAst)
    }
    
    val localAndAssignmentAsts = astsForPatternAssignments(patternExpr, rootAst)
    u
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
    ???
  }

  def astsForPatternAssignments(patternExpr: PatternExpr, sourceObject: Ast): List[Ast] = {
    patternExpr match {
      case typePatternExpr: TypePatternExpr     => astsForTypePatternAssignment(typePatternExpr, sourceObject)
      case recordPatternExpr: RecordPatternExpr => astsForRecordPatternAssignment(recordPatternExpr, sourceObject)
    }
  }

  private def astsForTypePatternAssignment(patternExpr: TypePatternExpr, sourceObject: Ast): List[Ast] = {
    val patternType = tryWithSafeStackOverflow(patternExpr.getType).toOption

    val targetName = patternExpr.getNameAsString
    val targetType = patternType.flatMap(typeInfoCalc.fullName).getOrElse(TypeConstants.Any)

    val targetLocal = localNode(patternExpr, targetName, patternExpr.toString, targetType)

    val targetIdentifier = identifierNode(patternExpr, targetName, targetName, targetType)

    val castNode =
      newOperatorCallNode(
        Operators.cast,
        s"($targetType) ${sourceObject.rootCodeOrEmpty}",
        Option(targetType),
        line(patternExpr),
        column(patternExpr)
      )

    val castTypeRef = typeRefNode(patternExpr, patternType.map(_.toString).getOrElse(""), targetType)

    val assignmentNode = newOperatorCallNode(
      Operators.assignment,
      s"$targetName = ${castNode.code}",
      Option(targetType),
      line(patternExpr),
      column(patternExpr)
    )

    val localAst = Ast(targetLocal)

    val castCallAst = callAst(castNode, Ast(castTypeRef) :: sourceObject :: Nil)

    val assignmentAst =
      callAst(assignmentNode, Ast(targetIdentifier) :: castCallAst :: Nil).withRefEdge(targetIdentifier, targetLocal)

    localAst :: assignmentAst :: Nil
  }

  private def astsForRecordPatternAssignment(patternExpr: RecordPatternExpr, sourceObject: Ast): List[Ast] = {
    ???
  }
}
