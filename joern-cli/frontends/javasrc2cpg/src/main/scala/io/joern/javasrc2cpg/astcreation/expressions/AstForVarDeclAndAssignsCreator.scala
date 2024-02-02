package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.Node
import com.github.javaparser.ast.body.VariableDeclarator
import com.github.javaparser.ast.expr.AssignExpr.Operator
import com.github.javaparser.ast.expr.{AssignExpr, Expression, ObjectCreationExpr, VariableDeclarationExpr}
import com.github.javaparser.resolution.types.ResolvedType
import io.joern.javasrc2cpg.astcreation.expressions.AstForCallExpressionsCreator.PartialConstructor
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.scope.Scope.{
  NewVariableNode,
  ScopeMember,
  ScopeParameter,
  SimpleVariable,
  newVariableNodeTypeFullName
}
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.joern.x2cpg.utils.NodeBuilders.newOperatorCallNode
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewFieldIdentifier, NewIdentifier, NewLocal}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators}
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.Try
import io.joern.javasrc2cpg.scope.JavaScopeElement.PartialInit

trait AstForVarDeclAndAssignsCreator { this: AstCreator =>
  private val logger = LoggerFactory.getLogger(this.getClass())

  private[expressions] def astsForAssignExpr(expr: AssignExpr, expectedExprType: ExpectedType): Seq[Ast] = {
    val operatorName = expr.getOperator match {
      case Operator.ASSIGN               => Operators.assignment
      case Operator.PLUS                 => Operators.assignmentPlus
      case Operator.MINUS                => Operators.assignmentMinus
      case Operator.MULTIPLY             => Operators.assignmentMultiplication
      case Operator.DIVIDE               => Operators.assignmentDivision
      case Operator.BINARY_AND           => Operators.assignmentAnd
      case Operator.BINARY_OR            => Operators.assignmentOr
      case Operator.XOR                  => Operators.assignmentXor
      case Operator.REMAINDER            => Operators.assignmentModulo
      case Operator.LEFT_SHIFT           => Operators.assignmentShiftLeft
      case Operator.SIGNED_RIGHT_SHIFT   => Operators.assignmentArithmeticShiftRight
      case Operator.UNSIGNED_RIGHT_SHIFT => Operators.assignmentLogicalShiftRight
    }
    val operatorSymbol = expr.getOperator.asString

    val maybeResolvedType = tryWithSafeStackOverflow(expr.getTarget.calculateResolvedType()).toOption

    val targetAst = astsForExpression(expr.getTarget, expectedExprType)
    if (targetAst.length > 1) {
      logger.warn(s"Too many LHS ASTs for expression ${expr.toString}")
    }

    astsForAssignment(
      expr,
      targetAst.head,
      targetAst.head.rootCodeOrEmpty,
      expr.getValue,
      maybeResolvedType,
      operatorName,
      operatorSymbol
    )
  }

  private[expressions] def astsForVariableDeclaration(variableDeclaration: VariableDeclarationExpr): Seq[Ast] = {
    val variablesWithAssignments =
      variableDeclaration.getVariables.asScala.map(astsForVariableDeclarator(_, Some(variableDeclaration))).toList

    variablesWithAssignments.map(_.head) ++ variablesWithAssignments.flatMap(_.drop(1))
  }

  def astsForVariableDeclarator(variableDeclarator: VariableDeclarator, originNode: Option[Node] = None): List[Ast] = {
    val localNode = localForVariableDeclarator(variableDeclarator, originNode)
    scope.enclosingBlock.foreach(_.addLocal(localNode))

    val identifierForAssignment =
      identifierNode(variableDeclarator, localNode.name, localNode.name, localNode.typeFullName)

    // Need the actual resolvedType here for when the RHS is a lambda expression.
    val resolvedExpectedType =
      tryWithSafeStackOverflow(symbolSolver.toResolvedType(variableDeclarator.getType, classOf[ResolvedType])).toOption

    val assignmentAsts: List[Ast] = variableDeclarator.getInitializer.toScala
      .map { initializer =>
        astsForAssignment(
          variableDeclarator,
          Ast(identifierForAssignment),
          localNode.code,
          initializer,
          resolvedExpectedType,
          Operators.assignment,
          "="
        ).toList
      }
      .getOrElse(Nil)

    Ast(localNode) :: assignmentAsts
  }

  private def localForVariableDeclarator(variable: VariableDeclarator, originNode: Option[Node]): NewLocal = {
    val name = variable.getNameAsString

    val typeFullName = tryWithSafeStackOverflow {
      scope.lookupType(variable.getTypeAsString).orElse(typeInfoCalc.fullName(variable.getType))
    }.toOption.flatten

    val code = s"${variable.getType} $name"

    localNode(originNode.getOrElse(variable), name, code, typeFullName.getOrElse(TypeConstants.Any))
  }

  private def astsForAssignment(
    assignmentNode: Node,
    targetAst: Ast,
    targetCode: String,
    initializer: Expression,
    expectedInitializerType: Option[ResolvedType],
    assignmentOperatorName: String,
    assignmentOperatorCode: String
  ): Seq[Ast] = {

    scope.pushAssignmentScope(targetAst)
    val initializerAsts =
      astsForExpression(initializer, ExpectedType(targetAst.rootType, expectedInitializerType)).toList
    scope.popAssignmentScope()

    val assignmentCode =
      s"$targetCode $assignmentOperatorCode ${initializerAsts.headOption.flatMap(_.rootCode).getOrElse("")}"
    val assignmentType =
      targetAst.rootType
        .orElse(initializerAsts.headOption.flatMap(_.rootType))
        .orElse(expectedInitializerType.map(_.toString))
        .getOrElse(TypeConstants.Any)

    val assignmentCallNode = newOperatorCallNode(
      assignmentOperatorName,
      assignmentCode,
      Some(assignmentType),
      line(assignmentNode),
      column(assignmentNode)
    )

    val assignmentArgs = targetAst :: initializerAsts.headOption.toList

    val assignmentAst = callAst(assignmentCallNode, assignmentArgs)

    assignmentAst :: initializerAsts.drop(1)
  }
}
