package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.Node
import com.github.javaparser.ast.body.VariableDeclarator
import com.github.javaparser.ast.expr.AssignExpr.Operator
import com.github.javaparser.ast.expr.{AssignExpr, Expression, ObjectCreationExpr, VariableDeclarationExpr}
import com.github.javaparser.resolution.types.ResolvedType
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.scope.Scope.{NewVariableNode, typeFullName}
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNodeNew,
  NewCall,
  NewFieldIdentifier,
  NewIdentifier,
  NewMember,
  NewUnknown
}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.Try
import io.joern.javasrc2cpg.scope.JavaScopeElement.PartialInit

trait AstForVarDeclAndAssignsCreator { this: AstCreator =>
  private val logger = LoggerFactory.getLogger(this.getClass())

  private[expressions] def astsForAssignExpr(expr: AssignExpr, expectedType: ExpectedType): Seq[Ast] = {
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

    val maybeResolvedType = Try(expr.getTarget.calculateResolvedType()).toOption
    val expectedInitializerType = maybeResolvedType
      .map { resolvedType =>
        ExpectedType(typeInfoCalc.fullName(resolvedType), Some(resolvedType))
      }
      .getOrElse(expectedType) // resolved target type should be more accurate

    // TODO What to do if target is empty?
    val targetAst = astsForExpression(expr.getTarget, expectedInitializerType).head

    astsForAssignment(
      expr,
      targetAst,
      expr.getValue,
      operatorName,
      expr.getOperator.asString,
      expectedInitializerType,
      varDeclType = None
    )
  }

  private def copyAstForVarDeclInit(targetAst: Ast): Ast = {
    targetAst.root match {
      case Some(identifier: NewIdentifier) => Ast(identifier.copy)

      case Some(fieldAccess: NewCall) if fieldAccess.name == Operators.fieldAccess =>
        val maybeIdentifier = targetAst.nodes.collectFirst { case node if node.isInstanceOf[NewIdentifier] => node }
        val maybeField = targetAst.nodes.collectFirst { case node if node.isInstanceOf[NewFieldIdentifier] => node }

        (maybeIdentifier, maybeField) match {
          case (Some(identifier), Some(fieldIdentifier)) =>
            val args = List(identifier, fieldIdentifier).map(node => Ast(node.copy))
            callAst(fieldAccess.copy, args)

          case _ =>
            logger.warn(s"Attempting to copy field access without required children: ${fieldAccess.code}")
            Ast()
        }

      case Some(root) =>
        logger.warn(s"Attempting to copy unhandled root type for var decl init: $root")
        Ast()

      case None =>
        Ast()
    }
  }

  private[expressions] def astsForVariableDeclarationExpr(
    variableDeclarationExpr: VariableDeclarationExpr
  ): Seq[Ast] = {
    val variableDeclaratorAsts = variableDeclarationExpr.getVariables.asScala
      .map(astsForVariableDeclarator(_, variableDeclarationExpr))
      .toList
    variableDeclaratorAsts.flatMap(_.headOption) ++ variableDeclaratorAsts.flatMap(_.drop(1))
  }

  def astsForVariableDeclarator(variableDeclarator: VariableDeclarator, originNode: Node): Seq[Ast] = {
    val typeFullName = tryWithSafeStackOverflow(
      scope
        .lookupType(variableDeclarator.getTypeAsString, includeWildcards = false)
        .orElse(typeInfoCalc.fullName(variableDeclarator.getType))
    ).toOption.flatten

    val (correspondingNode, localAst): (NewVariableNode, Option[Ast]) =
      scope.lookupVariable(variableDeclarator.getNameAsString).variableNode.map((_, None)).getOrElse {
        val localCode = s"${variableDeclarator.getTypeAsString} ${variableDeclarator.getNameAsString}"
        val local =
          localNode(
            originNode,
            variableDeclarator.getNameAsString,
            localCode,
            typeFullName.getOrElse(TypeConstants.Any)
          )

        scope.enclosingBlock.foreach(_.addLocal(local))

        (local, Some(Ast(local)))
      }

    val assignmentTarget = correspondingNode match {
      case member: NewMember =>
        val name =
          if (scope.isEnclosingScopeStatic)
            scope.enclosingTypeDecl.map(_.typeDecl.name).getOrElse(NameConstants.Unknown)
          else NameConstants.This
        fieldAccessAst(
          name,
          scope.enclosingTypeDecl.fullName,
          correspondingNode.name,
          Option(correspondingNode.typeFullName),
          line(originNode),
          column(originNode)
        )

      case variable =>
        val node = identifierNode(variableDeclarator, variable.name, variable.name, variable.typeFullName)
        Ast(node).withRefEdge(node, correspondingNode)
    }

    val assignmentAsts = variableDeclarator.getInitializer.toScala.toList.flatMap { initializer =>

      val expectedType =
        tryWithSafeStackOverflow(
          symbolSolver.toResolvedType(variableDeclarator.getType, classOf[ResolvedType])
        ).toOption

      astsForAssignment(
        variableDeclarator,
        assignmentTarget,
        initializer,
        Operators.assignment,
        "=",
        ExpectedType(typeFullName, expectedType),
        Some(variableDeclarator.getTypeAsString)
      )
    }

    localAst.toList ++ assignmentAsts
  }

  def astsForAssignment(
    node: Node,
    target: Ast,
    initializer: Expression,
    operatorName: String,
    symbol: String,
    expectedType: ExpectedType,
    varDeclType: Option[String]
  ): List[Ast] = {
    val codeTypePrefix = varDeclType.map(_ + " ").getOrElse("")
    val code           = s"$codeTypePrefix${target.rootCodeOrEmpty} $symbol ${initializer.toString}"
    val assignmentNode = callNode(node, code, operatorName, operatorName, DispatchTypes.STATIC_DISPATCH)

    target.rootType.foreach(assignmentNode.typeFullName(_))

    val isSimpleAssign = (operatorName == Operators.assignment)
    val isVarOrFieldAssign = target.root.exists {
      case _: NewIdentifier => true
      case call: NewCall    => call.methodFullName == Operators.fieldAccess
      case _                => false
    }

    val initializerAsts = initializer match {
      case objectCreationExpr: ObjectCreationExpr if isSimpleAssign && isVarOrFieldAssign =>
        val initReceiver =
          target.subTreeCopy(target.root.collect { case astNode: AstNodeNew => astNode }.getOrElse(NewUnknown()))
        val inlinedAsts =
          inlinedAstsForObjectCreationExpr(
            objectCreationExpr,
            initReceiver,
            expectedType,
            resetAssignmentTargetType = false
          )
        inlinedAsts.allocAst :: inlinedAsts.initAst :: Nil

      case _ => astsForExpression(initializer, expectedType).toList
    }

    val assignmentAst = callAst(assignmentNode, target :: initializerAsts.headOption.toList)

    assignmentAst :: initializerAsts.drop(1)
  }
}
