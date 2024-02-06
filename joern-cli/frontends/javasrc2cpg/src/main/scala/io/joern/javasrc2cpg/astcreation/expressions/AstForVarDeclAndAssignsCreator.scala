package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.Node
import com.github.javaparser.ast.body.VariableDeclarator
import com.github.javaparser.ast.expr.AssignExpr.Operator
import com.github.javaparser.ast.expr.{AssignExpr, Expression, VariableDeclarationExpr}
import com.github.javaparser.resolution.types.ResolvedType
import io.joern.javasrc2cpg.astcreation.expressions.AstForCallExpressionsCreator.PartialConstructor
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.scope.Scope.{
  NewVariableNode,
  ScopeLocal,
  ScopeMember,
  ScopeParameter,
  ScopeVariable,
  SimpleVariable,
  newVariableNodeType
}
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.joern.x2cpg.utils.NodeBuilders.newOperatorCallNode
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewFieldIdentifier, NewIdentifier, NewLocal, NewMember}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}
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

    val maybeResolvedType = Try(expr.getTarget.calculateResolvedType()).toOption
    val expectedType = maybeResolvedType
      .map { resolvedType =>
        ExpectedType(typeInfoCalc.fullName(resolvedType), Some(resolvedType))
      }
      .getOrElse(expectedExprType) // resolved target type should be more accurate
    val targetAst = astsForExpression(expr.getTarget, expectedType)
    val argsAsts  = astsForExpression(expr.getValue, expectedType)
    val valueType = argsAsts.headOption.flatMap(_.rootType)

    val typeFullName =
      targetAst.headOption
        .flatMap(_.rootType)
        .orElse(valueType)
        .orElse(expectedType.fullName)
        .getOrElse(TypeConstants.Any)

    val code = s"${targetAst.rootCodeOrEmpty} ${expr.getOperator.asString} ${argsAsts.rootCodeOrEmpty}"

    val callNode = newOperatorCallNode(operatorName, code, Some(typeFullName), line(expr), column(expr))

    if (partialConstructorQueue.isEmpty) {
      val assignAst = callAst(callNode, targetAst ++ argsAsts)
      Seq(assignAst)
    } else {
      if (partialConstructorQueue.size > 1) {
        logger.warn("BUG: Received multiple partial constructors from assignment. Dropping all but the first.")
      }
      val partialConstructor = partialConstructorQueue.head
      partialConstructorQueue.clear()

      targetAst.flatMap(_.root).toList match {
        case List(identifier: NewIdentifier) =>
          // In this case we have a simple assign. No block needed.
          // e.g. Foo f = new Foo();
          val initAst = completeInitForConstructor(partialConstructor, Ast(identifier.copy))
          Seq(callAst(callNode, targetAst ++ argsAsts), initAst)

        case _ =>
          // In this case the left hand side is more complex than an identifier, so
          // we need to contain the constructor in a block.
          // e.g. items[10] = new Foo();
          val valueAst = partialConstructor.blockAst
          Seq(callAst(callNode, targetAst ++ Seq(valueAst)))
      }
    }
  }

  private[expressions] def completeInitForConstructor(partialConstructor: PartialConstructor, targetAst: Ast): Ast = {
    val initNode = partialConstructor.initNode
    val args     = partialConstructor.initArgs

    targetAst.root match {
      case Some(identifier: NewIdentifier) =>
        scope.lookupVariable(identifier.name).variableNode.foreach { variableNode =>
          diffGraph.addEdge(identifier, variableNode, EdgeTypes.REF)
        }

      case _ => // Nothing to do in this case
    }

    val initAst = Ast(initNode)

    val capturedThis = scope.lookupVariable(NameConstants.This) match {
      case SimpleVariable(param: ScopeParameter) => Some(param.node)
      case _                                     => None
    }

    for {
      enclosingDecl <- scope.enclosingTypeDecl;
      typeFullName  <- partialConstructor.typeFullName
    } enclosingDecl.registerInitToComplete(PartialInit(typeFullName, initAst, targetAst, args.toList, capturedThis))

    initAst
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
          Some(newVariableNodeType(correspondingNode)),
          line(originNode),
          column(originNode)
        )

      case variable =>
        val node = identifierNode(variableDeclarator, variable.name, variable.name, newVariableNodeType(variable))
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

  private def astsForAssignment(
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

    assignmentNode.typeFullName = target.rootType.getOrElse(TypeConstants.Any)

    val initializerAsts = astsForExpression(initializer, expectedType)

    val constructorAsts = partialConstructorQueue.map(completeInitForConstructor(_, copyAstForVarDeclInit(target)))
    partialConstructorQueue.clear()

    val assignmentAst = callAst(assignmentNode, target :: initializerAsts.toList)

    assignmentAst :: constructorAsts.toList
  }
}
