package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.body.VariableDeclarator
import com.github.javaparser.ast.expr.AssignExpr.Operator
import com.github.javaparser.ast.expr.{AssignExpr, VariableDeclarationExpr}
import com.github.javaparser.resolution.types.ResolvedType
import io.joern.javasrc2cpg.astcreation.expressions.AstForCallExpressionsCreator.PartialConstructor
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.scope.Scope.{ScopeMember, ScopeParameter, ScopeStaticImport, SimpleVariable}
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

  private[expressions] def astsForVariableDecl(varDecl: VariableDeclarationExpr): Seq[Ast] = {
    val locals    = localsForVarDecl(varDecl)
    val localAsts = locals.map { Ast(_) }

    locals.foreach { local =>
      scope.enclosingBlock.get.addLocal(local)
    }

    val assignments =
      assignmentsForVarDecl(varDecl.getVariables.asScala)

    localAsts ++ assignments
  }

  private def localsForVarDecl(varDecl: VariableDeclarationExpr): List[NewLocal] = {
    varDecl.getVariables.asScala.map { variable =>
      val name = variable.getName.toString
      val typeFullName =
        tryWithSafeStackOverflow(
          scope
            .lookupType(variable.getTypeAsString)
            .orElse(typeInfoCalc.fullName(variable.getType))
        ).toOption.flatten.getOrElse(TypeConstants.Any)
      val code = s"${variable.getType} $name"
      NewLocal()
        .name(name)
        .code(code)
        .typeFullName(typeFullName)
        .lineNumber(line(varDecl))
        .columnNumber(column(varDecl))
    }.toList
  }

  def assignmentsForVarDecl(variables: Iterable[VariableDeclarator]): Seq[Ast] = {
    val variablesWithInitializers =
      variables.filter(_.getInitializer.toScala.isDefined)
    val assignments = variablesWithInitializers.flatMap { variable =>
      val name              = variable.getName.toString
      val initializer       = variable.getInitializer.toScala.get // Won't crash because of filter
      val javaParserVarType = variable.getTypeAsString
      val typeFullName =
        scope
          .lookupType(javaParserVarType, includeWildcards = false)
          .orElse(tryWithSafeStackOverflow(typeInfoCalc.fullName(variable.getType)).toOption.flatten)
          // TODO: Surely the variable being declared can't already be in scope?
          .orElse(scope.lookupVariable(name).typeFullName)

      // Need the actual resolvedType here for when the RHS is a lambda expression.
      val resolvedExpectedType =
        tryWithSafeStackOverflow(symbolSolver.toResolvedType(variable.getType, classOf[ResolvedType])).toOption
      val initializerAsts = astsForExpression(initializer, ExpectedType(typeFullName, resolvedExpectedType))

      val typeName = typeFullName
        .map(TypeNodePass.fullToShortName)
        .getOrElse(s"${Defines.UnresolvedNamespace}.${variable.getTypeAsString}")
      val code = s"$typeName $name = ${initializerAsts.rootCodeOrEmpty}"

      val callNode = newOperatorCallNode(Operators.assignment, code, typeFullName, line(variable), column(variable))

      val targetAst = scope.lookupVariable(name).getVariable() match {
        case Some(member: ScopeMember) =>
          val baseName =
            if (member.isStatic)
              scope.enclosingTypeDecl.name.getOrElse(NameConstants.Unknown)
            else
              NameConstants.This

          val thisType = scope.enclosingTypeDecl.fullName.getOrElse(TypeConstants.Any)
          fieldAccessAst(
            baseName,
            scope.enclosingTypeDecl.fullName,
            name,
            Some(member.typeFullName),
            line(variable),
            column(variable)
          )

        case Some(staticImport: ScopeStaticImport) =>
          val targetName = staticImport.typeFullName.stripSuffix(s".${staticImport.name}")
          val fieldName  = staticImport.name
          fieldAccessAst(targetName, Some(targetName), fieldName, typeFullName, line(variable), column(variable))

        case maybeCorrespNode =>
          val identifier = identifierNode(variable, name, name, typeFullName.getOrElse(TypeConstants.Any))
          Ast(identifier).withRefEdges(identifier, maybeCorrespNode.map(_.node).toList)
      }

      val declAst = callAst(callNode, Seq(targetAst) ++ initializerAsts)

      // Since all partial constructors will be dealt with here, don't pass them up.
      val constructorAsts = partialConstructorQueue.map(completeInitForConstructor(_, copyAstForVarDeclInit(targetAst)))
      partialConstructorQueue.clear()

      Seq(declAst) ++ constructorAsts
    }

    assignments.toList
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
}
