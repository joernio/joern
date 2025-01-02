package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.expr.{
  ArrayAccessExpr,
  ArrayCreationExpr,
  ArrayInitializerExpr,
  BinaryExpr,
  CastExpr,
  ClassExpr,
  ConditionalExpr,
  EnclosedExpr,
  Expression,
  FieldAccessExpr,
  InstanceOfExpr,
  LiteralExpr,
  MethodReferenceExpr,
  NameExpr,
  SuperExpr,
  ThisExpr,
  TypeExpr,
  UnaryExpr
}
import com.github.javaparser.ast.nodeTypes.NodeWithName
import com.github.javaparser.ast.visitor.NodeFinderVisitor
import com.github.javaparser.symbolsolver.javaparsermodel.contexts.{BinaryExprContext, ConditionalExprContext}
import com.github.javaparser.symbolsolver.resolution.typesolvers.CombinedTypeSolver
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.scope.PatternVariableInfo
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.{NameConstants, Util}
import io.joern.x2cpg.{Ast, Defines}
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.joern.x2cpg.utils.NodeBuilders.{newIdentifierNode, newOperatorCallNode}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, NewCall, NewFieldIdentifier, NewLiteral, NewTypeRef}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

trait AstForSimpleExpressionsCreator { this: AstCreator =>

  private[expressions] def astForArrayAccessExpr(expr: ArrayAccessExpr, expectedType: ExpectedType): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(getTypeFullName(expectedType))
        .map(typeInfoCalc.registerType)
        .getOrElse(defaultTypeFallback())
    val callNode = newOperatorCallNode(
      Operators.indexAccess,
      code = expr.toString,
      typeFullName = Some(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    val arrayExpectedType = expectedType.copy(fullName = expectedType.fullName.map(_ ++ "[]"))
    val nameAst           = astsForExpression(expr.getName, arrayExpectedType)
    val indexAst          = astsForExpression(expr.getIndex, ExpectedType.Int)
    val args              = nameAst ++ indexAst
    callAst(callNode, args)
  }

  private[expressions] def astForArrayCreationExpr(expr: ArrayCreationExpr, expectedType: ExpectedType): Ast = {
    val elementType = tryWithSafeStackOverflow(expr.getElementType.resolve()).map(elementType =>
      ExpectedType(typeInfoCalc.fullName(elementType).map(_ ++ "[]"), Option(elementType))
    )
    val maybeInitializerAst =
      expr.getInitializer.toScala.map(astForArrayInitializerExpr(_, elementType.getOrElse(expectedType)))

    maybeInitializerAst.flatMap(_.root) match {
      case Some(initializerRoot: NewCall) => initializerRoot.code(expr.toString)
      case _                              => // This should never happen
    }

    maybeInitializerAst.getOrElse {
      val typeFullName = expressionReturnTypeFullName(expr)
        .orElse(getTypeFullName(expectedType))
        .map(typeInfoCalc.registerType)
        .getOrElse(defaultTypeFallback(expr.getElementType))
      val callNode = newOperatorCallNode(Operators.alloc, code = expr.toString, typeFullName = Some(typeFullName))
      val levelAsts = expr.getLevels.asScala.flatMap { lvl =>
        lvl.getDimension.toScala match {
          case Some(dimension) => astsForExpression(dimension, ExpectedType.Int)

          case None => Seq.empty
        }
      }.toSeq
      callAst(callNode, levelAsts)
    }
  }

  private[expressions] def astForArrayInitializerExpr(expr: ArrayInitializerExpr, expectedType: ExpectedType): Ast = {
    // In the expression `new int[] { 1, 2 }`, the ArrayInitializerExpr is only the `{ 1, 2 }` part and does not have
    // a type itself. We need to use the expected type from the parent expr here.
    val typeFullName = getTypeFullName(expectedType)
      .map(typeInfoCalc.registerType)
      .getOrElse(defaultTypeFallback())

    val callNode = newOperatorCallNode(
      Operators.arrayInitializer,
      code = expr.toString,
      typeFullName = Some(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    val MAX_INITIALIZERS = 1000

    val expectedValueType = expr.getValues.asScala.headOption.map { value =>
      // typeName and resolvedType may represent different types since typeName can fall
      // back to known information or primitive types. While this certainly isn't ideal,
      // it shouldn't cause issues since resolvedType is only used where the extra type
      // information not available in typeName is necessary.
      val typeName     = expressionReturnTypeFullName(value).map(typeInfoCalc.registerType)
      val resolvedType = tryWithSafeStackOverflow(value.calculateResolvedType()).toOption
      ExpectedType(typeName, resolvedType)
    }
    val args = expr.getValues.asScala
      .slice(0, MAX_INITIALIZERS)
      .flatMap(astsForExpression(_, expectedValueType.getOrElse(ExpectedType.empty)))
      .toSeq

    val ast = callAst(callNode, args)

    if (expr.getValues.size() > MAX_INITIALIZERS) {
      val placeholder = NewLiteral()
        .typeFullName(defaultTypeFallback())
        .code("<too-many-initializers>")
        .lineNumber(line(expr))
        .columnNumber(column(expr))
      ast.withChild(Ast(placeholder)).withArgEdge(callNode, placeholder)
    } else {
      ast
    }
  }

  private[expressions] def astForBinaryExpr(expr: BinaryExpr, expectedType: ExpectedType): Ast = {
    val operatorName = expr.getOperator match {
      case BinaryExpr.Operator.OR                   => Operators.logicalOr
      case BinaryExpr.Operator.AND                  => Operators.logicalAnd
      case BinaryExpr.Operator.BINARY_OR            => Operators.or
      case BinaryExpr.Operator.BINARY_AND           => Operators.and
      case BinaryExpr.Operator.DIVIDE               => Operators.division
      case BinaryExpr.Operator.EQUALS               => Operators.equals
      case BinaryExpr.Operator.GREATER              => Operators.greaterThan
      case BinaryExpr.Operator.GREATER_EQUALS       => Operators.greaterEqualsThan
      case BinaryExpr.Operator.LESS                 => Operators.lessThan
      case BinaryExpr.Operator.LESS_EQUALS          => Operators.lessEqualsThan
      case BinaryExpr.Operator.LEFT_SHIFT           => Operators.shiftLeft
      case BinaryExpr.Operator.SIGNED_RIGHT_SHIFT   => Operators.logicalShiftRight
      case BinaryExpr.Operator.UNSIGNED_RIGHT_SHIFT => Operators.arithmeticShiftRight
      case BinaryExpr.Operator.XOR                  => Operators.xor
      case BinaryExpr.Operator.NOT_EQUALS           => Operators.notEquals
      case BinaryExpr.Operator.PLUS                 => Operators.addition
      case BinaryExpr.Operator.MINUS                => Operators.subtraction
      case BinaryExpr.Operator.MULTIPLY             => Operators.multiplication
      case BinaryExpr.Operator.REMAINDER            => Operators.modulo
    }

    val lhsArgs = astsForExpression(expr.getLeft, expectedType)
    // TODO Fix code
    // val lhsCode = lhsArgs.headOption.flatMap(_.rootCode).getOrElse("")

    scope.pushBlockScope()
    val context = new BinaryExprContext(expr, new CombinedTypeSolver())

    context
      .typePatternExprsExposedToChild(expr.getRight)
      .asScala
      .flatMap(pattern => scope.enclosingMethod.flatMap(_.getPatternVariableInfo(pattern)))
      .foreach { case PatternVariableInfo(pattern, local, _, _, _, _) =>
        scope.enclosingBlock.foreach(_.addPatternLocal(local, pattern))
      }

    val rhsArgs = astsForExpression(expr.getRight, expectedType)
    // TODO Fix code
    // val rhsCode = rhsArgs.headOption.flatMap(_.rootCode).getOrElse("")
    scope.popBlockScope()

    val args = lhsArgs ++ rhsArgs

    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(args.headOption.flatMap(_.rootType))
        .orElse(args.lastOption.flatMap(_.rootType))
        .orElse(getTypeFullName(expectedType))
        .map(typeInfoCalc.registerType)
        .getOrElse(defaultTypeFallback())

    val callNode = newOperatorCallNode(
      operatorName,
      // code = s"$lhsCode ${expr.getOperator.asString()} $rhsCode",
      code = code(expr),
      typeFullName = Some(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    callAst(callNode, args)
  }

  private[expressions] def astForCastExpr(expr: CastExpr, expectedType: ExpectedType): Ast = {
    val typeFullName =
      tryWithSafeStackOverflow(expr.getType).toOption
        .map { typ =>
          typeInfoCalc
            .fullName(typ)
            .orElse(getTypeFullName(expectedType))
            .getOrElse(defaultTypeFallback(typ))
        }
        .getOrElse(defaultTypeFallback())

    val callNode = newOperatorCallNode(
      Operators.cast,
      code = expr.toString,
      typeFullName = Option(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    val typeNode = NewTypeRef()
      .code(tryWithSafeStackOverflow(expr.getType.toString).getOrElse(code(expr)))
      .lineNumber(line(expr))
      .columnNumber(column(expr))
      .typeFullName(typeFullName)
    val typeAst = Ast(typeNode)

    val exprAst = astsForExpression(expr.getExpression, ExpectedType.empty)

    callAst(callNode, Seq(typeAst) ++ exprAst)
  }

  private[expressions] def astForClassExpr(expr: ClassExpr): Ast = {
    val someTypeFullName = Some(TypeConstants.Class)
    val callNode = newOperatorCallNode(Operators.fieldAccess, expr.toString, someTypeFullName, line(expr), column(expr))

    val identifierType = tryWithSafeStackOverflow(expr.getType).toOption.flatMap(typeInfoCalc.fullName)
    val exprTypeString =
      tryWithSafeStackOverflow(expr.getTypeAsString).toOption
        .orElse(identifierType)
        .getOrElse(code(expr).stripSuffix(".class"))
    val identifier =
      identifierNode(expr, Util.stripGenericTypes(exprTypeString), exprTypeString, identifierType.getOrElse("ANY"))
    val idAst = Ast(identifier)

    val fieldIdentifier = NewFieldIdentifier()
      .canonicalName("class")
      .code("class")
      .lineNumber(line(expr))
      .columnNumber(column(expr))
    val fieldIdAst = Ast(fieldIdentifier)

    callAst(callNode, Seq(idAst, fieldIdAst))
  }

  private[expressions] def astForConditionalExpr(expr: ConditionalExpr, expectedType: ExpectedType): Ast = {
    val condAst = astsForExpression(expr.getCondition, ExpectedType.Boolean)

    val context = new ConditionalExprContext(expr, new CombinedTypeSolver())

    val patternsExposedToThen = context.typePatternExprsExposedToChild(expr.getThenExpr).asScala.toList
    val patternsExposedToElse = context.typePatternExprsExposedToChild(expr.getElseExpr).asScala.toList

    scope.pushBlockScope()
    scope.addLocalsForPatternsToEnclosingBlock(patternsExposedToThen)
    val thenAst = astsForExpression(expr.getThenExpr, expectedType)
    scope.popBlockScope()

    scope.pushBlockScope()
    scope.addLocalsForPatternsToEnclosingBlock(patternsExposedToElse)
    val elseAst = astsForExpression(expr.getElseExpr, expectedType)
    scope.popBlockScope()

    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(thenAst.headOption.flatMap(_.rootType))
        .orElse(elseAst.headOption.flatMap(_.rootType))
        .orElse(getTypeFullName(expectedType))
        .map(typeInfoCalc.registerType)
        .getOrElse(defaultTypeFallback())

    val callNode =
      newOperatorCallNode(Operators.conditional, expr.toString, Some(typeFullName), line(expr), column(expr))

    callAst(callNode, condAst ++ thenAst ++ elseAst)
  }

  private[expressions] def astForEnclosedExpression(expr: EnclosedExpr, expectedType: ExpectedType): Seq[Ast] = {
    astsForExpression(expr.getInner, expectedType)
  }

  private[expressions] def astForFieldAccessExpr(expr: FieldAccessExpr, expectedType: ExpectedType): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(getTypeFullName(expectedType))
        .map(typeInfoCalc.registerType)
        .getOrElse(defaultTypeFallback())

    val fieldIdentifier = expr.getName
    val identifierAsts  = astsForExpression(expr.getScope, ExpectedType.empty)

    fieldAccessAst(
      identifierAsts.head,
      expr.toString,
      line(expr),
      column(expr),
      fieldIdentifier.toString,
      typeFullName,
      line(fieldIdentifier),
      column(fieldIdentifier)
    )
  }

  private[expressions] def astForInstanceOfExpr(expr: InstanceOfExpr): Ast = {
    // TODO: handle multiple ASTs
    val lhsAst = astsForExpression(expr.getExpression, ExpectedType.empty).head
    expr.getPattern.toScala
      .map { patternExpression =>
        instanceOfAstForPattern(patternExpression, lhsAst)
      }
      .getOrElse {
        val booleanTypeFullName = Some(TypeConstants.Boolean)
        val callNode =
          newOperatorCallNode(Operators.instanceOf, expr.toString, booleanTypeFullName, line(expr), column(expr))

        val exprAst  = astsForExpression(expr.getExpression, ExpectedType.empty)
        val exprType = tryWithSafeStackOverflow(expr.getType).toOption
        val typeFullName = exprType
          .map(typ => typeInfoCalc.fullName(typ).getOrElse(defaultTypeFallback(typ)))
          .getOrElse(defaultTypeFallback())
        val typeNode =
          NewTypeRef()
            .code(exprType.map(_.toString).getOrElse(code(expr).split("instanceof").lastOption.getOrElse("")))
            .lineNumber(line(expr))
            .columnNumber(exprType.map(column(_)).getOrElse(column(expr)))
            .typeFullName(typeFullName)
        val typeAst = Ast(typeNode)

        callAst(callNode, exprAst ++ Seq(typeAst))
      }
  }

  private[expressions] def astForLiteralExpr(expr: LiteralExpr): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(expr).map(typeInfoCalc.registerType).getOrElse(defaultTypeFallback())
    val literalNode =
      NewLiteral()
        .code(code(expr))
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .typeFullName(typeFullName)
    Ast(literalNode)
  }

  private[expressions] def astForSuperExpr(superExpr: SuperExpr, expectedType: ExpectedType): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(superExpr)
        .orElse(getTypeFullName(expectedType))
        .map(typeInfoCalc.registerType)
        .getOrElse(defaultTypeFallback())

    val identifier = identifierNode(superExpr, NameConstants.This, NameConstants.Super, typeFullName)
    Ast(identifier)
  }

  private[expressions] def astForThisExpr(expr: ThisExpr, expectedType: ExpectedType): Ast = {
    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(getTypeFullName(expectedType))
        .map(typeInfoCalc.registerType)

    val identifier = identifierNode(expr, expr.toString, expr.toString, typeFullName.getOrElse("ANY"))
    val thisParam  = scope.lookupVariable(NameConstants.This).variableNode

    thisParam.foreach { thisNode =>
      diffGraph.addEdge(identifier, thisNode, EdgeTypes.REF)
    }

    Ast(identifier)
  }

  private[expressions] def astForUnaryExpr(expr: UnaryExpr, expectedType: ExpectedType): Ast = {
    val operatorName = expr.getOperator match {
      case UnaryExpr.Operator.LOGICAL_COMPLEMENT => Operators.logicalNot
      case UnaryExpr.Operator.POSTFIX_DECREMENT  => Operators.postDecrement
      case UnaryExpr.Operator.POSTFIX_INCREMENT  => Operators.postIncrement
      case UnaryExpr.Operator.PREFIX_DECREMENT   => Operators.preDecrement
      case UnaryExpr.Operator.PREFIX_INCREMENT   => Operators.preIncrement
      case UnaryExpr.Operator.BITWISE_COMPLEMENT => Operators.not
      case UnaryExpr.Operator.PLUS               => Operators.plus
      case UnaryExpr.Operator.MINUS              => Operators.minus
    }

    val argsAsts = astsForExpression(expr.getExpression, expectedType)

    val typeFullName =
      expressionReturnTypeFullName(expr)
        .orElse(argsAsts.headOption.flatMap(_.rootType))
        .orElse(getTypeFullName(expectedType))
        .map(typeInfoCalc.registerType)
        .getOrElse(defaultTypeFallback())

    // TODO Fix code
    val callNode = newOperatorCallNode(
      operatorName,
      code = expr.toString,
      typeFullName = Some(typeFullName),
      line = line(expr),
      column = column(expr)
    )

    callAst(callNode, argsAsts)
  }

  private[expressions] def astForMethodReferenceExpr(expr: MethodReferenceExpr, expectedType: ExpectedType): Ast = {
    val typeFullName = expr.getScope match {
      case typeExpr: TypeExpr =>
        val rawType = tryWithSafeStackOverflow(typeExpr.getTypeAsString).map(Util.stripGenericTypes).toOption
        // JavaParser wraps the "type" scope of a MethodReferenceExpr in a TypeExpr, but this also catches variable names.
        rawType.flatMap(scope.lookupVariableOrType).orElse(expressionReturnTypeFullName(typeExpr))
      case scopeExpr => expressionReturnTypeFullName(scopeExpr)
    }

    val namespacePrefix = typeFullName.getOrElse(Defines.UnresolvedNamespace)
    val methodName      = expr.getIdentifier

    val signature = tryWithSafeStackOverflow(expr.resolve()) match {
      case Failure(_) => Defines.UnresolvedSignature

      case Success(resolvedMethod) =>
        val returnType = tryWithSafeStackOverflow(resolvedMethod.getReturnType).toOption.flatMap(typeInfoCalc.fullName)
        val parameterTypes = argumentTypesForMethodLike(Option(resolvedMethod))
        composeSignature(returnType, parameterTypes, resolvedMethod.getNumberOfParams)
    }

    val methodFullName = Util.composeMethodFullName(namespacePrefix, methodName, signature)

    Ast(methodRefNode(expr, expr.toString, methodFullName, typeFullName.getOrElse(defaultTypeFallback())))
  }
}
