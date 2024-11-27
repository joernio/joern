package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.expr.{
  AnnotationExpr,
  ArrayAccessExpr,
  ArrayCreationExpr,
  ArrayInitializerExpr,
  AssignExpr,
  BinaryExpr,
  CastExpr,
  ClassExpr,
  ConditionalExpr,
  EnclosedExpr,
  Expression,
  FieldAccessExpr,
  InstanceOfExpr,
  LambdaExpr,
  LiteralExpr,
  MethodCallExpr,
  MethodReferenceExpr,
  NameExpr,
  ObjectCreationExpr,
  SuperExpr,
  ThisExpr,
  UnaryExpr,
  VariableDeclarationExpr
}
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.x2cpg.Ast

trait AstForExpressionsCreator
    extends AstForSimpleExpressionsCreator
    with AstForLambdasCreator
    with AstForCallExpressionsCreator
    with AstForNameExpressionsCreator
    with AstForPatternExpressionsCreator
    with AstForVarDeclAndAssignsCreator { this: AstCreator =>
  def astsForExpression(expression: Expression, expectedType: ExpectedType): Seq[Ast] = {
    // TODO: Implement missing handlers
    // case _: MethodReferenceExpr     => Seq()
    // case _: PatternExpr             => Seq()
    // case _: SuperExpr               => Seq()
    // case _: SwitchExpr              => Seq()
    // case _: TypeExpr                => Seq()
    expression match {
      case _: AnnotationExpr          => Seq()
      case x: ArrayAccessExpr         => Seq(astForArrayAccessExpr(x, expectedType))
      case x: ArrayCreationExpr       => Seq(astForArrayCreationExpr(x, expectedType))
      case x: ArrayInitializerExpr    => Seq(astForArrayInitializerExpr(x, expectedType))
      case x: AssignExpr              => astsForAssignExpr(x, expectedType)
      case x: BinaryExpr              => Seq(astForBinaryExpr(x, expectedType))
      case x: CastExpr                => Seq(astForCastExpr(x, expectedType))
      case x: ClassExpr               => Seq(astForClassExpr(x))
      case x: ConditionalExpr         => Seq(astForConditionalExpr(x, expectedType))
      case x: EnclosedExpr            => astForEnclosedExpression(x, expectedType)
      case x: FieldAccessExpr         => Seq(astForFieldAccessExpr(x, expectedType))
      case x: InstanceOfExpr          => Seq(astForInstanceOfExpr(x))
      case x: LambdaExpr              => Seq(astForLambdaExpr(x, expectedType))
      case x: LiteralExpr             => Seq(astForLiteralExpr(x))
      case x: MethodCallExpr          => Seq(astForMethodCall(x, expectedType))
      case x: MethodReferenceExpr     => Seq(astForMethodReferenceExpr(x, expectedType))
      case x: NameExpr                => Seq(astForNameExpr(x, expectedType))
      case x: ObjectCreationExpr      => Seq(blockAstForObjectCreationExpr(x, expectedType))
      case x: SuperExpr               => Seq(astForSuperExpr(x, expectedType))
      case x: ThisExpr                => Seq(astForThisExpr(x, expectedType))
      case x: UnaryExpr               => Seq(astForUnaryExpr(x, expectedType))
      case x: VariableDeclarationExpr => astsForVariableDeclarationExpr(x)
      case x                          => Seq(unknownAst(x))
    }
  }
}
