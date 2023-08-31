package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import ujson.Value

trait AstForMethodCallExpressionCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForCallExpression(expr: ParserNodeInfo): Seq[Ast] = {
    val methodName = expr.json(ParserKeys.Fun)(ParserKeys.Name).str
    val cpgCall = callNode(
      expr,
      expr.code,
      methodName,
      callMethodFullName(methodName),
      DispatchTypes.STATIC_DISPATCH,
      Some(callMethodSignature(methodName, expr.json(ParserKeys.Args))),
      Some(returnTypeFullName())
    )
    Seq(callAst(cpgCall, astForArgs(expr.json(ParserKeys.Args))))
  }
  private def astForArgs(args: Value): Seq[Ast] = {
    Seq.empty
  }
  private def returnTypeFullName(): String = {
    ""
  }
  private def callMethodFullName(methodName: String): String = {
    s"$fullyQualifiedPackage.$methodName"
  }
  private def callMethodSignature(methodName: String, args: Value): String = {
    s"$fullyQualifiedPackage.$methodName()"
  }
}
