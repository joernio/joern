package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.{Ident, SelectorExpr}
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import ujson.Value

trait AstForMethodCallExpressionCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForCallExpression(expr: ParserNodeInfo): Seq[Ast] = {
    val funcDetails = createParserNodeInfo(expr.json(ParserKeys.Fun))
    val (alias, methodName) = funcDetails.node match
      case Ident =>
        (None, funcDetails.json(ParserKeys.Name).str)
      case SelectorExpr =>
        (funcDetails.json(ParserKeys.X)(ParserKeys.Name).strOpt, funcDetails.json(ParserKeys.Sel)(ParserKeys.Name).str)
    val (signature, fullName, typeFullName) =
      callMethodFullNameTypeFullNameAndSignature(methodName, alias, expr.json(ParserKeys.Args))
    val cpgCall = callNode(
      expr,
      expr.code,
      methodName,
      fullName,
      DispatchTypes.STATIC_DISPATCH,
      Some(signature),
      Some(typeFullName)
    )
    Seq(callAst(cpgCall, astForArgs(expr.json(ParserKeys.Args))))
  }
  private def astForArgs(args: Value): Seq[Ast] = {
    args.arrOpt
      .getOrElse(Seq.empty)
      .flatMap(x => {
        val primitiveNode = createParserNodeInfo(x)
        astForPrimitive(primitiveNode)
      })
      .toSeq
  }
  private def callMethodFullNameTypeFullNameAndSignature(
    methodName: String,
    aliasName: Option[String] = None,
    args: Value
  ): (String, String, String) = {
    // NOTE: There is an assumption that the import nodes have been processed before this method is being called
    // and mapping of alias to their respective namespace is already done.
    aliasName match
      case None =>
        // NOTE: If the given type is not found in primitiveTypeMap.
        // Then we are assuming the type is custom type defined inside same pacakge as that of current file's package.
        // This assumption will be invalid when another package is imported with alias "."
        Defines.builtinFunctions.getOrElse(
          methodName,
          (s"$fullyQualifiedPackage.$methodName()", s"$fullyQualifiedPackage.$methodName", Defines.tobeFilled)
        )
      case Some(alias) =>
        (
          s"${aliasToNameSpaceMapping.getOrElse(alias, s"${XDefines.Unknown}.<$alias>")}.$methodName()",
          s"${aliasToNameSpaceMapping.getOrElse(alias, s"${XDefines.Unknown}.<$alias>")}.$methodName",
          Defines.tobeFilled
        )
  }
}
