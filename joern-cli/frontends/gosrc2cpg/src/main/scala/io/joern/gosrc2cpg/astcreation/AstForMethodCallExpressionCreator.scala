package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.{BasicLit, Ident, SelectorExpr}
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import org.slf4j.LoggerFactory
import ujson.Value

trait AstForMethodCallExpressionCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForCallExpression(expr: ParserNodeInfo): Seq[Ast] = {
    val funcDetails = createParserNodeInfo(expr.json(ParserKeys.Fun))
    val (aliasOpt, methodName) = funcDetails.node match
      case Ident =>
        (None, funcDetails.json(ParserKeys.Name).str)
      case SelectorExpr =>
        (
          Some(funcDetails.json(ParserKeys.X)(ParserKeys.Name).str, funcDetails.json(ParserKeys.X)),
          funcDetails.json(ParserKeys.Sel)(ParserKeys.Name).str
        )
      case x =>
        logger.warn(s"Unhandled class ${x.getClass} under astForCallExpression!")
        (None, "")
    val (signature, fullName, typeFullName, thisObjIdentifier) =
      callMethodFullNameTypeFullNameAndSignature(methodName, aliasOpt)
    val cpgCall = callNode(
      expr,
      expr.code,
      methodName,
      fullName,
      DispatchTypes.STATIC_DISPATCH,
      Some(signature),
      Some(typeFullName)
    )
    Seq(callAst(cpgCall, astForArgs(expr.json(ParserKeys.Args)), thisObjIdentifier.headOption))
  }

  protected def astForConstructorCall(compositeLit: ParserNodeInfo): Seq[Ast] = {
    val typeNode = createParserNodeInfo(compositeLit.json(ParserKeys.Type))
    val (alias, methodName) = typeNode.node match
      case Ident =>
        (None, typeNode.json(ParserKeys.Name).str)
      case SelectorExpr =>
        (
          Some(typeNode.json(ParserKeys.X)(ParserKeys.Name).str, typeNode.json(ParserKeys.X)),
          typeNode.json(ParserKeys.Sel)(ParserKeys.Name).str
        )
      case x =>
        logger.warn(s"Unhandled class ${x.getClass} under astForConstructorCall!")
        (None, "")
    val (signature, fullName, _, _) = callMethodFullNameTypeFullNameAndSignature(methodName, alias)

    val cpgCall = callNode(
      compositeLit,
      compositeLit.code,
      methodName,
      fullName + "." + XDefines.ConstructorMethodName,
      DispatchTypes.STATIC_DISPATCH,
      Some(signature),
      Some(fullName)
    )
    Seq(callAst(cpgCall, astForStructureDeclarationArgument(compositeLit.json(ParserKeys.Elts))))
  }

  private def astForStructureDeclarationArgument(args: Value): Seq[Ast] = {
    args.arrOpt
      .getOrElse(Seq.empty)
      .flatMap(x => {
        val argument = createParserNodeInfo(x)
        argument.node match
          case BasicLit => astForNode(argument)
          case _        => astForNode(createParserNodeInfo(argument.json(ParserKeys.Value)))
      })
      .toSeq
  }

  private def astForArgs(args: Value): Seq[Ast] = {
    args.arrOpt
      .getOrElse(Seq.empty)
      .flatMap(x => {
        val argNode = createParserNodeInfo(x)
        astForNode(argNode)
      })
      .toSeq
  }
  private def callMethodFullNameTypeFullNameAndSignature(
    methodName: String,
    aliasName: Option[(String, Value)] = None
  ): (String, String, String, Seq[Ast]) = {
    // NOTE: There is an assumption that the import nodes have been processed before this method is being called
    // and mapping of alias to their respective namespace is already done.
    aliasName match
      case None =>
        // NOTE: If the given type is not found in primitiveTypeMap.
        // Then we are assuming the type is custom type defined inside same pacakge as that of current file's package.
        // This assumption will be invalid when another package is imported with alias "."
        val (signature, fullName, returnTypeFullName) = Defines.builtinFunctions.getOrElse(
          methodName,
          (s"$fullyQualifiedPackage.$methodName()", s"$fullyQualifiedPackage.$methodName", Defines.tobeFilled)
        )
        (signature, fullName, returnTypeFullName, Seq.empty)
      case Some(alias, jsonNode) =>
        // Note check if given alias is an object, in that case we will find the expected variable in scope.
        val variableOption = scope.lookupVariable(alias)
        variableOption match {
          case Some((_, variableTypeName)) =>
            val thisObjIdentifier = astForNode(jsonNode)
            (
              s"$variableTypeName.$methodName()",
              s"$variableTypeName.$methodName",
              Defines.tobeFilled,
              thisObjIdentifier
            )
          case _ =>
            (
              s"${aliasToNameSpaceMapping.getOrElse(alias, s"${XDefines.Unknown}.<$alias>")}.$methodName()",
              s"${aliasToNameSpaceMapping.getOrElse(alias, s"${XDefines.Unknown}.<$alias>")}.$methodName",
              Defines.tobeFilled,
              Seq.empty
            )
        }
  }
}
