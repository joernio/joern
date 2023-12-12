package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, PropertyNames}
import ujson.Value

import scala.util.{Success, Try}

trait AstForMethodCallExpressionCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForCallExpression(expr: ParserNodeInfo): Seq[Ast] = {
    val (methodName, signature, fullName, typeFullName, receiverAst) =
      preReqForCallNode(createParserNodeInfo(expr.json(ParserKeys.Fun)))
    val cpgCall = callNode(
      expr,
      expr.code,
      methodName,
      fullName,
      DispatchTypes.STATIC_DISPATCH,
      Some(signature),
      Some(typeFullName)
    )
    Seq(callAst(cpgCall, astForArgs(expr.json(ParserKeys.Args)), receiverAst.headOption))
  }

  protected def astForConstructorCall(compositeLit: ParserNodeInfo): Seq[Ast] = {
    val (methodName, signature, fullName, _, _) = preReqForCallNode(
      createParserNodeInfo(compositeLit.json(ParserKeys.Type))
    )
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

  private def preReqForCallNode(funcDetails: ParserNodeInfo): (String, String, String, String, Seq[Ast]) = {
    val (aliasOpt, methodName) = funcDetails.node match
      case Ident =>
        (None, funcDetails.json(ParserKeys.Name).str)
      case SelectorExpr =>
        val xNode = createParserNodeInfo(funcDetails.json(ParserKeys.X))
        (Some(xNode), funcDetails.json(ParserKeys.Sel)(ParserKeys.Name).str)
      case x =>
        logger.warn(
          s"Unhandled class ${x.getClass} under astForCallExpression! file -> ${parserResult.fullPath} -> Line no -> ${funcDetails.lineNumber.get}"
        )
        (None, "")
    callMethodFullNameTypeFullNameAndSignature(methodName, aliasOpt)
  }

  private def astForStructureDeclarationArgument(args: Value): Seq[Ast] = {
    args.arrOpt
      .getOrElse(Seq.empty)
      .flatMap(x => {
        val argument = createParserNodeInfo(x)
        argument.node match
          case BasicLit => astForNode(argument)
          case Ident    => astForNode(argument)
          case _        => astForNode(argument)
      })
      .toSeq
  }

  private def astForArgs(args: Value): Seq[Ast] = {
    args.arrOpt
      .getOrElse(Seq.empty)
      .flatMap(x => {
        val argNode = createParserNodeInfo(x)
        argNode.node match
          case MapType  => Seq(Ast(literalNode(argNode, argNode.code, Defines.map)))
          case ChanType => Seq(Ast(literalNode(argNode, argNode.code, Defines.chan)))
          case _        => astForNode(argNode)
      })
      .toSeq
  }

  private def callMethodFullNameTypeFullNameAndSignature(
    methodName: String,
    aliasName: Option[ParserNodeInfo] = None
  ): (String, String, String, String, Seq[Ast]) = {
    // NOTE: There is an assumption that the import nodes have been processed before this method is being called
    // and mapping of alias to their respective namespace is already done.
    aliasName match
      case None =>
        // NOTE: If the given function is not found in builtinFunctions.
        // Then we are assuming that the given function is defined inside same package as that of current file's package.
        // This assumption will be invalid when another package is imported with alias "."
        val methodFullName = s"$fullyQualifiedPackage.$methodName"
        val (returnTypeFullNameCache, signatureCache) =
          goGlobal.methodFullNameReturnTypeMap
            .getOrDefault(methodFullName, (Defines.anyTypeName, s"$methodFullName()"))
        val (signature, fullName, returnTypeFullName) =
          Defines.builtinFunctions.getOrElse(methodName, (signatureCache, methodFullName, returnTypeFullNameCache))
        val probableLambdaTypeFullName = scope.lookupVariable(methodName) match
          case Some((_, lambdaTypeFullName)) => Some(lambdaTypeFullName)
          case _ =>
            Option(goGlobal.structTypeMemberTypeMapping.get(methodFullName)) match
              case Some(globalLambdaTypeFullName) => Some(globalLambdaTypeFullName)
              case _                              => None
        val (postLambdaFullname, postLambdaSignature, postLambdaReturnTypeFullName) = probableLambdaTypeFullName match
          case Some(lambdaTypeFullName) =>
            Option(
              goGlobal.methodFullNameReturnTypeMap
                .get(lambdaTypeFullName)
            ) match
              case Some((lambdaReturnTypeFullNameCache, lambdaSignatureCache)) =>
                (lambdaTypeFullName, lambdaSignatureCache, lambdaReturnTypeFullNameCache)
              case _ => (fullName, signature, returnTypeFullName)
          case _ =>
            (fullName, signature, returnTypeFullName)
        (methodName, postLambdaSignature, postLambdaFullname, postLambdaReturnTypeFullName, Seq.empty)
      case Some(xnode) =>
        xnode.node match
          case Ident =>
            Try(xnode.json(ParserKeys.Obj)) match
              case Success(_) =>
                // The presence of "Obj" field indicates its variable identifier and not an alias
                processReceiverAst(methodName, xnode)
              case _ =>
                // Otherwise its an alias to imported namespace on which method call is made
                val alias = xnode.json(ParserKeys.Name).str
                val callMethodFullName =
                  resolveAliasToFullName(alias, methodName)
                val lambdaFullName =
                  goGlobal.structTypeMemberTypeMapping.getOrDefault(callMethodFullName, callMethodFullName)
                val (returnTypeFullNameCache, signatureCache) = Option(
                  goGlobal.methodFullNameReturnTypeMap
                    .get(lambdaFullName)
                ) match
                  case Some((returnTypeFullName, signature)) => (returnTypeFullName, signature)
                  case _ => (s"$callMethodFullName.${Defines.ReturnType}.${XDefines.Unknown}", s"$callMethodFullName()")

                (methodName, signatureCache, lambdaFullName, returnTypeFullNameCache, Seq.empty)
          case _ =>
            // This will take care of chained method calls. It will call `astForCallExpression` in recursive way,
            // and the call node is used as receiver to this current call node.
            processReceiverAst(methodName, xnode)
  }

  private def processReceiverAst(
    methodName: String,
    xnode: ParserNodeInfo
  ): (String, String, String, String, Seq[Ast]) = {
    val receiverAst = astForNode(xnode)
    val receiverTypeFullName =
      receiverAst.headOption
        .flatMap(_.root)
        .map(_.properties.get(PropertyNames.TYPE_FULL_NAME).get.toString)
        .getOrElse(Defines.anyTypeName)
        .stripPrefix("*")
    val callMethodFullName = s"$receiverTypeFullName.$methodName"
    val (returnTypeFullNameCache, signatureCache) =
      goGlobal.methodFullNameReturnTypeMap
        .getOrDefault(
          callMethodFullName,
          (s"$receiverTypeFullName.$methodName.${Defines.ReturnType}.${XDefines.Unknown}", s"$callMethodFullName()")
        )
    (methodName, signatureCache, callMethodFullName, returnTypeFullNameCache, receiverAst)
  }
}
