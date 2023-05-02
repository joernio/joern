package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.DispatchTypes

trait AstForPrimitivesCreator { this: AstCreator =>

  protected def astForIdentifier(ident: BabelNodeInfo, typeFullName: Option[String] = None): Ast = {
    val name      = ident.json("name").str
    val identNode = createIdentifierNode(name, ident)
    val tpe = typeFullName match {
      case Some(Defines.Any) => typeFor(ident)
      case Some(otherType)   => otherType
      case None              => typeFor(ident)
    }
    identNode.typeFullName = tpe
    scope.addVariableReference(name, identNode)
    Ast(identNode)
  }

  protected def astForSuperKeyword(superKeyword: BabelNodeInfo): Ast =
    Ast(createIdentifierNode("super", superKeyword))

  protected def astForImportKeyword(importKeyword: BabelNodeInfo): Ast =
    Ast(createIdentifierNode("import", importKeyword))

  protected def astForNullLiteral(nullLiteral: BabelNodeInfo): Ast =
    Ast(createLiteralNode(nullLiteral.code, Option(Defines.Null), nullLiteral.lineNumber, nullLiteral.columnNumber))

  protected def astForStringLiteral(stringLiteral: BabelNodeInfo): Ast = {
    val code = s"\"${stringLiteral.json("value").str}\""
    Ast(createLiteralNode(code, Option(Defines.String), stringLiteral.lineNumber, stringLiteral.columnNumber))
  }

  protected def astForPrivateName(privateName: BabelNodeInfo): Ast =
    astForIdentifier(createBabelNodeInfo(privateName.json("id")))

  protected def astForSpreadOrRestElement(spreadElement: BabelNodeInfo, arg1Ast: Option[Ast] = None): Ast = {
    val ast = astForNodeWithFunctionReference(spreadElement.json("argument"))
    val callNode = createCallNode(
      spreadElement.code,
      "<operator>.spread",
      DispatchTypes.STATIC_DISPATCH,
      spreadElement.lineNumber,
      spreadElement.columnNumber
    )
    callAst(callNode, arg1Ast.toList :+ ast)
  }

  protected def astForTemplateElement(templateElement: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        s"\"${templateElement.json("value")("raw").str}\"",
        Option(Defines.String),
        templateElement.lineNumber,
        templateElement.columnNumber
      )
    )

  protected def astForRegExpLiteral(regExpLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        regExpLiteral.code,
        Option(Defines.String),
        regExpLiteral.lineNumber,
        regExpLiteral.columnNumber
      )
    )

  protected def astForRegexLiteral(regexLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(regexLiteral.code, Option(Defines.String), regexLiteral.lineNumber, regexLiteral.columnNumber)
    )

  protected def astForNumberLiteral(numberLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        numberLiteral.code,
        Option(Defines.Number),
        numberLiteral.lineNumber,
        numberLiteral.columnNumber
      )
    )

  protected def astForNumericLiteral(numericLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        numericLiteral.code,
        Option(Defines.Number),
        numericLiteral.lineNumber,
        numericLiteral.columnNumber
      )
    )

  protected def astForDecimalLiteral(decimalLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        decimalLiteral.code,
        Option(Defines.Number),
        decimalLiteral.lineNumber,
        decimalLiteral.columnNumber
      )
    )

  protected def astForBigIntLiteral(bigIntLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        bigIntLiteral.code,
        Option(Defines.Number),
        bigIntLiteral.lineNumber,
        bigIntLiteral.columnNumber
      )
    )

  protected def astForBooleanLiteral(booleanLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        booleanLiteral.code,
        Option(Defines.Boolean),
        booleanLiteral.lineNumber,
        booleanLiteral.columnNumber
      )
    )

  protected def astForTemplateLiteral(templateLiteral: BabelNodeInfo): Ast = {
    val expressions = templateLiteral.json("expressions").arr.toList
    val quasis      = templateLiteral.json("quasis").arr.toList.filterNot(_("tail").bool)
    val quasisTail  = templateLiteral.json("quasis").arr.toList.filter(_("tail").bool).head

    if (expressions.isEmpty && quasis.isEmpty) {
      astForTemplateElement(createBabelNodeInfo(quasisTail))
    } else {
      val callName = "<operator>.formatString"
      val argsCodes = expressions.zip(quasis).flatMap { case (expression, quasi) =>
        List(s"\"${quasi("value")("raw").str}\"", code(expression))
      }
      val callCode = s"$callName${(argsCodes :+ s"\"${quasisTail("value")("raw").str}\"").mkString("(", ", ", ")")}"
      val templateCall =
        createCallNode(
          callCode,
          callName,
          DispatchTypes.STATIC_DISPATCH,
          templateLiteral.lineNumber,
          templateLiteral.columnNumber
        )

      val argumentAsts = expressions.zip(quasis).flatMap { case (expression, quasi) =>
        List(astForNodeWithFunctionReference(quasi), astForNodeWithFunctionReference(expression))
      }
      val argAsts = argumentAsts :+ astForNodeWithFunctionReference(quasisTail)
      callAst(templateCall, argAsts)
    }
  }
}
