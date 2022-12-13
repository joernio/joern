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
      case Some(Defines.ANY) => typeFor(ident)
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
    Ast(createLiteralNode(nullLiteral.code, Some(Defines.NULL), nullLiteral.lineNumber, nullLiteral.columnNumber))

  protected def astForStringLiteral(stringLiteral: BabelNodeInfo): Ast = {
    val code = s"\"${stringLiteral.json("value").str}\""
    Ast(createLiteralNode(code, Some(Defines.STRING), stringLiteral.lineNumber, stringLiteral.columnNumber))
  }

  protected def astForSpreadOrRestElement(spreadElement: BabelNodeInfo, arg1Ast: Option[Ast] = None): Ast = {
    val ast = astForNodeWithFunctionReference(spreadElement.json("argument"))
    val callNode = createCallNode(
      spreadElement.code,
      "<operator>.starredUnpack",
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
        Some(Defines.STRING),
        templateElement.lineNumber,
        templateElement.columnNumber
      )
    )

  protected def astForRegExpLiteral(regExpLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(regExpLiteral.code, Some(Defines.STRING), regExpLiteral.lineNumber, regExpLiteral.columnNumber)
    )

  protected def astForRegexLiteral(regexLiteral: BabelNodeInfo): Ast =
    Ast(createLiteralNode(regexLiteral.code, Some(Defines.STRING), regexLiteral.lineNumber, regexLiteral.columnNumber))

  protected def astForNumberLiteral(numberLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(numberLiteral.code, Some(Defines.NUMBER), numberLiteral.lineNumber, numberLiteral.columnNumber)
    )

  protected def astForNumericLiteral(numericLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        numericLiteral.code,
        Some(Defines.NUMBER),
        numericLiteral.lineNumber,
        numericLiteral.columnNumber
      )
    )

  protected def astForDecimalLiteral(decimalLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        decimalLiteral.code,
        Some(Defines.NUMBER),
        decimalLiteral.lineNumber,
        decimalLiteral.columnNumber
      )
    )

  protected def astForBigIntLiteral(bigIntLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(bigIntLiteral.code, Some(Defines.NUMBER), bigIntLiteral.lineNumber, bigIntLiteral.columnNumber)
    )

  protected def astForBooleanLiteral(booleanLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        booleanLiteral.code,
        Some(Defines.BOOLEAN),
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
      val callName = "__Runtime.TO_STRING"
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
        List(astForNode(quasi), astForNode(expression))
      }
      val argAsts = argumentAsts :+ astForNode(quasisTail)
      createCallAst(templateCall, argAsts)
    }
  }
}
