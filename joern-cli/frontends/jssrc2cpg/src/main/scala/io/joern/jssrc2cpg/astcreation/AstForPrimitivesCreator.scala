package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.DispatchTypes

trait AstForPrimitivesCreator {

  this: AstCreator =>

  protected def astForIdentifier(ident: BabelNodeInfo): Ast = {
    val name      = ident.json("name").str
    val identNode = createIdentifierNode(name, ident)
    val tpe       = typeFor(ident)
    identNode.typeFullName = tpe
    scope.addVariableReference(name, identNode)
    Ast(identNode)
  }

  protected def astForSuperKeyword(superKeyword: BabelNodeInfo): Ast =
    Ast(createIdentifierNode("super", superKeyword))

  protected def astForImportKeyword(importKeyword: BabelNodeInfo): Ast =
    Ast(createIdentifierNode("import", importKeyword))

  protected def astForNullLiteral(nullLiteral: BabelNodeInfo): Ast =
    Ast(createLiteralNode(nullLiteral.code, Some(Defines.NULL.label), nullLiteral.lineNumber, nullLiteral.columnNumber))

  protected def astForStringLiteral(stringLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        stringLiteral.code,
        Some(Defines.STRING.label),
        stringLiteral.lineNumber,
        stringLiteral.columnNumber
      )
    )

  protected def astForTemplateElement(templateElement: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        s"\"${templateElement.json("value")("raw").str}\"",
        Some(Defines.STRING.label),
        templateElement.lineNumber,
        templateElement.columnNumber
      )
    )

  protected def astForRegExpLiteral(regExpLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        regExpLiteral.code,
        Some(Defines.STRING.label),
        regExpLiteral.lineNumber,
        regExpLiteral.columnNumber
      )
    )

  protected def astForRegexLiteral(regexLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        regexLiteral.code,
        Some(Defines.STRING.label),
        regexLiteral.lineNumber,
        regexLiteral.columnNumber
      )
    )

  protected def astForNumberLiteral(numberLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        numberLiteral.code,
        Some(Defines.NUMBER.label),
        numberLiteral.lineNumber,
        numberLiteral.columnNumber
      )
    )

  protected def astForNumericLiteral(numericLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        numericLiteral.code,
        Some(Defines.NUMBER.label),
        numericLiteral.lineNumber,
        numericLiteral.columnNumber
      )
    )

  protected def astForDecimalLiteral(decimalLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        decimalLiteral.code,
        Some(Defines.NUMBER.label),
        decimalLiteral.lineNumber,
        decimalLiteral.columnNumber
      )
    )

  protected def astForBigIntLiteral(bigIntLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        bigIntLiteral.code,
        Some(Defines.NUMBER.label),
        bigIntLiteral.lineNumber,
        bigIntLiteral.columnNumber
      )
    )

  protected def astForBooleanLiteral(booleanLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        booleanLiteral.code,
        Some(Defines.BOOLEAN.label),
        booleanLiteral.lineNumber,
        booleanLiteral.columnNumber
      )
    )

  protected def astForTemplateLiteral(templateLiteral: BabelNodeInfo): Ast = {
    val expressions = templateLiteral.json("expressions").arr.toList
    val quasis      = templateLiteral.json("quasis").arr.toList.filterNot(_("tail").bool)
    val quasisTail  = templateLiteral.json("quasis").arr.toList.filter(_("tail").bool).head

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
