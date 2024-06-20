package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

trait AstForPrimitivesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForIdentifier(ident: BabelNodeInfo, maybePossibleType: Option[String] = None): Ast = {
    val name      = ident.json("name").str
    val identNode = identifierNode(ident, name)
    val possibleType = maybePossibleType match {
      case None              => typeFor(ident)
      case Some(Defines.Any) => typeFor(ident)
      case Some(otherType)   => otherType
    }
    val typeFullName = if (Defines.isBuiltinType(possibleType)) possibleType else Defines.Any
    identNode.typeFullName(typeFullName)
    identNode.possibleTypes(Seq(possibleType))
    scope.addVariableReference(name, identNode)
    Ast(identNode)
  }

  protected def astForSuperKeyword(superKeyword: BabelNodeInfo): Ast =
    Ast(identifierNode(superKeyword, "super"))

  protected def astForImportKeyword(importKeyword: BabelNodeInfo): Ast =
    Ast(identifierNode(importKeyword, "import"))

  protected def astForNullLiteral(nullLiteral: BabelNodeInfo): Ast =
    Ast(literalNode(nullLiteral, nullLiteral.code, Option(Defines.Null)))

  protected def astForStringLiteral(stringLiteral: BabelNodeInfo): Ast = {
    val code = s"\"${stringLiteral.json("value").str}\""
    Ast(literalNode(stringLiteral, code, Option(Defines.String)))
  }

  protected def astForPrivateName(privateName: BabelNodeInfo): Ast =
    astForIdentifier(createBabelNodeInfo(privateName.json("id")))

  protected def astForSpreadOrRestElement(spreadElement: BabelNodeInfo, arg1Ast: Option[Ast] = None): Ast = {
    val ast = astForNodeWithFunctionReference(spreadElement.json("argument"))
    val callNode_ =
      callNode(spreadElement, spreadElement.code, "<operator>.spread", DispatchTypes.STATIC_DISPATCH)
    callAst(callNode_, arg1Ast.toList :+ ast)
  }

  protected def astForTemplateElement(templateElement: BabelNodeInfo): Ast =
    Ast(literalNode(templateElement, s"\"${templateElement.json("value")("raw").str}\"", Option(Defines.String)))

  protected def astForRegExpLiteral(regExpLiteral: BabelNodeInfo): Ast =
    Ast(literalNode(regExpLiteral, regExpLiteral.code, Option(Defines.String)))

  protected def astForRegexLiteral(regexLiteral: BabelNodeInfo): Ast =
    Ast(literalNode(regexLiteral, regexLiteral.code, Option(Defines.String)))

  protected def astForNumberLiteral(numberLiteral: BabelNodeInfo): Ast =
    Ast(literalNode(numberLiteral, numberLiteral.code, Option(Defines.Number)))

  protected def astForNumericLiteral(numericLiteral: BabelNodeInfo): Ast =
    Ast(literalNode(numericLiteral, numericLiteral.code, Option(Defines.Number)))

  protected def astForDecimalLiteral(decimalLiteral: BabelNodeInfo): Ast =
    Ast(literalNode(decimalLiteral, decimalLiteral.code, Option(Defines.Number)))

  protected def astForBigIntLiteral(bigIntLiteral: BabelNodeInfo): Ast =
    Ast(literalNode(bigIntLiteral, bigIntLiteral.code, Option(Defines.Number)))

  protected def astForBooleanLiteral(booleanLiteral: BabelNodeInfo): Ast =
    Ast(literalNode(booleanLiteral, booleanLiteral.code, Option(Defines.Boolean)))

  protected def astForTemplateLiteral(templateLiteral: BabelNodeInfo): Ast = {
    val expressions = templateLiteral.json("expressions").arr.toList
    val quasis      = templateLiteral.json("quasis").arr.toList.filterNot(_("tail").bool)
    val quasisTail  = templateLiteral.json("quasis").arr.toList.filter(_("tail").bool).head

    if (expressions.isEmpty && quasis.isEmpty) {
      astForTemplateElement(createBabelNodeInfo(quasisTail))
    } else {
      val callName = Operators.formatString
      val argsCodes = expressions.zip(quasis).flatMap { case (expression, quasi) =>
        List(s"\"${quasi("value")("raw").str}\"", code(expression))
      }
      val callCode = s"$callName${(argsCodes :+ s"\"${quasisTail("value")("raw").str}\"").mkString("(", ", ", ")")}"
      val templateCall =
        callNode(templateLiteral, callCode, callName, DispatchTypes.STATIC_DISPATCH)
      val argumentAsts = expressions.zip(quasis).flatMap { case (expression, quasi) =>
        List(astForNodeWithFunctionReference(quasi), astForNodeWithFunctionReference(expression))
      }
      val argAsts = argumentAsts :+ astForNodeWithFunctionReference(quasisTail)
      callAst(templateCall, argAsts)
    }
  }
}
