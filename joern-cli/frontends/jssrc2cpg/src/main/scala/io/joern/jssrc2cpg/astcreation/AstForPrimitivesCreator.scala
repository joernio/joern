package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeSyntax.*
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies

trait AstForPrimitivesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForIdentifier(ident: Identifier, maybePossibleType: Option[String] = None): Ast = {
    astForModuleMemberReference(ident).getOrElse(astForVariableReference(ident, maybePossibleType))
  }

  private def astForVariableReference(ident: Identifier, maybePossibleType: Option[String]): Ast = {
    val name         = ejsOutputCallName(ident).getOrElse(ident.name)
    val identNode    = identifierNode(ident, name)
    val possibleType = maybePossibleType match {
      case None              => typeFor(ident)
      case Some(Defines.Any) => typeFor(ident)
      case Some(otherType)   => otherType
    }
    val typeFullName = if (Defines.isBuiltinType(possibleType)) possibleType else Defines.Any
    identNode.typeFullName(typeFullName)
    identNode.possibleTypes(Seq(possibleType))
    scope.addVariableReference(name, identNode, typeFullName, EvaluationStrategies.BY_REFERENCE)
    Ast(identNode)
  }

  protected def astForSuperKeyword(superKeyword: Super): Ast =
    Ast(identifierNode(superKeyword, "super"))

  protected def astForImportKeyword(importKeyword: Import): Ast =
    Ast(identifierNode(importKeyword, "import"))

  protected def astForNullLiteral(nullLiteral: NullLiteral): Ast =
    Ast(literalNode(nullLiteral, nullLiteral.code, Option(Defines.Null)))

  protected def astForStringLiteral(stringLiteral: StringLiteral): Ast = {
    val code = s"\"${stringLiteral.value}\""
    Ast(literalNode(stringLiteral, code, Option(Defines.String)))
  }

  protected def astForPrivateName(privateName: PrivateName): Ast =
    astForIdentifier(privateName.id)

  protected def astForSpreadOrRestElement(
    spreadElement: SpreadElement | RestElement,
    arg1Ast: Option[Ast] = None
  ): Ast = {
    val argument = spreadElement match {
      case spread: SpreadElement => spread.argument
      case rest: RestElement     => rest.argument
    }
    val ast       = astForNodeWithFunctionReference(argument)
    val callNode_ =
      callNode(spreadElement, spreadElement.code, "<operator>.spread", DispatchTypes.STATIC_DISPATCH)
    callAst(callNode_, arg1Ast.toList :+ ast)
  }

  protected def astForTemplateElement(templateElement: TemplateElement): Ast =
    Ast(literalNode(templateElement, s"\"${templateElement.value("raw").str}\"", Option(Defines.String)))

  protected def astForRegExpLiteral(regExpLiteral: RegExpLiteral): Ast =
    Ast(literalNode(regExpLiteral, regExpLiteral.code, Option(Defines.String)))

  protected def astForNumericLiteral(numericLiteral: NumericLiteral): Ast =
    Ast(literalNode(numericLiteral, numericLiteral.code, Option(Defines.Number)))

  protected def astForBigIntLiteral(bigIntLiteral: BigIntLiteral): Ast =
    Ast(literalNode(bigIntLiteral, bigIntLiteral.code, Option(Defines.Number)))

  protected def astForBooleanLiteral(booleanLiteral: BooleanLiteral): Ast =
    Ast(literalNode(booleanLiteral, booleanLiteral.code, Option(Defines.Boolean)))

  protected def astForTemplateLiteral(templateLiteral: TemplateLiteral): Ast = {
    val expressions           = templateLiteral.expressions
    val (quasisTails, quasis) = templateLiteral.quasis.partition(_.tail)
    val quasisTail            = quasisTails.head

    if (expressions.isEmpty && quasis.isEmpty) {
      astForTemplateElement(quasisTail)
    } else {
      val callName        = Operators.formatString
      val expressionQuasi = expressions.zip(quasis)
      val argsCodes       = expressionQuasi.flatMap { case (expression, quasi) =>
        List(s"\"${quasi.value("raw").str}\"", expression.code)
      }
      val callCode     = s"$callName${(argsCodes :+ s"\"${quasisTail.value("raw").str}\"").mkString("(", ", ", ")")}"
      val templateCall = callNode(templateLiteral, callCode, callName, DispatchTypes.STATIC_DISPATCH)
      val argumentAsts = expressionQuasi.flatMap { case (expression, quasi) =>
        List(astForNodeWithFunctionReference(quasi), astForNodeWithFunctionReference(expression))
      }
      val argAsts = (argumentAsts :+ astForNodeWithFunctionReference(quasisTail)).toSeq
      callAst(templateCall, argAsts)
    }
  }
}
