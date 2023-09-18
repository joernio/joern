package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.{ArrayType, BasePrimitive, BasicLit, CompositeLit, Ident, SelectorExpr}
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

import scala.util.{Failure, Success, Try}

trait AstForPrimitivesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForPrimitive(primitive: ParserNodeInfo): Seq[Ast] = {
    primitive.node match {
      case BasicLit     => Seq(astForLiteral(primitive))
      case CompositeLit => astForCompositeLiteral(primitive)
      case Ident        => Seq(astForIdentifier(primitive))
      case _            => Seq(Ast())
    }
  }

  def astForCompositeLiteralHavingTypeKey(typeNode: ParserNodeInfo, compositeLiteralNode: ParserNodeInfo): Seq[Ast] = {
    typeNode.node match
      case ArrayType =>
        val elementsAsts = Try(compositeLiteralNode.json(ParserKeys.Elts)) match
          case Success(value) if !value.isNull => value.arr.flatMap(e => astForNode(createParserNodeInfo(e))).toSeq
          case _                               => Seq.empty
        elementsAsts ++ Seq(astForArrayInitializer(compositeLiteralNode))
      // Handling structure initialisation by creating a call node and arguments
      case Ident =>
        astForConstructorCall(compositeLiteralNode)
      // Handling structure initialisation(alias present) by creating a call node and arguments
      case SelectorExpr =>
        astForConstructorCall(compositeLiteralNode)
      case _ =>
        Seq.empty
  }

  protected def astForCompositeLiteral(compositeLiteralNodeInfo: ParserNodeInfo): Seq[Ast] = {
    Try(createParserNodeInfo(compositeLiteralNodeInfo.json(ParserKeys.Type))) match
      case Success(typeNode) =>
        astForCompositeLiteralHavingTypeKey(typeNode, compositeLiteralNodeInfo)
      case _ =>
        val elementsAsts = Try(compositeLiteralNodeInfo.json(ParserKeys.Elts)) match
          case Success(compositeElements) if !compositeElements.isNull =>
            compositeElements.arr.flatMap(e => astForNode(createParserNodeInfo(e))).toSeq
          case _ => Seq.empty
          // TODO: merge array initializer node Seq(astForArrayInitializer(compositeLiteralNodeInfo))
        elementsAsts
  }

  private def astForLiteral(stringLiteral: ParserNodeInfo): Ast = {
    val code = stringLiteral.json(ParserKeys.Value).str
    // TODO May need to revisit this
    val typ = getTypeOfToken(stringLiteral)
    Ast(literalNode(stringLiteral, code, typ))
  }

  private def astForIdentifier(ident: ParserNodeInfo): Ast = {
    val identifierName = ident.json(ParserKeys.Name).str
    val variableOption = scope.lookupVariable(identifierName)
    variableOption match {
      case Some((variable, variableTypeName)) =>
        val node = identifierNode(ident, identifierName, ident.code, variableTypeName)
        Ast(node).withRefEdge(node, variable)
      case _ =>
        // TODO: something is wrong here. Refer to SwitchTests -> "be correct for switch case 4"
        Ast(identifierNode(ident, identifierName, ident.json(ParserKeys.Name).str, Defines.anyTypeName))
    }
  }

  protected def getTypeOfToken(basicLit: ParserNodeInfo): String = {
    // TODO need to add more primitive types
    Try(basicLit.json(ParserKeys.Kind).str match {
      case "INT"    => "int"
      case "FLOAT"  => "float32"
      case "IMAG"   => "imag"
      case "CHAR"   => "char"
      case "STRING" => "string"
      case _        => Defines.anyTypeName
    }).toOption.getOrElse(Defines.anyTypeName)
  }

  protected def astForArrayInitializer(primitive: ParserNodeInfo): Ast = {
    val (typeFullName, typeFullNameForcode, isVariadic, _) = processTypeInfo(primitive)
    Ast(
      callNode(
        primitive,
        primitive.code,
        Operators.arrayInitializer,
        Operators.arrayInitializer,
        DispatchTypes.STATIC_DISPATCH,
        Option(Defines.empty),
        Option(typeFullName) // The "" around the typename is eliminated
      )
    )
  }
}
