package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.{ArrayType, BasePrimitive, BasicLit, CompositeLit, Ident}
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

  protected def astForCompositeLiteral(primitive: ParserNodeInfo): Seq[Ast] = {
    val elementsAsts = Try(primitive.json(ParserKeys.Elts)) match
      case Success(value) if !value.isNull => value.arr.flatMap(e => astForNode(createParserNodeInfo(e))).toSeq
      case _                               => Seq.empty
    val typeNode = createParserNodeInfo(primitive.json(ParserKeys.Type))
    val typeAst = typeNode.node match
      case ArrayType =>
        Seq(astForArrayInitializer(primitive))
      case _ =>
        Seq.empty
    elementsAsts ++ typeAst
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
    val identifierType = variableOption match {
      case Some((_, variableTypeName)) => variableTypeName
      case _                           => ""
    }
    val node = identifierNode(ident, identifierName, ident.json(ParserKeys.Name).str, identifierType)
    variableOption match {
      case Some((variable, _)) =>
        Ast(node).withRefEdge(node, variable)
      case None => Ast(node)
    }
  }

  protected def getTypeOfToken(basicLit: ParserNodeInfo): String = {
    // TODO Maybe in future need to add __BuiltIn kind of prefix
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
