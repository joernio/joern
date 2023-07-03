package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.Ast

import scala.util.Try

trait AstForPrimitivesCreator { this: AstCreator =>

  protected def astForLiteral(stringLiteral: ParserNodeInfo): Seq[Ast] = {
    val code = stringLiteral.json(ParserKeys.Value).str
    // TODO May need to revisit this
    val typ = getTypeOfToken(stringLiteral)
    Seq(Ast(literalNode(stringLiteral, code, typ)))
  }

  protected def astForIdentifier(ident: ParserNodeInfo): Seq[Ast] = {
    val identifierName = ident.json(ParserKeys.Name).str

    val variableOption = scope.lookupVariable(identifierName)
    val identifierType = variableOption match {
      case Some((_, variableTypeName)) => variableTypeName
      case _                           => ""
    }
    val node = identifierNode(ident, identifierName, ident.json(ParserKeys.Name).str, identifierType)
    variableOption match {
      case Some((variable, _)) =>
        Seq(Ast(node).withRefEdge(node, variable))
      case None => Seq(Ast(node))
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
      case _        => "ANY"
    }).toOption.getOrElse("ANY")
  }

}
