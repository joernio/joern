package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast

trait AstForPrimitivesCreator {

  this: AstCreator =>

  protected def astForIdentifier(ident: BabelNodeInfo): Ast = {
    val name      = ident.json("name").str
    val identNode = createIdentifierNode(name, ident)
    scope.addVariableReference(name, identNode)
    Ast(identNode)
  }

  protected def astForStringLiteral(stringLiteral: BabelNodeInfo): Ast =
    Ast(
      createLiteralNode(
        stringLiteral.code,
        Some(Defines.STRING.label),
        stringLiteral.lineNumber,
        stringLiteral.columnNumber
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

}
