package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.Ast

trait AstForPrimitivesCreator { this: AstCreator =>

  protected def astForLiteral(stringLiteral: ParserNodeInfo): Ast = {
    val code = stringLiteral.json(ParserKeys.Value).str
    // TODO May need to revisit this
    Ast(literalNode(stringLiteral, code, code.trim))
  }

}
