package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.LiteralExpr
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, ValidationMode}

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForEqualsValueClause(clause: DotNetNodeInfo): Seq[Ast] = {
    val rhsNode = createDotNetNodeInfo(clause.json(ParserKeys.Value))
    rhsNode.node match
      case _: LiteralExpr => Seq(Ast(literalNode(rhsNode, code(rhsNode), nodeTypeFullName(rhsNode))))
      case _              => notHandledYet(rhsNode)
  }

}
