package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.{ExpressionStatement, GlobalStatement, ThrowStatement, TryStatement}
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, ValidationMode}

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForStatement(nodeInfo: DotNetNodeInfo): Seq[Ast] = {
    nodeInfo.node match
      case ExpressionStatement => astForExpression(nodeInfo)
      case GlobalStatement     => astForGlobalStatement(nodeInfo)
      case ThrowStatement      => astForThrowStatement(nodeInfo)
      case TryStatement        => astForTryStatement(nodeInfo)
      case _                   => notHandledYet(nodeInfo)
  }

  protected def astForGlobalStatement(globalStatement: DotNetNodeInfo): Seq[Ast] = {
    astForNode(globalStatement.json(ParserKeys.Statement))
  }

}
