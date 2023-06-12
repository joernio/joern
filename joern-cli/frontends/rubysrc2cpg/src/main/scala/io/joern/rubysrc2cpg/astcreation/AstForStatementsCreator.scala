package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.RubyParser.{
  AliasStatementContext,
  BeginStatementContext,
  EndStatementContext,
  UndefStatementContext
}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock

trait AstForStatementsCreator { this: AstCreator =>

  protected def astForAliasStatement(ctx: AliasStatementContext): Ast = {
    val aliasName  = ctx.definedMethodNameOrSymbol(0).getText.substring(1)
    val methodName = ctx.definedMethodNameOrSymbol(1).getText.substring(1)
    methodAliases.addOne(aliasName, methodName)
    Ast()
  }

  protected def astForUndefStatement(ctx: UndefStatementContext): Ast = {
    // TODO to be implemented
    Ast()
  }

  protected def astForBeginStatement(ctx: BeginStatementContext): Ast = {
    val stmts     = astForStatementsContext(ctx.statements())
    val blockNode = NewBlock().typeFullName(Defines.Any)
    blockAst(blockNode, stmts.toList)
  }

  protected def astForEndStatement(ctx: EndStatementContext): Ast = {
    val stmts     = astForStatementsContext(ctx.statements())
    val blockNode = NewBlock().typeFullName(Defines.Any)
    blockAst(blockNode, stmts.toList)
  }

}
