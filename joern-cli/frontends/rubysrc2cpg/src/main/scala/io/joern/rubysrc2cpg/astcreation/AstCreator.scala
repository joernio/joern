package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.{RubyLexer, RubyParser, RubyParserVisitor}
import org.antlr.v4.runtime.tree.{ErrorNode, ParseTree, ParseTreeListener, ParseTreeWalker, RuleNode, TerminalNode}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, ParserRuleContext}
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.datastructures.{Global, Scope}
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.{Ast, AstCreatorBase, Defines}
import io.joern.x2cpg.utils.NodeBuilders.{
  fieldIdentifierNode,
  identifierNode,
  methodReturnNode,
  modifierNode,
  operatorCallNode
}
import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyDefaults
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

class AstCreator(filename: String, global: Global) extends AstCreatorBase(filename) {
  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val charStream  = CharStreams.fromFileName(filename)
    val lexer       = new RubyLexer(charStream)
    val tokenStream = new CommonTokenStream(lexer)
    val parser      = new RubyParser(tokenStream)
    val programCtx  = parser.program()

    println(s"Compound statement text: ${programCtx.compoundStatement().getText}")
    programCtx.compoundStatement().statements()
    val statementCtx = programCtx.compoundStatement().statements()

    visitStatementCtx(statementCtx)

    // storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  def visitStatementCtx(ctx: RubyParser.StatementsContext): Unit = {
    ctx
      .statement()
      .forEach(st => {
        println(s"Statement: ${st.getText}. Rule: ${st.getRuleContext().getText}. Alt: ${st.getRuleContext().getAltNumber()}")

        if( st.isInstanceOf[RubyParser.AliasStatementContext] ){
          val ctx = st.asInstanceOf[RubyParser.AliasStatementContext]
          println(s"RubyParser.AliasStatementContext ${ctx.getText}")
        } else if( st.isInstanceOf[RubyParser.UndefStatementContext] ) {
          val ctx = st.asInstanceOf[RubyParser.UndefStatementContext]
          println(s"RubyParser.UndefStatementContext ${ctx.getText}")
        } else if (st.isInstanceOf[RubyParser.BeginStatementContext]) {
          val ctx = st.asInstanceOf[RubyParser.BeginStatementContext]
          println(s"RubyParser.BeginStatementContext ${ctx.getText}")
        } else if (st.isInstanceOf[RubyParser.EndStatementContext]) {
          val ctx = st.asInstanceOf[RubyParser.EndStatementContext]
          println(s"RubyParser.EndStatementContext ${ctx.getText}")
        } else if (st.isInstanceOf[RubyParser.ModifierStatementContext]) {
          val ctx = st.asInstanceOf[RubyParser.ModifierStatementContext]
          println(s"RubyParser.ModifierStatementContext ${ctx.getText}")
        } else if (st.isInstanceOf[RubyParser.ExpressionOrCommandStatementContext]) {
          val ctx = st.asInstanceOf[RubyParser.ExpressionOrCommandStatementContext]
          println(s"RubyParser.ExpressionOrCommandStatementContext ${ctx.getText}")
        }
      })
  }
}
