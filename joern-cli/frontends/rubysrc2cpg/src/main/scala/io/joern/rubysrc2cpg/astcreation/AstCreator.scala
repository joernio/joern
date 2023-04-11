package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.{RubyLexer, RubyParser}
import org.antlr.v4.runtime.tree.{ErrorNode, ParseTreeListener, ParseTreeWalker, TerminalNode}
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
  class RubyTreeListener extends ParseTreeListener {
    override def visitTerminal(node: TerminalNode): Unit = {
      println(s"visitTerminal(): ${node.getSymbol}")
    }

    override def visitErrorNode(node: ErrorNode): Unit = {
      println(s"visitErrorNode(): ${node.getSymbol}")
    }

    override def enterEveryRule(ctx: ParserRuleContext): Unit = {
      println(s"enterEveryRule(): ${ctx.toString()}")
    }

    override def exitEveryRule(ctx: ParserRuleContext): Unit = {
      println(s"exitEveryRule(): ${ctx.toString()}")
    }
  }
  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val charStream  = CharStreams.fromFileName(filename)
    val lexer       = new RubyLexer(charStream)
    val tokenStream = new CommonTokenStream(lexer)
    val treeWalker  = new ParseTreeWalker()
    val parser      = new RubyParser(tokenStream)
    val listener    = new RubyTreeListener()
    val tree        = parser.methodName() // see other contexts
    treeWalker.walk(listener, tree)
    // storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def registerType(typ: String): String = {
    global.usedTypes.putIfAbsent(typ, true)
    typ
  }
}
