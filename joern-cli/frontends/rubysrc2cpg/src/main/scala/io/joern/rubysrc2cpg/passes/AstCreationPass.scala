package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.rubysrc2cpg.parser.{RubyLexer, RubyParser}
import org.antlr.v4.runtime.tree.{ErrorNode, ParseTreeListener, ParseTreeWalker, TerminalNode}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, ParserRuleContext}
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

class AstCreationPass(inputPath: String, cpg: Cpg) extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)
  val global         = new Global()
  val RubySourceFileExtensions: Set[String] = Set(".rb")

  override def generateParts(): Array[String] = SourceFiles.determine(inputPath, RubySourceFileExtensions).toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, fileName: String): Unit = {
    val charStream = CharStreams.fromFileName(fileName)
    val lexer = new RubyLexer(charStream)
    val tokenStream = new CommonTokenStream(lexer)
    val treeWalker = new ParseTreeWalker()
    val parser = new RubyParser(tokenStream)
    val listener = new RubyTreeListener()
    val tree = parser.methodName()//see other contexts
    treeWalker.walk(listener, tree )
  }

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

    override def exitEveryRule(ctx: ParserRuleContext): Unit ={
      println(s"exitEveryRule(): ${ctx.toString()}")
    }
  }
}
