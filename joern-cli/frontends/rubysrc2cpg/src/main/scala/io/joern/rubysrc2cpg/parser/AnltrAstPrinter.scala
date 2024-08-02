package io.joern.rubysrc2cpg.parser

import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.TerminalNode

/** General purpose ANTLR parse tree printer.
  */
object AnltrAstPrinter {
  private val indentationIncrement = 1

  private def print(level: Int, sb: StringBuilder, context: ParserRuleContext): StringBuilder = {
    val indentation = " ".repeat(level)
    val contextName = context.getClass.getSimpleName.stripSuffix("Context")
    val nextLevel   = level + indentationIncrement
    sb.append(s"$indentation$contextName\n")
    Option(context.children).foreach(_.forEach {
      case c: ParserRuleContext => print(nextLevel, sb, c)
      case t: TerminalNode      => print(nextLevel, sb, t)
    })
    sb
  }

  private def print(level: Int, sb: StringBuilder, terminal: TerminalNode): StringBuilder = {
    val indentation = " ".repeat(level)
    sb.append(s"$indentation${terminal.getText}\n")
    sb
  }

  /** Pretty-prints an entire `ParserRuleContext` together with its descendants.
    * @param context
    *   the context to pretty-print
    * @return
    *   an indented, multiline string representation
    */
  def print(context: ParserRuleContext): String = print(0, new StringBuilder, context).toString()
}
