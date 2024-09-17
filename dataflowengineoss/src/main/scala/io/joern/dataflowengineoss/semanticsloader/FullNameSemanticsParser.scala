package io.joern.dataflowengineoss.semanticsloader

import io.joern.dataflowengineoss.SemanticsParser.MappingContext
import io.joern.dataflowengineoss.{SemanticsBaseListener, SemanticsLexer, SemanticsParser}
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class FullNameSemanticsParser {

  def parse(input: String): List[FlowSemantic] = {
    val charStream = CharStreams.fromString(input)
    parseCharStream(charStream)
  }

  def parseFile(fileName: String): List[FlowSemantic] = {
    val charStream = CharStreams.fromFileName(fileName)
    parseCharStream(charStream)
  }

  private def parseCharStream(charStream: CharStream): List[FlowSemantic] = {
    val lexer       = new SemanticsLexer(charStream)
    val tokenStream = new CommonTokenStream(lexer)
    val parser      = new SemanticsParser(tokenStream)
    val treeWalker  = new ParseTreeWalker()

    val tree     = parser.taintSemantics()
    val listener = new Listener()
    treeWalker.walk(listener, tree)
    listener.result.toList
  }

  implicit class AntlrFlowExtensions(val ctx: MappingContext) {

    def isPassThrough: Boolean = Option(ctx.PASSTHROUGH()).isDefined

    def srcIdx: Int = ctx.src().argIdx().NUMBER().getText.toInt

    def srcArgName: Option[String] = Option(ctx.src().argName()).map(_.name().getText)

    def dstIdx: Int = ctx.dst().argIdx().NUMBER().getText.toInt

    def dstArgName: Option[String] = Option(ctx.dst().argName()).map(_.name().getText)

  }

  private class Listener extends SemanticsBaseListener {

    val result: mutable.ListBuffer[FlowSemantic] = mutable.ListBuffer[FlowSemantic]()

    override def enterTaintSemantics(ctx: SemanticsParser.TaintSemanticsContext): Unit = {
      ctx.singleSemantic().asScala.foreach { semantic =>
        val methodName = semantic.methodName().name().getText
        val mappings   = semantic.mapping().asScala.toList.map(ctxToParamMapping)
        result.addOne(FlowSemantic(methodName, mappings))
      }
    }

    private def ctxToParamMapping(ctx: MappingContext): FlowPath =
      if (ctx.isPassThrough) {
        PassThroughMapping
      } else {
        val src = ParameterNode(ctx.srcIdx, ctx.srcArgName)
        val dst = ParameterNode(ctx.dstIdx, ctx.dstArgName)

        FlowMapping(src, dst)
      }

  }

}
