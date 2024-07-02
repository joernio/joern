package io.joern.dataflowengineoss.semanticsloader

import io.joern.dataflowengineoss.SemanticsParser.MappingContext
import io.joern.dataflowengineoss.{SemanticsBaseListener, SemanticsLexer, SemanticsParser}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object Semantics {

  def fromList(elements: List[FlowSemantic]): Semantics = {
    new Semantics(
      mutable.Map.newBuilder
        .addAll(elements.map { e =>
          e.methodFullName -> e
        })
        .result()
    )
  }

  def empty: Semantics = fromList(List())

}

class Semantics private (methodToSemantic: mutable.Map[String, FlowSemantic]) {

  /** The map below keeps a mapping between results of a regex and the regex string it matches. e.g.
    *
    * `path/to/file.py:<module>.Foo.sink` -> `^path.*Foo\\.sink$`
    */
  private val regexMatchedFullNames = mutable.HashMap.empty[String, String]

  /** Initialize all the method semantics that use regex with all their regex results before query time.
    */
  def loadRegexSemantics(cpg: Cpg): Unit = {
    import io.shiftleft.semanticcpg.language._

    methodToSemantic.filter(_._2.regex).foreach { case (regexString, _) =>
      cpg.method.fullName(regexString).fullName.foreach { methodMatch =>
        regexMatchedFullNames.put(methodMatch, regexString)
      }
    }
  }

  def elements: List[FlowSemantic] = methodToSemantic.values.toList

  def forMethod(fullName: String): Option[FlowSemantic] = regexMatchedFullNames.get(fullName) match {
    case Some(matchedFullName) => methodToSemantic.get(matchedFullName)
    case None                  => methodToSemantic.get(fullName)
  }

  def serialize: String = {
    elements
      .sortBy(_.methodFullName)
      .map { elem =>
        s"\"${elem.methodFullName}\" " + elem.mappings
          .collect { case FlowMapping(x, y) => s"$x -> $y" }
          .mkString(" ")
      }
      .mkString("\n")
  }

}
case class FlowSemantic(methodFullName: String, mappings: List[FlowPath] = List.empty, regex: Boolean = false)

object FlowSemantic {

  def from(methodFullName: String, mappings: List[?], regex: Boolean = false): FlowSemantic = {
    FlowSemantic(
      methodFullName,
      mappings.map {
        case (src: Int, dst: Int)                                 => FlowMapping(src, dst)
        case (srcIdx: Int, src: String, dst: Int)                 => FlowMapping(srcIdx, src, dst)
        case (src: Int, dstIdx: Int, dst: String)                 => FlowMapping(src, dstIdx, dst)
        case (srcIdx: Int, src: String, dstIdx: Int, dst: String) => FlowMapping(srcIdx, src, dstIdx, dst)
        case x: FlowMapping                                       => x
      },
      regex
    )
  }

}

abstract class FlowNode

/** Collects parameters and return nodes under a common trait. This trait acknowledges their argument index which is
  * relevant when a caller wants to coordinate relevant tainted flows through specific arguments and the return flow.
  */
trait ParamOrRetNode extends FlowNode {

  /** Temporary backward compatible idx field.
    *
    * @return
    *   the argument index.
    */
  def index: Int
}

/** A parameter where the index of the argument matches the position of the parameter at the callee. The name is used to
  * match named arguments if used instead of positional arguments.
  *
  * @param index
  *   the position or argument index.
  * @param name
  *   the name of the parameter.
  */
case class ParameterNode(index: Int, name: Option[String] = None) extends ParamOrRetNode

object ParameterNode {
  def apply(index: Int, name: String): ParameterNode = ParameterNode(index, Option(name))
}

/** Represents explicit mappings or special cases.
  */
sealed trait FlowPath

/** Maps flow between arguments based on how they interact as parameters at the callee.
  *
  * @param src
  *   source of the flow.
  * @param dst
  *   destination of the flow.
  */
case class FlowMapping(src: FlowNode, dst: FlowNode) extends FlowPath

object FlowMapping {
  def apply(from: Int, to: Int): FlowMapping = FlowMapping(ParameterNode(from), ParameterNode(to))

  def apply(fromIdx: Int, from: String, toIdx: Int, to: String): FlowMapping =
    FlowMapping(ParameterNode(fromIdx, from), ParameterNode(toIdx, to))

  def apply(fromIdx: Int, from: String, toIdx: Int): FlowMapping =
    FlowMapping(ParameterNode(fromIdx, from), ParameterNode(toIdx))

  def apply(from: Int, toIdx: Int, to: String): FlowMapping = FlowMapping(ParameterNode(from), ParameterNode(toIdx, to))

}

/** Represents an instance where parameters are not sanitized, may affect the return value, and do not cross-taint. e.g.
  * foo(1, 2) = 1 -> 1, 2 -> 2, 1 -> -1, 2 -> -1
  *
  * The main benefit is that this works for unbounded parameters e.g. VARARGS. Note this does not taint 0 -> 0.
  */
object PassThroughMapping extends FlowPath

class Parser() {

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
