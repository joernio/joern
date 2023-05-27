package io.joern.dataflowengineoss.semanticsloader

import io.joern.dataflowengineoss.SemanticsParser.{MappingContext, SrcContext}
import io.joern.dataflowengineoss.{SemanticsBaseListener, SemanticsLexer, SemanticsParser}
import io.shiftleft.codepropertygraph.Cpg
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream}

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try

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
          .collect { case ParamMapping(x, y) => s"$x -> $y" }
          .mkString(" ")
      }
      .mkString("\n")
  }

}
case class FlowSemantic(methodFullName: String, mappings: List[FlowMapping], regex: Boolean = false)

object FlowSemantic {

  def from(methodFullName: String, mappings: List[_], regex: Boolean = false): FlowSemantic = {
    FlowSemantic(
      methodFullName,
      mappings.map {
        case (src: Int, dst: Int)       => ParamMapping(src, dst)
        case (src: String, dst: Int)    => ParamMapping(src, dst)
        case (src: Int, dst: String)    => ParamMapping(src, dst)
        case (src: String, dst: String) => ParamMapping(src, dst)
        case x: ParamMapping            => x
      },
      regex
    )
  }

}

/** Represents how arguments are identified to their respective parameters at a call site.
  * @tparam P
  *   a positional argument.
  * @tparam N
  *   a named argument.
  */
sealed trait FlowArgument[P, N]

/** A positional argument where the index of the argument matches the position of the parameter at the callee.
  * @param idx
  *   the position or argument index.
  */
case class PosArg(idx: Int) extends FlowArgument[PosArg, NamedArg]

/** A named argument where the name corresponds to the name of the parameter at the callee.
  * @param name
  *   the name of the parameter.
  */
case class NamedArg(name: String) extends FlowArgument[PosArg, NamedArg]

/** Represents explicit mappings or special cases.
  */
sealed trait FlowMapping

/** Maps flow between arguments based on how they interact as parameters at the callee.
  *
  * @param src
  *   source of the flow.
  * @param dst
  *   destination of the flow.
  */
case class ParamMapping(src: FlowArgument[PosArg, NamedArg], dst: FlowArgument[PosArg, NamedArg]) extends FlowMapping
object ParamMapping {
  def apply(from: Int, to: Int): ParamMapping = ParamMapping(PosArg(from), PosArg(to))

  def apply(from: String, to: String): ParamMapping = ParamMapping(NamedArg(from), NamedArg(to))

  def apply(from: String, to: Int): ParamMapping = ParamMapping(NamedArg(from), PosArg(to))

  def apply(from: Int, to: String): ParamMapping = ParamMapping(PosArg(from), NamedArg(to))

}

/** Represents an instance where parameters are not sanitized, may affect the return value, and do not cross-taint.
  *
  * TODO: Implement this for varargs operations such as <operators>.tupleLiteral
  */
object PassThroughMapping extends FlowMapping

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

  private class Listener extends SemanticsBaseListener {

    val result: mutable.ListBuffer[FlowSemantic] = mutable.ListBuffer[FlowSemantic]()

    override def enterTaintSemantics(ctx: SemanticsParser.TaintSemanticsContext): Unit = {
      ctx.singleSemantic().asScala.foreach { semantic =>
        val methodName = semantic.methodName().name().getText
        val mappings   = semantic.mapping().asScala.toList.map(ctxToParamMapping)
        result.addOne(FlowSemantic(methodName, mappings))
      }
    }

    private def ctxToParamMapping(ctx: MappingContext): FlowMapping = {
      val src =
        if (ctx.src().argName() != null) NamedArg(ctx.src().argName().name().getText)
        else PosArg(ctx.src().argIdx().NUMBER().getText.toInt)
      val dst =
        if (ctx.dst().argName() != null) NamedArg(ctx.dst().argName().name().getText)
        else PosArg(ctx.dst().argIdx().NUMBER().getText.toInt)

      ParamMapping(src, dst)
    }

  }

}
