package io.joern.dataflowengineoss.semanticsloader

import io.joern.dataflowengineoss.{SemanticsBaseListener, SemanticsLexer, SemanticsParser}
import io.shiftleft.codepropertygraph.Cpg
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

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
        s"\"${elem.methodFullName}\" " + elem.mappings.map { case (x, y) => s"$x -> $y" }.mkString(" ")
      }
      .mkString("\n")
  }

}
case class FlowSemantic(methodFullName: String, mappings: List[(Int, Int)], regex: Boolean = false)

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
        val mappings = semantic.mapping().asScala.toList.map { mapping =>
          val src = mapping.src().NUMBER().getText.toInt
          val dst = mapping.dst().NUMBER().getText.toInt
          (src, dst)
        }
        result.addOne(FlowSemantic(methodName, mappings))
      }
    }

  }

}
