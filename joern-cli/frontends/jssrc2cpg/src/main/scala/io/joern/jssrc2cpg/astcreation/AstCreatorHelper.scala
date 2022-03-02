package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.joern.x2cpg.Ast
import ujson.Value

object AstCreatorHelper {
  implicit class OptionSafeAst(val ast: Ast) extends AnyVal {
    def withArgEdge(src: NewNode, dst: Option[NewNode]): Ast = dst match {
      case Some(value) => ast.withArgEdge(src, value)
      case None        => ast
    }

    def withConditionEdge(src: NewNode, dst: Option[NewNode]): Ast = dst match {
      case Some(value) => ast.withConditionEdge(src, value)
      case None        => ast
    }

    def withArgEdges(src: NewNode, dsts: Seq[Ast]): Ast = {
      val args = dsts.collect { case a if a.root.isDefined => a.root.get }
      ast.withArgEdges(src, args)
    }
  }
}

trait AstCreatorHelper {

  this: AstCreator =>

  protected def registerType(typeName: String): String = {
    global.usedTypes.putIfAbsent(typeName, true)
    typeName
  }

  protected def withOrder[T, X](nodes: Seq[T])(f: (T, Int) => X): Seq[X] =
    nodes.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }

  protected def withOrder[T, X](nodes: Array[T])(f: (T, Int) => X): Seq[X] =
    nodes.toIndexedSeq.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }

  private def notHandledText(node: Value, order: Int): String =
    s"""Node type '${nodeType(node)}' not handled yet!
       |  Code: '${code(node)}'
       |  File: '${parserResult.filename}'
       |  Line: ${line(node).getOrElse(-1)}
       |  Order: $order
       |  """.stripMargin

  protected def notHandledYet(node: Value, order: Int): Ast = {
    val text = notHandledText(node, order)
    logger.info(text)
    Ast(newUnknown(node, order))
  }

  protected def nodeType(node: Value): BabelAst.Node =
    BabelAst.fromString(node("type").str)

  protected def code(node: Value): String = {
    val start = node("start").num.toInt
    val end   = node("end").num.toInt
    parserResult.fileContent.substring(start, end).trim
  }

  protected def columnEnd(node: Value): Option[Integer] =
    node("loc").objOpt.flatMap(loc => loc("end").objOpt.flatMap(start => start("column").numOpt.map(_.toInt)))

  protected def column(node: Value): Option[Integer] =
    node("loc").objOpt.flatMap(loc => loc("start").objOpt.flatMap(start => start("column").numOpt.map(_.toInt)))

  protected def lineEnd(node: Value): Option[Integer] =
    node("loc").objOpt.flatMap(loc => loc("end").objOpt.flatMap(start => start("line").numOpt.map(_.toInt)))

  protected def line(node: Value): Option[Integer] =
    node("loc").objOpt.flatMap(loc => loc("start").objOpt.flatMap(start => start("line").numOpt.map(_.toInt)))
}
