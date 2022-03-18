package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.joern.x2cpg.Ast
import org.apache.commons.lang.StringUtils
import ujson.Value

import scala.collection.mutable
import scala.util.Success
import scala.util.Try

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

    def withReceiverEdges(src: NewNode, dsts: Seq[Ast]): Ast = {
      val recvs = dsts.collect { case a if a.root.isDefined => a.root.get }
      ast.withReceiverEdges(src, recvs.toList)
    }
  }
}

trait AstCreatorHelper {

  this: AstCreator =>

  protected def withOrder[T, X](nodes: Seq[T])(f: (T, Int) => X): Seq[X] =
    nodes.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }

  protected def withOrder[T, X](nodes: Array[T])(f: (T, Int) => X): Seq[X] =
    nodes.toIndexedSeq.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }

  private def notHandledText(node: BabelNodeInfo, order: Int): String =
    s"""Node type '${node.node.toString}' not handled yet!
       |  Code: '${node.code}'
       |  File: '${parserResult.filename}'
       |  Line: ${node.lineNumber.getOrElse(-1)}
       |  Column: ${node.columnNumber.getOrElse(-1)}
       |  Order: $order
       |  """.stripMargin

  protected def notHandledYet(node: BabelNodeInfo, order: Int): Ast = {
    val text = notHandledText(node, order)
    logger.info(text)
    Ast(newUnknown(node, order))
  }

  protected def registerType(typeName: String): Unit =
    global.usedTypes.putIfAbsent(typeName, true)

  protected def nodeType(node: Value): BabelAst.BabelNode =
    BabelAst.fromString(node("type").str)

  protected def generateUnusedVariableName(
    usedVariableNames: mutable.HashMap[String, Int],
    usedIdentNodes: Set[String],
    variableName: String
  ): String = {
    var counter = usedVariableNames.getOrElse(variableName, 0)

    var currentVariableName = ""
    while ({
      currentVariableName = s"${variableName}_$counter"
      counter += 1
      usedIdentNodes.contains(currentVariableName)
    }) {}

    usedVariableNames.put(variableName, counter)

    currentVariableName
  }

  protected def code(node: Value): String = {
    val start = node("start").num.toInt
    val end   = node("end").num.toInt
    parserResult.fileContent.substring(start, end).trim
  }

  // maximum length of re-mapped code fields after transpilation in number of characters
  private val MAX_CODE_LENGTH: Int = 1000
  private val MIN_CODE_LENGTH: Int = 50

  protected def shortenCode(code: String, length: Int = MAX_CODE_LENGTH): String =
    StringUtils.abbreviate(code, math.max(MIN_CODE_LENGTH, length))

  private def safeObj(node: Value, key: String): Option[mutable.LinkedHashMap[String, Value]] = Try(
    node(key).obj
  ) match {
    case Success(value) if value.nonEmpty => Some(value)
    case _                                => None
  }

  protected def columnEnd(node: Value): Option[Integer] =
    safeObj(node, "loc").flatMap(loc => safeObj(loc, "end").flatMap(start => start("column").numOpt.map(_.toInt)))

  protected def column(node: Value): Option[Integer] =
    safeObj(node, "loc").flatMap(loc => safeObj(loc, "start").flatMap(start => start("column").numOpt.map(_.toInt)))

  protected def lineEnd(node: Value): Option[Integer] =
    safeObj(node, "loc").flatMap(loc => safeObj(loc, "end").flatMap(start => start("line").numOpt.map(_.toInt)))

  protected def line(node: Value): Option[Integer] = {
    safeObj(node, "loc").flatMap(loc => safeObj(loc, "start").flatMap(start => start("line").numOpt.map(_.toInt)))
  }
}
