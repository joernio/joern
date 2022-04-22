package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.Ast
import org.apache.commons.lang.StringUtils
import ujson.Value

import scala.collection.mutable
import scala.util.Success
import scala.util.Try

trait AstCreatorHelper {

  this: AstCreator =>

  protected def createBabelNodeInfo(json: Value): BabelNodeInfo = {
    val c     = shortenCode(code(json))
    val ln    = line(json)
    val cn    = column(json)
    val lnEnd = lineEnd(json)
    val cnEnd = columnEnd(json)
    val node  = nodeType(json)
    BabelNodeInfo(node, json, c, ln, cn, lnEnd, cnEnd)
  }

  private def notHandledText(node: BabelNodeInfo): String =
    s"""Node type '${node.node.toString}' not handled yet!
       |  Code: '${node.code}'
       |  File: '${parserResult.fullPath}'
       |  Line: ${node.lineNumber.getOrElse(-1)}
       |  Column: ${node.columnNumber.getOrElse(-1)}
       |  """.stripMargin

  protected def notHandledYet(node: BabelNodeInfo): Ast = {
    val text = notHandledText(node)
    logger.info(text)
    Ast(newUnknown(node))
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
    val start = Try(node("start").num.toInt).getOrElse(0)
    val end   = Try(node("end").num.toInt).getOrElse(0)
    parserResult.fileContent.substring(start, end).trim
  }

  // maximum length of re-mapped code fields after transpilation in number of characters
  private val MAX_CODE_LENGTH: Int = 1000
  private val MIN_CODE_LENGTH: Int = 50

  protected def shortenCode(code: String, length: Int = MAX_CODE_LENGTH): String =
    StringUtils.abbreviate(code, math.max(MIN_CODE_LENGTH, length))

  protected def hasKey(node: Value, key: String): Boolean = Try(node(key)).isSuccess

  protected def safeObj(node: Value, key: String): Option[mutable.LinkedHashMap[String, Value]] = Try(
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
