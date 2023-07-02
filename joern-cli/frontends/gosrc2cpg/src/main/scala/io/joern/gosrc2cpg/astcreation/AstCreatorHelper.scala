package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.{ImportSpec, ParserNode, fromString}
import ujson.Value
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.Ast
import org.apache.commons.lang.StringUtils

import scala.util.Try

trait AstCreatorHelper { this: AstCreator =>

  // maximum length of code fields in number of characters
  private val MaxCodeLength: Int = 1000
  private val MinCodeLength: Int = 50

  protected def createParserNodeInfo(json: Value): ParserNodeInfo = {
    val c     = shortenCode(code(json))
    val ln    = line(json)
    val cn    = column(json)
    val lnEnd = lineEndNo(json)
    val cnEnd = columnEndNo(json)
    val node  = nodeType(json)
    ParserNodeInfo(node, json, c, ln, cn, lnEnd, cnEnd)
  }

  private def nodeType(node: Value): ParserNode = fromString(node(ParserKeys.NodeType).str)
  protected def code(node: Value): String = {
    // TODO Need to build a utlity which returns code for a node
    ""
  }

  private def shortenCode(code: String, length: Int = MaxCodeLength): String =
    StringUtils.abbreviate(code, math.max(MinCodeLength, length))

  protected def line(node: Value): Option[Integer] = Try(node(ParserKeys.NodeLineNo).num).toOption.map(_.toInt)

  protected def column(node: Value): Option[Integer] = Try(node(ParserKeys.NodeColNo).num).toOption.map(_.toInt)

  protected def lineEndNo(node: Value): Option[Integer] = Try(node(ParserKeys.NodeLineEndNo).num).toOption.map(_.toInt)

  protected def columnEndNo(node: Value): Option[Integer] = Try(node(ParserKeys.NodeColEndNo).num).toOption.map(_.toInt)

}
