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
    val c     = shortenCode(code(json).toOption.getOrElse(""))
    val ln    = line(json)
    val cn    = column(json)
    val lnEnd = lineEndNo(json)
    val cnEnd = columnEndNo(json)
    val node  = nodeType(json)
    ParserNodeInfo(node, json, c, ln, cn, lnEnd, cnEnd)
  }

  private def nodeType(node: Value): ParserNode = fromString(node(ParserKeys.NodeType).str)
  protected def code(node: Value): Try[String] = Try {

    val lineNumber    = line(node).get
    val colNumber     = column(node).get - 1
    val lineEndNumber = lineEndNo(node).get
    val colEndNumber  = columnEndNo(node).get - 1

    var nodeCode = ""
    if (lineNumber == lineEndNumber) {
      nodeCode = lineNumberMapping(lineNumber).substring(colNumber, colEndNumber)
    } else {
      nodeCode += lineNumberMapping(lineNumber).substring(colNumber)
      var currentLineNumber = lineNumber + 1
      while (currentLineNumber < lineEndNumber) {
        nodeCode += "\n" + lineNumberMapping(currentLineNumber)
        currentLineNumber += 1
      }
      nodeCode += "\n" + lineNumberMapping(lineEndNumber).substring(0, colEndNumber)
    }
    nodeCode
  }

  private def shortenCode(code: String, length: Int = MaxCodeLength): String =
    StringUtils.abbreviate(code, math.max(MinCodeLength, length))

  protected def line(node: Value): Option[Integer] = Try(node(ParserKeys.NodeLineNo).num).toOption.map(_.toInt)

  protected def column(node: Value): Option[Integer] = Try(node(ParserKeys.NodeColNo).num).toOption.map(_.toInt)

  protected def lineEndNo(node: Value): Option[Integer] = Try(node(ParserKeys.NodeLineEndNo).num).toOption.map(_.toInt)

  protected def columnEndNo(node: Value): Option[Integer] = Try(node(ParserKeys.NodeColEndNo).num).toOption.map(_.toInt)

  protected def positionLookupTables(source: String): Map[Int, String] = {
    source
      .split("\n")
      .zipWithIndex
      .map { case (sourceLine, lineNumber) =>
        (lineNumber + 1, sourceLine)
      }
      .toMap
  }

}
