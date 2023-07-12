package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.{Ident, ParserNode, fromString}
import ujson.Value
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import org.apache.commons.lang.StringUtils

import scala.collection.mutable.ListBuffer
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

    if (lineNumber == lineEndNumber) {
      lineNumberMapping(lineNumber).substring(colNumber, colEndNumber)
    } else {
      val stringList = new ListBuffer[String]()
      stringList.addOne(lineNumberMapping(lineNumber).substring(colNumber))
      Iterator
        .from(lineNumber + 1)
        .takeWhile(currentLineNumber => currentLineNumber < lineEndNumber)
        .foreach(currentLineNumber => stringList.addOne(lineNumberMapping(currentLineNumber)))
      stringList.addOne(lineNumberMapping(lineEndNumber).substring(0, colEndNumber))
      stringList.mkString("\n")
    }
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
  protected def getTypeForJsonNode(jsonNode: Value): String = {
    val nodeInfo = createParserNodeInfo(jsonNode)
    nodeInfo.node match {
      case Ident => jsonNode.obj(ParserKeys.Name).str
      case _     => Defines.anyTypeName
    }
  }
}
