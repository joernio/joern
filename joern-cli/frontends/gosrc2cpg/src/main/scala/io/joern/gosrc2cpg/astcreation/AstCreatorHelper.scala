package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.parser.ParserAst.{Ident, ParserNode, fromString}
import io.joern.gosrc2cpg.parser.{ParserAst, ParserKeys, ParserNodeInfo}
import org.apache.commons.lang.StringUtils
import ujson.Value

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

trait AstCreatorHelper { this: AstCreator =>

  // maximum length of code fields in number of characters
  private val MaxCodeLength: Int = 1000
  private val MinCodeLength: Int = 50

  private val parserNodeCache = mutable.TreeMap[Long, ParserNodeInfo]()

  protected def createParserNodeInfo(json: Value): ParserNodeInfo = {
    Try(json(ParserKeys.NodeReferenceId).num.toLong) match
      case Failure(_) =>
        val c     = shortenCode(code(json).toOption.getOrElse(""))
        val ln    = line(json)
        val cn    = column(json)
        val lnEnd = lineEndNo(json)
        val cnEnd = columnEndNo(json)
        val node  = nodeType(json)
        val pni   = ParserNodeInfo(node, json, c, ln, cn, lnEnd, cnEnd)
        parserNodeCache.addOne(json(ParserKeys.NodeId).num.toLong, pni)
        pni
      case Success(nodeReferenceId) => parserNodeCache(nodeReferenceId)

  }

  protected def nullSafeCreateParserNodeInfo(json: Option[Value]): ParserNodeInfo = {
    json match
      case Some(value) => createParserNodeInfo(value)
      case None        => ParserNodeInfo(ParserAst.Unknown, ujson.Null, "", None, None, None, None)
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

  protected def registerType(typeName: String): String = {
    val fixedTypeName = fixQualifiedName(StringUtils.normalizeSpace(typeName))
    GoGlobal.usedTypes.putIfAbsent(fixedTypeName, true)
    fixedTypeName
  }

  protected def fixQualifiedName(name: String): String =
    name.stripPrefix(Defines.qualifiedNameSeparator).replace(Defines.qualifiedNameSeparator, ".")
}
