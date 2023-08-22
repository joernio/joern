package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserAst, ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.Defines as XDefines
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
    // TODO This can throw error if nodeReferenceId is not present in Cache, but have kept it open for now to know all the different cases
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

  protected def generateTypeFullName(typeName: String = "", aliasName: Option[String] = None): String = {
    // NOTE: There is an assumption that the import nodes have been processed before this method is being called
    // and mapping of alias to their respective namespace is already done.
    typeName match
      case "" =>
        Defines.anyTypeName
      case _ =>
        aliasName match
          case None =>
            // NOTE: If the given type is not found in primitiveTypeMap.
            // Then we are assuming the type is custom type defined inside same pacakge as that of current file's package.
            // This assumption will be invalid when another package is imported with alias "."
            Defines.primitiveTypeMap.getOrElse(typeName, s"${fullyQualifiedPackage}.${typeName}")
          case Some(alias) =>
            s"${aliasToNameSpaceMapping.getOrElse(alias, s"${XDefines.Uknown}.<${alias}>")}.${typeName}"

  }
  private def internalTypeFullName(nodeInfo: ParserNodeInfo): (String, String) = {
    nodeInfo.node match {
      case Ident =>
        val typeNameForcode = nodeInfo.json(ParserKeys.Name).str
        val fullName        = generateTypeFullName(typeNameForcode)
        (fullName, typeNameForcode)
      case SelectorExpr =>
        val typeNameForcode =
          s"${nodeInfo.json(ParserKeys.X)(ParserKeys.Name).str}.${nodeInfo.json(ParserKeys.Sel)(ParserKeys.Name).str}"
        val fullName = generateTypeFullName(
          typeName = nodeInfo.json(ParserKeys.Sel)(ParserKeys.Name).str,
          aliasName = Some(nodeInfo.json(ParserKeys.X)(ParserKeys.Name).str)
        )
        (fullName, typeNameForcode)
      case _ =>
        val fullName = generateTypeFullName()
        (fullName, fullName)
    }
  }

  private def internalArrayTypeHandler(nodeInfo: ParserNodeInfo): (String, String) = {
    nodeInfo.node match {
      case ArrayType =>
        val (fullName, typeNameForcode) = internalTypeFullName(createParserNodeInfo(nodeInfo.json(ParserKeys.Elt)))
        (s"[]${fullName}", s"[]${typeNameForcode}")
      case CompositeLit =>
        val (fullName, typeNameForcode) = internalTypeFullName(
          createParserNodeInfo(nodeInfo.json(ParserKeys.Type)(ParserKeys.Elt))
        )
        (s"[]${fullName}", s"[]${typeNameForcode}")
      case _ =>
        val (fullName, typeNameForcode) = internalTypeFullName(nodeInfo)
        (fullName, typeNameForcode)
    }
  }

  private def internalStarExpHandler(nodeInfo: ParserNodeInfo): (String, String) = {
    nodeInfo.node match {
      case StarExpr =>
        // TODO: Need to handle pointer to pointer use case.
        val (fullName, typeNameForcode) = internalArrayTypeHandler(createParserNodeInfo(nodeInfo.json(ParserKeys.X)))
        (s"*${fullName}", s"*${typeNameForcode}")
      case _ =>
        val (fullName, typeNameForcode) = internalArrayTypeHandler(nodeInfo)
        (fullName, typeNameForcode)
    }
  }

  protected def getTypeFullName(jsonNode: Value): (String, String, Boolean) = {
    val nodeInfo = createParserNodeInfo(jsonNode)
    nodeInfo.node match {
      case ArrayType =>
        val (fullName, typeNameForcode) = internalStarExpHandler(createParserNodeInfo(jsonNode.obj(ParserKeys.Elt)))
        (s"[]${fullName}", s"[]${typeNameForcode}", false)
      case CompositeLit =>
        val (fullName, typeNameForcode) = internalStarExpHandler(
          createParserNodeInfo(jsonNode.obj(ParserKeys.Type)(ParserKeys.Elt))
        )
        (s"[]${fullName}", s"[]${typeNameForcode}", false)
      case Ellipsis =>
        val (fullName, typeNameForcode) = internalStarExpHandler(createParserNodeInfo(jsonNode.obj(ParserKeys.Elt)))
        (s"[]${fullName}", s"...${typeNameForcode}", true)
      case _ =>
        val (fullName, typeNameForcode) = internalStarExpHandler(nodeInfo)
        (fullName, typeNameForcode, false)
    }
  }

  protected def registerType(typeName: String): String = {
    val fixedTypeName = fixQualifiedName(StringUtils.normalizeSpace(typeName))
    GoGlobal.usedTypes.putIfAbsent(fixedTypeName, true)
    fixedTypeName
  }

  protected def fixQualifiedName(name: String): String =
    name.stripPrefix(Defines.qualifiedNameSeparator).replace(Defines.qualifiedNameSeparator, ".")

  override protected def line(node: ParserNodeInfo): Option[Integer] = node.lineNumber

  override protected def column(node: ParserNodeInfo): Option[Integer] = node.columnNumber

  override protected def lineEnd(node: ParserNodeInfo): Option[Integer] = node.lineNumberEnd

  override protected def columnEnd(node: ParserNodeInfo): Option[Integer] = node.columnNumberEnd
}
