package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserAst, ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.Defines as XDefines
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.nodes.{NewModifier, NewNode}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, EvaluationStrategies, ModifierTypes}
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
        node match
          case CallExpr =>
            Try(json(ParserKeys.Fun)(ParserKeys.Obj)(ParserKeys.Decl)) match
              case Success(obj) =>
                // This is being called only to cache this objects
                createParserNodeInfo(obj)
              case _ =>
          case _ =>
        // Do nothing
        pni
      case Success(nodeReferenceId) => parserNodeCache(nodeReferenceId)

  }

  protected def addModifier(node: NewNode, name: String): NewModifier = {
    // NOTE: In golang, the access modifiers are exported and un-exported.
    // If the first letter of the node (function, typeDecl, etc) is uppercase, then it is exported.
    // Else, it is un-exported
    // The scope of the node is the package it is defined in.
    if (name(0).isUpper) {
      newModifierNode(ModifierTypes.PUBLIC)
    } else {
      newModifierNode(ModifierTypes.PRIVATE)
    }
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

  protected def generateTypeFullName(
    typeName: Option[String] = None,
    genericTypeMethodMap: Map[String, List[String]] = Map.empty,
    aliasName: Option[String] = None
  ): String = {
    // NOTE: There is an assumption that the import nodes have been processed before this method is being called
    // and mapping of alias to their respective namespace is already done.
    typeName match
      case None =>
        Defines.anyTypeName
      case Some(typname) =>
        aliasName match
          case None =>
            // NOTE: If the given type is not found in primitiveTypeMap.
            // Then we are assuming the type is custom type defined inside same pacakge as that of current file's package.
            // This assumption will be invalid when another package is imported with alias "."
            if (genericTypeMethodMap.contains(typname)) {
              genericTypeMethodMap(typname).mkString("|")
            } else {
              Defines.primitiveTypeMap.getOrElse(typname, s"$fullyQualifiedPackage.$typname")
            }
          case Some(alias) =>
            s"${aliasToNameSpaceMapping.getOrElse(alias, s"${XDefines.Unknown}.<$alias>")}.$typname"

  }
  private def internalTypeFullName(
    nodeInfo: ParserNodeInfo,
    genericTypeMethodMap: Map[String, List[String]] = Map.empty
  ): (String, String) = {
    nodeInfo.node match {
      case Ident =>
        val typeNameForcode = nodeInfo.json(ParserKeys.Name).str
        val fullName =
          generateTypeFullName(typeName = Some(typeNameForcode), genericTypeMethodMap = genericTypeMethodMap)
        (fullName, typeNameForcode)
      case SelectorExpr =>
        val typeNameForcode =
          s"${nodeInfo.json(ParserKeys.X)(ParserKeys.Name).str}.${nodeInfo.json(ParserKeys.Sel)(ParserKeys.Name).str}"
        val fullName = generateTypeFullName(
          typeName = Some(nodeInfo.json(ParserKeys.Sel)(ParserKeys.Name).str),
          aliasName = Some(nodeInfo.json(ParserKeys.X)(ParserKeys.Name).str)
        )
        (fullName, typeNameForcode)
      case InterfaceType =>
        val typeNameForcode = "interface{}"
        val fullName        = generateTypeFullName(typeName = Some(typeNameForcode))
        (fullName, typeNameForcode)
      case _ =>
        val fullName = generateTypeFullName()
        (fullName, fullName)
    }
  }

  private def internalArrayTypeHandler(
    nodeInfo: ParserNodeInfo,
    genericTypeMethodMap: Map[String, List[String]] = Map.empty
  ): (String, String) = {
    nodeInfo.node match {
      case ArrayType =>
        val (fullName, typeNameForcode) = internalTypeFullName(createParserNodeInfo(nodeInfo.json(ParserKeys.Elt)))
        (s"[]$fullName", s"[]$typeNameForcode")
      case CompositeLit =>
        val (fullName, typeNameForcode) = internalTypeFullName(
          createParserNodeInfo(nodeInfo.json(ParserKeys.Type)(ParserKeys.Elt))
        )
        (s"[]$fullName", s"[]$typeNameForcode")
      case _ =>
        val (fullName, typeNameForcode) = internalTypeFullName(nodeInfo, genericTypeMethodMap)
        (fullName, typeNameForcode)
    }
  }

  private def internalStarExpHandler(
    nodeInfo: ParserNodeInfo,
    genericTypeMethodMap: Map[String, List[String]] = Map.empty
  ): (String, String, String) = {
    nodeInfo.node match {
      case StarExpr =>
        // TODO: Need to handle pointer to pointer use case.
        val (fullName, typeNameForcode) = internalArrayTypeHandler(createParserNodeInfo(nodeInfo.json(ParserKeys.X)))
        (s"*$fullName", s"*$typeNameForcode", EvaluationStrategies.BY_SHARING)
      case _ =>
        val (fullName, typeNameForcode) = internalArrayTypeHandler(nodeInfo, genericTypeMethodMap)
        (fullName, typeNameForcode, EvaluationStrategies.BY_VALUE)
    }
  }

  protected def processTypeInfo(
    nodeInfo: ParserNodeInfo,
    genericTypeMethodMap: Map[String, List[String]] = Map.empty
  ): (String, String, Boolean, String) = {
    nodeInfo.node match {
      case ArrayType =>
        val (fullName, typeNameForcode, evaluationStrategy) = internalStarExpHandler(
          createParserNodeInfo(nodeInfo.json(ParserKeys.Elt))
        )
        (s"[]$fullName", s"[]$typeNameForcode", false, evaluationStrategy)
      case CompositeLit =>
        val (fullName, typeNameForcode, evaluationStrategy) = internalStarExpHandler(
          createParserNodeInfo(nodeInfo.json(ParserKeys.Type)(ParserKeys.Elt))
        )
        (s"[]$fullName", s"[]$typeNameForcode", false, evaluationStrategy)
      case Ellipsis =>
        val (fullName, typeNameForcode, evaluationStrategy) = internalStarExpHandler(
          createParserNodeInfo(nodeInfo.json(ParserKeys.Elt))
        )
        (s"[]$fullName", s"...$typeNameForcode", true, evaluationStrategy)
      case _ =>
        val (fullName, typeNameForcode, evaluationStrategy) = internalStarExpHandler(nodeInfo, genericTypeMethodMap)
        (fullName, typeNameForcode, false, evaluationStrategy)
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
