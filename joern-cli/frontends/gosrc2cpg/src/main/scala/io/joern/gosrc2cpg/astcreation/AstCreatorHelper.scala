package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserAst, ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.{Ast, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.nodes.{NewModifier, NewNode}
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, ModifierTypes, PropertyNames}
import ujson.{Arr, Obj, Value}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

trait AstCreatorHelper { this: AstCreator =>

  private val parserNodeCache = mutable.TreeMap[Long, ParserNodeInfo]()

  protected def preProcessParserNodeCache(json: Value): Unit = {
    json match {
      case obj: Obj =>
        // TODO: Add unit tests for "ast.ValueSpec" and "ast.FuncLit"
        if (
          json.obj
            .contains(ParserKeys.NodeType) && (obj(ParserKeys.NodeType).str == "ast.FuncDecl" || obj(
            ParserKeys.NodeType
          ).str == "ast.ValueSpec" || obj(ParserKeys.NodeType).str == "ast.FuncLit" || obj(
            ParserKeys.NodeType
          ).str == "ast.TypeSpec") && !json.obj.contains(ParserKeys.NodeReferenceId)
        ) {
          createParserNodeInfo(obj)
        }
        obj.value.values.foreach(subJson => preProcessParserNodeCache(subJson))
      case arr: Arr =>
        arr.value.foreach(subJson => preProcessParserNodeCache(subJson))
      case _ =>
    }
  }
  protected def createParserNodeInfo(json: Value): ParserNodeInfo = {
    Try(json(ParserKeys.NodeReferenceId).num.toLong) match
      case Failure(_) =>
        val c     = code(json)
        val ln    = line(json)
        val cn    = column(json)
        val lnEnd = lineEndNo(json)
        val cnEnd = columnEndNo(json)
        val node  = nodeType(json)
        val pni   = ParserNodeInfo(node, json, c, ln, cn, lnEnd, cnEnd)
        parserNodeCache.addOne(json(ParserKeys.NodeId).num.toLong, pni)
        cacheReferenceNode(json, node)
        pni
      case Success(nodeReferenceId) =>
        // Get the parser node info from the cache using the node reference ID
        val parserNodeInfo = parserNodeCache.get(nodeReferenceId)

        // If the parser node info exists in the cache, return it
        parserNodeInfo match
          case Some(value) => value
          case None        =>
            // If the parser node info does not exist in the cache, log a warning message and create a null-safe parser node info
            val nodeType = json(ParserKeys.NodeType).str
            logger.warn(s"Unhandled node_type $nodeType filename: $jsonAstFilePath")
            nullSafeCreateParserNodeInfo(None)
  }

  private def cacheReferenceNode(json: Value, node: ParserNode) = {
    // This is being called only to cache this objects
    node match
      case CallExpr =>
        Try(json(ParserKeys.Fun)(ParserKeys.Obj)(ParserKeys.Decl)) match
          case Success(obj) =>
            createParserNodeInfo(obj)
          case _ =>
      case Ident =>
        Try(json(ParserKeys.Obj)(ParserKeys.Decl)) match
          // NOTE: For now only handling caching for node reference id of struct type
          case Success(obj) if obj(ParserKeys.NodeType).str == "ast.TypeSpec" =>
            createParserNodeInfo(obj)
          case _ =>
      case _ =>
  }

  protected def getTypeFullNameFromAstNode(ast: Seq[Ast]): String = {
    ast.headOption
      .flatMap(_.root)
      .map(_.properties.get(PropertyNames.TYPE_FULL_NAME).get.toString)
      .getOrElse(Defines.anyTypeName)
  }

  protected def addModifier(node: NewNode, name: String): NewModifier = {
    // NOTE: In golang, the access modifiers are exported and un-exported.
    // If the first letter of the node (function, typeDecl, etc) is uppercase, then it is exported.
    // Else, it is un-exported
    // The scope of the node is the package it is defined in.
    if (name.headOption.exists(_.isUpper)) {
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

  private def nodeType(node: Value): ParserNode = fromString(node(ParserKeys.NodeType).str, relPathFileName)

  protected def code(node: Value): String = {
    codeForValue(node).toOption.fold("")(shortenCode)
  }

  private def codeForValue(node: Value): Try[String] = Try {
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

  protected def line(node: Value): Option[Integer] = Try(node(ParserKeys.NodeLineNo).num).toOption.map(_.toInt)

  protected def column(node: Value): Option[Integer] = Try(node(ParserKeys.NodeColNo).num).toOption.map(_.toInt)

  protected def lineEndNo(node: Value): Option[Integer] = Try(node(ParserKeys.NodeLineEndNo).num).toOption.map(_.toInt)

  protected def columnEndNo(node: Value): Option[Integer] = Try(node(ParserKeys.NodeColEndNo).num).toOption.map(_.toInt)

  protected def positionLookupTables: Map[Int, String] = {
    val result = if (!goGlobal.processingDependencies) {
      val map = parserResult.fileContent
        .split("\n")
        .zipWithIndex
        .map { case (sourceLine, lineNumber) =>
          (lineNumber + 1, sourceLine)
        }
        .toMap
      map
    } else {
      Map[Int, String]()
    }
    parserResult.fileContent = ""
    result
  }

  protected def resolveAliasToFullName(alias: String): String = {
    s"${aliasToNameSpaceMapping.getOrElse(alias, goGlobal.aliasToNameSpaceMapping.getOrDefault(alias, s"${XDefines.Unknown}.<$alias>"))}"
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
            s"${resolveAliasToFullName(alias)}.$typname"

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
      case CompositeLit if createParserNodeInfo(nodeInfo.json(ParserKeys.Type)).json.obj.contains(ParserKeys.Elt) =>
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
      case CompositeLit if createParserNodeInfo(nodeInfo.json(ParserKeys.Type)).json.obj.contains(ParserKeys.Elt) =>
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

  protected def fixQualifiedName(name: String): String =
    name.stripPrefix(Defines.qualifiedNameSeparator).replace(Defines.qualifiedNameSeparator, ".")

}
