package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst.*
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.joern.x2cpg.utils.IntervalKeyPool
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyDefaults, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.*
import ujson.Value

import scala.collection.{SortedMap, mutable}
import scala.util.Try

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val anonClassKeyPool = new IntervalKeyPool(first = 0, last = Long.MaxValue)

  protected def createBabelNodeInfo(json: Value): BabelNodeInfo = {
    val c     = code(json)
    val ln    = line(json)
    val cn    = column(json)
    val lnEnd = lineEnd(json)
    val cnEnd = columnEnd(json)
    val node  = nodeType(json)
    BabelNodeInfo(node, json, c, ln, cn, lnEnd, cnEnd)
  }

  private def nodeType(node: Value): BabelNode = fromString(node("type").str)

  protected def line(node: Value): Option[Int] = start(node).map(getLineOfSource)

  protected def start(node: Value): Option[Int] = Try(node("start").num.toInt).toOption

  protected def range(node: Value): Option[String] = {
    for {
      nodeStart <- start(node)
      nodeEnd   <- end(node)
    } yield s"$nodeStart:$nodeEnd"
  }

  // Returns the line number for a given position in the source.
  private def getLineOfSource(position: Int): Int = {
    val (_, lineNumber) = positionToLineNumberMapping.minAfter(position).get
    lineNumber
  }

  protected def lineEnd(node: Value): Option[Int] = end(node).map(getLineOfSource)

  protected def end(node: Value): Option[Int] = Try(node("end").num.toInt).toOption

  protected def column(node: Value): Option[Int] = start(node).map(getColumnOfSource)

  // Returns the column number for a given position in the source.
  private def getColumnOfSource(position: Int): Int = {
    val (_, firstPositionInLine) = positionToFirstPositionInLineMapping.minAfter(position).get
    position - firstPositionInLine
  }

  protected def columnEnd(node: Value): Option[Int] = end(node).map(getColumnOfSource)

  protected def setOrderExplicitly(ast: Ast, order: Int): Unit = {
    ast.root.foreach { case expr: ExpressionNew => expr.order = order }
  }

  protected def notHandledYet(node: BabelNodeInfo): Ast = {
    val text =
      s"""Node type '${node.node}' not handled yet!
         |  Code: '${node.code}'
         |  File: '${parserResult.fullPath}'
         |  Line: ${node.lineNumber.getOrElse(-1)}
         |  Column: ${node.columnNumber.getOrElse(-1)}
         |  """.stripMargin
    logger.info(text)
    Ast(unknownNode(node, node.code))
  }

  protected def createFunctionTypeAndTypeDeclAst(
    node: BabelNodeInfo,
    methodNode: NewMethod,
    parentNode: NewNode,
    methodName: String,
    methodFullName: String,
    filename: String
  ): Ast = {
    registerType(methodFullName)

    val astParentType     = parentNode.label
    val astParentFullName = parentNode.properties(PropertyNames.FullName).toString
    val functionTypeDeclNode =
      typeDeclNode(
        node,
        methodName,
        methodFullName,
        filename,
        methodName,
        astParentType = astParentType,
        astParentFullName = astParentFullName,
        List(Defines.Any)
      )

    // Problem for https://github.com/ShiftLeftSecurity/codescience/issues/3626 here.
    // As the type (thus, the signature) of the function node is unknown (i.e., ANY*)
    // we can't generate the correct binding with signature.
    val bindingNode = NewBinding().name("").signature("")
    Ast(functionTypeDeclNode).withBindsEdge(functionTypeDeclNode, bindingNode).withRefEdge(bindingNode, methodNode)
  }

  protected def registerType(typeFullName: String): Unit = {
    global.usedTypes.putIfAbsent(typeFullName, true)
  }

  protected def codeForNodes(nodes: Seq[NewNode]): Option[String] = nodes.collectFirst {
    case id: NewIdentifier => id.name.replace("...", "")
    case clazz: NewTypeRef => clazz.code.stripPrefix("class ")
  }

  protected def nameForBabelNodeInfo(nodeInfo: BabelNodeInfo, defaultName: Option[String]): String = {
    defaultName
      .orElse(codeForBabelNodeInfo(nodeInfo).headOption)
      .getOrElse {
        val tmpName    = generateUnusedVariableName(usedVariableNames, "_tmp")
        val nLocalNode = localNode(nodeInfo, tmpName, tmpName, Defines.Any).order(0)
        diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)
        tmpName
      }
  }

  protected def generateUnusedVariableName(
    usedVariableNames: mutable.HashMap[String, Int],
    variableName: String
  ): String = {
    val counter             = usedVariableNames.get(variableName).map(_ + 1).getOrElse(0)
    val currentVariableName = s"${variableName}_$counter"
    usedVariableNames.put(variableName, counter)
    currentVariableName
  }

  protected def safeBool(node: Value, key: String): Option[Boolean] =
    if (hasKey(node, key)) Try(node(key).bool).toOption else None

  protected def safeObj(node: Value, key: String): Option[upickle.core.LinkedHashMap[String, Value]] =
    Try(node(key).obj).toOption.filter(_.nonEmpty)

  protected def positionLookupTables(source: String): (SortedMap[Int, Int], SortedMap[Int, Int]) = {
    val positionToLineNumber, positionToFirstPositionInLine = mutable.TreeMap.empty[Int, Int]
    val data                                                = source.toCharArray
    var lineNumber                                          = 1
    var firstPositionInLine                                 = 0
    var position                                            = 0
    while (position < data.length) {
      val isNewLine = data(position) == '\n'
      if (isNewLine) {
        positionToLineNumber.put(position, lineNumber)
        lineNumber += 1
        positionToFirstPositionInLine.put(position, firstPositionInLine)
        firstPositionInLine = position + 1
      }
      position += 1
    }
    positionToLineNumber.put(position, lineNumber)
    positionToFirstPositionInLine.put(position, firstPositionInLine)

    // for empty line at the end of each JS/TS file generated by BabelJsonParser:
    positionToLineNumber.put(position + 1, lineNumber + 1)
    positionToFirstPositionInLine.put(position + 1, 0)
    (positionToLineNumber, positionToFirstPositionInLine)
  }

  protected def calcMethodNameAndFullName(func: BabelNodeInfo): (String, String) = {
    // functionNode.getName is not necessarily unique and thus the full name calculated based on the scope
    // is not necessarily unique. Specifically we have this problem with lambda functions which are defined
    // in the same scope.
    functionNodeToNameAndFullName.get(func) match {
      case Some(nameAndFullName) => nameAndFullName
      case None =>
        val intendedName   = calcMethodName(func)
        val fullNamePrefix = s"${parserResult.filename}:${scope.computeScopePath}:"
        var name           = intendedName
        var fullName       = ""
        var isUnique       = false
        var i              = 1
        while (!isUnique) {
          fullName = s"$fullNamePrefix$name"
          if (functionFullNames.contains(fullName)) {
            name = s"$intendedName$i"
            i += 1
          } else {
            isUnique = true
          }
        }
        functionFullNames.add(fullName)
        functionNodeToNameAndFullName(func) = (name, fullName)
        (name, fullName)
    }
  }

  private def calcMethodName(func: BabelNodeInfo): String = func.node match {
    case ObjectMethod if isMethodOrGetSet(func) && code(func.json("key")).startsWith("'") => nextClosureName()
    case TSCallSignatureDeclaration                                                       => nextClosureName()
    case TSConstructSignatureDeclaration => io.joern.x2cpg.Defines.ConstructorMethodName
    case _ if isMethodOrGetSet(func) =>
      if (hasKey(func.json("key"), "name")) func.json("key")("name").str
      else code(func.json("key"))
    case _ if safeStr(func.json, "kind").contains("constructor") => io.joern.x2cpg.Defines.ConstructorMethodName
    case _ if func.json("id").isNull                             => nextClosureName()
    case _                                                       => func.json("id")("name").str
  }

  protected def safeStr(node: Value, key: String): Option[String] =
    if (hasKey(node, key)) Try(node(key).str).toOption else None

  private def isMethodOrGetSet(func: BabelNodeInfo): Boolean = {
    if (hasKey(func.json, "kind") && !func.json("kind").isNull) {
      val t = func.json("kind").str
      t == "method" || t == "get" || t == "set"
    } else false
  }

  protected def codeOf(node: NewNode): String = node match {
    case astNodeNew: AstNodeNew => astNodeNew.code
    case _                      => ""
  }

  protected def stripQuotes(str: String): String = str
    .stripPrefix("\"")
    .stripSuffix("\"")
    .stripPrefix("'")
    .stripSuffix("'")
    .stripPrefix("`")
    .stripSuffix("`")

  protected def calcTypeNameAndFullName(
    classNode: BabelNodeInfo,
    preCalculatedName: Option[String] = None
  ): (String, String) = {
    val name           = preCalculatedName.getOrElse(calcTypeName(classNode))
    val fullNamePrefix = s"${parserResult.filename}:${scope.computeScopePath}:"
    val fullName       = s"$fullNamePrefix$name"
    (name, fullName)
  }

  /** In JS it is possible to create anonymous classes. We have to handle this here.
    */
  private def calcTypeName(classNode: BabelNodeInfo): String =
    if (hasKey(classNode.json, "id") && !classNode.json("id").isNull) code(classNode.json("id"))
    else nextAnonClassName()

  protected def code(node: Value): String = {
    nodeOffsets(node) match {
      case Some((startOffset, endOffset)) =>
        shortenCode(parserResult.fileContent.substring(startOffset, endOffset).trim)
      case _ =>
        PropertyDefaults.Code
    }
  }

  protected def hasKey(node: Value, key: String): Boolean = Try(node(key)).isSuccess

  protected def nextAnonClassName(): String = s"<anon-class>${anonClassKeyPool.next}"

}
