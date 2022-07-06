package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.BlockScopeElement
import io.joern.jssrc2cpg.datastructures.MethodScope
import io.joern.jssrc2cpg.datastructures.MethodScopeElement
import io.joern.jssrc2cpg.datastructures.ResolvedReference
import io.joern.jssrc2cpg.datastructures.ScopeElement
import io.joern.jssrc2cpg.datastructures.ScopeElementIterator
import io.joern.jssrc2cpg.datastructures.ScopeType
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import org.apache.commons.lang.StringUtils
import ujson.Value

import scala.collection.mutable
import scala.collection.SortedMap
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.util.Success
import scala.util.Try

trait AstCreatorHelper {

  this: AstCreator =>

  // maximum length of code fields in number of characters
  private val MAX_CODE_LENGTH: Int = 1000
  private val MIN_CODE_LENGTH: Int = 50

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
    s"""Node type '${node.node}' not handled yet!
       |  Code: '${shortenCode(node.code, length = 50)}'
       |  File: '${parserResult.fullPath}'
       |  Line: ${node.lineNumber.getOrElse(-1)}
       |  Column: ${node.columnNumber.getOrElse(-1)}
       |  """.stripMargin

  protected def notHandledYet(node: BabelNodeInfo, additionalInfo: String = ""): Ast = {
    val text =
      if (additionalInfo.isEmpty) notHandledText(node)
      else s"$additionalInfo: ${notHandledText(node)}"
    logger.info(text)
    Ast(newUnknown(node))
  }

  protected def registerType(typeName: String, typeFullName: String): Unit = {
    if (usedTypes.containsKey((typeName, typeName))) {
      usedTypes.put((typeName, typeFullName), true)
      usedTypes.remove((typeName, typeName))
    } else if (!usedTypes.keys().asScala.exists { case (tpn, _) => tpn == typeName }) {
      usedTypes.putIfAbsent((typeName, typeFullName), true)
    }
  }

  private def nodeType(node: Value): BabelAst.BabelNode =
    BabelAst.fromString(node("type").str)

  protected def generateUnusedVariableName(
    usedVariableNames: mutable.HashMap[String, Int],
    variableName: String
  ): String = {
    val counter             = usedVariableNames.get(variableName).map(_ + 1).getOrElse(0)
    val currentVariableName = s"${variableName}_$counter"
    usedVariableNames.put(variableName, counter)
    currentVariableName
  }

  protected def code(node: Value): String =
    parserResult.fileContent.substring(start(node).getOrElse(0), end(node).getOrElse(0)).trim

  private def shortenCode(code: String, length: Int = MAX_CODE_LENGTH): String =
    StringUtils.abbreviate(code, math.max(MIN_CODE_LENGTH, length))

  protected def hasKey(node: Value, key: String): Boolean = Try(node(key)).isSuccess

  protected def safeStr(node: Value, key: String): Option[String] =
    if (hasKey(node, key)) Some(node(key).str) else None

  protected def safeBool(node: Value, key: String): Option[Boolean] =
    if (hasKey(node, key)) Some(node(key).bool) else None

  protected def safeObj(node: Value, key: String): Option[mutable.LinkedHashMap[String, Value]] = Try(
    node(key).obj
  ) match {
    case Success(value) if value.nonEmpty => Some(value)
    case _                                => None
  }

  private def start(node: Value): Option[Int] = Try(node("start").num.toInt).toOption

  private def end(node: Value): Option[Int] = Try(node("end").num.toInt).toOption

  protected def line(node: Value): Option[Integer] = start(node).map(getLineOfSource)

  protected def lineEnd(node: Value): Option[Integer] = end(node).map(getLineOfSource)

  protected def column(node: Value): Option[Integer] = start(node).map(getColumnOfSource)

  protected def columnEnd(node: Value): Option[Integer] = end(node).map(getColumnOfSource)

  // Returns the line number for a given position in the source.
  private def getLineOfSource(position: Int): Int = {
    val (_, lineNumber) = positionToLineNumberMapping.minAfter(position).get
    lineNumber
  }

  // Returns the column number for a given position in the source.
  private def getColumnOfSource(position: Int): Int = {
    val (_, firstPositionInLine) = positionToFirstPositionInLineMapping.minAfter(position).get
    position - firstPositionInLine
  }

  protected def positionLookupTables(source: String): (SortedMap[Int, Int], SortedMap[Int, Int]) = {
    val positionToLineNumber, positionToFirstPositionInLine = mutable.TreeMap.empty[Int, Int]

    val data                = source.toCharArray
    var lineNumber          = 1
    var firstPositionInLine = 0
    var position            = 0

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

    (positionToLineNumber, positionToFirstPositionInLine)
  }

  private def computeScopePath(stack: Option[ScopeElement]): String =
    new ScopeElementIterator(stack)
      .to(Seq)
      .reverse
      .collect { case methodScopeElement: MethodScopeElement =>
        methodScopeElement.name
      }
      .mkString(":")

  private def calcMethodName(func: BabelNodeInfo): String = func.node match {
    case BabelAst.TSCallSignatureDeclaration                     => "anonymous"
    case BabelAst.TSConstructSignatureDeclaration                => "<constructor>"
    case _ if safeStr(func.json, "kind").contains("method")      => func.json("key")("name").str
    case _ if safeStr(func.json, "kind").contains("constructor") => "<constructor>"
    case _ if func.json("id").isNull                             => "anonymous"
    case _                                                       => func.json("id")("name").str
  }

  protected def calcMethodNameAndFullName(func: BabelNodeInfo): (String, String) = {
    // functionNode.getName is not necessarily unique and thus the full name calculated based on the scope
    // is not necessarily unique. Specifically we have this problem with lambda functions which are defined
    // in the same scope.
    functionNodeToNameAndFullName.get(func) match {
      case Some(nameAndFullName) => nameAndFullName
      case None =>
        val intendedName   = calcMethodName(func)
        val fullNamePrefix = s"${parserResult.filename}:${computeScopePath(scope.getScopeHead)}:"
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

  /** In JS it is possible to create anonymous classes. We have to handle this here.
    */
  private def calcTypeName(classNode: BabelNodeInfo): String =
    if (hasKey(classNode.json, "id") && !classNode.json("id").isNull) code(classNode.json("id"))
    else "_anon_cdecl"

  protected def calcTypeNameAndFullName(
    classNode: BabelNodeInfo,
    preCalculatedName: Option[String] = None
  ): (String, String) = {
    val name             = preCalculatedName.getOrElse(calcTypeName(classNode))
    val fullNamePrefix   = s"${parserResult.filename}:${computeScopePath(scope.getScopeHead)}:"
    val intendedFullName = s"$fullNamePrefix$name"
    val postfix          = typeFullNameToPostfix.getOrElse(intendedFullName, 0)
    val resultingFullName =
      if (postfix == 0) intendedFullName
      else s"$intendedFullName$postfix"
    typeFullNameToPostfix.put(intendedFullName, postfix + 1)
    (name, resultingFullName)
  }

  protected def createVariableReferenceLinks(): Unit = {
    val resolvedReferenceIt = scope.resolve(createMethodLocalForUnresolvedReference)
    val capturedLocals      = mutable.HashMap.empty[String, NewNode]

    resolvedReferenceIt.foreach { case ResolvedReference(variableNodeId, origin) =>
      var currentScope           = origin.stack
      var currentReference       = origin.referenceNode
      var nextReference: NewNode = null

      var done = false
      while (!done) {
        val localOrCapturedLocalNodeOption =
          if (currentScope.get.nameToVariableNode.contains(origin.variableName)) {
            done = true
            Some(variableNodeId)
          } else {
            currentScope.flatMap {
              case methodScope: MethodScopeElement =>
                // We have reached a MethodScope and still did not find a local variable to link to.
                // For all non local references the CPG format does not allow us to link
                // directly. Instead we need to create a fake local variable in method
                // scope and link to this local which itself carries the information
                // that it is a captured variable. This needs to be done for each
                // method scope until we reach the originating scope.
                val closureBindingIdProperty = s"${methodScope.methodFullName}:${origin.variableName}"
                capturedLocals.updateWith(closureBindingIdProperty) {
                  case None =>
                    val methodScopeNode = methodScope.scopeNode
                    val localNode =
                      createLocalNode(origin.variableName, Defines.ANY.label, Some(closureBindingIdProperty))
                    diffGraph.addEdge(methodScopeNode, localNode, EdgeTypes.AST)
                    val closureBindingNode = createClosureBindingNode(closureBindingIdProperty, origin.variableName)
                    methodScope.capturingRefId.foreach(ref =>
                      diffGraph.addEdge(ref, closureBindingNode, EdgeTypes.CAPTURE)
                    )
                    nextReference = closureBindingNode
                    Some(localNode)
                  case someLocalNode =>
                    // When there is already a LOCAL representing the capturing, we do not
                    // need to process the surrounding scope element as this has already
                    // been processed.
                    done = true
                    someLocalNode
                }
              case _: BlockScopeElement => None
            }
          }

        localOrCapturedLocalNodeOption.foreach { localOrCapturedLocalNode =>
          diffGraph.addEdge(currentReference, localOrCapturedLocalNode, EdgeTypes.REF)
          currentReference = nextReference
        }
        currentScope = currentScope.get.surroundingScope
      }
    }
  }

  private def createMethodLocalForUnresolvedReference(
    methodScopeNodeId: NewNode,
    variableName: String
  ): (NewNode, ScopeType) = {
    val local = createLocalNode(variableName, Defines.ANY.label)
    diffGraph.addEdge(methodScopeNodeId, local, EdgeTypes.AST)
    (local, MethodScope)
  }

}
