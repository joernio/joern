package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.*
import io.joern.jssrc2cpg.parser.BabelAst.*
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.joern.x2cpg.utils.IntervalKeyPool
import io.joern.x2cpg.utils.NodeBuilders.{newClosureBindingNode, newLocalNode}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, EvaluationStrategies}
import io.shiftleft.codepropertygraph.generated.nodes.File.PropertyDefaults
import ujson.Value

import scala.collection.{mutable, SortedMap}
import scala.util.{Success, Try}

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val anonClassKeyPool = new IntervalKeyPool(first = 0, last = Long.MaxValue)

  protected def nextAnonClassName(): String = s"<anon-class>${anonClassKeyPool.next}"

  protected def createBabelNodeInfo(json: Value): BabelNodeInfo = {
    val c     = code(json)
    val ln    = line(json)
    val cn    = column(json)
    val lnEnd = lineEnd(json)
    val cnEnd = columnEnd(json)
    val node  = nodeType(json)
    BabelNodeInfo(node, json, c, ln, cn, lnEnd, cnEnd)
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

  protected def registerType(typeFullName: String): Unit = {
    global.usedTypes.putIfAbsent(typeFullName, true)
  }

  private def nodeType(node: Value): BabelNode = fromString(node("type").str)

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

  protected def code(node: Value): String = {
    nodeOffsets(node) match {
      case Some((startOffset, endOffset)) =>
        shortenCode(parserResult.fileContent.substring(startOffset, endOffset).trim)
      case _ =>
        PropertyDefaults.Code
    }
  }

  protected def hasKey(node: Value, key: String): Boolean = Try(node(key)).isSuccess

  protected def safeStr(node: Value, key: String): Option[String] =
    if (hasKey(node, key)) Try(node(key).str).toOption else None

  protected def safeBool(node: Value, key: String): Option[Boolean] =
    if (hasKey(node, key)) Try(node(key).bool).toOption else None

  protected def safeObj(node: Value, key: String): Option[upickle.core.LinkedHashMap[String, Value]] = Try(
    node(key).obj
  ) match {
    case Success(value) if value.nonEmpty => Option(value)
    case _                                => None
  }

  protected def start(node: Value): Option[Int] = Try(node("start").num.toInt).toOption

  protected def end(node: Value): Option[Int] = Try(node("end").num.toInt).toOption

  protected def pos(node: Value): Option[Int] = Try(node("start").num.toInt).toOption

  protected def line(node: Value): Option[Int] = start(node).map(getLineOfSource)

  protected def lineEnd(node: Value): Option[Int] = end(node).map(getLineOfSource)

  protected def column(node: Value): Option[Int] = start(node).map(getColumnOfSource)

  protected def columnEnd(node: Value): Option[Int] = end(node).map(getColumnOfSource)

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

  private def computeScopePath(stack: Option[ScopeElement]): String =
    new ScopeElementIterator(stack)
      .to(Seq)
      .reverse
      .collect { case methodScopeElement: MethodScopeElement => methodScopeElement.name }
      .mkString(":")

  private def isMethodOrGetSet(func: BabelNodeInfo): Boolean = {
    if (hasKey(func.json, "kind") && !func.json("kind").isNull) {
      val t = func.json("kind").str
      t == "method" || t == "get" || t == "set"
    } else false
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

  protected def stripQuotes(str: String): String = str
    .stripPrefix("\"")
    .stripSuffix("\"")
    .stripPrefix("'")
    .stripSuffix("'")
    .stripPrefix("`")
    .stripSuffix("`")

  /** In JS it is possible to create anonymous classes. We have to handle this here.
    */
  private def calcTypeName(classNode: BabelNodeInfo): String =
    if (hasKey(classNode.json, "id") && !classNode.json("id").isNull) code(classNode.json("id"))
    else nextAnonClassName()

  protected def calcTypeNameAndFullName(
    classNode: BabelNodeInfo,
    preCalculatedName: Option[String] = None
  ): (String, String) = {
    val name           = preCalculatedName.getOrElse(calcTypeName(classNode))
    val fullNamePrefix = s"${parserResult.filename}:${computeScopePath(scope.getScopeHead)}:"
    val fullName       = s"$fullNamePrefix$name"
    (name, fullName)
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
            Option(variableNodeId)
          } else {
            currentScope.flatMap {
              case methodScope: MethodScopeElement
                  if methodScope.scopeNode.isInstanceOf[NewTypeDecl] || methodScope.scopeNode
                    .isInstanceOf[NewNamespaceBlock] =>
                currentScope = Option(Scope.getEnclosingMethodScopeElement(currentScope))
                None
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
                      newLocalNode(origin.variableName, Defines.Any, Option(closureBindingIdProperty)).order(0)
                    diffGraph.addEdge(methodScopeNode, localNode, EdgeTypes.AST)
                    val closureBindingNode = newClosureBindingNode(
                      closureBindingIdProperty,
                      origin.variableName,
                      EvaluationStrategies.BY_REFERENCE
                    )
                    methodScope.capturingRefId.foreach(ref =>
                      diffGraph.addEdge(ref, closureBindingNode, EdgeTypes.CAPTURE)
                    )
                    nextReference = closureBindingNode
                    Option(localNode)
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
    val local = newLocalNode(variableName, Defines.Any).order(0)
    diffGraph.addEdge(methodScopeNodeId, local, EdgeTypes.AST)
    (local, MethodScope)
  }

}
