package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.datastructures.*
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.FunctionDeclSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.SwiftNode
import io.joern.swiftsrc2cpg.passes.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.utils.NodeBuilders.{newClosureBindingNode, newLocalNode}
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, EvaluationStrategies}
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl

import scala.collection.mutable
import scala.jdk.CollectionConverters.EnumerationHasAsScala

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def notHandledYet(node: SwiftNode): Ast = {
    val text =
      s"""Node type '${node.toString}' not handled yet!
         |  Code: '${code(node)}'
         |  File: '${parserResult.fullPath}'
         |  Line: ${line(node).getOrElse(-1)}
         |  Column: ${column(node).getOrElse(-1)}
         |  """.stripMargin
    logger.info(text)
    Ast(unknownNode(node, code(node)))
  }

  protected def registerType(typeName: String, typeFullName: String): Unit = {
    if (usedTypes.containsKey((typeName, typeName)) && typeName != typeFullName) {
      usedTypes.put((typeName, typeFullName), true)
      usedTypes.remove((typeName, typeName))
    } else if (!usedTypes.keys().asScala.exists { case (tpn, _) => tpn == typeName }) {
      usedTypes.putIfAbsent((typeName, typeFullName), true)
    }
  }

  protected def generateUnusedVariableName(
    usedVariableNames: mutable.HashMap[String, Int],
    variableName: String
  ): String = {
    val counter             = usedVariableNames.get(variableName).fold(0)(_ + 1)
    val currentVariableName = s"${variableName}_$counter"
    usedVariableNames.put(variableName, counter)
    currentVariableName
  }

  private def computeScopePath(stack: Option[ScopeElement]): String =
    new ScopeElementIterator(stack)
      .to(Seq)
      .reverse
      .collect { case methodScopeElement: MethodScopeElement => methodScopeElement.name }
      .mkString(":")

  private def calcMethodName(func: FunctionDeclSyntax): String = code(func.name)

  protected def calcMethodNameAndFullName(func: FunctionDeclSyntax): (String, String) = {
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
              case _: BlockScopeElement =>
                None
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
