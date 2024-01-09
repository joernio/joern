package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.datastructures.*
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.AccessorDeclSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.CodeBlockItemSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.DeinitializerDeclSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.FunctionDeclSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.GuardStmtSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.InitializerDeclSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.SwiftNode
import io.joern.swiftsrc2cpg.passes.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.utils.NodeBuilders.{newClosureBindingNode, newLocalNode}
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, EvaluationStrategies}
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes

import scala.collection.mutable

object AstCreatorHelper {

  implicit class OptionSafeAst(val ast: Ast) extends AnyVal {
    def withArgEdge(src: NewNode, dst: Option[NewNode]): Ast = dst match {
      case Some(value) => ast.withArgEdge(src, value)
      case None        => ast
    }
  }

}

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

  protected def astsForBlockElements(elements: List[SwiftNode]): List[Ast] = {
    val indexOfGuardStmt = elements.indexWhere(n =>
      n.isInstanceOf[CodeBlockItemSyntax] && n.asInstanceOf[CodeBlockItemSyntax].item.isInstanceOf[GuardStmtSyntax]
    )
    if (indexOfGuardStmt < 0) {
      val childrenAsts = elements.map(astForNode)
      setArgumentIndices(childrenAsts)
      childrenAsts
    } else {
      val elementsBeforeGuard = elements.slice(0, indexOfGuardStmt)
      val guardStmt = elements(indexOfGuardStmt).asInstanceOf[CodeBlockItemSyntax].item.asInstanceOf[GuardStmtSyntax]
      val elementsAfterGuard = elements.slice(indexOfGuardStmt + 1, elements.size)

      val code         = this.code(guardStmt)
      val ifNode       = controlStructureNode(guardStmt, ControlStructureTypes.IF, code)
      val conditionAst = astForNode(guardStmt.conditions)

      val thenAst = astsForBlockElements(elementsAfterGuard) match {
        case Nil => Ast()
        case blockElement :: Nil =>
          setOrderExplicitly(blockElement, 2)
          blockElement
        case blockChildren =>
          val block = blockNode(elementsAfterGuard.head).order(2)
          setArgumentIndices(blockChildren)
          blockAst(block, blockChildren)
      }
      val elseAst = astForNode(guardStmt.body)
      setOrderExplicitly(elseAst, 3)

      val ifAst         = controlStructureAst(ifNode, Some(conditionAst), Seq(thenAst, elseAst))
      val resultingAsts = astsForBlockElements(elementsBeforeGuard) :+ ifAst
      setArgumentIndices(resultingAsts)
      resultingAsts
    }
  }

  protected def registerType(typeFullName: String): Unit = {
    global.usedTypes.putIfAbsent(typeFullName, true)
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

  private def calcMethodName(func: SwiftNode): String = func match {
    case f: FunctionDeclSyntax      => code(f.name)
    case a: AccessorDeclSyntax      => code(a.accessorSpecifier)
    case d: DeinitializerDeclSyntax => code(d.deinitKeyword)
    case i: InitializerDeclSyntax   => code(i.initKeyword)
    case _                          => nextClosureName()
  }

  protected def calcTypeNameAndFullName(name: String): (String, String) = {
    val fullNamePrefix   = s"${parserResult.filename}:${computeScopePath(scope.getScopeHead)}:"
    val intendedFullName = s"$fullNamePrefix$name"
    val postfix          = typeFullNameToPostfix.getOrElse(intendedFullName, 0)
    val resultingFullName =
      if (postfix == 0) intendedFullName
      else s"$intendedFullName$postfix"
    typeFullNameToPostfix.put(intendedFullName, postfix + 1)
    (name, resultingFullName)
  }

  protected def calcMethodNameAndFullName(func: SwiftNode): (String, String) = {
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
