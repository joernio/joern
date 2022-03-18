package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelJsonParser.ParseResult
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.datastructures.scope.BlockScopeElement
import io.joern.jssrc2cpg.datastructures.scope.MethodScope
import io.joern.jssrc2cpg.datastructures.scope.MethodScopeElement
import io.joern.jssrc2cpg.datastructures.scope.ResolvedReference
import io.joern.jssrc2cpg.datastructures.scope.Scope
import io.joern.jssrc2cpg.datastructures.scope.ScopeType
import io.joern.jssrc2cpg.passes.Defines
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import org.slf4j.{Logger, LoggerFactory}
import ujson.Value

import scala.collection.mutable

class AstCreator(val config: Config, val diffGraph: DiffGraphBuilder, val parserResult: ParseResult, val global: Global)
    extends AstNodeBuilder
    with AstCreatorHelper {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope = new Scope()

  // TypeDecls with their bindings (with their refs) for lambdas and methods are not put in the AST
  // where the respective nodes are defined. Instead we put them under the parent TYPE_DECL in which they are defined.
  // To achieve this we need this extra stack.
  protected val methodAstParentStack: Stack[NewNode] = new Stack[NewNode]()
  protected val localAstParentStack: Stack[NewBlock] = new Stack[NewBlock]()

  def createAst(): Unit = {
    Ast.storeInDiffGraph(astForFile(), diffGraph)
    createVariableReferenceLinks()
  }

  private def astForFile(): Ast = {
    val name    = parserResult.filename
    val cpgFile = Ast(NewFile().name(name).order(0))
    cpgFile.withChild(astForParseResult())
  }

  protected def astsForNode(node: Value, order: Int): Seq[Ast] = nodeType(node) match {
    case BabelAst.File    => astsForNode(node("program"), order)
    case BabelAst.Program => astsForNodes(node("body").arr.toSeq)
    case _                => Seq(notHandledYet(node, order))
  }

  protected def astsForNodes(nodes: Seq[Value]): Seq[Ast] = withOrder(nodes) { (n, o) =>
    astsForNode(n, o)
  }.flatten

  private def createProgramMethod(path: String): Ast = {
    val allDecls     = Seq(parserResult.json("ast"))
    val lineNumber   = allDecls.headOption.flatMap(line)
    val columnNumber = allDecls.lastOption.flatMap(column)
    val name         = ":program"
    val fullName     = parserResult.filename + ":" + name

    val programMethod =
      NewMethod()
        .name(name)
        .code(name)
        .fullName(fullName)
        .filename(path)
        .lineNumber(lineNumber)
        .lineNumberEnd(columnNumber)
        .astParentType(NodeTypes.TYPE_DECL)
        .astParentFullName(fullName)

    methodAstParentStack.push(programMethod)

    val blockNode = NewBlock()
      .order(1)
      .argumentIndex(1)
      .typeFullName("ANY")

    scope.pushNewMethodScope(fullName, name, blockNode, None)
    localAstParentStack.push(blockNode)

    // We always create an instance parameter because in JS every function could get called with an instance.
    val thisParam = createParameterInNode("this", "this", 0, lineNumber, columnNumber)

    var currOrder = 1
    val methodChildren = allDecls.flatMap { node =>
      val r = astsForNode(node, currOrder)
      currOrder = currOrder + r.length
      r
    }.toIndexedSeq

    val methodReturn = NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName("ANY")
      .order(2)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(programMethod, methodAstParentStack.head, name, fullName, path)

    functionTypeAndTypeDeclAst.withChild(
      Ast(programMethod)
        .withChild(Ast(thisParam))
        .withChild(Ast(blockNode).withChildren(methodChildren))
        .withChild(Ast(methodReturn))
    )
  }

  private def astForParseResult(): Ast = {
    val absolutePath = parserResult.fullPath
    val name         = NamespaceTraversal.globalNamespaceName
    val fullName     = MetaDataPass.getGlobalNamespaceBlockFullName(Some(absolutePath))
    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .fullName(fullName)
      .filename(absolutePath)
      .order(1)
    methodAstParentStack.push(namespaceBlock)
    Ast(namespaceBlock).withChild(createProgramMethod(absolutePath))
  }

  private def createVariableReferenceLinks(): Unit = {
    val resolvedReferenceIt = scope.resolve(createMethodLocalForUnresolvedReference)
    val capturedLocals      = mutable.HashMap.empty[String, NewNode]

    resolvedReferenceIt.foreach { case ResolvedReference(variableNodeId, origin) =>
      var currentScope             = origin.stack
      var currentReferenceId       = origin.referenceNodeId
      var nextReferenceId: NewNode = null

      var done = false
      while (!done) {
        val localOrCapturedLocalIdOption =
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
                val closureBindingIdProperty =
                  methodScope.methodFullName + ":" + origin.variableName
                capturedLocals
                  .updateWith(closureBindingIdProperty) {
                    case None =>
                      val methodScopeNodeId = methodScope.scopeNode
                      val localId =
                        createLocalNode(origin.variableName, Defines.ANY.label, 0, Some(closureBindingIdProperty))
                      diffGraph.addEdge(methodScopeNodeId, localId, EdgeTypes.AST)
                      val closureBindingId = createClosureBindingNode(closureBindingIdProperty, origin.variableName)
                      methodScope.capturingRefId.foreach(ref =>
                        diffGraph.addEdge(ref, closureBindingId, EdgeTypes.CAPTURE)
                      )
                      nextReferenceId = closureBindingId
                      Some(localId)
                    case someLocalId =>
                      // When there is already a LOCAL representing the capturing, we do not
                      // need to process the surrounding scope element as this has already
                      // been processed.
                      done = true
                      someLocalId
                  }
              case _: BlockScopeElement => None
            }
          }

        localOrCapturedLocalIdOption.foreach { localOrCapturedLocalId =>
          diffGraph.addEdge(currentReferenceId, localOrCapturedLocalId, EdgeTypes.REF)
          currentReferenceId = nextReferenceId
        }

        currentScope = currentScope.get.surroundingScope
      }
    }
  }

  private def createMethodLocalForUnresolvedReference(
    methodScopeNodeId: NewNode,
    variableName: String
  ): (NewNode, ScopeType) = {
    val varId = createLocalNode(variableName, Defines.ANY.label, 0)
    diffGraph.addEdge(methodScopeNodeId, varId, EdgeTypes.AST)
    (varId, MethodScope)
  }

}
