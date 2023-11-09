package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.datastructures.{MethodScope, Scope}
import io.joern.swiftsrc2cpg.parser.SwiftJsonParser.ParseResult
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.SourceFileSyntax
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.SwiftNode
import io.joern.swiftsrc2cpg.passes.Defines
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.utils.NodeBuilders.newMethodReturnNode
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode, AstNodeBuilder as X2CpgAstNodeBuilder}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeRef
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import ujson.Value

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

class AstCreator(
  val config: Config,
  val parserResult: ParseResult,
  val usedTypes: ConcurrentHashMap[(String, String), Boolean]
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase(parserResult.filename)
    with AstCreatorHelper
    with X2CpgAstNodeBuilder[SwiftNode, AstCreator] {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope = new Scope()

  protected val methodAstParentStack          = new Stack[NewNode]()
  protected val typeRefIdStack                = new Stack[NewTypeRef]
  protected val dynamicInstanceTypeStack      = new Stack[String]
  protected val localAstParentStack           = new Stack[NewBlock]()
  protected val rootTypeDecl                  = new Stack[NewTypeDecl]()
  protected val typeFullNameToPostfix         = mutable.HashMap.empty[String, Int]
  protected val functionNodeToNameAndFullName = mutable.HashMap.empty[SwiftNode, (String, String)]
  protected val usedVariableNames             = mutable.HashMap.empty[String, Int]
  protected val seenAliasTypes                = mutable.HashSet.empty[NewTypeDecl]
  protected val functionFullNames             = mutable.HashSet.empty[String]

  override def createAst(): DiffGraphBuilder = {
    val fileNode       = NewFile().name(parserResult.filename).order(1)
    val namespaceBlock = globalNamespaceBlock()
    methodAstParentStack.push(namespaceBlock)
    val astForFakeMethod =
      astInFakeMethod(namespaceBlock.fullName, parserResult.filename, parserResult.ast)
    val ast = Ast(fileNode).withChild(Ast(namespaceBlock).withChild(astForFakeMethod))
    Ast.storeInDiffGraph(ast, diffGraph)
    createVariableReferenceLinks()
    diffGraph
  }

  private def astInFakeMethod(fullName: String, path: String, ast: SwiftNode): Ast = {
    val allSiftNodes = ast match {
      case sourceFile: SourceFileSyntax => sourceFile.statements.children
      case _                            => Seq.empty
    }
    val name = NamespaceTraversal.globalNamespaceName

    val fakeGlobalTypeDecl =
      typeDeclNode(ast, name, fullName, path, name, NodeTypes.NAMESPACE_BLOCK, fullName)
    methodAstParentStack.push(fakeGlobalTypeDecl)

    val fakeGlobalMethod =
      methodNode(ast, name, name, fullName, None, path, Option(NodeTypes.TYPE_DECL), Option(fullName))
    methodAstParentStack.push(fakeGlobalMethod)
    scope.pushNewMethodScope(fullName, name, fakeGlobalMethod, None)

    val blockNode_ = blockNode(ast, "<empty>", Defines.Any)

    val childrenAsts = allSiftNodes.map(astForNode)
    setArgumentIndices(childrenAsts)

    val methodReturn = newMethodReturnNode(Defines.Any, None, line(ast), column(ast))
    Ast(fakeGlobalTypeDecl).withChild(
      methodAst(fakeGlobalMethod, Seq.empty, blockAst(blockNode_, childrenAsts.toList), methodReturn)
    )
  }

  protected def astForNode(node: SwiftNode): Ast = node match {
    case _ => notHandledYet(node)
  }

  protected def line(node: SwiftNode): Option[Integer]      = node.startLine.map(Integer.valueOf)
  protected def column(node: SwiftNode): Option[Integer]    = node.startColumn.map(Integer.valueOf)
  protected def lineEnd(node: SwiftNode): Option[Integer]   = node.endLine.map(Integer.valueOf)
  protected def columnEnd(node: SwiftNode): Option[Integer] = node.endColumn.map(Integer.valueOf)
  protected def code(node: SwiftNode): String = {
    val startIndex = node.startOffset.getOrElse(0)
    val endIndex   = Math.min(node.endOffset.getOrElse(0), parserResult.fileContent.length)
    shortenCode(parserResult.fileContent.substring(startIndex, endIndex).trim)
  }
}
