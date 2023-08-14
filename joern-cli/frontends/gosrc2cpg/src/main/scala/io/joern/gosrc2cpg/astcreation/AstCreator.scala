package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.model.GoMod
import io.joern.gosrc2cpg.parser.GoAstJsonParser.ParserResult
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.datastructures.Scope
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode, AstNodeBuilder as X2CpgAstNodeBuilder}
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewNamespaceBlock, NewNode}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import ujson.Value

class AstCreator(val relPathFileName: String, val parserResult: ParserResult)(implicit
  withSchemaValidation: ValidationMode
) extends AstCreatorBase(relPathFileName)
    with AstCreatorHelper
    with AstForGenDeclarationCreator
    with AstForExpressionCreator
    with AstForFunctionsCreator
    with AstForPrimitivesCreator
    with AstForStatementsCreator
    with X2CpgAstNodeBuilder[ParserNodeInfo, AstCreator] {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val methodAstParentStack: Stack[NewNode]             = new Stack()
  protected val scope: Scope[String, (NewNode, String), NewNode] = new Scope()
  protected val fullyQualifiedPackage                            = new ThreadLocal[String]

  protected val lineNumberMapping: Map[Int, String] = positionLookupTables(parserResult.fileContent)

  override def createAst(): DiffGraphBuilder = {
    val rootNode = createParserNodeInfo(parserResult.json)
    val ast      = astForTranslationUnit(rootNode)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def astForTranslationUnit(rootNode: ParserNodeInfo): Ast = {
    fullyQualifiedPackage.set(
      GoMod.getNameSpace(parserResult.filename, parserResult.json(ParserKeys.Name)(ParserKeys.Name).str)
    )
    val namespaceBlock = NewNamespaceBlock()
      .name(fullyQualifiedPackage.get())
      .fullName(s"$relPathFileName:${fullyQualifiedPackage.get()}")
      .filename(relPathFileName)
    methodAstParentStack.push(namespaceBlock)
    Ast(namespaceBlock).withChild(
      astInFakeMethod(
        fullyQualifiedPackage.get() + "." + NamespaceTraversal.globalNamespaceName,
        namespaceBlock.fullName,
        relPathFileName,
        rootNode
      )
    )
  }

  /** Creates an AST of all declarations found in the translation unit - wrapped in a fake method.
    */
  private def astInFakeMethod(name: String, fullName: String, path: String, rootNode: ParserNodeInfo): Ast = {

    val fakeGlobalTypeDecl =
      typeDeclNode(rootNode, name, fullName, relPathFileName, name, NodeTypes.NAMESPACE_BLOCK, fullName)
    methodAstParentStack.push(fakeGlobalTypeDecl)
    val fakeGlobalMethod =
      methodNode(rootNode, name, name, fullName, None, path, Option(NodeTypes.TYPE_DECL), Option(fullName))
    methodAstParentStack.push(fakeGlobalMethod)
    scope.pushNewScope(fakeGlobalMethod)
    val blockNode_ = blockNode(rootNode, Defines.empty, Defines.anyTypeName)

    val methodReturn = methodReturnNode(rootNode, Defines.anyTypeName)
    val declsAsts    = rootNode.json(ParserKeys.Decls).arr.flatMap(item => astForNode(item)).toList
    Ast(fakeGlobalTypeDecl).withChild(
      methodAst(fakeGlobalMethod, Seq.empty, blockAst(blockNode_, declsAsts), methodReturn)
    )
  }

  protected def astForNode(nodeInfo: ParserNodeInfo): Seq[Ast] = {
    nodeInfo.node match {
      case GenDecl          => astForGenDecl(nodeInfo)
      case FuncDecl         => astForFuncDecl(nodeInfo)
      case _: BasePrimitive => Seq(astForPrimitive(nodeInfo))
      case _: BaseExpr      => astsForExpression(nodeInfo)
      case _: BaseStmt      => astsForStatement(nodeInfo)
      case _                => Seq()
    }
  }
  protected def astForNode(json: Value): Seq[Ast] = {
    astForNode(createParserNodeInfo(json))
  }
}
