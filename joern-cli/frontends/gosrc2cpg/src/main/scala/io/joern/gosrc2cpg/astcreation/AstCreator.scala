package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.model.GoMod
import io.joern.gosrc2cpg.parser.GoAstJsonParser.ParserResult
import io.joern.gosrc2cpg.parser.ParserAst._
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.datastructures.Scope
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder => X2CpgAstNodeBuilder}
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewNode, NewNamespaceBlock}
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import ujson.Value

class AstCreator(val relPathFileName: String, val parserResult: ParserResult)
    extends AstCreatorBase(relPathFileName)
    with AstCreatorHelper
    with AstForDeclarationCreator
    with AstForExpressionCreator
    with AstForFunctionsCreator
    with AstForPrimitivesCreator
    with AstForStatementsCreator
    with X2CpgAstNodeBuilder[ParserNodeInfo, AstCreator] {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope: Scope[String, (NewNode, String), NewNode] = new Scope()

  override def createAst(): DiffGraphBuilder = {
    val rootNode = createParserNodeInfo(parserResult.json)
    val ast      = astForTranslationUnit(rootNode)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def astForTranslationUnit(rootNode: ParserNodeInfo): Ast = {
    val fullyQualifiedPackage =
      GoMod.getNameSpace(parserResult.filename, parserResult.json(ParserKeys.Name)(ParserKeys.Name).str)
    val namespaceBlock = NewNamespaceBlock()
      .name(fullyQualifiedPackage)
      .fullName(s"$relPathFileName:${fullyQualifiedPackage}")
      .filename(relPathFileName)
    Ast(namespaceBlock).withChild(
      astInFakeMethod(fullyQualifiedPackage, namespaceBlock.fullName, relPathFileName, rootNode)
    )
  }

  /** Creates an AST of all declarations found in the translation unit - wrapped in a fake method.
    */
  private def astInFakeMethod(name: String, fullName: String, path: String, rootNode: ParserNodeInfo): Ast = {

    val fakeGlobalTypeDecl =
      typeDeclNode(rootNode, name, fullName, relPathFileName, name, NodeTypes.NAMESPACE_BLOCK, fullName)

    val fakeGlobalMethod =
      methodNode(rootNode, name, name, fullName, None, path, Option(NodeTypes.TYPE_DECL), Option(fullName))

    val blockNode_ = blockNode(rootNode, Defines.empty, Defines.anyTypeName)

    val methodReturn = methodReturnNode(rootNode, Defines.anyTypeName)
    val declsAsts    = rootNode.json(ParserKeys.Decls).arr.map(item => astForNode(item)).toList
    Ast(fakeGlobalTypeDecl).withChild(
      methodAst(fakeGlobalMethod, Seq.empty, blockAst(blockNode_, declsAsts), methodReturn)
    )
  }

  protected def astForNode(json: Value): Ast = {
    val nodeInfo = createParserNodeInfo(json)
    val output = nodeInfo.node match {
      case GenDecl if isImportDeclaration(nodeInfo) => astForImport(nodeInfo)
      case BasicLit                                 => astForLiteral(nodeInfo)
      case Ident                                    => astForIdentifier(nodeInfo)
      case FuncDecl                                 => astForFuncDecl(nodeInfo)
      case _: BaseExprStmt                          => astForExpression(nodeInfo)
      case _                                        => Ast()
    }
    output
  }
  override protected def line(node: ParserNodeInfo): Option[Integer] = node.lineNumber

  override protected def column(node: ParserNodeInfo): Option[Integer] = node.columnNumber

  override protected def lineEnd(node: ParserNodeInfo): Option[Integer] = node.lineNumberEnd

  override protected def columnEnd(node: ParserNodeInfo): Option[Integer] = node.columnNumberEnd
}
