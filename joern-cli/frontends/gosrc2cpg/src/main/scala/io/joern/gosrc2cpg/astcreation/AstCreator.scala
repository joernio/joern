package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{GoAstJsonParser, ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.astgen.AstGenNodeBuilder
import io.joern.x2cpg.datastructures.Scope
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, NodeTypes}
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import ujson.Value

import java.nio.file.Paths
import scala.collection.mutable

class AstCreator(
  val jsonAstFilePath: String,
  val relPathFileName: String,
  val goMod: GoModHelper,
  val goGlobal: GoGlobal
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase(relPathFileName)
    with AstCreatorHelper
    with AstForGenDeclarationCreator
    with AstForExpressionCreator
    with AstForFunctionsCreator
    with AstForPrimitivesCreator
    with AstForStatementsCreator
    with AstForTypeDeclCreator
    with AstForMethodCallExpressionCreator
    with CommonCacheBuilder
    with AstForLambdaCreator
    with InitialMainSrcProcessor
    with DepdencySrcProcessor
    with AstGenNodeBuilder[AstCreator] {

  protected val logger: Logger                                   = LoggerFactory.getLogger(classOf[AstCreator])
  val parserResult                                               = GoAstJsonParser.readFile(Paths.get(jsonAstFilePath))
  protected val methodAstParentStack: Stack[NewNode]             = new Stack()
  protected val scope: Scope[String, (NewNode, String), NewNode] = new Scope()
  protected val aliasToNameSpaceMapping: mutable.Map[String, String] = mutable.Map.empty
  protected val lineNumberMapping: Map[Int, String]                  = positionLookupTables
  protected val declaredPackageName = parserResult.json(ParserKeys.Name)(ParserKeys.Name).str
  protected val fullyQualifiedPackage =
    goMod.getNameSpace(parserResult.fullPath, declaredPackageName)

  override def createAst(): DiffGraphBuilder = {
    val rootNode = createParserNodeInfo(parserResult.json)
    preProcessParserNodeCache(parserResult.json)
    val ast = astForTranslationUnit(rootNode)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def astForTranslationUnit(rootNode: ParserNodeInfo): Ast = {
    val name     = s"$fullyQualifiedPackage.${parserResult.filename}"
    val fullName = s"$relPathFileName:$name"
    val fakeGlobalMethodForFile =
      methodNode(
        rootNode,
        name,
        name,
        fullName,
        None,
        relPathFileName,
        Option(NodeTypes.TYPE_DECL),
        Option(fullyQualifiedPackage)
      )
    methodAstParentStack.push(fakeGlobalMethodForFile)
    scope.pushNewScope(fakeGlobalMethodForFile)
    val blockNode_   = blockNode(rootNode, Defines.empty, Defines.anyTypeName)
    val methodReturn = methodReturnNode(rootNode, Defines.anyTypeName)
    val declsAsts = rootNode
      .json(ParserKeys.Decls)
      .arr
      .flatMap { item =>
        val node = createParserNodeInfo(item)
        astForNode(node, true)
      }
      .toList
    methodAstParentStack.pop()
    scope.popScope()
    methodAst(
      fakeGlobalMethodForFile,
      Seq.empty,
      blockAst(blockNode_, declsAsts),
      methodReturn,
      newModifierNode(ModifierTypes.MODULE) :: Nil
    )
  }

  protected def astForNode(nodeInfo: ParserNodeInfo, globalStatements: Boolean = false): Seq[Ast] = {
    nodeInfo.node match {
      case GenDecl          => astForGenDecl(nodeInfo, globalStatements)
      case FuncDecl         => astForFuncDecl(nodeInfo)
      case _: BasePrimitive => astForPrimitive(nodeInfo)
      case _: BaseExpr      => astsForExpression(nodeInfo)
      case _: BaseStmt      => astsForStatement(nodeInfo)
      case _                => Seq()
    }
  }

  protected def astForNode(json: Value): Seq[Ast] = {
    astForNode(createParserNodeInfo(json))
  }
}
