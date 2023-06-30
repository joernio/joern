package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.parser.GoAstJsonParser.ParserResult
import io.joern.gosrc2cpg.parser.ParserAst._
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.datastructures.Scope
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder => X2CpgAstNodeBuilder}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewNode}
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import ujson.Value

class AstCreator(val config: Config, val parserResult: ParserResult)
    extends AstCreatorBase(parserResult.filename)
    with AstForDeclarationCreator
    with AstForPrimitivesCreator
    with AstForFunctionsCreator
    with AstForStatementsCreator
    with AstCreatorHelper
    with X2CpgAstNodeBuilder[ParserNodeInfo, AstCreator] {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope: Scope[String, (NewNode, String), NewNode] = new Scope()

  override def createAst(): DiffGraphBuilder = {

    val fileNode = NewFile().name(parserResult.filename).order(1)

    val astForDeclaration = parserResult.json(ParserKeys.Decls).arr.map(item => astForNode(item))
    val ast               = Ast(fileNode).withChildren(astForDeclaration)
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  protected def astForNode(json: Value): Ast = {
    val nodeInfo = createParserNodeInfo(json)
    val output = nodeInfo.node match {
      case GenDecl if isImportDeclaration(nodeInfo) => astForImport(nodeInfo)
      case BasicLit                                 => astForLiteral(nodeInfo)
      case Ident                                    => astForIdentifier(nodeInfo)
      case FuncDecl                                 => astForFuncDecl(nodeInfo)
      case _                                        => Ast()
    }
    output
  }
  override protected def line(node: ParserNodeInfo): Option[Integer] = node.lineNumber

  override protected def column(node: ParserNodeInfo): Option[Integer] = node.columnNumber

  override protected def lineEnd(node: ParserNodeInfo): Option[Integer] = node.lineNumberEnd

  override protected def columnEnd(node: ParserNodeInfo): Option[Integer] = node.columnNumberEnd
}
