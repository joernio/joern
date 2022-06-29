package io.joern.jssrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelJsonParser.ParseResult
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.datastructures.Scope
import io.joern.jssrc2cpg.passes.Defines
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.datastructures.Stack.Stack
import org.slf4j.{Logger, LoggerFactory}
import ujson.Value

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

class AstCreator(
  val config: Config,
  val parserResult: ParseResult,
  val usedTypes: ConcurrentHashMap[(String, String), Boolean]
) extends AstCreatorBase(parserResult.filename)
    with AstForExpressionsCreator
    with AstForPrimitivesCreator
    with AstForTypesCreator
    with AstForFunctionsCreator
    with AstForDeclarationsCreator
    with AstForStatementsCreator
    with AstForTemplateDomCreator
    with AstNodeBuilder
    with TypeHelper
    with AstCreatorHelper {

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  protected val scope = new Scope()

  // TypeDecls with their bindings (with their refs) for lambdas and methods are not put in the AST
  // where the respective nodes are defined. Instead we put them under the parent TYPE_DECL in which they are defined.
  // To achieve this we need this extra stack.
  protected val methodAstParentStack          = new Stack[NewNode]()
  protected val metaTypeRefIdStack            = new Stack[NewTypeRef]
  protected val dynamicInstanceTypeStack      = new Stack[String]
  protected val localAstParentStack           = new Stack[NewBlock]()
  protected val rootTypeDecl                  = new Stack[NewTypeDecl]()
  protected val typeFullNameToPostfix         = mutable.HashMap.empty[String, Int]
  protected val functionNodeToNameAndFullName = mutable.HashMap.empty[BabelNodeInfo, (String, String)]
  protected val usedVariableNames             = mutable.HashMap.empty[String, Int]
  protected val seenAliasTypes                = mutable.HashSet.empty[NewTypeDecl]
  protected val functionFullNames             = mutable.HashSet.empty[String]

  // we track line and column numbers manually because astgen / @babel-parser sometimes
  // fails to deliver them at all -  strange, but this even happens with its latest version
  protected val (positionToLineNumberMapping, positionToFirstPositionInLineMapping) =
    positionLookupTables(parserResult.fileContent)

  // we want to keep it local, just like the old js2cpg did
  override def absolutePath(filename: String): String = filename

  override def createAst(): DiffGraphBuilder = {
    val fileNode       = NewFile().name(parserResult.filename).order(1)
    val namespaceBlock = globalNamespaceBlock()
    methodAstParentStack.push(namespaceBlock)
    val ast = Ast(fileNode).withChild(Ast(namespaceBlock).withChild(createProgramMethod()))
    Ast.storeInDiffGraph(ast, diffGraph)
    createVariableReferenceLinks()
    diffGraph
  }

  private def createProgramMethod(): Ast = {
    val path            = parserResult.filename
    val astNodeInfo     = createBabelNodeInfo(parserResult.json("ast"))
    val lineNumber      = astNodeInfo.lineNumber
    val columnNumber    = astNodeInfo.columnNumber
    val lineNumberEnd   = astNodeInfo.lineNumberEnd
    val columnNumberEnd = astNodeInfo.columnNumberEnd
    val name            = ":program"
    val fullName        = path + ":" + name

    val programMethod =
      NewMethod()
        .name(name)
        .code(name)
        .fullName(fullName)
        .filename(path)
        .lineNumber(lineNumber)
        .lineNumberEnd(lineNumberEnd)
        .columnNumber(columnNumber)
        .columnNumberEnd(columnNumberEnd)
        .astParentType(NodeTypes.TYPE_DECL)
        .astParentFullName(fullName)

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(programMethod, methodAstParentStack.head, name, fullName, path)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    rootTypeDecl.push(functionTypeAndTypeDeclAst.nodes.head.asInstanceOf[NewTypeDecl])

    methodAstParentStack.push(programMethod)

    val blockNode = NewBlock().typeFullName("ANY")

    scope.pushNewMethodScope(fullName, name, blockNode, None)
    localAstParentStack.push(blockNode)

    val thisParam =
      createParameterInNode("this", "this", 0, isVariadic = false, line = lineNumber, column = columnNumber)

    val methodChildren = astsForFile(astNodeInfo)
    setIndices(methodChildren)

    val methodReturn = NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName(Defines.ANY.label)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    methodAst(programMethod, List(thisParam), Ast(blockNode).withChildren(methodChildren), methodReturn)
  }

  protected def astForNode(json: Value): Ast = {
    val nodeInfo = createBabelNodeInfo(json)
    nodeInfo.node match {
      case BabelAst.ClassDeclaration          => astForClass(nodeInfo)
      case BabelAst.DeclareClass              => astForClass(nodeInfo)
      case BabelAst.ClassExpression           => astForClass(nodeInfo)
      case BabelAst.TSInterfaceDeclaration    => astForInterface(nodeInfo)
      case BabelAst.TSModuleDeclaration       => astForModule(nodeInfo)
      case BabelAst.TSExportAssignment        => astForExportAssignment(nodeInfo)
      case BabelAst.ExportNamedDeclaration    => astForExportNamedDeclaration(nodeInfo)
      case BabelAst.ExportDefaultDeclaration  => astForExportDefaultDeclaration(nodeInfo)
      case BabelAst.ExportAllDeclaration      => astForExportAllDeclaration(nodeInfo)
      case BabelAst.ImportDeclaration         => astForImportDeclaration(nodeInfo)
      case BabelAst.FunctionDeclaration       => astForFunctionDeclaration(nodeInfo)
      case BabelAst.TSDeclareFunction         => astForTSDeclareFunction(nodeInfo)
      case BabelAst.VariableDeclaration       => astForVariableDeclaration(nodeInfo)
      case BabelAst.ArrowFunctionExpression   => astForFunctionDeclaration(nodeInfo)
      case BabelAst.FunctionExpression        => astForFunctionDeclaration(nodeInfo)
      case BabelAst.TSEnumDeclaration         => astForEnum(nodeInfo)
      case BabelAst.DeclareTypeAlias          => astForTypeAlias(nodeInfo)
      case BabelAst.TypeAlias                 => astForTypeAlias(nodeInfo)
      case BabelAst.TSTypeAliasDeclaration    => astForTypeAlias(nodeInfo)
      case BabelAst.NewExpression             => astForNewExpression(nodeInfo)
      case BabelAst.ThisExpression            => astForThisExpression(nodeInfo)
      case BabelAst.MemberExpression          => astForMemberExpression(nodeInfo)
      case BabelAst.OptionalMemberExpression  => astForMemberExpression(nodeInfo)
      case BabelAst.CallExpression            => astForCallExpression(nodeInfo)
      case BabelAst.OptionalCallExpression    => astForCallExpression(nodeInfo)
      case BabelAst.SequenceExpression        => astForSequenceExpression(nodeInfo)
      case BabelAst.AssignmentExpression      => astForAssignmentExpression(nodeInfo)
      case BabelAst.BinaryExpression          => astForBinaryExpression(nodeInfo)
      case BabelAst.LogicalExpression         => astForLogicalExpression(nodeInfo)
      case BabelAst.TSAsExpression            => astForCastExpression(nodeInfo)
      case BabelAst.UpdateExpression          => astForUpdateExpression(nodeInfo)
      case BabelAst.UnaryExpression           => astForUnaryExpression(nodeInfo)
      case BabelAst.ArrayExpression           => astForArrayExpression(nodeInfo)
      case BabelAst.AwaitExpression           => astForAwaitExpression(nodeInfo)
      case BabelAst.ConditionalExpression     => astForConditionalExpression(nodeInfo)
      case BabelAst.TaggedTemplateExpression  => astForTemplateExpression(nodeInfo)
      case BabelAst.ObjectExpression          => astForObjectExpression(nodeInfo)
      case BabelAst.YieldExpression           => astForReturnStatement(nodeInfo)
      case BabelAst.ExpressionStatement       => astForExpressionStatement(nodeInfo)
      case BabelAst.IfStatement               => astForIfStatement(nodeInfo)
      case BabelAst.BlockStatement            => astForBlockStatement(nodeInfo)
      case BabelAst.ReturnStatement           => astForReturnStatement(nodeInfo)
      case BabelAst.TryStatement              => astForTryStatement(nodeInfo)
      case BabelAst.ForStatement              => astForForStatement(nodeInfo)
      case BabelAst.WhileStatement            => astForWhileStatement(nodeInfo)
      case BabelAst.DoWhileStatement          => astForDoWhileStatement(nodeInfo)
      case BabelAst.SwitchStatement           => astForSwitchStatement(nodeInfo)
      case BabelAst.BreakStatement            => astForBreakStatement(nodeInfo)
      case BabelAst.ContinueStatement         => astForContinueStatement(nodeInfo)
      case BabelAst.ThrowStatement            => astForThrowStatement(nodeInfo)
      case BabelAst.ForInStatement            => astForInOfStatement(nodeInfo)
      case BabelAst.ForOfStatement            => astForInOfStatement(nodeInfo)
      case BabelAst.ObjectPattern             => astForObjectExpression(nodeInfo)
      case BabelAst.ArrayPattern              => astForArrayExpression(nodeInfo)
      case BabelAst.Identifier                => astForIdentifier(nodeInfo)
      case BabelAst.Super                     => astForSuperKeyword(nodeInfo)
      case BabelAst.Import                    => astForImportKeyword(nodeInfo)
      case BabelAst.TSImportEqualsDeclaration => astForTSImportEqualsDeclaration(nodeInfo)
      case BabelAst.StringLiteral             => astForStringLiteral(nodeInfo)
      case BabelAst.NumericLiteral            => astForNumericLiteral(nodeInfo)
      case BabelAst.NumberLiteral             => astForNumberLiteral(nodeInfo)
      case BabelAst.DecimalLiteral            => astForDecimalLiteral(nodeInfo)
      case BabelAst.NullLiteral               => astForNullLiteral(nodeInfo)
      case BabelAst.BooleanLiteral            => astForBooleanLiteral(nodeInfo)
      case BabelAst.RegExpLiteral             => astForRegExpLiteral(nodeInfo)
      case BabelAst.RegexLiteral              => astForRegexLiteral(nodeInfo)
      case BabelAst.BigIntLiteral             => astForBigIntLiteral(nodeInfo)
      case BabelAst.TemplateLiteral           => astForTemplateLiteral(nodeInfo)
      case BabelAst.TemplateElement           => astForTemplateElement(nodeInfo)
      case BabelAst.JSXElement                => astForJsxElement(nodeInfo)
      case BabelAst.JSXOpeningElement         => astForJsxOpeningElement(nodeInfo)
      case BabelAst.JSXClosingElement         => astForJsxClosingElement(nodeInfo)
      case BabelAst.JSXText                   => astForJsxText(nodeInfo)
      case BabelAst.JSXExpressionContainer    => astForJsxExprContainer(nodeInfo)
      case BabelAst.JSXSpreadChild            => astForJsxExprContainer(nodeInfo)
      case BabelAst.JSXSpreadAttribute        => astForJsxSpreadAttribute(nodeInfo)
      case BabelAst.JSXFragment               => astForJsxFragment(nodeInfo)
      case BabelAst.JSXAttribute              => astForJsxAttribute(nodeInfo)
      case BabelAst.EmptyStatement            => Ast()
      case _                                  => notHandledYet(nodeInfo)
    }
  }

  protected def astForNodeWithFunctionReference(json: Value): Ast = {
    val nodeInfo = createBabelNodeInfo(json)
    nodeInfo.node match {
      case BabelAst.FunctionDeclaration =>
        astForFunctionDeclaration(nodeInfo, shouldCreateFunctionReference = true)
      case BabelAst.FunctionExpression =>
        astForFunctionDeclaration(nodeInfo, shouldCreateFunctionReference = true)
      case BabelAst.ArrowFunctionExpression =>
        astForFunctionDeclaration(nodeInfo, shouldCreateFunctionReference = true)
      case _ => astForNode(json)
    }
  }

  protected def astForNodes(jsons: List[Value]): List[Ast] = jsons.map(astForNodeWithFunctionReference)

  private def astsForFile(file: BabelNodeInfo): List[Ast] = astsForProgram(createBabelNodeInfo(file.json("program")))

  private def astsForProgram(program: BabelNodeInfo): List[Ast] = createBlockStatementAsts(program.json("body"))

}
