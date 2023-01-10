package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.datastructures.Scope
import io.joern.jssrc2cpg.parser.BabelJsonParser.ParseResult
import io.joern.jssrc2cpg.parser.BabelAst._
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.datastructures.Stack.{Stack, _}
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodReturn
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
  protected val typeRefIdStack                = new Stack[NewTypeRef]
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

    val blockNode = NewBlock().typeFullName(Defines.ANY)

    scope.pushNewMethodScope(fullName, name, blockNode, None)
    localAstParentStack.push(blockNode)

    val thisParam =
      createParameterInNode("this", "this", 0, isVariadic = false, line = lineNumber, column = columnNumber)

    val methodChildren = astsForFile(astNodeInfo)
    setArgIndices(methodChildren)

    val methodReturn = NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName(Defines.ANY)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    methodAst(programMethod, List(thisParam), Ast(blockNode).withChildren(methodChildren), methodReturn)
  }

  protected def astForNode(json: Value): Ast = {
    val nodeInfo = createBabelNodeInfo(json)
    nodeInfo.node match {
      case ClassDeclaration          => astForClass(nodeInfo, shouldCreateAssignmentCall = true)
      case DeclareClass              => astForClass(nodeInfo, shouldCreateAssignmentCall = true)
      case ClassExpression           => astForClass(nodeInfo)
      case TSInterfaceDeclaration    => astForInterface(nodeInfo)
      case TSModuleDeclaration       => astForModule(nodeInfo)
      case TSExportAssignment        => astForExportAssignment(nodeInfo)
      case ExportNamedDeclaration    => astForExportNamedDeclaration(nodeInfo)
      case ExportDefaultDeclaration  => astForExportDefaultDeclaration(nodeInfo)
      case ExportAllDeclaration      => astForExportAllDeclaration(nodeInfo)
      case ImportDeclaration         => astForImportDeclaration(nodeInfo)
      case FunctionDeclaration       => astForFunctionDeclaration(nodeInfo)
      case TSDeclareFunction         => astForTSDeclareFunction(nodeInfo)
      case VariableDeclaration       => astForVariableDeclaration(nodeInfo)
      case ArrowFunctionExpression   => astForFunctionDeclaration(nodeInfo)
      case FunctionExpression        => astForFunctionDeclaration(nodeInfo)
      case TSEnumDeclaration         => astForEnum(nodeInfo)
      case DeclareTypeAlias          => astForTypeAlias(nodeInfo)
      case TypeAlias                 => astForTypeAlias(nodeInfo)
      case TSTypeAliasDeclaration    => astForTypeAlias(nodeInfo)
      case NewExpression             => astForNewExpression(nodeInfo)
      case ThisExpression            => astForThisExpression(nodeInfo)
      case MemberExpression          => astForMemberExpression(nodeInfo)
      case OptionalMemberExpression  => astForMemberExpression(nodeInfo)
      case MetaProperty              => astForMetaProperty(nodeInfo)
      case CallExpression            => astForCallExpression(nodeInfo)
      case OptionalCallExpression    => astForCallExpression(nodeInfo)
      case SequenceExpression        => astForSequenceExpression(nodeInfo)
      case AssignmentExpression      => astForAssignmentExpression(nodeInfo)
      case AssignmentPattern         => astForAssignmentExpression(nodeInfo)
      case BinaryExpression          => astForBinaryExpression(nodeInfo)
      case LogicalExpression         => astForLogicalExpression(nodeInfo)
      case TSAsExpression            => astForCastExpression(nodeInfo)
      case UpdateExpression          => astForUpdateExpression(nodeInfo)
      case UnaryExpression           => astForUnaryExpression(nodeInfo)
      case ArrayExpression           => astForArrayExpression(nodeInfo)
      case AwaitExpression           => astForAwaitExpression(nodeInfo)
      case ConditionalExpression     => astForConditionalExpression(nodeInfo)
      case TaggedTemplateExpression  => astForTemplateExpression(nodeInfo)
      case ObjectExpression          => astForObjectExpression(nodeInfo)
      case TSNonNullExpression       => astForTSNonNullExpression(nodeInfo)
      case YieldExpression           => astForReturnStatement(nodeInfo)
      case ExpressionStatement       => astForExpressionStatement(nodeInfo)
      case IfStatement               => astForIfStatement(nodeInfo)
      case BlockStatement            => astForBlockStatement(nodeInfo)
      case ReturnStatement           => astForReturnStatement(nodeInfo)
      case TryStatement              => astForTryStatement(nodeInfo)
      case ForStatement              => astForForStatement(nodeInfo)
      case WhileStatement            => astForWhileStatement(nodeInfo)
      case DoWhileStatement          => astForDoWhileStatement(nodeInfo)
      case SwitchStatement           => astForSwitchStatement(nodeInfo)
      case BreakStatement            => astForBreakStatement(nodeInfo)
      case ContinueStatement         => astForContinueStatement(nodeInfo)
      case LabeledStatement          => astForLabeledStatement(nodeInfo)
      case ThrowStatement            => astForThrowStatement(nodeInfo)
      case ForInStatement            => astForInOfStatement(nodeInfo)
      case ForOfStatement            => astForInOfStatement(nodeInfo)
      case ObjectPattern             => astForObjectExpression(nodeInfo)
      case ArrayPattern              => astForArrayExpression(nodeInfo)
      case Identifier                => astForIdentifier(nodeInfo)
      case PrivateName               => astForPrivateName(nodeInfo)
      case Super                     => astForSuperKeyword(nodeInfo)
      case Import                    => astForImportKeyword(nodeInfo)
      case TSImportEqualsDeclaration => astForTSImportEqualsDeclaration(nodeInfo)
      case StringLiteral             => astForStringLiteral(nodeInfo)
      case NumericLiteral            => astForNumericLiteral(nodeInfo)
      case NumberLiteral             => astForNumberLiteral(nodeInfo)
      case DecimalLiteral            => astForDecimalLiteral(nodeInfo)
      case NullLiteral               => astForNullLiteral(nodeInfo)
      case BooleanLiteral            => astForBooleanLiteral(nodeInfo)
      case RegExpLiteral             => astForRegExpLiteral(nodeInfo)
      case RegexLiteral              => astForRegexLiteral(nodeInfo)
      case BigIntLiteral             => astForBigIntLiteral(nodeInfo)
      case TemplateLiteral           => astForTemplateLiteral(nodeInfo)
      case TemplateElement           => astForTemplateElement(nodeInfo)
      case SpreadElement             => astForSpreadOrRestElement(nodeInfo)
      case JSXElement                => astForJsxElement(nodeInfo)
      case JSXOpeningElement         => astForJsxOpeningElement(nodeInfo)
      case JSXClosingElement         => astForJsxClosingElement(nodeInfo)
      case JSXText                   => astForJsxText(nodeInfo)
      case JSXExpressionContainer    => astForJsxExprContainer(nodeInfo)
      case JSXSpreadChild            => astForJsxExprContainer(nodeInfo)
      case JSXSpreadAttribute        => astForJsxSpreadAttribute(nodeInfo)
      case JSXFragment               => astForJsxFragment(nodeInfo)
      case JSXAttribute              => astForJsxAttribute(nodeInfo)
      case EmptyStatement            => Ast()
      case DebuggerStatement         => Ast()
      case _                         => notHandledYet(nodeInfo)
    }
  }

  protected def astForNodeWithFunctionReference(json: Value): Ast = {
    val nodeInfo = createBabelNodeInfo(json)
    nodeInfo.node match {
      case _: FunctionLike => astForFunctionDeclaration(nodeInfo, shouldCreateFunctionReference = true)
      case _               => astForNode(json)
    }
  }

  protected def astForNodeWithFunctionReferenceAndCall(json: Value): Ast = {
    val nodeInfo = createBabelNodeInfo(json)
    nodeInfo.node match {
      case _: FunctionLike =>
        astForFunctionDeclaration(nodeInfo, shouldCreateFunctionReference = true, shouldCreateAssignmentCall = true)
      case _ => astForNode(json)
    }
  }

  protected def astForNodes(jsons: List[Value]): List[Ast] = jsons.map(astForNodeWithFunctionReference)

  private def astsForFile(file: BabelNodeInfo): List[Ast] = astsForProgram(createBabelNodeInfo(file.json("program")))

  private def astsForProgram(program: BabelNodeInfo): List[Ast] = createBlockStatementAsts(program.json("body"))

}
