package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.parser.BabelNodeSyntax.*
import io.joern.jssrc2cpg.parser.BabelJsonParser.ParseResult
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.utils.OffsetUtils
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeRef
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable

class AstCreator(val config: Config, val usedTypes: mutable.HashSet[String], val parserResult: ParseResult)(implicit
  withSchemaValidation: ValidationMode
) extends AstCreatorBase[BabelNode, AstCreator](parserResult.filename)
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

  protected val scope = new VariableScopeManager()

  // TypeDecls with their bindings (with their refs) for lambdas and methods are not put in the AST
  // where the respective nodes are defined. Instead, we put them under the parent TYPE_DECL in which they are defined.
  // To achieve this we need this extra stack.
  protected val methodAstParentStack     = new Stack[NewNode]()
  protected val typeRefIdStack           = new Stack[NewTypeRef]
  protected val dynamicInstanceTypeStack = new Stack[String]
  protected val localAstParentStack      = new Stack[NewBlock]()
  protected val rootTypeDecl             = new Stack[NewTypeDecl]()
  // Keyed by the function node's source range (start:end): the typed nodes are case classes wrapping a ujson.Value and
  // would hash that structurally, recursing over the entire function subtree on every lookup.
  protected val functionNodeToNameAndFullName = mutable.HashMap.empty[String, (String, String)]
  protected val usedVariableNames             = mutable.HashMap.empty[String, Int]
  protected val seenAliasTypes                = mutable.HashMap.empty[String, NewTypeDecl]
  protected val functionFullNames             = mutable.HashSet.empty[String]

  /** The TYPE_DECL created for each function, keyed by the function's full name. Only used for lookups. */
  protected val functionTypeDecls = mutable.HashMap.empty[String, NewTypeDecl]

  override def createAst(): DiffGraphBuilder = {
    val fileContent = if (!config.disableFileContent) Option(parserResult.fileContent) else None
    val fileNode    = NewFile().name(parserResult.filename).order(0)
    fileContent.foreach(fileNode.content(_))
    val namespaceBlock = globalNamespaceBlock()
    methodAstParentStack.push(namespaceBlock)
    val ast = Ast(fileNode).withChild(Ast(namespaceBlock).withChild(createProgramMethod()))
    Ast.storeInDiffGraph(ast, diffGraph)
    scope.createVariableReferenceLinks(diffGraph, parserResult.filename)
    diffGraph
  }

  private def createProgramMethod(): Ast = {
    val path     = parserResult.filename
    val file     = parserResult.ast
    val name     = Defines.Program
    val fullName = s"$path:$name"

    val programMethod =
      methodNode(file, name, name, fullName, None, path, Some(NodeTypes.TYPE_DECL), Some(fullName)).order(1)

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(file, programMethod, methodAstParentStack.head, name, fullName, path)
    rootTypeDecl.push(functionTypeAndTypeDeclAst.nodes.head.asInstanceOf[NewTypeDecl])

    methodAstParentStack.push(programMethod)

    val blockNode = NewBlock().typeFullName(Defines.Any)

    scope.pushNewMethodScope(fullName, name, blockNode, None)
    localAstParentStack.push(blockNode)

    val thisParam = parameterInNode(file, "this", "this", 0, false, EvaluationStrategies.BY_VALUE)
      .dynamicTypeHintFullName(typeHintForThisExpression())
    scope.addVariable("this", thisParam, Defines.Any, VariableScopeManager.ScopeType.MethodScope)

    val methodChildren = createBlockStatementAsts(file.program.body)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    functionTypeAndTypeDeclAst.withChild(
      methodAst(
        programMethod,
        Ast(thisParam) :: Nil,
        blockAst(blockNode, methodChildren),
        methodReturnNode(file, Defines.Any),
        modifierNode(file, ModifierTypes.MODULE) :: Nil
      )
    )
  }

  protected def astForNodeWithFunctionReferenceAndCall(node: BabelNode): Ast = node match {
    case f: FunctionLike =>
      astForFunctionDeclaration(f, shouldCreateFunctionReference = true, shouldCreateAssignmentCall = true)
    case _ => astForNode(node)
  }

  protected def astForNode(node: BabelNode): Ast = node match {
    case n: (ClassDeclaration | DeclareClass)                       => astForClass(n, shouldCreateAssignmentCall = true)
    case n: ClassExpression                                         => astForClass(n)
    case n: TSInterfaceDeclaration                                  => astForInterface(n)
    case n: TSModuleDeclaration                                     => astForModule(n)
    case n: Decorator                                               => astForDecorator(n)
    case n: TSExportAssignment                                      => astForExportAssignment(n)
    case n: ExportNamedDeclaration                                  => astForExportNamedDeclaration(n)
    case n: ExportDefaultDeclaration                                => astForExportDefaultDeclaration(n)
    case n: ExportAllDeclaration                                    => astForExportAllDeclaration(n)
    case n: ImportDeclaration                                       => astForImportDeclaration(n)
    case n: FunctionLike                                            => astForFunctionDeclaration(n)
    case n: TSDeclareFunction                                       => astForTSDeclareFunction(n)
    case n: VariableDeclaration                                     => astForVariableDeclaration(n)
    case n: TSEnumDeclaration                                       => astForEnum(n)
    case n: (DeclareTypeAlias | TypeAlias | TSTypeAliasDeclaration) => astForTypeAlias(n)
    case n: (TypeCastExpression | TSTypeAssertion | TSAsExpression | TSTypeCastExpression) =>
      astForCastExpression(n)
    case n: NewExpression                                                => astForNewExpression(n)
    case n: ThisExpression                                               => astForThisExpression(n)
    case n: (MemberExpression | OptionalMemberExpression)                => astForMemberExpression(n)
    case n: MetaProperty                                                 => astForMetaProperty(n)
    case n: (CallExpression | OptionalCallExpression | ImportExpression) => astForCallExpression(n)
    case n: SequenceExpression                                           => astForSequenceExpression(n)
    case n: (AssignmentExpression | AssignmentPattern)                   => astForAssignmentExpression(n)
    case n: BinaryExpression                                             => astForBinaryExpression(n)
    case n: LogicalExpression                                            => astForLogicalExpression(n)
    case n: UpdateExpression                                             => astForUpdateExpression(n)
    case n: UnaryExpression                                              => astForUnaryExpression(n)
    case n: (ArrayExpression | ArrayPattern)                             => astForArrayExpression(n)
    case n: AwaitExpression                                              => astForAwaitExpression(n)
    case n: ConditionalExpression                                        => astForConditionalExpression(n)
    case n: TaggedTemplateExpression                                     => astForTemplateExpression(n)
    case n: (ObjectExpression | ObjectPattern)                           => astForObjectExpression(n)
    case n: TSNonNullExpression                                          => astForTSNonNullExpression(n)
    case n: (YieldExpression | ReturnStatement)                          => astForReturnStatement(n)
    case n: ExpressionStatement                                          => astForExpressionStatement(n)
    case n: IfStatement                                                  => astForIfStatement(n)
    case n: BlockStatement                                               => astForBlockStatement(n)
    case n: TryStatement                                                 => astForTryStatement(n)
    case n: ForStatement                                                 => astForForStatement(n)
    case n: WhileStatement                                               => astForWhileStatement(n)
    case n: DoWhileStatement                                             => astForDoWhileStatement(n)
    case n: SwitchStatement                                              => astForSwitchStatement(n)
    case n: BreakStatement                                               => astForBreakStatement(n)
    case n: ContinueStatement                                            => astForContinueStatement(n)
    case n: LabeledStatement                                             => astForLabeledStatement(n)
    case n: ThrowStatement                                               => astForThrowStatement(n)
    case n: (ForInStatement | ForOfStatement)                            => astForInOfStatement(n)
    case n: Identifier                                                   => astForIdentifier(n)
    case n: PrivateName                                                  => astForPrivateName(n)
    case n: Super                                                        => astForSuperKeyword(n)
    case n: Import                                                       => astForImportKeyword(n)
    case n: TSImportEqualsDeclaration                                    => astForTSImportEqualsDeclaration(n)
    case n: StringLiteral                                                => astForStringLiteral(n)
    case n: NumericLiteral                                               => astForNumericLiteral(n)
    case n: NullLiteral                                                  => astForNullLiteral(n)
    case n: BooleanLiteral                                               => astForBooleanLiteral(n)
    case n: RegExpLiteral                                                => astForRegExpLiteral(n)
    case n: BigIntLiteral                                                => astForBigIntLiteral(n)
    case n: TemplateLiteral                                              => astForTemplateLiteral(n)
    case n: TemplateElement                                              => astForTemplateElement(n)
    case n: SpreadElement                                                => astForSpreadOrRestElement(n)
    case n: TSSatisfiesExpression                                        => astForTSSatisfiesExpression(n)
    case n: JSXElement                                                   => astForJsxElement(n)
    case n: JSXOpeningElement                                            => astForJsxOpeningElement(n)
    case n: JSXClosingElement                                            => astForJsxClosingElement(n)
    case n: JSXText                                                      => astForJsxText(n)
    case n: (JSXExpressionContainer | JSXSpreadChild)                    => astForJsxExprContainer(n)
    case n: JSXSpreadAttribute                                           => astForJsxSpreadAttribute(n)
    case n: JSXFragment                                                  => astForJsxFragment(n)
    case n: JSXAttribute                                                 => astForJsxAttribute(n)
    case n: WithStatement                                                => astForWithStatement(n)
    case _: EmptyStatement                                               => Ast()
    case _: DebuggerStatement                                            => Ast()
    case _                                                               => notHandledYet(node)
  }

  protected def astForNodes(nodes: collection.Seq[BabelNode]): List[Ast] =
    nodes.iterator.map(astForNodeWithFunctionReference).toList

  protected def astForNodeWithFunctionReference(node: BabelNode): Ast = node match {
    case f: FunctionLike => astForFunctionDeclaration(f, shouldCreateFunctionReference = true)
    case _               => astForNode(node)
  }

  override protected def code(node: BabelNode): String  = node.code
  protected def line(node: BabelNode): Option[Int]      = node.lineNumber
  protected def column(node: BabelNode): Option[Int]    = node.columnNumber
  protected def lineEnd(node: BabelNode): Option[Int]   = node.lineNumberEnd
  protected def columnEnd(node: BabelNode): Option[Int] = node.columnNumberEnd

  override protected def offsetSource: Option[OffsetUtils.OffsetSourceType] =
    Option.unless(config.disableFileContent)(OffsetUtils.OffsetSourceType.Utf_16)

  override protected def unadjustedOffset(node: BabelNode): Option[(Int, Int)] = {
    for {
      startOffset <- node.start
      endOffset   <- node.end
    } yield clampOffsets(startOffset, endOffset)
  }
}
