package io.joern.jssrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelJsonParser.ParseResult
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.datastructures.scope.Scope
import io.joern.jssrc2cpg.passes.Defines
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.datastructures.Stack.Stack
import org.slf4j.{Logger, LoggerFactory}
import ujson.Value

import scala.collection.mutable

class AstCreator(val config: Config, val parserResult: ParseResult, val global: Global)
    extends AstCreatorBase(parserResult.filename)
    with AstForExpressionsCreator
    with AstForPrimitivesCreator
    with AstForTypesCreator
    with AstForFunctionsCreator
    with AstForDeclarationsCreator
    with AstForStatementsCreator
    with AstNodeBuilder
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
  protected val typeFullNameToPostfix         = mutable.HashMap.empty[String, Int]
  protected val typeToNameAndFullName         = mutable.HashMap.empty[BabelNodeInfo, (String, String)]
  protected val functionNodeToNameAndFullName = mutable.HashMap.empty[BabelNodeInfo, (String, String)]
  protected val functionFullNames             = mutable.HashSet.empty[String]
  protected val usedVariableNames             = mutable.HashMap.empty[String, Int]

  // we want to keep it local, just like the old js2cpg did
  override def absolutePath(filename: String): String = filename

  override def createAst(): DiffGraphBuilder = {
    val namespaceBlock = globalNamespaceBlock()
    methodAstParentStack.push(namespaceBlock)
    val ast = Ast(namespaceBlock).withChild(createProgramMethod())
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

    methodAstParentStack.push(programMethod)

    val blockNode = NewBlock().typeFullName("ANY")

    scope.pushNewMethodScope(fullName, name, blockNode, None)
    localAstParentStack.push(blockNode)

    val thisParam = createParameterInNode("this", "this", 0, isVariadic = false, lineNumber, columnNumber)

    val methodChildren = astsForFile(astNodeInfo)

    val methodReturn = NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName(Defines.ANY.label)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDecl(programMethod, methodAstParentStack.head, name, fullName, path)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)

    methodAst(programMethod, List(thisParam), Ast(blockNode).withChildren(methodChildren), methodReturn)
  }

  protected def astForNode(json: Value): Ast = createBabelNodeInfo(json) match {
    case exprStmt @ BabelNodeInfo(BabelAst.ExpressionStatement)    => astForExpressionStatement(exprStmt)
    case callExpr @ BabelNodeInfo(BabelAst.CallExpression)         => astForCallExpression(callExpr)
    case memberExpr @ BabelNodeInfo(BabelAst.MemberExpression)     => astForMemberExpression(memberExpr)
    case ident @ BabelNodeInfo(BabelAst.Identifier)                => astForIdentifier(ident)
    case stringLiteral @ BabelNodeInfo(BabelAst.StringLiteral)     => astForStringLiteral(stringLiteral)
    case numLiteral @ BabelNodeInfo(BabelAst.NumericLiteral)       => astForNumericLiteral(numLiteral)
    case func @ BabelNodeInfo(BabelAst.FunctionDeclaration)        => astForFunctionDeclaration(func)
    case decl @ BabelNodeInfo(BabelAst.VariableDeclaration)        => astForVariableDeclaration(decl)
    case assignment @ BabelNodeInfo(BabelAst.AssignmentExpression) => astForAssignmentExpression(assignment)
    case binExpr @ BabelNodeInfo(BabelAst.BinaryExpression)        => astForBinaryExpression(binExpr)
    case updateExpr @ BabelNodeInfo(BabelAst.UpdateExpression)     => astForUpdateExpression(updateExpr)
    case unaryExpr @ BabelNodeInfo(BabelAst.UnaryExpression)       => astForUnaryExpression(unaryExpr)
    case block @ BabelNodeInfo(BabelAst.BlockStatement)            => astForBlockStatement(block)
    case ret @ BabelNodeInfo(BabelAst.ReturnStatement)             => astForReturnStatement(ret)
    case seq @ BabelNodeInfo(BabelAst.SequenceExpression)          => astForSequenceExpression(seq)
    case classExpr @ BabelNodeInfo(BabelAst.ClassExpression)       => astForClass(classExpr)
    case classDecl @ BabelNodeInfo(BabelAst.ClassDeclaration)      => astForClass(classDecl)
    case arrExpr @ BabelNodeInfo(BabelAst.ArrayExpression)         => astForArrayExpression(arrExpr)
    case other                                                     => notHandledYet(other)

  }
  protected def astForNodes(jsons: List[Value]): List[Ast] = jsons.map(astForNode)

  private def astsForFile(file: BabelNodeInfo): List[Ast] = astsForProgram(createBabelNodeInfo(file.json("program")))

  private def astsForProgram(program: BabelNodeInfo): List[Ast] = createBlockStatementAsts(program.json("body"))

}
