package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{GoAstJsonParser, ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.astgen.{AstGenNodeBuilder, BaseNodeInfo}
import io.joern.x2cpg.datastructures.Scope
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewClosureBinding, NewLocal, NewMethod, NewMethodRef, NewNode}
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, ModifierTypes, NodeTypes}
import org.slf4j.{Logger, LoggerFactory}
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
import ujson.Value

import java.nio.file.Paths
import scala.collection.mutable

class AstCreator(
  val jsonAstFilePath: String,
  val relPathFileName: String,
  val goMod: GoModHelper,
  val goGlobal: GoGlobal
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase[BaseNodeInfo[?], AstCreator](relPathFileName)
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
    with DependencySrcProcessor
    with AstGenNodeBuilder[AstCreator] {

  protected val logger: Logger                                  = LoggerFactory.getLogger(classOf[AstCreator])
  val parserResult                                              = GoAstJsonParser.readFile(Paths.get(jsonAstFilePath))
  protected val methodAstParentStack: Stack[NewNode]            = new Stack()
  protected val scope: GoScope                                  = new GoScope()
  protected val lambdaCaptureStack: Stack[LambdaCaptureContext] = new Stack()
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
      modifierNode(rootNode, ModifierTypes.MODULE) :: Nil
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

  protected class GoScope extends Scope[String, (NewNode, String), NewNode] {
    def lookupVariableWithDeclaringMethod(identifier: String): Option[((NewNode, String), Option[NewNode])] = {
      stack.zipWithIndex.collectFirst {
        case (scopeElement, index) if scopeElement.variables.contains(identifier) =>
          val declaringMethod = stack.drop(index).collectFirst {
            case scopeElement if scopeElement.scopeNode.isInstanceOf[NewMethod] =>
              scopeElement.scopeNode
          }
          (scopeElement.variables(identifier), declaringMethod)
      }
    }

    def addToScopeNode(scopeNode: NewNode, identifier: String, variable: (NewNode, String)): Unit = {
      stack = stack.map { scopeElement =>
        if (sameNode(scopeElement.scopeNode, scopeNode)) {
          scopeElement.addVariable(identifier, variable)
        } else {
          scopeElement
        }
      }
    }
  }

  protected def targetForIdentifierReference(
    identifierName: String,
    variable: NewNode,
    variableTypeName: String,
    declaringMethod: Option[NewNode],
    ident: ParserNodeInfo
  ): NewNode = {
    lambdaCaptureStack.headOption match {
      case Some(currentLambda)
          if declaringMethod.exists(methodNode => !sameNode(methodNode, currentLambda.methodNode)) =>
        captureTargetFor(identifierName, variable, variableTypeName, declaringMethod, ident)
      case _ => variable
    }
  }

  private def captureTargetFor(
    identifierName: String,
    variable: NewNode,
    variableTypeName: String,
    declaringMethod: Option[NewNode],
    ident: ParserNodeInfo
  ): NewNode = {
    val contextsToCapture = lambdaContextsBetweenCurrentAnd(declaringMethod)
    contextsToCapture.foldLeft(variable) { case (target, context) =>
      val captureKey = (identifierName, System.identityHashCode(target))
      val captured = context.capturedVariables.getOrElseUpdate(
        captureKey,
        createCapturedVariable(context, identifierName, variableTypeName, target, ident)
      )
      captured.local
    }
  }

  private def lambdaContextsBetweenCurrentAnd(declaringMethod: Option[NewNode]): Seq[LambdaCaptureContext] = {
    val outermostToInnermost = lambdaCaptureStack.toSeq.reverse
    declaringMethod match {
      case Some(methodNode) =>
        val fromDeclaring = outermostToInnermost.dropWhile(context => !sameNode(context.methodNode, methodNode))
        if (fromDeclaring.nonEmpty) fromDeclaring.tail else outermostToInnermost
      case None => outermostToInnermost
    }
  }

  private def createCapturedVariable(
    context: LambdaCaptureContext,
    identifierName: String,
    variableTypeName: String,
    capturedTarget: NewNode,
    ident: ParserNodeInfo
  ): CapturedVariable = {
    val closureBindingId = s"${context.fullName}:$identifierName"
    val local            = localNode(ident, identifierName, identifierName, variableTypeName, Some(closureBindingId))
    val closureBinding =
      NewClosureBinding().closureBindingId(closureBindingId).evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
    scope.addToScopeNode(context.methodNode, identifierName, (local, variableTypeName))
    CapturedVariable(
      local,
      Ast(closureBinding)
        .withCaptureEdge(context.methodRef, closureBinding)
        .withRefEdge(closureBinding, capturedTarget)
    )
  }

  protected def sameNode(left: NewNode, right: NewNode): Boolean = {
    left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]
  }
}

protected[astcreation] case class CapturedVariable(local: NewLocal, bindingAst: Ast)

protected[astcreation] case class LambdaCaptureContext(
  methodNode: NewMethod,
  methodRef: NewMethodRef,
  fullName: String,
  capturedVariables: mutable.LinkedHashMap[(String, Int), CapturedVariable] = mutable.LinkedHashMap.empty
)
