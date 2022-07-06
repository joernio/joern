package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.KtFileWithMeta
import io.joern.kotlin2cpg.ast.KtPsiToAst
import io.joern.kotlin2cpg.types.{TypeConstants, TypeInfoProvider}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.passes.IntervalKeyPool
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Global

import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.lexer.{KtToken, KtTokens}
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.annotation.tailrec
import scala.collection.mutable

case class BindingInfo(node: NewBinding, edgeMeta: Seq[(NewNode, NewNode, String)])
case class ClosureBindingDef(node: NewClosureBinding, captureEdgeTo: NewMethodRef, refEdgeTo: NewNode)

class AstCreator(fileWithMeta: KtFileWithMeta, xTypeInfoProvider: TypeInfoProvider, global: Global)
    extends AstCreatorBase(fileWithMeta.filename)
    with KtPsiToAst {

  protected val closureBindingDefQueue: mutable.ArrayBuffer[ClosureBindingDef] = mutable.ArrayBuffer.empty
  protected val bindingInfoQueue: mutable.ArrayBuffer[BindingInfo]             = mutable.ArrayBuffer.empty
  protected val lambdaAstQueue: mutable.ArrayBuffer[Ast]                       = mutable.ArrayBuffer.empty
  protected val lambdaBindingInfoQueue: mutable.ArrayBuffer[BindingInfo]       = mutable.ArrayBuffer.empty

  protected val lambdaKeyPool   = new IntervalKeyPool(first = 1, last = Long.MaxValue)
  protected val tmpKeyPool      = new IntervalKeyPool(first = 1, last = Long.MaxValue)
  protected val iteratorKeyPool = new IntervalKeyPool(first = 1, last = Long.MaxValue)

  protected val relativizedPath = fileWithMeta.relativizedPath

  protected val scope: Scope[String, DeclarationNew, NewNode] = new Scope()

  def createAst(): DiffGraphBuilder = {
    implicit val typeInfoProvider: TypeInfoProvider = xTypeInfoProvider
    logger.debug(s"Started parsing file `${fileWithMeta.filename}`.")

    val defaultTypes = Set(TypeConstants.javaLangObject, TypeConstants.kotlin)
    defaultTypes.foreach(registerType)
    storeInDiffGraph(astForFile(fileWithMeta))
    diffGraph
  }

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  /** Add `typeName` to a global map and return it. The map is later passed to a pass that creates TYPE nodes for each
    * key in the map.
    */
  protected def registerType(typeName: String): String = {
    global.usedTypes.putIfAbsent(typeName, true)
    typeName
  }

  protected def getName(node: NewImport) = {
    val isWildcard = node.isWildcard.getOrElse(false: java.lang.Boolean)
    if (isWildcard) Constants.wildcardImportName
    else node.importedEntity.getOrElse("")
  }

  protected def storeInDiffGraph(ast: Ast): Unit = {
    Ast.storeInDiffGraph(ast, diffGraph)

    for {
      bindingInfo <- bindingInfoQueue ++ lambdaBindingInfoQueue
      _ = diffGraph.addNode(bindingInfo.node)
      (src, dst, label) <- bindingInfo.edgeMeta
    } diffGraph.addEdge(src, dst, label)

    closureBindingDefQueue.foreach { case ClosureBindingDef(node, captureEdgeTo, refEdgeTo) =>
      diffGraph.addNode(node)
      diffGraph.addEdge(captureEdgeTo, node, EdgeTypes.CAPTURE)
      diffGraph.addEdge(node, refEdgeTo, EdgeTypes.REF)
    }
  }

  protected def ktTokenToOperator(forPostfixExpr: Boolean): PartialFunction[KtToken, String] = {
    val postfixKtTokenToOperator: PartialFunction[KtToken, String] = {
      case KtTokens.EXCLEXCL   => Operators.notNullAssert
      case KtTokens.MINUSMINUS => Operators.postDecrement
      case KtTokens.PLUSPLUS   => Operators.postIncrement
    }
    val prefixKtTokenToOperator: PartialFunction[KtToken, String] = {
      case KtTokens.EXCL       => Operators.logicalNot
      case KtTokens.MINUSMINUS => Operators.preDecrement
      case KtTokens.MINUS      => Operators.minus
      case KtTokens.PLUS       => Operators.plus
      case KtTokens.PLUSPLUS   => Operators.preIncrement
    }
    if (forPostfixExpr) postfixKtTokenToOperator
    else prefixKtTokenToOperator
  }

  protected def astWithRefEdgeMaybe(lookupName: String, srcNode: NewNode): Ast = {
    scope.lookupVariable(lookupName) match {
      case Some(n) => Ast(srcNode).withRefEdge(srcNode, n)
      case None    => Ast(srcNode)
    }
  }

  @tailrec
  final def astsForExpression(expr: KtExpression, argIdxOpt: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Seq[Ast] = {
    expr match {
      case typedExpr: KtAnnotatedExpression         => astsForExpression(typedExpr.getBaseExpression, argIdxOpt)
      case typedExpr: KtArrayAccessExpression       => Seq(astForArrayAccess(typedExpr, argIdxOpt))
      case typedExpr: KtBinaryExpression            => Seq(astForBinaryExpr(typedExpr, argIdxOpt))
      case typedExpr: KtBlockExpression             => List(astForBlock(typedExpr, argIdxOpt))
      case typedExpr: KtBinaryExpressionWithTypeRHS => Seq(astForBinaryExprWithTypeRHS(typedExpr, argIdxOpt))
      case typedExpr: KtBreakExpression             => Seq(astForBreak(typedExpr))
      case typedExpr: KtCallExpression              => Seq(astForCall(typedExpr, argIdxOpt))
      case typedExpr: KtConstantExpression          => Seq(astForLiteral(typedExpr, argIdxOpt))
      case typedExpr: KtClass                       => astsForClassOrObject(typedExpr)
      case typedExpr: KtClassLiteralExpression      => Seq(astForClassLiteral(typedExpr, argIdxOpt))
      case typedExpr: KtSafeQualifiedExpression     => Seq(astForQualifiedExpression(typedExpr, argIdxOpt))
      case typedExpr: KtContinueExpression          => Seq(astForContinue(typedExpr))
      case typedExpr: KtDestructuringDeclaration    => astsForDestructuringDeclaration(typedExpr)
      case typedExpr: KtDotQualifiedExpression      => Seq(astForQualifiedExpression(typedExpr, argIdxOpt))
      case typedExpr: KtDoWhileExpression           => Seq(astForDoWhile(typedExpr))
      case typedExpr: KtForExpression               => Seq(astForFor(typedExpr))
      case typedExpr: KtIfExpression                => Seq(astForIf(typedExpr, argIdxOpt))
      case typedExpr: KtIsExpression                => Seq(astForIsExpression(typedExpr, argIdxOpt))
      case typedExpr: KtLabeledExpression           => astsForExpression(typedExpr.getBaseExpression, argIdxOpt)
      case typedExpr: KtLambdaExpression            => Seq(astForLambda(typedExpr, argIdxOpt))
      case typedExpr: KtNameReferenceExpression if typedExpr.getReferencedNameElementType == KtTokens.IDENTIFIER =>
        Seq(astForNameReference(typedExpr, argIdxOpt))
      // TODO: callable reference
      case _: KtNameReferenceExpression               => Seq()
      case typedExpr: KtObjectLiteralExpression       => Seq(astForUnknown(typedExpr, argIdxOpt))
      case typedExpr: KtParenthesizedExpression       => astsForExpression(typedExpr.getExpression, argIdxOpt)
      case typedExpr: KtPostfixExpression             => Seq(astForPostfixExpression(typedExpr, argIdxOpt))
      case typedExpr: KtPrefixExpression              => Seq(astForPrefixExpression(typedExpr, argIdxOpt))
      case typedExpr: KtProperty if typedExpr.isLocal => astsForProperty(typedExpr)
      case typedExpr: KtReturnExpression              => Seq(astForReturnExpression(typedExpr))
      case typedExpr: KtStringTemplateExpression      => Seq(astForStringTemplate(typedExpr, argIdxOpt))
      case typedExpr: KtSuperExpression               => Seq(astForSuperExpression(typedExpr, argIdxOpt))
      case typedExpr: KtThisExpression                => Seq(astForThisExpression(typedExpr, argIdxOpt))
      case typedExpr: KtThrowExpression               => Seq(astForUnknown(typedExpr, argIdxOpt))
      case typedExpr: KtTryExpression                 => Seq(astForTry(typedExpr, argIdxOpt))
      case typedExpr: KtWhenExpression                => Seq(astForWhen(typedExpr, argIdxOpt))
      case typedExpr: KtWhileExpression               => Seq(astForWhile(typedExpr))
      case typedExpr: KtNamedFunction =>
        logger.debug(
          s"Creating empty AST node for unknown expression `${typedExpr.getClass}` with text `${typedExpr.getText}`."
        )
        Seq(astForUnknown(typedExpr, argIdxOpt))
      case null =>
        logger.trace("Received null expression! Skipping...")
        Seq()
      // TODO: handle `KtCallableReferenceExpression` like `this::baseTerrain`
      case unknownExpr =>
        logger.debug(
          s"Creating empty AST node for unknown expression `${unknownExpr.getClass}` with text `${unknownExpr.getText}`."
        )
        Seq(astForUnknown(unknownExpr, argIdxOpt))
    }
  }
}
