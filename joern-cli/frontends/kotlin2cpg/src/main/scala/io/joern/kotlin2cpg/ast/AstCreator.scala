package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.KtFileWithMeta
import io.joern.kotlin2cpg.types.{TypeConstants, TypeInfoProvider}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.passes.IntervalKeyPool
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.datastructures.Stack._
import io.joern.x2cpg.AstNodeBuilder
import io.joern.kotlin2cpg.datastructures.Scope
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
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
    with KtPsiToAst
    with AstNodeBuilder[PsiElement, AstCreator] {

  protected val closureBindingDefQueue: mutable.ArrayBuffer[ClosureBindingDef] = mutable.ArrayBuffer.empty
  protected val bindingInfoQueue: mutable.ArrayBuffer[BindingInfo]             = mutable.ArrayBuffer.empty
  protected val lambdaAstQueue: mutable.ArrayBuffer[Ast]                       = mutable.ArrayBuffer.empty
  protected val lambdaBindingInfoQueue: mutable.ArrayBuffer[BindingInfo]       = mutable.ArrayBuffer.empty
  protected val methodAstParentStack: Stack[NewNode]                           = new Stack()

  protected val lambdaKeyPool   = new IntervalKeyPool(first = 1, last = Long.MaxValue)
  protected val tmpKeyPool      = new IntervalKeyPool(first = 1, last = Long.MaxValue)
  protected val iteratorKeyPool = new IntervalKeyPool(first = 1, last = Long.MaxValue)

  protected val relativizedPath: String = fileWithMeta.relativizedPath

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

  def line(element: PsiElement): Option[Integer] = {
    try {
      Some(
        element.getContainingFile.getViewProvider.getDocument
          .getLineNumber(element.getTextOffset) + 1
      )
    } catch {
      case _: Throwable => None
    }
  }

  def column(element: PsiElement): Option[Integer] = {
    try {
      val lineNumber =
        element.getContainingFile.getViewProvider.getDocument
          .getLineNumber(element.getTextOffset)
      val lineOffset =
        element.getContainingFile.getViewProvider.getDocument.getLineStartOffset(lineNumber)
      Some(element.getTextOffset - lineOffset)
    } catch {
      case _: Throwable => None
    }
  }

  def lineEnd(element: PsiElement): Option[Integer] = {
    val lastElement = element match {
      case namedFn: KtNamedFunction =>
        Option(namedFn.getBodyBlockExpression)
          .map(_.getRBrace)
          .getOrElse(element)
      case elem => elem
    }
    line(lastElement)
  }

  def columnEnd(element: PsiElement): Option[Integer] = {
    val lastElement = element match {
      case namedFn: KtNamedFunction =>
        Option(namedFn.getBodyBlockExpression)
          .map(_.getRBrace)
          .getOrElse(element)
      case elem => elem
    }
    column(lastElement)
  }

  protected def getName(node: NewImport): String = {
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
  final def astsForExpression(expr: KtExpression, argIdxOpt: Option[Int], argNameOpt: Option[String] = None)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Seq[Ast] = {
    expr match {
      case typedExpr: KtAnnotatedExpression   => astsForExpression(typedExpr.getBaseExpression, argIdxOpt)
      case typedExpr: KtArrayAccessExpression => Seq(astForArrayAccess(typedExpr, argIdxOpt, argNameOpt))
      case typedExpr: KtAnonymousInitializer  => astsForExpression(typedExpr.getBody, argIdxOpt)
      case typedExpr: KtBinaryExpression      => Seq(astForBinaryExpr(typedExpr, argIdxOpt))
      case typedExpr: KtBlockExpression       => astsForBlock(typedExpr, argIdxOpt)
      case typedExpr: KtBinaryExpressionWithTypeRHS =>
        Seq(astForBinaryExprWithTypeRHS(typedExpr, argIdxOpt, argNameOpt))
      case typedExpr: KtBreakExpression          => Seq(astForBreak(typedExpr))
      case typedExpr: KtCallExpression           => astsForCall(typedExpr, argIdxOpt)
      case typedExpr: KtConstantExpression       => Seq(astForLiteral(typedExpr, argIdxOpt, argNameOpt))
      case typedExpr: KtClass                    => astsForClassOrObject(typedExpr)
      case typedExpr: KtClassLiteralExpression   => Seq(astForClassLiteral(typedExpr, argIdxOpt, argNameOpt))
      case typedExpr: KtSafeQualifiedExpression  => Seq(astForQualifiedExpression(typedExpr, argIdxOpt))
      case typedExpr: KtContinueExpression       => Seq(astForContinue(typedExpr))
      case typedExpr: KtDestructuringDeclaration => astsForDestructuringDeclaration(typedExpr)
      case typedExpr: KtDotQualifiedExpression   => Seq(astForQualifiedExpression(typedExpr, argIdxOpt))
      case typedExpr: KtDoWhileExpression        => Seq(astForDoWhile(typedExpr))
      case typedExpr: KtForExpression            => Seq(astForFor(typedExpr))
      case typedExpr: KtIfExpression             => Seq(astForIf(typedExpr, argIdxOpt))
      case typedExpr: KtIsExpression             => Seq(astForIsExpression(typedExpr, argIdxOpt, argNameOpt))
      case typedExpr: KtLabeledExpression        => astsForExpression(typedExpr.getBaseExpression, argIdxOpt)
      case typedExpr: KtLambdaExpression         => Seq(astForLambda(typedExpr, argIdxOpt))
      case typedExpr: KtNameReferenceExpression if typedExpr.getReferencedNameElementType == KtTokens.IDENTIFIER =>
        Seq(astForNameReference(typedExpr, argIdxOpt, argNameOpt))
      // TODO: callable reference
      case _: KtNameReferenceExpression               => Seq()
      case typedExpr: KtObjectLiteralExpression       => Seq(astForUnknown(typedExpr, argIdxOpt))
      case typedExpr: KtParenthesizedExpression       => astsForExpression(typedExpr.getExpression, argIdxOpt)
      case typedExpr: KtPostfixExpression             => Seq(astForPostfixExpression(typedExpr, argIdxOpt, argNameOpt))
      case typedExpr: KtPrefixExpression              => Seq(astForPrefixExpression(typedExpr, argIdxOpt, argNameOpt))
      case typedExpr: KtProperty if typedExpr.isLocal => astsForProperty(typedExpr)
      case typedExpr: KtReturnExpression              => Seq(astForReturnExpression(typedExpr))
      case typedExpr: KtStringTemplateExpression      => Seq(astForStringTemplate(typedExpr, argIdxOpt, argNameOpt))
      case typedExpr: KtSuperExpression               => Seq(astForSuperExpression(typedExpr, argIdxOpt, argNameOpt))
      case typedExpr: KtThisExpression                => Seq(astForThisExpression(typedExpr, argIdxOpt, argNameOpt))
      case typedExpr: KtThrowExpression               => Seq(astForUnknown(typedExpr, argIdxOpt))
      case typedExpr: KtTryExpression                 => Seq(astForTry(typedExpr, argIdxOpt))
      case typedExpr: KtWhenExpression                => Seq(astForWhen(typedExpr, argIdxOpt))
      case typedExpr: KtWhileExpression               => Seq(astForWhile(typedExpr))
      case typedExpr: KtNamedFunction if Option(typedExpr.getName).isEmpty =>
        Seq(astForAnonymousFunction(typedExpr, argIdxOpt))
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
