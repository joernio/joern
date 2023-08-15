package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.KtFileWithMeta
import io.joern.kotlin2cpg.types.{TypeConstants, TypeInfoProvider, TypeRenderer}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.passes.IntervalKeyPool
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, ValidationMode}
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.kotlin2cpg.datastructures.Scope
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.lexer.{KtToken, KtTokens}
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala

case class BindingInfo(node: NewBinding, edgeMeta: Seq[(NewNode, NewNode, String)])
case class ClosureBindingDef(node: NewClosureBinding, captureEdgeTo: NewMethodRef, refEdgeTo: NewNode)

class AstCreator(fileWithMeta: KtFileWithMeta, xTypeInfoProvider: TypeInfoProvider, global: Global)(implicit
  withSchemaValidation: ValidationMode
) extends AstCreatorBase(fileWithMeta.filename)
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

    val defaultTypes =
      Set(TypeConstants.javaLangObject, TypeConstants.kotlin) ++ TypeRenderer.primitiveArrayMappings.keys
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

  protected def stringForUUID(node: KtExpression, name: String, typeFullName: String): String = {
    node.getText + node.getContainingKtFile.getName + node.getContainingKtFile.getPackageFqName.toString + name + typeFullName
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
  final def astsForExpression(
    expr: KtExpression,
    argIdxMaybe: Option[Int],
    argNameMaybe: Option[String] = None,
    annotations: Seq[KtAnnotationEntry] = Seq()
  )(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    expr match {
      case typedExpr: KtAnnotatedExpression =>
        astsForExpression(
          typedExpr.getBaseExpression,
          argIdxMaybe,
          argNameMaybe,
          typedExpr.getAnnotationEntries.asScala.toSeq
        )
      case typedExpr: KtArrayAccessExpression =>
        Seq(astForArrayAccess(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtAnonymousInitializer => astsForExpression(typedExpr.getBody, argIdxMaybe)
      case typedExpr: KtBinaryExpression     => astsForBinaryExpr(typedExpr, argIdxMaybe, argNameMaybe, annotations)
      case typedExpr: KtBlockExpression      => astsForBlock(typedExpr, argIdxMaybe, argNameMaybe)
      case typedExpr: KtBinaryExpressionWithTypeRHS =>
        Seq(astForBinaryExprWithTypeRHS(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtBreakExpression    => Seq(astForBreak(typedExpr))
      case typedExpr: KtCallExpression     => astsForCall(typedExpr, argIdxMaybe, argNameMaybe, annotations)
      case typedExpr: KtConstantExpression => Seq(astForLiteral(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtClass              => astsForClassOrObject(typedExpr, None, annotations)
      case typedExpr: KtClassLiteralExpression =>
        Seq(astForClassLiteral(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtSafeQualifiedExpression =>
        Seq(astForQualifiedExpression(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtContinueExpression => Seq(astForContinue(typedExpr))
      // note: annotations are not currently (Kotlin 1.9.0) supported on destructuring declarations
      case typedExpr: KtDestructuringDeclaration => astsForDestructuringDeclaration(typedExpr)
      case typedExpr: KtDotQualifiedExpression =>
        Seq(astForQualifiedExpression(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtDoWhileExpression => Seq(astForDoWhile(typedExpr, annotations))
      case typedExpr: KtForExpression     => Seq(astForFor(typedExpr, annotations))
      case typedExpr: KtIfExpression      => Seq(astForIf(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtIsExpression      => Seq(astForIsExpression(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtLabeledExpression =>
        astsForExpression(typedExpr.getBaseExpression, argIdxMaybe, argNameMaybe, annotations)
      case typedExpr: KtLambdaExpression => Seq(astForLambda(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtNameReferenceExpression if typedExpr.getReferencedNameElementType == KtTokens.IDENTIFIER =>
        Seq(astForNameReference(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      // TODO: callable reference
      case _: KtNameReferenceExpression => Seq()
      case typedExpr: KtObjectLiteralExpression =>
        Seq(astForObjectLiteralExpr(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtParenthesizedExpression =>
        astsForExpression(typedExpr.getExpression, argIdxMaybe, argNameMaybe, annotations)
      case typedExpr: KtPostfixExpression =>
        Seq(astForPostfixExpression(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtPrefixExpression =>
        Seq(astForPrefixExpression(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtProperty if typedExpr.isLocal =>
        astsForProperty(typedExpr, annotations ++ typedExpr.getAnnotationEntries.asScala.toSeq)
      case typedExpr: KtReturnExpression => Seq(astForReturnExpression(typedExpr))
      case typedExpr: KtStringTemplateExpression =>
        Seq(astForStringTemplate(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtSuperExpression => Seq(astForSuperExpression(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtThisExpression  => Seq(astForThisExpression(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtThrowExpression => Seq(astForUnknown(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtTryExpression   => Seq(astForTry(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtWhenExpression  => Seq(astForWhen(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtWhileExpression => Seq(astForWhile(typedExpr, annotations))
      case typedExpr: KtNamedFunction if Option(typedExpr.getName).isEmpty =>
        Seq(astForAnonymousFunction(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtNamedFunction =>
        logger.debug(
          s"Creating empty AST node for unknown expression `${typedExpr.getClass}` with text `${typedExpr.getText}`."
        )
        Seq(astForUnknown(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case null =>
        logger.trace("Received null expression! Skipping...")
        Seq()
      // TODO: handle `KtCallableReferenceExpression` like `this::baseTerrain`
      case unknownExpr =>
        logger.debug(
          s"Creating empty AST node for unknown expression `${unknownExpr.getClass}` with text `${unknownExpr.getText}`."
        )
        Seq(astForUnknown(unknownExpr, argIdxMaybe, argNameMaybe, annotations))
    }
  }
}
