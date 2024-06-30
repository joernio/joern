package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.KtFileWithMeta
import io.joern.kotlin2cpg.datastructures.Scope
import io.joern.kotlin2cpg.types.TypeConstants
import io.joern.kotlin2cpg.types.TypeInfoProvider
import io.joern.kotlin2cpg.types.TypeRenderer
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.AstNodeBuilder
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.newMethodReturnNode
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.DescriptorVisibility
import org.jetbrains.kotlin.lexer.KtToken
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.*
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder

import java.io.PrintWriter
import java.io.StringWriter
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

case class BindingInfo(node: NewBinding, edgeMeta: Seq[(NewNode, NewNode, String)])
case class ClosureBindingDef(node: NewClosureBinding, captureEdgeTo: NewMethodRef, refEdgeTo: NewNode)

class AstCreator(fileWithMeta: KtFileWithMeta, xTypeInfoProvider: TypeInfoProvider, global: Global)(implicit
  withSchemaValidation: ValidationMode
) extends AstCreatorBase(fileWithMeta.filename)
    with AstForDeclarationsCreator
    with AstForPrimitivesCreator
    with AstForFunctionsCreator
    with AstForStatementsCreator
    with AstForExpressionsCreator
    with AstNodeBuilder[PsiElement, AstCreator] {

  protected val closureBindingDefQueue: mutable.ArrayBuffer[ClosureBindingDef] = mutable.ArrayBuffer.empty
  protected val bindingInfoQueue: mutable.ArrayBuffer[BindingInfo]             = mutable.ArrayBuffer.empty
  protected val lambdaAstQueue: mutable.ArrayBuffer[Ast]                       = mutable.ArrayBuffer.empty
  protected val lambdaBindingInfoQueue: mutable.ArrayBuffer[BindingInfo]       = mutable.ArrayBuffer.empty
  protected val methodAstParentStack: Stack[NewNode]                           = new Stack()

  protected val tmpKeyPool      = new IntervalKeyPool(first = 1, last = Long.MaxValue)
  protected val iteratorKeyPool = new IntervalKeyPool(first = 1, last = Long.MaxValue)

  protected val relativizedPath: String = fileWithMeta.relativizedPath

  protected val scope: Scope[String, DeclarationNew, NewNode] = new Scope()
  protected val debugScope: mutable.Stack[KtDeclaration]      = mutable.Stack.empty[KtDeclaration]

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

  // TODO: use this everywhere in kotlin2cpg instead of manual .getText calls
  override def code(element: PsiElement): String = shortenCode(element.getText)

  override def line(element: PsiElement): Option[Int] = {
    try {
      Some(
        element.getContainingFile.getViewProvider.getDocument
          .getLineNumber(element.getTextOffset) + 1
      )
    } catch {
      case _: Throwable => None
    }
  }

  override def column(element: PsiElement): Option[Int] = {
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

  override def lineEnd(element: PsiElement): Option[Int] = {
    val lastElement = element match {
      case namedFn: KtNamedFunction =>
        Option(namedFn.getBodyBlockExpression)
          .map(_.getRBrace)
          .getOrElse(element)
      case elem => elem
    }
    line(lastElement)
  }

  override def columnEnd(element: PsiElement): Option[Int] = {
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
    val isWildcard = node.isWildcard.getOrElse(false)
    if (isWildcard) Constants.wildcardImportName
    else node.importedEntity.getOrElse("")
  }

  private def storeInDiffGraph(ast: Ast): Unit = {
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

  private def logDebugWithTestAndStackTrace(message: String): Unit = {
    val declString = debugScope.headOption.map(_.getText).getOrElse("Declaration scope empty")
    logger.debug(message + "\nIn declaration:\n" + declString + "\nStack trace to declaration:" + getStackTrace)
  }

  private def getStackTrace: String = {
    val stackTrace = Thread.currentThread().getStackTrace
    var endIndex   = stackTrace.indexWhere(_.toString.contains("astsForDeclaration"))
    if (endIndex == -1) {
      endIndex = stackTrace.length
    }
    val partialStackTrace = stackTrace.slice(2, endIndex + 1)
    partialStackTrace.mkString("\n\t", "\n\t", "")
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
        logDebugWithTestAndStackTrace("Received null expression! Skipping...")
        Seq()
      // TODO: handle `KtCallableReferenceExpression` like `this::baseTerrain`
      case unknownExpr =>
        logger.debug(
          s"Creating empty AST node for unknown expression `${unknownExpr.getClass}` with text `${unknownExpr.getText}`."
        )
        Seq(astForUnknown(unknownExpr, argIdxMaybe, argNameMaybe, annotations))
    }
  }

  private def astForFile(fileWithMeta: KtFileWithMeta)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val ktFile = fileWithMeta.f

    val importDirectives = ktFile.getImportList.getImports.asScala
    val importAsts       = importDirectives.toList.map(astForImportDirective)
    val namespaceBlocksForImports =
      for {
        node <- importAsts.flatMap(_.root.collectAll[NewImport])
        name = getName(node)
      } yield Ast(NodeBuilders.newNamespaceBlockNode(name, name, relativizedPath))

    val packageName = ktFile.getPackageFqName.toString
    val node =
      if (packageName == Constants.root)
        NodeBuilders.newNamespaceBlockNode(
          NamespaceTraversal.globalNamespaceName,
          NamespaceTraversal.globalNamespaceName,
          relativizedPath
        )
      else {
        val name = packageName.split("\\.").lastOption.getOrElse("")
        NodeBuilders.newNamespaceBlockNode(name, packageName, relativizedPath)
      }
    methodAstParentStack.push(node)

    val name     = NamespaceTraversal.globalNamespaceName
    val fullName = node.fullName
    val fakeGlobalTypeDecl =
      typeDeclNode(ktFile, name, fullName, relativizedPath, name, NodeTypes.NAMESPACE_BLOCK, fullName)
    methodAstParentStack.push(fakeGlobalTypeDecl)

    val fakeGlobalMethod =
      methodNode(ktFile, name, name, fullName, None, relativizedPath, Option(NodeTypes.TYPE_DECL), Option(fullName))
    methodAstParentStack.push(fakeGlobalMethod)
    scope.pushNewScope(fakeGlobalMethod)

    val blockNode_   = blockNode(ktFile, "<empty>", registerType(TypeConstants.any))
    val methodReturn = newMethodReturnNode(TypeConstants.any, None, None, None)

    val declarationsAsts = ktFile.getDeclarations.asScala.flatMap(astsForDeclaration)
    val fileNode         = NewFile().name(fileWithMeta.relativizedPath)
    val lambdaTypeDecls =
      lambdaBindingInfoQueue.flatMap(_.edgeMeta.collect { case (node: NewTypeDecl, _, _) => Ast(node) })
    methodAstParentStack.pop()

    val allDeclarationAsts = declarationsAsts ++ lambdaAstQueue ++ lambdaTypeDecls
    val fakeTypeDeclAst =
      Ast(fakeGlobalTypeDecl)
        .withChild(
          methodAst(fakeGlobalMethod, Seq.empty, blockAst(blockNode_, allDeclarationAsts.toList), methodReturn)
        )
    val namespaceBlockAst =
      Ast(node).withChildren(importAsts).withChild(fakeTypeDeclAst)
    Ast(fileNode).withChildren(namespaceBlockAst :: namespaceBlocksForImports)
  }

  def astsForDeclaration(decl: KtDeclaration)(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    debugScope.push(decl)
    val result =
      try {
        decl match {
          case c: KtClass             => astsForClassOrObject(c)
          case o: KtObjectDeclaration => astsForClassOrObject(o)
          case n: KtNamedFunction =>
            val isExtensionFn = typeInfoProvider.isExtensionFn(n)
            astsForMethod(n, isExtensionFn)
          case t: KtTypeAlias            => Seq(astForTypeAlias(t))
          case s: KtSecondaryConstructor => Seq(astForUnknown(s, None, None))
          case p: KtProperty             => astsForProperty(p)
          case unhandled =>
            logger.error(
              s"Unknown declaration type encountered in this file `$relativizedPath` with text `${unhandled.getText}` and class `${unhandled.getClass}`!"
            )
            Seq()
        }
      } catch {
        case exception: Exception =>
          val declText     = decl.getText
          val stringWriter = new StringWriter()
          val printWriter  = new PrintWriter(stringWriter)
          exception.printStackTrace(printWriter)
          logger.warn(
            s"Caught exception while processing decl in this file `$relativizedPath`:\n$declText\n${stringWriter.toString}"
          )
          Seq()
      }
    debugScope.pop()
    result
  }

  def astForUnknown(
    expr: KtExpression,
    argIdx: Option[Int],
    argNameMaybe: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val node = unknownNode(expr, Option(expr).map(_.getText).getOrElse(Constants.codePropUndefinedValue))
    Ast(withArgumentIndex(node, argIdx).argumentName(argNameMaybe))
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  protected def assignmentAstForDestructuringEntry(
    entry: KtDestructuringDeclarationEntry,
    componentNReceiverName: String,
    componentNTypeFullName: String,
    componentIdx: Integer
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val entryTypeFullName = registerType(typeInfoProvider.typeFullName(entry, TypeConstants.any))
    val assignmentLHSNode = identifierNode(entry, entry.getText, entry.getText, entryTypeFullName)
    val assignmentLHSAst  = astWithRefEdgeMaybe(assignmentLHSNode.name, assignmentLHSNode)

    val componentNIdentifierNode =
      identifierNode(entry, componentNReceiverName, componentNReceiverName, componentNTypeFullName)
        .argumentIndex(0)

    val fallbackSignature = s"${Defines.UnresolvedNamespace}()"
    val fallbackFullName =
      s"${Defines.UnresolvedNamespace}${Constants.componentNPrefix}$componentIdx:$fallbackSignature"
    val (fullName, signature) =
      typeInfoProvider.fullNameWithSignature(entry, (fallbackFullName, fallbackSignature))
    val componentNCallCode = s"$componentNReceiverName.${Constants.componentNPrefix}$componentIdx()"
    val componentNCallNode = callNode(
      entry,
      componentNCallCode,
      s"${Constants.componentNPrefix}$componentIdx",
      fullName,
      DispatchTypes.DYNAMIC_DISPATCH,
      Some(signature),
      Some(entryTypeFullName)
    )

    val componentNIdentifierAst = astWithRefEdgeMaybe(componentNIdentifierNode.name, componentNIdentifierNode)
    val componentNAst =
      callAst(componentNCallNode, Seq(), Option(componentNIdentifierAst))

    val assignmentCallNode = NodeBuilders.newOperatorCallNode(
      Operators.assignment,
      s"${entry.getText} = $componentNCallCode",
      None,
      line(entry),
      column(entry)
    )
    callAst(assignmentCallNode, List(assignmentLHSAst, componentNAst))
  }

  protected def astDerivedFullNameWithSignature(expr: KtQualifiedExpression, argAsts: List[Ast])(implicit
    typeInfoProvider: TypeInfoProvider
  ): (String, String) = {
    val astDerivedMethodFullName = expr.getSelectorExpression match {
      case expression: KtCallExpression =>
        val receiverPlaceholderType = Defines.UnresolvedNamespace
        val shortName               = expr.getSelectorExpression.getFirstChild.getText
        val args                    = expression.getValueArguments
        s"$receiverPlaceholderType.$shortName:${typeInfoProvider.anySignature(args.asScala.toList)}"
      case _: KtNameReferenceExpression =>
        Operators.fieldAccess
      case _ =>
        // TODO: add more test cases for this scenario
        ""
    }

    val astDerivedSignature = typeInfoProvider.anySignature(argAsts)
    (astDerivedMethodFullName, astDerivedSignature)
  }

  protected def selectorExpressionArgAsts(
    expr: KtQualifiedExpression
  )(implicit typeInfoProvider: TypeInfoProvider): List[Ast] = {
    expr.getSelectorExpression match {
      case typedExpr: KtCallExpression =>
        withIndex(typedExpr.getValueArguments.asScala.toSeq) { case (arg, idx) =>
          astsForExpression(arg.getArgumentExpression, Some(idx))
        }.flatten.toList
      case typedExpr: KtNameReferenceExpression =>
        val node = fieldIdentifierNode(typedExpr, typedExpr.getText, typedExpr.getText).argumentIndex(2)
        List(Ast(node))
      case _ => List()
    }
  }

  protected def modifierTypeForVisibility(visibility: DescriptorVisibility): String = {
    if (visibility.toString == DescriptorVisibilities.PUBLIC.toString)
      ModifierTypes.PUBLIC
    else if (visibility.toString == DescriptorVisibilities.PRIVATE.toString)
      ModifierTypes.PRIVATE
    else if (visibility.toString == DescriptorVisibilities.PROTECTED.toString)
      ModifierTypes.PROTECTED
    else if (visibility.toString == DescriptorVisibilities.INTERNAL.toString)
      ModifierTypes.INTERNAL
    else "UNKNOWN"
  }
}
