package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.{Constants, KtFileWithMeta}
import io.joern.kotlin2cpg.datastructures.Scope
import io.joern.kotlin2cpg.types.{NameRenderer, TypeConstants, TypeInfoProvider}
import io.joern.x2cpg.{Ast, AstCreatorBase, Defines, ValidationMode}
import io.joern.x2cpg.AstNodeBuilder.bindingNode
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.utils.IntervalKeyPool
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.descriptors.{
  DeclarationDescriptor,
  DescriptorVisibilities,
  DescriptorVisibility,
  FunctionDescriptor
}
import org.jetbrains.kotlin.lexer.{KtToken, KtTokens}
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.resolve.BindingContext
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Try
import org.jetbrains.kotlin.types.KotlinType

object AstCreator {
  case class AnonymousObjectContext(declaration: KtElement)
  case class BindingInfo(node: NewBinding, edgeMeta: Seq[(NewNode, NewNode, String)])
  case class ClosureBindingDef(node: NewClosureBinding, captureEdgeTo: NewMethodRef, refEdgeTo: NewNode)
  case class SamImplInfo(
    samImplClass: String,
    methodRefName: String,
    signature: String,
    inheritsFrom: List[String],
    samMethodName: String,
    samMethodSig: String,
    receiverParam: Option[(Ast, String)],
    receiverTypeFullName: Option[String],
    samMethodParams: List[(String, String)],
    isStaticReference: Boolean, // true for Type::method, false for obj::method
    expr: KtCallableReferenceExpression,
    isUnboundReference: Boolean = false // true for ::topLevelFunction
  )
}

class AstCreator(
  fileWithMeta: KtFileWithMeta,
  bindingContext: BindingContext,
  global: Global,
  disableFileContent: Boolean
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase[PsiElement, AstCreator](fileWithMeta.filename)
    with AstForDeclarationsCreator
    with AstForPrimitivesCreator
    with AstForFunctionsCreator
    with AstForStatementsCreator
    with AstForExpressionsCreator {

  import AstCreator.{BindingInfo, ClosureBindingDef, SamImplInfo}

  protected val closureBindingDefQueue: mutable.ArrayBuffer[ClosureBindingDef] = mutable.ArrayBuffer.empty
  protected val bindingInfoQueue: mutable.ArrayBuffer[BindingInfo]             = mutable.ArrayBuffer.empty
  protected val lambdaAstQueue: mutable.ArrayBuffer[Ast]                       = mutable.ArrayBuffer.empty
  protected val lambdaBindingInfoQueue: mutable.ArrayBuffer[BindingInfo]       = mutable.ArrayBuffer.empty
  protected val samImplInfoQueue: mutable.Map[String, SamImplInfo]             = mutable.Map.empty
  protected val methodAstParentStack: Stack[NewNode]                           = new Stack()

  protected val tmpKeyPool             = new IntervalKeyPool(first = 1, last = Long.MaxValue)
  protected val destructedParamKeyPool = new IntervalKeyPool(first = 1, last = Long.MaxValue)
  protected val iteratorKeyPool        = new IntervalKeyPool(first = 1, last = Long.MaxValue)

  protected val relativizedPath: String = fileWithMeta.relativizedPath

  protected val scope: Scope[String, DeclarationNew, NewNode] = new Scope()
  protected val debugScope: mutable.Stack[KtDeclaration]      = mutable.Stack.empty[KtDeclaration]

  protected val nameRenderer     = new NameRenderer()
  protected val bindingUtils     = new BindingContextUtils(bindingContext)
  protected val typeInfoProvider = new TypeInfoProvider(bindingContext)

  def createAst(): DiffGraphBuilder = {
    logger.debug(s"Started parsing file `${fileWithMeta.filename}`.")
    val defaultTypes = Set(TypeConstants.JavaLangObject, TypeConstants.Kotlin)
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

  protected def getFallback[T](
    expr: KtExpression,
    propertyExtractor: FunctionDescriptor => Option[T]
  ): Option[FunctionDescriptor] = {
    val candidates = bindingUtils.getAmbiguousCalledFunctionDescs(expr)
    if (candidates.isEmpty) {
      return None
    }

    val candidateProperties = candidates.map(propertyExtractor)
    val allPropertiesEqual  = candidateProperties.forall(_ == candidateProperties.head)

    if (allPropertiesEqual) {
      candidates.headOption
    } else {
      None
    }
  }

  protected def getAmbiguousFuncDescIfFullNamesEqual(expr: KtExpression): Option[FunctionDescriptor] = {
    getFallback(expr, nameRenderer.descFullName)
  }

  protected def getAmbiguousFuncDescIfSignaturesEqual(expr: KtExpression): Option[FunctionDescriptor] = {
    getFallback(expr, nameRenderer.funcDescSignature)
  }

  protected def calleeFullnameAndSignature(
    calleeExpr: KtExpression,
    fullNameFallback: => String,
    signatureFallback: => String
  ): (String, String) = {
    val funcDesc = bindingUtils.getCalledFunctionDesc(calleeExpr)
    val descFullName = funcDesc
      .orElse(getAmbiguousFuncDescIfFullNamesEqual(calleeExpr))
      .flatMap(nameRenderer.descFullName)
      .getOrElse(fullNameFallback)
    val signature = funcDesc
      .orElse(getAmbiguousFuncDescIfSignaturesEqual(calleeExpr))
      .flatMap(nameRenderer.funcDescSignature)
      .getOrElse(signatureFallback)
    val fullName = nameRenderer.combineFunctionFullName(descFullName, signature)

    (fullName, signature)
  }

  protected def getCalleeExpr(expr: KtExpression): KtExpression = {
    expr match {
      case qualifiedExpression: KtQualifiedExpression =>
        getCalleeExpr(qualifiedExpression.getSelectorExpression)
      case callExpr: KtCallExpression =>
        callExpr.getCalleeExpression
    }
  }

  // TODO: use this everywhere in kotlin2cpg instead of manual .getText calls
  override def code(element: PsiElement): String =
    shortenCode(element.getText)

  override def line(element: PsiElement): Option[Int] =
    Try(element.getContainingFile.getViewProvider.getDocument.getLineNumber(element.getTextOffset) + 1).toOption

  override def column(element: PsiElement): Option[Int] = {
    for {
      lineNumber <- line(element)
      lineOffset <- Try(
        element.getContainingFile.getViewProvider.getDocument.getLineStartOffset(lineNumber - 1)
      ).toOption
    } yield element.getTextOffset - lineOffset
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

  override def offset(element: PsiElement): Option[(Int, Int)] = {
    Option
      .unless(disableFileContent) {
        Option(element).map { someElement =>
          val textRange = someElement.getTextRange
          (textRange.getStartOffset, textRange.getEndOffset)
        }
      }
      .flatten
  }

  protected def getName(node: NewImport): String = {
    val isWildcard = node.isWildcard.getOrElse(false)
    if (isWildcard) Constants.WildcardImportName
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

  protected def logDebugWithTestAndStackTrace(message: String): Unit = {
    val declString = debugScope.headOption.map(_.getText).getOrElse("Declaration scope empty")
    logger.debug(message + "\nIn declaration:\n" + declString + "\nStack trace to declaration:" + getStackTrace)
  }

  protected def logWarnWithTestAndStackTrace(message: String): Unit = {
    val declString = debugScope.headOption.map(_.getText).getOrElse("Declaration scope empty")
    logger.warn(message + "\nIn declaration:\n" + declString + "\nStack trace to declaration:" + getStackTrace)
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
    annotations: Seq[KtAnnotationEntry] = Seq(),
    argTypeFallback: Option[KotlinType] = None
  ): Seq[Ast] = {
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
      case typedExpr: KtBlockExpression      => Seq(astForBlock(typedExpr, argIdxMaybe, argNameMaybe))
      case typedExpr: KtBinaryExpressionWithTypeRHS =>
        Seq(astForBinaryExprWithTypeRHS(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtBreakExpression    => Seq(astForBreak(typedExpr))
      case typedExpr: KtCallExpression     => astsForCall(typedExpr, argIdxMaybe, argNameMaybe, annotations)
      case typedExpr: KtConstantExpression => Seq(astForLiteral(typedExpr, argIdxMaybe, argNameMaybe, annotations))
      case typedExpr: KtClass              => Seq(astForClassOrObject(typedExpr, None, annotations))
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
      case _: KtNameReferenceExpression => Seq()
      case typedExpr: KtCallableReferenceExpression =>
        Seq(astForCallableReferenceExpression(typedExpr, argIdxMaybe, argNameMaybe, annotations, argTypeFallback))
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
      case unknownExpr =>
        logger.debug(
          s"Creating empty AST node for unknown expression `${unknownExpr.getClass}` with text `${unknownExpr.getText}`."
        )
        Seq(astForUnknown(unknownExpr, argIdxMaybe, argNameMaybe, annotations))
    }
  }

  private def astForFile(fileWithMeta: KtFileWithMeta): Ast = {
    val ktFile = fileWithMeta.f

    val importDirectives = ktFile.getImportList.getImports.asScala
    val importAsts       = importDirectives.toList.map(astForImportDirective)
    val namespaceBlocksForImports =
      for {
        node <- importAsts.flatMap(_.root.collectAll[NewImport])
        name = getName(node)
      } yield Ast(namespaceBlockNode(fileWithMeta.f, name, name, relativizedPath))

    val packageName = ktFile.getPackageFqName.toString
    val node =
      if (packageName == Constants.Root) {
        namespaceBlockNode(
          fileWithMeta.f,
          NamespaceTraversal.globalNamespaceName,
          NamespaceTraversal.globalNamespaceName,
          relativizedPath
        )
      } else {
        val name = packageName.split("\\.").lastOption.getOrElse("")
        namespaceBlockNode(fileWithMeta.f, name, packageName, relativizedPath)
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

    val blockNode_   = blockNode(ktFile, "<empty>", registerType(TypeConstants.Any))
    val methodReturn = methodReturnNode(ktFile, TypeConstants.Any)

    val declarationsAsts = ktFile.getDeclarations.asScala.flatMap(astsForDeclaration)
    val fileNode         = NewFile().name(fileWithMeta.relativizedPath)
    if (!disableFileContent) {
      fileNode.content(fileWithMeta.f.getText)
    }
    val lambdaTypeDecls =
      lambdaBindingInfoQueue.flatMap(_.edgeMeta.collect { case (node: NewTypeDecl, _, _) => Ast(node) })

    val samImplTypeDecls = samImplInfoQueue.values.map { samInfo =>
      createSamImplTypeDecl(samInfo)
    }

    methodAstParentStack.pop()

    val allDeclarationAsts = declarationsAsts ++ lambdaAstQueue ++ lambdaTypeDecls.distinct ++ samImplTypeDecls
    val fakeTypeDeclAst =
      Ast(fakeGlobalTypeDecl)
        .withChild(
          methodAst(fakeGlobalMethod, Seq.empty, blockAst(blockNode_, allDeclarationAsts.toList), methodReturn)
        )
    val namespaceBlockAst =
      Ast(node).withChildren(importAsts).withChild(fakeTypeDeclAst)
    Ast(fileNode).withChildren(namespaceBlockAst :: namespaceBlocksForImports)
  }

  def astsForDeclaration(decl: KtDeclaration): Seq[Ast] = {
    debugScope.push(decl)
    val result =
      try {
        decl match {
          case c: KtClass                => Seq(astForClassOrObject(c))
          case o: KtObjectDeclaration    => Seq(astForClassOrObject(o))
          case n: KtNamedFunction        => Seq(astForMethod(n))
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
          logger.warn(
            s"Caught exception while processing decl in this file `$relativizedPath`:\n${decl.getText}",
            exception
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
  ): Ast = {
    val node = unknownNode(expr, Option(expr).map(_.getText).getOrElse(Constants.CodePropUndefinedValue))
    Ast(withArgumentIndex(node, argIdx).argumentName(argNameMaybe))
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  protected def assignmentAstForDestructuringEntry(
    entry: KtDestructuringDeclarationEntry,
    rhsBaseAst: Ast,
    componentIdx: Integer
  ): Ast = {
    val entryTypeFullName = registerType(
      bindingUtils
        .getVariableDesc(entry)
        .flatMap(desc => nameRenderer.typeFullName(desc.getType))
        .getOrElse(TypeConstants.Any)
    )
    val assignmentLHSNode = identifierNode(entry, entry.getText, entry.getText, entryTypeFullName)
    val assignmentLHSAst  = astWithRefEdgeMaybe(assignmentLHSNode.name, assignmentLHSNode)

    val desc = bindingUtils.getCalledFunctionDesc(entry)
    val descFullName = desc
      .flatMap(nameRenderer.descFullName)
      .getOrElse(s"${Defines.UnresolvedNamespace}${Constants.ComponentNPrefix}$componentIdx")
    val signature = desc
      .flatMap(nameRenderer.funcDescSignature)
      .getOrElse(s"${Defines.UnresolvedSignature}()")
    val fullName = nameRenderer.combineFunctionFullName(descFullName, signature)

    val componentNCallCode =
      s"${rhsBaseAst.root.get.asInstanceOf[ExpressionNew].code}.${Constants.ComponentNPrefix}$componentIdx()"
    val componentNCallNode = callNode(
      entry,
      componentNCallCode,
      s"${Constants.ComponentNPrefix}$componentIdx",
      fullName,
      DispatchTypes.DYNAMIC_DISPATCH,
      Some(signature),
      Some(entryTypeFullName)
    )

    val componentNAst =
      callAst(componentNCallNode, Seq(), Option(rhsBaseAst))

    val assignmentCallNode =
      operatorCallNode(entry, s"${entry.getText} = $componentNCallCode", Operators.assignment, None)
    callAst(assignmentCallNode, List(assignmentLHSAst, componentNAst))
  }

  protected def astDerivedFullNameWithSignature(expr: KtQualifiedExpression, argAsts: List[Ast]): (String, String) = {
    val astDerivedMethodFullName = expr.getSelectorExpression match {
      case expression: KtCallExpression =>
        val receiverPlaceholderType = Defines.UnresolvedNamespace
        val shortName               = expression.getFirstChild.getText
        s"$receiverPlaceholderType.$shortName"
      case _: KtNameReferenceExpression =>
        Operators.fieldAccess
      case _ =>
        // TODO: add more test cases for this scenario
        ""
    }

    val astDerivedSignature = s"${Defines.UnresolvedSignature}(${argAsts.size})"
    (astDerivedMethodFullName, astDerivedSignature)
  }

  protected def astsForKtCallExpressionArguments(callExpr: KtCallExpression, startIndex: Int = 1): List[Ast] = {
    val calledFuncDesc = bindingUtils.getCalledFunctionDesc(callExpr.getCalleeExpression)

    withIndex(callExpr.getValueArguments.asScala.toSeq) { case (arg, idx) =>
      val argumentNameMaybe = Option(arg.getArgumentName).map(_.getText)
      val argumentTypeMaybe = calledFuncDesc
        .flatMap { funcDesc =>
          val params = funcDesc.getValueParameters.asScala.toList
          if (idx - 1 < params.size) {
            Some(params(idx - 1))
          } else {
            None
          }
        }
        .flatMap(paramDesc => Some(paramDesc.getType))

      astsForExpression(
        arg.getArgumentExpression,
        Some(startIndex + idx - 1),
        argumentNameMaybe,
        argTypeFallback = argumentTypeMaybe
      )
    }.flatten.toList
  }

  protected def selectorExpressionArgAsts(expr: KtQualifiedExpression, startIndex: Int = 1): List[Ast] = {
    val callExpr = expr.getSelectorExpression.asInstanceOf[KtCallExpression]
    astsForKtCallExpressionArguments(callExpr, startIndex)
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

  protected def addToLambdaBindingInfoQueue(
    bindingNode: NewBinding,
    typeDecl: NewTypeDecl,
    methodNode: NewMethod
  ): Unit = {
    lambdaBindingInfoQueue.prepend(
      BindingInfo(bindingNode, Seq((typeDecl, bindingNode, EdgeTypes.BINDS), (bindingNode, methodNode, EdgeTypes.REF)))
    )
  }

  protected def createSamImplTypeDecl(samInfo: SamImplInfo): Ast = {
    val samTypeDecl = typeDeclNode(
      samInfo.expr,
      samInfo.samImplClass,
      samInfo.samImplClass,
      relativizedPath,
      samInfo.inheritsFrom.map(registerType),
      None
    )

    // For unbound references (top-level functions), don't create an invoke method
    // Just create the type and binding directly to the original function
    if (samInfo.isUnboundReference) {
      val methodRefBinding =
        bindingNode(samInfo.samMethodName, samInfo.samMethodSig, s"${samInfo.methodRefName}:${samInfo.signature}")
      bindingInfoQueue.prepend(BindingInfo(methodRefBinding, Seq((samTypeDecl, methodRefBinding, EdgeTypes.BINDS))))
      return Ast(samTypeDecl)
    }

    // For bound references, create the full invoke method
    val samMethodFullName = s"${samInfo.samImplClass}.${samInfo.samMethodName}:${samInfo.samMethodSig}"
    val samMethodNode = methodNode(
      samInfo.expr,
      samInfo.samMethodName,
      samInfo.samMethodName,
      samMethodFullName,
      Some(samInfo.samMethodSig),
      relativizedPath
    )

    val thisParamAst = {
      val paramNode =
        parameterInNode(samInfo.expr, "this", "this", 0, false, EvaluationStrategies.BY_SHARING, samInfo.samImplClass)
          .dynamicTypeHintFullName(IndexedSeq(samInfo.samImplClass))
      Ast(paramNode)
    }

    val valueParameterAsts = samInfo.samMethodParams.zipWithIndex.map { case ((paramName, paramType), idx) =>
      val paramNode = parameterInNode(
        samInfo.expr,
        paramName,
        paramName,
        idx + 1,
        false,
        EvaluationStrategies.BY_VALUE,
        registerType(paramType)
      )
      Ast(paramNode)
    }

    val allParameterAsts = thisParamAst +: valueParameterAsts

    val paramString = samInfo.samMethodParams.map(_._1).mkString(", ")

    val returnType   = extractReturnTypeFromSignature(samInfo.samMethodSig)
    val methodReturn = methodReturnNode(samInfo.expr, registerType(returnType))

    val receiverType     = samInfo.receiverTypeFullName.getOrElse(TypeConstants.Any)
    val receiverTypeName = receiverType.split('.').last
    val calledMethodName = samInfo.methodRefName.split('.').last

    val callCode = samInfo.receiverParam match {
      case Some((_, receiverText)) if !samInfo.isStaticReference =>
        s"(receiver as ${receiverTypeName}).${calledMethodName}(${paramString})"
      case _ => samInfo.methodRefName
    }
    val blockNode_ = blockNode(samInfo.expr, s"return $callCode", TypeConstants.JavaLangVoid)

    val (dispatchType, receiverAstOpt) = (samInfo.receiverParam, samInfo.isStaticReference) match {
      case (Some((_, receiverText)), false) =>
        val thisIdent  = identifierNode(samInfo.expr, "this", "this", registerType(samInfo.samImplClass))
        val fieldIdent = fieldIdentifierNode(samInfo.expr, "receiver", "receiver")
        val receiverFieldAccess = callNode(
          samInfo.expr,
          "this.receiver",
          Operators.fieldAccess,
          Operators.fieldAccess,
          DispatchTypes.STATIC_DISPATCH,
          None,
          Some(TypeConstants.JavaLangObject)
        )
        val fieldAccessAst = callAst(receiverFieldAccess, Seq(Ast(thisIdent), Ast(fieldIdent)), None)

        // Wrap the field access in a cast operation
        val typeRefNode_ = typeRefNode(samInfo.expr, receiverTypeName, registerType(receiverType))
        val castCallNode = callNode(
          samInfo.expr,
          s"receiver as ${receiverTypeName}",
          Operators.cast,
          Operators.cast,
          DispatchTypes.STATIC_DISPATCH,
          None,
          Some(TypeConstants.Any)
        )
        val castAst = callAst(castCallNode, Seq(fieldAccessAst, Ast(typeRefNode_)), None)

        (DispatchTypes.DYNAMIC_DISPATCH, Some(castAst))
      case _ =>
        (DispatchTypes.STATIC_DISPATCH, None)
    }

    val callNode_ = callNode(
      samInfo.expr,
      callCode,
      samInfo.methodRefName.split("\\.").lastOption.getOrElse(samInfo.methodRefName),
      s"${samInfo.methodRefName}:${samInfo.signature}",
      dispatchType,
      Some(samInfo.signature),
      Some(registerType(returnType))
    )

    val callArgumentAsts = samInfo.samMethodParams.zipWithIndex.map { case ((paramName, paramType), idx) =>
      val identNode = identifierNode(samInfo.expr, paramName, paramName, registerType(paramType)).argumentIndex(idx + 1)
      astWithRefEdgeMaybe(paramName, identNode)
    }

    val callAst_ = callAst(callNode_, callArgumentAsts.toList, receiverAstOpt)

    val returnNode_ = returnNode(samInfo.expr, s"return ${callCode}")
    val returnAst_  = returnAst(returnNode_, List(callAst_))

    val modifiers =
      Seq(modifierNode(samInfo.expr, ModifierTypes.PUBLIC), modifierNode(samInfo.expr, ModifierTypes.VIRTUAL))
    val methodAst_ =
      methodAst(samMethodNode, allParameterAsts, blockAst(blockNode_, List(returnAst_)), methodReturn, modifiers)

    val samBinding = bindingNode(samInfo.samMethodName, samInfo.samMethodSig, samMethodFullName)
    bindingInfoQueue.prepend(
      BindingInfo(
        samBinding,
        Seq((samTypeDecl, samBinding, EdgeTypes.BINDS), (samBinding, samMethodNode, EdgeTypes.REF))
      )
    )

    // Create nested TypeDecl for the invoke method (represents the function type)
    val methodTypeDecl = typeDeclNode(
      samInfo.expr,
      samInfo.samMethodName,
      samMethodFullName,
      relativizedPath,
      samInfo.samMethodName,
      NodeTypes.TYPE_DECL,
      samInfo.samImplClass,
      Seq(registerType("kotlin.Function"))
    )

    // Create constructor for bound references
    val constructorAsts = if (samInfo.receiverParam.isDefined && !samInfo.isStaticReference) {
      val receiverType = samInfo.receiverTypeFullName.getOrElse(TypeConstants.Any)
      val ctorFullName = s"${samInfo.samImplClass}.<init>:void(${receiverType})"
      val ctorNode =
        methodNode(samInfo.expr, "<init>", "<init>", ctorFullName, Some(s"void(${receiverType})"), relativizedPath)

      val ctorThisParam =
        parameterInNode(samInfo.expr, "this", "this", 0, false, EvaluationStrategies.BY_SHARING, samInfo.samImplClass)
          .dynamicTypeHintFullName(IndexedSeq(samInfo.samImplClass))

      val receiverParamName = samInfo.receiverParam
        .flatMap(_._1.root.collect { case expr: ExpressionNew => expr.code })
        .getOrElse("receiver")

      val ctorReceiverParam = parameterInNode(
        samInfo.expr,
        receiverParamName,
        receiverParamName,
        1,
        false,
        EvaluationStrategies.BY_VALUE,
        registerType(receiverType)
      )

      val ctorBlock     = blockNode(samInfo.expr, "", registerType(TypeConstants.Void))
      val ctorReturn    = methodReturnNode(samInfo.expr, registerType(TypeConstants.Void))
      val ctorModifiers = Seq(modifierNode(samInfo.expr, ModifierTypes.CONSTRUCTOR))

      Seq(
        methodAst(ctorNode, Seq(Ast(ctorThisParam), Ast(ctorReceiverParam)), Ast(ctorBlock), ctorReturn, ctorModifiers)
      )
    } else Seq.empty

    Ast(samTypeDecl).withChild(methodAst_).withChildren(constructorAsts).withChild(Ast(methodTypeDecl))
  }

  private def extractReturnTypeFromSignature(signature: String): String = {
    val parenIndex = signature.indexOf('(')
    if (parenIndex > 0) {
      signature.substring(0, parenIndex)
    } else {
      TypeConstants.Any
    }
  }

  protected def exprTypeFullName(expr: KtExpression): Option[String] = {
    bindingUtils.getExprType(expr).flatMap(nameRenderer.typeFullName)
  }

  protected def fullNameByImportPath(typeRef: KtTypeReference, file: KtFile): Option[String] = {
    if (typeRef == null) { None }
    else {
      val typeRefText = typeRef.getText.stripSuffix("?")
      file.getImportList.getImports.asScala.collectFirst {
        case directive if directive.getImportedName != null && directive.getImportedName.toString == typeRefText =>
          directive.getImportPath.getPathStr
      }
    }
  }
}
