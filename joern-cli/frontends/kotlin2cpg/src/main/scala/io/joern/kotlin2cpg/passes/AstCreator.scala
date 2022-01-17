package io.joern.kotlin2cpg.passes

import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBinding,
  NewBlock,
  NewCall,
  NewClosureBinding,
  NewControlStructure,
  NewFieldIdentifier,
  NewFile,
  NewIdentifier,
  NewImport,
  NewJumpTarget,
  NewLiteral,
  NewLocal,
  NewMember,
  NewMethod,
  NewMethodParameterIn,
  NewMethodRef,
  NewMethodReturn,
  NewNamespaceBlock,
  NewNode,
  NewReturn,
  NewTypeDecl,
  NewTypeRef,
  NewUnknown
}
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  EdgeTypes,
  EvaluationStrategies,
  Operators
}
import io.shiftleft.passes.DiffGraph
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.x2cpg.Ast

import java.util.UUID.randomUUID
import scala.jdk.CollectionConverters._
import org.jetbrains.kotlin.psi.{
  KtAnnotatedExpression,
  KtAnonymousInitializer,
  KtArrayAccessExpression,
  KtBinaryExpression,
  KtBinaryExpressionWithTypeRHS,
  KtBlockExpression,
  KtBreakExpression,
  KtCallExpression,
  KtClass,
  KtClassLiteralExpression,
  KtClassOrObject,
  KtConstantExpression,
  KtContinueExpression,
  KtDeclaration,
  KtDoWhileExpression,
  KtDotQualifiedExpression,
  KtExpression,
  KtForExpression,
  KtIfExpression,
  KtImportDirective,
  KtIsExpression,
  KtLambdaArgument,
  KtLambdaExpression,
  KtNameReferenceExpression,
  KtNamedFunction,
  KtObjectDeclaration,
  KtObjectLiteralExpression,
  KtParameter,
  KtParenthesizedExpression,
  KtPostfixExpression,
  KtPrefixExpression,
  KtProperty,
  KtQualifiedExpression,
  KtReturnExpression,
  KtSafeQualifiedExpression,
  KtStringTemplateExpression,
  KtSuperExpression,
  KtThisExpression,
  KtThrowExpression,
  KtTryExpression,
  KtTypeAlias,
  KtTypeReference,
  KtValueArgumentList,
  KtWhenEntry,
  KtWhenExpression,
  KtWhileExpression
}
import com.intellij.psi.PsiElement
import io.joern.kotlin2cpg.KtFileWithMeta
import io.joern.kotlin2cpg.types.{BindingKinds, TypeInfoProvider, Constants => TypeConstants}
import org.jetbrains.kotlin.lexer.KtTokens
import org.slf4j.{Logger, LoggerFactory}

object Constants {
  val empty = "<empty>"
  val lambdaName = "<lambda>"
  val retCode = "RET"
  val tryCode = "try"
  val parserTypeName = "KOTLIN_PSI_PARSER"
  val caseNodePrefix = "case"
  val defaultCaseNode = "default"
  val caseNodeParserTypeName = "CaseNode"
  val unknownOperator = "<operator>.unknown"
  val operatorSuffix = "<operator>"
  val paramNameLambdaDestructureDecl = "DESTRUCTURE_PARAM"
  val wildcardImportName = "*"
}

case class ImportEntry(
    fqName: String,
    name: String,
    explicit: Boolean,
    isWildcard: Boolean = false,
    lineNumber: Int = -1,
    column: Int = -1
)

case class BindingInfo(node: NewBinding, edgeMeta: Seq[(NewNode, NewNode, String)])
case class ClosureBindingInfo(node: NewClosureBinding, edgeMeta: Seq[(NewNode, NewNode, String)])

// TODO: add description
case class Context(
    locals: Seq[NewLocal] = List(),
    identifiers: Seq[NewIdentifier] = List(),
    methodParameters: Seq[NewMethodParameterIn] = List(),
    bindingsInfo: Seq[BindingInfo] = List(),
    lambdaAsts: Seq[Ast] = List(),
    closureBindingInfo: Seq[ClosureBindingInfo] = List()
)

// TODO: add description
case class AstWithCtx(ast: Ast, ctx: Context)

// TODO: add description
case class ScopeContext(
    typeDecl: Option[NewTypeDecl] = None,
    methodParameters: Seq[NewMethodParameterIn] = List(),
    locals: Seq[NewLocal] = List()
)

// TODO: add description
case class FileInfo(imports: Seq[ImportEntry], classes: List[KtClass])

// TODO: add description
class AstCreator(
    fileWithMeta: KtFileWithMeta,
    xTypeInfoProvider: TypeInfoProvider,
    global: Global
) {

  // TODO: remove flag as soon as all ASTs without root are not being passed around any more
  // debug flag; when turned on, parsing continues even if an AST has no root
  // only here to help in paying back technical debt. Remove after
  private val continueParsingOnAstNodesWithoutRoot = false

  private val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder

  val relativizedPath = fileWithMeta.relativizedPath

  def createAst(): Iterator[DiffGraph] = {
    implicit val typeInfoProvider = xTypeInfoProvider
    logger.debug("Started parsing of file `" + fileWithMeta.filename + "`")
    storeInDiffGraph(astForFile(fileWithMeta))
    Iterator(diffGraph.build())
  }

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  /** Add `typeName` to a global map and return it. The
    * map is later passed to a pass that creates TYPE
    * nodes for each key in the map.
    */
  private def registerType(typeName: String): String = {
    global.usedTypes.put(typeName, true)
    typeName
  }

  private def storeInDiffGraph(astWithCtx: AstWithCtx): Unit = {
    val ast = astWithCtx.ast
    ast.nodes.foreach { node =>
      diffGraph.addNode(node)
    }
    ast.edges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.AST)
    }
    ast.conditionEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.CONDITION)
    }
    ast.refEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.REF)
    }
    ast.argEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.ARGUMENT)
    }
    ast.receiverEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.RECEIVER)
    }

    astWithCtx.ctx.bindingsInfo.foreach { bindingInfo =>
      diffGraph.addNode(bindingInfo.node)

      bindingInfo.edgeMeta.foreach { edgeMeta =>
        diffGraph.addEdge(edgeMeta._1, edgeMeta._2, edgeMeta._3)
      }
    }

    astWithCtx.ctx.closureBindingInfo.map { closureBindingInfo =>
      diffGraph.addNode(closureBindingInfo.node)

      closureBindingInfo.edgeMeta.foreach { edgeMeta =>
        diffGraph.addEdge(edgeMeta._1, edgeMeta._2, edgeMeta._3)
      }
    }
  }

  var lambdaCounter = 0

  private def nextLambdaName(): String = {
    lambdaCounter += 1
    "<lambda>" + lambdaCounter.toString
  }

  def withOrder[T <: Any, X](nodeList: java.util.List[T])(f: (T, Int) => X): Seq[X] = {
    nodeList.asScala.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }.toSeq
  }

  private def line(element: PsiElement): Int = {
    try {
      element
        .getContainingFile()
        .getViewProvider()
        .getDocument()
        .getLineNumber(element.getTextOffset())
    } catch {
      case _: Throwable => -1
    }
  }

  private def column(element: PsiElement): Int = {
    try {
      val lineNumber =
        element
          .getContainingFile()
          .getViewProvider()
          .getDocument()
          .getLineNumber(element.getTextOffset())
      val lineOffset =
        element.getContainingFile().getViewProvider().getDocument().getLineStartOffset(lineNumber)
      element.getTextOffset() - lineOffset
    } catch {
      case _: Throwable => -1
    }
  }

  private def astForFile(
      fileWithMeta: KtFileWithMeta
  )(implicit typeInfoProvider: TypeInfoProvider): AstWithCtx = {
    val ktFile = fileWithMeta.f
    val classDecls =
      ktFile.getDeclarations.asScala
        .filter(_.isInstanceOf[KtClass])
        .map(_.asInstanceOf[KtClass])
        .toList

    val allImports =
      combinedImports(ktFile.getImportList().getImports().asScala.toList)
    implicit val fileInfo = FileInfo(allImports, classDecls)

    val importAsts =
      withOrder(allImports.asJava) { (entry, order) =>
        astForImportEntry(entry, order)
      }

    val lastImportOrder = importAsts.size
    var idxEpsilon = 0 // when multiple AST nodes are returned by `astForDeclaration`
    val declarationsAsts =
      withOrder(ktFile.getDeclarations()) { (decl, order) =>
        val asts = astForDeclaration(decl, ScopeContext(), order + lastImportOrder + idxEpsilon)
        idxEpsilon += asts.size - 1
        asts
      }.flatten

    val fileNode =
      NewFile()
        .name(fileWithMeta.relativizedPath)
        .order(0)
    val ast =
      Ast(fileNode)
        .withChild(
          astForPackageDeclaration(ktFile.getPackageFqName.toString).ast
            .withChildren(importAsts.map(_.ast))
            .withChildren(declarationsAsts.map(_.ast))
            .withChildren(mergedCtx(declarationsAsts.map(_.ctx)).lambdaAsts)
        )
    AstWithCtx(ast, mergedCtx(declarationsAsts.map(_.ctx)))
  }

  def combinedImports(
      explicitImports: Seq[KtImportDirective]
  ): Seq[ImportEntry] = {
    val explicitImportEntries =
      explicitImports.map { entry =>
        // TODO: add more test cases for import directives
        // e.g. of a project where parsing fails with NPE if the null check in isWildcard is not in:
        // https://github.com/CypherpunkArmory/UserLAnd
        val isWildcard = entry.getLastChild.getText() == "*" || entry.getImportedName() == null
        val importedName =
          if (isWildcard) {
            Constants.wildcardImportName
          } else {
            entry.getImportedName().toString
          }

        ImportEntry(
          entry.getText().replaceAll("^import ", ""),
          importedName,
          true,
          isWildcard,
          line(entry),
          column(entry)
        )
      }
    explicitImportEntries
  }

  def astForImportEntry(entry: ImportEntry, order: Int): AstWithCtx = {
    // TODO: check for import as
    // TODO: check for the wildcard stuff
    val node =
      NewImport()
        .isWildcard(entry.isWildcard)
        .isExplicit(entry.explicit)
        .importedEntity(entry.fqName)
        .code("import " + entry.fqName)
        .order(order)
        .lineNumber(entry.lineNumber)
        .columnNumber(entry.column)
    AstWithCtx(Ast(node), Context())
  }

  def astForPackageDeclaration(packageName: String): AstWithCtx = {
    val namespaceBlock =
      packageName match {
        case "<root>" =>
          NewNamespaceBlock()
            .name(NamespaceTraversal.globalNamespaceName)
            .fullName(NamespaceTraversal.globalNamespaceName)
        case _ =>
          val name = packageName.split("\\.").lastOption.getOrElse("")
          NewNamespaceBlock()
            .name(name)
            .fullName(packageName)
      }
    AstWithCtx(Ast(namespaceBlock.filename(relativizedPath).order(1)), Context())
  }

  def astForDeclaration(decl: KtDeclaration, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithCtx] = {
    decl match {
      case d: KtClass             => Seq(astForClassOrObject(d, order))
      case d: KtObjectDeclaration => Seq(astForClassOrObject(d, order))
      case d: KtNamedFunction     => Seq(astForMethod(d, scopeContext, order))
      case d: KtTypeAlias         => Seq(astForTypeAlias(d, order))
      case d: KtProperty          => astForTopLevelProperty(d, scopeContext, order)
      case unhandled =>
        logger.error(
          "Unknown declaration type encountered with text `" + unhandled.getText + "` and class `" + unhandled.getClass + "`!"
        )
        Seq()
    }
  }

  // TODO: lower them
  def astForTopLevelProperty(prop: KtProperty, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ) = {
    Seq()
  }

  def astForTypeAlias(typeAlias: KtTypeAlias, order: Int)(implicit
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val fullName = typeInfoProvider.fullName(typeAlias, TypeConstants.any)
    val aliasTypeFullName = typeInfoProvider.aliasTypeFullName(typeAlias, TypeConstants.any)
    registerType(fullName)
    registerType(aliasTypeFullName)

    val typeDecl =
      NewTypeDecl()
        .code(typeAlias.getText)
        .name(typeAlias.getName)
        .fullName(fullName)
        .aliasTypeFullName(Some(aliasTypeFullName))
        .order(order)
        .isExternal(false)
        .lineNumber(line(typeAlias))
        .columnNumber(column(typeAlias))
        .filename(relativizedPath)
    AstWithCtx(Ast(typeDecl), Context())
  }

  def astForClassOrObject(ktClass: KtClassOrObject, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val className = ktClass.getName()
    val explicitFullName = {
      val fqName = ktClass.getContainingKtFile.getPackageFqName.toString
      fqName + "." + className
    }
    val fullName = typeInfoProvider.fullName(ktClass, explicitFullName)
    registerType(fullName)

    val explicitBaseTypeFullNames =
      ktClass
        .getSuperTypeListEntries()
        .asScala
        .map(_.getTypeAsUserType)
        .filterNot(
          _ == null
        ) // TODO: write test and pick up code from git@github.com:RedApparat/Fotoapparat.git
        .map(_.getText)
        .toList
    val baseTypeFullNames = typeInfoProvider.inheritanceTypes(ktClass, explicitBaseTypeFullNames)
    val typeDecl =
      NewTypeDecl()
        .code(ktClass.getName)
        .name(className)
        .fullName(fullName)
        .order(order)
        .inheritsFromTypeFullName(baseTypeFullNames)
        .isExternal(false)
        .lineNumber(line(ktClass))
        .columnNumber(column(ktClass))
        .filename(relativizedPath)
    val classFunctions =
      if (ktClass.getBody != null) {
        ktClass
          .getBody()
          .getFunctions()
          .asScala
          .filter { decl => decl.isInstanceOf[KtNamedFunction] }
          .asJava
      } else {
        List().asJava
      }
    val classDeclarations =
      if (ktClass.getBody != null) {
        ktClass
          .getBody()
          .getDeclarations()
          .asScala
          .filter { decl => !decl.isInstanceOf[KtNamedFunction] }
          .asJava
      } else {
        List().asJava
      }
    val blockInitializers =
      if (ktClass.getBody != null) {
        ktClass.getBody.getAnonymousInitializers()
      } else {
        List().asJava
      }

    val scopeCtx = ScopeContext(typeDecl = Some(typeDecl))
    val methodAstsWithCtx =
      withOrder(classFunctions) { (method, order) =>
        astForMethod(method, scopeCtx, order)
      }

    val bindingsInfo =
      methodAstsWithCtx.map(_.ast).map { ast =>
        // TODO: add a try catch here
        val methodNode = ast.root.get.asInstanceOf[NewMethod]
        val node =
          NewBinding()
            .name(methodNode.name)
            .signature(methodNode.signature)
        BindingInfo(
          node,
          List((typeDecl, node, EdgeTypes.BINDS), (node, ast.root.get, EdgeTypes.REF))
        )
      }

    val ctorOrder = 1
    val constructorParams = ktClass.getPrimaryConstructorParameters().asScala.toList
    val constructorMethod =
      NewMethod()
        .name(className)
        .fullName(fullName + ":" + erasedSignature(constructorParams))
        .isExternal(false)
        .order(ctorOrder)
        .filename(relativizedPath)
        .lineNumber(line(ktClass.getPrimaryConstructor()))
        .columnNumber(column(ktClass.getPrimaryConstructor()))
    val constructorParamsWithCtx =
      withOrder(constructorParams.asJava) { (p, order) =>
        astForParameter(p, order)
      }

    val constructorMethodReturn =
      NewMethodReturn()
        .order(1)
        .evaluationStrategy(EvaluationStrategies.BY_VALUE)
        .typeFullName(TypeConstants.any)
        .dynamicTypeHintFullName(Some(fullName))
        .code(Constants.retCode)
        .lineNumber(line(ktClass.getPrimaryConstructor()))
        .columnNumber(column(ktClass.getPrimaryConstructor()))
    val constructorAst =
      Ast(constructorMethod)
        .withChildren(constructorParamsWithCtx.map(_.ast))
        .withChild(Ast(constructorMethodReturn))

    val secondaryConstructorAsts =
      withOrder(ktClass.getSecondaryConstructors()) { (secondaryCtor, order) =>
        val constructorParams = secondaryCtor.getValueParameters().asScala.toList
        val constructorMethod =
          NewMethod()
            .name(className)
            .fullName(fullName + ":" + erasedSignature(constructorParams))
            .isExternal(false)
            .order(ctorOrder + order)
            .filename(relativizedPath)
            .lineNumber(line(secondaryCtor))
            .columnNumber(column(secondaryCtor))
        val constructorMethodReturn =
          NewMethodReturn()
            .order(1)
            .evaluationStrategy(EvaluationStrategies.BY_VALUE)
            .typeFullName(TypeConstants.any)
            .dynamicTypeHintFullName(Some(fullName))
            .code(Constants.retCode)
            .lineNumber(line(secondaryCtor))
            .columnNumber(column(secondaryCtor))

        val constructorParamsWithCtx =
          withOrder(constructorParams.asJava) { (p, order) =>
            astForParameter(p, order)
          }
        val constructorAst =
          Ast(constructorMethod)
            .withChildren(constructorParamsWithCtx.map(_.ast))
            .withChild(Ast(constructorMethodReturn))
        constructorAst
      }

    val orderAfterCtors = ctorOrder + secondaryConstructorAsts.size
    val ast =
      Ast(typeDecl)
        .withChildren(methodAstsWithCtx.map(_.ast))
        .withChild(constructorAst)
        .withChildren(secondaryConstructorAsts)
        // TODO: reenable initializer block parsing when methodReturn nodes have been added
        // otherwise the CfgCreator throws
        /*
        .withChildren(
          withOrder(blockInitializers) { (initializer, order) =>
            astForInitializerBlock(initializer, order).ast
          }
        )
         */
        .withChildren(
          withOrder(classDeclarations) { (method, order) =>
            astForMember(method, orderAfterCtors + order)
          }
        )

    val finalCtx = mergedCtx(methodAstsWithCtx.map(_.ctx) ++ List(Context(bindingsInfo = bindingsInfo)))
    AstWithCtx(ast, finalCtx)
  }

  private def astForInitializerBlock(
      initBlock: KtAnonymousInitializer,
      scopeContext: ScopeContext,
      order: Int
  )(implicit fileInfo: FileInfo, typeInfoProvider: TypeInfoProvider): AstWithCtx = {
    val methodNode =
      NewMethod()
        .name("PLACEHOLDER_INIT_NAME")
        .fullName("PLACHOLDER_INIT_FULLNAME")
        .isExternal(false)
        .order(order)
        .filename(relativizedPath)
        .lineNumber(line(initBlock.getBody))
        .columnNumber(column(initBlock.getBody))

    val blockOrder = 1
    val block =
      NewBlock()
        .order(blockOrder)
        .lineNumber(line(initBlock.getBody))
        .columnNumber(column(initBlock.getBody))
        .code(initBlock.getText)
    val bodyAsts = astsForExpression(initBlock.getBody, scopeContext, blockOrder + 1, blockOrder + 1)
    val blockAst =
      Ast(block)
        .withChildren(bodyAsts.map(_.ast))
    val ast =
      Ast(methodNode)
        .withChild(blockAst)
    AstWithCtx(ast, Context())
  }

  private def astForMethod(
      ktFn: KtNamedFunction,
      scopeContext: ScopeContext,
      childNum: Int
  )(implicit fileInfo: FileInfo, typeInfoProvider: TypeInfoProvider): AstWithCtx = {

    // TODO: add the annotations as soon as they're part of the open source schema
    // ktFn.getModifierList.getAnnotationEntries().asScala.map(_.getText)
    //
    val typeDeclName =
      if (scopeContext.typeDecl.isDefined) {
        scopeContext.typeDecl.get.fullName
      } else {
        ""
      }
    val methodNode = createMethodNode(ktFn, childNum)
    val parametersWithCtx =
      withOrder(ktFn.getValueParameters()) { (p, order) =>
        astForParameter(p, order)
      }

    val mergedScopeContext =
      parametersWithCtx.foldLeft(scopeContext)((acc, paramWithCtx) => {
        val params = paramWithCtx.ctx.methodParameters
        val locals = paramWithCtx.ctx.locals
        ScopeContext(None, params ++ acc.methodParameters, locals ++ acc.locals)
      })

    val lastOrder = parametersWithCtx.size + 2
    val bodyAstWithCtx =
      astForMethodBody(ktFn.getBodyBlockExpression(), mergedScopeContext, lastOrder)
    val returnAst = astForMethodReturn(ktFn, lastOrder + 1)

    val paramMap =
      parametersWithCtx
        .map(_.ctx.methodParameters)
        .flatten
        .map { methodParam => methodParam.name -> methodParam }
        .toMap
    val identifierMatchingParams =
      bodyAstWithCtx.ctx.identifiers
        .filter { ident => paramMap.isDefinedAt(ident.name) }
        .map { ident => (ident, paramMap(ident.name)) }
    val nodesForRefEdges = identifierMatchingParams

    val ast =
      Ast(methodNode)
        .withChildren(parametersWithCtx.map(_.ast))
        .withChild(bodyAstWithCtx.ast)
        .withChild(returnAst)
    val astWithRefEdges = {
      nodesForRefEdges.foldLeft(ast)((acc, nodes) => {
        acc.withRefEdge(nodes._1, nodes._2)
      })
    }
    val finalCtx = mergedCtx(Seq(bodyAstWithCtx.ctx))
    AstWithCtx(astWithRefEdges, finalCtx)
  }

  private def mergedCtx(ctxs: Seq[Context]): Context = {
    ctxs.foldLeft(Context())((acc, ctx) => {
      val locals = acc.locals ++ ctx.locals
      val identifiers = acc.identifiers ++ ctx.identifiers
      val methodParameters = acc.methodParameters ++ ctx.methodParameters
      val bindingsInfo = acc.bindingsInfo ++ ctx.bindingsInfo
      val lambdaAsts = acc.lambdaAsts ++ ctx.lambdaAsts
      val closureBindingInfo = acc.closureBindingInfo ++ ctx.closureBindingInfo
      Context(locals, identifiers, methodParameters, bindingsInfo, lambdaAsts, closureBindingInfo)
    })
  }

  private def astForMethodReturn(ktFn: KtNamedFunction, order: Int)(implicit
      typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val explicitTypeName =
      if (ktFn.getTypeReference() != null) { // TODO: use `Option` for these types of checks
        ktFn.getTypeReference().getText()
      } else {
        "" // TODO: add test case for this scenario; maybe replace with Constants.Any
      }
    val typeFullName = typeInfoProvider.returnType(ktFn, explicitTypeName)
    registerType(typeFullName)

    val node =
      NewMethodReturn()
        .order(order)
        .evaluationStrategy(EvaluationStrategies.BY_VALUE)
        .typeFullName(typeFullName)
        .code(typeFullName)
        .lineNumber(line(ktFn))
        .columnNumber(column(ktFn))
    Ast(node)
  }

  private def astForMethodBody(body: KtBlockExpression, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    body match {
      case blockExpr if blockExpr != null => astForBlock(blockExpr, scopeContext, order)
      case _ =>
        val blockNode = NewBlock()
        AstWithCtx(Ast(blockNode), Context())
    }
  }

  private def astForBlock(expr: KtBlockExpression, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    val block =
      NewBlock()
        .order(order)
        .argumentIndex(order)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .code(expr.getStatements().asScala.map(_.getText).mkString("\n"))
        .typeFullName(typeFullName)

    var orderRemainder = 0
    var locals = List[NewLocal]()
    val expressions =
      withOrder(expr.getStatements()) { (statement, order) =>
        val mergedScopeContext =
          ScopeContext(None, scopeContext.methodParameters, scopeContext.locals ++ locals)
        val asts = astsForExpression(statement, mergedScopeContext, order + orderRemainder, order + orderRemainder)
        locals = locals ++ asts
          .filterNot { astWithCtx =>
            val hasNullRoot = astWithCtx.ast.root == null
            if (hasNullRoot) {
              // TODO: find all the spots which creates these, add test for them, and fix one by one
              // >>>> this is a big pile of tech debt shoved under the rug
              logger.warn("Filtered out expression which cannot be parsed correctly at the moment.")
              logger.debug("  > reason: ast root not present.")
            }
            hasNullRoot || continueParsingOnAstNodesWithoutRoot
          }
          .filter(_.ast.root.get.isInstanceOf[NewLocal])
          .map(_.ast.root.get.asInstanceOf[NewLocal])

        orderRemainder = if (asts.size > 1) {
          orderRemainder + asts.size - 1
        } else {
          orderRemainder
        }
        asts
      }.flatten

    val childrenCtx = mergedCtx(expressions.map(_.ctx))
    val localExprs =
      expressions
        .filter { expr => expr.ast.root.get.isInstanceOf[NewLocal] }
        .map { expr => expr.ast.root.get.asInstanceOf[NewLocal] }
    val identifiersMatchingLocals =
      childrenCtx.identifiers
        .filter { identifier =>
          localExprs.map(_.name).contains(identifier.name)
        }
        // If identifiers are on the same line as locals, it means that
        // they're referencing a declaration, and the REF edges are already added
        // when created, so just ignore them
        .filter { identifier =>
          val matchingLocal = localExprs.filter { _.name == identifier.name }.head
          identifier.lineNumber != matchingLocal.lineNumber
        }
        .map { identifier =>
          val matchingLocal = localExprs.filter { _.name == identifier.name }.head
          (identifier, matchingLocal)
        }
    val identifiersNotMatchingLocals =
      childrenCtx.identifiers
        .filterNot { identifier =>
          localExprs.map(_.name).contains(identifier.name)
        }

    val childrenCtxMinusMatchedIdentifiers =
      Context(
        childrenCtx.locals,
        identifiersNotMatchingLocals,
        childrenCtx.methodParameters,
        childrenCtx.bindingsInfo,
        childrenCtx.lambdaAsts,
        childrenCtx.closureBindingInfo
      )
    val ast = Ast(block)
      .withChildren(expressions.map(_.ast))
    val astWithRefEdges =
      identifiersMatchingLocals.foldLeft(ast)((acc, nodes) => {
        acc.withRefEdge(nodes._1, nodes._2)
      })
    AstWithCtx(astWithRefEdges, childrenCtxMinusMatchedIdentifiers)
  }

  private def astsForReturnNode(expr: KtReturnExpression, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithCtx] = {
    val returnNode =
      NewReturn()
        .code(expr.getText())
        .order(order)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val child = astsForExpression(expr.getReturnedExpression(), scopeContext, 1, 1).headOption
      .getOrElse(AstWithCtx(Ast(), Context()))
    val ast =
      Ast(returnNode)
        .withChild(child.ast)
        .withArgEdges(returnNode, child.ast.root.toList)
    Seq(
      AstWithCtx(ast, child.ctx)
    )
  }

  def astForIsExpression(expr: KtIsExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val retType = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(retType)

    val callNode =
      NewCall()
        .name(Operators.is)
        .methodFullName(Operators.is)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText())
        .argumentIndex(argIdx)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .order(order)
        .typeFullName(retType)
    val args =
      astsForExpression(expr.getLeftHandSide(), scopeContext, 1, 1) ++ Seq(
        astForTypeReference(expr.getTypeReference(), scopeContext, 2, 2)
      )
    val ast = callAst(callNode, args.map(_.ast))
    AstWithCtx(ast, mergedCtx(args.map(_.ctx)))
  }

  def astForBinaryExprWithTypeRHS(
      expr: KtBinaryExpressionWithTypeRHS,
      scopeContext: ScopeContext,
      order: Int,
      argIdx: Int
  )(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val callNode =
      NewCall()
        .name(Operators.cast)
        .methodFullName(Operators.cast)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText())
        .argumentIndex(argIdx)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .order(order)
        .typeFullName(typeFullName)
    val args =
      astsForExpression(expr.getLeft(), scopeContext, 1, 1) ++ Seq(
        astForTypeReference(expr.getRight(), scopeContext, 2, 2)
      )
    val ast = callAst(callNode, args.map(_.ast))
    AstWithCtx(ast, mergedCtx(args.map(_.ctx)))
  }

  private def astForTypeReference(expr: KtTypeReference, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
      fileInfo: FileInfo
  ): AstWithCtx = {
    val node =
      NewTypeRef()
        .code(expr.getText)
        .order(order)
        .argumentIndex(argIdx)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    AstWithCtx(Ast(node), Context())
  }

  private def astsForExpression(expr: KtExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithCtx] = {
    expr match {
      case blockStmt: KtBlockExpression    => List(astForBlock(blockStmt, scopeContext, order))
      case returnExpr: KtReturnExpression  => astsForReturnNode(returnExpr, scopeContext, order)
      case typedExpr: KtCallExpression     => Seq(astForCall(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtConstantExpression => Seq(astForLiteral(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtBinaryExpression   => Seq(astForBinaryExpr(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtIsExpression       => Seq(astForIsExpression(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtBinaryExpressionWithTypeRHS =>
        Seq(astForBinaryExprWithTypeRHS(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtNameReferenceExpression if typedExpr.getReferencedNameElementType() == KtTokens.IDENTIFIER =>
        Seq(astForIdentifier(typedExpr, order, argIdx))
      case _: KtNameReferenceExpression =>
        // TODO: handle this
        Seq()
      case typedExpr: KtProperty if typedExpr.isLocal() =>
        astsForProperty(typedExpr, scopeContext, order)
      case typedExpr: KtIfExpression       => Seq(astForIf(typedExpr, scopeContext, order))
      case typedExpr: KtWhenExpression     => Seq(astForWhen(typedExpr, scopeContext, order))
      case typedExpr: KtForExpression      => Seq(astForFor(typedExpr, scopeContext, order))
      case typedExpr: KtWhileExpression    => Seq(astForWhile(typedExpr, scopeContext, order))
      case typedExpr: KtDoWhileExpression  => Seq(astForDoWhile(typedExpr, scopeContext, order))
      case typedExpr: KtTryExpression      => Seq(astForTry(typedExpr, scopeContext, order))
      case typedExpr: KtBreakExpression    => Seq(astForBreak(typedExpr, scopeContext, order))
      case typedExpr: KtContinueExpression => Seq(astForContinue(typedExpr, scopeContext, order))
      case typedExpr: KtDotQualifiedExpression =>
        Seq(astForQualifiedExpression(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtStringTemplateExpression =>
        Seq(astForStringTemplate(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtPrefixExpression =>
        Seq(astForPrefixExpression(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtPostfixExpression =>
        Seq(astForPostfixExpression(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtParenthesizedExpression =>
        astsForExpression(typedExpr.getExpression(), scopeContext, order, argIdx)
      case typedExpr: KtArrayAccessExpression =>
        Seq(astForArrayAccess(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtLambdaExpression =>
        Seq(astForLambda(typedExpr, scopeContext, order))
      case typedExpr: KtNamedFunction =>
        Seq(astForAnonymousFunction(typedExpr, scopeContext, order))
      case classExpr: KtClassLiteralExpression =>
        Seq(astForClassLiteral(classExpr, scopeContext, order, argIdx))
      case sqExpr: KtSafeQualifiedExpression =>
        Seq(astForQualifiedExpression(sqExpr, scopeContext, order, argIdx))
      case typedExpr: KtThisExpression =>
        Seq(astForThisExpression(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtAnnotatedExpression =>
        astsForExpression(typedExpr.getBaseExpression, scopeContext, order, argIdx)
      case typedExpr: KtObjectLiteralExpression =>
        // TODO: handle properly
        Seq(astForUnknown(typedExpr, order, argIdx))
      case typedExpr: KtThrowExpression =>
        Seq(astForUnknown(typedExpr, order, argIdx))
      case null =>
        logger.debug("Received null expression! Skipping...")
        Seq()
      case unknownExpr =>
        logger.debug(
          "Creating empty AST node for unknown expression `" + unknownExpr.getClass + "` with text `" + unknownExpr.getText + "`"
        )
        Seq(astForUnknown(unknownExpr, order, argIdx))
    }
  }

  def astForThisExpression(expr: KtThisExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val node =
      NewIdentifier()
        .name(expr.getText)
        .code(expr.getText)
        .typeFullName(typeFullName)
        .order(order)
        .argumentIndex(argIdx)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    AstWithCtx(Ast(node), Context())
  }

  def astForClassLiteral(expr: KtClassLiteralExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val getClassMethodName = "getClass"
    val receiverName = expr.getReceiverExpression.getText
    val signature = "java.lang.Class()"
    val methodFullName = receiverName + "." + getClassMethodName + ":" + signature

    val fullNameWithSignature = typeInfoProvider.fullNameWithSignature(expr, ("", ""))
    val typeFullName = typeInfoProvider.expressionType(expr, "java.lang.Class")
    registerType(typeFullName)

    val callNode =
      NewCall()
        .name(getClassMethodName)
        .code(expr.getText())
        .order(order)
        .argumentIndex(argIdx)
        .methodFullName(fullNameWithSignature._1)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .signature(fullNameWithSignature._2)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .typeFullName(typeFullName)
    AstWithCtx(Ast(callNode), Context())
  }

  // TODO: try to merge with astForLambda if possible
  def astForAnonymousFunction(expr: KtNamedFunction, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val containingFile = expr.getContainingKtFile()
    val fullName = containingFile.getPackageFqName.toString() + ":" + nextLambdaName()
    val signature = erasedSignature(expr.getValueParameters().asScala.toList)
    val lambdaMethod =
      NewMethod()
        .name(Constants.lambdaName)
        .code("")
        .isExternal(false)
        .fullName(fullName)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .signature(signature)
        .filename(relativizedPath)

    val parametersWithCtx =
      withOrder(expr.getValueParameters()) { (p, order) =>
        astForParameter(p, order)
      }
    val lastOrder = parametersWithCtx.size + 2

    val bodyAstWithCtx = astForMethodBody(expr.getBodyBlockExpression, scopeContext, lastOrder)

    val methodReturnNode =
      NewMethodReturn()
        .order(lastOrder + 1)
        .evaluationStrategy(EvaluationStrategies.BY_VALUE)
        .typeFullName(TypeConstants.any)
        .code(Constants.retCode)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val lambdaMethodAst =
      Ast(lambdaMethod)
        .withChildren(parametersWithCtx.map(_.ast))
        .withChild(bodyAstWithCtx.ast)
        .withChild(Ast(methodReturnNode))

    val methodRef =
      NewMethodRef()
        .code("")
        .methodFullName(fullName)
        .typeFullName(TypeConstants.any)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .order(order)
    val methodRefAst = Ast(methodRef)

    AstWithCtx(
      methodRefAst,
      Context(lambdaAsts = Seq(lambdaMethodAst), identifiers = bodyAstWithCtx.ctx.identifiers)
    )
  }

  def astForLambda(expr: KtLambdaExpression, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val containingFile = expr.getContainingKtFile()
    val fullName = containingFile.getPackageFqName().toString + ":" + nextLambdaName()
    val signature = erasedSignature(expr.getValueParameters().asScala.toList)

    // TODO: use typeInfoProvider for the fullName and signature
    val lambdaMethod =
      NewMethod()
        .name(Constants.lambdaName)
        .code("")
        .isExternal(false)
        .fullName(fullName)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .signature(signature)
        .filename(relativizedPath)

    val parametersWithCtx =
      withOrder(expr.getValueParameters()) { (p, order) =>
        astForParameter(p, order)
      }
    val lastOrder = parametersWithCtx.size + 2

    val bodyAstWithCtx = astForMethodBody(expr.getBodyExpression, scopeContext, lastOrder)
    val bodyIndentifiers = bodyAstWithCtx.ctx.identifiers // TODO: delete refs to global decls

    val namesToMethodParams =
      parametersWithCtx
        .map(_.ast)
        .filter(_.root.get.isInstanceOf[NewMethodParameterIn])
        .map { paramAst =>
          val node = paramAst.root.get.asInstanceOf[NewMethodParameterIn]
          node.name -> paramAst
        }
        .toMap
    val (identifiersMatchingMethodParams, identifiersNotMatchingMethodParams) = {
      bodyAstWithCtx.ctx.identifiers.partition { ident =>
        namesToMethodParams.contains(ident.name)
      }
    }
    val refEdgePairs =
      identifiersMatchingMethodParams.map { ident =>
        val methodParamInNode =
          namesToMethodParams.get(ident.name).get.root.get.asInstanceOf[NewMethodParameterIn]
        (ident, methodParamInNode)
      }

    // TODO: delete refs to METHOD_PARAMETER
    val closureBindings =
      identifiersNotMatchingMethodParams.map { ident =>
        val closureBindingId = randomUUID().toString
        val closure =
          NewClosureBinding()
            .closureBindingId(Some(closureBindingId))
            .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
            .closureOriginalName(Some(ident.name))
        (ident, closure, closureBindingId)
      }
    val localsForCapturedIdentifiers =
      closureBindings.zipWithIndex.map { case (bindingWithInfo, idx) =>
        Ast(
          NewLocal()
            .name(bindingWithInfo._1.name)
            .code(bindingWithInfo._1.code)
            .typeFullName(bindingWithInfo._1.typeFullName)
            .lineNumber(bindingWithInfo._1.lineNumber)
            .columnNumber(bindingWithInfo._1.columnNumber)
            .closureBindingId(bindingWithInfo._3)
            .order(idx + 1)
        )
      }

    val methodRef =
      NewMethodRef()
        .code("")
        .methodFullName(fullName)
        .typeFullName(TypeConstants.any)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .order(order)
    val methodReturnNode =
      NewMethodReturn()
        .order(lastOrder + 1)
        .evaluationStrategy(EvaluationStrategies.BY_VALUE)
        .typeFullName(TypeConstants.any)
        .code(Constants.retCode)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val lambdaMethodAst =
      Ast(lambdaMethod)
        .withChildren(parametersWithCtx.map(_.ast))
        .withChild(bodyAstWithCtx.ast.withChildren(localsForCapturedIdentifiers))
        .withChild(Ast(methodReturnNode))
    val lamdbaMethodAstWithRefEdges =
      refEdgePairs.foldLeft(lambdaMethodAst)((acc, nodes) => {
        acc.withRefEdge(nodes._1, nodes._2)
      })

    val methodRefAst =
      Ast(methodRef)

    // TODO: improve matching
    // TODO: remove the matched identifiers from the returned context
    val nameToScopeMethodParameter = scopeContext.methodParameters.map { p => p.name -> p }.toMap
    val nameToScopeLocal = scopeContext.locals.map { p => p.name -> p }.toMap
    val closureBindingInfo =
      closureBindings.map { closureBindingInfo =>
        val edges = List((methodRef, closureBindingInfo._2, EdgeTypes.CAPTURE))
        val refEdges =
          if (nameToScopeMethodParameter.contains(closureBindingInfo._1.name)) {
            val param = nameToScopeMethodParameter(closureBindingInfo._1.name)
            List((closureBindingInfo._2, param, EdgeTypes.REF))
          } else if (nameToScopeLocal.contains(closureBindingInfo._1.name)) {
            val local = nameToScopeLocal(closureBindingInfo._1.name)
            List((closureBindingInfo._2, local, EdgeTypes.REF))
          } else {
            List()
          }
        ClosureBindingInfo(closureBindingInfo._2, edges ++ refEdges)
      }

    AstWithCtx(
      methodRefAst,
      Context(
        lambdaAsts = Seq(lamdbaMethodAstWithRefEdges) ++ bodyAstWithCtx.ctx.lambdaAsts,
        identifiers = bodyAstWithCtx.ctx.identifiers,
        closureBindingInfo = closureBindingInfo ++ bodyAstWithCtx.ctx.closureBindingInfo
      )
    )
  }

  def astForArrayAccess(expr: KtArrayAccessExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val identifierElem = expr.getArrayExpression
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val identifier =
      NewIdentifier()
        .name(identifierElem.getText())
        .order(1)
        .argumentIndex(1)
        .code(identifierElem.getText())
        .typeFullName(typeFullName)
        .lineNumber(line(identifierElem))
        .columnNumber(column(identifierElem))
    val indexExpr =
      if (expr.getIndexExpressions.size == 1) {
        expr.getIndexExpressions.get(0)
      } else {
        None
      }
    if (indexExpr == None) {
      // TODO: check if this should be handled differently,
      // i.e. not return an empty val but throwing an exception
      AstWithCtx(Ast(), Context())
    } else if (indexExpr.isInstanceOf[KtConstantExpression]) {
      val assignment =
        NewCall()
          .name(Operators.indexAccess)
          .dispatchType(DispatchTypes.STATIC_DISPATCH)
          .code(expr.getText())
          .order(order)
          .argumentIndex(argIdx)
          .typeFullName(typeFullName)
          .methodFullName(Operators.indexAccess)
          .lineNumber(line(expr))
          .columnNumber(column(expr))
      val call = callAst(assignment, Seq(Ast(identifier)))
      AstWithCtx(call, Context(identifiers = List(identifier)))
    } else {
      val assignment =
        NewCall()
          .name(Operators.indexAccess)
          .dispatchType(DispatchTypes.STATIC_DISPATCH)
          .code(expr.getText())
          .order(order)
          .argumentIndex(argIdx)
          .typeFullName(typeFullName)
          .methodFullName(Operators.indexAccess)
          .lineNumber(line(expr))
          .columnNumber(column(expr))
      val call = callAst(assignment, Seq(Ast(identifier)))
      AstWithCtx(call, Context(identifiers = List(identifier)))
    }
  }

  def astForPostfixExpression(expr: KtPostfixExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val operatorType =
      expr.getOperationToken() match {
        case KtTokens.PLUSPLUS   => Operators.postIncrement
        case KtTokens.MINUSMINUS => Operators.postDecrement
        case KtTokens.EXCLEXCL   => Operators.notNullAssert
        case _ =>
          logger.warn(
            "Creating empty AST node for unknown postfix expr: " + expr.getOperationToken()
          )
          Constants.unknownOperator
      }

    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val callNode =
      NewCall()
        .name(operatorType)
        .methodFullName(operatorType)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText())
        .argumentIndex(argIdx)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .order(order)
        .typeFullName(typeFullName)
    val args = List(
      astsForExpression(expr.getBaseExpression, scopeContext, 1, 1).headOption
        .getOrElse(AstWithCtx(Ast(), Context()))
    ).filterNot(_.ast.root == null)
    val ast = callAst(callNode, args.map(_.ast))
    AstWithCtx(ast, mergedCtx(args.map(_.ctx)))
  }

  def astForPrefixExpression(expr: KtPrefixExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val operatorType =
      expr.getOperationToken() match {
        case KtTokens.EXCL       => Operators.logicalNot
        case KtTokens.PLUS       => Operators.plus
        case KtTokens.MINUS      => Operators.minus
        case KtTokens.PLUSPLUS   => Operators.preIncrement
        case KtTokens.MINUSMINUS => Operators.preDecrement
        case _ =>
          logger.warn(
            "Creating empty AST node for unknown prefix expr: " + expr.getOperationToken()
          )
          Constants.unknownOperator
      }
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val callNode =
      NewCall()
        .name(operatorType)
        .methodFullName(operatorType)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText())
        .argumentIndex(argIdx)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .order(order)
        .typeFullName(typeFullName)
    val args = List(
      astsForExpression(expr.getBaseExpression, scopeContext, 1, 1).headOption
        .getOrElse(AstWithCtx(Ast(), Context()))
    ).filterNot(_.ast.root == null)
    val ast = callAst(callNode, args.map(_.ast))
    AstWithCtx(ast, mergedCtx(args.map(_.ctx)))
  }

  def astForUnknown(expr: KtExpression, order: Int, argIdx: Int): AstWithCtx = {
    val unknownNode =
      NewUnknown()
        .code(if (expr != null) { expr.getText }
        else { null })
        .parserTypeName(Constants.parserTypeName)
        .typeFullName(TypeConstants.any)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .order(order)
        .argumentIndex(argIdx)
    AstWithCtx(Ast(unknownNode), Context())
  }

  private def erasedSignature(args: Seq[Any]): String = {
    val argsSignature = {
      if (args.size == 0) {
        ""
      } else if (args.size == 1) {
        TypeConstants.any
      } else {
        TypeConstants.any + ("," + TypeConstants.any) * (args.size - 1)
      }
    }
    TypeConstants.any + "(" + argsSignature + ")"
  }

  def astForStringTemplate(
      expr: KtStringTemplateExpression,
      scopeContext: ScopeContext,
      order: Int,
      argIdx: Int
  )(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    if (expr.hasInterpolation()) {
      val callNode =
        NewCall()
          .name(Operators.formatString)
          .methodFullName(Operators.formatString)
          .dispatchType(DispatchTypes.STATIC_DISPATCH)
          .code(expr.getText())
          .argumentIndex(argIdx)
          .lineNumber(line(expr))
          .columnNumber(column(expr))
          .order(order)
          .typeFullName(typeFullName)
      val args =
        expr
          .getEntries()
          .filter { entry =>
            entry.getExpression != null
          }
          .zipWithIndex
          .map { case (entry, idx) =>
            if (entry.getExpression != null) {
              val valueCallNode =
                NewCall()
                  .name(Operators.formattedValue)
                  .methodFullName(Operators.formattedValue)
                  .dispatchType(DispatchTypes.STATIC_DISPATCH)
                  .code(entry.getExpression.getText())
                  .argumentIndex(idx + 1)
                  .lineNumber(line(entry.getExpression))
                  .columnNumber(column(entry.getExpression))
                  .order(idx + 1)
                  // TODO: specify _String_ type directly
                  .typeFullName(TypeConstants.any)
              val valueArgs = astsForExpression(entry.getExpression, scopeContext, idx + 1, idx + 1)
              val call = callAst(valueCallNode, valueArgs.map(_.ast))
              Seq(AstWithCtx(call, Context()))
            } else {
              Seq()
            }
          }
          .flatten
      val ast = callAst(callNode, args.toIndexedSeq.map(_.ast))
      AstWithCtx(ast, mergedCtx(args.toIndexedSeq.map(_.ctx)))
    } else {
      val ast =
        Ast(
          NewLiteral()
            .order(order)
            .argumentIndex(argIdx)
            .code(expr.getText)
            .typeFullName(typeFullName)
            .lineNumber(line(expr))
            .columnNumber(column(expr))
        )
      AstWithCtx(ast, Context())
    }
  }

  def astForQualifiedExpression(
      expr: KtQualifiedExpression,
      scopeContext: ScopeContext,
      order: Int,
      argIdx: Int
  )(implicit fileInfo: FileInfo, typeInfoProvider: TypeInfoProvider): AstWithCtx = {
    val bindingKind = typeInfoProvider.bindingKind(expr)
    val isStaticCall = bindingKind == BindingKinds.Static
    val isDynamicCall = bindingKind == BindingKinds.Dynamic

    val orderForReceiver = 1
    val argIdxForReceiver = if (isDynamicCall) 0 else if (isStaticCall) 1 else 1
    val receiverExpr = expr.getReceiverExpression()
    val receiverAstWithCtx: AstWithCtx = {
      receiverExpr match {
        case typedExpr: KtConstantExpression =>
          astForLiteral(typedExpr, scopeContext, orderForReceiver, argIdxForReceiver)
        case typedExpr: KtNameReferenceExpression =>
          astForIdentifier(typedExpr, orderForReceiver, argIdxForReceiver)
        case thisExpr: KtThisExpression =>
          astForThisExpression(thisExpr, scopeContext, orderForReceiver, 0)
        case superExpr: KtSuperExpression =>
          val node =
            NewUnknown()
              .code(superExpr.getText)
              .typeFullName(TypeConstants.any)
              .order(orderForReceiver)
              .argumentIndex(argIdxForReceiver)
              .lineNumber(line(superExpr))
              .columnNumber(column(superExpr))
          AstWithCtx(Ast(node), Context())
        case typedExpr: KtDotQualifiedExpression =>
          astForQualifiedExpression(typedExpr, scopeContext, orderForReceiver, argIdxForReceiver)
        case typedExpr: KtClassLiteralExpression =>
          astForClassLiteral(typedExpr, scopeContext, orderForReceiver, argIdxForReceiver)
        case typedExpr: KtPostfixExpression =>
          astForPostfixExpression(typedExpr, scopeContext, orderForReceiver, argIdxForReceiver)
        case typedExpr: KtStringTemplateExpression =>
          astForStringTemplate(typedExpr, scopeContext, orderForReceiver, argIdxForReceiver)
        case typedExpr: KtParenthesizedExpression =>
          val astsWithCtx = astsForExpression(typedExpr, scopeContext, order, argIdxForReceiver)
          // TODO: get to the root cause of why the asts here are empty; write unit tests
          // e.g. for when this is the case in https://github.com/detekt/detekt
          val astForExpr = {
            if (!astsWithCtx.isEmpty) {
              astsWithCtx.head.ast
            } else {
              val node =
                NewUnknown()
                  .code(typedExpr.getText)
                  .typeFullName(TypeConstants.any)
                  .order(orderForReceiver)
                  .argumentIndex(argIdxForReceiver)
                  .lineNumber(line(typedExpr))
                  .columnNumber(column(typedExpr))
              Ast(node)
            }
          }
          AstWithCtx(astForExpr, Context())
        case typedExpr: KtSafeQualifiedExpression =>
          astForQualifiedExpression(typedExpr, scopeContext, orderForReceiver, argIdxForReceiver)
        case typedExpr: KtWhenExpression =>
          astForWhen(typedExpr, scopeContext, orderForReceiver)
        case typedExpr: KtCallExpression =>
          astForCall(typedExpr, scopeContext, orderForReceiver, argIdxForReceiver)
        case typedExpr: KtArrayAccessExpression =>
          astForArrayAccess(typedExpr, scopeContext, orderForReceiver, argIdxForReceiver)
        // TODO: handle `KtCallableReferenceExpression` like `this::baseTerrain`
        // KtObjectLiteralExpression
        case unhandled: KtExpression =>
          logger.debug(
            "Creating UNKNOWN node in DQE for expression `" + unhandled.getText + "` of class `" + unhandled.getClass + "`."
          )
          val node =
            NewUnknown()
              .code(unhandled.getText)
              .typeFullName(TypeConstants.any)
              .order(orderForReceiver)
              .argumentIndex(argIdxForReceiver)
              .lineNumber(line(unhandled))
              .columnNumber(column(unhandled))
          AstWithCtx(Ast(node), Context())
      }
    }
    val receiverAst = receiverAstWithCtx.ast

    var selectorOrderCount = argIdxForReceiver
    val argAsts =
      expr.getSelectorExpression() match {
        case selectorExpression: KtCallExpression =>
          withOrder(selectorExpression.getValueArguments()) { case (arg, order) =>
            val selectorOrder = if (isStaticCall) order else selectorOrderCount + order + 1
            val selectorArgIndex = if (isStaticCall) order else selectorOrder - 1
            val asts =
              astsForExpression(
                arg.getArgumentExpression(),
                scopeContext,
                selectorOrder,
                selectorArgIndex
              )
            selectorOrderCount += 1
            asts
          }.flatten
        case typedExpr: KtNameReferenceExpression =>
          val order = if (isStaticCall) 1 else 2
          val argIdx = if (isStaticCall) 1 else 2
          val node =
            NewFieldIdentifier()
              .code(typedExpr.getText)
              .canonicalName(typedExpr.getText)
              .order(order)
              .argumentIndex(argIdx)
          List(AstWithCtx(Ast(node), Context()))
        case _ =>
          List()
      }

    // TODO: add more test cases for this
    val astDerivedMethodFullName = {
      if (scopeContext.typeDecl.isDefined) {
        // TODO: handle parameters here and insert the correct types
        val name = expr.getSelectorExpression.getText.replace("(", "").replace(")", "")
        scopeContext.typeDecl.get.fullName + "." + name + ":ANY()"
      } else if (expr.getSelectorExpression.isInstanceOf[KtCallExpression]) {
        val receiverPlaceholderType = TypeConstants.kotlinAny
        val shortName = expr.getSelectorExpression.getFirstChild.getText
        val args = expr.getSelectorExpression().asInstanceOf[KtCallExpression].getValueArguments()
        receiverPlaceholderType + "." + shortName + ":" + erasedSignature(args.asScala.toList)
      } else if (expr.getSelectorExpression.isInstanceOf[KtNameReferenceExpression]) {
        Operators.fieldAccess
      } else {
        // TODO: add more test cases for this scenario
        ""
      }
    }

    val astDerivedSignature =
      if (astDerivedMethodFullName.startsWith(Constants.operatorSuffix)) {
        ""
      } else {
        erasedSignature(argAsts)
      }
    val fullNameWithSig =
      typeInfoProvider.fullNameWithSignature(
        expr,
        (astDerivedMethodFullName, astDerivedSignature)
      )
    val declType = typeInfoProvider.containingDeclType(expr, TypeConstants.any)
    registerType(declType)

    val retType = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(retType)

    val dispatchType =
      if (bindingKind == BindingKinds.Dynamic) {
        DispatchTypes.DYNAMIC_DISPATCH
      } else {
        DispatchTypes.STATIC_DISPATCH
      }
    val methodName = expr.getSelectorExpression.getFirstChild.getText
    val callNode =
      NewCall()
        .order(order)
        .argumentIndex(argIdx)
        .code(expr.getText())
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .typeFullName(retType)
        .name(methodName)
        .methodFullName(fullNameWithSig._1)
        .dispatchType(dispatchType)
        .signature(fullNameWithSig._2)
    val receiverNode = receiverAst.root.get
    val finalAst = {
      if (isStaticCall) {
        Ast(callNode)
          .withChild(receiverAst)
          .withChildren(argAsts.map(_.ast))
          .withArgEdges(callNode, argAsts.map(_.ast.root.get))
      } else {
        val ast =
          Ast(callNode)
            .withChild(receiverAst)
            .withArgEdge(callNode, receiverNode)
            .withChildren(argAsts.map(_.ast))
            .withArgEdges(callNode, argAsts.map(_.ast.root.get))
        if (argAsts.size == 1 && argAsts(0).ast.root.get.isInstanceOf[NewMethodRef]) {
          ast
            .withReceiverEdge(callNode, argAsts(0).ast.root.get)
        } else {
          ast
            .withReceiverEdge(callNode, receiverNode)
        }
      }
    }
    val argCtx = mergedCtx(argAsts.map(_.ctx) ++ Seq(receiverAstWithCtx.ctx))
    AstWithCtx(finalAst, argCtx)
  }

  def astForBreak(expr: KtBreakExpression, scopeContext: ScopeContext, order: Int)(implicit
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val node = NewControlStructure()
      .controlStructureType(ControlStructureTypes.BREAK)
      .code(expr.getText)
      .order(order)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
    val ast = Ast(node)
    AstWithCtx(ast, Context())
  }

  def astForContinue(
      expr: KtContinueExpression,
      scopeContext: ScopeContext,
      order: Int
  )(implicit typeInfoProvider: TypeInfoProvider): AstWithCtx = {
    val node = NewControlStructure()
      .controlStructureType(ControlStructureTypes.CONTINUE)
      .code(expr.getText)
      .order(order)
      .lineNumber(line(expr))
      .columnNumber(column(expr))
    val ast = Ast(node)
    AstWithCtx(ast, Context())
  }

  // TODO: handle parameters passed to the clauses
  def astForTry(expr: KtTryExpression, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val tryNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.TRY)
        .code(expr.getText)
        .order(order)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .argumentIndex(order)
    val tryAstWithCtx = astsForExpression(expr.getTryBlock, scopeContext, 1, 1).headOption
      .getOrElse(AstWithCtx(Ast(), Context()))
    val tryAst =
      Ast(tryNode)
        .withChild(tryAstWithCtx.ast)

    val clauseAstsWitCtx =
      withOrder(expr.getCatchClauses) { (entry, order) =>
        astsForExpression(entry.getCatchBody, scopeContext, order + 1, order + 1)
      }.flatten

    val finallyAstsWithCtx =
      if (expr.getFinallyBlock() == null) {
        Seq()
      } else {
        val numClauses = clauseAstsWitCtx.size
        astsForExpression(expr.getFinallyBlock.getFinalExpression, scopeContext, numClauses + 2, numClauses + 2)
      }

    val tryWithClausesAst =
      tryAst
        .withChildren(clauseAstsWitCtx.map(_.ast))
    val finalAst =
      if (finallyAstsWithCtx.size > 0) {
        tryWithClausesAst
          .withChildren(finallyAstsWithCtx.map(_.ast))
      } else {
        tryWithClausesAst
      }
    val finalCtx = mergedCtx(Seq(tryAstWithCtx.ctx) ++ clauseAstsWitCtx.map(_.ctx) ++ finallyAstsWithCtx.map(_.ctx))
    AstWithCtx(finalAst, finalCtx)
  }

  def astForWhile(expr: KtWhileExpression, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val whileNode =
      NewControlStructure()
        .code(expr.getText)
        .controlStructureType(ControlStructureTypes.WHILE)
        .order(order)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val conditionAst = astsForExpression(expr.getCondition, scopeContext, 1, 1).headOption
      .getOrElse(AstWithCtx(Ast(), Context()))
    val stmtAsts = astsForExpression(expr.getBody, scopeContext, 2, 2)
    val tempAst = Ast(whileNode)
      .withChild(conditionAst.ast)
      .withChildren(stmtAsts.map(_.ast))

    val ast =
      conditionAst.ast.root match {
        case Some(r) =>
          tempAst.withConditionEdge(whileNode, r)
        case None =>
          tempAst
      }
    AstWithCtx(ast, Context())
  }

  def astForDoWhile(expr: KtDoWhileExpression, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val doNode =
      NewControlStructure()
        .code(expr.getText)
        .controlStructureType(ControlStructureTypes.DO)
        .order(order)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val stmtAsts = astsForExpression(expr.getBody, scopeContext, 1, 1)
    val conditionAst = astsForExpression(expr.getCondition(), scopeContext, 2, 2).headOption
      .getOrElse(AstWithCtx(Ast(), Context()))
    val tempAst = Ast(doNode)
      .withChildren(stmtAsts.map(_.ast))
      .withChild(conditionAst.ast)

    val ast =
      conditionAst.ast.root match {
        case Some(r) =>
          tempAst.withConditionEdge(doNode, r)
        case None =>
          tempAst
      }
    AstWithCtx(ast, mergedCtx(stmtAsts.map(_.ctx) ++ Seq(conditionAst.ctx)))
  }

  def astForFor(expr: KtForExpression, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val forNode =
      NewControlStructure()
        .controlStructureType(ControlStructureTypes.FOR)
        .order(order)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .code(expr.getText)
    val stmtAst =
      astsForExpression(expr.getBody(), scopeContext, order + 1, order + 1)
    val ast = Ast(forNode)
      .withChildren(stmtAst.map(_.ast))
    AstWithCtx(ast, mergedCtx(stmtAst.map(_.ctx)))
  }

  def astForWhen(expr: KtWhenExpression, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val astForSubject =
      astsForExpression(expr.getSubjectExpression(), scopeContext, 1, 1).headOption
        .getOrElse(AstWithCtx(Ast(), Context()))

    val astsForEntries =
      withOrder(expr.getEntries()) { (e, order) =>
        astsForWhenEntry(e, scopeContext, order)
      }.flatten

    val switchBlockNode =
      NewBlock()
        .order(2)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .code(expr.getEntries.asScala.map(_.getText).mkString("\n"))
        .typeFullName(TypeConstants.any)
    val astForBlock =
      Ast(switchBlockNode)
        .withChildren(astsForEntries)

    val codeForSwitch =
      if (expr.getSubjectExpression() != null) {
        s"when(${expr.getSubjectExpression().getText()})"
      } else {
        "when"
      }
    val switchNode =
      NewControlStructure()
        .code(expr.getText)
        .controlStructureType(ControlStructureTypes.SWITCH)
        .order(order)
        .argumentIndex(order)
        .code(codeForSwitch)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val ast =
      Ast(switchNode)
        .withChild(astForSubject.ast)
        .withChild(astForBlock)
    val astWithCondition =
      astForSubject.ast.root match {
        case Some(root) =>
          ast.withConditionEdge(switchNode, root)
        case None =>
          ast
      }
    AstWithCtx(astWithCondition, Context())
  }

  def astsForWhenEntry(entry: KtWhenEntry, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): Seq[Ast] = {
    // TODO: get all conditions with entry.getConditions()
    val jumpTargetName =
      if (entry.getElseKeyword == null) {
        Constants.caseNodePrefix + order.toString
      } else {
        Constants.defaultCaseNode
      }
    val jumpNode =
      NewJumpTarget()
        .order(order)
        .argumentIndex(order)
        .code(entry.getText)
        .name(jumpTargetName)
        .lineNumber(line(entry.getExpression))
        .columnNumber(column(entry.getExpression))
        .parserTypeName(Constants.caseNodeParserTypeName)
    val exprNode = astsForExpression(entry.getExpression, scopeContext, order + 1, order + 1).headOption
      .getOrElse(AstWithCtx(Ast(), Context()))
    Seq(Ast(jumpNode)) ++ Seq(exprNode.ast)
  }

  def astForIf(expr: KtIfExpression, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    if (expr.getParent.isInstanceOf[KtProperty]) {
      astForIfAsExpression(expr, scopeContext, order)
    } else {
      astForIfAsControlStructure(expr, scopeContext, order)
    }
  }

  def astForIfAsControlStructure(expr: KtIfExpression, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val ifNode =
      NewControlStructure()
        .code(expr.getText)
        .controlStructureType(ControlStructureTypes.IF)
        .order(order)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .argumentIndex(order)
    val conditionAst = astsForExpression(expr.getCondition(), scopeContext, 1, 1)
    val thenAsts = astsForExpression(expr.getThen(), scopeContext, 2, 2)
    val elseAsts = astsForExpression(expr.getElse(), scopeContext, 3, 3)

    val ast = Ast(ifNode)
      .withChild(conditionAst.head.ast)
      .withChildren(thenAsts.map(_.ast) ++ elseAsts.map(_.ast))

    val withCondition =
      conditionAst.head.ast.root match {
        case Some(r) =>
          ast.withConditionEdge(ifNode, r)
        case None =>
          ast
      }
    AstWithCtx(withCondition, Context())
  }

  def astForIfAsExpression(expr: KtIfExpression, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val retType = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(retType)

    val callNode =
      NewCall()
        .name(Operators.conditional)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText())
        .order(order)
        .argumentIndex(order)
        .typeFullName(retType)
        .methodFullName(Operators.conditional)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val conditionAsts = astsForExpression(expr.getCondition(), scopeContext, 1, 1)
    val thenAsts = astsForExpression(expr.getThen(), scopeContext, 2, 2)
    val elseAsts = astsForExpression(expr.getElse(), scopeContext, 3, 3)

    val childAsts = conditionAsts.map(_.ast) ++ thenAsts.map(_.ast) ++ elseAsts.map(_.ast)
    val ctx = mergedCtx(
      conditionAsts.map(_.ctx) ++ thenAsts.map(_.ctx) ++ elseAsts.map(_.ctx)
    )
    if (conditionAsts.size > 0 && conditionAsts.head.ast.root != null) {
      val ast =
        Ast(callNode)
          .withChildren(childAsts)
          .withArgEdges(
            callNode,
            childAsts.map(_.root.get)
          )
      AstWithCtx(ast, ctx)
    } else {
      logger.warn(
        "Parsing failed for expr `" + expr.getName + "` in file `" + fileWithMeta.filename + "`."
      )
      logger.debug(" > expr.text `" + expr.getText + "`")

      val unknownNode =
        NewUnknown()
          .code(expr.getText())
          .order(order)
          .argumentIndex(order)
          .lineNumber(line(expr))
          .columnNumber(column(expr))
      AstWithCtx(Ast(unknownNode), Context())
    }
  }

  private def astsForProperty(expr: KtProperty, scopeContext: ScopeContext, order: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithCtx] = {
    val explicitTypeName =
      if (expr.getTypeReference() != null) {
        expr.getTypeReference().getText()
      } else {
        TypeConstants.any
      }
    val elem = expr.getIdentifyingElement()
    val typeFullName = typeInfoProvider.propertyType(expr, explicitTypeName)
    registerType(typeFullName)

    val identifier =
      NewIdentifier()
        .name(elem.getText())
        .order(1)
        .argumentIndex(1)
        .code(elem.getText())
        .typeFullName(typeFullName)
        .lineNumber(line(elem))
        .columnNumber(column(elem))
    val assignment =
      NewCall()
        .name(Operators.assignment)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText())
        .order(order + 1)
        .argumentIndex(order + 1)
        .typeFullName(typeFullName)
        .methodFullName(Operators.assignment)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val localNode =
      NewLocal()
        .name(expr.getName)
        .code(expr.getText)
        .typeFullName(typeFullName)
        .lineNumber(line(elem))
        .columnNumber(column(elem))
        .order(order)

    val initAsts = astsForExpression(expr.getDelegateExpressionOrInitializer, scopeContext, 2, 2)
    val call = callAst(assignment, Seq(Ast(identifier)) ++ initAsts.map(_.ast))

    val initCtx = mergedCtx(initAsts.map(_.ctx))
    // TODO: check if everything is merged as it should be in this place (i.e. all entries)
    val finalCtx =
      Context(
        initCtx.locals,
        initCtx.identifiers ++ List(identifier),
        Seq(),
        Seq(),
        lambdaAsts = initCtx.lambdaAsts
      )
    Seq(AstWithCtx(call, Context())) ++
      Seq(AstWithCtx(Ast(localNode).withRefEdge(identifier, localNode), finalCtx))
  }

  def astForIdentifier(expr: KtNameReferenceExpression, order: Int, argIdx: Int)(implicit
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val typeFullName = typeInfoProvider.typeFullName(expr, TypeConstants.any)
    registerType(typeFullName)

    val name = expr.getIdentifier().getText()
    val identifierNode =
      NewIdentifier()
        .name(name)
        .order(order)
        .argumentIndex(argIdx)
        .code(name)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .typeFullName(typeFullName)
    AstWithCtx(Ast(identifierNode), Context(identifiers = List(identifierNode)))
  }

  def astForLiteral(
      expr: KtConstantExpression,
      scopeContext: ScopeContext,
      order: Int,
      argIdx: Int
  )(implicit typeInfoProvider: TypeInfoProvider): AstWithCtx = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val ast = Ast(
      NewLiteral()
        .order(order)
        .argumentIndex(argIdx)
        .code(expr.getText)
        .typeFullName(typeFullName)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    )
    AstWithCtx(ast, Context())
  }

  def astForBinaryExpr(expr: KtBinaryExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val opRef = expr.getOperationReference()

    // TODO: add the rest of the operators
    val operatorOption = {
      opRef.getOperationSignTokenType match {
        case KtTokens.PLUS       => Some(Operators.addition)
        case KtTokens.MINUS      => Some(Operators.subtraction)
        case KtTokens.MUL        => Some(Operators.multiplication)
        case KtTokens.DIV        => Some(Operators.division)
        case KtTokens.LT         => Some(Operators.lessThan)
        case KtTokens.LTEQ       => Some(Operators.lessEqualsThan)
        case KtTokens.GT         => Some(Operators.greaterThan)
        case KtTokens.GTEQ       => Some(Operators.greaterEqualsThan)
        case KtTokens.EXCLEQ     => Some(Operators.notEquals)
        case KtTokens.EXCLEQEQEQ => Some(Operators.notEquals)
        case KtTokens.EQ         => Some(Operators.assignment)
        case KtTokens.EQEQ       => Some(Operators.equals)
        case KtTokens.EQEQEQ     => Some(Operators.equals)
        case KtTokens.ANDAND     => Some(Operators.logicalAnd)
        case KtTokens.OROR       => Some(Operators.logicalOr)
        case KtTokens.PLUSEQ     => Some(Operators.assignmentPlus)
        case KtTokens.MINUSEQ    => Some(Operators.assignmentMinus)
        case KtTokens.MULTEQ     => Some(Operators.assignmentMultiplication)
        case KtTokens.DIVEQ      => Some(Operators.assignmentDivision)
        case KtTokens.PERCEQ     => Some(Operators.assignmentModulo)
        case KtTokens.PERC       => Some(Operators.modulo)
        case KtTokens.ELVIS      => Some(Operators.elvis)
        case KtTokens.RANGE      => Some(Operators.range)
        case KtTokens.NOT_IN     => Some(Operators.notIn)
        case KtTokens.IN_KEYWORD => Some(Operators.in)
        case null =>
          val opElement = expr.getOperationReference.getReferencedNameElement()
          opElement.getText match {
            case "and"  => Some(Operators.and)
            case "or"   => Some(Operators.or)
            case "xor"  => Some(Operators.xor)
            case "shl"  => Some(Operators.shiftLeft)
            case "ushl" => Some(Operators.shiftLeft)
            case "shr"  => Some(Operators.arithmeticShiftRight)
            case "ushr" => Some(Operators.logicalShiftRight)
            case _ =>
              None
          }
        case _ =>
          logger.warn(
            "Unhandled operator token type `" + opRef
              .getOperationSignTokenType() + "` for expression `" + expr.getText + "`."
          )
          Some(Constants.unknownOperator)
      }
    }
    val fullNameWithSignature =
      if (operatorOption.isDefined) {
        (operatorOption.get, TypeConstants.any)
      } else {
        // TODO: fix the fallback METHOD_FULL_NAME and SIGNATURE here (should be a correct number of ANYs)
        typeInfoProvider.fullNameWithSignature(expr, (TypeConstants.any, TypeConstants.any))
      }
    val fullName = fullNameWithSignature._1
    val signature =
      if (fullName.startsWith(Constants.operatorSuffix)) {
        Constants.empty // TODO: add test case for this situation
      } else {
        fullNameWithSignature._2
      }
    val expressionType = typeInfoProvider.expressionType(expr, TypeConstants.any)
    val name = if (operatorOption.isDefined) {
      operatorOption.get
    } else if (expr.getChildren().toList.size >= 2) {
      expr.getChildren().toList(1).getText
    } else {
      expr.getName
    }
    // TODO: DYNAMIC_DISPATCH check
    val callNode =
      NewCall()
        .name(name)
        .methodFullName(fullName)
        .signature(signature)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText())
        .argumentIndex(order)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .order(order)
        .typeFullName(expressionType)

    val args =
      astsForExpression(expr.getLeft(), scopeContext, 1, 1) ++ astsForExpression(
        expr.getRight(),
        scopeContext,
        2,
        2
      )
    val ast = callAst(callNode, args.map(_.ast))
    AstWithCtx(ast, mergedCtx(args.map(_.ctx)))
  }

  def callAst(rootNode: NewNode, args: Seq[Ast]): Ast = {
    Ast(rootNode)
      .withChildren(args)
      .withArgEdges(rootNode, args.flatMap(_.root))
  }

  private def astForCall(
      expr: KtCallExpression,
      scopeContext: ScopeContext,
      order: Int = 1,
      argIdx: Int
  )(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val args = expr.getValueArguments()
    val argAsts =
      withOrder(args) { case (arg, argOrder) =>
        astsForExpression(arg.getArgumentExpression(), scopeContext, argOrder, argOrder)
      }.flatten

    // TODO: add tests for the empty `referencedName` here
    val referencedName =
      expr.getFirstChild match {
        case c: KtNameReferenceExpression => c.getText()
        case _                            => ""
      }

    val nameToClass = fileInfo.classes.map { klass => klass.getName -> klass }.toMap
    val importedNames =
      fileInfo.imports.map { imp => imp.name -> imp }.toMap
    val methodFqName = {
      if (importedNames.isDefinedAt(referencedName)) {
        importedNames.get(referencedName).get.fqName
      } else if (nameToClass.contains(expr.getCalleeExpression.getText)) {
        val klass = nameToClass(expr.getCalleeExpression.getText)
        klass.getContainingKtFile.getPackageFqName.toString + "." + referencedName
      } else {
        expr.getContainingKtFile.getPackageFqName.toString + "." + referencedName
      }
    }
    val signature =
      TypeConstants.any + "(" + args.asScala.map { _ => TypeConstants.any }.mkString(",") + ")"

    val fullName = methodFqName + ":" + signature
    val fullNameWithSig = typeInfoProvider.fullNameWithSignature(expr, (fullName, signature))

    // TODO: add test case to confirm whether the ANY fallback makes sense (could be void)
    val returnType = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(returnType)

    val callNode =
      NewCall()
        .name(referencedName)
        .code(expr.getText())
        .order(order)
        .argumentIndex(argIdx)
        .methodFullName(fullNameWithSig._1)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .signature(fullNameWithSig._2)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .typeFullName(returnType)

    val ast =
      Ast(callNode)
        .withChildren(argAsts.map(_.ast))
        .withArgEdges(callNode, argAsts.flatMap(_.ast.root))

    AstWithCtx(ast, mergedCtx(argAsts.map(_.ctx)))
  }

  private def astForMember(decl: KtDeclaration, childNum: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): Ast = {
    // TODO: handle `null` names in a clean way
    // e.g. found in projects like:
    //   - git@github.com:vsouhrada/kotlin-anko-demo.git
    //   - git@github.com:dgewe/Movie-App-Android.git
    val name =
      if (decl.getName() != null) {
        decl.getName()
      } else {
        TypeConstants.any
      }
    val code = name

    val explicitTypeName =
      decl.getOriginalElement() match {
        case p: KtProperty if p.getTypeReference() != null => p.getTypeReference().getText()
        case _                                             => TypeConstants.any
      }

    val typeFullName = decl match {
      case typed: KtProperty =>
        typeInfoProvider.propertyType(typed, explicitTypeName)
      case _ =>
        explicitTypeName
    }
    registerType(typeFullName)

    val memberNode =
      NewMember()
        .name(name)
        .code(code)
        .typeFullName(typeFullName)
        .lineNumber(line(decl))
        .columnNumber(column(decl))
        .order(childNum)
    Ast(memberNode)
  }

  private def astForParameter(param: KtParameter, childNum: Int)(implicit
      fileInfo: FileInfo,
      typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    // TODO: !!!! lower destructuring declarations inside lambdas properly
    val name = if (param.getDestructuringDeclaration != null) {
      Constants.paramNameLambdaDestructureDecl
    } else {
      param.getName()
    }
    val explicitTypeName =
      if (param.getTypeReference() != null) {
        param.getTypeReference().getText()
      } else {
        TypeConstants.any
      }
    val typeFullName = typeInfoProvider.parameterType(param, explicitTypeName)
    registerType(typeFullName)

    val parameterNode =
      NewMethodParameterIn()
        .name(name)
        .code(param.getText)
        .typeFullName(typeFullName)
        .order(childNum)
        .lineNumber(line(param))
        .columnNumber(column(param))
    val ast = Ast(parameterNode)
    AstWithCtx(ast, Context(List(), List(), List(parameterNode)))
  }

  private def createMethodNode(expr: KtNamedFunction, childNum: Int)(implicit
      typeInfoProvider: TypeInfoProvider
  ) = {
    val fnWithSig = typeInfoProvider.fullNameWithSignature(expr, ("", ""))
    val code = codeForFn(expr)
    val methodNode =
      NewMethod()
        .name(expr.getName())
        .fullName(fnWithSig._1)
        .code(code)
        .signature(fnWithSig._2)
        .isExternal(false)
        .order(childNum)
        .filename(relativizedPath)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    methodNode
  }

  private def codeForFn(ktFunction: KtNamedFunction): String = {
    val paramTypesWithName =
      try {
        val nodeParams = ktFunction.getValueParameters()
        nodeParams.asScala
          .map { p =>
            val paramTypeName =
              if (p.getTypeReference() != null) {
                p.getTypeReference.getText()
              } else { TypeConstants.any }
            val paramName = p.getName()
            paramName + ":" + paramTypeName
          }
      } catch {
        case _: Throwable => List()
      }
    val returnTypeName =
      if (ktFunction.getTypeReference() != null) {
        ktFunction.getTypeReference().getText()
      } else {
        ""
      }
    returnTypeName + "(" + paramTypesWithName.mkString(", ") + ")"
  }
}
