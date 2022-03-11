package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.KtFileWithMeta
import io.joern.kotlin2cpg.types.{CallKinds, NameReferenceKinds, TypeConstants, TypeInfoProvider}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.passes.{IntervalKeyPool}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.joern.x2cpg.Ast

import java.util.UUID.randomUUID
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.lexer.KtTokens
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.jdk.CollectionConverters._
import scala.annotation.tailrec

object Constants {
  val empty                          = "<empty>"
  val lambdaName                     = "<lambda>"
  val init                           = "<init>"
  val retCode                        = "RET"
  val tryCode                        = "try"
  val parserTypeName                 = "KOTLIN_PSI_PARSER"
  val caseNodePrefix                 = "case"
  val defaultCaseNode                = "default"
  val caseNodeParserTypeName         = "CaseNode"
  val unknownOperator                = "<operator>.unknown"
  val operatorSuffix                 = "<operator>"
  val paramNameLambdaDestructureDecl = "DESTRUCTURE_PARAM"
  val wildcardImportName             = "*"
  val lambdaBindingName              = "invoke" // the underlying _invoke_ fn for Kotlin FunctionX types
  val lambdaTypeDeclName             = "LAMBDA_TYPE_DECL"
  val this_                          = "this"
  val componentNSuffix               = "component"
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
  closureBindingInfo: Seq[ClosureBindingInfo] = List(),
  lambdaBindingInfo: Seq[BindingInfo] = List()
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
class AstCreator(fileWithMeta: KtFileWithMeta, xTypeInfoProvider: TypeInfoProvider, global: Global) {

  // TODO: remove flag as soon as all ASTs without root are not being passed around any more
  // debug flag; when turned on, parsing continues even if an AST has no root
  // only here to help in paying back technical debt. Remove after
  private val continueParsingOnAstNodesWithoutRoot = false

  private val diffGraph: DiffGraphBuilder = new DiffGraphBuilder()

  private val lambdaKeyPool = new IntervalKeyPool(first = 1, last = Long.MaxValue)
  private val tmpKeyPool    = new IntervalKeyPool(first = 1, last = Long.MaxValue)

  private val relativizedPath = fileWithMeta.relativizedPath

  def createAst(): DiffGraphBuilder = {
    implicit val typeInfoProvider: TypeInfoProvider = xTypeInfoProvider
    logger.debug("Started parsing of file `" + fileWithMeta.filename + "`")
    storeInDiffGraph(astForFile(fileWithMeta))
    diffGraph
  }

  protected val logger: Logger = LoggerFactory.getLogger(classOf[AstCreator])

  /** Add `typeName` to a global map and return it. The map is later passed to a pass that creates TYPE nodes for each
    * key in the map.
    */
  private def registerType(typeName: String): String = {
    global.usedTypes.putIfAbsent(typeName, true)
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

    astWithCtx.ctx.lambdaBindingInfo.foreach { bindingInfo =>
      diffGraph.addNode(bindingInfo.node)

      bindingInfo.edgeMeta.foreach { edgeMeta =>
        diffGraph.addEdge(edgeMeta._1, edgeMeta._2, edgeMeta._3)
      }
    }

    astWithCtx.ctx.closureBindingInfo.foreach { closureBindingInfo =>
      diffGraph.addNode(closureBindingInfo.node)

      closureBindingInfo.edgeMeta.foreach { edgeMeta =>
        diffGraph.addEdge(edgeMeta._1, edgeMeta._2, edgeMeta._3)
      }
    }
  }

  def withOrder[T <: Any, X](nodeList: java.util.List[T])(f: (T, Int) => X): Seq[X] = {
    nodeList.asScala.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }.toSeq
  }

  private def line(element: PsiElement): Int = {
    try {
      element.getContainingFile.getViewProvider.getDocument
        .getLineNumber(element.getTextOffset)
    } catch {
      case _: Throwable => -1
    }
  }

  private def column(element: PsiElement): Int = {
    try {
      val lineNumber =
        element.getContainingFile.getViewProvider.getDocument
          .getLineNumber(element.getTextOffset)
      val lineOffset =
        element.getContainingFile.getViewProvider.getDocument.getLineStartOffset(lineNumber)
      element.getTextOffset - lineOffset
    } catch {
      case _: Throwable => -1
    }
  }

  private def astForFile(fileWithMeta: KtFileWithMeta)(implicit typeInfoProvider: TypeInfoProvider): AstWithCtx = {
    val ktFile = fileWithMeta.f
    val classDecls =
      ktFile.getDeclarations.asScala
        .filter(_.isInstanceOf[KtClass])
        .map(_.asInstanceOf[KtClass])
        .toList

    val allImports =
      combinedImports(ktFile.getImportList.getImports.asScala.toList)
    implicit val fileInfo: FileInfo = FileInfo(allImports, classDecls)

    val importAsts =
      withOrder(allImports.asJava) { (entry, order) =>
        astForImportEntry(entry, order)
      }
    val namespaceBlocksForImports =
      allImports.asJava.asScala.collect {
        case e if !e.isWildcard =>
          Ast(NewNamespaceBlock().name(e.fqName).fullName(e.fqName))
      }.toSeq

    val lastImportOrder = importAsts.size
    var idxEpsilon      = 0 // when multiple AST nodes are returned by `astForDeclaration`
    val declarationsAstsWithCtx =
      withOrder(ktFile.getDeclarations) { (decl, order) =>
        val asts = astForDeclaration(decl, ScopeContext(), order + lastImportOrder + idxEpsilon)
        idxEpsilon += asts.size - 1
        asts
      }.flatten

    val fileNode =
      NewFile()
        .name(fileWithMeta.relativizedPath)
        .order(0)
    val finalCtx                 = mergedCtx(declarationsAstsWithCtx.map(_.ctx))
    val namespaceBlockAstWithCtx = astForPackageDeclaration(ktFile.getPackageFqName.toString)
    val lambdaTypeDecls =
      finalCtx.lambdaBindingInfo.flatMap(
        _.edgeMeta
          .map(_._1)
          .collect { case n: NewTypeDecl => Ast(n) }
      )
    val ast =
      Ast(fileNode)
        .withChild(
          namespaceBlockAstWithCtx.ast
            .withChildren(importAsts.map(_.ast))
            .withChildren(declarationsAstsWithCtx.map(_.ast))
            .withChildren(mergedCtx(declarationsAstsWithCtx.map(_.ctx)).lambdaAsts)
            .withChildren(lambdaTypeDecls)
        )
        .withChildren(namespaceBlocksForImports)
    AstWithCtx(ast, finalCtx)
  }

  def combinedImports(explicitImports: Seq[KtImportDirective]): Seq[ImportEntry] = {
    val explicitImportEntries =
      explicitImports.map { entry =>
        // TODO: add more test cases for import directives
        // e.g. of a project where parsing fails with NPE if the null check in isWildcard is not in:
        // https://github.com/CypherpunkArmory/UserLAnd
        val isWildcard = entry.getLastChild.getText == "*" || entry.getImportedName == null
        val importedName =
          if (isWildcard) {
            Constants.wildcardImportName
          } else {
            entry.getImportedName.toString
          }

        ImportEntry(
          entry.getImportPath.getPathStr,
          importedName,
          explicit = true,
          isWildcard = isWildcard,
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
  ): Seq[AstWithCtx] = {
    Seq()
  }

  def astForTypeAlias(typeAlias: KtTypeAlias, order: Int)(implicit typeInfoProvider: TypeInfoProvider): AstWithCtx = {
    val fullName          = typeInfoProvider.fullName(typeAlias, TypeConstants.any)
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
    val className = ktClass.getName
    val explicitFullName = {
      val fqName = ktClass.getContainingKtFile.getPackageFqName.toString
      fqName + "." + className
    }
    val fullName = typeInfoProvider.fullName(ktClass, explicitFullName)
    registerType(fullName)

    val explicitBaseTypeFullNames =
      ktClass.getSuperTypeListEntries.asScala
        .map(_.getTypeAsUserType)
        .filterNot(_ == null) // TODO: write test and pick up code from git@github.com:RedApparat/Fotoapparat.git
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
        ktClass.getBody.getFunctions.asScala.filter { decl =>
          decl.isInstanceOf[KtNamedFunction]
        }.asJava
      } else {
        List().asJava
      }
    val classDeclarations =
      if (ktClass.getBody != null) {
        ktClass.getBody.getDeclarations.asScala.filter { decl =>
          !decl.isInstanceOf[KtNamedFunction]
        }.asJava
      } else {
        List().asJava
      }

    /** curently unused val blockInitializers = if (ktClass.getBody != null) { ktClass.getBody.getAnonymousInitializers
      * } else { List().asJava }
      */
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
        BindingInfo(node, List((typeDecl, node, EdgeTypes.BINDS), (node, ast.root.get, EdgeTypes.REF)))
      }
    val constructorParams = ktClass.getPrimaryConstructorParameters.asScala.toList
    val defaultSignature =
      if (ktClass.getPrimaryConstructor == null) {
        TypeConstants.void + "()"
      } else {
        typeInfoProvider.erasedSignature(constructorParams)
      }
    val defaultFullName = fullName + "." + TypeConstants.initPrefix + ":" + defaultSignature
    val ctorFnWithSig =
      typeInfoProvider.fullNameWithSignature(ktClass.getPrimaryConstructor, (defaultFullName, defaultSignature))
    val primaryCtorOrder = 1
    val constructorMethod =
      NewMethod()
        .name(className)
        .fullName(ctorFnWithSig._1)
        .signature(ctorFnWithSig._2)
        .isExternal(false)
        .order(primaryCtorOrder)
        .filename(relativizedPath)
        .lineNumber(line(ktClass.getPrimaryConstructor))
        .columnNumber(column(ktClass.getPrimaryConstructor))
    val constructorParamsWithCtx =
      withOrder(constructorParams.asJava) { (p, order) =>
        astForParameter(p, order)
      }

    val typeFullName = typeInfoProvider.typeFullName(ktClass.getPrimaryConstructor, TypeConstants.any)
    val constructorMethodReturn =
      NewMethodReturn()
        .order(1)
        .evaluationStrategy(EvaluationStrategies.BY_VALUE)
        .typeFullName(typeFullName)
        .dynamicTypeHintFullName(Some(fullName))
        .code(Constants.retCode)
        .lineNumber(line(ktClass.getPrimaryConstructor))
        .columnNumber(column(ktClass.getPrimaryConstructor))
    val constructorAst =
      Ast(constructorMethod)
        .withChildren(constructorParamsWithCtx.map(_.ast))
        .withChild(Ast(constructorMethodReturn))

    val membersFromPrimaryCtorAsts =
      ktClass.getPrimaryConstructorParameters.asScala.toList
        .filter(_.hasValOrVar)
        .zipWithIndex
        .collect { case (param, idx) =>
          val typeFullName = typeInfoProvider.parameterType(param, TypeConstants.any)
          val node =
            NewMember()
              .name(param.getName)
              .code(param.getText)
              .typeFullName(typeFullName)
              .lineNumber(line(param))
              .columnNumber(column(param))
              .order(idx + primaryCtorOrder)
          Ast(node)
        }

    val orderAfterPrimaryCtorAndItsMemberDefs = membersFromPrimaryCtorAsts.size + primaryCtorOrder
    val secondaryConstructorAsts =
      withOrder(ktClass.getSecondaryConstructors) { (secondaryCtor, order) =>
        val constructorParams = secondaryCtor.getValueParameters.asScala.toList
        val defaultSignature  = typeInfoProvider.erasedSignature(constructorParams)
        val defaultFullName   = fullName + "." + TypeConstants.initPrefix + ":" + defaultSignature
        val ctorFnWithSig = typeInfoProvider.fullNameWithSignature(secondaryCtor, (defaultFullName, defaultSignature))
        val constructorMethod =
          NewMethod()
            .name(className)
            .fullName(ctorFnWithSig._1)
            .signature(ctorFnWithSig._2)
            .isExternal(false)
            .order(orderAfterPrimaryCtorAndItsMemberDefs + order)
            .filename(relativizedPath)
            .lineNumber(line(secondaryCtor))
            .columnNumber(column(secondaryCtor))

        val typeFullName = typeInfoProvider.typeFullName(secondaryCtor, TypeConstants.any)
        val constructorMethodReturn =
          NewMethodReturn()
            .order(1)
            .evaluationStrategy(EvaluationStrategies.BY_VALUE)
            .typeFullName(typeFullName)
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

    val orderAfterCtors = orderAfterPrimaryCtorAndItsMemberDefs + secondaryConstructorAsts.size
    val ast =
      Ast(typeDecl)
        .withChildren(methodAstsWithCtx.map(_.ast))
        .withChild(constructorAst)
        .withChildren(membersFromPrimaryCtorAsts)
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
        .withChildren(withOrder(classDeclarations) { (method, order) =>
          astForMember(method, orderAfterCtors + order)
        })

    val finalCtx = mergedCtx(methodAstsWithCtx.map(_.ctx) ++ List(Context(bindingsInfo = bindingsInfo)))
    AstWithCtx(ast, finalCtx)
  }

  private def astForInitializerBlock(initBlock: KtAnonymousInitializer, scopeContext: ScopeContext, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
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

  private def astForMethod(ktFn: KtNamedFunction, scopeContext: ScopeContext, childNum: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {

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
      withOrder(ktFn.getValueParameters) { (p, order) =>
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
      astForMethodBody(ktFn.getBodyBlockExpression, mergedScopeContext, lastOrder)
    val returnAst = astForMethodReturn(ktFn, lastOrder + 1)

    val paramMap =
      parametersWithCtx
        .flatMap(_.ctx.methodParameters)
        .map { methodParam =>
          methodParam.name -> methodParam
        }
        .toMap
    val identifierMatchingParams =
      bodyAstWithCtx.ctx.identifiers
        .filter { ident =>
          paramMap.isDefinedAt(ident.name)
        }
        .map { ident =>
          (ident, paramMap(ident.name))
        }
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
      val locals             = acc.locals ++ ctx.locals
      val identifiers        = acc.identifiers ++ ctx.identifiers
      val methodParameters   = acc.methodParameters ++ ctx.methodParameters
      val bindingsInfo       = acc.bindingsInfo ++ ctx.bindingsInfo
      val lambdaAsts         = acc.lambdaAsts ++ ctx.lambdaAsts
      val closureBindingInfo = acc.closureBindingInfo ++ ctx.closureBindingInfo
      val lambdaBindingInfo  = acc.lambdaBindingInfo ++ ctx.lambdaBindingInfo
      Context(locals, identifiers, methodParameters, bindingsInfo, lambdaAsts, closureBindingInfo, lambdaBindingInfo)
    })
  }

  private def astForMethodReturn(ktFn: KtNamedFunction, order: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val explicitTypeName =
      if (ktFn.getTypeReference != null) { // TODO: use `Option` for these types of checks
        ktFn.getTypeReference.getText
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
        .code(expr.getStatements.asScala.map(_.getText).mkString("\n"))
        .typeFullName(typeFullName)

    var orderRemainder = 0
    var locals         = List[NewLocal]()
    val expressions =
      withOrder(expr.getStatements) { (statement, order) =>
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
        .filter { expr =>
          expr.ast.root.get.isInstanceOf[NewLocal]
        }
        .map { expr =>
          expr.ast.root.get.asInstanceOf[NewLocal]
        }
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
        childrenCtx.closureBindingInfo,
        childrenCtx.lambdaBindingInfo
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
        .code(expr.getText)
        .order(order)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val child = astsForExpression(expr.getReturnedExpression, scopeContext, 1, 1).headOption
      .getOrElse(AstWithCtx(Ast(), Context()))
    val ast =
      Ast(returnNode)
        .withChild(child.ast)
        .withArgEdges(returnNode, child.ast.root.toList)
    Seq(AstWithCtx(ast, child.ctx))
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
        .code(expr.getText)
        .argumentIndex(argIdx)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .order(order)
        .typeFullName(retType)
    val args =
      astsForExpression(expr.getLeftHandSide, scopeContext, 1, 1) ++ Seq(
        astForTypeReference(expr.getTypeReference, scopeContext, 2, 2)
      )
    val ast = callAst(callNode, args.map(_.ast))
    AstWithCtx(ast, mergedCtx(args.map(_.ctx)))
  }

  def astForBinaryExprWithTypeRHS(
    expr: KtBinaryExpressionWithTypeRHS,
    scopeContext: ScopeContext,
    order: Int,
    argIdx: Int
  )(implicit fileInfo: FileInfo, typeInfoProvider: TypeInfoProvider): AstWithCtx = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val callNode =
      NewCall()
        .name(Operators.cast)
        .methodFullName(Operators.cast)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText)
        .argumentIndex(argIdx)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .order(order)
        .typeFullName(typeFullName)
    val args =
      astsForExpression(expr.getLeft, scopeContext, 1, 1) ++ Seq(astForTypeReference(expr.getRight, scopeContext, 2, 2))
    val ast = callAst(callNode, args.map(_.ast))
    AstWithCtx(ast, mergedCtx(args.map(_.ctx)))
  }

  private def astForTypeReference(expr: KtTypeReference, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider,
    fileInfo: FileInfo
  ): AstWithCtx = {
    val typeFullName = typeInfoProvider.typeFullName(expr, TypeConstants.any)
    registerType(typeFullName)

    val node =
      NewTypeRef()
        .code(expr.getText)
        .order(order)
        .argumentIndex(argIdx)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .typeFullName(typeFullName)
    AstWithCtx(Ast(node), Context())
  }

  @tailrec
  private def astsForExpression(expr: KtExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithCtx] = {
    expr match {
      case blockStmt: KtBlockExpression   => List(astForBlock(blockStmt, scopeContext, order))
      case returnExpr: KtReturnExpression => astsForReturnNode(returnExpr, scopeContext, order)
      case typedExpr: KtCallExpression =>
        val isCtorCall = typeInfoProvider.isConstructorCall(typedExpr)
        if (isCtorCall.getOrElse(false)) {
          Seq(astForCtorCall(typedExpr, scopeContext, order, argIdx))
        } else {
          Seq(astForCall(typedExpr, scopeContext, order, argIdx))
        }
      case typedExpr: KtConstantExpression => Seq(astForLiteral(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtBinaryExpression   => Seq(astForBinaryExpr(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtIsExpression       => Seq(astForIsExpression(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtBinaryExpressionWithTypeRHS =>
        Seq(astForBinaryExprWithTypeRHS(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtNameReferenceExpression if typedExpr.getReferencedNameElementType == KtTokens.IDENTIFIER =>
        Seq(astForNameReference(typedExpr, order, argIdx))
      case _: KtNameReferenceExpression =>
        // TODO: handle this
        Seq()
      case typedExpr: KtProperty if typedExpr.isLocal =>
        astsForProperty(typedExpr, scopeContext, order)
      case typedExpr: KtIfExpression       => Seq(astForIf(typedExpr, scopeContext, order, argIdx))
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
        astsForExpression(typedExpr.getExpression, scopeContext, order, argIdx)
      case typedExpr: KtArrayAccessExpression =>
        Seq(astForArrayAccess(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtLambdaExpression =>
        Seq(astForLambda(typedExpr, scopeContext, order, argIdx))
      case typedExpr: KtNamedFunction =>
        // TODO: re-enable after all AST issues have been fixed
        // Seq(astForAnonymousFunction(typedExpr, scopeContext, order))
        Seq()
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
      case typedExpr: KtDestructuringDeclaration =>
        astsForDestructuringDeclaration(typedExpr, scopeContext, order)
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
    val receiverName       = expr.getReceiverExpression.getText
    val signature          = "java.lang.Class()"
    val methodFullName     = receiverName + "." + getClassMethodName + ":" + signature

    val fullNameWithSignature = typeInfoProvider.fullNameWithSignature(expr, ("", ""))
    val typeFullName          = typeInfoProvider.expressionType(expr, "java.lang.Class")
    registerType(typeFullName)

    val callNode =
      NewCall()
        .name(getClassMethodName)
        .code(expr.getText)
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

  def astForAnonymousFunction(expr: KtNamedFunction, scopeContext: ScopeContext, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val containingFile = expr.getContainingKtFile
    val fileName       = expr.getContainingKtFile.getName
    val lambdaNum      = lambdaKeyPool.next
    val fullName =
      containingFile.getPackageFqName.toString + ":<lambda>" + "<f_" + fileName + "_no" + lambdaNum + ">()"
    val signature = typeInfoProvider.erasedSignature(expr.getValueParameters.asScala.toList)
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
      withOrder(expr.getValueParameters) { (p, order) =>
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

    val finalCtx =
      Context(
        lambdaAsts = Seq(lambdaMethodAst),
        identifiers = bodyAstWithCtx.ctx.identifiers,
        closureBindingInfo = bodyAstWithCtx.ctx.closureBindingInfo,
        lambdaBindingInfo = bodyAstWithCtx.ctx.lambdaBindingInfo,
        bindingsInfo = bodyAstWithCtx.ctx.bindingsInfo
      )
    AstWithCtx(methodRefAst, finalCtx)
  }

  def astForLambda(expr: KtLambdaExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {

    val parametersWithCtx =
      withOrder(expr.getValueParameters) { (p, order) =>
        astForParameter(p, order)
      }
    val lastOrder = parametersWithCtx.size + 2

    val bodyAstWithCtx   = astForMethodBody(expr.getBodyExpression, scopeContext, lastOrder)
    val bodyIndentifiers = bodyAstWithCtx.ctx.identifiers // TODO: delete refs to global decls

    val methodParameterNodes: Seq[NewMethodParameterIn] =
      parametersWithCtx
        .map(_.ast)
        .filter(_.root.get.isInstanceOf[NewMethodParameterIn])
        .map(_.root.get.asInstanceOf[NewMethodParameterIn])
    val namesToMethodParams =
      methodParameterNodes.map { node =>
        node.name -> node
      }.toMap
    val (identifiersMatchingMethodParams, identifiersNotMatchingMethodParams) = {
      bodyAstWithCtx.ctx.identifiers.partition { ident =>
        namesToMethodParams.contains(ident.name)
      }
    }
    val refEdgePairs =
      identifiersMatchingMethodParams.flatMap { ident =>
        val methodParamInNode = namesToMethodParams.get(ident.name)
        methodParamInNode match {
          case Some(mp) =>
            Some((ident, mp))
          case _ => None
        }
      }

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

    val fullNameWithSig    = typeInfoProvider.fullNameWithSignature(expr, lambdaKeyPool)
    val returnTypeFullName = typeInfoProvider.returnTypeFullName(expr)
    registerType(returnTypeFullName)

    val lambdaTypeDeclFullName = fullNameWithSig._1.split(":").head
    val methodRef =
      NewMethodRef()
        .code("")
        .methodFullName(fullNameWithSig._1)
        .typeFullName(lambdaTypeDeclFullName)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .order(order)
        .argumentIndex(argIdx)
    val methodReturnNode =
      NewMethodReturn()
        .order(lastOrder + 1)
        .evaluationStrategy(EvaluationStrategies.BY_VALUE)
        .typeFullName(returnTypeFullName)
        .code(Constants.retCode)
        .lineNumber(line(expr))
        .columnNumber(column(expr))

    val lambdaModifierNode =
      NewModifier()
        .modifierType(ModifierTypes.VIRTUAL)
    val lambdaNode =
      NewMethod()
        .name(Constants.lambdaName)
        .code(expr.getText)
        .isExternal(false)
        .fullName(fullNameWithSig._1)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .signature(fullNameWithSig._2)
        .filename(relativizedPath)

    val lambdaMethodAst =
      Ast(lambdaNode)
        .withChildren(parametersWithCtx.map(_.ast))
        .withChild(bodyAstWithCtx.ast.withChildren(localsForCapturedIdentifiers))
        .withChild(Ast(methodReturnNode))
        .withChild(Ast(lambdaModifierNode))
    val lamdbaMethodAstWithRefEdges =
      refEdgePairs.foldLeft(lambdaMethodAst)((acc, nodes) => {
        acc.withRefEdge(nodes._1, nodes._2)
      })

    val methodRefAst =
      Ast(methodRef)

    // TODO: improve matching
    // TODO: remove the matched identifiers from the returned context
    val nameToScopeMethodParameter = scopeContext.methodParameters.map { p =>
      p.name -> p
    }.toMap
    val nameToScopeLocal = scopeContext.locals.map { p =>
      p.name -> p
    }.toMap
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

    val lambdaTypeDeclInheritsFromTypeFullName =
      TypeConstants.kotlinFunctionXPrefix + expr.getValueParameters.size
    val lambdaTypeDecl =
      NewTypeDecl()
        .code("")
        .name(Constants.lambdaTypeDeclName)
        .inheritsFromTypeFullName(Seq(lambdaTypeDeclInheritsFromTypeFullName))
        .fullName(lambdaTypeDeclFullName)
        .isExternal(true)
        .filename(relativizedPath)
    val lambdaBinding =
      NewBinding()
        .name(Constants.lambdaBindingName)
        .signature(fullNameWithSig._2)
    val bindingInfo = BindingInfo(
      lambdaBinding,
      Seq((lambdaTypeDecl, lambdaBinding, EdgeTypes.BINDS), (lambdaBinding, lambdaNode, EdgeTypes.REF))
    )

    val finalCtx =
      Context(
        lambdaAsts = Seq(lamdbaMethodAstWithRefEdges) ++ bodyAstWithCtx.ctx.lambdaAsts,
        identifiers = bodyAstWithCtx.ctx.identifiers,
        closureBindingInfo = closureBindingInfo ++ bodyAstWithCtx.ctx.closureBindingInfo,
        lambdaBindingInfo = Seq(bindingInfo)
      )

    AstWithCtx(methodRefAst, finalCtx)
  }

  def astForArrayAccess(expr: KtArrayAccessExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val identifierElem = expr.getArrayExpression
    val typeFullName   = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val identifier =
      NewIdentifier()
        .name(identifierElem.getText)
        .order(1)
        .argumentIndex(1)
        .code(identifierElem.getText)
        .typeFullName(typeFullName)
        .lineNumber(line(identifierElem))
        .columnNumber(column(identifierElem))
    val indexExpr =
      if (expr.getIndexExpressions.size >= 1) {
        Some(expr.getIndexExpressions.get(0))
      } else {
        None
      }
    val callNode =
      NewCall()
        .name(Operators.indexAccess)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText)
        .order(order)
        .argumentIndex(argIdx)
        .typeFullName(typeFullName)
        .methodFullName(Operators.indexAccess)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val astsForIndexExpr = indexExpr match {
      case Some(ie) =>
        astsForExpression(ie, scopeContext, 2, 2)
      case None =>
        List()
    }
    val call = callAst(callNode, Seq(Ast(identifier)))
    val finalAst =
      call
        .withChildren(astsForIndexExpr.map(_.ast))
        .withArgEdges(callNode, astsForIndexExpr.map(_.ast.root.get))
    AstWithCtx(finalAst, Context(identifiers = List(identifier)))
  }

  def astForPostfixExpression(expr: KtPostfixExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val operatorType =
      expr.getOperationToken match {
        case KtTokens.PLUSPLUS   => Operators.postIncrement
        case KtTokens.MINUSMINUS => Operators.postDecrement
        case KtTokens.EXCLEXCL   => Operators.notNullAssert
        case _ =>
          logger.warn("Creating empty AST node for unknown postfix expr: " + expr.getOperationToken)
          Constants.unknownOperator
      }

    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val callNode =
      NewCall()
        .name(operatorType)
        .methodFullName(operatorType)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText)
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
      expr.getOperationToken match {
        case KtTokens.EXCL       => Operators.logicalNot
        case KtTokens.PLUS       => Operators.plus
        case KtTokens.MINUS      => Operators.minus
        case KtTokens.PLUSPLUS   => Operators.preIncrement
        case KtTokens.MINUSMINUS => Operators.preDecrement
        case _ =>
          logger.warn("Creating empty AST node for unknown prefix expr: " + expr.getOperationToken)
          Constants.unknownOperator
      }
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val callNode =
      NewCall()
        .name(operatorType)
        .methodFullName(operatorType)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText)
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

  def astsForDestructuringDeclarationWithCtorRHS(
    expr: KtDestructuringDeclaration,
    scopeContext: ScopeContext,
    order: Int
  )(implicit fileInfo: FileInfo, typeInfoProvider: TypeInfoProvider): Seq[AstWithCtx] = {
    Seq(AstWithCtx(Ast(), Context()))
  }

  /*
   _______ example lowering _________
  | val (one, two) = person
  |
  | -> LOCAL one
  | -> LOCAL two
  | -> CALL one = person.component1()
  | -> CALL two = person.component1()
  |__________________________________
   */
  def astsForDestructuringDeclarationWithVarRHS(
    expr: KtDestructuringDeclaration,
    scopeContext: ScopeContext,
    order: Int
  )(implicit fileInfo: FileInfo, typeInfoProvider: TypeInfoProvider): Seq[AstWithCtx] = {
    val typedInit = {
      expr.getInitializer match {
        case typed: KtNameReferenceExpression => Some(typed)
        case _                                => None
      }
    }
    if (typedInit.isEmpty) {
      logger.warn(s"Unhandled case for destructuring declaration: `${expr.getText}`.")
      return Seq()
    }
    val destructuringRHS = typedInit.get
    val localsForEntries =
      expr.getEntries.asScala.zipWithIndex.map { entryWithIdx =>
        val entry        = entryWithIdx._1
        val orderForNode = entryWithIdx._2 + order

        val typeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
        registerType(typeFullName)

        val node =
          NewLocal()
            .code(entry.getText)
            .name(entry.getName)
            .typeFullName(typeFullName)
            .order(orderForNode)
            .lineNumber(line(entry))
            .columnNumber(column(entry))
        Ast(node)
      }.toSeq

    val orderAfterLocals = localsForEntries.size + order
    val assignmentsForEntries =
      expr.getEntries.asScala.zipWithIndex.map { entryWithIdx =>
        val entry             = entryWithIdx._1
        val entryTypeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
        registerType(entryTypeFullName)

        val assignmentLHSNode =
          NewIdentifier()
            .name(entry.getName)
            .code(entry.getText)
            .typeFullName(entryTypeFullName)
            .order(1)
            .argumentIndex(1)
            .lineNumber(line(entry))
            .columnNumber(column(entry))
        val relevantLocal = localsForEntries(entryWithIdx._2).root.get
        val assignmentLHSAst =
          Ast(assignmentLHSNode)
            .withRefEdge(assignmentLHSNode, relevantLocal)

        val componentNIdentifierTFN = typeInfoProvider.typeFullName(destructuringRHS, TypeConstants.any)
        registerType(componentNIdentifierTFN)

        val componentNIdentifierNode =
          NewIdentifier()
            .code(destructuringRHS.getText)
            .name(destructuringRHS.getName)
            .order(1)
            .argumentIndex(0)
            .typeFullName(componentNIdentifierTFN)
            .lineNumber(line(entry))
            .columnNumber(column(entry))

        val componentIdx      = entryWithIdx._2 + 1
        val fallbackSignature = TypeConstants.cpgUnresolved + "()"
        val fallbackFullName =
          TypeConstants.cpgUnresolved + Constants.componentNSuffix + componentIdx + ":" + fallbackSignature
        val componentNFullNameWithSignature =
          typeInfoProvider.fullNameWithSignature(entry, (fallbackFullName, fallbackSignature))
        val componentNCallCode = destructuringRHS.getText + "." + Constants.componentNSuffix + componentIdx + "()"
        val componentNCallNode =
          NewCall()
            .code(componentNCallCode)
            .methodFullName(componentNFullNameWithSignature._1)
            .signature(componentNFullNameWithSignature._2)
            .typeFullName(entryTypeFullName)
            .order(2)
            .argumentIndex(2)
            .dispatchType(DispatchTypes.DYNAMIC_DISPATCH)
            .lineNumber(line(entry))
            .columnNumber(column(entry))

        val matchingLocal =
          scopeContext.locals.filter { l =>
            l.name == destructuringRHS.getText
          }.headOption
        val matchingMethodParam =
          scopeContext.methodParameters.filter { l =>
            l.name == destructuringRHS.getText
          }.headOption

        val matchingRefOption =
          if (matchingLocal.isDefined) {
            Some(matchingLocal.get)
          } else if (matchingMethodParam.isDefined) {
            Some(matchingMethodParam.get)
          } else {
            None
          }
        val componentNIdentifierAst =
          if (matchingRefOption.isDefined) {
            Ast(componentNIdentifierNode)
              .withRefEdge(componentNIdentifierNode, matchingRefOption.get)
          } else {
            Ast(componentNIdentifierNode)
          }
        val componentNAst =
          Ast(componentNCallNode)
            .withChild(componentNIdentifierAst)
            .withArgEdge(componentNCallNode, componentNIdentifierNode)
            .withReceiverEdge(componentNCallNode, componentNIdentifierNode)

        val orderForNode = orderAfterLocals + entryWithIdx._2
        val assignmentCallNode =
          NewCall()
            .code(entry.getText + " = " + componentNCallCode)
            .methodFullName(Operators.assignment)
            .signature("")
            .dispatchType(DispatchTypes.STATIC_DISPATCH)
            .order(orderForNode)
            .lineNumber(line(entry))
            .columnNumber(column(entry))

        val assignmentAst =
          Ast(assignmentCallNode)
            .withChild(assignmentLHSAst)
            .withArgEdge(assignmentCallNode, assignmentLHSNode)
            .withChild(componentNAst)
            .withArgEdge(assignmentCallNode, componentNCallNode)
        assignmentAst
      }.toSeq

    localsForEntries.map(AstWithCtx(_, Context())) ++
      assignmentsForEntries.map(AstWithCtx(_, Context()))
  }

  def astsForDestructuringDeclaration(expr: KtDestructuringDeclaration, scopeContext: ScopeContext, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithCtx] = {
    val isCtor = expr.getInitializer match {
      case typedExpr: KtCallExpression =>
        typeInfoProvider
          .isConstructorCall(typedExpr)
          .getOrElse(false)
      case _ => false
    }
    if (isCtor) {
      astsForDestructuringDeclarationWithCtorRHS(expr, scopeContext, order)
    } else {
      astsForDestructuringDeclarationWithVarRHS(expr, scopeContext, order)
    }
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

  def astForStringTemplate(expr: KtStringTemplateExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(
    implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    if (expr.hasInterpolation) {
      val callNode =
        NewCall()
          .name(Operators.formatString)
          .methodFullName(Operators.formatString)
          .dispatchType(DispatchTypes.STATIC_DISPATCH)
          .code(expr.getText)
          .argumentIndex(argIdx)
          .lineNumber(line(expr))
          .columnNumber(column(expr))
          .order(order)
          .typeFullName(typeFullName)
      val args =
        expr.getEntries
          .filter { entry =>
            entry.getExpression != null
          }
          .zipWithIndex
          .flatMap { case (entry, idx) =>
            if (entry.getExpression != null) {
              val valueCallNode =
                NewCall()
                  .name(Operators.formattedValue)
                  .methodFullName(Operators.formattedValue)
                  .dispatchType(DispatchTypes.STATIC_DISPATCH)
                  .code(entry.getExpression.getText)
                  .argumentIndex(idx + 1)
                  .lineNumber(line(entry.getExpression))
                  .columnNumber(column(entry.getExpression))
                  .order(idx + 1)
                  .typeFullName(TypeConstants.javaLangString)
              val valueArgs = astsForExpression(entry.getExpression, scopeContext, idx + 1, idx + 1)
              val call      = callAst(valueCallNode, valueArgs.map(_.ast))
              Seq(AstWithCtx(call, Context()))
            } else {
              Seq()
            }
          }
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

  def astForQualifiedExpression(expr: KtQualifiedExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(
    implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val callKind        = typeInfoProvider.bindingKind(expr)
    val isStaticCall    = callKind == CallKinds.StaticCall
    val isDynamicCall   = callKind == CallKinds.DynamicCall
    val isExtensionCall = callKind == CallKinds.ExtensionCall

    val isCallToSuper = expr.getReceiverExpression match {
      case _: KtSuperExpression => true
      case _                    => false
    }

    val orderForReceiver = 1
    val argIdxForReceiver =
      if (isCallToSuper) 0
      else if (isDynamicCall) 0
      else if (isExtensionCall) 0
      else if (isStaticCall) 1
      else 1
    val receiverExpr = expr.getReceiverExpression
    val receiverAstWithCtx: AstWithCtx =
      receiverExpr match {
        case typedExpr: KtConstantExpression =>
          astForLiteral(typedExpr, scopeContext, orderForReceiver, argIdxForReceiver)
        case typedExpr: KtNameReferenceExpression =>
          astForNameReference(typedExpr, orderForReceiver, argIdxForReceiver)
        case thisExpr: KtThisExpression =>
          astForThisExpression(thisExpr, scopeContext, orderForReceiver, 0)
        case superExpr: KtSuperExpression =>
          val typeFullName = typeInfoProvider.expressionType(superExpr, TypeConstants.any)
          registerType(typeFullName)

          val node =
            NewIdentifier()
              .code(superExpr.getText)
              .typeFullName(typeFullName)
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
          val astsWithCtx = astsForExpression(typedExpr, scopeContext, orderForReceiver, argIdxForReceiver)
          // TODO: get to the root cause of why the asts here are empty; write unit tests
          // e.g. for when this is the case in https://github.com/detekt/detekt
          val astForExpr = {
            if (astsWithCtx.nonEmpty) {
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
          val isCtorCall = typeInfoProvider.isConstructorCall(typedExpr)
          if (isCtorCall.getOrElse(false)) {
            astForCtorCall(typedExpr, scopeContext, orderForReceiver, argIdxForReceiver)
          } else {
            astForCall(typedExpr, scopeContext, orderForReceiver, argIdxForReceiver)
          }
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
    val receiverAst = receiverAstWithCtx.ast

    val selectorOrderCount = argIdxForReceiver
    val argAsts =
      expr.getSelectorExpression match {
        case selectorExpression: KtCallExpression =>
          withOrder(selectorExpression.getValueArguments) { case (arg, order) =>
            val selectorOrder    = if (isStaticCall) order else selectorOrderCount + order + 1
            val selectorArgIndex = if (isStaticCall) order else selectorOrder - 1
            val asts =
              astsForExpression(arg.getArgumentExpression, scopeContext, selectorOrder, selectorArgIndex)
            asts
          }.flatten
        case typedExpr: KtNameReferenceExpression =>
          val order  = if (isStaticCall) 1 else 2
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
      } else
        expr.getSelectorExpression match {
          case expression: KtCallExpression =>
            val receiverPlaceholderType = TypeConstants.cpgUnresolved
            val shortName               = expr.getSelectorExpression.getFirstChild.getText
            val args                    = expression.getValueArguments
            receiverPlaceholderType + "." + shortName + ":" + typeInfoProvider.erasedSignature(args.asScala.toList)
          case _: KtNameReferenceExpression =>
            Operators.fieldAccess
          case _ =>
            // TODO: add more test cases for this scenario
            ""
        }
    }

    val astDerivedSignature =
      if (astDerivedMethodFullName.startsWith(Constants.operatorSuffix)) {
        ""
      } else {
        typeInfoProvider.erasedSignature(argAsts)
      }
    val fullNameWithSig =
      typeInfoProvider.fullNameWithSignature(expr, (astDerivedMethodFullName, astDerivedSignature))
    val declType = typeInfoProvider.containingDeclType(expr, TypeConstants.any)
    registerType(declType)

    val retType = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(retType)

    val dispatchType =
      if (callKind == CallKinds.DynamicCall) {
        DispatchTypes.DYNAMIC_DISPATCH
      } else if (callKind == CallKinds.ExtensionCall) {
        DispatchTypes.STATIC_DISPATCH
      } else {
        DispatchTypes.STATIC_DISPATCH
      }
    val methodName = expr.getSelectorExpression.getFirstChild.getText
    val callNode =
      NewCall()
        .order(order)
        .argumentIndex(argIdx)
        .code(expr.getText)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .typeFullName(retType)
        .name(methodName)
        .methodFullName(fullNameWithSig._1)
        .dispatchType(dispatchType)
        .signature(fullNameWithSig._2)
    val root         = Ast(callNode)
    val receiverNode = receiverAst.root.get
    val finalAst = {
      if (isExtensionCall || isCallToSuper) {
        root
          .withChild(receiverAst)
          .withArgEdge(callNode, receiverNode)
          .withChildren(argAsts.map(_.ast))
          .withArgEdges(callNode, argAsts.map(_.ast.root.get))
      } else if (isStaticCall) {
        root
          .withChild(receiverAst)
          .withChildren(argAsts.map(_.ast))
          .withArgEdges(callNode, argAsts.map(_.ast.root.get))
      } else {
        val ast =
          root
            .withChild(receiverAst)
            .withArgEdge(callNode, receiverNode)
            .withChildren(argAsts.map(_.ast))
            .withArgEdges(callNode, argAsts.map(_.ast.root.get))
        if (argAsts.size == 1 && argAsts.head.ast.root.get.isInstanceOf[NewMethodRef]) {
          ast
            .withReceiverEdge(callNode, argAsts.head.ast.root.get)
        } else {
          ast
            .withReceiverEdge(callNode, receiverNode)
        }
      }
    }
    val finalCtx = mergedCtx(argAsts.map(_.ctx) ++ Seq(receiverAstWithCtx.ctx))
    AstWithCtx(finalAst, finalCtx)
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

  def astForContinue(expr: KtContinueExpression, scopeContext: ScopeContext, order: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
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
      if (expr.getFinallyBlock == null) {
        Seq()
      } else {
        val numClauses = clauseAstsWitCtx.size
        astsForExpression(expr.getFinallyBlock.getFinalExpression, scopeContext, numClauses + 2, numClauses + 2)
      }

    val tryWithClausesAst =
      tryAst
        .withChildren(clauseAstsWitCtx.map(_.ast))
    val finalAst =
      if (finallyAstsWithCtx.nonEmpty) {
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

    val ast = {
      conditionAst.ast.root match {
        case Some(r) =>
          tempAst.withConditionEdge(whileNode, r)
        case None =>
          tempAst
      }
    }
    val finalCtx = mergedCtx(Seq(conditionAst.ctx) ++ stmtAsts.map(_.ctx))
    AstWithCtx(ast, finalCtx)
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
    val conditionAst = astsForExpression(expr.getCondition, scopeContext, 2, 2).headOption
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
    val loopAsts = if (expr.getChildren.toList.size >= 2) {
      val loopExpr = expr.getChildren.toList(1)
      loopExpr match {
        case e: KtContainerNode =>
          e.getFirstChild match {
            case typed: KtExpression =>
              astsForExpression(typed, scopeContext, 1, 1)
            case _ => Seq()
          }
        case _ =>
          Seq()
      }

    } else {
      Seq()
    }
    val stmtAst = astsForExpression(expr.getBody, scopeContext, loopAsts.size + 1, loopAsts.size + 1)

    val ast =
      if (loopAsts.nonEmpty) {
        Ast(forNode)
          .withChildren(loopAsts.map(_.ast))
          .withChildren(stmtAst.map(_.ast))
      } else {
        Ast(forNode)
          .withChildren(stmtAst.map(_.ast))
      }
    AstWithCtx(ast, mergedCtx(stmtAst.map(_.ctx) ++ loopAsts.map(_.ctx)))
  }

  def astForWhen(expr: KtWhenExpression, scopeContext: ScopeContext, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val astForSubject =
      astsForExpression(expr.getSubjectExpression, scopeContext, 1, 1).headOption
        .getOrElse(AstWithCtx(Ast(), Context()))

    val astsForEntries =
      withOrder(expr.getEntries) { (e, order) =>
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
        .withChildren(astsForEntries.map(_.ast))

    val codeForSwitch =
      if (expr.getSubjectExpression != null) {
        s"when(${expr.getSubjectExpression.getText})"
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
    val astWithCondition = {
      astForSubject.ast.root match {
        case Some(root) =>
          ast.withConditionEdge(switchNode, root)
        case None =>
          ast
      }
    }
    val finalCtx = mergedCtx(astsForEntries.map(_.ctx))
    AstWithCtx(astWithCondition, finalCtx)
  }

  def astsForWhenEntry(entry: KtWhenEntry, scopeContext: ScopeContext, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithCtx] = {
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
    val jumpNodeAstsWithCtx = AstWithCtx(Ast(jumpNode), Context())
    Seq(jumpNodeAstsWithCtx) ++ Seq(exprNode)
  }

  def astForIf(expr: KtIfExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    if (expr.getParent.isInstanceOf[KtProperty]) {
      astForIfAsExpression(expr, scopeContext, order, argIdx)
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
    val conditionAst = astsForExpression(expr.getCondition, scopeContext, 1, 1)
    val thenAsts     = astsForExpression(expr.getThen, scopeContext, 2, 2)
    val elseAsts     = astsForExpression(expr.getElse, scopeContext, 3, 3)

    val ast = Ast(ifNode)
      .withChild(conditionAst.head.ast)
      .withChildren(thenAsts.map(_.ast) ++ elseAsts.map(_.ast))

    val withCondition = conditionAst.head.ast.root match {
      case Some(r) =>
        ast.withConditionEdge(ifNode, r)
      case None =>
        ast
    }
    val finalCtx = mergedCtx(conditionAst.map(_.ctx) ++ thenAsts.map(_.ctx) ++ elseAsts.map(_.ctx))
    AstWithCtx(withCondition, finalCtx)
  }

  def astForIfAsExpression(expr: KtIfExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val retType = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(retType)

    val callNode =
      NewCall()
        .name(Operators.conditional)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText)
        .order(order)
        .argumentIndex(argIdx)
        .typeFullName(retType)
        .methodFullName(Operators.conditional)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val conditionAsts = astsForExpression(expr.getCondition, scopeContext, 1, 1)
    val thenAsts      = astsForExpression(expr.getThen, scopeContext, 2, 2)
    val elseAsts      = astsForExpression(expr.getElse, scopeContext, 3, 3)

    val childAsts = conditionAsts.map(_.ast) ++ thenAsts.map(_.ast) ++ elseAsts.map(_.ast)
    val ctx       = mergedCtx(conditionAsts.map(_.ctx) ++ thenAsts.map(_.ctx) ++ elseAsts.map(_.ctx))
    if (conditionAsts.nonEmpty && conditionAsts.head.ast.root != null) {
      val ast =
        Ast(callNode)
          .withChildren(childAsts)
          .withArgEdges(callNode, childAsts.map(_.root.get))
      AstWithCtx(ast, ctx)
    } else {
      logger.warn("Parsing failed for expr `" + expr.getName + "` in file `" + fileWithMeta.filename + "`.")
      logger.debug(" > expr.text `" + expr.getText + "`")

      val unknownNode =
        NewUnknown()
          .code(expr.getText)
          .order(order)
          .argumentIndex(order)
          .lineNumber(line(expr))
          .columnNumber(column(expr))
      AstWithCtx(Ast(unknownNode), Context())
    }
  }

  private def astForCtorCall(expr: KtCallExpression, scopeContext: ScopeContext, order: Int = 1, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.cpgUnresolved)
    registerType(typeFullName)

    val tmpBlockNode =
      NewBlock()
        .code("")
        .typeFullName(typeFullName)
        .order(order)
        .argumentIndex(argIdx)
    val tmpName = "tmp_" + tmpKeyPool.next
    val tmpLocalNode =
      NewLocal()
        .code(tmpName)
        .name(tmpName)
        .typeFullName(typeFullName)
        .order(1)
    val assignmentRhsNode =
      NewCall()
        .name("<operator>.alloc")
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code("alloc")
        .order(2)
        .argumentIndex(2)
        .typeFullName(typeFullName)
        .methodFullName("<operator>.alloc")
        .lineNumber(line(expr))
        .columnNumber(column(expr))

    // TODO: add check here for the `.get`
    val assignmentLhsNode =
      NewIdentifier()
        .name(tmpName)
        .order(1)
        .argumentIndex(0)
        .code(tmpName)
        .typeFullName(typeFullName)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val assignmentNode =
      NewCall()
        .name(Operators.assignment)
        .code(Operators.assignment)
        .methodFullName(Operators.assignment)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .signature("")
        .order(2)
    val assignmentAst =
      Ast(assignmentNode)
        .withChild(Ast(assignmentLhsNode))
        .withChild(Ast(assignmentRhsNode))
        .withArgEdges(assignmentNode, Seq(assignmentLhsNode, assignmentRhsNode))

    val initReceiverNode =
      NewIdentifier()
        .name(tmpName)
        .order(1)
        .argumentIndex(0)
        .code(tmpName)
        .typeFullName(typeFullName)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val initReceiverAst = Ast(initReceiverNode)

    val args = expr.getValueArguments
    val argAsts =
      withOrder(args) { case (arg, argOrder) =>
        astsForExpression(arg.getArgumentExpression, scopeContext, argOrder, argOrder)
      }.flatten

    val fullNameWithSig = typeInfoProvider.fullNameWithSignature(expr, (TypeConstants.any, TypeConstants.any))
    val returnType      = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(returnType)

    val initCallNode =
      NewCall()
        .name(Constants.init)
        .code(expr.getText)
        .order(3)
        .argumentIndex(2)
        .methodFullName(fullNameWithSig._1)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(TypeConstants.void)
        .signature(fullNameWithSig._2)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val initCallAst =
      Ast(initCallNode)
        .withChild(initReceiverAst)
        .withChildren(argAsts.map(_.ast))
        .withArgEdges(initCallNode, Seq(initReceiverNode) ++ argAsts.flatMap(_.ast.root))

    val lastIdentifier =
      NewIdentifier()
        .name(tmpName)
        .order(3)
        .code(tmpName)
        .typeFullName(typeFullName)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val lastIdentifierAst = Ast(lastIdentifier)
    val tmpLocalAst =
      Ast(tmpLocalNode)
        .withRefEdge(assignmentLhsNode, tmpLocalNode)
        .withRefEdge(initReceiverNode, tmpLocalNode)
        .withRefEdge(lastIdentifier, tmpLocalNode)
    val blockAst =
      Ast(tmpBlockNode)
        .withChild(tmpLocalAst)
        .withChild(assignmentAst)
        .withChild(initCallAst)
        .withChild(lastIdentifierAst)

    val initArgsCtx = mergedCtx(argAsts.map(_.ctx))
    AstWithCtx(blockAst, initArgsCtx)
  }

  private def astsForProperty(expr: KtProperty, scopeContext: ScopeContext, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithCtx] = {
    val explicitTypeName =
      if (expr.getTypeReference != null) {
        expr.getTypeReference.getText
      } else {
        TypeConstants.any
      }
    val elem         = expr.getIdentifyingElement
    val typeFullName = typeInfoProvider.propertyType(expr, explicitTypeName)
    registerType(typeFullName)

    val identifier =
      NewIdentifier()
        .name(elem.getText)
        .order(1)
        .argumentIndex(1)
        .code(elem.getText)
        .typeFullName(typeFullName)
        .lineNumber(line(elem))
        .columnNumber(column(elem))
    val assignmentNode =
      NewCall()
        .name(Operators.assignment)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText)
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

    val hasRHSCtorCall = expr.getDelegateExpressionOrInitializer match {
      case typed: KtCallExpression =>
        typeInfoProvider.isConstructorCall(typed).getOrElse(false)
      case _ => false
    }
    val rhsAsts = if (hasRHSCtorCall) {
      Seq(astForCtorCall(expr.getDelegateExpressionOrInitializer.asInstanceOf[KtCallExpression], scopeContext, 2, 2))
    } else {
      astsForExpression(expr.getDelegateExpressionOrInitializer, scopeContext, 2, 2)
    }
    val call = callAst(assignmentNode, Seq(Ast(identifier)) ++ rhsAsts.map(_.ast))

    val rhsCtx = mergedCtx(rhsAsts.map(_.ctx))
    val finalCtx = Context(
      rhsCtx.locals,
      rhsCtx.identifiers ++ List(identifier),
      Seq(),
      rhsCtx.bindingsInfo,
      rhsCtx.lambdaAsts,
      rhsCtx.closureBindingInfo,
      rhsCtx.lambdaBindingInfo
    )
    Seq(AstWithCtx(call, Context())) ++
      Seq(AstWithCtx(Ast(localNode).withRefEdge(identifier, localNode), finalCtx))
  }

  def astForNameReference(expr: KtNameReferenceExpression, order: Int, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    typeInfoProvider.isReferencingMember(expr) match {
      case true  => astForNameReferenceToMember(expr, order, argIdx)
      case false => astForNonSpecialNameReference(expr, order, argIdx)
    }
  }

  private def astForNameReferenceToMember(expr: KtNameReferenceExpression, order: Int, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val typeFullName = typeInfoProvider.typeFullName(expr, TypeConstants.any)
    registerType(typeFullName)

    val callNode =
      NewCall()
        .name(expr.getReferencedName)
        .code(Constants.this_ + "." + expr.getReferencedName)
        .dispatchType(DispatchTypes.DYNAMIC_DISPATCH)
        .methodFullName(Operators.fieldAccess)
        .signature("")
        .typeFullName(typeFullName)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .order(order)
        .argumentIndex(argIdx)

    val referenceTargetTypeFullName = typeInfoProvider.referenceTargetTypeFullName(expr, TypeConstants.any)
    registerType(referenceTargetTypeFullName)

    val thisNode =
      NewIdentifier()
        .code(Constants.this_)
        .name(Constants.this_)
        .typeFullName(referenceTargetTypeFullName)
        .order(1)
        .argumentIndex(0)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val fieldIdentifierNode =
      NewFieldIdentifier()
        .code(expr.getReferencedName)
        .canonicalName(expr.getReferencedName)
        .order(2)
        .argumentIndex(1)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
    val ast =
      Ast(callNode)
        .withChild(Ast(thisNode))
        .withChild(Ast(fieldIdentifierNode))
        .withArgEdge(callNode, thisNode)
        .withArgEdge(callNode, fieldIdentifierNode)
        .withReceiverEdge(callNode, thisNode)
    AstWithCtx(ast, Context())
  }

  private def astForNonSpecialNameReference(expr: KtNameReferenceExpression, order: Int, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val typeFullName = typeInfoProvider.typeFullName(expr, TypeConstants.any)
    registerType(typeFullName)

    val name = expr.getIdentifier.getText
    val identifierNode =
      NewIdentifier()
        .name(name)
        .order(order)
        .argumentIndex(argIdx)
        .code(name)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .typeFullName(typeFullName)

    val nameRefKind = typeInfoProvider.nameReferenceKind(expr)
    val identifiersForCtx =
      if (nameRefKind == NameReferenceKinds.ClassName) {
        Seq()
      } else {
        Seq(identifierNode)
      }
    AstWithCtx(Ast(identifierNode), Context(identifiers = identifiersForCtx))
  }

  def astForLiteral(expr: KtConstantExpression, scopeContext: ScopeContext, order: Int, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
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
    val opRef = expr.getOperationReference

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
          val opElement = expr.getOperationReference.getReferencedNameElement
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
            "Unhandled operator token type `" + opRef.getOperationSignTokenType + "` for expression `" + expr.getText + "`."
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
    val typeFullName = typeInfoProvider.typeFullName(expr, TypeConstants.any)
    val name = if (operatorOption.isDefined) {
      operatorOption.get
    } else if (expr.getChildren.toList.size >= 2) {
      expr.getChildren.toList(1).getText
    } else {
      expr.getName
    }
    val callNode =
      NewCall()
        .name(name)
        .methodFullName(fullName)
        .signature(signature)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .code(expr.getText)
        .argumentIndex(argIdx)
        .lineNumber(line(expr))
        .columnNumber(column(expr))
        .order(order)
        .typeFullName(typeFullName)

    val args =
      astsForExpression(expr.getLeft, scopeContext, 1, 1) ++ astsForExpression(expr.getRight, scopeContext, 2, 2)
    val ast = callAst(callNode, args.map(_.ast))
    AstWithCtx(ast, mergedCtx(args.map(_.ctx)))
  }

  def callAst(rootNode: NewNode, args: Seq[Ast]): Ast = {
    Ast(rootNode)
      .withChildren(args)
      .withArgEdges(rootNode, args.flatMap(_.root))
  }

  private def astForCall(expr: KtCallExpression, scopeContext: ScopeContext, order: Int = 1, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithCtx = {
    val args = expr.getValueArguments
    val argAsts =
      withOrder(args) { case (arg, argOrder) =>
        astsForExpression(arg.getArgumentExpression, scopeContext, argOrder, argOrder)
      }.flatten

    // TODO: add tests for the empty `referencedName` here
    val referencedName =
      expr.getFirstChild match {
        case c: KtNameReferenceExpression => c.getText
        case _                            => ""
      }

    val nameToClass = fileInfo.classes.map { klass =>
      klass.getName -> klass
    }.toMap
    val importedNames =
      fileInfo.imports.map { imp =>
        imp.name -> imp
      }.toMap
    val methodFqName = {
      if (importedNames.isDefinedAt(referencedName)) {
        importedNames(referencedName).fqName
      } else if (nameToClass.contains(expr.getCalleeExpression.getText)) {
        val klass = nameToClass(expr.getCalleeExpression.getText)
        klass.getContainingKtFile.getPackageFqName.toString + "." + referencedName
      } else {
        expr.getContainingKtFile.getPackageFqName.toString + "." + referencedName
      }
    }
    val signature =
      TypeConstants.any + "(" + args.asScala
        .map { _ =>
          TypeConstants.any
        }
        .mkString(",") + ")"

    val fullName        = methodFqName + ":" + signature
    val fullNameWithSig = typeInfoProvider.fullNameWithSignature(expr, (fullName, signature))

    // TODO: add test case to confirm whether the ANY fallback makes sense (could be void)
    val returnType = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(returnType)

    val callNode =
      NewCall()
        .name(referencedName)
        .code(expr.getText)
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

    val finalCtx = mergedCtx(argAsts.map(_.ctx))
    AstWithCtx(ast, finalCtx)
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
      if (decl.getName != null) {
        decl.getName
      } else {
        TypeConstants.any
      }
    val code = name

    val explicitTypeName =
      decl.getOriginalElement match {
        case p: KtProperty if p.getTypeReference != null => p.getTypeReference.getText
        case _                                           => TypeConstants.any
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
      param.getName
    }
    val explicitTypeName =
      if (param.getTypeReference != null) {
        param.getTypeReference.getText
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

  private def createMethodNode(expr: KtNamedFunction, childNum: Int)(implicit typeInfoProvider: TypeInfoProvider) = {
    val fnWithSig = typeInfoProvider.fullNameWithSignature(expr, ("", ""))
    val code      = codeForFn(expr)
    val methodNode =
      NewMethod()
        .name(expr.getName)
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
        val nodeParams = ktFunction.getValueParameters
        nodeParams.asScala
          .map { p =>
            val paramTypeName =
              if (p.getTypeReference != null) {
                p.getTypeReference.getText
              } else { TypeConstants.any }
            val paramName = p.getName
            paramName + ":" + paramTypeName
          }
      } catch {
        case _: Throwable => List()
      }
    val returnTypeName =
      if (ktFunction.getTypeReference != null) {
        ktFunction.getTypeReference.getText
      } else {
        ""
      }
    returnTypeName + "(" + paramTypesWithName.mkString(", ") + ")"
  }
}
