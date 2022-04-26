package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.KtFileWithMeta
import io.joern.kotlin2cpg.ast.Nodes._
import io.joern.kotlin2cpg.ast.Nodes.{methodReturnNode => _methodReturnNode}
import io.joern.kotlin2cpg.types.{CallKinds, TypeConstants, TypeInfoProvider}
import io.joern.kotlin2cpg.psi.Extractor._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.joern.x2cpg.datastructures.Global

import java.util.UUID.randomUUID
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.lexer.KtTokens
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.jdk.CollectionConverters._
import scala.annotation.tailrec

object Constants {
  val empty                          = "<empty>"
  val lambdaName                     = "<lambda>"
  val init                           = "<init>"
  val root                           = "<root>"
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
  val lambdaBindingName              = "invoke"    // the underlying _invoke_ fn for Kotlin FunctionX types
  val lambdaTypeDeclName             = "LAMBDA_TYPE_DECL"
  val this_                          = "this"
  val componentNPrefix               = "component"
  val tmpLocalPrefix                 = "tmp_"
  val ret                            = "RET"
  val javaUtilIterator               = "java.util.Iterator"
  val collectionsIteratorName        = "kotlin.collections.Iterator"
  val getIteratorMethodName          = "iterator"
  val iteratorPrefix                 = "iterator_"
  val hasNextIteratorMethodName      = "hasNext"
  val nextIteratorMethodName         = "next"
  val codeForLoweredForBlock         = "FOR-BLOCK" // TODO: improve this
  val underscore                     = "_"
  val alloc                          = "alloc"
  val when                           = "when"
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
case class ClosureBindingDef(node: NewClosureBinding, captureEdgeTo: NewMethodRef, refEdgeTo: NewNode)

// TODO: add description
case class Additionals(
  bindingsInfo: Seq[BindingInfo] = List(),
  lambdaAsts: Seq[Ast] = List(),
  lambdaBindingInfo: Seq[BindingInfo] = List(),
  closureBindingDefs: Seq[ClosureBindingDef] = List()
)

// TODO: add description
case class AstWithAdditionals(ast: Ast, additionals: Additionals)

// TODO: add description
case class FileInfo(imports: Seq[ImportEntry], classes: List[KtClass])

// TODO: add description
class AstCreator(fileWithMeta: KtFileWithMeta, xTypeInfoProvider: TypeInfoProvider, global: Global)
    extends AstCreatorBase(fileWithMeta.filename) {

  private val lambdaKeyPool   = new IntervalKeyPool(first = 1, last = Long.MaxValue)
  private val tmpKeyPool      = new IntervalKeyPool(first = 1, last = Long.MaxValue)
  private val iteratorKeyPool = new IntervalKeyPool(first = 1, last = Long.MaxValue)

  private val relativizedPath = fileWithMeta.relativizedPath

  protected val scope: Scope[String, DeclarationNew, NewNode] = new Scope()

  def createAst(): DiffGraphBuilder = {
    implicit val typeInfoProvider: TypeInfoProvider = xTypeInfoProvider
    logger.debug("Started parsing of file `" + fileWithMeta.filename + "`")

    val defaultTypes = Set(TypeConstants.javaLangObject)
    defaultTypes.foreach { t =>
      registerType(t)
    }
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

  private def storeInDiffGraph(astWithCtx: AstWithAdditionals): Unit = {
    val ast = astWithCtx.ast

    Ast.storeInDiffGraph(ast, diffGraph)

    astWithCtx.additionals.bindingsInfo.foreach { bindingInfo =>
      diffGraph.addNode(bindingInfo.node)

      bindingInfo.edgeMeta.foreach { edgeMeta =>
        diffGraph.addEdge(edgeMeta._1, edgeMeta._2, edgeMeta._3)
      }
    }

    astWithCtx.additionals.lambdaBindingInfo.foreach { bindingInfo =>
      diffGraph.addNode(bindingInfo.node)

      bindingInfo.edgeMeta.foreach { edgeMeta =>
        diffGraph.addEdge(edgeMeta._1, edgeMeta._2, edgeMeta._3)
      }
    }

    astWithCtx.additionals.closureBindingDefs.foreach { cbd =>
      diffGraph.addNode(cbd.node)
      diffGraph.addEdge(cbd.captureEdgeTo, cbd.node, EdgeTypes.CAPTURE)
      diffGraph.addEdge(cbd.node, cbd.refEdgeTo, EdgeTypes.REF)
    }
  }

  private def mergedAdditionals(adds: Seq[Additionals]): Additionals = {
    adds.foldLeft(Additionals())((acc, adds) => {
      val bindingsInfo       = acc.bindingsInfo ++ adds.bindingsInfo
      val lambdaAsts         = acc.lambdaAsts ++ adds.lambdaAsts
      val lambdaBindingInfo  = acc.lambdaBindingInfo ++ adds.lambdaBindingInfo
      val closureBindingDefs = acc.closureBindingDefs ++ adds.closureBindingDefs
      Additionals(bindingsInfo, lambdaAsts, lambdaBindingInfo, closureBindingDefs)
    })
  }

  private def astForFile(
    fileWithMeta: KtFileWithMeta
  )(implicit typeInfoProvider: TypeInfoProvider): AstWithAdditionals = {
    val ktFile = fileWithMeta.f
    val classDecls =
      ktFile.getDeclarations.asScala.collect { case c: KtClass => c }.toList
    val allImports                  = combinedImports(ktFile.getImportList.getImports.asScala.toList)
    implicit val fileInfo: FileInfo = FileInfo(allImports, classDecls)

    val importAsts =
      withIndex(allImports) { (entry, order) =>
        astForImportEntry(entry, order)
      }
    val namespaceBlocksForImports =
      allImports.asJava.asScala.collect {
        case e if !e.isWildcard =>
          val node = namespaceBlockNode(e.fqName, e.fqName, relativizedPath)
          Ast(node)
      }.toSeq

    val lastImportOrder = importAsts.size
    var idxEpsilon      = 0 // when multiple AST nodes are returned by `astForDeclaration`
    val declarationsAstsWithCtx =
      withIndex(ktFile.getDeclarations.asScala.toSeq) { (decl, order) =>
        val asts = astForDeclaration(decl, order + lastImportOrder + idxEpsilon)
        idxEpsilon += asts.size - 1
        asts
      }.flatten

    val fileNode =
      NewFile()
        .name(fileWithMeta.relativizedPath)
        .order(0)
    val finalCtx                 = mergedAdditionals(declarationsAstsWithCtx.map(_.additionals))
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
            .withChildren(mergedAdditionals(declarationsAstsWithCtx.map(_.additionals)).lambdaAsts)
            .withChildren(lambdaTypeDecls)
        )
        .withChildren(namespaceBlocksForImports)
    AstWithAdditionals(ast, finalCtx)
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

  def astForImportEntry(entry: ImportEntry, order: Int): AstWithAdditionals = {
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
    AstWithAdditionals(Ast(node), Additionals())
  }

  def astForPackageDeclaration(packageName: String): AstWithAdditionals = {
    val node =
      packageName match {
        case Constants.root =>
          namespaceBlockNode(
            NamespaceTraversal.globalNamespaceName,
            NamespaceTraversal.globalNamespaceName,
            relativizedPath
          )
            .order(1)
        case _ =>
          val name = packageName.split("\\.").lastOption.getOrElse("")
          namespaceBlockNode(name, packageName, relativizedPath)
            .order(1)
      }
    AstWithAdditionals(Ast(node), Additionals())
  }

  def astForDeclaration(decl: KtDeclaration, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithAdditionals] = {
    decl match {
      case d: KtClass             => Seq(astForClassOrObject(d, order))
      case d: KtObjectDeclaration => Seq(astForClassOrObject(d, order))
      case d: KtNamedFunction     => Seq(astForMethod(d, order))
      case d: KtTypeAlias         => Seq(astForTypeAlias(d, order))
      case d: KtProperty          => astForTopLevelProperty(d, order)
      case unhandled =>
        logger.error(
          "Unknown declaration type encountered with text `" + unhandled.getText + "` and class `" + unhandled.getClass + "`!"
        )
        Seq()
    }
  }

  // TODO: lower them
  def astForTopLevelProperty(prop: KtProperty, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithAdditionals] = {
    Seq()
  }

  def astForTypeAlias(typeAlias: KtTypeAlias, order: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val fullName          = typeInfoProvider.fullName(typeAlias, TypeConstants.any)
    val aliasTypeFullName = typeInfoProvider.aliasTypeFullName(typeAlias, TypeConstants.any)
    registerType(fullName)
    registerType(aliasTypeFullName)

    // TODO: check whether this should be a TYPE_REF
    val node =
      typeDeclNode(
        typeAlias.getName,
        fullName,
        relativizedPath,
        Seq(),
        Some(aliasTypeFullName),
        line(typeAlias),
        column(typeAlias)
      )
        .order(order)
    AstWithAdditionals(Ast(node), Additionals())
  }

  def astForClassOrObject(ktClass: KtClassOrObject, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {

    val className = ktClass.getName
    val explicitFullName = {
      val fqName = ktClass.getContainingKtFile.getPackageFqName.toString
      fqName + "." + className
    }
    val classFullName = typeInfoProvider.fullName(ktClass, explicitFullName)
    registerType(classFullName)

    val explicitBaseTypeFullNames =
      ktClass.getSuperTypeListEntries.asScala
        .map(_.getTypeAsUserType)
        .filterNot(_ == null) // TODO: write test and pick up code from git@github.com:RedApparat/Fotoapparat.git
        .map(_.getText)
        .toList

    val baseTypeFullNames =
      typeInfoProvider.inheritanceTypes(ktClass, explicitBaseTypeFullNames)
    val outBaseTypeFullNames =
      if (baseTypeFullNames.isEmpty) {
        Seq(TypeConstants.javaLangObject)
      } else {
        baseTypeFullNames
      }
    val typeDecl =
      typeDeclNode(
        className,
        classFullName,
        relativizedPath,
        outBaseTypeFullNames,
        None,
        line(ktClass),
        column(ktClass)
      )
        .order(order)
    scope.pushNewScope(typeDecl)

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
    val methodAstsWithCtx =
      withIndex(classFunctions.asScala.toSeq) { (method, order) =>
        astForMethod(method, order)
      }

    val bindingsInfo =
      methodAstsWithCtx.map(_.ast).map { ast =>
        // TODO: add a try catch here
        val method = ast.root.get.asInstanceOf[NewMethod]
        val node   = bindingNode(method.name, method.signature)
        BindingInfo(node, List((typeDecl, node, EdgeTypes.BINDS), (node, method, EdgeTypes.REF)))
      }
    val constructorParams = ktClass.getPrimaryConstructorParameters.asScala.toList
    val defaultSignature =
      if (ktClass.getPrimaryConstructor == null) {
        TypeConstants.void + "()"
      } else {
        typeInfoProvider.erasedSignature(constructorParams)
      }
    val defaultFullName = classFullName + "." + TypeConstants.initPrefix + ":" + defaultSignature
    val ctorFnWithSig =
      typeInfoProvider.fullNameWithSignature(ktClass.getPrimaryConstructor, (defaultFullName, defaultSignature))
    val primaryCtorOrder = 1
    val constructorMethod =
      methodNode(
        TypeConstants.initPrefix,
        ctorFnWithSig._1,
        ctorFnWithSig._2,
        relativizedPath,
        line(ktClass.getPrimaryConstructor),
        column(ktClass.getPrimaryConstructor)
      ).order(primaryCtorOrder)

    val ctorThisParam =
      methodParameterNode(Constants.this_, classFullName)
        .order(0)
    scope.addToScope(Constants.this_, ctorThisParam)

    val constructorParamsWithCtx = {
      Seq(AstWithAdditionals(Ast(ctorThisParam), Additionals())) ++
        withIndex(constructorParams) { (p, order) =>
          astForParameter(p, order)
        }
    }
    val orderAfterParams = constructorParamsWithCtx.size + 1
    val ctorMethodBlock =
      blockNode("", TypeConstants.void)
        .order(orderAfterParams)

    val memberSetCalls =
      constructorParams
        .filter(_.hasValOrVar)
        .zipWithIndex
        .map { case (ctorParam, idx) =>
          val typeFullName = typeInfoProvider.typeFullName(ctorParam, TypeConstants.any)
          registerType(typeFullName)

          val paramName = ctorParam.getName
          val paramIdentifier =
            identifierNode(paramName, typeFullName)
              .argumentIndex(2)
              .order(2)

          val matchingMethodParamNode =
            constructorParamsWithCtx.flatMap { pWithCtx =>
              val node = pWithCtx.ast.root.get.asInstanceOf[NewMethodParameterIn]
              if (node.name == paramName) {
                Some(node)
              } else {
                None
              }
            }.head
          val paramIdentifierAst =
            Ast(paramIdentifier)
              .withRefEdge(paramIdentifier, matchingMethodParamNode)

          val this_ =
            identifierNode(Constants.this_, classFullName)
              .argumentIndex(1)
              .order(1)

          val fieldIdentifier =
            fieldIdentifierNode(paramName)
              .argumentIndex(2)
              .order(2)

          val fieldAccessCall =
            operatorCallNode(Operators.fieldAccess, Constants.this_ + "." + paramName, Some(typeFullName))
              .order(1)
              .argumentIndex(1)

          val fieldAccessCallAst =
            Ast(fieldAccessCall)
              .withChild(Ast(this_))
              .withArgEdge(fieldAccessCall, this_)
              .withChild(Ast(fieldIdentifier))
              .withArgEdge(fieldAccessCall, fieldIdentifier)

          val assignmentNode =
            operatorCallNode(Operators.assignment, fieldAccessCall.code + " = " + paramIdentifier.code)
              .order(idx + 1)

          val assignmentAst =
            Ast(assignmentNode)
              .withChild(fieldAccessCallAst)
              .withArgEdge(assignmentNode, fieldAccessCall)
              .withChild(paramIdentifierAst)
              .withArgEdge(assignmentNode, paramIdentifier)

          assignmentAst
        }

    val ctorMethodAst =
      Ast(ctorMethodBlock)
        .withChildren(memberSetCalls)

    val orderAfterParamsAndBlock = orderAfterParams + 1
    val typeFullName             = typeInfoProvider.typeFullName(ktClass.getPrimaryConstructor, TypeConstants.any)
    val constructorMethodReturn =
      _methodReturnNode(
        typeFullName,
        Some(classFullName),
        line(ktClass.getPrimaryConstructor),
        column(ktClass.getPrimaryConstructor)
      )
        .order(orderAfterParamsAndBlock)
    val constructorAst =
      Ast(constructorMethod)
        .withChildren(constructorParamsWithCtx.map(_.ast))
        .withChild(ctorMethodAst)
        .withChild(Ast(constructorMethodReturn))

    val membersFromPrimaryCtorAsts =
      ktClass.getPrimaryConstructorParameters.asScala.toList
        .filter(_.hasValOrVar)
        .zipWithIndex
        .collect { case (param, idx) =>
          val typeFullName = typeInfoProvider.parameterType(param, TypeConstants.any)
          val node =
            memberNode(param.getName, typeFullName, line(param), column(param))
              .order(idx + primaryCtorOrder)
          Ast(node)
        }

    val orderAfterPrimaryCtorAndItsMemberDefs = membersFromPrimaryCtorAsts.size + primaryCtorOrder
    val secondaryConstructorAsts =
      withIndex(ktClass.getSecondaryConstructors.asScala.toSeq) { (secondaryCtor, order) =>
        val constructorParams = secondaryCtor.getValueParameters.asScala.toList
        val defaultSignature  = typeInfoProvider.erasedSignature(constructorParams)
        val defaultFullName   = classFullName + "." + TypeConstants.initPrefix + ":" + defaultSignature
        val ctorFnWithSig = typeInfoProvider.fullNameWithSignature(secondaryCtor, (defaultFullName, defaultSignature))
        val constructorMethod =
          methodNode(
            Constants.init,
            ctorFnWithSig._1,
            ctorFnWithSig._2,
            relativizedPath,
            line(secondaryCtor),
            column(secondaryCtor)
          )
            .order(orderAfterPrimaryCtorAndItsMemberDefs + order)
        scope.pushNewScope(constructorMethod)

        val typeFullName = typeInfoProvider.typeFullName(secondaryCtor, TypeConstants.any)
        registerType(typeFullName)

        val ctorThisParam =
          methodParameterNode(Constants.this_, classFullName)
            .order(0)
        scope.addToScope(Constants.this_, ctorThisParam)
        val constructorParamsWithCtx =
          Seq(AstWithAdditionals(Ast(ctorThisParam), Additionals())) ++
            withIndex(constructorParams) { (p, order) =>
              astForParameter(p, order)
            }

        val orderAfterCtorParams = constructorParamsWithCtx.size + 1

        val ctorMethodBlockAst =
          if (secondaryCtor.getBodyExpression != null) {
            Seq(astForBlock(secondaryCtor.getBodyExpression, orderAfterCtorParams))
          } else {
            Seq()
          }
        val constructorMethodReturn =
          _methodReturnNode(typeFullName, Some(classFullName), line(secondaryCtor), column(secondaryCtor))
            .order(orderAfterCtorParams + ctorMethodBlockAst.size + 1)
        val constructorAst =
          Ast(constructorMethod)
            .withChildren(constructorParamsWithCtx.map(_.ast))
            .withChildren(ctorMethodBlockAst.map(_.ast))
            .withChild(Ast(constructorMethodReturn))
        scope.popScope()
        constructorAst
      }

    val orderAfterCtors = orderAfterPrimaryCtorAndItsMemberDefs + secondaryConstructorAsts.size

    val isDataClass =
      ktClass match {
        case typedExpr: KtClass =>
          typedExpr.isData
        case _ => false
      }

    val componentNMethodAsts =
      if (isDataClass) {
        ktClass.getPrimaryConstructor.getValueParameters.asScala.zipWithIndex.map { valueParamWithIdx =>
          val valueParam   = valueParamWithIdx._1
          val order        = valueParamWithIdx._2
          val componentIdx = valueParamWithIdx._2 + 1

          val typeFullName = typeInfoProvider.typeFullName(valueParam, TypeConstants.any)
          registerType(typeFullName)

          val componentName = Constants.componentNPrefix + componentIdx
          val signature     = typeFullName + "()"
          val fullName      = typeDecl.fullName + "." + componentName + ":" + signature

          val thisParam =
            methodParameterNode(Constants.this_, classFullName)
              .order(0)
          val _methodNode =
            methodNode(componentName, fullName, signature, relativizedPath)
              .order(order)
          val thisIdentifier =
            identifierNode(Constants.this_, typeDecl.fullName)
              .argumentIndex(1)
              .order(1)
          val fieldIdentifier =
            fieldIdentifierNode(valueParam.getName, line(valueParam), column(valueParam))
              .argumentIndex(2)
              .order(2)

          val fieldAccessCall =
            operatorCallNode(Operators.fieldAccess, Constants.this_ + "." + valueParam.getName, Some(typeFullName))
              .order(1)
              .argumentIndex(1)
          val fieldAccessCallAst =
            Ast(fieldAccessCall)
              .withChild(Ast(thisIdentifier))
              .withArgEdge(fieldAccessCall, thisIdentifier)
              .withChild(Ast(fieldIdentifier))
              .withArgEdge(fieldAccessCall, fieldIdentifier)

          val _returnNode = returnNode(Constants.ret).order(1)
          val returnAst =
            Ast(_returnNode)
              .withChild(fieldAccessCallAst)
              .withArgEdge(_returnNode, fieldAccessCall)

          val methodBlock =
            blockNode(fieldAccessCall.code, typeFullName)
              .order(1)
          val methodBlockAst =
            Ast(methodBlock)
              .withChild(returnAst)

          val methodReturn = _methodReturnNode(typeFullName, None).order(2)
          Ast(_methodNode)
            .withChild(Ast(thisParam))
            .withChild(methodBlockAst)
            .withChild(Ast(methodReturn))
        }
      } else {
        Seq()
      }

    val componentNBindingsInfo =
      componentNMethodAsts.map { methodAst =>
        val method = methodAst.root.get.asInstanceOf[NewMethod]
        val node   = bindingNode(method.name, method.signature)
        BindingInfo(node, List((typeDecl, node, EdgeTypes.BINDS), (node, method, EdgeTypes.REF)))
      }

    val orderAfterComponentN = orderAfterCtors + componentNMethodAsts.size

    val ast =
      Ast(typeDecl)
        .withChildren(methodAstsWithCtx.map(_.ast))
        .withChild(constructorAst)
        .withChildren(membersFromPrimaryCtorAsts)
        .withChildren(secondaryConstructorAsts)
        .withChildren(componentNMethodAsts.toList)
        .withChildren(withIndex(classDeclarations.asScala.toSeq) { (method, order) =>
          astForMember(method, orderAfterComponentN + order)
        })

    val finalCtx = mergedAdditionals(
      methodAstsWithCtx.map(_.additionals) ++ List(Additionals(bindingsInfo = bindingsInfo ++ componentNBindingsInfo))
    )
    scope.popScope()
    AstWithAdditionals(ast, finalCtx)
  }

  private def astForMethod(ktFn: KtNamedFunction, childNum: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val fnWithSig = typeInfoProvider.fullNameWithSignature(ktFn, ("", ""))
    val _methodNode =
      methodNode(ktFn.getName, fnWithSig._1, fnWithSig._2, relativizedPath, line(ktFn), column(ktFn))
        .order(childNum)
    scope.pushNewScope(_methodNode)

    val parametersWithCtx =
      withIndex(ktFn.getValueParameters.asScala.toSeq) { (p, order) =>
        astForParameter(p, order)
      }

    val lastOrder = parametersWithCtx.size + 2
    val bodyAstWithCtx =
      ktFn.getBodyBlockExpression match {
        case blockExpr if blockExpr != null => astForBlock(blockExpr, lastOrder)
        case _ =>
          val blockNode = NewBlock()
          AstWithAdditionals(Ast(blockNode), Additionals())
      }
    val returnAst = astForMethodReturn(ktFn, lastOrder + 1)
    val ast =
      Ast(_methodNode)
        .withChildren(parametersWithCtx.map(_.ast))
        .withChild(bodyAstWithCtx.ast)
        .withChild(returnAst)
    val finalCtx = mergedAdditionals(Seq(bodyAstWithCtx.additionals))

    scope.popScope()
    AstWithAdditionals(ast, finalCtx)
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
      _methodReturnNode(typeFullName, None, line(ktFn), column(ktFn))
        .order(order)
    Ast(node)
  }

  private def astForBlock(
    expr: KtBlockExpression,
    order: Int,
    pushToScope: Boolean = false,
    localsForCaptures: List[NewLocal] = List()
  )(implicit fileInfo: FileInfo, typeInfoProvider: TypeInfoProvider): AstWithAdditionals = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    val block =
      blockNode(expr.getStatements.asScala.map(_.getText).mkString("\n"), typeFullName, line(expr), column(expr))
        .order(order)
        .argumentIndex(order)
    if (pushToScope) {
      scope.pushNewScope(block)
    }

    val localAstsForCaptures =
      withIndex(localsForCaptures) { (local, order) =>
        Ast(local.order(order))
      }
    val orderAfterAdditionalLocals = localsForCaptures.size

    var orderRemainder = orderAfterAdditionalLocals
    val expressions = {
      withIndex(expr.getStatements.asScala.toSeq) { (statement, order) =>
        val asts = astsForExpression(statement, order + orderRemainder, order + orderRemainder)
        orderRemainder = if (asts.size > 1) {
          orderRemainder + asts.size - 1
        } else {
          orderRemainder
        }
        asts
      }.flatten
    }
    if (pushToScope) {
      scope.popScope()
    }

    val childrenCtx = mergedAdditionals(expressions.map(_.additionals))
    val ast =
      Ast(block)
        .withChildren(localAstsForCaptures)
        .withChildren(expressions.map(_.ast))
    AstWithAdditionals(ast, childrenCtx)
  }

  private def astsForReturnExpression(expr: KtReturnExpression, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithAdditionals] = {
    val child = astsForExpression(expr.getReturnedExpression, 1, 1).headOption
      .getOrElse(AstWithAdditionals(Ast(), Additionals()))
    val node =
      returnNode(expr.getText, line(expr), column(expr))
        .order(order)
    val ast =
      Ast(node)
        .withChild(child.ast)
        .withArgEdges(node, child.ast.root.toList)
    Seq(AstWithAdditionals(ast, child.additionals))
  }

  def astForIsExpression(expr: KtIsExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val retType = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(retType)

    val callNode =
      operatorCallNode(Operators.is, expr.getText, None, line(expr), column(expr))
        .argumentIndex(argIdx)
        .order(order)
    val args =
      astsForExpression(expr.getLeftHandSide, 1, 1) ++ Seq(astForTypeReference(expr.getTypeReference, 2, 2))
    val ast = callAst(callNode, args.map(_.ast).toList)
    AstWithAdditionals(ast, mergedAdditionals(args.map(_.additionals)))
  }

  def astForBinaryExprWithTypeRHS(expr: KtBinaryExpressionWithTypeRHS, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val args =
      astsForExpression(expr.getLeft, 1, 1) ++ Seq(astForTypeReference(expr.getRight, 2, 2))
    val callNode =
      operatorCallNode(Operators.cast, expr.getText, None, line(expr), column(expr))
        .argumentIndex(argIdx)
        .order(order)
    val ast = callAst(callNode, args.map(_.ast).toList)
    AstWithAdditionals(ast, mergedAdditionals(args.map(_.additionals)))
  }

  private def astForTypeReference(expr: KtTypeReference, order: Int, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider,
    fileInfo: FileInfo
  ): AstWithAdditionals = {
    val typeFullName = typeInfoProvider.typeFullName(expr, TypeConstants.any)
    registerType(typeFullName)

    val node =
      typeRefNode(expr.getText, typeFullName, line(expr), column(expr))
        .order(order)
        .argumentIndex(argIdx)
    AstWithAdditionals(Ast(node), Additionals())
  }

  @tailrec
  private def astsForExpression(expr: KtExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithAdditionals] = {
    expr match {
      case typedExpr: KtAnnotatedExpression         => astsForExpression(typedExpr.getBaseExpression, order, argIdx)
      case typedExpr: KtArrayAccessExpression       => Seq(astForArrayAccess(typedExpr, order, argIdx))
      case typedExpr: KtBinaryExpression            => Seq(astForBinaryExpr(typedExpr, order, argIdx))
      case typedExpr: KtBlockExpression             => List(astForBlock(typedExpr, order))
      case typedExpr: KtBinaryExpressionWithTypeRHS => Seq(astForBinaryExprWithTypeRHS(typedExpr, order, argIdx))
      case typedExpr: KtBreakExpression             => Seq(astForBreak(typedExpr, order))
      case typedExpr: KtCallExpression =>
        val isCtorCall = typeInfoProvider.isConstructorCall(typedExpr)
        if (isCtorCall.getOrElse(false)) {
          Seq(astForCtorCall(typedExpr, order, argIdx))
        } else {
          Seq(astForCall(typedExpr, order, argIdx))
        }
      case typedExpr: KtConstantExpression       => Seq(astForLiteral(typedExpr, order, argIdx))
      case typedExpr: KtClassLiteralExpression   => Seq(astForClassLiteral(typedExpr, order, argIdx))
      case typedExpr: KtSafeQualifiedExpression  => Seq(astForQualifiedExpression(typedExpr, order, argIdx))
      case typedExpr: KtContinueExpression       => Seq(astForContinue(typedExpr, order))
      case typedExpr: KtDestructuringDeclaration => astsForDestructuringDeclaration(typedExpr, order)
      case typedExpr: KtDotQualifiedExpression   => Seq(astForQualifiedExpression(typedExpr, order, argIdx))
      case typedExpr: KtDoWhileExpression        => Seq(astForDoWhile(typedExpr, order))
      case typedExpr: KtForExpression            => Seq(astForFor(typedExpr, order))
      case typedExpr: KtIfExpression             => Seq(astForIf(typedExpr, order, argIdx))
      case typedExpr: KtIsExpression             => Seq(astForIsExpression(typedExpr, order, argIdx))
      case typedExpr: KtLabeledExpression        => astsForExpression(typedExpr.getBaseExpression, order, argIdx)
      case typedExpr: KtLambdaExpression         => Seq(astForLambda(typedExpr, order, argIdx))
      case typedExpr: KtNameReferenceExpression if typedExpr.getReferencedNameElementType == KtTokens.IDENTIFIER =>
        Seq(astForNameReference(typedExpr, order, argIdx))
      // TODO: callable reference
      case _: KtNameReferenceExpression =>
        // TODO: handle this
        Seq()
      case typedExpr: KtObjectLiteralExpression => Seq(astForUnknown(typedExpr, order, argIdx)) // TODO: handle properly
      case typedExpr: KtParenthesizedExpression => astsForExpression(typedExpr.getExpression, order, argIdx)
      case typedExpr: KtPostfixExpression       => Seq(astForPostfixExpression(typedExpr, order, argIdx))
      case typedExpr: KtPrefixExpression        => Seq(astForPrefixExpression(typedExpr, order, argIdx))
      case typedExpr: KtProperty if typedExpr.isLocal => astsForProperty(typedExpr, order)
      case typedExpr: KtReturnExpression              => astsForReturnExpression(typedExpr, order)
      case typedExpr: KtStringTemplateExpression      => Seq(astForStringTemplate(typedExpr, order, argIdx))
      case typedExpr: KtSuperExpression               => Seq(astForSuperExpression(typedExpr, order, argIdx))
      case typedExpr: KtThisExpression                => Seq(astForThisExpression(typedExpr, order, argIdx))
      case typedExpr: KtThrowExpression               => Seq(astForUnknown(typedExpr, order, argIdx))
      case typedExpr: KtTryExpression                 => Seq(astForTry(typedExpr, order, argIdx))
      case typedExpr: KtWhenExpression                => Seq(astForWhen(typedExpr, order, argIdx))
      case typedExpr: KtWhileExpression               => Seq(astForWhile(typedExpr, order))
      case typedExpr: KtNamedFunction =>
        logger.debug(
          "Creating empty AST node for unknown expression `${typedExpr.getClass}` with text `${typedExpr.getText}`"
        )
        Seq(astForUnknown(typedExpr, order, argIdx))
      case null =>
        logger.debug("Received null expression! Skipping...")
        Seq()
      // TODO: handle `KtCallableReferenceExpression` like `this::baseTerrain`
      case unknownExpr =>
        logger.debug(
          "Creating empty AST node for unknown expression `" + unknownExpr.getClass + "` with text `" + unknownExpr.getText + "`"
        )
        Seq(astForUnknown(unknownExpr, order, argIdx))
    }
  }

  def astForSuperExpression(expr: KtSuperExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val node =
      identifierNode(expr.getText, typeFullName, line(expr), column(expr))
        .order(order)
        .argumentIndex(argIdx)
    val ast =
      scope.lookupVariable(expr.getText) match {
        case Some(n) => Ast(node).withRefEdge(node, n)
        case None    => Ast(node)
      }
    AstWithAdditionals(ast, Additionals())
  }

  def astForThisExpression(expr: KtThisExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val node =
      identifierNode(expr.getText, typeFullName, line(expr), column(expr))
        .order(order)
        .argumentIndex(argIdx)
    val ast =
      scope.lookupVariable(expr.getText) match {
        case Some(n) => Ast(node).withRefEdge(node, n)
        case None    => Ast(node)
      }
    AstWithAdditionals(ast, Additionals())
  }

  def astForClassLiteral(expr: KtClassLiteralExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val fullNameWithSignature = typeInfoProvider.fullNameWithSignature(expr, ("", "")) // TODO: fix the fallback names
    val typeFullName          = typeInfoProvider.expressionType(expr, TypeConstants.javaLangObject)
    registerType(typeFullName)

    val _callNode =
      callNode(
        expr.getText,
        TypeConstants.classLiteralReplacementMethodName,
        fullNameWithSignature._1,
        fullNameWithSignature._2,
        typeFullName,
        DispatchTypes.STATIC_DISPATCH,
        line(expr),
        column(expr)
      ).argumentIndex(argIdx)
        .order(order)
    AstWithAdditionals(Ast(_callNode), Additionals())
  }

  def astForLambda(expr: KtLambdaExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {

    val fullNameWithSig    = typeInfoProvider.fullNameWithSignature(expr, lambdaKeyPool)
    val lambdaModifierNode = modifierNode(ModifierTypes.VIRTUAL)
    val lambdaNode =
      methodNode(
        Constants.lambdaName,
        fullNameWithSig._1,
        fullNameWithSig._2,
        relativizedPath,
        line(expr),
        column(expr)
      ).order(1)

    val captured = scope.pushClosureScope(lambdaNode)
    val closureBindingEntriesForCaptured =
      captured
        .collect {
          case mpi: NewMethodParameterIn => mpi
          case l: NewLocal               => l
          case mem: NewMember            => mem
        }
        .map { capturedNode =>
          val closureBindingId = randomUUID().toString
          val node             = closureBinding(closureBindingId, capturedNode.name)
          (node, capturedNode)
        }

    val localsForCaptured =
      closureBindingEntriesForCaptured.map { entry =>
        val node =
          localNode(entry._2.name, entry._2.typeFullName, entry._1.closureBindingId)
        scope.addToScope(entry._2.name, node)
        node
      }

    val parametersWithCtx =
      typeInfoProvider.implicitParameterName(expr) match {
        case Some(implicitParamName) =>
          val node = methodParameterNode(implicitParamName, TypeConstants.any)
          scope.addToScope(implicitParamName, node)
          Seq(AstWithAdditionals(Ast(node), Additionals()))
        case None =>
          withIndex(expr.getValueParameters.asScala.toSeq) { (p, order) =>
            astForParameter(p, order)
          }
      }
    val lastOrder = parametersWithCtx.size + 2

    val bodyAstWithCtx =
      expr.getBodyExpression match {
        case blockExpr if blockExpr != null => astForBlock(blockExpr, lastOrder, false, localsForCaptured)
        case _ =>
          val blockNode = NewBlock()
          AstWithAdditionals(Ast(blockNode), Additionals())
      }

    val returnTypeFullName = typeInfoProvider.returnTypeFullName(expr)
    registerType(returnTypeFullName)

    val lambdaTypeDeclFullName = fullNameWithSig._1.split(":").head
    val methodRef =
      methodRefNode(expr.getName, fullNameWithSig._1, lambdaTypeDeclFullName, line(expr), column(expr))
        .order(order)
        .argumentIndex(argIdx)
    val returnNode =
      _methodReturnNode(returnTypeFullName, None, line(expr), column(expr))
        .order(lastOrder + 1)

    val lambdaMethodAst =
      Ast(lambdaNode)
        .withChildren(parametersWithCtx.map(_.ast))
        .withChild(bodyAstWithCtx.ast)
        .withChild(Ast(returnNode))
        .withChild(Ast(lambdaModifierNode))

    val methodRefAst =
      Ast(methodRef)

    val lambdaTypeDeclInheritsFromTypeFullName =
      TypeConstants.kotlinFunctionXPrefix + expr.getValueParameters.size
    val lambdaTypeDecl =
      typeDeclNode(
        Constants.lambdaTypeDeclName,
        lambdaTypeDeclFullName,
        relativizedPath,
        Seq(lambdaTypeDeclInheritsFromTypeFullName)
      )
    val lambdaBinding = bindingNode(Constants.lambdaBindingName, fullNameWithSig._2)
    val bindingInfo = BindingInfo(
      lambdaBinding,
      Seq((lambdaTypeDecl, lambdaBinding, EdgeTypes.BINDS), (lambdaBinding, lambdaNode, EdgeTypes.REF))
    )
    scope.popScope()

    val closureBindingDefs =
      closureBindingEntriesForCaptured.map { entry =>
        ClosureBindingDef(entry._1, methodRef, entry._2)
      }

    val localizedAdditionals =
      Additionals(
        lambdaAsts = Seq(lambdaMethodAst),
        lambdaBindingInfo = Seq(bindingInfo),
        closureBindingDefs = closureBindingDefs
      )
    AstWithAdditionals(methodRefAst, mergedAdditionals(Seq(localizedAdditionals) ++ Seq(bodyAstWithCtx.additionals)))
  }

  def astForArrayAccess(expr: KtArrayAccessExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val identifierElem = expr.getArrayExpression
    val typeFullName   = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val identifier =
      identifierNode(identifierElem.getText, typeFullName, line(identifierElem), column(identifierElem))
        .order(1)
        .argumentIndex(1)
    val identifierAst =
      scope.lookupVariable(identifierElem.getText) match {
        case Some(v) => Ast(identifier).withRefEdge(identifier, v)
        case None    => Ast(identifier)
      }

    val indexExpr =
      if (expr.getIndexExpressions.size >= 1) {
        Some(expr.getIndexExpressions.get(0))
      } else {
        None
      }
    val astsForIndexExpr = indexExpr match {
      case Some(ie) =>
        astsForExpression(ie, 2, 2)
      case None =>
        List()
    }

    val callNode =
      operatorCallNode(Operators.indexAccess, expr.getText, Some(typeFullName), line(expr), column(expr))
        .order(order)
        .argumentIndex(argIdx)
    val call = callAst(callNode, List(identifierAst))
    val finalAst =
      call
        .withChildren(astsForIndexExpr.map(_.ast))
        .withArgEdges(callNode, astsForIndexExpr.map(_.ast.root.get))
    AstWithAdditionals(finalAst, Additionals())
  }

  def astForPostfixExpression(expr: KtPostfixExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
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

    val args = List(
      astsForExpression(expr.getBaseExpression, 1, 1).headOption
        .getOrElse(AstWithAdditionals(Ast(), Additionals()))
    ).filterNot(_.ast.root == null)
    val callNode =
      operatorCallNode(operatorType, expr.getText, Some(typeFullName), line(expr), column(expr))
        .argumentIndex(argIdx)
        .order(order)
    val ast = callAst(callNode, args.map(_.ast))
    AstWithAdditionals(ast, mergedAdditionals(args.map(_.additionals)))
  }

  def astForPrefixExpression(expr: KtPrefixExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
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

    val args = List(
      astsForExpression(expr.getBaseExpression, 1, 1).headOption
        .getOrElse(AstWithAdditionals(Ast(), Additionals()))
    ).filterNot(_.ast.root == null)
    val callNode =
      operatorCallNode(operatorType, expr.getText, Some(typeFullName), line(expr), column(expr))
        .argumentIndex(argIdx)
        .order(order)
    val ast = callAst(callNode, args.map(_.ast))
    AstWithAdditionals(ast, mergedAdditionals(args.map(_.additionals)))
  }

  /*
   _______ example lowering _________
  | -> val (one, two) = makeA("AMESSAGE")
  | -> LOCAL one
  | -> LOCAL two
  | -> LOCAL tmp
  | -> tmp = makeA("AMESSAGE")
  | -> CALL one = tmp.component1()
  | -> CALL two = tmp.component2()
  |__________________________________
   */
  def astsForDestructuringDeclarationWithNonCtorCallRHS(expr: KtDestructuringDeclaration, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithAdditionals] = {
    val initExpr = expr.getInitializer
    val localsForEntries =
      nonUnderscoreEntries(expr).zipWithIndex.map { entryWithIdx =>
        val entry        = entryWithIdx._1
        val orderForNode = entryWithIdx._2 + order

        val typeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
        registerType(typeFullName)

        val node =
          localNode(entry.getName, typeFullName, None, line(entry), column(entry))
            .order(orderForNode)
        Ast(node)
      }

    val orderAfterEntryLocals = localsForEntries.size + order

    val callRhsTypeFullName = typeInfoProvider.expressionType(initExpr, TypeConstants.cpgUnresolved)
    registerType(callRhsTypeFullName)

    val tmpName          = Constants.tmpLocalPrefix + tmpKeyPool.next
    val orderForTmpLocal = orderAfterEntryLocals + 1
    val localForTmpNode =
      localNode(tmpName, callRhsTypeFullName)
        .order(orderForTmpLocal)
    val localForTmpAst =
      Ast(localForTmpNode)

    val astForRhsCall    = astsForExpression(initExpr, 2, 2).head
    val assignmentRhsAst = astForRhsCall.ast
    val assignmentRhsNode =
      assignmentRhsAst.root.get

    val assignmentLhsNode =
      identifierNode(tmpName, callRhsTypeFullName, line(expr), column(expr))
        .argumentIndex(1)
        .order(1)

    val assignmentLhsAst =
      Ast(assignmentLhsNode)
        .withRefEdge(assignmentLhsNode, localForTmpNode)

    val orderForTmpAssignmentCall = orderForTmpLocal + 1
    val assignmentNode =
      operatorCallNode(Operators.assignment, tmpName + " = " + initExpr.getText, None)
        .order(orderForTmpAssignmentCall)
    val assignmentAst =
      Ast(assignmentNode)
        .withChild(assignmentLhsAst)
        .withChild(assignmentRhsAst)
        .withArgEdges(assignmentNode, Seq(assignmentLhsNode, assignmentRhsNode))

    val returnType = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(returnType)

    val orderAfterLocalsAndTmpLowering = orderForTmpAssignmentCall + 1
    val assignmentsForEntries =
      nonUnderscoreEntries(expr).zipWithIndex.map { entryWithIdx =>
        val entry             = entryWithIdx._1
        val entryTypeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
        registerType(entryTypeFullName)

        val assignmentLHSNode =
          identifierNode(entry.getText, entryTypeFullName, line(entry), column(entry))
            .argumentIndex(1)
            .order(1)
        val relevantLocal = localsForEntries(entryWithIdx._2).root.get
        val assignmentLHSAst =
          Ast(assignmentLHSNode)
            .withRefEdge(assignmentLHSNode, relevantLocal)

        val componentNIdentifierNode =
          identifierNode(localForTmpNode.name, callRhsTypeFullName, line(entry), column(entry))
            .argumentIndex(0)
            .order(1)

        val componentIdx      = entryWithIdx._2 + 1
        val fallbackSignature = TypeConstants.cpgUnresolved + "()"
        val fallbackFullName =
          TypeConstants.cpgUnresolved + Constants.componentNPrefix + componentIdx + ":" + fallbackSignature
        val componentNFullNameWithSignature =
          typeInfoProvider.fullNameWithSignature(entry, (fallbackFullName, fallbackSignature))
        val componentNCallCode = localForTmpNode.name + "." + Constants.componentNPrefix + componentIdx + "()"
        val componentNCallNode =
          callNode(
            componentNCallCode,
            Constants.componentNPrefix + componentIdx,
            componentNFullNameWithSignature._1,
            componentNFullNameWithSignature._2,
            entryTypeFullName,
            DispatchTypes.DYNAMIC_DISPATCH,
            line(entry),
            column(entry)
          )
            .order(2)
            .argumentIndex(2)

        val componentNIdentifierAst =
          Ast(componentNIdentifierNode)
            .withRefEdge(componentNIdentifierNode, localForTmpNode)
        val componentNAst =
          Ast(componentNCallNode)
            .withChild(componentNIdentifierAst)
            .withArgEdge(componentNCallNode, componentNIdentifierNode)
            .withReceiverEdge(componentNCallNode, componentNIdentifierNode)

        val orderForNode = orderAfterLocalsAndTmpLowering + entryWithIdx._2
        val assignmentCallNode =
          operatorCallNode(
            Operators.assignment,
            entry.getText + " = " + componentNCallCode,
            None,
            line(entry),
            column(entry)
          )
            .order(orderForNode)
        val assignmentAst =
          Ast(assignmentCallNode)
            .withChild(assignmentLHSAst)
            .withArgEdge(assignmentCallNode, assignmentLHSNode)
            .withChild(componentNAst)
            .withArgEdge(assignmentCallNode, componentNCallNode)
        assignmentAst
      }

    localsForEntries.map(AstWithAdditionals(_, Additionals())) ++
      Seq(AstWithAdditionals(localForTmpAst, Additionals())) ++
      Seq(AstWithAdditionals(assignmentAst, Additionals())) ++
      assignmentsForEntries.map(AstWithAdditionals(_, Additionals()))
  }

  def nonUnderscoreEntries(expr: KtDestructuringDeclaration): Seq[KtDestructuringDeclarationEntry] = {
    expr.getEntries.asScala.filterNot(_.getText == Constants.underscore).toSeq
  }

  /*
   _______ example lowering _________
  | -> val (one, two) = Person("a", "b")
  | -> LOCAL one
  | -> LOCAL two
  | -> LOCAL tmp
  | -> tmp = alloc
  | -> tmp.<init>
  | -> CALL one = tmp.component1()
  | -> CALL two = tmp.component2()
  |__________________________________
   */
  def astsForDestructuringDeclarationWithCtorRHS(expr: KtDestructuringDeclaration, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithAdditionals] = {
    val typedInit =
      Option(expr.getInitializer)
        .collect { case e: KtCallExpression => e }
    if (typedInit.isEmpty) {
      logger.warn(s"Unhandled case for destructuring declaration: `${expr.getText}`.")
      return Seq()
    }
    val ctorCall = typedInit.get

    val localsForEntries =
      nonUnderscoreEntries(expr).zipWithIndex.map { entryWithIdx =>
        val entry        = entryWithIdx._1
        val orderForNode = entryWithIdx._2 + order

        val typeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
        registerType(typeFullName)

        val node =
          localNode(entry.getName, typeFullName, None, line(entry), column(entry))
            .order(orderForNode)
        Ast(node)
      }

    val orderAfterEntryLocals = localsForEntries.size + order

    val ctorTypeFullName = typeInfoProvider.expressionType(ctorCall, TypeConstants.cpgUnresolved)
    registerType(ctorTypeFullName)

    val tmpName          = Constants.tmpLocalPrefix + tmpKeyPool.next
    val orderForTmpLocal = orderAfterEntryLocals + 1
    val localForTmpNode =
      localNode(tmpName, ctorTypeFullName)
        .order(orderForTmpLocal)

    val localForTmpAst =
      Ast(localForTmpNode)

    val assignmentRhsNode =
      operatorCallNode(Operators.alloc, Constants.alloc, Some(ctorTypeFullName), line(expr), column(expr))
        .order(2)
        .argumentIndex(2)
    val assignmentLhsNode =
      identifierNode(tmpName, ctorTypeFullName, line(expr), column(expr))
        .argumentIndex(1)
        .order(1)

    val assignmentLhsAst =
      Ast(assignmentLhsNode)
        .withRefEdge(assignmentLhsNode, localForTmpNode)

    val orderForTmpAssignmentCall = orderForTmpLocal + 1
    val assignmentNode =
      operatorCallNode(Operators.assignment, tmpName + " = " + Constants.alloc, None)
        .order(orderForTmpAssignmentCall)
    val assignmentAst =
      Ast(assignmentNode)
        .withChild(assignmentLhsAst)
        .withChild(Ast(assignmentRhsNode))
        .withArgEdges(assignmentNode, Seq(assignmentLhsNode, assignmentRhsNode))

    val initReceiverNode =
      identifierNode(tmpName, ctorTypeFullName, line(expr), column(expr))
        .argumentIndex(0)
        .order(1)
    val initReceiverAst =
      Ast(initReceiverNode)
        .withRefEdge(initReceiverNode, localForTmpNode)

    val args = ctorCall.getValueArguments
    val argAsts =
      withIndex(args.asScala.toSeq) { case (arg, argOrder) =>
        astsForExpression(arg.getArgumentExpression, argOrder + 1, argOrder)
      }.flatten

    val fullNameWithSig = typeInfoProvider.fullNameWithSignature(ctorCall, (TypeConstants.any, TypeConstants.any))
    val returnType      = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(returnType)

    val orderForTmpInitCall = orderForTmpAssignmentCall + 1
    val initCallNode =
      callNode(
        Constants.init,
        Constants.init,
        fullNameWithSig._1,
        fullNameWithSig._2,
        TypeConstants.void,
        DispatchTypes.STATIC_DISPATCH,
        line(expr),
        column(expr)
      )
        .order(orderForTmpInitCall)
    val initCallAst =
      Ast(initCallNode)
        .withChild(initReceiverAst)
        .withChildren(argAsts.map(_.ast))
        .withArgEdges(initCallNode, Seq(initReceiverNode) ++ argAsts.flatMap(_.ast.root))

    val orderAfterLocalsAndTmpLowering = orderForTmpInitCall + 1
    val assignmentsForEntries =
      nonUnderscoreEntries(expr).zipWithIndex.map { entryWithIdx =>
        val entry             = entryWithIdx._1
        val entryTypeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
        registerType(entryTypeFullName)

        val assignmentLHSNode =
          identifierNode(entry.getText, entryTypeFullName, line(entry), column(entry))
            .argumentIndex(1)
            .order(1)
        val relevantLocal = localsForEntries(entryWithIdx._2).root.get
        val assignmentLHSAst =
          Ast(assignmentLHSNode)
            .withRefEdge(assignmentLHSNode, relevantLocal)

        val componentNIdentifierNode =
          identifierNode(localForTmpNode.name, ctorTypeFullName, line(entry), column(entry))
            .argumentIndex(0)
            .order(1)

        val componentIdx      = entryWithIdx._2 + 1
        val fallbackSignature = TypeConstants.cpgUnresolved + "()"
        val fallbackFullName =
          TypeConstants.cpgUnresolved + Constants.componentNPrefix + componentIdx + ":" + fallbackSignature
        val componentNFullNameWithSignature =
          typeInfoProvider.fullNameWithSignature(entry, (fallbackFullName, fallbackSignature))
        val componentNCallCode = localForTmpNode.name + "." + Constants.componentNPrefix + componentIdx + "()"
        val componentNCallNode =
          callNode(
            componentNCallCode,
            Constants.componentNPrefix + componentIdx,
            componentNFullNameWithSignature._1,
            componentNFullNameWithSignature._2,
            entryTypeFullName,
            DispatchTypes.DYNAMIC_DISPATCH,
            line(entry),
            column(entry)
          )
            .order(2)
            .argumentIndex(2)

        val componentNIdentifierAst =
          Ast(componentNIdentifierNode)
            .withRefEdge(componentNIdentifierNode, localForTmpNode)
        val componentNAst =
          Ast(componentNCallNode)
            .withChild(componentNIdentifierAst)
            .withArgEdge(componentNCallNode, componentNIdentifierNode)
            .withReceiverEdge(componentNCallNode, componentNIdentifierNode)

        val orderForNode = orderAfterLocalsAndTmpLowering + entryWithIdx._2
        val assignmentCallNode =
          operatorCallNode(
            Operators.assignment,
            entry.getText + " = " + componentNCallCode,
            None,
            line(entry),
            column(entry)
          ).order(orderForNode)

        val assignmentAst =
          Ast(assignmentCallNode)
            .withChild(assignmentLHSAst)
            .withArgEdge(assignmentCallNode, assignmentLHSNode)
            .withChild(componentNAst)
            .withArgEdge(assignmentCallNode, componentNCallNode)
        assignmentAst
      }

    localsForEntries.map(AstWithAdditionals(_, Additionals())) ++
      Seq(AstWithAdditionals(localForTmpAst, Additionals())) ++
      Seq(AstWithAdditionals(assignmentAst, Additionals())) ++
      Seq(AstWithAdditionals(initCallAst, Additionals())) ++
      assignmentsForEntries.map(AstWithAdditionals(_, Additionals()))
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
  def astsForDestructuringDeclarationWithVarRHS(expr: KtDestructuringDeclaration, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithAdditionals] = {
    val typedInit =
      Option(expr.getInitializer)
        .collect { case e: KtNameReferenceExpression => e }
    if (typedInit.isEmpty) {
      logger.warn(s"Unhandled case for destructuring declaration: `${expr.getText}`.")
      return Seq()
    }
    val destructuringRHS = typedInit.get
    val localsForEntries =
      nonUnderscoreEntries(expr).zipWithIndex
        .map { entryWithIdx =>
          val entry        = entryWithIdx._1
          val orderForNode = entryWithIdx._2 + order

          val typeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
          registerType(typeFullName)

          val node =
            localNode(entry.getName, typeFullName, None, line(entry), column(entry))
              .order(orderForNode)
          Ast(node)
        }

    val orderAfterLocals = localsForEntries.size + order
    val assignmentsForEntries =
      nonUnderscoreEntries(expr).zipWithIndex
        .map { entryWithIdx =>
          val entry             = entryWithIdx._1
          val entryTypeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
          registerType(entryTypeFullName)

          val assignmentLHSNode =
            identifierNode(entry.getText, entryTypeFullName, line(entry), column(entry))
              .argumentIndex(1)
              .order(1)
          val relevantLocal = localsForEntries(entryWithIdx._2).root.get
          val assignmentLHSAst =
            Ast(assignmentLHSNode)
              .withRefEdge(assignmentLHSNode, relevantLocal)

          val componentNIdentifierTFN = typeInfoProvider.typeFullName(typedInit.get, TypeConstants.any)
          registerType(componentNIdentifierTFN)

          val componentNIdentifierNode =
            identifierNode(destructuringRHS.getText, componentNIdentifierTFN, line(entry), column(entry))
              .argumentIndex(0)
              .order(1)

          val componentIdx      = entryWithIdx._2 + 1
          val fallbackSignature = TypeConstants.cpgUnresolved + "()"
          val fallbackFullName =
            TypeConstants.cpgUnresolved + Constants.componentNPrefix + componentIdx + ":" + fallbackSignature
          val componentNFullNameWithSignature =
            typeInfoProvider.fullNameWithSignature(entry, (fallbackFullName, fallbackSignature))
          val componentNCallCode = destructuringRHS.getText + "." + Constants.componentNPrefix + componentIdx + "()"
          val componentNCallNode =
            callNode(
              componentNCallCode,
              Constants.componentNPrefix + componentIdx,
              componentNFullNameWithSignature._1,
              componentNFullNameWithSignature._2,
              entryTypeFullName,
              DispatchTypes.DYNAMIC_DISPATCH,
              line(entry),
              column(entry)
            )
              .order(2)
              .argumentIndex(2)

          val componentNIdentifierAst =
            scope.lookupVariable(componentNIdentifierNode.name) match {
              case Some(n) => Ast(componentNIdentifierNode).withRefEdge(componentNIdentifierNode, n)
              case None    => Ast(componentNIdentifierNode)
            }

          val componentNAst =
            Ast(componentNCallNode)
              .withChild(componentNIdentifierAst)
              .withArgEdge(componentNCallNode, componentNIdentifierNode)
              .withReceiverEdge(componentNCallNode, componentNIdentifierNode)

          val orderForNode = orderAfterLocals + entryWithIdx._2
          val assignmentCallNode =
            operatorCallNode(
              Operators.assignment,
              entry.getText + " = " + componentNCallCode,
              None,
              line(entry),
              column(entry)
            )
              .order(orderForNode)
          val assignmentAst =
            Ast(assignmentCallNode)
              .withChild(assignmentLHSAst)
              .withArgEdge(assignmentCallNode, assignmentLHSNode)
              .withChild(componentNAst)
              .withArgEdge(assignmentCallNode, componentNCallNode)
          assignmentAst
        }

    localsForEntries.map(AstWithAdditionals(_, Additionals())) ++
      assignmentsForEntries.map(AstWithAdditionals(_, Additionals()))
  }

  def astsForDestructuringDeclaration(expr: KtDestructuringDeclaration, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithAdditionals] = {
    val hasNonRefExprRHS = expr.getInitializer match {
      case _: KtNameReferenceExpression => false
      case _: KtExpression              => true
      case _                            => false
    }
    val isCtor = expr.getInitializer match {
      case typedExpr: KtCallExpression =>
        typeInfoProvider
          .isConstructorCall(typedExpr)
          .getOrElse(false)
      case _ => false
    }
    if (isCtor) {
      astsForDestructuringDeclarationWithCtorRHS(expr, order)
    } else if (hasNonRefExprRHS) {
      astsForDestructuringDeclarationWithNonCtorCallRHS(expr, order)
    } else {
      astsForDestructuringDeclarationWithVarRHS(expr, order)
    }
  }

  def astForUnknown(expr: KtExpression, order: Int, argIdx: Int): AstWithAdditionals = {
    val code = if (expr != null) { expr.getText }
    else { null } // TODO: add test case to check if this is necessary
    val node =
      unknownNode(code, Constants.parserTypeName, line(expr), column(expr))
        .order(order)
        .argumentIndex(argIdx)
    AstWithAdditionals(Ast(node), Additionals())
  }

  def astForStringTemplate(expr: KtStringTemplateExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    if (expr.hasInterpolation) {
      val callNode =
        operatorCallNode(Operators.formatString, expr.getText, Some(typeFullName), line(expr), column(expr))
          .argumentIndex(argIdx)
          .order(order)
      val args =
        expr.getEntries
          .filter { entry =>
            entry.getExpression != null
          }
          .zipWithIndex
          .flatMap { case (entry, idx) =>
            if (entry.getExpression != null) {
              val entryTypeFullName = typeInfoProvider.expressionType(entry.getExpression, TypeConstants.any)
              registerType(entryTypeFullName)

              val valueCallNode =
                operatorCallNode(
                  Operators.formattedValue,
                  entry.getExpression.getText,
                  Some(entryTypeFullName),
                  line(entry.getExpression),
                  column(entry.getExpression)
                )
                  .argumentIndex(idx + 1)
                  .order(idx + 1)
              val valueArgs = astsForExpression(entry.getExpression, idx + 1, idx + 1)
              val call      = callAst(valueCallNode, valueArgs.map(_.ast).toList)
              Seq(AstWithAdditionals(call, Additionals()))
            } else {
              Seq()
            }
          }
      val ast = callAst(callNode, args.toIndexedSeq.map(_.ast).toList)
      AstWithAdditionals(ast, mergedAdditionals(args.toIndexedSeq.map(_.additionals)))
    } else {
      val node =
        literalNode(expr.getText, typeFullName, line(expr), column(expr))
          .order(order)
          .argumentIndex(argIdx)
      AstWithAdditionals(Ast(node), Additionals())
    }
  }

  // TODO: clean up this whole fn
  def astForQualifiedExpression(expr: KtQualifiedExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {

    val callKind        = typeInfoProvider.bindingKind(expr)
    val isStaticCall    = callKind == CallKinds.StaticCall
    val isDynamicCall   = callKind == CallKinds.DynamicCall
    val isExtensionCall = callKind == CallKinds.ExtensionCall

    val hasThisSuperOrNameRefReceiver =
      expr.getReceiverExpression match {
        case _: KtThisExpression          => true
        case _: KtNameReferenceExpression => true
        case _: KtSuperExpression         => true
        case _                            => false
      }
    val hasNameRefSelector = expr.getSelectorExpression.isInstanceOf[KtNameReferenceExpression]
    val isFieldAccessCall  = hasThisSuperOrNameRefReceiver && hasNameRefSelector
    val isCallToSuper =
      expr.getReceiverExpression match {
        case _: KtSuperExpression => true
        case _                    => false
      }
    val isStaticMethodCall = typeInfoProvider.isStaticMethodCall(expr)
    val hasRefToClassReceiver =
      expr.getReceiverExpression match {
        case r: KtNameReferenceExpression =>
          typeInfoProvider.isReferenceToClass(r)
        case _ =>
          false
      }
    val noAstForReceiver = isStaticMethodCall && hasRefToClassReceiver
    val orderForReceiver = 1
    val argIdxForReceiver =
      if (isFieldAccessCall) 1
      else if (isCallToSuper) 0
      else if (isDynamicCall) 0
      else if (isExtensionCall) 0
      else if (isStaticCall) 1
      else 1
    val receiverAstWithCtx =
      astsForExpression(expr.getReceiverExpression, orderForReceiver, argIdxForReceiver).head
    val receiverAst        = receiverAstWithCtx.ast
    val selectorOrderCount = argIdxForReceiver
    val argAsts =
      expr.getSelectorExpression match {
        case selectorExpression: KtCallExpression =>
          withIndex(selectorExpression.getValueArguments.asScala.toSeq) { case (arg, order) =>
            val selectorOrder    = if (isStaticCall) order else selectorOrderCount + order + 1
            val selectorArgIndex = if (isStaticCall) order else selectorOrder - 1
            val asts =
              astsForExpression(arg.getArgumentExpression, selectorOrder, selectorArgIndex)
            asts
          }.flatten
        case typedExpr: KtNameReferenceExpression =>
          val order  = if (isStaticCall) 1 else 2
          val argIdx = if (isStaticCall) 1 else 2
          val node =
            fieldIdentifierNode(typedExpr.getText)
              .order(order)
              .argumentIndex(argIdx)
          List(AstWithAdditionals(Ast(node), Additionals()))
        case _ =>
          List()
      }

    // TODO: add more test cases for this
    // TODO: use the typedecl from the scope here as soon as it's available
    /*
    val astDerivedMethodFullName = {
      if (scopeContext.typeDecl.isDefined) {
        // TODO: handle parameters here and insert the correct types
        val name = expr.getSelectorExpression.getText.replace("(", "").replace(")", "")
        scopeContext.typeDecl.get.fullName + "." + name + ":" + TypeConstants.any + "()"
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
     */

    val astDerivedMethodFullName =
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

    val methodName =
      if (isFieldAccessCall || fullNameWithSig._1 == Operators.fieldAccess) {
        Operators.fieldAccess
      } else {
        expr.getSelectorExpression.getFirstChild.getText
      }
    val dispatchType =
      if (isFieldAccessCall) {
        DispatchTypes.STATIC_DISPATCH
      } else if (callKind == CallKinds.DynamicCall) {
        DispatchTypes.DYNAMIC_DISPATCH
      } else if (callKind == CallKinds.ExtensionCall) {
        DispatchTypes.STATIC_DISPATCH
      } else {
        DispatchTypes.STATIC_DISPATCH
      }

    val _callNode =
      callNode(
        expr.getText,
        methodName,
        fullNameWithSig._1,
        fullNameWithSig._2,
        retType,
        dispatchType,
        line(expr),
        column(expr)
      )
        .order(order)
        .argumentIndex(argIdx)
    val root         = Ast(_callNode)
    val receiverNode = receiverAst.root.get
    val finalAst = {
      if (isExtensionCall || isCallToSuper) {
        root
          .withChild(receiverAst)
          .withArgEdge(_callNode, receiverNode)
          .withChildren(argAsts.map(_.ast))
          .withArgEdges(_callNode, argAsts.map(_.ast.root.get))
      } else if (noAstForReceiver) {
        root
          .withChild(receiverAst)
          .withChildren(argAsts.map(_.ast))
          .withArgEdges(_callNode, argAsts.map(_.ast.root.get))
      } else {
        val ast =
          root
            .withChild(receiverAst)
            .withArgEdge(_callNode, receiverNode)
            .withChildren(argAsts.map(_.ast))
            .withArgEdges(_callNode, argAsts.map(_.ast.root.get))
        if (argAsts.size == 1 && argAsts.head.ast.root.get.isInstanceOf[NewMethodRef]) {
          ast
            .withReceiverEdge(_callNode, argAsts.head.ast.root.get)
        } else {
          ast
            .withReceiverEdge(_callNode, receiverNode)
        }
      }
    }
    val finalCtx = mergedAdditionals(argAsts.map(_.additionals) ++ Seq(receiverAstWithCtx.additionals))
    AstWithAdditionals(finalAst, finalCtx)
  }

  def astForBreak(expr: KtBreakExpression, order: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val node = controlStructureNode(expr.getText, ControlStructureTypes.BREAK, line(expr), column(expr)).order(order)
    AstWithAdditionals(Ast(node), Additionals())
  }

  def astForContinue(expr: KtContinueExpression, order: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val node = controlStructureNode(expr.getText, ControlStructureTypes.CONTINUE, line(expr), column(expr)).order(order)
    AstWithAdditionals(Ast(node), Additionals())
  }

  private def astForTryAsStatement(expr: KtTryExpression, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val tryNode =
      controlStructureNode(expr.getText, ControlStructureTypes.TRY, line(expr), column(expr))
        .order(order)
    val tryAstWithCtx = astsForExpression(expr.getTryBlock, 1, 1).headOption
      .getOrElse(AstWithAdditionals(Ast(), Additionals()))
    val tryAst =
      Ast(tryNode)
        .withChild(tryAstWithCtx.ast)

    val clauseAstsWitCtx =
      withIndex(expr.getCatchClauses.asScala.toSeq) { (entry, order) =>
        astsForExpression(entry.getCatchBody, order + 1, order + 1)
      }.flatten

    val finallyAstsWithCtx =
      if (expr.getFinallyBlock == null) {
        Seq()
      } else {
        val numClauses = clauseAstsWitCtx.size
        astsForExpression(expr.getFinallyBlock.getFinalExpression, numClauses + 2, numClauses + 2)
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
    val finalCtx = mergedAdditionals(
      Seq(tryAstWithCtx.additionals) ++ clauseAstsWitCtx.map(_.additionals) ++ finallyAstsWithCtx.map(_.additionals)
    )
    AstWithAdditionals(finalAst, finalCtx)
  }

  private def astForTryAsExpression(expr: KtTryExpression, order: Int, argumentIndex: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val typeFullName = typeInfoProvider.expressionType(expr.getTryBlock.getStatements.asScala.last, TypeConstants.any)
    registerType(typeFullName)

    val callNode =
      operatorCallNode(Operators.tryCatch, expr.getText, Some(typeFullName), line(expr), column(expr))
        .order(order)
        .argumentIndex(argumentIndex)

    val tryAstWithCtx = astsForExpression(expr.getTryBlock, 1, 1).headOption
      .getOrElse(AstWithAdditionals(Ast(), Additionals()))
    val tryAst =
      Ast(callNode)
        .withChild(tryAstWithCtx.ast)
        .withArgEdge(callNode, tryAstWithCtx.ast.root.get)

    val clauseAstsWitCtx =
      withIndex(expr.getCatchClauses.asScala.toSeq) { (entry, order) =>
        astsForExpression(entry.getCatchBody, order + 1, order + 1)
      }.flatten

    val finalAst =
      tryAst
        .withChildren(clauseAstsWitCtx.map(_.ast))
        .withArgEdges(callNode, clauseAstsWitCtx.map(_.ast.root.get))
    val finalCtx = mergedAdditionals(Seq(tryAstWithCtx.additionals) ++ clauseAstsWitCtx.map(_.additionals))
    AstWithAdditionals(finalAst, finalCtx)
  }

  // TODO: handle parameters passed to the clauses
  def astForTry(expr: KtTryExpression, order: Int, argumentIndex: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    if (KtPsiUtil.isStatement(expr)) {
      astForTryAsStatement(expr, order)
    } else {
      astForTryAsExpression(expr, order, argumentIndex)
    }
  }

  def astForWhile(expr: KtWhileExpression, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val whileNode =
      controlStructureNode(expr.getText, ControlStructureTypes.WHILE, line(expr), column(expr)).order(order)
    val conditionAst = astsForExpression(expr.getCondition, 1, 1).headOption
      .getOrElse(AstWithAdditionals(Ast(), Additionals()))
    val stmtAsts = astsForExpression(expr.getBody, 2, 2)
    val tempAst =
      Ast(whileNode)
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
    val finalCtx = mergedAdditionals(Seq(conditionAst.additionals) ++ stmtAsts.map(_.additionals))
    AstWithAdditionals(ast, finalCtx)
  }

  def astForDoWhile(expr: KtDoWhileExpression, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val doNode   = controlStructureNode(expr.getText, ControlStructureTypes.DO, line(expr), column(expr)).order(order)
    val stmtAsts = astsForExpression(expr.getBody, 1, 1)
    val conditionAst = astsForExpression(expr.getCondition, 2, 2).headOption
      .getOrElse(AstWithAdditionals(Ast(), Additionals()))
    val tempAst =
      Ast(doNode)
        .withChildren(stmtAsts.map(_.ast))
        .withChild(conditionAst.ast)

    val ast =
      conditionAst.ast.root match {
        case Some(r) =>
          tempAst.withConditionEdge(doNode, r)
        case None =>
          tempAst
      }
    AstWithAdditionals(ast, mergedAdditionals(stmtAsts.map(_.additionals) ++ Seq(conditionAst.additionals)))
  }

  /// \/\//\\\\\\\\\\\\\\///\\\//\\//\/\\/\//\/\\//\\/
  ////////////// \\/\/\/\/\/\/\\//\\/\/\///\\/\//\\/\/\/\/\//\/\/\/\/\/\//////\\\|||||/\/\/\/\/\/\\/\/\//\
  // e.g. lowering:
  // for `for (one in l) { <statements> }`
  // BLOCK
  //     LOCAL iterator
  //     loweringOf{iterator = l.iterator()}
  //     CONTROL_STRUCTURE (while)
  //         --AST[order.1]--> loweringOf{iterator.hasNext()}
  //         --AST[order.2]--> BLOCK
  //                            |-> LOCAL one
  //                            |-> loweringOf{one = iterator.next()}
  //                            |-> <statements>
  //
  private def astForForWithSimpleVarLHS(expr: KtForExpression, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val loopRangeText = expr.getLoopRange.getText
    val iteratorName  = Constants.iteratorPrefix + iteratorKeyPool.next()
    val iteratorLocal =
      localNode(iteratorName, TypeConstants.any)
        .order(1)
    val iteratorAssignmentLhs =
      identifierNode(iteratorName, TypeConstants.any)
        .argumentIndex(1)
        .order(1)
    val iteratorLocalAst =
      Ast(iteratorLocal)
        .withRefEdge(iteratorAssignmentLhs, iteratorLocal)

    // TODO: maybe use a different method here, one which does not translate `kotlin.collections.List` to `java.util.List`
    val loopRangeExprTypeFullName =
      typeInfoProvider.expressionType(expr.getLoopRange, TypeConstants.any)
    registerType(loopRangeExprTypeFullName)

    val iteratorAssignmentRhsIdentifier =
      identifierNode(loopRangeText, loopRangeExprTypeFullName)
        .argumentIndex(0)
        .order(1)
    val iteratorAssignmentRhs =
      callNode(
        loopRangeText + "." + Constants.getIteratorMethodName + "()",
        Constants.getIteratorMethodName,
        loopRangeExprTypeFullName + "." + Constants.getIteratorMethodName + ":" + Constants.javaUtilIterator + "()",
        Constants.javaUtilIterator + "()",
        Constants.javaUtilIterator,
        DispatchTypes.DYNAMIC_DISPATCH
      )
        .argumentIndex(2)
        .order(2)

    val iteratorAssignmentRhsAst =
      Ast(iteratorAssignmentRhs)
        .withChild(Ast(iteratorAssignmentRhsIdentifier))
        .withArgEdge(iteratorAssignmentRhs, iteratorAssignmentRhsIdentifier)
        .withReceiverEdge(iteratorAssignmentRhs, iteratorAssignmentRhsIdentifier)

    val iteratorAssignment =
      operatorCallNode(Operators.assignment, iteratorName + " = " + iteratorAssignmentRhs.code, None)
        .order(2)
    val iteratorAssignmentAst =
      Ast(iteratorAssignment)
        .withChild(Ast(iteratorAssignmentLhs))
        .withArgEdge(iteratorAssignment, iteratorAssignmentLhs)
        .withChild(iteratorAssignmentRhsAst)
        .withArgEdge(iteratorAssignment, iteratorAssignmentRhs)

    val controlStructure =
      controlStructureNode(expr.getText, ControlStructureTypes.WHILE, line(expr), column(expr))
        .order(3)

    val conditionIdentifier =
      identifierNode(loopRangeText, loopRangeExprTypeFullName)
        .argumentIndex(0)
        .order(1)

    val hasNextFullName =
      Constants.collectionsIteratorName + "." + Constants.hasNextIteratorMethodName + ":" + TypeConstants.javaLangBoolean + "()"
    val controlStructureCondition =
      callNode(
        iteratorName + "." + Constants.hasNextIteratorMethodName + "()",
        Constants.hasNextIteratorMethodName,
        hasNextFullName,
        TypeConstants.javaLangBoolean + "()",
        TypeConstants.javaLangBoolean,
        DispatchTypes.DYNAMIC_DISPATCH
      ).order(1)
        .argumentIndex(0)
    val controlStructureConditionAst =
      Ast(controlStructureCondition)
        .withChild(Ast(conditionIdentifier))
        .withArgEdge(controlStructureCondition, conditionIdentifier)
        .withReceiverEdge(controlStructureCondition, conditionIdentifier)

    val loopParameterTypeFullName = typeInfoProvider.typeFullName(expr.getLoopParameter, TypeConstants.any)
    registerType(loopParameterTypeFullName)

    val loopParameterName = expr.getLoopParameter.getText
    val loopParameterLocal =
      localNode(loopParameterName, loopParameterTypeFullName)
        .order(1)
    scope.addToScope(loopParameterName, loopParameterLocal) // TODO: remove from scope after the block

    val loopParameterIdentifier =
      identifierNode(loopParameterName, TypeConstants.any)
        .argumentIndex(1)
        .order(1)
    val loopParameterAst =
      Ast(loopParameterLocal)
        .withRefEdge(loopParameterIdentifier, loopParameterLocal)

    val iteratorNextIdentifier =
      identifierNode(iteratorName, TypeConstants.any)
        .argumentIndex(0)
        .order(1)
    val iteratorNextIdentifierAst =
      Ast(iteratorNextIdentifier)
        .withRefEdge(iteratorNextIdentifier, iteratorLocal)

    val iteratorNextCall =
      callNode(
        iteratorName + "." + Constants.nextIteratorMethodName + "()",
        Constants.nextIteratorMethodName,
        Constants.collectionsIteratorName + "." + Constants.nextIteratorMethodName + ":" + TypeConstants.javaLangObject + "()",
        TypeConstants.javaLangObject + "()",
        TypeConstants.javaLangObject,
        DispatchTypes.DYNAMIC_DISPATCH
      ).order(2)
        .argumentIndex(2)
    val iteratorNextCallAst =
      Ast(iteratorNextCall)
        .withChild(iteratorNextIdentifierAst)
        .withArgEdge(iteratorNextCall, iteratorNextIdentifier)
        .withReceiverEdge(iteratorNextCall, iteratorNextIdentifier)
    val loopParameterNextAssignment =
      operatorCallNode(Operators.assignment, loopParameterName + " = " + iteratorNextCall.code, None)
        .order(2)
    val loopParameterNextAssignmentAst =
      Ast(loopParameterNextAssignment)
        .withChild(Ast(loopParameterIdentifier))
        .withArgEdge(loopParameterNextAssignment, loopParameterIdentifier)
        .withChild(iteratorNextCallAst)
        .withArgEdge(loopParameterNextAssignment, iteratorNextCall)

    val stmtAsts = astsForExpression(expr.getBody, 3, 3)
    val controlStructureBody =
      blockNode("", "")
        .order(2)
    val controlStructureBodyAst =
      Ast(controlStructureBody)
        .withChild(loopParameterAst)
        .withChild(loopParameterNextAssignmentAst)
        .withChildren(stmtAsts.map(_.ast))

    val controlStructureAst =
      Ast(controlStructure)
        .withChild(controlStructureConditionAst)
        .withChild(controlStructureBodyAst)
        .withConditionEdge(controlStructure, controlStructureCondition)
    val topLevelBlock =
      blockNode(Constants.codeForLoweredForBlock, "")
        .order(order)
    val outAst =
      Ast(topLevelBlock)
        .withChild(iteratorLocalAst)
        .withChild(iteratorAssignmentAst)
        .withChild(controlStructureAst)
    val outCtx = mergedAdditionals(stmtAsts.map(_.additionals))
    AstWithAdditionals(outAst, outCtx)
  }

  /// \/\//\\\\\\\\\\\\\\///\\\//\\//\/\\/\//\/\\//\\/
  ////////////// \\/\/\/\/\/\/\\//\\/\/\///\\/\//\\/\/\/\/\//\/\/\/\/\/\//////\\\|||||/\/\/\/\/\/\\/\/\//\
  // e.g. lowering:
  // for `for ((d1, d2) in l) { <statements> }`
  // BLOCK
  //     LOCAL iterator
  //     loweringOf{iterator = l.iterator()}
  //     CONTROL_STRUCTURE (while)
  //         --AST[order.1]--> loweringOf{iterator.hasNext()}
  //         --AST[order.2]--> BLOCK
  //                            |-> LOCAL d1
  //                            |-> LOCAL d2
  //                            |-> LOCAL tmp
  //                            |-> loweringOf{tmp = iterator.next()}
  //                            |-> loweringOf{d1 = tmp.component1()}
  //                            |-> loweringOf{d2 = tmp.component2()}
  //                            |-> <statements>
  //
  private def astForForWithDestructuringLHS(expr: KtForExpression, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val loopRangeText = expr.getLoopRange.getText
    val iteratorName  = Constants.iteratorPrefix + iteratorKeyPool.next()
    val localForIterator =
      localNode(iteratorName, TypeConstants.any)
        .order(1)
    val iteratorAssignmentLhs =
      identifierNode(iteratorName, TypeConstants.any)
        .argumentIndex(1)
        .order(1)
    val iteratorLocalAst =
      Ast(localForIterator)
        .withRefEdge(iteratorAssignmentLhs, localForIterator)

    // TODO: maybe use a different method here, one which does not translate `kotlin.collections.List` to `java.util.List`
    val loopRangeExprTypeFullName =
      typeInfoProvider.expressionType(expr.getLoopRange, TypeConstants.any)
    registerType(loopRangeExprTypeFullName)

    val iteratorAssignmentRhsIdentifier =
      identifierNode(loopRangeText, loopRangeExprTypeFullName)
        .argumentIndex(0)
        .order(1)
    val iteratorAssignmentRhs =
      callNode(
        loopRangeText + "." + Constants.getIteratorMethodName + "()",
        Constants.getIteratorMethodName,
        loopRangeExprTypeFullName + "." + Constants.getIteratorMethodName + ":" + Constants.javaUtilIterator + "()",
        Constants.javaUtilIterator + "()",
        Constants.javaUtilIterator,
        DispatchTypes.DYNAMIC_DISPATCH
      )
        .argumentIndex(2)

    val iteratorAssignmentRhsAst =
      Ast(iteratorAssignmentRhs)
        .withChild(Ast(iteratorAssignmentRhsIdentifier))
        .withArgEdge(iteratorAssignmentRhs, iteratorAssignmentRhsIdentifier)
        .withReceiverEdge(iteratorAssignmentRhs, iteratorAssignmentRhsIdentifier)

    val iteratorAssignment =
      operatorCallNode(Operators.assignment, iteratorName + " = " + iteratorAssignmentRhs.code, None)
        .order(2)
    val iteratorAssignmentAst =
      Ast(iteratorAssignment)
        .withChild(Ast(iteratorAssignmentLhs))
        .withArgEdge(iteratorAssignment, iteratorAssignmentLhs)
        .withChild(iteratorAssignmentRhsAst)
        .withArgEdge(iteratorAssignment, iteratorAssignmentRhs)

    val controlStructure =
      controlStructureNode(expr.getText, ControlStructureTypes.WHILE, line(expr), column(expr)).order(3)
    val conditionIdentifier =
      identifierNode(loopRangeText, loopRangeExprTypeFullName)
        .argumentIndex(0)
        .order(1)

    val hasNextFullName =
      Constants.collectionsIteratorName + "." + Constants.hasNextIteratorMethodName + ":" + TypeConstants.javaLangBoolean + "()"
    val controlStructureCondition =
      callNode(
        iteratorName + "." + Constants.hasNextIteratorMethodName + "()",
        Constants.hasNextIteratorMethodName,
        hasNextFullName,
        TypeConstants.javaLangBoolean + "()",
        TypeConstants.javaLangBoolean,
        DispatchTypes.DYNAMIC_DISPATCH
      ).order(1)
        .argumentIndex(0)
    val controlStructureConditionAst =
      Ast(controlStructureCondition)
        .withChild(Ast(conditionIdentifier))
        .withArgEdge(controlStructureCondition, conditionIdentifier)
        .withReceiverEdge(controlStructureCondition, conditionIdentifier)

    val destructuringDeclEntries = expr.getDestructuringDeclaration.getEntries
    val localsForDestructuringVars =
      withIndex(destructuringDeclEntries.asScala.toSeq) { (entry, order) =>
        val entryTypeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
        registerType(entryTypeFullName)

        val entryName = entry.getText
        val node =
          localNode(entryName, entryTypeFullName, None, line(entry), column(entry))
            .order(order)
        // TODO: remove from scope after the block exits [add test where that is not the case]
        scope.addToScope(entryName, node)
        Ast(node)
      }.toList
    val orderAfterDestructuringVarLocals = localsForDestructuringVars.size

    val tmpName          = Constants.tmpLocalPrefix + tmpKeyPool.next
    val orderForTmpLocal = orderAfterDestructuringVarLocals + 1
    val localForTmp =
      localNode(tmpName, TypeConstants.any)
        .order(orderForTmpLocal)
    val localForTmpAst =
      Ast(localForTmp)

    val tmpIdentifier =
      identifierNode(tmpName, TypeConstants.any)
        .argumentIndex(1)
        .order(1)
    val tmpIdentifierAst =
      Ast(tmpIdentifier)
        .withRefEdge(tmpIdentifier, localForTmp)

    val iteratorNextIdentifier =
      identifierNode(iteratorName, TypeConstants.any)
        .argumentIndex(0)
        .order(1)
    val iteratorNextIdentifierAst =
      Ast(iteratorNextIdentifier)
        .withRefEdge(iteratorNextIdentifier, localForIterator)

    val iteratorNextCall =
      callNode(
        iteratorNextIdentifier.code + "." + Constants.nextIteratorMethodName + "()",
        Constants.nextIteratorMethodName,
        Constants.collectionsIteratorName + "." + Constants.nextIteratorMethodName + ":" + TypeConstants.javaLangObject + "()",
        TypeConstants.javaLangObject + "()",
        TypeConstants.javaLangObject,
        DispatchTypes.DYNAMIC_DISPATCH
      ).order(2)
        .argumentIndex(2)

    val iteratorNextCallAst =
      Ast(iteratorNextCall)
        .withChild(iteratorNextIdentifierAst)
        .withArgEdge(iteratorNextCall, iteratorNextIdentifier)
        .withReceiverEdge(iteratorNextCall, iteratorNextIdentifier)
    val orderForTmpEqNextAssignment = orderForTmpLocal + 1
    val tmpParameterNextAssignment =
      operatorCallNode(Operators.assignment, tmpName + " = " + iteratorNextCall.code)
        .order(orderForTmpEqNextAssignment)
    val tmpParameterNextAssignmentAst =
      Ast(tmpParameterNextAssignment)
        .withChild(tmpIdentifierAst)
        .withArgEdge(tmpParameterNextAssignment, tmpIdentifier)
        .withChild(iteratorNextCallAst)
        .withArgEdge(tmpParameterNextAssignment, iteratorNextCall)

    val componentNCalls =
      withIndex(destructuringDeclEntries.asScala.toSeq) { (entry, order) =>
        val entryIdentifier =
          identifierNode(entry.getText, TypeConstants.any, line(entry), column(entry))
            .argumentIndex(1)
            .order(1)

        val matchingLocalForEntry =
          localsForDestructuringVars.filter { l =>
            l.root.get.asInstanceOf[NewLocal].code == entry.getText
          }.head

        val entryIdentifierAst =
          Ast(entryIdentifier)
            .withRefEdge(entryIdentifier, matchingLocalForEntry.root.get)

        val componentIdx      = order
        val fallbackSignature = TypeConstants.cpgUnresolved + "()"
        val fallbackFullName =
          TypeConstants.cpgUnresolved + Constants.componentNPrefix + componentIdx + ":" + fallbackSignature
        val componentNFullNameWithSignature =
          typeInfoProvider.fullNameWithSignature(entry, (fallbackFullName, fallbackSignature))
        val componentNCallCode = tmpName + "." + Constants.componentNPrefix + componentIdx + "()"

        val tmpForComponentNIdentifier =
          identifierNode(tmpName, TypeConstants.any)
            .argumentIndex(0)
            .order(1)

        val tmpForComponentNIdentifierAst =
          Ast(tmpForComponentNIdentifier)
            .withRefEdge(tmpForComponentNIdentifier, localForTmp)

        val componentNName = Constants.componentNPrefix + order
        val tmpComponentNCall =
          callNode(
            componentNCallCode,
            componentNName,
            componentNFullNameWithSignature._1,
            componentNFullNameWithSignature._2,
            TypeConstants.any,
            DispatchTypes.DYNAMIC_DISPATCH
          ).order(2)
            .argumentIndex(2)
        val tmpComponentNCallAst =
          Ast(tmpComponentNCall)
            .withChild(tmpForComponentNIdentifierAst)
            .withArgEdge(tmpComponentNCall, tmpForComponentNIdentifier)
            .withReceiverEdge(tmpComponentNCall, tmpForComponentNIdentifier)

        val componentNAssignment =
          operatorCallNode(Operators.assignment, entryIdentifier.code + " = " + tmpComponentNCall.code)
            .order(order + orderForTmpEqNextAssignment)
        val outAst =
          Ast(componentNAssignment)
            .withChild(entryIdentifierAst)
            .withArgEdge(componentNAssignment, entryIdentifier)
            .withChild(tmpComponentNCallAst)
            .withArgEdge(componentNAssignment, tmpComponentNCall)
        outAst
      }
    val orderAfterComponentNCalls = componentNCalls.map(_.root.get.asInstanceOf[NewCall].order).reverse.take(1).head + 1

    val stmtAsts = astsForExpression(expr.getBody, orderAfterComponentNCalls, orderAfterComponentNCalls)
    val controlStructureBody =
      blockNode("", "")
        .order(2)
    val controlStructureBodyAst =
      Ast(controlStructureBody)
        .withChildren(localsForDestructuringVars)
        .withChild(localForTmpAst)
        .withChild(tmpParameterNextAssignmentAst)
        .withChildren(componentNCalls)
        .withChildren(stmtAsts.map(_.ast))

    val controlStructureAst =
      Ast(controlStructure)
        .withChild(controlStructureConditionAst)
        .withChild(controlStructureBodyAst)
        .withConditionEdge(controlStructure, controlStructureCondition)
    val topLevelBlock = blockNode(Constants.codeForLoweredForBlock, "").order(order)
    val outAst =
      Ast(topLevelBlock)
        .withChild(iteratorLocalAst)
        .withChild(iteratorAssignmentAst)
        .withChild(controlStructureAst)
    val outCtx = mergedAdditionals(stmtAsts.map(_.additionals))
    AstWithAdditionals(outAst, outCtx)
  }

  def astForFor(expr: KtForExpression, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    if (expr.getDestructuringDeclaration != null) {
      astForForWithDestructuringLHS(expr, order)
    } else {
      astForForWithSimpleVarLHS(expr, order)
    }
  }

  def astForWhen(expr: KtWhenExpression, order: Int, argumentIndex: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val astForSubject =
      astsForExpression(expr.getSubjectExpression, 1, 1).headOption
        .getOrElse(AstWithAdditionals(Ast(), Additionals()))

    val astsForEntries =
      withIndex(expr.getEntries.asScala.toSeq) { (e, order) =>
        astsForWhenEntry(e, order)
      }.flatten

    val switchBlockNode =
      blockNode(expr.getEntries.asScala.map(_.getText).mkString("\n"), TypeConstants.any, line(expr), column(expr))
        .order(2)
    val astForBlock =
      Ast(switchBlockNode)
        .withChildren(astsForEntries.map(_.ast))

    val codeForSwitch =
      if (expr.getSubjectExpression != null) {
        Constants.when + s"(${expr.getSubjectExpression.getText})"
      } else {
        Constants.when
      }
    val switchNode =
      controlStructureNode(codeForSwitch, ControlStructureTypes.SWITCH, line(expr), column(expr))
        .order(order)
        .argumentIndex(argumentIndex)
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
    val finalCtx = mergedAdditionals(astsForEntries.map(_.additionals))
    AstWithAdditionals(astWithCondition, finalCtx)
  }

  def astsForWhenEntry(entry: KtWhenEntry, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithAdditionals] = {
    // TODO: get all conditions with entry.getConditions()
    val name =
      if (entry.getElseKeyword == null) {
        Constants.caseNodePrefix + order.toString
      } else {
        Constants.defaultCaseNode
      }
    val jumpNode =
      jumpTargetNode(entry.getText, name, Constants.caseNodeParserTypeName, line(entry), column(entry))
        .order(order)
        .argumentIndex(order)
    val exprNode = astsForExpression(entry.getExpression, order + 1, order + 1).headOption
      .getOrElse(AstWithAdditionals(Ast(), Additionals()))
    val jumpNodeAstsWithCtx = AstWithAdditionals(Ast(jumpNode), Additionals())
    Seq(jumpNodeAstsWithCtx) ++ Seq(exprNode)
  }

  def astForIf(expr: KtIfExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val isChildOfControlStructureBody = expr.getParent.isInstanceOf[KtContainerNodeForControlStructureBody]
    if (KtPsiUtil.isStatement(expr) && !isChildOfControlStructureBody) {
      astForIfAsControlStructure(expr, order)
    } else {
      astForIfAsExpression(expr, order, argIdx)
    }
  }

  def astForIfAsControlStructure(expr: KtIfExpression, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val ifNode =
      controlStructureNode(expr.getText, ControlStructureTypes.IF, line(expr), column(expr))
        .order(order)
    val conditionAst = astsForExpression(expr.getCondition, 1, 1)

    val thenAsts =
      expr.getThen match {
        case b: KtBlockExpression => Seq(astForBlock(b, 2, true))
        case e                    => astsForExpression(e, 2, 2)
      }
    val elseAsts =
      expr.getElse match {
        case b: KtBlockExpression => Seq(astForBlock(b, 3, true))
        case e                    => astsForExpression(e, 3, 3)
      }

    val ast =
      Ast(ifNode)
        .withChild(conditionAst.head.ast)
        .withChildren(thenAsts.map(_.ast) ++ elseAsts.map(_.ast))
    val withCondition = conditionAst.head.ast.root match {
      case Some(r) =>
        ast.withConditionEdge(ifNode, r)
      case None =>
        ast
    }
    val finalCtx = mergedAdditionals(
      conditionAst.map(_.additionals) ++ thenAsts.map(_.additionals) ++ elseAsts.map(_.additionals)
    )
    AstWithAdditionals(withCondition, finalCtx)
  }

  def astForIfAsExpression(expr: KtIfExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val retType = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(retType)

    val callNode =
      operatorCallNode(Operators.conditional, expr.getText, Some(retType), line(expr), column(expr))
        .order(order)
        .argumentIndex(argIdx)
    val conditionAsts = astsForExpression(expr.getCondition, 1, 1)
    val thenAsts      = astsForExpression(expr.getThen, 2, 2)
    val elseAsts      = astsForExpression(expr.getElse, 3, 3)

    val childAsts = conditionAsts.map(_.ast) ++ thenAsts.map(_.ast) ++ elseAsts.map(_.ast)
    val ctx = mergedAdditionals(
      conditionAsts.map(_.additionals) ++ thenAsts.map(_.additionals) ++ elseAsts.map(_.additionals)
    )
    if (conditionAsts.nonEmpty && conditionAsts.head.ast.root != null) {
      val ast =
        Ast(callNode)
          .withChildren(childAsts)
          .withArgEdges(callNode, childAsts.map(_.root.get))
      AstWithAdditionals(ast, ctx)
    } else {
      logger.warn("Parsing failed for expr `" + expr.getName + "` in file `" + fileWithMeta.filename + "`.")
      logger.debug(" > expr.text `" + expr.getText + "`")

      val _unknownNode =
        unknownNode(expr.getText, Constants.parserTypeName, line(expr), column(expr))
          .order(order)
          .argumentIndex(order)
      AstWithAdditionals(Ast(_unknownNode), Additionals())
    }
  }

  private def astForCtorCall(expr: KtCallExpression, order: Int = 1, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.cpgUnresolved)
    registerType(typeFullName)

    val tmpBlockNode =
      blockNode("", typeFullName)
        .order(order)
        .argumentIndex(argIdx)
    val tmpName = Constants.tmpLocalPrefix + tmpKeyPool.next
    val tmpLocalNode =
      localNode(tmpName, typeFullName)
        .order(1)
    val assignmentRhsNode =
      operatorCallNode(Operators.alloc, Constants.alloc, Some(typeFullName), line(expr), column(expr))
        .order(2)
        .argumentIndex(2)

    // TODO: add check here for the `.get`
    val assignmentLhsNode =
      identifierNode(tmpName, typeFullName, line(expr), column(expr))
        .argumentIndex(0)
        .order(1)
    val assignmentNode =
      operatorCallNode(Operators.assignment, Operators.assignment)
        .order(2)
    val assignmentAst =
      Ast(assignmentNode)
        .withChild(Ast(assignmentLhsNode))
        .withChild(Ast(assignmentRhsNode))
        .withArgEdges(assignmentNode, Seq(assignmentLhsNode, assignmentRhsNode))

    val initReceiverNode =
      identifierNode(tmpName, typeFullName, line(expr), column(expr))
        .argumentIndex(0)
        .order(1)
    val initReceiverAst = Ast(initReceiverNode)

    val args = expr.getValueArguments
    val argAsts =
      withIndex(args.asScala.toSeq) { case (arg, argOrder) =>
        astsForExpression(arg.getArgumentExpression, argOrder, argOrder)
      }.flatten

    val fullNameWithSig = typeInfoProvider.fullNameWithSignature(expr, (TypeConstants.any, TypeConstants.any))
    val returnType      = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(returnType)

    val initCallNode =
      callNode(
        expr.getText,
        Constants.init,
        fullNameWithSig._1,
        fullNameWithSig._2,
        TypeConstants.void,
        DispatchTypes.STATIC_DISPATCH,
        line(expr),
        column(expr)
      )
        .order(3)
        .argumentIndex(2)
    val initCallAst =
      Ast(initCallNode)
        .withChild(initReceiverAst)
        .withChildren(argAsts.map(_.ast))
        .withArgEdges(initCallNode, Seq(initReceiverNode) ++ argAsts.flatMap(_.ast.root))

    val lastIdentifier =
      identifierNode(tmpName, typeFullName, line(expr), column(expr))
        .order(3)
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

    val initArgsCtx = mergedAdditionals(argAsts.map(_.additionals))
    AstWithAdditionals(blockAst, initArgsCtx)
  }

  private def astsForProperty(expr: KtProperty, order: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): Seq[AstWithAdditionals] = {
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
      identifierNode(elem.getText, typeFullName, line(elem), column(elem))
        .argumentIndex(1)
        .order(1)
    val assignmentNode =
      operatorCallNode(Operators.assignment, expr.getText, None, line(expr), column(expr))
        .order(order + 1)
        .argumentIndex(order + 1)
    val node =
      localNode(expr.getName, typeFullName, None, line(expr), column(expr))
        .order(order)
    scope.addToScope(expr.getName, node)

    val hasRHSCtorCall =
      expr.getDelegateExpressionOrInitializer match {
        case typed: KtCallExpression =>
          typeInfoProvider.isConstructorCall(typed).getOrElse(false)
        case _ => false
      }
    val rhsAsts = if (hasRHSCtorCall) {
      Seq(astForCtorCall(expr.getDelegateExpressionOrInitializer.asInstanceOf[KtCallExpression], 2, 2))
    } else {
      astsForExpression(expr.getDelegateExpressionOrInitializer, 2, 2)
    }
    val call = callAst(assignmentNode, List(Ast(identifier)) ++ rhsAsts.map(_.ast))

    val rhsCtx   = mergedAdditionals(rhsAsts.map(_.additionals))
    val finalCtx = Additionals(rhsCtx.bindingsInfo, rhsCtx.lambdaAsts, rhsCtx.lambdaBindingInfo)
    Seq(AstWithAdditionals(call, Additionals())) ++
      Seq(AstWithAdditionals(Ast(node).withRefEdge(identifier, node), finalCtx))
  }

  def astForNameReference(expr: KtNameReferenceExpression, order: Int, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val isRefToClass = typeInfoProvider.isReferenceToClass(expr)
    if (isRefToClass) {
      astForNameReferenceToType(expr, order, argIdx)
    } else {
      typeInfoProvider.isReferencingMember(expr) match {
        case true  => astForNameReferenceToMember(expr, order, argIdx)
        case false => astForNonSpecialNameReference(expr, order, argIdx)
      }
    }
  }

  private def astForNameReferenceToType(expr: KtNameReferenceExpression, order: Int, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val typeFullName = typeInfoProvider.typeFullName(expr, TypeConstants.any)
    registerType(typeFullName)

    val node =
      typeRefNode(expr.getIdentifier.getText, typeFullName, line(expr), column(expr))
        .order(order)
        .argumentIndex(argIdx)
    AstWithAdditionals(Ast(node), Additionals())
  }

  private def astForNameReferenceToMember(expr: KtNameReferenceExpression, order: Int, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val typeFullName = typeInfoProvider.typeFullName(expr, TypeConstants.any)
    registerType(typeFullName)

    val callNode =
      operatorCallNode(
        Operators.fieldAccess,
        Constants.this_ + "." + expr.getReferencedName,
        Some(typeFullName),
        line(expr),
        column(expr)
      )
        .order(order)
        .argumentIndex(argIdx)

    val referenceTargetTypeFullName = typeInfoProvider.referenceTargetTypeFullName(expr, TypeConstants.any)
    registerType(referenceTargetTypeFullName)

    val thisNode =
      identifierNode(Constants.this_, referenceTargetTypeFullName, line(expr), column(expr))
        .argumentIndex(1)
        .order(1)
    val thisAst =
      scope.lookupVariable(Constants.this_) match {
        case Some(n) => Ast(thisNode).withRefEdge(thisNode, n)
        case None    => Ast(thisNode)
      }

    val _fieldIdentifierNode =
      fieldIdentifierNode(expr.getReferencedName, line(expr), column(expr))
        .order(2)
        .argumentIndex(2)
    val ast =
      Ast(callNode)
        .withChild(thisAst)
        .withChild(Ast(_fieldIdentifierNode))
        .withArgEdge(callNode, thisNode)
        .withArgEdge(callNode, _fieldIdentifierNode)
        .withReceiverEdge(callNode, thisNode)
    AstWithAdditionals(ast, Additionals())
  }

  private def astForNonSpecialNameReference(expr: KtNameReferenceExpression, order: Int, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val typeFullName = typeInfoProvider.typeFullName(expr, TypeConstants.any)
    registerType(typeFullName)

    val name = expr.getIdentifier.getText
    val node =
      identifierNode(name, typeFullName, line(expr), column(expr))
        .argumentIndex(argIdx)
        .order(order)

    val ast =
      scope.lookupVariable(name) match {
        case Some(n) => Ast(node).withRefEdge(node, n)
        case None    => Ast(node)
      }
    AstWithAdditionals(ast, Additionals())
  }

  def astForLiteral(expr: KtConstantExpression, order: Int, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val node =
      literalNode(expr.getText, typeFullName, line(expr), column(expr))
        .order(order)
        .argumentIndex(argIdx)
    AstWithAdditionals(Ast(node), Additionals())
  }

  def astForBinaryExpr(expr: KtBinaryExpression, order: Int, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
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
    val _callNode =
      callNode(
        expr.getText,
        name,
        fullName,
        signature,
        typeFullName,
        DispatchTypes.STATIC_DISPATCH,
        line(expr),
        column(expr)
      )
        .argumentIndex(argIdx)
        .order(order)
    val args =
      astsForExpression(expr.getLeft, 1, 1) ++ astsForExpression(expr.getRight, 2, 2)
    val ast = callAst(_callNode, args.map(_.ast).toList)
    AstWithAdditionals(ast, mergedAdditionals(args.map(_.additionals)))
  }

  private def astForCall(expr: KtCallExpression, order: Int = 1, argIdx: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    val declFullNameOption = typeInfoProvider.containingDeclFullName(expr)
    declFullNameOption.foreach(registerType)

    val args = expr.getValueArguments
    val argAsts =
      withIndex(args.asScala.toSeq) { case (arg, argOrder) =>
        astsForExpression(arg.getArgumentExpression, argOrder, argOrder)
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

    val _callNode =
      callNode(
        expr.getText,
        referencedName,
        fullNameWithSig._1,
        fullNameWithSig._2,
        returnType,
        DispatchTypes.STATIC_DISPATCH,
        line(expr),
        column(expr)
      )
        .order(order)
        .argumentIndex(argIdx)
    val ast =
      Ast(_callNode)
        .withChildren(argAsts.map(_.ast))
        .withArgEdges(_callNode, argAsts.flatMap(_.ast.root))
    val finalCtx = mergedAdditionals(argAsts.map(_.additionals))
    AstWithAdditionals(ast, finalCtx)
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

    val node =
      memberNode(name, typeFullName, line(decl), column(decl))
        .order(childNum)
    scope.addToScope(name, node)
    Ast(node)
  }

  private def astForParameter(param: KtParameter, childNum: Int)(implicit
    fileInfo: FileInfo,
    typeInfoProvider: TypeInfoProvider
  ): AstWithAdditionals = {
    // TODO: !!!! lower destructuring declarations inside lambdas properly
    val name =
      if (param.getDestructuringDeclaration != null) {
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
      methodParameterNode(name, typeFullName, line(param), column(param))
        .order(childNum)
    scope.addToScope(name, parameterNode)
    AstWithAdditionals(Ast(parameterNode), Additionals())
  }
}
