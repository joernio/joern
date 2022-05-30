package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.ast.Nodes._
import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.KtFileWithMeta
import io.joern.kotlin2cpg.psi.Extractor._
import io.joern.kotlin2cpg.types.{CallKinds, TypeConstants, TypeInfoProvider}
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
import scala.collection.mutable

case class BindingInfo(node: NewBinding, edgeMeta: Seq[(NewNode, NewNode, String)])
case class ClosureBindingDef(node: NewClosureBinding, captureEdgeTo: NewMethodRef, refEdgeTo: NewNode)

class AstCreator(fileWithMeta: KtFileWithMeta, xTypeInfoProvider: TypeInfoProvider, global: Global)
    extends AstCreatorBase(fileWithMeta.filename) {

  private val closureBindingDefQueue: mutable.ArrayBuffer[ClosureBindingDef] = mutable.ArrayBuffer.empty
  private val bindingInfoQueue: mutable.ArrayBuffer[BindingInfo]             = mutable.ArrayBuffer.empty
  private val lambdaAstQueue: mutable.ArrayBuffer[Ast]                       = mutable.ArrayBuffer.empty
  private val lambdaBindingInfoQueue: mutable.ArrayBuffer[BindingInfo]       = mutable.ArrayBuffer.empty

  private val lambdaKeyPool   = new IntervalKeyPool(first = 1, last = Long.MaxValue)
  private val tmpKeyPool      = new IntervalKeyPool(first = 1, last = Long.MaxValue)
  private val iteratorKeyPool = new IntervalKeyPool(first = 1, last = Long.MaxValue)

  private val relativizedPath = fileWithMeta.relativizedPath

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
  private def registerType(typeName: String): String = {
    global.usedTypes.putIfAbsent(typeName, true)
    typeName
  }

  private def storeInDiffGraph(ast: Ast): Unit = {
    Ast.storeInDiffGraph(ast, diffGraph)

    bindingInfoQueue.foreach { bindingInfo =>
      diffGraph.addNode(bindingInfo.node)

      bindingInfo.edgeMeta.foreach { edgeMeta =>
        diffGraph.addEdge(edgeMeta._1, edgeMeta._2, edgeMeta._3)
      }
    }

    lambdaBindingInfoQueue.foreach { bindingInfo =>
      diffGraph.addNode(bindingInfo.node)

      bindingInfo.edgeMeta.foreach { edgeMeta =>
        diffGraph.addEdge(edgeMeta._1, edgeMeta._2, edgeMeta._3)
      }
    }

    closureBindingDefQueue.foreach { cbd =>
      diffGraph.addNode(cbd.node)
      diffGraph.addEdge(cbd.captureEdgeTo, cbd.node, EdgeTypes.CAPTURE)
      diffGraph.addEdge(cbd.node, cbd.refEdgeTo, EdgeTypes.REF)
    }
  }

  private def astForFile(fileWithMeta: KtFileWithMeta)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val ktFile = fileWithMeta.f

    val importDirectives = ktFile.getImportList.getImports.asScala
    val importAsts       = importDirectives.toList.map(astForImportDirective)
    val namespaceBlocksForImports =
      importAsts
        .flatMap { ast =>
          ast.root match {
            case Some(node) => Some(node)
            case _          => None
          }
        }
        .collect { case n: NewImport => n }
        .map { n =>
          val importedName =
            if (n.isWildcard.getOrElse(false).asInstanceOf[Boolean]) {
              Constants.wildcardImportName
            } else {
              n.importedEntity.getOrElse("")
            }
          val node = namespaceBlockNode(importedName, importedName, relativizedPath)
          Ast(node)
        }

    val declarationsAsts =
      ktFile.getDeclarations.asScala.toSeq.map(astForDeclaration).flatten
    val fileNode = NewFile().name(fileWithMeta.relativizedPath)
    val lambdaTypeDecls =
      lambdaBindingInfoQueue.flatMap(
        _.edgeMeta
          .map(_._1)
          .collect { case n: NewTypeDecl => Ast(n) }
      )

    val namespaceBlockAst =
      astForPackageDeclaration(ktFile.getPackageFqName.toString)
        .withChildren(importAsts ++ declarationsAsts ++ lambdaAstQueue ++ lambdaTypeDecls)
    Ast(fileNode)
      .withChild(namespaceBlockAst)
      .withChildren(namespaceBlocksForImports)
  }

  def astForImportDirective(directive: KtImportDirective): Ast = {
    val isWildcard = directive.getLastChild.getText == Constants.wildcardImportName || directive.getImportedName == null
    val node =
      NewImport()
        .isWildcard(isWildcard)
        .isExplicit(true)
        .importedEntity(directive.getImportPath.getPathStr)
        .code(Constants.importKeyword + " " + directive.getImportPath.getPathStr)
        .lineNumber(line(directive))
        .columnNumber(column(directive))
    Ast(node)
  }

  def astForPackageDeclaration(packageName: String): Ast = {
    val node =
      packageName match {
        case Constants.root =>
          namespaceBlockNode(
            NamespaceTraversal.globalNamespaceName,
            NamespaceTraversal.globalNamespaceName,
            relativizedPath
          )
        case _ =>
          val name = packageName.split("\\.").lastOption.getOrElse("")
          namespaceBlockNode(name, packageName, relativizedPath)
      }
    Ast(node)
  }

  def astForDeclaration(decl: KtDeclaration)(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    decl match {
      case d: KtClass             => astsForClassOrObject(d)
      case d: KtObjectDeclaration => astsForClassOrObject(d)
      case d: KtNamedFunction     => Seq(astForMethod(d))
      case d: KtTypeAlias         => Seq(astForTypeAlias(d))
      case d: KtProperty          => Seq(astForUnknown(d, None)) // TODO: these are globals, represent them correctly
      case unhandled =>
        logger.error(
          "Unknown declaration type encountered with text `" + unhandled.getText + "` and class `" + unhandled.getClass + "`!"
        )
        Seq()
    }
  }

  def astForTypeAlias(typeAlias: KtTypeAlias)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val fullName          = typeInfoProvider.fullName(typeAlias, TypeConstants.any)
    val aliasTypeFullName = typeInfoProvider.aliasTypeFullName(typeAlias, TypeConstants.any)
    registerType(fullName)
    registerType(aliasTypeFullName)

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
    Ast(node)
  }

  def astsForClassOrObject(ktClass: KtClassOrObject)(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
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
    scope.pushNewScope(typeDecl)

    val classFunctions =
      Option(ktClass.getBody)
        .map(_.getFunctions.asScala.collect { case f: KtNamedFunction => f })
        .getOrElse(List())
    val classDeclarations =
      Option(ktClass.getBody)
        .map(_.getDeclarations.asScala.filterNot(_.isInstanceOf[KtNamedFunction]))
        .getOrElse(List())

    /** curently unused val blockInitializers = if (ktClass.getBody != null) { ktClass.getBody.getAnonymousInitializers
      * } else { List().asJava }
      */
    val methodAsts = classFunctions.toSeq.map(astForMethod)
    val bindingsInfo =
      methodAsts
        .flatMap { ast =>
          ast.root match {
            case Some(node) =>
              node match {
                case m: NewMethod => Some(m)
                case _            => None
              }
            case _ => None
          }
        }
        .map { _methodNode =>
          val node = bindingNode(_methodNode.name, _methodNode.signature)
          BindingInfo(node, List((typeDecl, node, EdgeTypes.BINDS), (node, _methodNode, EdgeTypes.REF)))
        }
    val constructorParams = ktClass.getPrimaryConstructorParameters.asScala.toList
    val defaultSignature =
      Option(ktClass.getPrimaryConstructor)
        .map { _ => typeInfoProvider.erasedSignature(constructorParams) }
        .getOrElse(TypeConstants.void + "()")
    val defaultFullName = classFullName + "." + TypeConstants.initPrefix + ":" + defaultSignature
    val ctorFnWithSig =
      typeInfoProvider.fullNameWithSignature(ktClass.getPrimaryConstructor, (defaultFullName, defaultSignature))
    val primaryCtorMethodNode =
      methodNode(
        TypeConstants.initPrefix,
        ctorFnWithSig._1,
        ctorFnWithSig._2,
        relativizedPath,
        line(ktClass.getPrimaryConstructor),
        column(ktClass.getPrimaryConstructor)
      )

    val ctorThisParam =
      methodParameterNode(Constants.this_, classFullName)
        .order(0)
    scope.addToScope(Constants.this_, ctorThisParam)

    val constructorParamsAsts =
      Seq(Ast(ctorThisParam)) ++
        withIndex(constructorParams) { (p, order) =>
          astForParameter(p, order)
        }
    val ctorMethodBlock =
      blockNode("", TypeConstants.void)

    val memberSetCalls =
      constructorParams
        .filter(_.hasValOrVar)
        .map { case ctorParam =>
          val typeFullName = typeInfoProvider.typeFullName(ctorParam, TypeConstants.any)
          registerType(typeFullName)

          val paramName       = ctorParam.getName
          val paramIdentifier = identifierNode(paramName, typeFullName)

          val matchingMethodParamNode =
            constructorParamsAsts.flatMap { ast =>
              val node = ast.root.get.asInstanceOf[NewMethodParameterIn]
              if (node.name == paramName) {
                Some(node)
              } else {
                None
              }
            }.head
          val paramIdentifierAst =
            Ast(paramIdentifier)
              .withRefEdge(paramIdentifier, matchingMethodParamNode)

          val this_           = identifierNode(Constants.this_, classFullName)
          val fieldIdentifier = fieldIdentifierNode(paramName)
          val fieldAccessCall =
            operatorCallNode(Operators.fieldAccess, Constants.this_ + "." + paramName, Some(typeFullName))
          val fieldAccessCallAst = callAst(fieldAccessCall, List(this_, fieldIdentifier).map(Ast(_)))

          val assignmentNode =
            operatorCallNode(Operators.assignment, fieldAccessCall.code + " = " + paramIdentifier.code)
          callAst(assignmentNode, List(fieldAccessCallAst, paramIdentifierAst))
        }

    val ctorMethodBlockAst =
      Ast(ctorMethodBlock)
        .withChildren(memberSetCalls)

    val typeFullName = typeInfoProvider.typeFullName(ktClass.getPrimaryConstructor, TypeConstants.any)
    val constructorMethodReturn =
      methodReturnNode(
        Some(line(ktClass.getPrimaryConstructor)),
        Some(column(ktClass.getPrimaryConstructor)),
        typeFullName,
        Some(classFullName)
      )

    val constructorAst =
      Ast(primaryCtorMethodNode)
        .withChildren(constructorParamsAsts ++ List(ctorMethodBlockAst, Ast(constructorMethodReturn)))

    val membersFromPrimaryCtorAsts =
      ktClass.getPrimaryConstructorParameters.asScala.toList
        .filter(_.hasValOrVar)
        .collect { case param =>
          val typeFullName = typeInfoProvider.parameterType(param, TypeConstants.any)
          val node =
            memberNode(param.getName, typeFullName, line(param), column(param))

          Ast(node)
        }

    val secondaryConstructorAsts =
      ktClass.getSecondaryConstructors.asScala.toSeq.map { secondaryCtor =>
        val constructorParams = secondaryCtor.getValueParameters.asScala.toList
        val defaultSignature  = typeInfoProvider.erasedSignature(constructorParams)
        val defaultFullName   = classFullName + "." + TypeConstants.initPrefix + ":" + defaultSignature
        val ctorFnWithSig = typeInfoProvider.fullNameWithSignature(secondaryCtor, (defaultFullName, defaultSignature))
        val secondaryCtorMethodNode =
          methodNode(
            Constants.init,
            ctorFnWithSig._1,
            ctorFnWithSig._2,
            relativizedPath,
            line(secondaryCtor),
            column(secondaryCtor)
          )

        scope.pushNewScope(secondaryCtorMethodNode)

        val typeFullName = typeInfoProvider.typeFullName(secondaryCtor, TypeConstants.any)
        registerType(typeFullName)

        val ctorThisParam =
          methodParameterNode(Constants.this_, classFullName)
            .order(0)
        scope.addToScope(Constants.this_, ctorThisParam)
        val constructorParamsAsts =
          Seq(Ast(ctorThisParam)) ++
            withIndex(constructorParams) { (p, order) =>
              astForParameter(p, order)
            }

        val orderAfterCtorParams = constructorParamsAsts.size + 1

        val ctorMethodBlockAst =
          if (secondaryCtor.getBodyExpression != null) {
            Seq(astForBlock(secondaryCtor.getBodyExpression, orderAfterCtorParams))
          } else {
            Seq()
          }
        scope.popScope()

        val ctorMethodReturnNode =
          methodReturnNode(Some(line(secondaryCtor)), Some(column(secondaryCtor)), typeFullName, Some(classFullName))

        val children =
          constructorParamsAsts ++ ctorMethodBlockAst ++ List(Ast(ctorMethodReturnNode))
        Ast(secondaryCtorMethodNode)
          .withChildren(children)
      }

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
          val componentIdx = valueParamWithIdx._2 + 1

          val typeFullName = typeInfoProvider.typeFullName(valueParam, TypeConstants.any)
          registerType(typeFullName)

          val componentName = Constants.componentNPrefix + componentIdx
          val signature     = typeFullName + "()"
          val fullName      = typeDecl.fullName + "." + componentName + ":" + signature

          val _methodNode =
            methodNode(componentName, fullName, signature, relativizedPath)

          val thisParam =
            methodParameterNode(Constants.this_, classFullName)
              .order(0)
          val thisIdentifier =
            identifierNode(Constants.this_, typeDecl.fullName)
          val fieldIdentifier =
            fieldIdentifierNode(valueParam.getName, line(valueParam), column(valueParam))

          val fieldAccessCall =
            operatorCallNode(Operators.fieldAccess, Constants.this_ + "." + valueParam.getName, Some(typeFullName))
          val fieldAccessCallAst = callAst(fieldAccessCall, List(thisIdentifier, fieldIdentifier).map(Ast(_)))

          val _returnNode =
            returnNode(Constants.ret)
          val returnAst =
            Ast(_returnNode)
              .withChild(fieldAccessCallAst)
              .withArgEdge(_returnNode, fieldAccessCall)

          val methodBlock = blockNode(fieldAccessCall.code, typeFullName)
          val methodBlockAst =
            Ast(methodBlock)
              .withChild(returnAst)

          val methodReturn = methodReturnNode(None, None, typeFullName)
          methodAst(_methodNode, Seq(thisParam), methodBlockAst, methodReturn)
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

    val memberAsts = classDeclarations.toSeq.map(astForMember)
    val children =
      methodAsts ++
        List(constructorAst) ++
        membersFromPrimaryCtorAsts ++
        secondaryConstructorAsts ++
        componentNMethodAsts.toList ++
        memberAsts
    val ast = Ast(typeDecl).withChildren(children)

    (bindingsInfo ++ componentNBindingsInfo)
      .foreach(bindingInfoQueue.prepend(_))

    val finalAst =
      if (typeInfoProvider.isCompanionObject(ktClass)) {
        val companionMemberTypeFullName =
          ktClass.getParent.getParent match {
            case c: KtClassOrObject => typeInfoProvider.typeFullName(c, TypeConstants.any)
            case _                  => TypeConstants.any
          }
        registerType(companionMemberTypeFullName)

        val companionObjectMember =
          memberNode(Constants.companionObjectMemberName, companionMemberTypeFullName)
        ast.withChild(Ast(companionObjectMember))
      } else {
        ast
      }
    val companionObjectAsts =
      if (ktClass.getCompanionObjects.isEmpty) Seq()
      else astsForClassOrObject(ktClass.getCompanionObjects.asScala.head)
    scope.popScope()

    Seq(finalAst) ++ companionObjectAsts
  }

  private def astForMethod(ktFn: KtNamedFunction)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val fnWithSig = typeInfoProvider.fullNameWithSignature(ktFn, ("", ""))
    val lineEndElement =
      Option(ktFn.getBodyBlockExpression)
        .map(_.getRBrace)
        .getOrElse(ktFn)
    val _methodNode =
      methodNode(
        ktFn.getName,
        fnWithSig._1,
        fnWithSig._2,
        relativizedPath,
        line(ktFn),
        column(ktFn),
        line(lineEndElement),
        column(lineEndElement)
      )
    scope.pushNewScope(_methodNode)

    val parameters =
      withIndex(ktFn.getValueParameters.asScala.toSeq) { (p, order) =>
        astForParameter(p, order)
      }.flatMap { ast =>
        ast.root match {
          case Some(node) =>
            node match {
              case p: NewMethodParameterIn => Some(p)
              case _                       => None
            }
          case None => None
        }
      }

    val lastOrder = parameters.size + 2
    val bodyAst =
      ktFn.getBodyBlockExpression match {
        case blockExpr if blockExpr != null => astForBlock(blockExpr, lastOrder)
        case _ =>
          val blockNode = NewBlock()
          Ast(blockNode)
      }
    scope.popScope()

    val explicitTypeName =
      Option(ktFn.getTypeReference)
        .map(_.getText)
        .getOrElse(TypeConstants.any)
    val typeFullName = typeInfoProvider.returnType(ktFn, explicitTypeName)
    registerType(typeFullName)

    val returnNode = methodReturnNode(Some(line(ktFn)), Some(column(ktFn)), typeFullName)
    methodAst(_methodNode, parameters, bodyAst, returnNode)
  }

  private def astForBlock(
    expr: KtBlockExpression,
    argIdx: Int,
    pushToScope: Boolean = false,
    localsForCaptures: List[NewLocal] = List()
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    val block =
      blockNode(expr.getStatements.asScala.map(_.getText).mkString("\n"), typeFullName, line(expr), column(expr))
        .argumentIndex(argIdx)
    if (pushToScope) scope.pushNewScope(block)
    val statementAsts =
      withIndex(expr.getStatements.asScala.toSeq) { (statement, order) =>
        astsForExpression(statement, order, order)
      }.flatten
    if (pushToScope) scope.popScope()

    Ast(block).withChildren(localsForCaptures.map(Ast(_)) ++ statementAsts)
  }

  private def astsForReturnExpression(
    expr: KtReturnExpression
  )(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    val child = astsForExpression(expr.getReturnedExpression, 1, 1).headOption
      .getOrElse(Ast())
    val node =
      returnNode(expr.getText, line(expr), column(expr))
    val ast =
      Ast(node)
        .withChild(child)
        .withArgEdges(node, child.root.toList)
    Seq(ast)
  }

  def astForIsExpression(expr: KtIsExpression, argIdx: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val retType = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(retType)

    val callNode =
      operatorCallNode(Operators.is, expr.getText, None, line(expr), column(expr))
        .argumentIndex(argIdx)
    val args =
      astsForExpression(expr.getLeftHandSide, 1, 1) ++ Seq(astForTypeReference(expr.getTypeReference, 2))
    callAst(callNode, args.toList)
  }

  def astForBinaryExprWithTypeRHS(expr: KtBinaryExpressionWithTypeRHS, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val args =
      astsForExpression(expr.getLeft, 1, 1) ++ Seq(astForTypeReference(expr.getRight, 2))
    val callNode =
      operatorCallNode(Operators.cast, expr.getText, None, line(expr), column(expr))
        .argumentIndex(argIdx)
    callAst(callNode, args.toList)
  }

  private def astForTypeReference(expr: KtTypeReference, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = typeInfoProvider.typeFullName(expr, TypeConstants.any)
    registerType(typeFullName)

    val node =
      typeRefNode(expr.getText, typeFullName, line(expr), column(expr))
        .argumentIndex(argIdx)
    Ast(node)
  }

  @tailrec
  private def astsForExpression(expr: KtExpression, order: Int, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Seq[Ast] = {
    expr match {
      case typedExpr: KtAnnotatedExpression         => astsForExpression(typedExpr.getBaseExpression, order, argIdx)
      case typedExpr: KtArrayAccessExpression       => Seq(astForArrayAccess(typedExpr, argIdx))
      case typedExpr: KtBinaryExpression            => Seq(astForBinaryExpr(typedExpr, argIdx))
      case typedExpr: KtBlockExpression             => List(astForBlock(typedExpr, argIdx))
      case typedExpr: KtBinaryExpressionWithTypeRHS => Seq(astForBinaryExprWithTypeRHS(typedExpr, argIdx))
      case typedExpr: KtBreakExpression             => Seq(astForBreak(typedExpr))
      case typedExpr: KtCallExpression =>
        val isCtorCall = typeInfoProvider.isConstructorCall(typedExpr)
        if (isCtorCall.getOrElse(false)) {
          Seq(astForCtorCall(typedExpr, argIdx))
        } else {
          Seq(astForCall(typedExpr, argIdx))
        }
      case typedExpr: KtConstantExpression       => Seq(astForLiteral(typedExpr, argIdx))
      case typedExpr: KtClass                    => astsForClassOrObject(typedExpr)
      case typedExpr: KtClassLiteralExpression   => Seq(astForClassLiteral(typedExpr, argIdx))
      case typedExpr: KtSafeQualifiedExpression  => Seq(astForQualifiedExpression(typedExpr, argIdx))
      case typedExpr: KtContinueExpression       => Seq(astForContinue(typedExpr))
      case typedExpr: KtDestructuringDeclaration => astsForDestructuringDeclaration(typedExpr)
      case typedExpr: KtDotQualifiedExpression   => Seq(astForQualifiedExpression(typedExpr, argIdx))
      case typedExpr: KtDoWhileExpression        => Seq(astForDoWhile(typedExpr))
      case typedExpr: KtForExpression            => Seq(astForFor(typedExpr))
      case typedExpr: KtIfExpression             => Seq(astForIf(typedExpr, argIdx))
      case typedExpr: KtIsExpression             => Seq(astForIsExpression(typedExpr, argIdx))
      case typedExpr: KtLabeledExpression        => astsForExpression(typedExpr.getBaseExpression, order, argIdx)
      case typedExpr: KtLambdaExpression         => Seq(astForLambda(typedExpr, argIdx))
      case typedExpr: KtNameReferenceExpression if typedExpr.getReferencedNameElementType == KtTokens.IDENTIFIER =>
        Seq(astForNameReference(typedExpr, argIdx))
      // TODO: callable reference
      case _: KtNameReferenceExpression =>
        // TODO: handle this
        Seq()
      case typedExpr: KtObjectLiteralExpression       => Seq(astForUnknown(typedExpr, Some(argIdx)))
      case typedExpr: KtParenthesizedExpression       => astsForExpression(typedExpr.getExpression, order, argIdx)
      case typedExpr: KtPostfixExpression             => Seq(astForPostfixExpression(typedExpr, argIdx))
      case typedExpr: KtPrefixExpression              => Seq(astForPrefixExpression(typedExpr, argIdx))
      case typedExpr: KtProperty if typedExpr.isLocal => astsForProperty(typedExpr)
      case typedExpr: KtReturnExpression              => astsForReturnExpression(typedExpr)
      case typedExpr: KtStringTemplateExpression      => Seq(astForStringTemplate(typedExpr, argIdx))
      case typedExpr: KtSuperExpression               => Seq(astForSuperExpression(typedExpr, argIdx))
      case typedExpr: KtThisExpression                => Seq(astForThisExpression(typedExpr, argIdx))
      case typedExpr: KtThrowExpression               => Seq(astForUnknown(typedExpr, Some(argIdx)))
      case typedExpr: KtTryExpression                 => Seq(astForTry(typedExpr, argIdx))
      case typedExpr: KtWhenExpression                => Seq(astForWhen(typedExpr, argIdx))
      case typedExpr: KtWhileExpression               => Seq(astForWhile(typedExpr))
      case typedExpr: KtNamedFunction =>
        logger.debug(
          s"Creating empty AST node for unknown expression `${typedExpr.getClass}` with text `${typedExpr.getText}`."
        )
        Seq(astForUnknown(typedExpr, Some(argIdx)))
      case null =>
        logger.trace("Received null expression! Skipping...")
        Seq()
      // TODO: handle `KtCallableReferenceExpression` like `this::baseTerrain`
      case unknownExpr =>
        logger.debug(
          s"Creating empty AST node for unknown expression `${unknownExpr.getClass}` with text `${unknownExpr.getText}`."
        )
        Seq(astForUnknown(unknownExpr, Some(argIdx)))
    }
  }

  def astForSuperExpression(expr: KtSuperExpression, argIdx: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val node =
      identifierNode(expr.getText, typeFullName, line(expr), column(expr))
        .argumentIndex(argIdx)
    scope.lookupVariable(expr.getText) match {
      case Some(n) => Ast(node).withRefEdge(node, n)
      case None    => Ast(node)
    }
  }

  def astForThisExpression(expr: KtThisExpression, argIdx: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val node =
      identifierNode(expr.getText, typeFullName, line(expr), column(expr))
        .argumentIndex(argIdx)
    scope.lookupVariable(expr.getText) match {
      case Some(n) => Ast(node).withRefEdge(node, n)
      case None    => Ast(node)
    }
  }

  def astForClassLiteral(expr: KtClassLiteralExpression, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
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
    Ast(_callNode)
  }

  def astForLambda(expr: KtLambdaExpression, argIdx: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {

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
      )

    val captured = scope.pushClosureScope(lambdaNode)
    val closureBindingEntriesForCaptured =
      captured
        .collect {
          case node: NewMethodParameterIn => node
          case node: NewLocal             => node
          case node: NewMember            => node
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

    val parameters =
      typeInfoProvider.implicitParameterName(expr) match {
        case Some(implicitParamName) =>
          val node = methodParameterNode(implicitParamName, TypeConstants.any)
          scope.addToScope(implicitParamName, node)
          Seq(Ast(node))
        case None =>
          withIndex(expr.getValueParameters.asScala.toSeq) { (p, order) =>
            astForParameter(p, order)
          }
      }
    val lastOrder = parameters.size + 2

    val bodyAst =
      expr.getBodyExpression match {
        case blockExpr if blockExpr != null => astForBlock(blockExpr, lastOrder, false, localsForCaptured)
        case _ =>
          val blockNode = NewBlock()
          Ast(blockNode)
      }

    val returnTypeFullName = typeInfoProvider.returnTypeFullName(expr)
    registerType(returnTypeFullName)

    val lambdaTypeDeclFullName = fullNameWithSig._1.split(":").head
    val methodRef =
      methodRefNode(expr.getText, fullNameWithSig._1, lambdaTypeDeclFullName, line(expr), column(expr))
        .argumentIndex(argIdx)
    val returnNode =
      methodReturnNode(Some(line(expr)), Some(column(expr)), returnTypeFullName)

    val lambdaMethodAst =
      Ast(lambdaNode)
        .withChildren(parameters ++ List(bodyAst, Ast(returnNode), Ast(lambdaModifierNode)))

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
    registerType(lambdaTypeDeclInheritsFromTypeFullName)

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
    closureBindingDefs
      .foreach(closureBindingDefQueue.prepend(_))
    lambdaBindingInfoQueue.prepend(bindingInfo)
    lambdaAstQueue.prepend(lambdaMethodAst)
    methodRefAst
  }

  def astForArrayAccess(expr: KtArrayAccessExpression, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val identifierElem = expr.getArrayExpression
    val typeFullName   = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val identifier =
      identifierNode(identifierElem.getText, typeFullName, line(identifierElem), column(identifierElem))
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
        .argumentIndex(argIdx)
    callAst(callNode, List(identifierAst) ++ astsForIndexExpr)
  }

  def astForPostfixExpression(expr: KtPostfixExpression, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
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
        .getOrElse(Ast())
    ).filterNot(_.root == null)
    val callNode =
      operatorCallNode(operatorType, expr.getText, Some(typeFullName), line(expr), column(expr))
        .argumentIndex(argIdx)
    callAst(callNode, args)
  }

  def astForPrefixExpression(expr: KtPrefixExpression, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
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
        .getOrElse(Ast())
    ).filterNot(_.root == null)
    val callNode =
      operatorCallNode(operatorType, expr.getText, Some(typeFullName), line(expr), column(expr))
        .argumentIndex(argIdx)
    callAst(callNode, args)
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
  def astsForDestructuringDeclarationWithNonCtorCallRHS(
    expr: KtDestructuringDeclaration
  )(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    val initExpr = expr.getInitializer
    val localsForEntries =
      nonUnderscoreEntries(expr).zipWithIndex.map { entryWithIdx =>
        val entry = entryWithIdx._1

        val typeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
        registerType(typeFullName)

        val node = localNode(entry.getName, typeFullName, None, line(entry), column(entry))
        Ast(node)
      }

    val callRhsTypeFullName = typeInfoProvider.expressionType(initExpr, TypeConstants.cpgUnresolved)
    registerType(callRhsTypeFullName)

    val tmpName         = Constants.tmpLocalPrefix + tmpKeyPool.next
    val localForTmpNode = localNode(tmpName, callRhsTypeFullName)
    val localForTmpAst  = Ast(localForTmpNode)

    val assignmentRhsAst = astsForExpression(initExpr, 2, 2).head

    val assignmentLhsNode =
      identifierNode(tmpName, callRhsTypeFullName, line(expr), column(expr))
    val assignmentLhsAst =
      Ast(assignmentLhsNode)
        .withRefEdge(assignmentLhsNode, localForTmpNode)

    val assignmentNode =
      operatorCallNode(Operators.assignment, tmpName + " = " + initExpr.getText, None)
    val assignmentAst = callAst(assignmentNode, List(assignmentLhsAst, assignmentRhsAst))
    val returnType    = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(returnType)

    val assignmentsForEntries =
      nonUnderscoreEntries(expr).zipWithIndex.map { entryWithIdx =>
        val entry             = entryWithIdx._1
        val entryTypeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
        registerType(entryTypeFullName)

        val assignmentLHSNode =
          identifierNode(entry.getText, entryTypeFullName, line(entry), column(entry))
            .argumentIndex(1)
        val relevantLocal = localsForEntries(entryWithIdx._2).root.get
        val assignmentLHSAst =
          Ast(assignmentLHSNode)
            .withRefEdge(assignmentLHSNode, relevantLocal)

        val componentNIdentifierNode =
          identifierNode(localForTmpNode.name, callRhsTypeFullName, line(entry), column(entry))
            .argumentIndex(0)

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
            .argumentIndex(2)

        val componentNIdentifierAst =
          Ast(componentNIdentifierNode)
            .withRefEdge(componentNIdentifierNode, localForTmpNode)
        val componentNAst =
          Ast(componentNCallNode)
            .withChild(componentNIdentifierAst)
            .withArgEdge(componentNCallNode, componentNIdentifierNode)
            .withReceiverEdge(componentNCallNode, componentNIdentifierNode)

        val assignmentCallNode =
          operatorCallNode(
            Operators.assignment,
            entry.getText + " = " + componentNCallCode,
            None,
            line(entry),
            column(entry)
          )
        callAst(assignmentCallNode, List(assignmentLHSAst, componentNAst))
      }

    localsForEntries ++ Seq(localForTmpAst) ++
      Seq(assignmentAst) ++ assignmentsForEntries
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
  def astsForDestructuringDeclarationWithCtorRHS(
    expr: KtDestructuringDeclaration
  )(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
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
        val entry = entryWithIdx._1

        val typeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
        registerType(typeFullName)

        val node = localNode(entry.getName, typeFullName, None, line(entry), column(entry))
        Ast(node)
      }

    val ctorTypeFullName = typeInfoProvider.expressionType(ctorCall, TypeConstants.cpgUnresolved)
    registerType(ctorTypeFullName)

    val tmpName         = Constants.tmpLocalPrefix + tmpKeyPool.next
    val localForTmpNode = localNode(tmpName, ctorTypeFullName)
    val localForTmpAst  = Ast(localForTmpNode)

    val assignmentRhsNode =
      operatorCallNode(Operators.alloc, Constants.alloc, Some(ctorTypeFullName), line(expr), column(expr))
    val assignmentLhsNode =
      identifierNode(tmpName, ctorTypeFullName, line(expr), column(expr))

    val assignmentLhsAst =
      Ast(assignmentLhsNode)
        .withRefEdge(assignmentLhsNode, localForTmpNode)

    val assignmentNode = operatorCallNode(Operators.assignment, tmpName + " = " + Constants.alloc, None)
    val assignmentAst  = callAst(assignmentNode, List(assignmentLhsAst, Ast(assignmentRhsNode)))

    val initReceiverNode =
      identifierNode(tmpName, ctorTypeFullName, line(expr), column(expr))
        .argumentIndex(0)
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
    val initCallAst =
      Ast(initCallNode)
        .withChild(initReceiverAst)
        .withChildren(argAsts)
        .withArgEdges(initCallNode, Seq(initReceiverNode) ++ argAsts.flatMap(_.root))

    val assignmentsForEntries =
      nonUnderscoreEntries(expr).zipWithIndex.map { entryWithIdx =>
        val entry             = entryWithIdx._1
        val entryTypeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
        registerType(entryTypeFullName)

        val assignmentLHSNode =
          identifierNode(entry.getText, entryTypeFullName, line(entry), column(entry))
            .argumentIndex(1)
        val relevantLocal = localsForEntries(entryWithIdx._2).root.get
        val assignmentLHSAst =
          Ast(assignmentLHSNode)
            .withRefEdge(assignmentLHSNode, relevantLocal)

        val componentNIdentifierNode =
          identifierNode(localForTmpNode.name, ctorTypeFullName, line(entry), column(entry))
            .argumentIndex(0)

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
            .argumentIndex(2)

        val componentNIdentifierAst =
          Ast(componentNIdentifierNode)
            .withRefEdge(componentNIdentifierNode, localForTmpNode)
        val componentNAst =
          Ast(componentNCallNode)
            .withChild(componentNIdentifierAst)
            .withArgEdge(componentNCallNode, componentNIdentifierNode)
            .withReceiverEdge(componentNCallNode, componentNIdentifierNode)

        val assignmentCallNode =
          operatorCallNode(
            Operators.assignment,
            entry.getText + " = " + componentNCallCode,
            None,
            line(entry),
            column(entry)
          )
        callAst(assignmentCallNode, List(assignmentLHSAst, componentNAst))
      }

    localsForEntries ++ Seq(localForTmpAst) ++
      Seq(assignmentAst) ++ Seq(initCallAst) ++ assignmentsForEntries
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
    expr: KtDestructuringDeclaration
  )(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
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
          val entry = entryWithIdx._1

          val typeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
          registerType(typeFullName)

          val node = localNode(entry.getName, typeFullName, None, line(entry), column(entry))
          Ast(node)
        }

    val assignmentsForEntries =
      nonUnderscoreEntries(expr).zipWithIndex
        .map { entryWithIdx =>
          val entry             = entryWithIdx._1
          val entryTypeFullName = typeInfoProvider.typeFullName(entry, TypeConstants.any)
          registerType(entryTypeFullName)

          val assignmentLHSNode =
            identifierNode(entry.getText, entryTypeFullName, line(entry), column(entry))
              .argumentIndex(1)
          val relevantLocal = localsForEntries(entryWithIdx._2).root.get
          val assignmentLHSAst =
            Ast(assignmentLHSNode)
              .withRefEdge(assignmentLHSNode, relevantLocal)

          val componentNIdentifierTFN = typeInfoProvider.typeFullName(typedInit.get, TypeConstants.any)
          registerType(componentNIdentifierTFN)

          val componentNIdentifierNode =
            identifierNode(destructuringRHS.getText, componentNIdentifierTFN, line(entry), column(entry))
              .argumentIndex(0)

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

          val assignmentCallNode =
            operatorCallNode(
              Operators.assignment,
              entry.getText + " = " + componentNCallCode,
              None,
              line(entry),
              column(entry)
            )
          callAst(assignmentCallNode, List(assignmentLHSAst, componentNAst))
        }

    localsForEntries ++ assignmentsForEntries
  }

  def astsForDestructuringDeclaration(
    expr: KtDestructuringDeclaration
  )(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
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
      astsForDestructuringDeclarationWithCtorRHS(expr)
    } else if (hasNonRefExprRHS) {
      astsForDestructuringDeclarationWithNonCtorCallRHS(expr)
    } else {
      astsForDestructuringDeclarationWithVarRHS(expr)
    }
  }

  def astForUnknown(expr: KtExpression, argIdx: Option[Int]): Ast = {
    val code = if (expr != null) { expr.getText }
    else { null } // TODO: add test case to check if this is necessary
    val node =
      argIdx match {
        case Some(value) =>
          unknownNode(code, Constants.parserTypeName, line(expr), column(expr))
            .argumentIndex(value)
        case _ =>
          unknownNode(code, Constants.parserTypeName, line(expr), column(expr))
      }
    Ast(node)
  }

  def astForStringTemplate(expr: KtStringTemplateExpression, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    if (expr.hasInterpolation) {
      val callNode =
        operatorCallNode(Operators.formatString, expr.getText, Some(typeFullName), line(expr), column(expr))
          .argumentIndex(argIdx)
      val args =
        expr.getEntries
          .filter(_.getExpression != null)
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
              val valueArgs = astsForExpression(entry.getExpression, idx + 1, idx + 1)
              val call      = callAst(valueCallNode, valueArgs.toList)
              Seq(call)
            } else {
              Seq()
            }
          }
      callAst(callNode, args.toIndexedSeq.toList)
    } else {
      val node =
        literalNode(expr.getText, typeFullName, line(expr), column(expr))
          .argumentIndex(argIdx)
      Ast(node)
    }
  }

  // TODO: clean up this whole fn
  def astForQualifiedExpression(expr: KtQualifiedExpression, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {

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
    val receiverAst        = astsForExpression(expr.getReceiverExpression, orderForReceiver, argIdxForReceiver).head
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
          val argIdx = if (isStaticCall) 1 else 2
          val node =
            fieldIdentifierNode(typedExpr.getText)
              .argumentIndex(argIdx)
          List(Ast(node))
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
        .argumentIndex(argIdx)
    val root         = Ast(_callNode)
    val receiverNode = receiverAst.root.get
    val finalAst = {
      if (isExtensionCall || isCallToSuper) {
        root
          .withChild(receiverAst)
          .withArgEdge(_callNode, receiverNode)
          .withChildren(argAsts)
          .withArgEdges(_callNode, argAsts.map(_.root.get))
      } else if (noAstForReceiver) {
        root
          .withChild(receiverAst)
          .withChildren(argAsts)
          .withArgEdges(_callNode, argAsts.map(_.root.get))
      } else {
        val ast =
          root
            .withChild(receiverAst)
            .withArgEdge(_callNode, receiverNode)
            .withChildren(argAsts)
            .withArgEdges(_callNode, argAsts.map(_.root.get))
        if (argAsts.size == 1 && argAsts.head.root.get.isInstanceOf[NewMethodRef]) {
          ast
            .withReceiverEdge(_callNode, argAsts.head.root.get)
        } else {
          ast
            .withReceiverEdge(_callNode, receiverNode)
        }
      }
    }
    finalAst
  }

  def astForBreak(expr: KtBreakExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val node = controlStructureNode(expr.getText, ControlStructureTypes.BREAK, line(expr), column(expr))
    Ast(node)
  }

  def astForContinue(expr: KtContinueExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val node = controlStructureNode(expr.getText, ControlStructureTypes.CONTINUE, line(expr), column(expr))
    Ast(node)
  }

  private def astForTryAsStatement(expr: KtTryExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val tryNode =
      controlStructureNode(expr.getText, ControlStructureTypes.TRY, line(expr), column(expr))
    val tryAstOption = astsForExpression(expr.getTryBlock, 1, 1).headOption
      .getOrElse(Ast())
    val tryAst =
      Ast(tryNode)
        .withChild(tryAstOption)

    val clauseAsts =
      withIndex(expr.getCatchClauses.asScala.toSeq) { (entry, order) =>
        astsForExpression(entry.getCatchBody, order + 1, order + 1)
      }.flatten

    val finallyAsts =
      if (expr.getFinallyBlock == null) {
        Seq()
      } else {
        val numClauses = clauseAsts.size
        astsForExpression(expr.getFinallyBlock.getFinalExpression, numClauses + 2, numClauses + 2)
      }

    val tryWithClausesAst =
      tryAst
        .withChildren(clauseAsts)
    val finalAst =
      if (finallyAsts.nonEmpty) {
        tryWithClausesAst
          .withChildren(finallyAsts)
      } else {
        tryWithClausesAst
      }
    finalAst
  }

  private def astForTryAsExpression(expr: KtTryExpression, argumentIndex: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = typeInfoProvider.expressionType(expr.getTryBlock.getStatements.asScala.last, TypeConstants.any)
    registerType(typeFullName)

    val callNode =
      operatorCallNode(Operators.tryCatch, expr.getText, Some(typeFullName), line(expr), column(expr))
        .argumentIndex(argumentIndex)

    val tryBlockAst = astsForExpression(expr.getTryBlock, 1, 1).headOption
      .getOrElse(Ast())

    val clauseAsts =
      withIndex(expr.getCatchClauses.asScala.toSeq) { (entry, order) =>
        astsForExpression(entry.getCatchBody, order + 1, order + 1)
      }.flatten

    callAst(callNode, List(tryBlockAst) ++ clauseAsts)
  }

  // TODO: handle parameters passed to the clauses
  def astForTry(expr: KtTryExpression, argumentIndex: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    if (KtPsiUtil.isStatement(expr)) {
      astForTryAsStatement(expr)
    } else {
      astForTryAsExpression(expr, argumentIndex)
    }
  }

  def astForWhile(expr: KtWhileExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val whileNode = controlStructureNode(expr.getText, ControlStructureTypes.WHILE, line(expr), column(expr))
    val conditionAst = astsForExpression(expr.getCondition, 1, 1).headOption
      .getOrElse(Ast())
    val stmtAsts = astsForExpression(expr.getBody, 2, 2)
    val ast =
      Ast(whileNode)
        .withChild(conditionAst)
        .withChildren(stmtAsts)
    conditionAst.root match {
      case Some(node) =>
        ast.withConditionEdge(whileNode, node)
      case None =>
        ast
    }
  }

  def astForDoWhile(expr: KtDoWhileExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val doNode   = controlStructureNode(expr.getText, ControlStructureTypes.DO, line(expr), column(expr))
    val stmtAsts = astsForExpression(expr.getBody, 1, 1)
    val conditionAst = astsForExpression(expr.getCondition, 2, 2).headOption
      .getOrElse(Ast())
    val ast =
      Ast(doNode)
        .withChildren(stmtAsts ++ List(conditionAst))
    conditionAst.root match {
      case Some(node) =>
        ast.withConditionEdge(doNode, node)
      case None =>
        ast
    }
  }

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
  private def astForForWithSimpleVarLHS(expr: KtForExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val loopRangeText = expr.getLoopRange.getText
    val iteratorName  = Constants.iteratorPrefix + iteratorKeyPool.next()
    val iteratorLocal =
      localNode(iteratorName, TypeConstants.any)
    val iteratorAssignmentLhs =
      identifierNode(iteratorName, TypeConstants.any)
        .argumentIndex(1)
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
    val iteratorAssignmentAst =
      Ast(iteratorAssignment)
        .withChild(Ast(iteratorAssignmentLhs))
        .withArgEdge(iteratorAssignment, iteratorAssignmentLhs)
        .withChild(iteratorAssignmentRhsAst)
        .withArgEdge(iteratorAssignment, iteratorAssignmentRhs)

    val controlStructure =
      controlStructureNode(expr.getText, ControlStructureTypes.WHILE, line(expr), column(expr))

    val conditionIdentifier =
      identifierNode(loopRangeText, loopRangeExprTypeFullName)
        .argumentIndex(0)

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
      ).argumentIndex(0)
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
    scope.addToScope(loopParameterName, loopParameterLocal) // TODO: remove from scope after the block

    val loopParameterIdentifier =
      identifierNode(loopParameterName, TypeConstants.any)
        .argumentIndex(1)
    val loopParameterAst =
      Ast(loopParameterLocal)
        .withRefEdge(loopParameterIdentifier, loopParameterLocal)

    val iteratorNextIdentifier =
      identifierNode(iteratorName, TypeConstants.any)
        .argumentIndex(0)
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
      ).argumentIndex(2)
    val iteratorNextCallAst =
      Ast(iteratorNextCall)
        .withChild(iteratorNextIdentifierAst)
        .withArgEdge(iteratorNextCall, iteratorNextIdentifier)
        .withReceiverEdge(iteratorNextCall, iteratorNextIdentifier)
    val loopParameterNextAssignment =
      operatorCallNode(Operators.assignment, loopParameterName + " = " + iteratorNextCall.code, None)
    val loopParameterNextAssignmentAst =
      Ast(loopParameterNextAssignment)
        .withChild(Ast(loopParameterIdentifier))
        .withArgEdge(loopParameterNextAssignment, loopParameterIdentifier)
        .withChild(iteratorNextCallAst)
        .withArgEdge(loopParameterNextAssignment, iteratorNextCall)

    val stmtAsts = astsForExpression(expr.getBody, 3, 3)
    val controlStructureBody =
      blockNode("", "")
    val controlStructureBodyAst =
      Ast(controlStructureBody)
        .withChild(loopParameterAst)
        .withChild(loopParameterNextAssignmentAst)
        .withChildren(stmtAsts)

    val controlStructureAst =
      Ast(controlStructure)
        .withChild(controlStructureConditionAst)
        .withChild(controlStructureBodyAst)
        .withConditionEdge(controlStructure, controlStructureCondition)
    val topLevelBlock = blockNode(Constants.codeForLoweredForBlock, "")
    Ast(topLevelBlock)
      .withChild(iteratorLocalAst)
      .withChild(iteratorAssignmentAst)
      .withChild(controlStructureAst)
  }

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
  private def astForForWithDestructuringLHS(expr: KtForExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val loopRangeText = expr.getLoopRange.getText
    val iteratorName  = Constants.iteratorPrefix + iteratorKeyPool.next()
    val localForIterator =
      localNode(iteratorName, TypeConstants.any)
    val iteratorAssignmentLhs =
      identifierNode(iteratorName, TypeConstants.any)
        .argumentIndex(1)
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
      ).argumentIndex(0)
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

    val tmpName = Constants.tmpLocalPrefix + tmpKeyPool.next
    val localForTmp =
      localNode(tmpName, TypeConstants.any)
    val localForTmpAst =
      Ast(localForTmp)

    val tmpIdentifier =
      identifierNode(tmpName, TypeConstants.any)
        .argumentIndex(1)
    val tmpIdentifierAst =
      Ast(tmpIdentifier)
        .withRefEdge(tmpIdentifier, localForTmp)

    val iteratorNextIdentifier =
      identifierNode(iteratorName, TypeConstants.any)
        .argumentIndex(0)
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
      ).argumentIndex(2)

    val iteratorNextCallAst =
      Ast(iteratorNextCall)
        .withChild(iteratorNextIdentifierAst)
        .withArgEdge(iteratorNextCall, iteratorNextIdentifier)
        .withReceiverEdge(iteratorNextCall, iteratorNextIdentifier)
    val tmpParameterNextAssignment =
      operatorCallNode(Operators.assignment, tmpName + " = " + iteratorNextCall.code)
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

        // TODO: rewrite this in a cleaner way
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
          ).argumentIndex(2)
        val tmpComponentNCallAst =
          Ast(tmpComponentNCall)
            .withChild(tmpForComponentNIdentifierAst)
            .withArgEdge(tmpComponentNCall, tmpForComponentNIdentifier)
            .withReceiverEdge(tmpComponentNCall, tmpForComponentNIdentifier)

        val componentNAssignment =
          operatorCallNode(Operators.assignment, entryIdentifier.code + " = " + tmpComponentNCall.code)
        callAst(componentNAssignment, List(entryIdentifierAst, tmpComponentNCallAst))
      }
    val orderAfterComponentNCalls = componentNCalls.map(_.root.get.asInstanceOf[NewCall].order).reverse.take(1).head + 1

    val stmtAsts = astsForExpression(expr.getBody, orderAfterComponentNCalls, orderAfterComponentNCalls)
    val controlStructureBody =
      blockNode("", "")
    val controlStructureBodyAst =
      Ast(controlStructureBody)
        .withChildren(
          localsForDestructuringVars ++
            List(localForTmpAst, tmpParameterNextAssignmentAst) ++
            componentNCalls ++
            stmtAsts
        )

    val controlStructureAst =
      Ast(controlStructure)
        .withChild(controlStructureConditionAst)
        .withChild(controlStructureBodyAst)
        .withConditionEdge(controlStructure, controlStructureCondition)
    val topLevelBlock = blockNode(Constants.codeForLoweredForBlock, "")
    Ast(topLevelBlock)
      .withChildren(List(iteratorLocalAst, iteratorAssignmentAst, controlStructureAst))
  }

  def astForFor(expr: KtForExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    if (expr.getDestructuringDeclaration != null) {
      astForForWithDestructuringLHS(expr)
    } else {
      astForForWithSimpleVarLHS(expr)
    }
  }

  def astForWhen(expr: KtWhenExpression, argIdx: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val astForSubject =
      astsForExpression(expr.getSubjectExpression, 1, 1).headOption
        .getOrElse(Ast())

    val astsForEntries =
      withIndex(expr.getEntries.asScala.toSeq) { (e, order) =>
        astsForWhenEntry(e, order)
      }.flatten

    val switchBlockNode =
      blockNode(expr.getEntries.asScala.map(_.getText).mkString("\n"), TypeConstants.any, line(expr), column(expr))
    val astForBlock =
      Ast(switchBlockNode)
        .withChildren(astsForEntries)

    val codeForSwitch =
      if (expr.getSubjectExpression != null) {
        Constants.when + s"(${expr.getSubjectExpression.getText})"
      } else {
        Constants.when
      }
    val switchNode =
      controlStructureNode(codeForSwitch, ControlStructureTypes.SWITCH, line(expr), column(expr))
        .argumentIndex(argIdx)
    val ast =
      Ast(switchNode)
        .withChild(astForSubject)
        .withChild(astForBlock)
    astForSubject.root match {
      case Some(root) =>
        ast.withConditionEdge(switchNode, root)
      case None =>
        ast
    }
  }

  def astsForWhenEntry(entry: KtWhenEntry, argIdx: Int)(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    // TODO: get all conditions with entry.getConditions()
    val name =
      Option(entry.getElseKeyword)
        .map { _ => Constants.caseNodePrefix + argIdx.toString }
        .getOrElse(Constants.defaultCaseNode)
    val jumpNode =
      jumpTargetNode(entry.getText, name, Constants.caseNodeParserTypeName, line(entry), column(entry))
        .argumentIndex(argIdx)
    val exprNode = astsForExpression(entry.getExpression, argIdx + 1, argIdx + 1).headOption
      .getOrElse(Ast())
    Seq(Ast(jumpNode)) ++ Seq(exprNode)
  }

  def astForIf(expr: KtIfExpression, argIdx: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val isChildOfControlStructureBody = expr.getParent.isInstanceOf[KtContainerNodeForControlStructureBody]
    if (KtPsiUtil.isStatement(expr) && !isChildOfControlStructureBody) {
      astForIfAsControlStructure(expr)
    } else {
      astForIfAsExpression(expr, argIdx)
    }
  }

  def astForIfAsControlStructure(expr: KtIfExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val ifNode =
      controlStructureNode(expr.getText, ControlStructureTypes.IF, line(expr), column(expr))
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
        .withChild(conditionAst.head)
        .withChildren(thenAsts ++ elseAsts)
    val withCondition = conditionAst.head.root match {
      case Some(r) =>
        ast.withConditionEdge(ifNode, r)
      case None =>
        ast
    }
    withCondition
  }

  def astForIfAsExpression(expr: KtIfExpression, argIdx: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val retType = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(retType)

    val callNode =
      operatorCallNode(Operators.conditional, expr.getText, Some(retType), line(expr), column(expr))
        .argumentIndex(argIdx)
    val conditionAsts = astsForExpression(expr.getCondition, 1, 1)
    val thenAsts      = astsForExpression(expr.getThen, 2, 2)
    val elseAsts      = astsForExpression(expr.getElse, 3, 3)

    val childAsts = conditionAsts ++ thenAsts ++ elseAsts
    if (conditionAsts.nonEmpty && conditionAsts.head.root != null) {
      callAst(callNode, childAsts.toList)
    } else {
      logger.warn("Parsing failed for expr `" + expr.getName + "` in file `" + fileWithMeta.filename + "`.")
      logger.debug(" > expr.text `" + expr.getText + "`")

      val _unknownNode =
        unknownNode(expr.getText, Constants.parserTypeName, line(expr), column(expr))
          .argumentIndex(argIdx)
      Ast(_unknownNode)
    }
  }

  private def astForCtorCall(expr: KtCallExpression, argIdx: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.cpgUnresolved)
    registerType(typeFullName)

    val tmpBlockNode =
      blockNode("", typeFullName)
        .argumentIndex(argIdx)
    val tmpName = Constants.tmpLocalPrefix + tmpKeyPool.next
    val tmpLocalNode =
      localNode(tmpName, typeFullName)
    val assignmentRhsNode =
      operatorCallNode(Operators.alloc, Constants.alloc, Some(typeFullName), line(expr), column(expr))
    val assignmentLhsNode =
      identifierNode(tmpName, typeFullName, line(expr), column(expr))
    val assignmentNode =
      operatorCallNode(Operators.assignment, Operators.assignment)
    val assignmentAst = callAst(assignmentNode, List(assignmentLhsNode, assignmentRhsNode).map(Ast(_)))
    val initReceiverNode =
      identifierNode(tmpName, typeFullName, line(expr), column(expr))
        .argumentIndex(0)
    val initReceiverAst = Ast(initReceiverNode)

    val argAsts =
      withIndex(expr.getValueArguments.asScala.toSeq) { case (arg, argOrder) =>
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
        .argumentIndex(2)
    val initCallAst =
      Ast(initCallNode)
        .withChild(initReceiverAst)
        .withChildren(argAsts)
        .withArgEdges(initCallNode, Seq(initReceiverNode) ++ argAsts.flatMap(_.root))

    val lastIdentifier =
      identifierNode(tmpName, typeFullName, line(expr), column(expr))
    val lastIdentifierAst = Ast(lastIdentifier)
    val tmpLocalAst =
      Ast(tmpLocalNode)
        .withRefEdge(assignmentLhsNode, tmpLocalNode)
        .withRefEdge(initReceiverNode, tmpLocalNode)
        .withRefEdge(lastIdentifier, tmpLocalNode)
    Ast(tmpBlockNode)
      .withChild(tmpLocalAst)
      .withChild(assignmentAst)
      .withChild(initCallAst)
      .withChild(lastIdentifierAst)
  }

  private def astsForProperty(expr: KtProperty)(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    val explicitTypeName =
      Option(expr.getTypeReference)
        .map(_.getText)
        .getOrElse(TypeConstants.any)
    val elem         = expr.getIdentifyingElement
    val typeFullName = typeInfoProvider.propertyType(expr, explicitTypeName)
    registerType(typeFullName)

    val identifier = identifierNode(elem.getText, typeFullName, line(elem), column(elem))
    val assignmentNode =
      operatorCallNode(Operators.assignment, expr.getText, None, line(expr), column(expr))
    val node =
      localNode(expr.getName, typeFullName, None, line(expr), column(expr))
    scope.addToScope(expr.getName, node)

    val hasRHSCtorCall =
      expr.getDelegateExpressionOrInitializer match {
        case typed: KtCallExpression =>
          typeInfoProvider.isConstructorCall(typed).getOrElse(false)
        case _ => false
      }
    val rhsAsts = if (hasRHSCtorCall) {
      Seq(astForCtorCall(expr.getDelegateExpressionOrInitializer.asInstanceOf[KtCallExpression], 2))
    } else {
      astsForExpression(expr.getDelegateExpressionOrInitializer, 2, 2)
    }
    val call = callAst(assignmentNode, List(Ast(identifier)) ++ rhsAsts)

    Seq(call) ++
      Seq(Ast(node).withRefEdge(identifier, node))
  }

  def astForNameReference(expr: KtNameReferenceExpression, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val isRefToClass = typeInfoProvider.isReferenceToClass(expr)
    if (isRefToClass) {
      astForNameReferenceToType(expr, argIdx)
    } else {
      typeInfoProvider.isReferencingMember(expr) match {
        case true  => astForNameReferenceToMember(expr, argIdx)
        case false => astForNonSpecialNameReference(expr, argIdx)
      }
    }
  }

  private def astForNameReferenceToType(expr: KtNameReferenceExpression, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = typeInfoProvider.typeFullName(expr, TypeConstants.any)
    registerType(typeFullName)

    val referencesCompanionObject = typeInfoProvider.isRefToCompanionObject(expr)
    if (referencesCompanionObject) {
      val argAsts = List(
        // TODO: change this to a TYPE_REF node as soon as the closed source data-flow engine supports it
        identifierNode(expr.getIdentifier.getText, typeFullName, line(expr), column(expr)),
        fieldIdentifierNode(Constants.companionObjectMemberName, line(expr), column(expr))
      ).map(Ast(_))

      val callNode =
        operatorCallNode(Operators.fieldAccess, expr.getText, Some(typeFullName), line(expr), column(expr))
          .argumentIndex(argIdx)
      callAst(callNode, argAsts)
    } else {
      val node =
        typeRefNode(expr.getIdentifier.getText, typeFullName, line(expr), column(expr))
          .argumentIndex(argIdx)
      Ast(node)
    }
  }

  private def astForNameReferenceToMember(expr: KtNameReferenceExpression, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
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
        .argumentIndex(argIdx)

    val referenceTargetTypeFullName = typeInfoProvider.referenceTargetTypeFullName(expr, TypeConstants.any)
    registerType(referenceTargetTypeFullName)

    val thisNode =
      identifierNode(Constants.this_, referenceTargetTypeFullName, line(expr), column(expr))
    val thisAst =
      scope.lookupVariable(Constants.this_) match {
        case Some(n) => Ast(thisNode).withRefEdge(thisNode, n)
        case None    => Ast(thisNode)
      }
    val _fieldIdentifierNode = fieldIdentifierNode(expr.getReferencedName, line(expr), column(expr))
    callAst(callNode, List(thisAst, Ast(_fieldIdentifierNode)))
  }

  private def astForNonSpecialNameReference(expr: KtNameReferenceExpression, argIdx: Int)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = typeInfoProvider.typeFullName(expr, TypeConstants.any)
    registerType(typeFullName)

    val name = expr.getIdentifier.getText
    val node =
      identifierNode(name, typeFullName, line(expr), column(expr))
        .argumentIndex(argIdx)

    scope.lookupVariable(name) match {
      case Some(n) => Ast(node).withRefEdge(node, n)
      case None    => Ast(node)
    }
  }

  def astForLiteral(expr: KtConstantExpression, argIdx: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val typeFullName = typeInfoProvider.expressionType(expr, TypeConstants.any)
    registerType(typeFullName)

    val node =
      literalNode(expr.getText, typeFullName, line(expr), column(expr))
        .argumentIndex(argIdx)
    Ast(node)
  }

  def astForBinaryExpr(expr: KtBinaryExpression, argIdx: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
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
    registerType(typeFullName)

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
    val args =
      astsForExpression(expr.getLeft, 1, 1) ++ astsForExpression(expr.getRight, 2, 2)
    callAst(_callNode, args.toList)
  }

  private def astForCall(expr: KtCallExpression, argIdx: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
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

    val nameToClass = expr.getContainingKtFile.getDeclarations.asScala.toList
      .collect { case d: KtClass => d }
      .map { klass =>
        klass.getName -> klass
      }
      .toMap

    val imports = expr.getContainingKtFile.getImportList.getImports.asScala.toList
    val importedNames = imports.map { imp =>
      val importedName =
        Option(imp.getImportedName)
          .map(_.toString)
          .getOrElse(Constants.wildcardImportName)
      importedName -> imp
    }.toMap

    val methodFqName = {
      if (importedNames.isDefinedAt(referencedName)) {
        importedNames(referencedName).getImportedFqName.toString
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

    val node =
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
        .argumentIndex(argIdx)
    callAst(node, argAsts.toList)
  }

  private def astForMember(decl: KtDeclaration)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val name = Option(decl.getName).getOrElse(TypeConstants.any)

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

    val node = memberNode(name, typeFullName, line(decl), column(decl))
    scope.addToScope(name, node)
    Ast(node)
  }

  def astForParameter(param: KtParameter, order: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val name =
      Option(param.getDestructuringDeclaration)
        .map { _ => Constants.paramNameLambdaDestructureDecl }
        .getOrElse(param.getName)

    val explicitTypeName =
      Option(param.getTypeReference)
        .map(_.getText)
        .getOrElse(TypeConstants.any)
    val typeFullName = typeInfoProvider.parameterType(param, explicitTypeName)
    registerType(typeFullName)

    val parameterNode =
      methodParameterNode(name, typeFullName, line(param), column(param))
        .order(order)
    scope.addToScope(name, parameterNode)
    Ast(parameterNode)
  }
}
