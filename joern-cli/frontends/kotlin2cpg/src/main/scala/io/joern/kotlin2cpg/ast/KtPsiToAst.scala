package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.passes.{AstCreator, ClosureBindingDef, BindingInfo}

import io.joern.kotlin2cpg.ast.Nodes._
import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.KtFileWithMeta
import io.joern.kotlin2cpg.psi.PsiUtils._
import io.joern.kotlin2cpg.types.{CallKinds, TypeConstants, TypeInfoProvider}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.joern.x2cpg.{Ast}

import java.util.UUID.randomUUID
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.lexer.{KtToken, KtTokens}
import overflowdb.traversal.iterableToTraversal

import scala.jdk.CollectionConverters._

trait KtPsiToAst {
  this: AstCreator =>

  def astForFile(fileWithMeta: KtFileWithMeta)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val ktFile = fileWithMeta.f

    val importDirectives = ktFile.getImportList.getImports.asScala
    val importAsts       = importDirectives.toList.map(astForImportDirective)
    val namespaceBlocksForImports =
      for {
        node <- importAsts.flatMap(_.root.collectAll[NewImport])
        name = getName(node)
      } yield Ast(namespaceBlockNode(name, name, relativizedPath))

    val declarationsAsts = ktFile.getDeclarations.asScala.flatMap(astForDeclaration)
    val fileNode         = NewFile().name(fileWithMeta.relativizedPath)
    val lambdaTypeDecls =
      lambdaBindingInfoQueue.flatMap(_.edgeMeta.collect { case (node: NewTypeDecl, _, _) => Ast(node) })

    val namespaceBlockAst = astForPackageDeclaration(ktFile.getPackageFqName.toString)
      .withChildren(importAsts ++ declarationsAsts ++ lambdaAstQueue ++ lambdaTypeDecls)
    Ast(fileNode).withChildren(namespaceBlockAst :: namespaceBlocksForImports)
  }

  def astForImportDirective(directive: KtImportDirective): Ast = {
    val isWildcard = directive.getLastChild.getText == Constants.wildcardImportName || directive.getImportedName == null
    val node =
      NewImport()
        .isWildcard(isWildcard)
        .isExplicit(true)
        .importedEntity(directive.getImportPath.getPathStr)
        .code(s"${Constants.importKeyword} ${directive.getImportPath.getPathStr}")
        .lineNumber(line(directive))
        .columnNumber(column(directive))
    Ast(node)
  }

  def astForPackageDeclaration(packageName: String): Ast = {
    val node =
      if (packageName == Constants.root)
        namespaceBlockNode(
          NamespaceTraversal.globalNamespaceName,
          NamespaceTraversal.globalNamespaceName,
          relativizedPath
        )
      else {
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
    val node = typeDeclNode(
      typeAlias.getName,
      registerType(typeInfoProvider.fullName(typeAlias, TypeConstants.any)),
      relativizedPath,
      Seq(),
      Some(registerType(typeInfoProvider.aliasTypeFullName(typeAlias, TypeConstants.any))),
      line(typeAlias),
      column(typeAlias)
    )
    Ast(node)
  }

  def componentNMethodAsts(typeDecl: NewTypeDecl, parameters: Seq[KtParameter])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Seq[Ast] = {
    parameters.zipWithIndex.map { case (valueParam, idx) =>
      val componentIdx = idx + 1

      val typeFullName  = registerType(typeInfoProvider.typeFullName(valueParam, TypeConstants.any))
      val componentName = s"${Constants.componentNPrefix}$componentIdx"
      val signature     = s"$typeFullName()"
      val fullName      = s"${typeDecl.fullName}.$componentName:$signature"

      val thisParam       = methodParameterNode(Constants.this_, typeDecl.fullName).order(0)
      val thisIdentifier  = identifierNode(Constants.this_, typeDecl.fullName)
      val fieldIdentifier = fieldIdentifierNode(valueParam.getName, line(valueParam), column(valueParam))
      val fieldAccessCall =
        operatorCallNode(Operators.fieldAccess, s"${Constants.this_}.${valueParam.getName}", Some(typeFullName))
      val fieldAccessCallAst = callAst(fieldAccessCall, List(thisIdentifier, fieldIdentifier).map(Ast(_)))
      val methodBlockAst = blockAst(
        blockNode(fieldAccessCall.code, typeFullName),
        List(returnAst(returnNode(Constants.ret), List(fieldAccessCallAst)))
      )
      methodAst(
        methodNode(componentName, fullName, signature, relativizedPath),
        Seq(thisParam),
        methodBlockAst,
        methodReturnNode(None, None, typeFullName)
      )
    }
  }

  def secondaryCtorAsts(ctors: Seq[KtSecondaryConstructor], classFullName: String)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Seq[Ast] = {
    ctors.map { ctor =>
      val constructorParams     = ctor.getValueParameters.asScala.toList
      val defaultSignature      = typeInfoProvider.erasedSignature(constructorParams)
      val defaultFullName       = s"$classFullName.${TypeConstants.initPrefix}:$defaultSignature"
      val (fullName, signature) = typeInfoProvider.fullNameWithSignature(ctor, (defaultFullName, defaultSignature))
      val secondaryCtorMethodNode =
        methodNode(Constants.init, fullName, signature, relativizedPath, line(ctor), column(ctor))
      scope.pushNewScope(secondaryCtorMethodNode)

      val typeFullName  = registerType(typeInfoProvider.typeFullName(ctor, TypeConstants.any))
      val ctorThisParam = methodParameterNode(Constants.this_, classFullName).order(0)
      scope.addToScope(Constants.this_, ctorThisParam)

      val constructorParamsAsts = Seq(Ast(ctorThisParam)) ++
        withIndex(constructorParams) { (p, idx) => astForParameter(p, idx) }
      val ctorMethodBlockAst = Option(ctor.getBodyExpression).map(astForBlock(_, None)).getOrElse(Ast())
      scope.popScope()

      val ctorMethodReturnNode =
        methodReturnNode(Some(line(ctor)), Some(column(ctor)), typeFullName, Some(classFullName))
      val ctorParams = constructorParamsAsts.flatMap(_.root.collectAll[NewMethodParameterIn])
      methodAst(secondaryCtorMethodNode, ctorParams, ctorMethodBlockAst, ctorMethodReturnNode)
    }
  }

  def astsForClassOrObject(ktClass: KtClassOrObject)(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    val className = ktClass.getName
    val explicitFullName = {
      val fqName = ktClass.getContainingKtFile.getPackageFqName.toString
      s"$fqName.$className"
    }
    val classFullName = registerType(typeInfoProvider.fullName(ktClass, explicitFullName))
    val explicitBaseTypeFullNames = ktClass.getSuperTypeListEntries.asScala
      .map(_.getTypeAsUserType)
      .collect { case t if t != null => t.getText }
      // TODO: write test and pick up code from git@github.com:RedApparat/Fotoapparat.git
      .toList

    val baseTypeFullNames    = typeInfoProvider.inheritanceTypes(ktClass, explicitBaseTypeFullNames)
    val outBaseTypeFullNames = Option(baseTypeFullNames).filter(_.nonEmpty).getOrElse(Seq(TypeConstants.javaLangObject))
    val typeDecl = typeDeclNode(
      className,
      classFullName,
      relativizedPath,
      outBaseTypeFullNames,
      None,
      line(ktClass),
      column(ktClass)
    )
    scope.pushNewScope(typeDecl)

    val classFunctions = Option(ktClass.getBody)
      .map(_.getFunctions.asScala.collect { case f: KtNamedFunction => f })
      .getOrElse(List())
    val classDeclarations = Option(ktClass.getBody)
      .map(_.getDeclarations.asScala.filterNot(_.isInstanceOf[KtNamedFunction]))
      .getOrElse(List())

    /** curently unused val blockInitializers = if (ktClass.getBody != null) { ktClass.getBody.getAnonymousInitializers
      * } else { List().asJava }
      */
    val methodAsts = classFunctions.toSeq.map(astForMethod)
    val bindingsInfo = methodAsts.flatMap(_.root.collectAll[NewMethod]).map { _methodNode =>
      val node = bindingNode(_methodNode.name, _methodNode.signature)
      BindingInfo(node, List((typeDecl, node, EdgeTypes.BINDS), (node, _methodNode, EdgeTypes.REF)))
    }
    val constructorParams = ktClass.getPrimaryConstructorParameters.asScala.toList
    val defaultSignature = Option(ktClass.getPrimaryConstructor)
      .map { _ => typeInfoProvider.erasedSignature(constructorParams) }
      .getOrElse(s"${TypeConstants.void}()")
    val defaultFullName = s"$classFullName.${TypeConstants.initPrefix}:$defaultSignature"
    val (fullName, signature) =
      typeInfoProvider.fullNameWithSignature(ktClass.getPrimaryConstructor, (defaultFullName, defaultSignature))
    val primaryCtorMethodNode = methodNode(
      TypeConstants.initPrefix,
      fullName,
      signature,
      relativizedPath,
      line(ktClass.getPrimaryConstructor),
      column(ktClass.getPrimaryConstructor)
    )
    val ctorThisParam = methodParameterNode(Constants.this_, classFullName).order(0)
    scope.addToScope(Constants.this_, ctorThisParam)

    val constructorParamsAsts = Seq(Ast(ctorThisParam)) ++ withIndex(constructorParams) { (p, idx) =>
      astForParameter(p, idx)
    }

    val memberSetCalls = constructorParams.collect {
      case ctorParam if ctorParam.hasValOrVar =>
        val typeFullName       = registerType(typeInfoProvider.typeFullName(ctorParam, TypeConstants.any))
        val paramName          = ctorParam.getName
        val paramIdentifier    = identifierNode(paramName, typeFullName)
        val paramIdentifierAst = astWithRefEdgeMaybe(paramName, paramIdentifier)
        val thisIdentifier     = identifierNode(Constants.this_, classFullName)
        val fieldIdentifier    = fieldIdentifierNode(paramName)
        val fieldAccessCall =
          operatorCallNode(Operators.fieldAccess, Constants.this_ + "." + paramName, Some(typeFullName))
        val fieldAccessCallAst = callAst(fieldAccessCall, List(thisIdentifier, fieldIdentifier).map(Ast(_)))

        val assignmentNode =
          operatorCallNode(Operators.assignment, fieldAccessCall.code + " = " + paramIdentifier.code)
        callAst(assignmentNode, List(fieldAccessCallAst, paramIdentifierAst))
    }

    val typeFullName = typeInfoProvider.typeFullName(ktClass.getPrimaryConstructor, TypeConstants.any)
    val constructorMethodReturn = methodReturnNode(
      Some(line(ktClass.getPrimaryConstructor)),
      Some(column(ktClass.getPrimaryConstructor)),
      typeFullName,
      Some(classFullName)
    )
    val constructorAst = methodAst(
      primaryCtorMethodNode,
      constructorParamsAsts.flatMap(_.root.collectAll[NewMethodParameterIn]),
      blockAst(blockNode("", TypeConstants.void), memberSetCalls),
      constructorMethodReturn
    )

    val membersFromPrimaryCtorAsts = ktClass.getPrimaryConstructorParameters.asScala.toList.collect {
      case param if param.hasValOrVar =>
        val typeFullName = registerType(typeInfoProvider.parameterType(param, TypeConstants.any))
        Ast(memberNode(param.getName, typeFullName, line(param), column(param)))
    }

    val secondaryConstructorAsts = secondaryCtorAsts(ktClass.getSecondaryConstructors.asScala.toSeq, classFullName)
    val _componentNMethodAsts = ktClass match {
      case typedExpr: KtClass if typedExpr.isData =>
        componentNMethodAsts(typeDecl, ktClass.getPrimaryConstructor.getValueParameters.asScala.toSeq)
      case _ => Seq()
    }
    val componentNBindingsInfo = _componentNMethodAsts.flatMap(_.root.collectAll[NewMethod]).map { methodNode =>
      val node = bindingNode(methodNode.name, methodNode.signature)
      BindingInfo(node, List((typeDecl, node, EdgeTypes.BINDS), (node, methodNode, EdgeTypes.REF)))
    }

    val memberAsts = classDeclarations.toSeq.map(astForMember)
    val children = methodAsts ++ List(constructorAst) ++ membersFromPrimaryCtorAsts ++ secondaryConstructorAsts ++
      _componentNMethodAsts.toList ++ memberAsts
    val ast = Ast(typeDecl).withChildren(children)

    (bindingsInfo ++ componentNBindingsInfo).foreach(bindingInfoQueue.prepend)

    val finalAst = if (typeInfoProvider.isCompanionObject(ktClass)) {
      val companionMemberTypeFullName = ktClass.getParent.getParent match {
        case c: KtClassOrObject => typeInfoProvider.typeFullName(c, TypeConstants.any)
        case _                  => TypeConstants.any
      }
      registerType(companionMemberTypeFullName)

      val companionObjectMember = memberNode(Constants.companionObjectMemberName, companionMemberTypeFullName)
      ast.withChild(Ast(companionObjectMember))
    } else {
      ast
    }
    val companionObjectAsts = ktClass.getCompanionObjects.asScala.flatMap(astsForClassOrObject)
    scope.popScope()

    Seq(finalAst) ++ companionObjectAsts
  }

  def astForMethod(ktFn: KtNamedFunction)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val (fullName, signature) = typeInfoProvider.fullNameWithSignature(ktFn, ("", ""))
    val _methodNode = methodNode(
      ktFn.getName,
      fullName,
      signature,
      relativizedPath,
      line(ktFn),
      column(ktFn),
      lineEnd(ktFn),
      columnEnd(ktFn)
    )
    scope.pushNewScope(_methodNode)

    val parameters = withIndex(ktFn.getValueParameters.asScala.toSeq) { (p, idx) =>
      astForParameter(p, idx)
    }.flatMap(_.root.collectAll[NewMethodParameterIn])
    val bodyAst = ktFn.getBodyBlockExpression match {
      case blockExpr if blockExpr != null => astForBlock(blockExpr, None)
      case _                              => Ast(NewBlock())
    }
    scope.popScope()

    val explicitTypeName  = Option(ktFn.getTypeReference).map(_.getText).getOrElse(TypeConstants.any)
    val typeFullName      = registerType(typeInfoProvider.returnType(ktFn, explicitTypeName))
    val _methodReturnNode = methodReturnNode(Some(line(ktFn)), Some(column(ktFn)), typeFullName)
    methodAst(_methodNode, parameters, bodyAst, _methodReturnNode)
  }

  def astForBlock(
    expr: KtBlockExpression,
    argIdx: Option[Int],
    pushToScope: Boolean = true,
    localsForCaptures: List[NewLocal] = List()
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val typeFullName = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val node = withArgumentIndex(
      blockNode(expr.getStatements.asScala.map(_.getText).mkString("\n"), typeFullName, line(expr), column(expr)),
      argIdx
    )
    if (pushToScope) scope.pushNewScope(node)
    val statementAsts = withIndex(expr.getStatements.asScala.toSeq) { (statement, idx) =>
      astsForExpression(statement, Some(idx))
    }.flatten
    if (pushToScope) scope.popScope()
    blockAst(node, localsForCaptures.map(Ast(_)) ++ statementAsts)
  }

  def astForReturnExpression(expr: KtReturnExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val children = astsForExpression(expr.getReturnedExpression, Some(1))
    returnAst(returnNode(expr.getText, line(expr), column(expr)), children.toList)
  }

  def astForIsExpression(expr: KtIsExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val args = astsForExpression(expr.getLeftHandSide, Some(1)) ++
      Seq(astForTypeReference(expr.getTypeReference, Some(2)))
    val node = operatorCallNode(Operators.is, expr.getText, None, line(expr), column(expr))
    callAst(withArgumentIndex(node, argIdx), args.toList)
  }

  def astForBinaryExprWithTypeRHS(expr: KtBinaryExpressionWithTypeRHS, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val args = astsForExpression(expr.getLeft, Some(1)) ++ Seq(astForTypeReference(expr.getRight, Some(2)))
    val node = operatorCallNode(Operators.cast, expr.getText, None, line(expr), column(expr))
    callAst(withArgumentIndex(node, argIdx), args.toList)
  }

  def astForTypeReference(expr: KtTypeReference, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = registerType(typeInfoProvider.typeFullName(expr, TypeConstants.any))
    val node         = typeRefNode(expr.getText, typeFullName, line(expr), column(expr))
    Ast(withArgumentIndex(node, argIdx))
  }

  def astForSuperExpression(expr: KtSuperExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val node         = withArgumentIndex(identifierNode(expr.getText, typeFullName, line(expr), column(expr)), argIdx)
    astWithRefEdgeMaybe(expr.getText, node)
  }

  def astForThisExpression(expr: KtThisExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val node         = withArgumentIndex(identifierNode(expr.getText, typeFullName, line(expr), column(expr)), argIdx)
    astWithRefEdgeMaybe(expr.getText, node)
  }

  def astForClassLiteral(expr: KtClassLiteralExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val (fullName, signature) = typeInfoProvider.fullNameWithSignature(expr, ("", "")) // TODO: fix the fallback names
    val typeFullName          = registerType(typeInfoProvider.expressionType(expr, TypeConstants.javaLangObject))
    val node = callNode(
      expr.getText,
      TypeConstants.classLiteralReplacementMethodName,
      fullName,
      signature,
      typeFullName,
      DispatchTypes.STATIC_DISPATCH,
      line(expr),
      column(expr)
    )
    Ast(withArgumentIndex(node, argIdx))
  }

  def astForLambda(expr: KtLambdaExpression, argIdx: Option[Int])(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val (fullName, signature) = typeInfoProvider.fullNameWithSignature(expr, lambdaKeyPool)
    val lambdaMethodNode =
      methodNode(Constants.lambdaName, fullName, signature, relativizedPath, line(expr), column(expr))

    val closureBindingEntriesForCaptured = scope
      .pushClosureScope(lambdaMethodNode)
      .collect {
        case node: NewMethodParameterIn => node
        case node: NewLocal             => node
        case node: NewMember            => node
      }
      .map { capturedNode =>
        // TODO: remove the randomness here, two CPGs created from the same codebase should be the same
        val closureBindingId   = randomUUID().toString
        val closureBindingNode = closureBinding(closureBindingId, capturedNode.name)
        (closureBindingNode, capturedNode)
      }

    val localsForCaptured = closureBindingEntriesForCaptured.map { case (closureBindingNode, capturedNode) =>
      val node = localNode(capturedNode.name, capturedNode.typeFullName, closureBindingNode.closureBindingId)
      scope.addToScope(capturedNode.name, node)
      node
    }
    val parametersAsts = typeInfoProvider.implicitParameterName(expr) match {
      case Some(implicitParamName) =>
        val node = methodParameterNode(implicitParamName, TypeConstants.any)
        scope.addToScope(implicitParamName, node)
        Seq(Ast(node))
      case None =>
        withIndex(expr.getValueParameters.asScala.toSeq) { (p, idx) =>
          astForParameter(p, idx)
        }
    }

    val bodyAst = Option(expr.getBodyExpression)
      .map(astForBlock(_, None, pushToScope = false, localsForCaptured))
      .getOrElse(Ast(NewBlock()))

    val returnTypeFullName     = registerType(typeInfoProvider.returnTypeFullName(expr))
    val lambdaTypeDeclFullName = fullName.split(":").head
    val lambdaMethodAst = methodAst(
      lambdaMethodNode,
      parametersAsts.flatMap(_.root.collectAll[NewMethodParameterIn]),
      bodyAst,
      methodReturnNode(Some(line(expr)), Some(column(expr)), returnTypeFullName)
    ).withChild(Ast(modifierNode(ModifierTypes.VIRTUAL)))

    val _methodRefNode =
      withArgumentIndex(methodRefNode(expr.getText, fullName, lambdaTypeDeclFullName, line(expr), column(expr)), argIdx)

    val lambdaTypeDeclInheritsFromTypeFullName = TypeConstants.kotlinFunctionXPrefix + expr.getValueParameters.size
    val lambdaTypeDecl = typeDeclNode(
      Constants.lambdaTypeDeclName,
      lambdaTypeDeclFullName,
      relativizedPath,
      Seq(lambdaTypeDeclInheritsFromTypeFullName)
    )
    registerType(lambdaTypeDeclInheritsFromTypeFullName)

    val lambdaBinding = bindingNode(Constants.lambdaBindingName, signature)
    val bindingInfo = BindingInfo(
      lambdaBinding,
      Seq((lambdaTypeDecl, lambdaBinding, EdgeTypes.BINDS), (lambdaBinding, lambdaMethodNode, EdgeTypes.REF))
    )
    scope.popScope()
    val closureBindingDefs = closureBindingEntriesForCaptured.collect { case (closureBinding, node) =>
      ClosureBindingDef(closureBinding, _methodRefNode, node)
    }
    closureBindingDefs.foreach(closureBindingDefQueue.prepend)
    lambdaBindingInfoQueue.prepend(bindingInfo)
    lambdaAstQueue.prepend(lambdaMethodAst)
    Ast(_methodRefNode)
  }

  def astForArrayAccess(expr: KtArrayAccessExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val arrayExpr     = expr.getArrayExpression
    val typeFullName  = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val identifier    = identifierNode(arrayExpr.getText, typeFullName, line(arrayExpr), column(arrayExpr))
    val identifierAst = astWithRefEdgeMaybe(arrayExpr.getText, identifier)
    val astsForIndexExpr = expr.getIndexExpressions.asScala.zipWithIndex.map { case (expr, idx) =>
      astsForExpression(expr, Some(idx + 1))
    }.flatten
    val callNode =
      operatorCallNode(Operators.indexAccess, expr.getText, Some(typeFullName), line(expr), column(expr))
    callAst(withArgumentIndex(callNode, argIdx), List(identifierAst) ++ astsForIndexExpr)
  }

  def astForPostfixExpression(expr: KtPostfixExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val operatorType = ktTokenToOperator(forPostfixExpr = true).applyOrElse(
      KtPsiUtil.getOperationToken(expr),
      { (token: KtToken) =>
        logger.warn("Unsupported token type encountered: " + token)
        Constants.unknownOperator
      }
    )
    val typeFullName = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val args = List(astsForExpression(expr.getBaseExpression, Some(1)).headOption.getOrElse(Ast()))
      .filterNot(_.root == null)
    val node = operatorCallNode(operatorType, expr.getText, Some(typeFullName), line(expr), column(expr))
    callAst(withArgumentIndex(node, argIdx), args)
  }

  def astForPrefixExpression(expr: KtPrefixExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val operatorType = ktTokenToOperator(forPostfixExpr = false).applyOrElse(
      KtPsiUtil.getOperationToken(expr),
      { (token: KtToken) =>
        logger.warn("Unsupported token type encountered: " + token)
        Constants.unknownOperator
      }
    )
    val typeFullName = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val args = List(astsForExpression(expr.getBaseExpression, Some(1)).headOption.getOrElse(Ast()))
      .filterNot(_.root == null)
    val node = operatorCallNode(operatorType, expr.getText, Some(typeFullName), line(expr), column(expr))
    callAst(withArgumentIndex(node, argIdx), args)
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
  private def astsForDestructuringDeclarationWithNonCtorCallRHS(
    expr: KtDestructuringDeclaration
  )(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    val initExpr             = expr.getInitializer
    val destructuringEntries = nonUnderscoreEntries(expr)
    val localsForEntries = destructuringEntries.map { entry =>
      val typeFullName = registerType(typeInfoProvider.typeFullName(entry, TypeConstants.any))
      Ast(localNode(entry.getName, typeFullName, None, line(entry), column(entry)))
    }

    val callRhsTypeFullName = registerType(typeInfoProvider.expressionType(initExpr, TypeConstants.cpgUnresolved))
    val tmpName             = Constants.tmpLocalPrefix + tmpKeyPool.next
    val localForTmpNode     = localNode(tmpName, callRhsTypeFullName)
    val localForTmpAst      = Ast(localForTmpNode)

    val assignmentRhsAst = astsForExpression(initExpr, Some(2)).head

    val assignmentLhsNode = identifierNode(tmpName, callRhsTypeFullName, line(expr), column(expr))
    val assignmentLhsAst  = Ast(assignmentLhsNode).withRefEdge(assignmentLhsNode, localForTmpNode)

    val assignmentNode = operatorCallNode(Operators.assignment, s"$tmpName = ${initExpr.getText}", None)
    val assignmentAst  = callAst(assignmentNode, List(assignmentLhsAst, assignmentRhsAst))
    registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))

    val assignmentsForEntries = componentNCallAstsForDestructuringEntries(destructuringEntries, localForTmpNode)
    localsForEntries ++ Seq(localForTmpAst) ++
      Seq(assignmentAst) ++ assignmentsForEntries
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
  private def astsForDestructuringDeclarationWithCtorRHS(
    expr: KtDestructuringDeclaration
  )(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    val typedInit = Option(expr.getInitializer).collect { case e: KtCallExpression => e }
    if (typedInit.isEmpty) {
      logger.warn(s"Unhandled case for destructuring declaration: `${expr.getText}`.")
      return Seq()
    }
    val ctorCall = typedInit.get

    val destructuringEntries = nonUnderscoreEntries(expr)
    val localsForEntries = destructuringEntries.map { entry =>
      val typeFullName = registerType(typeInfoProvider.typeFullName(entry, TypeConstants.any))
      Ast(localNode(entry.getName, typeFullName, None, line(entry), column(entry)))
    }

    val ctorTypeFullName = registerType(typeInfoProvider.expressionType(ctorCall, TypeConstants.cpgUnresolved))
    val tmpName          = Constants.tmpLocalPrefix + tmpKeyPool.next
    val localForTmpNode  = localNode(tmpName, ctorTypeFullName)
    val localForTmpAst   = Ast(localForTmpNode)

    val assignmentRhsNode =
      operatorCallNode(Operators.alloc, Constants.alloc, Some(ctorTypeFullName), line(expr), column(expr))
    val assignmentLhsNode = identifierNode(tmpName, ctorTypeFullName, line(expr), column(expr))
    val assignmentLhsAst  = Ast(assignmentLhsNode).withRefEdge(assignmentLhsNode, localForTmpNode)

    val assignmentNode = operatorCallNode(Operators.assignment, tmpName + " = " + Constants.alloc, None)
    val assignmentAst  = callAst(assignmentNode, List(assignmentLhsAst, Ast(assignmentRhsNode)))

    val initReceiverNode = identifierNode(tmpName, ctorTypeFullName, line(expr), column(expr))
      .argumentIndex(0)
    val initReceiverAst = Ast(initReceiverNode).withRefEdge(initReceiverNode, localForTmpNode)

    val argAsts = withIndex(ctorCall.getValueArguments.asScala.toSeq) { case (arg, idx) =>
      astsForExpression(arg.getArgumentExpression, Some(idx))
    }.flatten

    val (fullName, signature) = typeInfoProvider.fullNameWithSignature(ctorCall, (TypeConstants.any, TypeConstants.any))
    registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val initCallNode = callNode(
      Constants.init,
      Constants.init,
      fullName,
      signature,
      TypeConstants.void,
      DispatchTypes.STATIC_DISPATCH,
      line(expr),
      column(expr)
    )
    val initCallAst = Ast(initCallNode)
      .withChildren(List(initReceiverAst) ++ argAsts)
      .withArgEdges(initCallNode, Seq(initReceiverNode) ++ argAsts.flatMap(_.root))

    val assignmentsForEntries = destructuringEntries.zipWithIndex.map { case (entry, idx) =>
      val entryTypeFullName = registerType(typeInfoProvider.typeFullName(entry, TypeConstants.any))
      val assignmentLHSNode = identifierNode(entry.getText, entryTypeFullName, line(entry), column(entry))
      val relevantLocal     = localsForEntries(idx).root.get
      val assignmentLHSAst  = Ast(assignmentLHSNode).withRefEdge(assignmentLHSNode, relevantLocal)

      val componentNIdentifierNode = identifierNode(localForTmpNode.name, ctorTypeFullName, line(entry), column(entry))
        .argumentIndex(0)

      val componentIdx      = idx + 1
      val fallbackSignature = s"${TypeConstants.cpgUnresolved}()"
      val fallbackFullName =
        s"${TypeConstants.cpgUnresolved}${Constants.componentNPrefix}$componentIdx:$fallbackSignature"
      val (fullName, signature) =
        typeInfoProvider.fullNameWithSignature(entry, (fallbackFullName, fallbackSignature))
      val componentNCallCode = s"${localForTmpNode.name}.${Constants.componentNPrefix}$componentIdx()"
      val componentNCallNode = callNode(
        componentNCallCode,
        s"${Constants.componentNPrefix}$componentIdx",
        fullName,
        signature,
        entryTypeFullName,
        DispatchTypes.DYNAMIC_DISPATCH,
        line(entry),
        column(entry)
      )
        .argumentIndex(2)

      val componentNIdentifierAst = Ast(componentNIdentifierNode)
        .withRefEdge(componentNIdentifierNode, localForTmpNode)
      val componentNAst = Ast(componentNCallNode)
        .withChild(componentNIdentifierAst)
        .withArgEdge(componentNCallNode, componentNIdentifierNode)
        .withReceiverEdge(componentNCallNode, componentNIdentifierNode)

      val assignmentCallNode = operatorCallNode(
        Operators.assignment,
        s"${entry.getText} = $componentNCallCode",
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
  | -> CALL two = person.component2()
  |__________________________________
   */
  private def astsForDestructuringDeclarationWithVarRHS(
    expr: KtDestructuringDeclaration
  )(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    val typedInit = Option(expr.getInitializer).collect { case e: KtNameReferenceExpression => e }
    if (typedInit.isEmpty) {
      logger.warn(s"Unhandled case for destructuring declaration: `${expr.getText}`.")
      return Seq()
    }
    val destructuringRHS = typedInit.get
    val localsForEntries = nonUnderscoreEntries(expr).map { entry =>
      val typeFullName = registerType(typeInfoProvider.typeFullName(entry, TypeConstants.any))
      Ast(localNode(entry.getName, typeFullName, None, line(entry), column(entry)))
    }

    val assignmentsForEntries = nonUnderscoreEntries(expr).zipWithIndex.map { case (entry, idx) =>
      val entryTypeFullName = registerType(typeInfoProvider.typeFullName(entry, TypeConstants.any))
      val assignmentLHSNode = identifierNode(entry.getText, entryTypeFullName, line(entry), column(entry))
      val relevantLocal     = localsForEntries(idx).root.get
      val assignmentLHSAst  = Ast(assignmentLHSNode).withRefEdge(assignmentLHSNode, relevantLocal)

      val componentNIdentifierTFN = registerType(typeInfoProvider.typeFullName(typedInit.get, TypeConstants.any))
      val componentNIdentifierNode =
        identifierNode(destructuringRHS.getText, componentNIdentifierTFN, line(entry), column(entry))
          .argumentIndex(0)

      val componentIdx      = idx + 1
      val fallbackSignature = s"${TypeConstants.cpgUnresolved}()"
      val fallbackFullName =
        s"${TypeConstants.cpgUnresolved}${Constants.componentNPrefix}$componentIdx:$fallbackSignature"
      val (fullName, signature) =
        typeInfoProvider.fullNameWithSignature(entry, (fallbackFullName, fallbackSignature))
      val componentNCallCode = s"${destructuringRHS.getText}.${Constants.componentNPrefix}$componentIdx()"
      val componentNCallNode = callNode(
        componentNCallCode,
        s"${Constants.componentNPrefix}$componentIdx",
        fullName,
        signature,
        entryTypeFullName,
        DispatchTypes.DYNAMIC_DISPATCH,
        line(entry),
        column(entry)
      )
        .argumentIndex(2)

      val componentNIdentifierAst = astWithRefEdgeMaybe(componentNIdentifierNode.name, componentNIdentifierNode)
      val componentNAst = Ast(componentNCallNode)
        .withChild(componentNIdentifierAst)
        .withArgEdge(componentNCallNode, componentNIdentifierNode)
        .withReceiverEdge(componentNCallNode, componentNIdentifierNode)

      val assignmentCallNode = operatorCallNode(
        Operators.assignment,
        s"${entry.getText} = $componentNCallCode",
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
      case typedExpr: KtCallExpression => typeInfoProvider.isConstructorCall(typedExpr).getOrElse(false)
      case _                           => false
    }
    if (isCtor) astsForDestructuringDeclarationWithCtorRHS(expr)
    else if (hasNonRefExprRHS) astsForDestructuringDeclarationWithNonCtorCallRHS(expr)
    else astsForDestructuringDeclarationWithVarRHS(expr)
  }

  def astForUnknown(expr: KtExpression, argIdx: Option[Int]): Ast = {
    val node = unknownNode(
      Option(expr).map(_.getText).getOrElse(Constants.codePropUndefinedValue),
      Constants.parserTypeName,
      line(expr),
      column(expr)
    )
    Ast(withArgumentIndex(node, argIdx))
  }

  def astForStringTemplate(expr: KtStringTemplateExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    if (expr.hasInterpolation) {
      val args = expr.getEntries.filter(_.getExpression != null).zipWithIndex.map { case (entry, idx) =>
        val entryTypeFullName = registerType(typeInfoProvider.expressionType(entry.getExpression, TypeConstants.any))
        val valueCallNode = operatorCallNode(
          Operators.formattedValue,
          entry.getExpression.getText,
          Some(entryTypeFullName),
          line(entry.getExpression),
          column(entry.getExpression)
        )
        val valueArgs = astsForExpression(entry.getExpression, Some(idx + 1))
        callAst(valueCallNode, valueArgs.toList)
      }
      val node = operatorCallNode(Operators.formatString, expr.getText, Some(typeFullName), line(expr), column(expr))
      callAst(withArgumentIndex(node, argIdx), args.toIndexedSeq.toList)
    } else {
      val node = literalNode(expr.getText, typeFullName, line(expr), column(expr))
      Ast(withArgumentIndex(node, argIdx))
    }
  }

  // TODO: clean up this whole fn
  def astForQualifiedExpression(expr: KtQualifiedExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val callKind        = typeInfoProvider.bindingKind(expr)
    val isStaticCall    = callKind == CallKinds.StaticCall
    val isDynamicCall   = callKind == CallKinds.DynamicCall
    val isExtensionCall = callKind == CallKinds.ExtensionCall

    val hasThisSuperOrNameRefReceiver = expr.getReceiverExpression match {
      case _: KtThisExpression          => true
      case _: KtNameReferenceExpression => true
      case _: KtSuperExpression         => true
      case _                            => false
    }
    val hasNameRefSelector = expr.getSelectorExpression.isInstanceOf[KtNameReferenceExpression]
    val isFieldAccessCall  = hasThisSuperOrNameRefReceiver && hasNameRefSelector
    val isCallToSuper = expr.getReceiverExpression match {
      case _: KtSuperExpression => true
      case _                    => false
    }
    val isStaticMethodCall = typeInfoProvider.isStaticMethodCall(expr)
    val hasRefToClassReceiver = expr.getReceiverExpression match {
      case r: KtNameReferenceExpression =>
        typeInfoProvider.isReferenceToClass(r)
      case _ =>
        false
    }
    val noAstForReceiver = isStaticMethodCall && hasRefToClassReceiver
    val argIdxForReceiver =
      if (isFieldAccessCall) 1
      else if (isCallToSuper) 0
      else if (isDynamicCall) 0
      else if (isExtensionCall) 0
      else if (isStaticCall) 1
      else 1
    val receiverAst        = astsForExpression(expr.getReceiverExpression, Some(argIdxForReceiver)).head
    val selectorOrderCount = argIdxForReceiver
    val argAsts = expr.getSelectorExpression match {
      case selectorExpression: KtCallExpression =>
        withIndex(selectorExpression.getValueArguments.asScala.toSeq) { case (arg, idx) =>
          val selectorOrder    = if (isStaticCall) idx else selectorOrderCount + idx + 1
          val selectorArgIndex = if (isStaticCall) idx else selectorOrder - 1
          astsForExpression(arg.getArgumentExpression, Some(selectorArgIndex))
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

    val astDerivedMethodFullName = expr.getSelectorExpression match {
      case expression: KtCallExpression =>
        val receiverPlaceholderType = TypeConstants.cpgUnresolved
        val shortName               = expr.getSelectorExpression.getFirstChild.getText
        val args                    = expression.getValueArguments
        s"$receiverPlaceholderType.$shortName:${typeInfoProvider.erasedSignature(args.asScala.toList)}"
      case _: KtNameReferenceExpression =>
        Operators.fieldAccess
      case _ =>
        // TODO: add more test cases for this scenario
        ""
    }

    val astDerivedSignature = if (astDerivedMethodFullName.startsWith(Constants.operatorSuffix)) {
      ""
    } else {
      typeInfoProvider.erasedSignature(argAsts)
    }
    val (fullName, signature) =
      typeInfoProvider.fullNameWithSignature(expr, (astDerivedMethodFullName, astDerivedSignature))
    registerType(typeInfoProvider.containingDeclType(expr, TypeConstants.any))
    val retType = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val methodName = if (isFieldAccessCall || fullName == Operators.fieldAccess) {
      Operators.fieldAccess
    } else {
      expr.getSelectorExpression.getFirstChild.getText
    }
    val dispatchType = if (isFieldAccessCall) {
      DispatchTypes.STATIC_DISPATCH
    } else if (callKind == CallKinds.DynamicCall) {
      DispatchTypes.DYNAMIC_DISPATCH
    } else if (callKind == CallKinds.ExtensionCall) {
      DispatchTypes.STATIC_DISPATCH
    } else {
      DispatchTypes.STATIC_DISPATCH
    }

    val _callNode =
      callNode(expr.getText, methodName, fullName, signature, retType, dispatchType, line(expr), column(expr))
    val root         = Ast(withArgumentIndex(_callNode, argIdx))
    val receiverNode = receiverAst.root.get
    val finalAst = {
      if (isExtensionCall || isCallToSuper) {
        root
          .withChild(receiverAst)
          .withArgEdge(_callNode, receiverNode)
          .withChildren(argAsts)
          .withArgEdges(_callNode, argAsts.map(_.root.get))
      } else if (noAstForReceiver) {
        root.withChild(receiverAst).withChildren(argAsts).withArgEdges(_callNode, argAsts.map(_.root.get))
      } else {
        val ast = root
          .withChild(receiverAst)
          .withArgEdge(_callNode, receiverNode)
          .withChildren(argAsts)
          .withArgEdges(_callNode, argAsts.map(_.root.get))
        if (argAsts.size == 1 && argAsts.head.root.get.isInstanceOf[NewMethodRef]) {
          ast.withReceiverEdge(_callNode, argAsts.head.root.get)
        } else {
          ast.withReceiverEdge(_callNode, receiverNode)
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
    val tryAstOption = astsForExpression(expr.getTryBlock, Some(1)).headOption
      .getOrElse(Ast())
    val clauseAsts = withIndex(expr.getCatchClauses.asScala.toSeq) { (entry, idx) =>
      astsForExpression(entry.getCatchBody, Some(idx + 1))
    }.flatten
    val finallyAsts = Option(expr.getFinallyBlock)
      .map(_.getFinalExpression)
      .map(astsForExpression(_, Some(clauseAsts.size + 2)))
      .getOrElse(Seq())
    val node = controlStructureNode(expr.getText, ControlStructureTypes.TRY, line(expr), column(expr))
    controlStructureAst(node, None, tryAstOption :: (clauseAsts ++ finallyAsts).toList)
  }

  private def astForTryAsExpression(expr: KtTryExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = registerType(
      // TODO: remove the `last`
      typeInfoProvider.expressionType(expr.getTryBlock.getStatements.asScala.last, TypeConstants.any)
    )
    val tryBlockAst = astsForExpression(expr.getTryBlock, Some(1)).headOption.getOrElse(Ast())
    val clauseAsts = withIndex(expr.getCatchClauses.asScala.toSeq) { (entry, idx) =>
      astsForExpression(entry.getCatchBody, Some(idx + 1))
    }.flatten
    val node = operatorCallNode(Operators.tryCatch, expr.getText, Some(typeFullName), line(expr), column(expr))
    callAst(withArgumentIndex(node, argIdx), List(tryBlockAst) ++ clauseAsts)
  }

  // TODO: handle parameters passed to the clauses
  def astForTry(expr: KtTryExpression, argIdx: Option[Int])(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    if (KtPsiUtil.isStatement(expr)) astForTryAsStatement(expr)
    else astForTryAsExpression(expr, argIdx)
  }

  def astForWhile(expr: KtWhileExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val conditionAst = astsForExpression(expr.getCondition, Some(1)).headOption
    val stmtAsts     = astsForExpression(expr.getBody, Some(2))
    val node         = controlStructureNode(expr.getText, ControlStructureTypes.WHILE, line(expr), column(expr))
    controlStructureAst(node, conditionAst, stmtAsts.toList)
  }

  def astForDoWhile(expr: KtDoWhileExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val conditionAst = astsForExpression(expr.getCondition, Some(2)).headOption
    val stmtAsts     = astsForExpression(expr.getBody, Some(1))
    val node         = controlStructureNode(expr.getText, ControlStructureTypes.DO, line(expr), column(expr))
    controlStructureAst(node, conditionAst, stmtAsts.toList, true)
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
    val loopRangeText         = expr.getLoopRange.getText
    val iteratorName          = Constants.iteratorPrefix + iteratorKeyPool.next()
    val iteratorLocal         = localNode(iteratorName, TypeConstants.any)
    val iteratorAssignmentLhs = identifierNode(iteratorName, TypeConstants.any)
    val iteratorLocalAst      = Ast(iteratorLocal).withRefEdge(iteratorAssignmentLhs, iteratorLocal)

    // TODO: maybe use a different method here, one which does not translate `kotlin.collections.List` to `java.util.List`
    val loopRangeExprTypeFullName = registerType(typeInfoProvider.expressionType(expr.getLoopRange, TypeConstants.any))

    val iteratorAssignmentRhsIdentifier = identifierNode(loopRangeText, loopRangeExprTypeFullName)
      .argumentIndex(0)
    val iteratorAssignmentRhs = callNode(
      s"$loopRangeText.${Constants.getIteratorMethodName}()",
      Constants.getIteratorMethodName,
      s"$loopRangeExprTypeFullName.${Constants.getIteratorMethodName}:${Constants.javaUtilIterator}()",
      s"${Constants.javaUtilIterator}()",
      Constants.javaUtilIterator,
      DispatchTypes.DYNAMIC_DISPATCH
    )
      .argumentIndex(2)

    val iteratorAssignmentRhsAst = Ast(iteratorAssignmentRhs)
      .withChild(Ast(iteratorAssignmentRhsIdentifier))
      .withArgEdge(iteratorAssignmentRhs, iteratorAssignmentRhsIdentifier)
      .withReceiverEdge(iteratorAssignmentRhs, iteratorAssignmentRhsIdentifier)
    val iteratorAssignment =
      operatorCallNode(Operators.assignment, s"$iteratorName = ${iteratorAssignmentRhs.code}", None)

    val iteratorAssignmentAst = callAst(iteratorAssignment, List(Ast(iteratorAssignmentLhs), iteratorAssignmentRhsAst))
    val controlStructure = controlStructureNode(expr.getText, ControlStructureTypes.WHILE, line(expr), column(expr))

    val conditionIdentifier = identifierNode(loopRangeText, loopRangeExprTypeFullName).argumentIndex(0)

    val hasNextFullName =
      s"${Constants.collectionsIteratorName}.${Constants.hasNextIteratorMethodName}:${TypeConstants.javaLangBoolean}()"
    val controlStructureCondition = callNode(
      s"$iteratorName.${Constants.hasNextIteratorMethodName}()",
      Constants.hasNextIteratorMethodName,
      hasNextFullName,
      s"${TypeConstants.javaLangBoolean}()",
      TypeConstants.javaLangBoolean,
      DispatchTypes.DYNAMIC_DISPATCH
    ).argumentIndex(0)
    val controlStructureConditionAst = Ast(controlStructureCondition)
      .withChild(Ast(conditionIdentifier))
      .withArgEdge(controlStructureCondition, conditionIdentifier)
      .withReceiverEdge(controlStructureCondition, conditionIdentifier)

    val loopParameterTypeFullName = registerType(
      typeInfoProvider.typeFullName(expr.getLoopParameter, TypeConstants.any)
    )
    val loopParameterName  = expr.getLoopParameter.getText
    val loopParameterLocal = localNode(loopParameterName, loopParameterTypeFullName)
    scope.addToScope(loopParameterName, loopParameterLocal) // TODO: remove from scope after the block

    val loopParameterIdentifier = identifierNode(loopParameterName, TypeConstants.any)
    val loopParameterAst        = Ast(loopParameterLocal).withRefEdge(loopParameterIdentifier, loopParameterLocal)

    val iteratorNextIdentifier    = identifierNode(iteratorName, TypeConstants.any).argumentIndex(0)
    val iteratorNextIdentifierAst = Ast(iteratorNextIdentifier).withRefEdge(iteratorNextIdentifier, iteratorLocal)

    val iteratorNextCall = callNode(
      s"$iteratorName.${Constants.nextIteratorMethodName}()",
      Constants.nextIteratorMethodName,
      s"${Constants.collectionsIteratorName}.${Constants.nextIteratorMethodName}:${TypeConstants.javaLangObject}()",
      s"${TypeConstants.javaLangObject}()",
      TypeConstants.javaLangObject,
      DispatchTypes.DYNAMIC_DISPATCH
    ).argumentIndex(2)
    val iteratorNextCallAst = Ast(iteratorNextCall)
      .withChild(iteratorNextIdentifierAst)
      .withArgEdge(iteratorNextCall, iteratorNextIdentifier)
      .withReceiverEdge(iteratorNextCall, iteratorNextIdentifier)
    val loopParameterNextAssignment =
      operatorCallNode(Operators.assignment, s"$loopParameterName = ${iteratorNextCall.code}", None)
    val loopParameterNextAssignmentAst =
      callAst(loopParameterNextAssignment, List(Ast(loopParameterIdentifier), iteratorNextCallAst))

    val stmtAsts             = astsForExpression(expr.getBody, Some(3))
    val controlStructureBody = blockNode("", "")
    val controlStructureBodyAst = Ast(controlStructureBody)
      .withChildren(List(loopParameterAst, loopParameterNextAssignmentAst) ++ stmtAsts)

    val controlStructureAst = Ast(controlStructure)
      .withChildren(List(controlStructureConditionAst, controlStructureBodyAst))
      .withConditionEdge(controlStructure, controlStructureCondition)
    blockAst(
      blockNode(Constants.codeForLoweredForBlock, ""),
      List(iteratorLocalAst, iteratorAssignmentAst, controlStructureAst)
    )
  }

  def componentNCallAstsForDestructuringEntries(entries: Seq[KtDestructuringDeclarationEntry], localForTmp: NewLocal)(
    implicit typeInfoProvider: TypeInfoProvider
  ): Seq[Ast] = {
    withIndex(entries) { (entry, idx) =>
      val entryTypeFullName     = registerType(typeInfoProvider.typeFullName(entry, TypeConstants.any))
      val entryIdentifier       = identifierNode(entry.getText, entryTypeFullName, line(entry), column(entry))
      val entryIdentifierAst    = astWithRefEdgeMaybe(entryIdentifier.name, entryIdentifier)
      val componentNName        = Constants.componentNPrefix + idx
      val fallbackSignature     = s"${TypeConstants.cpgUnresolved}()"
      val fallbackFullName      = s"${TypeConstants.cpgUnresolved}$componentNName:$fallbackSignature"
      val (fullName, signature) = typeInfoProvider.fullNameWithSignature(entry, (fallbackFullName, fallbackSignature))
      val tmpName               = localForTmp.name
      val componentNCallCode    = s"$tmpName.$componentNName()"

      val tmpForComponentNIdentifier = identifierNode(tmpName, localForTmp.typeFullName).argumentIndex(0)
      val tmpForComponentNIdentifierAst = Ast(tmpForComponentNIdentifier)
        .withRefEdge(tmpForComponentNIdentifier, localForTmp)

      val tmpComponentNCall = callNode(
        componentNCallCode,
        componentNName,
        fullName,
        signature,
        entryTypeFullName,
        DispatchTypes.DYNAMIC_DISPATCH
      ).argumentIndex(2)
      val tmpComponentNCallAst = Ast(tmpComponentNCall)
        .withChild(tmpForComponentNIdentifierAst)
        .withArgEdge(tmpComponentNCall, tmpForComponentNIdentifier)
        .withReceiverEdge(tmpComponentNCall, tmpForComponentNIdentifier)

      val componentNAssignment =
        operatorCallNode(Operators.assignment, entryIdentifier.code + " = " + tmpComponentNCall.code)
      callAst(componentNAssignment, List(entryIdentifierAst, tmpComponentNCallAst))
    }
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
    val loopRangeText         = expr.getLoopRange.getText
    val iteratorName          = Constants.iteratorPrefix + iteratorKeyPool.next()
    val localForIterator      = localNode(iteratorName, TypeConstants.any)
    val iteratorAssignmentLhs = identifierNode(iteratorName, TypeConstants.any)
    val iteratorLocalAst      = Ast(localForIterator).withRefEdge(iteratorAssignmentLhs, localForIterator)

    // TODO: maybe use a different method here, one which does not translate `kotlin.collections.List` to `java.util.List`
    val loopRangeExprTypeFullName = registerType(typeInfoProvider.expressionType(expr.getLoopRange, TypeConstants.any))
    val iteratorAssignmentRhsIdentifier = identifierNode(loopRangeText, loopRangeExprTypeFullName)
      .argumentIndex(0)
    val iteratorAssignmentRhs = callNode(
      s"$loopRangeText.${Constants.getIteratorMethodName}()",
      Constants.getIteratorMethodName,
      s"$loopRangeExprTypeFullName.${Constants.getIteratorMethodName}:${Constants.javaUtilIterator}()",
      s"${Constants.javaUtilIterator}()",
      Constants.javaUtilIterator,
      DispatchTypes.DYNAMIC_DISPATCH
    )
      .argumentIndex(2)

    val iteratorAssignmentRhsAst = Ast(iteratorAssignmentRhs)
      .withChild(Ast(iteratorAssignmentRhsIdentifier))
      .withArgEdge(iteratorAssignmentRhs, iteratorAssignmentRhsIdentifier)
      .withReceiverEdge(iteratorAssignmentRhs, iteratorAssignmentRhsIdentifier)

    val iteratorAssignment =
      operatorCallNode(Operators.assignment, s"$iteratorName = ${iteratorAssignmentRhs.code}", None)
    val iteratorAssignmentAst = callAst(iteratorAssignment, List(Ast(iteratorAssignmentLhs), iteratorAssignmentRhsAst))

    val controlStructure    = controlStructureNode(expr.getText, ControlStructureTypes.WHILE, line(expr), column(expr))
    val conditionIdentifier = identifierNode(loopRangeText, loopRangeExprTypeFullName).argumentIndex(0)

    val hasNextFullName =
      s"${Constants.collectionsIteratorName}.${Constants.hasNextIteratorMethodName}:${TypeConstants.javaLangBoolean}()"
    val controlStructureCondition = callNode(
      s"$iteratorName.${Constants.hasNextIteratorMethodName}()",
      Constants.hasNextIteratorMethodName,
      hasNextFullName,
      s"${TypeConstants.javaLangBoolean}()",
      TypeConstants.javaLangBoolean,
      DispatchTypes.DYNAMIC_DISPATCH
    ).argumentIndex(0)
    val controlStructureConditionAst = Ast(controlStructureCondition)
      .withChild(Ast(conditionIdentifier))
      .withArgEdge(controlStructureCondition, conditionIdentifier)
      .withReceiverEdge(controlStructureCondition, conditionIdentifier)

    val destructuringDeclEntries = expr.getDestructuringDeclaration.getEntries
    val localsForDestructuringVars = destructuringDeclEntries.asScala.map { entry =>
      val entryTypeFullName = registerType(typeInfoProvider.typeFullName(entry, TypeConstants.any))
      val entryName         = entry.getText
      val node              = localNode(entryName, entryTypeFullName, None, line(entry), column(entry))
      // TODO: remove from scope after the block exits [add test where that is not the case]
      scope.addToScope(entryName, node)
      Ast(node)
    }.toList

    val tmpName        = Constants.tmpLocalPrefix + tmpKeyPool.next
    val localForTmp    = localNode(tmpName, TypeConstants.any)
    val localForTmpAst = Ast(localForTmp)

    val tmpIdentifier             = identifierNode(tmpName, TypeConstants.any)
    val tmpIdentifierAst          = Ast(tmpIdentifier).withRefEdge(tmpIdentifier, localForTmp)
    val iteratorNextIdentifier    = identifierNode(iteratorName, TypeConstants.any).argumentIndex(0)
    val iteratorNextIdentifierAst = Ast(iteratorNextIdentifier).withRefEdge(iteratorNextIdentifier, localForIterator)

    val iteratorNextCall = callNode(
      s"${iteratorNextIdentifier.code}.${Constants.nextIteratorMethodName}()",
      Constants.nextIteratorMethodName,
      s"${Constants.collectionsIteratorName}.${Constants.nextIteratorMethodName}:${TypeConstants.javaLangObject}()",
      s"${TypeConstants.javaLangObject}()",
      TypeConstants.javaLangObject,
      DispatchTypes.DYNAMIC_DISPATCH
    ).argumentIndex(2)

    val iteratorNextCallAst = Ast(iteratorNextCall)
      .withChild(iteratorNextIdentifierAst)
      .withArgEdge(iteratorNextCall, iteratorNextIdentifier)
      .withReceiverEdge(iteratorNextCall, iteratorNextIdentifier)
    val tmpParameterNextAssignment    = operatorCallNode(Operators.assignment, s"$tmpName = ${iteratorNextCall.code}")
    val tmpParameterNextAssignmentAst = callAst(tmpParameterNextAssignment, List(tmpIdentifierAst, iteratorNextCallAst))

    val componentNCalls = componentNCallAstsForDestructuringEntries(destructuringDeclEntries.asScala.toSeq, localForTmp)
    val stmtAsts        = astsForExpression(expr.getBody, None)
    val controlStructureBody = blockNode("", "")
    val controlStructureBodyAst = Ast(controlStructureBody).withChildren(
      localsForDestructuringVars ++
        List(localForTmpAst, tmpParameterNextAssignmentAst) ++
        componentNCalls ++
        stmtAsts
    )

    val controlStructureAst = Ast(controlStructure)
      .withChildren(List(controlStructureConditionAst, controlStructureBodyAst))
      .withConditionEdge(controlStructure, controlStructureCondition)
    blockAst(
      blockNode(Constants.codeForLoweredForBlock, ""),
      List(iteratorLocalAst, iteratorAssignmentAst, controlStructureAst)
    )
  }

  def astForFor(expr: KtForExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    if (expr.getDestructuringDeclaration != null) astForForWithDestructuringLHS(expr)
    else astForForWithSimpleVarLHS(expr)
  }

  def astForWhen(expr: KtWhenExpression, argIdx: Option[Int])(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val astForSubject = astsForExpression(expr.getSubjectExpression, Some(1)).headOption.getOrElse(Ast())
    val astsForEntries = withIndex(expr.getEntries.asScala.toSeq) { (e, idx) =>
      astsForWhenEntry(e, idx)
    }.flatten

    val switchBlockNode =
      blockNode(expr.getEntries.asScala.map(_.getText).mkString("\n"), TypeConstants.any, line(expr), column(expr))
    val astForBlock = Ast(switchBlockNode).withChildren(astsForEntries)

    val codeForSwitch = Option(expr.getSubjectExpression)
      .map(_.getText)
      .map { text => s"${Constants.when}($text)" }
      .getOrElse(Constants.when)
    val switchNode = controlStructureNode(codeForSwitch, ControlStructureTypes.SWITCH, line(expr), column(expr))
    val ast        = Ast(withArgumentIndex(switchNode, argIdx)).withChildren(List(astForSubject, astForBlock))
    // TODO: rewrite this as well
    astForSubject.root match {
      case Some(root) => ast.withConditionEdge(switchNode, root)
      case None       => ast
    }
  }

  def astsForWhenEntry(entry: KtWhenEntry, argIdx: Int)(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    // TODO: get all conditions with entry.getConditions()
    val name =
      if (entry.getElseKeyword == null) Constants.defaultCaseNode
      else s"${Constants.caseNodePrefix}$argIdx"
    val jumpNode = jumpTargetNode(entry.getText, name, Constants.caseNodeParserTypeName, line(entry), column(entry))
      .argumentIndex(argIdx)
    val exprNode = astsForExpression(entry.getExpression, Some(argIdx + 1)).headOption.getOrElse(Ast())
    Seq(Ast(jumpNode), exprNode)
  }

  def astForIf(expr: KtIfExpression, argIdx: Option[Int])(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val isChildOfControlStructureBody = expr.getParent.isInstanceOf[KtContainerNodeForControlStructureBody]
    if (KtPsiUtil.isStatement(expr) && !isChildOfControlStructureBody) astForIfAsControlStructure(expr)
    else astForIfAsExpression(expr, argIdx)
  }

  def astForIfAsControlStructure(expr: KtIfExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val conditionAst = astsForExpression(expr.getCondition, None).headOption
    val thenAsts     = astsForExpression(expr.getThen, None)
    val elseAsts     = astsForExpression(expr.getElse, None)

    val node = controlStructureNode(expr.getText, ControlStructureTypes.IF, line(expr), column(expr))
    controlStructureAst(node, conditionAst, List(thenAsts ++ elseAsts).flatten)
  }

  def astForIfAsExpression(expr: KtIfExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val conditionAsts = astsForExpression(expr.getCondition, Some(1))
    val thenAsts      = astsForExpression(expr.getThen, Some(2))
    val elseAsts      = astsForExpression(expr.getElse, Some(3))

    val returnTypeFullName = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val node = operatorCallNode(Operators.conditional, expr.getText, Some(returnTypeFullName), line(expr), column(expr))
    callAst(withArgumentIndex(node, argIdx), (conditionAsts ++ thenAsts ++ elseAsts).toList)
  }

  private def astForCtorCall(expr: KtCallExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = registerType(typeInfoProvider.expressionType(expr, TypeConstants.cpgUnresolved))
    val tmpBlockNode = blockNode("", typeFullName)
    val tmpName      = Constants.tmpLocalPrefix + tmpKeyPool.next
    val tmpLocalNode = localNode(tmpName, typeFullName)
    val assignmentRhsNode =
      operatorCallNode(Operators.alloc, Constants.alloc, Some(typeFullName), line(expr), column(expr))
    val assignmentLhsNode = identifierNode(tmpName, typeFullName, line(expr), column(expr))
    val assignmentNode    = operatorCallNode(Operators.assignment, Operators.assignment)
    val assignmentAst     = callAst(assignmentNode, List(assignmentLhsNode, assignmentRhsNode).map(Ast(_)))
    val initReceiverNode = identifierNode(tmpName, typeFullName, line(expr), column(expr))
      .argumentIndex(0)
    val initReceiverAst = Ast(initReceiverNode)

    val argAsts = withIndex(expr.getValueArguments.asScala.toSeq) { case (arg, idx) =>
      astsForExpression(arg.getArgumentExpression, Some(idx))
    }.flatten

    val (fullName, signature) = typeInfoProvider.fullNameWithSignature(expr, (TypeConstants.any, TypeConstants.any))
    registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))

    val initCallNode = callNode(
      expr.getText,
      Constants.init,
      fullName,
      signature,
      TypeConstants.void,
      DispatchTypes.STATIC_DISPATCH,
      line(expr),
      column(expr)
    )
      .argumentIndex(2)
    val initCallAst = Ast(initCallNode)
      .withChildren(List(initReceiverAst) ++ argAsts)
      .withArgEdges(initCallNode, Seq(initReceiverNode) ++ argAsts.flatMap(_.root))

    val lastIdentifier    = identifierNode(tmpName, typeFullName, line(expr), column(expr))
    val lastIdentifierAst = Ast(lastIdentifier)
    val tmpLocalAst = Ast(tmpLocalNode)
      .withRefEdge(assignmentLhsNode, tmpLocalNode)
      .withRefEdge(initReceiverNode, tmpLocalNode)
      .withRefEdge(lastIdentifier, tmpLocalNode)

    Ast(withArgumentIndex(tmpBlockNode, argIdx))
      .withChildren(List(tmpLocalAst, assignmentAst, initCallAst, lastIdentifierAst))
  }

  def astsForProperty(expr: KtProperty)(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    val explicitTypeName = Option(expr.getTypeReference).map(_.getText).getOrElse(TypeConstants.any)
    val elem             = expr.getIdentifyingElement
    val typeFullName     = registerType(typeInfoProvider.propertyType(expr, explicitTypeName))
    val node             = localNode(expr.getName, typeFullName, None, line(expr), column(expr))
    scope.addToScope(expr.getName, node)

    val hasRHSCtorCall = expr.getDelegateExpressionOrInitializer match {
      case typed: KtCallExpression => typeInfoProvider.isConstructorCall(typed).getOrElse(false)
      case _                       => false
    }
    val rhsAsts =
      // TODO: remove the hard cast
      if (hasRHSCtorCall)
        Seq(astForCtorCall(expr.getDelegateExpressionOrInitializer.asInstanceOf[KtCallExpression], Some(2)))
      else astsForExpression(expr.getDelegateExpressionOrInitializer, Some(2))
    val identifier     = identifierNode(elem.getText, typeFullName, line(elem), column(elem))
    val assignmentNode = operatorCallNode(Operators.assignment, expr.getText, None, line(expr), column(expr))
    val call           = callAst(assignmentNode, List(Ast(identifier)) ++ rhsAsts)

    val localAst = Ast(node).withRefEdge(identifier, node)
    Seq(call) ++ Seq(localAst)
  }

  def astForNameReference(expr: KtNameReferenceExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    if (typeInfoProvider.isReferenceToClass(expr)) astForNameReferenceToType(expr, argIdx)
    else
      typeInfoProvider.isReferencingMember(expr) match {
        case true  => astForNameReferenceToMember(expr, argIdx)
        case false => astForNonSpecialNameReference(expr, argIdx)
      }
  }

  private def astForNameReferenceToType(expr: KtNameReferenceExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName              = registerType(typeInfoProvider.typeFullName(expr, TypeConstants.any))
    val referencesCompanionObject = typeInfoProvider.isRefToCompanionObject(expr)
    if (referencesCompanionObject) {
      val argAsts = List(
        // TODO: change this to a TYPE_REF node as soon as the closed source data-flow engine supports it
        identifierNode(expr.getIdentifier.getText, typeFullName, line(expr), column(expr)),
        fieldIdentifierNode(Constants.companionObjectMemberName, line(expr), column(expr))
      ).map(Ast(_))
      val node = operatorCallNode(Operators.fieldAccess, expr.getText, Some(typeFullName), line(expr), column(expr))
      callAst(withArgumentIndex(node, argIdx), argAsts)
    } else {
      val node = typeRefNode(expr.getIdentifier.getText, typeFullName, line(expr), column(expr))
      Ast(withArgumentIndex(node, argIdx))
    }
  }

  private def astForNameReferenceToMember(expr: KtNameReferenceExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = registerType(typeInfoProvider.typeFullName(expr, TypeConstants.any))
    val referenceTargetTypeFullName = registerType(
      typeInfoProvider.referenceTargetTypeFullName(expr, TypeConstants.any)
    )
    val thisNode             = identifierNode(Constants.this_, referenceTargetTypeFullName, line(expr), column(expr))
    val thisAst              = astWithRefEdgeMaybe(Constants.this_, thisNode)
    val _fieldIdentifierNode = fieldIdentifierNode(expr.getReferencedName, line(expr), column(expr))
    val node = operatorCallNode(
      Operators.fieldAccess,
      s"${Constants.this_}.${expr.getReferencedName}",
      Some(typeFullName),
      line(expr),
      column(expr)
    )
    callAst(withArgumentIndex(node, argIdx), List(thisAst, Ast(_fieldIdentifierNode)))
  }

  private def astForNonSpecialNameReference(expr: KtNameReferenceExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = registerType(typeInfoProvider.typeFullName(expr, TypeConstants.any))
    val name         = expr.getIdentifier.getText
    val node         = withArgumentIndex(identifierNode(name, typeFullName, line(expr), column(expr)), argIdx)
    astWithRefEdgeMaybe(name, node)
  }

  def astForLiteral(expr: KtConstantExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val node         = literalNode(expr.getText, typeFullName, line(expr), column(expr))
    Ast(withArgumentIndex(node, argIdx))
  }

  def astForBinaryExpr(expr: KtBinaryExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
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
            case _      => None
          }
        case _ =>
          logger.warn(
            "Unhandled operator token type `" + opRef.getOperationSignTokenType + "` for expression `" + expr.getText + "`."
          )
          Some(Constants.unknownOperator)
      }
    }
    val (fullName, signature) =
      if (operatorOption.isDefined) (operatorOption.get, TypeConstants.any)
      // TODO: fix the fallback METHOD_FULL_NAME and SIGNATURE here (should be a correct number of ANYs)
      else typeInfoProvider.fullNameWithSignature(expr, (TypeConstants.any, TypeConstants.any))

    val finalSignature =
      // TODO: add test case for this situation
      if (fullName.startsWith(Constants.operatorSuffix)) Constants.empty
      else signature
    val typeFullName = registerType(typeInfoProvider.typeFullName(expr, TypeConstants.any))
    val name =
      if (operatorOption.isDefined) operatorOption.get
      else if (expr.getChildren.toList.size >= 2) expr.getChildren.toList(1).getText
      else expr.getName
    val node = callNode(
      expr.getText,
      name,
      fullName,
      finalSignature,
      typeFullName,
      DispatchTypes.STATIC_DISPATCH,
      line(expr),
      column(expr)
    )
    val args = astsForExpression(expr.getLeft, Some(1)) ++ astsForExpression(expr.getRight, Some(2))
    callAst(withArgumentIndex(node, argIdx), args.toList)
  }

  def astForCall(expr: KtCallExpression, argIdx: Option[Int])(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val isCtorCall = typeInfoProvider.isConstructorCall(expr)
    if (isCtorCall.getOrElse(false)) astForCtorCall(expr, argIdx)
    else astForNonCtorCall(expr, argIdx)
  }

  private def astForNonCtorCall(expr: KtCallExpression, argIdx: Option[Int])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val declFullNameOption = typeInfoProvider.containingDeclFullName(expr)
    declFullNameOption.foreach(registerType)

    val argAsts = withIndex(expr.getValueArguments.asScala.toSeq) { case (arg, idx) =>
      astsForExpression(arg.getArgumentExpression, Some(idx))
    }.flatten

    // TODO: add tests for the empty `referencedName` here
    val referencedName = Option(expr.getFirstChild)
      .collect { case expr: KtNameReferenceExpression => expr }
      .map(_.getText)
      .getOrElse("")
    val nameToClass =
      expr.getContainingKtFile.getDeclarations.asScala.collect { case c: KtClass => c.getName -> c }.toMap

    val imports = expr.getContainingKtFile.getImportList.getImports.asScala.toList
    val importedNames = imports.map { imp =>
      val importedName = Option(imp.getImportedName).map(_.toString).getOrElse(Constants.wildcardImportName)
      importedName -> imp
    }.toMap

    val methodFqName = if (importedNames.isDefinedAt(referencedName)) {
      importedNames(referencedName).getImportedFqName.toString
    } else if (nameToClass.contains(expr.getCalleeExpression.getText)) {
      val klass = nameToClass(expr.getCalleeExpression.getText)
      s"${klass.getContainingKtFile.getPackageFqName.toString}.$referencedName"
    } else {
      s"${expr.getContainingKtFile.getPackageFqName.toString}.$referencedName"
    }
    val explicitSignature     = s"${TypeConstants.any}(${argAsts.map { _ => TypeConstants.any }.mkString(",")})"
    val explicitFullName      = s"$methodFqName:$explicitSignature"
    val (fullName, signature) = typeInfoProvider.fullNameWithSignature(expr, (explicitFullName, explicitSignature))

    // TODO: add test case to confirm whether the ANY fallback makes sense (could be void)
    val returnType = registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
    val node = callNode(
      expr.getText,
      referencedName,
      fullName,
      signature,
      returnType,
      DispatchTypes.STATIC_DISPATCH,
      line(expr),
      column(expr)
    )
    callAst(withArgumentIndex(node, argIdx), argAsts.toList)
  }

  def astForMember(decl: KtDeclaration)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val name = Option(decl.getName).getOrElse(TypeConstants.any)
    val explicitTypeName = decl.getOriginalElement match {
      case p: KtProperty if p.getTypeReference != null => p.getTypeReference.getText
      case _                                           => TypeConstants.any
    }
    val typeFullName = decl match {
      case typed: KtProperty => typeInfoProvider.propertyType(typed, explicitTypeName)
      case _                 => explicitTypeName
    }
    registerType(typeFullName)

    val node = memberNode(name, typeFullName, line(decl), column(decl))
    scope.addToScope(name, node)
    Ast(node)
  }

  def astForParameter(param: KtParameter, order: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val name =
      if (param.getDestructuringDeclaration != null)
        Constants.paramNameLambdaDestructureDecl
      else
        param.getName

    val explicitTypeName = Option(param.getTypeReference).map(_.getText).getOrElse(TypeConstants.any)
    val typeFullName     = registerType(typeInfoProvider.parameterType(param, explicitTypeName))
    val node             = methodParameterNode(name, typeFullName, line(param), column(param)).order(order)
    scope.addToScope(name, node)
    Ast(node)
  }

}
