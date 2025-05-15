package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}
import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.internal.core.dom.parser.cpp.{CPPASTAliasDeclaration, CPPClassType, CPPClosureType}
import org.eclipse.cdt.internal.core.model.ASTStringUtil

trait AstForTypesCreator { this: AstCreator =>

  import FullNameProvider.*

  protected def astForDecltypeSpecifier(decl: ICPPASTDecltypeSpecifier): Ast = {
    val op = Defines.OperatorTypeOf
    val cpgUnary =
      callNode(decl, code(decl), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(registerType(Defines.Any)))
    val operand = nullSafeAst(decl.getDecltypeExpression)
    callAst(cpgUnary, List(operand))
  }

  protected def astForNamespaceAlias(namespaceAlias: ICPPASTNamespaceAlias): Ast = {
    val TypeFullNameInfo(name, fullName) = typeFullNameInfo(namespaceAlias)
    val codeString                       = code(namespaceAlias)
    val filename                         = fileName(namespaceAlias)
    Ast(namespaceBlockNode(namespaceAlias, name, s"$fullName<alias>", filename).code(codeString))
  }

  private def typeForIASTDeclarator(
    declaration: IASTSimpleDeclaration,
    declarator: IASTDeclarator,
    index: Int
  ): String = {
    declarator match {
      case arrayDecl: IASTArrayDeclarator => registerType(typeFor(arrayDecl))
      case _ =>
        safeGetBinding(declarator.getName) match {
          case Some(variable: ICPPVariable) if variable.getType.isInstanceOf[CPPClosureType] =>
            registerType(Defines.Function)
          case _ =>
            registerType(typeForDeclSpecifier(declaration.getDeclSpecifier, index = index))
        }
    }
  }

  protected def astForDeclarator(declaration: IASTSimpleDeclaration, declarator: IASTDeclarator, index: Int): Ast = {
    val name = shortName(declarator)
    declaration match {
      case d if isTypeDef(d) && shortName(d.getDeclSpecifier).nonEmpty =>
        val filename = fileName(declaration)
        val typeDefName = if (name.isEmpty) { safeGetBinding(declarator.getName).map(_.getName).getOrElse("") }
        else { name }
        val tpe                = registerType(typeFor(declarator))
        val (name_, fullName_) = scopeLocalUniqueName(cleanType(typeDefName), fullName(declarator), "")
        Ast(typeDeclNode(declarator, name_, registerType(fullName_), filename, code(d), alias = Option(tpe)))
      case d if parentIsClassDef(d) =>
        val tpe = typeForIASTDeclarator(declaration, declarator, index)
        Ast(memberNode(declarator, name, code(declarator), tpe))
      case d if isAssignmentFromBrokenMacro(d, declarator) && scope.lookupVariable(name).nonEmpty =>
        Ast()
      case _ =>
        val tpe  = typeForIASTDeclarator(declaration, declarator, index)
        val code = codeForDeclarator(declaration, declarator)
        val node = localNode(declarator, name, code, tpe)
        scope.addVariable(name, node, tpe, VariableScopeManager.ScopeType.BlockScope)
        Ast(node)
    }
  }

  private def astForIASTInitializer(
    init: IASTNode,
    declarator: IASTDeclarator,
    leftAst: Ast,
    args: List[Ast],
    name: String,
    tpe: String,
    signature: String,
    fullNameWithSig: String,
    constructorCallName: String,
    initCode: String
  ): Ast = {
    val constructorCallNode = callNode(
      init,
      s"$tpe.$constructorCallName($initCode)",
      tpe,
      fullNameWithSig,
      DispatchTypes.STATIC_DISPATCH,
      Some(signature),
      Some(registerType(Defines.Void))
    )
    val constructorCallAst = callAst(constructorCallNode, args)

    val newCallNode =
      callNode(
        init,
        s"new ${constructorCallNode.code}",
        Defines.OperatorNew,
        Defines.OperatorNew,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Some(tpe)
      )
    val newCallAst = callAst(newCallNode, List(constructorCallAst))

    val assignmentCallNode =
      callNode(
        declarator,
        s"$name = ${newCallNode.code}",
        Operators.assignment,
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Some(registerType(Defines.Void))
      )
    callAst(assignmentCallNode, List(leftAst, newCallAst))
  }

  private def astForICPPASTConstructorInitializer(
    init: ICPPASTConstructorInitializer,
    declarator: IASTDeclarator,
    leftAst: Ast,
    name: String,
    tpe: String
  ): Ast = {
    val assignmentCallNode =
      callNode(
        declarator,
        s"$name = $tpe${code(init)}",
        Operators.assignment,
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Some(registerType(Defines.Void))
      )
    val args = List(leftAst, astForNode(init))
    callAst(assignmentCallNode, args)
  }

  protected def astForInitializer(declarator: IASTDeclarator, init: IASTInitializer): Ast = {
    val name = ASTStringUtil.getSimpleName(declarator.getName)
    val tpe  = registerType(scope.lookupVariable(name).map(_._2.takeWhile(isValidFullNameChar)).getOrElse(Defines.Any))
    val constructorCallName = tpe.split(".").lastOption.getOrElse(tpe)
    val initCode            = code(init).stripPrefix("{").stripSuffix("}").stripPrefix("(").stripSuffix(")")
    val signature           = s"${Defines.Void}(${initializerSignature(init)})"
    val fullNameWithSig     = s"$tpe.$constructorCallName:$signature"
    val leftAst             = astForNode(declarator.getName)

    init match {
      case i: IASTEqualsInitializer =>
        astForIASTEqualsInitializer(declarator, leftAst, astForNode(i.getInitializerClause))
      case i: ICPPASTConstructorInitializer if isFundamentalTypeKeywords(tpe) =>
        astForICPPASTConstructorInitializer(i, declarator, leftAst, name, tpe)
      case i: ICPPASTConstructorInitializer =>
        astForIASTInitializer(
          i,
          declarator,
          leftAst,
          astsForConstructorInitializer(init),
          name,
          tpe,
          signature,
          fullNameWithSig,
          constructorCallName,
          initCode
        )
      case i: IASTInitializerList if isFundamentalTypeKeywords(tpe) =>
        astForIASTEqualsInitializer(declarator, leftAst, astForNode(i))
      case i: IASTInitializerList =>
        astForIASTInitializer(
          i,
          declarator,
          leftAst,
          astsForInitializerClauses(i.getClauses),
          name,
          tpe,
          signature,
          fullNameWithSig,
          constructorCallName,
          initCode
        )
      case _ => astForNode(init)
    }
  }

  protected def astForInitializer(declarator: ICPPASTDeclarator): Ast = {
    val name = ASTStringUtil.getSimpleName(declarator.getName)
    val tpe  = registerType(scope.lookupVariable(name).map(_._2.takeWhile(isValidFullNameChar)).getOrElse(Defines.Any))
    val constructorCallName = tpe.split(".").lastOption.getOrElse(tpe)
    val signature           = s"${Defines.Void}()"
    val fullNameWithSig     = s"$tpe.$constructorCallName:$signature"
    val leftAst             = astForNode(declarator.getName)
    astForIASTInitializer(
      declarator,
      declarator,
      leftAst,
      List.empty,
      name,
      tpe,
      signature,
      fullNameWithSig,
      constructorCallName,
      ""
    )
  }

  private def astForIASTEqualsInitializer(declarator: IASTDeclarator, leftAst: Ast, rightAst: Ast) = {
    val assignmentCallNode = callNode(
      declarator,
      code(declarator),
      Operators.assignment,
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH,
      None,
      Some(registerType(Defines.Void))
    )
    callAst(assignmentCallNode, List(leftAst, rightAst))
  }

  private def initializerSignature(init: IASTInitializer): String = {
    val argTypes = init match {
      case c: ICPPASTConstructorInitializer =>
        c.getArguments.collect { case e: IASTExpression => e }.map(t => cleanType(safeGetType(t.getExpressionType)))
      case list: IASTInitializerList =>
        list.getClauses.collect { case e: IASTExpression => e }.map(t => cleanType(safeGetType(t.getExpressionType)))
      case _ => Array.empty[String]
    }
    StringUtils.normalizeSpace(argTypes.mkString(","))
  }

  protected def astForAliasDeclaration(aliasDeclaration: ICPPASTAliasDeclaration): Ast = {
    val (name, fullName_) = scopeLocalUniqueName(aliasDeclaration.getAlias.toString, fullName(aliasDeclaration), "")
    val mappedName        = registerType(typeFor(aliasDeclaration.getMappingTypeId))
    val typeDeclNode_ =
      typeDeclNode(
        aliasDeclaration,
        name,
        registerType(fullName_),
        fileName(aliasDeclaration),
        code(aliasDeclaration),
        alias = Option(mappedName)
      )
    Ast(typeDeclNode_)
  }

  protected def astForASMDeclaration(asm: IASTASMDeclaration): Ast = Ast(unknownNode(asm, code(asm)))

  private def isCPPClass(decl: IASTSimpleDeclaration): Boolean = {
    decl.getDeclSpecifier match {
      case t: ICPPASTNamedTypeSpecifier =>
        safeGetBinding(t.getName).exists { binding =>
          binding.isInstanceOf[ICPPClassSpecialization] || binding.isInstanceOf[CPPClassType]
        }
      case _ => false
    }
  }

  protected def astsForDeclaration(decl: IASTDeclaration): Seq[Ast] = {
    val declAsts = decl match {
      case sb: ICPPASTStructuredBindingDeclaration => Seq(astForStructuredBindingDeclaration(sb))
      case declaration: IASTSimpleDeclaration =>
        declaration.getDeclSpecifier match {
          case spec: IASTCompositeTypeSpecifier =>
            astsForCompositeType(spec, declaration.getDeclarators.toList)
          case spec: IASTEnumerationSpecifier =>
            astsForEnum(spec, declaration.getDeclarators.toList)
          case spec: IASTElaboratedTypeSpecifier =>
            astsForElaboratedType(spec, declaration.getDeclarators.toList)
          case spec: IASTNamedTypeSpecifier if declaration.getDeclarators.isEmpty =>
            val filename  = fileName(spec)
            val name      = shortName(spec)
            val fullName_ = fullName(spec)
            Seq(Ast(typeDeclNode(spec, name, registerType(fullName_), filename, code(spec), alias = Option(name))))
          case _ if declaration.getDeclarators.nonEmpty =>
            declaration.getDeclarators.toIndexedSeq.zipWithIndex.map {
              case (d: IASTFunctionDeclarator, _) =>
                astForFunctionDeclarator(d)
              case (d: IASTSimpleDeclaration, _) if d.getInitializer != null =>
                Ast() // we do the AST for this down below with initAsts
              case (d, i) =>
                astForDeclarator(declaration, d, i)
            }
          case _ if code(declaration) == ";" =>
            Seq.empty // dangling decls from unresolved macros; we ignore them
          case _ if declaration.getDeclarators.isEmpty && declaration.getParent.isInstanceOf[IASTTranslationUnit] =>
            Seq.empty // dangling decls from unresolved macros; we ignore them
          case _ if declaration.getDeclarators.isEmpty => Seq(astForNode(declaration))
        }
      case alias: CPPASTAliasDeclaration                         => Seq(astForAliasDeclaration(alias))
      case functionDefinition: IASTFunctionDefinition            => Seq(astForFunctionDefinition(functionDefinition))
      case namespaceAlias: ICPPASTNamespaceAlias                 => Seq(astForNamespaceAlias(namespaceAlias))
      case namespaceDefinition: ICPPASTNamespaceDefinition       => Seq(astForNamespaceDefinition(namespaceDefinition))
      case a: ICPPASTStaticAssertDeclaration                     => Seq(astForStaticAssert(a))
      case asm: IASTASMDeclaration                               => Seq(astForASMDeclaration(asm))
      case t: ICPPASTTemplateDeclaration                         => astsForDeclaration(t.getDeclaration)
      case l: ICPPASTLinkageSpecification                        => astsForLinkageSpecification(l)
      case _: ICPPASTUsingDeclaration | _: ICPPASTUsingDirective => Seq.empty // handled by CDT itself
      case _: ICPPASTVisibilityLabel                             => Seq.empty
      case _: ICPPASTExplicitTemplateInstantiation               => Seq.empty
      case _                                                     => Seq(astForNode(decl))
    }

    val initAsts = decl match {
      case declaration: IASTSimpleDeclaration if declaration.getDeclarators.nonEmpty =>
        declaration.getDeclarators.toList.map {
          case d: ICPPASTDeclarator if d.getInitializer == null && isCPPClass(declaration) =>
            astForInitializer(d)
          case d: IASTDeclarator if d.getInitializer != null =>
            astForInitializer(d, d.getInitializer)
          case arrayDecl: IASTArrayDeclarator =>
            val op = Operators.arrayInitializer
            val initCallNode =
              callNode(
                arrayDecl,
                code(arrayDecl),
                op,
                op,
                DispatchTypes.STATIC_DISPATCH,
                None,
                Some(registerType(Defines.Any))
              )
            val initArgs =
              arrayDecl.getArrayModifiers.toList.filter(m => m.getConstantExpression != null).map(astForNode)
            callAst(initCallNode, initArgs)
          case _ => Ast()
        }
      case _ => Nil
    }
    declAsts ++ initAsts
  }

  private def parentIsClassDef(node: IASTNode): Boolean = Option(node.getParent) match {
    case Some(_: IASTCompositeTypeSpecifier) => true
    case _                                   => false
  }

  private def isTypeDef(decl: IASTSimpleDeclaration): Boolean = decl.getRawSignature.startsWith("typedef")

  private def astForNamespaceDefinition(namespaceDefinition: ICPPASTNamespaceDefinition): Ast = {
    val TypeFullNameInfo(name, fullName) = typeFullNameInfo(namespaceDefinition)
    val codeString                       = code(namespaceDefinition)
    val filename                         = fileName(namespaceDefinition)
    val namespaceBlockNode_ = namespaceBlockNode(namespaceDefinition, name, fullName, filename).code(codeString)
    val blockNode_          = blockNode(namespaceDefinition)
    methodAstParentStack.push(blockNode_)
    scope.pushNewMethodScope(fullName, name, namespaceBlockNode_, None)
    scope.pushNewBlockScope(blockNode_)
    val childrenAsts = namespaceDefinition.getDeclarations.flatMap { decl =>
      val declAsts = astsForDeclaration(decl)
      declAsts
    }.toIndexedSeq
    methodAstParentStack.pop()
    scope.popScope()
    scope.popScope()
    setArgumentIndices(childrenAsts)
    Ast(namespaceBlockNode_).withChild(Ast(blockNode_).withChildren(childrenAsts))
  }

  private def isAssignmentFromBrokenMacro(declaration: IASTSimpleDeclaration, declarator: IASTDeclarator): Boolean = {
    declaration.getParent.isInstanceOf[IASTTranslationUnit] &&
    declarator.getInitializer.isInstanceOf[IASTEqualsInitializer]
  }

  private def codeForDeclarator(declaration: IASTSimpleDeclaration, declarator: IASTDeclarator): String = {
    val specCode    = declaration.getDeclSpecifier.getRawSignature
    val declCodeRaw = declarator.getRawSignature
    val declCode = declarator.getInitializer match {
      case null => declCodeRaw
      case _    => declCodeRaw.replace(declarator.getInitializer.getRawSignature, "")
    }
    val normalizedCode = StringUtils.normalizeSpace(s"$specCode $declCode")
    normalizedCode.strip()
  }

  private def astForStructuredBindingDeclaration(decl: ICPPASTStructuredBindingDeclaration): Ast = {
    val node = blockNode(decl)
    scope.pushNewBlockScope(node)
    val childAsts = decl.getNames.toList.map(astForNode)
    scope.popScope()
    setArgumentIndices(childAsts)
    blockAst(node, childAsts)
  }

  private def astsForLinkageSpecification(l: ICPPASTLinkageSpecification): Seq[Ast] = {
    l.getDeclarations.toIndexedSeq.flatMap(astsForDeclaration)
  }

  private def filterNameAlias(nameAlias: Option[String], fullName: String): Option[String] = {
    nameAlias.toList.filter(n => n != fullName).distinct.headOption
  }

  private def astsForCompositeType(typeSpecifier: IASTCompositeTypeSpecifier, decls: List[IASTDeclarator]): Seq[Ast] = {
    val filename                         = fileName(typeSpecifier)
    val TypeFullNameInfo(name, fullName) = typeFullNameInfo(typeSpecifier)
    val codeString                       = code(typeSpecifier)

    val declAsts = decls.zipWithIndex.map { case (d, i) =>
      astForDeclarator(typeSpecifier.getParent.asInstanceOf[IASTSimpleDeclaration], d, i)
    }

    val nameAlias = decls.headOption.map(d => registerType(shortName(d))).filter(_.nonEmpty)
    val alias     = filterNameAlias(nameAlias, fullName)

    val typeDecl = typeSpecifier match {
      case c: ICPPASTCompositeTypeSpecifier =>
        val baseClassList = c.getBaseSpecifiers.map(s => registerType(cleanType(s.getNameSpecifier.toString))).toSeq
        typeDeclNode(c, name, fullName, filename, codeString, inherits = baseClassList, alias = alias)
      case other =>
        typeDeclNode(other, name, fullName, filename, codeString, alias = alias)
    }
    val typeRefNode_ = typeRefNode(typeSpecifier, codeString, fullName)

    methodAstParentStack.push(typeDecl)
    typeRefIdStack.push(typeRefNode_)
    scope.pushNewMethodScope(typeDecl.fullName, typeDecl.name, typeDecl, None)

    val memberAsts = typeSpecifier.getDeclarations(true).toList.flatMap(astsForDeclaration)

    methodAstParentStack.pop()
    typeRefIdStack.pop()
    scope.popScope()

    val (calls, member) = memberAsts.partition(_.nodes.headOption.exists(_.isInstanceOf[NewCall]))
    val asts = if (calls.isEmpty) {
      Ast(typeDecl).withChildren(member) +: declAsts
    } else {
      val init = staticInitMethodAst(
        typeSpecifier,
        calls,
        s"$fullName.${io.joern.x2cpg.Defines.StaticInitMethodName}",
        None,
        Defines.Any,
        Some(filename)
      )
      Ast(typeDecl).withChildren(member).withChild(init) +: declAsts
    }
    asts.foreach { ast =>
      Ast.storeInDiffGraph(ast, diffGraph)
      ast.root.foreach(r => diffGraph.addEdge(methodAstParentStack.head, r, EdgeTypes.AST))
    }
    Seq(Ast(typeRefNode_))
  }

  private def astsForElaboratedType(
    typeSpecifier: IASTElaboratedTypeSpecifier,
    decls: List[IASTDeclarator]
  ): Seq[Ast] = {
    val filename                         = fileName(typeSpecifier)
    val TypeFullNameInfo(name, fullName) = typeFullNameInfo(typeSpecifier)

    val declAsts = decls.zipWithIndex.map { case (d, i) =>
      astForDeclarator(typeSpecifier.getParent.asInstanceOf[IASTSimpleDeclaration], d, i)
    }

    val nameAlias = decls.headOption.map(d => registerType(shortName(d))).filter(_.nonEmpty)
    val alias     = filterNameAlias(nameAlias, fullName)

    val typeDecl = typeDeclNode(typeSpecifier, name, fullName, filename, code(typeSpecifier), alias = alias)
    Ast(typeDecl) +: declAsts
  }

  private def astsForEnumerator(enumerator: IASTEnumerationSpecifier.IASTEnumerator): Seq[Ast] = {
    val tpe = enumerator.getParent match {
      case enumeration: ICPPASTEnumerationSpecifier if enumeration.getBaseType != null =>
        enumeration.getBaseType.toString
      case _ => typeFor(enumerator)
    }
    val cpgMember =
      memberNode(enumerator, ASTStringUtil.getSimpleName(enumerator.getName), code(enumerator), registerType(tpe))

    if (enumerator.getValue != null) {
      val operatorName = Operators.assignment
      val callNode_ = callNode(
        enumerator,
        code(enumerator),
        operatorName,
        operatorName,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Some(registerType(Defines.Void))
      )
      val left  = astForNode(enumerator.getName)
      val right = astForNode(enumerator.getValue)
      val ast   = callAst(callNode_, List(left, right))
      Seq(Ast(cpgMember), ast)
    } else {
      Seq(Ast(cpgMember))
    }
  }

  private def astsForEnum(typeSpecifier: IASTEnumerationSpecifier, decls: List[IASTDeclarator]): Seq[Ast] = {
    val filename                         = fileName(typeSpecifier)
    val codeString                       = code(typeSpecifier)
    val TypeFullNameInfo(name, fullName) = typeFullNameInfo(typeSpecifier)

    val declAsts = decls.zipWithIndex.map { case (d, i) =>
      astForDeclarator(typeSpecifier.getParent.asInstanceOf[IASTSimpleDeclaration], d, i)
    }

    val nameAlias = decls.headOption.map(d => registerType(shortName(d))).filter(_.nonEmpty)
    val alias     = filterNameAlias(nameAlias, fullName)
    val (deAliasedName, deAliasedFullName, newAlias) = if (name.contains("<enum>") && alias.isDefined) {
      (alias.get, fullName.substring(0, fullName.indexOf("<enum>")) + alias.get, None)
    } else { (name, fullName, alias) }

    val typeDecl =
      typeDeclNode(
        typeSpecifier,
        deAliasedName,
        registerType(deAliasedFullName),
        filename,
        codeString,
        alias = newAlias
      )
    val typeRefNode_ = typeRefNode(typeSpecifier, codeString, fullName)

    methodAstParentStack.push(typeDecl)
    typeRefIdStack.push(typeRefNode_)
    scope.pushNewMethodScope(typeDecl.fullName, typeDecl.name, typeDecl, None)
    val memberAsts = typeSpecifier.getEnumerators.toList.flatMap { e =>
      astsForEnumerator(e)
    }
    typeRefIdStack.pop()
    methodAstParentStack.pop()
    scope.popScope()

    val (calls, member) = memberAsts.partition(_.nodes.headOption.exists(_.isInstanceOf[NewCall]))
    val asts = if (calls.isEmpty) {
      Ast(typeDecl).withChildren(member) +: declAsts
    } else {
      val init = staticInitMethodAst(
        typeSpecifier,
        calls,
        s"$deAliasedFullName:${io.joern.x2cpg.Defines.StaticInitMethodName}",
        None,
        Defines.Any,
        Some(filename)
      )
      Ast(typeDecl).withChildren(member).withChild(init) +: declAsts
    }
    asts.foreach { ast =>
      Ast.storeInDiffGraph(ast, diffGraph)
      ast.root.foreach(r => diffGraph.addEdge(methodAstParentStack.head, r, EdgeTypes.AST))
    }
    Seq(Ast(typeRefNode_))
  }

}
