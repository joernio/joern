package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.joern.x2cpg.Ast
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.dom.ast.cpp._
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTAliasDeclaration
import org.eclipse.cdt.internal.core.model.ASTStringUtil
import io.joern.x2cpg.datastructures.Stack._

trait AstForTypesCreator { this: AstCreator =>

  private def parentIsClassDef(node: IASTNode): Boolean = Option(node.getParent) match {
    case Some(_: IASTCompositeTypeSpecifier) => true
    case _                                   => false
  }

  private def isTypeDef(decl: IASTSimpleDeclaration): Boolean =
    nodeSignature(decl).startsWith("typedef")

  protected def templateParameters(e: IASTNode): Option[String] = {
    val templateDeclaration = e match {
      case _: IASTElaboratedTypeSpecifier | _: IASTFunctionDeclarator | _: IASTCompositeTypeSpecifier
          if e.getParent != null =>
        Some(e.getParent.getParent)
      case _: IASTFunctionDefinition if e.getParent != null =>
        Some(e.getParent)
      case _ => None
    }

    val decl           = templateDeclaration.collect { case t: ICPPASTTemplateDeclaration => t }
    val templateParams = decl.map(d => ASTStringUtil.getTemplateParameterArray(d.getTemplateParameters))
    templateParams.map(_.mkString("<", ",", ">"))
  }

  private def astForNamespaceDefinition(namespaceDefinition: ICPPASTNamespaceDefinition): Ast = {
    val (name, fullname) =
      uniqueName("namespace", namespaceDefinition.getName.getLastName.toString, fullName(namespaceDefinition))
    val code         = "namespace " + fullname
    val cpgNamespace = newNamespaceBlockNode(namespaceDefinition, name, fullname, code, fileName(namespaceDefinition))
    scope.pushNewScope(cpgNamespace)

    val childrenAsts = namespaceDefinition.getDeclarations.flatMap { decl =>
      val declAsts = astsForDeclaration(decl)
      declAsts
    }.toIndexedSeq

    val namespaceAst = Ast(cpgNamespace).withChildren(childrenAsts)
    scope.popScope()
    namespaceAst
  }

  protected def astForNamespaceAlias(namespaceAlias: ICPPASTNamespaceAlias): Ast = {
    val name     = ASTStringUtil.getSimpleName(namespaceAlias.getAlias)
    val fullname = fullName(namespaceAlias)

    if (!isQualifiedName(name)) {
      usingDeclarationMappings.put(name, fullname)
    }

    val code         = "namespace " + name + " = " + fullname
    val cpgNamespace = newNamespaceBlockNode(namespaceAlias, name, fullname, code, fileName(namespaceAlias))
    Ast(cpgNamespace)
  }

  protected def astForDeclarator(declaration: IASTSimpleDeclaration, declarator: IASTDeclarator, index: Int): Ast = {
    val name = ASTStringUtil.getSimpleName(declarator.getName)
    declaration match {
      case d if isTypeDef(d) =>
        val filename = fileName(declaration)
        val tpe      = registerType(typeFor(declarator))
        Ast(newTypeDeclNode(declarator, name, registerType(name), filename, nodeSignature(d), alias = Some(tpe)))
      case d if parentIsClassDef(d) =>
        val tpe = registerType(typeFor(declaration.getDeclSpecifier))
        Ast(newMemberNode(declarator, name, nodeSignature(declarator), tpe))
      case _ if declarator.isInstanceOf[IASTArrayDeclarator] =>
        val tpe       = registerType(typeFor(declarator))
        val codeTpe   = typeFor(declarator, stripKeywords = false)
        val localNode = newLocalNode(declarator, name, s"$codeTpe $name", tpe)
        scope.addToScope(name, (localNode, tpe))
        Ast(localNode)
      case _ =>
        val tpe = registerType(
          cleanType(typeForDeclSpecifier(declaration.getDeclSpecifier, stripKeywords = true, index))
        )
        val codeTpe   = typeForDeclSpecifier(declaration.getDeclSpecifier, stripKeywords = false, index)
        val localNode = newLocalNode(declarator, name, s"$codeTpe $name", tpe)
        scope.addToScope(name, (localNode, tpe))
        Ast(localNode)
    }

  }

  protected def astForInitializer(declarator: IASTDeclarator, init: IASTInitializer): Ast = init match {
    case i: IASTEqualsInitializer =>
      val operatorName = Operators.assignment
      val callNode     = newCallNode(declarator, operatorName, operatorName, DispatchTypes.STATIC_DISPATCH)
      val left         = astForNode(declarator.getName)
      val right        = astForNode(i.getInitializerClause)
      callAst(callNode, List(left, right))
    case i: ICPPASTConstructorInitializer =>
      val name     = ASTStringUtil.getSimpleName(declarator.getName)
      val callNode = newCallNode(declarator, name, name, DispatchTypes.STATIC_DISPATCH)
      val args     = i.getArguments.toList.map(x => astForNode(x))
      callAst(callNode, args)
    case i: IASTInitializerList =>
      val operatorName = Operators.assignment
      val callNode     = newCallNode(declarator, operatorName, operatorName, DispatchTypes.STATIC_DISPATCH)
      val left         = astForNode(declarator.getName)
      val right        = astForNode(i)
      callAst(callNode, List(left, right))
    case _ => astForNode(init)
  }

  protected def handleUsingDeclaration(usingDecl: ICPPASTUsingDeclaration): Seq[Ast] = {
    val simpleName = ASTStringUtil.getSimpleName(usingDecl.getName)
    val mappedName = lastNameOfQualifiedName(simpleName)
    // we only do the mapping if the declaration is not global because this is already handled by the parser itself
    if (!isQualifiedName(simpleName)) {
      usingDecl.getParent match {
        case ns: ICPPASTNamespaceDefinition =>
          usingDeclarationMappings.put(fullName(ns) + "." + mappedName, fixQualifiedName(simpleName))
        case _ =>
          usingDeclarationMappings.put(mappedName, fixQualifiedName(simpleName))
      }
    }
    Seq.empty
  }

  protected def astForAliasDeclaration(aliasDeclaration: ICPPASTAliasDeclaration): Ast = {
    val name       = aliasDeclaration.getAlias.toString
    val mappedName = registerType(typeFor(aliasDeclaration.getMappingTypeId))
    val typeDeclNode =
      newTypeDeclNode(
        aliasDeclaration,
        name,
        registerType(name),
        fileName(aliasDeclaration),
        nodeSignature(aliasDeclaration),
        alias = Some(mappedName)
      )
    Ast(typeDeclNode)
  }

  protected def astForASMDeclaration(asm: IASTASMDeclaration): Ast = Ast(newUnknownNode(asm))

  private def astForStructuredBindingDeclaration(decl: ICPPASTStructuredBindingDeclaration): Ast = {
    val cpgBlock = newBlockNode(decl, Defines.voidTypeName)
    scope.pushNewScope(cpgBlock)
    val childAsts = decl.getNames.toList.map { name =>
      astForNode(name)
    }
    val blockAst = Ast(cpgBlock).withChildren(childAsts)
    scope.popScope()
    blockAst
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
            val filename = fileName(spec)
            val name     = ASTStringUtil.getSimpleName(spec.getName)
            Seq(Ast(newTypeDeclNode(spec, name, registerType(name), filename, nodeSignature(spec), alias = Some(name))))
          case _ if declaration.getDeclarators.nonEmpty =>
            declaration.getDeclarators.toIndexedSeq.zipWithIndex.map {
              case (d: IASTFunctionDeclarator, _) =>
                astForFunctionDeclarator(d)
              case (d: IASTSimpleDeclaration, _) if d.getInitializer != null =>
                Ast() // we do the AST for this down below with initAsts
              case (d, i) =>
                astForDeclarator(declaration, d, i)
            }
          case _ if nodeSignature(declaration) == ";" =>
            Seq.empty // dangling decls from unresolved macros; we ignore them
          case _ if declaration.getDeclarators.isEmpty && declaration.getParent.isInstanceOf[IASTTranslationUnit] =>
            Seq.empty // dangling decls from unresolved macros; we ignore them
          case _ if declaration.getDeclarators.isEmpty => Seq(astForNode(declaration))
        }
      case alias: CPPASTAliasDeclaration                   => Seq(astForAliasDeclaration(alias))
      case functDef: IASTFunctionDefinition                => Seq(astForFunctionDefinition(functDef))
      case namespaceAlias: ICPPASTNamespaceAlias           => Seq(astForNamespaceAlias(namespaceAlias))
      case namespaceDefinition: ICPPASTNamespaceDefinition => Seq(astForNamespaceDefinition(namespaceDefinition))
      case a: ICPPASTStaticAssertDeclaration               => Seq(astForStaticAssert(a))
      case asm: IASTASMDeclaration                         => Seq(astForASMDeclaration(asm))
      case t: ICPPASTTemplateDeclaration                   => astsForDeclaration(t.getDeclaration)
      case l: ICPPASTLinkageSpecification                  => astsForLinkageSpecification(l)
      case u: ICPPASTUsingDeclaration                      => handleUsingDeclaration(u)
      case _: ICPPASTVisibilityLabel                       => Seq.empty
      case _: ICPPASTUsingDirective                        => Seq.empty
      case _: ICPPASTExplicitTemplateInstantiation         => Seq.empty
      case _                                               => Seq(astForNode(decl))
    }

    val initAsts = decl match {
      case declaration: IASTSimpleDeclaration if declaration.getDeclarators.nonEmpty =>
        declaration.getDeclarators.toList.map {
          case d: IASTDeclarator if d.getInitializer != null =>
            astForInitializer(d, d.getInitializer)
          case arrayDecl: IASTArrayDeclarator =>
            val op           = Operators.arrayInitializer
            val initCallNode = newCallNode(arrayDecl, op, op, DispatchTypes.STATIC_DISPATCH)
            val initArgs =
              arrayDecl.getArrayModifiers.toList.filter(m => m.getConstantExpression != null).map(astForNode)
            callAst(initCallNode, initArgs)
          case _ => Ast()
        }
      case _ => Nil
    }
    declAsts ++ initAsts
  }

  private def astsForLinkageSpecification(l: ICPPASTLinkageSpecification): Seq[Ast] =
    l.getDeclarations.toList.flatMap { d =>
      astsForDeclaration(d)
    }

  private def astsForCompositeType(typeSpecifier: IASTCompositeTypeSpecifier, decls: List[IASTDeclarator]): Seq[Ast] = {
    val filename = fileName(typeSpecifier)
    val declAsts = decls.zipWithIndex.map { case (d, i) =>
      astForDeclarator(typeSpecifier.getParent.asInstanceOf[IASTSimpleDeclaration], d, i)
    }

    val name                   = ASTStringUtil.getSimpleName(typeSpecifier.getName)
    val fullname               = registerType(cleanType(fullName(typeSpecifier)))
    val code                   = nodeSignature(typeSpecifier)
    val nameWithTemplateParams = templateParameters(typeSpecifier).map(t => registerType(fullname + t))

    val typeDecl = typeSpecifier match {
      case cppClass: ICPPASTCompositeTypeSpecifier =>
        val baseClassList =
          cppClass.getBaseSpecifiers.toSeq.map(s => registerType(s.getNameSpecifier.toString))
        newTypeDeclNode(
          typeSpecifier,
          name,
          fullname,
          filename,
          code,
          inherits = baseClassList,
          alias = nameWithTemplateParams
        )
      case _ =>
        newTypeDeclNode(typeSpecifier, name, fullname, filename, code, alias = nameWithTemplateParams)
    }

    methodAstParentStack.push(typeDecl)
    scope.pushNewScope(typeDecl)

    val memberAsts = typeSpecifier.getDeclarations(true).toList.flatMap(astsForDeclaration)

    methodAstParentStack.pop()
    scope.popScope()

    val (calls, member) = memberAsts.partition(_.nodes.headOption.exists(_.isInstanceOf[NewCall]))
    if (calls.isEmpty) {
      Ast(typeDecl).withChildren(member) +: declAsts
    } else {
      val init = staticInitMethodAst(
        calls,
        s"$fullname:${io.joern.x2cpg.Defines.StaticInitMethodName}",
        None,
        Defines.anyTypeName
      )
      Ast(typeDecl).withChildren(member).withChild(init) +: declAsts
    }
  }

  private def astsForElaboratedType(
    typeSpecifier: IASTElaboratedTypeSpecifier,
    decls: List[IASTDeclarator]
  ): Seq[Ast] = {
    val filename = fileName(typeSpecifier)
    val declAsts = decls.zipWithIndex.map { case (d, i) =>
      astForDeclarator(typeSpecifier.getParent.asInstanceOf[IASTSimpleDeclaration], d, i)
    }

    val name                   = ASTStringUtil.getSimpleName(typeSpecifier.getName)
    val fullname               = registerType(cleanType(fullName(typeSpecifier)))
    val nameWithTemplateParams = templateParameters(typeSpecifier).map(t => registerType(fullname + t))

    val typeDecl =
      newTypeDeclNode(
        typeSpecifier,
        name,
        fullname,
        filename,
        nodeSignature(typeSpecifier),
        alias = nameWithTemplateParams
      )

    Ast(typeDecl) +: declAsts
  }

  private def astsForEnumerator(enumerator: IASTEnumerationSpecifier.IASTEnumerator): Seq[Ast] = {
    val tpe = enumerator.getParent match {
      case enumeration: ICPPASTEnumerationSpecifier if enumeration.getBaseType != null =>
        enumeration.getBaseType.toString
      case _ => typeFor(enumerator)
    }
    val cpgMember = newMemberNode(
      enumerator,
      ASTStringUtil.getSimpleName(enumerator.getName),
      nodeSignature(enumerator),
      registerType(cleanType(tpe))
    )

    if (enumerator.getValue != null) {
      val operatorName = Operators.assignment
      val callNode     = newCallNode(enumerator, operatorName, operatorName, DispatchTypes.STATIC_DISPATCH)
      val left         = astForNode(enumerator.getName)
      val right        = astForNode(enumerator.getValue)
      val ast          = callAst(callNode, List(left, right))
      Seq(Ast(cpgMember), ast)
    } else {
      Seq(Ast(cpgMember))
    }

  }

  private def astsForEnum(typeSpecifier: IASTEnumerationSpecifier, decls: List[IASTDeclarator]): Seq[Ast] = {
    val filename = fileName(typeSpecifier)
    val declAsts = decls.zipWithIndex.map { case (d, i) =>
      astForDeclarator(typeSpecifier.getParent.asInstanceOf[IASTSimpleDeclaration], d, i)
    }

    val (name, fullname) =
      uniqueName("enum", ASTStringUtil.getSimpleName(typeSpecifier.getName), fullName(typeSpecifier))
    val typeDecl = newTypeDeclNode(typeSpecifier, name, registerType(fullname), filename, nodeSignature(typeSpecifier))

    methodAstParentStack.push(typeDecl)
    scope.pushNewScope(typeDecl)

    val memberAsts = typeSpecifier.getEnumerators.toList.flatMap { e =>
      astsForEnumerator(e)
    }
    methodAstParentStack.pop()
    scope.popScope()

    val (calls, member) = memberAsts.partition(_.nodes.headOption.exists(_.isInstanceOf[NewCall]))
    if (calls.isEmpty) {
      Ast(typeDecl).withChildren(member) +: declAsts
    } else {
      val init = staticInitMethodAst(
        calls,
        s"$fullname:${io.joern.x2cpg.Defines.StaticInitMethodName}",
        None,
        Defines.anyTypeName
      )
      Ast(typeDecl).withChildren(member).withChild(init) +: declAsts
    }
  }

}
