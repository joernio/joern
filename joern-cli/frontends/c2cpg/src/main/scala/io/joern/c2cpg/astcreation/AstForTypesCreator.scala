package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.joern.x2cpg.Ast
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.dom.ast.cpp._
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTAliasDeclaration
import org.eclipse.cdt.internal.core.model.ASTStringUtil
import io.joern.x2cpg.datastructures.Stack._

trait AstForTypesCreator {

  this: AstCreator =>

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
    val linenumber   = line(namespaceDefinition)
    val columnnumber = column(namespaceDefinition)
    val filename     = fileName(namespaceDefinition)

    val (name, fullname) =
      uniqueName("namespace", namespaceDefinition.getName.getLastName.toString, fullName(namespaceDefinition))
    val code = "namespace " + fullname
    val cpgNamespace = NewNamespaceBlock()
      .code(code)
      .lineNumber(linenumber)
      .columnNumber(columnnumber)
      .filename(filename)
      .name(name)
      .fullName(fullname)

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
    val linenumber   = line(namespaceAlias)
    val columnnumber = column(namespaceAlias)
    val filename     = fileName(namespaceAlias)

    val name     = ASTStringUtil.getSimpleName(namespaceAlias.getAlias)
    val fullname = fullName(namespaceAlias)

    if (!isQualifiedName(name)) {
      usingDeclarationMappings.put(name, fullname)
    }

    val code = "namespace " + name + " = " + fullname
    val cpgNamespace = NewNamespaceBlock()
      .code(code)
      .lineNumber(linenumber)
      .columnNumber(columnnumber)
      .filename(filename)
      .name(name)
      .fullName(fullname)

    Ast(cpgNamespace)
  }

  protected def astForDeclarator(declaration: IASTSimpleDeclaration, declarator: IASTDeclarator, index: Int): Ast = {
    val name         = ASTStringUtil.getSimpleName(declarator.getName)
    val lineNumber   = line(declarator)
    val columnNumber = column(declarator)
    declaration match {
      case d if isTypeDef(d) =>
        val filename = fileName(declaration)
        val tpe      = registerType(typeFor(declarator))
        Ast(
          newTypeDecl(
            name,
            registerType(name),
            filename,
            nodeSignature(d),
            alias = Some(tpe),
            line = lineNumber,
            column = columnNumber
          )
        )
      case d if parentIsClassDef(d) =>
        val tpe = registerType(typeFor(declaration.getDeclSpecifier))
        Ast(
          NewMember()
            .code(nodeSignature(declarator))
            .name(name)
            .typeFullName(tpe)
            .lineNumber(lineNumber)
            .columnNumber(columnNumber)
        )
      case _ if declarator.isInstanceOf[IASTArrayDeclarator] =>
        val tpe     = registerType(typeFor(declarator))
        val codeTpe = typeFor(declarator, stripKeywords = false)
        val l = NewLocal()
          .code(s"$codeTpe $name")
          .name(name)
          .typeFullName(tpe)
          .lineNumber(lineNumber)
          .columnNumber(columnNumber)
        scope.addToScope(name, (l, tpe))
        Ast(l)
      case _ =>
        val tpe = registerType(
          cleanType(typeForDeclSpecifier(declaration.getDeclSpecifier, stripKeywords = true, index))
        )
        val codeTpe = typeForDeclSpecifier(declaration.getDeclSpecifier, stripKeywords = false, index)
        val l = NewLocal()
          .code(s"$codeTpe $name")
          .name(name)
          .typeFullName(tpe)
          .lineNumber(lineNumber)
          .columnNumber(columnNumber)
        scope.addToScope(name, (l, tpe))
        Ast(l)
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
    val linenumber   = line(aliasDeclaration)
    val columnnumber = column(aliasDeclaration)
    val filename     = fileName(aliasDeclaration)

    val name       = aliasDeclaration.getAlias.toString
    val mappedName = registerType(typeFor(aliasDeclaration.getMappingTypeId))
    val typeDeclNode =
      newTypeDecl(
        name,
        registerType(name),
        filename,
        nodeSignature(aliasDeclaration),
        alias = Some(mappedName),
        line = linenumber,
        column = columnnumber
      )
    Ast(typeDeclNode)
  }

  protected def astForASMDeclaration(asm: IASTASMDeclaration): Ast = Ast(newUnknown(asm))

  private def astForStructuredBindingDeclaration(
    structuredBindingDeclaration: ICPPASTStructuredBindingDeclaration
  ): Ast = {
    val cpgBlock = NewBlock()
      .typeFullName(registerType(Defines.voidTypeName))
      .lineNumber(line(structuredBindingDeclaration))
      .columnNumber(column(structuredBindingDeclaration))

    scope.pushNewScope(cpgBlock)

    val childAsts = structuredBindingDeclaration.getNames.toList.map { name =>
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
            Seq(Ast(newTypeDecl(name, registerType(name), filename, nodeSignature(spec), alias = Some(name))))
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
        withIndex(declaration.getDeclarators) {
          case (d: IASTDeclarator, _) if d.getInitializer != null =>
            astForInitializer(d, d.getInitializer)
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
        newTypeDecl(name, fullname, filename, code, inherits = baseClassList, alias = nameWithTemplateParams)
      case _ =>
        newTypeDecl(name, fullname, filename, code, alias = nameWithTemplateParams)
    }

    methodAstParentStack.push(typeDecl)
    scope.pushNewScope(typeDecl)

    val memberAsts = withIndex(typeSpecifier.getDeclarations(true)) { (m, _) =>
      astsForDeclaration(m)
    }.flatten

    methodAstParentStack.pop()
    scope.popScope()

    val (calls, member) = memberAsts.partition(_.nodes.headOption.exists(_.isInstanceOf[NewCall]))
    if (calls.isEmpty) {
      Ast(typeDecl).withChildren(member) +: declAsts
    } else {
      val init = astForFakeStaticInitMethod(fullname, line(typeSpecifier), NodeTypes.TYPE_DECL, fullname, calls)
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
      newTypeDecl(name, fullname, filename, nodeSignature(typeSpecifier), alias = nameWithTemplateParams)

    Ast(typeDecl) +: declAsts
  }

  private def astsForEnumerator(enumerator: IASTEnumerationSpecifier.IASTEnumerator): Seq[Ast] = {
    val tpe = enumerator.getParent match {
      case enumeration: ICPPASTEnumerationSpecifier if enumeration.getBaseType != null =>
        enumeration.getBaseType.toString
      case _ => typeFor(enumerator)
    }
    val cpgMember = NewMember()
      .code(nodeSignature(enumerator))
      .name(ASTStringUtil.getSimpleName(enumerator.getName))
      .typeFullName(registerType(cleanType(tpe)))

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
    val typeDecl = newTypeDecl(name, registerType(fullname), filename, nodeSignature(typeSpecifier))

    methodAstParentStack.push(typeDecl)
    scope.pushNewScope(typeDecl)

    val memberAsts = typeSpecifier.getEnumerators.toIndexedSeq.flatMap { e =>
      astsForEnumerator(e)
    }
    methodAstParentStack.pop()
    scope.popScope()

    val (calls, member) = memberAsts.partition(_.nodes.headOption.exists(_.isInstanceOf[NewCall]))
    if (calls.isEmpty) {
      Ast(typeDecl).withChildren(member) +: declAsts
    } else {
      val init = astForFakeStaticInitMethod(fullname, line(typeSpecifier), NodeTypes.TYPE_DECL, fullname, calls)
      Ast(typeDecl).withChildren(member).withChild(init) +: declAsts
    }
  }

}
