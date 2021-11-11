package io.joern.c2cpg.astcreation

import io.joern.c2cpg.datastructures.Stack._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.x2cpg.Ast
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.core.dom.ast.cpp._
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTAliasDeclaration
import org.eclipse.cdt.internal.core.model.ASTStringUtil

trait AstForTypesCreator {

  this: AstCreator =>

  private def parentIsClassDef(node: IASTNode): Boolean = Option(node.getParent) match {
    case Some(_: IASTCompositeTypeSpecifier) => true
    case _                                   => false
  }

  private def isTypeDef(decl: IASTSimpleDeclaration): Boolean =
    nodeSignature(decl).startsWith("typedef") ||
      decl.getDeclSpecifier.isInstanceOf[IASTCompositeTypeSpecifier] ||
      decl.getDeclSpecifier.isInstanceOf[IASTEnumerationSpecifier]

  protected def templateParameters(e: IASTNode): Option[String] = {
    val templateDeclaration = e match {
      case _: IASTElaboratedTypeSpecifier | _: IASTFunctionDeclarator | _: IASTCompositeTypeSpecifier
          if e.getParent != null =>
        Some(e.getParent.getParent)
      case _: IASTFunctionDefinition if e.getParent != null =>
        Some(e.getParent)
      case _ => None
    }

    val decl = templateDeclaration.collect { case t: ICPPASTTemplateDeclaration => t }
    val templateParams = decl.map(d => ASTStringUtil.getTemplateParameterArray(d.getTemplateParameters))
    templateParams.map(_.mkString("<", ",", ">"))
  }

  private def astForNamespaceDefinition(namespaceDefinition: ICPPASTNamespaceDefinition, order: Int): Ast = {
    val linenumber = line(namespaceDefinition)
    val columnnumber = column(namespaceDefinition)
    val filename = fileName(namespaceDefinition)

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
      .order(order)

    scope.pushNewScope(cpgNamespace)
    var currOrder = order
    val childrenAsts = namespaceDefinition.getDeclarations.flatMap { decl =>
      val declAsts = astsForDeclaration(decl, currOrder)
      currOrder = currOrder + declAsts.length
      declAsts
    }.toIndexedSeq

    val namespaceAst = Ast(cpgNamespace).withChildren(childrenAsts)
    scope.popScope()
    namespaceAst
  }

  protected def astForNamespaceAlias(namespaceAlias: ICPPASTNamespaceAlias, order: Int): Ast = {
    val linenumber = line(namespaceAlias)
    val columnnumber = column(namespaceAlias)
    val filename = fileName(namespaceAlias)

    val name = namespaceAlias.getAlias.toString
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
      .order(order)

    Ast(cpgNamespace)
  }

  protected def astForDeclarator(declaration: IASTSimpleDeclaration, declarator: IASTDeclarator, order: Int): Ast = {
    val declTypeName = registerType(typeForDeclSpecifier(declaration.getDeclSpecifier))
    val name = declarator.getName.toString
    declaration match {
      case d if isTypeDef(d) =>
        val filename = fileName(declaration)
        Ast(newTypeDecl(name, registerType(name), filename, alias = Some(declTypeName), order = order))
      case d if parentIsClassDef(d) =>
        Ast(
          NewMember()
            .code(nodeSignature(declarator))
            .name(name)
            .typeFullName(declTypeName)
            .order(order))
      case _ if declarator.isInstanceOf[IASTArrayDeclarator] =>
        val tpe = registerType(typeFor(declarator))
        val l = NewLocal()
          .code(name)
          .name(name)
          .typeFullName(tpe)
          .order(order)
        scope.addToScope(name, (l, tpe))
        Ast(l)
      case _ =>
        val tpe = registerType(typeFor(declarator))
        val l = NewLocal()
          .code(name)
          .name(name)
          .typeFullName(tpe)
          .order(order)
        scope.addToScope(name, (l, tpe))
        Ast(l)
    }

  }

  protected def astForInitializer(declarator: IASTDeclarator, init: IASTInitializer, order: Int): Ast = init match {
    case i: IASTEqualsInitializer =>
      val operatorName = Operators.assignment
      val callNode = newCallNode(declarator, operatorName, operatorName, DispatchTypes.STATIC_DISPATCH, order)
      val left = astForNode(declarator.getName, 1)
      val right = astForNode(i.getInitializerClause, 2)
      val ast = Ast(callNode)
        .withChild(left)
        .withArgEdge(callNode, left.root.get)
        .withChild(right)
      right.root match {
        case Some(value) => ast.withArgEdge(callNode, value)
        case None        => ast
      }
    case i: ICPPASTConstructorInitializer =>
      val name = declarator.getName.toString
      val callNode = newCallNode(declarator, name, name, DispatchTypes.STATIC_DISPATCH, order)
      val args = withOrder(i.getArguments) { case (a, o) => astForNode(a, o) }
      val ast = Ast(callNode).withChildren(args)
      val validArgs = args.collect { case a if a.root.isDefined => a.root.get }
      ast.withArgEdges(callNode, validArgs)
    case i: IASTInitializerList =>
      val operatorName = Operators.assignment
      val callNode = newCallNode(declarator, operatorName, operatorName, DispatchTypes.STATIC_DISPATCH, order)
      val left = astForNode(declarator.getName, 1)
      val right = astForNode(i, 2)
      val ast = Ast(callNode)
        .withChild(left)
        .withArgEdge(callNode, left.root.get)
        .withChild(right)
      right.root match {
        case Some(value) => ast.withArgEdge(callNode, value)
        case None        => ast
      }
    case _ => astForNode(init, order)
  }

  protected def handleUsingDeclaration(usingDecl: ICPPASTUsingDeclaration): Unit = {
    val mappedName = lastNameOfQualifiedName(usingDecl.getName.toString)
    // we only do the mapping if the declaration is not global because this is already handled by the parser itself
    if (!isQualifiedName(usingDecl.getName.toString)) {
      usingDecl.getParent match {
        case ns: ICPPASTNamespaceDefinition =>
          usingDeclarationMappings.put(fullName(ns) + "." + mappedName, fixQualifiedName(usingDecl.getName.toString))
        case _ =>
          usingDeclarationMappings.put(mappedName, fixQualifiedName(usingDecl.getName.toString))
      }
    }
  }

  protected def astForAliasDeclaration(aliasDeclaration: ICPPASTAliasDeclaration, order: Int): Ast = {
    val linenumber = line(aliasDeclaration)
    val columnnumber = column(aliasDeclaration)
    val filename = fileName(aliasDeclaration)

    val name = aliasDeclaration.getAlias.toString
    val mappedName = ASTTypeUtil.getType(aliasDeclaration.getMappingTypeId)
    val typeDeclNode =
      newTypeDecl(
        name,
        registerType(name),
        filename,
        alias = Some(registerType(mappedName)),
        line = linenumber,
        column = columnnumber,
        order = order
      )
    Ast(typeDeclNode)
  }

  protected def astForASMDeclaration(asm: IASTASMDeclaration, order: Int): Ast = Ast(newUnknown(asm, order))

  private def astForStructuredBindingDeclaration(structuredBindingDeclaration: ICPPASTStructuredBindingDeclaration,
                                                 order: Int): Ast = {
    val cpgBlock = NewBlock()
      .order(order)
      .argumentIndex(order)
      .typeFullName(registerType(Defines.voidTypeName))
      .lineNumber(line(structuredBindingDeclaration))
      .columnNumber(column(structuredBindingDeclaration))

    scope.pushNewScope(cpgBlock)
    val childAsts = withOrder(structuredBindingDeclaration.getNames) {
      case (name, o) =>
        astForNode(name, o)
    }

    val blockAst = Ast(cpgBlock).withChildren(childAsts)
    scope.popScope()
    blockAst
  }

  protected def astsForDeclaration(decl: IASTDeclaration, order: Int): Seq[Ast] = {
    val declAsts = decl match {
      case u: CPPASTAliasDeclaration => Seq(astForAliasDeclaration(u, order))
      case functDef: IASTFunctionDefinition =>
        Seq(astForFunctionDefinition(functDef, order))
      case declaration: IASTSimpleDeclaration
          if declaration.getDeclSpecifier != null && declaration.getDeclSpecifier
            .isInstanceOf[IASTCompositeTypeSpecifier] =>
        astsForCompositeType(declaration.getDeclSpecifier.asInstanceOf[IASTCompositeTypeSpecifier],
                             declaration.getDeclarators.toList,
                             order)
      case declaration: IASTSimpleDeclaration
          if declaration.getDeclSpecifier != null && declaration.getDeclSpecifier
            .isInstanceOf[IASTEnumerationSpecifier] =>
        astsForEnum(declaration.getDeclSpecifier.asInstanceOf[IASTEnumerationSpecifier],
                    declaration.getDeclarators.toList,
                    order)
      case declaration: IASTSimpleDeclaration
          if declaration.getDeclSpecifier != null && declaration.getDeclSpecifier
            .isInstanceOf[IASTElaboratedTypeSpecifier] =>
        astsForElaboratedType(declaration.getDeclSpecifier.asInstanceOf[IASTElaboratedTypeSpecifier],
                              declaration.getDeclarators.toList,
                              order)

      case declaration: IASTSimpleDeclaration
          if declaration.getDeclSpecifier != null && declaration.getDeclSpecifier
            .isInstanceOf[IASTNamedTypeSpecifier] && declaration.getDeclarators.isEmpty =>
        val spec = declaration.getDeclSpecifier.asInstanceOf[IASTNamedTypeSpecifier]
        val filename = fileName(spec)
        val name = nodeSignature(spec.getName)
        Seq(Ast(newTypeDecl(name, registerType(name), filename, alias = Some(name), order = order)))
      case declaration: IASTSimpleDeclaration if declaration.getDeclarators.nonEmpty =>
        declaration.getDeclarators.toIndexedSeq.map {
          case d: IASTFunctionDeclarator => astForFunctionDeclarator(d, order)
          case d                         => astForDeclarator(declaration, d, order)
        }
      case namespaceAlias: ICPPASTNamespaceAlias           => Seq(astForNamespaceAlias(namespaceAlias, order))
      case namespaceDefinition: ICPPASTNamespaceDefinition => Seq(astForNamespaceDefinition(namespaceDefinition, order))
      case _: ICPPASTVisibilityLabel                       => Seq.empty
      case usingDecl: ICPPASTUsingDeclaration =>
        handleUsingDeclaration(usingDecl)
        Seq.empty
      case _: ICPPASTUsingDirective =>
        Seq.empty
      case _: ICPPASTExplicitTemplateInstantiation             => Seq.empty
      case s: IASTSimpleDeclaration if nodeSignature(s) == ";" => Seq.empty
      case l: ICPPASTLinkageSpecification                      => astsForLinkageSpecification(l)
      case t: ICPPASTTemplateDeclaration                       => astsForDeclaration(t.getDeclaration, order)
      case a: ICPPASTStaticAssertDeclaration                   => Seq(astForStaticAssert(a, order))
      case asm: IASTASMDeclaration                             => Seq(astForASMDeclaration(asm, order))
      case structuredBindingDeclaration: ICPPASTStructuredBindingDeclaration =>
        Seq(astForStructuredBindingDeclaration(structuredBindingDeclaration, order))
      case _ => Seq(astForNode(decl, order))
    }

    val initAsts = decl match {
      case declaration: IASTSimpleDeclaration if declaration.getDeclarators.nonEmpty =>
        withOrder(declaration.getDeclarators) {
          case (d: IASTDeclarator, o) if d.getInitializer != null =>
            astForInitializer(d, d.getInitializer, order + o)
          case _ => Ast()
        }
      case _ => Nil
    }
    declAsts ++ initAsts
  }

  private def astsForLinkageSpecification(l: ICPPASTLinkageSpecification): Seq[Ast] =
    withOrder(l.getDeclarations) {
      case (d, o) =>
        astsForDeclaration(d, o)
    }.flatten

  private def astsForCompositeType(typeSpecifier: IASTCompositeTypeSpecifier,
                                   decls: List[IASTDeclarator],
                                   order: Int): Seq[Ast] = {
    val filename = fileName(typeSpecifier)

    val declAsts = withOrder(decls) { (d, o) =>
      astForDeclarator(typeSpecifier.getParent.asInstanceOf[IASTSimpleDeclaration], d, order + o)
    }

    val name = typeSpecifier.getName.toString
    val fullname = fullName(typeSpecifier)
    val nameWithTemplateParams = templateParameters(typeSpecifier).map(fullname + _)

    val typeDecl = typeSpecifier match {
      case cppClass: ICPPASTCompositeTypeSpecifier =>
        val baseClassList = cppClass.getBaseSpecifiers.toSeq.map(s => fixQualifiedName(s.getNameSpecifier.toString))
        baseClassList.foreach(registerType)
        newTypeDecl(name,
                    registerType(fullname),
                    filename,
                    inherits = baseClassList,
                    alias = nameWithTemplateParams,
                    order = order)
      case _ =>
        newTypeDecl(name, registerType(fullname), filename, alias = nameWithTemplateParams, order = order)
    }

    methodAstParentStack.push(typeDecl)
    scope.pushNewScope(typeDecl)

    val member = withOrder(typeSpecifier.getDeclarations(true)) { (m, o) =>
      astsForDeclaration(m, o)
    }.flatten

    methodAstParentStack.pop()
    scope.popScope()

    Ast(typeDecl).withChildren(member) +: declAsts
  }

  private def astsForElaboratedType(typeSpecifier: IASTElaboratedTypeSpecifier,
                                    decls: List[IASTDeclarator],
                                    order: Int): Seq[Ast] = {
    val filename = fileName(typeSpecifier)
    val declAsts = withOrder(decls) { (d, o) =>
      astForDeclarator(typeSpecifier.getParent.asInstanceOf[IASTSimpleDeclaration], d, order + o)
    }

    val name = typeSpecifier.getName.toString
    val fullname = fullName(typeSpecifier)
    val nameWithTemplateParams = templateParameters(typeSpecifier).map(fullname + _)

    val typeDecl =
      newTypeDecl(name, registerType(fullname), filename, alias = nameWithTemplateParams, order = order)

    Ast(typeDecl) +: declAsts
  }

  private def astsForEnumerator(enumerator: IASTEnumerationSpecifier.IASTEnumerator, order: Int): Seq[Ast] = {
    val tpe = enumerator.getParent match {
      case enumeration: ICPPASTEnumerationSpecifier if enumeration.getBaseType != null =>
        enumeration.getBaseType.toString
      case _ => typeFor(enumerator)
    }
    val cpgMember = NewMember()
      .code(nodeSignature(enumerator))
      .name(enumerator.getName.toString)
      .typeFullName(registerType(tpe))
      .order(order)

    if (enumerator.getValue != null) {
      val operatorName = Operators.assignment
      val callNode = newCallNode(enumerator, operatorName, operatorName, DispatchTypes.STATIC_DISPATCH, order + 1)
      val left = astForNode(enumerator.getName, 1)
      val right = astForNode(enumerator.getValue, 2)

      var ast = Ast(callNode).withChild(left).withChild(right)
      if (left.root.isDefined) ast = ast.withArgEdge(callNode, left.root.get)
      if (right.root.isDefined) ast = ast.withArgEdge(callNode, right.root.get)
      Seq(Ast(cpgMember), ast)
    } else {
      Seq(Ast(cpgMember))
    }

  }

  private def astsForEnum(enumSpecifier: IASTEnumerationSpecifier,
                          decls: List[IASTDeclarator],
                          order: Int): Seq[Ast] = {
    val filename = fileName(enumSpecifier)
    val declAsts = withOrder(decls) { (d, o) =>
      astForDeclarator(enumSpecifier.getParent.asInstanceOf[IASTSimpleDeclaration], d, order + o)
    }

    val (name, fullname) = uniqueName("enum", enumSpecifier.getName.toString, fullName(enumSpecifier))
    val typeDecl = newTypeDecl(name, registerType(fullname), filename, order = order)

    methodAstParentStack.push(typeDecl)
    scope.pushNewScope(typeDecl)

    var currentOrder = 0
    val member = enumSpecifier.getEnumerators.toIndexedSeq.flatMap { e =>
      val eCpg = astsForEnumerator(e, currentOrder)
      currentOrder = eCpg.size + currentOrder
      eCpg
    }
    methodAstParentStack.pop()
    scope.popScope()

    Ast(typeDecl).withChildren(member) +: declAsts
  }

}
