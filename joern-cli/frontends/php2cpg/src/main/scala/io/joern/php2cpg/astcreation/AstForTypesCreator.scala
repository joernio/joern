package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.{NameConstants, TypeConstants}
import io.joern.php2cpg.parser.Domain.*
import io.joern.php2cpg.utils.TypeScope
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBinding,
  NewCall,
  NewFieldIdentifier,
  NewIdentifier,
  NewMember,
  NewMethod,
  NewTypeDecl,
  NewTypeRef
}
import io.shiftleft.codepropertygraph.generated.{
  DispatchTypes,
  EdgeTypes,
  ModifierTypes,
  NodeTypes,
  Operators,
  PropertyDefaults
}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

trait AstForTypesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForClassLikeStmt(stmt: PhpClassLikeStmt): List[Ast] = {
    val (staticStmts, dynamicStmts) = stmt.stmts.partition {
      case x: PhpPropertyStmt if x.modifiers.contains(ModifierTypes.STATIC) => true
      case x: PhpMethodDecl if x.modifiers.contains(ModifierTypes.STATIC)   => true
      case x: PhpConstStmt                                                  => true
      case _                                                                => false
    }

    stmt.name match {
      case None => astForAnonymousClass(stmt, dynamicStmts, staticStmts) :: Nil
      case Some(name) if name.name.contains("anon-class") =>
        astForAnonymousClass(stmt, dynamicStmts, staticStmts) :: Nil
      case Some(name) => astForNamedClass(stmt, name, dynamicStmts, staticStmts)
    }
  }

  protected def astForGlobalDecl(stmt: PhpClassLikeStmt, name: PhpNameExpr): Ast = {
    val inheritsFrom = (stmt.extendsNames ++ stmt.implementedInterfaces).map(_.name)
    val code         = codeForClassStmt(stmt, name)

    val fullName = globalNamespace.fullName

    val typeDecl = typeDeclNode(stmt, name.name, fullName, relativeFileName, code, inherits = inheritsFrom)
    val createDefaultConstructor = stmt.hasConstructor

    scope.pushNewScope(TypeScope(typeDecl, fullName))
    val bodyStmts      = astsForClassLikeBody(stmt, stmt.stmts, createDefaultConstructor)
    val clinitAst      = astForStaticAndConstInits(stmt).getOrElse(Ast())
    val modifiers      = stmt.modifiers.map(modifierNode(stmt, _)).map(Ast(_))
    val annotationAsts = stmt.attributeGroups.flatMap(astForAttributeGroup)
    scope.popScope()

    Ast(typeDecl).withChildren(modifiers).withChildren(bodyStmts :+ clinitAst).withChildren(annotationAsts)
  }

  private def addBindingsForTypeDecl(typeDecl: NewTypeDecl, bodyAsts: List[Ast]): Unit = {
    bodyAsts.flatMap(_.root).collect { case method: NewMethod =>
      val binding = NewBinding().name(method.name).methodFullName(method.fullName)
      diffGraph.addEdge(typeDecl, binding, EdgeTypes.BINDS)
      diffGraph.addEdge(binding, method, EdgeTypes.REF)
    }
  }

  private def astForAnonymousClass(
    stmt: PhpClassLikeStmt,
    dynamicStmts: List[PhpStmt],
    staticStmts: List[PhpStmt]
  ): Ast = {
    val inheritsFrom = (stmt.extendsNames ++ stmt.implementedInterfaces).map(_.name)
    val inheritsFromMeta =
      (stmt.extendsNames ++ stmt.implementedInterfaces).map(name => s"${name.name}$MetaTypeDeclExtension")

    val className = stmt.name match {
      case Some(name) => name.name
      case None       => this.scope.getNewClassTmp
    }

    val classFullName             = prependNamespacePrefix(className)
    val metaTypeDeclClassFullName = s"${classFullName}$MetaTypeDeclExtension"

    val code = codeForClassStmt(stmt, PhpNameExpr(className, stmt.attributes))

    val typeDeclTemp = typeDeclNode(
      node = stmt,
      name = className,
      fullName = classFullName,
      filename = relativeFileName,
      code = code,
      inherits = inheritsFrom,
      alias = None
    )

    val metaTypeDecl = typeDeclNode(
      node = stmt,
      name = s"${className}$MetaTypeDeclExtension",
      fullName = metaTypeDeclClassFullName,
      filename = relativeFileName,
      code = code,
      inherits = inheritsFromMeta,
      alias = None
    )

    scope.surroundingAstLabel.foreach(typeDeclTemp.astParentType(_))
    scope.surroundingScopeFullName.foreach(typeDeclTemp.astParentFullName(_))
    scope.pushNewScope(TypeScope(typeDeclTemp, classFullName))

    val bodyStmts      = astsForClassLikeBody(stmt, dynamicStmts, stmt.hasConstructor)
    val modifiers      = stmt.modifiers.map(modifierNode(stmt, _)).map(Ast(_))
    val annotationAsts = stmt.attributeGroups.flatMap(astForAttributeGroup)

    scope.popScope()

    (scope.surroundingAstLabel, scope.surroundingScopeFullName) match {
      case (Some(astLabel), Some(sfn)) =>
        metaTypeDecl.astParentType(astLabel)
        metaTypeDecl.astParentFullName(sfn)
      case _ =>
        logger.warn(
          s"Expected values for `surroundingAstLabel` and `surroundingScopeFullName` in `astForAnonymousClass`"
        )
    }

    scope.pushNewScope(TypeScope(metaTypeDecl, metaTypeDeclClassFullName))
    val metaTypeDeclAst = astForMetaTypeDecl(stmt, staticStmts, metaTypeDecl)
    scope.popScope()

    if scope.surroundingAstLabel.contains(NodeTypes.TYPE_DECL) then {
      val typeDeclMember = NewMember()
        .name(className)
        .code(className)
        .dynamicTypeHintFullName(Seq(classFullName))

      scope.getEnclosingTypeDeclTypeFullName.foreach { tfn =>
        typeDeclMember.astParentFullName(tfn)
        typeDeclMember.astParentType(NodeTypes.TYPE_DECL)
      }

      val metaTypeDeclMember = NewMember()
        .name(s"${className}$MetaTypeDeclExtension")
        .code(s"${className}$MetaTypeDeclExtension")
        .dynamicTypeHintFullName(Seq(s"${classFullName}${MetaTypeDeclExtension}"))

      scope.getEnclosingTypeDeclTypeFullName.foreach { tfn =>
        metaTypeDeclMember.astParentFullName(s"$tfn$MetaTypeDeclExtension")
        metaTypeDeclMember.astParentType(NodeTypes.TYPE_DECL)
      }

      diffGraph.addNode(typeDeclMember)
      diffGraph.addNode(metaTypeDeclMember)
    }

    addBindingsForTypeDecl(typeDeclTemp, bodyStmts)

    val prefixAst = createTypeRefPointer(typeDeclTemp)
    val typeDeclAst = Ast(typeDeclTemp)
      .withChildren(modifiers)
      .withChildren(bodyStmts)
      .withChildren(annotationAsts)

    Ast.storeInDiffGraph(typeDeclAst, diffGraph)
    Ast.storeInDiffGraph(metaTypeDeclAst, diffGraph)

    prefixAst
  }

  private def createTypeRefPointer(typeDecl: NewTypeDecl): Ast = {
    val typeRefNode = Ast(
      NewTypeRef()
        .code(typeDecl.code)
        .typeFullName(typeDecl.fullName)
        .lineNumber(typeDecl.lineNumber)
        .columnNumber(typeDecl.columnNumber)
    )

    val typeRefIdent = {
      val thisIdent =
        NewIdentifier().name(NameConstants.This).code(s"$$${NameConstants.This}").typeFullName(Defines.Any)
      val fi = NewFieldIdentifier()
        .code(typeDecl.name)
        .canonicalName(typeDecl.name)
        .lineNumber(typeDecl.lineNumber)
        .columnNumber(typeDecl.columnNumber)
      val fieldAccess = NewCall()
        .name(Operators.fieldAccess)
        .code(s"$$this.${typeDecl.name}")
        .methodFullName(Operators.fieldAccess)
        .dispatchType(DispatchTypes.STATIC_DISPATCH)
        .typeFullName(Defines.Any)
      val thisAst = scope
        .lookupVariable(NameConstants.This)
        .map(thisParam => Ast(thisIdent).withRefEdge(thisIdent, thisParam))
        .getOrElse(Ast(thisIdent))
      callAst(fieldAccess, Seq(thisAst, Ast(fi)))
    }

    val assignment = NewCall()
      .name(Operators.assignment)
      .methodFullName(Operators.assignment)
      .code(s"$$this.${typeDecl.name} = ${typeDecl.code}")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(typeDecl.lineNumber)
      .columnNumber(typeDecl.columnNumber)

    callAst(assignment, Seq(typeRefIdent, typeRefNode))
  }

  private def astForNamedClass(
    stmt: PhpClassLikeStmt,
    name: PhpNameExpr,
    dynamicStmts: List[PhpStmt],
    staticStmts: List[PhpStmt]
  ): List[Ast] = {
    val inheritsFrom = (stmt.extendsNames ++ stmt.implementedInterfaces).map(_.name)
    val inheritsFromMeta =
      (stmt.extendsNames ++ stmt.implementedInterfaces).map(name => s"${name.name}$MetaTypeDeclExtension")
    val code = codeForClassStmt(stmt, name)

    val (dedupedName, fullName) =
      if (name.name == NamespaceTraversal.globalNamespaceName)
        (name.name, globalNamespace.fullName)
      else {
        val dedupClassName = scope.getDeduplicatedClassName(name.name)
        (name.name, prependNamespacePrefix(dedupClassName))
      }

    val metaTypeDeclFullName = s"$fullName$MetaTypeDeclExtension"

    val typeDecl = typeDeclNode(stmt, dedupedName, fullName, relativeFileName, code, inherits = inheritsFrom)
    val metaTypeDeclNode = typeDeclNode(
      stmt,
      s"$dedupedName$MetaTypeDeclExtension",
      metaTypeDeclFullName,
      relativeFileName,
      code,
      inherits = inheritsFromMeta
    )

    // Add this to scope for symbol resolution
    scope.useTypeDecl(dedupedName, fullName)

    val createDefaultConstructor = stmt.hasConstructor

    scope.pushNewScope(TypeScope(typeDecl, fullName))

    val bodyStmts      = astsForClassLikeBody(stmt, dynamicStmts, createDefaultConstructor)
    val modifiers      = stmt.modifiers.map(modifierNode(stmt, _)).map(Ast(_))
    val annotationAsts = stmt.attributeGroups.flatMap(astForAttributeGroup)

    // This needs to be carried over for enums
    val staticConsts = if (stmt.classLikeType == ClassLikeTypes.Enum) {
      scope.getConstAndStaticInits
    } else {
      List.empty
    }

    scope.popScope()

    val classTypeDeclAst = Ast(typeDecl).withChildren(modifiers).withChildren(bodyStmts).withChildren(annotationAsts)
    addBindingsForTypeDecl(typeDecl, bodyStmts)

    scope.pushNewScope(TypeScope(metaTypeDeclNode, metaTypeDeclFullName))

    staticConsts.foreach(init => scope.addConstOrStaticInitToScope(init.originNode, init.memberNode, init.value))
    val metaTypeDeclAst = astForMetaTypeDecl(stmt, staticStmts, metaTypeDeclNode)
    scope.popScope()

    List(classTypeDeclAst, metaTypeDeclAst)
  }

  private def astForMetaTypeDecl(
    stmt: PhpClassLikeStmt,
    staticStmts: List[PhpStmt],
    metaTypeDeclNode: NewTypeDecl
  ): Ast = {
    val classConsts = staticStmts.collect { case cs: PhpConstStmt => cs }.flatMap(astsForConstStmt)
    val properties  = staticStmts.collect { case cp: PhpPropertyStmt => cp }.flatMap(astsForPropertyStmt)
    val methodStmts = staticStmts.collect { case mp: PhpMethodDecl => mp }.map(astForMethodDecl(_))

    val clinitAst = astForStaticAndConstInits(stmt)

    val metaTypeDeclBodyStmts = List(classConsts, properties, methodStmts, clinitAst).flatten

    Ast(metaTypeDeclNode).withChildren(metaTypeDeclBodyStmts)
  }

  protected def astsForClassLikeBody(
    classLike: PhpStmt,
    bodyStmts: List[PhpStmt],
    createDefaultConstructor: Boolean
  ): List[Ast] = {

    val classConsts = bodyStmts.collect { case cs: PhpConstStmt => cs }.flatMap(astsForConstStmt)
    val properties  = bodyStmts.collect { case cp: PhpPropertyStmt => cp }.flatMap(astsForPropertyStmt)

    val explicitConstructorAst = bodyStmts.collectFirst {
      case m: PhpMethodDecl if m.name.name == ConstructorMethodName => astForConstructor(m)
    }

    val constructorAst =
      explicitConstructorAst.orElse(Option.when(createDefaultConstructor)(defaultConstructorAst(classLike)))

    val otherBodyStmts = bodyStmts.flatMap {
      case _: PhpConstStmt => Nil // Handled above

      case _: PhpPropertyStmt => Nil // Handled above

      case method: PhpMethodDecl if method.name.name == ConstructorMethodName => Nil // Handled above

      // Not all statements are supported in class bodies, but since this is re-used for namespaces
      // we allow that here.
      case stmt => astsForStmt(stmt)
    }

    val anonymousMethodAsts = scope.getAndClearAnonymousMethods

    List(classConsts, properties, constructorAst, anonymousMethodAsts, otherBodyStmts).flatten
  }

  private def codeForClassStmt(stmt: PhpClassLikeStmt, name: PhpNameExpr): String = {
    val extendsString = stmt.extendsNames match {
      case Nil   => ""
      case names => s" extends ${names.map(_.name).mkString(", ")}"
    }
    val implementsString =
      if (stmt.implementedInterfaces.isEmpty)
        ""
      else
        s" implements ${stmt.implementedInterfaces.map(_.name).mkString(", ")}"

    s"${stmt.classLikeType} ${name.name}$extendsString$implementsString"
  }

  private def astsForConstStmt(stmt: PhpConstStmt): List[Ast] = {
    stmt.consts.map { constDecl =>
      val finalModifier = Ast(modifierNode(stmt, ModifierTypes.FINAL))
      // `final const` is not allowed, so this is a safe way to represent constants in the CPG
      val modifierAsts = finalModifier :: stmt.modifiers.map(modifierNode(stmt, _)).map(Ast(_))

      val name      = constDecl.name.name
      val code      = s"const $name"
      val someValue = Option(constDecl.value)
      astForConstOrStaticOrFieldValue(stmt, name, code, someValue, scope.addConstOrStaticInitToScope)
        .withChildren(modifierAsts)
    }
  }

  protected def astForEnumCase(stmt: PhpEnumCaseStmt): Ast = {
    val finalModifier = Ast(modifierNode(stmt, ModifierTypes.FINAL))

    val name = stmt.name.name
    val code = s"case $name"

    astForConstOrStaticOrFieldValue(stmt, name, code, stmt.expr, scope.addConstOrStaticInitToScope)
      .withChild(finalModifier)
  }

  private def astsForPropertyStmt(stmt: PhpPropertyStmt): List[Ast] = {
    stmt.variables.map { varDecl =>
      val modifiers    = stmt.modifiers
      val modifierAsts = modifiers.map(modifierNode(stmt, _)).map(Ast(_))

      val name = varDecl.name.name
      val ast = if (modifiers.contains(ModifierTypes.STATIC)) {
        // A static member belongs to a class, not an instance
        val memberCode = s"static $$$name"
        astForConstOrStaticOrFieldValue(stmt, name, memberCode, varDecl.defaultValue, scope.addConstOrStaticInitToScope)
      } else {
        astForConstOrStaticOrFieldValue(stmt, name, s"$$$name", varDecl.defaultValue, scope.addFieldInitToScope)
      }

      ast.withChildren(modifierAsts)
    }
  }

  private def astForConstOrStaticOrFieldValue(
    originNode: PhpNode,
    name: String,
    code: String,
    value: Option[PhpExpr],
    addToScope: (PhpNode, NewMember, PhpExpr) => Unit
  ): Ast = {
    val member = memberNode(originNode, name, code, Defines.Any)

    value match {
      case Some(v) =>
        addToScope(originNode, member, v)
      case None => // Nothing to do here
    }

    Ast(member)
  }

}
