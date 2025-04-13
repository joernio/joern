package io.joern.php2cpg.astcreation

import io.joern.php2cpg.astcreation.AstCreator.TypeConstants
import io.joern.php2cpg.parser.Domain.*
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

trait AstForTypesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForClassLikeStmt(stmt: PhpClassLikeStmt): Ast = {
    stmt.name match {
      case None       => astForAnonymousClass(stmt)
      case Some(name) => astForNamedClass(stmt, name)
    }
  }

  private def astForAnonymousClass(stmt: PhpClassLikeStmt): Ast = {
    logger.warn(s"Anonymous class encountered. This is not yet supported. Location: $relativeFileName:${line(stmt)}")
    Ast(unknownNode(stmt, code(stmt)))
  }

  private def astForNamedClass(stmt: PhpClassLikeStmt, name: PhpNameExpr): Ast = {
    val inheritsFrom = (stmt.extendsNames ++ stmt.implementedInterfaces).map(_.name)
    val code         = codeForClassStmt(stmt, name)

    val fullName =
      if (name.name == NamespaceTraversal.globalNamespaceName)
        globalNamespace.fullName
      else {
        prependNamespacePrefix(name.name)
      }

    val typeDecl = typeDeclNode(stmt, name.name, fullName, relativeFileName, code, inherits = inheritsFrom)
    val createDefaultConstructor = stmt.hasConstructor

    scope.pushNewScope(typeDecl)
    val bodyStmts      = astsForClassLikeBody(stmt, stmt.stmts, createDefaultConstructor)
    val modifiers      = stmt.modifiers.map(modifierNode(stmt, _)).map(Ast(_))
    val annotationAsts = stmt.attributeGroups.flatMap(astForAttributeGroup)
    scope.popScope()

    Ast(typeDecl).withChildren(modifiers).withChildren(bodyStmts).withChildren(annotationAsts)
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

    val clinitAst           = astForStaticAndConstInits(classLike)
    val anonymousMethodAsts = scope.getAndClearAnonymousMethods

    List(classConsts, properties, clinitAst, constructorAst, anonymousMethodAsts, otherBodyStmts).flatten
  }

  private def codeForClassStmt(stmt: PhpClassLikeStmt, name: PhpNameExpr): String = {
    // TODO Extend for anonymous classes
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
      astForConstOrStaticOrFieldValue(stmt, name, code, someValue, scope.addConstOrStaticInitToScope, isField = false)
        .withChildren(modifierAsts)
    }
  }

  protected def astForEnumCase(stmt: PhpEnumCaseStmt): Ast = {
    val finalModifier = Ast(modifierNode(stmt, ModifierTypes.FINAL))

    val name = stmt.name.name
    val code = s"case $name"

    astForConstOrStaticOrFieldValue(stmt, name, code, stmt.expr, scope.addConstOrStaticInitToScope, isField = false)
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
        astForConstOrStaticOrFieldValue(
          stmt,
          name,
          memberCode,
          varDecl.defaultValue,
          scope.addConstOrStaticInitToScope,
          false
        )
      } else {
        astForConstOrStaticOrFieldValue(stmt, name, s"$$$name", varDecl.defaultValue, scope.addFieldInitToScope, true)
      }

      ast.withChildren(modifierAsts)
    }
  }

  private def astForConstOrStaticOrFieldValue(
    originNode: PhpNode,
    name: String,
    code: String,
    value: Option[PhpExpr],
    addToScope: Ast => Unit,
    isField: Boolean
  ): Ast = {
    val member = memberNode(originNode, name, code, Defines.Any)

    value match {
      case Some(v) =>
        val assignAst = astForMemberAssignment(originNode, member, v, isField)
        addToScope(assignAst)
      case None => // Nothing to do here
    }

    Ast(member)
  }

}
