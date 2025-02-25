package io.joern.jimple2cpg.astcreation.declarations

import io.joern.jimple2cpg.astcreation.AstCreator
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import soot.tagkit.*
import soot.{RefType, Local as _, *}

import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala

trait AstForTypeDeclsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  /** Creates the AST root for type declarations and acts as the entry point for method generation.
    */
  protected def astForTypeDecl(typ: RefType, namespaceBlockFullName: String): Ast = {
    val fullName  = registerType(typ.toQuotedString)
    val shortName = typ.getSootClass.getShortJavaStyleName
    val clz       = typ.getSootClass
    val code      = new mutable.StringBuilder()

    if (clz.isPublic) code.append("public ")
    else if (clz.isPrivate) code.append("private ")
    else if (clz.isProtected) code.append("protected ")
    if (clz.isStatic) code.append("static ")
    if (clz.isFinal) code.append("final ")
    if (clz.isInterface) code.append("interface ")
    else if (clz.isAbstract) code.append("abstract ")
    if (clz.isEnum) code.append("enum ")
    if (!clz.isInterface) code.append(s"class $shortName")
    else code.append(shortName)

    val modifiers                = astsForModifiers(clz)
    val (inherited, implemented) = inheritedAndImplementedClasses(typ.getSootClass)

    if (inherited.nonEmpty) code.append(s" extends ${inherited.mkString(", ")}")
    if (implemented.nonEmpty) code.append(s" implements ${implemented.mkString(", ")}")

    val typeDecl = typeDeclNode(
      typ.getSootClass,
      shortName,
      fullName,
      filename,
      code.toString(),
      NodeTypes.NAMESPACE_BLOCK,
      namespaceBlockFullName,
      inherited ++ implemented
    )

    val methodAsts = clz.getMethods.asScala.map(astForMethod(_, typ)).toList
    val memberAsts = clz.getFields.asScala.filter(_.isDeclared).map(astForField).toList

    Ast(typeDecl)
      .withChildren(astsForHostTags(clz))
      .withChildren(memberAsts)
      .withChildren(methodAsts)
      .withChildren(modifiers)
  }

  private def astForField(field: SootField): Ast = {
    val typeFullName = registerType(field.getType.toQuotedString)
    val name         = field.getName
    val code         = if (field.getDeclaration.contains("enum")) name else s"$typeFullName $name"
    val annotations = field.getTags.asScala
      .collect { case x: VisibilityAnnotationTag => x }
      .flatMap(_.getAnnotations.asScala)
    val modifiers = astsForModifiers(field)
    Ast(memberNode(field, name, code, typeFullName))
      .withChildren(annotations.map(astsForAnnotations(_, field)).toSeq)
      .withChildren(modifiers)
  }

  /** Creates a list of all inherited classes and implemented interfaces. If there are none then a list with a single
    * element 'java.lang.Object' is returned by default. Returns two lists in the form of (List[Super Classes],
    * List[Interfaces]).
    */
  private def inheritedAndImplementedClasses(clazz: SootClass): (List[String], List[String]) = {
    val implementsTypeFullName = clazz.getInterfaces.asScala.map { (i: SootClass) =>
      registerType(i.getType.toQuotedString)
    }.toList
    val inheritsFromTypeFullName =
      if (clazz.hasSuperclass && clazz.getSuperclass.getType.toQuotedString != "java.lang.Object") {
        List(registerType(clazz.getSuperclass.getType.toQuotedString))
      } else if (implementsTypeFullName.isEmpty) {
        List(registerType("java.lang.Object"))
      } else List()

    (inheritsFromTypeFullName, implementsTypeFullName)
  }

}
