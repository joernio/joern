package io.joern.jimple2cpg.astcreation.declarations

import io.joern.jimple2cpg.astcreation.AstCreator
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import sootup.core.types.ClassType
import sootup.java.core.types.JavaClassType
import sootup.java.core.{JavaSootClass, JavaSootField}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

trait AstForTypeDeclsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  /** Creates the AST root for type declarations and acts as the entry point for method generation.
    */
  protected def astForTypeDecl(typ: JavaSootClass, namespaceBlockFullName: String): Ast = {
    val clz       = typ.getType
    val fullName  = registerType(clz.getFullyQualifiedName)
    val shortName = clz.getClassName

    val code = new mutable.StringBuilder()

    if (typ.isPublic) code.append("public ")
    else if (typ.isPrivate) code.append("private ")
    else if (typ.isProtected) code.append("protected ")
    if (typ.isStatic) code.append("static ")
    if (typ.isFinal) code.append("final ")
    if (typ.isInterface) code.append("interface ")
    else if (typ.isAbstract) code.append("abstract ")
    if (typ.isEnum) code.append("enum ")
    if (!typ.isInterface) code.append(s"class $shortName")
    else code.append(shortName)

    val modifiers                = astsForModifiers(clz)
    val (inherited, implemented) = inheritedAndImplementedClasses(typ)

    if (inherited.nonEmpty) code.append(s" extends ${inherited.mkString(", ")}")
    if (implemented.nonEmpty) code.append(s" implements ${implemented.mkString(", ")}")

    val typeDecl = typeDeclNode(
      typ,
      shortName,
      fullName,
      filename,
      code.toString(),
      NodeTypes.NAMESPACE_BLOCK,
      namespaceBlockFullName,
      inherited ++ implemented
    )

    val methodAsts = typ.getMethods.asScala.map(astForMethod(_, typ)).toList
    val memberAsts = typ.getFields.asScala.map(astForField).toList

    Ast(typeDecl)
      .withChildren(astsForHostTags(clz))
      .withChildren(memberAsts)
      .withChildren(methodAsts)
      .withChildren(modifiers)
  }

  private def astForField(field: JavaSootField): Ast = {
    val typeFullName = registerType(field.getType)
    val name         = field.getName
    val code         = if (field.getDeclaration.contains("enum")) name else s"$typeFullName $name"
    val annotations = field
      .getAnnotations(Option(view).toJava)
      .asScala

    Ast(memberNode(field, name, code, typeFullName))
      .withChildren(annotations.map(astsForAnnotations(_, field)).toSeq)
  }

  /** Creates a list of all inherited classes and implemented interfaces. If there are none then a list with a single
    * element 'java.lang.Object' is returned by default. Returns two lists in the form of (List[Super Classes],
    * List[Interfaces]).
    */
  private def inheritedAndImplementedClasses(clazz: JavaSootClass): (List[String], List[String]) = {
    val implementsTypeFullName = clazz.getInterfaces.asScala.map(i => registerType(i.getFullyQualifiedName)).toList
    val inheritsFromTypeFullName =
      clazz.getSuperclass.toScala.map(_.getFullyQualifiedName) match
        case Some(superClass) if superClass != "java.lang.Object" => registerType(superClass) :: Nil
        case _ if implementsTypeFullName.isEmpty                  => registerType("java.lang.Object") :: Nil
        case _                                                    => Nil

    (inheritsFromTypeFullName, implementsTypeFullName)
  }

}
