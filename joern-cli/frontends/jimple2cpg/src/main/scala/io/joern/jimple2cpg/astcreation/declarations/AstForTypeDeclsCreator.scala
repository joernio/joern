package io.joern.jimple2cpg.astcreation.declarations

import io.joern.jimple2cpg.astcreation.AstCreator
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.datastructures.Global
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.{Ast, AstCreatorBase, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.objectweb.asm.Type
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder
import soot.jimple.*
import soot.jimple.internal.JimpleLocal
import soot.tagkit.*
import soot.{RefType, Local as _, *}

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}

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

    val typeDecl = NewTypeDecl()
      .name(shortName)
      .fullName(fullName)
      .order(1) // Jimple always has 1 class per file
      .filename(filename)
      .code(code.toString())
      .inheritsFromTypeFullName(inherited ++ implemented)
      .astParentType(NodeTypes.NAMESPACE_BLOCK)
      .astParentFullName(namespaceBlockFullName)
    val methodAsts = withOrder(typ.getSootClass.getMethods.asScala.toList.sortWith((x, y) => x.getName > y.getName)) {
      (m, order) =>
        astForMethod(m, typ, order)
    }

    val memberAsts = typ.getSootClass.getFields.asScala
      .filter(_.isDeclared)
      .zipWithIndex
      .map { case (v, i) =>
        astForField(v, i + methodAsts.size + 1)
      }
      .toList

    Ast(typeDecl)
      .withChildren(astsForHostTags(clz))
      .withChildren(memberAsts)
      .withChildren(methodAsts)
      .withChildren(modifiers)
  }

  protected def astForField(field: SootField, order: Int): Ast = {
    val typeFullName = registerType(field.getType.toQuotedString)
    val name         = field.getName
    val code         = if (field.getDeclaration.contains("enum")) name else s"$typeFullName $name"
    val annotations = field.getTags.asScala
      .collect { case x: VisibilityAnnotationTag => x }
      .flatMap(_.getAnnotations.asScala)

    Ast(
      NewMember()
        .name(name)
        .lineNumber(line(field))
        .columnNumber(column(field))
        .typeFullName(typeFullName)
        .order(order)
        .code(code)
    ).withChildren(withOrder(annotations) { (a, aOrder) => astsForAnnotations(a, aOrder, field) })
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
