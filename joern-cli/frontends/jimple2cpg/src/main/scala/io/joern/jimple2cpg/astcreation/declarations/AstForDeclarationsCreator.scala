package io.joern.jimple2cpg.astcreation.declarations

import io.joern.jimple2cpg.astcreation.{AstCreator, JvmStringOpts}
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, PropertyNames}
import soot.{SootClass, SootMethod}
import soot.tagkit.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala

trait AstForDeclarationsCreator(implicit withSchemaValidation: ValidationMode)
    extends AstForTypeDeclsCreator
    with AstForMethodsCreator { this: AstCreator =>

  protected def astsForModifiers(classDeclaration: SootClass): Seq[Ast] = {
    Seq(
      if (classDeclaration.isStatic) Some(ModifierTypes.STATIC) else None,
      if (classDeclaration.isPublic) Some(ModifierTypes.PUBLIC) else None,
      if (classDeclaration.isProtected) Some(ModifierTypes.PROTECTED) else None,
      if (classDeclaration.isPrivate) Some(ModifierTypes.PRIVATE) else None,
      if (classDeclaration.isAbstract) Some(ModifierTypes.ABSTRACT) else None,
      if (classDeclaration.isInterface) Some("INTERFACE") else None,
      if (!classDeclaration.isFinal && !classDeclaration.isStatic && classDeclaration.isPublic)
        Some(ModifierTypes.VIRTUAL)
      else None,
      if (classDeclaration.isSynchronized) Some("SYNCHRONIZED") else None
    ).flatten.map(m => Ast(NodeBuilders.newModifierNode(m).code(m.toLowerCase)))
  }

  protected def astsForModifiers(methodDeclaration: SootMethod): Seq[NewModifier] = {
    Seq(
      if (methodDeclaration.isStatic) Some(ModifierTypes.STATIC) else None,
      if (methodDeclaration.isPublic) Some(ModifierTypes.PUBLIC) else None,
      if (methodDeclaration.isProtected) Some(ModifierTypes.PROTECTED) else None,
      if (methodDeclaration.isPrivate) Some(ModifierTypes.PRIVATE) else None,
      if (methodDeclaration.isAbstract) Some(ModifierTypes.ABSTRACT) else None,
      if (methodDeclaration.isConstructor) Some(ModifierTypes.CONSTRUCTOR) else None,
      if (!methodDeclaration.isFinal && !methodDeclaration.isStatic && methodDeclaration.isPublic)
        Some(ModifierTypes.VIRTUAL)
      else None,
      if (methodDeclaration.isSynchronized) Some("SYNCHRONIZED") else None
    ).flatten.map(m => NodeBuilders.newModifierNode(m).code(m.toLowerCase))
  }

  protected def astsForHostTags(host: AbstractHost): Seq[Ast] = {
    host.getTags.asScala
      .collect { case x: VisibilityAnnotationTag => x }
      .flatMap(_.getAnnotations.asScala.map(a => astsForAnnotations(a, host)))
      .toSeq
  }

  protected def astsForAnnotations(annotationTag: AnnotationTag, host: AbstractHost): Ast = {
    val typeFullName = registerType(annotationTag.getType.parseAsJavaType)
    val name         = typeFullName.split('.').last
    val elementNodes = annotationTag.getElems.asScala.map(astForAnnotationElement(_, host)).toSeq
    val code = s"@$name(${elementNodes.flatMap(_.root).flatMap(_.properties.get(PropertyNames.CODE)).mkString(", ")})"

    val annotation = annotationNode(host, code, name, typeFullName)
    annotationAst(annotation, elementNodes)
  }

  private def astForAnnotationElement(annoElement: AnnotationElem, parent: AbstractHost): Ast = {
    def getLiteralElementNameAndCode(annoElement: AnnotationElem): (String, String) = annoElement match {
      case x: AnnotationClassElem =>
        val desc = registerType(x.getDesc.parseAsJavaType)
        (desc, desc)
      case x: AnnotationBooleanElem => (x.getValue.toString, x.getValue.toString)
      case x: AnnotationDoubleElem  => (x.getValue.toString, x.getValue.toString)
      case x: AnnotationEnumElem    => (x.getConstantName, x.getConstantName)
      case x: AnnotationFloatElem   => (x.getValue.toString, x.getValue.toString)
      case x: AnnotationIntElem     => (x.getValue.toString, x.getValue.toString)
      case x: AnnotationLongElem    => (x.getValue.toString, x.getValue.toString)
      case _                        => ("", "")
    }

    val lineNo      = line(parent)
    val columnNo    = column(parent)
    val codeBuilder = new mutable.StringBuilder()
    val astChildren = ListBuffer.empty[Ast]
    if (annoElement.getName != null) {
      astChildren.append(
        Ast(
          NewAnnotationParameter()
            .code(annoElement.getName)
            .lineNumber(lineNo)
            .columnNumber(columnNo)
        )
      )
      codeBuilder.append(s"${annoElement.getName} = ")
    }
    astChildren.append(annoElement match {
      case x: AnnotationAnnotationElem =>
        val rhsAst = astsForAnnotations(x.getValue, parent)
        codeBuilder.append(s"${rhsAst.root.flatMap(_.properties.get(PropertyNames.CODE)).mkString(", ")}")
        rhsAst
      case x: AnnotationArrayElem =>
        val (rhsAst, code) = astForAnnotationArrayElement(x, parent)
        codeBuilder.append(code)
        rhsAst
      case x =>
        val (name, code) = x match {
          case y: AnnotationStringElem => (y.getValue, s"\"${y.getValue}\"")
          case _                       => getLiteralElementNameAndCode(x)
        }
        codeBuilder.append(code)
        Ast(NewAnnotationLiteral().name(name).code(code).lineNumber(lineNo))
    })

    if (astChildren.size == 1) {
      astChildren.head
    } else {
      val paramAssign = NewAnnotationParameterAssign()
        .code(codeBuilder.toString)
        .lineNumber(lineNo)
        .columnNumber(columnNo)

      setArgumentIndices(astChildren.toSeq)
      Ast(paramAssign)
        .withChildren(astChildren)
    }
  }

  private def astForAnnotationArrayElement(x: AnnotationArrayElem, parent: AbstractHost): (Ast, String) = {
    val elems = x.getValues.asScala.map(astForAnnotationElement(_, parent)).toSeq
    val code  = s"{${elems.flatMap(_.root).flatMap(_.properties.get(PropertyNames.CODE)).mkString(", ")}}"
    val array = NewArrayInitializer().code(code).lineNumber(line(parent)).columnNumber(column(parent))
    setArgumentIndices(elems)
    (Ast(array).withChildren(elems), code)
  }

}
