package io.joern.javasrc2cpg.astcreation.declarations

import com.github.javaparser.ast.`type`.{
  ArrayType,
  ClassOrInterfaceType,
  PrimitiveType,
  Type,
  TypeParameter,
  VarType,
  VoidType,
  WildcardType
}
import com.github.javaparser.ast.body.{
  AnnotationDeclaration,
  CallableDeclaration,
  ClassOrInterfaceDeclaration,
  CompactConstructorDeclaration,
  ConstructorDeclaration,
  EnumDeclaration,
  MethodDeclaration,
  RecordDeclaration,
  TypeDeclaration,
  VariableDeclarator
}
import io.joern.javasrc2cpg.astcreation.declarations.BinarySignatureCalculator.{
  javaEnumName,
  javaObjectName,
  javaRecordName
}
import io.joern.x2cpg.Defines
import org.objectweb.asm.signature.SignatureWriter
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

object BinarySignatureCalculator {
  private val javaObjectName        = "java/lang/Object"
  private val javaEnumName          = "java/lang/Enum"
  private val javaRecordName        = "java/lang/Record"
  private val unresolvedPlaceholder = "__unresolved_namespace_placeholder__"
}

class BinarySignatureCalculator {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def typeDeclBinarySignature(typeDeclaration: TypeDeclaration[?]): String = {
    typeDeclaration match {
      case decl: AnnotationDeclaration       => annotationDecBinarySignature(decl)
      case decl: RecordDeclaration           => recordDeclBinarySignature(decl)
      case decl: ClassOrInterfaceDeclaration => classDeclBinarySignature(decl)
      case decl: EnumDeclaration             => enumDeclBinarySignature(decl)
      case decl =>
        logger.warn(s"Attempting to get binary signature for unhandled type declaration $typeDeclaration")
        Defines.Unknown
    }
  }

  def callableDeclarationBinarySignature(
    declaration: CallableDeclaration[?] | CompactConstructorDeclaration
  ): String = {
    declaration match {
      case decl: MethodDeclaration      => methodBinarySignature(decl)
      case decl: ConstructorDeclaration =>
        // TODO: Generic signature
        Defines.Unknown
      case decl: CompactConstructorDeclaration =>
        // TODO: Generic signature
        Defines.Unknown
    }
  }

  def annotationDecBinarySignature(annotationDecl: AnnotationDeclaration): String = {
    val writer = SignatureWriter()

    writer.visitClassType(javaObjectName)
    writer.visitEnd()

    writer.toString
  }

  def enumDeclBinarySignature(enumDecl: EnumDeclaration): String = {
    val writer = SignatureWriter()

    writer.visitSuperclass()
    writer.visitClassType(javaEnumName)
    writer.visitTypeArgument('=')
    writer.visitClassType(enumDecl.getNameAsString)
    writer.visitEnd()

    enumDecl.getImplementedTypes.asScala.foreach(addType(writer, _))

    writer.toString
  }

  def classDeclBinarySignature(classDecl: ClassOrInterfaceDeclaration): String = {
    val writer = SignatureWriter()
    classDecl.getTypeParameters.asScala.foreach(addTypeParam(writer, _))

    writer.visitSuperclass()
    if (classDecl.isInterface) {
      writer.visitClassType(javaObjectName)
      writer.visitEnd()
      classDecl.getExtendedTypes.asScala.foreach(addType(writer, _))
    } else {
      if (classDecl.getExtendedTypes.isEmpty) {
        writer.visitClassType(javaObjectName)
        writer.visitEnd()
      } else {
        classDecl.getExtendedTypes.asScala.foreach(addType(writer, _))
      }
      classDecl.getImplementedTypes.asScala.foreach(addType(writer, _))
    }

    writer.toString
  }

  def recordDeclBinarySignature(recordDecl: RecordDeclaration): String = {
    val writer = SignatureWriter()
    recordDecl.getTypeParameters.asScala.foreach(addTypeParam(writer, _))

    writer.visitSuperclass()
    writer.visitClassType(javaRecordName)
    writer.visitEnd()

    recordDecl.getImplementedTypes.asScala.foreach(addType(writer, _))

    writer.toString
  }

  def methodBinarySignature(methodDecl: MethodDeclaration): String = {
    val writer = SignatureWriter()
    methodDecl.getTypeParameters.asScala.foreach(addTypeParam(writer, _))

    methodDecl.getParameters.asScala.foreach { param =>
      writer.visitParameterType()
      addType(writer, param.getType)
    }

    writer.visitReturnType()
    addType(writer, methodDecl.getType)

    methodDecl.getThrownExceptions.asScala.foreach { exception =>
      writer.visitExceptionType()
      addType(writer, exception)
    }

    writer.toString
  }

  def fieldBinarySignature(variableDeclarator: VariableDeclarator): String = {
    val writer = SignatureWriter()
    addType(writer, variableDeclarator.getType)
    writer.toString
  }

  private def addTypeParam(writer: SignatureWriter, typeParam: TypeParameter): Unit = {
    writer.visitFormalTypeParameter(typeParam.getNameAsString())
    writer.visitClassBound()
    val typeBoundIt = typeParam.getTypeBound.asScala.iterator
    if (typeBoundIt.isEmpty) {
      writer.visitClassType(javaObjectName)
      writer.visitEnd()
    } else {
      addType(writer, typeBoundIt.next)
    }
    typeBoundIt.foreach { typeBound =>
      writer.visitInterfaceBound()
      addType(writer, typeBound)
    }
  }

  private def addType(writer: SignatureWriter, typ: Type): Unit = {
    typ match {
      case classOrInterface: ClassOrInterfaceType =>
        writer.visitClassType(classOrInterface.getNameAsString)
        val typeArgs = classOrInterface.getTypeArguments.toScala.map(_.asScala).getOrElse(Nil)
        typeArgs.foreach {
          case wildcardType: WildcardType =>
            if (wildcardType.getExtendedType.isPresent) {
              writer.visitTypeArgument('+')
              addType(writer, wildcardType.getExtendedType.get())
            } else if (wildcardType.getSuperType.isPresent) {
              writer.visitTypeArgument('-')
              addType(writer, wildcardType.getSuperType.get())
            } else {
              writer.visitTypeArgument('*')
            }
          case typeArg =>
            writer.visitTypeArgument('=')
            addType(writer, typeArg)
        }
        writer.visitEnd()
      case arrayType: ArrayType =>
        writer.visitArrayType()
        addType(writer, arrayType.getElementType)
      case typeParam: TypeParameter =>
        writer.visitTypeVariable(typeParam.getNameAsString)
      case primitiveType: PrimitiveType =>
        writer.visitBaseType(primitiveType.getType.toDescriptor.charAt(0))
      case varType: VarType =>
        // TODO Solve the type?
        writer.visitTypeVariable(Defines.Unknown)
      case _: VoidType =>
        writer.visitBaseType('V')
    }
  }

}
