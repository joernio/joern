package io.joern.javasrc2cpg.astcreation.declarations

import com.github.javaparser.ast.`type`.{
  ArrayType,
  ClassOrInterfaceType,
  PrimitiveType,
  Type,
  TypeParameter,
  UnknownType,
  VarType,
  VoidType,
  WildcardType
}
import com.github.javaparser.ast.body.{
  AnnotationDeclaration,
  CallableDeclaration,
  ClassOrInterfaceDeclaration,
  ConstructorDeclaration,
  EnumConstantDeclaration,
  EnumDeclaration,
  MethodDeclaration,
  Parameter,
  RecordDeclaration,
  TypeDeclaration
}
import com.github.javaparser.ast.expr.{LambdaExpr, TypePatternExpr}
import com.github.javaparser.printer.configuration.DefaultPrinterConfiguration.ConfigOption
import com.github.javaparser.printer.configuration.{DefaultConfigurationOption, DefaultPrinterConfiguration}
import io.joern.javasrc2cpg.astcreation.declarations.BinarySignatureCalculator.{
  BaseTypeMap,
  javaEnumName,
  javaObjectName,
  javaRecordName,
  unspecifiedType
}
import io.joern.javasrc2cpg.scope.Scope
import io.joern.javasrc2cpg.scope.Scope.ScopeTypeParam
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.Util
import org.objectweb.asm.signature.SignatureWriter
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

object BinarySignatureCalculator {
  private val javaObjectName  = "Object"
  private val javaEnumName    = "Enum"
  private val javaRecordName  = "Record"
  private val unspecifiedType = "__unspecified_type"

  // From https://docs.oracle.com/javase/specs/jvms/se23/html/jvms-4.html#jvms-4.3
  val BaseTypeMap: Map[String, Char] = Seq(
    TypeConstants.Byte    -> 'B',
    TypeConstants.Char    -> 'C',
    TypeConstants.Double  -> 'D',
    TypeConstants.Float   -> 'F',
    TypeConstants.Int     -> 'I',
    TypeConstants.Long    -> 'J',
    TypeConstants.Short   -> 'S',
    TypeConstants.Boolean -> 'Z',
    TypeConstants.Void    -> 'V'
  ).toMap
}

class BinarySignatureCalculator(scope: Scope) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val typePrinterOptions = new DefaultPrinterConfiguration()
    .removeOption(new DefaultConfigurationOption(ConfigOption.PRINT_COMMENTS))
    .removeOption(new DefaultConfigurationOption(ConfigOption.PRINT_JAVADOC))

  private def typeToString(typ: Type): String = {
    Util.stripGenericTypes(typ.toString(typePrinterOptions))
  }

  val unspecifiedClassType: String = {
    val writer = SignatureWriter()

    writer.visitClassType(unspecifiedType)
    writer.visitEnd()

    writer.toString
  }

  def defaultConstructorSignature(parameters: List[Parameter]): String = {
    val writer = SignatureWriter()

    parameters.foreach { param =>
      writer.visitParameterType()
      addType(writer, param.getType)
    }

    writer.visitReturnType()
    addType(writer, new VoidType())

    writer.toString
  }

  def recordParameterAccessorBinarySignature(parameter: Parameter): String = {
    val writer = SignatureWriter()

    writer.visitReturnType()
    addType(writer, parameter.getType)

    writer.toString
  }

  def enumEntryBinarySignature(enumEntry: EnumConstantDeclaration): String = {
    val writer = SignatureWriter()

    enumEntry.getParentNode.toScala collect { case enumDeclaration: EnumDeclaration =>
      writer.visitClassType(enumDeclaration.getNameAsString)
      writer.visitEnd()
    }

    writer.toString
  }

  def variableBinarySignature(typ: String): String = {
    val writer = SignatureWriter()

    BaseTypeMap.get(typ) match {
      case Some(baseType) =>
        writer.visitBaseType(baseType)

      case None =>
        writer.visitClassType(typ)
        writer.visitEnd()
    }

    writer.toString
  }

  def typeDeclBinarySignature(typeDeclaration: TypeDeclaration[?]): String = {
    typeDeclaration match {
      case decl: AnnotationDeclaration       => annotationDecBinarySignature(decl)
      case decl: RecordDeclaration           => recordDeclBinarySignature(decl)
      case decl: ClassOrInterfaceDeclaration => classDeclBinarySignature(decl)
      case decl: EnumDeclaration             => enumDeclBinarySignature(decl)
      case decl =>
        throw new IllegalArgumentException(
          s"Attempting to get binary signature for unhandled type declaration $typeDeclaration"
        )
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
    writer.visitEnd()

    writer.toString
  }

  def patternVariableBinarySignature(typePatternExpr: TypePatternExpr): String = {
    val writer = SignatureWriter()
    addType(writer, typePatternExpr.getType)
    writer.toString
  }

  def classDeclBinarySignature(
    classDecl: ClassOrInterfaceDeclaration,
    classNameOverride: Option[String] = None
  ): String = {
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

  def variableBinarySignature(variableType: Type): String = {
    val writer = SignatureWriter()
    addType(writer, variableType)
    writer.toString
  }

  def methodBinarySignature(callableDecl: CallableDeclaration[?]): String = {
    val writer = SignatureWriter()
    callableDecl.getTypeParameters.asScala.foreach(addTypeParam(writer, _))

    callableDecl.getParameters.asScala.foreach { param =>
      writer.visitParameterType()
      addType(writer, param.getType)
    }

    writer.visitReturnType()
    callableDecl match {
      case methodDeclaration: MethodDeclaration => addType(writer, methodDeclaration.getType)
      case constructorDeclaration: ConstructorDeclaration =>
        BaseTypeMap.get(TypeConstants.Void).foreach(writer.visitBaseType(_))
    }

    callableDecl.getThrownExceptions.asScala.foreach { exception =>
      writer.visitExceptionType()
      addType(writer, exception)
    }

    writer.toString
  }

  def lambdaMethodBinarySignature(expr: LambdaExpr): String = {
    val writer = SignatureWriter()

    expr.getParameters.asScala.foreach { param =>
      writer.visitParameterType()
      addType(writer, param.getType)
    }

    writer.visitReturnType()
    writer.visitClassType(unspecifiedType)
    writer.visitEnd()

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

  private def isTypeVariable(name: String): Boolean = {
    scope.lookupScopeType(name, includeWildcards = false).exists(_.isInstanceOf[ScopeTypeParam])
  }

  private def addType(writer: SignatureWriter, typ: Type): Unit = {
    val name = typeToString(typ)
    typ match {
      case _ if isTypeVariable(name) =>
        writer.visitTypeVariable(typeToString(typ))
      case classOrInterface: ClassOrInterfaceType =>
        val internalClassName = scope
          .lookupScopeType(name, includeWildcards = false)
          .map(_.name)
          .getOrElse(name)
        writer.visitClassType(internalClassName)
        val typeArgs = classOrInterface.getTypeArguments.toScala.map(_.asScala).getOrElse(Nil)
        typeArgs.foreach {
          case wildcardType: WildcardType =>
            if (wildcardType.getExtendedType.isPresent) {
              writer.visitTypeArgument('+')
              addType(writer, wildcardType.getExtendedType.get())
            } else if (wildcardType.getSuperType.isPresent) {
              writer.visitTypeArgument('-')
              addType(writer, wildcardType.getSuperType.get())
            } else writer.visitTypeArgument('*')
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
        writer.visitClassType(unspecifiedType)
        writer.visitEnd()
      case _: VoidType =>
        writer.visitBaseType('V')
      case _: UnknownType =>
        writer.visitClassType(unspecifiedType)
        writer.visitEnd()
    }
  }

}
