package io.joern.javasrc2cpg.jartypereader.model

import Bound.Bound

sealed trait Named {
  def name: String
  def qualifiedName: String

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: Named =>
        this.name == other.name && this.qualifiedName == other.qualifiedName
      case _ => false
    }
  }

  def buildQualifiedClassName(name: String, packageSpecifier: Option[String]): String = {
    packageSpecifier.map(ps => s"$ps.$name").getOrElse(name)
  }
}

case class NameWithTypeArgs(name: String, typeArguments: List[TypeArgument])

object Bound {
  sealed trait Bound
  case object BoundAbove extends Bound
  case object BoundBelow extends Bound
}

sealed trait TypeArgument
case class SimpleTypeArgument(typeSignature: ReferenceTypeSignature)          extends TypeArgument
case class BoundWildcard(bound: Bound, typeSignature: ReferenceTypeSignature) extends TypeArgument
case object UnboundWildcard                                                   extends TypeArgument

sealed trait JavaTypeSignature extends Named
case class PrimitiveType(fullName: String) extends JavaTypeSignature {
  override val name: String          = fullName
  override val qualifiedName: String = fullName
}

sealed trait ReferenceTypeSignature extends JavaTypeSignature

case class ClassTypeSignature(
  packageSpecifier: Option[String],
  typedName: NameWithTypeArgs,
  suffix: List[NameWithTypeArgs]
) extends ReferenceTypeSignature {
  override val name: String          = (typedName :: suffix).map(_.name).mkString(".")
  override val qualifiedName: String = buildQualifiedClassName(name, packageSpecifier)
}

case class TypeVariableSignature(identifier: String) extends ReferenceTypeSignature {
  override val name: String          = identifier
  override val qualifiedName: String = identifier
}
case class ArrayTypeSignature(signature: JavaTypeSignature) extends ReferenceTypeSignature {
  private def buildArrayName(baseName: String): String = s"[$baseName]"
  override val name: String                            = buildArrayName(signature.name)
  override val qualifiedName: String                   = buildArrayName(signature.qualifiedName)
}

case class TypeParameter(
  name: String,
  classBound: Option[ReferenceTypeSignature],
  interfaceBounds: List[ReferenceTypeSignature]
)
case class ClassSignature(
  typeParameters: List[TypeParameter],
  superclassSignature: Option[ClassTypeSignature],
  superinterfaceSignatures: List[ClassTypeSignature]
)
case class MethodSignature(
  typeParameters: List[TypeParameter],
  paramTypes: List[JavaTypeSignature],
  returnSignature: JavaTypeSignature,
  throwsSignatures: List[JavaTypeSignature]
)

sealed trait ResolvedType extends Named

object Unresolved extends ResolvedType {
  override val name: String          = "Unresolved"
  override val qualifiedName: String = name
}

class ResolvedTypeDecl(
  override val name: String,
  val packageSpecifier: Option[String],
  val signature: ClassSignature,
  val isInterface: Boolean,
  val isAbstract: Boolean,
  val fields: List[ResolvedVariableType],
  initDeclaredMethods: List[ResolvedMethod]
) extends ResolvedType {
  override val qualifiedName: String = buildQualifiedClassName(name, packageSpecifier)

  private var declaredMethods = initDeclaredMethods

  def getDeclaredMethods: List[ResolvedMethod] = declaredMethods

  private[jartypereader] def addMethods(newMethods: List[ResolvedMethod]): Unit = {
    declaredMethods ++= newMethods
  }
}
object ResolvedTypeDecl {
  def apply(
    name: String,
    packageSpecifier: Option[String],
    signature: ClassSignature,
    isInterface: Boolean,
    isAbstract: Boolean,
    fields: List[ResolvedVariableType],
    methods: List[ResolvedMethod] = Nil
  ): ResolvedTypeDecl = {
    new ResolvedTypeDecl(name, packageSpecifier, signature, isInterface, isAbstract, fields, methods)
  }
}

case class ResolvedMethod(
  override val name: String,
  parentTypeDecl: ResolvedTypeDecl,
  signature: MethodSignature,
  isAbstract: Boolean
) extends ResolvedType {
  override val qualifiedName: String = s"${parentTypeDecl.qualifiedName}.$name"
}

case class ResolvedVariableType(name: String, signature: ReferenceTypeSignature) extends ResolvedType {
  override val qualifiedName: String = name
}

object Model {
  // TODO: This is a duplicate of the TypeConstants object in TypeInfoCalculator. Remove the other one once
  //  we switch to the new solver.
  object TypeConstants {
    val Byte: String     = "byte"
    val Short: String    = "short"
    val Int: String      = "int"
    val Long: String     = "long"
    val Float: String    = "float"
    val Double: String   = "double"
    val Char: String     = "char"
    val Boolean: String  = "boolean"
    val Object: String   = "java.lang.Object"
    val Class: String    = "java.lang.Class"
    val Iterator: String = "java.util.Iterator"
    val Void: String     = "void"
    val Any: String      = "ANY"
  }
}
