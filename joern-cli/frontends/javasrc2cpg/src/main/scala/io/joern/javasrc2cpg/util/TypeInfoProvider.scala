package io.joern.javasrc2cpg.util

import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, PrimitiveType, ReferenceType}
import com.github.javaparser.ast.body.{EnumConstantDeclaration, TypeDeclaration}
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.nodeTypes.{NodeWithName, NodeWithSimpleName, NodeWithType}
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt
import com.github.javaparser.resolution.Resolvable
import com.github.javaparser.resolution.declarations._
import com.github.javaparser.resolution.types.{ResolvedReferenceType, ResolvedType}
import io.joern.x2cpg.datastructures.Global
import org.slf4j.LoggerFactory

import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

class TypeInfoProvider(global: Global, scope: Scope) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  /** Add `typeName` to a global map and return it. The map is later passed to a pass that creates TYPE nodes for each
    * key in the map. Skip the `ANY` type, since this is created by default.
    */
  def registerType(typeName: String): String = {
    if (typeName != "ANY") {
      global.usedTypes.putIfAbsent(typeName, true)
    }
    typeName
  }

  def getResolvedTypeFullName(resolvedType: ResolvedType): Option[String] = {
    JP2JavaSrcTypeAdapter.resolvedTypeFullName(resolvedType).map(registerType)
  }

  def getResolvedTypeDeclFullName(resolveTypeDecl: ResolvedTypeDeclaration): String = {
    JP2JavaSrcTypeAdapter.resolvedTypeDeclFullName(resolveTypeDecl)
  }

  def getTypeDeclType(typeDecl: TypeDeclaration[_], fullName: Boolean = true): String = {
    val typeName = JP2JavaSrcTypeAdapter.typeNameForTypeDecl(typeDecl, fullName)
    if (fullName) {
      registerType(typeName)
    } else {
      typeName
    }
  }

  def getTypeFullName(node: NodeWithType[_, _ <: Resolvable[ResolvedType]]): Option[String] = {
    val typeFullName = Try(node.getType.resolve()) match {
      case Success(resolvedType: ResolvedParameterDeclaration) =>
        getTypeFullName(resolvedType)

      case Success(resolvedType: ResolvedReferenceType) =>
        JP2JavaSrcTypeAdapter.resolvedReferenceTypeFullName(resolvedType)

      case Success(resolvedType: ResolvedType) => JP2JavaSrcTypeAdapter.simpleResolvedTypeFullName(resolvedType)

      case Failure(_) =>
        node.getType match {
          case primitive: PrimitiveType => Some(primitive.toString)
          case _                        => None
        }
    }

    typeFullName.map(registerType)
  }

  def getTypeFullName(typ: ClassOrInterfaceType): Option[String] = {
    val typeFullName = Try(typ.resolve) match {
      case Success(resolvedType) => JP2JavaSrcTypeAdapter.resolvedReferenceTypeFullName(resolvedType)

      case Failure(_) =>
        logger.debug(s"Failed to resolve class type ${typ.getNameAsString}. Falling back to imports info.")
        None
    }

    typeFullName.map(registerType)
  }

  def getTypeFullName(enumConstant: EnumConstantDeclaration): Option[String] = {
    val typeFullName = Try(enumConstant.resolve()) match {
      case Success(resolvedDeclaration) =>
        JP2JavaSrcTypeAdapter.resolvedTypeFullName(resolvedDeclaration.getType)

      case Failure(_) =>
        logger.debug(s"Failed to resolve enum entry type for ${enumConstant.getNameAsString}")
        None
    }

    typeFullName.map(registerType)
  }

  def getTypeFullName(referenceType: ReferenceType): Option[String] = {
    val typeFullName = Try(referenceType.resolve()).toOption.flatMap(JP2JavaSrcTypeAdapter.resolvedTypeFullName)

    typeFullName.map(registerType)
  }

  def getReturnType(node: Resolvable[ResolvedMethodDeclaration]): Option[String] = {
    val typeFullName = Try(node.resolve().getReturnType) match {
      case Success(resolved) => JP2JavaSrcTypeAdapter.resolvedTypeFullName(resolved)

      case Failure(_) =>
        logger.debug(s"Failed to resolve return type.")
        None
    }

    typeFullName.map(registerType)
  }

  def getTypeFullName(invocation: ExplicitConstructorInvocationStmt): Option[String] = {
    val typeFullName = JP2JavaSrcTypeAdapter.typeForMethodLike(invocation)

    typeFullName.map(registerType)
  }

  def getTypeFullName(resolvedParam: ResolvedParameterDeclaration): Option[String] = {
    val typeFullName = JP2JavaSrcTypeAdapter.resolvedTypeFullName(resolvedParam.getType)
    val arraySuffix = if (resolvedParam.isVariadic) {
      "[]"
    } else {
      ""
    }

    typeFullName.map(_ ++ arraySuffix).map(registerType)
  }

  def getTypeForExpression(expr: Expression): Option[String] = {
    val fallbackType: Option[String] = expr match {
      case namedExpr: NodeWithName[_] => scope.lookupVariableType(namedExpr.getNameAsString)

      case namedExpr: NodeWithSimpleName[_] => scope.lookupVariableType(namedExpr.getNameAsString)

      // JavaParser doesn't handle literals well for some reason
      case literal: LiteralExpr => JP2JavaSrcTypeAdapter.typeForLiteral(literal)

      case _ => None
    }

    val typeFullName = JP2JavaSrcTypeAdapter.typeForExpression(expr).orElse(fallbackType)

    typeFullName.map(registerType)
  }

}

object TypeInfoProvider {
  def apply(global: Global, scope: Scope): TypeInfoProvider = {
    new TypeInfoProvider(global, scope)
  }

  def isAutocastType(typeName: String): Boolean = {
    NumericTypes.contains(typeName)
  }

  object TypeConstants {
    val Byte: String    = "byte"
    val Short: String   = "short"
    val Int: String     = "int"
    val Long: String    = "long"
    val Float: String   = "float"
    val Double: String  = "double"
    val Char: String    = "char"
    val Boolean: String = "boolean"
    val Object: String  = "java.lang.Object"
    val Class: String   = "java.lang.Class"
  }

  val NumericTypes: Set[String] = Set(
    "byte",
    "short",
    "int",
    "long",
    "float",
    "double",
    "char",
    "boolean",
    "java.lang.Byte",
    "java.lang.Short",
    "java.lang.Integer",
    "java.lang.Long",
    "java.lang.Float",
    "java.lang.Double",
    "java.lang.Character",
    "java.lang.Boolean"
  )

  val UnresolvedTypeDefault = "ANY"
}
