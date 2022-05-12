package io.joern.javasrc2cpg.util

import com.github.javaparser.ast.`type`.{PrimitiveType, Type, WildcardType}
import com.github.javaparser.resolution.SymbolResolver
import com.github.javaparser.resolution.declarations.{
  ResolvedDeclaration,
  ResolvedEnumConstantDeclaration,
  ResolvedMethodDeclaration,
  ResolvedTypeDeclaration,
  ResolvedTypeParameterDeclaration
}
import com.github.javaparser.resolution.types.{
  ResolvedArrayType,
  ResolvedLambdaConstraintType,
  ResolvedPrimitiveType,
  ResolvedReferenceType,
  ResolvedType,
  ResolvedTypeVariable,
  ResolvedVoidType,
  ResolvedWildcard
}
import com.github.javaparser.symbolsolver.javaparsermodel.declarations.JavaParserClassDeclaration
import com.github.javaparser.symbolsolver.model.typesystem.{LazyType, NullType}
import com.github.javaparser.symbolsolver.reflectionmodel.{ReflectionClassDeclaration, ReflectionTypeParameter}
import io.joern.javasrc2cpg.util.TypeInfoCalculator.{TypeConstants, TypeNameConstants}
import io.joern.x2cpg.datastructures.Global
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try

class TypeInfoCalculator(global: Global, symbolResolver: SymbolResolver) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  def name(typ: ResolvedType): String = {
    nameOrFullName(typ, false)
  }

  def fullName(typ: ResolvedType): String = {
    registerType(nameOrFullName(typ, true))
  }

  private def nameOrFullName(typ: ResolvedType, fullyQualified: Boolean): String = {
    typ match {
      case refType: ResolvedReferenceType =>
        nameOrFullName(refType.getTypeDeclaration.get, fullyQualified)
      case lazyType: LazyType if lazyType.isReferenceType =>
        nameOrFullName(lazyType.asReferenceType(), fullyQualified)
      case voidType: ResolvedVoidType =>
        voidType.describe()
      case primitiveType: ResolvedPrimitiveType =>
        primitiveType.describe()
      case arrayType: ResolvedArrayType =>
        arrayType.describe()
      case nullType: NullType =>
        nullType.describe()
      case typeVariable: ResolvedTypeVariable =>
        val extendsBoundOption = typeVariable.asTypeParameter().getBounds.asScala.find(_.isExtends)
        extendsBoundOption
          .map(bound => fullName(bound.getType))
          .getOrElse(objectType(fullyQualified))
      case lambdaConstraintType: ResolvedLambdaConstraintType =>
        nameOrFullName(lambdaConstraintType.getBound, fullyQualified)
      case wildcardType: ResolvedWildcard =>
        if (wildcardType.isBounded) {
          nameOrFullName(wildcardType.getBoundedType, fullyQualified)
        } else {
          objectType(fullyQualified)
        }
    }
  }

  private def objectType(fullyQualified: Boolean): String = {
    if (fullyQualified) {
      TypeConstants.Object
    } else {
      TypeNameConstants.Object
    }
  }

  def name(typ: Type): Option[String] = {
    nameOrFullName(typ, false)
  }

  def fullName(typ: Type): Option[String] = {
    nameOrFullName(typ, true).map(registerType)
  }

  private def nameOrFullName(typ: Type, fullyQualified: Boolean): Option[String] = {
    typ match {
      case primitiveType: PrimitiveType =>
        Some(primitiveType.toString)
      case _ =>
        // We are using symbolResolver.toResolvedType() instead of typ.resolve() because
        // the resolve() is just a wrapper for a call to symbolResolver.toResolvedType()
        // with a specific class given as argument to which the result is casted to.
        // It appears to be that ClassOrInterfaceType.resolve() is using a too restrictive
        // bound (ResolvedReferenceType.class) which invalidates an otherwise successful
        // resolve. Since we anyway dont care about the type cast, we directly access the
        // symbolResolver and specifiy the most generic type ResolvedType.
        Try(symbolResolver.toResolvedType(typ, classOf[ResolvedType])).toOption
          .map(resolvedType => nameOrFullName(resolvedType, fullyQualified))
    }
  }

  def name(decl: ResolvedDeclaration): String = {
    nameOrFullName(decl, false)
  }

  def fullName(decl: ResolvedDeclaration): String = {
    registerType(nameOrFullName(decl, true))
  }

  private def nameOrFullName(decl: ResolvedDeclaration, fullyQualified: Boolean): String = {
    decl match {
      case typeDecl: ResolvedTypeDeclaration =>
        nameOrFullName(typeDecl, fullyQualified)
    }
  }

  private def nameOrFullName(typeDecl: ResolvedTypeDeclaration, fullyQualified: Boolean): String = {
    typeDecl match {
      case typeParamDecl: ResolvedTypeParameterDeclaration =>
        if (fullyQualified) {
          val containFullName = nameOrFullName(typeParamDecl.getContainer.asInstanceOf[ResolvedDeclaration], true)
          containFullName + "." + typeParamDecl.getName
        } else {
          typeParamDecl.getName
        }
      case _ =>
        val typeName = Option(typeDecl.getName).getOrElse(throw new RuntimeException("TODO Investigate"))

        // TODO Sadly we need to use a try here in order to catch the exception emitted by
        // the javaparser library instead of just returning an empty option.
        // In almost all cases we get here the exception is thrown. Check impact on performance
        // and hopefully find a better solution if necessary.
        val isInnerTypeDecl = Try(typeDecl.containerType().isPresent).getOrElse(false)
        if (isInnerTypeDecl) {
          nameOrFullName(typeDecl.containerType().get, fullyQualified) + "$" + typeName
        } else {
          if (fullyQualified) {
            val packageName = typeDecl.getPackageName

            if (packageName == null || packageName == "") {
              typeName
            } else {
              packageName + "." + typeName
            }
          } else {
            typeName
          }
        }
    }
  }

  /** Add `typeName` to a global map and return it. The map is later passed to a pass that creates TYPE nodes for each
    * key in the map. Skip the `ANY` type, since this is created by default. TODO: I want the type registration not in
    * here but for now it is the easiest.
    */
  def registerType(typeName: String): String = {
    if (typeName != "ANY") {
      global.usedTypes.putIfAbsent(typeName, true)
    }
    typeName
  }
}

object TypeInfoCalculator {
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

  object TypeNameConstants {
    val Object: String = "Object"
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
