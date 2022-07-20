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
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap
import com.github.javaparser.resolution.types.{
  ResolvedArrayType,
  ResolvedLambdaConstraintType,
  ResolvedPrimitiveType,
  ResolvedReferenceType,
  ResolvedType,
  ResolvedTypeVariable,
  ResolvedUnionType,
  ResolvedVoidType,
  ResolvedWildcard
}
import com.github.javaparser.symbolsolver.javaparsermodel.declarations.JavaParserClassDeclaration
import com.github.javaparser.symbolsolver.logic.InferenceVariableType
import com.github.javaparser.symbolsolver.model.typesystem.{LazyType, NullType}
import com.github.javaparser.symbolsolver.reflectionmodel.{ReflectionClassDeclaration, ReflectionTypeParameter}
import io.joern.javasrc2cpg.util.TypeInfoCalculator.{TypeConstants, TypeNameConstants}
import io.joern.x2cpg.datastructures.Global
import org.checkerframework.checker.signature.qual.FullyQualifiedName
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

class TypeInfoCalculator(global: Global, symbolResolver: SymbolResolver) {
  private val logger               = LoggerFactory.getLogger(this.getClass)
  private val emptyTypeParamValues = ResolvedTypeParametersMap.empty()

  def name(typ: ResolvedType): String = {
    nameOrFullName(typ, emptyTypeParamValues, false)
  }

  def name(typ: ResolvedType, typeParamValues: ResolvedTypeParametersMap): String = {
    nameOrFullName(typ, typeParamValues, false)
  }

  def fullName(typ: ResolvedType): String = {
    registerType(nameOrFullName(typ, emptyTypeParamValues, true))
  }

  def fullName(typ: ResolvedType, typeParamValues: ResolvedTypeParametersMap): String = {
    registerType(nameOrFullName(typ, typeParamValues, true))
  }

  private def typesSubstituted(
    trySubstitutedType: Try[ResolvedType],
    typeParamDecl: ResolvedTypeParameterDeclaration
  ): Boolean = {
    trySubstitutedType
      .map { substitutedType =>
        // substitutedType.isTypeVariable can crash with an UnsolvedSymbolException if it is an instance of LazyType,
        // in which case the type hasn't been successfully substituted.
        !(substitutedType.isTypeVariable && substitutedType.asTypeParameter() == typeParamDecl)
      }
      .getOrElse(false)
  }

  private def nameOrFullName(
    typ: ResolvedType,
    typeParamValues: ResolvedTypeParametersMap,
    fullyQualified: Boolean
  ): String = {
    typ match {
      case refType: ResolvedReferenceType =>
        nameOrFullName(refType.getTypeDeclaration.get, fullyQualified)
      case lazyType: LazyType =>
        lazyType match {
          case _ if lazyType.isReferenceType =>
            nameOrFullName(lazyType.asReferenceType(), typeParamValues, fullyQualified)
          case _ if lazyType.isTypeVariable =>
            nameOrFullName(lazyType.asTypeVariable(), typeParamValues, fullyQualified)
          case _ if lazyType.isArray =>
            nameOrFullName(lazyType.asArrayType(), typeParamValues, fullyQualified)
          case _ if lazyType.isPrimitive =>
            nameOrFullName(lazyType.asPrimitive(), typeParamValues, fullyQualified)
          case _ if lazyType.isWildcard =>
            nameOrFullName(lazyType.asWildcard(), typeParamValues, fullyQualified)
        }
      case voidType: ResolvedVoidType =>
        voidType.describe()
      case primitiveType: ResolvedPrimitiveType =>
        primitiveType.describe()
      case arrayType: ResolvedArrayType =>
        nameOrFullName(arrayType.getComponentType, typeParamValues, fullyQualified) + "[]"
      case nullType: NullType =>
        nullType.describe()
      case typeVariable: ResolvedTypeVariable =>
        val typeParamDecl   = typeVariable.asTypeParameter()
        val substitutedType = Try(typeParamValues.getValue(typeParamDecl))

        if (typesSubstituted(substitutedType, typeParamDecl)) {
          nameOrFullName(substitutedType.get, typeParamValues, fullyQualified)
        } else {
          val extendsBoundOption = Try(typeParamDecl.getBounds.asScala.find(_.isExtends)).toOption.flatten
          extendsBoundOption
            .map(bound => nameOrFullName(bound.getType, typeParamValues, fullyQualified))
            .getOrElse(objectType(fullyQualified))
        }
      case lambdaConstraintType: ResolvedLambdaConstraintType =>
        nameOrFullName(lambdaConstraintType.getBound, typeParamValues, fullyQualified)
      case wildcardType: ResolvedWildcard =>
        if (wildcardType.isBounded) {
          nameOrFullName(wildcardType.getBoundedType, typeParamValues, fullyQualified)
        } else {
          objectType(fullyQualified)
        }
      case unionType: ResolvedUnionType =>
        // The individual elements of the type union cannot be accessed in ResolvedUnionType.
        // For whatever reason there is no accessor and the field is private.
        // So for now we settle with the ancestor type. Maybe we use reflection later.
        Try(unionType.getCommonAncestor.toScala).toOption.flatten
          .map(nameOrFullName(_, typeParamValues, fullyQualified))
          .getOrElse(objectType(fullyQualified))
      case _: InferenceVariableType =>
        // From the JavaParser docs, the InferenceVariableType is: An element using during type inference.
        // At this point JavaParser has failed to resolve the type.
        TypeConstants.UnresolvedType
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
          .map(resolvedType => nameOrFullName(resolvedType, emptyTypeParamValues, fullyQualified))
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
    val Byte: String                = "byte"
    val Short: String               = "short"
    val Int: String                 = "int"
    val Long: String                = "long"
    val Float: String               = "float"
    val Double: String              = "double"
    val Char: String                = "char"
    val Boolean: String             = "boolean"
    val Object: String              = "java.lang.Object"
    val Class: String               = "java.lang.Class"
    val Iterator: String            = "java.util.Iterator"
    val Void: String                = "void"
    val UnresolvedType: String      = "<unresolvedType>"
    val UnresolvedSignature: String = "<unresolvedSignature>"
    val UnresolvedReceiver: String  = "<unresolvedReceiverType>"
  }

  val unresolvedConstants =
    List(TypeConstants.UnresolvedType, TypeConstants.UnresolvedSignature, TypeConstants.UnresolvedReceiver)

  object TypeNameConstants {
    val Object: String = "Object"
  }

  // The method signatures for all methods implemented by java.lang.Object, as returned by JavaParser. This is used
  // to filter out Object methods when determining which functional interface method a lambda implements. See
  // https://docs.oracle.com/javase/8/docs/api/java/lang/FunctionalInterface.html for more details.
  val ObjectMethodSignatures: Set[String] = Set(
    "wait(long, int)",
    "equals(java.lang.Object)",
    "clone()",
    "toString()",
    "wait()",
    "hashCode()",
    "getClass()",
    "notify()",
    "finalize()",
    "wait(long)",
    "notifyAll()",
    "registerNatives()"
  )

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

  def apply(global: Global, symbolResolver: SymbolResolver): TypeInfoCalculator = {
    val typeInfoCalculator = new TypeInfoCalculator(global, symbolResolver)
    unresolvedConstants.foreach(typeInfoCalculator.registerType)
    typeInfoCalculator
  }
}
