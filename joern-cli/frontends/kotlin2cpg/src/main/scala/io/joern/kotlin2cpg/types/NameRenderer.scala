package io.joern.kotlin2cpg.types

import io.joern.kotlin2cpg.types.NameRenderer.{BuiltinTypeTranslationTable, logger}
import io.joern.x2cpg.Defines
import org.jetbrains.kotlin.builtins.jvm.JavaToKotlinClassMap
import org.jetbrains.kotlin.descriptors.impl.TypeAliasConstructorDescriptor
import org.jetbrains.kotlin.descriptors.{
  ClassDescriptor,
  ConstructorDescriptor,
  DeclarationDescriptor,
  FunctionDescriptor,
  ModuleDescriptor,
  PackageFragmentDescriptor,
  TypeParameterDescriptor
}
import org.jetbrains.kotlin.name.FqNameUnsafe
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.error.ErrorClassDescriptor
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object NameRenderer {
  private val logger = LoggerFactory.getLogger(getClass)

  private val BuiltinTypeTranslationTable = mutable.HashMap(
    "kotlin.Unit"         -> "void",
    "kotlin.Boolean"      -> "boolean",
    "kotlin.Char"         -> "char",
    "kotlin.Byte"         -> "byte",
    "kotlin.Short"        -> "short",
    "kotlin.Int"          -> "int",
    "kotlin.Float"        -> "float",
    "kotlin.Long"         -> "long",
    "kotlin.Double"       -> "double",
    "kotlin.BooleanArray" -> "boolean[]",
    "kotlin.CharArray"    -> "char[]",
    "kotlin.ByteArray"    -> "byte[]",
    "kotlin.ShortArray"   -> "short[]",
    "kotlin.IntArray"     -> "int[]",
    "kotlin.FloatArray"   -> "float[]",
    "kotlin.LongArray"    -> "long[]",
    "kotlin.DoubleArray"  -> "double[]"
  )
}

class NameRenderer {
  private val anonDescriptorToIndex = mutable.HashMap.empty[DeclarationDescriptor, Int]
  private var anonObjectCounter     = 0

  def descName(desc: DeclarationDescriptor): String = {
    if (desc.getName.isSpecial) {
      desc match {
        case _: ConstructorDescriptor =>
          Defines.ConstructorMethodName
        case functionDesc: FunctionDescriptor =>
          Defines.ClosurePrefix + getAnonDescIndex(functionDesc)
        case _ =>
          "object$" + getAnonDescIndex(desc)
      }
    } else {
      desc.getName.getIdentifier
    }
  }

  def descFullName(desc: DeclarationDescriptor): Option[String] = {
    val dealiasedDesc = desc match {
      case typeAliasDesc: TypeAliasConstructorDescriptor => typeAliasDesc.getUnderlyingConstructorDescriptor
      case _                                             => desc
    }
    descFullNameInternal(dealiasedDesc).map(_.reverse.mkString(""))
  }

  def funcDescSignature(functionDesc: FunctionDescriptor): Option[String] = {
    val originalDesc        = functionDesc.getOriginal
    val extRecvDesc         = Option(originalDesc.getExtensionReceiverParameter)
    val extRecvTypeFullName = extRecvDesc.flatMap(paramDesc => typeFullName(paramDesc.getType))

    if (extRecvDesc.nonEmpty && extRecvTypeFullName.isEmpty) { return None }

    val paramTypeFullNames = originalDesc.getValueParameters.asScala.map(paramDesc => typeFullName(paramDesc.getType))
    if (paramTypeFullNames.exists(_.isEmpty)) { return None }

    val returnTypeFullName = if (isConstructorDesc(originalDesc)) { Some("void") }
    else { typeFullName(originalDesc.getReturnType) }

    if (returnTypeFullName.isEmpty) { return None }

    val combinedParamTypeFn = paramTypeFullNames.prepended(extRecvTypeFullName)
    val signature           = s"${returnTypeFullName.get}(${combinedParamTypeFn.flatten.mkString(",")})"
    Some(signature)
  }

  def combineFunctionFullName(descFullName: String, signature: String): String = {
    s"$descFullName:$signature"
  }

  def typeFullName(typ: KotlinType): Option[String] = {
    val javaFullName =
      typ.getConstructor.getDeclarationDescriptor match {
        case classDesc: ClassDescriptor =>
          val kotlinFullName = descFullName(classDesc)
          if (kotlinFullName.contains("kotlin.Array")) {
            val elementTypeFullName = typeFullName(typ.getArguments.get(0).getType)
            elementTypeFullName.map(_ + "[]")
          } else {
            kotlinFullName.map(typeFullNameKotlinToJava)
          }
        case typeParamDesc: TypeParameterDescriptor =>
          val upperBoundTypeFns = typeParamDesc.getUpperBounds.asScala.map(typeFullName)
          if (upperBoundTypeFns.exists(_.isEmpty)) {
            None
          } else {
            Some(upperBoundTypeFns.flatten.mkString("&"))
          }
        case null =>
          // We do not expect this because to my understanding a typ should always have a constructor
          // descriptor.
          logger.warn(
            s"Found type without constructor descriptor. Typ: $typ Constructor class: ${typ.getConstructor.getClass}"
          )
          None
      }

    javaFullName
  }

  private def typeFullNameKotlinToJava(kotlinFullName: String): String = {
    val javaFullName = BuiltinTypeTranslationTable.get(kotlinFullName)
    if (javaFullName.isDefined) {
      javaFullName.get
    } else {
      // Nested class fullnames contain '$' in our representation which need to be mapped to '.'
      // in order to make use of JavaToKotlinClassMap.
      val kotlinFullNameDotOnly = kotlinFullName.replace('$', '.')
      val javaFullName          = JavaToKotlinClassMap.INSTANCE.mapKotlinToJava(FqNameUnsafe(kotlinFullNameDotOnly))

      val result =
        if (javaFullName != null) {
          // In front of nested class sub names we find '.' which needs to be mapped to '$' in our representation.
          // After that we can map the normal name separator '/' to '.'.
          javaFullName.toString.replace('.', '$').replace('/', '.')
        } else {
          kotlinFullName
        }
      result
    }
  }

  private def getAnonDescIndex(desc: DeclarationDescriptor): Int = {
    anonDescriptorToIndex.getOrElseUpdate(
      desc, {
        val index = anonObjectCounter
        anonObjectCounter += 1
        index
      }
    )
  }

  private def descFullNameInternal(desc: DeclarationDescriptor): Option[List[String]] = {
    if (desc.isInstanceOf[ErrorClassDescriptor]) {
      return None
    }
    val parentDesc = desc.getContainingDeclaration

    val parentFnParts =
      Option(parentDesc) match {
        case None =>
          Some(Nil)
        case Some(parentDesc) =>
          descFullNameInternal(parentDesc)
      }

    if (parentFnParts.isEmpty) {
      return None
    }

    var extendedFnParts = parentFnParts.get
    desc match {
      case packageFragmentDesc: PackageFragmentDescriptor =>
        if (!packageFragmentDesc.getName.isSpecial) {
          extendedFnParts = packageFragmentDesc.getFqName.toString :: extendedFnParts
        }
      case _: ModuleDescriptor => // Do nothing since this is just the root element which has no namespace representation
      case _ =>
        if (extendedFnParts.nonEmpty) {
          val separator = if (parentDesc.isInstanceOf[ClassDescriptor] && desc.isInstanceOf[ClassDescriptor]) {
            // Nested class
            "$"
          } else {
            "."
          }
          extendedFnParts = separator :: extendedFnParts
        }
        val name = descName(desc)
        extendedFnParts = name :: extendedFnParts
    }
    Some(extendedFnParts)
  }

  private def isConstructorDesc(functionDesc: FunctionDescriptor): Boolean = {
    functionDesc match {
      case _: ConstructorDescriptor => true
      case _                        => false
    }
  }

}
