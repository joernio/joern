package io.joern.kotlin2cpg.types

import org.jetbrains.kotlin.descriptors.{DeclarationDescriptor, SimpleFunctionDescriptor}
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.types.{ErrorType, ErrorUtils, KotlinType, TypeUtils, UnresolvedType}
import org.jetbrains.kotlin.builtins.jvm.JavaToKotlinClassMap
import org.jetbrains.kotlin.renderer.{DescriptorRenderer, DescriptorRendererImpl, DescriptorRendererOptionsImpl}
import org.jetbrains.kotlin.types.typeUtil.TypeUtilsKt

import org.jetbrains.kotlin.resolve.jvm.JvmPrimitiveType

import scala.jdk.CollectionConverters._

object TypeRenderer {

  private val cpgUnresolvedType = ErrorUtils.createUnresolvedType(TypeConstants.cpgUnresolved, List().asJava)

  val primitiveArrayMappings = Map[String, String](
    "kotlin.BooleanArray" -> "boolean[]",
    "kotlin.ByteArray"    -> "byte[]",
    "kotlin.CharArray"    -> "char[]",
    "kotlin.DoubleArray"  -> "double[]",
    "kotlin.FloatArray"   -> "float[]",
    "kotlin.IntArray"     -> "int[]",
    "kotlin.LongArray"    -> "long[]",
    "kotlin.ShortArray"   -> "short[]"
  )

  def descriptorRenderer(): DescriptorRenderer = {
    val opts = new DescriptorRendererOptionsImpl
    opts.setParameterNamesInFunctionalTypes(false)
    opts.setInformativeErrorType(false)
    opts.setTypeNormalizer {
      case _: UnresolvedType => cpgUnresolvedType
      case _: ErrorType      => cpgUnresolvedType
      case t                 => t
    }
    new DescriptorRendererImpl(opts)
  }

  def renderFqName(desc: DeclarationDescriptor): String = {
    val renderer = descriptorRenderer()
    val fqName   = DescriptorUtils.getFqName(desc)
    stripped(renderer.renderFqName(fqName))
  }

  def render(t: KotlinType, shouldMapPrimitiveArrayTypes: Boolean = true, unwrapPrimitives: Boolean = true): String = {
    val renderer = descriptorRenderer()
    val rendered = {
      if (TypeUtilsKt.isTypeParameter(t)) {
        TypeConstants.javaLangObject
      } else if (isFunctionXType(t)) {
        TypeConstants.kotlinFunctionXPrefix + (t.getArguments.size() - 1).toString
      } else {
        val descriptor = TypeUtils.getClassDescriptor(t)
        if (descriptor != null) {
          val fqName     = DescriptorUtils.getFqName(descriptor)
          val mappedType = JavaToKotlinClassMap.INSTANCE.mapKotlinToJava(fqName)
          if (mappedType != null) {
            val fqName                   = mappedType.asSingleFqName()
            val nonUnwrappedRender       = stripped(renderer.renderFqName(fqName.toUnsafe))
            val isWrapperOfPrimitiveType = JvmPrimitiveType.isWrapperClassName(fqName)

            if (unwrapPrimitives && isWrapperOfPrimitiveType) {
              JvmPrimitiveType
                .values()
                .toList
                .filter(_.getWrapperFqName.toString == fqName.toString)
                .map(_.getJavaKeywordName)
                .head
            } else {
              nonUnwrappedRender
            }
          } else {
            val descriptor = TypeUtils.getClassDescriptor(t)
            if (DescriptorUtils.isCompanionObject(descriptor)) {
              val rendered            = stripped(renderer.renderFqName(fqName))
              val companionObjectName = descriptor.getName
              // replaces `apkg.ContaininClass.CompanionObjectName` with `apkg.ContainingClass$CompanionObjectName`
              rendered.replaceFirst("\\." + companionObjectName, "\\$" + companionObjectName)
            } else {
              descriptor.getContainingDeclaration match {
                case fn: SimpleFunctionDescriptor =>
                  val renderedFqName     = stripped(renderer.renderFqName(DescriptorUtils.getFqName(descriptor)))
                  val containingDescName = fn.getName
                  // replaces `apkg.containingMethodName.className` with `apkg.className$containingMethodName`
                  renderedFqName.replaceFirst(
                    "\\." + containingDescName + "\\.([^.]+)",
                    ".$1" + "\\$" + containingDescName
                  )
                case _ => stripped(renderer.renderType(t))
              }
            }
          }
        } else {
          val relevantT =
            Option(TypeUtilsKt.getImmediateSuperclassNotAny(t))
              .getOrElse(t)
          val rendered = renderer.renderType(relevantT)
          stripped(rendered)
        }
      }
    }

    if (shouldMapPrimitiveArrayTypes && primitiveArrayMappings.contains(rendered)) {
      primitiveArrayMappings.get(rendered).get
    } else if (rendered == TypeConstants.kotlinUnit) {
      TypeConstants.void
    } else {
      rendered
    }
  }

  private def isFunctionXType(t: KotlinType): Boolean = {
    val renderer            = descriptorRenderer()
    val renderedConstructor = renderer.renderTypeConstructor(t.getConstructor)
    renderedConstructor.startsWith(TypeConstants.kotlinFunctionXPrefix) ||
    renderedConstructor.startsWith(TypeConstants.kotlinSuspendFunctionXPrefix)
  }

  def stripped(typeName: String): String = {
    def stripTypeParams(typeName: String): String = {
      typeName.replaceAll("<.*>", "")
    }
    def stripOut(name: String): String = {
      if (name.contains("<") && name.contains(">") && name.contains("out")) {
        name.replaceAll("(<[^o]*)[(]?out[)]?[ ]*([a-zA-Z])", "<$2")
      } else {
        name
      }
    }
    def stripOptionality(typeName: String): String = {
      typeName.replaceAll("!", "").replaceAll("\\?", "")
    }
    def stripDebugInfo(typeName: String): String = {
      if (typeName.contains("/* =")) {
        typeName.split("/\\* =")(0)
      } else {
        typeName
      }
    }

    stripTypeParams(stripOptionality(stripDebugInfo(stripOut(typeName))).trim().replaceAll(" ", ""))
  }
}
