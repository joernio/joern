package io.joern.kotlin2cpg.types

import io.joern.kotlin2cpg.psi.PsiUtils
import io.joern.x2cpg.Defines
import org.jetbrains.kotlin.descriptors.{ClassDescriptor, DeclarationDescriptor, SimpleFunctionDescriptor}
import org.jetbrains.kotlin.resolve.{DescriptorToSourceUtils, DescriptorUtils}
import org.jetbrains.kotlin.types.{ErrorUtils, KotlinType, TypeProjection, TypeUtils}
import org.jetbrains.kotlin.types.error.ErrorType
import org.jetbrains.kotlin.builtins.jvm.JavaToKotlinClassMap
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.renderer.{DescriptorRenderer, DescriptorRendererImpl, DescriptorRendererOptionsImpl}
import org.jetbrains.kotlin.types.typeUtil.TypeUtilsKt
import org.jetbrains.kotlin.resolve.jvm.JvmPrimitiveType

import scala.jdk.CollectionConverters.*

object TypeRenderer {

  private val cpgUnresolvedType =
    ErrorUtils.createUnresolvedType(Defines.UnresolvedNamespace, new java.util.ArrayList[TypeProjection]())

  val primitiveArrayMappings: Map[String, String] = Map[String, String](
    "kotlin.BooleanArray" -> "boolean[]",
    "kotlin.ByteArray"    -> "byte[]",
    "kotlin.CharArray"    -> "char[]",
    "kotlin.DoubleArray"  -> "double[]",
    "kotlin.FloatArray"   -> "float[]",
    "kotlin.IntArray"     -> "int[]",
    "kotlin.LongArray"    -> "long[]",
    "kotlin.ShortArray"   -> "short[]"
  )

}

class TypeRenderer(val keepTypeArguments: Boolean = false) {

  import TypeRenderer.*

  private def descriptorRenderer(): DescriptorRenderer = {
    val opts = new DescriptorRendererOptionsImpl
    opts.setParameterNamesInFunctionalTypes(false)
    opts.setInformativeErrorType(false)
    opts.setTypeNormalizer {
      case _: ErrorType => cpgUnresolvedType
      case t            => t
    }
    opts.lock()
    new DescriptorRendererImpl(opts)
  }

  def renderFqNameForDesc(desc: DeclarationDescriptor): String = {
    val renderer     = descriptorRenderer()
    val fqName       = DescriptorUtils.getFqName(desc)
    val simpleRender = stripped(renderer.renderFqName(fqName))
    def maybeReplacedOrTake(c: DeclarationDescriptor, or: String): String = {
      c match {
        case tc: ClassDescriptor if DescriptorUtils.isCompanionObject(tc) || tc.isInner =>
          val rendered = stripped(renderer.renderFqName(fqName))
          rendered.replaceFirst("\\." + c.getName, "\\$" + c.getName)
        case tc: ClassDescriptor if DescriptorUtils.isAnonymousObject(tc) =>
          val rendered = stripped(renderer.renderFqName(fqName))

          val psiElement        = DescriptorToSourceUtils.getSourceFromDescriptor(tc)
          val psiContainingDecl = DescriptorToSourceUtils.getSourceFromDescriptor(tc.getContainingDeclaration)
          val objectIdx =
            PsiUtils
              .objectIdxMaybe(psiElement, psiContainingDecl)
              .getOrElse("nan")
          val out = rendered.replaceFirst("\\.$", "\\$object\\$" + s"$objectIdx")
          out
        case _ => or
      }
    }
    val strippedOfContainingDeclarationIfNeeded =
      Option(desc.getContainingDeclaration)
        .map {
          case c: ClassDescriptor => maybeReplacedOrTake(c, simpleRender)
          case _                  => simpleRender
        }
        .getOrElse(simpleRender)
    desc match {
      case c: ClassDescriptor => maybeReplacedOrTake(c, strippedOfContainingDeclarationIfNeeded)
      case _                  => strippedOfContainingDeclarationIfNeeded
    }
  }

  private def maybeUnwrappedRender(render: String, unwrapPrimitives: Boolean, fqName: FqName) = {
    val isWrapperOfPrimitiveType = JvmPrimitiveType.isWrapperClassName(fqName)
    if (unwrapPrimitives && isWrapperOfPrimitiveType) {
      JvmPrimitiveType
        .values()
        .toList
        .filter(_.getWrapperFqName.toString == fqName.toString)
        .map(_.getJavaKeywordName)
        .head
    } else render
  }

  private def renderForDescriptor(descriptor: ClassDescriptor, unwrapPrimitives: Boolean, t: KotlinType): String = {
    val renderer = descriptorRenderer()
    val fqName   = DescriptorUtils.getFqName(descriptor)
    Option(JavaToKotlinClassMap.INSTANCE.mapKotlinToJava(fqName))
      .map { mappedType =>
        val fqName = mappedType.asSingleFqName()
        val render = stripped(renderer.renderFqName(fqName.toUnsafe))
        maybeUnwrappedRender(render, unwrapPrimitives, fqName)
      }
      .getOrElse {
        if (DescriptorUtils.isCompanionObject(descriptor) || descriptor.isInner) {
          val rendered            = stripped(renderer.renderFqName(fqName))
          val companionObjectName = descriptor.getName
          // replaces `apkg.ContainingClass.CompanionObjectName` with `apkg.ContainingClass$CompanionObjectName`
          rendered.replaceFirst("\\." + companionObjectName, "\\$" + companionObjectName)
        } else {
          descriptor.getContainingDeclaration match {
            case fn: SimpleFunctionDescriptor =>
              val renderedFqName     = stripped(renderer.renderFqName(DescriptorUtils.getFqName(descriptor)))
              val containingDescName = fn.getName
              // replaces `apkg.containingMethodName.className` with `apkg.className$containingMethodName`
              renderedFqName.replaceFirst("\\." + containingDescName + "\\.([^.]+)", ".$1" + "\\$" + containingDescName)
            case _ => stripped(renderer.renderType(t))
          }
        }
      }
  }

  def render(t: KotlinType, shouldMapPrimitiveArrayTypes: Boolean = true, unwrapPrimitives: Boolean = true): String = {
    val rendered =
      if (t.isInstanceOf[ErrorType]) TypeConstants.any
      else if (TypeUtilsKt.isTypeParameter(t)) TypeConstants.javaLangObject
      else if (isFunctionXType(t)) TypeConstants.kotlinFunctionXPrefix + (t.getArguments.size() - 1).toString
      else
        Option(TypeUtils.getClassDescriptor(t))
          .map { descriptor =>
            renderForDescriptor(descriptor, unwrapPrimitives, t)
          }
          .getOrElse {
            val renderer  = descriptorRenderer()
            val relevantT = Option(TypeUtilsKt.getImmediateSuperclassNotAny(t)).getOrElse(t)
            stripped(renderer.renderType(relevantT))
          }
    val renderedType =
      if (shouldMapPrimitiveArrayTypes && primitiveArrayMappings.contains(rendered)) primitiveArrayMappings(rendered)
      else if (rendered == TypeConstants.kotlinUnit) TypeConstants.void
      else rendered

    if (keepTypeArguments && !t.getArguments.isEmpty) {
      val typeArgs = t.getArguments.asScala
        .map(_.getType)
        .map(render(_, shouldMapPrimitiveArrayTypes, unwrapPrimitives))
        .mkString(",")
      s"$renderedType<$typeArgs>"
    } else {
      renderedType
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
      // (?<!^) is a regex lookbehind expression which allows to not
      // replace stuff between < > when it is right at the beginning.
      // We do this because at the beginning of a type name we cannot
      // have type parameters but instead <unresolvedNamespace> which
      // we do not want to strip.
      // Sometimes with a lambda expression as an argument, we see a
      // named function instead of a lambda, so we keep the
      // <anonymous> tag.
      typeName.replaceAll("(?<!^)<(?!anonymous).*>", "")
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

    val t1 = stripOut(typeName)
    val t2 = stripDebugInfo(t1)
    val t3 = stripOptionality(t2)
    val t4 = stripTypeParams(t3)
    t4.trim().replaceAll(" ", "").replaceAll("`", "")
  }
}
